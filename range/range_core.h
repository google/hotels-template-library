// Copyright 2022 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// https://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#ifndef THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_RANGE_RANGE_CORE_H_
#define THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_RANGE_RANGE_CORE_H_

#include <algorithm>
#include <cstddef>
#include <functional>
#include <iterator>
#include <optional>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#include <absl/base/attributes.h>

namespace htls::range {

// Applies combinators.
/* Example: ********************************************************************
std::vector<int> result = Apply(std::vector{1, 2, 3, 4},                 //
                                Transform(&Foo::bar),                    //
                                Transform([](int i) { return 2 * i; }),  //
                                ToVector()                               //
);
// ****************************************************************************/
template <typename Range, typename... Combinators>
decltype(auto) Apply(Range&& range, Combinators&&... combinators);

// Returns a combinator composed of other combinators.
template <typename... Combinators>
auto Compose(Combinators&&... combinators);

/*******************************************************************************
* Only implementation details and dragons below here. **************************
********************************************************************************
                              ______________
                        ,===:'.,            `-._
Art by                       `:.`---.__         `-._
 John VanderZwaag              `:.     `--.         `.
                                 \.        `.         `.
                         (,,(,    \.         `.   ____,-`.,
                      (,'     `/   \.   ,--.___`.'
                  ,  ,'  ,--.  `,   \.;'         `
                   `{D, {    \  :    \;
                     V,,'    /  /    //
                     j;;    /  ,' ,-//.    ,---.      ,
                     \;'   /  ,' /  _  \  /  _  \   ,'/
                           \   `'  / \  `'  / \  `.' /
                            `.___,'   `.__,'   `.__,'

*******************************************************************************/

// Whether a combinator's input or output is a range or value.
enum class ProcessingStyle {
  kIncremental,
  kComplete,
};

// Unwraps std::reference_wrapper or else returns the reference.
template <typename T>
ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) UnwrapReference(
    std::reference_wrapper<T> ref) {
  return ref.get();
}

template <typename T>
ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) UnwrapReference(T&& ref) {
  return std::forward<T>(ref);
}

// Struct to treat types as values.
template <typename T>
struct TypeIdentity {
  using type = T;
};

// Alias to get the type back out of Type.
template <typename T>
using TypeIdentityT = typename std::decay_t<T>::type;

// Used to create a combinator.
// See FilterImpl and Filter function in range_combinators.h for an example.
// An (incremental) combinator is of the form:
/* Example: ********************************************************************
template <typename InputType>
struct IncrementalIdentityCombinator {
  using OutputType = InputType;

  // Called with each element of the range.
  // One of ProcessIncremental or ProcessComplete must be implemented.
  template <typename Next>
  void ProcessIncremental(InputType input, Next&& next) {
    next.ProcessIncremental(t);
  }

  // Called when all input has been passed. This is optional.
  template <typename Next>
  void End(Next&&) {}

  // Called to check whether previous combinators need to stop. Useful for
  // implementing Take.
  // This is optional.
  bool Done() { return false; }
};
// ****************************************************************************/
//
// Then there is a function to return the combinator creator
/* Example: ********************************************************************
auto IncrementalIdentity() {
  return MakeCombinatorCreator<ProcessingStyle::kIncremental,
                               ProcessingStyle::kIncremental>([](auto type) {
    return IncrementalIdentityCombinator<Type_t<decltype(type)>>{};
  });
}
// ****************************************************************************/
template <ProcessingStyle input_processing_style,
          ProcessingStyle output_processing_style,
          template <typename...> typename Combinator, typename... Parameters,
          typename... Ts>
auto MakeCombinator(Ts&&... ts);

// Implementation details below:

namespace internal_htls_range {

// Void_t specializations and template variables to determine if a combinator
// implements the various combinator member functions.
template <typename Combinator, typename T, typename Next, typename = void>
struct HasProcessIncremental : std::false_type {};

template <typename Combinator, typename T, typename Next>
struct HasProcessIncremental<
    Combinator, T, Next,
    std::void_t<decltype(std::declval<Combinator&>().ProcessIncremental(
        std::declval<T>(), std::declval<Next>()))>> : std::true_type {};

template <typename Combinator, typename T, typename Next>
static constexpr bool kHasProcessIncremental =
    HasProcessIncremental<Combinator, T, Next>::value;

template <typename Combinator, typename T, typename Next, typename = void>
struct HasProcessComplete : std::false_type {};

template <typename Combinator, typename T, typename Next>
struct HasProcessComplete<
    Combinator, T, Next,
    std::void_t<decltype(std::declval<Combinator&>().ProcessComplete(
        std::declval<T>(), std::declval<Next>()))>> : std::true_type {};

template <typename Combinator, typename T, typename Next>
static constexpr bool kHasProcessComplete =
    HasProcessComplete<Combinator, T, Next>::value;

template <typename Combinator, typename Next, typename = void>
struct HasEnd : std::false_type {};

template <typename Combinator, typename Next>
struct HasEnd<Combinator, Next,
              std::void_t<decltype(std::declval<Combinator&>().End(
                  std::declval<Next>()))>> : std::true_type {};

template <typename Combinator, typename Next>
static constexpr bool kHasEnd = HasEnd<Combinator, Next>::value;

template <typename Combinator, typename = void>
struct HasDone : std::false_type {};

template <typename Combinator>
struct HasDone<Combinator,
               std::void_t<decltype(std::declval<const Combinator&>().Done())>>
    : std::true_type {};

template <typename Combinator>
static constexpr bool kHasDone = HasDone<Combinator>::value;

template <size_t I>
struct Index : std::integral_constant<size_t, I> {};

// Flatten nested tuples
// https://gcc.godbolt.org/z/5r1eGroa3 (written by jbandela)

template <size_t I, typename Tuple>
decltype(auto) operator+(Tuple&& t, Index<I>) {
  return std::get<I>(std::forward<Tuple>(t));
}

struct TupleFlattenerBase {
  static void Get() {}
};

// Used in fold expression to flatten tuple.
template <typename Base, size_t starting_index, size_t I, size_t... Is>
struct TupleFlattener : Base {
  using Base::Get;
  template <typename Tuple>
  static decltype(auto) Get(Index<I> index, Tuple&& tuple) {
    return std::get<I - starting_index>(
        (std::forward<Tuple>(tuple) + ... + Index<Is>()));
  }
  static constexpr size_t size() { return I + 1; }
};

// Starting type of the fold expression.
template <typename Base, size_t starting_index, size_t I, size_t... Is>
struct StartingTupleFlattener : Base {};

template <typename Base, size_t starting_index, size_t I, size_t... Is,
          typename T>
auto operator+(TupleFlattener<Base, starting_index, I, Is...>, T&&) {
  using NewBase = TupleFlattener<Base, starting_index, I, Is...>;
  return TupleFlattener<NewBase, starting_index, I + 1, Is...>();
}

template <typename Base, size_t starting_index, size_t I, size_t... Is,
          typename T>
auto operator+(StartingTupleFlattener<Base, starting_index, I, Is...>, T&&) {
  return TupleFlattener<Base, starting_index, I, Is...>();
}

template <typename Base, size_t starting_index, size_t I, size_t... Is,
          typename... T>
auto operator+(TupleFlattener<Base, starting_index, I, Is...>,
               std::tuple<T...>& t) {
  using NewBase = TupleFlattener<Base, starting_index, I, Is...>;
  using FlattenedTuple =
      decltype((StartingTupleFlattener<NewBase, I + 1, I + 1, Is...,
                                       I - starting_index + 1>() +
                ... + std::declval<T&>()));
  static constexpr auto sz = FlattenedTuple::size();
  return StartingTupleFlattener<FlattenedTuple, sz - (2 + I - starting_index),
                                sz, Is...>();
}

template <typename Base, size_t starting_index, size_t I, size_t... Is,
          typename... T>
auto operator+(StartingTupleFlattener<Base, starting_index, I, Is...>,
               std::tuple<T...>& t) {
  using FlattenedTuple = decltype((
      StartingTupleFlattener<Base, I, I, Is..., I - starting_index>() + ... +
      std::declval<T&>()));
  static constexpr auto sz = FlattenedTuple::size();

  return StartingTupleFlattener<FlattenedTuple, sz - (I - starting_index + 1),
                                sz, Is...>();
}

template <typename FlattenerType, size_t... Is, typename Tuple, typename F>
decltype(auto) FlattenArgsImpl(std::index_sequence<Is...>, Tuple&& tuple,
                               F&& f) {
  return f(FlattenerType::Get(Index<Is>(), std::forward<Tuple>(tuple))...);
}

template <typename... Ts, typename F>
decltype(auto) FlattenArgs(F&& f, Ts&&... ts) {
  using FlattenerType =
      decltype((StartingTupleFlattener<TupleFlattenerBase, 0, 0>() + ... + ts));
  return FlattenArgsImpl<FlattenerType>(
      std::make_index_sequence<FlattenerType::size()>(),
      std::forward_as_tuple(std::forward<Ts>(ts)...), std::forward<F>(f));
}

// Wraps a combinator struct. This uses the CRTP trick where Chain is the type
// that will inherit from all the combinator wrappers in a tuple like structure.
// Knowing the derived class Chain, allows a CombinatorWrapper to get the next
// combinator in the chain.
template <typename Chain, size_t I, typename InputTypeParam,
          typename CombinatorCreator>
class CombinatorWrapper {
 public:
  using InputType = InputTypeParam;
  using Combinator =
      typename CombinatorCreator::template Combinator<InputTypeParam>;

  using OutputType = typename Combinator::OutputType;
  static_assert(std::is_lvalue_reference_v<InputType> ||
                    std::is_rvalue_reference_v<InputType>,
                "InputType must be a reference.");
  static_assert(std::is_lvalue_reference_v<OutputType> ||
                    std::is_rvalue_reference_v<OutputType>,
                "OutputType must be a reference.");

  decltype(auto) Get(Index<I>) { return *this; }
  decltype(auto) Get(Index<I>) const { return *this; }

  template <typename Creator>
  explicit CombinatorWrapper(Creator&& creator)
      : combinator_(std::forward<Creator>(creator)(TypeIdentity<InputType>())) {
  }

  ABSL_ATTRIBUTE_ALWAYS_INLINE auto& GetChain() {
    return static_cast<Chain&>(*this);
  }

  ABSL_ATTRIBUTE_ALWAYS_INLINE auto& GetChain() const {
    return static_cast<const Chain&>(*this);
  }

  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) Next() {
    return GetChain().Get(Index<I + 1>());
  }

  ABSL_ATTRIBUTE_ALWAYS_INLINE constexpr decltype(auto) End() {
    static_assert(CombinatorCreator::kInputProcessingStyle ==
                      ProcessingStyle::kIncremental,
                  "End called on a complete processing "
                  "combinator.");
    if constexpr (kHasEnd<Combinator, decltype(Next())>) {
      static_assert(!std::is_void_v<decltype(combinator_.End(Next()))>,
                    "End must not return void.");
      return combinator_.End(Next());
    } else {
      return Next().End();
    }
  }
  ABSL_ATTRIBUTE_ALWAYS_INLINE constexpr bool Done() const {
    if constexpr (kHasDone<Combinator>) {
      return combinator_.Done();
    } else {
      return false;
    }
  }

  ABSL_ATTRIBUTE_ALWAYS_INLINE constexpr bool AnyDone() const {
    return GetChain().AnyDone(Index<I>());
  }

  // Only accept InputType with exactly matching reference type.
  template <typename T>
  ABSL_ATTRIBUTE_ALWAYS_INLINE void ProcessIncremental(T&& t) = delete;
  ABSL_ATTRIBUTE_ALWAYS_INLINE void ProcessIncremental(InputType t) {
    constexpr bool has_process_incremental =
        kHasProcessIncremental<Combinator, InputType, decltype(Next())>;

    static_assert(
        CombinatorCreator::kInputProcessingStyle ==
            ProcessingStyle::kIncremental,
        "ProcessIncremental called on combinator that is not incremental.");
    static_assert(
        CombinatorCreator::kInputProcessingStyle !=
                ProcessingStyle::kIncremental ||
            has_process_incremental,
        "Combinator with incremental processing style does not implement "
        "ProcessIncremental.");
    if constexpr (has_process_incremental) {
      combinator_.ProcessIncremental(static_cast<InputType>(t), Next());
    }
  }

  // Only accept InputType with exactly matching reference type.
  template <
      typename T,
      ProcessingStyle input_style = CombinatorCreator::kInputProcessingStyle,
      typename = std::enable_if_t<input_style == ProcessingStyle::kComplete>>
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) ProcessComplete(T&&) = delete;
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) ProcessComplete(InputType t) {
    constexpr bool has_process_complete =
        kHasProcessComplete<Combinator, InputType, decltype(Next())>;
    static_assert(
        has_process_complete,
        "Combinator with complete processing style does not implement "
        "ProcessComplete.");
    if constexpr (has_process_complete) {
      static_assert(!std::is_void_v<decltype(combinator_.ProcessComplete(
                        static_cast<InputType>(t), Next()))>,
                    "ProcessComplete must not return void.");
      return combinator_.ProcessComplete(static_cast<InputType>(t), Next());
    }
  }
  // If we need to do an implicit Complete->Incremental conversion, we don't
  // care what the reference type of the argument is.
  template <
      typename T, typename = void,
      ProcessingStyle input_style = CombinatorCreator::kInputProcessingStyle,
      typename = std::enable_if_t<input_style == ProcessingStyle::kIncremental>>
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) ProcessComplete(T&& t) {
    constexpr bool has_process_incremental =
        kHasProcessIncremental<Combinator, InputType, decltype(Next())>;
    for (InputType input : t) {
      if (AnyDone()) break;
      static_assert(
          has_process_incremental,
          "Combinator with incremental processing style does not implement "
          "ProcessIncremental.");

      if constexpr (has_process_incremental) {
        combinator_.ProcessIncremental(static_cast<InputType>(input), Next());
      }
    }
    return End();
  }

 private:
  Combinator combinator_;
};

struct BaseTypeHelper {
  static void Get() {}
};

// Does the type computation, and is used the fold to compute the input and
// output types of the combinator chain.
template <typename Base, size_t I, ProcessingStyle output_processing_style,
          typename InputTypeParam, typename OutputTypeParam>
struct TypeHelper : Base {
  using InputType = InputTypeParam;
  using OutputType = OutputTypeParam;

  using Base::Get;
  static TypeHelper Get(Index<I>) { return {}; }

  template <typename CombinatorCreator>
  auto operator+(CombinatorCreator&) {
    if constexpr (output_processing_style == ProcessingStyle::kComplete &&
                  CombinatorCreator::kInputProcessingStyle ==
                      ProcessingStyle::kIncremental) {
      using std::begin;
      using ValueType = decltype(*begin(
          std::declval<std::add_lvalue_reference_t<OutputType>>()));
      return TypeHelper<TypeHelper, I + 1,
                        CombinatorCreator::kOutputProcessingStyle, ValueType,
                        typename CombinatorCreator::template Combinator<
                            ValueType>::OutputType>();

    } else {
      return TypeHelper<TypeHelper, I + 1,
                        CombinatorCreator::kOutputProcessingStyle, OutputType,
                        typename CombinatorCreator::template Combinator<
                            OutputType>::OutputType>();
    }
  }
};

// Tuple like struct computing the types.
template <ProcessingStyle starting_input_processing_style, typename StartingT,
          typename... CombinatorCreators>
using Types =
    decltype((TypeHelper<BaseTypeHelper, 0, starting_input_processing_style,
                         StartingT, StartingT>() +
              ... + std::declval<CombinatorCreators&>()));

// The index of types for the combinators are offset 1, because the initial type
// is just the starting type, so we add 1 to the index given.
template <typename Types, size_t I>
using InputType = typename decltype(Types::Get(Index<I + 1>()))::InputType;

template <typename Types, size_t I>
using OutputType = typename decltype(Types::Get(Index<I + 1>()))::OutputType;

// Holds the chain of combinators.
template <template <typename> typename Terminator, typename Sequence,
          typename Types, typename... CombinatorCreators>
struct CombinatorChainImpl;

// Holds the final value at the end of the chain of combinators.
template <typename InputType>
struct ChainTerminator {
  template <typename T>
  ABSL_ATTRIBUTE_ALWAYS_INLINE T ProcessComplete(T&& t) {
    return std::forward<T>(t);
  }
};

template <typename Chain, size_t I>
using Combinator =
    std::decay_t<decltype(std::declval<Chain&>().Get(Index<I>()))>;

// Derives from all the wrapped CombinatorCreators.
template <template <typename> typename Terminator, size_t... Is, typename Types,
          typename... CombinatorCreators>
struct CombinatorChainImpl<Terminator, std::index_sequence<Is...>, Types,
                           CombinatorCreators...>
    : CombinatorWrapper<
          CombinatorChainImpl<Terminator, std::index_sequence<Is...>, Types,
                              CombinatorCreators...>,
          Is, InputType<Types, Is>, CombinatorCreators>... {
  using Self = CombinatorChainImpl;
  static constexpr size_t kSize = sizeof...(CombinatorCreators);

  static_assert(sizeof...(CombinatorCreators) > 0);

  using OutputType = internal_htls_range::OutputType<Types, kSize - 1>;

  using CombinatorWrapper<
      CombinatorChainImpl<Terminator, std::index_sequence<Is...>, Types,
                          CombinatorCreators...>,
      Is, internal_htls_range::InputType<Types, Is>,
      CombinatorCreators>::Get...;

  Terminator<OutputType> terminator;

  ABSL_ATTRIBUTE_ALWAYS_INLINE auto& Get(Index<kSize>) { return terminator; }

  template <typename... Ts>
  ABSL_ATTRIBUTE_ALWAYS_INLINE explicit CombinatorChainImpl(Ts&&... ts)
      : CombinatorWrapper<Self, Is, internal_htls_range::InputType<Types, Is>,
                          CombinatorCreators>{std::forward<Ts>(ts)}... {}

  // Checks if any of the combinators is done, starting at index.
  template <size_t index>
  ABSL_ATTRIBUTE_ALWAYS_INLINE constexpr bool AnyDone(Index<index>) const {
    return ((Is >= index && this->Get(Index<Is>()).Done()) || ...);
  }
};

// Alias for CombinatorChain passing the index sequence to CombinatorChainImpl.
template <template <typename T> typename Terminator, typename StartingT,
          typename... CombinatorCreators>
using CombinatorChain = CombinatorChainImpl<
    Terminator, std::make_index_sequence<sizeof...(CombinatorCreators)>,
    Types<ProcessingStyle::kComplete, StartingT, CombinatorCreators...>,
    CombinatorCreators...>;

// Wraps the combinator creator and exposes the processing styles.
template <ProcessingStyle input_processing_style,
          ProcessingStyle output_processing_style, typename ParamsTuple,
          template <typename...> typename CombinatorTemplate,
          typename... CombinatorParams>
struct CombinatorCreator {
  template <typename T>
  using Combinator = CombinatorTemplate<T, CombinatorParams...>;
  static constexpr auto kInputProcessingStyle = input_processing_style;
  static constexpr auto kOutputProcessingStyle = output_processing_style;
  ParamsTuple params_tuple;
  template <typename... Args>
  explicit CombinatorCreator(Args&&... args)
      : params_tuple(std::forward<Args>(args)...) {}

  template <typename T>
  auto operator()(TypeIdentity<T>) const {
    return std::apply(
        [](auto&&... args) {
          return Combinator<T>{std::forward<decltype(args)>(args)...};
        },
        std::move(params_tuple));
  }
};

std::false_type IsTuple(...);

template <typename... Ts>
std::true_type IsTuple(const std::tuple<Ts...>&);

// Forwards lvalue references as lvalue references and values or rvalue
// references as values.
template <typename T>
T ForwardLRef(T& t) {
  return std::forward<T>(t);
}

}  // namespace internal_htls_range

template <typename Range, typename... Combinators>
ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) Apply(
    Range&& range, Combinators&&... combinators) {
  if constexpr (sizeof...(Combinators) == 0) {
    return internal_htls_range::ForwardLRef<Range>(range);
  } else {
    using InputType = decltype(std::forward<Range>(range));
    auto make_chain = [&](auto&&... combinators) mutable {
      using Chain = internal_htls_range::CombinatorChain<
          internal_htls_range::ChainTerminator, InputType,
          std::decay_t<decltype(combinators)>...>;
      return Chain{std::forward<decltype(combinators)>(combinators)...};
    };
    // If there are no tuples, avoid calling FlattenArgs. See cl/490598090
    // which has benchmarks that this makes a difference.
    if constexpr ((decltype(internal_htls_range::IsTuple(combinators))::value ||
                   ...)) {
      auto chain = internal_htls_range::FlattenArgs(
          make_chain, std::forward<Combinators>(combinators)...);
      return chain.Get(internal_htls_range::Index<0>())
          .ProcessComplete(std::forward<Range>(range));
    } else {
      auto chain = make_chain(std::forward<Combinators>(combinators)...);
      return chain.Get(internal_htls_range::Index<0>())
          .ProcessComplete(std::forward<Range>(range));
    }
  }
}

template <typename... Combinators>
ABSL_ATTRIBUTE_ALWAYS_INLINE auto Compose(Combinators&&... combinators) {
  return std::make_tuple(std::forward<Combinators>(combinators)...);
}

template <ProcessingStyle input_processing_style,
          ProcessingStyle output_processing_style,
          template <typename...> typename Combinator,
          typename... CombinatorParameters, typename... Ts>
auto MakeCombinator(Ts&&... ts) {
  return internal_htls_range::CombinatorCreator<
      input_processing_style, output_processing_style,
      std::tuple<std::decay_t<Ts>...>, Combinator, CombinatorParameters...>(
      std::forward<Ts>(ts)...);
}

}  // namespace htls::range

#endif  // THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_RANGE_RANGE_CORE_H_
