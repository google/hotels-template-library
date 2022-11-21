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

#ifndef THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_RANGE_RANGE_H_
#define THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_RANGE_RANGE_H_

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
#include <absl/status/status.h>
#include <absl/status/statusor.h>

// See go/output-combinators for the document on which this is based

// Alternative solution to composing ranges
// Based on
// https://www.fluentcpp.com/2019/02/12/the-terrible-problem-of-incrementing-a-smart-iterator/
// and
// https://www.fluentcpp.com/2017/11/28/output-iterator-adaptors-symmetry-range-adaptors/

// The idea is basically to do the transformations on the output side, instead
// of the input side. We gain several advantages:
// 1. No need for stash
// 2. Linear growth of stack-size instead of exponential
// 3. Combinators are simpler to read and write.

// The most important concept to understand when using this library is that of
// Processing Style. Processing Style refers to how a combinator processes its
// input and output. There are two kinds of styles, complete and incremental.

// Complete processing style processes a value as a whole. Think of sort. It
// sorts the whole collection. Incremental processing style, processes the
// elements of the value. Think of filter which filters each element
// individually, independent of the other elements.

// A combinator can have different styles for input and output. For example,
// ToVector, takes incremental input, and outputs complete output (an entire
// vector). Another example of this is Accumulate, which takes incremental
// input, and outputs the accumulated value as a whole.

// A combinator chain that is run, must end with a combinator that outputs a
// complete value.

// When you chain combinators, the input style of the current combinator must
// match the output style of the previous combinator. However, the library
// automatically will convert complete output of one combinator to incremental
// input for the next combinator. That way you can have Filter, immediately
// after Sort.

namespace htls::range {

// Applies combinators.
// Example:
// std::vector<int> result = Apply(
//   input_range, //
//   TransformEach(&Foo::bar), //
//   TransformEach([](int i){return 2 * i;}), //
//   ToVector() //
// );
template <typename Range, typename... Combinators>
decltype(auto) Apply(Range&& range, Combinators&&... combinators);

// Returns a combinator composed of other combinators.
template <typename FirstCombinator, typename... Combinators>
auto Compose(FirstCombinator&& first, Combinators&&... combinators);

// Filters based on a predicate. Only outputs values for which the predicate
// returns true.
// Input Processing Style: Incremental
// Output Processing Style: Incremental
// Example:
// std::vector<int> input_range = {1, 2, 3, 4};
// std::vector<int> result = Apply(
//   input_range, //
//   Filter([](int i){return i%2 == 1;}), //
//   ToVector() //
// );
//// result is {1, 3}
template <typename Predicate>
auto Filter(Predicate predicate);

// Transforms a range. It outputs the result of f.
// Input Processing Style: Incremental
// Output Processing Style: Incremental
// Example:
// std::vector<int> input_range = {1, 2, 3, 4};
// std::vector<int> result = Apply(
//   input_range, //
//   Transform([](int i){return static_cast<double>(i) + 0.5;}), //
//   ToVector() //
// );
//// result is {1.5, 2.5, 3.5, 4.5}
//
// You can pass an invocable like a pointer to member
// Transform(&MyStruct::int_member_variable)
template <typename F>
auto TransformEach(F f);

// Transforms an entire value as a whole.
template <typename F>
auto Transform(F f);

// Converts the output to a vector. The type of the vector is deduced.
// Input Processing Style: Incremental
// Output Processing Style: Complete
inline auto ToVector();

// Sorts the output.
// Input Processing Style: Complete
// Output Processing Style: Complete
// Note: Sort will sort the range in-place.
template <typename Comparator = std::less<>>
auto Sort(Comparator comparator = {});

// For sorted input, filters out the repeating elements.
// Input Processing Style: Incremental
// Output Processing Style: Incremental
template <typename Equality = std::equal_to<>>
auto UniqueEach(Equality equality = {});

template <typename Equality = std::equal_to<>>
auto Unique(Equality equality = {});

// Flattens a range of ranges into a single range.
// Input Processing Style: Incremental
// Output Processing Style: Incremental
inline auto Flatten();

// Takes count elements from the range.
// Input Processing Style: Incremental
// Output Processing Style: Incremental
inline auto Take(size_t count);

// Transforms to std::reference_wrapper
// Input Processing Style: Incremental
// Output Processing Style: Incremental
inline auto RefEach();

// Transforms to r-value reference.
// Input Processing Style: Incremental
// Output Processing Style: Incremental
inline auto MoveEach();

// Structure to hold enumated values
template <typename R>
struct EnumeratedValue {
  size_t index;
  R value;

  decltype(auto) ForwardValue() { return static_cast<R>(value); }
};
template <typename R>
EnumeratedValue(size_t, R) -> EnumeratedValue<R>;

// Enumerates values, providing an index.
// Input Processing Style: Incremental
// Output Processing Style: Incremental
inline auto Enumerate();

// Drops the index and returns values
// Input Processing Style: Incremental
// Output Processing Style: Incremental
inline auto Unenumerate();

// Calls f for each value and returns the number of times f was called.
// Input Processing Style: Incremental
// Output Processing Style: Complete
template <typename F>
auto ForEach(F f);

// Accumulates the values.
// Input Processing Style: Incremental
// Output Processing Style: Complete
template <typename Accumulated, typename F>
auto Accumulate(Accumulated accumulated, F f);

// Tests if any element fulfills a condition
// Input Processing Style: Incremental
// Output Processing Style: Complete
template <typename Predicate>
auto AnyOf(Predicate predicate);

// Tests if all the elements fulfills a condition
// Input Processing Style: Incremental
// Output Processing Style: Complete
template <typename Predicate>
auto AllOf(Predicate predicate);

// Tests if none of the elements fulfills a condition
// Input Processing Style: Incremental
// Output Processing Style: Complete
template <typename Predicate>
auto NoneOf(Predicate predicate);

// The following are needed to implement combinators:

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
// See FilterImpl and Filter function below for an example.
// A combinator is of the form:
// template<typename InputType>
// struct IncrementalIdentityCombinator{
//   using OutputType = InputType;
//
//   // Called with each element of the range.
//   // One ProcessIncremental or ProcessComplete must be implemented.
//   template<typename Next>
//   void ProcessIncremental(InputType input, Next&& next){
//     next.ProcessIncremental(t);
//   }
//
//   // Called when all input has been passed. This is optional.
//   template<typename Next>
//   void End(Next&&){}
//
//   // Called to check whether previous combinators need to stop. Useful for
//   // implementing Take.
//   // This is optional.
//   bool Done(){return false;}
// };

// Then there is a function to return the combinator creator
//  auto IncrementalIdentity(){
//    return MakeCombinatorCreator<ProcessingStyle::kIncremental,
//        ProcessingStyle::kIncremental>([](auto type){
//      return IncrementalIdentityCombinator<Type_t<decltype(type)>>{};
//     });
//  }
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

template <typename T, typename = void>
struct IsPointerMonad : std::false_type {};

template <typename T>
struct IsPointerMonad<T, std::void_t<decltype(std::declval<T>() == nullptr),
                                     decltype(*std::declval<T>())>>
    : std::true_type {};

template <typename T>
static constexpr bool kIsPointerMonad =
    IsPointerMonad<std::remove_reference_t<T>>::value;

template <typename T, typename = void>
struct IsOptionalMonad : std::false_type {};

template <typename T>
struct IsOptionalMonad<T, std::void_t<decltype(std::declval<T>().has_value()),
                                      decltype(*std::declval<T>())>>
    : std::true_type {};

template <typename T>
static constexpr bool kIsOptionalMonad =
    IsOptionalMonad<std::remove_reference_t<T>>::value;

template <typename T>
struct IsStatusOrMonad : std::false_type {};

template <typename T>
struct IsStatusOrMonad<absl::StatusOr<T>> : std::true_type {};

template <typename T>
static constexpr bool kIsStatusOrMonad =
    IsStatusOrMonad<std::decay_t<T>>::value;

template <size_t I>
using Index = std::integral_constant<size_t, I>;

// Wraps a combinator struct. This uses the CRTP trick where Chain is the type
// that will inherit from all the combinator wrappers in a tuple like structure.
// Knowing the derived class Chain, allows a CombinatorWrapper to get the next
// combinator in the chain.
template <typename Chain, size_t I, typename InputType,
          typename CombinatorCreator>
class CombinatorWrapper;

// Uses overloading and upcasting to get the CombinatorWrapper with the
// specified index in the chain.
template <size_t I, typename Chain, typename InputType,
          typename CombinatorCreator>
ABSL_ATTRIBUTE_ALWAYS_INLINE auto& GetWrapper(
    CombinatorWrapper<Chain, I, InputType, CombinatorCreator>& wrapper) {
  return wrapper;
}

// Uses overloading and upcasting to get the CombinatorWrapper with the
// specified index in the chain.
template <size_t I, typename Chain, typename InputType,
          typename CombinatorCreator>
ABSL_ATTRIBUTE_ALWAYS_INLINE auto& GetWrapper(
    const CombinatorWrapper<Chain, I, InputType, CombinatorCreator>& wrapper) {
  return wrapper;
}

template <typename Wrapper, typename Final>
struct WrapperAndFinal {
  Wrapper& wrapper;
  Final& final;

  template <typename T>
  ABSL_ATTRIBUTE_ALWAYS_INLINE void ProcessIncremental(T&& t) {
    wrapper.ProcessIncremental(std::forward<T>(t), final);
  }

  template <typename T>
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) ProcessComplete(T&& t) {
    return wrapper.ProcessComplete(std::forward<T>(t), final);
  }

  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) End() {
    return wrapper.End(final);
  }

  ABSL_ATTRIBUTE_ALWAYS_INLINE Final& GetFinal() { return final; }
};

template <typename Wrapper, typename Final>
WrapperAndFinal(Wrapper& wrapper, Final&& final)
    -> WrapperAndFinal<Wrapper, Final>;

template <typename Chain, size_t I, typename InputTypeParam,
          typename CombinatorCreator>
class CombinatorWrapper {
 public:
  using InputType = InputTypeParam;
  using Combinator =
      decltype(std::declval<CombinatorCreator&>()(TypeIdentity<InputType>()));

  using OutputType = typename Combinator::OutputType;
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

  template <typename Final>
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) Next(Final&& final) {
    if constexpr (I < Chain::kSize - 1) {
      return WrapperAndFinal{GetWrapper<I + 1>(GetChain()), final};
    } else {
      return final;
    }
  }

  template <typename Final>
  ABSL_ATTRIBUTE_ALWAYS_INLINE constexpr decltype(auto) End(Final&& final) {
    static_assert(CombinatorCreator::kInputProcessingStyle ==
                      ProcessingStyle::kIncremental,
                  "End called on a complete processing "
                  "combinator.");
    if constexpr (kHasEnd<Combinator, decltype(Next(final))>) {
      return combinator_.End(Next(final));
    } else {
      return Next(final).End();
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

  template <typename T, typename Final>
  ABSL_ATTRIBUTE_ALWAYS_INLINE void ProcessIncremental(T&& t, Final&& final) {
    constexpr bool has_process_incremental =
        kHasProcessIncremental<Combinator, InputType, decltype(Next(final))>;

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
      combinator_.ProcessIncremental(static_cast<InputType>(t), Next(final));
    }
  }

  template <typename T, typename Final>
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) ProcessComplete(T&& t,
                                                              Final&& final) {
    constexpr bool has_process_complete =
        kHasProcessComplete<Combinator, InputType, decltype(Next(final))>;
    constexpr bool has_process_incremental =
        kHasProcessIncremental<Combinator, InputType, decltype(Next(final))>;
    if constexpr (CombinatorCreator::kInputProcessingStyle ==
                  ProcessingStyle::kComplete) {
      static_assert(
          has_process_complete,
          "Combinator with complete processing style does not implement "
          "ProcessComplete.");
      if constexpr (has_process_complete) {
        return combinator_.ProcessComplete(static_cast<InputType>(t),
                                           Next(final));
      }
    } else {
      for (InputType input : t) {
        if (AnyDone()) break;
        static_assert(
            has_process_incremental,
            "Combinator with incremental processing style does not implement "
            "ProcessIncremental.");

        if constexpr (has_process_incremental) {
          combinator_.ProcessIncremental(static_cast<InputType>(input),
                                         Next(final));
        }
      }
      return End(final);
    }
  }

 private:
  Combinator combinator_;
};

template <typename CombinatorCreator>
struct CombinatorCreatorHolder {
  template <typename T>
  using OutputType =
      typename std::decay_t<decltype(std::declval<CombinatorCreator>()(
          TypeIdentity<T>()))>::OutputType;

  static constexpr ProcessingStyle kInputProcessingStyle =
      CombinatorCreator::kInputProcessingStyle;

  static constexpr ProcessingStyle kOutputProcessingStyle =
      CombinatorCreator::kOutputProcessingStyle;
};

struct BaseTypeHelper {};

// Does the type computation, and is used the fold to compute the input and
// output types of the combinator chain.
template <typename Base, size_t I, ProcessingStyle output_processing_style,
          typename InputTypeParam, typename OutputTypeParam>
struct TypeHelper : Base {
  using InputType = InputTypeParam;
  using OutputType = OutputTypeParam;

  template <typename CombinatorCreator>
  auto operator+(CombinatorCreatorHolder<CombinatorCreator> holder) {
    if constexpr (output_processing_style == ProcessingStyle::kComplete &&
                  holder.kInputProcessingStyle ==
                      ProcessingStyle::kIncremental) {
      using std::begin;
      using ValueType = decltype(*begin(std::declval<OutputType>()));
      return TypeHelper<
          TypeHelper, I + 1, holder.kOutputProcessingStyle, ValueType,
          typename CombinatorCreatorHolder<
              CombinatorCreator>::template OutputType<ValueType>>();

    } else {
      return TypeHelper<
          TypeHelper, I + 1, holder.kOutputProcessingStyle, OutputType,
          typename CombinatorCreatorHolder<
              CombinatorCreator>::template OutputType<OutputType>>();
    }
  }
};

// Uses overloading and upcasting to find the type helper corresponding to the
// index.
template <size_t I, typename Base, ProcessingStyle output_processing_style,
          typename InputType, typename OutputType>
auto GetTypeHelper(
    TypeHelper<Base, I, output_processing_style, InputType, OutputType>
        helper) {
  return helper;
}

// Tuple like struct computing the types.
template <ProcessingStyle starting_input_processing_style, typename StartingT,
          typename... CombinatorCreators>
using Types =
    decltype((TypeHelper<BaseTypeHelper, 0, starting_input_processing_style,
                         StartingT, StartingT>() +

              ... + CombinatorCreatorHolder<CombinatorCreators>()));

// The index of types for the combinators are offset 1, because the initial type
// is just the starting type, so we add 1 to the index given.
template <typename Types, size_t I>
using InputType = typename decltype(GetTypeHelper<I + 1>(Types()))::InputType;

template <typename Types, size_t I>
using OutputType = typename decltype(GetTypeHelper<I + 1>(Types()))::OutputType;

// Holds the chain of combinators.
template <ProcessingStyle starting_input_processing_style, typename StartingT,
          typename Sequence, typename Types, typename... CombinatorCreators>
struct CombinatorChainImpl;

// Holds the final value at the end of the chain of combinators.
template <typename InputType>
struct ChainTerminator {
  template <typename T>
  ABSL_ATTRIBUTE_ALWAYS_INLINE T ProcessComplete(T&& t) {
    return std::forward<T>(t);
  }

  ABSL_ATTRIBUTE_ALWAYS_INLINE ChainTerminator& GetFinal() { return *this; }
};

// Holds the final value at the end of the chain of combinators.
template <typename InputType>
struct StatusChainTerminator {
  absl::Status status = absl::OkStatus();
  using OutputType = absl::StatusOr<std::decay_t<InputType>>;
  template <typename T>
  ABSL_ATTRIBUTE_ALWAYS_INLINE OutputType ProcessComplete(T&& t) {
    return OutputType(std::forward<T>(t));
  }

  StatusChainTerminator& GetFinal() { return *this; }

  ABSL_ATTRIBUTE_ALWAYS_INLINE void SetError(absl::Status new_status) {
    status = new_status;
  }

  ABSL_ATTRIBUTE_ALWAYS_INLINE OutputType GetError() {
    return OutputType(status);
  }
};

template <typename Chain, size_t I>
using Combinator =
    std::decay_t<decltype(GetWrapper<I>(std::declval<Chain&>()))>;

// Derives from all the wrapped CombinatorCreators.
template <ProcessingStyle starting_input_processing_style, typename StartingT,
          size_t... Is, typename Types, typename... CombinatorCreators>
struct CombinatorChainImpl<starting_input_processing_style, StartingT,
                           std::index_sequence<Is...>, Types,
                           CombinatorCreators...>
    : CombinatorWrapper<
          CombinatorChainImpl<starting_input_processing_style, StartingT,
                              std::index_sequence<Is...>, Types,
                              CombinatorCreators...>,
          Is, InputType<Types, Is>, CombinatorCreators>... {
  using Self = CombinatorChainImpl;
  static constexpr size_t kSize = sizeof...(CombinatorCreators);

  static_assert(sizeof...(CombinatorCreators) > 0);

  using InputType = StartingT;
  using OutputType = internal_htls_range::OutputType<Types, kSize - 1>;

  static constexpr ProcessingStyle kInputProcessingStyle =
      starting_input_processing_style;
  static constexpr ProcessingStyle kOutputProcessingStyle =
      (CombinatorCreators::kOutputProcessingStyle, ...);

  template <typename... Ts>
  ABSL_ATTRIBUTE_ALWAYS_INLINE explicit CombinatorChainImpl(Ts&&... ts)
      : CombinatorWrapper<Self, Is, internal_htls_range::InputType<Types, Is>,
                          CombinatorCreators>{std::forward<Ts>(ts)}... {}

  // Checks if any of the combinators is done, starting at index.
  template <size_t index>
  ABSL_ATTRIBUTE_ALWAYS_INLINE constexpr bool AnyDone(Index<index>) const {
    return (
        (Is >= index && internal_htls_range::GetWrapper<Is>(*this).Done()) ||
        ...);
  }

  // Passes t to the first combinator.
  template <typename Final,
            typename = std::enable_if_t<sizeof(Final) != 0 &&
                                        kInputProcessingStyle ==
                                            ProcessingStyle::kComplete>>
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) ProcessComplete(InputType input,
                                                              Final&& final) {
    return internal_htls_range::GetWrapper<0>(*this).ProcessComplete(
        static_cast<InputType>(input), final);
  }

  template <typename Final,
            typename = std::enable_if_t<sizeof(Final) != 0 &&
                                        kInputProcessingStyle ==
                                            ProcessingStyle::kIncremental>>
  ABSL_ATTRIBUTE_ALWAYS_INLINE void ProcessIncremental(InputType input,
                                                       Final&& final) {
    internal_htls_range::GetWrapper<0>(*this).ProcessIncremental(
        static_cast<InputType>(input), final);
  }

  ABSL_ATTRIBUTE_ALWAYS_INLINE constexpr bool Done() const {
    return (internal_htls_range::GetWrapper<Is>(*this).Done() || ...);
  }

  // Calls End on all the combinators.
  template <typename Final>
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) End(Final&& final) {
    static_assert(kInputProcessingStyle == ProcessingStyle::kIncremental,
                  "End called on complete processing combinator");
    return internal_htls_range::GetWrapper<0>(*this).End(final);
  }
};

// Alias for CombinatorChain passing the index sequence to CombinatorChainImpl.
template <ProcessingStyle starting_input_processing_style, typename StartingT,
          typename... CombinatorCreators>
using CombinatorChain = CombinatorChainImpl<
    starting_input_processing_style, StartingT,
    std::make_index_sequence<sizeof...(CombinatorCreators)>,
    Types<starting_input_processing_style, StartingT, CombinatorCreators...>,
    CombinatorCreators...>;

template <typename Chain, size_t I>
using Combinator =
    std::decay_t<decltype(GetWrapper<I>(std::declval<Chain&>()))>;

// Wraps the combinator creator and exposes the processing styles.
template <ProcessingStyle input_processing_style,
          ProcessingStyle output_processing_style, typename CombinatorCreator,
          bool and_then = false>
struct CombinatorCreatorWrapper : CombinatorCreator {
  static constexpr auto kInputProcessingStyle = input_processing_style;
  static constexpr auto kOutputProcessingStyle = output_processing_style;
};

template <typename T>
struct HasAndThen;

template <ProcessingStyle starting_input_processing_style, typename StartingT,
          typename Sequence, typename Types, typename... CombinatorCreators>
struct HasAndThen<
    CombinatorChainImpl<starting_input_processing_style, StartingT, Sequence,
                        Types, CombinatorCreators...>>
    : std::integral_constant<bool,
                             (HasAndThen<CombinatorCreators>::value || ...)> {};

template <ProcessingStyle input_processing_style,
          ProcessingStyle output_processing_style, typename CombinatorCreator,
          bool has_and_then>
struct HasAndThen<
    CombinatorCreatorWrapper<input_processing_style, output_processing_style,
                             CombinatorCreator, has_and_then>>
    : std::integral_constant<bool, has_and_then> {};

template <ProcessingStyle starting_input_processing_style,
          typename... CombinatorCreators>
struct ComposedCombinatorCreator : CombinatorCreators... {
  static constexpr auto kInputProcessingStyle = starting_input_processing_style;
  static constexpr auto kOutputProcessingStyle =
      (CombinatorCreators::kOutputProcessingStyle, ...);
  template <typename T>
  auto operator()(TypeIdentity<T>) {
    return CombinatorChain<starting_input_processing_style, T,
                           CombinatorCreators...>{
        std::move(static_cast<CombinatorCreators&>(*this))...};
  }
};

template <ProcessingStyle starting_input_processing_style,
          typename... CombinatorCreators>
struct HasAndThen<ComposedCombinatorCreator<starting_input_processing_style,
                                            CombinatorCreators...>>
    : std::integral_constant<bool,
                             (HasAndThen<CombinatorCreators>::value || ...)> {};

template <typename InputType>
struct ToVectorImpl {
  using OutputType = std::vector<std::decay_t<InputType>>&&;

  std::vector<std::decay_t<InputType>> vector;

  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE void ProcessIncremental(InputType input,
                                                       Next&&) {
    vector.push_back(static_cast<InputType>(input));
  }

  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) End(Next&& next) {
    return next.ProcessComplete(std::move(vector));
  }
};

template <typename InputType, typename Predicate>
struct FilterImpl {
  using OutputType = InputType;

  Predicate f;

  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE void ProcessIncremental(InputType input,
                                                       Next&& next) {
    if (f(UnwrapReference(input))) {
      next.ProcessIncremental(input);
    }
  }
};

template <typename InputType, typename F>
struct TransformEachImpl {
  using InvokeResult =
      std::invoke_result_t<F&, decltype(UnwrapReference(
                                   std::declval<InputType>()))>;
  using OutputType =
      decltype(std::forward<InvokeResult>(std::declval<InvokeResult>()));
  F f;

  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE void ProcessIncremental(InputType input,
                                                       Next&& next) {
    next.ProcessIncremental(
        std::invoke(f, UnwrapReference(static_cast<InputType>(input))));
  }
};

template <typename T>
decltype(auto) UnwrapMonad(T&& t) {
  if constexpr (kIsPointerMonad<T>) {
    return *t;
  } else if constexpr (kIsOptionalMonad<T>) {
    return *std::forward<T>(t);
  } else if constexpr (kIsStatusOrMonad<T>) {
    return *std::forward<T>(t);
  } else {
    static_assert(sizeof(T) == 0, "Unsupported error type.");
  }
}

template <typename T>
bool CheckMonad(T&& t) {
  if constexpr (kIsPointerMonad<T>) {
    return static_cast<bool>(t);
  } else if constexpr (kIsOptionalMonad<T>) {
    return static_cast<bool>(t);
  } else if constexpr (kIsStatusOrMonad<T>) {
    return t.ok();
  } else {
    static_assert(sizeof(T) == 0, "Unsupported error type.");
  }
}

template <typename T>
absl::Status MonadToStatus(T&& t) {
  if constexpr (kIsPointerMonad<T>) {
    return absl::InvalidArgumentError("Null pointer");
  } else if constexpr (kIsOptionalMonad<T>) {
    return absl::InvalidArgumentError("Empty optional");
  } else if constexpr (kIsStatusOrMonad<T>) {
    return t.status();
  } else {
    static_assert(sizeof(T) == 0, "Unsupported error type.");
  }
}

template <typename InputType>
struct AndThenImpl {
  using OutputType = decltype(UnwrapMonad(std::declval<InputType>()));
  bool has_error = false;

  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE void ProcessIncremental(InputType input,
                                                       Next&& next) {
    if (CheckMonad(input)) {
      next.ProcessIncremental(UnwrapMonad(static_cast<InputType>(input)));
    } else {
      next.GetFinal().SetError(MonadToStatus(input));
      has_error = true;
    }
  }

  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) ProcessComplete(InputType input,
                                                              Next&& next) {
    if (CheckMonad(input)) {
      return next.ProcessComplete(UnwrapMonad(static_cast<InputType>(input)));
    } else {
      next.GetFinal().SetError(MonadToStatus(input));
      return next.GetFinal().GetError();
    }
  }

  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) End(Next&& next) {
    if (has_error) {
      return next.GetFinal().GetError();
    } else {
      return next.End();
    }
  }

  bool Done() const { return has_error; }
};

template <typename InputType, typename F>
struct TransformImpl {
  using InvokeResult =
      std::invoke_result_t<F&, decltype(std::declval<InputType>())>;
  using OutputType =
      decltype(std::forward<InvokeResult>(std::declval<InvokeResult>()));
  F f;

  static_assert(std::is_reference_v<OutputType>);
  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) ProcessComplete(InputType input,
                                                              Next&& next) {
    return next.ProcessComplete(std::invoke(f, static_cast<InputType>(input)));
  }
};
template <typename InputType, typename Equal>
struct UniqueEachImpl {
  using OutputType = InputType;
  Equal equal;
  std::conditional_t<std::is_rvalue_reference_v<InputType>,
                     std::optional<std::decay_t<InputType>>,
                     std::remove_reference_t<InputType>*>
      test_element;
  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE void ProcessIncremental(InputType input,
                                                       Next&& next) {
    if constexpr (std::is_rvalue_reference_v<InputType>) {
      const bool has_value = test_element.has_value();
      const bool is_equal = has_value && equal(UnwrapReference(input),
                                               UnwrapReference(*test_element));
      if (!is_equal) {
        if (has_value) {
          next.ProcessIncremental(std::move(*test_element));
        }
        test_element = static_cast<InputType>(input);
      }
    } else {
      const bool has_value = test_element;
      const bool is_equal = has_value && equal(UnwrapReference(input),
                                               UnwrapReference(*test_element));

      if (!is_equal) {
        if (has_value) {
          next.ProcessIncremental(*test_element);
        }
        test_element = &input;
      }
    }
  }

  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) End(Next&& next) {
    if constexpr (std::is_rvalue_reference_v<InputType>) {
      if (test_element.has_value()) {
        next.ProcessIncremental(*std::move(test_element));
      }
    } else {
      if (test_element) {
        next.ProcessIncremental(*test_element);
      }
    }
    return next.End();
  }
};

template <typename InputType>
struct FlattenImpl {
  using OutputType = decltype(*std::begin(std::declval<InputType>()));

  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE void ProcessIncremental(InputType input,
                                                       Next&& next) {
    for (OutputType element : input) {
      next.ProcessIncremental(static_cast<OutputType>(element));
    }
  }
};
template <typename InputType>
struct TakeImpl {
  using OutputType = InputType;

  size_t max_count;
  size_t count = 0;

  template <typename T, typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE void ProcessIncremental(T&& t, Next&& next) {
    next.ProcessIncremental(std::forward<T>(t));
    ++count;
  }

  ABSL_ATTRIBUTE_ALWAYS_INLINE bool Done() const { return count >= max_count; }
};

template <typename InputType>
struct EnumerateImpl {
  using OutputType =
      EnumeratedValue<decltype(UnwrapReference(std::declval<InputType>()))>;

  size_t index = 0;
  template <typename T, typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE void ProcessIncremental(T&& t, Next&& next) {
    next.ProcessIncremental(
        OutputType{index, UnwrapReference(std::forward<T>(t))});
    ++index;
  }
};

template <typename InputType, typename Accumulated, typename F>
struct AccumulateInPlaceImpl {
  using OutputType = Accumulated;

  Accumulated accumulated;
  F f;

  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE void ProcessIncremental(InputType input,
                                                       Next&&) {
    f(UnwrapReference(accumulated),
      UnwrapReference(static_cast<InputType>(input)));
  }

  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) End(Next&& next) {
    return next.ProcessComplete(std::move(accumulated));
  }
};

template <typename InputType, typename Predicate>
struct AllOfImpl {
  using OutputType = bool&&;

  Predicate predicate;

  bool all_of = true;

  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE void ProcessIncremental(InputType input,
                                                       Next&&) {
    all_of = predicate(UnwrapReference(static_cast<InputType>(input)));
  }

  ABSL_ATTRIBUTE_ALWAYS_INLINE bool Done() const { return !all_of; }

  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) End(Next&& next) {
    return next.ProcessComplete(static_cast<OutputType>(all_of));
  }
};

template <typename InputType, typename Predicate>
struct AnyOfImpl {
  using OutputType = bool;

  Predicate predicate;
  bool any_of = false;

  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE void ProcessIncremental(InputType input,
                                                       Next&&) {
    any_of = predicate(UnwrapReference(static_cast<InputType>(input)));
  }

  ABSL_ATTRIBUTE_ALWAYS_INLINE bool Done() const { return any_of; }
  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) End(Next&& next) {
    return next.ProcessComplete(static_cast<OutputType>(any_of));
  }
};

struct AddressOfFunctor {
  template <typename T>
  ABSL_ATTRIBUTE_ALWAYS_INLINE T* operator()(T& t) const {
    return &t;
  }

  template <typename T>
  ABSL_ATTRIBUTE_ALWAYS_INLINE T* operator()(
      std::reference_wrapper<T> r) const {
    return &r.get();
  }
};

template <typename Container,
          typename = std::void_t<
              decltype(std::declval<Container&>().remove_suffix(0))>>
std::true_type HasRemoveSuffix(Container&);

std::false_type HasRemoveSuffix(...);
}  // namespace internal_htls_range

template <template <typename...> typename Terminator,
          typename... TerminatorParams, typename Range, typename... Combinators>
ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) Apply(
    Range&& range, Combinators&&... combinators) {
  if constexpr (sizeof...(Combinators) == 0) {
    return std::forward<Range>(range);
  } else {
    using Chain = internal_htls_range::CombinatorChain<
        ProcessingStyle::kComplete, decltype(std::forward<Range>(range)),
        std::decay_t<Combinators>...>;
    Chain chain{std::forward<Combinators>(combinators)...};
    Terminator<typename Chain::OutputType, TerminatorParams...> terminator;
    return chain.ProcessComplete(std::forward<Range>(range), terminator);
  }
}
template <typename Range, typename... Combinators>
ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) Apply(
    Range&& range, Combinators&&... combinators) {
  constexpr bool has_and_then =
      (internal_htls_range::HasAndThen<std::decay_t<Combinators>>::value ||
       ...);
  if constexpr (has_and_then) {
    return Apply<internal_htls_range::StatusChainTerminator>(
        std::forward<Range>(range), std::forward<Combinators>(combinators)...);
  } else {
    return Apply<internal_htls_range::ChainTerminator>(
        std::forward<Range>(range), std::forward<Combinators>(combinators)...);
  }
}

template <typename FirstCombinator, typename... Combinators>
ABSL_ATTRIBUTE_ALWAYS_INLINE auto Compose(FirstCombinator&& first,
                                          Combinators&&... combinators) {
  return internal_htls_range::ComposedCombinatorCreator<
      FirstCombinator::kInputProcessingStyle, std::decay_t<FirstCombinator>,
      std::decay_t<Combinators>...>{std::forward<FirstCombinator>(first),
                                    std::forward<Combinators>(combinators)...};
}

template <bool has_and_then, ProcessingStyle input_processing_style,
          ProcessingStyle output_processing_style,
          template <typename...> typename Combinator,
          typename... CombinatorParameters, typename... Ts>
auto MakeCombinator(Ts&&... ts) {
  auto creator =
      [tuple = std::make_tuple(std::forward<Ts>(ts)...)](auto type) mutable {
        return std::apply(
            [](auto&&... ts) {
              return Combinator<TypeIdentityT<decltype(type)>,
                                CombinatorParameters...>{
                  std::forward<decltype(ts)>(ts)...};
            },
            std::move(tuple));
      };
  return internal_htls_range::CombinatorCreatorWrapper<
      input_processing_style, output_processing_style,
      std::decay_t<decltype(creator)>, has_and_then>{creator};
}

template <ProcessingStyle input_processing_style,
          ProcessingStyle output_processing_style,
          template <typename...> typename Combinator,
          typename... CombinatorParameters, typename... Ts>
auto MakeCombinator(Ts&&... ts) {
  return MakeCombinator<false, input_processing_style, output_processing_style,
                        Combinator, CombinatorParameters...>(
      std::forward<Ts>(ts)...);
}

inline auto ToVector() {
  return MakeCombinator<ProcessingStyle::kIncremental,
                        ProcessingStyle::kComplete,
                        internal_htls_range::ToVectorImpl>();
}

template <typename Predicate>
auto Filter(Predicate predicate) {
  return MakeCombinator<ProcessingStyle::kIncremental,
                        ProcessingStyle::kIncremental,
                        internal_htls_range::FilterImpl, Predicate>(
      std::move(predicate));
}

template <typename F>
auto Transform(F f) {
  return MakeCombinator<ProcessingStyle::kComplete, ProcessingStyle::kComplete,
                        internal_htls_range::TransformImpl, F>(std::move(f));
}

template <typename F>
auto TransformEach(F f) {
  return MakeCombinator<ProcessingStyle::kIncremental,
                        ProcessingStyle::kIncremental,
                        internal_htls_range::TransformEachImpl, F>(
      std::move(f));
}

inline auto AndThen() {
  return MakeCombinator<true, ProcessingStyle::kComplete,
                        ProcessingStyle::kComplete,
                        internal_htls_range::AndThenImpl>();
}

inline auto AndThenEach() {
  return MakeCombinator<true, ProcessingStyle::kIncremental,
                        ProcessingStyle::kIncremental,
                        internal_htls_range::AndThenImpl>();
}

template <typename Comparator>
auto Sort(Comparator comparator) {
  auto sort = [comparator =
                   std::move(comparator)](auto&& range) -> decltype(auto) {
    auto unwrapped_comparator = [&](auto& a, auto& b) {
      return comparator(UnwrapReference(a), UnwrapReference(b));
    };
    std::sort(range.begin(), range.end(), unwrapped_comparator);
    return std::forward<decltype(range)>(range);
  };
  return Transform(sort);
}
template <typename Equality>
auto Unique(Equality equality) {
  auto unique = [equality =
                     std::move(equality)](auto&& range) -> decltype(auto) {
    auto unwrapped_equality = [&](auto& a, auto& b) {
      return equality(UnwrapReference(a), UnwrapReference(b));
    };
    auto last = std::unique(range.begin(), range.end(), unwrapped_equality);
    if constexpr (decltype(internal_htls_range::HasRemoveSuffix(
                      range))::value) {
      range.remove_suffix(std::distance(last, range.end()));
    } else {
      range.erase(last, range.end());
    }
    return std::forward<decltype(range)>(range);
  };

  return Transform(unique);
}

template <typename Equal>
auto UniqueEach(Equal equal) {
  return MakeCombinator<ProcessingStyle::kIncremental,
                        ProcessingStyle::kIncremental,
                        internal_htls_range::UniqueEachImpl, Equal>(
      std::move(equal));
}

inline auto Take(size_t count) {
  return MakeCombinator<ProcessingStyle::kIncremental,
                        ProcessingStyle::kIncremental,
                        internal_htls_range::TakeImpl>(count);
}

inline auto Flatten() {
  return MakeCombinator<ProcessingStyle::kIncremental,
                        ProcessingStyle::kIncremental,
                        internal_htls_range::FlattenImpl>();
}

inline auto Enumerate() {
  return MakeCombinator<ProcessingStyle::kIncremental,
                        ProcessingStyle::kIncremental,
                        internal_htls_range::EnumerateImpl>();
}
inline auto Unenumerate() {
  auto un_enumerate = [](auto&& e) -> decltype(auto) {
    return e.ForwardValue();
  };
  return TransformEach((un_enumerate));
}

inline auto RefEach() {
  auto to_ref = [](auto&& r) {
    static_assert(!std::is_rvalue_reference_v<decltype(r)>,
                  "Attempting to call std::ref on a temporary.");
    return std::ref(std::forward<decltype(r)>(r));
  };
  return TransformEach(to_ref);
}

inline auto MoveEach() {
  auto move = [](auto& r) -> decltype(auto) {
    return std::move(UnwrapReference(r));
  };
  return TransformEach(move);
}

inline auto LRefEach() {
  auto lref = [](auto&& r) -> decltype(auto) { return UnwrapReference(r); };
  return TransformEach(lref);
}

inline auto AddressOfEach() {
  return TransformEach(internal_htls_range::AddressOfFunctor{});
}

inline auto DerefEach() {
  auto deref = [](auto&& r) -> decltype(auto) {
    return *UnwrapReference(std::forward<decltype(r)>(r));
  };
  return TransformEach(deref);
}

template <typename Accumulated, typename F>
auto AccumulateInPlace(Accumulated accumulated, F f) {
  return MakeCombinator<
      ProcessingStyle::kIncremental, ProcessingStyle::kComplete,
      internal_htls_range::AccumulateInPlaceImpl, Accumulated, F>(
      std::move(accumulated), std::move(f));
}

template <typename Accumulated, typename F>
auto Accumulate(Accumulated accumulated, F f) {
  auto accumulate = [f = std::move(f)](auto& accumulated, auto&& value) {
    // Prevents f from returning a reference causing self-move.
    auto concrete = [](auto&& value) { return value; };
    accumulated = concrete(
        f(std::move(accumulated), std::forward<decltype(value)>(value)));
  };
  return AccumulateInPlace(std::move(accumulated), std::move(accumulate));
}

// Calls f for each value, and returns the number of values processed.
template <typename F>
auto ForEach(F f) {
  return AccumulateInPlace(
      std::size_t{0}, [f = std::move(f)](size_t& count, auto&& value) mutable {
        ++count;
        f(std::forward<decltype(value)>(value));
      });
}

template <typename Predicate>
auto AnyOf(Predicate predicate) {
  return MakeCombinator<ProcessingStyle::kIncremental,
                        ProcessingStyle::kComplete,
                        internal_htls_range::AnyOfImpl, Predicate>(
      std::move(predicate));
}

template <typename Predicate>
auto AllOf(Predicate predicate) {
  return MakeCombinator<ProcessingStyle::kIncremental,
                        ProcessingStyle::kComplete,
                        internal_htls_range::AllOfImpl, Predicate>(
      std::move(predicate));
}

template <typename Predicate>
auto NoneOf(Predicate predicate) {
  return AllOf(std::not_fn(std::move(predicate)));
}

}  // namespace htls::range

#endif  // THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_RANGE_RANGE_H_
