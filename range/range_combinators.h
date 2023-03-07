// Copyright 2023 Google LLC
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

// IWYU pragma: private, include "third_party/hotels_template_library/range/range.h"

#ifndef THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_RANGE_RANGE_COMBINATORS_H_
#define THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_RANGE_RANGE_COMBINATORS_H_

#include <algorithm>
#include <cstddef>
#include <functional>
#include <iterator>
#include <optional>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#include "range/range_core.h"

namespace htls::range {

// Filters based on a predicate. Only outputs values for which the predicate
// returns true.
// In: Incremental
// Out: Incremental
/* Example: ********************************************************************
std::vector<int> input_range = {1, 2, 3, 4};
std::vector<int> result = Apply(input_range,                               //
                                Filter([](int i) { return i % 2 == 1; }),  //
                                ToVector()                                 //
);
// result is {1, 3}
// ****************************************************************************/
template <typename Predicate>
auto Filter(Predicate predicate);

// Transforms a range. It outputs the result of f.
// In: Incremental
// Out: Incremental
/* Example: ********************************************************************
std::vector<int> input_range = {1, 2, 3, 4};
std::vector<double> result =
    Apply(input_range,                                                    //
          Transform([](int i) { return static_cast<double>(i) + 0.5; }),  //
          ToVector()                                                      //
    );
// result is {1.5, 2.5, 3.5, 4.5}
// ****************************************************************************/
//
// You can pass an invocable like a pointer to member
// Transform(&MyStruct::int_member_variable)
template <typename F>
auto TransformComplete(F f);

// Collects the inputs into a vector. The value type of the vector is deduced.
// In: Incremental
// Out: Complete
inline auto ToVector();

// Converts the input to a collection. The value type of the collection is
// deduced. This takes the collection as a template template parameter,
// followed by additional type parameters that are passed to the template.
//
// Appender: A function that takes a mutable refernce to the collection being
//   built, and a value that it should add to the collection.
// Args: Forwarded to the collection's constructor.
//
// In: Incremental
// Out: Complete
/* Example: ********************************************************************
std::set<int> result =
    Apply(std::vector{1, 2, 2, 3},
          ToCollection<std::set>([](auto& set, auto&& value) {
            set.insert(std::forward<decltype(value)>(value));
          }));
// result is {1, 2, 3}
// ****************************************************************************/
template <template <typename...> typename Collection,
          typename... CollectionParams, typename Appender, typename... Args>
auto ToCollection(Appender appender, Args&&... args);

// Yields the first incremental input as an std::optional complete value.
// The input will be converted to a value then moved out.
// If the range is empty, an empty std::optional will be returned
/* Example: ********************************************************************
std::vector v{1, 2, 3, 4};
std::optional<int> result = Apply(v, Front());                   // *result == 1

std::vector empty_v;
std::optional<int> result = Apply(empty_v, Front()); // result == std::nullopt

// ****************************************************************************/
// In: Incremental
// Out: Complete
inline auto Front();

// Sorts the input.
// In: Complete
// Out: Complete
// Note: Sort will sort the range in-place.
template <typename Comparator = std::less<>>
auto Sort(Comparator comparator = {});

// Filters out repeating elements. Normally used with sorted input.
// In: Complete
// Out: Complete
template <typename Equality = std::equal_to<>>
auto Unique(Equality equality = {});

// Filters out repeating elements. Normally used with sorted input.
// In: Incremental
// Out: Incremental
template <typename Equality = std::equal_to<>>
auto FilterDuplicates(Equality equality = {});

// Flattens a range of ranges into a single range.
// In: Incremental
// Out: Incremental
inline auto Flatten();

// Takes count elements from the range.
// In: Incremental
// Out: Incremental
inline auto Take(size_t count);

// Transforms to std::reference_wrapper
// In: Incremental
// Out: Incremental
inline auto Ref();

// Transforms to r-value reference.
// In: Incremental
// Out: Incremental
inline auto Move();

// Transforms to l-value reference.
// In: Incremental
// Out: Incremental
inline auto LRef();

// Takes the address of each input.
// In: Incremental
// Out: Incremental
inline auto AddressOf();

// Dereferences each input.
// In: Incremental
// Out: Incremental
inline auto Deref();

// Structure to hold enumerated values.
template <typename R>
struct EnumeratedValue {
  size_t index = 0;
  R value{};

  decltype(auto) ForwardValue() { return static_cast<R>(value); }
};
template <typename R>
EnumeratedValue(size_t, R) -> EnumeratedValue<R>;

// Transforms values into `EnumeratedValue`s with an index.
// In: Incremental
// Out: Incremental
inline auto Enumerate();

// Zips with other range(s).
// In: Incremental
// Out: Incremental
template <typename... Ranges>
auto ZipWith(Ranges&&... ranges);

// Transforms `EnumeratedValue`s into just the values.
// In: Incremental
// Out: Incremental
inline auto Unenumerate();

// Gets the element of Tuple.
// In: Incremental
// Out: Incremental
template <size_t I>
decltype(auto) Get();

// Gets the element of Tuple.
// In: Incremental
// Out: Incremental
template <typename T>
decltype(auto) Get();

// Calls the `.get()` method on the input.
// In: Incremental
// Out: Incremental
decltype(auto) Get();

// Calls f for each value and returns the number of times f was called.
// In: Incremental
// Out: Complete
template <typename F>
auto ForEach(F f);

// Accumulates the inputs.
//
//  init: The initial accumulated value to pass to f.
//  f: An invocable called with the accumulated value and the next incremental
//    input which should return the new accumulated value.
//
// In: Incremental
// Out: Complete
template <typename Init, typename F>
auto Accumulate(Init init, F f);

// Accumulates the inputs in place and returns the accumulated value.
//
//  init: The initial accumulated value to pass to f.
//  f: An invocable called with a reference to the accumulated value and the
//    next incremental input which should update the accumulated value in place.
//
// In: Incremental
// Out: Complete
template <typename Init, typename F>
auto AccumulateInPlace(Init init, F f);

// Tests if any inputs fulfills a condition.
// In: Incremental
// Out: Complete
template <typename Predicate>
auto AnyOf(Predicate predicate);

// Tests if all the inputs fulfills a condition.
// In: Incremental
// Out: Complete
template <typename Predicate>
auto AllOf(Predicate predicate);

// Tests if none of the inputs fulfills a condition.
// In: Incremental
// Out: Complete
template <typename Predicate>
auto NoneOf(Predicate predicate);

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

namespace internal_htls_range {

// Since CombinatorImpls have to take all type template parameters, we have to
// wrap the collection template, in a type.
template <template <typename...> typename Collection,
          typename... CollectionParams>
struct CollectionTemplate {
  template <typename T>
  using type = Collection<T, CollectionParams...>;
};

// Implements converting incremental input to a generic collection.
template <typename InputType, typename CollectionTemplateParam,
          typename Appender>
struct ToCollectionImpl {
  using Collection =
      typename CollectionTemplateParam::template type<std::decay_t<InputType>>;
  using OutputType = Collection&&;

  Collection collection;
  Appender appender;

  template <typename... Args>
  explicit ToCollectionImpl(Appender appender, Args&&... args)
      : appender(std::move(appender)),
        collection(std::forward<Args>(args)...) {}
  template <typename Next>
  void ProcessIncremental(InputType input,
                                                       Next&&) {
    appender(collection, static_cast<InputType>(input));
  }

  template <typename Next>
  decltype(auto) End(Next&& next) {
    return next.ProcessComplete(std::move(collection));
  }
};

template <typename InputType, typename Predicate>
struct FilterImpl {
  using OutputType = InputType;

  Predicate f;

  template <typename Next>
  void ProcessIncremental(InputType input,
                                                       Next&& next) {
    if (f(input)) {
      next.ProcessIncremental(static_cast<InputType>(input));
    }
  }
};

template <typename InputType, typename F>
struct TransformImpl {
  using OutputType =
      std::invoke_result_t<F&, decltype(std::declval<InputType>())>&&;
  F f;

  template <typename Next>
  void ProcessIncremental(InputType input,
                                                       Next&& next) {
    next.ProcessIncremental(std::invoke(f, static_cast<InputType>(input)));
  }
};

template <typename InputType, typename F>
struct TransformCompleteImpl {
  using InvokeResult =
      std::invoke_result_t<F&, decltype(std::declval<InputType>())>;
  using OutputType =
      decltype(std::forward<InvokeResult>(std::declval<InvokeResult>()));
  F f;

  template <typename Next>
  decltype(auto) ProcessComplete(InputType input,
                                                              Next&& next) {
    return next.ProcessComplete(f(static_cast<InputType>(input)));
  }
};

template <typename InputType, typename Equal>
struct FilterDuplicatesImpl {
  using OutputType = InputType;
  Equal equal;
  std::conditional_t<std::is_lvalue_reference_v<InputType>,
                     std::remove_reference_t<InputType>*,
                     std::optional<std::decay_t<InputType>>>
      test_element;
  template <typename Next>
  void ProcessIncremental(InputType input,
                                                       Next&& next) {
    const bool has_value(test_element);
    const bool is_equal = has_value && equal(input, *test_element);
    if (!is_equal) {
      if (has_value) {
        next.ProcessIncremental(*std::move(test_element));
      }
      if constexpr (std::is_lvalue_reference_v<InputType>) {
        test_element = &input;
      } else {
        test_element.emplace(static_cast<InputType>(input));
      }
    }
  }

  template <typename Next>
  decltype(auto) End(Next&& next) {
    if (test_element) {
      next.ProcessIncremental(*std::move(test_element));
    }
    return next.End();
  }
};

template <typename InputType>
struct FlattenImpl {
  using OutputType = decltype(*std::begin(std::declval<InputType>()));

  template <typename Next>
  void ProcessIncremental(InputType input,
                                                       Next&& next) {
    for (OutputType element : input) {
      next.ProcessIncremental(element);
    }
  }
};
template <typename InputType>
struct TakeImpl {
  using OutputType = InputType;

  size_t max_count;
  size_t count = 0;

  template <typename Next>
  void ProcessIncremental(InputType input,
                                                       Next&& next) {
    next.ProcessIncremental(static_cast<InputType>(input));
    ++count;
  }

  bool Done() const { return count >= max_count; }
};

template <typename InputType>
struct FrontImpl {
  using OutputType = std::optional<std::decay_t<InputType>>&&;

  std::optional<std::decay_t<InputType>> element;

  template <typename T, typename Next>
  void ProcessIncremental(T&& t, Next&& next) {
    element = std::forward<T>(t);
  }

  bool Done() const { return element.has_value(); }

  template <typename Next>
  decltype(auto) End(Next&& next) {
      return next.ProcessComplete(std::move(element));
  }
};

template <typename InputType>
struct EnumerateImpl {
  using OutputType = EnumeratedValue<decltype(std::declval<InputType>())>&&;

  size_t index = 0;
  template <typename T, typename Next>
  void ProcessIncremental(T&& t, Next&& next) {
    next.ProcessIncremental(OutputType{index, std::forward<T>(t)});
    ++index;
  }
};

template <typename InputType, typename Iterator, typename EndIterator>
struct ZipWithImpl {
  template <typename T>
  static decltype(auto) WrapInTuple(T&& t) {
    if constexpr (decltype(internal_htls_range::IsTuple(t))::value) {
      return std::forward<T>(t);
    } else {
      return std::forward_as_tuple(std::forward<T>(t));
    }
  }
  using OutputType =
      decltype(std::tuple_cat(WrapInTuple(std::declval<InputType>()),
                              WrapInTuple(*std::declval<Iterator>())))&&;

  Iterator iterator{};
  EndIterator end_iterator{};
  template <typename T, typename Next>
  void ProcessIncremental(T&& t, Next&& next) {
    next.ProcessIncremental(std::tuple_cat(WrapInTuple(std::forward<T>(t)),
                                           WrapInTuple(*iterator)));
    ++iterator;
  }

  bool Done() const { return iterator == end_iterator; }
};

template <typename InputType, typename Init, typename F>
struct AccumulateInPlaceImpl {
  static_assert(!std::is_lvalue_reference_v<Init> &&
                !std::is_rvalue_reference_v<Init>);
  using OutputType = Init&&;

  Init accumulated;
  F f;

  template <typename Next>
  void ProcessIncremental(InputType input,
                                                       Next&&) {
    f(accumulated, static_cast<InputType>(input));
  }

  template <typename Next>
  decltype(auto) End(Next&& next) {
    return next.ProcessComplete(std::move(accumulated));
  }
};

struct AddressOfFunctor {
  template <typename T>
  T* operator()(T& t) const {
    return &t;
  }
};

template <typename Container,
          typename = std::void_t<
              decltype(std::declval<Container&>().remove_suffix(0))>>
std::true_type HasRemoveSuffix(Container&);

std::false_type HasRemoveSuffix(...);

}  // namespace internal_htls_range

template <template <typename...> typename Collection,
          typename... CollectionParams, typename Appender, typename... Args>
auto ToCollection(Appender appender, Args&&... args) {
  return MakeCombinator<
      ProcessingStyle::kIncremental, ProcessingStyle::kComplete,
      internal_htls_range::ToCollectionImpl,
      internal_htls_range::CollectionTemplate<Collection, CollectionParams...>,
      Appender>(std::move(appender), std::forward<Args>(args)...);
}

inline auto ToVector() {
  // It is safe to pass std::vector as template-template parameter since
  // implementators are not allowed to add additional template parameters.
  // https://www.open-std.org/jtc1/sc22/wg21/docs/lwg-closed.html#94
  return ToCollection<std::vector>([](auto& vector, auto&& value) {
    vector.push_back(std::forward<decltype(value)>(value));
  });
}

template <typename Predicate>
auto Filter(Predicate predicate) {
  return MakeCombinator<ProcessingStyle::kIncremental,
                        ProcessingStyle::kIncremental,
                        internal_htls_range::FilterImpl, Predicate>(
      std::move(predicate));
}

template <typename F>
auto TransformComplete(F f) {
  return MakeCombinator<ProcessingStyle::kComplete, ProcessingStyle::kComplete,
                        internal_htls_range::TransformCompleteImpl, F>(
      std::move(f));
}

template <typename F>
auto Transform(F f) {
  return MakeCombinator<ProcessingStyle::kIncremental,
                        ProcessingStyle::kIncremental,
                        internal_htls_range::TransformImpl, F>(std::move(f));
}

template <typename Comparator>
auto Sort(Comparator comparator) {
  auto sort = [comparator =
                   std::move(comparator)](auto&& range) -> decltype(auto) {
    std::sort(range.begin(), range.end(), comparator);
    return std::forward<decltype(range)>(range);
  };
  return TransformComplete(sort);
}
template <typename Equality>
auto Unique(Equality equality) {
  auto unique = [equality =
                     std::move(equality)](auto&& range) -> decltype(auto) {
    auto last = std::unique(range.begin(), range.end(), equality);
    if constexpr (decltype(internal_htls_range::HasRemoveSuffix(
                      range))::value) {
      range.remove_suffix(std::distance(last, range.end()));
    } else {
      range.erase(last, range.end());
    }
    return std::forward<decltype(range)>(range);
  };

  return TransformComplete(unique);
}

inline auto Take(size_t count) {
  return MakeCombinator<ProcessingStyle::kIncremental,
                        ProcessingStyle::kIncremental,
                        internal_htls_range::TakeImpl>(count);
}

inline auto Front() {
  return MakeCombinator<ProcessingStyle::kIncremental,
                        ProcessingStyle::kComplete,
                        internal_htls_range::FrontImpl>();
}

template <typename Equal>
auto FilterDuplicates(Equal equal) {
  return MakeCombinator<ProcessingStyle::kIncremental,
                        ProcessingStyle::kIncremental,
                        internal_htls_range::FilterDuplicatesImpl, Equal>(
      std::move(equal));
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

template <typename... Ranges>
auto ZipWith(Ranges&&... ranges) {
  auto make_combinator = [](auto&& range) {
    return MakeCombinator<ProcessingStyle::kIncremental,
                          ProcessingStyle::kIncremental,
                          internal_htls_range::ZipWithImpl,
                          decltype(range.begin()), decltype(range.end())>(
        range.begin(), range.end());
  };

  return Compose(make_combinator(std::forward<Ranges>(ranges))...);
}

inline auto Unenumerate() {
  auto un_enumerate = [](auto&& e) -> decltype(auto) {
    return e.ForwardValue();
  };
  return Transform(un_enumerate);
}

template <size_t I>
decltype(auto) Get() {
  return Transform([](auto&& tuple) -> decltype(auto) {
    using std::get;
    return get<I>(std::forward<decltype(tuple)>(tuple));
  });
}

template <typename T>
decltype(auto) Get() {
  return Transform([](auto&& tuple) -> decltype(auto) {
    using std::get;
    return get<T>(std::forward<decltype(tuple)>(tuple));
  });
}

inline decltype(auto) Get() {
  auto get = [](auto&& r) -> decltype(auto) {
    return std::forward<decltype(r)>(r).get();
  };
  return Transform(get);
}

inline auto Ref() {
  auto to_ref = [](auto&& r) { return std::ref(std::forward<decltype(r)>(r)); };
  return Transform(to_ref);
}

inline auto Move() {
  auto move = [](auto& r) -> decltype(auto) { return std::move(r); };
  return Transform(move);
}

inline auto LRef() {
  auto lref = [](auto&& r) -> decltype(auto) {
    return static_cast<decltype(r)&>(r);
  };
  return Transform(lref);
}

inline auto AddressOf() {
  return Transform(internal_htls_range::AddressOfFunctor{});
}

inline auto Deref() {
  auto deref = [](auto&& r) -> decltype(auto) {
    return *std::forward<decltype(r)>(r);
  };
  return Transform(deref);
}

template <typename Init, typename F>
auto AccumulateInPlace(Init init, F f) {
  return MakeCombinator<ProcessingStyle::kIncremental,
                        ProcessingStyle::kComplete,
                        internal_htls_range::AccumulateInPlaceImpl, Init, F>(
      std::move(init), std::move(f));
}

template <typename Init, typename F>
auto Accumulate(Init init, F f) {
  auto accumulate = [f = std::move(f)](auto& accumulated, auto&& value) {
    // Prevents f from returning a reference causing self-move.
    auto concrete = [](auto&& value) { return value; };
    accumulated = concrete(
        f(std::move(accumulated), std::forward<decltype(value)>(value)));
  };
  return AccumulateInPlace(std::move(init), std::move(accumulate));
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
  return Compose(Filter(std::move(predicate)), Take(1),
                 Accumulate(false, [](auto&&...) { return true; }));
}
template <typename Predicate>
auto NoneOf(Predicate predicate) {
  return Compose(AnyOf(std::move(predicate)),
                 TransformComplete(+[](bool b) { return !b; }));
}
template <typename Predicate>
auto AllOf(Predicate predicate) {
  return NoneOf(std::not_fn(std::move(predicate)));
}

}  // namespace htls::range

#endif  // THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_RANGE_RANGE_COMBINATORS_H_
