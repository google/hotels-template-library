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

#include <absl/base/attributes.h>
#include "range/range_core.h"

namespace htls::range {

// Filters based on a predicate. Only outputs values for which the predicate
// returns true.
// In: Incremental
// Out: Incremental
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
// In: Incremental
// Out: Incremental
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
auto TransformComplete(F f);

// Converts the output to a vector. The type of the vector is deduced.
// In: Incremental
// Out: Complete
inline auto ToVector();

// Sorts the output.
// In: Complete
// Out: Complete
// Note: Sort will sort the range in-place.
template <typename Comparator = std::less<>>
auto Sort(Comparator comparator = {});

// For sorted input, filters out the repeating elements.
// In: Incremental
// Out: Incremental
template <typename Equality = std::equal_to<>>
auto Unique(Equality equality = {});

// For sorted input, filters out the repeating elements.
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
// In: Incremental
// Out: Incremental
inline auto Enumerate();

// Drops the index and returns values
// In: Incremental
// Out: Incremental
inline auto Unenumerate();

// Calls f for each value and returns the number of times f was called.
// In: Incremental
// Out: Complete
template <typename F>
auto ForEach(F f);

// Accumulates the values.
// In: Incremental
// Out: Complete
template <typename Accumulated, typename F>
auto Accumulate(Accumulated accumulated, F f);

// Tests if any element fulfills a condition
// In: Incremental
// Out: Complete
template <typename Predicate>
auto AnyOf(Predicate predicate);

// Tests if all the elements fulfills a condition
// In: Incremental
// Out: Complete
template <typename Predicate>
auto AllOf(Predicate predicate);

// Tests if none of the elements fulfills a condition
// In: Incremental
// Out: Complete
template <typename Predicate>
auto NoneOf(Predicate predicate);


namespace internal_htls_range {

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
struct TransformImpl {
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

template <typename InputType, typename F>
struct TransformCompleteImpl {
  using InvokeResult =
      std::invoke_result_t<F&, decltype(std::declval<InputType>())>;
  using OutputType =
      decltype(std::forward<InvokeResult>(std::declval<InvokeResult>()));
  F f;

  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) ProcessComplete(InputType input,
                                                              Next&& next) {
    return next.ProcessComplete(
        static_cast<OutputType>(f(UnwrapReference(input))));
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
  ABSL_ATTRIBUTE_ALWAYS_INLINE void ProcessIncremental(InputType input,
                                                       Next&& next) {
    if constexpr (std::is_lvalue_reference_v<InputType>) {
      const bool has_value = test_element;
      const bool is_equal = has_value && equal(UnwrapReference(input),
                                               UnwrapReference(*test_element));

      if (!is_equal) {
        if (has_value) {
          next.ProcessIncremental(*test_element);
        }
        test_element = &input;
      }
    } else {
      const bool has_value = test_element.has_value();
      const bool is_equal = has_value && equal(UnwrapReference(input),
                                               UnwrapReference(*test_element));
      if (!is_equal) {
        if (has_value) {
          next.ProcessIncremental(std::move(*test_element));
        }
        test_element = static_cast<InputType>(input);
      }
    }
  }

  template <typename Next>
  ABSL_ATTRIBUTE_ALWAYS_INLINE decltype(auto) End(Next&& next) {
    if constexpr (std::is_lvalue_reference_v<InputType>) {
      if (test_element) {
        next.ProcessIncremental(*test_element);
      }
    } else {
      if (test_element.has_value()) {
        next.ProcessIncremental(*std::move(test_element));
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
  using OutputType = bool&&;

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
    auto unwrapped_comparator = [&](auto& a, auto& b) {
      return comparator(UnwrapReference(a), UnwrapReference(b));
    };
    std::sort(range.begin(), range.end(), unwrapped_comparator);
    return std::forward<decltype(range)>(range);
  };
  return TransformComplete(sort);
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

  return TransformComplete(unique);
}

inline auto Take(size_t count) {
  return MakeCombinator<ProcessingStyle::kIncremental,
                        ProcessingStyle::kIncremental,
                        internal_htls_range::TakeImpl>(count);
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
inline auto Unenumerate() {
  auto un_enumerate = [](auto&& e) -> decltype(auto) {
    return e.ForwardValue();
  };
  return Transform((un_enumerate));
}

inline auto Ref() {
  auto to_ref = [](auto&& r) {
    static_assert(!std::is_rvalue_reference_v<decltype(r)>,
                  "Attempting to call std::ref on a temporary.");
    return std::ref(std::forward<decltype(r)>(r));
  };
  return Transform(to_ref);
}

inline auto Move() {
  auto move = [](auto& r) -> decltype(auto) {
    return std::move(UnwrapReference(r));
  };
  return Transform(move);
}

inline auto LRef() {
  auto lref = [](auto&& r) -> decltype(auto) { return UnwrapReference(r); };
  return Transform(lref);
}

inline auto AddressOf() {
  return Transform(internal_htls_range::AddressOfFunctor{});
}

inline auto Deref() {
  auto deref = [](auto&& r) -> decltype(auto) {
    return *UnwrapReference(std::forward<decltype(r)>(r));
  };
  return Transform(deref);
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

#endif  // THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_RANGE_RANGE_COMBINATORS_H_
