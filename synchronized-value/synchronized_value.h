// Copyright 2020 Google LLC
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

#ifndef SYNCHRONIZED_VALUE_H_
#define SYNCHRONIZED_VALUE_H_

#include <functional>
#include <memory>
#include <type_traits>
#include <utility>

namespace synchronized_value {
namespace synchronized_value_internal {

template <class, class, class = std::void_t<>>
struct is_inplace_updatable : std::false_type {};

template <class Strat, class T>
struct is_inplace_updatable<
    Strat, T,
    std::void_t<decltype(std::declval<Strat>().update_inplace(
        std::declval<typename Strat::template value_type<T>&>(),
        std::declval<
            std::function<void(typename Strat::template value_type<T>&)>>()))>>
    : std::true_type {};

template <class Strat, class T>
inline constexpr bool is_inplace_updatable_v =
    is_inplace_updatable<Strat, T>::value;

template <class, class, class = std::void_t<>>
struct is_copy_updatable : std::false_type {};

template <class Strat, class T>
struct is_copy_updatable<
    Strat, T,
    std::void_t<decltype(std::declval<Strat>().update_copy(
        std::declval<typename Strat::template value_type<T>&>(),
        std::declval<std::function<typename Strat::template value_type<T>(
            const typename Strat::template value_type<T>&)>>()))>>
    : std::true_type {};

template <class Strat, class T>
inline constexpr bool is_copy_updatable_v = is_copy_updatable<Strat, T>::value;

}  // namespace synchronized_value_internal

class no_lock_strategy {
 public:
  template <typename T>
  using value_type = T;

  template <typename T>
  using view_type = const T*;

  template <typename T>
  const view_type<T> make_view(const value_type<T>& value) const {
    return &value;
  }

  template <typename T, typename Mutator>
  void update_inplace(value_type<T>& value, Mutator&& mutate) {
    mutate(value);
  }
};

template <typename T, typename LockStrategy>
class synchronized_value {
  using lock_strategy = LockStrategy;

 public:
  using view_type = typename lock_strategy::template view_type<T>;
  using value_type = T;

  static constexpr bool is_inplace_updatable =
      synchronized_value_internal::is_inplace_updatable_v<lock_strategy, T>;
  static constexpr bool is_copy_updatable =
      synchronized_value_internal::is_copy_updatable_v<lock_strategy, T>;
  static_assert(is_copy_updatable || is_inplace_updatable,
                "Strategy must be copy updatable or inplace updatable");

  // Constructs a synchronized value from t.
  explicit synchronized_value(T value) : value_(std::move(value)) {}

  template <typename... Args>
  explicit synchronized_value(Args&&... args)
      : value_(std::forward<Args>(args)...) {}

  // Make synchronized_value non-copyable and non-movable
  synchronized_value(const synchronized_value&) = delete;
  synchronized_value& operator=(const synchronized_value&) = delete;
  synchronized_value(synchronized_value&&) = delete;
  synchronized_value& operator=(synchronized_value&&) = delete;

  // Gets a proxy that keeps a const reference to the object and locks it with
  // RAII idiom. It allows using const methods on the object. Note: there is a
  // specialization for std::unique_ptr, you should treat a view as pointer
  // (not as a pointer on a pointer) in that case.
  view_type get_view() const { return strategy_.template make_view<T>(value_); }

  // Takes a lambda that takes a const reference to the value and returns what
  // lambda returns. Note: there is a specialization for std::unique_ptr, you
  // should take const reference not to std::unique_ptr<Type>, but to
  // Type (i. e. const Type&).
  template <typename Reader>
  auto read(Reader&& reader) const {
    return reader(*get_view());
  }

  // Takes a lambda that takes a const reference to the object, updates it and
  // then the object is replaced with the one returned from lambda.
  template <typename Updater>
  void update_copy(Updater&& updater) {
    static_assert(
        std::is_convertible_v<Updater&&,
                              std::function<value_type(const value_type&)>>,
        "Invalid func signature: updater must be of signature (const "
        "value_type&) -> value_type");
    if constexpr (is_copy_updatable) {
      strategy_.update_copy(value_, std::forward<Updater>(updater));
    } else {
      update_inplace([&updater](T& value) { value = updater(value); });
    }
  }

  // Takes a lambda that takes a pointer to the object and updates it in place.
  template <typename Updater>
  void update_inplace(Updater&& updater) {
    static_assert(
        std::is_convertible_v<Updater&&, std::function<void(value_type&)>>,
        "Invalid func signature: updater must be of signature (value_type&) -> "
        "void");
    if constexpr (is_inplace_updatable) {
      strategy_.update_inplace(value_, std::forward<Updater>(updater));
    } else {
      update_copy([&updater](const T& value) {
        auto new_value = value;
        updater(new_value);
        return new_value;
      });
    }
  }

 private:
  lock_strategy strategy_;
  typename lock_strategy::template value_type<T> value_;
};

}  // namespace synchronized_value

#endif  // SYNCHRONIZED_VALUE_H_
