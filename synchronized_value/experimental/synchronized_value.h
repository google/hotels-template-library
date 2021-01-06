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

#include <condition_variable>
#include <functional>
#include <memory>
#include <mutex>
#include <optional>
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

template <typename Pair, typename Match>
struct is_pair_left : std::false_type {};

template <typename First, typename Second>
struct is_pair_left<std::pair<First, Second>, First> : std::true_type {};

template <typename Pair, typename Match>
inline constexpr bool is_pair_left_v = is_pair_left<Pair, Match>::value;

struct discard {};

template <typename T>
using t_or_discard = std::conditional_t<std::is_void_v<T>, discard, T>;
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

  template <typename Updater>
  void update_copy_void(Updater&& updater) {
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

  template <typename Updater>
  void update_inplace_void(Updater&& updater) {
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

 public:
  using view_type = typename lock_strategy::template view_type<T>;
  using value_type = T;

  static constexpr bool is_inplace_updatable =
      synchronized_value_internal::is_inplace_updatable_v<lock_strategy, T>;
  static constexpr bool is_copy_updatable =
      synchronized_value_internal::is_copy_updatable_v<lock_strategy, T>;
  static_assert(is_copy_updatable || is_inplace_updatable,
                "Strategy must be copy updatable or inplace updatable");

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

  template <typename Predicate>
  view_type get_view_when(Predicate&& predicate) const {
    std::optional<view_type> view_opt;
    std::unique_lock lock{condition_mu_};
    condition_.wait(lock, [&]() {
      view_opt.emplace(get_view());
      if (!predicate(**view_opt)) view_opt.reset();
      return static_cast<bool>(view_opt);
    });
    return {std::move(view_opt.value())};
  }

  // Takes a lambda that takes a const reference to the value and returns what
  // lambda returns. Note: there is a specialization for std::unique_ptr, you
  // should take const reference not to std::unique_ptr<Type>, but to
  // Type (i. e. const Type&).
  template <typename Reader>
  auto read(Reader&& reader) const {
    return reader(*get_view());
  }

  template <typename Reader, typename Predicate>
  auto read_when(Reader&& reader, Predicate&& predicate) {
    using return_type = decltype(read(std::move(reader)));
    std::optional<synchronized_value_internal::t_or_discard<return_type>>
        result;

    std::unique_lock lock{condition_mu_};
    condition_.wait(lock, [&]() {
      auto view = get_view();
      if (!predicate(*view)) return false;
      if constexpr (std::is_void_v<return_type>) {
        reader(*view);
        result.emplace();
      } else
        result = reader(*view);
      return static_cast<bool>(result);
    });
    if constexpr (!std::is_void_v<return_type>)
      return return_type{std::move(result.value())};
  }

  // Takes a lambda that takes a const reference to the object, updates it and
  // then the object is replaced with the one returned from lambda.
  template <typename Updater, bool should_notify = true>
  auto update_copy(Updater&& updater) {
    using return_type = decltype(updater(std::declval<const T&>()));
    constexpr bool returns_t = std::is_same_v<return_type, T>;
    constexpr bool returns_paired_t =
        synchronized_value_internal::is_pair_left_v<return_type, T>;
    static_assert(returns_t || returns_paired_t,
                  "Invalid func signature: updater must be of signature (const "
                  "value_type&) -> value_type or of signature (const "
                  "value_type&) -> [value_type, <result_type>]");

    if constexpr (returns_t) {
      update_copy_void(std::move(updater));
      if constexpr (should_notify) condition_.notify_all();
    } else {
      typename return_type::second_type result;
      update_copy_void([&](const T& value) {
        T new_val;
        std::tie(new_val, result) = updater(value);
        return new_val;
      });
      if constexpr (should_notify) condition_.notify_all();
      return result;
    }
  }

  template <typename Updater, typename Predicate>
  auto update_copy_when(Updater&& updater, Predicate&& predicate) {
    std::unique_lock lock{condition_mu_};
    using return_type = decltype(update_copy(std::move(updater)));
    constexpr bool returns_void = std::is_void_v<return_type>;
    std::optional<synchronized_value_internal::t_or_discard<return_type>>
        result;
    condition_.wait(lock, [&]() {
      if (!predicate(*get_view())) return false;
      auto modifier = [&](const T& val) {
        if (!predicate(val)) return std::make_pair(val, false);
        if constexpr (returns_void) {
          result.emplace();
          return std::make_pair(updater(val), true);
        } else {
          auto [new_val, return_val] = updater(val);
          result.emplace(std::move(return_val));
          return std::make_pair(std::move(new_val), true);
        }
      };
      return update_copy<decltype(modifier), false>(std::move(modifier));
    });
    condition_.notify_all();
    if constexpr (!returns_void) return return_type{std::move(result.value())};
  }

  // Takes a lambda that takes a pointer to the object and updates it in place.
  template <typename Updater, bool should_notify = true>
  auto update_inplace(Updater&& updater) {
    using return_type = decltype(updater(std::declval<T&>()));
    if constexpr (std::is_void_v<return_type>) {
      update_inplace_void(std::move(updater));
      if constexpr (should_notify) condition_.notify_all();
    } else {
      return_type result;
      update_inplace_void([&](T& value) { result = updater(value); });
      if constexpr (should_notify) condition_.notify_all();
      return result;
    }
  }

  template <typename Updater, typename Predicate>
  auto update_inplace_when(Updater&& updater, Predicate&& predicate) {
    std::unique_lock lock{condition_mu_};
    using return_type = decltype(updater(std::declval<T&>()));
    constexpr bool returns_void = std::is_void_v<return_type>;
    synchronized_value_internal::t_or_discard<return_type> result;
    condition_.wait(lock, [&]() {
      if (!predicate(*get_view())) return false;
      auto modifier = [&](T& val) {
        if (!predicate(val)) return false;
        if constexpr (returns_void)
          updater(val);
        else
          result = updater(val);
        return true;
      };
      return update_inplace<decltype(modifier), false>(std::move(modifier));
    });
    condition_.notify_all();
    if constexpr (!returns_void) return result;
  }

 private:
  lock_strategy strategy_;
  typename lock_strategy::template value_type<T> value_;

  mutable std::mutex condition_mu_;
  mutable std::condition_variable condition_;
};

}  // namespace synchronized_value

#endif  // SYNCHRONIZED_VALUE_H_
