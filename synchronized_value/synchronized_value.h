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
#include <tuple>
#include <type_traits>
#include <utility>

#include <absl/synchronization/mutex.h>

namespace htls {

enum class ShouldLockWrites : bool { kLockWrites, kLocklessWrites };

namespace synchronized_value_internal {

template <class, class, class = std::void_t<>>
struct is_inplace_updatable : std::false_type {};

template <class Strategy, class T>
struct is_inplace_updatable<
    Strategy, T,
    std::void_t<decltype(Strategy::UpdateInplace(
        std::declval<absl::Mutex&>(),
        std::declval<typename Strategy::template ValueType<T>&>(),
        std::declval<std::function<void(
            typename Strategy::template ValueType<T>&)>>()))>>
    : std::true_type {};

template <class Strategy, class T>
inline constexpr bool is_inplace_updatable_v =
    is_inplace_updatable<Strategy, T>::value;

template <class, class, class = std::void_t<>>
struct is_copy_updatable : std::false_type {};

template <class Strategy, class T>
struct is_copy_updatable<
    Strategy, T,
    std::void_t<decltype(Strategy::UpdateCopy(
        std::declval<absl::Mutex&>(),
        std::declval<typename Strategy::template ValueType<T>&>(),
        std::declval<std::function<typename Strategy::template ValueType<T>(
            const typename Strategy::template ValueType<T>&)>>()))>>
    : std::true_type {};

template <class Strategy, class T>
inline constexpr bool is_copy_updatable_v =
    is_copy_updatable<Strategy, T>::value;

template <class Strategy, class = void>
struct should_lock_writes : std::true_type {};
template <class Strategy>
struct should_lock_writes<Strategy,
                          std::void_t<decltype(Strategy::kShouldLockWrites)>> {
  static constexpr bool value =
      Strategy::kShouldLockWrites == htls::ShouldLockWrites::kLockWrites;
};
template <class Strategy>
inline constexpr bool should_lock_writes_v =
    should_lock_writes<Strategy>::value;

}  // namespace synchronized_value_internal

// Defines a movable lock.

enum class MovableLockType { kLock, kReader, kWriter, kNoLock };

template <MovableLockType LockType>
class MovableLock {
 public:
  explicit MovableLock(absl::Mutex& mutex) : mutex_(&mutex) {
    if constexpr (LockType == MovableLockType::kLock) {
      mutex_->Lock();
    } else if constexpr (LockType == MovableLockType::kReader) {
      mutex_->ReaderLock();
    } else if constexpr (LockType == MovableLockType::kWriter) {
      mutex_->WriterLock();
    } else {
      // This static_assert is the same condition as the above if condition so
      // if we reach this block, the static_assert will fail.
      static_assert(LockType == MovableLockType::kLock,
                    "Should be exhaustive if-else chain.");
    }
  }

  explicit operator bool() const { return mutex_; }

  template <typename Predicate>
  MovableLock(absl::Mutex& mutex, const Predicate& predicate) : mutex_(&mutex) {
    absl::Condition condition(
        +[](const Predicate* predicate) { return (*predicate)(); }, &predicate);
    if constexpr (LockType == MovableLockType::kLock) {
      mutex_->LockWhen(condition);
    } else if constexpr (LockType == MovableLockType::kReader) {
      mutex_->ReaderLockWhen(condition);
    } else if constexpr (LockType == MovableLockType::kWriter) {
      mutex_->WriterLockWhen(condition);
    } else {
      // This static_assert is the same condition as the above if condition so
      // if we reach this block, the static_assert will fail.
      static_assert(LockType == MovableLockType::kLock,
                    "Should be exhaustive if-else chain.");
    }
  }

  MovableLock() = default;

  MovableLock(const MovableLock&) = delete;
  MovableLock& operator=(const MovableLock&) = delete;

  MovableLock(MovableLock&& other)
      : mutex_(std::exchange(other.mutex_, nullptr)) {}
  MovableLock& operator=(MovableLock&& other) {
    mutex_ = std::exchange(other.mutex_, nullptr);
    return *this;
  }

  ~MovableLock() {
    if (!mutex_) return;
    if constexpr (LockType == MovableLockType::kLock) {
      mutex_->Unlock();
    } else if constexpr (LockType == MovableLockType::kReader) {
      mutex_->ReaderUnlock();
    } else if constexpr (LockType == MovableLockType::kWriter) {
      mutex_->WriterUnlock();
    } else {
      // This static_assert is the same condition as the above if condition so
      // if we reach this block, the static_assert will fail.
      static_assert(LockType == MovableLockType::kLock,
                    "Should be exhaustive if-else chain.");
    }
  }

 private:
  absl::Mutex* mutex_ = nullptr;
};
template <>
class MovableLock<MovableLockType::kNoLock> {
 public:
  template <typename... Args>
  MovableLock(Args&&...) {}
};

using MovableReaderLock = MovableLock<MovableLockType::kReader>;
using MovableWriterLock = MovableLock<MovableLockType::kWriter>;

template <typename T, typename LockStrategy>
class SynchronizedValue {
  using WriterLockT = std::conditional_t<
      synchronized_value_internal::should_lock_writes_v<LockStrategy>,
      MovableWriterLock, MovableLock<MovableLockType::kNoLock>>;

 public:
  using ViewType = typename LockStrategy::template ViewType<T>;
  using ValueType = T;

  static constexpr bool kInplaceUpdatable =
      synchronized_value_internal::is_inplace_updatable_v<LockStrategy, T>;
  static constexpr bool kCopyUpdatable =
      synchronized_value_internal::is_copy_updatable_v<LockStrategy, T>;
  static_assert(kCopyUpdatable || kInplaceUpdatable,
                "Strategy must be copy updatable or inplace updatable");

  // Constructs a synchronized value from t.
  explicit SynchronizedValue(T value) : value_(std::move(value)) {}

  template <typename... Args>
  explicit SynchronizedValue(Args&&... args)
      : value_(std::forward<Args>(args)...) {}

  // Make SynchronizedValue non-copyable and non-movable
  SynchronizedValue(const SynchronizedValue&) = delete;
  SynchronizedValue& operator=(const SynchronizedValue&) = delete;
  SynchronizedValue(SynchronizedValue&&) = delete;
  SynchronizedValue& operator=(SynchronizedValue&&) = delete;

  // Gets a proxy that keeps a const reference to the object and locks it with
  // RAII idiom.
  ViewType GetView() const {
    return LockStrategy::template MakeView<T>(mutex_, value_);
  }

  template <typename Predicate>
  ViewType GetViewWhen(const Predicate& predicate) const {
    return LockStrategy::template MakeView<T>(mutex_, value_, [&]() {
      return LockStrategy::EvaluateUpdateLockedPredicate(value_, predicate);
    });
  }

  // Takes a lambda that takes a const reference to the value and returns what
  // lambda returns.
  template <typename Reader>
  auto Read(Reader&& reader) const {
    return reader(*GetView());
  }

  // Condition version of Read. Predicate is a function object that takes `const
  // T&`. This function calls reader after predicate returns true.
  template <typename Reader, typename Predicate>
  auto ReadWhen(Reader&& reader, const Predicate& predicate) const {
    return reader(*GetViewWhen(predicate));
  }

  // Takes a lambda that takes a const reference to the object, updates it and
  // then the object is replaced with the one returned from lambda.
  template <typename Updater>
  void UpdateCopy(Updater&& updater) {
    static_assert(
        std::is_constructible_v<
            typename LockStrategy::template ValueType<T>,
            std::invoke_result_t<Updater,const ValueType&>>,
        "Invalid updater. Updater must take a const reference of ValueType and "
        "return a value that can be converted to ValueType");
    if constexpr (kCopyUpdatable) {
      WriterLockT lock(mutex_);
      LockStrategy::UpdateCopy(mutex_, value_, std::forward<Updater>(updater));
    } else {
      UpdateInplace([&updater](T& value) { value = updater(value); });
    }
  }

  // Takes a parameterless lambda and then the object is replaced with the one
  // returned from lambda.
  //
  // Only for strategies that support Set().
  template <typename Updater>
  void Set(Updater&& updater) {
    static_assert(
        std::is_constructible_v<typename LockStrategy::template ValueType<T>,
                                std::invoke_result_t<Updater>>,
        "Invalid updater. Updater must be an invocable that returns a value "
        "that can be converted to ValueType");
    WriterLockT lock(mutex_);
    LockStrategy::Set(mutex_, value_, std::forward<Updater>(updater));
  }

  // Condition version of UpdateCopy. Predicate is a function object that takes
  // `const T&`. This function calls updater after predicate returns true.
  template <typename Updater, typename Predicate>
  void UpdateCopyWhen(Updater&& updater, const Predicate& predicate) {
    static_assert(
        std::is_constructible_v<
            typename LockStrategy::template ValueType<T>,
            std::invoke_result_t<Updater,const ValueType&>>,
        "Invalid updater. Updater must take a const reference of ValueType and "
        "return a value that can be converted to ValueType");
    if constexpr (kCopyUpdatable) {
      WriterLockT lock(mutex_, [&]() {
        return LockStrategy::EvaluateUpdateLockedPredicate(value_, predicate);
      });
      LockStrategy::UpdateCopy(mutex_, value_, std::forward<Updater>(updater));
    } else {
      UpdateInplaceWhen([&updater](T& value) { value = updater(value); },
                        predicate);
    }
  }

  // Takes a lambda that takes a pointer to the object and updates it in place.
  template <typename Updater>
  void UpdateInplace(Updater&& updater) {
    static_assert(
        std::is_convertible_v<Updater&&, std::function<void(ValueType&)>>,
        "Invalid func signature: updater must be of signature (ValueType&) -> "
        "void");
    if constexpr (kInplaceUpdatable) {
      WriterLockT lock(mutex_);
      LockStrategy::UpdateInplace(mutex_, value_,
                                  std::forward<Updater>(updater));
    } else {
      UpdateCopy([&updater](const T& value) {
        auto new_value = value;
        updater(new_value);
        return new_value;
      });
    }
  }

  // Condition version of UpdateInplaceWhen. Predicate is a function object that
  // takes `const T&`. This function calls updater after predicate returns true.
  template <typename Updater, typename Predicate>
  void UpdateInplaceWhen(Updater&& updater, const Predicate& predicate) {
    static_assert(
        std::is_convertible_v<Updater&&, std::function<void(ValueType&)>>,
        "Invalid func signature: updater must be of signature (ValueType&) -> "
        "void");
    if constexpr (kInplaceUpdatable) {
      WriterLockT lock(mutex_, [&]() {
        return LockStrategy::EvaluateUpdateLockedPredicate(value_, predicate);
      });
      LockStrategy::UpdateInplace(mutex_, value_,
                                  std::forward<Updater>(updater));
    } else {
      UpdateCopyWhen(
          [&updater](const T& value) {
            auto new_value = value;
            updater(new_value);
            return new_value;
          },
          predicate);
    }
  }

 private:
  mutable absl::Mutex mutex_;
  typename LockStrategy::template ValueType<T> value_;
};

}  // namespace htls

#endif  // SYNCHRONIZED_VALUE_H_
