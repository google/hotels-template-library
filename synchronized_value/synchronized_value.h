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

#include "third_party/absl/synchronization/mutex.h"

namespace htls {
namespace synchronized_value_internal {

template <class, class, class = std::void_t<>>
struct is_inplace_updatable : std::false_type {};

template <class Strategy, class T>
struct is_inplace_updatable<
    Strategy, T,
    std::void_t<decltype(Strategy::UpdateInplace(
        std::declval<absl::Mutex&>(),
        std::declval<typename Strategy::template value_type<T>&>(),
        std::declval<std::function<void(
            typename Strategy::template value_type<T>&)>>()))>>
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
        std::declval<typename Strategy::template value_type<T>&>(),
        std::declval<std::function<typename Strategy::template value_type<T>(
            const typename Strategy::template value_type<T>&)>>()))>>
    : std::true_type {};

template <class Strategy, class T>
inline constexpr bool is_copy_updatable_v =
    is_copy_updatable<Strategy, T>::value;

}  // namespace synchronized_value_internal

// Defines a movable lock.

enum class MovableLockType { kLock, kReader, kWriter };

template <MovableLockType LockType>
class MovableLock {
 public:
  explicit MovableLock(absl::Mutex& mutex) : mutex_(&mutex) {
    switch (LockType) {
      case MovableLockType::kLock:
        mutex_->Lock();
        break;
      case MovableLockType::kReader:
        mutex_->ReaderLock();
        break;
      case MovableLockType::kWriter:
        mutex_->WriterLock();
        break;
    }
  }

  explicit operator bool() const { return mutex_; }

  template <typename Predicate>
  MovableLock(absl::Mutex& mutex, const Predicate& predicate) : mutex_(&mutex) {
    absl::Condition condition(
        +[](const Predicate* predicate) { return (*predicate)(); }, &predicate);
    switch (LockType) {
      case MovableLockType::kLock:
        mutex_->LockWhen(condition);
        break;
      case MovableLockType::kReader:
        mutex_->ReaderLockWhen(condition);
        break;
      case MovableLockType::kWriter:
        mutex_->WriterLockWhen(condition);
        break;
    }
  }

  MovableLock() : mutex_(nullptr) {}

  MovableLock(const MovableLock&) = delete;
  MovableLock& operator=(const MovableLock&) = delete;

  MovableLock(MovableLock&& other) : mutex_(other.mutex_) {
    other.mutex_ = nullptr;
  }
  MovableLock& operator=(MovableLock&& other) {
    mutex_ = other.mutex_;
    other.mutex_ = nullptr;
    return *this;
  }

  ~MovableLock() {
    if (!mutex_) return;
    switch (LockType) {
      case MovableLockType::kLock:
        mutex_->Unlock();
        break;
      case MovableLockType::kReader:
        mutex_->ReaderUnlock();
        break;
      case MovableLockType::kWriter:
        mutex_->WriterUnlock();
        break;
    }
  }

 private:
  absl::Mutex* mutex_;
};

using MovableReaderLock = MovableLock<MovableLockType::kReader>;
using MovableWriterLock = MovableLock<MovableLockType::kWriter>;


template <typename T, typename LockStrategy>
class SynchronizedValue {
 public:
  using ViewType = typename LockStrategy::template ViewType<T>;
  using value_type = T;
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
      return predicate(LockStrategy::GetUnsafeValue(value_));
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
        std::is_convertible_v<Updater&&,
                              std::function<value_type(const value_type&)>>,
        "Invalid func signature: updater must be of signature (const "
        "value_type&) -> value_type");
    if constexpr (kCopyUpdatable) {
      absl::WriterMutexLock lock(&mutex_);
      LockStrategy::UpdateCopy(mutex_, value_, std::forward<Updater>(updater));
    } else {
      UpdateInplace([&updater](T& value) { value = updater(value); });
    }
  }

  // Condition version of UpdateCopy. Predicate is a function object that takes
  // `const T&`. This function calls updater after predicate returns true.
  template <typename Updater, typename Predicate>
  void UpdateCopyWhen(Updater&& updater, const Predicate& predicate) {
    static_assert(
        std::is_convertible_v<Updater&&,
                              std::function<value_type(const value_type&)>>,
        "Invalid func signature: updater must be of signature (const "
        "value_type&) -> value_type");
    if constexpr (kCopyUpdatable) {
      MovableWriterLock lock(mutex_, [&]() {
        return predicate(LockStrategy::GetUnsafeValue(value_));
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
        std::is_convertible_v<Updater&&, std::function<void(value_type&)>>,
        "Invalid func signature: updater must be of signature (value_type&) -> "
        "void");
    if constexpr (kInplaceUpdatable) {
      absl::WriterMutexLock lock(&mutex_);
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
        std::is_convertible_v<Updater&&, std::function<void(value_type&)>>,
        "Invalid func signature: updater must be of signature (value_type&) -> "
        "void");
    if constexpr (kInplaceUpdatable) {
      MovableWriterLock lock(mutex_, [&]() {
        return predicate(LockStrategy::GetUnsafeValue(value_));
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
  typename LockStrategy::template value_type<T> value_;
};

}  // namespace htls

#endif  // SYNCHRONIZED_VALUE_H_
