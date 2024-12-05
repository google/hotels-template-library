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

#ifndef SMALL_ATOMIC_STRATEGY_H_
#define SMALL_ATOMIC_STRATEGY_H_

#include <atomic>
#include <memory>
#include <type_traits>
#include <utility>

#include <absl/synchronization/mutex.h>
#include "synchronized_value/synchronized_value.h"

namespace htls {
class SmallAtomicStrategy {
  template <typename T>
  class View {
    const T value_;

   public:
    explicit View(T value) : value_(value) {}

    const T &operator*() const { return value_; }
    const T *operator->() const { return &value_; }
  };

 public:
  template <typename T>
  using ValueType = std::atomic<T>;

  template <typename T>
  using ViewType = View<T>;

  template <typename T, typename Mutator>
  static void UpdateInplace(absl::Mutex &, ValueType<T> &value,
                            Mutator &&mutate) {
    // This function is called with the SynchronizedValue mutex locked.
    auto temp = value.load();
    mutate(temp);
    value.store(temp);
  }

  template <typename T>
  static ViewType<T> MakeView(absl::Mutex &, const ValueType<T> &value) {
    static_assert(ValueType<T>::is_always_lock_free);
    return ViewType<T>{value.load()};
  }

  template <typename T, typename Predicate>
  static ViewType<T> MakeView(absl::Mutex &mutex, const ValueType<T> &value,
                              const Predicate &predicate) {
    static_assert(ValueType<T>::is_always_lock_free);
    MovableReaderLock lock(mutex, predicate);
    return ViewType<T>{value.load()};
  }

  template <typename T, typename Predicate>
  static bool EvaluateUpdateLockedPredicate(const ValueType<T> &value,
                                            const Predicate &predicate) {
    return predicate(value.load());
  }
};

}  // namespace htls

#endif  // SMALL_ATOMIC_STRATEGY_H_
