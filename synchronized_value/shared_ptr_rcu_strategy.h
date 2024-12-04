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

#ifndef SHARED_PTR_RCU_STRATEGY_H
#define SHARED_PTR_RCU_STRATEGY_H

#include <atomic>
#include <memory>
#include <type_traits>

#include <absl/synchronization/mutex.h>
#include "synchronized_value/absl_mutex_strategy.h"
#include "synchronized_value/synchronized_value.h"

namespace htls {

class SharedPtrRcuStrategy {
  template <typename T>
  struct ForwardingWrapper {
    SynchronizedValue<std::shared_ptr<const T>, AbslMutexStrategy> val_ptr;

    template <typename... Args>
    ForwardingWrapper(Args&&... args)
        : val_ptr(std::make_shared<const T>(std::forward<Args>(args)...)) {}
  };

 public:
  template <typename T>
  using ValueType = ForwardingWrapper<T>;

  template <typename T>
  using ViewType = std::shared_ptr<const T>;

  template <typename T, typename CopyUpdate>
  static void UpdateCopy(absl::Mutex&, ValueType<T>& value,
                         CopyUpdate&& update) {
    auto ptr = std::make_shared<T>(
        std::forward<CopyUpdate>(update)(**value.val_ptr.GetView()));
    value.val_ptr.UpdateCopy(
        [ptr = std::move(ptr)](const auto&) mutable { return std::move(ptr); });
  }

  template <typename T, typename CopyUpdate>
  static void Set(absl::Mutex&, ValueType<T>& value, CopyUpdate&& update) {
    auto ptr = std::make_shared<T>(std::forward<CopyUpdate>(update)());
    value.val_ptr.UpdateCopy(
        [ptr = std::move(ptr)](const auto&) mutable { return std::move(ptr); });
  }

  template <typename T>
  static ViewType<T> MakeView(absl::Mutex& mutex, const ValueType<T>& value) {
    return value.val_ptr.Read([](const auto& ptr) { return ptr; });
  }
  template <typename T, typename Predicate>
  static ViewType<T> MakeView(absl::Mutex& mutex, const ValueType<T>& value,
                              const Predicate& predicate) {
    MovableReaderLock lock(mutex, predicate);
    return value.val_ptr.Read([](const auto& ptr) { return ptr; });
  }

  template <typename T, typename Predicate>
  static bool EvaluateUpdateLockedPredicate(const ValueType<T>& value,
                                            const Predicate& predicate) {
    return predicate(**value.val_ptr.GetView());
  }
};
}  // namespace htls

#endif  // SHARED_PTR_RCU_STRATEGY_H
