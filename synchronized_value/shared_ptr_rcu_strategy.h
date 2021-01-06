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

#include "third_party/absl/synchronization/mutex.h"
#include "third_party/hotels_template_library/synchronized_value/synchronized_value.h"

namespace htls {

class SharedPtrRcuStrategy {
  template <typename T>
  struct ForwardingWrapper {
    std::shared_ptr<T> val_ptr;

    template <typename... Args>
    ForwardingWrapper(Args&&... args)
        : val_ptr(std::make_shared<T>(std::forward<Args>(args)...)) {}
  };

 public:
  template <typename T>
  using value_type = ForwardingWrapper<T>;

  template <typename T>
  using ViewType = std::shared_ptr<const T>;

  template <typename T, typename CopyUpdate>
  static void UpdateCopy(absl::Mutex&, value_type<T>& value,
                         CopyUpdate&& update) {
    value.val_ptr = std::make_shared<T>(
        std::forward<CopyUpdate>(update)(std::as_const(*value.val_ptr)));
  }

  template <typename T>
  static ViewType<T> MakeView(absl::Mutex& mutex, const value_type<T>& value) {
    MovableReaderLock lock(mutex);
    return value.val_ptr;
  }
  template <typename T, typename Predicate>
  static ViewType<T> MakeView(absl::Mutex& mutex, const value_type<T>& value,
                              const Predicate& predicate) {
    MovableReaderLock lock(mutex, predicate);
    return value.val_ptr;
  }

  template <typename T>
  static const T& GetUnsafeValue(const value_type<T>& value) {
    return *value.val_ptr;
  }
};
}  // namespace htls

#endif  // SHARED_PTR_RCU_STRATEGY_H
