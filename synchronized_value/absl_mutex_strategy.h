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

#ifndef ABSL_MUTEX_STRATEGY_H_
#define ABSL_MUTEX_STRATEGY_H_

#include <memory>
#include <type_traits>
#include <utility>

#include "third_party/absl/synchronization/mutex.h"
#include "third_party/hotels_template_library/synchronized_value/synchronized_value.h"

namespace htls {
class AbslMutexStrategy {
  template <typename T>
  class View {
    MovableReaderLock lock_;
    const T &value_;

   public:
    explicit View(absl::Mutex &mu, const T &value) : lock_{mu}, value_(value) {}

    template <typename Predicate>
    explicit View(absl::Mutex &mu, const T &value, const Predicate &predicate)
        : lock_{mu, predicate}, value_(value) {}

    const T &operator*() const { return value_; }
    const T *operator->() const { return &value_; }
  };

 public:
  template <typename T>
  using value_type = T;

  template <typename T>
  using ViewType = View<T>;

  template <typename T, typename Mutator>
  static void UpdateInplace(absl::Mutex &, value_type<T> &value,
                            Mutator &&mutate) {
    mutate(value);
  }

  template <typename T>
  static ViewType<T> MakeView(absl::Mutex &mutex, const value_type<T> &value) {
    return ViewType<T>{mutex, value};
  }

  template <typename T, typename Predicate>
  static ViewType<T> MakeView(absl::Mutex &mutex, const value_type<T> &value,
                              const Predicate &predicate) {
    return ViewType<T>{mutex, value, predicate};
  }

  template <typename T>
  static const T &GetUnsafeValue(const value_type<T> &value) {
    return value;
  }
};

}  // namespace htls

#endif  // ABSL_MUTEX_STRATEGY_H_
