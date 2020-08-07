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

#include "absl/synchronization/mutex.h"

namespace synchronized_value {
class absl_mutex_strategy {
  template <typename T>
  class view {
    absl::ReaderMutexLock lock_;
    const T &value_;

   public:
    explicit view(absl::Mutex &mu, const T &value)
        : lock_{&mu}, value_(value) {}

    const T &operator*() const { return value_; }
    const T *operator->() const { return &value_; }
  };

  mutable absl::Mutex mu_;

 public:
  template <typename T>
  using value_type = T;

  template <typename T>
  using view_type = view<T>;

  template <typename T, typename Mutator>
  void update_inplace(value_type<T> &value, Mutator &&mutate) {
    absl::WriterMutexLock lock{&mu_};
    mutate(value);
  }

  template <typename T>
  const view_type<T> make_view(const value_type<T> &value) const {
    return view_type<T>{mu_, value};
  }
};

}  // namespace synchronized_value

#endif  // ABSL_MUTEX_STRATEGY_H_
