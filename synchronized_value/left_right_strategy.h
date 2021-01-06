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

#ifndef LEFT_RIGHT_STRATEGY_H
#define LEFT_RIGHT_STRATEGY_H

#include <array>
#include <atomic>
#include <thread>

#include "third_party/absl/synchronization/mutex.h"
#include "third_party/hotels_template_library/synchronized_value/synchronized_value.h"

namespace htls {
namespace synchronized_value_internal {
// Toggleable : an wrapper around two versions of a variable, which can be
//              atomically toggled between. Construction arguments are forwarded
//              to both versions. Toggling the variable switches which version
//              is "active" (the live version) and which is "inactive" (the
//              staging version)
template <typename T>
class Toggleable {
  std::array<T, 2> vals_;
  std::atomic<bool> index_;

 public:
  T& GetActive() { return vals_[index_]; }
  const T& GetActive() const { return vals_[index_]; }

  T& GetInactive() { return vals_[!index_]; }
  const T& GetInactive() const { return vals_[!index_]; }

  void Toggle() { index_ = !index_; }

  template <typename... Args>
  Toggleable(Args&&... args) : vals_{T(args...), T(args...)}, index_{0} {}
};
}  // namespace synchronized_value_internal

// LeftRightStrategy : a lock strategy that maintains two separate versions
//                       of the value and toggles between them. Uses one version
//                       for readers and one for writers. Based on a paper found
//                       here: https://hal.archives-ouvertes.fr/hal-01207881
class LeftRightStrategy {
  using count_t = std::atomic<int>;
  template <typename T>
  class View {
    count_t& count_;
    const T& reference_;

   public:
    View(const T& _ref, count_t& _ref_count)
        : count_{_ref_count}, reference_{_ref} {}

    ~View() { --count_; }

    const T& operator*() const { return reference_; }
    const T* operator->() const { return &reference_; }
  };

 public:
  template <typename T>
  struct value_type {
    synchronized_value_internal::Toggleable<T> value;
    mutable synchronized_value_internal::Toggleable<count_t> reader_counts;
    template <typename... Args>
    explicit value_type(Args&&... args) : value{std::forward<Args>(args)...} {}
  };

  template <typename T>
  using ViewType = View<T>;

  template <typename T, typename Mutator>
  static void UpdateInplace(absl::Mutex&, value_type<T>& value,
                            const Mutator& mutate) {
    // Apply the change to the inactive value and make it visible to readers
    mutate(value.value.GetInactive());
    value.value.Toggle();
    // Wait for residual readers to leave and toggle
    // Note: this can and should be replaced by std::atomic_wait in C++20
    while (value.reader_counts.GetInactive()) std::this_thread::yield();
    value.reader_counts.Toggle();
    while (value.reader_counts.GetInactive()) std::this_thread::yield();
    // Apply the change to the newly inactive value
    mutate(value.value.GetInactive());
  }

  template <typename T>
  static ViewType<T> MakeView(absl::Mutex&, const value_type<T>& value) {
    count_t& count = value.reader_counts.GetActive();
    ++count;
    return ViewType<T>(value.value.GetActive(), count);
  }
  template <typename T, typename Predicate>
  static ViewType<T> MakeView(absl::Mutex& mutex, const value_type<T>& value,
                              const Predicate& predicate) {
    MovableReaderLock lock(mutex, predicate);
    count_t& count = value.reader_counts.GetActive();
    ++count;
    return ViewType<T>(value.value.GetActive(), count);
  }

  template <typename T>
  static const T& GetUnsafeValue(const value_type<T>& value) {
    return value.value.GetActive();
  }
};
}  // namespace htls

#endif  // LEFT_RIGHT_STRATEGY_H
