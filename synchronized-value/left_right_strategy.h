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

namespace synchronized_value {
namespace synchronized_value_internal {
// toggleable : an wrapper around two versions of a variable, which can be
//              atomically toggled between. Construction arguments are forwarded
//              to both versions. Toggling the variable switches which version
//              is "active" (the live version) and which is "inactive" (the
//              staging version)
template <typename T>
class toggleable {
  std::array<T, 2> vals_;
  std::atomic<bool> index_;

 public:
  T& get_active() { return vals_[index_]; }
  const T& get_active() const { return vals_[index_]; }

  T& get_inactive() { return vals_[!index_]; }
  const T& get_inactive() const { return vals_[!index_]; }

  void toggle() { index_ = !index_; }

  template <typename... Args>
  toggleable(Args&&... args) : vals_{T(args...), T(args...)}, index_{0} {}
};
}  // namespace synchronized_value_internal

// left_right_strategy : a lock strategy that maintains two separate versions
//                       of the value and toggles between them. Uses one version
//                       for readers and one for writers. Based on a paper found
//                       here: https://hal.archives-ouvertes.fr/hal-01207881
class left_right_strategy {
  using count_t = std::atomic<int>;
  template <typename T>
  class view {
    count_t& count_;
    const T& reference_;

   public:
    view(const T& _ref, count_t& _ref_count)
        : count_{_ref_count}, reference_{_ref} {}

    ~view() { --count_; }

    const T& operator*() const { return reference_; }
    const T* operator->() const { return &reference_; }
  };

  std::mutex writer_mu_;
  mutable synchronized_value_internal::toggleable<count_t> reader_counts_;

 public:
  template <typename T>
  using value_type = synchronized_value_internal::toggleable<T>;

  template <typename T>
  using view_type = view<T>;

  template <typename T, typename Mutator>
  void update_inplace(value_type<T>& value, const Mutator& mutate) {
    std::lock_guard lock{writer_mu_};

    // Apply the change to the inactive value and make it visible to readers
    mutate(value.get_inactive());
    value.toggle();
    // Wait for residual readers to leave and toggle
    // Note: this can and should be replaced by std::atomic_wait in C++20
    while (reader_counts_.get_inactive()) std::this_thread::yield();
    reader_counts_.toggle();
    while (reader_counts_.get_inactive()) std::this_thread::yield();
    // Apply the change to the newly inactive value
    mutate(value.get_inactive());
  }

  template <typename T>
  const view_type<T> make_view(const value_type<T>& value) const {
    count_t& count = reader_counts_.get_active();
    ++count;
    return view(value.get_active(), count);
  }
};
}  // namespace synchronized_value

#endif  // LEFT_RIGHT_STRATEGY_H
