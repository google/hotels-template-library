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

namespace synchronized_value {
namespace synchronized_value_internal {
// atomic_shared_ptr : A wrapper for the atomic_* operations on std::shared_ptr
//                     deprecated in C++20. This wrapper implements the
//                     interface for std::atomic<shared_ptr> defined in C++20
template <typename T>
class atomic_shared_ptr {
  std::shared_ptr<T> val_;

 public:
  constexpr atomic_shared_ptr() noexcept = default;
  atomic_shared_ptr(std::shared_ptr<T> desired) noexcept : val_{desired} {}
  atomic_shared_ptr(const atomic_shared_ptr&) = delete;

  void operator=(const atomic_shared_ptr&) = delete;
  void operator=(std::shared_ptr<T> desired) noexcept { store(desired); }

  void store(std::shared_ptr<T> desired,
             std::memory_order order = std::memory_order_seq_cst) noexcept {
    std::atomic_store_explicit(&val_, desired, order);
  }

  std::shared_ptr<T> load(
      std::memory_order order = std::memory_order_seq_cst) const noexcept {
    return std::atomic_load_explicit(&val_, order);
  }
  
  bool compare_exchange_strong(
      std::shared_ptr<T>& expected, std::shared_ptr<T> desired,
      std::memory_order order = std::memory_order_seq_cst) noexcept {
    return std::atomic_compare_exchange_strong_explicit(&val_, &expected,
                                                        desired, order, order);
  }

  bool compare_exchange_weak(
      std::shared_ptr<T>& expected, std::shared_ptr<T> desired,
      std::memory_order order = std::memory_order_seq_cst) noexcept {
    return std::atomic_compare_exchange_weak_explicit(&val_, &expected,
                                                        desired, order, order);
  }
};

// a simple shim struct to template specialize around std::shared_ptr<T>
template <typename T>
struct atomic_shim {
  using type = std::atomic<T>;
};

template <typename T>
struct atomic_shim<std::shared_ptr<T>> {
  using type = atomic_shared_ptr<T>;
};

template <typename T>
// In C++20, replace this line with "using atomic = std::atomic<T>;"
using atomic = typename atomic_shim<T>::type;
}  // namespace synchronized_value_internal

class shared_ptr_rcu_strategy {
  template <typename T>
  struct forwarding_wrapper {
    synchronized_value_internal::atomic<std::shared_ptr<T>> val_ptr;

    template <typename... Args>
    forwarding_wrapper(Args&&... args)
        : val_ptr(std::make_shared<T>(std::forward<Args>(args)...)) {}
  };

 public:
  template <typename T>
  using value_type = forwarding_wrapper<T>;

  template <typename T>
  using view_type = std::shared_ptr<const T>;

  template <typename T, typename CopyUpdate>
  void update_copy(value_type<T>& value, CopyUpdate&& update) {
    std::shared_ptr<T> val_ptr, new_val_ptr;
    // If someone else changed the value in between, redo the operation
    do {
      val_ptr = value.val_ptr.load();
      new_val_ptr = std::make_shared<T>(std::forward<CopyUpdate>(update)(std::as_const(*val_ptr)));
    }
    while (!value.val_ptr.compare_exchange_strong(val_ptr, new_val_ptr));
  }

  template <typename T>
  const view_type<T> make_view(const value_type<T>& value) const {
    return value.val_ptr.load();
  }
};
}  // namespace synchronized_value

#endif  // SHARED_PTR_RCU_STRATEGY_H
