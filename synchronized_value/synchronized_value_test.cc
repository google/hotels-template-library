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

#include "synchronized_value/synchronized_value.h"

#include <algorithm>
#include <functional>
#include <initializer_list>
#include <iterator>
#include <memory>
#include <string>
#include <thread>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector>

#include <gtest/gtest.h>
#include <absl/container/node_hash_map.h>
#include <absl/functional/any_invocable.h>
#include "synchronized_value/absl_mutex_strategy.h"
#include "synchronized_value/left_right_strategy.h"
#include "synchronized_value/shared_ptr_rcu_strategy.h"
#include "synchronized_value/small_atomic_strategy.h"

namespace htls {
namespace synchronized_value_internal {
std::vector<std::thread> generate_pool(int num_threads,
                                       std::function<void()> func) {
  std::vector<std::thread> pool(num_threads);
  std::generate(pool.begin(), pool.end(), [&]() { return std::thread(func); });
  return pool;
}

std::vector<std::thread> iota_pool(int num_threads,
                                   std::function<void(int)> func) {
  std::vector<std::thread> pool(num_threads);
  std::generate(pool.begin(), pool.end(),
                [&func, n = 0]() mutable { return std::thread(func, n++); });
  return pool;
}
}  // namespace synchronized_value_internal
namespace {

template <typename Strat>
class SynchronizedValueTest : public ::testing::Test {};

TYPED_TEST_SUITE_P(SynchronizedValueTest);

template <typename Strat>
class SynchronizedValueSetTest : public ::testing::Test {};

TYPED_TEST_SUITE_P(SynchronizedValueSetTest);

TYPED_TEST_P(SynchronizedValueTest, NonMovableAndCopyable) {
  static_assert(
      std::is_copy_constructible_v<SynchronizedValue<int, TypeParam>> == false,
      "synchronized_value must be non-copyable.");
  static_assert(
      std::is_move_constructible<SynchronizedValue<int, TypeParam>>::value ==
          false,
      "synchronized_value must be non-movable.");
}

TYPED_TEST_P(SynchronizedValueTest, ReadSingleThread) {
  SynchronizedValue<int, TypeParam> sv(5);
  // Try all different lambda signatures
  sv.Read([](const int& x) { EXPECT_EQ(x, 5); });
  sv.Read([](const int x) { EXPECT_EQ(x, 5); });
  sv.Read([](int x) { EXPECT_EQ(x, 5); });

  sv.Read([](const auto& x) { EXPECT_EQ(x, 5); });
  sv.Read([](const auto x) { EXPECT_EQ(x, 5); });
  sv.Read([](auto x) { EXPECT_EQ(x, 5); });
  // Ensure read returns any type
  EXPECT_EQ(sv.Read([](auto x) { return std::vector{x}; }), std::vector{5});
}

TYPED_TEST_P(SynchronizedValueTest, UpdateCopySingleThread) {
  SynchronizedValue<int, TypeParam> sv(5);
  sv.UpdateCopy([](const int& x) { return x + 1; });
  sv.Read([](const int& x) { EXPECT_EQ(x, 6); });
  // Try all different lambda signatures
  sv.Read([](const int& x) { return x + 1; });
  sv.Read([](const int x) { return x + 1; });
  sv.Read([](int x) { return x + 1; });

  sv.Read([](const auto& x) { return x + 1; });
  sv.Read([](const auto x) { return x + 1; });
  sv.Read([](auto x) { return x + 1; });
  // Try setting the value
  sv.UpdateCopy([](auto _) { return 6; });
  sv.Read([](const int& x) { EXPECT_EQ(x, 6); });
}

TYPED_TEST_P(SynchronizedValueTest, UpdateInplaceSingleThread) {
  SynchronizedValue<int, TypeParam> sv(5);
  sv.UpdateInplace([](int& x) { x++; });
  sv.Read([](const int& x) { EXPECT_EQ(x, 6); });
  // Try all different lambda signatures
  sv.UpdateInplace([](auto& x) { x++; });
  // Try setting the value
  sv.UpdateInplace([](auto& x) { x = 6; });
  sv.Read([](const int& x) { EXPECT_EQ(x, 6); });
}

TYPED_TEST_P(SynchronizedValueSetTest, SetSingleThread) {
  SynchronizedValue<int, TypeParam> sv(5);
  sv.Set([]() { return 6; });
  sv.Read([](const int& x) { EXPECT_EQ(x, 6); });
  // Try all different lambda signatures
  sv.Read([](const int& x) { return x + 1; });
  sv.Read([](const int x) { return x + 1; });
  sv.Read([](int x) { return x + 1; });

  sv.Read([](const auto& x) { return x + 1; });
  sv.Read([](const auto x) { return x + 1; });
  sv.Read([](auto x) { return x + 1; });
  // Try setting the value
  sv.Set([]() { return 7; });
  sv.Read([](const int& x) { EXPECT_EQ(x, 7); });
}

TYPED_TEST_P(SynchronizedValueSetTest, SetWithRValueInvocable) {
  SynchronizedValue<int, TypeParam> sv(5);
  sv.Read([](const int& x) { EXPECT_EQ(x, 5); });

  absl::AnyInvocable<int() &&> invocable = []() { return 6; };
  sv.Set(std::move(invocable));
  sv.Read([](const int& x) { EXPECT_EQ(x, 6); });
}

TYPED_TEST_P(SynchronizedValueTest, DefaultConstructible) {
  if constexpr (!std::is_same_v<TypeParam, SmallAtomicStrategy>) {
    constexpr auto hello = "Hello world";

    struct constructable {
      std::string str;
      constructable() : str{hello} {}
    };

    SynchronizedValue<constructable, TypeParam> sv;
    sv.Read([&](const constructable& c) { EXPECT_EQ(c.str, hello); });
  }
}

TYPED_TEST_P(SynchronizedValueTest, GetViewSingleThread) {
  if constexpr (!std::is_same_v<TypeParam, SmallAtomicStrategy>) {
    SynchronizedValue<std::vector<int>, TypeParam> vec{
        std::initializer_list<int>{1, 2, 3, 4}};
    EXPECT_EQ(vec.GetView()->size(), 4);
  }

  SynchronizedValue<int, TypeParam> num{4};
  EXPECT_EQ(*num.GetView(), 4);
}

TYPED_TEST_P(SynchronizedValueTest, MoveonlyUpdateInplace) {
  struct moveonly {
    int value;

    explicit moveonly(int value) : value(value) {}
    // Make the type move-only
    moveonly(const moveonly&) = delete;
    moveonly& operator=(const moveonly&) = delete;
    moveonly(moveonly&&) = default;
    moveonly& operator=(moveonly&&) = default;
  };

  if constexpr (SynchronizedValue<moveonly, TypeParam>::kInplaceUpdatable) {
    if constexpr (!std::is_same_v<TypeParam, SmallAtomicStrategy>) {
      SynchronizedValue<moveonly, TypeParam> sv{5};
      sv.Read([&](const moveonly& m) { EXPECT_EQ(m.value, 5); });
      sv.UpdateInplace([](moveonly& m) { ++m.value; });
      sv.Read([&](const moveonly& m) { EXPECT_EQ(m.value, 6); });
    }
  }
}

template <bool Inplace, typename T>
void update_int(T& sync_val) {
  if constexpr (Inplace) {
    sync_val.UpdateInplace([](int& x) {
      int n = x;
      std::this_thread::yield();
      x = n + 1;
      n = x;
      std::this_thread::yield();
      x = n - 1;
      n = x;
      std::this_thread::yield();
      x = n + 1;
    });
  } else {
    sync_val.UpdateCopy([](int x) {
      std::this_thread::yield();
      return x + 1;
    });
  }
}

template <typename TypeParam, bool Inplace>
void test_update_multithread() {
  constexpr auto num_threads = 1000;
  SynchronizedValue<int, TypeParam> sv{0};
  auto threads = synchronized_value_internal::generate_pool(
      num_threads, [&]() { update_int<true>(sv); });
  sv.ReadWhen([&](int i) { EXPECT_EQ(i, num_threads); },
              [&](const int& i) { return i == num_threads; });
  if constexpr (sv.kCopyUpdatable) {
    sv.UpdateCopyWhen([&](const int& i) { return i * 2; },
                      [&](const int& i) { return i == num_threads; });

  } else {
    sv.UpdateInplaceWhen([&](int& i) { i *= 2; },
                         [&](const int& i) { return i == num_threads; });
  }

  for (auto& thread : threads) thread.join();

  sv.Read([&](int i) { EXPECT_EQ(i, 2 * num_threads); });
}

TYPED_TEST_P(SynchronizedValueTest, UpdateInplaceMultithread) {
  test_update_multithread<TypeParam, true>();
}

TYPED_TEST_P(SynchronizedValueTest, UpdateCopyMultithread) {
  test_update_multithread<TypeParam, false>();
}

template <bool Inplace, typename T, typename Updater>
void update_map(T& sv, int key, Updater&& updater) {
  if constexpr (Inplace) {
    sv.UpdateInplace([key, &updater](auto& map) { updater(map[key]); });
  } else {
    sv.UpdateCopy([key, &updater](const auto& map) {
      auto new_map(map);
      std::this_thread::yield();
      updater(new_map[key]);
      return new_map;
    });
  }
}

template <typename TypeParam, bool Inplace>
void test_update_map_multithread() {
  constexpr auto num_entries = 100;
  constexpr auto num_iterations = 10;
  constexpr auto threads_per_entry = 2;

  SynchronizedValue<std::unordered_map<int, int>, TypeParam> sv;

  std::vector<std::thread> pool;
  for (int i = 0; i < threads_per_entry; i++) {
    auto incrementer = [](int& i) {
      int n = i;
      std::this_thread::yield();
      i = n + 1;
    };
    auto func = [&](int i) {
      for (int j = 0; j < num_iterations; ++j)
        update_map<Inplace>(sv, i, incrementer);
    };
    auto sub_pool =
        synchronized_value_internal::iota_pool(num_entries, std::move(func));
    std::move(sub_pool.begin(), sub_pool.end(), std::back_inserter(pool));
  }
  for (auto& thread : pool) thread.join();
  sv.Read([&](const auto& map) {
    for (int i = 0; i < num_entries; ++i)
      EXPECT_EQ(map.at(i), num_iterations * threads_per_entry);
  });
}

TYPED_TEST_P(SynchronizedValueTest, MapUpdateInplaceMulti) {
  if constexpr (!std::is_same_v<TypeParam, SmallAtomicStrategy>) {
    test_update_map_multithread<TypeParam, true>();
  }
}

TYPED_TEST_P(SynchronizedValueTest, MapUpdateCopyMulti) {
  if constexpr (!std::is_same_v<TypeParam, SmallAtomicStrategy>) {
    test_update_map_multithread<TypeParam, false>();
  }
}

REGISTER_TYPED_TEST_SUITE_P(SynchronizedValueTest,      //
                            NonMovableAndCopyable,      //
                            ReadSingleThread,           //
                            UpdateCopySingleThread,     //
                            UpdateInplaceSingleThread,  //
                            DefaultConstructible,       //
                            GetViewSingleThread,        //
                            MoveonlyUpdateInplace,      //
                            UpdateInplaceMultithread,   //
                            UpdateCopyMultithread,      //
                            MapUpdateInplaceMulti,      //
                            MapUpdateCopyMulti);

using strategies = ::testing::Types<AbslMutexStrategy, LeftRightStrategy,
                                    SharedPtrRcuStrategy, SmallAtomicStrategy>;
INSTANTIATE_TYPED_TEST_SUITE_P(SVTests, SynchronizedValueTest, strategies);

REGISTER_TYPED_TEST_SUITE_P(SynchronizedValueSetTest,  //
                            SetSingleThread,           //
                            SetWithRValueInvocable);

using strategies_that_support_set = ::testing::Types<SharedPtrRcuStrategy>;
INSTANTIATE_TYPED_TEST_SUITE_P(SVSetTests, SynchronizedValueSetTest,
                               strategies_that_support_set);

}  // namespace
}  // namespace htls
