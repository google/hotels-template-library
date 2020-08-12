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

#include "synchronized-value/src/experimental/synchronized_value.h"

#include <functional>
#include <memory>
#include <string>
#include <thread>
#include <type_traits>
#include <unordered_map>

#include "gtest/gtest.h"
#include "synchronized-value/src/mutex_strategy.h"
#include "synchronized-value/src/rw_mutex_strategy.h"

namespace synchronized_value {
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

TYPED_TEST_P(SynchronizedValueTest, NonMovableAndCopyable) {
  static_assert(
      std::is_copy_constructible_v<synchronized_value<int, mutex_strategy>> ==
          false,
      "synchronized_value must be non-copyable.");
  static_assert(std::is_move_constructible<
                    synchronized_value<int, mutex_strategy>>::value == false,
                "synchronized_value must be non-movable.");
}

TYPED_TEST_P(SynchronizedValueTest, ReadSingleThread) {
  synchronized_value<int, mutex_strategy> sv(5);
  // Try all different lambda signatures
  sv.read([](const int& x) { EXPECT_EQ(x, 5); });
  sv.read([](const int x) { EXPECT_EQ(x, 5); });
  sv.read([](int x) { EXPECT_EQ(x, 5); });

  sv.read([](const auto& x) { EXPECT_EQ(x, 5); });
  sv.read([](const auto x) { EXPECT_EQ(x, 5); });
  sv.read([](auto x) { EXPECT_EQ(x, 5); });
  // Ensure read returns any type
  EXPECT_EQ(sv.read([](auto x) { return std::vector{x}; }), std::vector{5});
}

TYPED_TEST_P(SynchronizedValueTest, ReadWhen) {
  synchronized_value<int, mutex_strategy> sv(5);
  sv.read_when([](int x) { EXPECT_EQ(x, 5); }, [](int x) { return x > 0; });
  auto thread = std::thread([&]() {
    sv.read_when([](int x) { EXPECT_EQ(x, 7); }, [](int x) { return x > 5; });
  });
  sv.update_copy([](int x) { return 7; });
  thread.join();
  sv.read([](int x) { EXPECT_EQ(x, 7); });
}

TYPED_TEST_P(SynchronizedValueTest, GetViewWhen) {
  synchronized_value<int, mutex_strategy> sv(5);
  EXPECT_EQ(5, *sv.get_view_when([](int x) { return x > 0; }));
  auto thread = std::thread(
      [&]() { EXPECT_EQ(7, *sv.get_view_when([](int x) { return x > 5; })); });
  sv.update_copy([](int x) { return 7; });
  thread.join();
  sv.read([](int x) { EXPECT_EQ(x, 7); });
}

TYPED_TEST_P(SynchronizedValueTest, UpdateCopySingleThread) {
  synchronized_value<int, mutex_strategy> sv(5);
  sv.update_copy([](const int& x) { return x + 1; });
  sv.read([](const int& x) { EXPECT_EQ(x, 6); });
  // Try all different lambda signatures
  sv.read([](const int& x) { return x + 1; });
  sv.read([](const int x) { return x + 1; });
  sv.read([](int x) { return x + 1; });

  sv.read([](const auto& x) { return x + 1; });
  sv.read([](const auto x) { return x + 1; });
  sv.read([](auto x) { return x + 1; });
  // Try setting the value
  sv.update_copy([](auto _) { return 6; });
  sv.read([](const int& x) { EXPECT_EQ(x, 6); });

  // Try getting a return value
  std::string seven = sv.update_copy(
      [](int x) { return std::make_pair(x + 1, std::to_string(x + 1)); });
  EXPECT_EQ(seven, "7");
  sv.read([](const int& x) { EXPECT_EQ(x, 7); });
}

TYPED_TEST_P(SynchronizedValueTest, UpdateCopyWhen) {
  synchronized_value<int, mutex_strategy> sv(5);
  sv.update_copy_when([](int _) { return 6; }, [](int x) { return x > 0; });
  sv.read([](int x) { EXPECT_EQ(6, x); });
  auto thread = std::thread([&]() {
    sv.update_copy_when([](int _) { return 8; }, [](int x) { return x > 6; });
  });
  sv.update_copy([](int x) { return 7; });
  thread.join();
  sv.read([](int x) { EXPECT_EQ(x, 8); });
  thread = std::thread([&]() {
    EXPECT_EQ("9",
              sv.update_copy_when(
                  [](int x) { return std::make_pair(10, std::to_string(x)); },
                  [](int x) { return x > 8; }));
  });
  sv.update_copy([](int x) { return 9; });
  thread.join();
  sv.read([](int x) { EXPECT_EQ(x, 10); });
}

TYPED_TEST_P(SynchronizedValueTest, UpdateInplaceSingleThread) {
  synchronized_value<int, mutex_strategy> sv(5);
  sv.update_inplace([](int& x) { x++; });
  sv.read([](const int& x) { EXPECT_EQ(x, 6); });
  // Try all different lambda signatures
  sv.update_inplace([](auto& x) { x++; });
  // Try setting the value
  sv.update_inplace([](auto& x) { x = 6; });
  sv.read([](const int& x) { EXPECT_EQ(x, 6); });
  // Try getting a return value
  EXPECT_EQ("7", sv.update_inplace([](int& x) { return std::to_string(++x); }));
  sv.read([](const int& x) { EXPECT_EQ(x, 7); });
}

TYPED_TEST_P(SynchronizedValueTest, UpdateInplaceWhen) {
  synchronized_value<int, mutex_strategy> sv(5);
  sv.update_inplace_when([](int& x) { x = 6; }, [](int x) { return x > 0; });
  sv.read([](int x) { EXPECT_EQ(6, x); });
  auto thread = std::thread([&]() {
    sv.update_inplace_when([](int& x) { x = 8; }, [](int x) { return x > 6; });
  });
  sv.update_copy([](int x) { return 7; });
  thread.join();
  sv.read([](int x) { EXPECT_EQ(x, 8); });
  thread = std::thread([&]() {
    EXPECT_EQ("9",
              sv.update_inplace_when([](int& x) { return std::to_string(x++); },
                                     [](int x) { return x > 8; }));
  });
  sv.update_copy([](int x) { return 9; });
  thread.join();
  sv.read([](int x) { EXPECT_EQ(x, 10); });
}

TYPED_TEST_P(SynchronizedValueTest, DefaultConstructible) {
  constexpr auto hello = "Hello world";

  struct constructable {
    std::string str;
    constructable() : str{hello} {}
  };

  synchronized_value<constructable, TypeParam> sv;
  sv.read([&](const constructable& c) { EXPECT_EQ(c.str, hello); });
}

TYPED_TEST_P(SynchronizedValueTest, GetViewSingleThread) {
  synchronized_value<std::vector<int>, mutex_strategy> vec{
      std::initializer_list<int>{1, 2, 3, 4}};
  EXPECT_EQ(vec.get_view()->size(), 4);

  synchronized_value<int, mutex_strategy> num{4};
  EXPECT_EQ(*num.get_view(), 4);
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

  if constexpr (synchronized_value<moveonly, TypeParam>::is_inplace_updatable) {
    synchronized_value<moveonly, TypeParam> sv{5};
    sv.read([&](const moveonly& m) { EXPECT_EQ(m.value, 5); });
    sv.update_inplace([](moveonly& m) { ++m.value; });
    sv.read([&](const moveonly& m) { EXPECT_EQ(m.value, 6); });
  }
}

template <bool Inplace, typename T>
void update_int(T& sync_val) {
  if constexpr (Inplace)
    sync_val.update_inplace([](int& x) {
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
  else
    sync_val.update_copy([](int x) {
      std::this_thread::yield();
      return x + 1;
    });
}

template <typename TypeParam, bool Inplace>
void test_update_multithread() {
  constexpr auto num_threads = 1000;
  synchronized_value<int, TypeParam> sv{0};
  for (auto& thread : synchronized_value_internal::generate_pool(
           num_threads, [&]() { update_int<true>(sv); }))
    thread.join();
  sv.read([&](int i) { EXPECT_EQ(i, num_threads); });
}

TYPED_TEST_P(SynchronizedValueTest, UpdateInplaceMultithread) {
  test_update_multithread<TypeParam, true>();
}

TYPED_TEST_P(SynchronizedValueTest, UpdateCopyMultithread) {
  test_update_multithread<TypeParam, false>();
}

template <bool Inplace, typename T, typename Updater>
void update_map(T& sv, int key, Updater&& updater) {
  if constexpr (Inplace)
    sv.update_inplace([key, &updater](std::unordered_map<int, int>& map) {
      updater(map[key]);
    });
  else
    sv.update_copy([key, &updater](const std::unordered_map<int, int>& map) {
      auto new_map(map);
      std::this_thread::yield();
      updater(new_map[key]);
      return new_map;
    });
}

template <typename TypeParam, bool Inplace>
void test_update_map_multithread() {
  constexpr auto num_entries = 100;
  constexpr auto num_iterations = 10;
  constexpr auto threads_per_entry = 2;

  synchronized_value<std::unordered_map<int, int>, TypeParam> sv;

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
  sv.read([&](const auto& map) {
    for (int i = 0; i < num_entries; ++i)
      EXPECT_EQ(map.at(i), num_iterations * threads_per_entry);
  });
}

TYPED_TEST_P(SynchronizedValueTest, MapUpdateInplaceMulti) {
  test_update_map_multithread<TypeParam, true>();
}

TYPED_TEST_P(SynchronizedValueTest, MapUpdateCopyMulti) {
  test_update_map_multithread<TypeParam, false>();
}

REGISTER_TYPED_TEST_SUITE_P(SynchronizedValueTest, NonMovableAndCopyable,
                            ReadSingleThread, UpdateCopySingleThread,
                            UpdateInplaceSingleThread, DefaultConstructible,
                            GetViewSingleThread, MoveonlyUpdateInplace,
                            UpdateInplaceMultithread, UpdateCopyMultithread,
                            MapUpdateInplaceMulti, MapUpdateCopyMulti, ReadWhen,
                            GetViewWhen, UpdateCopyWhen, UpdateInplaceWhen);

using strategies = ::testing::Types<mutex_strategy, rw_mutex_strategy>;
INSTANTIATE_TYPED_TEST_SUITE_P(SVTests, SynchronizedValueTest, strategies);

}  // namespace
}  // namespace synchronized_value
