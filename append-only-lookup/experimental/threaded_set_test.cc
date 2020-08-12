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

#include <random>
#include <thread>
#include <unordered_map>

#include "append-only-lookup/atomic_set.h"
#include "gtest/gtest.h"
#include "synchronized-value/mutex_strategy.h"
#include "append-only-lookup/synchronized_set.h"

namespace atomic_lookup {
namespace atomic_lookup_internal {

template <typename T>
auto make_generator();

template <>
auto make_generator<int>() {
  return [n = 0]() mutable { return n++; };
}

}  // namespace atomic_lookup_internal

namespace {

template <typename Set>
class ThreadedSetTest : public ::testing::Test {};

TYPED_TEST_SUITE_P(ThreadedSetTest);

template <typename Set>
void insert(Set& set, std::vector<typename Set::value_type> to_insert,
            std::unordered_map<typename Set::value_type, int>& insertion_counts,
            std::mutex& count_mu) {
  std::vector<bool> insertion_statuses(to_insert.size());
  // Try to insert the selected values and track their results
  for (int i = 0; i < static_cast<int>(to_insert.size()); ++i)
    insertion_statuses[i] = set.insert(to_insert[i]);
  // Update the global insertion counts for the selected values
  std::unique_lock lock{count_mu};
  for (int i = 0; i < static_cast<int>(to_insert.size()); ++i)
    if (insertion_statuses[i]) ++insertion_counts[to_insert[i]];
}

TYPED_TEST_P(ThreadedSetTest, TestThreadedInsertion) {
  using value_type = typename TypeParam::value_type;
  constexpr int insertions_per_thread = 256;
  constexpr int num_threads = 1024;
  // There will be more insertions than there are room for to increase thrashing
  constexpr int set_size = insertions_per_thread * num_threads / 4;

  std::vector<value_type> values(set_size);
  std::generate(values.begin(), values.end(),
                atomic_lookup_internal::make_generator<value_type>());

  std::mt19937 gen(std::random_device{}());  // Standard mersenne_twister_engine
  std::uniform_int_distribution<> index_dist(0, set_size - 1);

  TypeParam set(set_size);
  std::unordered_map<value_type, int> insertion_counts;
  std::mutex count_mu;

  std::vector<std::thread> thread_pool(num_threads);
  std::generate(thread_pool.begin(), thread_pool.end(), [&]() {
    std::vector<value_type> to_insert(insertions_per_thread);
    // Get `insertions_per_thread` random elements to insert
    std::generate(to_insert.begin(), to_insert.end(),
                  [&]() { return values.at(index_dist(gen)); });
    return std::thread(insert<TypeParam>, std::ref(set), std::move(to_insert),
                       std::ref(insertion_counts), std::ref(count_mu));
  });
  for (auto& thread : thread_pool) thread.join();

  // Each key that has been inserted should have been done so once
  for (const auto& val : values) {
    EXPECT_EQ(insertion_counts.count(val), set.contains(val));
    if (insertion_counts.count(val)) {
      EXPECT_EQ(insertion_counts[val], 1);
      EXPECT_TRUE(set.contains(val));
    } else {
      EXPECT_FALSE(set.contains(val));
    }
  }
}

TYPED_TEST_P(ThreadedSetTest, TestInsertion) {
  using value_type = typename TypeParam::value_type;
  constexpr int set_size = 16;
  std::vector<value_type> all_values(set_size * 2);
  std::vector<value_type> samples(set_size);
  std::vector<value_type> controls(set_size);
  std::generate(all_values.begin(), all_values.end(),
                atomic_lookup_internal::make_generator<value_type>());
  // Insert the first half and save the second half as a control
  std::move(all_values.begin(), all_values.begin() + set_size, samples.begin());
  std::move(all_values.begin() + set_size, all_values.end(), controls.begin());

  TypeParam set(set_size);

  for (auto sample : samples) set.insert(std::move(sample));
  for (const auto& sample : samples) {
    EXPECT_TRUE(set.contains(sample));
    EXPECT_EQ(*set.find(sample), sample);
  }
  for (const auto& control : controls) {
    EXPECT_FALSE(set.contains(control));
    EXPECT_EQ(set.find(control), std::nullopt);
  }
}

REGISTER_TYPED_TEST_SUITE_P(ThreadedSetTest, TestThreadedInsertion,
                            TestInsertion);

// Declare set types here
using set_types = ::testing::Types<atomic_set<
    int>, synchronized_set<int, synchronized_value::mutex_strategy>>;
INSTANTIATE_TYPED_TEST_SUITE_P(TSTests, ThreadedSetTest, set_types);

}  // namespace
}  // namespace atomic_lookup
