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

#include <algorithm>
#include <random>
#include <thread>
#include <vector>

#include "benchmark/benchmark.h"

#include "append-only-lookup/atomic_set.h"
#include "append-only-lookup/synchronized_set.h"
#include "synchronized-value/mutex_strategy.h"
#include "synchronized-value/rw_mutex_strategy.h"
#include "synchronized-value/shared_ptr_rcu_strategy.h"
#include "synchronized-value/left_right_strategy.h"
#include "synchronized-value/absl_mutex_strategy.h"

template <typename SetType>
void BM_Continuous(benchmark::State& state) {
  const auto total_ops = state.range(0);
  const auto ops_per_thread = total_ops / state.threads;

  const auto total_writes = state.range(1);
  const auto writes_per_thread = total_writes / state.threads;
  const auto write_period = ops_per_thread / writes_per_thread;

  const auto set_size = static_cast<int>(state.range(2));

  // Set up global randomization for read / write values
  std::mt19937 gen(std::random_device{}());  // Standard mersenne_twister_engine
  std::uniform_int_distribution<> dist(1, set_size);

  static SetType set(set_size);
  for (auto _ : state) {
    for (int op_num = 0; op_num < ops_per_thread; op_num++) {
      const int val = dist(gen);
      if (op_num % write_period == 0)
        set.insert(val);
      else
        benchmark::DoNotOptimize(set.contains(val));
    }
  }

  state.SetItemsProcessed(total_ops);
}

#define BENCHMARK_SET(set_type)             \
BENCHMARK_TEMPLATE(BM_Continuous, set_type) \
    ->UseRealTime()                         \
    ->Threads(1)                            \
    ->Threads(2)                            \
    ->Threads(3)                            \
    ->Threads(4)                            \
    ->Threads(6)                            \
    ->Threads(8)                            \
    ->Threads(12)                           \
    ->Threads(16)                           \
    ->Threads(32)                           \
    ->Threads(64)                           \
    ->Threads(128)                          \
    ->Ranges({{1 << 20, 1 << 20}, {128, 128}, {1024, 1024}});

template <typename T>
using mutex_set = append_only_lookup::synchronized_set<T, synchronized_value::mutex_strategy>;

template <typename T>
using rw_mutex_set = append_only_lookup::synchronized_set<T, synchronized_value::rw_mutex_strategy>;

template <typename T>
using smartptr_rcu_set = append_only_lookup::synchronized_set<T, synchronized_value::shared_ptr_rcu_strategy>;

template <typename T>
using left_right_set = append_only_lookup::synchronized_set<T, synchronized_value::left_right_strategy>;

template <typename T>
using absl_mutex_set = append_only_lookup::synchronized_set<T, synchronized_value::absl_mutex_strategy>;

using append_only_lookup::atomic_set;

BENCHMARK_SET(mutex_set<int>);
BENCHMARK_SET(rw_mutex_set<int>);
BENCHMARK_SET(smartptr_rcu_set<int>);
BENCHMARK_SET(left_right_set<int>);
BENCHMARK_SET(atomic_set<int>);
BENCHMARK_SET(absl_mutex_set<int>);
