#
# Copyright 2020 Google LLC.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

package(default_visibility = ["//visibility:public"])

licenses(["notice"])

cc_library(
    name = "synchronized_value",
    hdrs = ["synchronized_value.h"],
    deps = [
        "//third_party/absl/synchronization",
    ],
)

cc_library(
    name = "absl_mutex_strategy",
    hdrs = ["absl_mutex_strategy.h"],
    deps = [
        ":synchronized_value",
        "//third_party/absl/synchronization",
    ],
)

cc_library(
    name = "left_right_strategy",
    hdrs = ["left_right_strategy.h"],
    deps = [
        ":synchronized_value",
        "//third_party/absl/synchronization",
    ],
)

cc_library(
    name = "shared_ptr_rcu_strategy",
    hdrs = ["shared_ptr_rcu_strategy.h"],
    deps = [
        ":synchronized_value",
        "//third_party/absl/synchronization",
    ],
)

cc_test(
    name = "synchronized_value_test",
    srcs = ["synchronized_value_test.cc"],
    deps = [
        ":absl_mutex_strategy",
        ":left_right_strategy",
        ":shared_ptr_rcu_strategy",
        ":synchronized_value",
        "//testing/base/public:gunit_main",
        "//third_party/absl/container:node_hash_map",
    ],
)
