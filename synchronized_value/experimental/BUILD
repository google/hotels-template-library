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
    copts = [
        "-Wno-ctad-maybe-unsupported",
    ],
)

cc_test(
    name = "synchronized_value_test",
    srcs = ["synchronized_value_test.cc"],
    copts = [
        "-Wno-unused-parameter",
        "-Wno-gnu-zero-variadic-macro-arguments",
    ],
    deps = [
        ":synchronized_value",
        "//testing/base/public:gunit_main",
        "//third_party/absl/container:node_hash_map",
        "//third_party/hotels_template_library/synchronized_value:mutex_strategy",
        "//third_party/hotels_template_library/synchronized_value:rw_mutex_strategy",
    ],
)
