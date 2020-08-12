cc_library(
    name = "atomic_set",
    hdrs = ["atomic_set.h"],
)

cc_library(
    name = "atomic_map",
    hdrs = ["atomic_map.h"],
    deps = ["atomic_set"],
)

cc_library(
    name = "synchronized_set",
    hdrs = ["synchronized_set.h"],
    deps = ["//synchronized-value:synchronized_value"],
)

cc_library(
    name = "synchronized_map",
    hdrs = ["synchronized_map.h"],
    deps = ["//synchronized-value:synchronized_value"],
)

cc_test(
    name = "threaded_set_test",
    srcs = ["threaded_set_test.cc"],
    copts = ["-Wno-unused-parameter", "-Wno-gnu-zero-variadic-macro-arguments"],
    deps = [
        ":atomic_set",
        ":synchronized_set",
        "//synchronized-value:mutex_strategy",
        "@com_google_googletest//:gtest_main",
    ],
)

cc_binary(
    name = "threaded_set_benchmark",
    testonly = True,
    srcs = ["threaded_set_benchmark.cc"],
    deps = [
        ":atomic_set",
        ":synchronized_set",
        "//synchronized-value:mutex_strategy",
        "//synchronized-value:absl_mutex_strategy",
        "//synchronized-value:rw_mutex_strategy",
        "//synchronized-value:shared_ptr_rcu_strategy",
        "//synchronized-value:left_right_strategy",
        "@com_google_benchmark//:benchmark_main",
    ],
)
