// Copyright 2021 Google LLC
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

#ifndef HOTELS_TEMPLATE_LIBRARY_HAVERSACK_HAVERSACK_TEST_UTIL_H_
#define HOTELS_TEMPLATE_LIBRARY_HAVERSACK_HAVERSACK_TEST_UTIL_H_

#include <type_traits>

#include "haversack/haversack.h"
#include "haversack/internal/basic_tuple.h"

namespace hotels::haversack {
namespace internal {

struct FakeCompatibility {
  static constexpr BasicTuple<> GetMatchChecks() { return BasicTuple<>(); }
  static constexpr bool IsCompatible() { return true; }
};

// friend declaration requires this definition is in the internal namespace.
struct HaversackTestUtil {
  template <typename... HaversackTs, typename... Args,
            typename = std::enable_if_t<
                sizeof...(HaversackTs) != 0 &&
                (... && internal::IsHaversack(types::type_c<HaversackTs>))>>
  static auto MakeFakeHaversack(Args&&... args) {
    // Haversack containing only the direct dependencies to HaversackT.
    constexpr auto direct_type_set =
        (... | TraitsOf(types::type_c<HaversackTs>).direct);
    using RequiredTypesHaversack =
        typename decltype(types::FromTuple<Haversack>(
            direct_type_set.Tuple()))::type;
    auto direct_deps_haversack =
        RequiredTypesHaversack(std::forward<Args>(args)...);

    constexpr auto all_deps_type_set =
        (... | TraitsOf(types::type_c<HaversackTs>).all_deps);
    using AllTypesHaversack = typename decltype(types::FromTuple<Haversack>(
        all_deps_type_set.Tuple()))::type;
    // Instance of AllTypesHaversack, initially only containing nullptrs.
    auto result = AllTypesHaversack(
        internal::CtorSentinel(), FakeCompatibility(),
        typename decltype(TraitsOf(types::type_c<AllTypesHaversack>)
                              .MemberTupleType())::type());

    // Copy direct dependencies over from direct_deps_haversack into result.
    Apply(
        // Capture by pointer because Apply does not support mutable invocables.
        [result_ptr = &result,
         direct_ptr = &direct_deps_haversack](auto... types) {
          ((get<typename decltype(types)::type>(result_ptr->members_) =
                std::move(
                    get<typename decltype(types)::type>(direct_ptr->members_))),
           ...);
        },
        types::AsTuple(types::Type(direct_deps_haversack.members_)));
    return result;
  }
};

}  // namespace internal

// Create an instance of a Haversack for testing. It only contains direct
// dependencies, so accessing any indirect dependencies (e.g. by propagating the
// Haversack) will cause a runtime error.
//
// Args are used to populate the direct dependencies of the Haversack and use
// the same conversion rules as regular Haversack construction.
//
// This is useful when part of the call chain is mocked so that the dependencies
// only needed in that subgraph of the call chain don't need to be instaniated
// in testing.
//
// Multiple Haversack types can be specified which allows some Haversack
// propagation as long as there is a hierarchy between the types.
template <typename... HaversackTs, typename... Args>
auto MakeFakeHaversack(Args&&... args) {
  return internal::HaversackTestUtil::MakeFakeHaversack<HaversackTs...>(
      std::forward<Args>(args)...);
}

}  // namespace hotels::haversack

#endif  // HOTELS_TEMPLATE_LIBRARY_HAVERSACK_HAVERSACK_TEST_UTIL_H_
