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

#include <memory>
#include <type_traits>

#include "haversack/haversack.h"
#include "haversack/internal/haversack_impl.h"
#include "meta/basic_tuple.h"
#include "meta/type.h"
#include "meta/type_set.h"

#ifndef HAVERSACK_GET_TESTER_MODE
// If HAVERSACK_GET_TESTER_MODE is unset, use the default of 0 and unset it
// again at the end of this file.
#define HAVERSACK_GET_TESTER_MODE 0
#define UNSET_HAVERSACK_GET_TESTER_MODE
#endif

namespace hotels::haversack {
namespace internal {

struct FakeCompatibilityTag;

template <>
struct CompatibleArgs<FakeCompatibilityTag, FakeCompatibilityTag> {
  static constexpr htls::meta::BasicTuple<> GetMatchChecks() {
    return htls::meta::BasicTuple<>();
  }
  static constexpr bool IsCompatible() { return true; }
};

// friend declaration requires this definition is in the internal namespace.
struct HaversackTestUtil {
 private:
  template <typename HaversackT, typename... SubHaversackTs,
            typename... CoercedCtorArgs>
    requires(HaversackInstance<HaversackT> &&
             (HaversackInstance<SubHaversackTs> && ...) &&
             (CoercedCtorArgC<CoercedCtorArgs> && ...))
  static htls::meta::Concept<
      htls::meta::IsTemplateInstance<htls::meta::Type>> auto
  ValidateArgs(htls::meta::Type<CoercedCtorArgs>...) {
    constexpr auto direct_type_set =
        TraitsOf(htls::meta::type_c<HaversackT>).direct |
        (htls::meta::MakeTypeSet() | ... |
         (TraitsOf(htls::meta::type_c<SubHaversackTs>).direct &
          TraitsOf(htls::meta::type_c<HaversackT>).all_deps));
    using RequiredTypesHaversack =
        typename decltype(htls::meta::FromTuple<Haversack>(
            direct_type_set.Tuple()))::type;
    constexpr CompatibleArgs compatibility =
        RequiredTypesHaversack::Traits().CtorCompatibility(
            htls::meta::MakeTypeSet(),
            htls::meta::MakeBasicTuple(htls::meta::type_c<CoercedCtorArgs>...));
    RequiredTypesHaversack::RunChecks(compatibility.GetMatchChecks());
    if constexpr (compatibility.IsCompatible()) {
      return htls::meta::type_c<RequiredTypesHaversack>;
    } else {
      return htls::meta::type_c<void>;
    }
  }

 public:
  template <typename HaversackT, typename... SubHaversackTs, typename... Args,
            typename RequiredTypesHaversack =
                typename decltype(ValidateArgs<HaversackT, SubHaversackTs...>(
                    htls::meta::type_c<CoercedCtorArg<Args>>...))::type>
    requires(HaversackInstance<HaversackT> &&
             (HaversackInstance<SubHaversackTs> && ...) &&
             !std::is_void_v<RequiredTypesHaversack>)
  static HaversackT MakeFakeHaversack(Args&&... args) {
    using AllDepsHaversack = typename decltype(htls::meta::FromTuple<Haversack>(
        TraitsOf(htls::meta::type_c<HaversackT>).all_deps.Tuple()))::type;
    using AllDepsTuple =
        typename decltype(TraitsOf(htls::meta::type_c<AllDepsHaversack>)
                              .MemberTupleType())::type;
    // Instance of AllDepsHaversack, initially only containing nullptrs.
    AllDepsHaversack result(
        internal::CtorSentinel(),
        CompatibleArgs<FakeCompatibilityTag, FakeCompatibilityTag>(),
        htls::meta::Apply(
            []<typename... Ts>(htls::meta::Type<Ts>... ts) {
              return std::make_shared<const AllDepsTuple>(
                  Ts(SecurityBadge<HaversackTestUtil>())...);
            },
            AsTuple(htls::meta::type_c<AllDepsTuple>)));

#if HAVERSACK_GET_TESTER_MODE > 0
    return result;
#else

    // Haversack containing only the direct dependencies to HaversackT.
    // Also, any direct dependencies in SubHaversackTs that are in HaversackT.
    // This specifically doesn't include dependencies that are Provided
    // somewhere.
    auto direct_deps_haversack =
        RequiredTypesHaversack(std::forward<Args>(args)...);

    // Pointer to non-const `members_` type by copy.
    std::shared_ptr<typename decltype(TraitsOf(
                                          htls::meta::type_c<AllDepsHaversack>)
                                          .MemberTupleType())::type>
        new_members = std::make_shared<
            typename decltype(TraitsOf(htls::meta::type_c<AllDepsHaversack>)
                                  .MemberTupleType())::type>(*result.members_);
    // Copy direct dependencies over from direct_deps_haversack into result.
    htls::meta::Apply(
        // Capture by pointer because Apply does not support mutable
        // invocables.
        [&new_members, &direct_deps_haversack](auto... types) {
          ((std::get<typename decltype(types)::type>(*new_members) =
                std::move(std::get<typename decltype(types)::type>(
                    *direct_deps_haversack.members_))),
           ...);
        },
        htls::meta::AsTuple(htls::meta::Type(*direct_deps_haversack.members_)));
    result.members_ = std::move(new_members);
    return result;
#endif
  }

  template <typename HaversackT>
  static const auto& InternalGetMembers(const HaversackT& sack) {
    return sack.members_;
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
// only needed in that subgraph of the call chain don't need to be instantiated
// in testing.
//
// Multiple Haversack htls::meta can be specified which allows some Haversack
// propagation as long as there is a hierarchy between the htls::meta.
template <typename HaversackT, typename... SubHaversackTs, typename... Args>
  requires(internal::HaversackInstance<HaversackT> &&
           (internal::HaversackInstance<SubHaversackTs> && ...))
HaversackT MakeFakeHaversack(Args&&... args) {
  return internal::HaversackTestUtil::MakeFakeHaversack<HaversackT,
                                                        SubHaversackTs...>(
      std::forward<Args>(args)...);
}

// Get any dependency out of a Haversack. This technique is doable in non-test
// code, but there isn't a good use case to make it easier outside of tests.
//
// Example:
//  struct Alpha {};
//  using Bar = Haversack<Alpha>;
//  using Foo = Haversack<Deps<Bar>>;
//
//  Foo foo{std::make_shared<Alpha>()};
//
//  // Cannot get Alpha from Foo because it is not a direct dependency.
//  // foo.Get<Alpha>();
//  const Alpha& alpha = *IndirectGetShared<Alpha>(foo);
template <typename T>
decltype(auto) IndirectGetShared(const Haversack<T>& dependencies) {
  return dependencies.template GetShared<T>();
}

}  // namespace hotels::haversack

#ifdef UNSET_HAVERSACK_GET_TESTER_MODE
#undef HAVERSACK_GET_TESTER_MODE
#undef UNSET_HAVERSACK_GET_TESTER_MODE
#endif

#endif  // HOTELS_TEMPLATE_LIBRARY_HAVERSACK_HAVERSACK_TEST_UTIL_H_
