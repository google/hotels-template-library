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

// Haversack is a data-structure designed to make propagating values across a
// statically defined call graph as easy as possible. A Haversack instance holds
// all the types that are explicitly specified for it to use, as well as any
// types any descendant Haversacks need. At the top of the call graph, a root
// Haversack is instantiated with everything needed for the entire call graph,
// which is known at compile time, and then that instance can be propagated down
// the call graph to all consumers.
//
// Example:
//   struct Alpha {};
//   struct Bravo {};
//   struct Charlie {};
//
//   // A Haversack type can be declared as a using declaration, as a subclass,
//   // or just inline. Prefer to use the subclass style when a Haversack to get
//   // better codesearch support and tracebacks; if the Haversack type is only
//   // used in one place, the inline style is also acceptable.
//   using LeafHaversack = Haversack<Alpha>;
//   struct CenterHaversack : Haversack<Bravo, Deps<LeafHaversack>> {
//     using HaversackT::HaversackT;
//   };
//
//   void LeafFlow(LeafHaversack sack) {
//     const Alpha& a = sack.Get<Alpha>();
//   }
//   void CenterFlow(CenterHaversack sack) {
//     const Bravo& b = sack.Get<Bravo>();
//     LeafFlow(sack);
//   }
//   void RootFlow(Haversack<Charlie, CenterHaversack> sack) {
//     const Charlie& c = sack.Get<Charlie>();
//     CenterFlow(sack);
//   }
//
//
//   void main() {
//     RootFlow(Haversack<Calls<RootFlow>>(Alpha{}, Bravo{}, Charlie{}));
//   }
//
//
// Note:
//   Pointers are used ubiquitously in the interface of Haversack and generally
//   speaking raw pointers and smart pointers which can be convered to a
//   SharedProxy are supported. Additionally, wrapped pointer types are
//   supported as long as the wrapping type has the interface "value() &&" which
//   returns the wrapped pointer value. The example wrapping type referenced
//   elsewhere is not_null; e.g. not_null<unique_ptr<T>>.

#ifndef HOTELS_TEMPLATE_LIBRARY_HAVERSACK_HAVERSACK_H_
#define HOTELS_TEMPLATE_LIBRARY_HAVERSACK_HAVERSACK_H_

#include <cassert>
#include <functional>
#include <iostream>
#include <tuple>
#include <type_traits>
#include <utility>

#include "./internal/basic_tuple.h"
#include "./internal/haversack_impl.h"
#include "./internal/type.h"
#include "./internal/type_set.h"
#include "./internal/util.h"

namespace hotels::haversack {

// ImplTs is used in Traits() to compute all the metadata about this
// Haversack.
template <typename... ImplTs>
class Haversack {
  // Normal operation calls one of the normal public ctors, which calls the
  // public but internal ctor which calls the private ctor.
  //
  // Internal ctor sentinal type.
  using CtorSentinel = internal::CtorSentinel;

 public:
  // Get the traits for this Haversack.
  static constexpr auto Traits() {
    return internal::kHaversackTraits<ImplTs...>;
  }

 public:
  // Public alias for the Haversack type.
  using HaversackT = Haversack;

  // Builds a new haversack from members. Each argument must be a pointer (raw,
  // unique, or shared) and may optionally be wrapped in not_null. Each pointer
  // will be converted to a SharedProxy and raw pointers will be converted to
  // non-owning SharedProxys. Arguments may not be nullptr value.
  template <typename... Ptrs,
            typename = std::enable_if_t<
                !internal::FirstIsHaversack<Ptrs...>() &&
                (size(internal::types::Type(Traits().all_deps.Tuple())) == 0 ||
                 sizeof...(Ptrs) >= 1)>,
            typename = std::void_t<internal::PtrElementType<Ptrs>...>>
  explicit Haversack(Ptrs... ptrs)
      : Haversack(
            CtorSentinel(),
            Traits().CtorCompatibility(
                internal::types::MakeTypeSet(),
                internal::MakeBasicTuple(internal::types::Type<
                                         internal::PtrElementType<Ptrs>>()...)),
            std::tuple<>(), internal::AsShared(std::move(ptrs))...) {}

  // Copies an existing haversack and merge new members into it. See the above
  // ctor for documentation on allowed Ptr types.
  //
  // Note: This ctor is only enabled if ptrs is non-empty so we can ignore the
  // warning about explicit unary ctors.
  template <
      typename OtherHaversack, typename... Ptrs,
      typename = std::enable_if_t<
          internal::IsHaversack(internal::types::type_c<OtherHaversack>) &&
          std::greater<>()(sizeof...(Ptrs), 0)>,
      typename = std::void_t<internal::PtrElementType<Ptrs>...>>
  Haversack(OtherHaversack cxt, Ptrs... ptrs)  // NOLINT
      : Haversack(
            CtorSentinel(),
            Traits().CtorCompatibility(
                OtherHaversack::Traits().all_deps,
                internal::MakeBasicTuple(internal::types::Type<
                                         internal::PtrElementType<Ptrs>>()...)),
            std::move(cxt).members_, internal::AsShared(std::move(ptrs))...) {}

  // A Haversack can be converted to any other Haversack type that contains a
  // subset of the member types.
  //
  // Conversion operator is preferred over conversion ctor for the case of
  // converting a Haversack to its subclass.
  //
  // NOTE: This operator is NOT explicit because it models a bespoke type
  // hierarchy (similar to the hierarchy between base-classes and sub-classes).
  template <
      typename OtherHaversack,
      typename = std::enable_if_t<
          internal::IsHaversack(internal::types::type_c<OtherHaversack>) &&
          internal::types::type_c<OtherHaversack> !=
              internal::types::type_c<Haversack> &&
          OtherHaversack::Traits()
              .CtorCompatibility(Traits().all_deps, internal::BasicTuple<>())
              .IsCompatible()>>
  operator OtherHaversack() && {  // NOLINT
    return OtherHaversack(CtorSentinel(),
                          OtherHaversack::Traits().CtorCompatibility(
                              Traits().all_deps, internal::BasicTuple<>()),
                          std::move(members_));
  }
  template <
      typename OtherHaversack,
      typename = std::enable_if_t<
          internal::IsHaversack(internal::types::type_c<OtherHaversack>) &&
          internal::types::type_c<OtherHaversack> !=
              internal::types::type_c<Haversack> &&
          OtherHaversack::Traits()
              .CtorCompatibility(Traits().all_deps, internal::BasicTuple<>())
              .IsCompatible()>>
  operator OtherHaversack() const& {  // NOLINT
    auto self = *this;
    return static_cast<OtherHaversack>(std::move(self));
  }

  // Get shared ownership to the T member. The returned value is never null.
  //
  // Prefer the Get method unless the shared ownership is required.
  template <typename T, typename GetType = internal::GetType<Haversack, T>>
  [[nodiscard]] const internal::SharedProxy<GetType>& GetShared() const {
    const auto& value = std::get<internal::SharedProxy<GetType>>(members_);
    // Non-nullness invariant is enforced by the ctor in production, so this
    // assert exists to prevent segfaults in tests.
#ifndef NDEBUG
    if (!value) {
      std::cerr << "A value for \""
                << internal::debug_type_name_v<T> << "\" was not "
                << "injected with MakeFakeHaversack but is used in this test.";
      assert(value);
      abort();
    }
#endif
    return value;
  }

  // Gets a reference to the T member.
  //
  // If T matches KnownThreadSafe<U>, a mutable `U&` is returned;
  // otherwise a `const T&` reference is returned.
  template <typename T, typename GetType = internal::GetType<Haversack, T>>
  [[nodiscard]] GetType& Get() const {
    return *GetShared<T>();
  }

  // Gets a new haversack without the Ts types.
  template <typename... Ts>
  [[nodiscard]] auto Without() && {
    constexpr auto removes = MakeTypeSet(internal::types::type_c<Ts>...);
    static_assert(sizeof...(Ts) > 0,
                  "Must specify at least one type to remove.");
    static_assert(size(removes.Tuple()) == sizeof...(Ts),
                  "Types to remove must all be unique.");
    static_assert(
        Traits().all_deps >= removes,
        "The original haversack must contain all the types being removed.");
    return typename decltype(internal::types::FromTuple<Haversack>(
        (Traits().all_deps - removes).Tuple()))::type(std::move(*this));
  }
  template <typename... Ts>
  [[nodiscard]] auto Without() const& {
    auto self = *this;  // Make a copy of this.
    return std::move(self).template Without<Ts...>();
  }

  // Gets a new haversack with the ptrs added. This method takes ptrs as pointer
  // types similar to the Haversack ctor.
  //
  // ExplicitTs are optional, and if ExplicitTs are provided, ptrs will be
  // converted to the explicit types.  Otherwise if ExplicitTs are not provided,
  // the types to be added to the Haversack are deduced from the ptrs and
  // assumed to be non-mutable.
  template <
      typename... ExplicitTs, typename... Ptrs,
      typename = std::enable_if_t<sizeof...(ExplicitTs) == sizeof...(Ptrs)>>
  [[nodiscard]] auto Insert(Ptrs... ptrs) && {
    return std::move(*this).template ExplicitInsertImpl<ExplicitTs...>(
        internal::AsShared(std::move(ptrs))...);
  }
  template <
      typename... ExplicitTs, typename... Ptrs,
      typename = std::enable_if_t<sizeof...(ExplicitTs) == sizeof...(Ptrs)>>
  [[nodiscard]] auto Insert(Ptrs&&... ptrs) const& {
    auto self = *this;  // Make a copy of this.
    return std::move(self).template Insert<ExplicitTs...>(
        std::forward(ptrs)...);
  }
  template <typename... Ptrs>
  [[nodiscard]] auto Insert(Ptrs... ptrs) && {
    return std::move(*this).InsertImpl(internal::AsShared(std::move(ptrs))...);
  }
  template <typename... Ptrs>
  [[nodiscard]] auto Insert(Ptrs&&... ptrs) const& {
    auto self = *this;  // Make a copy of this.
    return std::move(self).Insert(std::forward(ptrs)...);
  }

  // Replace a value in the haversack with a new one of the same type.
  template <typename T = void, typename Arg>
  [[nodiscard]] Haversack Replace(Arg arg) && {
    using TypeToReplace = std::conditional_t<
        std::is_same_v<T, void>,
        typename decltype(internal::MakeKnownThreadSafe(
            Traits()
                .CtorCompatibility(internal::types::MakeTypeSet(),
                                   internal::BasicTuple<>())
                .template FindMatch(internal::types::type_c<
                                    internal::PtrElementType<Arg>>)))::type,
        T>;
    using ConstType = typename decltype(internal::ThreadSafeType(
        internal::types::type_c<TypeToReplace>))::type;
    static_assert(
        Contains(internal::types::type_c<TypeToReplace>, Traits().all_deps));
    std::get<internal::SharedProxy<ConstType>>(members_) =
        internal::AsShared(std::move(arg));
    return std::move(*this);
  }
  template <typename T = void, typename Arg>
  [[nodiscard]] Haversack Replace(Arg arg) const& {
    auto self = *this;  // Make a copy of this.
    return std::move(self).template Replace<T>(std::move(arg));
  }

  // Internal use only.
  template <typename... AddedTs, typename Compatibility,
            typename PropagatedTuple>
  explicit Haversack(CtorSentinel, Compatibility compatibility,
                     PropagatedTuple pt, AddedTs... added)
      : members_(internal::CatAndSortTuples(Traits().MemberTupleType(),
                                            compatibility, std::move(pt),
                                            std::move(added)...)) {
    auto checks = Traits().AssertIsValidHaversack(compatibility);
    RunChecks(checks);
  }

 private:
  // Assert that all Ts can be added to this haversack.
  template <typename... Ts>
  static constexpr void AssertAdds() {
    constexpr auto adds = MakeTypeSet(internal::types::type_c<Ts>...);
    static_assert(sizeof...(Ts) > 0, "Must add at least one type.");
    static_assert(size((adds & Traits().all_deps).Tuple()) == 0,
                  "All the added types must not be in the haversack already.");
  }

  template <typename... Ts>
  static constexpr void RunChecks(
      internal::BasicTuple<internal::types::Type<Ts>...>) {
    (Ts::template Check<Ts>(), ...);
  }

  // If no ExplicitTs were passed to Insert, deduce the types from the
  // arguments and pass it to InsertImpl as the ExplicitTs.
  template <typename... Ts>
  auto InsertImpl(internal::SharedProxy<Ts>... ts) && {
    return std::move(*this)
        .template ExplicitInsertImpl<std::remove_const_t<Ts>...>(
            std::move(ts)...);
  }

  template <typename... ExplicitTs, typename... Ts,
            typename = std::enable_if_t<sizeof...(ExplicitTs) == sizeof...(Ts)>>
  auto ExplicitInsertImpl(internal::SharedProxy<Ts>... ts) && {
    AssertAdds<ExplicitTs...>();
    using OtherHaversack = Haversack<ImplTs..., ExplicitTs...>;
    return OtherHaversack(
        CtorSentinel(),
        OtherHaversack::Traits().CtorCompatibility(
            Traits().all_deps | internal::types::MakeTypeSet(
                                    internal::types::type_c<ExplicitTs>...),
            internal::BasicTuple<>()),
        std::tuple_cat(
            std::move(members_),
            std::make_tuple(typename decltype(internal::types::FromTuple<
                                              internal::SharedProxy>(
                internal::MakeBasicTuple(internal::ThreadSafeType(
                    internal::types::type_c<
                        ExplicitTs>))))::type(std::move(ts))...)));
  }

  template <typename... Ts>
  friend class Haversack;
  template <typename T>
  friend constexpr auto internal::TraitsOf(internal::types::Type<T>);
  friend internal::HaversackTestUtil;

  typename decltype(Traits().MemberTupleType())::type members_;
};

// Calls is used to indicate to the Haversack any relevant functions that are
// called using it.
//
// Deps is used to indicate to the Haversack any dependent Haversacks that are
// further down the call chain.
//
// Example:
//   struct Alpha {};
//   struct Bravo {};
//
//   using OtherHaversack = Haversack<Bravo>;
//
//   void MyOtherFlow(Haversack<Bravo> sack);
//
//   void MyCallsFlow(Haversack<Bravo, Calls<MyOtherFlow>>);
//   void MyDepsFlow(Haversack<Bravo, Deps<OtherHaversack>);
//
//
//   void main() {
//     // MyCallsFlow's Haversack requires Alpha and Bravo because it knew about
//     // MyOtherFlow's Haversack dependencies from Calls.
//     MyCallsFlow(Haversack<Alpha, Bravo>(Alpha{}, Bravo{}));
//
//     // MyDepsFlow's Haversack requires Alpha and Bravo because it knew about
//     // MyOtherFlow's Haversack dependencies from Deps.
//     MyDepsFlow(Haversack<Alpha, Bravo>(Alpha{}, Bravo{}));
//   }
template <auto&&... xs>
struct Calls : internal::CallsBase {
  using types = internal::BasicTuple<internal::types::Type<decltype(xs)>...>;
};
template <typename...>
struct Deps {};

// Provides is used to indicate to the Haversack that this Haversack will inject
// the provided types and that any Haversacks higher in the call chain do not
// need to.
//
// Example:
//   struct Alpha {};
//   struct Bravo {};
//
//   using OtherHaversack = Haversack<Bravo, Provides<Bravo>>;
//
//   void MyOtherFlow(Haversack<Bravo> sack);
//
//   void MyFlow(Haversack<Bravo, Calls<MyOtherFlow>>);
//

//   void main() {
//     // MyFlow's Haversack requires only Alpha because it knows about
//     // OtherHaversack's dependencies but OtherHaversack Provides its own
//     // Bravo.
//     MyFlow(Haversack<Alpha>(Alpha{}));
//   }
template <typename...>
struct Provides {};

// KnownThreadSafe designates that a type in the Haversack should be non-const.
// This is treated as a completely distinct type in the Haversack. This should
// only be used if the type in question is thread-safe.
//
// Example:
//   struct Alpha {
//     void ThreadSafeMethod() {}
//   };
//
//   void main() {
//     Haversack<KnownThreadSafe<Alpha>> sack(std::make_shared<Alpha>());
//     sack.Get<KnownThreadSafe<Alpha>>().ThreadSafeMethod();
//   }
template <typename>
struct KnownThreadSafe {};

}  // namespace hotels::haversack

#endif  // HOTELS_TEMPLATE_LIBRARY_HAVERSACK_HAVERSACK_H_
