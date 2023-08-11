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
//   The following names are effectively C++20 concepts and used conventionally
//   here to indicate the kind of type these names represent.
//   - UncoercedCtorArg is any type that is accepted as input to the Haversack
//     ctor or one of its methods. Raw pointers, unique_ptr, shared_ptr,
//     not_null (see *Note below), and Tagged instances are all valid.
//   - CtorArg is any UncoercedCtorArg after it is transformed by CoerceCtorArg.
//     These are a SharedProxy or a Tagged instance.
//   - Tag is any type which is being used as the tag for a Tagged instance.
//   - DisplayableCtorArg is a user friendly version of a CtorArg, the
//     element_type if the CtorArg is a pointer or the CtorArg itself if the
//     CtorArg is a Tagged.
//   - WrappedType is any type that could be used in a Haversack declaration
//     including wrappers, e.g. KnownThreadSafe<Arena>.
//   - UnwrappedType is a WrappedType that has been converted by
//     kUnwrapTypeWrappers.
//
//   * Note: Any pointer wrapping type that has an "element_type" type member
//     and a "value() &&" method which returns a pointer are supported as
//     UncoercedCtorArg.
//   // TODO(c++20): Make proper concepts.

#ifndef HOTELS_TEMPLATE_LIBRARY_HAVERSACK_HAVERSACK_H_
#define HOTELS_TEMPLATE_LIBRARY_HAVERSACK_HAVERSACK_H_

#include <cassert>
#include <iostream>
#include <tuple>
#include <type_traits>

#include "haversack/internal/haversack_impl.h"
#include "meta/basic_tuple.h"
#include "meta/type.h"
#include "meta/type_set.h"

#ifndef HTLS_NO_DUNDER_ATTRIBUTE

#define HTLS_UNAVAILABLE_ATTRIBUTE(...) \
  __attribute__((unavailable(__VA_ARGS__)))
#define HTLS_ENABLE_IF_ATTRIBUTE(...) __attribute__((enable_if(__VA_ARGS__)))

#else

#define HTLS_UNAVAILABLE_ATTRIBUTE(...) = delete;
#define HTLS_ENABLE_IF_ATTRIBUTE(...)

#endif

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
    return internal::TraitsOf(htls::meta::type_c<Haversack>);
  }
  // Eagerly initialize Traits() for each haversack at compile-time and verify
  // there were no issues.
  static_assert(!std::is_same_v<decltype(Traits()), void>,
                "There was an issue building the haversack traits.");

  // Public alias for the Haversack type.
  using HaversackT = Haversack;

  // Default ctor only for empty haversacks.
  explicit Haversack()
    requires(size(htls::meta::Type(Traits().all_deps.Tuple())) == 0)
      : Haversack(CtorSentinel(),
                  Traits().CtorCompatibility(htls::meta::MakeTypeSet(),
                                             htls::meta::MakeBasicTuple()),
                  std::tuple<>()) {}

  // Builds a new haversack from members.
  template <internal::UncoercedCtorArg... UncoercedCtorArgs>
    requires(sizeof...(UncoercedCtorArgs) > 0)
  explicit Haversack(UncoercedCtorArgs... args)
      : Haversack(CtorSentinel(),
                  Traits().CtorCompatibility(
                      htls::meta::MakeTypeSet(),
                      htls::meta::MakeBasicTuple(
                          htls::meta::type_c<
                              internal::CoercedCtorArg<UncoercedCtorArgs>>...)),
                  std::tuple<>(), internal::CoerceCtorArg(std::move(args))...) {
  }

  // Copies an existing haversack and merge new members into it.
  //
  // Note: This ctor is only enabled if args is non-empty so we
  // can ignore the warning about explicit unary ctors.
  template <internal::HaversackInstance OtherHaversack,
            internal::UncoercedCtorArg... UncoercedCtorArgs>
    requires(sizeof...(UncoercedCtorArgs) > 0)
  Haversack(OtherHaversack cxt,
            UncoercedCtorArgs... args)  // NOLINT
      : Haversack(CtorSentinel(),
                  Traits().CtorCompatibility(
                      OtherHaversack::Traits().all_deps,
                      htls::meta::MakeBasicTuple(
                          htls::meta::type_c<
                              internal::CoercedCtorArg<UncoercedCtorArgs>>...)),
                  *std::move(cxt).members_,
                  internal::CoerceCtorArg(std::move(args))...) {}

  // Honeypot overload for when the arguments are (incorrectly) not pointers.
  // We use the 'unavailable' attribute to give a better compiler error than
  // just 'method is deleted'.
  // TODO(cmgp): consider removing this overload now that concepts give better
  // error messages.
  template <typename First, typename... Rest,
            bool kAnyInvalidCtorArgs =
                // Any of the "rest" of the arguments are not valid,
            (... || !internal::kIsValidCtorArg<Rest>) ||
            // or the first argument is not a valid pointer and not a valid
            // Haversack.
            (!internal::kIsValidCtorArg<First> &&
             !internal::IsHaversack(htls::meta::type_c<First>)),
            typename = std::enable_if_t<kAnyInvalidCtorArgs>>
  explicit Haversack(First, Rest...)
      HTLS_UNAVAILABLE_ATTRIBUTE(
          "One of the Haversack constructor arguments wasn't a pointer "
          "when it should have been (or it wasn't a supported pointer).");

  // A Haversack can be converted to any other Haversack type that contains a
  // subset of the member types.
  //
  // Conversion operator is preferred over conversion ctor for the case of
  // converting a Haversack to its subclass.
  //
  // NOTE: This operator is NOT explicit because it models a bespoke type
  // hierarchy (similar to the hierarchy between base-classes and sub-classes).
  template <internal::HaversackInstance OtherHaversack>
    requires(htls::meta::type_c<OtherHaversack> !=
                 htls::meta::type_c<Haversack> &&
             OtherHaversack::Traits()
                 .CtorCompatibility(Traits().all_deps,
                                    htls::meta::BasicTuple<>())
                 .IsCompatible())
  operator OtherHaversack() && {  // NOLINT
    return OtherHaversack(CtorSentinel(),
                          OtherHaversack::Traits().CtorCompatibility(
                              Traits().all_deps, htls::meta::BasicTuple<>()),
                          *std::move(members_));
  }
  template <internal::HaversackInstance OtherHaversack>
    requires(htls::meta::type_c<OtherHaversack> !=
                 htls::meta::type_c<Haversack> &&
             OtherHaversack::Traits()
                 .CtorCompatibility(Traits().all_deps,
                                    htls::meta::BasicTuple<>())
                 .IsCompatible())
  operator OtherHaversack() const& {  // NOLINT
    Haversack self = *this;
    return static_cast<OtherHaversack>(std::move(self));
  }

  // Get shared ownership to the T member. The returned value is never null.
  //
  // Prefer the Get method unless the shared ownership is required.
  template <typename T,
            typename GetSharedHelper = internal::GetSharedHelper<T, Haversack>>
  [[nodiscard]] decltype(auto) GetShared() const HTLS_ENABLE_IF_ATTRIBUTE(
      GetSharedHelper::GetMatchingWrappedType() != htls::meta::type_c<void>,
      "Requested type is not a direct dependency in the Haversack.") {
    return GetSharedHelper::Get(*members_);
  }

  // Gets a reference to the T member.
  //
  // If T matches KnownThreadSafe<U>, a mutable `U&` is returned;
  // otherwise a `const T&` reference is returned.
  template <typename T,
            typename GetSharedHelper = internal::GetSharedHelper<T, Haversack>>
  [[nodiscard]] decltype(auto) Get() const HTLS_ENABLE_IF_ATTRIBUTE(
      GetSharedHelper::GetMatchingWrappedType() != htls::meta::type_c<void>,
      "Requested type is not a direct dependency in the Haversack.") {
    if constexpr (internal::IsNullable(
                      GetSharedHelper::GetMatchingWrappedType())) {
      return GetShared<T>().get();
    } else {
      return *GetShared<T>();
    }
  }

  // Gets a new haversack without the WrappedTypes types.
  //
  // Prefer to use the Haversack ctor if the final Haversack type is known.
  template <typename... WrappedTypes>
  [[nodiscard]] auto Without() && {
    constexpr htls::meta::TypeSet removes =
        MakeTypeSet(htls::meta::type_c<WrappedTypes>...);
    static_assert(sizeof...(WrappedTypes) > 0,
                  "Must specify at least one type to remove.");
    static_assert(size(removes.Tuple()) == sizeof...(WrappedTypes),
                  "Types to remove must all be unique.");
    static_assert(
        Traits().all_deps >= removes,
        "The original haversack must contain all the types being removed.");
    return typename decltype(htls::meta::FromTuple<Haversack>(
        (Traits().all_deps - removes).Tuple()))::type(std::move(*this));
  }
  template <typename... WrappedTypes>
  [[nodiscard]] auto Without() const& {
    Haversack self = *this;  // Make a copy of this.
    return std::move(self).template Without<WrappedTypes...>();
  }

  // Gets a new haversack with the args added. This method takes
  // the same kind of arguments as the Haversack ctor.
  //
  // Prefer to use the Haversack ctor if the final Haversack type is known.
  //
  // ExplicitWrappedTypes are optional, and if ExplicitWrappedTypes are
  // provided, args will be converted to the explicit types.
  // Otherwise if ExplicitWrappedTypes are not provided, the types to be added
  // to the Haversack are deduced from the args and assumed to be
  // non-mutable.
  template <typename... ExplicitWrappedTypes,
            internal::UncoercedCtorArg... UncoercedCtorArgs>
    requires(sizeof...(ExplicitWrappedTypes) == sizeof...(UncoercedCtorArgs))
  [[nodiscard]] auto Insert(UncoercedCtorArgs... args) && {
    return std::move(*this)
        .template ExplicitInsertImpl<ExplicitWrappedTypes...>(
            internal::CoerceCtorArg(std::move(args))...);
  }
  template <typename... ExplicitWrappedTypes,
            internal::UncoercedCtorArg... UncoercedCtorArgs>
    requires(sizeof...(ExplicitWrappedTypes) == sizeof...(UncoercedCtorArgs))
  [[nodiscard]] auto Insert(UncoercedCtorArgs... args) const& {
    Haversack self = *this;  // Make a copy of this.
    return std::move(self).template Insert<ExplicitWrappedTypes...>(
        std::move(args)...);
  }
  template <internal::UncoercedCtorArg... UncoercedCtorArgs>
  [[nodiscard]] auto Insert(UncoercedCtorArgs... args) && {
    return std::move(*this).InsertImpl(
        internal::CoerceCtorArg(std::move(args))...);
  }
  template <internal::UncoercedCtorArg... UncoercedCtorArgs>
  [[nodiscard]] auto Insert(UncoercedCtorArgs... args) const& {
    Haversack self = *this;  // Make a copy of this.
    return std::move(self).Insert(std::move(args)...);
  }

  // Replace a value in the haversack with a new one of the same type.
  template <typename ExplicitWrappedType = void,
            internal::UncoercedCtorArg UncoercedCtorArg>
  [[nodiscard]] Haversack Replace(UncoercedCtorArg arg) && {
    // If an ExplicitWrappedType is provided, ExplicitWrappedType is used as the
    // type to replace, otherwise the type to replace is deduced from the type
    // of the argument.
    using TypeToReplace = std::conditional_t<
        std::is_same_v<ExplicitWrappedType, void>,
        typename decltype(Traits()
                              .CtorCompatibility(htls::meta::MakeTypeSet(),
                                                 htls::meta::BasicTuple<>())
                              .template FindMatch(
                                  htls::meta::type_c<
                                      internal::CoercedCtorArg<
                                          UncoercedCtorArg>>))::type,
        ExplicitWrappedType>;
    static_assert(
        Contains(htls::meta::type_c<TypeToReplace>, Traits().all_deps));
    std::get<internal::Holder<TypeToReplace>>(*members_).value =
        internal::CoerceCtorArg(std::move(arg));
    return std::move(*this);
  }
  template <typename ExplicitWrappedType = void,
            internal::UncoercedCtorArg UncoercedCtorArg>
  [[nodiscard]] Haversack Replace(UncoercedCtorArg arg) const& {
    Haversack self = *this;  // Make a copy of this.
    return std::move(self).template Replace<ExplicitWrappedType>(
        std::move(arg));
  }

  // Internal use only.
  template <internal::CoercedCtorArgC... AddedCtorArgs, typename Compatibility,
            htls::meta::Concept<htls::meta::IsTemplateInstance<std::tuple>>
                PropagatedTuple>
  explicit Haversack(CtorSentinel, Compatibility compatibility,
                     PropagatedTuple pt, AddedCtorArgs... added)
      : members_(std::make_shared<typename decltype(members_)::element_type>(
            internal::CatAndSortTuples(Traits().MemberTupleType(),
                                       compatibility, std::move(pt),
                                       std::move(added)...))) {
    auto checks = Traits().AssertIsValidHaversack(compatibility);
    RunChecks(checks);
  }

 private:
  // Assert that all WrappedTypes can be added to this haversack.
  template <typename... WrappedTypes>
  static constexpr void AssertAdds() {
    constexpr auto adds =
        MakeTypeSet(htls::meta::type_c<WrappedTypes>...);
    static_assert(sizeof...(WrappedTypes) > 0, "Must add at least one type.");
    static_assert(size((adds & Traits().all_deps).Tuple()) == 0,
                  "All the added types must not be in the haversack already.");
  }

  template <typename... CheckTypes>
  static constexpr void RunChecks(
      htls::meta::BasicTuple<htls::meta::Type<CheckTypes>...>) {
    (CheckTypes::template Check<CheckTypes>(), ...);
  }

  // If no ExplicitWrappedTypes were passed to Insert, deduce the types from the
  // arguments and pass it to ExplicitInsertImpl as the ExplicitWrappedTypes.
  template <typename... CtorArgs>
  auto InsertImpl(CtorArgs... args) && {
    return std::move(*this)
        .template ExplicitInsertImpl<
            typename decltype(internal::DeduceWrappedTypeFromCtorArg(
                htls::meta::type_c<CtorArgs>))::type...>(std::move(args)...);
  }

  template <typename... ExplicitWrappedTypes, typename... CtorArgs>
    requires(sizeof...(ExplicitWrappedTypes) == sizeof...(CtorArgs))
  auto ExplicitInsertImpl(CtorArgs... args) && {
    AssertAdds<ExplicitWrappedTypes...>();
    using OtherHaversack = Haversack<ImplTs..., ExplicitWrappedTypes...>;
    return OtherHaversack(
        CtorSentinel(),
        OtherHaversack::Traits().CtorCompatibility(
            Traits().all_deps |
                htls::meta::MakeTypeSet(
                    htls::meta::type_c<ExplicitWrappedTypes>...),
            htls::meta::BasicTuple<>()),
        std::tuple_cat(
            std::move(*members_),
            std::make_tuple(
                internal::ConvertOne<ExplicitWrappedTypes>()(args)...)));
  }

  template <typename...>
  friend class Haversack;
  template <typename T>
  friend constexpr auto internal::TraitsOf(htls::meta::Type<T>);
  friend internal::HaversackTestUtil;

  std::shared_ptr<typename decltype(Traits().MemberTupleType())::type> members_;
};

// Calls is used to indicate to the Haversack any relevant functions that are
// called using it.
//
// Deps is used to indicate to the Haversack any dependent Haversacks that are
// further down the call chain.
//
// Example:
/*******************************************************************************
struct Alpha {};
struct Bravo {};

using OtherHaversack = Haversack<Bravo>;
void OtherFlow(OtherHaversack deps) {
  DoSomething(deps.Get<Bravo>());
}

using MyCallsHaversack = Haversack<Alpha, Calls<OtherFlow>>;
void MyCallsFlow(MyCallsHaversack deps) {
  OtherFlow(deps);
}

using MyDepsHaversack = Haversack<Alpha, Deps<OtherHaversack>>;
void MyDepsFlow(MyDepsHaversack deps) {
  OtherFlow(deps);
}


void main() {
  Alpha alpha;
  Bravo bravo;

  // MyCallsHaversack requires Alpha and Bravo because it knew about OtherFlow's
  // Haversack dependencies from Calls.
  MyCallsFlow(MyCallsHaversack(&alpha, &bravo));

  // MyDepsHaversack requires Alpha and Bravo because it knew about
  // OtherHaversack's dependencies from Deps.
  MyDepsFlow(MyDepsHaversack(&alpha, &bravo));
}
// ****************************************************************************/
template <auto&&... xs>
struct Calls : internal::CallsBase {
  using IndividualCalls =
      htls::meta::BasicTuple<htls::meta::Type<Calls<xs>>...>;
  using FirstType = std::decay_t<decltype((xs, ...))>;
};
template <typename...>
struct Deps {};

// Provides is used to indicate to the Haversack that this Haversack's owner
// will inject the provided types and that any Haversacks higher in the call
// chain do not need to.
//
// Example:
/*******************************************************************************
struct Alpha {};
struct Bravo {};

struct ChildHaversack : Haversack<Alpha, Bravo> {
  using HaversackT::HaversackT;
};

struct ParentHaversack : Haversack<Deps<ChildHaversack>, Provides<Bravo>> {
 using HaversackT::HaversackT;
};

struct GrandParentHaversack : Haversack<Deps<ParentHaversack>> {
 using HaversackT::HaversackT;
};

void main() {
 Alpha alpha;
 Bravo bravo;

 // GrandParentHaversack needs an Alpha because its descendant needs one, but it
 // doesn not need a Bravo because one of GrandParentHaversack's intermediate
 // descendants provides Bravo.
 GrandParentHaversack gp(&alpha);

 // ParentHaversack has an Alpha but not a Bravo because its owner will provide
 // the Bravo.
 ParentHaversack p(gp);

 // ChildHaversack has an Alpha and a Bravo. The Alpha is propagated from
 // GrandParentHaversack and Bravo is provided by the owner of ParentHaversack.
 ChildHaversack c(p, &bravo);

 c.Get<Alpha>();
 c.Get<Bravo>();
}
// ****************************************************************************/
template <typename...>
struct Provides {};

// KnownThreadSafe designates that a type in the Haversack should be non-const.
// This is treated as a completely distinct type in the Haversack. This should
// only be used if the type in question is thread-safe.
//
// Example:
/*******************************************************************************
struct Alpha {
  void ThreadSafeMethod() {}
};

void main() {
  Haversack<KnownThreadSafe<Alpha>> sack(std::make_shared<Alpha>());
  sack.Get<KnownThreadSafe<Alpha>>().ThreadSafeMethod();
}
// ****************************************************************************/
template <typename>
struct KnownThreadSafe {};

// Nullable designates that a type in the Haversack may be null. This is
// treated as a completely distinct type in the Haversack. This should only be
// used if the type in question is only conditionally available in production
// and the null case can be handled gracefully.
//
// Example:
/*******************************************************************************
struct Alpha {
  void methodAlpha() const {}
};
struct Beta {};

void main() {
  Haversack<Nullable<Alpha>, Nullable<Beta>> sack(std::make_shared<Alpha>(),
                                                  static_cast<Beta*>(nullptr));
  // alpha is a raw pointer to an Alpha because we did inject an Alpha value.
  if (const auto* alpha = sack.Get<Nullable<Alpha>>()) {
    alpha->methodAlpha();
  }
  // beta is nullptr because we injected nullptr for Beta.
  if (const auto* beta = sack.Get<Nullable<Beta>>()) {
  }
}
// ****************************************************************************/
template <typename>
struct Nullable {};

// Tagged designates that a type in the Haversack can be accessed via the Tag as
// well as the full explicit type. Tagged also allows multiple otherwise
// identical types to be included in the Haversack. Each Tag type must be unique
// in the direct dependencies to a Haversack.
//
// Example:
/*******************************************************************************
struct Alpha {
  Alpha(int v) : value(v) {}
  int value = 0;
};
struct TagA {};
struct TagB {};

void main() {
  Haversack<Tagged<Alpha, TagA>, Tagged<Alpha, TagB>> deps{
      MakeTagged<TagA>(std::make_shared<Alpha>(1)),
      MakeTagged<TagB>(std::make_shared<Alpha>(2))};
  LOG(INFO) << deps.Get<TagA>().value;
  // prints: 1
  LOG(INFO) << deps.Get<TagB>().value
  // prints: 2
}
// ****************************************************************************/
template <typename WrappedType, typename Tag>
struct Tagged {
  internal::SharedProxy<typename decltype(internal::kUnwrapTypeWrappers(
      htls::meta::type_c<WrappedType>))::type>
      tagged;
};
template <typename Tag, internal::UncoercedCtorArg UncoercedCtorArg>
auto MakeTagged(UncoercedCtorArg arg) {
  constexpr htls::meta::Type element_type =
      internal::DeduceWrappedTypeFromCtorArg(
          htls::meta::type_c<internal::CoercedCtorArg<UncoercedCtorArg>>);
  return Tagged<typename decltype(element_type)::type, Tag>{
      .tagged = internal::CoerceCtorArg(arg)};
}

}  // namespace hotels::haversack

#undef HTLS_UNAVAILABLE_ATTRIBUTE
#undef HTLS_ENABLE_IF_ATTRIBUTE

#endif  // HOTELS_TEMPLATE_LIBRARY_HAVERSACK_HAVERSACK_H_
