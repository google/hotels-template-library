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

#ifndef HOTELS_TEMPLATE_LIBRARY_HAVERSACK_HAVERSACK_H_
#define HOTELS_TEMPLATE_LIBRARY_HAVERSACK_HAVERSACK_H_

#include <cassert>
#include <concepts>
#include <memory>
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
  // Normal operation calls one of the normal public constructors, which calls
  // the public but internal constructor which calls the private constructor.
  //
  // Internal constructor sentinel type.
  using CtorSentinel = internal::CtorSentinel;

 public:
  // Get the traits for this Haversack.
  static consteval htls::meta::Concept<
      htls::meta::IsTemplateInstance<internal::HaversackTraits>> auto
  Traits() {
    return internal::TraitsOf(htls::meta::type_c<Haversack>);
  }
  // Eagerly initialize Traits() for each haversack at compile-time and verify
  // there were no issues.
  static_assert(!std::is_same_v<decltype(Traits()), void>,
                "There was an issue building the haversack traits.");

  // Public alias for the Haversack type.
  using HaversackT = Haversack;

  // Default constructor only for empty haversacks.
  explicit Haversack()
    requires(size(htls::meta::Type(Traits().all_deps.Tuple())) == 0)
      : Haversack(CtorSentinel(),
                  Traits().CtorCompatibility(htls::meta::MakeTypeSet(),
                                             htls::meta::MakeBasicTuple()),
                  std::tuple<>()) {}

  // Builds a new haversack from members.
  template <typename... UncoercedCtorArgs>
    requires((internal::UncoercedCtorArg<UncoercedCtorArgs> && ...) &&
             sizeof...(UncoercedCtorArgs) > 0)
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
  // Note: This constructor is only enabled if args is non-empty so we
  // can ignore the warning about explicit unary constructors.
  template <typename... UncoercedCtorArgs>
    requires((internal::UncoercedCtorArg<UncoercedCtorArgs> && ...) &&
             sizeof...(UncoercedCtorArgs) > 0)
  Haversack(internal::HaversackInstance auto cxt,
            UncoercedCtorArgs... args)  // NOLINT
      : Haversack(CtorSentinel(),
                  Traits().CtorCompatibility(
                      cxt.Traits().all_deps,
                      htls::meta::MakeBasicTuple(
                          htls::meta::type_c<
                              internal::CoercedCtorArg<UncoercedCtorArgs>>...)),
                  *cxt.members_, internal::CoerceCtorArg(std::move(args))...) {}

  // Honeypot overload for when the arguments are (incorrectly) not pointers.
  // We use the 'unavailable' attribute to give a better compiler error than
  // just 'method is deleted'.
  // TODO(cmgp): consider removing this overload now that concepts give better
  // error messages.
  template <typename First, typename... Rest,
            bool kAnyInvalidCtorArgs =
                // Any of the "rest" of the arguments are not valid,
            (... || !internal::UncoercedCtorArg<Rest>) ||
            // or the first argument is not a valid pointer and not a valid
            // Haversack.
            (!internal::UncoercedCtorArg<First> &&
             !internal::IsHaversack(htls::meta::type_c<First>)),
            typename = std::enable_if_t<kAnyInvalidCtorArgs>>
  explicit Haversack(First, Rest...) HTLS_UNAVAILABLE_ATTRIBUTE(
      "One of the Haversack constructor arguments wasn't a pointer "
      "when it should have been (or it wasn't a supported pointer).");

  // A Haversack can be converted to any other Haversack type that contains a
  // subset of the member types.
  //
  // Conversion operator is preferred over conversion constructor for the case
  // of converting a Haversack to its subclass.
  //
  // NOTE: This operator is NOT explicit because it models a bespoke type
  // hierarchy (similar to the hierarchy between base-classes and subclasses).
  template <typename OtherHaversack>
  [[nodiscard]] operator OtherHaversack() &&
    requires(internal::HaversackInstance<OtherHaversack> &&
             !std::same_as<Haversack, OtherHaversack> &&
             OtherHaversack::Traits()
                 .CtorCompatibility(Traits().all_deps,
                                    htls::meta::BasicTuple<>())
                 .IsCompatible()) {
    return OtherHaversack(
        CtorSentinel(),
        OtherHaversack::Traits().CtorCompatibility(
            Traits().all_deps, htls::meta::BasicTuple<>()),
        *members_);
  }
  template <typename OtherHaversack>
  [[nodiscard]] operator OtherHaversack() const&
        requires(internal::HaversackInstance<OtherHaversack> &&
                 !std::same_as<Haversack, OtherHaversack> &&
                 OtherHaversack::Traits()
                     .CtorCompatibility(Traits().all_deps,
                                        htls::meta::BasicTuple<>())
                     .IsCompatible()) {
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
  template <internal::ConstexprString S,
            typename GetSharedHelper = internal::GetSharedHelper<
                std::integral_constant<decltype(S), S>, Haversack>>
  [[nodiscard]] decltype(auto) GetShared() const HTLS_ENABLE_IF_ATTRIBUTE(
      GetSharedHelper::GetMatchingWrappedType() != htls::meta::type_c<void>,
      "Requested type is not a direct dependency in the Haversack.") {
    return GetSharedHelper::Get(*members_);
  }

  // Gets a reference to the T member.
  //
  // If T matches KnownThreadSafe<U>, a mutable `U&` is returned;
  // otherwise a `const T&` reference is returned.
  template <internal::ConstexprString S,
            typename GetSharedHelper = internal::GetSharedHelper<
                std::integral_constant<decltype(S), S>, Haversack>>
  [[nodiscard]] decltype(auto) Get() const HTLS_ENABLE_IF_ATTRIBUTE(
      GetSharedHelper::GetMatchingWrappedType() != htls::meta::type_c<void>,
      "Requested type is not a direct dependency in the Haversack.") {
    if constexpr (internal::IsNullable(
                      GetSharedHelper::GetMatchingWrappedType())) {
      return GetShared<S>().get();
    } else {
      return *GetShared<S>();
    }
  }

  // Gets a new haversack without the WrappedTypes types.
  //
  // Prefer to use the Haversack constructor if the final Haversack type is
  // known.
  template <typename... WrappedTypes>
  [[nodiscard]] internal::HaversackInstance auto Without() && {
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
  [[nodiscard]] internal::HaversackInstance auto Without() const& {
    Haversack self = *this;  // Make a copy of this.
    return std::move(self).template Without<WrappedTypes...>();
  }

  // Gets a new haversack with the args added. This method takes
  // the same kind of arguments as the Haversack constructor.
  //
  // Prefer to use the Haversack constructor if the final Haversack type is
  // known.
  //
  // ExplicitWrappedTypes are optional, and if ExplicitWrappedTypes are
  // provided, args will be converted to the explicit types.
  // Otherwise if ExplicitWrappedTypes are not provided, the types to be added
  // to the Haversack are deduced from the args and assumed to be
  // non-mutable.
  template <typename... ExplicitWrappedTypes>
  [[nodiscard]] internal::HaversackInstance auto Insert(
      internal::ExplicitInsertArg auto... args) &&
    requires(sizeof...(ExplicitWrappedTypes) == sizeof...(args))
  {
    return std::move(*this)
        .template ExplicitInsertImpl<ExplicitWrappedTypes...>(
            internal::CoerceExplicitInsertArg(std::move(args))...);
  }
  template <typename... ExplicitWrappedTypes>
  [[nodiscard]] internal::HaversackInstance auto Insert(
      internal::ExplicitInsertArg auto... args) const&
    requires(sizeof...(ExplicitWrappedTypes) == sizeof...(args))
  {
    Haversack self = *this;  // Make a copy of this.
    return std::move(self).template Insert<ExplicitWrappedTypes...>(
        std::move(args)...);
  }
  [[nodiscard]] internal::HaversackInstance auto Insert(
      internal::UncoercedCtorArg auto... args) && {
    return std::move(*this).InsertImpl(
        internal::CoerceCtorArg(std::move(args))...);
  }
  [[nodiscard]] internal::HaversackInstance auto Insert(
      internal::UncoercedCtorArg auto... args) const& {
    Haversack self = *this;  // Make a copy of this.
    return std::move(self).Insert(std::move(args)...);
  }

  // Replace a value in the haversack with a new one of the same type.
  template <typename ExplicitWrappedType = void, typename Arg>
    requires(internal::ExplicitInsertArg<Arg> &&
             (internal::UncoercedCtorArg<Arg> ||
              !std::is_same_v<ExplicitWrappedType, void>))
  [[nodiscard]] Haversack Replace(Arg arg) && {
    // If an ExplicitWrappedType is provided, ExplicitWrappedType is used as the
    // type to replace, otherwise the type to replace is deduced from the type
    // of the argument.
    using TypeToReplace = decltype([] {
      if constexpr (std::is_same_v<ExplicitWrappedType, void>) {
        return Traits()
            .CtorCompatibility(htls::meta::MakeTypeSet(),
                               htls::meta::BasicTuple<>())
            .template FindMatch(
                htls::meta::type_c<internal::CoercedCtorArg<Arg>>);
      } else {
        return htls::meta::type_c<ExplicitWrappedType>;
      }
    }())::type;
    static_assert(
        Contains(htls::meta::type_c<TypeToReplace>, Traits().all_deps));

    // Pointer to non-const `members_` type by copy.
    std::shared_ptr<typename decltype(Traits().MemberTupleType())::type>
        new_members = std::make_shared<
            typename decltype(Traits().MemberTupleType())::type>(*members_);
    std::get<internal::Holder<TypeToReplace>>(*new_members) =
        internal::Holder<TypeToReplace>(
            internal::CoerceExplicitInsertArg(std::move(arg)));
    members_ = std::move(new_members);
    return std::move(*this);
  }
  template <typename ExplicitWrappedType = void, typename Arg>
    requires(internal::ExplicitInsertArg<Arg> &&
             (internal::UncoercedCtorArg<Arg> ||
              !std::is_same_v<ExplicitWrappedType, void>))
  [[nodiscard]] Haversack Replace(Arg arg) const& {
    Haversack self = *this;  // Make a copy of this.
    return std::move(self).template Replace<ExplicitWrappedType>(
        std::move(arg));
  }

  // Internal use only.
  explicit Haversack(
      CtorSentinel,
      htls::meta::Concept<
          htls::meta::IsTemplateInstance<internal::CompatibleArgs>> auto
          compatibility,
      htls::meta::Concept<htls::meta::IsTemplateInstance<std::tuple>> auto pt,
      internal::CoercedCtorArgC auto... added)
      : members_(std::make_shared<typename decltype(members_)::element_type>(
            internal::CatAndSortTuples(Traits().MemberTupleType(),
                                       compatibility, std::move(pt),
                                       std::move(added)...))) {
    htls::meta::BasicTuple checks =
        Traits().AssertIsValidHaversack(compatibility);
    RunChecks(checks);
  }

 private:
  // Assert that all WrappedTypes can be added to this haversack.
  template <typename... WrappedTypes>
  static consteval void AssertAdds() {
    constexpr htls::meta::TypeSet adds =
        MakeTypeSet(htls::meta::type_c<WrappedTypes>...);
    static_assert(sizeof...(WrappedTypes) > 0, "Must add at least one type.");
    static_assert(size((adds & Traits().all_deps).Tuple()) == 0,
                  "All the added types must not be in the haversack already.");
  }

  template <typename... CheckTypes>
      requires((internal::CheckC<CheckTypes> && ...))
  static consteval void RunChecks(
      htls::meta::BasicTuple<htls::meta::Type<CheckTypes>...>) {
    (CheckTypes::template Check<CheckTypes>(), ...);
  }

  // If no ExplicitWrappedTypes were passed to Insert, deduce the types from the
  // arguments and pass it to ExplicitInsertImpl as the ExplicitWrappedTypes.
  template <typename... CtorArgs>
    requires((internal::CoercedCtorArgC<CtorArgs> && ...))
  internal::HaversackInstance auto InsertImpl(CtorArgs... args) && {
    return std::move(*this)
        .template ExplicitInsertImpl<
            typename decltype(internal::DeduceWrappedTypeFromCtorArg(
                htls::meta::type_c<CtorArgs>))::type...>(std::move(args)...);
  }

  template <typename... ExplicitWrappedTypes, typename... CtorArgs>
    requires((internal::CoercedExplicitInsertArg<CtorArgs> && ...) &&
             sizeof...(ExplicitWrappedTypes) == sizeof...(CtorArgs))
  internal::HaversackInstance auto ExplicitInsertImpl(CtorArgs... args) && {
    AssertAdds<ExplicitWrappedTypes...>();

    // Deducing the return Haversack type. The returned type is *roughly* the
    // same type as `*this` except with the inserted types added as direct
    // dependencies and any provided types that were inserted removed. *roughly*
    // because the Provides directive is potentially shuffled around in the
    // template list.
    constexpr auto new_provides =
        Traits().provided_deps -
        htls::meta::MakeTypeSet(htls::meta::type_c<ExplicitWrappedTypes>...);
    constexpr auto other_haversack_t = htls::meta::Apply(
        [new_provides](auto... impl_ts) {
          return htls::meta::type_c<Haversack<
              typename decltype(FromTuple<Provides>(
                  new_provides.Tuple()))::type,
              typename decltype(impl_ts)::type..., ExplicitWrappedTypes...>>;
        },
        Filter(
            [](auto impl_t) {
              return !htls::meta::IsTemplateInstance<Provides>(impl_t);
            },
            htls::meta::MakeBasicTuple(htls::meta::type_c<ImplTs>...)));
    using OtherHaversack = typename decltype(other_haversack_t)::type;

    return OtherHaversack(
        CtorSentinel(),
        OtherHaversack::Traits().CtorCompatibility(
            OtherHaversack::Traits().all_deps, htls::meta::BasicTuple<>()),
        std::tuple_cat(
            *members_,
            std::make_tuple(internal::ConvertInsertArg<ExplicitWrappedTypes>(
                std::move(args))...)));
  }

  template <typename...>
  friend class Haversack;
  template <typename T>
  friend consteval auto internal::TraitsOf(htls::meta::Type<T>);
  friend internal::HaversackTestUtil;

  std::shared_ptr<const typename decltype(Traits().MemberTupleType())::type>
      members_;
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
template <typename... Ts>
  requires((internal::HaversackInstance<Ts> && ...))
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
template <typename WrappedType, typename Tag, typename AliasC>
struct Tagged {
  static constexpr bool kAlias = AliasC::value;
  using type = WrappedType;

  internal::SharedProxy<typename decltype(internal::kUnwrapTypeWrappers(
      htls::meta::type_c<WrappedType>))::type>
      tagged;
};

// TaggedAlias works very similarly to Tagged except that the tag does not
// define a new type value in the Haversack. TaggedAlias simply provides an
// alternative way to access the value from the Haversack.
//
// Example:
/*******************************************************************************
struct Alpha {
  Alpha(int v) : value(v) {}
  int value = 0;
};
struct TagA {};

void main() {
  using ChildA = Haversack<Alpha>;
  using ChildB = Haversack<TaggedAlias<Alpha, TagA>>;

  // Note: Only one dependency is passed to the constructor.
  Haversack<Deps<ChildA, ChildB>> deps(std::make_shared<Alpha>(1));
  ChildA child_a = deps;
  ChildB child_b = deps;
  LOG(INFO) << child_a.Get<Alpha>().value;
  // prints: 1
  LOG(INFO) << child_b.Get<TagA>().value
  // prints: 1
}
// ****************************************************************************/
template <typename WrappedType, typename Tag>
using TaggedAlias = Tagged<WrappedType, Tag, std::bool_constant<true>>;

template <typename Tag, typename UncoercedCtorArg>
  requires(internal::UncoercedCtorArg<UncoercedCtorArg>)
auto MakeTagged(UncoercedCtorArg arg) {
  constexpr htls::meta::Type element_type =
      internal::DeduceWrappedTypeFromCtorArg(
          htls::meta::type_c<internal::CoercedCtorArg<UncoercedCtorArg>>);
  return Tagged<typename decltype(element_type)::type, Tag>{
      .tagged = internal::CoerceCtorArg(arg)};
}

// STagged and STaggedAlias work exactly as Tagged and TaggedAlias,
// respectively, except that the tag should be a string literal instead of a
// type.
//
// Example:
/*******************************************************************************
struct Alpha {
  Alpha(int v) : value(v) {}
  int value = 0;
};

void main() {
  Haversack<STagged<Alpha, "a">> deps(
      MakeTagged<"a">(std::make_shared<Alpha>(1)));
  LOG(INFO) << deps.Get<"a">().value;
  // prints: 1
}
// ****************************************************************************/
template <typename WrappedType, internal::ConstexprString Str>
using STagged = Tagged<WrappedType, std::integral_constant<decltype(Str), Str>>;
template <typename WrappedType, internal::ConstexprString Str>
using STaggedAlias =
    Tagged<WrappedType, std::integral_constant<decltype(Str), Str>,
           std::bool_constant<true>>;

template <internal::ConstexprString Str, typename UncoercedCtorArg>
  requires(internal::UncoercedCtorArg<UncoercedCtorArg>)
auto MakeTagged(UncoercedCtorArg arg) {
  constexpr htls::meta::Type element_type =
      internal::DeduceWrappedTypeFromCtorArg(
          htls::meta::type_c<internal::CoercedCtorArg<UncoercedCtorArg>>);
  return STagged<typename decltype(element_type)::type, Str>{
      .tagged = internal::CoerceCtorArg(arg)};
}

}  // namespace hotels::haversack

#undef HTLS_UNAVAILABLE_ATTRIBUTE
#undef HTLS_ENABLE_IF_ATTRIBUTE

#endif  // HOTELS_TEMPLATE_LIBRARY_HAVERSACK_HAVERSACK_H_
