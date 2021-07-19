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

#ifndef HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_HAVERSACK_IMPL_H_
#define HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_HAVERSACK_IMPL_H_

#include <cassert>
#include <iostream>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>

#include "third_party/hotels_template_library/haversack/internal/basic_tuple.h"
#include "third_party/hotels_template_library/haversack/internal/type.h"
#include "third_party/hotels_template_library/haversack/internal/type_set.h"
#include "third_party/hotels_template_library/haversack/internal/util.h"

namespace hotels::haversack {

template <typename...>
struct Haversack;

template <auto&&...>
struct Calls;

template <typename...>
struct Deps;

template <typename...>
struct Provides;

template <typename>
struct KnownThreadSafe;

namespace internal {

template <typename T>
using SharedProxy = std::shared_ptr<T>;

template <typename T>
constexpr auto ThreadSafeType(types::Type<T> t) {
  if constexpr (is_template_instance_v<T, KnownThreadSafe>) {
    return get<0>(types::AsTuple(t));
  } else {
    return types::type_c<const T>;
  }
}

// Make a KnownThreadSafe<T> from a mutable T or just a T from a "const T".
template <typename T>
constexpr auto MakeKnownThreadSafe(types::Type<T> t) {
  constexpr types::MetaValueFunction<std::is_const> is_const;
  constexpr types::MetaTypeFunction<std::remove_const> remove_const;
  if constexpr (is_const(t)) {
    return remove_const(t);
  } else {
    return types::type_c<KnownThreadSafe<T>>;
  }
}

// Forward decl for friends.
struct HaversackTestUtil;

// Sentinel type to identify the root Haversack ctor.
struct CtorSentinel {};

// Access the private traits of the Haversack.
template <typename HaversackT>
constexpr auto TraitsOf(types::Type<HaversackT> t) {
  return HaversackT::Traits();
}

template <typename T, typename... Ts>
struct SourceMatches {
  static constexpr std::size_t kMatches = sizeof...(Ts);

  template <typename U>
  static constexpr void Check() {
    if constexpr (U::kMatches < 1) {
      static_assert(
          U::kMatches == 1,
          "The source type doesn't have any matches (SourceMatches is of the "
          "form SourceMatches<SourceType>). This type may have been removed "
          "from the Haversack type list or isn't convertible to the intended "
          "type.");
    } else if constexpr (U::kMatches > 1) {
      static_assert(
          U::kMatches == 1,
          "The source type has more than one match (SourceMatches is of the "
          "form SourceMatches<SourceType, MatchingTargetTypes...>). This type "
          "is convertible to too many types in the Haversack, try using a more "
          "explicit type when constructing.");
    }
  }
};

template <typename T, typename... Ts>
struct TargetMatches {
  static constexpr std::size_t kMatches = sizeof...(Ts);

  template <typename U>
  static constexpr void Check() {
    if constexpr (U::kMatches < 1) {
      static_assert(
          U::kMatches == 1,
          "The target type doesn't have any matches (TargetMatches is of the "
          "form TargetMatches<TargetType>). This type may not have been added "
          "to "
          "the constructor callsite or the intended value isn't convertible to "
          "this type.");
    } else if constexpr (U::kMatches > 1) {
      static_assert(
          U::kMatches == 1,
          "The target type has more than one match (TargetMatches is of the "
          "form TargetMatches<TargetType, MatchingSourceTypes...>). Too many "
          "types "
          "are convertible to this type at the constructor callsite, you may "
          "be passing duplicate values to the constructor or try using more "
          "explicit types at the constructor callsite.");
    }
  }
};

// kFindMatches is a constexpr memoization cache for FindMatchesImpl to avoid
// maximum step limit compilation errors.
template <typename Sources, typename Targets, typename Source>
constexpr auto FindMatchesImpl(types::Type<Source> source) {
  constexpr types::MetaValueFunction<std::is_const> is_const;
  constexpr types::MetaTypeFunction<std::add_const> add_const;
  return Filter(
      [=](auto target) {
        if (!is_const(target) && is_const(source)) {
          // const T cannot be converted to (non-const) T.
          return false;
        }
        return types::MetaValueFunction<std::is_base_of>()(target, source)  //
               || source == target                                          //
               || add_const(source) == target;
      },
      Targets());
}
template <typename Sources, typename Targets, typename Source>
constexpr auto kFindMatches =
    FindMatchesImpl<Sources, Targets>(types::type_c<Source>);

// kGetMatchChecks is a constexpr memoization cache for GetMatchChecksImpl to
// avoid maximum step limit compilation errors.
template <typename CompatibleArgsInstance>
constexpr auto GetMatchChecksImpl() {
  auto all_source_matches = CompatibleArgsInstance::FindAllSourceMatches();
  auto srcs = Transform(
      [=](auto t) {
        return types::FromTuple<SourceMatches>(
            MakeBasicTuple(get<0>(t)) +
            Transform([](auto t) { return MakeKnownThreadSafe(t); },
                      get<1>(t)));
      },
      all_source_matches);
  auto targets = Transform(
      [=](auto t) {
        return types::FromTuple<TargetMatches>(
            MakeBasicTuple(MakeKnownThreadSafe(t)) +
            CompatibleArgsInstance::FindTargetMatches(t, all_source_matches));
      },
      typename CompatibleArgsInstance::Targets());
  return srcs + targets;
}
template <typename CompatibleArgsInstance>
constexpr auto kGetMatchChecks = GetMatchChecksImpl<CompatibleArgsInstance>();

// ConvertOne<T> has an operator() to convert any valid SharedProxy to a
// SharedProxy<T>.
template <typename T>
struct ConvertOne {
  template <typename U, typename = std::enable_if_t<std::is_convertible_v<
                            SharedProxy<U>, SharedProxy<T>>>>
  constexpr SharedProxy<T> operator()(SharedProxy<U> u) const {
    return u;
  }
};

// A Converter<Ts...> has operator() to convert any valid SharedProxy to
// whichever SharedProxy<Ts> it is compatible with. If the argument is not a
// one-to-one match with one of Ts, the operator() invocation is a template
// error.
template <typename... Ts>
struct Converter : ConvertOne<Ts>... {
  using ConvertOne<Ts>::operator()...;
};

// Determine if and how the types in Sources can be uniquely converted to the
// types in Targets.
//
// - Sources is a BasicTuple of Type types which represents the types of the
// arguments actually provided.
//
// - Targets is a BasicTuple of Type types which represents the types of the
// parameters expected to a certain invokable. Each type should be
// ThreadSafeType-ized before being passed here.
template <typename Sources, typename TargetsArg>
class CompatibleArgs {
 public:
  using Targets = TargetsArg;
  // Find the unique type in Targets that T can be implicitly converted to. If
  // T cannot be converted to anything or more than one thing, returns
  // Type<void> instead.
  template <typename Source>
  static constexpr auto FindMatch(types::Type<Source> source) {
    auto matches = FindMatches(source);
    if constexpr (size(matches) == 1) {
      return get<0>(matches);
    } else {
      return types::type_c<void>;
    }
  }

  // Find all the target types that source can be converted to, as a BasicTuple
  // of Types.
  template <typename Source>
  static constexpr auto FindMatches(types::Type<Source> source) {
    return kFindMatches<Sources, Targets, Source>;
  }
  // Find all the matching target types for each source match as a BasicTuple of
  // BasicTuples in the form:
  //  BasicTuple<
  //    BasicTuple<FirstSource, BasicTuple<FirstSourceMatches...>>,
  //    BasicTuple<SecondSource, BasicTuple<SecondSourceMatches...>>,
  //    ...>
  static constexpr auto FindAllSourceMatches() {
    return Transform([](auto t) { return MakeBasicTuple(t, FindMatches(t)); },
                     Sources());
  }
  // Find all the source types that can be converted to a given target type.
  // all_source_matches is of the form returned by FindAllSourceMatches.
  template <typename Target, typename... SourceMatches>
  static constexpr auto FindTargetMatches(
      types::Type<Target> target,
      BasicTuple<SourceMatches...> all_source_matches) {
    return Transform(
        [](auto t) { return get<0>(t); },
        Filter(
            [=](auto t) constexpr {
              return types::Contains(
                  target, Apply(
                              [](auto... ts) constexpr {
                                return types::MakePrevalidatedTypeSet(ts...);
                              },
                              get<1>(t)));
            },
            all_source_matches));
  }

 public:
  constexpr CompatibleArgs(Sources, Targets) {}

  constexpr bool QuickTest() const {
    // Sources and Targets are bijective (one-to-one and onto) if:
    // - They are the same size (the size comparison) and
    // - Each Sources has a one-to-one mapping in Targets (all of the
    // conversions with converter are valid).
    return size(types::type_c<Sources>) == size(types::type_c<Targets>) &&
           AllOf(
               [](auto t) {
                 return types::IsValidExpr(
                     t,
                     [](auto u)
                         -> decltype(converter_(
                             SharedProxy<typename decltype(u)::type>())) {});
               },
               Sources());
  }

  // Returns true if Sources is compatible with Targets.
  constexpr bool IsCompatible() const {
    constexpr CompatibleArgs self{Sources(), Targets()};
    if constexpr (self.QuickTest()) {
      return true;
    } else {
      return AllOf([](auto t) { return decltype(t)::type::kMatches == 1; },
                   GetMatchChecks());
    }
  }

  // Returns a BasicTuple of SourceMatches for each source and TargetMatches for
  // each target.
  constexpr auto GetMatchChecks() const {
    constexpr CompatibleArgs self{Sources(), Targets()};
    // If it is compatible, then there are no failing match checks.
    // kGetMatchChecks is very expensive so we want to avoid evaluating it if we
    // know that it will return nothing.
    if constexpr (self.QuickTest()) {
      return MakeBasicTuple();
    } else {
      return kGetMatchChecks<CompatibleArgs>;
    }
  }

  // Assuming T has a unique match in Targets (e.g. Converted), make a
  // SharedProxy<Converted> from the t value.
  template <typename T>
  auto Convert(SharedProxy<T> t) const {
    return converter_(std::move(t));
  }

  typename decltype(types::FromTuple<Converter>(Targets()))::type converter_;
};
template <typename Sources, typename Targets>
CompatibleArgs(Sources, Targets) -> CompatibleArgs<Sources, Targets>;

// True if T is an instance of Haversack or a Haversack subclass.
template <typename T>
constexpr auto IsHaversack(types::Type<T> t) {
  if constexpr (types::IsValidExpr(t,
                                   [](auto u) ->
                                   typename decltype(u)::type::HaversackT {})) {
    return std::is_base_of_v<typename T::HaversackT, T> &&
           is_template_instance_v<typename T::HaversackT, Haversack>;
  } else {
    return false;
  }
}

// True if there is at least one type in Ts and the first one is a Haversack.
template <typename... Ts>
constexpr bool FirstIsHaversack() {
  if constexpr (sizeof...(Ts) == 0) {
    return false;
  } else {
    return [](auto first, auto... rest) {
      return IsHaversack(first);
    }(types::type_c<Ts>...);
  }
}

// All types specified in Deps::Value must exist in the Haversack, but are not
// directly readable unless the type is also a direct dependency.
//
// Each Ts must be a Haversack type and all the types in those haversacks are
// flattened into the result of Value.
template <typename... Ts>
constexpr auto GetTypesFromDeps(types::Type<Deps<Ts...>>) {
  auto get_arguments_that_are_haversacks = [](auto t) {
    static_assert(IsHaversack(t), "All types in Deps must be Haversacks.");
    if constexpr (IsHaversack(t)) {
      return TraitsOf(t).all_deps.Tuple();
    } else {
      // Non-runtime reachable fallthrough case to avoid further compilation
      // errors other than the above static_assert.
      return BasicTuple<>();
    }
  };
  return Concat(get_arguments_that_are_haversacks(types::type_c<Ts>)...);
}

// is_template_instance only works when all template parameters are types, so
// we use std::is_base_of<T, CallsBase> instead to check if a type is an
// instance of Calls.
struct CallsBase {};
// All types specified in Calls::Value must exist in the Haversack, but are not
// directly readable unless the type is also a direct dependency.
//
// Each Funcs must be an invocable, and any parameters to each Funcs that are
// Haversack instances have their contained types flattened into the result.
template <typename CallsInstance>
constexpr auto GetTypesFromCalls(types::Type<CallsInstance>) {
  auto get_arguments_that_are_haversacks = [](auto t) {
    using FuncT = std::remove_const_t<
        std::remove_reference_t<typename decltype(t)::type>>;
    auto args =
        types::AsTuple(types::type_c<typename function_traits<FuncT>::args>);
    return Filter([](auto arg) { return IsHaversack(arg); },
                  Transform(types::MetaTypeFunction<std::decay>(), args));
  };
  // For each function in Funcs, get all of its arguments that are Haversacks
  // as a BasicTuple. Concatenate each Funcs result into one BasicTuple of
  // Haversacks then create a Deps from those types and use its Haversack
  // flattening logic.
  return GetTypesFromDeps(types::FromTuple<Deps>(Flatten(Transform(
      get_arguments_that_are_haversacks, typename CallsInstance::types()))));
}

// All types specified in Provides::Value are expected to be passed to the
// Haversack ctor directly as arguments and not inherited from another Haversack
// instance.
template <typename... Ts>
constexpr auto GetTypesFromProvides(types::Type<Provides<Ts...>>) {
  return MakeBasicTuple(types::type_c<Ts>...);
}

// Holds the different sets of types, organized into different categories.
//
// - DirectDepsT is a TypeSet of all of the types which are direct dependencies
// of the associated Haversack.
// - AllDepsT is a TypeSet of all of the types which are dependencies of the
// associated Haversack (direct and indirect) that are not provided by this
// Haversack or a descendant Haversack.
// - BuilderSuccessT is a std::bool_constant which is std::true_type iff all the
// checks in HaversackTraitsBuilder passed. This lets us skip later checks if we
// already have failures to avoid exploding the error output. Failing a
// static_assert does not stop compilation automatically so we explicitly skip
// the rest of the validation if we get any failures.
template <typename DirectDepsT, typename AllDepsT, typename BuilderSuccessT>
struct HaversackTraits {
  DirectDepsT direct;
  AllDepsT all_deps;

  template <typename Compatibility>
  constexpr auto AssertIsValidHaversack(Compatibility compatibility) const {
    if constexpr (BuilderSuccessT::value) {
      return compatibility.GetMatchChecks();
    } else {
      return BasicTuple<>();
    }
  }

  // Gets the "Compatibility" (an instance of CompatibleArgs) of arguments to a
  // Haversack ctor. The compatibility determines if the Haversack can actually
  // be constructed from the arguments provided, and if so, how.
  //
  // - propagated_set is the TypeSet of types contained in the "other" haversack
  // being passed to this one. Empty set if there is no "other" haversack.
  // - added is the BasicTuple of Types of arguments being directly passed into
  // this haversack.  It is empty when converting from one haversack directly to
  // another.
  template <typename Propagated, typename Added>
  constexpr auto CtorCompatibility(Propagated propagated_set, Added added) {
    return CompatibleArgs(added,
                          Transform([](auto t) { return ThreadSafeType(t); },
                                    (all_deps - propagated_set).Tuple()));
  }

  // The tuple holds nullable SharedProxys to enable making empty haversacks for
  // testing. Nullability invariants are enforced via constructors instead in
  // production.
  constexpr auto MemberTupleType() const {
    return types::FromTuple<std::tuple>(Transform(
        [](auto t) {
          return types::FromTuple<SharedProxy>(
              MakeBasicTuple(ThreadSafeType(t)));
        },
        all_deps.Tuple()));
  }
  constexpr HaversackTraits(DirectDepsT direct, AllDepsT all_deps,
                            BuilderSuccessT)
      : direct(direct), all_deps(all_deps) {}
};
template <typename DirectDepsT, typename AllDepsT, typename BuilderSuccessT>
HaversackTraits(DirectDepsT, AllDepsT, BuilderSuccessT)
    -> HaversackTraits<DirectDepsT, AllDepsT, BuilderSuccessT>;

// Holds the different sets of types, organized into different categories.
//
// - DirectDepsT is a BasicTuple of Type types which represents all the types
// which are direct dependencies that have been added to the builder so far.
// - IndirectDepsT is a BasicTuple of Type types which represents all the types
// which are indirect dependencies that have been added to the builder so far.
// - ProvidesT is a BasicTuple of Type types which represents all the types
// which are "provided" which have been added to the builder so far.
template <typename DirectDepsT, typename IndirectDepsT, typename ProvidesT>
struct HaversackTraitsBuilder {
  DirectDepsT direct;
  IndirectDepsT indirect;
  ProvidesT provides;

  template <typename T>
  constexpr auto ExtendDirectDeps(T t) const {
    return internal::HaversackTraitsBuilder(direct + t, indirect, provides);
  }
  template <typename T>
  constexpr auto ExtendIndirectDeps(T t) const {
    static_assert(size(types::type_c<DirectDepsT>) == 0,
                  "All `Calls` and other directives must come before any "
                  "direct dependencies.");
    return internal::HaversackTraitsBuilder(direct, indirect + t, provides);
  }
  template <typename T>
  constexpr auto ExtendProvides(T t) const {
    static_assert(size(types::type_c<DirectDepsT>) == 0,
                  "All `Calls` and other directives must come before any "
                  "direct dependencies.");
    return internal::HaversackTraitsBuilder(direct, indirect, provides + t);
  }

  constexpr auto Build() const {
    constexpr HaversackTraitsBuilder self;
    constexpr auto make_type_set = [](auto... ts) {
      return types::MakeTypeSet(ts...);
    };
    constexpr auto direct_dep_set = Apply(make_type_set, self.direct);
    constexpr auto dep_set =
        direct_dep_set | Apply(make_type_set, self.indirect);
    constexpr auto provide_set = Apply(make_type_set, self.provides);

    constexpr bool no_direct_deps_are_provided =
        size((direct_dep_set & provide_set).Tuple()) == 0;
    constexpr bool indirect_deps_is_superset_of_provides =
        dep_set >= provide_set;
    constexpr bool all_direct_deps_unique =
        size(self.direct) == size(direct_dep_set.Tuple());
    static_assert(
        no_direct_deps_are_provided,
        "A direct dependency cannot be provided by the same Haversack.");
    static_assert(
        indirect_deps_is_superset_of_provides,
        "The set of indirect dependencies should be a superset of the "
        "\"provided\" dependencies.");
    static_assert(all_direct_deps_unique,
                  "Each direct dependency should be unique.");
    // Failing a static_assert does not interrupt compilation so we still
    // explicitly pass this BuilderSuccessT every time.
    using BuilderSuccessT =
        std::bool_constant<no_direct_deps_are_provided &&
                           indirect_deps_is_superset_of_provides &&
                           all_direct_deps_unique>;
    return HaversackTraits(direct_dep_set, dep_set - provide_set,
                           BuilderSuccessT());
  }

  constexpr HaversackTraitsBuilder() = default;
  constexpr HaversackTraitsBuilder(DirectDepsT, IndirectDepsT, ProvidesT) {}
};
template <typename DirectDepsT, typename IndirectDepsT, typename ProvidesT>
HaversackTraitsBuilder(DirectDepsT, IndirectDepsT, ProvidesT)
    -> HaversackTraitsBuilder<DirectDepsT, IndirectDepsT, ProvidesT>;

// Initial type/value to be used when accumulating with
// AccumulateHaversackTraits.
using EmptyHaversackTraitsBuilder =
    HaversackTraitsBuilder<BasicTuple<>, BasicTuple<>, BasicTuple<>>;

// Accumulation functor which appends the type being accumulated to the correct
// field of HaversackTraitsBuilder.
struct AccumulateHaversackTraits {
  template <typename Sum, typename T>
  constexpr auto operator()(Sum sum, types::Type<T> t) const {
    if constexpr (is_template_instance_v<T, Provides>) {
      return sum.ExtendProvides(GetTypesFromProvides(t));
    } else if constexpr (is_template_instance_v<T, Deps>) {
      return sum.ExtendIndirectDeps(GetTypesFromDeps((t)));
    } else if constexpr (std::is_base_of_v<CallsBase, T>) {
      return sum.ExtendIndirectDeps(GetTypesFromCalls(t));
    } else {
      return sum.ExtendDirectDeps(MakeBasicTuple(t));
    }
  }
};

template <typename... Outputs, typename Input>
auto RearrangeTuple(types::Type<std::tuple<Outputs...>>, Input input) {
  return std::make_tuple(std::move(std::get<Outputs>(input))...);
}

// Combine the PropagatedTuple and the AddedTs into an instance of
// DesiredTuple.
//
// Each AddedTs is converted according to Compatibility.
//
// - DesiredTuple is a std::tuple which should be the result type of this
// function.
// - Compatibility is an instance of CompatibleArgs where Sources is all the
// types in PropagatedTuple concatenated with all the types in AddedTs. Targets
// is all the types in DesiredTuple.
// - PropagatedTuple is the std::tuple of values which are being propagated from
// another Haversack.
// - AddedTs is all the arguments directly passed to the Haversack ctor.
template <typename DesiredTuple, typename Compatibility,
          typename PropagatedTuple, typename... AddedTs>
DesiredTuple CatAndSortTuples(types::Type<DesiredTuple> desired,
                      Compatibility compatibility,
                      PropagatedTuple propagated_tuple, AddedTs... added) {
  if constexpr (compatibility.IsCompatible()) {
    return RearrangeTuple(
        desired,
        std::tuple_cat(
            std::move(propagated_tuple),
            std::make_tuple(compatibility.Convert(std::move(added))...)));
  } else {
    return DesiredTuple();
  }
}

// Convert any supported pointer type (raw, unique, shared and with or without
// not_null) to a not_null SharedProxy. Raw pointers are converted to non-owning
// SharedProxys. Different overloads handle the different input types...
//
// Takes a raw ptr.
template <typename T>
auto AsShared(T* t) {
  if (!t) {
    std::cerr << "Pointers should never be null in haversack, but a "
              << internal::debug_type_name_v<T*> << " was null";
    assert(t);
    abort();
  }
  return SharedProxy<T>(t, [](auto&&...) { /* No-op deleter */ });
}
// Takes a unique_ptr, shared_ptr, not_null<unique_ptr>, or
// not_null<shared_ptr>.
template <typename T, typename = typename T::element_type>
auto AsShared(T t) {
  if constexpr (types::IsValidExpr(
                    t,
                    [](const auto& x)
                        -> decltype(std::declval<std::decay_t<decltype(x)>&&>()
                                        .value()) {})) {
    return AsShared(std::move(t).value());
  } else {
    if (!t) {
      std::cerr << "Pointers should never be null in haversack, but a "
                << internal::debug_type_name_v<T*> << " was null";
      assert(t);
      abort();
    }
    return SharedProxy<typename T::element_type>(std::move(t));
  }
}

// Get the type of the value inside Ptr. Template specialization failure if Ptr
// is not a supported pointer type.
template <typename Ptr>
using PtrElementType =
    typename decltype(AsShared(std::declval<Ptr&&>()))::element_type;

// HaversackTraits transforms and organizes all the template arguments to
// Haversack into different TypeSets for use for the different validations
// inside this class.
//
// This is a constexpr template constant to effectively memoize this operation
// at compile time.
template <typename... Ts>
constexpr auto kHaversackTraits =
    Accumulate(internal::AccumulateHaversackTraits(),
               internal::MakeBasicTuple(internal::types::type_c<Ts>...),
               internal::EmptyHaversackTraitsBuilder())
        .Build();

template <typename HaversackT, typename T>
using GetType = std::enable_if_t<
    Contains(types::type_c<T>, TraitsOf(types::type_c<HaversackT>).direct),
    typename decltype(internal::ThreadSafeType(types::type_c<T>))::type>;

}  // namespace internal
}  // namespace hotels::haversack

#endif  // HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_HAVERSACK_IMPL_H_
