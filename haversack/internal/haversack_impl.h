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
#include <cstddef>
#include <iostream>
#include <memory>
#include <tuple>
#include <type_traits>

#include "haversack/internal/basic_tuple.h"
#include "haversack/internal/type.h"
#include "haversack/internal/type_set.h"
#include "haversack/internal/util.h"

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
template <typename>
struct Nullable;
template <typename, typename>
struct Tagged;

namespace internal {

template <typename T>
using SharedProxy = std::shared_ptr<T>;

// Lower values should be outside larger values.
enum class WrappingMetadataOrder {
  kOutOfOrder = -1,
  kTag = 1,
  kNullable = 2,
  kKnownThreadSafe = 3,
  kEnd = 4
};

template <typename Tag>
struct WrappedTypeMetadata {
  types::Type<Tag> tag;
  bool nullable = false;
  bool known_thread_safe = false;
  WrappingMetadataOrder order = WrappingMetadataOrder::kEnd;

  template <typename T, typename NewTag>
  constexpr WrappedTypeMetadata<NewTag> SetTag(
      types::Type<Tagged<T, NewTag>>) const {
    WrappedTypeMetadata<NewTag> result;
    result.nullable = nullable;
    result.known_thread_safe = known_thread_safe;
    result.order = order;
    return result;
  }

  constexpr WrappedTypeMetadata& SetOrder(WrappingMetadataOrder new_order) {
    if (new_order < order)
      order = new_order;
    else
      order = WrappingMetadataOrder::kOutOfOrder;
    return *this;
  }
};

// Converts a wrapped type to a "real" type, e.g.
//
// T                            -> const T
// KnownThreadSafe<T>           -> T
// Nullable<T>                  -> const T
// Nullable<KnownThreadSafe<T>> -> T
//
// Also, builds up a WrappedTypeMetadata object with information about the type
// wrappers.
class UnwrapTypeWrappersFunctor {
  template <typename WrappedType>
  static constexpr auto Impl(types::Type<WrappedType> t) {
    constexpr types::MetaTypeFunction<std::add_const> add_const;
    constexpr types::MetaTypeFunction<std::remove_const> remove_const;
    if constexpr (is_template_instance_v<WrappedType, KnownThreadSafe>) {
      auto [result, metadata] = Impl(get<0>(types::AsTuple(t)));
      metadata.known_thread_safe = true;
      return std::make_pair(
          remove_const(result),
          metadata.SetOrder(WrappingMetadataOrder::kKnownThreadSafe));
    } else if constexpr (is_template_instance_v<WrappedType, Nullable>) {
      auto [result, metadata] = Impl(get<0>(types::AsTuple(t)));
      metadata.nullable = true;
      return std::make_pair(
          result, metadata.SetOrder(WrappingMetadataOrder::kNullable));
    } else if constexpr (is_template_instance_v<WrappedType, Tagged>) {
      auto [result, metadata] = Impl(get<0>(types::AsTuple(t)));
      return std::make_pair(
          result, metadata.SetTag(t).SetOrder(WrappingMetadataOrder::kTag));
    } else {
      static_assert(
          std::is_same_v<std::remove_reference_t<WrappedType>, WrappedType>,
          "Reference types are not allowed in Haversack.");
      static_assert(
          std::is_same_v<std::remove_const_t<WrappedType>, WrappedType>,
          "Constness is implicit in Haversack. KnownThreadSafe "
          "should be used for non-const types instead.");
      return std::make_pair(add_const(t), WrappedTypeMetadata<void>{});
    }
  }

 public:
  template <typename WrappedType>
  constexpr auto operator()(types::Type<WrappedType> t) const {
    constexpr auto pair = Impl(types::type_c<WrappedType>);
    constexpr auto metadata = pair.second;
    static_assert(metadata.order != WrappingMetadataOrder::kOutOfOrder,
                  "See internal::WrappingMetadataOrder for more information "
                  "about type wrapping order.");
    return pair.first;
  }
  template <typename WrappedType>
  constexpr auto GetMetadata(types::Type<WrappedType> t) const {
    return Impl(t).second;
  }
};
inline constexpr UnwrapTypeWrappersFunctor kUnwrapTypeWrappers;
template <typename WrappedType>
constexpr bool IsTagged(types::Type<WrappedType> t) {
  return kUnwrapTypeWrappers.GetMetadata(t).tag != types::type_c<void>;
}
template <typename WrappedType>
constexpr auto GetTag(types::Type<WrappedType> t) {
  return kUnwrapTypeWrappers.GetMetadata(t).tag;
}
template <typename WrappedType>
constexpr bool IsNullable(types::Type<WrappedType> t) {
  return kUnwrapTypeWrappers.GetMetadata(t).nullable;
}
template <typename WrappedType>
constexpr bool IsKnownThreadSafe(types::Type<WrappedType> t) {
  return kUnwrapTypeWrappers.GetMetadata(t).known_thread_safe;
}
template <typename CtorArg>
constexpr auto GetDisplayableCtorArgType(types::Type<CtorArg> t) {
  if constexpr (t == types::type_c<std::nullptr_t>) {
    return types::type_c<void>;
  } else if constexpr (IsTagged(t)) {
    return t;
  } else {
    return types::type_c<typename CtorArg::element_type>;
  }
}
template <typename CtorArg>
constexpr auto DeduceWrappedTypeFromCtorArg(types::Type<CtorArg> t) {
  constexpr types::MetaTypeFunction<std::remove_const> remove_const;
  return remove_const(GetDisplayableCtorArgType(t));
}

// "Holder" type to own a WrappedType in the Haversack. Especially useful for
// discriminating between Nullable<WrappedType> and WrappedType which both are
// "const WrappedType*".
//
// value is nullable to allow for MakeFakeHaversack in testing. Nulability
// invariants are enforced during Haversack construction in production.
template <typename WrappedType>
struct Holder {
  SharedProxy<typename decltype(kUnwrapTypeWrappers(
      types::type_c<WrappedType>))::type>
      value;
};

// Forward decl for friends.
struct HaversackTestUtil;

// Sentinel type to identify the root Haversack ctor.
struct CtorSentinel {};

// Access the private traits of the Haversack.
template <typename HaversackT>
constexpr auto TraitsOf(types::Type<HaversackT> t) {
  return HaversackT::Traits();
}

template <typename SourceDisplayableCtorArg, typename... TargetWrappedTypes>
struct SourceMatches {
  static constexpr std::size_t kMatches = sizeof...(TargetWrappedTypes);

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

template <typename TargetWrappedType, typename... SourceDisplayableTypes>
struct TargetMatches {
  static constexpr std::size_t kMatches = sizeof...(SourceDisplayableTypes);

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
template <typename TargetWrappedTypes, typename SourceCtorArg>
constexpr auto FindMatchesImpl(types::Type<SourceCtorArg> source_ctor_arg) {
  constexpr types::MetaValueFunction<std::is_convertible> is_convertible;
  return Filter(
      [=](auto wrapped_target) {
        if constexpr (IsTagged(source_ctor_arg)) {
          if constexpr (IsTagged(wrapped_target)) {
            if (GetTag(source_ctor_arg) != GetTag(wrapped_target)) {
              return false;
            }
            using TargetWrappedType = typename decltype(wrapped_target)::type;
            return std::is_convertible_v<decltype(SourceCtorArg::tagged),
                                         decltype(TargetWrappedType::tagged)>;
          } else {
            // A tagged source can only be converted to a tagged target.
            return false;
          }
        } else {
          constexpr auto shared_proxy_of = [](auto t) {
            return types::FromTuple<SharedProxy>(MakeBasicTuple(t));
          };
          constexpr auto target_shared_proxy =
              shared_proxy_of(kUnwrapTypeWrappers(wrapped_target));
          return is_convertible(source_ctor_arg, target_shared_proxy);
        }
      },
      TargetWrappedTypes());
}
template <typename TargetWrappedTypes, typename SourceCtorArg>
constexpr auto kFindMatches =
    FindMatchesImpl<TargetWrappedTypes>(types::type_c<SourceCtorArg>);

// kGetMatchChecks is a constexpr memoization cache for GetMatchChecksImpl to
// avoid maximum step limit compilation errors.
template <typename CompatibleArgsInstance>
constexpr auto GetMatchChecksImpl() {
  auto all_source_matches = CompatibleArgsInstance::FindAllSourceMatches();
  auto source_checks = Transform(
      [=](auto t) {
        return types::FromTuple<SourceMatches>(
            MakeBasicTuple(GetDisplayableCtorArgType(get<0>(t))) + get<1>(t));
      },
      all_source_matches);
  auto target_checks = Transform(
      [=](auto t) {
        return types::FromTuple<TargetMatches>(
            MakeBasicTuple(t) +
            Transform([](auto u) { return GetDisplayableCtorArgType(u); },
                      CompatibleArgsInstance::FindTargetMatches(
                          t, all_source_matches)));
      },
      typename CompatibleArgsInstance::TargetWrappedTypes());
  return source_checks + target_checks;
}
template <typename CompatibleArgsInstance>
constexpr auto kGetMatchChecks = GetMatchChecksImpl<CompatibleArgsInstance>();

// ConvertOne<TargetWrappedType> has an operator() to convert any valid CtorArg
// to a Holder<TargetWrappedType>.
template <typename TargetWrappedType,
          typename TargetUnwrappedType = typename decltype(kUnwrapTypeWrappers(
              types::type_c<TargetWrappedType>))::type>
struct ConvertOne {
  // Returns the appropriate Holder if a conversion is possible, otherwise void.
  template <typename U>
  constexpr static auto Impl(U u) {
    if constexpr (is_template_instance_v<U, std::shared_ptr> &&
                  std::is_convertible_v<U, SharedProxy<TargetUnwrappedType>>) {
      return Holder<TargetWrappedType>{std::move(u)};
    } else if constexpr (IsTagged(types::type_c<TargetWrappedType>))
      if constexpr (GetTag(types::type_c<U>) ==
                    GetTag(types::type_c<TargetWrappedType>)) {
        return Holder<TargetWrappedType>{std::move(u.tagged)};
      }
  }
  template <typename U, typename = std::enable_if_t<!std::is_same_v<
                            decltype(Impl(std::declval<U&&>())), void>>>
  constexpr Holder<TargetWrappedType> operator()(U u) const {
    auto holder = Impl(std::move(u));
    if constexpr (!IsNullable(types::type_c<TargetWrappedType>)) {
      if (!holder.value) {
        std::cerr
            << "Pointers should never be null in haversack, but a "
            << internal::debug_type_name_v<TargetUnwrappedType*> << " was null";
        assert(holder.value);
        abort();
      }
    }
    return holder;
  }
};

// A Converter<TargetWrappedTypes...> has operator() to convert any valid
// CtorArg to whichever Holder<TargetWrappedTypes> it is compatible with. If the
// argument is not a one-to-one match with one of TargetWrappedTypes, the
// operator() invocation is a template error.
template <typename... TargetWrappedTypes>
struct Converter : ConvertOne<TargetWrappedTypes>... {
  using ConvertOne<TargetWrappedTypes>::operator()...;
};

// Determine if and how the types in SourceCtorArgs can be uniquely converted to
// the types in TargetWrappedTypes.
//
// - SourceCtorArgs is a BasicTuple of CtorArg Type instances which represents
//   the types of the arguments actually provided.
//
// - TargetWrappedTypes is a BasicTuple of Type types which represents the types
//   of the parameters expected to a certain invokable.
template <typename SourceCtorArgs, typename TargetWrappedTypes_>
class CompatibleArgs {
 public:
  using TargetWrappedTypes = TargetWrappedTypes_;
  // Find the unique type in TargetWrappedTypes that SourceCtorArg can be
  // implicitly converted to. If SourceCtorArg cannot be converted to anything
  // or more than one thing, returns Type<void> instead.
  template <typename SourceCtorArg>
  static constexpr auto FindMatch(types::Type<SourceCtorArg> source) {
    auto matches = FindMatches(source);
    if constexpr (size(matches) == 1) {
      return get<0>(matches);
    } else {
      return types::type_c<void>;
    }
  }

  // Find all the TargetWrappedTypes that source can be converted to, as a
  // BasicTuple of Types.
  template <typename SourceCtorArg>
  static constexpr auto FindMatches(types::Type<SourceCtorArg> source) {
    return kFindMatches<TargetWrappedTypes, SourceCtorArg>;
  }
  // Find all the matching TargetWrappedTypes for each SourceCtorArg match as a
  // BasicTuple of BasicTuples in the form:
  //  BasicTuple<
  //    BasicTuple<
  //      FirstSourceCtorArg,
  //      BasicTuple<FirstMatchingTargetWrappedTypes...>>,
  //    BasicTuple<
  //      SecoundSourceCtorArg,
  //      BasicTuple<SecondMatchingTargetWrappedTypes...>>,
  //    ...>
  static constexpr auto FindAllSourceMatches() {
    return Transform([](auto t) { return MakeBasicTuple(t, FindMatches(t)); },
                     SourceCtorArgs());
  }
  // Find all the SourceCtorArgs that can be converted to a given
  // WrappedTargetType. all_source_matches is of the form returned by
  // FindAllSourceMatches.
  template <typename TargetWrappedType, typename... AllSourceMatches>
  static constexpr auto FindTargetMatches(
      types::Type<TargetWrappedType> target,
      BasicTuple<AllSourceMatches...> all_source_matches) {
    return Transform(
        [](auto source_matches) { return get<0>(source_matches); },
        Filter(
            [=](auto source_matches) constexpr {
              return types::Contains(
                  target,
                  Apply(
                      [](auto... matching_wrapped_target_types) constexpr {
                        return types::MakePrevalidatedTypeSet(
                            matching_wrapped_target_types...);
                      },
                      get<1>(source_matches)));
            },
            all_source_matches));
  }

 public:
  constexpr CompatibleArgs(SourceCtorArgs, TargetWrappedTypes) {}

  constexpr bool QuickTest() const {
    // SourceCtorArgs and TargetWrappedTypes are bijective (one-to-one and onto)
    // if:
    // - They are the same size (the size comparison) and
    // - Each SourceCtorArgs has a one-to-one mapping in TargetWrappedTypes (all
    // of the conversions with converter are valid).
    return size(types::type_c<SourceCtorArgs>) ==
               size(types::type_c<TargetWrappedTypes>) &&
           AllOf(
               [](auto t) {
                 return types::IsValidExpr(
                     t,
                     [](auto u)
                         -> decltype(converter_(
                             std::declval<typename decltype(u)::type&&>())) {});
               },
               SourceCtorArgs());
  }

  // Returns true if SourceCtorArgs is compatible with TargetWrappedTypes.
  constexpr bool IsCompatible() const {
    constexpr CompatibleArgs self{SourceCtorArgs(), TargetWrappedTypes()};
    if constexpr (self.QuickTest()) {
      return true;
    } else {
      return AllOf([](auto t) { return decltype(t)::type::kMatches == 1; },
                   kGetMatchChecks<CompatibleArgs>);
    }
  }

  // Returns a BasicTuple of SourceMatches for each source and TargetMatches for
  // each target.
  constexpr auto GetMatchChecks() const {
    constexpr CompatibleArgs self{SourceCtorArgs(), TargetWrappedTypes()};
    // If it is compatible, then there are no failing match checks.
    // kGetMatchChecks is very expensive so we want to avoid evaluating it if we
    // know that it will return nothing.
    if constexpr (self.QuickTest()) {
      return MakeBasicTuple();
    } else {
      return kGetMatchChecks<CompatibleArgs>;
    }
  }

  // Assuming SourceCtorArg has a unique match in TargetWrappedTypes, make a
  // Holder from the arg.
  template <typename SourceCtorArg>
  auto Convert(SourceCtorArg arg) const {
    return converter_(std::move(arg));
  }

  typename decltype(types::FromTuple<Converter>(
      TargetWrappedTypes()))::type converter_;
};
template <typename SourceCtorArgs, typename TargetWrappedTypes>
CompatibleArgs(SourceCtorArgs, TargetWrappedTypes)
    -> CompatibleArgs<SourceCtorArgs, TargetWrappedTypes>;

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
template <typename... WrappedTypes>
constexpr auto GetTypesFromProvides(types::Type<Provides<WrappedTypes...>>) {
  return MakeBasicTuple(types::type_c<WrappedTypes>...);
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
// - All the types in the sets are WrappedTypes.
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
    return CompatibleArgs(added, (all_deps - propagated_set).Tuple());
  }

  constexpr auto MemberTupleType() const {
    return types::FromTuple<std::tuple>(Transform(
        [](auto t) { return types::FromTuple<Holder>(MakeBasicTuple(t)); },
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
// - DirectDepsT is a BasicTuple of WrappedType Types which represents all the
// types which are direct dependencies that have been added to the builder so
// far.
// - IndirectDepsT is a BasicTuple of WrappedType Types which represents all the
// types which are indirect dependencies that have been added to the builder so
// far.
// - ProvidesT is a BasicTuple of WrappedType Types which represents all the
// types which are "provided" which have been added to the builder so far.
template <typename DirectDepsT, typename IndirectDepsT, typename ProvidesT>
struct HaversackTraitsBuilder {
  DirectDepsT direct;
  IndirectDepsT indirect;
  ProvidesT provides;

  template <typename WrappedTypes>
  constexpr auto ExtendDirectDeps(WrappedTypes t) const {
    return internal::HaversackTraitsBuilder(direct + t, indirect, provides);
  }
  template <typename WrappedTypes>
  constexpr auto ExtendIndirectDeps(WrappedTypes t) const {
    static_assert(size(types::type_c<DirectDepsT>) == 0,
                  "All `Calls` and other directives must come before any "
                  "direct dependencies.");
    return internal::HaversackTraitsBuilder(direct, indirect + t, provides);
  }
  template <typename WrappedTypes>
  constexpr auto ExtendProvides(WrappedTypes t) const {
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
    constexpr auto tags = Filter(
        [](auto t) { return t != types::type_c<void>; },
        Transform([](auto t) { return GetTag(t); }, direct_dep_set.Tuple()));
    constexpr auto tags_set = Apply(make_type_set, tags);
    constexpr bool all_tags_unique = size(tags) == size(tags_set.Tuple());
    static_assert(all_tags_unique,
                  "A Haversack cannot have multiple direct dependencies with "
                  "the same Tag.");
    constexpr bool no_tags_as_deps =
        size((direct_dep_set & tags_set).Tuple()) == 0;
    static_assert(no_tags_as_deps, "Don't use Tag types as dependencies.");
    // Failing a static_assert does not interrupt compilation so we still
    // explicitly pass this BuilderSuccessT every time.
    using BuilderSuccessT = std::bool_constant<
        no_direct_deps_are_provided && indirect_deps_is_superset_of_provides &&
        all_direct_deps_unique && all_tags_unique && no_tags_as_deps>;
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

// Combine the PropagatedTuple and the AddedCtorArgs into an instance of
// DesiredTuple.
//
// Each AddedCtorArgs is converted according to Compatibility.
//
// - DesiredTuple is a std::tuple which should be the result type of this
// function.
// - Compatibility is an instance of CompatibleArgs where SourceCtorArgs is all
// the types in PropagatedTuple concatenated with all the types in
// AddedCtorArgs. TargetWrappedTypes is all the types in DesiredTuple.
// - PropagatedTuple is the std::tuple of values which are being propagated from
// another Haversack.
// - AddedCtorArgs is all the arguments directly passed to the Haversack ctor.
template <typename DesiredTuple, typename Compatibility,
          typename PropagatedTuple, typename... AddedCtorArgs>
DesiredTuple CatAndSortTuples(types::Type<DesiredTuple> desired,
                              Compatibility compatibility,
                              PropagatedTuple propagated_tuple,
                              AddedCtorArgs... added) {
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

// Convert any supported UncoercedCtorArg (raw, unique, shared and with or
// without not_null pointers or a Tagged) to a CtorArg (SharedProxy or Tagged).
// Raw pointers are converted to non-owning SharedProxys. Different overloads
// handle the different input types...
//
// Takes a raw ptr.
template <typename T>
auto CoerceCtorArg(T* t) {
  return SharedProxy<T>(t, [](auto&&...) { /* No-op deleter */ });
}
// Takes a unique_ptr, shared_ptr, not_null<unique_ptr>, or
// not_null<shared_ptr>.
template <typename T, typename = typename T::element_type>
auto CoerceCtorArg(T t) {
  if constexpr (types::IsValidExpr(
                    t,
                    [](const auto& x)
                        -> decltype(std::declval<std::decay_t<decltype(x)>&&>()
                                        .value()) {})) {
    return CoerceCtorArg(std::move(t).value());
  } else {
    return SharedProxy<typename T::element_type>(std::move(t));
  }
}
template <typename T, typename Tag>
auto CoerceCtorArg(Tagged<T, Tag> tagged) {
  return tagged;
}
inline std::nullptr_t CoerceCtorArg(std::nullptr_t) { return nullptr; }

template <typename T>
using CoercedCtorArg = decltype(CoerceCtorArg(std::declval<T&&>()));
template <typename CtorArg>
inline constexpr bool kIsValidCtorArg = types::IsValidExpr(
    types::type_c<CtorArg>,
    [](auto ctor_arg)
        -> decltype(CoerceCtorArg(
            std::declval<typename decltype(ctor_arg)::type&&>())) {});

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

// T can be a WrappedType or a Tag.
template <typename T, typename HaversackT>
struct GetSharedHelper {
  using MemberTupleType = typename decltype(TraitsOf(types::type_c<HaversackT>)
                                                .MemberTupleType())::type;
  static const auto& Get(const MemberTupleType& members) {
    using HolderT = Holder<typename decltype(GetMatchingWrappedType())::type>;
    const auto& value = std::get<HolderT>(members).value;
    // Non-nullness invariant is enforced by the ctor in production, so this
    // assert exists to prevent segfaults in tests.
#ifndef NDEBUG
    if (!IsNullable(GetMatchingWrappedType()) && !value) {
      std::cerr << "A value for \""
                << internal::debug_type_name_v<T> << "\" was not "
                << "injected with MakeFakeHaversack but is used in this test.";
      assert(value);
      abort();
    }
#endif
    return value;
  }
  // Returns a Type<U> where U is the WrappedType in the Haversack that T
  // matches. If there is no match, it returns void (note Type<void>).
  constexpr static auto GetMatchingWrappedType() {
    if constexpr (Contains(types::type_c<T>,
                           TraitsOf(types::type_c<HaversackT>).direct)) {
      return types::type_c<T>;
    } else {
      constexpr auto matching_tags = Filter(
          [](auto t_arg) {
            constexpr auto t = t_arg;
            return GetTag(t) == types::type_c<T>;
          },
          TraitsOf(types::type_c<HaversackT>).direct.Tuple());
      if constexpr (size(matching_tags) == 1) {
        auto real_type = get<0>(matching_tags);
        return real_type;
      }
    }
  }
};

}  // namespace internal
}  // namespace hotels::haversack

#endif  // HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_HAVERSACK_IMPL_H_
