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

// HAVERSACK_GET_TESTER_MODE is used to determine what kind of special behavior
// to apply to detect unused Haversack direct dependencies.
//
// HAVERSACK_GET_TESTER_MODE == 0
//   Normal behavior mode.
// HAVERSACK_GET_TESTER_MODE == 1
//   Identify all direct dependencies mode. When this mode is used, a
//   static_assert failure will be emitted for every combination of instantiated
//   haversacks and their direct dependencies. The compiler failure traceback
//   will include a hash for every direct dependency that is unique to that
//   Haversack and dependency combination.
// HAVERSACK_GET_TESTER_MODE > 1
//   Test if a specific dependency is actually used. HAVERSACK_GET_TESTER_MODE
//   should be set to one of the hashes from "HAVERSACK_GET_TESTER_MODE == 1"
//   mode. If the dependency is accessed (e.g. by calling Get) from the
//   haversack type associated with that hash then a static_assert failure is
//   emitted. By building the relevant code that uses this haversack,
//   compilation will fail if the dependency is used and succeed if the
//   dependency is unused!
#ifndef HAVERSACK_GET_TESTER_MODE
// If HAVERSACK_GET_TESTER_MODE is unset, use the default of 0 and unset it
// again at the end of this file.
#define HAVERSACK_GET_TESTER_MODE 0
#define UNSET_HAVERSACK_GET_TESTER_MODE
#endif

#include <algorithm>
#include <array>
#include <cassert>
#include <concepts>
#include <cstddef>
#include <functional>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>

#include "meta/basic_tuple.h"
#include "meta/function_traits.h"
#include "meta/type.h"
#include "meta/type_set.h"

#ifndef HAVERSACK_NO_ABSL

#include <absl/log/log.h>
#define HAVERSACK_DIE(msg) LOG(FATAL) << msg

#else

#include <iostream>
#define HAVERSACK_DIE(msg)       \
  std::cerr << msg << std::endl; \
  assert(false);                 \
  abort()

#endif

namespace hotels::haversack {

template <typename...>
struct Haversack;
template <typename, typename, typename = std::bool_constant<false>>
struct Tagged;
template <typename>
struct KnownThreadSafe;
template <typename>
struct Nullable;

namespace internal {
namespace internal_assert_is {

enum class Placeholder : int;

template <std::size_t N, std::size_t M, typename Compare,
          typename... ContextPtrs>
consteval void assert_is_impl(Compare cmp, ContextPtrs*...) {
  static_assert(cmp(N, M));
}

template <std::size_t N, std::size_t M, typename Context,
          typename... ContextRest, typename Compare, typename... ContextPtrs>
consteval void assert_is_helper(Compare cmp, ContextPtrs*... context_ptrs) {
  if constexpr (sizeof...(ContextRest) == 0) {
    assert_is_impl<N, M>(cmp, context_ptrs..., static_cast<Context*>(nullptr),
                         static_cast<Placeholder*>(nullptr));
  } else {
    assert_is_helper<N, M, ContextRest...>(cmp, context_ptrs...,
                                           static_cast<Context*>(nullptr),
                                           static_cast<Placeholder*>(nullptr));
  }
}

}  // namespace internal_assert_is

// A `SecurityBadge` is only constructible by its friend, `T`. By adding a
// `SecurityBadge<T>` as an argument for a function, that function can only be
// called by `T`.
template <typename T>
class SecurityBadge {
 private:
  SecurityBadge() = default;
  friend T;
};

template <typename T>
concept Nullptr = std::same_as<T, nullptr_t>;
template <typename T>
concept NullablePointer =
    htls::meta::Concept<T, htls::meta::IsTemplateInstance<std::unique_ptr>> ||
    htls::meta::Concept<T, htls::meta::IsTemplateInstance<std::shared_ptr>> ||
    std::is_pointer_v<T>;
template <typename T>
concept NullablePointerOrRRef =
    NullablePointer<T> || (std::is_rvalue_reference_v<T> &&
                           NullablePointer<std::remove_reference_t<T>>);
template <typename T>
concept NotNullPointer = requires(T t) {
  { std::move(t).value() } -> NullablePointerOrRRef;
  typename T::element_type;
  requires std::is_same_v<typename std::pointer_traits<std::remove_reference_t<
                              decltype(std::move(t).value())>>::element_type,
                          typename T::element_type>;
};
template <typename T>
concept UncoercedCtorArgImpl =
    NullablePointer<T>    //
    || NotNullPointer<T>  //
    || htls::meta::Concept<T, htls::meta::IsTemplateInstance<Tagged>>;
template <typename T>
concept UncoercedCtorArg =
    UncoercedCtorArgImpl<T> ||
    (htls::meta::Concept<
         T, htls::meta::IsTemplateInstance<std::reference_wrapper>> &&
     UncoercedCtorArgImpl<std::remove_const_t<typename T::type>> &&
     std::is_copy_constructible_v<std::remove_const_t<typename T::type>>);
template <typename T>
concept ExplicitInsertArg =
    UncoercedCtorArg<T> || std::is_same_v<T, std::nullptr_t>;

template <typename T>
concept CoercedCtorArgC =
    htls::meta::Concept<T, htls::meta::IsTemplateInstance<std::shared_ptr>> ||
    htls::meta::Concept<T, htls::meta::IsTemplateInstance<Tagged>>;
template <typename T>
concept CoercedExplicitInsertArg = CoercedCtorArgC<T> || Nullptr<T>;

template <typename T>
concept UnwrappedType =
    !htls::meta::Concept<T, htls::meta::IsTemplateInstance<Tagged>> &&
    !htls::meta::Concept<T, htls::meta::IsTemplateInstance<Nullable>> &&
    !htls::meta::Concept<T, htls::meta::IsTemplateInstance<KnownThreadSafe>>;

template <typename T>
concept HaversackInstance =
    htls::meta::Concept<typename T::HaversackT,
                        htls::meta::IsTemplateInstance<Haversack>> &&
    std::is_base_of_v<typename T::HaversackT, T>;

template <typename T>
concept IsComplete = htls::meta::IsValidExpr(
    htls::meta::type_c<T>,
    [](auto t) -> decltype(sizeof(typename decltype(t)::type)) {});

template <typename T>
struct Holder;
template <typename T>
struct DirectPropagatedTupleImpl : std::false_type {};
template <typename... Ts>
struct DirectPropagatedTupleImpl<std::tuple<Holder<Ts>...>> : std::true_type {};
template <typename T>
struct PtrPropagatedTupleImpl : std::false_type {};
template <typename... Ts>
struct PtrPropagatedTupleImpl<std::shared_ptr<const std::tuple<Holder<Ts>...>>>
    : std::true_type {};

// A std::tuple of Holders or std::shared_ptr of the former.
template <typename T>
concept PropagatedTuple =
    DirectPropagatedTupleImpl<T>::value || PtrPropagatedTupleImpl<T>::value;

}  // namespace internal

template <auto&&...>
struct Calls;

template <typename... Ts>
  requires((internal::HaversackInstance<Ts> && ...))
struct Deps;

template <typename...>
struct Provides;

namespace internal {

// Perform a static_assert on N and M using cmp. The list of Context types will
// be interleaved with Placeholder for easy parsing.
template <std::size_t N, std::size_t M, typename... Context, typename Compare>
consteval void assert_is(Compare cmp) {
  internal_assert_is::assert_is_helper<N, M, Context...>(
      cmp, static_cast<internal_assert_is::Placeholder*>(nullptr));
}

template <typename T>
using SharedProxy = std::shared_ptr<T>;

template <size_t N>
struct ConstexprString {
  consteval ConstexprString(const char (&s)[N]) {
    std::copy_n(s, N, storage.begin());
  }

  // Note: consider storing an integer hash instead of the full string here for
  // compile-time performance.
  std::array<char, N> storage;
};

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
  htls::meta::Type<Tag> tag;
  bool nullable = false;
  bool known_thread_safe = false;
  WrappingMetadataOrder order = WrappingMetadataOrder::kEnd;

  template <typename T, typename NewTag, typename Alias>
  consteval WrappedTypeMetadata<NewTag> SetTag(
      htls::meta::Type<Tagged<T, NewTag, Alias>>) const {
    WrappedTypeMetadata<NewTag> result;
    result.nullable = nullable;
    result.known_thread_safe = known_thread_safe;
    result.order = order;
    return result;
  }

  consteval WrappedTypeMetadata& SetOrder(WrappingMetadataOrder new_order) {
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
  static consteval auto Impl(htls::meta::Type<WrappedType> t) {
    constexpr htls::meta::MetaTypeFunction<std::add_const> add_const;
    constexpr htls::meta::MetaTypeFunction<std::remove_const> remove_const;
    if constexpr (htls::meta::IsTemplateInstance<KnownThreadSafe>(t)) {
      auto [result, metadata] =
          Impl(htls::meta::get<0>(htls::meta::AsTuple(t)));
      metadata.known_thread_safe = true;
      return std::make_pair(
          remove_const(result),
          metadata.SetOrder(WrappingMetadataOrder::kKnownThreadSafe));
    } else if constexpr (htls::meta::IsTemplateInstance<Nullable>(t)) {
      auto [result, metadata] =
          Impl(htls::meta::get<0>(htls::meta::AsTuple(t)));
      metadata.nullable = true;
      return std::make_pair(
          result, metadata.SetOrder(WrappingMetadataOrder::kNullable));
    } else if constexpr (htls::meta::IsTemplateInstance<Tagged>(t)) {
      auto [result, metadata] =
          Impl(htls::meta::get<0>(htls::meta::AsTuple(t)));
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
  consteval auto operator()(htls::meta::Type<WrappedType> t) const {
    constexpr std::pair pair = Impl(htls::meta::type_c<WrappedType>);
    constexpr WrappedTypeMetadata metadata = pair.second;
    static_assert(metadata.order != WrappingMetadataOrder::kOutOfOrder,
                  "See internal::WrappingMetadataOrder for more information "
                  "about type wrapping order.");
    return pair.first;
  }
  template <typename WrappedType>
  consteval auto GetMetadata(htls::meta::Type<WrappedType> t) const {
    return Impl(t).second;
  }
};
inline constexpr UnwrapTypeWrappersFunctor kUnwrapTypeWrappers;
template <typename WrappedType>
consteval bool IsTagged(htls::meta::Type<WrappedType> t) {
  return kUnwrapTypeWrappers.GetMetadata(t).tag != htls::meta::type_c<void>;
}
template <typename WrappedType>
consteval auto GetTag(htls::meta::Type<WrappedType> t) {
  return kUnwrapTypeWrappers.GetMetadata(t).tag;
}
template <typename WrappedType>
consteval bool IsNullable(htls::meta::Type<WrappedType> t) {
  return kUnwrapTypeWrappers.GetMetadata(t).nullable;
}
template <typename WrappedType>
consteval bool IsKnownThreadSafe(htls::meta::Type<WrappedType> t) {
  return kUnwrapTypeWrappers.GetMetadata(t).known_thread_safe;
}
template <CoercedExplicitInsertArg CtorArg>
consteval auto GetDisplayableCtorArgType(htls::meta::Type<CtorArg> t) {
  if constexpr (t == htls::meta::type_c<std::nullptr_t>) {
    return htls::meta::type_c<void>;
  } else if constexpr (IsTagged(t)) {
    return t;
  } else {
    return htls::meta::type_c<typename CtorArg::element_type>;
  }
}
// Deduce a type from a `CoercedCtorArgC`. We don't want to deduce
// KnownThreadSafe here because we don't want to default to inserting
// KnownThreadSafe types.
template <CoercedCtorArgC InsertArg>
consteval auto DeduceWrappedTypeFromInsertArg(htls::meta::Type<InsertArg> t) {
  constexpr htls::meta::MetaTypeFunction<std::remove_const> remove_const;
  return remove_const(GetDisplayableCtorArgType(t));
}
// Deduce a potentially KnownThreadSafe type from a `CoercedCtorArgC`. We want
// to deduce KnownThreadSafe here because we can rely on tag matching to get the
// right final type.
template <CoercedExplicitInsertArg MakeTaggedArg>
consteval auto DeduceWrappedTypeFromMakeTaggedArg(
    htls::meta::Type<MakeTaggedArg> t) {
  constexpr htls::meta::MetaTypeFunction<std::remove_const> remove_const;
  constexpr htls::meta::MetaValueFunction<std::is_const> is_const;
  constexpr auto display = GetDisplayableCtorArgType(t);
  if constexpr (is_const(display)) {
    return remove_const(GetDisplayableCtorArgType(t));
  } else {
    return htls::meta::type_c<
        KnownThreadSafe<typename decltype(display)::type>>;
  }
}

// Forward decl for friends.
struct HaversackTestUtil;

// "Holder" type to own a WrappedType in the Haversack. Especially useful for
// discriminating between Nullable<WrappedType> and WrappedType which both are
// "const WrappedType*".
//
// value is nullable to allow for MakeFakeHaversack in testing. Nulability
// invariants are enforced during Haversack construction in production.
//
// Note: Holder's implementation is very similar to Tagged. The main differences
// are:
//   - Tagged can hold a `void` for an undeduced type, but Holder always holds a
//   non-void type.
//   - Holder enforces non-nullness for non-Nullable types, but Tagged does not
//   to enforce non-nullness allow converting from a non-Nullable to a Nullable.
template <typename WrappedType>
class Holder {
 public:
  using type = WrappedType;
  using ProxyType = SharedProxy<typename decltype(kUnwrapTypeWrappers(
      htls::meta::type_c<WrappedType>))::type>;

  template <typename T>
    requires(std::is_convertible_v<T, ProxyType> && !internal::Nullptr<T>)
  explicit Holder(T t) : value_(std::move(t)) {
    if constexpr (!internal::IsNullable(htls::meta::type_c<type>)) {
      if (!value_) {
        HAVERSACK_DIE("Pointers should never be null in haversack, but a "
                      << htls::meta::DebugTypeName(htls::meta::type_c<type>)
                      << " was null");
      }
    }
  }
  explicit Holder(WrappedType ot)
    requires(IsTagged(htls::meta::type_c<WrappedType>))
      : Holder(std::move(ot).tagged) {}
  explicit Holder(Nullptr auto)
    requires(IsNullable(htls::meta::type_c<WrappedType>))
      : value_(nullptr) {}
  // Only used in tests.
  explicit Holder(SecurityBadge<HaversackTestUtil>) : value_(nullptr) {}

  const ProxyType& value() const { return value_; }

 private:
  ProxyType value_;
};

// Sentinel type to identify the root Haversack ctor.
struct CtorSentinel {};

template <typename, typename, typename, typename, typename, typename>
struct HaversackTraits;

// Access the private traits of the Haversack.
template <typename... Ts>
consteval htls::meta::Concept<
    htls::meta::IsTemplateInstance<HaversackTraits>> auto
    TraitsOf(htls::meta::Type<Haversack<Ts...>>);
template <typename HaversackT>
consteval htls::meta::Concept<
    htls::meta::IsTemplateInstance<HaversackTraits>> auto
TraitsOf(htls::meta::Type<HaversackT> t);

template <typename SourceDisplayableCtorArg, typename... TargetWrappedTypes>
struct SourceMatches {
  static constexpr std::size_t kMatches = sizeof...(TargetWrappedTypes);

  template <
      htls::meta::Concept<htls::meta::IsTemplateInstance<SourceMatches>> U>
  static consteval void Check() {
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

  template <
      htls::meta::Concept<htls::meta::IsTemplateInstance<TargetMatches>> U>
  static consteval void Check() {
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

template <typename T>
concept CheckC =
    htls::meta::Concept<T, htls::meta::IsTemplateInstance<SourceMatches>> ||
    htls::meta::Concept<T, htls::meta::IsTemplateInstance<TargetMatches>>;

// kFindMatches is a constexpr memoization cache for FindMatchesImpl to avoid
// maximum step limit compilation errors.
template <typename TargetWrappedTypes, CoercedCtorArgC SourceCtorArg>
consteval auto FindMatchesImpl(
    htls::meta::Type<SourceCtorArg> source_ctor_arg) {
  constexpr htls::meta::MetaValueFunction<std::is_convertible> is_convertible;
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
            return htls::meta::FromTuple<SharedProxy>(
                htls::meta::MakeBasicTuple(t));
          };
          constexpr htls::meta::Type target_shared_proxy =
              shared_proxy_of(kUnwrapTypeWrappers(wrapped_target));
          return is_convertible(source_ctor_arg, target_shared_proxy);
        }
      },
      TargetWrappedTypes());
}
template <typename TargetWrappedTypes, CoercedCtorArgC SourceCtorArg>
constexpr htls::meta::BasicTuple kFindMatches =
    FindMatchesImpl<TargetWrappedTypes>(htls::meta::type_c<SourceCtorArg>);

template <typename, typename>
class CompatibleArgs;

// kGetMatchChecks is a constexpr memoization cache for GetMatchChecksImpl to
// avoid maximum step limit compilation errors.
template <htls::meta::Concept<htls::meta::IsTemplateInstance<CompatibleArgs>>
              Compatibility>
consteval auto GetMatchChecksImpl() {
  htls::meta::BasicTuple all_source_matches =
      Compatibility::FindAllSourceMatches();
  htls::meta::BasicTuple source_checks = Transform(
      [=](auto t) {
        return htls::meta::FromTuple<SourceMatches>(
            htls::meta::MakeBasicTuple(
                GetDisplayableCtorArgType(htls::meta::get<0>(t))) +
            htls::meta::get<1>(t));
      },
      all_source_matches);
  htls::meta::BasicTuple target_checks = Transform(
      [=](auto t) {
        return htls::meta::FromTuple<TargetMatches>(
            htls::meta::MakeBasicTuple(t) +
            Transform([](auto u) { return GetDisplayableCtorArgType(u); },
                      Compatibility::FindTargetMatches(t, all_source_matches)));
      },
      typename Compatibility::TargetWrappedTypes());
  return source_checks + target_checks;
}
template <htls::meta::Concept<htls::meta::IsTemplateInstance<CompatibleArgs>>
              Compatibility>
constexpr htls::meta::BasicTuple kGetMatchChecks =
    GetMatchChecksImpl<Compatibility>();

// ConvertOne<TargetWrappedType> has an operator() to convert any valid
// CoercedExplicitInsertArg to a Holder<TargetWrappedType>.
template <typename TargetWrappedType,
          UnwrappedType TargetUnwrappedType =
              typename decltype(kUnwrapTypeWrappers(
                  htls::meta::type_c<TargetWrappedType>))::type>
struct ConvertOne {
  template <CoercedExplicitInsertArg U>
    requires(std::is_constructible_v<Holder<TargetWrappedType>, U &&>)
  Holder<TargetWrappedType> operator()(U u) const {
    return Holder<TargetWrappedType>{std::move(u)};
  }
};

// A Converter<TargetWrappedTypes...> has operator() to convert any valid
// CoercedExplicitInsertArg to whichever Holder<TargetWrappedTypes> it is
// compatible with. If the argument is not a one-to-one match with one of
// TargetWrappedTypes, the operator() invocation is a template error.
template <typename... TargetWrappedTypes>
struct Converter : ConvertOne<TargetWrappedTypes>... {
  using ConvertOne<TargetWrappedTypes>::operator()...;
};

// Determine if and how the types in SourceCtorArgs can be uniquely converted to
// the types in TargetWrappedTypes.
//
// - SourceCtorArgs is a htls::meta::BasicTuple of CtorArg Type instances which
// represents
//   the types of the arguments actually provided.
//
// - TargetWrappedTypes is a htls::meta::BasicTuple of Type types which
// represents the types
//   of the parameters expected to a certain invokable.
template <htls::meta::TypeTuple SourceCtorArgs,
          htls::meta::TypeTuple TargetWrappedTypes_>
  requires(AllOf(
      []<typename T>(htls::meta::Type<T> t) { return CoercedCtorArgC<T>; },
      SourceCtorArgs()))
class CompatibleArgs<SourceCtorArgs, TargetWrappedTypes_> {
 public:
  using TargetWrappedTypes = TargetWrappedTypes_;
  // Find the unique type in TargetWrappedTypes that SourceCtorArg can be
  // implicitly converted to. If SourceCtorArg cannot be converted to anything
  // or more than one thing, returns Type<void> instead.
  template <CoercedCtorArgC SourceCtorArg>
  static consteval auto FindMatch(htls::meta::Type<SourceCtorArg> source) {
    htls::meta::BasicTuple matches = FindMatches(source);
    if constexpr (size(matches) == 1) {
      return htls::meta::get<0>(matches);
    } else {
      return htls::meta::type_c<void>;
    }
  }

  // Find all the TargetWrappedTypes that source can be converted to, as a
  // htls::meta::BasicTuple of Types.
  template <CoercedCtorArgC SourceCtorArg>
  static consteval auto FindMatches(htls::meta::Type<SourceCtorArg> source) {
    return kFindMatches<TargetWrappedTypes, SourceCtorArg>;
  }
  // Find all the matching TargetWrappedTypes for each SourceCtorArg match as a
  // htls::meta::BasicTuple of BasicTuples in the form:
  //  htls::meta::BasicTuple<
  //    htls::meta::BasicTuple<
  //      FirstSourceCtorArg,
  //      htls::meta::BasicTuple<FirstMatchingTargetWrappedTypes...>>,
  //    htls::meta::BasicTuple<
  //      SecoundSourceCtorArg,
  //      htls::meta::BasicTuple<SecondMatchingTargetWrappedTypes...>>,
  //    ...>
  static consteval auto FindAllSourceMatches() {
    return Transform(
        [](auto t) { return htls::meta::MakeBasicTuple(t, FindMatches(t)); },
        SourceCtorArgs());
  }
  // Find all the SourceCtorArgs that can be converted to a given
  // WrappedTargetType. all_source_matches is of the form returned by
  // FindAllSourceMatches.
  template <typename TargetWrappedType, typename... AllSourceMatches>
  static consteval auto FindTargetMatches(
      htls::meta::Type<TargetWrappedType> target,
      htls::meta::BasicTuple<AllSourceMatches...> all_source_matches) {
    return Transform(
        [](auto source_matches) { return htls::meta::get<0>(source_matches); },
        Filter(
            [=](auto source_matches) consteval {
              return htls::meta::Contains(
                  target,
                  Apply(
                      [](auto... matching_wrapped_target_types) consteval {
                        return htls::meta::MakeTypeSet(
                            matching_wrapped_target_types...);
                      },
                      htls::meta::get<1>(source_matches)));
            },
            all_source_matches));
  }

 public:
  consteval CompatibleArgs(SourceCtorArgs, TargetWrappedTypes) {}

  consteval bool QuickTest() const {
    // SourceCtorArgs and TargetWrappedTypes are bijective (one-to-one and onto)
    // if:
    // - They are the same size (the size comparison) and
    // - Each SourceCtorArgs has a one-to-one mapping in TargetWrappedTypes (all
    // of the conversions with converter are valid).
    return size(htls::meta::type_c<SourceCtorArgs>) ==
               size(htls::meta::type_c<TargetWrappedTypes>) &&
           AllOf(
               [](auto t) {
                 return htls::meta::IsValidExpr(
                     t,
                     [](auto u)
                         -> decltype(converter_(
                             std::declval<typename decltype(u)::type&&>())) {});
               },
               SourceCtorArgs());
  }

  // Returns true if SourceCtorArgs is compatible with TargetWrappedTypes.
  consteval bool IsCompatible() const {
    constexpr CompatibleArgs self{SourceCtorArgs(), TargetWrappedTypes()};
    if constexpr (self.QuickTest()) {
      return true;
    } else {
      return AllOf([](auto t) { return decltype(t)::type::kMatches == 1; },
                   kGetMatchChecks<CompatibleArgs>);
    }
  }

  // Returns a htls::meta::BasicTuple of SourceMatches for each source and
  // TargetMatches for each target.
  consteval auto GetMatchChecks() const {
    constexpr CompatibleArgs self{SourceCtorArgs(), TargetWrappedTypes()};
    // If it is compatible, then there are no failing match checks.
    // kGetMatchChecks is very expensive so we want to avoid evaluating it if we
    // know that it will return nothing.
    if constexpr (self.QuickTest()) {
      return htls::meta::MakeBasicTuple();
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

  typename decltype(htls::meta::FromTuple<Converter>(
      TargetWrappedTypes()))::type converter_;
};
template <typename SourceCtorArgs, typename TargetWrappedTypes>
CompatibleArgs(SourceCtorArgs, TargetWrappedTypes)
    -> CompatibleArgs<SourceCtorArgs, TargetWrappedTypes>;

// True if T is an instance of Haversack or a Haversack subclass.
template <typename T>
consteval auto IsHaversack(htls::meta::Type<T> t) {
  if constexpr (htls::meta::IsValidExpr(t,
                                        []<typename U>(htls::meta::Type<U> u) ->
                                        typename U::HaversackT {})) {
    return std::is_base_of_v<typename T::HaversackT, T> &&
           htls::meta::IsTemplateInstance<Haversack>(
               htls::meta::type_c<typename T::HaversackT>);
  } else {
    return false;
  }
}

// True if there is at least one type in Ts and the first one is a Haversack.
template <typename... Ts>
consteval bool FirstIsHaversack() {
  if constexpr (sizeof...(Ts) == 0) {
    return false;
  } else {
    return [](auto first, auto... rest) {
      return IsHaversack(first);
    }(htls::meta::type_c<Ts>...);
  }
}

template <HaversackInstance ChildHaversackT, typename CallerT>
struct ChildHaversackMetadata {
  using ChildHaversack = ChildHaversackT;
  using Caller = CallerT;
  static constexpr bool has_caller =
      htls::meta::type_c<void> != htls::meta::type_c<Caller>;

  template <typename F, typename Parent>
    requires(HaversackInstance<Parent> || std::is_same_v<Parent, void>)
  constexpr void Visit(F f, htls::meta::Type<Parent> parent) const {
    f(htls::meta::type_c<ChildHaversack>, htls::meta::type_c<Caller>, parent);
    Transform(
        [&](auto grandchild_haversack) {
          typename decltype(grandchild_haversack)::type().Visit(
              f, htls::meta::type_c<ChildHaversack>);
          return false;
        },
        TraitsOf(htls::meta::type_c<ChildHaversack>).child_haversacks.Tuple());
  }
};
template <HaversackInstance ChildHaversackT, typename CallerT>
consteval auto MakeChildHaversackMetadata(htls::meta::Type<ChildHaversackT>,
                                          htls::meta::Type<CallerT>) {
  return htls::meta::type_c<ChildHaversackMetadata<ChildHaversackT, CallerT>>;
}

// All types specified in Deps::Value must exist in the Haversack, but are not
// directly readable unless the type is also a direct dependency.
//
// Each Ts must be a Haversack type and all the types in those haversacks are
// flattened into the result of Value.
template <HaversackInstance... Ts>
consteval auto GetTypesFromDeps(htls::meta::Type<Deps<Ts...>>) {
  return htls::meta::MakeBasicTuple(MakeChildHaversackMetadata(
      htls::meta::type_c<Ts>, htls::meta::type_c<void>)...);
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
template <std::derived_from<CallsBase> CallsInstance>
consteval auto GetTypesFromCalls(htls::meta::Type<CallsInstance>) {
  auto get_arguments_that_are_haversacks = [](auto individual_calls) {
    using FuncT = typename decltype(individual_calls)::type::FirstType;
    htls::meta::BasicTuple args = htls::meta::AsTuple(
        htls::meta::type_c<typename htls::meta::function_traits<FuncT>::args>);
    return Transform(
        [&](auto child_haversack) {
          return MakeChildHaversackMetadata(child_haversack, individual_calls);
        },
        Filter([](auto arg) { return IsHaversack(arg); },
               Transform(htls::meta::MetaTypeFunction<std::decay>(), args)));
  };
  // For each function in Funcs, htls::meta::get all of its arguments that are
  // Haversacks as a htls::meta::BasicTuple. Concatenate each Funcs result into
  // one htls::meta::BasicTuple of Haversacks then create a Deps from those
  // types and use its Haversack flattening logic.
  return Flatten(Transform(get_arguments_that_are_haversacks,
                           typename CallsInstance::IndividualCalls()));
}

template <htls::meta::Concept<
    htls::meta::IsTemplateInstance<ChildHaversackMetadata>>... Ts>
consteval auto GetAllDepsFromChildHaversackMetadatas(
    htls::meta::BasicTuple<htls::meta::Type<Ts>...>) {
  return Concat(TraitsOf(htls::meta::type_c<typename Ts::ChildHaversack>)
                    .all_deps.Tuple()...);
}

// All types specified in Provides::Value are expected to be passed to the
// Haversack ctor directly as arguments and not inherited from another Haversack
// instance.
template <typename... WrappedTypes>
consteval auto GetTypesFromProvides(
    htls::meta::Type<Provides<WrappedTypes...>>) {
  return htls::meta::MakeBasicTuple(htls::meta::type_c<WrappedTypes>...);
}

// Holds the different sets of types, organized into different categories.
//
// - DirectDepsT is a TypeSet of all of the types which are direct dependencies
// of the associated Haversack.
// - AllDepsT is a TypeSet of all of the types which are dependencies of the
// associated Haversack (direct and indirect) that are not provided by this
// Haversack or a descendant Haversack.
// - ProvidedDepsT is a TypeSet of all of the types which are provided directly
// by this Haversack.
// - ChildHaversacksT is a TypeSet of all the child haversack types as
// `ChildHaversackMetadata` template instances.
// - BuilderSuccessT is a std::bool_constant which is std::true_type iff all the
// checks in HaversackTraitsBuilder passed. This lets us skip later checks if we
// already have failures to avoid exploding the error output. Failing a
// static_assert does not stop compilation automatically so we explicitly skip
// the rest of the validation if we htls::meta::get any failures.
// - All the types in the sets are WrappedTypes.
template <
    htls::meta::Concept<htls::meta::IsTemplateInstance<htls::meta::TypeSet>>
        DirectDepsT,
    htls::meta::Concept<htls::meta::IsTemplateInstance<htls::meta::TypeSet>>
        AllDepsT,
    htls::meta::Concept<htls::meta::IsTemplateInstance<htls::meta::TypeSet>>
        ProvidedDepsT,
    htls::meta::Concept<htls::meta::IsTemplateInstance<htls::meta::TypeSet>>
        ChildHaversacksT,
    htls::meta::Concept<htls::meta::IsTemplateInstance<htls::meta::TypeSet>>
        AliasesT,
    typename BuilderSuccessT>
  requires requires {
    { BuilderSuccessT::value } -> std::same_as<const bool&>;
  }
struct HaversackTraits<DirectDepsT, AllDepsT, ProvidedDepsT, ChildHaversacksT,
                       AliasesT, BuilderSuccessT> {
  DirectDepsT direct;
  AllDepsT all_deps;
  ProvidedDepsT provided_deps;
  ChildHaversacksT child_haversacks;
  AliasesT aliases;
  using BuilderSuccessType = BuilderSuccessT;

  consteval auto AssertIsValidHaversack(
      htls::meta::Concept<htls::meta::IsTemplateInstance<CompatibleArgs>> auto
          compatibility) const {
    if constexpr (BuilderSuccessT::value) {
      return compatibility.GetMatchChecks();
    } else {
      return htls::meta::BasicTuple<>();
    }
  }

  // Gets the "Compatibility" (an instance of CompatibleArgs) of arguments to a
  // Haversack ctor. The compatibility determines if the Haversack can actually
  // be constructed from the arguments provided, and if so, how.
  //
  // - propagated_set is the TypeSet of types contained in the "other" haversack
  // being passed to this one. Empty set if there is no "other" haversack.
  // - added is the htls::meta::BasicTuple of Types of arguments being directly
  // passed into this haversack.  It is empty when converting from one haversack
  // directly to another.
  consteval auto CtorCompatibility(
      htls::meta::Concept<htls::meta::IsTemplateInstance<
          htls::meta::TypeSet>> auto propagated_set,
      htls::meta::TypeTuple auto added) {
    return CompatibleArgs(added, (all_deps - propagated_set).Tuple());
  }

  consteval auto MemberTupleType() const {
    return htls::meta::FromTuple<std::tuple>(Transform(
        [](auto t) {
          return htls::meta::FromTuple<Holder>(htls::meta::MakeBasicTuple(t));
        },
        all_deps.Tuple()));
  }
  consteval HaversackTraits(DirectDepsT direct, AllDepsT all_deps,
                            ProvidedDepsT provided_deps,
                            ChildHaversacksT child_haversacks, AliasesT aliases,
                            BuilderSuccessT)
      : direct(direct),
        all_deps(all_deps),
        provided_deps(provided_deps),
        child_haversacks(child_haversacks),
        aliases(aliases) {}
};
template <typename DirectDepsT, typename AllDepsT, typename ProvidedDepsT,
          typename ChildHaversacksT, typename AliasesT,
          typename BuilderSuccessT>
HaversackTraits(DirectDepsT, AllDepsT, ProvidedDepsT, ChildHaversacksT,
                AliasesT, BuilderSuccessT)
    -> HaversackTraits<DirectDepsT, AllDepsT, ProvidedDepsT, ChildHaversacksT,
                       AliasesT, BuilderSuccessT>;

template <typename... Ts>
consteval void AssertNoExtraProvides(htls::meta::TypeSet<Ts...>) {
  static_assert(sizeof...(Ts) == 0,
                "The set of indirect dependencies should be a superset of the "
                "\"provided\" dependencies.");
}

template <typename... Ts>
consteval auto BuildHaversackTraits(htls::meta::Type<Ts>... ts) {
  constexpr auto is_directive = []<typename T>(htls::meta::Type<T> t) {
    return htls::meta::IsTemplateInstance<Provides>(t) ||
           htls::meta::IsTemplateInstance<Deps>(t) ||
           std::conjunction_v<std::bool_constant<IsComplete<T>>,
                              std::is_base_of<CallsBase, T>>;
  };
  constexpr bool direct_after_directives = [&] {
    bool seen_direct = false;
    bool ok = true;
    (
        [&](bool is_directive_v) {
          if (is_directive_v) {
            if (seen_direct) ok = false;
          } else {
            seen_direct = true;
          }
        }(is_directive(ts)),
        ...);
    return ok;
  }();
  static_assert(direct_after_directives,
                "All `Calls` and other directives must come before any direct "
                "dependencies.");
  constexpr auto provides =
      htls::meta::Concat([]<typename T>(htls::meta::Type<T> t) {
        if constexpr (htls::meta::IsTemplateInstance<Provides>(t)) {
          return GetTypesFromProvides(t);
        } else {
          return htls::meta::MakeBasicTuple();
        }
      }(ts)...);
  constexpr auto indirect = htls::meta::Concat([]<typename T>(
                                                   htls::meta::Type<T> t) {
    if constexpr (htls::meta::IsTemplateInstance<Deps>(t)) {
      constexpr auto child_haversacks = GetTypesFromDeps(t);
      return GetAllDepsFromChildHaversackMetadatas(child_haversacks);
    } else if constexpr (std::conjunction_v<std::bool_constant<IsComplete<T>>,
                                            std::is_base_of<CallsBase, T>>) {
      constexpr auto child_haversacks = GetTypesFromCalls(t);
      return GetAllDepsFromChildHaversackMetadatas(child_haversacks);
    } else {
      return htls::meta::MakeBasicTuple();
    }
  }(ts)...);
  constexpr auto child_haversacks = htls::meta::Concat([]<typename T>(
                                                           htls::meta::Type<T>
                                                               t) {
    if constexpr (htls::meta::IsTemplateInstance<Deps>(t)) {
      return GetTypesFromDeps(t);
    } else if constexpr (std::conjunction_v<std::bool_constant<IsComplete<T>>,
                                            std::is_base_of<CallsBase, T>>) {
      return GetTypesFromCalls(t);
    } else {
      return htls::meta::MakeBasicTuple();
    }
  }(ts)...);
  constexpr auto direct = htls::meta::Concat([&]<typename T>(
                                                 htls::meta::Type<T> t) {
    if constexpr (is_directive(t)) {
      return htls::meta::MakeBasicTuple();
    } else {
      if constexpr (htls::meta::ValidExprOr(
                        t,
                        [](auto u)
                            -> std::enable_if_t<
                                htls::meta::IsTemplateInstance<Tagged>(u),
                                bool> { return decltype(u)::type::kAlias; },
                        false)) {
        return htls::meta::MakeBasicTuple(htls::meta::type_c<typename T::type>);
      } else {
        return htls::meta::MakeBasicTuple(t);
      }
    }
  }(ts)...);
  constexpr auto aliases =
      htls::meta::Concat([&]<typename T>(htls::meta::Type<T> t) {
        if constexpr (is_directive(t)) {
          return htls::meta::MakeBasicTuple();
        } else {
          if constexpr (htls::meta::ValidExprOr(
                            t,
                            [](auto u)
                                -> std::enable_if_t<
                                    htls::meta::IsTemplateInstance<Tagged>(u),
                                    bool> { return decltype(u)::type::kAlias; },
                            false)) {
            return htls::meta::MakeBasicTuple(t);
          } else {
            return htls::meta::MakeBasicTuple();
          }
        }
      }(ts)...);
  constexpr auto make_type_set = [](auto types) {
    return htls::meta::Apply(
        [](auto... ts) { return htls::meta::MakeTypeSet(ts...); }, types);
  };
  constexpr htls::meta::TypeSet direct_dep_set = make_type_set(direct);
  constexpr htls::meta::TypeSet dep_set =
      direct_dep_set | make_type_set(indirect);
  constexpr htls::meta::TypeSet provide_set = make_type_set(provides);

  constexpr bool no_direct_deps_are_provided =
      size((direct_dep_set & provide_set).Tuple()) == 0;
  constexpr htls::meta::TypeSet superfluous_provides = provide_set - dep_set;
  constexpr bool all_direct_deps_unique =
      size(direct) == size(direct_dep_set.Tuple());
  static_assert(
      no_direct_deps_are_provided,
      "A direct dependency cannot be provided by the same Haversack.");
  AssertNoExtraProvides(superfluous_provides);
  static_assert(all_direct_deps_unique,
                "Each direct dependency should be unique.");
  constexpr htls::meta::BasicTuple tags =
      Filter([](auto t) { return t != htls::meta::type_c<void>; },
             Transform([](auto t) { return GetTag(t); },
                       direct_dep_set.Tuple() + aliases));
  constexpr htls::meta::TypeSet tags_set = make_type_set(tags);
  constexpr bool all_tags_unique = size(tags) == size(tags_set.Tuple());
  static_assert(all_tags_unique,
                "A Haversack cannot have multiple direct dependencies with "
                "the same Tag.");
  constexpr bool no_tags_as_deps =
      size((direct_dep_set & tags_set).Tuple()) == 0;
  static_assert(no_tags_as_deps, "Don't use Tag types as dependencies.");

  constexpr htls::meta::TypeSet aliases_set = make_type_set(aliases);
  constexpr htls::meta::TypeSet aliased_set = make_type_set(Transform(
      []<typename T>(htls::meta::Type<T>) {
        return htls::meta::type_c<typename T::type>;
      },
      aliases));

  // Failing a static_assert does not interrupt compilation so we still
  // explicitly pass this BuilderSuccessT every time.
  using BuilderSuccessT = std::bool_constant<
      no_direct_deps_are_provided && size(superfluous_provides.Tuple()) == 0 &&
      all_direct_deps_unique && all_tags_unique && no_tags_as_deps>;
  return HaversackTraits(direct_dep_set, dep_set - provide_set, provide_set,
                         make_type_set(child_haversacks), aliases_set,
                         BuilderSuccessT());
}

template <typename... Outputs>
auto RearrangeTuple(
    htls::meta::Type<std::tuple<Outputs...>>,
    htls::meta::Concept<htls::meta::IsTemplateInstance<std::tuple>> auto
        input) {
  return std::make_shared<const std::tuple<Outputs...>>(
      std::move(std::get<Outputs>(input))...);
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
template <htls::meta::Concept<htls::meta::IsTemplateInstance<std::tuple>>
              DesiredTuple,
          typename... Propagateds>
std::shared_ptr<const DesiredTuple> CatAndSortTuples(
    htls::meta::Type<DesiredTuple> desired,
    htls::meta::Concept<htls::meta::IsTemplateInstance<CompatibleArgs>> auto
        compatibility,
    std::tuple<Propagateds...> propagated_tuple,
    CoercedCtorArgC auto... added) {
  if constexpr (compatibility.IsCompatible()) {
    return RearrangeTuple(
        desired,
        std::tuple_cat(
            std::move(propagated_tuple),
            std::make_tuple(compatibility.Convert(std::move(added))...)));
  } else {
    return std::make_shared<const DesiredTuple>();
  }
}
template <htls::meta::Concept<htls::meta::IsTemplateInstance<std::tuple>>
              DesiredTuple,
          typename... Propagateds>
std::shared_ptr<const DesiredTuple> CatAndSortTuples(
    htls::meta::Type<DesiredTuple> desired,
    htls::meta::Concept<htls::meta::IsTemplateInstance<CompatibleArgs>> auto
        compatibility,
    std::shared_ptr<const std::tuple<Propagateds...>> propagated_tuple,
    CoercedCtorArgC auto... added) {
  if constexpr (compatibility.IsCompatible()) {
    if constexpr (desired ==
                      htls::meta::type_c<
                          std::remove_cvref_t<decltype(*propagated_tuple)>> &&
                  sizeof...(added) == 0) {
      return propagated_tuple;
    } else {
      return CatAndSortTuples(desired, compatibility, *propagated_tuple,
                              std::move(added)...);
    }
  } else {
    return std::make_shared<const DesiredTuple>();
  }
}

// Convert any supported UncoercedCtorArg (raw, unique, shared and with or
// without not_null pointers or a Tagged) to a CtorArg (SharedProxy or Tagged).
// Raw pointers are converted to non-owning `SharedProxy`s. Different overloads
// handle the different input types...
template <typename T>
CoercedCtorArgC auto CoerceCtorArg(T* t) {
  return SharedProxy<T>(t, [](auto&&...) { /* No-op deleter */ });
}
template <NullablePointer T>
CoercedCtorArgC auto CoerceCtorArg(T t) {
  return SharedProxy<typename T::element_type>(std::move(t));
}
template <NotNullPointer T>
CoercedCtorArgC auto CoerceCtorArg(T t) {
  return CoerceCtorArg(std::move(t).value());
}
template <typename T, typename Tag>
CoercedCtorArgC auto CoerceCtorArg(Tagged<T, Tag> tagged) {
  return tagged;
}
template <typename T>
  requires(UncoercedCtorArg<std::remove_const_t<T>>)
CoercedCtorArgC auto CoerceCtorArg(std::reference_wrapper<T> t) {
  return CoerceCtorArg(t.get());
}

template <ExplicitInsertArg Arg>
CoercedExplicitInsertArg auto CoerceExplicitInsertArg(Arg arg) {
  if constexpr (std::is_same_v<Arg, std::nullptr_t>) {
    return nullptr;
  } else {
    return CoerceCtorArg(std::move(arg));
  }
}

template <typename T>
using CoercedCtorArg = decltype(CoerceCtorArg(std::declval<T&&>()));

// HaversackTraits transforms and organizes all the template arguments to
// Haversack into different TypeSets for use for the different validations
// inside this class.
//
// This is a constexpr template constant to effectively memoize this operation
// at compile time.
template <typename... Ts>
constexpr HaversackTraits kHaversackTraits =
    BuildHaversackTraits(htls::meta::type_c<Ts>...);

// Access the private traits of the Haversack.
//
// This should be eagerly instantiated for each haversack instance to ensure
// that all static_assert failures are emitted for
// "HAVERSACK_GET_TESTER_MODE == 1".
template <typename... Ts>
consteval htls::meta::Concept<
    htls::meta::IsTemplateInstance<HaversackTraits>> auto
TraitsOf(htls::meta::Type<Haversack<Ts...>>) {
#if HAVERSACK_GET_TESTER_MODE == 1
  // Emit a static_assert failure for each direct dependency in the haversack.
  Transform(
      [](auto t) {
        using T = typename decltype(t)::type;
        assert_is<
            htls::meta::internal_type_set::FnvHash64<Haversack<Ts...>, T>(), 0,
            Haversack<Ts...>, T>(std::equal_to<>{});
        return nullptr;
      },
      internal::kHaversackTraits<Ts...>.direct.Tuple());
#endif
  return internal::kHaversackTraits<Ts...>;
}
// Overload to dispatch to the actual Haversack<...> type instead of the
// subtype.
template <typename HaversackT>
consteval htls::meta::Concept<
    htls::meta::IsTemplateInstance<HaversackTraits>> auto
TraitsOf(htls::meta::Type<HaversackT> t) {
  if constexpr (t != htls::meta::type_c<typename HaversackT::HaversackT>) {
    return TraitsOf(htls::meta::type_c<typename HaversackT::HaversackT>);
  }
}

// T can be a WrappedType or a Tag.
template <typename T, HaversackInstance HaversackT>
struct GetSharedHelper {
  using MemberTupleType =
      typename decltype(TraitsOf(htls::meta::type_c<HaversackT>)
                            .MemberTupleType())::type;
  static const auto& Get(const MemberTupleType& members) {
    using HolderT = Holder<typename decltype(GetMatchingWrappedType())::type>;
#if HAVERSACK_GET_TESTER_MODE > 1
    // Assert iff the hash provided to HAVERSACK_GET_TESTER_MODE matches the
    // hash of this HaversackT and wrapped type being gotten.
    //
    // If this assert triggers, then the dependency associated with this hash is
    // USED, and if this assert never triggers for a hash then that dependency
    // is UNUSED.
    assert_is<HAVERSACK_GET_TESTER_MODE,
              htls::meta::internal_type_set::FnvHash64<
                  HaversackT, typename HolderT::type>(),
              HaversackT, typename HolderT::type>(std::not_equal_to<>{});
#endif
    const auto& value = std::get<HolderT>(members).value();
    // Non-nullness invariant is enforced by the ctor in production, so this
    // assert exists to prevent segfaults in tests.
#ifndef NDEBUG
    if (!IsNullable(GetMatchingWrappedType()) && !value) {
      HAVERSACK_DIE(
          "A value for \""
          << htls::meta::DebugTypeName(htls::meta::type_c<T>) << "\" was not "
          << "injected with MakeFakeHaversack but is used in this test.");
    }
#endif
    return value;
  }
  // Returns a Type<U> where U is the WrappedType in the Haversack that T
  // matches. If there is no match, it returns Type<void>.
  consteval static auto GetMatchingWrappedType() {
    constexpr auto tag_matches = [](auto t_arg) {
      constexpr htls::meta::Type t = t_arg;
      return GetTag(t) == htls::meta::type_c<T>;
    };

    if constexpr (Contains(htls::meta::type_c<T>,
                           TraitsOf(htls::meta::type_c<HaversackT>).direct)) {
      return htls::meta::type_c<T>;
    } else if constexpr (constexpr htls::meta::BasicTuple matching_tags =
                             Filter(tag_matches,
                                    TraitsOf(htls::meta::type_c<HaversackT>)
                                        .direct.Tuple());
                         size(matching_tags) == 1) {
      htls::meta::Type real_type = htls::meta::get<0>(matching_tags);
      return real_type;
    } else if constexpr (constexpr htls::meta::BasicTuple matching_aliases =
                             Filter(tag_matches,
                                    TraitsOf(htls::meta::type_c<HaversackT>)
                                        .aliases.Tuple());
                         size(matching_aliases) == 1) {
      htls::meta::Type real_type = htls::meta::get<0>(matching_aliases);
      return htls::meta::type_c<typename decltype(real_type)::type::type>;
    } else {
      return htls::meta::type_c<void>;
    }
  }
};

}  // namespace internal
}  // namespace hotels::haversack

#ifdef UNSET_HAVERSACK_GET_TESTER_MODE
#undef HAVERSACK_GET_TESTER_MODE
#undef UNSET_HAVERSACK_GET_TESTER_MODE
#endif

#undef HAVERSACK_DIE

#endif  // HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_HAVERSACK_IMPL_H_
