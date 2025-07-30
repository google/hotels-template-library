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

#ifndef HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_TYPE_SET_H_
#define HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_TYPE_SET_H_

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <type_traits>
#include <utility>

#include "meta/basic_tuple.h"
#include "meta/type.h"

namespace htls::meta {
namespace internal_type_set {

template <typename... Ts>
using TypeTuple = BasicTuple<Type<Ts>...>;

struct TypeSetCtorSentinel {
  // Disable ALL aggregate init.
  constexpr explicit TypeSetCtorSentinel() {}
};

template <typename T>
struct MembershipTester {};

}  // namespace internal_type_set

// Instances of TypeSet are contain a unique set of types. This is enforced by
// the constructor/MakeTypeSet so a TypeSet "type" is not necessarily valid.
//
// This type should only be instantiated by MakeTypeSet because Ts are expected
// to be ordered (by hash) and unique.
template <typename... Ts>
struct TypeSet : internal_type_set::MembershipTester<Type<Ts>>... {
  constexpr TypeSet()
    requires(sizeof...(Ts) == 0)
  = default;
  constexpr explicit TypeSet(internal_type_set::TypeSetCtorSentinel,
                             htls::meta::BasicTuple<Type<Ts>...>) {}

  static constexpr htls::meta::BasicTuple<Type<Ts>...> Tuple() { return {}; }
};
template <typename... Ts>
TypeSet(internal_type_set::TypeSetCtorSentinel,
        htls::meta::BasicTuple<Type<Ts>...>) -> TypeSet<Ts...>;

namespace internal_type_set {

// Basic constexpr c-string hashing function.
template <typename... Ts>
constexpr uint64_t FnvHash64() {
  // __PRETTY_FUNCTION__ is a c-string that contains a string representation of
  // Ts, so we use __PRETTY_FUNCTION__ as the basis of the hash.
  const char* str = __PRETTY_FUNCTION__;
  constexpr uint64_t kFnvPrime = 0x00000100000001B3;
  constexpr uint64_t kFnvOffsetBias = 0xcbf29ce484222325;
  uint64_t hash = kFnvOffsetBias;
  while (*str != 0) {
    hash = (hash ^ *(str++)) * kFnvPrime;
  }
  return hash;
}
template <typename... Ts>
constexpr uint64_t kTypeHash = FnvHash64<Ts...>();

// An object holding some metadata about a type to enable comparisons of types.
// The type order is derived from the hash of the type, therefore:
//   1. All equivalent types will have an equivalent hash and therefore be
//      sorted near each other.
//   2. Inequivalent types with hash collisions will be sorted near each other.
//
// The hash is not enough to guarantee equivalence of types so this TypeInfo
// alone cannot be used to establish type equivalence. TypeInfo can be used to
// establish type inequivalence, though.
struct TypeInfo {
  uint64_t hash = {};
  std::size_t index = {};

  // Arbitrary strong-ordering that compares hashes first to allow constant time
  // comparison most of the time.
  constexpr auto operator<=>(const TypeInfo& info) const {
    return hash <=> info.hash;
  }

  constexpr bool operator==(const TypeInfo& info) const {
    return hash == info.hash;
  }

  // Make a TypeInfo from a type T.
  template <typename T>
  constexpr static TypeInfo Make() {
    return TypeInfo{.hash = kTypeHash<T>};
  }

  // Make a sorted std::array of TypeInfos. The index of each TypeInfo is the
  // index of the associated type in Ts.
  template <typename... Ts>
  constexpr static std::array<TypeInfo, sizeof...(Ts)> MakeSortedArray() {
    std::array<TypeInfo, sizeof...(Ts)> infos = {TypeInfo::Make<Ts>()...};
    for (std::size_t i = 0; i < sizeof...(Ts); ++i) {
      infos[i].index = i;
    }
    std::sort(infos.begin(), infos.end());
    return infos;
  }

  // Same as `MakeSortedArray` except for the case when two pre-sorted inputs
  // are provided so a merge algorithm can be used to sort them together.
  template <typename... Ts, typename... Us>
  constexpr static std::array<TypeInfo, sizeof...(Ts) + sizeof...(Us)>
  MakeMergedArray(TypeTuple<Ts...>, TypeTuple<Us...>) {
    std::array<TypeInfo, sizeof...(Ts)> t_infos = {TypeInfo::Make<Ts>()...};
    std::array<TypeInfo, sizeof...(Us)> u_infos = {TypeInfo::Make<Us>()...};
    for (std::size_t i = 0; i < sizeof...(Ts); ++i) {
      t_infos[i].index = i;
    }
    for (std::size_t i = 0; i < sizeof...(Us); ++i) {
      u_infos[i].index = sizeof...(Ts) + i;
    }
    std::array<TypeInfo, sizeof...(Ts) + sizeof...(Us)> output;
    std::merge(t_infos.begin(), t_infos.end(), u_infos.begin(), u_infos.end(),
               output.begin());
    return output;
  }
};

// Create a reverse mapping of TypeInfos. reverse[n] is the index of the
// TypeInfo in infos with info.index == n. This allows looking up TypeInfo in
// the sorted infos array by the original index.
template <std::size_t N>
constexpr std::array<std::size_t, N> MakeReverse(
    const std::array<TypeInfo, N>& infos) {
  std::array<std::size_t, N> reverse = {};
  for (std::size_t i = 0; i < N; ++i) {
    reverse[infos[i].index] = i;
  }
  return reverse;
}

template <typename... Ts>
constexpr std::array kSortedTypeInfos = TypeInfo::MakeSortedArray<Ts...>();
template <typename T, typename U>
constexpr std::array kMergedTypeInfos = TypeInfo::MakeMergedArray(T(), U());

// If Us... is non-empty, Ts and Us are assumed to be sorted; otherwise Ts will
// be sorted.
template <typename... Ts, typename... Us>
constexpr auto GetSortedTypeInfos(TypeTuple<Ts...>, TypeTuple<Us...>) {
  if constexpr (sizeof...(Us) == 0) {
    return kSortedTypeInfos<Ts...>;
  } else {
    return kMergedTypeInfos<TypeTuple<Ts...>, TypeTuple<Us...>>;
  }
}

enum class UniqueMode { kSoFar, kThroughout, kNotThroughout };

template <typename T, typename U>
constexpr std::array kReverseIndexes =
    MakeReverse(GetSortedTypeInfos(T(), U()));

template <typename, typename = BasicTuple<>>
struct SortedTypeInfoCollection;
template <typename... Ts, typename... Us>
struct SortedTypeInfoCollection<TypeTuple<Ts...>, TypeTuple<Us...>> {
  static constexpr TypeTuple<Ts..., Us...> Types() { return {}; }
  static constexpr auto GetInfos() {
    return GetSortedTypeInfos(TypeTuple<Ts...>(), TypeTuple<Us...>());
  }

  template <UniqueMode mode, std::ptrdiff_t index>
  static constexpr bool IndexIsUnique() {
    if constexpr (mode == UniqueMode::kSoFar) {
      return IndexIsUniqueSoFar<index>();
    } else if constexpr (mode == UniqueMode::kThroughout) {
      return IndexIsUniqueThroughout<index>();
    } else if constexpr (mode == UniqueMode::kNotThroughout) {
      return !IndexIsUniqueThroughout<index>();
    } else {
      static_assert(mode == UniqueMode::kSoFar, "Should be unreachable.");
    }
  }

  // Returns true if Ts[index] is unique for all Ts with smaller indexes.
  template <std::ptrdiff_t index>
  static constexpr bool IndexIsUniqueSoFar() {
    return IndexIsUniqueImpl<index, -1, -1>();
  }

  // Returns true if Ts[index] is unique for all Ts with any index.
  template <std::ptrdiff_t index>
  static constexpr bool IndexIsUniqueThroughout() {
    return IndexIsUniqueImpl<index, -1, -1>() &&
           IndexIsUniqueImpl<index, 1, 1>();
  }

  // Returns if Ts[index] is not equal to Ts[index + compare_offset] and
  // potentially not checking other indexes in the direction of offset_delta if
  // there is a TypeInfo match but not an exact type match.
  template <std::ptrdiff_t index, std::ptrdiff_t compare_offset,
            std::ptrdiff_t offset_delta>
  static constexpr bool IndexIsUniqueImpl() {
    constexpr htls::meta::BasicTuple<Type<Ts>..., Type<Us>...> tuple;
    constexpr std::size_t rev =
        kReverseIndexes<TypeTuple<Ts...>, TypeTuple<Us...>>[index];
    constexpr std::size_t compare_index = rev + compare_offset;
    if constexpr (compare_index < 0 || compare_index >= size(tuple)) {
      // There is nothing left to check against so this index must be unique.
      return true;
    } else {
      if constexpr (GetInfos()[rev] == GetInfos()[compare_index]) {
        // This index has a matching info.
        if constexpr (htls::meta::get<index>(tuple) ==
                      htls::meta::get<GetInfos()[compare_index].index>(tuple)) {
          // The type and the info match so this index is not unique.
          return false;
        } else {
          // The info matches but not the type, but there may be other matching
          // infos that could potentially match this index's type.
          return IndexIsUniqueImpl<index, compare_offset + offset_delta,
                                   offset_delta>();
        }
      } else {
        // This index does not have a matching info so it cannot have a matching
        // type.
        return true;
      }
    }
  }
};

// Implements different set operations; e.g. union, intersection.
//
// - SortedInfos is the `SortedTypeInfoCollection`
// - mode is the comparison mode used when determining if a type is unique
// - candidate_count is the count of types that are candidates to be returned;
//   e.g. for the difference of Ts and Us, only Ts are candidates.
// - ShortCircuit is a TypeSet if short-circuiting is enabled or nullptr
//   otherwise. Some operators can short-circuit if the output size (`count`)
//   matches the candidate_count
// - indexes are the indexes of all the types.
template <typename SortedInfos, UniqueMode mode, std::size_t candidate_count,
          typename ShortCircuit, std::size_t... indexes>
constexpr auto TypeSetOperatorImpl(std::index_sequence<indexes...>,
                                   ShortCircuit sc) {
  constexpr SortedInfos type_infos;
  constexpr std::array<std::size_t, sizeof...(indexes)> mapped_indexes = [] {
    std::array<std::size_t, sizeof...(indexes)> arr;
    for (std::size_t index = 0; index < sizeof...(indexes); ++index) {
      arr[index] = SortedInfos::GetInfos()[index].index;
    }
    return arr;
  }();
  constexpr std::array<bool, sizeof...(indexes)> is_unique = {
      mapped_indexes[indexes] < candidate_count &&
      type_infos.template IndexIsUnique<mode, mapped_indexes[indexes]>()...};
  constexpr std::size_t count = [&is_unique] {
    std::size_t sum = 0;
    for (bool b : is_unique) {
      sum += b ? 1 : 0;
    }
    return sum;
  }();
  if constexpr (count == 0) {
    return TypeSet();
  } else if constexpr (count == candidate_count &&
                       !std::is_same_v<ShortCircuit, nullptr_t>) {
    return sc;
  } else {
    constexpr std::array<std::size_t, count> unique_indexes = [&] {
      std::array<std::size_t, count> unique_indexes{};
      std::size_t c = 0;

      for (std::size_t index = 0; index < sizeof...(indexes); ++index) {
        if (is_unique[index]) {
          unique_indexes[c] = index;
          ++c;
        }
      }
      return unique_indexes;
    }();
    return [&]<std::size_t... output_indexes>(
               std::index_sequence<output_indexes...>) {
      return TypeSet(
          internal_type_set::TypeSetCtorSentinel(),
          MakeBasicTuple(get<mapped_indexes[unique_indexes[output_indexes]]>(
              type_infos.Types())...));
    }(std::make_index_sequence<count>());
  }
}

}  // namespace internal_type_set

namespace internal_type_set {

template <typename Deps, typename T>
using Contains = std::is_base_of<internal_type_set::MembershipTester<T>, Deps>;

template <typename TargetDepsSet, typename NewDep>
struct AppendIfUnique;

template <typename... TargetDeps, typename NewDep>
struct AppendIfUnique<TypeSet<TargetDeps...>, TypeSet<NewDep>> {
  using type =
      std::conditional_t<Contains<TypeSet<TargetDeps...>, Type<NewDep>>::value,
                         TypeSet<TargetDeps...>,
                         TypeSet<TargetDeps..., NewDep>>;
};

template <typename TargetDepsSet, typename... Args>
struct Dedupe;

template <typename TargetDepsSet, typename NewDep, typename... MoreDeps>
struct Dedupe<TargetDepsSet, NewDep, MoreDeps...> {
  using type = Dedupe<typename AppendIfUnique<TargetDepsSet, NewDep>::type,
                      MoreDeps...>::type;
};

template <typename TargetDepsSet>
struct Dedupe<TargetDepsSet> {
  using type = TargetDepsSet;
};

template <typename... Ts>
consteval auto GetDeupdedSet() {
  return Dedupe<TypeSet<>, TypeSet<Ts>...>::type::Tuple();
}

}  // namespace internal_type_set

// True if t is in p.
template <typename T, typename... Us>
constexpr auto Contains(Type<T> t, TypeSet<Us...> tuple) {
  return internal_type_set::Contains<TypeSet<Us...>, Type<T>>::value;
}

// Append t to sum if t is not in sum.
//
// sum must be a TypeSet and t must be a Type.
template <typename... Ts, typename T>
constexpr auto AppendIfUnique(TypeSet<Ts...> sum, Type<T> t) {
  return internal_type_set::AppendIfUnique<TypeSet<Ts...>,
                                           Type<T>>::type::Tuple();
}

// Create a TypeSet instance by removing all duplicates from ts.
template <typename... Ts>
constexpr auto MakeTypeSet(Type<Ts>...) {
  // Deduping can be done cheaply, O(N) template
  // instantiations, so we do it first to avoid more expensive sorting
  // instantiations.
  //
  // If
  // https://discourse.llvm.org/t/rfc-adding-builtin-for-deduplicating-type-lists/80986
  // (b/362669344) lands then this can replaced with the clang builtin.
  constexpr auto deduped_set = internal_type_set::GetDeupdedSet<Ts...>();

  return internal_type_set::TypeSetOperatorImpl<
      internal_type_set::SortedTypeInfoCollection<
          std::remove_cvref_t<decltype(deduped_set)>>,
      internal_type_set::UniqueMode::kSoFar, size(deduped_set)>(
      std::make_index_sequence<size(deduped_set)>(), nullptr);
}

// True if the type of each ts is unique.
template <typename... Ts>
constexpr auto AllUnique(Type<Ts>... ts) {
  return sizeof...(ts) == size(MakeTypeSet(ts...).Tuple());
}

// Returns the TypeSet of types that are in t AND NOT in u.
template <typename... Ts, typename... Us>
constexpr auto operator-(TypeSet<Ts...> t, TypeSet<Us...> u) {
  if constexpr (sizeof...(Us) == 0) {
    return t;
  } else if constexpr (sizeof...(Ts) == 0) {
    return TypeSet();
  } else {
    return internal_type_set::TypeSetOperatorImpl<
        internal_type_set::SortedTypeInfoCollection<
            internal_type_set::TypeTuple<Ts...>,
            internal_type_set::TypeTuple<Us...>>,
        internal_type_set::UniqueMode::kThroughout, sizeof...(Ts)>(
        std::make_index_sequence<sizeof...(Ts) + sizeof...(Us)>(), t);
  }
}

// Returns the TypeSet of types that are in t OR u.
template <typename... Ts, typename... Us>
constexpr auto operator|(TypeSet<Ts...> t, TypeSet<Us...> u) {
  if constexpr (sizeof...(Ts) == 0) {
    return u;
  } else if constexpr (sizeof...(Us) == 0) {
    return t;
  } else {
    return MakeTypeSet(Type<Ts>()..., Type<Us>()...);
  }
}

// Returns the TypeSet of types that are in t AND u.
template <typename... Ts, typename... Us>
constexpr auto operator&(TypeSet<Ts...> t, TypeSet<Us...> u) {
  if constexpr (sizeof...(Ts) == 0 || sizeof...(Us) == 0) {
    return TypeSet();
  } else {
    return internal_type_set::TypeSetOperatorImpl<
        internal_type_set::SortedTypeInfoCollection<
            internal_type_set::TypeTuple<Ts...>,
            internal_type_set::TypeTuple<Us...>>,
        internal_type_set::UniqueMode::kNotThroughout, sizeof...(Ts)>(
        std::make_index_sequence<sizeof...(Ts) + sizeof...(Us)>(), t);
  }
}

// Returns a TypeSet containing the types that are in t XOR u.
template <typename... Ts, typename... Us>
constexpr auto operator^(TypeSet<Ts...> t, TypeSet<Us...> u) {
  if constexpr (sizeof...(Us) == 0) {
    return t;
  } else if constexpr (sizeof...(Ts) == 0) {
    return TypeSet();
  } else {
    return internal_type_set::TypeSetOperatorImpl<
        internal_type_set::SortedTypeInfoCollection<
            internal_type_set::TypeTuple<Ts...>,
            internal_type_set::TypeTuple<Us...>>,
        internal_type_set::UniqueMode::kThroughout,
        sizeof...(Ts) + sizeof...(Us)>(
        std::make_index_sequence<sizeof...(Ts) + sizeof...(Us)>(), nullptr);
  }
}

// t >= u iff the types in t are a superset of the types in u. `<=` is the
// subset relation.
template <typename... Ts, typename... Us>
constexpr auto operator>=(TypeSet<Ts...> t, TypeSet<Us...> u) {
  return size((u - t).Tuple()) == 0;
}

template <typename... Ts, typename... Us>
constexpr auto operator<=(TypeSet<Ts...> t, TypeSet<Us...> u) {
  return u >= t;
}

// True if t and u contains the same types ignoring ordering.
template <typename... Ts, typename... Us>
constexpr auto operator==(TypeSet<Ts...> t, TypeSet<Us...> u) {
  if constexpr (sizeof...(Ts) == sizeof...(Us)) {
    return t >= u;
  } else {
    return false;
  }
}

template <typename... Ts, typename... Us>
constexpr auto operator!=(TypeSet<Ts...> t, TypeSet<Us...> u) {
  return !(t == u);
}

}  // namespace htls::meta

#endif  // HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_TYPE_SET_H_
