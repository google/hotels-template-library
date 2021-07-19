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

#include <array>
#include <string>
#include <type_traits>
#include <utility>

#include "third_party/hotels_template_library/haversack/internal/basic_tuple.h"
#include "third_party/hotels_template_library/haversack/internal/type.h"

namespace hotels::haversack::internal::types {
namespace internal_type_set {

struct TypeSetCtorSentinel {
  // Disable ALL aggregate init.
  constexpr explicit TypeSetCtorSentinel() {}
};

}  // namespace internal_type_set

// Instances of TypeSet are assumed to contain a unique set of types.
//
// This type should only be instantiated by MakeTypeSet or
// MakePrevalidiatedTypeSet.
template <typename... Ts>
struct TypeSet {
  constexpr explicit TypeSet(internal_type_set::TypeSetCtorSentinel,
                             BasicTuple<Type<Ts>...>) {}

  static constexpr BasicTuple<Type<Ts>...> Tuple() { return {}; }
};
template <typename... Ts>
TypeSet(internal_type_set::TypeSetCtorSentinel, BasicTuple<Type<Ts>...>)
    -> TypeSet<Ts...>;

namespace internal_type_set {

// Basic constexpr c-string hashing function.
template <typename T>
constexpr uint64_t FnvHash64() {
  // __PRETTY_FUNCTION__ is a c-string that contains a string representation of
  // T, so we use __PRETTY_FUNCTION__ as the basis of the hash.
  const char* str = __PRETTY_FUNCTION__;
  constexpr uint64_t kFnvPrime = 0x00000100000001B3;
  constexpr uint64_t kFnvOffsetBias = 0xcbf29ce484222325;
  uint64_t hash = kFnvOffsetBias;
  while (*str != 0) {
    hash = (hash ^ *(str++)) * kFnvPrime;
  }
  return hash;
}
template <typename T>
constexpr uint64_t kTypeHash = FnvHash64<T>();

// An object holding some metadata about a type to enable comparisons of types.
// The type order is derived from the hash of the type, therefore:
//   1. All equivalent types will have an equivalent hash and therefore be
//      sorted near each other.
//   2. Unequivalent types with hash collisions will be sorted near each other.
//
// The hash is not enough to guarantee equivalence of types so this TypeInfo
// alone cannot be used to establish type equivalence. TypeInfo can be used to
// establish type unequivalence, though.
struct TypeInfo {
  uint64_t hash = {};
  std::size_t index = {};

  // std::swap is not constexpr until C++20...
  constexpr void swap(TypeInfo& other) {
    auto self = *this;
    *this = other;
    other = self;
  }

  // Arbitrary strong-ordering that compares hashes first to allow constant time
  // comparison most of the time.
  constexpr bool operator<(const TypeInfo& info) const {
    return hash < info.hash;
  }

  constexpr bool operator==(const TypeInfo& info) const {
    return hash == info.hash;
  }

  constexpr bool operator!=(const TypeInfo& info) const {
    return !(*this == info);
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
    qsort(0, sizeof...(Ts) - 1, infos);
    return infos;
  }
};

// std::sort isn't constexpr until C++20...
template <std::size_t N>
constexpr void qsort(std::size_t start, std::size_t end,
                     std::array<TypeInfo, N>& infos) {
  if (N == 0) return;
  while (start < end) {
    // 3-median pivot
    std::size_t mid = ((end - start) / 2) + start;
    if (infos[mid] < infos[start]) infos[start].swap(infos[mid]);
    if (infos[end] < infos[start]) infos[start].swap(infos[end]);
    if (infos[mid] < infos[end]) infos[end].swap(infos[mid]);

    const auto& pivot = infos[end];
    std::size_t i = start;
    for (std::size_t j = start; j < end; ++j) {
      if (infos[j] < pivot) {
        infos[i++].swap(infos[j]);
      }
    }
    infos[i].swap(infos[end]);
    qsort(i + 1, end, infos);
    if (i <= start) break;
    end = i - 1;
  }
}

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
constexpr auto kSortedTypeInfos = TypeInfo::MakeSortedArray<Ts...>();
template <typename... Ts>
constexpr auto kReverseIndexes = MakeReverse(kSortedTypeInfos<Ts...>);

template <typename... Ts>
struct SortedTypeInfoCollection {
  // Returns true if Ts[index] is unique for all Ts with smaller indexes.
  template <std::ptrdiff_t index>
  constexpr bool IndexIsUniqueSoFar() const {
    return IndexIsUniqueImpl<index, -1, -1>();
  }

  // Returns true if Ts[index] is unique for all Ts with any index.
  template <std::ptrdiff_t index>
  constexpr bool IndexIsUniqueThroughout() const {
    return IndexIsUniqueImpl<index, -1, -1>() &&
           IndexIsUniqueImpl<index, 1, 1>();
  }

  // Returns if Ts[index] is not equal to Ts[index + compare_offset] and
  // potentially not checking other indexes in the direction of offset_delta if
  // there is a TypeInfo match but not an exact type match.
  template <std::ptrdiff_t index, std::ptrdiff_t compare_offset,
            std::ptrdiff_t offset_delta>
  constexpr bool IndexIsUniqueImpl() const {
    constexpr BasicTuple<Type<Ts>...> tuple;
    constexpr auto rev = kReverseIndexes<Ts...>[index];
    constexpr auto compare_index = rev + compare_offset;
    if constexpr (compare_index < 0 || compare_index >= size(tuple)) {
      // There is nothing left to check against so this index must be unique.
      return true;
    } else {
      if constexpr (kSortedTypeInfos<Ts...>[rev] ==
                    kSortedTypeInfos<Ts...>[compare_index]) {
        // This index has a matching info.
        if constexpr (get<index>(tuple) ==
                      get<kSortedTypeInfos<Ts...>[compare_index].index>(
                          tuple)) {
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

template <typename... Ts, std::size_t... indexes>
constexpr auto MakeTypeSetImpl(std::index_sequence<indexes...>,
                               Type<Ts>... ts) {
  constexpr SortedTypeInfoCollection<Ts...> type_infos;
  auto transform = [&](auto t, auto index_constant) {
    // Note: function arguments are not constexpr so we encode the index in the
    // type and extract it into a constexpr variable here.
    constexpr auto index = decltype(index_constant)::value;
    // Predicate: Include t if it is exactly the first time it appears in infos,
    // otherwise exclude it.
    if constexpr (type_infos.template IndexIsUniqueSoFar<index>()) {
      return MakeBasicTuple(t);
    } else {
      return MakeBasicTuple();
    }
  };
  // Filter Ts using the above predicate.
  return TypeSet(
      internal_type_set::TypeSetCtorSentinel(),
      (transform(type_c<Ts>, std::integral_constant<std::size_t, indexes>()) +
       ... + MakeBasicTuple()));
}

template <typename... Ts, typename... Us, std::size_t... indexes>
constexpr auto DifferenceImpl(std::index_sequence<indexes...>, TypeSet<Us...>,
                              Type<Ts>... ts) {
  constexpr SortedTypeInfoCollection<Ts..., Us...> type_infos;
  auto transform = [&](auto t, auto index_constant) {
    constexpr auto index = decltype(index_constant)::value;
    // Predicate: Include t if it only appears once in infos otherwise exclude
    // it.
    if constexpr (type_infos.template IndexIsUniqueThroughout<index>()) {
      return MakeBasicTuple(t);
    } else {
      return MakeBasicTuple();
    }
  };
  // Filter Ts using the above predicate.
  // Note: We include Us in the infos array but do not include them in this
  // filtering process. In this way we include each Ts if it is unique in both
  // Ts and Us.
  return TypeSet(
      internal_type_set::TypeSetCtorSentinel(),
      (transform(ts, std::integral_constant<std::size_t, indexes>()) + ... +
       MakeBasicTuple()));
}

template <typename... Ts, typename... Us, std::size_t... indexes>
constexpr auto IntersectionImpl(std::index_sequence<indexes...>, TypeSet<Us...>,
                                Type<Ts>... ts) {
  constexpr SortedTypeInfoCollection<Ts..., Us...> type_infos;
  auto transform = [&](auto t, auto index_constant) {
    constexpr auto index = decltype(index_constant)::value;
    // Predicate: Include t if t is NOT unique in infos, otherwise exclude it.
    if constexpr (type_infos.template IndexIsUniqueThroughout<index>()) {
      return MakeBasicTuple();
    } else {
      return MakeBasicTuple(t);
    }
  };
  // Filter Ts using the above predicate.
  // Note: We include Us in the infos array but do not include them in this
  // filtering process. Since Ts and Us are both TypeSets, any duplicates in
  // Ts+Us means that the duplicate is in both groups.
  return TypeSet(
      internal_type_set::TypeSetCtorSentinel(),
      (transform(ts, std::integral_constant<std::size_t, indexes>()) + ... +
       MakeBasicTuple()));
}

}  // namespace internal_type_set

// Create a TypeSet instance of ts. Only use this if ts has already had its
// duplicates removed.
template <typename... Ts>
constexpr auto MakePrevalidatedTypeSet(Ts... ts) {
  return TypeSet(internal_type_set::TypeSetCtorSentinel(),
                 MakeBasicTuple(ts...));
}

// True if t is in p.
template <typename T, typename... Us>
constexpr auto Contains(Type<T> t, TypeSet<Us...> tuple) {
  return (... || (t == Type<Us>()));
}

// Append t to sum if t is not in sum.
//
// sum must be a TypeSet and t must be a Type.
template <typename... Ts, typename T>
constexpr auto AppendIfUnique(TypeSet<Ts...> sum, Type<T> t) {
  if constexpr (Contains(t, sum)) {
    return sum;
  } else {
    return Apply([](auto... ts) { return MakePrevalidatedTypeSet(ts...); },
                 std::move(sum.Tuple()) + MakeBasicTuple(t));
  }
}

// Create a TypeSet instance by removing all duplicates from ts.
template <typename... Ts>
constexpr auto MakeTypeSet(Type<Ts>... ts) {
  return internal_type_set::MakeTypeSetImpl(
      std::make_index_sequence<sizeof...(Ts)>(), ts...);
}

// True if the type of each ts is unique.
template <typename... Ts>
constexpr auto AllUnique(Type<Ts>... ts) {
  return sizeof...(ts) == size(MakeTypeSet(ts...).Tuple());
}

// Returns the TypeSet of types that are in t AND NOT in u.
template <typename... Ts, typename... Us>
constexpr auto operator-(TypeSet<Ts...> t, TypeSet<Us...> u) {
  return internal_type_set::DifferenceImpl(
      std::make_index_sequence<sizeof...(Ts)>(), u, types::type_c<Ts>...);
}

// Returns the TypeSet of types that are in t OR u.
template <typename... Ts, typename... Us>
constexpr auto operator|(TypeSet<Ts...> t, TypeSet<Us...> u) {
  return MakeTypeSet(types::type_c<Ts>..., types::type_c<Us>...);
}

// Returns the TypeSet of types that are in t AND u.
template <typename... Ts, typename... Us>
constexpr auto operator&(TypeSet<Ts...> t, TypeSet<Us...> u) {
  return internal_type_set::IntersectionImpl(
      std::make_index_sequence<sizeof...(Ts)>(), u, types::type_c<Ts>...);
}

// Returns a TypeSet containing the types that are in t XOR u.
template <typename... Ts, typename... Us>
constexpr auto operator^(TypeSet<Ts...> t, TypeSet<Us...> u) {
  // DifferenceImpl only includes types that are in the trailing argument pack
  // and are unique across the (empty here) typeset and the argument pack. In
  // this case, the types that are unique in Ts+Us is Ts XOR Us.
  return internal_type_set::DifferenceImpl(
      std::make_index_sequence<sizeof...(Ts) + sizeof...(Us)>(), MakeTypeSet(),
      types::type_c<Ts>..., types::type_c<Us>...);
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
  return sizeof...(Ts) == sizeof...(Us) && t >= u;
}

template <typename... Ts, typename... Us>
constexpr auto operator!=(TypeSet<Ts...> t, TypeSet<Us...> u) {
  return !(t == u);
}

}  // namespace hotels::haversack::internal::types

#endif  // HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_TYPE_SET_H_
