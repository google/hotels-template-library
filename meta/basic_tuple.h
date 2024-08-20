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

#ifndef HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_BASIC_TUPLE_H_
#define HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_BASIC_TUPLE_H_

#include <array>
#include <cstddef>
#include <type_traits>
#include <utility>

namespace htls::meta {

template <typename...>
struct BasicTuple;
template <typename... Ts>
constexpr auto MakeBasicTuple(Ts&&...);

namespace internal_basic_tuple {

template <std::size_t Index, typename T>
struct IndexedType {};

template <typename...>
struct BasicTupleImpl;
template <std::size_t... Indexes, typename... Ts>
struct BasicTupleImpl<std::index_sequence<Indexes...>, Ts...>
    : IndexedType<Indexes, Ts>... {
  using Base = BasicTupleImpl;

  constexpr BasicTupleImpl() = default;
  template <typename... Args>
  explicit constexpr BasicTupleImpl(Args&&... args) {}
};

// Both template parameter orderings are specified to allow specifying only one.
template <typename T, std::size_t Index>
constexpr T get(internal_basic_tuple::IndexedType<Index, T> h) {
  return {};
}
template <std::size_t Index, typename T>
constexpr T get(internal_basic_tuple::IndexedType<Index, T> h) {
  return {};
}

template <typename>
struct IsBasicTupleImpl : std::false_type {};
template <typename...Ts>
struct IsBasicTupleImpl<BasicTuple<Ts...>> : std::true_type {};

// True if T is a BasicTuple.
template <typename T>
constexpr bool IsBasicTuple =
    IsBasicTupleImpl<std::decay_t<T>>::value;

template <std::size_t... a_indexes, std::size_t... b_indexes, typename A,
          typename B>
constexpr auto AddImpl(std::index_sequence<a_indexes...>,
                       std::index_sequence<b_indexes...>, A&& a, B&& b) {
  // A and B are always BasicTuples so it is safe to forward a and b multiple
  // times since we are accessing unique elements with `get`.
  return MakeBasicTuple(get<a_indexes>(std::forward<A>(a))...,
                        get<b_indexes>(std::forward<B>(b))...);
}

template <typename F, std::size_t... indexes, typename T>
constexpr auto ApplyImpl(const F& f, T&& t, std::index_sequence<indexes...>) {
  // T is always a BasicTuple so it is safe to forward t multiple times since
  // we are accessing unique elements with `get`.
  return f(get<indexes>(std::forward<T>(t))...);
}

template <typename F, typename T>
struct AccumulateHelper {
  const F& f;
  T t;

  constexpr AccumulateHelper(const F& f, T t) : f(f), t(std::move(t)) {}

  template <typename U>
  constexpr auto operator<<(U&& u) && {
    return internal_basic_tuple::AccumulateHelper(
        this->f, this->f(std::move(t), std::forward<U>(u)));
  }
};
template <typename F, typename T>
AccumulateHelper(F, T) -> AccumulateHelper<F, T>;

template <typename F, std::size_t... indexes, typename T, typename Init>
constexpr auto AccumulateImpl(const F& f, T&& t, Init&& init,
                              std::index_sequence<indexes...>) {
  // T is always a BasicTuple so it is safe to forward t multiple times since
  // we are accessing unique elements with `get`.
  return (internal_basic_tuple::AccumulateHelper(f, std::forward<Init>(init))
          << ... << get<indexes>(std::forward<T>(t)))
      .t;
}

template <typename F, std::size_t... indexes, typename T>
constexpr auto TransformImpl(const F& f, T&& t,
                             std::index_sequence<indexes...>) {
  // T is always a BasicTuple so it is safe to forward t multiple times since
  // we are accessing unique elements with `get`.
  return MakeBasicTuple(f(get<indexes>(std::forward<T>(t)))...);
}

template <typename F, std::size_t... indexes, typename T>
constexpr bool AllOfImpl(const F& f, T&& t, std::index_sequence<indexes...>) {
  // T is always a BasicTuple so it is safe to forward t multiple times since
  // we are accessing unique elements with `get`.
  return (... && f(get<indexes>(std::forward<T>(t))));
}

template <typename F, std::size_t... indexes, typename T>
constexpr bool AnyOfImpl(const F& f, T&& t, std::index_sequence<indexes...>) {
  // T is always a BasicTuple so it is safe to forward t multiple times since
  // we are accessing unique elements with `get`.
  return (... || f(get<indexes>(std::forward<T>(t))));
}

}  // namespace internal_basic_tuple

// A minimally featured tuple implementation. This is used for complex
// metaprogramming because std::tuple has worse than O(n) performance.
//
// Moving a BasicTuple into a call to `get` will only move the element that
// `get` is accessing. A single BasicTuple can be safely moved multiple times to
// move different elements out.
template <typename... Ts>
struct BasicTuple final : internal_basic_tuple::BasicTupleImpl<
                              std::make_index_sequence<sizeof...(Ts)>, Ts...> {
  using BasicTuple::Base::Base;

  static constexpr auto Indexes() {
    return std::make_index_sequence<sizeof...(Ts)>();
  }
};
template <typename... Ts>
BasicTuple(const BasicTuple<Ts...>&) -> BasicTuple<Ts...>;
template <typename... Ts>
BasicTuple(BasicTuple<Ts...>&&) -> BasicTuple<Ts...>;

// Makes a BasicTuple holding any number of values.
//
// CTAD doesn't work very well for type-generic containers such as tuples
// because of the vagueness of selecting a copy CTOR or a user defined CTOR.
//
//   BasicTuple<int> bi;
//   BasicTuple other = bi;
//   auto other2 = BasicTuple(bi);
template <typename... Ts>
constexpr auto MakeBasicTuple(Ts&&... ts) {
  return BasicTuple<std::decay_t<Ts>...>(std::forward<Ts>(ts)...);
}

// Get a value out of a BasicTuple, either by index or by (unique) type.
//
//   BasicTuple<int> bi;
//   get<0>(bi) == get<int>(bi);
using internal_basic_tuple::get;

// Get the size of a BasicTuple instance or type.
template <typename... Ts>
constexpr std::size_t size(const BasicTuple<Ts...>&) {
  return sizeof...(Ts);
}

// Two BasicTuples are equal iff both tuples are the same size and all "zipped"
// values are equal.
//
// If a pair of "zipped" values are not comparable or the tuples are off
// different sizes, it is a compilation error. This behavior matches equality
// for std::tuple.
template <std::size_t... indexes, typename... As, typename... Bs>
constexpr bool operator==(const internal_basic_tuple::BasicTupleImpl<
                              std::index_sequence<indexes...>, As...>& a,
                          const BasicTuple<Bs...>& b) {
  constexpr bool same_size = sizeof...(As) == sizeof...(Bs);
  static_assert(same_size,
                "Can only compare BasicTuples that are the same size.");
  if constexpr (same_size) {
    return (... && (get<indexes>(a) == get<indexes>(b)));
  } else {
    return false;
  }
}

template <std::size_t... indexes, typename... As, typename... Bs>
constexpr bool operator!=(const internal_basic_tuple::BasicTupleImpl<
                              std::index_sequence<indexes...>, As...>& a,
                          const BasicTuple<Bs...>& b) {
  return !(a == b);
}

// Concatenates two BasicTuples.
template <typename A, typename B,
          typename = std::enable_if_t<internal_basic_tuple::IsBasicTuple<A> &&
                                      internal_basic_tuple::IsBasicTuple<B>>>
constexpr auto operator+(A&& a, B&& b) {
  return internal_basic_tuple::AddImpl(a.Indexes(), b.Indexes(),
                                       std::forward<A>(a), std::forward<B>(b));
}

// Concatenates N BasicTuples into one BasicTuple.
template <typename... Ts>
constexpr auto Concat(Ts&&... ts) {
  if constexpr (sizeof...(ts) <= 1) {
    return (BasicTuple<>(), ..., ts);
  } else {
    BasicTuple<Ts...> bt;
    // Extra size element to make iteration easier if the last tuple is empty.
    constexpr std::array<std::size_t, sizeof...(ts) + 1> sizes = {
        size(std::remove_cvref_t<Ts>())..., 1};
    constexpr std::array<std::size_t, sizeof...(ts) + 1> cumulative = [&] {
      std::array<std::size_t, sizeof...(ts) + 1> cum;
      cum[0] = 0;
      for (std::size_t i = 1; i < cum.size(); ++i) {
        cum[i] = cum[i - 1] + sizes[i - 1];
      }
      return cum;
    }();
    constexpr std::size_t total =
        cumulative[sizeof...(ts) - 1] + sizes[sizeof...(ts) - 1];
    constexpr auto ts_indexes = [&] {
      std::array<std::size_t, total> output;
      std::size_t threshold = size(get<0>(bt));
      std::size_t ts_index = 0;
      for (std::size_t index = 0; index < total; ++index) {
        while (index == threshold) {
          ++ts_index;
          threshold += sizes[ts_index];
        }
        output[index] = ts_index;
      }
      return output;
    }();
    return [&]<std::size_t... indexes>(std::index_sequence<indexes...>) {
      return MakeBasicTuple(get<indexes - cumulative[ts_indexes[indexes]]>(
          get<ts_indexes[indexes]>(bt))...);
    }(std::make_index_sequence<total>());
  }
}

// Applies a BasicTuple as arguments to a invocable.
template <typename F, std::size_t... indexes, typename T,
          typename = std::enable_if_t<internal_basic_tuple::IsBasicTuple<T>>>
constexpr auto Apply(const F& f, T&& t) {
  return internal_basic_tuple::ApplyImpl(f, std::forward<T>(t), t.Indexes());
}

// Flattens multiple tuples of tuples into a single tuple.
//
//   Flatten(MakeBasicTuple(MakeBasicTuple(a, b), MakeBasicTuple(c, d)))
//     == MakeBasicTuple(a, b, c, d)
template <typename... Ts>
constexpr auto Flatten(Ts&&... ts) {
  return Apply(
      [](auto&&... ts) { return Concat(std::forward<decltype(ts)>(ts)...); },
      Concat(std::forward<Ts>(ts)...));
}

// Applies a predicate invocable to each member of the BasicTuple and remove any
// members which do not pass the predicate.
template <typename Pred, typename T,
          typename = std::enable_if_t<internal_basic_tuple::IsBasicTuple<T>>>
constexpr auto Filter(Pred pred, T t) {
  constexpr std::array<bool, size(T())> passes_pred = Apply(
      [&](auto... ts) { return std::array<bool, size(T())>{pred(ts)...}; }, t);
  constexpr std::size_t count = [&passes_pred] {
    std::size_t sum = 0;
    for (bool b : passes_pred) {
      sum += b ? 1 : 0;
    }
    return sum;
  }();
  constexpr std::array<std::size_t, count> passing_indexes = [&] {
    std::array<std::size_t, count> passing_indexes{};
    std::size_t c = 0;
    for (std::size_t index = 0; index < size(t); ++index) {
      if (passes_pred[index]) {
        passing_indexes[c] = index;
        ++c;
      }
    }
    return passing_indexes;
  }();
  return [&]<std::size_t... indexes>(std::index_sequence<indexes...>) {
    return MakeBasicTuple(get<passing_indexes[indexes]>(t)...);
  }(std::make_index_sequence<count>());
}

// Performs an accumulate operation on a BasicTuple by passing the "sum" and
// each value from the tuple to the "accumulator" invocable.
template <typename F, typename T, typename Init,
          typename = std::enable_if_t<internal_basic_tuple::IsBasicTuple<T>>>
constexpr auto Accumulate(const F& f, T&& t, Init&& init) {
  return internal_basic_tuple::AccumulateImpl(
      f, std::forward<T>(t), std::forward<Init>(init), t.Indexes());
}

// Transforms each member of the tuple using the invocable.
template <typename F, typename T,
          typename = std::enable_if_t<internal_basic_tuple::IsBasicTuple<T>>>
constexpr auto Transform(const F& f, T&& t) {
  return internal_basic_tuple::TransformImpl(f, std::forward<T>(t),
                                             t.Indexes());
}

// Applies a predicate to a tuple, returning true if all members pass the
// predicate with short-circuiting.
template <typename F, typename T,
          typename = std::enable_if_t<internal_basic_tuple::IsBasicTuple<T>>>
constexpr bool AllOf(const F& f, T&& t) {
  return internal_basic_tuple::AllOfImpl(f, std::forward<T>(t), t.Indexes());
}

// Applies a predicate to a tuple, returning true if any members pass the
// predicate with short-circuiting.
template <typename F, typename T,
          typename = std::enable_if_t<internal_basic_tuple::IsBasicTuple<T>>>
constexpr bool AnyOf(const F& f, T&& t) {
  return internal_basic_tuple::AnyOfImpl(f, std::forward<T>(t), t.Indexes());
}

}  // namespace htls::meta

#endif  // HOTELS_TEMPLATE_LIBRARY_HAVERSACK_INTERNAL_BASIC_TUPLE_H_
