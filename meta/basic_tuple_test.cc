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

#include "meta/basic_tuple.h"

#include "meta/type.h"

namespace htls::meta {
namespace {

struct A {};
struct B {};
struct C {};
struct D {};
constexpr Type<A> a{};
constexpr Type<B> b{};
constexpr Type<C> c{};
constexpr Type<D> d{};

static_assert(Concat() == MakeBasicTuple());
static_assert(Concat(MakeBasicTuple(a, b)) == MakeBasicTuple(a, b));
static_assert(Concat(MakeBasicTuple(a, b), MakeBasicTuple(c),
                     MakeBasicTuple(d, a)) == MakeBasicTuple(a, b, c, d, a));
static_assert(Flatten(MakeBasicTuple(MakeBasicTuple(a, b),
                                     MakeBasicTuple(c, d))) ==
              MakeBasicTuple(a, b, c, d));
static_assert(MakeBasicTuple(MakeBasicTuple(a)) ==
              BasicTuple<BasicTuple<Type<A>>>());

template <typename... Ts>
struct Matches {
  template <typename T>
  constexpr bool operator()(Type<T> t) const {
    return (... || std::is_same_v<T, Ts>);
  }
};
constexpr Matches<A, C> kMatchesAorC;
static_assert(Filter(kMatchesAorC, MakeBasicTuple(a, b, c, d)) ==
              MakeBasicTuple(a, c));
static_assert(Filter(kMatchesAorC, MakeBasicTuple(a)) == MakeBasicTuple(a));

constexpr auto ReverseAccumulator = [](auto tuple, auto t) {
  return MakeBasicTuple(t) + std::move(tuple);
};
static_assert(Accumulate(ReverseAccumulator, MakeBasicTuple(a, b, c),
                         MakeBasicTuple()) == MakeBasicTuple(c, b, a));
static_assert(Accumulate(ReverseAccumulator, MakeBasicTuple(),
                         MakeBasicTuple()) == MakeBasicTuple());

static_assert(Transform([](auto t) { return MakeBasicTuple(t); },
                        MakeBasicTuple(a, b, c)) ==
              MakeBasicTuple(MakeBasicTuple(a), MakeBasicTuple(b),
                             MakeBasicTuple(c)));

}  // namespace
}  // namespace htls::meta
