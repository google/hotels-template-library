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

#include "third_party/hotels_template_library/haversack/internal/type_set.h"

#include "testing/base/public/gmock.h"
#include "testing/base/public/gunit.h"

namespace hotels::haversack::internal::types {
namespace {

struct A {};
struct B {};
struct C {};
struct D {};
constexpr Type<A> a{};
constexpr Type<B> b{};
constexpr Type<C> c{};
constexpr Type<D> d{};

template <auto Value>
struct ValueHolder {};

static_assert(AllUnique(a, b, c), "Expected all unique types");
static_assert(!AllUnique(c, b, c), "Expected not all unique types");

static_assert(MakeTypeSet(a, b, c).Tuple() == MakeBasicTuple(a, b, c),
              "Expected all unique types to remain after MakeTypeSet");
static_assert(MakeTypeSet(c, b, c) == MakePrevalidatedTypeSet(c, b),
              "Expected MakeTypeSet to remove duplicate types");
static_assert(MakePrevalidatedTypeSet(b, c).Tuple() == MakeBasicTuple(b, c),
              "Expected MakePrevalidatedTypeSet to do nothing");

static_assert(Contains(b, MakeTypeSet(a, b, c)),
              "Expected Contains to find a match");
static_assert(!Contains(d, MakeTypeSet(a, b, c)),
              "Expected Contains to not find a match");

static_assert((MakeTypeSet(a, b, c) - MakeTypeSet(a, c)).Tuple() ==
                  MakeBasicTuple(b),
              "Expected substracting a subset to leave the difference");
static_assert((MakeTypeSet(a, c) - MakeTypeSet(a, c, b)).Tuple() ==
                  MakeBasicTuple(),
              "Expected subtracting a superset to leave nothing");
static_assert(
    size(MakeTypeSet(type_c<ValueHolder<1>>, type_c<ValueHolder<1u>>)
             .Tuple()) == 2,
    "ValueHolders with values of different types should be different.");

static_assert((MakeTypeSet(a, b, c) | MakeTypeSet(a, d)) ==
                  MakePrevalidatedTypeSet(a, b, c, d),
              "Expected operator| to be union");

static_assert((MakeTypeSet(a, b, c) & MakeTypeSet(a, c, d)).Tuple() ==
                  MakeBasicTuple(a, c),
              "Expected operator& to be intersection");

static_assert((MakeTypeSet(a, b, c) ^ MakeTypeSet(a, d)).Tuple() ==
                  MakeBasicTuple(b, c, d),
              "Expected operator^ to be XOR (symmetric difference)");

static_assert(MakeTypeSet(a, b, c) >= MakeTypeSet(a, c),
              "Expected >= to be true for a superset");
static_assert(!(MakeTypeSet(a, c) >= MakeTypeSet(a, b, c)),
              "Expected >= to be false for a subset");

static_assert(!(MakeTypeSet(a, b, c) == MakeTypeSet(a, c)),
              "Expected == to be false for different sets");
static_assert(MakeTypeSet(a, b, c) != MakeTypeSet(a, c),
              "Expected != to be true for different sets");
static_assert(MakeTypeSet(a, b, c) == MakeTypeSet(a, b, c),
              "Expected == to be true for same sets");
static_assert(!(MakeTypeSet(a, b, c) != MakeTypeSet(a, b, c)),
              "Expected != to be false for same sets");
static_assert(MakeTypeSet(a, b, c) == MakeTypeSet(c, b, a),
              "Expected == to be true regardless of order for same sets");

static_assert(!(MakeTypeSet(a) >= MakeTypeSet(b)) &&
                  !(MakeTypeSet(b) >= MakeTypeSet(a)),
              "Expected >= to be false for disjoint sets");

}  // namespace
}  // namespace hotels::haversack::internal::types
