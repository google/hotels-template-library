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

#include "meta/type.h"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

namespace htls::meta {
namespace {

struct A {};
struct B {};
struct C {};
constexpr Type<A> a{};
constexpr Type<B> b{};
constexpr Type<C> c{};
template <typename...>
struct Types;

static_assert(type_c<A> == Type<A>());
static_assert(type_c<int> == Type(1));

static_assert(AsTuple(Type<Types<A, B, C>>()) ==
              htls::meta::MakeBasicTuple(a, b, c));

static_assert(FromTuple<Types>(htls::meta::MakeBasicTuple(a, b, c)) ==
              Type<Types<A, B, C>>());

struct Foo {
  int i;
};
static_assert(IsValidExpr(
    Type<Foo>(),
    [](auto t) -> decltype(std::declval<typename decltype(t)::type>().i) {}));
static_assert(!IsValidExpr(
    Type<Foo>(),
    [](auto t) -> decltype(std::declval<typename decltype(t)::type>().missing) {
    }));

static_assert(ValidExprOr(
                  Foo{.i = 5},
                  [](const auto& t) -> decltype(t.i) { return t.i; }, 0) == 5);
static_assert(ValidExprOr(
                  Foo{.i = 5},
                  [](const auto& t) -> decltype(t.missing) {
                    return t.missing;
                  },
                  0) == 0);

static_assert(MetaValueFunction<std::is_same>()(a, a));
static_assert(MetaTypeFunction<std::remove_pointer>()(type_c<A*>) == a);

static_assert(size(type_c<htls::meta::BasicTuple<A, B, C>>) == 3);

TEST(MetaValueFunctionTest, IsConst) {
  const auto& func = MetaValueFunction<std::is_same, A>();
  EXPECT_TRUE(func(type_c<A>));
  EXPECT_FALSE(func(type_c<B>));
}

TEST(MetaTypeFunctionTest, IsConst) {
  const auto& func = MetaTypeFunction<std::remove_const>();
  EXPECT_EQ(func(type_c<const A>), a);
  EXPECT_EQ(func(a), a);
}

}  // namespace
}  // namespace htls::meta
