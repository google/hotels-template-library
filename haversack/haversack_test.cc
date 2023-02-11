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

#include "haversack/haversack.h"

#include <memory>
#include <type_traits>

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include "haversack/haversack_test_external_helper.h"
#include "haversack/haversack_test_util.h"

namespace hotels::haversack {
namespace {

// Types A-O are placeholders for real-world types. They each hold an int so
// that we can inject arbitrary values in the test and verify that they are
// propagated correctly.
struct A {
  explicit A(int i = 0) : i(i) {}
  int i = 0;

  virtual ~A() = default;
  virtual const char* ClassName() const { return "A"; }
};
struct B {
  explicit B(int i = 0) : i(i) {}
  int i = 0;
};
struct C {
  explicit C(int i = 0) : i(i) {}
  int i = 0;
};
struct D {
  explicit D(int i = 0) : i(i) {}
  int i = 0;
};
struct E {
  explicit E(int i = 0) : i(i) {}
  int i = 0;
};
struct F {
  explicit F(int i = 0) : i(i) {}
  int i = 0;
};
struct G {
  explicit G(int i = 0) : i(i) {}
  int i = 0;
};

struct L {
  explicit L(int i = 0) : i(i) {}
  int i = 0;
};
struct M {
  explicit M(int i = 0) : i(i) {}
  int i = 0;
};
struct N {
  explicit N(int i = 0) : i(i) {}
  int i = 0;
};
struct O {
  explicit O(int i = 0) : i(i) {}
  int i = 0;
};

// There are Haversack types from AHSack -> GHSack and HandlerHSack with the
// dependency tree which matches the below drawing.
//
//       Handler
//      /   |   \
//     /    |    \
//    A     B     C
//   / \   / \    |
//  /   \ /   \   |
// D     E     F  |
//              \ |
//               \|
//                G

struct GHSack : Haversack<G, L> {
  using HaversackT::HaversackT;
};
struct DHSack : Haversack<D, M> {
  using HaversackT::HaversackT;
};
struct EHSack : Haversack<E, N> {
  using HaversackT::HaversackT;
};
struct FHSack : Haversack<Deps<GHSack>, Provides<G>, F, O> {
  using HaversackT::HaversackT;
};
struct AHSack : Haversack<Deps<DHSack, EHSack>, A, L> {
  using HaversackT::HaversackT;
};
struct BHSack : Haversack<Deps<EHSack, FHSack>, B, M> {
  using HaversackT::HaversackT;
};
struct CHSack : Haversack<Deps<GHSack>, Provides<G>, C, N> {
  using HaversackT::HaversackT;
};
struct HandlerHSack : Haversack<Deps<AHSack, BHSack, CHSack>> {
  using HaversackT::HaversackT;
};

struct Nonmoveable {
  Nonmoveable(int i, int j) : i(i), j(j) {}

  Nonmoveable(const Nonmoveable&) = delete;
  Nonmoveable(Nonmoveable&&) = delete;
  Nonmoveable& operator=(const Nonmoveable&) = delete;
  Nonmoveable& operator=(Nonmoveable&&) = delete;
  virtual ~Nonmoveable() = default;

  virtual int Getter() const { return i; }

  int i = 0;
  int j = 0;
};

struct NonmoveableChild : Nonmoveable {
  NonmoveableChild(int i_, int j_, int k) : Nonmoveable(i_, j_), k(k) {}
  int Getter() const override { return k; }
  int k = 0;
};

int MyFunc(A, B, Haversack<C, D>) { return 5; }
int MyFuncTakingRef(A, B, const Haversack<E, F>&) { return 10; }

struct NonCopyableFunctor {
  NonCopyableFunctor() = default;
  NonCopyableFunctor(const NonCopyableFunctor&) = delete;
  NonCopyableFunctor& operator=(const NonCopyableFunctor&) = delete;
  NonCopyableFunctor(NonCopyableFunctor&&) = delete;
  NonCopyableFunctor& operator=(NonCopyableFunctor&&) = delete;
  int operator()(Haversack<A, B>) const { return 10; }
};
constexpr NonCopyableFunctor non_copyable_functor;

struct VirtualParent {
  virtual int DoStuff() const { return 0; }
  virtual ~VirtualParent() = default;
};
struct VirtualChild : VirtualParent {
  explicit VirtualChild(int i) : i(i) {}
  int DoStuff() const override { return i; }

  int i = 1;
};

struct SubA : A {
  using A::A;
  const char* ClassName() const override { return "SubA"; }
};

struct TagA {};

MATCHER_P(CanGet, type, "") {
  *result_listener << "Cannot get a "
                   << htls::meta::DebugTypeName(
                          htls::meta::type_c<std::decay_t<decltype(type)>>)
                   << " from a "
                   << htls::meta::DebugTypeName(
                          htls::meta::type_c<std::decay_t<decltype(arg)>>);
  return htls::meta::IsValidExpr(
      arg,
      [=](const auto& sack)
          -> decltype(sack.template Get<std::decay_t<decltype(type)>>()) {});
}

class HaversackTest : public testing::Test {
 protected:
  // G is provided by CHSack and FHSack so HandlerHSack does not need to inject
  // it.
  HandlerHSack handler_cxt_ = HandlerHSack(
      std::make_shared<A>(1), std::make_shared<B>(2), std::make_shared<C>(3),
      std::make_shared<D>(4), std::make_shared<E>(5), std::make_shared<F>(6),
      // No G value
      std::make_shared<L>(7), std::make_shared<M>(8), std::make_shared<N>(9),
      std::make_shared<O>(10));
};

using CtorAndAccessTest = HaversackTest;

TEST_F(HaversackTest, HandlerCxtCannotGetAnything) {
  EXPECT_THAT(
      handler_cxt_,
      testing::AllOf(testing::Not(CanGet(A())), testing::Not(CanGet(B())),
                     testing::Not(CanGet(C())), testing::Not(CanGet(D())),
                     testing::Not(CanGet(E())), testing::Not(CanGet(F())),
                     testing::Not(CanGet(G())), testing::Not(CanGet(L())),
                     testing::Not(CanGet(M())), testing::Not(CanGet(N())),
                     testing::Not(CanGet(O()))));
}

TEST_F(CtorAndAccessTest, AHSackGetters) {
  AHSack cxt = handler_cxt_;
  EXPECT_THAT(
      cxt, testing::AllOf(CanGet(A()), CanGet(L()),  //
                          testing::Not(CanGet(B())), testing::Not(CanGet(C())),
                          testing::Not(CanGet(D())), testing::Not(CanGet(E())),
                          testing::Not(CanGet(F())), testing::Not(CanGet(G())),
                          testing::Not(CanGet(M())), testing::Not(CanGet(N())),
                          testing::Not(CanGet(O()))));
}

TEST_F(CtorAndAccessTest, FHSackGetters) {
  FHSack cxt = handler_cxt_;
  EXPECT_THAT(
      cxt, testing::AllOf(CanGet(F()), CanGet(O()),  //
                          testing::Not(CanGet(A())), testing::Not(CanGet(B())),
                          testing::Not(CanGet(C())), testing::Not(CanGet(D())),
                          testing::Not(CanGet(E())), testing::Not(CanGet(G())),
                          testing::Not(CanGet(L())), testing::Not(CanGet(M())),
                          testing::Not(CanGet(N()))));
}

TEST_F(CtorAndAccessTest, AHSackFromHandler) {
  AHSack cxt = handler_cxt_;
  EXPECT_THAT(cxt.Get<A>().i, 1);
  EXPECT_THAT(cxt.Get<L>().i, 7);
}

TEST_F(CtorAndAccessTest, BHSackFromHandler) {
  BHSack cxt = handler_cxt_;
  EXPECT_THAT(cxt.Get<B>().i, 2);
  EXPECT_THAT(cxt.Get<M>().i, 8);
}

TEST_F(CtorAndAccessTest, CHSackFromHandler) {
  CHSack cxt = handler_cxt_;
  EXPECT_THAT(cxt.Get<C>().i, 3);
  EXPECT_THAT(cxt.Get<N>().i, 9);
}

TEST_F(CtorAndAccessTest, DHSackFromHandler) {
  DHSack cxt = handler_cxt_;
  EXPECT_THAT(cxt.Get<D>().i, 4);
  EXPECT_THAT(cxt.Get<M>().i, 8);
}

TEST_F(CtorAndAccessTest, DHSackFromAHSack) {
  AHSack a_cxt = handler_cxt_;
  DHSack cxt = a_cxt;
  EXPECT_THAT(cxt.Get<D>().i, 4);
  EXPECT_THAT(cxt.Get<M>().i, 8);
}

TEST_F(CtorAndAccessTest, EHSackFromHandler) {
  EHSack cxt = handler_cxt_;
  EXPECT_THAT(cxt.Get<E>().i, 5);
  EXPECT_THAT(cxt.Get<N>().i, 9);
}

TEST_F(CtorAndAccessTest, EHSackFromAHSack) {
  AHSack a_cxt = handler_cxt_;
  EHSack cxt = a_cxt;
  EXPECT_THAT(cxt.Get<E>().i, 5);
  EXPECT_THAT(cxt.Get<N>().i, 9);
}

TEST_F(CtorAndAccessTest, EHSackFromBHSack) {
  AHSack b_cxt = handler_cxt_;
  EHSack cxt = b_cxt;
  EXPECT_THAT(cxt.Get<E>().i, 5);
  EXPECT_THAT(cxt.Get<N>().i, 9);
}

TEST_F(CtorAndAccessTest, FHSackFromHandler) {
  FHSack cxt = handler_cxt_;
  EXPECT_THAT(cxt.Get<F>().i, 6);
  EXPECT_THAT(cxt.Get<O>().i, 10);
}

TEST_F(CtorAndAccessTest, FHSackFromBHSack) {
  BHSack b_cxt = handler_cxt_;
  FHSack cxt = b_cxt;
  EXPECT_THAT(cxt.Get<F>().i, 6);
  EXPECT_THAT(cxt.Get<O>().i, 10);
}

TEST_F(CtorAndAccessTest, GHSackFromHander) {
  GHSack cxt(handler_cxt_, std::make_unique<G>(100));
  EXPECT_THAT(cxt.Get<G>().i, 100);
  EXPECT_THAT(cxt.Get<L>().i, 7);
}

TEST_F(CtorAndAccessTest, GHSackFromCHSack) {
  CHSack c_cxt = handler_cxt_;
  GHSack cxt(c_cxt, std::make_unique<G>(100));
  EXPECT_THAT(cxt.Get<G>().i, 100);
  EXPECT_THAT(cxt.Get<L>().i, 7);
}

TEST_F(CtorAndAccessTest, GHSackFromFHSack) {
  FHSack f_cxt = handler_cxt_;
  GHSack cxt(f_cxt, std::make_unique<G>(100));
  EXPECT_THAT(cxt.Get<G>().i, 100);
  EXPECT_THAT(cxt.Get<L>().i, 7);
}

TEST_F(HaversackTest, Insert) {
  Haversack<Nonmoveable> cxt =
      Haversack<>().Insert(std::make_shared<Nonmoveable>(100, 200));
  EXPECT_THAT(cxt.Get<Nonmoveable>().i, 100);
  EXPECT_THAT(cxt.Get<Nonmoveable>().j, 200);
  EXPECT_THAT(cxt.Get<Nonmoveable>().Getter(), 100);
}

TEST_F(HaversackTest, InsertUnique) {
  Haversack<Nonmoveable> cxt =
      Haversack<>().Insert(std::make_unique<Nonmoveable>(100, 200));
  EXPECT_THAT(cxt.Get<Nonmoveable>().i, 100);
  EXPECT_THAT(cxt.Get<Nonmoveable>().j, 200);
  EXPECT_THAT(cxt.Get<Nonmoveable>().Getter(), 100);
}

TEST_F(HaversackTest, InsertRaw) {
  Nonmoveable value(100, 200);
  Haversack<Nonmoveable> cxt = Haversack<>().Insert(&value);
  EXPECT_THAT(cxt.Get<Nonmoveable>().i, 100);
  EXPECT_THAT(cxt.Get<Nonmoveable>().j, 200);
  EXPECT_THAT(cxt.Get<Nonmoveable>().Getter(), 100);
}

TEST_F(HaversackTest, Replace) {
  A a{1}, aa{2};
  Haversack<A> cxt(&a);
  EXPECT_THAT(cxt.Get<A>().i, 1);
  cxt = cxt.Replace(&aa);
  EXPECT_THAT(cxt.Get<A>().i, 2);
}

TEST_F(HaversackTest, SubclassInsert) {
  Haversack<Nonmoveable> cxt = Haversack<>().Insert<Nonmoveable>(
      std::make_shared<NonmoveableChild>(100, 200, 300));
  EXPECT_THAT(cxt.Get<Nonmoveable>().i, 100);
  EXPECT_THAT(cxt.Get<Nonmoveable>().j, 200);
  EXPECT_THAT(cxt.Get<Nonmoveable>().Getter(), 300);
}

TEST_F(HaversackTest, InsertKnownThreadSafe) {
  Nonmoveable value(100, 200);
  Haversack<KnownThreadSafe<Nonmoveable>> cxt =
      Haversack<>().Insert<KnownThreadSafe<Nonmoveable>>(&value);
  EXPECT_THAT(cxt.Get<KnownThreadSafe<Nonmoveable>>().i, 100);
  EXPECT_THAT(cxt.Get<KnownThreadSafe<Nonmoveable>>().j, 200);
  EXPECT_THAT(cxt.Get<KnownThreadSafe<Nonmoveable>>().Getter(), 100);
  cxt.Get<KnownThreadSafe<Nonmoveable>>().i = 1000;
  EXPECT_THAT(value.i, 1000);
}

TEST_F(HaversackTest, InsertNonowning) {
  A a{10};
  {
    Haversack<A> cxt =
        Haversack<>().Insert(std::shared_ptr<const A>(&a, [](const A*) {}));
    EXPECT_THAT(cxt.Get<A>().i, 10);
  }
  EXPECT_THAT(a.i, 10);
}

TEST_F(HaversackTest, LRefInsert) {
  Haversack<> base;
  Haversack<Nonmoveable> cxt =
      base.Insert(std::make_shared<Nonmoveable>(100, 200));
  EXPECT_THAT(cxt.Get<Nonmoveable>().i, 100);
  EXPECT_THAT(cxt.Get<Nonmoveable>().j, 200);
  EXPECT_THAT(cxt.Get<Nonmoveable>().Getter(), 100);
}

TEST_F(HaversackTest, Copyable) {
  AHSack cxt = handler_cxt_;
  AHSack copy = cxt;
  copy = cxt;
  cxt = copy;
}

TEST_F(HaversackTest, NamedToRawHaversack) {
  AHSack cxt = handler_cxt_;
  AHSack::HaversackT copy = cxt;
  copy = cxt;
  cxt = copy;
}

TEST_F(HaversackTest, RawHaversackToNamed) {
  AHSack::HaversackT cxt = handler_cxt_;
  AHSack copy = cxt;
  copy = cxt;
  cxt = copy;
}

TEST_F(HaversackTest, RawHaversackToChild) {
  AHSack::HaversackT cxt = handler_cxt_;
  DHSack named = cxt;
  DHSack::HaversackT raw = cxt;
}

TEST_F(HaversackTest, HaversackFromNothing) {
  AHSack cxt(std::make_unique<A>(100), std::make_unique<L>(101),
             std::make_unique<D>(102), std::make_unique<M>(103),
             std::make_unique<E>(104), std::make_unique<N>(105));
  EXPECT_THAT(cxt.Get<A>().i, 100);
  EXPECT_THAT(cxt.Get<L>().i, 101);
}

TEST_F(HaversackTest, WithoutUnneededType) {
  AHSack cxt = handler_cxt_.Without<F>();
  EXPECT_EQ(cxt.Get<A>().i, 1);
}

TEST_F(HaversackTest, WithoutAndInsertType) {
  AHSack cxt = handler_cxt_.Without<A>().Insert(std::make_unique<A>(100));
  EXPECT_EQ(cxt.Get<A>().i, 100);
}

TEST_F(HaversackTest, CannotWithoutNonExistentType) {
  EXPECT_NON_COMPILE(
      "The original haversack must contain all the types being removed.",
      handler_cxt_.Without<G>());
}

TEST_F(HaversackTest, FunctionCallsDependency) {
  using HSack = Haversack<Calls<MyFunc>>;
  HSack cxt = HSack(std::make_unique<C>(100), std::make_unique<D>(101));
  EXPECT_EQ(MyFunc(A(1), B(2), cxt), 5);
}

TEST_F(HaversackTest, FunctionCallsDependencyTakingRef) {
  using HSack = Haversack<Calls<MyFuncTakingRef>>;
  HSack cxt = HSack(std::make_unique<E>(100), std::make_unique<F>(101));
  EXPECT_EQ(MyFuncTakingRef(A(1), B(2), cxt), 10);
}

TEST_F(HaversackTest, FunctionCallsDependencyNonCopyable) {
  using HSack = Haversack<Calls<non_copyable_functor>>;
  HSack cxt = HSack(std::make_unique<A>(100), std::make_unique<B>(101));
  EXPECT_EQ(non_copyable_functor(cxt), 10);
}

TEST_F(HaversackTest, NotDefaultConstructable) {
  EXPECT_FALSE(std::is_default_constructible_v<AHSack>);
}

TEST(HaversackCtor, Pointer) {
  using HSack = Haversack<A*>;
  A a{1};
  A* ap = &a;
  HSack cxt(&ap);
  EXPECT_THAT(cxt.Get<A*>()->i, 1);
}

TEST(HaversackCtor, Default) { Haversack<> cxt; }

TEST(HaversackCtor, UpcastWithoutSlicing) {
  Haversack<VirtualParent> sack(std::make_unique<VirtualChild>(10));
  EXPECT_EQ(sack.Get<VirtualParent>().DoStuff(), 10);
}

TEST(HaversackCtor, TakeShared) {
  Haversack<A> sack(std::make_shared<A>(10));
  EXPECT_EQ(sack.Get<A>().i, 10);
}

TEST(HaversackCtor, TakeNotNullShared) {
  Haversack<A> sack(gsl::not_null(std::make_shared<A>(10)));
  EXPECT_EQ(sack.Get<A>().i, 10);
}

TEST(HaversackCtor, TakeUnique) {
  Haversack<A> sack(std::make_unique<A>(10));
  EXPECT_EQ(sack.Get<A>().i, 10);
}

TEST(HaversackCtor, TakeNotNullUnique) {
  Haversack<A> sack(gsl::not_null(std::make_unique<A>(10)));
  EXPECT_EQ(sack.Get<A>().i, 10);
}

TEST(HaversackCtor, TakeRaw) {
  A a{10};
  Haversack<A> sack(&a);
  EXPECT_EQ(sack.Get<A>().i, 10);
}

TEST(HaversackCtor, TakeNotNullRaw) {
  A a{10};
  Haversack<A> sack{gsl::not_null(&a)};
  EXPECT_EQ(sack.Get<A>().i, 10);
}

TEST(HaversackCtor, WrongArity) {
  EXPECT_NON_COMPILE(
      "TargetMatches<hotels::haversack::\\(anonymous namespace\\)::A>.*The "
      "target type doesn\\'t have any matches",
      Haversack<A, B>(std::make_unique<B>(2)));
}

TEST(HaversackCtor, WrongArityNullable) {
  EXPECT_NON_COMPILE(
      "TargetMatches<hotels::haversack::Nullable<hotels::haversack::\\("
      "anonymous namespace\\)::A>>.*The "
      "target type doesn\\'t have any matches",
      Haversack<Nullable<A>, B>(std::make_unique<B>(2)));
}

TEST(HaversackCtor, NoArgumentMatch) {
  EXPECT_NON_COMPILE(
      "SourceMatches<hotels::haversack::\\(anonymous namespace\\)::B>.*The "
      "source type doesn\\'t have any matches",
      {
        B b{1};
        Haversack<> sack(&b);
      });
}

TEST(HaversackCtor, ShowsLineInTraceback) {
  EXPECT_NON_COMPILE("unique_variable_name", {
    B b{1};
    Haversack<> unique_variable_name(&b);
  });
}

TEST(HaversackCtor, VagueArgumentMatch) {
  EXPECT_NON_COMPILE(
      "TargetMatches<hotels::haversack::\\(anonymous namespace\\)::A, "
      "hotels::haversack::\\(anonymous namespace\\)::A, "
      "hotels::haversack::\\(anonymous namespace\\)::A>.*The "
      "target type has more than one match",
      {
        A a{1};
        Haversack<A> sack(&a, &a);
      });
}

TEST(HaversackCtor, VagueParameterMatch) {
  EXPECT_NON_COMPILE(
      "SourceMatches<hotels::haversack::\\(anonymous namespace\\)::SubA, "
      "hotels::haversack::\\(anonymous namespace\\)::A, "
      "hotels::haversack::\\(anonymous namespace\\)::SubA>.*"
      "The source type has more than one match",
      {
        SubA a(1);
        Haversack<A, SubA> sack(&a);
      });
}

TEST(FakeHaversack, MakeFakeHaversack) {
  AHSack cxt =
      MakeFakeHaversack<AHSack>(std::make_unique<A>(1), std::make_unique<L>(2));
  EXPECT_EQ(cxt.Get<A>().i, 1);
}

TEST(FakeHaversack, MakeFakeHaversackPropagate) {
  AHSack cxt = MakeFakeHaversack<AHSack, DHSack>(
      std::make_unique<A>(1), std::make_unique<L>(2), std::make_unique<D>(3),
      std::make_unique<M>(4));
  DHSack d = cxt;
  EXPECT_EQ(cxt.Get<A>().i, 1);
  EXPECT_EQ(d.Get<D>().i, 3);
}

TEST(FakeHaversack, MakeFakeHaversackWithProvides) {
  FHSack cxt = MakeFakeHaversack<FHSack, GHSack>(
      std::make_unique<F>(1), std::make_unique<O>(2), std::make_unique<L>(3));
  GHSack g(cxt, std::make_unique<G>(4));
  EXPECT_EQ(cxt.Get<F>().i, 1);
  EXPECT_EQ(g.Get<L>().i, 3);
  EXPECT_EQ(g.Get<G>().i, 4);
}

TEST(HaversackConversion, CanConvertToSubset) {
  Haversack<A, B> sack(std::make_unique<A>(1), std::make_unique<B>(2));
  Haversack<A> child = sack;
  EXPECT_EQ(child.Get<A>().i, 1);
  EXPECT_TRUE((std::is_convertible_v<Haversack<A, B>, Haversack<A>>));
}

TEST(HaversackConversion, CannotConvertToSuperset) {
  EXPECT_FALSE((std::is_convertible_v<Haversack<A>, Haversack<A, B>>));
}

TEST(HaversackConversion, CannotConvertToDisjointSet) {
  EXPECT_FALSE((std::is_convertible_v<Haversack<A>, Haversack<B>>));
}

TEST(KnownThreadSafe, GetKnownThreadSafeRef) {
  A a(1);
  Haversack<KnownThreadSafe<A>> sack(&a);
  sack.Get<KnownThreadSafe<A>>().i = 10;
  EXPECT_THAT(a.i, 10);
}

TEST(HaversackCtor, CannotConstToKnownThreadSafe) {
  EXPECT_NON_COMPILE(
      "SourceMatches<const hotels::haversack::\\(anonymous namespace\\)::A>.*"
      "The source type doesn\\'t have any matches",
      {
        const A a(1);
        Haversack<KnownThreadSafe<A>> sack(&a);
      });
  EXPECT_NON_COMPILE(
      "TargetMatches<hotels::haversack::KnownThreadSafe<hotels::haversack::\\("
      "anonymous namespace\\)::A>>.*The target type doesn\\'t have any matches",
      {
        const A a(1);
        Haversack<KnownThreadSafe<A>> sack(&a);
      });
}

TEST(HaversackCtor, NonPtrArgumentsGiveGoodMessage) {
  EXPECT_NON_COMPILE(
      "One of the Haversack constructor arguments wasn't a pointer when it "
      "should have been .or it wasn't a supported pointer..",
      { Haversack<A> sack(A{}); });
}

TEST(HaversackDefinitionOrder, DirectDepsLast) {
  EXPECT_NON_COMPILE(
      "All `Calls` and other directives must come before any "
      "direct dependencies.",
      { absl::optional<Haversack<A, Deps<AHSack>>> dependencies; });
}

TEST(HaversackNullable, RegularDoesntAcceptNullptr) {
  EXPECT_DEATH(Haversack<A>(static_cast<A*>(nullptr)),
               "Pointers should never be null in haversack");
}

TEST(HaversackNullable, AcceptsNullptr) {
  Haversack<Nullable<A>> deps{static_cast<A*>(nullptr)};
}

TEST(HaversackNullable, GetPtr) {
  A a(1);
  Haversack<Nullable<A>> deps{&a};
  EXPECT_THAT(deps.Get<Nullable<A>>(), &a);
}

TEST(HaversackNullable, GetNullptr) {
  Haversack<Nullable<A>> deps{static_cast<A*>(nullptr)};
  EXPECT_THAT(deps.Get<Nullable<A>>(), nullptr);
}

TEST(HaversackNullable, NullableKnownThreadSafe) {
  A a(1);
  Haversack<Nullable<KnownThreadSafe<A>>> deps(&a);
  deps.Get<Nullable<KnownThreadSafe<A>>>()->i = 5;
  EXPECT_THAT(a.i, 5);
}

TEST(HaversackNullable, KnownTheadSafeNullableNotAllowed) {
  EXPECT_NON_COMPILE(
      "See internal::WrappingMetadataOrder for more information about type "
      "wrapping order.",
      { absl::optional<Haversack<KnownThreadSafe<Nullable<A>>>> deps; });
}

TEST(HaversackNullable, NullableAndRegular) {
  A a{1};
  Haversack<A, Nullable<A>> deps{
      Haversack<>().Insert<Nullable<A>>(static_cast<A*>(nullptr)), &a};
  EXPECT_THAT(deps.Get<Nullable<A>>(), testing::IsNull());
  EXPECT_THAT(deps.Get<A>().i, 1);
}

TEST(HaversackNullable, NullableProvidesRegular) {
  A a{1};
  B b{2};
  using RequiredChild = Haversack<A, B>;
  using NullableParent =
      Haversack<Deps<RequiredChild>, Provides<A>, Nullable<A>>;
  NullableParent parent{&a, &b};
  RequiredChild child{parent, parent.Get<Nullable<A>>()};
  EXPECT_THAT(child.Get<A>().i, 1);
  EXPECT_THAT(child.Get<B>().i, 2);
}

TEST(HaversackNullable, NullableFromConst) {
  const A a{1};
  Haversack<Nullable<A>> deps{&a};
  EXPECT_THAT(deps.Get<Nullable<A>>()->i, 1);
}

TEST(HaversackNullable, ReplaceNullableExplicit) {
  A a{1};
  Haversack<Nullable<A>> deps{&a};
  deps = deps.Replace<Nullable<A>>(nullptr);
  EXPECT_THAT(deps.Get<Nullable<A>>(), testing::IsNull());
}

TEST(HaversackNullable, ReplaceNullableStaticCast) {
  A a{1};
  Haversack<Nullable<A>> deps{&a};
  deps = deps.Replace(static_cast<A*>(nullptr));
  EXPECT_THAT(deps.Get<Nullable<A>>(), testing::IsNull());
}

TEST(Haversack, AllWrappers) {
  Haversack<A, KnownThreadSafe<A>, Nullable<A>, Nullable<KnownThreadSafe<A>>>
      deps = Haversack<>()
                 .Insert<A>(std::make_shared<A>(1))
                 .Insert<KnownThreadSafe<A>>(std::make_shared<A>(2))
                 .Insert<Nullable<A>>(std::make_shared<A>(3))
                 .Insert<Nullable<KnownThreadSafe<A>>>(std::make_shared<A>(4));
  EXPECT_THAT(deps.Get<A>().i, 1);
  EXPECT_THAT(deps.Get<KnownThreadSafe<A>>().i, 2);
  EXPECT_THAT(deps.Get<Nullable<A>>()->i, 3);
  EXPECT_THAT(deps.Get<Nullable<KnownThreadSafe<A>>>()->i, 4);
}

TEST(HaversackTagged, CreateHaversackWithTagged) {
  Haversack<Tagged<A, TagA>> deps{std::make_shared<A>(1)};
}

TEST(HaversackTagged, GetTagged) {
  Haversack<Tagged<A, TagA>> deps{std::make_shared<A>(1)};
  EXPECT_EQ((deps.Get<Tagged<A, TagA>>().i), 1);
}

TEST(HaversackTagged, GetByTag) {
  Haversack<Tagged<A, TagA>> deps{std::make_shared<A>(1)};
  EXPECT_EQ(deps.Get<TagA>().i, 1);
}

TEST(HaversackTagged, DistinguishTypesByTag) {
  Haversack<Tagged<A, TagA>, Tagged<A, struct TagB>> deps{
      Haversack<Tagged<A, TagA>>{std::make_shared<A>(1)},
      std::make_shared<A>(2)};
  EXPECT_EQ(deps.Get<TagA>().i, 1);
  EXPECT_EQ(deps.Get<struct TagB>().i, 2);
}

TEST(HaversackTagged, ConvertMakeTagged) {
  Haversack<Tagged<A, TagA>> deps{MakeTagged<TagA>(std::make_shared<SubA>(1))};
  EXPECT_EQ(deps.Get<TagA>().i, 1);
  EXPECT_EQ(deps.Get<TagA>().ClassName(), "SubA");
}

TEST(HaversackTagged, CannotRepeatTag) {
  EXPECT_NON_COMPILE(
      "A Haversack cannot have multiple direct dependencies with the same Tag.",
      (Haversack<Tagged<A, TagA>, Tagged<B, TagA>>{std::make_shared<A>(1),
                                                   std::make_shared<B>(2)}));
}

TEST(HaversackTagged, RepeatTagsAtDifferentLevels) {
  using Child = Haversack<Tagged<B, TagA>>;
  Haversack<Deps<Child>, Tagged<A, TagA>> parent{std::make_shared<A>(1),
                                                 std::make_shared<B>(2)};
  Child child = parent;
  EXPECT_EQ(parent.Get<TagA>().i, 1);
  EXPECT_EQ(child.Get<TagA>().i, 2);
}

TEST(HaversackTagged, ConstructMultipleTagged) {
  Haversack<Tagged<A, TagA>, Tagged<A, struct TagB>> deps{
      MakeTagged<TagA>(std::make_shared<A>(1)),
      MakeTagged<struct TagB>(std::make_shared<A>(2))};
  EXPECT_EQ(deps.Get<TagA>().i, 1);
  EXPECT_EQ(deps.Get<struct TagB>().i, 2);
}

TEST(HaversackTagged, Insert) {
  auto deps = Haversack<>().Insert(MakeTagged<TagA>(std::make_shared<A>(1)));
  EXPECT_EQ(deps.Get<TagA>().i, 1);
}

TEST(HaversackTagged, ExplicitInsert) {
  auto deps = Haversack<>().Insert<Tagged<A, TagA>>(std::make_shared<A>(1));
  EXPECT_EQ(deps.Get<TagA>().i, 1);
}

TEST(Haversack, TaggedNullableKnownThreadSafe) {
  Haversack<Tagged<Nullable<KnownThreadSafe<A>>, TagA>> deps{
      static_cast<A*>(nullptr)};
  EXPECT_THAT(deps.Get<TagA>(), testing::IsNull());
  deps = deps.Replace(std::make_shared<A>(1));
  EXPECT_EQ(deps.Get<TagA>()->i, 1);
  deps.Get<TagA>()->i = 10;
  EXPECT_EQ(deps.Get<TagA>()->i, 10);
}

TEST(Haversack, NoReferences) {
  EXPECT_NON_COMPILE("Reference types are not allowed in Haversack.",
                     sizeof(Haversack<A&>));
}

TEST(Haversack, NoExplicitConst) {
  EXPECT_NON_COMPILE(
      "Constness is implicit in Haversack. KnownThreadSafe "
      "should be used for non-const types instead.",
      sizeof(Haversack<const A>));
}

TEST(Haversack, NoTagDependencies) {
  EXPECT_NON_COMPILE("Don't use Tag types as dependencies.",
                     sizeof(Haversack<A, Tagged<A, A>>));
}

TEST(Haversack, GetSharedNoCopy) {
  // GetShared should return a reference to the pointer inside the haversack
  // without any shared_ptr copies.
  Haversack<A> deps(std::make_shared<A>(1));
  const std::shared_ptr<const A>& first = deps.GetShared<A>();
  const std::shared_ptr<const A>& second = deps.GetShared<A>();
  EXPECT_EQ(&first, &second);
}

TEST(GoodCompilerErrors, Get) {
  EXPECT_NON_COMPILE(
      "Requested type is not a direct dependency in the Haversack.",
      (void)Haversack<>().Get<B>());
}

TEST(GoodCompilerErrors, GetShared) {
  EXPECT_NON_COMPILE(
      "Requested type is not a direct dependency in the Haversack.",
      (void)Haversack<>().GetShared<B>());
}

}  // namespace
}  // namespace hotels::haversack
