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

#include "./haversack.h"

#include <type_traits>

#include "testing/base/public/gmock.h"
#include "testing/base/public/gunit.h"
#include "./haversack_test.nc.h"
#include "./haversack_test_util.h"

namespace hotels::haversack {
namespace {

// Types A-O are placeholders for real-world types. They each hold an int so
// that we can inject arbitrary values in the test and verify that they are
// propagated correctly.
struct A {
  explicit A(int i = 0) : i(i) {}
  int i = 0;
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

MATCHER_P(CanGet, type, "") {
  *result_listener << "Cannot get a "
                   << internal::debug_type_name_v<
                          std::decay_t<decltype(type)>> << " from a "
                   << internal::debug_type_name_v<std::decay_t<decltype(arg)>>;
  return internal::types::IsValidExpr(
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
  GHSack cxt(handler_cxt_, absl::make_unique<G>(100));
  EXPECT_THAT(cxt.Get<G>().i, 100);
  EXPECT_THAT(cxt.Get<L>().i, 7);
}

TEST_F(CtorAndAccessTest, GHSackFromCHSack) {
  CHSack c_cxt = handler_cxt_;
  GHSack cxt(c_cxt, absl::make_unique<G>(100));
  EXPECT_THAT(cxt.Get<G>().i, 100);
  EXPECT_THAT(cxt.Get<L>().i, 7);
}

TEST_F(CtorAndAccessTest, GHSackFromFHSack) {
  FHSack f_cxt = handler_cxt_;
  GHSack cxt(f_cxt, absl::make_unique<G>(100));
  EXPECT_THAT(cxt.Get<G>().i, 100);
  EXPECT_THAT(cxt.Get<L>().i, 7);
}

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
  AHSack cxt(absl::make_unique<A>(100), absl::make_unique<L>(101),
             absl::make_unique<D>(102), absl::make_unique<M>(103),
             absl::make_unique<E>(104), absl::make_unique<N>(105));
  EXPECT_THAT(cxt.Get<A>().i, 100);
  EXPECT_THAT(cxt.Get<L>().i, 101);
}

TEST_F(HaversackTest, WithoutUnneededType) {
  AHSack cxt = handler_cxt_.Without<F>();
  EXPECT_EQ(cxt.Get<A>().i, 1);
}

TEST_F(HaversackTest, WithoutAndInsertType) {
  AHSack cxt = handler_cxt_.Without<A>().Insert(absl::make_unique<A>(100));
  EXPECT_EQ(cxt.Get<A>().i, 100);
}

TEST_F(HaversackTest, CannotWithoutNonExistentType) {
  EXPECT_NON_COMPILE(
      "The original haversack must contain all the types being removed.",
      handler_cxt_.Without<G>());
}

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

TEST_F(HaversackTest, FunctionCallsDependency) {
  using HSack = Haversack<Calls<MyFunc>>;
  HSack cxt = HSack(absl::make_unique<C>(100), absl::make_unique<D>(101));
  EXPECT_EQ(MyFunc(A(1), B(2), cxt), 5);
}

TEST_F(HaversackTest, FunctionCallsDependencyTakingRef) {
  using HSack = Haversack<Calls<MyFuncTakingRef>>;
  HSack cxt = HSack(absl::make_unique<E>(100), absl::make_unique<F>(101));
  EXPECT_EQ(MyFuncTakingRef(A(1), B(2), cxt), 10);
}

TEST_F(HaversackTest, FunctionCallsDependencyNonCopyable) {
  using HSack = Haversack<Calls<non_copyable_functor>>;
  HSack cxt = HSack(absl::make_unique<A>(100), absl::make_unique<B>(101));
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

struct VirtualParent {
  virtual int DoStuff() const { return 0; }
  virtual ~VirtualParent() = default;
};
struct VirtualChild : VirtualParent {
  explicit VirtualChild(int i) : i(i) {}
  int DoStuff() const override { return i; }

  int i = 1;
};

TEST(HaversackCtor, UpcastWithoutSlicing) {
  Haversack<VirtualParent> sack(std::make_unique<VirtualChild>(10));
  EXPECT_EQ(sack.Get<VirtualParent>().DoStuff(), 10);
}

TEST(HaversackCtor, TakeShared) {
  Haversack<A> sack(std::make_shared<A>(10));
  EXPECT_EQ(sack.Get<A>().i, 10);
}

TEST(HaversackCtor, TakeNotNullShared) {
  Haversack<A> sack(not_null(std::make_shared<A>(10)));
  EXPECT_EQ(sack.Get<A>().i, 10);
}

TEST(HaversackCtor, TakeUnique) {
  Haversack<A> sack(std::make_unique<A>(10));
  EXPECT_EQ(sack.Get<A>().i, 10);
}

TEST(HaversackCtor, TakeNotNullUnique) {
  Haversack<A> sack(not_null(std::make_unique<A>(10)));
  EXPECT_EQ(sack.Get<A>().i, 10);
}

TEST(HaversackCtor, TakeRaw) {
  A a{10};
  Haversack<A> sack(&a);
  EXPECT_EQ(sack.Get<A>().i, 10);
}

TEST(HaversackCtor, TakeNotNullRaw) {
  A a{10};
  Haversack<A> sack{not_null(&a)};
  EXPECT_EQ(sack.Get<A>().i, 10);
}

TEST(HaversackCtor, WrongArity) {
  EXPECT_NON_COMPILE(
      "TargetMatches<hotels::haversack::\\(anonymous namespace\\)::A \\*>.*The "
      "target type doesn\\'t have any matches",
      Haversack<A*, B>(absl::make_unique<B>(2)));
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

struct SubA : A {
  using A::A;
};

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

TEST(HaversackConversion, CanConvertToSubset) {
  Haversack<A, B> sack(absl::make_unique<A>(1), absl::make_unique<B>(2));
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

TEST(HaversackDefinitionOrder, DirectDepsLast) {
  EXPECT_NON_COMPILE(
      "All `Calls` and other directives must come before any "
      "direct dependencies.",
      { absl::optional<Haversack<A, Deps<AHSack>>> dependencies; });
}

}  // namespace
}  // namespace hotels::haversack
