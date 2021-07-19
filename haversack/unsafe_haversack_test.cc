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

#include "testing/base/public/gmock.h"
#include "testing/base/public/gunit.h"
#include "./haversack.h"
#include "./haversack_test_util.h"

namespace hotels::haversack {
namespace {

struct A {
  explicit A(int i = 0) : i(i) {}
  int i = 0;
};
struct B {
  explicit B(int i = 0) : i(i) {}
  int i = 0;
};

struct BHaversack : Haversack<B> {
  using HaversackT::HaversackT;
};
struct AHaversack : Haversack<Deps<BHaversack>, A> {
  using HaversackT::HaversackT;
};

TEST(FakeHaversack, MakeFakeHaversackDeathMessage) {
  AHaversack dependencies =
      MakeFakeHaversack<AHaversack>(std::make_unique<A>(1));
  BHaversack b = dependencies;
#ifndef NDEBUG
  // We only get a nice message if it's a debug build.
  EXPECT_DEATH((void)b.Get<B>(),
               "A value for .hotels::haversack::.anonymous namespace.::B. was "
               "not injected with MakeFakeHaversack");
#else
  // This is technically undefined behavior and triggers a warning from ASAN, so
  // we disable ASAN for this file.
  EXPECT_THAT(&b.Get<B>(), testing::IsNull());
#endif
}

}  // namespace
}  // namespace hotels::haversack
