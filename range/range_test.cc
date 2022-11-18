// Copyright 2022 Google LLC
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

#include "range/range.h"

#include <algorithm>
#include <functional>
#include <initializer_list>
#include <list>
#include <numeric>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include "testing/base/public/benchmark.h"
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <absl/status/status.h>
#include <absl/status/statusor.h>
#include <absl/types/span.h>

namespace htls::range {

namespace {

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::Field;
using ::testing::status::IsOkAndHolds;
using ::testing::status::StatusIs;

// To aid with testing things like stopping iteration early
// this combinator just does an identity transform and increments a counter

auto Counter(int& count) {
  return TransformEach([&count](auto&& value) mutable -> decltype(auto) {
    ++count;
    return std::forward<decltype(value)>(value);
  });
}

TEST(TestOutputCombinators, NoCombinators) {
  std::vector<int> v{1, 2, 3};
  auto result = Apply(v);
  EXPECT_THAT(result, ElementsAre(1, 2, 3));
}

TEST(TestOutputCombinators, ToVector) {
  std::vector<int> v{1, 2, 3};
  auto result = Apply(v, ToVector());
  EXPECT_THAT(result, ElementsAre(1, 2, 3));
}

TEST(TestOutputCombinators, ForEach) {
  std::vector<int> v{1, 2, 3};
  auto result = Apply(v, ForEach([count = 0](int i) mutable {
                        EXPECT_THAT(i, count + 1);
                        count++;
                      }));
  EXPECT_THAT(result, 3);
}

TEST(TestOutputCombinators, Accumulate) {
  std::vector<int> v{1, 2, 3};
  auto result = Apply(v, Accumulate(0, std::plus<>()));
  EXPECT_THAT(result, 6);
}

TEST(TestOutputCombinators, AccumulateInPlace) {
  std::vector<int> v{1, 2, 3};
  auto plus = [](auto& accumulated, auto& value) { accumulated += value; };
  auto result = Apply(v, AccumulateInPlace(0, plus));
  EXPECT_THAT(result, 6);
}

TEST(TestOutputCombinators, ToVectorAddressOf) {
  std::vector<int> v{1, 2, 3};
  auto result = Apply(v,                //
                      AddressOfEach(),  //
                      ToVector()        //
  );
  EXPECT_THAT(result, ElementsAre(&v[0], &v[1], &v[2]));
}

TEST(TestOutputCombinators, Enumerate) {
  std::vector<int> v{1, 2, 3, 4};
  auto index_not_equal_2 = [](auto&& e) { return e.index != 2; };
  auto result = Apply(v,            //
                      Enumerate(),  //
                      Filter(index_not_equal_2),
                      Unenumerate(),  //
                      ToVector()      //
  );
  EXPECT_THAT(result, ElementsAre(1, 2, 4));
}

TEST(TestOutputCombinators, TakeToVector) {
  std::vector<int> v{1, 2, 3};
  auto result = Apply(v,          //
                      Take(2),    //
                      ToVector()  //
  );
  EXPECT_THAT(result, ElementsAre(1, 2));
}

TEST(TestOutputCombinators, TakeToRefVector) {
  std::vector<int> v{1, 2, 3};
  auto result = Apply(v,          //
                      RefEach(),  //
                      Take(2),    //
                      ToVector()  //
  );
  static_assert(std::is_same_v<std::decay_t<decltype(result)>::value_type,
                               std::reference_wrapper<int>>);
  EXPECT_THAT(result, ElementsAre(1, 2));
}

TEST(TestOutputCombinators, TakeToMoveVector) {
  std::vector<std::string> v{"Hello", "World", "John"};
  auto result = Apply(v,           //
                      Take(2),     //
                      RefEach(),   //
                      MoveEach(),  //
                      ToVector()   //
  );
  static_assert(
      std::is_same_v<std::decay_t<decltype(result)>::value_type, std::string>);
  EXPECT_THAT(result, ElementsAre("Hello", "World"));
  EXPECT_THAT(v, ElementsAre("", "", "John"));
}

TEST(TestOutputCombinators, TakeToMoveLRefVector) {
  std::vector<std::string> v{"Hello", "World", "John"};
  auto result = Apply(v,           //
                      Take(2),     //
                      RefEach(),   //
                      MoveEach(),  //
                      LRefEach(),  //
                      ToVector()   //
  );
  static_assert(
      std::is_same_v<std::decay_t<decltype(result)>::value_type, std::string>);
  EXPECT_THAT(result, ElementsAre("Hello", "World"));
  EXPECT_THAT(v, ElementsAre("Hello", "World", "John"));
}

TEST(TestOutputCombinators, TransformToVector) {
  std::vector<int> v{1, 2, 3};
  auto to_double = [](int i) { return static_cast<double>(i) + 0.5; };
  auto result = Apply(v,                         //
                      TransformEach(to_double),  //
                      ToVector()                 //
  );
  EXPECT_THAT(result, ElementsAre(1.5, 2.5, 3.5));
}

TEST(TestOutputCombinators, TransformComplet) {
  auto to_double = [](int i) { return static_cast<double>(i) + 0.5; };
  auto result = Apply(1,                    //
                      Transform(to_double)  //
  );

  static_assert(std::is_same_v<decltype(Apply(1,                    //
                                              Transform(to_double)  //
                                              )),
                               double>);
  EXPECT_THAT(result, 1.5);
}

TEST(TestOutputCombinators, TransformCompleteAndThen) {
  auto to_double = [](int i) {
    return absl::StatusOr<double>(static_cast<double>(i) + 0.5);
  };
  auto result = Apply(1,                     //
                      Transform(to_double),  //
                      AndThen()              //
  );

  static_assert(std::is_same_v<decltype(Apply(1,                     //
                                              Transform(to_double),  //
                                              AndThen()              //
                                              )),
                               absl::StatusOr<double>>);
  EXPECT_THAT(result, IsOkAndHolds(1.5));
}

TEST(TestOutputCombinators, TransformCompleteAndThenWithError) {
  auto to_double = [](int i) -> absl::StatusOr<double> {
    if (i == 0) {
      return absl::UnknownError("test");
    }
    return absl::StatusOr<double>(static_cast<double>(i) + 0.5);
  };
  auto result = Apply(0,                     //
                      Transform(to_double),  //
                      AndThen()              //
  );

  EXPECT_THAT(result, StatusIs(absl::StatusCode::kUnknown, "test"));
}

TEST(TestOutputCombinators, FilterToVector) {
  std::vector<int> v{1, 2, 3};
  auto even_numbers = [](int i) { return i % 2 == 0; };
  auto result = Apply(v, Filter(even_numbers), ToVector());
  EXPECT_THAT(result, ElementsAre(2));
}

TEST(TestOutputCombinators, FilterTransformToVector) {
  std::vector<int> v{1, 2, 3};
  auto to_double = [](int i) { return static_cast<double>(i) + 0.5; };
  auto even_numbers = [](int i) { return i % 2 == 0; };
  auto result = Apply(v,                         //
                      Filter(even_numbers),      //
                      TransformEach(to_double),  //
                      ToVector()                 //
  );
  static_assert(
      std::is_same_v<std::decay_t<decltype(result)>, std::vector<double>>);
  EXPECT_THAT(result, ElementsAre(2.5));
}

TEST(TestOutputCombinators, TransformFilterToVector) {
  std::vector<int> v{1, 2, 3};
  auto to_double = [](int i) { return static_cast<double>(i) + 0.5; };
  auto greater_than_2 = [](double d) { return d > 2.0; };
  auto result = Apply(v,                         //
                      TransformEach(to_double),  //
                      Filter(greater_than_2),    //
                      ToVector()                 //
  );
  EXPECT_THAT(result, ElementsAre(2.5, 3.5));
}

TEST(TestOutputCombinators, TransformAndThenFilterToVectorError) {
  std::vector<int> v{1, 2, 3, 0};
  auto to_double = [](int i) -> absl::StatusOr<double> {
    if (i == 0) {
      return absl::UnknownError("test");
    }
    return static_cast<double>(i) + 0.5;
  };
  auto greater_than_2 = [](double d) { return d > 2.0; };
  auto result = Apply(v,                         //
                      TransformEach(to_double),  //
                      AndThenEach(),             //
                      Filter(greater_than_2),    //
                      ToVector()                 //
  );
  EXPECT_THAT(result, StatusIs(absl::StatusCode::kUnknown, "test"));
}

TEST(TestOutputCombinators, TransformAndThenFilterToVectorSuccess) {
  std::vector<int> v{1, 2, 3, 0};
  auto to_double = [](int i) -> absl::StatusOr<double> {
    return static_cast<double>(i) + 0.5;
  };
  auto greater_than_2 = [](double d) { return d > 2.0; };
  auto result = Apply(v,                         //
                      TransformEach(to_double),  //
                      AndThenEach(),             //
                      Filter(greater_than_2),    //
                      ToVector()                 //
  );
  EXPECT_THAT(result, IsOkAndHolds(ElementsAre(2.5, 3.5)));
}

TEST(TestOutputCombinators, TransformFilterTakeToVector) {
  std::vector<int> v{1, 2, 3};
  auto to_double = [](int i) { return static_cast<double>(i) + 0.5; };
  auto greater_than_2 = [](double d) { return d > 2.0; };
  int counter1 = 0;
  int counter2 = 0;
  auto result = Apply(v,                         //
                      Counter(counter1),         //
                      TransformEach(to_double),  //
                      Filter(greater_than_2),    //
                      Counter(counter2),         //
                      Take(1),                   //
                      ToVector()                 //
  );
  EXPECT_THAT(result, ElementsAre(2.5));
  // We have to iterator 2 elements because 1 gets dropped by Filter, before we
  // can Take(1).
  EXPECT_THAT(counter1, 2);
  EXPECT_THAT(counter2, 1);
}

TEST(TestOutputCombinators, SortGreater) {
  std::vector<int> v{1, 2, 3};
  auto& result = Apply(v, Sort(std::greater<>()));
  static_assert(std::is_same_v<decltype(Apply(v, Sort(std::greater<>()))),
                               std::vector<int>&>);
  EXPECT_THAT(result, ElementsAre(3, 2, 1));
  EXPECT_THAT(&v, &result);
}

TEST(TestOutputCombinators, Flatten) {
  std::vector<std::string> v{"Hello", "World"};
  auto result = Apply(v, Flatten(), ToVector());
  EXPECT_THAT(result,
              ElementsAre('H', 'e', 'l', 'l', 'o', 'W', 'o', 'r', 'l', 'd'));
}
TEST(TestOutputCombinators, SortUnique) {
  std::vector<int> v{3, 2, 3, 1};
  auto& result = Apply(v, Sort(), Unique());
  EXPECT_THAT(result, ElementsAre(1, 2, 3));
  EXPECT_THAT(&v, &result);
}

// See truth table at
// https://en.cppreference.com/w/cpp/algorithm/all_any_none_of

TEST(TestOutputCombinators, AllOf) {
  auto all_true = {true, true, true};
  auto some_true = {false, true, false};
  auto all_false = {false, false, false};
  auto empty = std::initializer_list<bool>{};

  auto identity = [](bool b) { return b; };
  EXPECT_THAT(Apply(all_true, AllOf(identity)), true);
  EXPECT_THAT(Apply(some_true, AllOf(identity)), false);
  EXPECT_THAT(Apply(all_false, AllOf(identity)), false);
  EXPECT_THAT(Apply(empty, AllOf(identity)), true);
}

TEST(TestOutputCombinators, AllOfShortCircuit) {
  auto some_true = {false, true, false};
  auto identity = [](bool b) { return b; };
  int count = 0;
  Apply(some_true, Counter(count), AllOf(identity));
  EXPECT_THAT(count, 1);
}
TEST(TestOutputCombinators, AnyOf) {
  auto all_true = {true, true, true};
  auto some_true = {false, true, false};
  auto all_false = {false, false, false};
  auto empty = std::initializer_list<bool>{};

  auto identity = [](bool b) { return b; };
  EXPECT_THAT(Apply(all_true, AnyOf(identity)), true);
  EXPECT_THAT(Apply(some_true, AnyOf(identity)), true);
  EXPECT_THAT(Apply(all_false, AnyOf(identity)), false);
  EXPECT_THAT(Apply(empty, AnyOf(identity)), false);
}

TEST(TestOutputCombinators, AnyOfShortCircuit) {
  auto some_true = {false, true, false};
  auto identity = [](bool b) { return b; };
  int count = 0;
  Apply(some_true, Counter(count), AnyOf(identity));
  EXPECT_THAT(count, 2);
}

TEST(TestOutputCombinators, NoneOf) {
  auto all_true = {true, true, true};
  auto some_true = {false, true, false};
  auto all_false = {false, false, false};
  auto empty = std::initializer_list<bool>{};

  auto identity = [](bool b) { return b; };
  EXPECT_THAT(Apply(all_true, NoneOf(identity)), false);
  EXPECT_THAT(Apply(some_true, NoneOf(identity)), false);
  EXPECT_THAT(Apply(all_false, NoneOf(identity)), true);
  EXPECT_THAT(Apply(empty, NoneOf(identity)), true);
}

TEST(TestOutputCombinators, NoneOfShortCircuit) {
  auto some_true = {false, true, false};
  auto identity = [](bool b) { return b; };
  int count = 0;
  Apply(some_true, Counter(count), NoneOf(identity));
  EXPECT_THAT(count, 2);
}

TEST(TestOutputCombinators, SortUniqueSpan) {
  std::vector<int> v{3, 2, 3, 1};
  auto result = Apply(absl::MakeSpan(v), Sort(), Unique());
  EXPECT_THAT(result, ElementsAre(1, 2, 3));
  EXPECT_THAT(v, ElementsAre(1, 2, 3, 3));
}

template <typename T>
class MoveTrackingVector {
 public:
  using value_type = T;

  MoveTrackingVector() = default;
  MoveTrackingVector(std::initializer_list<T> init) : vector_(init) {}
  MoveTrackingVector(const MoveTrackingVector& other) : vector_(other.vector_) {
    other.moved_ = false;
  }

  MoveTrackingVector& operator=(const MoveTrackingVector& other) {
    moved_ = false;
    vector_ = other.vector_;
    return *this;
  }

  MoveTrackingVector(MoveTrackingVector&& other)
      : moved_(false), vector_(std::move(other.vector_)) {
    other.moved_ = true;
  }

  MoveTrackingVector& operator=(MoveTrackingVector&& other) {
    moved_ = false;
    vector_ = std::move(other.vector_);
    other.moved_ = true;
    return *this;
  }

  // Silence use after move warning.
  [[clang::reinitializes]] void reset() { vector_.clear(); }

  bool moved() const { return moved_; }

  auto begin() { return vector_.begin(); }
  auto begin() const { return vector_.begin(); }
  auto end() { return vector_.end(); }
  auto end() const { return vector_.end(); }

 private:
  mutable bool moved_ = false;
  std::vector<T> vector_;
};

TEST(TestOutputCombinators, SortGreaterMove) {
  MoveTrackingVector<int> v{1, 2, 3};
  auto result = Apply(std::move(v), Sort(std::greater<>()));
  static_assert(
      std::is_same_v<decltype(Apply(std::move(v), Sort(std::greater<>()))),
                     MoveTrackingVector<int>>);

  EXPECT_THAT(result, ElementsAre(3, 2, 1));
  v.reset();
  EXPECT_TRUE(v.moved());
}

TEST(TestOutputCombinators, SortGreaterToVector) {
  std::vector<int> v{1, 2, 3};
  auto result = Apply(v,                       //
                      Sort(std::greater<>()),  //
                      ToVector()               //

  );
  EXPECT_THAT(result, ElementsAre(3, 2, 1));
}

TEST(TestOutputCombinators, TransformedSortGreaterToVector) {
  std::vector<int> v{1, 2, 3};
  auto subtract_from_10 = [](int i) { return 10 - i; };
  auto result = Apply(v,                                //
                      TransformEach(subtract_from_10),  //
                      ToVector(),                       // s
                      Sort(std::greater<>()),           //
                      ToVector()                        //

  );

  EXPECT_THAT(result, ElementsAre(9, 8, 7));
}
TEST(TestOutputCombinators, TransformedSortGreaterFilterToVector) {
  std::vector<int> v{1, 2, 3};
  auto subtract_from_10 = [](int i) { return 10 - i; };
  auto odd_numbers = [](int i) { return i % 2 == 1; };
  auto result = Apply(v,                                //
                      TransformEach(subtract_from_10),  //
                      ToVector(),                       //
                      Sort(std::greater<>()),           //
                      Filter(odd_numbers),              //
                      ToVector()                        //

  );
  EXPECT_THAT(result, ElementsAre(9, 7));
}

TEST(TestOutputCombinators, TransformedSortGreaterTakeFilterToVector) {
  std::vector<int> v{1, 2, 3};
  auto subtract_from_10 = [](int i) { return 10 - i; };
  auto odd_numbers = [](int i) { return i % 2 == 1; };
  auto result = Apply(v,                                //
                      TransformEach(subtract_from_10),  //
                      ToVector(),                       //
                      Sort(std::greater<>()),           //
                      Filter(odd_numbers),              //
                      Take(2),                          //
                      ToVector()                        //

  );
  EXPECT_THAT(result, ElementsAre(9, 7));
}
TEST(TestOutputCombinators,
     TransformedSortGreaterTakeFilterToVectorComposedBegin) {
  std::vector<int> v{1, 2, 3};
  auto subtract_from_10 = [](int i) { return 10 - i; };
  auto odd_numbers = [](int i) { return i % 2 == 1; };
  auto result = Apply(v,                                        //
                      Compose(TransformEach(subtract_from_10),  //
                              ToVector(),                       //
                              Sort(std::greater<>())),          //
                      Filter(odd_numbers),                      //
                      Take(2),                                  //
                      ToVector()                                //
  );
  EXPECT_THAT(result, ElementsAre(9, 7));
}

TEST(TestOutputCombinators,
     TransformedSortGreaterTakeFilterToVectorComposedEnd) {
  std::vector<int> v{1, 2, 3};
  auto subtract_from_10 = [](int i) { return 10 - i; };
  auto odd_numbers = [](int i) { return i % 2 == 1; };
  auto result = Apply(v,                                //
                      TransformEach(subtract_from_10),  //
                      ToVector(),                       //
                      Sort(std::greater<>()),           //
                      Compose(Filter(odd_numbers),      //
                              Take(2),                  //
                              ToVector())               //

  );
  EXPECT_THAT(result, ElementsAre(9, 7));
}

TEST(TestOutputCombinators,
     TransformedSortGreaterTakeFilterToVectorTakeComposedEnd) {
  std::vector<int> v{1, 2, 3};
  auto subtract_from_10 = [](int i) { return 10 - i; };
  auto odd_numbers = [](int i) { return i % 2 == 1; };
  auto result = Apply(v,                                //
                      TransformEach(subtract_from_10),  //
                      ToVector(),                       //
                      Sort(std::greater<>()),           //
                      Compose(Filter(odd_numbers),      //
                              Take(2),                  //
                              ToVector()),              //
                      Take(1),                          //
                      ToVector()                        //

  );
  EXPECT_THAT(result, ElementsAre(9));
}

TEST(TestOutputCombinators,
     TransformedSortGreaterTakeFilterToVectorComposedBeginEnd) {
  std::vector<int> v{1, 2, 3};
  auto subtract_from_10 = [](int i) { return 10 - i; };
  auto odd_numbers = [](int i) { return i % 2 == 1; };
  auto result = Apply(v,                                        //
                      Compose(TransformEach(subtract_from_10),  //
                              ToVector(),                       //
                              Sort(std::greater<>())),          //
                      Compose(Filter(odd_numbers),              //
                              Take(2),                          //
                              ToVector())                       //
  );
  EXPECT_THAT(result, ElementsAre(9, 7));
}

auto TransformVectorSort() {
  auto subtract_from_10 = [](int i) { return 10 - i; };
  return Compose(                       //
      TransformEach(subtract_from_10),  //
      ToVector(),                       //
      Sort(std::greater<>())            //
  );
}

TEST(TestOutputCombinators,
     TransformedSortGreaterTakeFilterToVectorComposedSeparate) {
  std::vector<int> v{1, 2, 3};
  auto odd_numbers = [](int i) { return i % 2 == 1; };
  auto result = Apply(v,                      //
                      TransformVectorSort(),  //
                      Filter(odd_numbers),    //
                      Take(2),                //
                      ToVector()              //
  );
  EXPECT_THAT(result, ElementsAre(9, 7));
}

struct StructWithInt {
  int i;
};

TEST(TestOutputCombinators, TransformMemberPointer) {
  std::vector<StructWithInt> v{{1}, {2}, {3}};
  auto result = Apply(v,                                 //
                      TransformEach(&StructWithInt::i),  //
                      ToVector()                         //
  );
  EXPECT_THAT(result, ElementsAre(1, 2, 3));
}

struct ImmovableInt {
  ImmovableInt(const ImmovableInt&) = delete;
  ImmovableInt& operator=(const ImmovableInt&) = delete;
  ImmovableInt(ImmovableInt&&) = delete;
  ImmovableInt& operator=(ImmovableInt&&) = delete;

  explicit ImmovableInt(int value) : value(value) {}

  int value = 0;
};

struct MoveOnlyInt {
  MoveOnlyInt(const MoveOnlyInt&) = delete;
  MoveOnlyInt& operator=(const MoveOnlyInt&) = delete;

  MoveOnlyInt(MoveOnlyInt&& other) : value(other.value) { other.value = 0; }
  MoveOnlyInt& operator=(MoveOnlyInt&& other) {
    value = other.value;
    other.value = 0;
    return *this;
  }

  explicit MoveOnlyInt(int value) : value(value) {}
  int value = 0;
};

TEST(TestOutputCombinators, ImmovableType) {
  std::list<ImmovableInt> list;
  list.emplace_back(1);
  list.emplace_back(2);
  list.emplace_back(3);

  auto evens = [](auto& i) { return i.value % 2 == 0; };
  auto result = Apply(list,                                 //
                      Filter(evens),                        //
                      TransformEach(&ImmovableInt::value),  //
                      ToVector()                            //
  );

  EXPECT_THAT(result, ElementsAre(2));
}

TEST(TestOutputCombinators, ImmovableTypeSortRef) {
  std::list<ImmovableInt> list;
  list.emplace_back(1);
  list.emplace_back(2);
  list.emplace_back(3);

  auto odds = [](auto& i) { return i.value % 2 == 1; };
  auto greater = [](auto& a, auto& b) { return a.value > b.value; };
  auto result = Apply(list,                                 //
                      Filter(odds),                         //
                      RefEach(),                            //
                      ToVector(),                           //
                      Sort(greater),                        //
                      TransformEach(&ImmovableInt::value),  //
                      ToVector()                            //
  );

  EXPECT_THAT(result, ElementsAre(3, 1));
}

TEST(TestOutputCombinators, MoveOnlyType) {
  std::vector<MoveOnlyInt> v;
  v.emplace_back(1);
  v.emplace_back(2);
  v.emplace_back(3);

  auto odds = [](auto& i) { return i.value % 2 == 1; };
  auto result = Apply(v,                                   //
                      Filter(odds),                        //
                      TransformEach(&MoveOnlyInt::value),  //
                      ToVector()                           //
  );

  EXPECT_THAT(result, ElementsAre(1, 3));
}
TEST(TestOutputCombinators, MoveOnlyTypeTransform) {
  std::vector<int> v{1, 2, 3};

  auto to_move_only = [](int i) { return MoveOnlyInt{i}; };

  auto odds = [](auto& i) { return i.value % 2 == 1; };
  auto result = Apply(v,                                   //
                      TransformEach(to_move_only),         //
                      Filter(odds),                        //
                      TransformEach(&MoveOnlyInt::value),  //
                      ToVector()                           //
  );

  EXPECT_THAT(result, ElementsAre(1, 3));
}

TEST(TestOutputCombinators, MoveOnlyTypeSort) {
  std::vector<MoveOnlyInt> v;
  v.emplace_back(1);
  v.emplace_back(2);
  v.emplace_back(3);

  auto odds = [](auto& i) { return i.value % 2 == 1; };
  auto greater = [](auto& a, auto& b) { return a.value > b.value; };
  auto result = Apply(std::move(v),                        //
                      Sort(greater),                       //
                      Filter(odds),                        //
                      TransformEach(&MoveOnlyInt::value),  //
                      ToVector()                           //
  );

  EXPECT_THAT(result, ElementsAre(3, 1));
}

// Original example from go/monadic-statusor
//
// absl::StatusOr<ExpenseReport> ItemizeExpenses(const Image& receipt) {
//   return StraightenAndUncrease(receipt)
//       .and_then([](Image img) { return CropToExpenseItems(img); })
//       .and_then([](Image img) { return SplitItems(img); })
//       .and_then([](std::vector<Image> items) {
//         return ParseDescriptionsAndCost(items);
//       })
//       .transform([](std::vector<ReceiptItem> parsed_items) {
//         return ExpensesToUsd(parsed_items);
//       })
//       .transform([](std::vector<ReceiptItem> expenses) {
//         return BuildExpenseReport(expenses);
//       });
// }

// This would be the equivalent code with this library
// Empty type and function implementations to verify type check works
struct Image {};
struct ReceiptItem {};
struct ExpenseReport {};
struct ExpenseReportBuilder {
  void Add(ReceiptItem item) {}
  ExpenseReport Build() && { return ExpenseReport{}; }
};

absl::StatusOr<Image> StraightenAndUncrease(const Image& image) {
  return {image};
}

absl::StatusOr<Image> CropToExpenseItems(Image image) {
  return absl::StatusOr<Image>(image);
}

absl::StatusOr<std::vector<Image>> SplitItems(Image) {
  return absl::StatusOr<std::vector<Image>>();
}

absl::StatusOr<ReceiptItem> ParseDescriptionAndCost(const Image&) {
  return absl::StatusOr<ReceiptItem>();
}

ReceiptItem ExpenseToUsd(const ReceiptItem& receipt) { return receipt; }

absl::StatusOr<ExpenseReport> ItemizeExpenses(const Image& receipt) {
  return Apply(                                //
      receipt,                                 //
      Transform(StraightenAndUncrease),        //
      AndThen(),                               //
      Transform(CropToExpenseItems),           //
      AndThen(),                               //
      Transform(SplitItems),                   //
      AndThen(),                               //
      TransformEach(ParseDescriptionAndCost),  //
      AndThenEach(),                           //
      TransformEach(ExpenseToUsd),             //
      AccumulateInPlace(ExpenseReportBuilder(),
                        [](ExpenseReportBuilder& builder,
                           const ReceiptItem& item) { builder.Add(item); }  //
                        ),                                                  //
      Transform([](ExpenseReportBuilder&& builder) {
        return std::move(builder).Build();
      })  //
  );
}

TEST(TestOutputCombinators, MonadChainExample) {
  absl::StatusOr<ExpenseReport> expense_report = ItemizeExpenses(Image{});
  (void)expense_report;

  /*
    absl::StatusOr<ExpenseReport> ItemizeExpenses(const Image& receipt) {
      return StraightenAndUncrease(receipt)
          .and_then([](Image img) { return CropToExpenseItems(img); })
          .and_then([](Image img) { return SplitItems(img); })
          .and_then([](std::vector<Image> items) {
            return ParseDescriptionsAndCost(items);
          })
          .transform([](std::vector<ReceiptItem> parsed_items) {
            return ExpensesToUsd(parsed_items);
          })
          .transform([](std::vector<ReceiptItem> expenses) {
            return BuildExpenseReport(expenses);
          });
    }
    */
}

TEST(TestOutputCombinators, MoveOnlyTypeRefSortMove) {
  std::vector<MoveOnlyInt> v;
  v.emplace_back(1);
  v.emplace_back(2);
  v.emplace_back(3);

  auto odds = [](auto& i) { return i.value % 2 == 1; };
  auto greater = [](auto& a, auto& b) { return a.value > b.value; };
  auto result = Apply(v,              //
                      Filter(odds),   //
                      RefEach(),      //
                      ToVector(),     //
                      Sort(greater),  //
                      MoveEach(),     //
                      ToVector()      //
  );
  EXPECT_THAT(result, ElementsAre(Field(&MoveOnlyInt::value, 3),
                                  Field(&MoveOnlyInt::value, 1)));
  EXPECT_THAT(v, ElementsAre(Field(&MoveOnlyInt::value, 0),
                             Field(&MoveOnlyInt::value, 2),
                             Field(&MoveOnlyInt::value, 0)));
}

TEST(TestOutputCombinators, SameAsHandWritten) {
  std::vector<int> v(100);
  std::iota(v.begin(), v.end(), 0);
  std::vector<int> expected;
  for (auto i : v) {
    if (i % 2 != 0 && i % 3 != 0 && i % 5 != 0 && i % 7 != 0 && i % 11 != 0) {
      expected.push_back(i);
    }
  }
  auto not_divisible_by_2 = [](int i) -> bool { return i % 2 != 0; };
  auto not_divisible_by_3 = [](int i) -> bool { return i % 3 != 0; };
  auto not_divisible_by_5 = [](int i) -> bool { return i % 5 != 0; };
  auto not_divisible_by_7 = [](int i) -> bool { return i % 7 != 0; };
  auto not_divisible_by_11 = [](int i) -> bool { return i % 11 != 0; };
  std::vector<int> result = Apply(v,                            //
                                  Filter(not_divisible_by_2),   //
                                  Filter(not_divisible_by_3),   //
                                  Filter(not_divisible_by_5),   //
                                  Filter(not_divisible_by_7),   //
                                  Filter(not_divisible_by_11),  //
                                  ToVector()                    //
  );
  EXPECT_THAT(result, ElementsAreArray(expected));
}

void BM_CombinatorFilter(benchmark::State& state) {
  std::vector<int> v(state.range(0));
  std::iota(v.begin(), v.end(), 0);
  auto not_divisible_by_2 = [](int i) -> bool { return i % 2 != 0; };
  auto not_divisible_by_3 = [](int i) -> bool { return i % 3 != 0; };
  auto not_divisible_by_5 = [](int i) -> bool { return i % 5 != 0; };
  auto not_divisible_by_7 = [](int i) -> bool { return i % 7 != 0; };
  auto not_divisible_by_11 = [](int i) -> bool { return i % 11 != 0; };
  for (auto s : state) {
    std::vector<int> result = Apply(v,                            //
                                    Filter(not_divisible_by_2),   //
                                    Filter(not_divisible_by_3),   //
                                    Filter(not_divisible_by_5),   //
                                    Filter(not_divisible_by_7),   //
                                    Filter(not_divisible_by_11),  //
                                    ToVector()                    //
    );
    benchmark::DoNotOptimize(result);
  }
}

void BM_CombinatorFilterTake(benchmark::State& state) {
  std::vector<int> v(state.range(0));
  std::iota(v.begin(), v.end(), 0);
  auto not_divisible_by_2 = [](int i) -> bool { return i % 2 != 0; };
  auto not_divisible_by_3 = [](int i) -> bool { return i % 3 != 0; };
  auto not_divisible_by_5 = [](int i) -> bool { return i % 5 != 0; };
  auto not_divisible_by_7 = [](int i) -> bool { return i % 7 != 0; };
  auto not_divisible_by_11 = [](int i) -> bool { return i % 11 != 0; };
  for (auto s : state) {
    std::vector<int> result = Apply(v,                            //
                                    Filter(not_divisible_by_2),   //
                                    Filter(not_divisible_by_3),   //
                                    Filter(not_divisible_by_5),   //
                                    Filter(not_divisible_by_7),   //
                                    Filter(not_divisible_by_11),  //
                                    Take(v.size()),               //
                                    ToVector()                    //
    );
    benchmark::DoNotOptimize(result);
  }
}

void BM_HandWrittenFilter(benchmark::State& state) {
  std::vector<int> v(state.range(0));
  std::iota(v.begin(), v.end(), 0);
  for (auto s : state) {
    std::vector<int> result;
    for (auto i : v) {
      if (i % 2 != 0 && i % 3 != 0 && i % 5 != 0 && i % 7 != 0 && i % 11 != 0) {
        result.push_back(i);
      }
    }
    benchmark::DoNotOptimize(result);
  }
}

void BM_CombinatorSortValues(benchmark::State& state) {
  std::vector<int> v(state.range(0));
  std::iota(v.begin(), v.end(), 0);
  for (auto s : state) {
    std::vector<int> result = Apply(v,                       //
                                    Sort(std::greater<>()),  //
                                    ToVector()               //
    );
    benchmark::DoNotOptimize(result);
  }
}

void BM_CombinatorSort(benchmark::State& state) {
  std::vector<int> v(state.range(0));
  std::iota(v.begin(), v.end(), 0);
  for (auto s : state) {
    std::vector<int> result = Apply(v,                      //
                                    Sort(std::greater<>())  //
    );
    benchmark::DoNotOptimize(result);
  }
}

void BM_StdSort(benchmark::State& state) {
  std::vector<int> v(state.range(0));
  std::iota(v.begin(), v.end(), 0);
  for (auto s : state) {
    auto result = v;
    std::sort(result.begin(), result.end(), std::greater<>());
    benchmark::DoNotOptimize(result);
  }
}

BENCHMARK(BM_CombinatorFilter)->Range(1, 1048576);
BENCHMARK(BM_CombinatorFilterTake)->Range(1, 1048576);
BENCHMARK(BM_HandWrittenFilter)->Range(1, 1048576);

BENCHMARK(BM_CombinatorSortValues)->Range(1, 1048576);
BENCHMARK(BM_CombinatorSort)->Range(1, 1048576);
BENCHMARK(BM_StdSort)->Range(1, 1048576);

}  // namespace

}  // namespace htls::range
