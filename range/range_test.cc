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
#include <set>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include "testing/base/public/benchmark.h"
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <absl/types/span.h>
#include "range/range_combinators.h"

namespace htls::range {

namespace {

using ::testing::ElementsAre;
using ::testing::ElementsAreArray;
using ::testing::Field;

// To aid with testing things like stopping iteration early
// this combinator just does an identity transform and increments a counter

auto Counter(int& count) {
  return Transform([&count](auto&& value) mutable -> decltype(auto) {
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

inline auto ToSet() {
  return ToCollection<std::set>([](auto& set, auto&& value) {
    set.insert(std::forward<decltype(value)>(value));
  });
}

TEST(TestOutputCombinators, ToSet) {
  std::vector<int> v{1, 3, 2, 1};
  auto result = Apply(v, ToSet());
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
  auto result = Apply(v,            //
                      AddressOf(),  //
                      ToVector()    //
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

TEST(TestOutputCombinators, FrontLValue) {
  std::vector<int> v{1, 2, 3};
  auto result = Apply(v,       //
                      Front()  //
  );

  EXPECT_THAT(result, 1);

  static_assert(std::is_same_v<decltype(Apply(v,       //
                                              Front()  //
                                              )),
                               int>);
}

TEST(TestOutputCombinators, FrontRef) {
  std::vector<int> v{1, 2, 3};
  auto& result = Apply(v,  //
                       Ref(),
                       Front()  //
  );

  EXPECT_THAT(result, 1);
  EXPECT_THAT(&result, &v[0]);

  static_assert(std::is_same_v<decltype(Apply(v,  //
                                              Ref(),
                                              Front()  //
                                              )),
                               int&>);
}

TEST(TestOutputCombinators, FrontRValue) {
  std::vector<int> v{1, 2, 3};
  auto add_one = [](int i) { return i + 1; };
  auto result = Apply(v,                   //
                      Transform(add_one),  //
                      Front()              //
  );

  EXPECT_THAT(result, 2);

  static_assert(std::is_same_v<decltype(Apply(v,                   //
                                              Transform(add_one),  //
                                              Front()              //
                                              )),
                               int>);
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
                      Ref(),      //
                      Take(2),    //
                      ToVector()  //
  );
  static_assert(std::is_same_v<std::decay_t<decltype(result)>::value_type,
                               std::reference_wrapper<int>>);
  EXPECT_THAT(result, ElementsAre(1, 2));
}

TEST(TestOutputCombinators, TakeToMoveVector) {
  std::vector<std::string> v{"Hello", "World", "John"};
  auto result = Apply(v,          //
                      Take(2),    //
                      Ref(),      //
                      Move(),     //
                      ToVector()  //
  );
  static_assert(
      std::is_same_v<std::decay_t<decltype(result)>::value_type, std::string>);
  EXPECT_THAT(result, ElementsAre("Hello", "World"));
  EXPECT_THAT(v, ElementsAre("", "", "John"));
}

TEST(TestOutputCombinators, TakeToMoveLRefVector) {
  std::vector<std::string> v{"Hello", "World", "John"};
  auto result = Apply(v,          //
                      Take(2),    //
                      Ref(),      //
                      Move(),     //
                      LRef(),     //
                      ToVector()  //
  );
  static_assert(
      std::is_same_v<std::decay_t<decltype(result)>::value_type, std::string>);
  EXPECT_THAT(result, ElementsAre("Hello", "World"));
  EXPECT_THAT(v, ElementsAre("Hello", "World", "John"));
}

TEST(TestOutputCombinators, TransformToVector) {
  std::vector<int> v{1, 2, 3};
  auto to_double = [](int i) { return static_cast<double>(i) + 0.5; };
  auto result = Apply(v,                     //
                      Transform(to_double),  //
                      ToVector()             //
  );
  EXPECT_THAT(result, ElementsAre(1.5, 2.5, 3.5));
}

TEST(TestOutputCombinators, TransformComplete) {
  auto to_double = [](int i) { return static_cast<double>(i) + 0.5; };
  auto result = Apply(1,                            //
                      TransformComplete(to_double)  //
  );

  static_assert(std::is_same_v<decltype(Apply(1,                            //
                                              TransformComplete(to_double)  //
                                              )),
                               double>);
  EXPECT_THAT(result, 1.5);
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
  auto result = Apply(v,                     //
                      Filter(even_numbers),  //
                      Transform(to_double),  //
                      ToVector()             //
  );
  static_assert(
      std::is_same_v<std::decay_t<decltype(result)>, std::vector<double>>);
  EXPECT_THAT(result, ElementsAre(2.5));
}

TEST(TestOutputCombinators, TransformFilterToVector) {
  std::vector<int> v{1, 2, 3};
  auto to_double = [](int i) { return static_cast<double>(i) + 0.5; };
  auto greater_than_2 = [](double d) { return d > 2.0; };
  auto result = Apply(v,                       //
                      Transform(to_double),    //
                      Filter(greater_than_2),  //
                      ToVector()               //
  );
  EXPECT_THAT(result, ElementsAre(2.5, 3.5));
}

TEST(TestOutputCombinators, TransformFilterTakeToVector) {
  std::vector<int> v{1, 2, 3};
  auto to_double = [](int i) { return static_cast<double>(i) + 0.5; };
  auto greater_than_2 = [](double d) { return d > 2.0; };
  auto result = Apply(v,                       //
                      Transform(to_double),    //
                      Filter(greater_than_2),  //
                      Take(1),                 //
                      ToVector()               //
  );
  EXPECT_THAT(result, ElementsAre(2.5));
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

TEST(TestOutputCombinators, SortFilterDuplicates) {
  std::vector<int> v{3, 2, 3, 1};
  auto result = Apply(v, Sort(), FilterDuplicates(), ToVector());
  EXPECT_THAT(result, ElementsAre(1, 2, 3));
}

TEST(TestOutputCombinators, SortTransformFilterDuplicates) {
  std::vector<int> v{3, 2, 3, 1};
  auto result = Apply(v,                                       //
                      Sort(),                                  //
                      Transform([](int i) { return i + 1; }),  //
                      FilterDuplicates(),                      //
                      ToVector()                               //
  );
  EXPECT_THAT(result, ElementsAre(2, 3, 4));
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
  auto result = Apply(v,                            //
                      Transform(subtract_from_10),  //
                      ToVector(),                   // s
                      Sort(std::greater<>()),       //
                      ToVector()                    //

  );

  EXPECT_THAT(result, ElementsAre(9, 8, 7));
}
TEST(TestOutputCombinators, TransformedSortGreaterFilterToVector) {
  std::vector<int> v{1, 2, 3};
  auto subtract_from_10 = [](int i) { return 10 - i; };
  auto odd_numbers = [](int i) { return i % 2 == 1; };
  auto result = Apply(v,                            //
                      Transform(subtract_from_10),  //
                      ToVector(),                   //
                      Sort(std::greater<>()),       //
                      Filter(odd_numbers),          //
                      ToVector()                    //

  );
  EXPECT_THAT(result, ElementsAre(9, 7));
}

TEST(TestOutputCombinators, TransformedSortGreaterTakeFilterToVector) {
  std::vector<int> v{1, 2, 3};
  auto subtract_from_10 = [](int i) { return 10 - i; };
  auto odd_numbers = [](int i) { return i % 2 == 1; };
  auto result = Apply(v,                            //
                      Transform(subtract_from_10),  //
                      ToVector(),                   //
                      Sort(std::greater<>()),       //
                      Filter(odd_numbers),          //
                      Take(2),                      //
                      ToVector()                    //

  );
  EXPECT_THAT(result, ElementsAre(9, 7));
}
TEST(TestOutputCombinators,
     TransformedSortGreaterTakeFilterToVectorComposedBegin) {
  std::vector<int> v{1, 2, 3};
  auto subtract_from_10 = [](int i) { return 10 - i; };
  auto odd_numbers = [](int i) { return i % 2 == 1; };
  auto result = Apply(v,                                    //
                      Compose(Transform(subtract_from_10),  //
                              ToVector(),                   //
                              Sort(std::greater<>())),      //
                      Filter(odd_numbers),                  //
                      Take(2),                              //
                      ToVector()                            //
  );
  EXPECT_THAT(result, ElementsAre(9, 7));
}

TEST(TestOutputCombinators,
     TransformedSortGreaterTakeFilterToVectorComposedEnd) {
  std::vector<int> v{1, 2, 3};
  auto subtract_from_10 = [](int i) { return 10 - i; };
  auto odd_numbers = [](int i) { return i % 2 == 1; };
  auto result = Apply(v,                            //
                      Transform(subtract_from_10),  //
                      ToVector(),                   //
                      Sort(std::greater<>()),       //
                      Compose(Filter(odd_numbers),  //
                              Take(2),              //
                              ToVector())           //

  );
  EXPECT_THAT(result, ElementsAre(9, 7));
}

TEST(TestOutputCombinators,
     TransformedSortGreaterTakeFilterToVectorTakeComposedEnd) {
  std::vector<int> v{1, 2, 3};
  auto subtract_from_10 = [](int i) { return 10 - i; };
  auto odd_numbers = [](int i) { return i % 2 == 1; };
  auto result = Apply(v,                            //
                      Transform(subtract_from_10),  //
                      ToVector(),                   //
                      Sort(std::greater<>()),       //
                      Compose(Filter(odd_numbers),  //
                              Take(2),              //
                              ToVector()),          //
                      Take(1),                      //
                      ToVector()                    //

  );
  EXPECT_THAT(result, ElementsAre(9));
}

TEST(TestOutputCombinators,
     TransformedSortGreaterTakeFilterToVectorComposedBeginEnd) {
  std::vector<int> v{1, 2, 3};
  auto subtract_from_10 = [](int i) { return 10 - i; };
  auto odd_numbers = [](int i) { return i % 2 == 1; };
  auto result = Apply(v,                                    //
                      Compose(Transform(subtract_from_10),  //
                              ToVector(),                   //
                              Sort(std::greater<>())),      //
                      Compose(Filter(odd_numbers),          //
                              Take(2),                      //
                              ToVector())                   //
  );
  EXPECT_THAT(result, ElementsAre(9, 7));
}

auto TransformVectorSort() {
  auto subtract_from_10 = [](int i) { return 10 - i; };
  return Compose(                   //
      Transform(subtract_from_10),  //
      ToVector(),                   //
      Sort(std::greater<>())        //
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
  auto result = Apply(v,                             //
                      Transform(&StructWithInt::i),  //
                      ToVector()                     //
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
  auto result = Apply(list,                             //
                      Filter(evens),                    //
                      Transform(&ImmovableInt::value),  //
                      ToVector()                        //
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
  auto result = Apply(list,                             //
                      Filter(odds),                     //
                      Ref(),                            //
                      ToVector(),                       //
                      Sort(greater),                    //
                      Transform(&ImmovableInt::value),  //
                      ToVector()                        //
  );

  EXPECT_THAT(result, ElementsAre(3, 1));
}

TEST(TestOutputCombinators, MoveOnlyType) {
  std::vector<MoveOnlyInt> v;
  v.emplace_back(1);
  v.emplace_back(2);
  v.emplace_back(3);

  auto odds = [](auto& i) { return i.value % 2 == 1; };
  auto result = Apply(v,                               //
                      Filter(odds),                    //
                      Transform(&MoveOnlyInt::value),  //
                      ToVector()                       //
  );

  EXPECT_THAT(result, ElementsAre(1, 3));
}
TEST(TestOutputCombinators, MoveOnlyTypeTransform) {
  std::vector<int> v{1, 2, 3};

  auto to_move_only = [](int i) { return MoveOnlyInt{i}; };

  auto odds = [](auto& i) { return i.value % 2 == 1; };
  auto result = Apply(v,                               //
                      Transform(to_move_only),         //
                      Filter(odds),                    //
                      Transform(&MoveOnlyInt::value),  //
                      ToVector()                       //
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
  auto result = Apply(std::move(v),                    //
                      Sort(greater),                   //
                      Filter(odds),                    //
                      Transform(&MoveOnlyInt::value),  //
                      ToVector()                       //
  );

  EXPECT_THAT(result, ElementsAre(3, 1));
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
                      Ref(),          //
                      ToVector(),     //
                      Sort(greater),  //
                      Move(),         //
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
