// Copyright 2023 Google LLC
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

#ifndef THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_RANGE_RANGE_H_
#define THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_RANGE_RANGE_H_

#include <algorithm>
#include <cstddef>
#include <functional>
#include <iterator>
#include <optional>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

#include "range/range_combinators.h"  // IWYU pragma: export
#include "range/range_core.h"  // IWYU pragma: export

// See go/output-combinators for the document on which this is based

// Alternative solution to composing ranges
// Based on
// https://www.fluentcpp.com/2019/02/12/the-terrible-problem-of-incrementing-a-smart-iterator/
// and
// https://www.fluentcpp.com/2017/11/28/output-iterator-adaptors-symmetry-range-adaptors/

// The idea is basically to do the transformations on the output side, instead
// of the input side. We gain several advantages:
// 1. No need for stash
// 2. Linear growth of stack-size instead of exponential
// 3. Combinators are simpler to read and write.

// The most important concept to understand when using this library is that of
// Processing Style. Processing Style refers to how a combinator processes its
// input and output. There are two kinds of styles, complete and incremental.

// Complete processing style processes a range as a whole. Think of sort. It
// sorts the whole range. Incremental processing style, processes the
// elements of the range. Think of filter which filters each element
// individually, independent of the other elements.

// A combinator can have different styles for input and output. For example,
// ToVector, takes incremental input, and outputs complete output (an entire
// vector). Another example of this is Accumulate, which takes incremental
// input, and outputs the accumulated value as a whole.

// A combinator chain that is run, must end with a combinator that outputs a
// complete value.

// When you chain combinators, the input style of the current combinator must
// match the output style of the previous combinator. However, the library
// automatically will convert complete output of one combinator to incremental
// input for the next combinator. That way you can have Filter, immediately
// after Sort.

// Example:
// std::vector<int> result = Apply(
//   input_range, //
//   Transform(&Foo::bar), //
//   Transform([](int i){return 2 * i;}), //
//   ToVector() //
// );

#endif  // THIRD_PARTY_HOTELS_TEMPLATE_LIBRARY_RANGE_RANGE_H_
