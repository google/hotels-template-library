#include "haversack/haversack_utils.h"

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <absl/strings/str_format.h>
#include <absl/strings/str_split.h>
#include "haversack/haversack.h"

namespace hotels::haversack {
namespace {

using ::testing::UnorderedElementsAre;

struct CHaversack : Haversack<> {};
struct BHaversack : Haversack<> {};
void BFlow(const BHaversack&) {}
struct AHaversack : Haversack<Calls<BFlow>, Deps<CHaversack>> {};

TEST(Streamhaversack, StreamsStructure) {
  EXPECT_THAT(
      absl::StrSplit(absl::StrFormat("%v", StreamHaversack<AHaversack>{}),
                     '\n'),
      UnorderedElementsAre(
          "",
          R"("void" -> "hotels::haversack::(anonymous namespace)::AHaversack")",
          R"("hotels::haversack::(anonymous namespace)::AHaversack" -> "hotels::haversack::(anonymous namespace)::BFlow" -> "hotels::haversack::(anonymous namespace)::BHaversack")",
          R"("hotels::haversack::(anonymous namespace)::AHaversack" -> "hotels::haversack::(anonymous namespace)::CHaversack")"));
}

}  // namespace
}  // namespace hotels::haversack
