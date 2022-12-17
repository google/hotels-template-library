#include "haversack/haversack_utils.h"

#include <gtest/gtest.h>
#include <absl/strings/str_format.h>
#include "haversack/haversack.h"

namespace hotels::haversack {
namespace {

struct CHaversack : Haversack<> {};
struct BHaversack : Haversack<> {};
void BFlow(const BHaversack&) {}
struct AHaversack : Haversack<Calls<BFlow>, Deps<CHaversack>> {};

TEST(Streamhaversack, StreamsStructure) {
  EXPECT_EQ(absl::StrFormat("%v", StreamHaversack<AHaversack>{}),
            R"("void" -> "hotels::haversack::(anonymous namespace)::AHaversack"
"hotels::haversack::(anonymous namespace)::AHaversack" -> "hotels::haversack::(anonymous namespace)::BFlow" -> "hotels::haversack::(anonymous namespace)::BHaversack"
"hotels::haversack::(anonymous namespace)::AHaversack" -> "hotels::haversack::(anonymous namespace)::CHaversack"
)");
}

}  // namespace
}  // namespace hotels::haversack
