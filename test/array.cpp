// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <mpark/patterns.hpp>

#include <array>

#include <gtest/gtest.h>

TEST(Patterns, CArray) {
  std::array<int, 3> x = {{1, 2, 3}};

  using namespace mpark::patterns;
  int i = match(x)(pattern(prod(3, 2, 1)) = [] { return 0; },
                   pattern(prod(_, _, 3)) = [] { return 1; },
                   pattern(prod(1, 2, _)) = [] { return 2; });

  EXPECT_EQ(1, i);
}
