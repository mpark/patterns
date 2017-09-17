// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#include <mpark/patterns.hpp>

#include <optional>

#include <gtest/gtest.h>

TEST(Optional, Pointer) {
  auto holds = [](int *p) {
    using namespace mpark::patterns;
    return match(p)(pattern(some(_)) = [] { return true; },
                    pattern(none) = [] { return false; });
  };

  int x = 42;
  EXPECT_TRUE(holds(&x));
  EXPECT_FALSE(holds(nullptr));
}

TEST(Optional, Std) {
  auto test_optional = [](const std::optional<std::optional<int>> &oo) {
    using namespace mpark::patterns;
    return match(oo)(pattern(some(some(_))) = [] { return 0; },
                     pattern(some(none)) = [] { return 1; },
                     pattern(none) = [] { return 2; });
  };

  std::optional<std::optional<int>> oo1(42);
  std::optional<std::optional<int>> oo2(std::optional<int>{});
  std::optional<std::optional<int>> oo3;

  EXPECT_EQ(0, test_optional(oo1));
  EXPECT_EQ(1, test_optional(oo2));
  EXPECT_EQ(2, test_optional(oo3));
}
