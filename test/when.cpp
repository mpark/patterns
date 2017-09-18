// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#include <mpark/patterns.hpp>

#include <array>

#include <gtest/gtest.h>

TEST(When, SubscriptOperator) {
  std::array<int, 3> xs = {{1, 2, 3}};

  using namespace mpark::patterns;
  int result = match(xs)(pattern(_).when(_[0] == 1) = [] { return 0; },
                         pattern(_).when(_[1] == 1) = [] { return 1; },
                         pattern(_).when(_[2] == 1) = [] { return 2; });

  EXPECT_EQ(0, result);
}

TEST(When, CallOperator_Guard) {
  std::array<int (*)(), 3> xs = {
      {[] { return 0; }, [] { return 1; }, [] { return 2; }}};

  using namespace mpark::patterns;
  int result = match(xs)(pattern(_).when(_[0]() == 1) = [] { return 0; },
                         pattern(_).when(_[1]() == 1) = [] { return 1; },
                         pattern(_).when(_[2]() == 1) = [] { return 2; });

  EXPECT_EQ(1, result);
}

TEST(When, CallOperator_Identifier) {
  std::array<int (*)(), 3> xs = {
      {[] { return 0; }, [] { return 1; }, [] { return 2; }}};

  using namespace mpark::patterns;
  IDENTIFIERS(x, y, z);
  int result = match(xs)(
      pattern(ds(x, y, z)).when(x() == 2) = [](auto...) { return 0; },
      pattern(ds(x, y, z)).when(y() == 2) = [](auto...) { return 1; },
      pattern(ds(x, y, z)).when(z() == 2) = [](auto...) { return 2; });

  EXPECT_EQ(2, result);
}

TEST(When, UnaryCallOperator_Identifier) {
  std::array<int (*)(int), 3> xs = {{[](int x) { return x; },
                                     [](int x) { return x + 1; },
                                     [](int x) { return x + 2; }}};

  using namespace mpark::patterns;
  IDENTIFIERS(x, y, z);
  int result = match(xs)(
      pattern(ds(x, y, z)).when(x(0) == 2) = [](auto...) { return 0; },
      pattern(ds(x, y, z)).when(y(0) == 2) = [](auto...) { return 1; },
      pattern(ds(x, y, z)).when(z(0) == 2) = [](auto...) { return 2; });

  EXPECT_EQ(2, result);
}

TEST(When, CallTwice_Identifier) {
  using IntToInt = int (*)(int);
  std::array<IntToInt (*)(int), 3> xs = {
      {[](int) -> IntToInt { return [](int x) { return x; }; },
       [](int) -> IntToInt { return [](int x) { return x + 1; }; },
       [](int) -> IntToInt { return [](int x) { return x + 2; }; }}};

  using namespace mpark::patterns;
  IDENTIFIERS(x, y, z);
  int result = match(xs)(
      pattern(ds(x, y, z)).when(x(0)(0) == 1) = [](auto...) { return 0; },
      pattern(ds(x, y, z)).when(y(0)(0) == 1) = [](auto...) { return 1; },
      pattern(ds(x, y, z)).when(z(0)(0) == 1) = [](auto...) { return 2; });

  EXPECT_EQ(1, result);
}
