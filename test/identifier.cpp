// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#include <mpark/patterns.hpp>

#include <optional>
#include <tuple>

#include <gtest/gtest.h>

TEST(Identifier, Simple) {
  std::tuple<int, int, int> t = {101, 202, 101};
  std::optional<int> o = 202;

  using namespace mpark::patterns;
  IDENTIFIERS(x, y, z);
  int actual = match(t, o)(
      pattern(ds(x, x, x), some(x)) = [](auto &&) { return 1; },
      pattern(ds(x, y, x), some(y)) = [](auto &&, auto &&) { return 2; },
      pattern(_, _) = [] { return 3; });

  EXPECT_EQ(2, actual);
}

TEST(Identifier, Complex) {
  std::tuple<std::optional<int>, int> t = {{101}, 202};
  std::optional<int> o = 101;

  using namespace mpark::patterns;
  IDENTIFIERS(x, y);
  int actual = match(t, o)(
      pattern(ds(x(some(202)), y), x) = [](auto &&, auto &&) { return 1; },
      pattern(ds(x(some(101)), y), x) = [](auto &&, auto &&) { return 2; },
      pattern(ds(x(some(101)), y), y) = [](auto &&, auto &&) { return 3; });

  EXPECT_EQ(2, actual);
}

TEST(Identifier, Discards) {
  std::tuple<int, int, int> t = {101, 202, 101};
  std::optional<int> o = 202;

  using namespace mpark::patterns;
  IDENTIFIERS(x, _y, z);
  int actual = match(t, o)(
      pattern(ds(x , x , x ), some(x )) = [](auto &&) { return 1; },
      pattern(ds(x , _y, _y), some(x )) = [](auto &&) { return 2; },
      pattern(ds(x , _y, x ), some(_y)) = [](auto &&) { return 3; },
      pattern(ds(_y, _y, x ), some(z )) = [](auto &&, auto &&) { return 4; });

  EXPECT_EQ(3, actual);
}
