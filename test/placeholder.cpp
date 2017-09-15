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

TEST(Placeholder, Simple) {
  std::tuple<int, int, int> t = {101, 202, 101};
  std::optional<int> o = 202;

  using namespace mpark::patterns;
  auto [x, y, z] = placeholders<3>();
  int actual = match(t, o)(
      pattern(prod(x, x, x), some(x)) = [](auto &&) {
        return 1;
      },
      pattern(prod(x, y, y), some(x)) = [](auto &&, auto &&) {
        return 2;
      },
      pattern(prod(x, y, x), some(y)) = [](auto &&, auto &&) {
        return 3;
      },
      pattern(prod(y, y, x), some(z)) = [](auto &&, auto &&, auto&&) {
        return 4;
      });

  EXPECT_EQ(3, actual);
}

TEST(Placeholder, Complex) {
  std::tuple<std::optional<int>, int> t = {{101}, 202};
  std::optional<int> o = 101;

  using namespace mpark::patterns;
  auto [x, y] = placeholders<2>();
  int actual = match(t, o)(
      pattern(prod(x(some(202)), y), x) = [](auto &&, auto&&) {
        return 1;
      },
      pattern(prod(x(some(101)), y), x) = [](auto &&, auto&&) {
        return 2;
      },
      pattern(prod(x(some(101)), y), y) = [](auto &&, auto&&) {
        return 3;
      });

  EXPECT_EQ(2, actual);
}
