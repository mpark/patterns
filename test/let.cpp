// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#include <mpark/patterns.hpp>

#include <optional>

#include <gtest/gtest.h>

TEST(Let, If) {
  std::optional<int> expected = 42;
  std::optional<int> actual;

  using namespace mpark::patterns;
  if_let (pattern(some(arg)) = expected) = [&](auto x) { actual = x; };

  EXPECT_EQ(expected, actual);
}

TEST(Let, For) {
  std::vector<std::pair<int, int>> pairs = {{1, 0}, {1, 1}, {0, 0}, {1, 2}};

  std::vector<int> expected = {0, 1, 2};
  std::vector<int> actual;

  using namespace mpark::patterns;
  for_let (pattern(ds(1, arg)) = pairs) = [&](auto x) { actual.push_back(x); };

  EXPECT_EQ(expected, actual);
}

TEST(Let, ForBreak) {
  std::vector<std::pair<int, int>> pairs = {{1, 0}, {1, 1}, {0, 0}, {1, 2}};

  std::vector<int> expected = {0};
  std::vector<int> actual;

  using namespace mpark::patterns;
  for_let (pattern(ds(1, arg)) = pairs) = [&](auto x) {
    if (x == 1) { return Break; }
    actual.push_back(x);
    return Continue;
  };

  EXPECT_EQ(expected, actual);
}

TEST(Let, ForContinue) {
  std::vector<std::pair<int, int>> pairs = {{1, 0}, {1, 1}, {0, 0}, {1, 2}};

  std::vector<int> expected = {1, 2};
  std::vector<int> actual;

  using namespace mpark::patterns;
  for_let (pattern(ds(1, arg)) = pairs) = [&](auto x) {
    if (x == 0) { return Continue; }
    actual.push_back(x);
    return Continue;
  };

  EXPECT_EQ(expected, actual);
}
