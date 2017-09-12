// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#include <mpark/patterns.hpp>

#include <string>
#include <variant>
#include <vector>

#include <gtest/gtest.h>

TEST(Sum, Unary) {
  std::variant<int, std::string> v = 42;
  using namespace mpark::patterns;
  match(v)(
      pattern(sum<int>(arg)) = [](const auto &n) { EXPECT_EQ(42, n); },
      pattern(sum<std::string>(arg)) = [](const auto &) { EXPECT_FALSE(true); });
}

TEST(Sum, Binary) {
  using str = std::string;

  std::vector<std::variant<int, str>> vs = {101, "hello"};
  std::vector<std::variant<int, str>> ws = {202, "world"};
  for (const auto &v : vs) {
    for (const auto &w : ws) {
      using namespace mpark::patterns;
      match(v, w)(
          pattern(sum<int>(arg), sum<int>(arg)) = [](auto x, auto y) {
            EXPECT_EQ(101, x);
            EXPECT_EQ(202, y);
          },
          pattern(sum<int>(arg), sum<str>(arg)) = [](auto x, auto y) {
            EXPECT_EQ(101, x);
            EXPECT_EQ("world", y);
          },
          pattern(sum<str>(arg), sum<int>(arg)) = [](auto x, auto y) {
            EXPECT_EQ("hello", x);
            EXPECT_EQ(202, y);
          },
          pattern(sum<str>(arg), sum<str>(arg)) = [](auto x, auto y) {
            EXPECT_EQ("hello", x);
            EXPECT_EQ("world", y);
          });
    }
  }
}
