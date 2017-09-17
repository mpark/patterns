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

TEST(As, Variant_Unary) {
  std::variant<int, std::string> v = 42;
  using namespace mpark::patterns;
  match(v)(
      pattern(as<int>(arg)) = [](const auto &n) { EXPECT_EQ(42, n); },
      pattern(as<std::string>(arg)) = [](const auto &) { EXPECT_FALSE(true); });
}

TEST(As, Variant_Binary) {
  using str = std::string;

  std::vector<std::variant<int, str>> vs = {101, "hello"};
  std::vector<std::variant<int, str>> ws = {202, "world"};
  for (const auto &v : vs) {
    for (const auto &w : ws) {
      using namespace mpark::patterns;
      match(v, w)(
          pattern(as<int>(arg), as<int>(arg)) = [](auto x, auto y) {
            EXPECT_EQ(101, x);
            EXPECT_EQ(202, y);
          },
          pattern(as<int>(arg), as<str>(arg)) = [](auto x, auto y) {
            EXPECT_EQ(101, x);
            EXPECT_EQ("world", y);
          },
          pattern(as<str>(arg), as<int>(arg)) = [](auto x, auto y) {
            EXPECT_EQ("hello", x);
            EXPECT_EQ(202, y);
          },
          pattern(as<str>(arg), as<str>(arg)) = [](auto x, auto y) {
            EXPECT_EQ("hello", x);
            EXPECT_EQ("world", y);
          });
    }
  }
}

struct Shape { virtual ~Shape() = default; };

struct Circle : Shape {};
struct Square : Shape {};
struct Triangle : Shape {};

TEST(As, Inheritance_Reference) {
  Circle circle;
  const Shape& shape = circle;
  using namespace mpark::patterns;
  int result = match(shape)(pattern(as<Circle>(_)) = [] { return 1; },
                            pattern(as<Square>(_)) = [] { return 2; },
                            pattern(as<Triangle>(_)) = [] { return 3; });

  EXPECT_EQ(1, result);
}

TEST(As, Inheritance_Pointer) {
  std::unique_ptr<Shape> shape = std::make_unique<Circle>();
  using namespace mpark::patterns;
  match(shape)(pattern(some(as<Circle>(_))) = [] { return 1; },
               pattern(some(as<Square>(_))) = [] { return 2; },
               pattern(some(as<Triangle>(_))) = [] { return 3; });
}
