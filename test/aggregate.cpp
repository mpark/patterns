// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#include <mpark/patterns.hpp>

#include <string>
#include <utility>
#include <vector>

#include <gtest/gtest.h>

struct Aggregate {
  double d;
  std::string s;
  std::vector<int> v;
};

TEST(Aggregate, LRef) {
  Aggregate aggregate{4.2, "hello", {101, 202}};

  using namespace mpark::patterns;
  double &result = match(aggregate)(
      pattern(prod(arg, "world", _)) = [](double &d) -> double & { return d; },
      pattern(prod(arg, "hello", _)) = [](double &d) -> double & { return d; });

  EXPECT_EQ(aggregate.d, result);
  EXPECT_EQ(&aggregate.d, &result);
}

TEST(Aggregate, ConstLRef) {
  const Aggregate aggregate{4.2, "hello", {101, 202}};

  using namespace mpark::patterns;
  const double &result = match(aggregate)(
      pattern(prod(arg, "world", _)) = [](const double &d) -> const double & {
        return d;
      },
      pattern(prod(arg, "hello", _)) = [](const double &d) -> const double & {
        return d;
      });

  EXPECT_EQ(aggregate.d, result);
  EXPECT_EQ(&aggregate.d, &result);
}

TEST(Aggregate, RRef) {
  auto aggregate = [] { return Aggregate{4.2, "hello", {101, 202}}; };

  using namespace mpark::patterns;
  double result = match(aggregate())(
      pattern(prod(arg, "world", _)) = [](double &&d) -> double {
        return std::move(d);
      },
      pattern(prod(arg, "hello", _)) = [](double &&d) -> double {
        return std::move(d);
      });

  EXPECT_EQ(4.2, result);
}

TEST(Aggregate, Empty) {
  struct {} empty;

  using namespace mpark::patterns;
  match(empty)(pattern(prod()) = [] {});
}

TEST(Aggregate, OneChar) {
  struct { char x; } one{'x'};

  using namespace mpark::patterns;
  int result = match(one)(pattern(prod('a')) = [] { return false; },
                          pattern(prod('x')) = [] { return true; });

  EXPECT_TRUE(result);
}

TEST(Aggregate, OneInt) {
  struct { int x; } one{101};

  using namespace mpark::patterns;
  int result = match(one)(pattern(prod(101)) = [] { return true; },
                          pattern(prod(1)) = [] { return false; });

  EXPECT_TRUE(result);
}

TEST(Aggregate, TwoChars) {
  struct { char x; char y; } two{'x', 'y'};

  using namespace mpark::patterns;
  bool result = match(two)(pattern(prod('a', 'b')) = [] { return false; },
                           pattern(prod('x', 'y')) = [] { return true; });

  EXPECT_TRUE(result);
}

TEST(Aggregate, TwoInts) {
  struct { int x; int y; } two{101, 202};

  using namespace mpark::patterns;
  bool result = match(two)(pattern(prod(1, 2)) = [] { return false; },
                           pattern(prod(101, 202)) = [] { return true; });

  EXPECT_TRUE(result);
}

TEST(Aggregate, ThreeChars) {
  struct { char x; char y; char z; } three{'x', 'y', 'z'};

  using namespace mpark::patterns;
  bool result =
      match(three)(pattern(prod('x', 'y', 'z')) = [] { return true; },
                   pattern(prod('a', 'b', 'c')) = [] { return false; });

  EXPECT_TRUE(result);
}

TEST(Aggregate, FourChars) {
  struct { char a; char b; char c; char d; } four{'a', 'b', 'c', 'd'};

  using namespace mpark::patterns;
  bool result =
      match(four)(pattern(prod('p', 'q', 'r', 's')) = [] { return false; },
                  pattern(prod('a', 'b', 'c', 'd')) = [] { return true; });

  EXPECT_TRUE(result);
}
