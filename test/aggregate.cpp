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
      pattern(ds(arg, "world", _)) = [](double &d) -> double & { return d; },
      pattern(ds(arg, "hello", _)) = [](double &d) -> double & { return d; });

  EXPECT_EQ(aggregate.d, result);
  EXPECT_EQ(&aggregate.d, &result);
}

TEST(Aggregate, ConstLRef) {
  const Aggregate aggregate{4.2, "hello", {101, 202}};

  using namespace mpark::patterns;
  const double &result = match(aggregate)(
      pattern(ds(arg, "world", _)) = [](const double &d) -> const double & {
        return d;
      },
      pattern(ds(arg, "hello", _)) = [](const double &d) -> const double & {
        return d;
      });

  EXPECT_EQ(aggregate.d, result);
  EXPECT_EQ(&aggregate.d, &result);
}

TEST(Aggregate, RRef) {
  auto aggregate = [] { return Aggregate{4.2, "hello", {101, 202}}; };

  using namespace mpark::patterns;
  double result = match(aggregate())(
      pattern(ds(arg, "world", _)) = [](double &&d) -> double {
        return std::move(d);
      },
      pattern(ds(arg, "hello", _)) = [](double &&d) -> double {
        return std::move(d);
      });

  EXPECT_EQ(4.2, result);
}

TEST(Aggregate, Empty) {
  struct {} empty;

  using namespace mpark::patterns;
  match(empty)(pattern(ds()) = [] {});
}

TEST(Aggregate, OneChar) {
  struct { char x; } one{'x'};

  using namespace mpark::patterns;
  int result = match(one)(pattern(ds('a')) = [] { return false; },
                          pattern(ds('x')) = [] { return true; });

  EXPECT_TRUE(result);
}

TEST(Aggregate, OneInt) {
  struct { int x; } one{101};

  using namespace mpark::patterns;
  int result = match(one)(pattern(ds(101)) = [] { return true; },
                          pattern(ds(1)) = [] { return false; });

  EXPECT_TRUE(result);
}

TEST(Aggregate, TwoChars) {
  struct { char x; char y; } two{'x', 'y'};

  using namespace mpark::patterns;
  bool result = match(two)(pattern(ds('a', 'b')) = [] { return false; },
                           pattern(ds('x', 'y')) = [] { return true; });

  EXPECT_TRUE(result);
}

TEST(Aggregate, TwoInts) {
  struct { int x; int y; } two{101, 202};

  using namespace mpark::patterns;
  bool result = match(two)(pattern(ds(1, 2)) = [] { return false; },
                           pattern(ds(101, 202)) = [] { return true; });

  EXPECT_TRUE(result);
}

TEST(Aggregate, ThreeChars) {
  struct { char x; char y; char z; } three{'x', 'y', 'z'};

  using namespace mpark::patterns;
  bool result =
      match(three)(pattern(ds('x', 'y', 'z')) = [] { return true; },
                   pattern(ds('a', 'b', 'c')) = [] { return false; });

  EXPECT_TRUE(result);
}

TEST(Aggregate, FourChars) {
  struct { char a; char b; char c; char d; } four{'a', 'b', 'c', 'd'};

  using namespace mpark::patterns;
  bool result =
      match(four)(pattern(ds('p', 'q', 'r', 's')) = [] { return false; },
                  pattern(ds('a', 'b', 'c', 'd')) = [] { return true; });

  EXPECT_TRUE(result);
}
