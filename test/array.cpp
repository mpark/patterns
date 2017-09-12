// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#include <mpark/patterns.hpp>

#include <array>
#include <string>
#include <utility>

#include <gtest/gtest.h>

TEST(CStyleArray, LRef) {
  int xs[3] = {1, 2, 3};

  using namespace mpark::patterns;
  int &result =
      match(xs)(pattern(prod(3, 2, arg)) = [](int &x) -> int & { return x; },
                pattern(prod(arg, 2, 3)) = [](int &x) -> int & { return x; });

  EXPECT_EQ(xs[0], result);
  EXPECT_EQ(&xs[0], &result);
}

TEST(CStyleArray, ConstLRef) {
  const int xs[3] = {1, 2, 3};

  using namespace mpark::patterns;
  const int &result = match(xs)(
      pattern(prod(3, 2, arg)) = [](const int &x) -> const int & { return x; },
      pattern(prod(arg, 2, 3)) = [](const int &x) -> const int & { return x; });

  EXPECT_EQ(xs[0], result);
  EXPECT_EQ(&xs[0], &result);
}

TEST(CStyleArray, RRef) {
  int xs[3] = {1, 2, 3};

  using namespace mpark::patterns;
  int result = match(std::move(xs))(
      pattern(prod(3, 2, arg)) = [](int &&x) -> int { return std::move(x); },
      pattern(prod(arg, 2, 3)) = [](int &&x) -> int { return std::move(x); });

  EXPECT_EQ(1, result);
}

TEST(StdArray, LRef) {
  std::array<std::string, 3> xs = {{"x", "y", "z"}};

  using namespace mpark::patterns;
  std::string &result = match(xs)(
      pattern(prod("z", "y", arg)) = [](std::string &s) -> std::string & {
        return s;
      },
      pattern(prod(arg,  _ , "z")) = [](std::string &s) -> std::string & {
        return s;
      },
      pattern(prod(arg, "y",  _ )) = [](std::string &s) -> std::string & {
        return s;
      });

  EXPECT_EQ(xs[0], result);
  EXPECT_EQ(&xs[0], &result);
}

TEST(StdArray, ConstLRef) {
  const std::array<std::string, 3> xs = {{"x", "y", "z"}};

  using namespace mpark::patterns;
  const std::string &result =
      match(xs)(pattern(prod("z", "y", arg)) = [](const std::string &s)
                    -> const std::string & { return s; },
                pattern(prod(arg,  _ , "z")) = [](const std::string &s)
                    -> const std::string & { return s; },
                pattern(prod(arg, "y",  _ )) = [](const std::string &s)
                    -> const std::string & { return s; });

  EXPECT_EQ(xs[0], result);
  EXPECT_EQ(&xs[0], &result);
}

TEST(StdArray, RRef) {
  auto xs = [] { return std::array<std::string, 3>{{"x", "y", "z"}}; };

  using namespace mpark::patterns;
  std::string result = match(xs())(
      pattern(prod("z", "y", arg)) = [](std::string &&s) -> std::string {
        return std::move(s);
      },
      pattern(prod(arg,  _ , "z")) = [](std::string &&s) -> std::string {
        return std::move(s);
      },
      pattern(prod(arg, "y",  _ )) = [](std::string &&s) -> std::string {
        return std::move(s);
      });

  EXPECT_EQ("x", result);
}
