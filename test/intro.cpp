// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#include <mpark/patterns.hpp>

#include <cassert>
#include <iostream>
#include <optional>
#include <string>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

#include <gtest/gtest.h>

void fizzbuzz() {
  using namespace mpark::patterns;
  for (int i = 1; i <= 100; ++i) {
    match(i % 3, i % 5)(
        pattern(0, 0) = [] { std::cout << "fizzbuzz\n"; },
        pattern(0, _) = [] { std::cout << "fizz\n"; },
        pattern(_, 0) = [] { std::cout << "buzz\n"; },
        pattern(_, _) = [i] { std::cout << i << '\n'; });
  }
}

int factorial(int n) {
  using namespace mpark::patterns;
  return match(n)(pattern(0) = [] { return 1; },
                  pattern(arg) = [](int n) { return n * factorial(n - 1); });
}

int fib_v0(int n) {
  using namespace mpark::patterns;
  assert(n >= 0);
  return match(n)(
      pattern(0) = [] { return 0; },
      pattern(1) = [] { return 1; },
      pattern(arg) = [](int n) { return fib_v0(n - 1) + fib_v0(n - 2); });
}

int fib_v1(int n) {
  using namespace mpark::patterns;
  return match(n)(
      pattern(arg) = [](int n) { WHEN(n <= 0) { return 0; }; },
      pattern(1) = [] { return 1; },
      pattern(arg) = [](int n) { return fib_v1(n - 1) + fib_v1(n - 2); });
}

int fib_v2(int n) {
  using namespace mpark::patterns;
  return match(n)(
      pattern(arg) = [](int n) { WHEN(n < 0) { return 0; }; },
      pattern(arg(anyof(0, 1))) = [](int n) { return n; },
      pattern(arg) = [](int n) { return fib_v2(n - 1) + fib_v2(n - 2); });
}

TEST(Intro, Factorial) {
  EXPECT_EQ(120, factorial(5));
  EXPECT_EQ(3628800, factorial(10));
}

TEST(Intro, Fibonacci) {
  EXPECT_EQ(55, fib_v0(10));
  EXPECT_EQ(55, fib_v1(10));
  EXPECT_EQ(55, fib_v2(10));
}

TEST(Intro, ExplicitReturnType) {
  std::optional<double> o(4.2);

  using namespace mpark::patterns;
  auto x = match<int>(o)(
      pattern(some(arg)) = [](double v) { return v; },
      pattern(none) = [] { return 'A'; });

  static_assert(std::is_same<decltype(x), int>::value, "");
  EXPECT_EQ(4, x);
}

TEST(Intro, ExplicitVoidReturn) {
  std::optional<double> o(4.2);

  using namespace mpark::patterns;
  match<void>(o)(
      pattern(some(arg)) = [](double v) { return v; },
      pattern(none) = [] { return 'A'; });
}
