// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <mpark/patterns.hpp>

#include <cassert>
#include <iostream>
#include <optional>
#include <variant>

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

TEST(Patterns, Intro) {
  EXPECT_EQ(120, factorial(5));
  EXPECT_EQ(55, fib_v0(10));
  EXPECT_EQ(55, fib_v1(10));
  EXPECT_EQ(55, fib_v2(10));
}

namespace N {

  struct S {
    int x;
    std::string y;

    template <typename Self>
    static auto to_tuple(Self &&self) {
      return std::forward_as_tuple(std::forward<Self>(self).x,
                                   std::forward<Self>(self).y);
    }
  };

  template <std::size_t I>
  auto &&get(S &s) { return std::get<I>(S::to_tuple(s)); }

  template <std::size_t I>
  auto &&get(const S &s) { return std::get<I>(S::to_tuple(s)); }

  template <std::size_t I>
  auto &&get(S &&s) { return std::get<I>(S::to_tuple(std::move(s))); }

  template <std::size_t I>
  auto &&get(const S &&s) { return std::get<I>(S::to_tuple(std::move(s))); }

}  // namespace N

namespace std {

  template <>
  class tuple_size<N::S>
      : public tuple_size<decltype(N::S::to_tuple(std::declval<N::S>()))> {};

}  // namespace std

TEST(Patterns, Prod) {
  std::vector<std::pair<int, int>> points = {{0, 0}, {1, 0}, {1, 1}, {2, 0}};
  int origin = 0;
  int y_zero = 0;
  int otherwise = 0;
  for (const auto &point : points) {
    using namespace mpark::patterns;
    match(point)(pattern(prod(0, 0)) = [&origin] { ++origin; },
                 pattern(prod(_, 0)) = [&y_zero] { ++y_zero; },
                 pattern(prod(_, _)) = [&otherwise] { ++otherwise; });
  }
  EXPECT_EQ(1, origin);
  EXPECT_EQ(2, y_zero);
  EXPECT_EQ(1, otherwise);
}

TEST(Patterns, CustomProd) {
  N::S s{101, "world"};

  using namespace mpark::patterns;
  int result = match(s)(
      pattern(prod(0, "")) = [] { return 0;  },
      pattern(arg(prod(101, arg))) = [](const auto &x, const auto &y) {
        static_assert(std::is_same<N::S, std::decay_t<decltype(x)>>::value, "");
        static_assert(std::is_same<std::string, std::decay_t<decltype(y)>>::value, "");
        return 1;
      });

  EXPECT_EQ(1, result);
}

TEST(Patterns, Sum) {
  std::variant<int, std::string> v = 42;
  using namespace mpark::patterns;
  match(v)(
      pattern(sum<int>(arg)) = [](const auto &n) { EXPECT_EQ(42, n); },
      pattern(sum<std::string>(arg)) = [](const auto &) { EXPECT_FALSE(true); });
}

TEST(Patterns, MultiSum) {
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

TEST(Patterns, Pointer) {
  auto holds = [](int *p) {
    using namespace mpark::patterns;
    return match(p)(pattern(some(_)) = [] { return true; },
                    pattern(none) = [] { return false; });
  };

  int x = 42;
  EXPECT_TRUE(holds(&x));
  EXPECT_FALSE(holds(nullptr));
}

TEST(Patterns, Optional) {
  auto test_optional = [](const std::optional<std::optional<int>> &oo) {
    using namespace mpark::patterns;
    return match(oo)(pattern(some(some(_))) = [] { return 0; },
                     pattern(some(none)) = [] { return 1; },
                     pattern(none) = [] { return 2; });
  };

  std::optional<std::optional<int>> oo1(42);
  std::optional<std::optional<int>> oo2(std::optional<int>{});
  std::optional<std::optional<int>> oo3;

  EXPECT_EQ(0, test_optional(oo1));
  EXPECT_EQ(1, test_optional(oo2));
  EXPECT_EQ(2, test_optional(oo3));
}

template <typename F, typename Tuple>
decltype(auto) apply(F &&f, Tuple &&t) {
  using namespace mpark::patterns;
  return match(std::forward<Tuple>(t))(
      pattern(prod(variadic(arg))) = std::forward<F>(f));
}

TEST(Patterns, Apply) {
  std::tuple<int, std::string> x = {42, "hello"};
  ::apply(
      [](const auto &lhs, const auto &rhs) {
        EXPECT_EQ(42, lhs);
        EXPECT_EQ("hello", rhs);
      },
      x);
}

template <typename F, typename... Vs>
decltype(auto) visit(F &&f, Vs &&... vs) {
  using namespace mpark::patterns;
  return match(std::forward<Vs>(vs)...)(
      pattern(variadic(sum(arg))) = std::forward<F>(f));
}

struct Visitor {
  void operator()(int lhs, const std::string &rhs) const {
    EXPECT_EQ(42, lhs);
    EXPECT_EQ("hello", rhs);
  }
  template <typename T, typename U>
  void operator()(const T &, const U &) const {
    EXPECT_TRUE(false);
  }
};

TEST(Patterns, Visit) {
  std::variant<int, std::string> x = 42, y = "hello";
  ::visit(Visitor{}, x, y);
}

//void f(int) {}

// TEST(Patterns, ExplicitReturnType) {
//   // boost::optional<int> o(42);
//
//   // using namespace mpark::patterns;
//   // auto x = match<int>(o)(
//   //     pattern(some(arg)) = [](int v) { return static_cast<std::size_t>(v); },
//   //     pattern(none) = [] { return 'A'; });
//
//   // f(42UL);
//
//   // static_assert(std::is_same<decltype(x), int>::value, "");
//   // EXPECT_EQ(42, x);
// }
