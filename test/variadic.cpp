// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#include <mpark/patterns.hpp>

#include <string>
#include <tuple>
#include <utility>
#include <variant>

#include <gtest/gtest.h>

namespace utility {

  template <typename F, typename Tuple>
  decltype(auto) apply(F &&f, Tuple &&t) {
    using namespace mpark::patterns;
    return match(std::forward<Tuple>(t))(
        pattern(prod(variadic(arg))) = std::forward<F>(f));
  }

}  // namespace utility

TEST(Variadic, Apply) {
  std::tuple<int, std::string> x = {42, "hello"};
  utility::apply(
      [](const auto &lhs, const auto &rhs) {
        EXPECT_EQ(42, lhs);
        EXPECT_EQ("hello", rhs);
      },
      x);
}

namespace utility {

  template <typename F, typename... Vs>
  decltype(auto) visit(F &&f, Vs &&... vs) {
    using namespace mpark::patterns;
    return match(std::forward<Vs>(vs)...)(
        pattern(variadic(sum(arg))) = std::forward<F>(f));
  }

}  // namespace utility

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

TEST(Variadic, Visit) {
  std::variant<int, std::string> x = 42, y = "hello";
  utility::visit(Visitor{}, x, y);
}

TEST(Variadic, Middle) {
  std::tuple<int, int, std::string, int> tuple = {42, 101, "hello", 42};

  using namespace mpark::patterns;
  IDENTIFIERS(x);
  match(tuple)(pattern(prod(x, variadic(arg), x)) = [](auto, auto... args) {
    static_assert(sizeof...(args) == 2);
  });
}
