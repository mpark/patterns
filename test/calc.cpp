// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <mpark/patterns/match.hpp>

#include <mpark/variant.hpp>

#include <gtest/gtest.h>

struct Expr;

struct Unary {
  template <typename Self>
  static auto to_tuple(Self &&self) {
    return std::forward_as_tuple(*std::forward<Self>(self).expr_);
  }

  explicit Unary(Expr &&expr)
      : expr_(std::make_unique<Expr>(std::move(expr))) {}

  template <std::size_t I>
  auto &&get() & { return std::get<I>(to_tuple(*this)); }

  template <std::size_t I>
  auto &&get() const & { return std::get<I>(to_tuple(*this)); }

  template <std::size_t I>
  auto &&get() && { return std::get<I>(to_tuple(std::move(*this))); }

  template <std::size_t I>
  auto &&get() const && { return std::get<I>(to_tuple(std::move(*this))); }

  std::unique_ptr<Expr> expr_;
};

struct Binary {
  template <typename Self>
  static auto to_tuple(Self &&self) {
    return std::forward_as_tuple(*std::forward<Self>(self).lhs_,
                                 *std::forward<Self>(self).rhs_);
  }

  explicit Binary(Expr &&lhs, Expr &&rhs)
      : lhs_(std::make_unique<Expr>(std::move(lhs))),
        rhs_(std::make_unique<Expr>(std::move(rhs))) {}

  template <std::size_t I>
  auto &&get() & { return std::get<I>(to_tuple(*this)); }

  template <std::size_t I>
  auto &&get() const & { return std::get<I>(to_tuple(*this)); }

  template <std::size_t I>
  auto &&get() && { return std::get<I>(to_tuple(std::move(*this))); }

  template <std::size_t I>
  auto &&get() const && { return std::get<I>(to_tuple(std::move(*this))); }

  std::unique_ptr<Expr> lhs_, rhs_;
};

struct Plus : Binary { using Binary::Binary; };
struct Mult : Binary { using Binary::Binary; };
struct Func : Unary { using Unary::Unary; };

namespace std {

  template <> struct tuple_size<Unary> : integral_constant<size_t, 1> {};
  template <> struct tuple_size<Binary> : integral_constant<size_t, 2> {};
  template <> struct tuple_size<Plus> : tuple_size<Binary> {};
  template <> struct tuple_size<Mult> : tuple_size<Binary> {};
  template <> struct tuple_size<Func> : tuple_size<Unary> {};

}  // namespace std

struct Expr : mpark::variant<int, Plus, Mult, Func> { using variant::variant; };

std::ostream &operator<<(std::ostream &strm, const Expr &expr) {
  using namespace mpark::patterns;
  match(expr)(
      pattern(sum<int>(arg)) = [&](auto x) { strm << x; },
      pattern(sum<Plus>(prod(arg, arg))) =
          [&](const auto &lhs, const auto &rhs) {
            strm << "(+ " << lhs << ' ' << rhs << ')';
          },
      pattern(sum<Mult>(prod(arg, arg))) =
          [&](const auto &lhs, const auto &rhs) {
            strm << "(* " << lhs << ' ' << rhs << ')';
          },
      pattern(sum<Func>(prod(arg))) =
          [&](const auto &body) { strm << "(fn [] " << body << ')'; });
  return strm;
}

int eval(const Expr &expr) {
  using namespace mpark::patterns;
  return match(expr)(
      pattern(sum<int>(arg)) = [](auto x) { return x; },
      pattern(sum<Plus>(prod(arg, arg))) = [](
          const auto &lhs, const auto &rhs) { return eval(lhs) + eval(rhs); },
      pattern(sum<Mult>(prod(arg, arg))) = [](
          const auto &lhs, const auto &rhs) { return eval(lhs) * eval(rhs); },
      pattern(sum<Func>(prod(arg))) = [](
          const auto &body) { return eval(body); });
}

TEST(Pattern, Calc) {
  auto to_string = [](const Expr &expr) {
    std::ostringstream strm;
    strm << expr;
    return strm.str();
  };

  {
    Expr expr = 101;
    EXPECT_EQ("101", to_string(expr));
    EXPECT_EQ(101, eval(expr));
  }

  {
    Expr expr = Mult(Plus(Expr(101), Expr(202)), 303);
    EXPECT_EQ("(* (+ 101 202) 303)", to_string(expr));
    EXPECT_EQ(91809, eval(expr));
  }

  {
    Expr expr = Func(Plus(Expr(101), Expr(202)));
    EXPECT_EQ("(fn [] (+ 101 202))", to_string(expr));
    EXPECT_EQ(303, eval(expr));
  }
}
