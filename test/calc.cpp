// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#include <mpark/patterns.hpp>

#include <cstddef>
#include <memory>
#include <ostream>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>

#include <gtest/gtest.h>

namespace memory {

  template <typename T>
  struct part_ptr : std::unique_ptr<T> {
    using super = std::unique_ptr<T>;
    using super::super;

    T &operator*() & { return *this->get(); }
    const T &operator*() const & { return *this->get(); }
    T &&operator*() && { return std::move(*this->get()); }
    const T &&operator*() const && { return std::move(*this->get()); }
  };

  template <typename T, typename... Args>
  auto make_part(Args&&... args) {
    return part_ptr<T>(new T(std::forward<Args>(args)...));
  }

}  // namespace memory

namespace calc {

  class Expr;

  class Unary {
    public:
    explicit Unary(Expr &&expr);

    private:
    memory::part_ptr<Expr> expr_;

    template <std::size_t I,
              typename FwdUnary,
              std::enable_if_t<std::is_base_of_v<Unary, std::decay_t<FwdUnary>>,
                               int> = 0>
    friend auto &&get(FwdUnary &&unary) noexcept {
      if constexpr (I == 0) return *std::forward<FwdUnary>(unary).expr_;
    }
  };

  class Binary {
    public:
    explicit Binary(Expr &&lhs, Expr &&rhs);

    private:
    memory::part_ptr<Expr> lhs_, rhs_;

    template <
        std::size_t I,
        typename FwdBinary,
        std::enable_if_t<std::is_base_of_v<Binary, std::decay_t<FwdBinary>>,
                         int> = 0>
    friend auto &&get(FwdBinary &&binary) noexcept {
      if constexpr (I == 0) return *std::forward<FwdBinary>(binary).lhs_;
      if constexpr (I == 1) return *std::forward<FwdBinary>(binary).rhs_;
    }
  };

  class Func : public Unary { public: using Unary::Unary; };
  class Plus : public Binary { public: using Binary::Binary; };
  class Mult : public Binary { public: using Binary::Binary; };

  class Expr : public std::variant<int, Plus, Mult, Func> {
    public:
    using variant::variant;
  };

  Unary::Unary(Expr &&expr) : expr_(memory::make_part<Expr>(std::move(expr))) {}

  Binary::Binary(Expr &&lhs, Expr &&rhs)
      : lhs_(memory::make_part<Expr>(std::move(lhs))),
        rhs_(memory::make_part<Expr>(std::move(rhs))) {}

}  // namespace calc

namespace std {

  template <>
  class tuple_size<calc::Unary> : public integral_constant<size_t, 1> {};

  template <>
  class tuple_size<calc::Binary> : public integral_constant<size_t, 2> {};

  template <> class tuple_size<calc::Func> : public tuple_size<calc::Unary> {};
  template <> class tuple_size<calc::Plus> : public tuple_size<calc::Binary> {};
  template <> class tuple_size<calc::Mult> : public tuple_size<calc::Binary> {};

}  // namespace std

namespace calc {

  std::ostream &operator<<(std::ostream &strm, const Expr &expr) {
    using namespace mpark::patterns;
    placeholder lhs, rhs;
    match(expr)(
        pattern(sum<int>(arg)) = [&](auto x) { strm << x; },
        pattern(sum<Plus>(prod(lhs, rhs))) = [&](auto &&lhs, auto &&rhs) {
          strm << "(+ " << lhs << ' ' << rhs << ')';
        },
        pattern(sum<Mult>(prod(lhs, rhs))) = [&](auto &&lhs, auto &&rhs) {
          strm << "(* " << lhs << ' ' << rhs << ')';
        },
        pattern(sum<Func>(prod(arg))) = [&](auto &&body) {
          strm << "(fn [] " << body << ')';
        });
    return strm;
  }

  int eval(const Expr &expr) {
    using namespace mpark::patterns;
    placeholder lhs, rhs;
    return match(expr)(
        pattern(sum<int>(arg)) = [](auto x) { return x; },
        pattern(sum<Plus>(prod(lhs, rhs))) = [](auto &&lhs, auto &&rhs) {
          return eval(lhs) + eval(rhs);
        },
        pattern(sum<Mult>(prod(lhs, rhs))) = [](auto &&lhs, auto &&rhs) {
          return eval(lhs) * eval(rhs);
        },
        pattern(sum<Func>(prod(arg))) = [](auto &&body) { return eval(body); });
  }

}  // namespace calc

TEST(Calc, Int) {
  using namespace calc;
  Expr expr = 101;
  EXPECT_EQ("101", (std::ostringstream{} << expr).str());
  EXPECT_EQ(101, eval(expr));
}

TEST(Calc, MultPlus) {
  using namespace calc;
  Expr expr = Mult(Plus(Expr(101), Expr(202)), 303);
  EXPECT_EQ("(* (+ 101 202) 303)", (std::ostringstream{} << expr).str());
  EXPECT_EQ(91809, eval(expr));
}

TEST(Calc, FuncPlus) {
  using namespace calc;
  Expr expr = Func(Plus(Expr(101), Expr(202)));
  EXPECT_EQ("(fn [] (+ 101 202))", (std::ostringstream{} << expr).str());
  EXPECT_EQ(303, eval(expr));
}
