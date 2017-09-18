// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_WHEN_HPP
#define MPARK_PATTERNS_WHEN_HPP

#include <cstddef>
#include <type_traits>
#include <utility>

namespace mpark::patterns {

#define MPARK_PATTERNS_UNARY_PREFIX_OPERATOR(op)                      \
  template <typename Arg,                                             \
            std::enable_if_t<(is_identifier_v<std::decay_t<Arg>> ||   \
                              detail::is_guard_v<std::decay_t<Arg>>), \
                             int> = 0>                                \
  auto operator op(Arg &&arg) {                                       \
    auto guard = detail::make_guard(                                  \
        [](auto &&arg_) -> decltype(auto) {                           \
          return op std::forward<decltype(arg_)>(arg_);               \
        },                                                            \
        std::forward<Arg>(arg));                                      \
    return detail::Guard<decltype(guard)>{std::move(guard)};          \
  }

#define MPARK_PATTERNS_UNARY_POSTFIX_OPERATOR(op)                     \
  template <typename Arg,                                             \
            std::enable_if_t<(is_identifier_v<std::decay_t<Arg>> ||   \
                              detail::is_guard_v<std::decay_t<Arg>>), \
                             int> = 0>                                \
  auto operator op(Arg &&arg, int) {                                  \
    auto guard = detail::make_guard(                                  \
        [](auto &&arg_) -> decltype(auto) {                           \
          return std::forward<decltype(arg_)>(arg_) op;               \
        },                                                            \
        std::forward<Arg>(arg));                                      \
    return detail::Guard<decltype(guard)>{std::move(guard)};          \
  }

#define MPARK_PATTERNS_BINARY_OPERATOR(op)                             \
  template <typename Lhs,                                              \
            typename Rhs,                                              \
            std::enable_if_t<(is_identifier_v<std::decay_t<Lhs>> ||    \
                              is_identifier_v<std::decay_t<Rhs>> ||    \
                              detail::is_guard_v<std::decay_t<Lhs>> || \
                              detail::is_guard_v<std::decay_t<Rhs>>),  \
                             int> = 0>                                 \
  auto operator op(Lhs &&lhs, Rhs &&rhs) {                             \
    auto guard = detail::make_guard(                                   \
        [](auto &&lhs_, auto &&rhs_) -> decltype(auto) {               \
          return std::forward<decltype(lhs_)>(lhs_) op                 \
                 std::forward<decltype(rhs_)>(rhs_);                   \
        },                                                             \
        std::forward<Lhs>(lhs),                                        \
        std::forward<Rhs>(rhs));                                       \
    return detail::Guard<decltype(guard)>{std::move(guard)};           \
  }

  MPARK_PATTERNS_UNARY_PREFIX_OPERATOR(+)
  MPARK_PATTERNS_UNARY_PREFIX_OPERATOR(-)
  MPARK_PATTERNS_UNARY_PREFIX_OPERATOR(*)
  MPARK_PATTERNS_UNARY_PREFIX_OPERATOR(~)
  MPARK_PATTERNS_UNARY_PREFIX_OPERATOR(&)
  MPARK_PATTERNS_UNARY_PREFIX_OPERATOR(!)
  MPARK_PATTERNS_UNARY_PREFIX_OPERATOR(++)
  MPARK_PATTERNS_UNARY_PREFIX_OPERATOR(--)

  MPARK_PATTERNS_UNARY_POSTFIX_OPERATOR(++)
  MPARK_PATTERNS_UNARY_POSTFIX_OPERATOR(--)

  MPARK_PATTERNS_BINARY_OPERATOR(<<)
  MPARK_PATTERNS_BINARY_OPERATOR(>>)
  MPARK_PATTERNS_BINARY_OPERATOR(*)
  MPARK_PATTERNS_BINARY_OPERATOR(/)
  MPARK_PATTERNS_BINARY_OPERATOR(%)
  MPARK_PATTERNS_BINARY_OPERATOR(+)
  MPARK_PATTERNS_BINARY_OPERATOR(-)
  MPARK_PATTERNS_BINARY_OPERATOR(<)
  MPARK_PATTERNS_BINARY_OPERATOR(>)
  MPARK_PATTERNS_BINARY_OPERATOR(<=)
  MPARK_PATTERNS_BINARY_OPERATOR(>=)
  MPARK_PATTERNS_BINARY_OPERATOR(==)
  MPARK_PATTERNS_BINARY_OPERATOR(!=)
  MPARK_PATTERNS_BINARY_OPERATOR(||)
  MPARK_PATTERNS_BINARY_OPERATOR(&&)
  MPARK_PATTERNS_BINARY_OPERATOR(&)
  MPARK_PATTERNS_BINARY_OPERATOR(|)
  MPARK_PATTERNS_BINARY_OPERATOR(^)
  MPARK_PATTERNS_BINARY_OPERATOR(->*)
  MPARK_PATTERNS_BINARY_OPERATOR(<<=)
  MPARK_PATTERNS_BINARY_OPERATOR(>>=)
  MPARK_PATTERNS_BINARY_OPERATOR(*=)
  MPARK_PATTERNS_BINARY_OPERATOR(/=)
  MPARK_PATTERNS_BINARY_OPERATOR(%=)
  MPARK_PATTERNS_BINARY_OPERATOR(+=)
  MPARK_PATTERNS_BINARY_OPERATOR(-=)
  MPARK_PATTERNS_BINARY_OPERATOR(&=)
  MPARK_PATTERNS_BINARY_OPERATOR(|=)
  MPARK_PATTERNS_BINARY_OPERATOR(^=)

#define MPARK_PATTERNS_COMMA ,
  MPARK_PATTERNS_BINARY_OPERATOR(MPARK_PATTERNS_COMMA)
#undef MPARK_PATTERNS_COMMA

  // operators `=`, `()`, `[]` are defined as members of `Identifier`.

  struct When { bool condition; };

  template <typename F>
  auto operator>>=(When when, F &&f) {
    return when.condition ? match_invoke(std::forward<F>(f)) : no_match;
  }

#define WHEN(condition) return mpark::patterns::When{condition} >>= [&]

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_WHEN_HPP
