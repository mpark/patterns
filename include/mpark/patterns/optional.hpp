// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_OPTIONAL_HPP
#define MPARK_PATTERNS_OPTIONAL_HPP

#include <utility>

namespace mpark::patterns {

  inline constexpr struct None {} none{};

  template <typename Value, typename F>
  auto matches(None, Value &&value, F &&f) {
    return value ? no_match : match_invoke(std::forward<F>(f));
  }

  template <typename Pattern>
  struct Some { const Pattern &pattern; };

  template <typename Pattern>
  auto some(const Pattern &pattern) { return Some<Pattern>{pattern}; }

  template <typename Pattern, typename Value, typename F>
  auto matches(const Some<Pattern> &some, Value &&value, F &&f) {
    return value ? matches(some.pattern,
                           *std::forward<Value>(value),
                           std::forward<F>(f))
                 : no_match;
  }

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_OPTIONAL_HPP
