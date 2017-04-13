// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_OPTIONAL_HPP
#define MPARK_PATTERNS_OPTIONAL_HPP

#include <utility>

#include <mpark/patterns/fallthrough.hpp>
#include <mpark/patterns/lib.hpp>

namespace mpark {

  namespace patterns {

    struct None {};

    template <typename Value, typename F>
    decltype(auto) matches(None, Value &&value, F &&f) {
      if (!std::forward<Value>(value)) {
        return patterns::lib::invoke(std::forward<F>(f));
      }
      fallthrough();
    }

    template <typename Pattern>
    struct Some { const Pattern &pattern; };

    template <typename Pattern, typename Value, typename F>
    decltype(auto) matches(const Some<Pattern> &some, Value &&value, F &&f) {
      if (std::forward<Value>(value)) {
        using mpark::matches;
        return matches(
            some.pattern, *std::forward<Value>(value), std::forward<F>(f));
      }
      fallthrough();
    }

  }  // namespace patterns

  constexpr patterns::None none;

  template <typename Pattern>
  auto some(const Pattern &pattern) { return patterns::Some<Pattern>{pattern}; }

}  // namespace mpark

#endif  // MPARK_PATTERNS_OPTIONAL_HPP
