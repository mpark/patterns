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

#include "lib.hpp"

namespace mpark {
  namespace patterns {

    struct none_t {};
    constexpr none_t none{};

    template <typename Value, typename F>
    auto matches(none_t, Value &&value, F &&f) {
      return value ? no_match : match_invoke(std::forward<F>(f));
    }

    template <typename Pattern>
    struct some_t { const Pattern &pattern; };

    template <typename Pattern>
    auto some(const Pattern &pattern) { return some_t<Pattern>{pattern}; }

    template <typename Pattern, typename Value, typename F>
    decltype(auto) matches(const some_t<Pattern> &some, Value &&value, F &&f) {
      return value ? matches(some.pattern,
                             *std::forward<Value>(value),
                             std::forward<F>(f))
                   : no_match;
    }

  }  // namespace patterns
}  // namespace mpark

#endif  // MPARK_PATTERNS_OPTIONAL_HPP
