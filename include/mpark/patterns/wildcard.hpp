// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_WILDCARD_HPP
#define MPARK_PATTERNS_WILDCARD_HPP

#include <utility>

#include "lib.hpp"
#include "match.hpp"

namespace mpark {
  namespace patterns {

    struct Wildcard {};

    template <typename Value, typename F>
    auto matches(Wildcard, Value &&, F &&f) {
      return match_invoke(std::forward<F>(f));
    }

    constexpr Wildcard _{};

  }  // namespace patterns
}  // namespace mpark

#endif  // MPARK_PATTERNS_WILDCARD_HPP
