// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_WILDCARD_HPP
#define MPARK_PATTERNS_WILDCARD_HPP

#include <utility>

namespace mpark::patterns {

  /* inline */ constexpr struct Wildcard {} _{};

  template <typename Value, typename F>
  auto matches(Wildcard, Value &&, F &&f) {
    return match_invoke(std::forward<F>(f));
  }

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_WILDCARD_HPP
