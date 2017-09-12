// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_WHEN_HPP
#define MPARK_PATTERNS_WHEN_HPP

#include <utility>

namespace mpark::patterns {

  struct When { bool condition; };

  template <typename F>
  auto operator>>=(When when, F &&f) {
    return when.condition ? match_invoke(std::forward<F>(f)) : no_match;
  }

#define WHEN(condition) return When{condition} >>= [&]

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_WHEN_HPP
