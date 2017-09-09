// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_WHEN_HPP
#define MPARK_PATTERNS_WHEN_HPP

#include <utility>

namespace mpark {
  namespace patterns {

    struct When { bool condition; };

        // <functional>
#define RETURN(...)                                          \
  noexcept(noexcept(__VA_ARGS__)) -> decltype(__VA_ARGS__) { \
    return __VA_ARGS__;                                      \
  }

    template <typename F>
    auto operator>>=(When when, F &&f)
      RETURN(when.condition ? match_invoke(std::forward<F>(f)) : no_match)

#undef RETURN

#define WHEN(condition) return When{condition} >>= [&]

  }  // namespace patterns
}  // namespace mpark

#endif  // MPARK_PATTERNS_WHEN_HPP
