// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_VIS_HPP
#define MPARK_PATTERNS_VIS_HPP

#include <type_traits>
#include <utility>
#include <variant>

namespace mpark::patterns {

  template <typename Pattern>
  struct Vis { const Pattern &pattern; };

  template <typename Pattern>
  auto vis(const Pattern &pattern) noexcept { return Vis<Pattern>{pattern}; }

  template <typename Pattern, typename Value, typename F>
  auto try_match(const Vis<Pattern> &vis, Value &&value, F &&f) {
    using std::visit;
    return visit(
        [&](auto &&v) {
          return try_match(
              vis.pattern, std::forward<decltype(v)>(v), std::forward<F>(f));
        },
        std::forward<Value>(value));
  }

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_VIS_HPP
