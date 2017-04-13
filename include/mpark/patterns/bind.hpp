// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_BIND_HPP
#define MPARK_PATTERNS_BIND_HPP

#include <utility>

#include <mpark/patterns/lib.hpp>
#include <mpark/patterns/wildcard.hpp>

namespace mpark {

  namespace patterns {

    template <typename Pattern>
    struct Arg { const Pattern &pattern; };

    template <>
    struct Arg<Wildcard> {
      template <typename Pattern_>
      auto operator()(const Pattern_ &pattern_) const noexcept {
        return Arg<Pattern_>{pattern_};
      }

      const Wildcard &pattern;
    };

    template <typename Pattern, typename Value, typename F>
    decltype(auto) matches(const Arg<Pattern> &arg, Value &&value, F &&f) {
      using mpark::matches;
      return matches(
          arg.pattern, std::forward<Value>(value), [&](auto &&... args) {
            return lib::invoke(std::forward<F>(f),
                               std::forward<Value>(value),
                               std::forward<decltype(args)>(args)...);
          });
    }

  }  // namespace patterns

  constexpr patterns::Arg<patterns::Wildcard> arg{_};

}  // namespace mpark

#endif  // MPARK_PATTERNS_BIND_HPP
