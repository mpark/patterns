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

#include <mpark/patterns/lib.hpp>

namespace mpark {

  namespace patterns {

    struct Wildcard {};

    template <typename Value, typename F>
    decltype(auto) matches(Wildcard, Value &&, F &&f) {
      return lib::invoke(std::forward<F>(f));
    }

  }  // namespace patterns

  constexpr patterns::Wildcard _{};

}  // namespace mpark

#endif  // MPARK_PATTERNS_WILDCARD_HPP
