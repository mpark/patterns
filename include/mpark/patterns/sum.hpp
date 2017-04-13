// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_SUM_HPP
#define MPARK_PATTERNS_SUM_HPP

#include <utility>

#include <mpark/patterns/fallthrough.hpp>
#include <mpark/variant.hpp>

namespace mpark {

  namespace patterns {

    template <typename T, typename Pattern>
    struct Sum { const Pattern &pattern; };

  }  // namespace patterns

  template <typename T, typename Pattern>
  auto sum(const Pattern &pattern) {
    return patterns::Sum<T, Pattern>{pattern};
  }

  namespace patterns {

    namespace detail {

      template <typename T, typename V>
      auto generic_get_impl(V &&v, int)
          -> decltype(std::forward<V>(v).template get<T>()) {
        return std::forward<V>(v).template get<T>();
      }

      template <typename T, typename V>
      decltype(auto) generic_get_impl(V &&v, long) {
        using mpark::get;
        return get<T>(std::forward<V>(v));
      }

      template <typename T, typename V>
      decltype(auto) generic_get(V &&v) {
        return generic_get_impl<T>(std::forward<V>(v), 0);
      }

    }  // namespace detail

    template <typename T, typename Pattern, typename Value, typename F>
    decltype(auto) matches(const Sum<T, Pattern> &sum, Value &&value, F &&f) {
      try {
        using mpark::matches;
        return matches(sum.pattern,
                       detail::generic_get<T>(std::forward<Value>(value)),
                       std::forward<F>(f));
      } catch (...) {
        fallthrough();
      }
    }

  }  // namespace patterns

}  // namespace mpark

#endif  // MPARK_PATTERNS_SUM_HPP
