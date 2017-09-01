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

#include <mpark/variant.hpp>

#include "fallthrough.hpp"
#include "lib.hpp"

namespace mpark {
  namespace patterns {

    namespace detail { struct Visit; }

    template <typename T, typename Pattern>
    struct Sum { const Pattern &pattern; };

    template <typename T = detail::Visit, typename Pattern>
    auto sum(const Pattern &pattern) {
      return Sum<T, Pattern>{pattern};
    }

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
        return matches(sum.pattern,
                       detail::generic_get<T>(std::forward<Value>(value)),
                       std::forward<F>(f));
      } catch (...) {
        fallthrough();
      }
    }

    template <typename Pattern, typename Value, typename F>
    decltype(auto) matches(const Sum<detail::Visit, Pattern> &sum,
                           Value &&value,
                           F &&f) {
      using mpark::visit;
      return visit(
          [&](auto &&arg) -> decltype(auto) {
            return matches(sum.pattern,
                           std::forward<decltype(arg)>(arg),
                           std::forward<F>(f));
          },
          std::forward<Value>(value));
    }

  }  // namespace patterns
}  // namespace mpark

#endif  // MPARK_PATTERNS_SUM_HPP
