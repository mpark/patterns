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

#include "lib.hpp"
#include "std/variant.hpp"

namespace mpark {
  namespace patterns {

    namespace detail { struct Visit; }

    template <typename T, typename Pattern>
    struct SumByType { const Pattern &pattern; };

    template <typename Pattern>
    struct SumByVisit { const Pattern &pattern; };

    template <typename T = detail::Visit, typename Pattern>
    auto sum(const Pattern &pattern) {
      using Sum = std::conditional_t<std::is_same<T, detail::Visit>::value,
                                     SumByVisit<Pattern>,
                                     SumByType<T, Pattern>>;
      return Sum{pattern};
    }

    namespace detail {

      template <typename T, typename V>
      auto generic_get_impl(V &&v, int)
          -> decltype(std::forward<V>(v).template get<T>()) {
        return std::forward<V>(v).template get<T>();
      }

      template <typename T, typename V>
      decltype(auto) generic_get_impl(V &&v, long) {
        using std::get;
        return get<T>(std::forward<V>(v));
      }

      template <typename T, typename V>
      decltype(auto) generic_get(V &&v) {
        return generic_get_impl<T>(std::forward<V>(v), 0);
      }

      template <typename T, typename V>
      auto generic_get_if_impl(V &&v, int)
          -> decltype(std::forward<V>(v).template get_if<T>()) {
        return std::forward<V>(v).template get_if<T>();
      }

      template <typename T, typename V>
      auto *generic_get_if_impl(V &&v, long) {
        using std::get_if;
        return get_if<T>(&v);
      }

      template <typename T, typename V>
      auto *generic_get_if(V &&v) {
        return generic_get_if_impl<T>(std::forward<V>(v), 0);
      }

    }  // namespace detail

    template <typename T, typename Pattern, typename Value, typename F>
    auto matches(const SumByType<T, Pattern> &sum, Value &&value, F &&f) {
      using V = decltype(detail::generic_get<T>(std::forward<Value>(value)));
      auto *v = detail::generic_get_if<T>(std::forward<Value>(value));
      return v ? matches(sum.pattern, std::forward<V>(*v), std::forward<F>(f))
               : no_match;
    }

    template <typename Pattern, typename Value, typename F>
    auto matches(const SumByVisit<Pattern> &sum, Value &&value, F &&f) {
      using std::visit;
      return visit(
          [&](auto &&arg) {
            return matches(sum.pattern,
                           std::forward<decltype(arg)>(arg),
                           std::forward<F>(f));
          },
          std::forward<Value>(value));
    }

  }  // namespace patterns
}  // namespace mpark

#endif  // MPARK_PATTERNS_SUM_HPP
