// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_ANYOF_HPP
#define MPARK_PATTERNS_ANYOF_HPP

#include <cstddef>
#include <tuple>
#include <utility>

namespace mpark::patterns {

  template <typename... Patterns>
  struct Anyof { std::tuple<const Patterns &...> patterns; };

  template <typename... Patterns>
  auto anyof(const Patterns &... patterns) noexcept {
    return Anyof<Patterns...>{std::tie(patterns...)};
  }

  namespace detail {

    template <typename... Patterns, typename Value, typename F, std::size_t I>
    auto try_match_impl(const Anyof<Patterns...> &anyof,
                        Value &&value,
                        F &&f,
                        std::index_sequence<I>) {
      return try_match(std::get<I>(anyof.patterns),
                       std::forward<Value>(value),
                       [&](auto &&... args) {
                         return match_invoke(
                             std::forward<F>(f),
                             std::forward<decltype(args)>(args)...);
                       });
    }

    template <typename... Patterns,
              typename Value,
              typename F,
              std::size_t I,
              std::size_t J,
              std::size_t... Js>
    auto try_match_impl(const Anyof<Patterns...> &anyof,
                        Value &&value,
                        F &&f,
                        std::index_sequence<I, J, Js...>) {
      auto result = try_match(std::get<I>(anyof.patterns),
                              std::forward<Value>(value),
                              [&](auto &&... args) {
                                return match_invoke(
                                    std::forward<F>(f),
                                    std::forward<decltype(args)>(args)...);
                              });
      return result ? std::move(result)
                    : try_match_impl(anyof,
                                     std::forward<Value>(value),
                                     std::forward<F>(f),
                                     std::index_sequence<J, Js...>{});
    }

  }  // namespace detail

  template <typename... Patterns, typename Value, typename F>
  auto try_match(const Anyof<Patterns...> &anyof, Value &&value, F &&f) {
    return detail::try_match_impl(anyof,
                                  std::forward<Value>(value),
                                  std::forward<F>(f),
                                  std::index_sequence_for<Patterns...>{});
  }

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_ANYOF_HPP
