// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_MATCH_HPP
#define MPARK_PATTERNS_MATCH_HPP

#include <cstddef>
#include <optional>
#include <stdexcept>
#include <tuple>
#include <type_traits>
#include <utility>

#include "config.hpp"
#include "detail/as_tuple.hpp"
#include "detail/forwarder.hpp"
#include "lib.hpp"

namespace mpark::patterns {

#ifdef MPARK_PATTERNS_EXCEPTIONS
  class match_error : public std::exception {
    virtual const char *what() const noexcept { return "match_error"; }
  };
#endif

  inline constexpr struct no_match_t {} no_match{};

  template <typename T>
  struct match_result : std::optional<detail::Forwarder<T>> {
    using super = std::optional<detail::Forwarder<T>>;
    using super::super;

    match_result(no_match_t) noexcept {}
    match_result(std::nullopt_t) = delete;

    decltype(auto) get() && {
      return (*static_cast<super &&>(*this)).forward();
    }
  };

  namespace detail {

    template <typename R>
    struct match_invoker {
      template <typename F, typename... Args>
      match_result<R> operator()(F &&f, Args &&... args) const {
        return std::invoke(std::forward<F>(f), std::forward<Args>(args)...);
      }
    };

    template <typename R>
    struct match_invoker<match_result<R>> {
      template <typename F, typename... Args>
      match_result<R> operator()(F &&f, Args &&... args) const {
        return std::invoke(std::forward<F>(f), std::forward<Args>(args)...);
      }
    };

    template <>
    struct match_invoker<void> {
      template <typename F, typename... Args>
      match_result<void> operator()(F &&f, Args &&... args) const {
        std::invoke(std::forward<F>(f), std::forward<Args>(args)...);
        return Void{};
      }
    };

  }  // namespace detail

  template <typename F, typename... Args>
  auto match_invoke(F &&f, Args &&... args) {
    static_assert(lib::is_invocable_v<F, Args...>,
                  "The given handler `F` is not invocable with `Args...`. "
                  "Inspect the error messages below to determine what "
                  "`F` and `Args...` are.");
    using R = lib::invoke_result_t<F, Args...>;
    return detail::match_invoker<R>{}(std::forward<F>(f),
                                      std::forward<Args>(args)...);
  }

  template <typename ExprPattern, typename Value, typename F>
  auto matches(const ExprPattern &expr_pattern, Value &&value, F &&f) {
    return expr_pattern == std::forward<Value>(value)
               ? match_invoke(std::forward<F>(f))
               : no_match;
  }

  template <typename Pattern>
  struct Variadic {
    const Pattern &pattern;
  };

  template <typename Pattern>
  auto variadic(const Pattern &pattern) noexcept {
    return Variadic<Pattern>{pattern};
  }

  template <typename... Patterns>
  struct Prod { std::tuple<const Patterns &...> patterns; };

  template <typename... Patterns>
  auto prod(const Patterns &... patterns) noexcept {
    return Prod<Patterns...>{std::tie(patterns...)};
  }

  namespace detail {

    template <
        std::size_t I,
        typename T,
        std::enable_if_t<std::is_array_v<std::remove_reference_t<T>>, int> = 0>
    decltype(auto) generic_get_impl(T &&t, lib::priority<0>) noexcept {
      return std::forward<T>(t)[I];
    }

    template <std::size_t I, typename T>
    auto generic_get_impl(T &&t, lib::priority<1>) noexcept
        -> decltype(std::forward<T>(t).template get<I>()) {
      return std::forward<T>(t).template get<I>();
    }

    template <std::size_t I, typename T>
    decltype(auto) generic_get_impl(T &&t, lib::priority<2>) noexcept {
      using std::get;
      return get<I>(std::forward<T>(t));
    }

    template <std::size_t I, typename T>
    decltype(auto) generic_get(T &&t) noexcept {
      return generic_get_impl<I>(std::forward<T>(t), lib::priority<>{});
    }

    template <typename... Patterns, typename Values, typename F>
    auto matches_recur(const Prod<Patterns...> &,
                       Values &&,
                       F &&f,
                       std::index_sequence<>) {
      return match_invoke(std::forward<F>(f));
    }

    template <typename... Patterns,
              typename Values,
              typename F,
              std::size_t I,
              std::size_t... Is>
    auto matches_recur(const Prod<Patterns...> &prod,
                       Values &&values,
                       F &&f,
                       std::index_sequence<I, Is...>) {
      return matches(
          std::get<I>(prod.patterns),
          generic_get<I>(std::forward<Values>(values)),
          [&](auto &&... head_args) {
            return matches_recur(
                prod,
                std::forward<Values>(values),
                [&](auto &&... tail_args) {
                  return match_invoke(
                      std::forward<F>(f),
                      std::forward<decltype(head_args)>(head_args)...,
                      std::forward<decltype(tail_args)>(tail_args)...);
                },
                std::index_sequence<Is...>{});
          });
    }

    template <typename Pattern>
    inline constexpr bool is_variadic_v = false;

    template <typename Pattern>
    inline constexpr bool is_variadic_v<Variadic<Pattern>> = true;

    template <std::size_t N, typename... Patterns>
    constexpr bool matches_check() noexcept {
      constexpr std::size_t size = sizeof...(Patterns);
      if (size > N + 1) {
        return false;
      }
      constexpr std::array<bool, sizeof...(Patterns)> bs = {
        { is_variadic_v<Patterns>... }
      };
      for (std::size_t i = 0; i < size; ++i) {
        if (bs[i]) {
          return i == size - 1;
        }
      }
      return size == N;
    }

    template <typename Pattern>
    const auto &get_pattern(const Variadic<Pattern> &variadic) noexcept {
      return variadic.pattern;
    }

    template <typename Pattern>
    const auto &get_pattern(const Pattern &pattern) noexcept {
      return pattern;
    }

    template <typename... Patterns, std::size_t... Is>
    auto canonicalize(const Prod<Patterns...> &p,
                      std::index_sequence<Is...>) noexcept {
      constexpr std::size_t size = sizeof...(Patterns);
      return prod(
          get_pattern(std::get<(Is < size ? Is : size - 1)>(p.patterns))...);
    }

    template <typename... Patterns,
              typename Array,
              typename F,
              std::enable_if_t<std::is_array_v<std::remove_reference_t<Array>>,
                               int> = 0>
    auto matches_impl(const Prod<Patterns...> &prod,
                      Array &&array,
                      F &&f,
                      lib::priority<0>) {
      constexpr std::size_t size =
          std::extent_v<std::remove_reference_t<Array>>;
      static_assert(matches_check<size, Patterns...>());
      using Is = std::make_index_sequence<size>;
      return matches_recur(canonicalize(prod, Is{}),
                           std::forward<Array>(array),
                           std::forward<F>(f),
                           Is{});
    }

    template <typename... Patterns,
              typename TupleLike,
              typename F,
              std::size_t = sizeof(std::tuple_size<std::decay_t<TupleLike>>)>
    auto matches_impl(const Prod<Patterns...> &prod,
                      TupleLike &&tuple_like,
                      F &&f,
                      lib::priority<1>) {
      constexpr std::size_t size = std::tuple_size_v<std::decay_t<TupleLike>>;
      static_assert(matches_check<size, Patterns...>());
      using Is = std::make_index_sequence<size>;
      return matches_recur(canonicalize(prod, Is{}),
                           std::forward<TupleLike>(tuple_like),
                           std::forward<F>(f),
                           Is{});
    }

    template <typename... Patterns, typename Aggregate, typename F>
    auto matches_impl(const Prod<Patterns...> &prod,
                      Aggregate &&aggregate,
                      F &&f,
                      lib::priority<2>) {
      using Decayed = std::decay_t<Aggregate>;
      static_assert(std::is_aggregate_v<Decayed>);
      static_assert(std::is_copy_constructible_v<Decayed>);
      return matches(prod,
                     as_tuple(std::forward<Aggregate>(aggregate)),
                     std::forward<F>(f));
    }

  }  // namespace detail

  template <typename... Patterns, typename Values, typename F>
  auto matches(const Prod<Patterns...> &prod, Values &&values, F &&f) {
    return detail::matches_impl(prod,
                                std::forward<Values>(values),
                                std::forward<F>(f),
                                lib::priority<>{});
  }

  // `match` DSL.

  namespace detail {

    struct Deduce;

    template <typename Patterns, typename F>
    struct Case {
      F &&f() && noexcept { return std::forward<F>(f_); }

      Patterns patterns;
      F &&f_;
    };

    template <typename... Patterns>
    struct Pattern {
      template <typename F>
      auto operator=(F &&f) && noexcept {
        return Case<Prod<Patterns...>, F>{std::move(patterns),
                                          std::forward<F>(f)};
      }

      Prod<Patterns...> patterns;
    };

    template <typename R>
    struct Matches {
      template <typename Patterns, typename Values, typename F>
      match_result<R> operator()(Patterns &&patterns,
                                 Values &&values,
                                 F &&f) const {
        return matches(std::forward<Patterns>(patterns),
                       std::forward<Values>(values),
                       std::forward<F>(f));
      }
    };

    template <>
    struct Matches<Deduce> {
      template <typename Patterns, typename Values, typename F>
      auto operator()(Patterns &&patterns, Values &&values, F &&f) const {
        return matches(std::forward<Patterns>(patterns),
                       std::forward<Values>(values),
                       std::forward<F>(f));
      }
    };

    template <typename R, typename... Values>
    struct Match {
      template <typename Patterns, typename F>
      decltype(auto) operator()(Case<Patterns, F> &&case_) && {
        auto result = Matches<R>{}(std::move(case_).patterns,
                                   std::move(values),
                                   std::move(case_).f());
        if (!result) {
#ifdef MPARK_PATTERNS_EXCEPTIONS
          throw match_error{};
#else
          std::terminate();
#endif
        }
        return std::move(result).get();
      }

      template <typename Patterns, typename F, typename... Cases>
      decltype(auto) operator()(Case<Patterns, F> &&case_,
                                Cases &&... cases) && {
        auto result = Matches<R>{}(std::move(case_).patterns,
                                   std::move(values),
                                   std::move(case_).f());
        if (result) {
          return std::move(result).get();
        }
        return std::move(*this)(std::forward<Cases>(cases)...);
      }

      std::tuple<Values &&...> values;
    };

  }  // namespace detail

  template <typename... Patterns>
  auto pattern(const Patterns &... patterns) noexcept {
    return detail::Pattern<Patterns...>{prod(patterns...)};
  }

  template <typename R = detail::Deduce, typename... Values>
  auto match(Values &&... values) noexcept {
    return detail::Match<R, Values...>{
        std::forward_as_tuple(std::forward<Values>(values)...)};
  }

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_MATCH_HPP
