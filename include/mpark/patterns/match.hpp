// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_MATCH_HPP
#define MPARK_PATTERNS_MATCH_HPP

#include <stdexcept>
#include <tuple>
#include <type_traits>
#include <utility>

#include <boost/optional.hpp>

#include "config.hpp"
#include "lib.hpp"

namespace mpark {
  namespace patterns {

    namespace detail {

      struct void_ {};

      template <typename T>
      struct forwarder {
        constexpr forwarder(T &&value) : value_(std::forward<T>(value)) {}

        forwarder(const forwarder &) noexcept = default;
        forwarder &operator=(const forwarder &) noexcept = delete;

        constexpr T forward() && { return std::forward<T>(value_); }

        private:
        T value_;
      };

      template <>
      struct forwarder<void> {
        constexpr forwarder(void_) noexcept {}

        forwarder(const forwarder &) noexcept = default;
        forwarder &operator=(const forwarder &) noexcept = delete;

        constexpr void forward() && noexcept {}
      };

    }  // namespace detail

#ifdef MPARK_PATTERNS_EXCEPTIONS
    class match_error : public std::exception {
      virtual const char *what() const noexcept { return "match_error"; }
    };
#endif

    struct no_match_t {};
    constexpr no_match_t no_match{};

    template <typename T>
    struct match_result : boost::optional<detail::forwarder<T>> {
      using super = boost::optional<detail::forwarder<T>>;
      using super::super;

      match_result(no_match_t) noexcept {}
      match_result(boost::none_t) = delete;

      decltype(auto) get() && {
        return (*static_cast<super &&>(*this)).forward();
      }
    };

    namespace detail {

      template <typename R>
      struct match_invoker {
        template <typename F, typename... Args>
        match_result<R> operator()(F &&f, Args &&... args) const {
          // return lib::invoke(std::forward<F>(f), std::forward<Args>(args)...);
          return match_result<R>(
              lib::invoke(std::forward<F>(f), std::forward<Args>(args)...));
        }
      };

      template <typename R>
      struct match_invoker<match_result<R>> {
        template <typename F, typename... Args>
        match_result<R> operator()(F &&f, Args &&... args) const {
          return lib::invoke(std::forward<F>(f), std::forward<Args>(args)...);
        }
      };

      template <>
      struct match_invoker<void> {
        template <typename F, typename... Args>
        match_result<void> operator()(F &&f, Args &&... args) const {
          lib::invoke(std::forward<F>(f), std::forward<Args>(args)...);
          // return void_{};
          return match_result<void>(void_{});
        }
      };

    }  // namespace detail

    template <typename F, typename... Args>
    auto match_invoke(F &&f, Args &&... args) {
      using R = decltype(
          lib::invoke(std::forward<F>(f), std::forward<Args>(args)...));
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

      template <std::size_t I, typename T>
      auto generic_get_impl(T &&t, int) noexcept
          -> decltype(std::forward<T>(t).template get<I>()) {
        return std::forward<T>(t).template get<I>();
      }

      template <std::size_t I, typename T>
      decltype(auto) generic_get_impl(T &&t, long) noexcept {
        using std::get;
        return get<I>(std::forward<T>(t));
      }

      template <std::size_t I, typename T>
      decltype(auto) generic_get(T &&t) noexcept {
        return generic_get_impl<I>(std::forward<T>(t), 0);
      }

      template <typename... Patterns, typename Values, typename F>
      auto matches_impl(const Prod<Patterns...> &,
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
      auto matches_impl(const Prod<Patterns...> &prod,
                        Values &&values,
                        F &&f,
                        std::index_sequence<I, Is...>) {
        return matches(
            std::get<I>(prod.patterns),
            generic_get<I>(std::forward<Values>(values)),
            [&](auto &&... head_args) {
              return matches_impl(
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
      constexpr bool is_variadic_v = false;

      template <typename Pattern>
      constexpr bool is_variadic_v<Variadic<Pattern>> = true;

      template <std::size_t N, typename... Patterns>
      constexpr bool matches_check() noexcept {
        constexpr std::size_t size = sizeof...(Patterns);
        if (size > N + 1) {
          return false;
        }
        constexpr bool bs[] = {is_variadic_v<Patterns>...};
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

    }  // namespace detail

    template <typename... Patterns, typename Values, typename F>
    auto matches(const Prod<Patterns...> &prod, Values &&values, F &&f) {
      constexpr std::size_t size = std::tuple_size<std::decay_t<Values>>::value;
      static_assert(detail::matches_check<size, Patterns...>(), "");
      using Is = std::make_index_sequence<size>;
      return detail::matches_impl(detail::canonicalize(prod, Is{}),
                                  std::forward<Values>(values),
                                  std::forward<F>(f),
                                  Is{});
    }

    // `match` DSL.

    namespace detail {

      struct Deduce;

      template <typename Patterns, typename F>
      struct Case {
        F &&f() && { return std::forward<F>(f_); }

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
          /*
          return matches(std::forward<Patterns>(patterns),
                         std::forward<Values>(values),
                         std::forward<F>(f));
          */
          auto result = matches(std::forward<Patterns>(patterns),
                                std::forward<Values>(values),
                                std::forward<F>(f));
          return result ? match_result<R>(std::move(result).get()) : no_match;
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
#ifdef MPARK_PATTERNS_EXCEPTIONS
          return result ? std::move(result).get() : throw match_error{};
#else
          return result;
#endif
        }

        template <typename Patterns, typename F, typename... Cases>
        decltype(auto) operator()(Case<Patterns, F> &&case_,
                                  Cases &&... cases) && {
          auto result = Matches<R>{}(std::move(case_).patterns,
                                     std::move(values),
                                     std::move(case_).f());
          if (result) {
#ifdef MPARK_PATTERNS_EXCEPTIONS
            return std::move(result).get();
#else
            return result;
#endif
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

  }  // namespace patterns
}  // namespace mpark

#include "anyof.hpp"
#include "bind.hpp"
#include "optional.hpp"
#include "sum.hpp"
#include "when.hpp"
#include "wildcard.hpp"

#endif  // MPARK_PATTERNS_MATCH_HPP
