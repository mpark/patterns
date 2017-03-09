// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef MPARK_MATCH_HPP
#define MPARK_MATCH_HPP

#include <cassert>
#include <stdexcept>
#include <tuple>
#include <utility>

#include <mpark/patterns/lib.hpp>
#include <mpark/variant.hpp>

namespace mpark {

  struct match_error : std::logic_error {
    using std::logic_error::logic_error;
  };

  // expr pattern.

  template <typename ExprPattern, typename Value>
  bool matches(const ExprPattern &expr_pattern, const Value &value) noexcept {
    return expr_pattern == value;
  }

  template <typename ExprPattern, typename Value>
  auto bindings(const ExprPattern &expr_pattern, Value &&) noexcept {
    return std::tuple<>{};
  }

  // `match` DSL.

  template <typename Pattern, typename Handler>
  struct Case {
    Handler &&handler() const && { return std::forward<Handler>(handler_); }

    const Pattern &pattern;
    Handler &&handler_;
  };

  template <typename Pattern>
  struct PatternDecl {
    template <typename Handler>
    auto operator=(Handler &&handler) && noexcept {
      return Case<Pattern, Handler>{pattern_, std::forward<Handler>(handler)};
    }

    const Pattern &pattern_;
  };

  template <typename Pattern>
  auto pattern(const Pattern &pattern) noexcept {
    return PatternDecl<Pattern>{pattern};
  }

  template <typename R, typename Value>
  struct Match {
    template <typename Pattern, typename Handler>
    decltype(auto) operator()(Case<Pattern, Handler> &&case_) && {
      const auto &pattern = std::move(case_).pattern;
      return matches(pattern, std::forward<Value>(value_))
                 ? patterns::lib::apply_r<R>(
                       std::move(case_).handler(),
                       bindings(pattern, std::forward<Value>(value_)))
                 : throw match_error("");
    }

    template <typename Pattern, typename Handler, typename... Cases>
    decltype(auto) operator()(Case<Pattern, Handler> &&case_,
                              Cases &&... cases) && {
      const auto &pattern = std::move(case_).pattern;
      return matches(pattern, std::forward<Value>(value_))
                 ? patterns::lib::apply_r<R>(
                       std::move(case_).handler(),
                       bindings(pattern, std::forward<Value>(value_)))
                 : std::move(*this)(std::forward<Cases>(cases)...);
    }

    Value &&value_;
  };

  template <typename R = patterns::lib::Deduce, typename Value>
  auto match(Value &&value) noexcept {
    return Match<R, Value>{std::forward<Value>(value)};
  }

  // wildcard pattern.

  struct Wildcard {};
  constexpr Wildcard _{};

  template <typename Value>
  bool matches(Wildcard, const Value &value) noexcept { return true; }

  template <typename Value>
  auto bindings(Wildcard, Value &&) noexcept { return std::tuple<>{}; }

  // bind pattern.

  template <typename Pattern>
  struct Arg {
    template <typename Pattern_>
    auto operator()(const Pattern_ &pattern_) const {
      return Arg<Pattern_>{pattern_};
    }

    const Pattern &pattern;
  };

  constexpr Arg<Wildcard> arg{_};

  template <typename Pattern, typename Value>
  bool matches(const Arg<Pattern> &arg, const Value &value) noexcept {
    return matches(arg.pattern, value);
  }

  template <typename Pattern, typename Value>
  auto bindings(const Arg<Pattern> &arg, Value &&value) noexcept {
    return std::tuple_cat(std::forward_as_tuple(std::forward<Value>(value)),
                          bindings(arg.pattern, std::forward<Value>(value)));
  }

  // product pattern.

  template <typename... Patterns>
  struct Prod : std::tuple<const Patterns &...> {
    using std::tuple<const Patterns &...>::tuple;
  };

  template <typename... Patterns>
  auto prod(const Patterns &... patterns) {
    return Prod<Patterns...>(patterns...);
  }

  namespace patterns {
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

      template <typename... Patterns, typename Value>
      bool matches_impl(const std::tuple<const Patterns &...> &prod,
                        const Value &,
                        std::index_sequence<>) noexcept {
        return true;
      }

      template <typename... Patterns,
                typename Value,
                std::size_t I,
                std::size_t... Is>
      bool matches_impl(const std::tuple<const Patterns &...> &prod,
                        const Value &value,
                        std::index_sequence<I, Is...>) noexcept {
        return matches(std::get<I>(prod), generic_get<I>(value)) &&
               matches_impl(prod, value, std::index_sequence<Is...>{});
      }

      template <typename... Patterns, typename Value, std::size_t... Is>
      auto bindings_impl(const std::tuple<const Patterns &...> &prod,
                         Value &&value,
                         std::index_sequence<Is...>) noexcept {
        return std::tuple_cat(bindings(
            std::get<Is>(prod), generic_get<Is>(std::forward<Value>(value)))...);
      }

    }  // namespace detail
  }  // namespace patterns

  template <typename... Patterns, typename Value>
  bool matches(const Prod<Patterns...> &prod, const Value &value) noexcept {
    static constexpr auto size = sizeof...(Patterns);
    static_assert(size == std::tuple_size<Value>::value, "");
    return patterns::detail::matches_impl(prod, value, std::make_index_sequence<size>{});
  }

  template <typename... Patterns, typename Value>
  auto bindings(const Prod<Patterns...> &prod, Value &&value) noexcept {
    static constexpr auto size = sizeof...(Patterns);
    static_assert(size == std::tuple_size<std::decay_t<Value>>::value, "");
    return patterns::detail::bindings_impl(
        prod, std::forward<Value>(value), std::make_index_sequence<size>{});
  }

  // sum pattern.
  template <typename T, typename Pattern>
  struct Sum {
    const Pattern &pattern;
  };

  template <typename T, typename Pattern>
  auto sum(const Pattern &pattern) {
    return Sum<T, Pattern>{pattern};
  }

  namespace patterns {
    namespace detail {

      template <typename T, typename V>
      auto generic_get_impl(V &&v, int) noexcept
          -> decltype(std::forward<V>(v).template get<T>()) {
        return std::forward<V>(v).template get<T>();
      }

      template <typename T, typename V>
      decltype(auto) generic_get_impl(V &&v, long) noexcept {
        using mpark::get;
        return get<T>(std::forward<V>(v));
      }

      template <typename T, typename V>
      decltype(auto) generic_get(V &&v) noexcept {
        return generic_get_impl<T>(std::forward<V>(v), 0);
      }

      template <typename T, typename V>
      auto generic_get_if_impl(V &&v, int) noexcept
          -> decltype(std::forward<V>(v).template get_if<T>()) {
        return std::forward<V>(v).template get_if<T>();
      }

      template <typename T, typename V>
      decltype(auto) generic_get_if_impl(V &&v, long) noexcept {
        using mpark::get_if;
        return get_if<T>(std::forward<V>(v));
      }

      template <typename T, typename V>
      decltype(auto) generic_get_if(V &&v) noexcept {
        return generic_get_if_impl<T>(std::forward<V>(v), 0);
      }

    }  // namespace detail
  }  // namespace patterns

  template <typename T, typename Pattern, typename Value>
  bool matches(const Sum<T, Pattern> &sum, const Value &value) noexcept {
    const auto *v = patterns::detail::generic_get_if<T>(&value);
    return v && matches(sum.pattern, *v);
  }

  template <typename T, typename Pattern, typename Value>
  auto bindings(const Sum<T, Pattern> &sum, Value &&value) noexcept {
    return bindings(
        sum.pattern,
        patterns::detail::generic_get<T>(std::forward<Value>(value)));
  }

  // optional pattern.

  struct None {};
  constexpr None none;

  template <typename Value>
  bool matches(None, const Value &value) noexcept { return !value; }

  template <typename Value>
  auto bindings(None, Value &&) noexcept { return std::tuple<>{}; };

  template <typename Pattern>
  struct Some { const Pattern &pattern; };

  template <typename Pattern>
  auto some(const Pattern &pattern) { return Some<Pattern>{pattern}; }

  template <typename Pattern, typename Value>
  bool matches(const Some<Pattern> &some, const Value &value) noexcept {
    return value && matches(some.pattern, *value);
  }

  template <typename Pattern, typename Value>
  auto bindings(const Some<Pattern> &some, Value &&value) noexcept {
    assert(value);
    return bindings(some.pattern, *std::forward<Value>(value));
  }

  // `as_tuple` utility.

  template <typename T, typename... Fs>
  auto as_tuple(T &&t, Fs &&... fs) {
    return std::forward_as_tuple(
        patterns::lib::invoke(std::forward<Fs>(fs), std::forward<T>(t))...);
  }

}  // namespace mpark

// ```cpp
// match(<expr>)(
//   pattern(<pattern>)[.when(<bool-expr>)] = <handler>,
//   pattern(<pattern>)[.when(<bool-expr>)] = <handler>,
//   ...);
// ```
//
// ## Patterns
//   - sum types: `sum(Circle)
//   - type-casting: `is<T>`, as<T>(<pattern>)

#endif  // MPARK_MATCH_HPP
