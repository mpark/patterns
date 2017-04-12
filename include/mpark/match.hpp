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

  template <typename T>
  struct is_pattern : std::false_type {};

  // wildcard pattern.

  struct Wildcard {};
  constexpr Wildcard _{};

  template <>
  struct is_pattern<Wildcard> : std::true_type {};

  template <typename Value>
  bool matches(Wildcard, const Value &value) noexcept { return true; }

  template <typename Value>
  auto bindings(Wildcard, Value &&) noexcept { return std::tuple<>{}; }

  // bind pattern.

  template <typename Pattern>
  struct Arg { const Pattern &pattern; };

  template <>
  struct Arg<Wildcard> {
    template <typename Pattern_>
    auto operator()(const Pattern_ &pattern_) const {
      return Arg<Pattern_>{pattern_};
    }

    const Wildcard &pattern;
  };

  constexpr Arg<Wildcard> arg{_};

  template <typename Pattern>
  struct is_pattern<Arg<Pattern>> : std::true_type {};

  template <typename Pattern, typename Value>
  bool matches(const Arg<Pattern> &arg, const Value &value) noexcept {
    return matches(arg.pattern, value);
  }

  template <typename Pattern, typename Value>
  auto bindings(const Arg<Pattern> &arg, Value &&value) noexcept {
    return std::tuple_cat(std::forward_as_tuple(std::forward<Value>(value)),
                          bindings(arg.pattern, std::forward<Value>(value)));
  }

  // variadic pattern.

  template <typename Pattern>
  struct Variadic { const Pattern &pattern; };

  template <typename Pattern,
            std::enable_if_t<is_pattern<Pattern>::value, int> = 0>
  auto variadic(const Pattern &pattern) {
    return Variadic<Pattern>{pattern};
  }

  template <typename Pattern>
  struct is_pattern<Variadic<Pattern>> : std::true_type {};

  template <typename Pattern>
  struct is_variadic : std::false_type {};

  template <typename Pattern>
  struct is_variadic<Variadic<Pattern>> : std::true_type {};

  // product pattern.

  template <typename... Patterns>
  struct Prod : std::tuple<const Patterns &...> {
    using std::tuple<const Patterns &...>::tuple;
  };

  template <typename... Patterns>
  struct is_pattern<Prod<Patterns...>> : std::true_type {};

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
      bool matches_impl(const std::tuple<const Patterns &...> &p,
                        const Value &,
                        std::index_sequence<>) noexcept {
        return true;
      }

      template <typename... Patterns,
                typename Value,
                std::size_t I,
                std::size_t... Is>
      bool matches_impl(const std::tuple<const Patterns &...> &p,
                        const Value &value,
                        std::index_sequence<I, Is...>) noexcept {
        static constexpr auto size = sizeof...(Patterns);
        static_assert(size == std::tuple_size<Value>::value, "");
        return matches(std::get<I>(p), generic_get<I>(value)) &&
               matches_impl(p, value, std::index_sequence<Is...>{});
      }

      template <std::size_t N, typename... Patterns>
      constexpr bool matches_check() {
        constexpr std::size_t size = sizeof...(Patterns);
        if (size > N + 1) {
          return false;
        }
        constexpr bool bs[] = {is_variadic<Patterns>::value...};
        for (int i = 0; i < size; ++i) {
          if (bs[i]) {
            return i == size - 1;
          }
        }
        return size == N;
      }

      template <typename Pattern>
      static const auto &get_pattern(
          const Variadic<Pattern> &variadic) noexcept {
        return variadic.pattern;
      }

      template <typename Pattern>
      static const auto &get_pattern(const Pattern &pattern) noexcept {
        return pattern;
      }

      template <typename... Patterns, std::size_t... Is>
      static auto canonicalize(const std::tuple<const Patterns &...> &t,
                               std::index_sequence<Is...>) noexcept {
        constexpr std::size_t size = sizeof...(Patterns);
        return prod(get_pattern(std::get<(Is < size ? Is : size - 1)>(t))...);
      }

      template <typename... Patterns, typename Value, std::size_t... Is>
      auto bindings_impl(const std::tuple<const Patterns &...> &t,
                         Value &&value,
                         std::index_sequence<Is...>) noexcept {
        static constexpr auto size = sizeof...(Patterns);
        static_assert(size == std::tuple_size<std::decay_t<Value>>::value, "");
        return std::tuple_cat(bindings(
            std::get<Is>(t), generic_get<Is>(std::forward<Value>(value)))...);
      }

      template <typename Pattern, typename Value, std::size_t... Is>
      auto bindings_impl(const std::tuple<const Variadic<Pattern> &> &t,
                         Value &&value,
                         std::index_sequence<Is...> indices) noexcept {
        return bindings_impl(
            prod((Is, std::get<0>(t))...), std::forward<Value>(value), indices);
      }

    }  // namespace detail
  }  // namespace patterns

  template <typename... Patterns, typename Value>
  bool matches(const Prod<Patterns...> &prod, const Value &value) noexcept {
    using namespace patterns::detail;
    static constexpr std::size_t size = std::tuple_size<Value>::value;
    static_assert(matches_check<size, Patterns...>(), "");
    using Indices = std::make_index_sequence<size>;
    return matches_impl(canonicalize(prod, Indices{}), value, Indices{});
  }

  template <typename... Patterns, typename Value>
  auto bindings(const Prod<Patterns...> &prod, Value &&value) noexcept {
    using namespace patterns::detail;
    static constexpr std::size_t size =
        std::tuple_size<std::decay_t<Value>>::value;
    static_assert(matches_check<size, Patterns...>(), "");
    using Indices = std::make_index_sequence<size>;
    return bindings_impl(
        canonicalize(prod, Indices{}), std::forward<Value>(value), Indices{});
  }

  // sum pattern.
  template <typename T, typename Pattern>
  struct Sum {
    const Pattern &pattern;
  };

  template <typename T, typename Pattern>
  struct is_pattern<Sum<T, Pattern>> : std::true_type {};

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

  template <>
  struct is_pattern<None> : std::true_type {};

  template <typename Value>
  bool matches(None, const Value &value) noexcept { return !value; }

  template <typename Value>
  auto bindings(None, Value &&) noexcept { return std::tuple<>{}; };

  template <typename Pattern>
  struct Some { const Pattern &pattern; };

  template <typename Pattern>
  auto some(const Pattern &pattern) { return Some<Pattern>{pattern}; }

  template <typename Pattern>
  struct is_pattern<Some<Pattern>> : std::true_type {};

  template <typename Pattern, typename Value>
  bool matches(const Some<Pattern> &some, const Value &value) noexcept {
    return value && matches(some.pattern, *value);
  }

  template <typename Pattern, typename Value>
  auto bindings(const Some<Pattern> &some, Value &&value) noexcept {
    assert(value);
    return bindings(some.pattern, *std::forward<Value>(value));
  }

  // `match` DSL.

  template <typename Patterns, typename Handler>
  struct Case {
    Handler &&handler() && { return std::forward<Handler>(handler_); }

    Patterns patterns;
    Handler &&handler_;
  };

  template <typename... Patterns>
  struct PatternDecl {
    template <typename Handler>
    auto operator=(Handler &&handler) && noexcept {
      return Case<Prod<Patterns...>, Handler>{std::move(patterns),
                                              std::forward<Handler>(handler)};
    }

    Prod<Patterns...> patterns;
  };

  template <typename... Patterns>
  auto pattern(const Patterns &... patterns) noexcept {
    return PatternDecl<Patterns...>{prod(patterns...)};
  }

  template <typename R, typename... Values>
  struct Match {
    template <typename Patterns, typename Handler>
    decltype(auto) operator()(Case<Patterns, Handler> &&case_) && {
      const auto &patterns = std::move(case_).patterns;
      return matches(patterns, std::move(values))
                 ? patterns::lib::apply_r<R>(
                       std::move(case_).handler(),
                       bindings(patterns, std::move(values)))
                 : throw match_error("");
    }

    template <typename Patterns, typename Handler, typename... Cases>
    decltype(auto) operator()(Case<Patterns, Handler> &&case_,
                              Cases &&... cases) && {
      const auto &patterns = std::move(case_).patterns;
      return matches(patterns, std::move(values))
                 ? patterns::lib::apply_r<R>(
                       std::move(case_).handler(),
                       bindings(patterns, std::move(values)))
                 : std::move(*this)(std::forward<Cases>(cases)...);
    }

    std::tuple<Values &&...> values;
  };

  template <typename R = patterns::lib::Deduce, typename... Values>
  auto match(Values &&... values) noexcept {
    return Match<R, Values...>{
        std::forward_as_tuple(std::forward<Values>(values)...)};
  }

}  // namespace mpark

#endif  // MPARK_MATCH_HPP
