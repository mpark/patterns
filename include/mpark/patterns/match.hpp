// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_MATCH_HPP
#define MPARK_PATTERNS_MATCH_HPP

#include <array>
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

  // The type of exception thrown when none of the patterns match.
  class match_error : public std::exception {
    public:
    virtual const char *what() const noexcept { return "match_error"; }
  };

  // Used to indicate a match failure in `matches` functions.
  inline constexpr struct no_match_t {} no_match{};

  // The return type of `matches` functions.

  template <typename T>
  struct match_result : std::optional<detail::Forwarder<T>> {
    using type = T;

    using super = std::optional<detail::Forwarder<T>>;
    using super::super;

    match_result(no_match_t) noexcept {}
    match_result(std::nullopt_t) = delete;

    decltype(auto) get() && {
      return (*static_cast<super &&>(*this)).forward();
    }
  };

  template <typename T>
  inline constexpr bool is_match_result_v = false;

  template <typename T>
  inline constexpr bool is_match_result_v<match_result<T>> = true;

  // `std::invoke`-like utility for `matches` functions.
  template <typename F, typename... Args>
  auto match_invoke(F &&f, Args &&... args) {
    static_assert(lib::is_invocable_v<F, Args...>,
                  "The given handler `F` is not invocable with `Args...`. "
                  "Inspect the error messages below to determine what "
                  "`F` and `Args...` are.");
    using R = lib::invoke_result_t<F, Args...>;
    if constexpr (std::is_void_v<R>) {
      std::invoke(std::forward<F>(f), std::forward<Args>(args)...);
      return match_result<void>(detail::Void{});
    } else if constexpr (is_match_result_v<R>) {
      return std::invoke(std::forward<F>(f), std::forward<Args>(args)...);
    } else {
      return match_result<R>(
          std::invoke(std::forward<F>(f), std::forward<Args>(args)...));
    }
  }

  // `std::apply`-like utility for `matches` functions.

  template <typename F, typename Args, std::size_t... Is>
  auto match_apply(F &&f, Args &&args, std::index_sequence<Is...>) {
    return match_invoke(std::forward<F>(f),
                        std::get<Is>(std::forward<Args>(args))...);
  }

  template <typename F, typename Args>
  auto match_apply(F &&f, Args &&args) {
    return match_apply(
        std::forward<F>(f),
        std::forward<Args>(args),
        std::make_index_sequence<std::tuple_size_v<std::decay_t<Args>>>{});
  }

  // Expression Pattern

  template <typename ExprPattern, typename Value, typename F>
  auto matches(const ExprPattern &expr_pattern, Value &&value, F &&f) {
    return expr_pattern == std::forward<Value>(value)
               ? match_invoke(std::forward<F>(f))
               : no_match;
  }

  // Wildcard Pattern

  inline constexpr struct Wildcard {} _{};

  template <typename Value, typename F>
  auto matches(Wildcard, Value &&, F &&f) {
    return match_invoke(std::forward<F>(f));
  }

  // Placeholder Pattern

  template <std::size_t I, typename Pattern>
  struct Placeholder {
    Placeholder(const Placeholder &) = delete;
    Placeholder &operator=(const Placeholder &) = delete;

    const Pattern &pattern;
  };

  template <std::size_t I>
  struct Placeholder<I, Wildcard> {
    constexpr Placeholder(int) noexcept {}

    Placeholder(const Placeholder &) = delete;
    Placeholder &operator=(const Placeholder &) = delete;

    template <typename Pattern>
    auto operator()(const Pattern &pattern_) const noexcept {
      return Placeholder<I, Pattern>{pattern_};
    }

    const Wildcard &pattern = _;
  };

  inline constexpr Placeholder<static_cast<std::size_t>(-1), Wildcard> arg{0};

  template <std::size_t... Is>
  constexpr std::tuple<Placeholder<Is, Wildcard>...> placeholders_impl(
      std::index_sequence<Is...>) {
    return {Is...};
  }

  template <std::size_t N>
  constexpr auto placeholders() {
    return placeholders_impl(std::make_index_sequence<N>{});
  }

  namespace detail {

    template <std::size_t I, typename T>
    struct IndexedForwarder : Forwarder<T> {
      using super = Forwarder<T>;
      using super::super;
    };

    template <typename T>
    inline constexpr std::size_t index_v = -1;

    template <std::size_t I, typename T>
    inline constexpr std::size_t index_v<IndexedForwarder<I, T>> = I;

  }  // namespace detail

  template <std::size_t I, typename Pattern, typename Value, typename F>
  auto matches(const Placeholder<I, Pattern> &ph, Value &&value, F &&f) {
    return matches(
        ph.pattern, std::forward<Value>(value), [&](auto &&... args) {
          return match_invoke(
              std::forward<F>(f),
              detail::IndexedForwarder<I, Value &&>{std::forward<Value>(value)},
              std::forward<decltype(args)>(args)...);
        });
  }

  // Variadic Pattern

  template <typename Pattern>
  struct Variadic { const Pattern &pattern; };

  template <typename Pattern>
  auto variadic(const Pattern &pattern) noexcept {
    return Variadic<Pattern>{pattern};
  }

  template <typename Pattern>
  inline constexpr bool is_variadic_v = false;

  template <typename Pattern>
  inline constexpr bool is_variadic_v<Variadic<Pattern>> = true;

  // Product Pattern

  template <typename... Patterns>
  struct Prod { std::tuple<const Patterns &...> patterns; };

  template <typename... Patterns>
  auto prod(const Patterns &... patterns) noexcept {
    return Prod<Patterns...>{std::tie(patterns...)};
  }

  namespace detail {

    // Both (l/r)value-ref versions are handled for C-style array because
    // `std::forward<Array>(array)[I]` always yields an lvalue-ref on GCC.

    template <std::size_t I, typename T, std::size_t N>
    T &generic_get_impl(T (&array)[N], lib::priority<0>) noexcept {
      return array[I];
    }

    template <std::size_t I, typename T, std::size_t N>
    T &&generic_get_impl(T (&&array)[N], lib::priority<0>) noexcept {
      return std::move(array[I]);
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

    template <std::size_t N, typename... Patterns>
    constexpr bool matches_check() noexcept {
      constexpr std::size_t size = sizeof...(Patterns);
      if constexpr (size > N + 1) {
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
              typename Tuple,
              typename F,
              std::size_t = sizeof(std::tuple_size<std::decay_t<Tuple>>)>
    auto matches_impl(const Prod<Patterns...> &prod,
                      Tuple &&tuple_like,
                      F &&f,
                      lib::priority<1>) {
      constexpr std::size_t size = std::tuple_size_v<std::decay_t<Tuple>>;
      static_assert(matches_check<size, Patterns...>());
      using Is = std::make_index_sequence<size>;
      return matches_recur(canonicalize(prod, Is{}),
                           std::forward<Tuple>(tuple_like),
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

    // Returns `true` iif the elements at indices `Is...` in the given
    // tuple-like argument all compare equal to the element at index `I`.
    template <typename TupleLike, std::size_t I, std::size_t... Is>
    bool equals(const TupleLike &tuple_like, std::index_sequence<I, Is...>) {
      return (... && (std::get<I>(tuple_like) == std::get<Is>(tuple_like)));
    }

    // Returns `true` iif the elements that belong to each group compare
    // equal amongst themselves.
    template <typename TupleLike, typename... GroupedIndices>
    bool equals(const TupleLike &tuple_like, lib::list<GroupedIndices...>) {
      return (... && equals(tuple_like, GroupedIndices{}));
    }

    template <typename Head, typename... Tail>
    lib::list<Head, Tail...> prepend(Head, lib::list<Tail...>);

    template <std::size_t P, std::size_t I>
    lib::list<lib::indexed_type<P, std::index_sequence<I>>> insert(lib::list<>);

    template <std::size_t P, std::size_t I,
              std::size_t Q, std::size_t... Is, typename... Tail>
    auto insert(
        lib::list<lib::indexed_type<Q, std::index_sequence<Is...>>, Tail...>) {
      using Head = lib::indexed_type<Q, std::index_sequence<Is...>>;
      if constexpr (P == -1) {
        return lib::
            list<Head, Tail..., lib::indexed_type<P, std::index_sequence<I>>>{};
      } else if constexpr (P == Q) {
        return lib::list<lib::indexed_type<Q, std::index_sequence<Is..., I>>,
                         Tail...>{};
      } else {
        return prepend(Head{}, insert<P, I>(lib::list<Tail...>{}));
      }
    }

    template <typename... Ts>
    lib::list<typename Ts::type...> group_indices(lib::list<Ts...> result,
                                                  std::index_sequence<>,
                                                  std::index_sequence<>);

    template <typename... Ts,
              std::size_t P, std::size_t... Ps,
              std::size_t I, std::size_t... Is>
    auto group_indices(lib::list<Ts...> result,
                       std::index_sequence<P, Ps...>,
                       std::index_sequence<I, Is...>) {
      return group_indices(insert<P, I>(result),
                           std::index_sequence<Ps...>{},
                           std::index_sequence<Is...>{});
    }

    // Group the indices of the same placeholders within the arguments.
    //
    // Example:
    //   Given placeholders `x`, `y` and
    //   `match(1, 2, 1, 4)(pattern(x, arg, x, y) = f)`,
    //
    //   The type of arguments passed to the intermediate lambda are
    //   ```
    //     IndexedForwarder<0, int&&>
    //     IndexedForwarder<-1, int&&>
    //     IndexedForwarder<0, int&&>
    //     IndexedForwarder<1, int&&>
    //   ```
    //
    //   We want to take this sequence of types and return `[[0, 2], [1], [3]]`.
    //   These are the groups of elements (by their indices) that need to
    //   compare equal in order for the pattern to match. Specifically,
    //   the values that the `x` placeholder binds to, at index 0 and 2,
    //   would need to compare equal in order for this pattern to match.
    template <typename... Ts>
    using group_indices_t =
        decltype(group_indices(lib::list<>{},
                               std::index_sequence<index_v<Ts>...>{},
                               std::index_sequence_for<Ts...>{}));

    template <typename T>
    inline constexpr std::size_t front_v = -1;

    template <std::size_t I, std::size_t... Is>
    inline constexpr std::size_t front_v<std::index_sequence<I, Is...>> = I;

    template <typename... GroupedIndices>
    std::index_sequence<front_v<GroupedIndices>...> indices(
        lib::list<GroupedIndices...>);

    // Get the indices of the arguments to be passed to the final lambda.
    //
    // Example:
    //   Given placeholders `x`, `y` and
    //   `match(1, 2, 1, 4)(pattern(x, arg, x, y) = f)`,
    //   `group_indices_t` returns `[[0, 2], [1], [3]]` (see above).
    //
    //   Given this, the indices of the arguments to be passed to the final
    //   lambda, are the first element of each of the lists. In this case,
    //   We want `[0, 1, 3]`, so that we don't pass the value matched by `x`
    //   (i.e., `1`) multiple times.
    template <typename GroupedIndices>
    using indices_t = decltype(indices(GroupedIndices{}));

    template <typename Patterns, typename F>
    struct Case { Patterns patterns; F f; };

    template <typename Arg>
    Arg &&fwd(Arg &&arg) {
      return std::forward<Arg>(arg);
    }

    template <std::size_t I, typename T>
    T fwd(IndexedForwarder<I, T> &&arg) {
      static_assert(std::is_reference_v<T>);
      return std::move(arg).forward();
    }

    template <typename... Patterns>
    struct Pattern {
      template <typename F>
      auto operator=(F &&f) && noexcept {
        // The intermediate function that performs the adjustments for
        // placeholder-related functionality and ultimately calls `f` with
        // the final arguments.
        auto f_ = [&](auto &&... args) {
          using GroupedIndices =
              group_indices_t<std::decay_t<decltype(args)>...>;
          auto args_ =
              std::forward_as_tuple(fwd(std::forward<decltype(args)>(args))...);
          return equals(args_, GroupedIndices{})
                     ? match_apply(std::forward<F>(f),
                                   std::move(args_),
                                   indices_t<GroupedIndices>{})
                     : no_match;
        };
        return Case<Prod<Patterns...>, decltype(f_)>{std::move(patterns),
                                                     std::move(f_)};
      }

      Prod<Patterns...> patterns;
    };

    template <typename R, typename... Values>
    struct Match {
      private:
      template <typename Patterns, typename F>
      static auto try_matches(Case<Patterns, F> &&case_,
                              std::tuple<Values &&...> &&values) {
        auto result = matches(
            std::move(case_).patterns, std::move(values), std::move(case_).f);

        using Result = decltype(result);
        static_assert(is_match_result_v<Result>,
                      "The function `matches` is required to return "
                      " a `mpark::patterns::match_result` type. "
                      "If you're using `std::invoke`, try using "
                      "`mpark::patterns::match_invoke` instead.");

        if constexpr (std::is_same_v<R, Deduce>) {
          return result;
        } else if constexpr (std::is_void_v<R>) {
          return match_result<void>(Void{});
        } else if constexpr (std::is_convertible_v<typename Result::type, R>) {
          return match_result<R>(
              [&result]() -> R { return std::move(result).get(); }());
        }
      }

      public:
      template <typename Patterns, typename F, typename... Cases>
      decltype(auto) operator()(Case<Patterns, F> &&case_,
                                Cases &&... cases) && {
        auto result = try_matches(std::move(case_), std::move(values));
        if (result) {
          return std::move(result).get();
        }
        if constexpr (sizeof...(Cases) == 0) {
#ifdef MPARK_PATTERNS_EXCEPTIONS
          throw match_error{};
#else
          std::terminate();
#endif
        } else {
          return std::move(*this)(std::forward<Cases>(cases)...);
        }
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
