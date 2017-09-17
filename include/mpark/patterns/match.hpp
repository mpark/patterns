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
#include <functional>
#include <optional>
#include <stdexcept>
#include <string_view>
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

  // Used to indicate a match failure in `try_match` functions.
  inline constexpr struct no_match_t {} no_match{};

  // The return type of `try_match` functions.

  template <typename T>
  struct match_result : std::optional<detail::forwarder<T>> {
    using type = T;

    using super = std::optional<detail::forwarder<T>>;
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

  // `std::invoke`-like utility for `try_match` functions.
  template <typename F, typename... Args>
  auto match_invoke(F &&f, Args &&... args) {
    static_assert(lib::is_invocable_v<F, Args...>,
                  "The given handler `F` is not invocable with `Args...`. "
                  "Inspect the error messages below to determine what "
                  "`F` and `Args...` are.");
    using R = lib::invoke_result_t<F, Args...>;
    if constexpr (std::is_void_v<R>) {
      std::invoke(std::forward<F>(f), std::forward<Args>(args)...);
      return match_result<void>(detail::void_{});
    } else if constexpr (is_match_result_v<R>) {
      return std::invoke(std::forward<F>(f), std::forward<Args>(args)...);
    } else {
      return match_result<R>(
          std::invoke(std::forward<F>(f), std::forward<Args>(args)...));
    }
  }

  // `std::apply`-like utility for `try_match` functions.

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

  inline constexpr std::size_t npos = static_cast<std::size_t>(-1);

  // Expression Pattern

  template <typename ExprPattern, typename Value, typename F>
  auto try_match(const ExprPattern &expr_pattern, Value &&value, F &&f) {
    return expr_pattern == std::forward<Value>(value)
               ? match_invoke(std::forward<F>(f))
               : no_match;
  }

  // Wildcard Pattern

  inline constexpr struct Wildcard {} _{};

  template <typename Value, typename F>
  auto try_match(Wildcard, Value &&, F &&f) {
    return match_invoke(std::forward<F>(f));
  }

  // Binding Patterns

  // Identifier Pattern

  template <std::size_t I, typename Pattern>
  struct Identifier {
    Identifier(const Identifier &) = delete;
    Identifier &operator=(const Identifier &) = delete;

    const Pattern &pattern;
  };

  template <std::size_t I>
  struct Identifier<I, Wildcard> {
    constexpr Identifier(int) noexcept {}

    Identifier(const Identifier &) = delete;
    Identifier &operator=(const Identifier &) = delete;

    template <typename Pattern>
    auto operator()(const Pattern &pattern_) const noexcept {
      return Identifier<I, Pattern>{pattern_};
    }

    const Wildcard &pattern = _;
  };

  // Arg Pattern

  inline constexpr Identifier<npos, Wildcard> arg{0};

  namespace detail {

    template <std::size_t... Is>
    constexpr std::tuple<Identifier<Is, Wildcard>...> identifiers_impl(
        std::index_sequence<Is...>) {
      return {Is...};
    }

    template <std::size_t N>
    constexpr auto identifiers() {
      return identifiers_impl(std::make_index_sequence<N>{});
    }

    constexpr std::size_t count_args(const char *s) {
      std::size_t result = 1;
      for (const char *iter = s; iter && *iter != '\0'; ++iter) {
        if (*iter == ',') {
          ++result;
        }
      }
      return result;
    }

  }  // namespace detail

#define IDENTIFIERS(...)                                     \
  auto [__VA_ARGS__] = mpark::patterns::detail::identifiers< \
      mpark::patterns::detail::count_args(#__VA_ARGS__)>()

  namespace detail {

    template <std::size_t I, typename T>
    struct indexed_forwarder : forwarder<T> {
      using super = forwarder<T>;
      using super::super;
    };

    template <typename T>
    inline constexpr bool is_indexed_forwarder_v = false;

    template <std::size_t I, typename T>
    inline constexpr bool is_indexed_forwarder_v<indexed_forwarder<I, T>> = true;

    template <typename T>
    inline constexpr std::size_t index_v = npos;

    template <std::size_t I, typename T>
    inline constexpr std::size_t index_v<indexed_forwarder<I, T>> = I;

  }  // namespace detail

  template <std::size_t I, typename Pattern, typename Value, typename F>
  auto try_match(const Identifier<I, Pattern> &identifier,
                 Value &&value,
                 F &&f) {
    return try_match(
        identifier.pattern, std::forward<Value>(value), [&](auto &&... args) {
          return match_invoke(std::forward<F>(f),
                              detail::indexed_forwarder<I, Value &&>{
                                  std::forward<Value>(value)},
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

  // Destructure Pattern

  template <typename... Patterns>
  struct Ds { std::tuple<const Patterns &...> patterns; };

  template <typename... Patterns>
  auto ds(const Patterns &... patterns) noexcept {
    return Ds<Patterns...>{std::tie(patterns...)};
  }

  template <typename T,
            std::enable_if_t<std::is_class_v<T>, int> = 0,
            std::size_t = sizeof(std::tuple_size<T>)>
  constexpr bool is_tuple_like(lib::priority<0>) noexcept { return true; }

  template <typename T>
  constexpr bool is_tuple_like(lib::priority<1>) noexcept { return false; }

  template <typename T>
  inline constexpr bool is_tuple_like_v = is_tuple_like<T>(lib::priority<>{});

  namespace detail {

    enum class DetectResult { Member, NonMember, None };

    using std::get;

    template <std::size_t I,
              typename T,
              typename = decltype(std::declval<T>().template get<I>())>
    constexpr DetectResult detect_get(lib::priority<0>) noexcept {
      return DetectResult::Member;
    }

    template <std::size_t I,
              typename T,
              typename = decltype(get<I>(std::declval<T>()))>
    constexpr DetectResult detect_get(lib::priority<1>) noexcept {
      return DetectResult::NonMember;
    }

    template <std::size_t I, typename T>
    constexpr DetectResult detect_get(lib::priority<2>) noexcept {
      return DetectResult::None;
    }

    template <std::size_t I, typename T>
    inline constexpr DetectResult detect_get_v =
        detect_get<I, T>(lib::priority<>{});

    template <typename... Patterns, typename Values, typename F>
    auto try_match_impl(const Ds<Patterns...> &,
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
    auto try_match_impl(const Ds<Patterns...> &ds,
                        Values &&values,
                        F &&f,
                        std::index_sequence<I, Is...>) {
      return try_match(
          std::get<I>(ds.patterns),
          [&]() -> decltype(auto) {
            if constexpr (std::is_array_v<std::remove_reference_t<Values>>) {
              // We handle the forwarding explicitly because
              // `std::forward<T>(t)[I]` always yields an lvalue-ref on GCC.
              if constexpr (std::is_rvalue_reference_v<Values &&>) {
                return std::move(values[I]);
              } else {
                return values[I];
              }
            } else if constexpr (is_tuple_like_v<std::decay_t<Values>>) {
              constexpr auto result = detail::detect_get_v<I, Values>;
              if constexpr (result == DetectResult::Member) {
                return std::forward<Values>(values).template get<I>();
              } else if constexpr (result == DetectResult::NonMember) {
                using std::get;
                return get<I>(std::forward<Values>(values));
              } else {
                static_assert(lib::false_v<Values>,
                              "The value attempting to be matched against a "
                              "`ds` pattern has a specialization for "
                              "`std::tuple_size`, but does not have a member "
                              "nor non-member `get` function available.");
              }
            }
          }(),
          [&](auto &&... head_args) {
            return try_match_impl(
                ds,
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

    enum class DsPatternCheckResult {
      Success,
      TooManyVariadics,
      NotEnoughPatterns,
      TooManyPatterns
    };

    template <typename... Patterns>
    constexpr std::size_t find_variadic() {
      constexpr std::size_t size = sizeof...(Patterns);
      constexpr std::array<bool, size> bs = {{is_variadic_v<Patterns>...}};
      for (std::size_t i = 0; i < size; ++i) {
        if (bs[i]) {
          return i;
        }
      }
      return npos;
    }

    template <std::size_t N, typename... Patterns>
    constexpr DsPatternCheckResult ds_pattern_check() noexcept {
      constexpr std::size_t size = sizeof...(Patterns);
      if constexpr (size == 0) {
        return N == 0 ? DsPatternCheckResult::Success
                      : DsPatternCheckResult::NotEnoughPatterns;
      } else {
        constexpr std::array<bool, size> bs = {{is_variadic_v<Patterns>...}};
        std::size_t index = npos;
        for (std::size_t i = 0; i < size; ++i) {
          if (bs[i]) {
            if (index == npos) {
              index = i;
            } else {
              return DsPatternCheckResult::TooManyVariadics;
            }
          }
        }
        if (index == npos) {  // non-variadic
          if constexpr (N > size) {
            return DsPatternCheckResult::NotEnoughPatterns;
          } else if constexpr (N < size) {
            return DsPatternCheckResult::TooManyPatterns;
          } else {
            return DsPatternCheckResult::Success;
          }
        } else {  // variadic
          if constexpr (N + 1 < size) {
            return DsPatternCheckResult::TooManyPatterns;
          } else {
            return DsPatternCheckResult::Success;
          }
        }
      }
    }

    template <typename... Patterns, std::size_t... Is>
    auto expand_variadics(const Ds<Patterns...> &p,
                          std::index_sequence<Is...>) noexcept {
      constexpr std::size_t variadic_index = find_variadic<Patterns...>();
      constexpr std::size_t num_values = sizeof...(Is);
      constexpr std::size_t num_patterns = sizeof...(Patterns);
      if constexpr (variadic_index == npos) {
        static_assert(num_values == num_patterns);
        return p;
      } else {
        if constexpr (num_values < num_patterns) {
          static_assert(num_values == num_patterns - 1);
          return ds(
              std::get<(variadic_index <= Is ? Is + 1 : Is)>(p.patterns)...);
        } else {
          static_assert(num_values >= num_patterns);
          constexpr std::size_t diff = num_values - num_patterns;
          auto index = [](std::size_t i) constexpr {
            if (i < variadic_index) {
              return i;
            } else if (variadic_index <= i && i <= variadic_index + diff) {
              return variadic_index;
            } else {
              return i - diff;
            }
          };
          return ds([](auto &&pattern) -> auto && {
            if constexpr (is_variadic_v<std::decay_t<decltype(pattern)>>) {
              return pattern.pattern;
            } else {
              return pattern;
            }
          }(std::get<index(Is)>(p.patterns))...);
        }
      }
    }

  }  // namespace detail

  template <typename... Patterns, typename Values, typename F>
  auto try_match(const Ds<Patterns...> &ds, Values &&values, F &&f) {
    constexpr bool is_array = std::is_array_v<std::remove_reference_t<Values>>;
    constexpr bool is_tuple_like = is_tuple_like_v<std::decay_t<Values>>;
    if constexpr (!is_array && !is_tuple_like) {
      using Aggregate = std::decay_t<Values>;
      static_assert(std::is_aggregate_v<Aggregate>);
      static_assert(std::is_copy_constructible_v<Aggregate>);
      return try_match(ds,
                       detail::as_tuple(std::forward<Values>(values)),
                       std::forward<F>(f));
    } else {
      constexpr auto size = [] {
        if constexpr (is_array) {
          return std::extent<std::remove_reference_t<Values>>{};
        } else if constexpr (is_tuple_like) {
          return std::tuple_size<std::decay_t<Values>>{};
        } else {
          static_assert(
              lib::false_v<Values>,
              "The value attempting to be matched against a `ds` "
              "pattern is not an array, tuple-like, nor an aggregate.");
        }
      }();
      constexpr auto result = detail::ds_pattern_check<size(), Patterns...>();
      static_assert(
          result != detail::DsPatternCheckResult::TooManyVariadics,
          "The variadic pattern can only appear once in a `ds` pattern.");
      static_assert(
          result != detail::DsPatternCheckResult::NotEnoughPatterns,
          "There are not enough patterns provided to match the values.");
      static_assert(
          result != detail::DsPatternCheckResult::TooManyPatterns,
          "There are too many patterns provided to match the values.");
      using Is = std::make_index_sequence<size()>;
      return detail::try_match_impl(detail::expand_variadics(ds, Is{}),
                                    std::forward<Values>(values),
                                    std::forward<F>(f),
                                    Is{});
    }
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
      if constexpr (P == npos) {
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
    //     indexed_forwarder<0, int&&>
    //     indexed_forwarder<npos, int&&>
    //     indexed_forwarder<0, int&&>
    //     indexed_forwarder<1, int&&>
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
    inline constexpr std::size_t front_v = npos;

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

    template <typename Pattern, typename Rhs>
    struct Case {
      Rhs &&rhs() && noexcept { return std::forward<Rhs>(rhs_); }

      Pattern pattern;
      Rhs &&rhs_;
    };

    template <typename... Patterns>
    struct Pattern {
      static constexpr std::size_t size = sizeof...(Patterns);

      template <typename Rhs>
      auto operator=(Rhs &&rhs) && noexcept {
        return Case<Pattern, Rhs>{std::move(*this), std::forward<Rhs>(rhs)};
      }

      Ds<Patterns...> patterns;
    };

    template <typename R, typename... Values>
    struct Match {
      public:
      template <typename Pattern, typename F, typename... Cases>
      decltype(auto) operator()(Case<Pattern, F> &&case_,
                                Cases &&... cases) && {
        auto result = [&] {
          auto result = try_match(
              std::move(case_).pattern.patterns,
              std::move(values),
              [&](auto &&... args) {
                // The intermediate function that performs the adjustments for
                // placeholder-related functionality and ultimately calls `f`
                // with the final arguments.
                using GroupedIndices =
                    group_indices_t<std::decay_t<decltype(args)>...>;
                auto args_ = std::forward_as_tuple([](auto &&arg) -> auto && {
                  if constexpr (is_indexed_forwarder_v<
                                    std::decay_t<decltype(arg)>>) {
                    static_assert(std::is_rvalue_reference_v<decltype(arg)>);
                    static_assert(std::is_reference_v<decltype(
                                      std::move(arg).forward())>);
                    return std::move(arg).forward();
                  } else {
                    return std::forward<decltype(arg)>(arg);
                  }
                }(std::forward<decltype(args)>(args))...);
                return equals(args_, GroupedIndices{})
                           ? match_apply(std::move(case_).rhs(),
                                         std::move(args_),
                                         indices_t<GroupedIndices>{})
                           : no_match;
              });

          using Result = decltype(result);
          static_assert(is_match_result_v<Result>,
                        "The function `try_match` is required to return "
                        " a `mpark::patterns::match_result` type. "
                        "If you're using `std::invoke`, try using "
                        "`mpark::patterns::match_invoke` instead.");

          if constexpr (std::is_same_v<R, Deduce>) {
            return result;
          } else if constexpr (std::is_void_v<R>) {
            return match_result<void>(void_{});
          } else if constexpr (std::is_convertible_v<typename Result::type,
                                                     R>) {
            return match_result<R>(
                [&result]() -> R { return std::move(result).get(); }());
          }
        }();
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
    return detail::Pattern<Patterns...>{ds(patterns...)};
  }

  template <typename R = detail::Deduce, typename... Values>
  auto match(Values &&... values) noexcept {
    return detail::Match<R, Values...>{
        std::forward_as_tuple(std::forward<Values>(values)...)};
  }

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_MATCH_HPP
