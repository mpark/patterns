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

  namespace detail {

    template <typename F>
    struct Guard;

    template <typename T>
    inline constexpr bool is_guard_v = false;

    template <typename F>
    inline constexpr bool is_guard_v<Guard<F>> = true;

  }  // namespace detail

  template <std::size_t I, typename Pattern>
  struct Identifier;

  template <typename T>
  inline constexpr bool is_identifier_v = false;

  template <std::size_t I, typename Pattern>
  inline constexpr bool is_identifier_v<Identifier<I, Pattern>> = true;

  // Special indices.
  inline constexpr std::size_t wildcard_index = npos;
  inline constexpr std::size_t arg_index = npos / 2;

  namespace detail {

    template <std::size_t I, typename T>
    struct indexed_forwarder : forwarder<T> {
      static constexpr std::size_t index = I;

      using super = forwarder<T>;
      using super::super;
    };

    template <typename T>
    inline constexpr bool is_indexed_forwarder_v = false;

    template <std::size_t I, typename T>
    inline constexpr bool is_indexed_forwarder_v<indexed_forwarder<I, T>> = true;

    template <typename Is, typename... Ts>
    struct set;

    template <std::size_t... Is, typename... Ts>
    struct set<std::index_sequence<Is...>, Ts...>
        : lib::indexed_type<Is, Ts>... {};

    template <std::size_t I, typename T>
    struct find_indexed_forwarder;

    template <std::size_t I, typename... Ts>
    struct find_indexed_forwarder<I, std::tuple<Ts...>> {
      template <std::size_t Idx, typename T>
      static constexpr std::size_t impl(
          lib::indexed_type<Idx, indexed_forwarder<I, T> &&>) {
        return Idx;
      }

      static constexpr void impl(...) {
        static_assert(I == wildcard_index || I == arg_index);

        static_assert(I != wildcard_index,
                      "Reference to the wildcard pattern (`_`) in the `when` "
                      "clause is ambiguous. There are multiple instances of "
                      "them in the source pattern.");
        static_assert(I != arg_index,
                      "Reference to the arg pattern (`arg`) in the `when` "
                      "clause is ambiguous. There are multiple instances of "
                      "them in the source pattern.");
      }

      static constexpr std::size_t value =
          impl(set<std::index_sequence_for<Ts...>, Ts...>{});
    };

    template <std::size_t I, typename T>
    inline constexpr std::size_t find_indexed_forwarder_v =
        find_indexed_forwarder<I, T>::value;

    template <typename Arg, typename... Args>
    decltype(auto) eval(Arg &&arg, Args &&... args) {
      auto args_tuple = std::forward_as_tuple(std::forward<Args>(args)...);
      using Decayed = std::decay_t<Arg>;
      if constexpr (is_identifier_v<Decayed>) {
        if constexpr (Decayed::has_pattern) {
          return std::invoke(std::forward<Arg>(arg).as_guard().f,
                             std::forward<Args>(args)...);
        } else {
          constexpr std::size_t i =
              find_indexed_forwarder_v<Decayed::index, decltype(args_tuple)>;
          return std::get<i>(std::move(args_tuple)).forward();
        }
      } else if constexpr (is_guard_v<Decayed>) {
        return std::invoke(std::forward<Arg>(arg).f,
                           std::forward<Args>(args)...);
      } else {
        return std::forward<Arg>(arg);
      }
    }

    template <typename F, typename... Args>
    auto make_guard(F &&f, Args &&... args) {
      return [&](auto &&... args_) {
        return std::invoke(std::forward<F>(f),
                           eval(std::forward<Args>(args),
                                std::forward<decltype(args_)>(args_)...)...);
      };
    }

  }  // namespace detail

#define MPARK_PATTERNS_MEMBER_OPERATORS(type)                                 \
  template <typename Arg>                                                     \
  auto operator=(Arg &&arg) const {                                           \
    auto guard = make_guard(                                                  \
        [](auto &&this_, auto &&arg_) -> decltype(auto) {                     \
          using This_ = decltype(this_);                                      \
          using Arg_ = decltype(arg_);                                        \
          return std::forward<This_>(this_) = std::forward<Arg_>(arg_);       \
        },                                                                    \
        static_cast<const type &>(*this),                                     \
        std::forward<Arg>(arg));                                              \
    return Guard<decltype(guard)>{std::move(guard)};                          \
  }                                                                           \
                                                                              \
  template <typename... Args>                                                 \
  auto operator()(Args &&... args) const {                                    \
    auto guard = make_guard(                                                  \
        [](auto &&this_, auto &&... args_) -> decltype(auto) {                \
          using This_ = decltype(this_);                                      \
          return std::invoke(std::forward<This_>(this_),                      \
                             std::forward<decltype(args_)>(args_)...);        \
        },                                                                    \
        static_cast<const type &>(*this),                                     \
        std::forward<Args>(args)...);                                         \
    return Guard<decltype(guard)>{std::move(guard)};                          \
  }                                                                           \
                                                                              \
  template <typename Arg>                                                     \
  auto operator[](Arg &&arg) const {                                          \
    auto guard = make_guard(                                                  \
        [](auto &&this_, auto &&arg_) -> decltype(auto) {                     \
          using This_ = decltype(this_);                                      \
          using Arg_ = decltype(arg_);                                        \
          if constexpr (std::is_array_v<std::remove_reference_t<This_>>) {    \
            /* For arrays, we handle the forwarding explicitly because     */ \
            /* `std::forward<T>(t)[I]` always yields an lvalue-ref on GCC. */ \
            if constexpr (std::is_rvalue_reference_v<This_>) {                \
              return std::move(this_[std::forward<Arg_>(arg_)]);              \
            } else {                                                          \
              return this_[std::forward<Arg_>(arg_)];                         \
            }                                                                 \
          } else {                                                            \
            return std::forward<This_>(this_)[std::forward<Arg_>(arg_)];      \
          }                                                                   \
        },                                                                    \
        static_cast<const type &>(*this),                                     \
        std::forward<Arg>(arg));                                              \
    return Guard<decltype(guard)>{std::move(guard)};                          \
  }

  namespace detail {

    template <typename F>
    struct Guard {
      MPARK_PATTERNS_MEMBER_OPERATORS(Guard)

      F f;
    };

    template <std::size_t I, typename Pattern>
    struct IdentifierBase {
      using type = Identifier<I, Pattern>;

      MPARK_PATTERNS_MEMBER_OPERATORS(type)

      static constexpr std::size_t index = I;
      static constexpr bool has_pattern = !std::is_void_v<Pattern>;
    };

  }  // namespace detail

  // Identifier Pattern

  template <std::size_t I, typename Pattern>
  struct Identifier : detail::IdentifierBase<I, Pattern> {
    using super = detail::IdentifierBase<I, Pattern>;

    Identifier(const Identifier &) = delete;
    Identifier &operator=(const Identifier &) = delete;

    using super::operator=;
    using super::operator();
    using super::operator[];

    // When this type of identifier is found within a `when` clause, we convert
    // it to the guard since that's what the user must have meant.
    auto as_guard() const {
      return Identifier<I, void>{0}.super::operator()(
          std::forward<Pattern>(pattern));
    }

    Pattern &&pattern;
  };

  template <std::size_t I>
  struct Identifier<I, void> : detail::IdentifierBase<I, void> {
    using super = detail::IdentifierBase<I, void>;

    constexpr Identifier(int) noexcept {}

    Identifier(const Identifier &) = delete;
    Identifier &operator=(const Identifier &) = delete;

    using super::operator=;
    using super::operator();
    using super::operator[];

    template <typename Pattern>
    auto operator()(Pattern &&pattern) const noexcept {
      return Identifier<I, Pattern>{{}, std::forward<Pattern>(pattern)};
    }
  };

  // Wildcard Pattern
  inline constexpr Identifier<wildcard_index, void> _{0};

  // Arg Pattern
  inline constexpr Identifier<arg_index, void> arg{0};

  namespace detail {

    template <std::size_t Head, std::size_t... Tail>
    constexpr auto prepend(std::index_sequence<Tail...>) {
      return std::index_sequence<Head, Tail...>{};
    }

    template <std::size_t... Is>
    constexpr std::tuple<Identifier<Is, void>...> make_identifiers(
        std::index_sequence<Is...>) {
      return {Is...};
    }

    template <std::size_t Bind, std::size_t Discard,
              std::size_t I, typename StrView>
    constexpr auto parse(StrView str_view) {
      constexpr std::string_view sv = str_view();
      if constexpr (I == std::string_view::npos) {
        return std::index_sequence<>{};
      } else {
        constexpr std::size_t comma = sv.find(',', I);
        constexpr std::string_view token = sv.substr(I, comma - I);
        static_assert(token.size() != 0, "expected identifier");
        constexpr std::size_t first = token.find_first_not_of(" \f\n\r\t\v");
        static_assert(first != std::string_view::npos, "expected identifier");
        constexpr std::size_t next =
            comma == std::string_view::npos ? npos : comma + 1;
        if constexpr (token[first] == '_') {
          return prepend<Discard>(parse<Bind, Discard + 1, next>(str_view));
        } else {
          return prepend<Bind>(parse<Bind + 1, Discard, next>(str_view));
        }
      }
    }

  }  // namespace detail

#define IDENTIFIERS(...)                                          \
  auto [__VA_ARGS__] = mpark::patterns::detail::make_identifiers( \
      mpark::patterns::detail::parse<0, arg_index + 1, 0>([] {    \
        using namespace std::literals;                            \
        return #__VA_ARGS__##sv;                                  \
      }))

  template <std::size_t I, typename Pattern, typename Value, typename F>
  auto try_match(const Identifier<I, Pattern> &identifier,
                 Value &&value,
                 F &&f) {
    auto f_ = [&](auto &&... args) {
      return match_invoke(
          std::forward<F>(f),
          detail::indexed_forwarder<I, Value &&>{std::forward<Value>(value)},
          std::forward<decltype(args)>(args)...);
    };
    if constexpr (Identifier<I, Pattern>::has_pattern) {
      return try_match(
          identifier.pattern, std::forward<Value>(value), std::move(f_));
    } else {
      return f_();
    }
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
    auto prepend(Head, lib::list<Tail...>) {
      return lib::list<Head, Tail...>{};
    }

    template <std::size_t P, std::size_t I>
    lib::list<lib::indexed_type<P, std::index_sequence<I>>> insert(lib::list<>);

    template <std::size_t P, std::size_t I,
              std::size_t Q, std::size_t... Is, typename... Tail>
    auto insert(
        lib::list<lib::indexed_type<Q, std::index_sequence<Is...>>, Tail...>) {
      using Head = lib::indexed_type<Q, std::index_sequence<Is...>>;
      if constexpr (P == wildcard_index || P == arg_index) {
        return lib::
            list<Head, Tail..., lib::indexed_type<P, std::index_sequence<I>>>{};
      } else if constexpr (P == Q) {
        return lib::list<lib::indexed_type<Q, std::index_sequence<Is..., I>>,
                         Tail...>{};
      } else {
        return prepend(Head{}, insert<P, I>(lib::list<Tail...>{}));
      }
    }

    template <bool (*)(std::size_t), typename... Ts>
    auto grouped_indices(lib::list<Ts...>,
                         std::index_sequence<>,
                         std::index_sequence<>) {
      return lib::list<typename Ts::type...>{};
    }

    template <bool (*pred)(std::size_t),
              typename... Ts,
              std::size_t P, std::size_t... Ps,
              std::size_t I, std::size_t... Is>
    auto grouped_indices(lib::list<Ts...> result,
                         std::index_sequence<P, Ps...>,
                         std::index_sequence<I, Is...>) {
      return grouped_indices<pred>(
          [&] {
            if constexpr (pred(P)) {
              return insert<P, I>(result);
            } else {
              return result;
            }
          }(),
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
    //     indexed_forwarder<arg_index, int&&>
    //     indexed_forwarder<0, int&&>
    //     indexed_forwarder<1, int&&>
    //   ```
    //
    //   We want to take this sequence of types and return `[[0, 2], [1], [3]]`.
    //   These are the groups of elements (by their indices) that need to
    //   compare equal in order for the pattern to match. Specifically,
    //   the values that the `x` placeholder binds to, at index 0 and 2,
    //   would need to compare equal in order for this pattern to match.
    template <bool (*pred)(std::size_t), typename... Ts>
    using grouped_indices_t =
        decltype(grouped_indices<pred>(lib::list<>{},
                                       std::index_sequence<Ts::index...>{},
                                       std::index_sequence_for<Ts...>{}));

    template <typename T>
    inline constexpr std::size_t front_v = npos;

    template <std::size_t I, std::size_t... Is>
    inline constexpr std::size_t front_v<std::index_sequence<I, Is...>> = I;

    template <typename... GroupedIndices>
    std::index_sequence<front_v<GroupedIndices>...> fronts(
        lib::list<GroupedIndices...>);

    template <typename... Ts>
    using guard_indices_t = decltype(
        fronts(grouped_indices_t<+[](std::size_t) { return true; }, Ts...>{}));

    // Get the indices of the arguments to be passed to the final lambda.
    //
    // Example:
    //   Given placeholders `x`, `y` and
    //   `match(1, 2, 1, 4)(pattern(x, arg, x, y) = f)`,
    //   `grouped_indices_t` returns `[[0, 2], [1], [3]]` (see above).
    //
    //   Given this, the indices of the arguments to be passed to the final
    //   lambda, are the first element of each of the lists. In this case,
    //   We want `[0, 1, 3]`, so that we don't pass the value matched by `x`
    //   (i.e., `1`) multiple times.
    template <typename... Ts>
    using args_indices_t = decltype(
        fronts(grouped_indices_t<+[](std::size_t i) { return i <= arg_index; },
                                 Ts...>{}));

    template <typename... Ts>
    using equals_indices_t = grouped_indices_t<
        +[](std::size_t i) { return i != wildcard_index && i != arg_index; },
        Ts...>;

    template <typename Pattern, typename Rhs>
    struct Case {
      Rhs &&rhs() && noexcept { return std::forward<Rhs>(rhs_); }

      Pattern pattern;
      Rhs &&rhs_;
    };

    template <bool Guarded, typename...>
    struct Pattern;

    template <typename F, typename... Patterns>
    struct Pattern<true, F, Patterns...> {
      static constexpr bool guarded = true;
      static constexpr std::size_t size = sizeof...(Patterns);

      template <typename Rhs>
      auto operator=(Rhs &&rhs) && noexcept {
        return Case<Pattern, Rhs>{std::move(*this), std::forward<Rhs>(rhs)};
      }

      Guard<F> guard;
      Ds<Patterns...> patterns;
    };

    template <typename... Patterns>
    struct Pattern<false, Patterns...> {
      static constexpr bool guarded = false;
      static constexpr std::size_t size = sizeof...(Patterns);

      template <typename Rhs>
      auto operator=(Rhs &&rhs) && noexcept {
        return Case<Pattern, Rhs>{std::move(*this), std::forward<Rhs>(rhs)};
      }

      template <typename F>
      auto when(Guard<F> &&guard) && noexcept {
        return Pattern<true, F, Patterns...>{std::move(guard),
                                             std::move(patterns)};
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
              [&](auto &&... ifs) {
                // The intermediate function that performs the adjustments for
                // placeholder-related functionality and ultimately calls `f`
                // with the final arguments.
                using EqualsIndices =
                    equals_indices_t<std::decay_t<decltype(ifs)>...>;
                using ArgsIndices =
                    args_indices_t<std::decay_t<decltype(ifs)>...>;
                auto args_tuple =
                    std::forward_as_tuple([](auto &&if_) -> auto && {
                      static_assert(
                          is_indexed_forwarder_v<std::decay_t<decltype(if_)>>);
                      static_assert(std::is_rvalue_reference_v<decltype(if_)>);
                      static_assert(std::is_reference_v<decltype(
                                        std::move(if_).forward())>);
                      return std::move(if_).forward();
                    }(std::forward<decltype(ifs)>(ifs))...);
                auto guard = [&] {
                  if constexpr (Pattern::guarded) {
                    return lib::apply(
                        std::move(case_).pattern.guard.f,
                        std::forward_as_tuple(
                            std::forward<decltype(ifs)>(ifs)...),
                        guard_indices_t<std::decay_t<decltype(ifs)>...>{});
                  } else {
                    return true;
                  }
                };
                return equals(args_tuple, EqualsIndices{}) && guard()
                           ? match_apply(std::move(case_).rhs(),
                                         std::move(args_tuple),
                                         ArgsIndices{})
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
    return detail::Pattern<false, Patterns...>{ds(patterns...)};
  }

  template <typename R = detail::Deduce, typename... Values>
  auto match(Values &&... values) noexcept {
    return detail::Match<R, Values...>{
        std::forward_as_tuple(std::forward<Values>(values)...)};
  }

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_MATCH_HPP
