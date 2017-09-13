// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_HPP
#define MPARK_PATTERNS_HPP

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

// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_CONFIG_HPP
#define MPARK_PATTERNS_CONFIG_HPP

#if __cplusplus < 201703L
#error "MPark.Patterns requires C++17 support."
#endif

#ifndef __has_feature
#define __has_feature(x) 0
#endif

#if __has_feature(cxx_exceptions) || defined(__cpp_exceptions) || \
    (defined(_MSC_VER) && defined(_CPPUNWIND))
#define MPARK_PATTERNS_EXCEPTIONS
#endif

#endif  // MPARK_PATTERNS_CONFIG_HPP

// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_DETAIL_AS_TUPLE_HPP
#define MPARK_PATTERNS_DETAIL_AS_TUPLE_HPP

#include <cstddef>
#include <tuple>
#include <utility>

// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_LIB_HPP
#define MPARK_PATTERNS_LIB_HPP

#include <cstddef>
#include <functional>
#include <type_traits>
#include <utility>

namespace mpark::patterns::lib {

  template <typename T>
  struct identity { using type = T; };

  inline namespace cpp17 {

    namespace detail {

      template <typename Void, typename, typename...>
      struct invoke_result {};

      template <typename F, typename... Args>
      struct invoke_result<decltype(void(std::invoke(std::declval<F>(),
                                                     std::declval<Args>()...))),
                           F,
                           Args...>
          : identity<decltype(
                std::invoke(std::declval<F>(), std::declval<Args>()...))> {};

    }  // namespace detail

    template <typename F, typename... Args>
    using invoke_result = detail::invoke_result<void, F, Args...>;

    template <typename F, typename... Args>
    using invoke_result_t = typename invoke_result<F, Args...>::type;

    namespace detail {

      template <typename Void, typename, typename...>
      struct is_invocable : std::false_type {};

      template <typename F, typename... Args>
      struct is_invocable<std::void_t<invoke_result_t<F, Args...>>,
                          F,
                          Args...> : std::true_type {};

      template <typename Void, typename, typename, typename...>
      struct is_invocable_r : std::false_type {};

      template <typename R, typename F, typename... Args>
      struct is_invocable_r<std::void_t<invoke_result_t<F, Args...>>,
                            R,
                            F,
                            Args...>
          : std::is_convertible<invoke_result_t<F, Args...>, R> {};

    }  // namespace detail

    template <typename F, typename... Args>
    using is_invocable = detail::is_invocable<void, F, Args...>;

    template <typename F, typename... Args>
    inline constexpr bool is_invocable_v = is_invocable<F, Args...>::value;

    template <typename R, typename F, typename... Args>
    using is_invocable_r = detail::is_invocable_r<void, R, F, Args...>;

    template <typename R, typename F, typename... Args>
    inline constexpr bool is_invocable_r_v = is_invocable_r<R, F, Args...>::value;

  }  // namespace cpp17

  template <std::size_t I>
  using size_constant = std::integral_constant<std::size_t, I>;

  template <std::size_t N = 0>
  struct priority : priority<N + 1> {};

  template <>
  struct priority<4> {};

}  // namespace mpark::patterns::lib

#endif  // MPARK_PATTERNS_LIB_HPP

// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_DETAIL_QUALIFY_AS_HPP
#define MPARK_PATTERNS_DETAIL_QUALIFY_AS_HPP

namespace mpark::patterns::detail {

  template <typename T, typename U>
  struct qualify_as : lib::identity<T> {};

  template <typename T, typename U>
  using qualify_as_t = typename qualify_as<T, U>::type;

  template <typename T, typename U>
  struct qualify_as<T, U &> : lib::identity<qualify_as_t<T, U> &> {};

  template <typename T, typename U>
  struct qualify_as<T, U &&> : lib::identity<qualify_as_t<T, U> &&> {};

  template <typename T, typename U>
  struct qualify_as<T, const U> : lib::identity<const qualify_as_t<T, U>> {};

  template <typename T, typename U>
  struct qualify_as<T, U volatile>
      : lib::identity<qualify_as_t<T, U> volatile> {};

  template <typename T, typename U>
  struct qualify_as<T, const U volatile>
      : lib::identity<const qualify_as_t<T, U> volatile> {};

}  // namespace mpark::patterns::detail

#endif  // MPARK_PATTERNS_DETAIL_QUALIFY_AS_HPP


namespace mpark::patterns::detail {

  struct fill {
    template <typename T>
    constexpr operator T &() const noexcept;
  };

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
  template <typename T,
            std::size_t... Is,
            typename = decltype(T{(Is, fill{})...})>
  constexpr bool is_n_constructible_impl(std::index_sequence<Is...>,
                                         lib::priority<0>) {
    return true;
  }
#pragma GCC diagnostic pop

  template <typename T, std::size_t... Is>
  constexpr bool is_n_constructible_impl(std::index_sequence<Is...>,
                                         lib::priority<1>) {
    return false;
  }

  template <typename T, std::size_t N>
  constexpr bool is_n_constructible() {
    return is_n_constructible_impl<T>(std::make_index_sequence<N>{},
                                      lib::priority<>{});
  }

  template <typename T>
  struct AggregateSize {
    template <std::size_t B, std::size_t E>
    static constexpr std::optional<std::size_t> impl() {
      constexpr std::size_t M = B + ((E - B) / 2);
      constexpr bool is_mid_constructible = is_n_constructible<T, M>();
      if constexpr (B == M) {
        if constexpr (is_mid_constructible) {
          return M;
        } else {
          return std::nullopt;
        }
      } else if constexpr (is_mid_constructible) {
        // We recursve into `[M, E)` rather than `[M + 1, E)` since `M` could be
        // the answer.
        return impl<M, E>();
      } else if constexpr (constexpr auto lhs = impl<B, M>()) {
        return lhs;
      } else if constexpr (constexpr auto rhs = impl<M, E>()) {
        return rhs;
      }
    }
  };

  template <typename T>
  inline constexpr std::size_t aggregate_size_v =
      *AggregateSize<T>::template impl<0, sizeof(T) + 1>();

  template <typename T>
  struct aggregate_size : lib::size_constant<aggregate_size_v<T>> {};

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&, lib::size_constant<0>) {
    return std::forward_as_tuple();
  }

#define FWD(x) static_cast<qualify_as_t<decltype(x), T>>(x)

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&aggregate, lib::size_constant<1>) {
    using T = decltype(std::forward<Aggregate>(aggregate));
    auto && [x00] =
        std::forward<Aggregate>(aggregate);
    return std::forward_as_tuple(FWD(x00));
  }

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&aggregate, lib::size_constant<2>) {
    using T = decltype(std::forward<Aggregate>(aggregate));
    auto && [x00, x01] =
        std::forward<Aggregate>(aggregate);
    return std::forward_as_tuple(FWD(x00), FWD(x01));
  }

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&aggregate, lib::size_constant<3>) {
    using T = decltype(std::forward<Aggregate>(aggregate));
    auto && [x00, x01, x02] =
        std::forward<Aggregate>(aggregate);
    return std::forward_as_tuple(FWD(x00), FWD(x01), FWD(x02));
  }

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&aggregate, lib::size_constant<4>) {
    using T = decltype(std::forward<Aggregate>(aggregate));
    auto && [x00, x01, x02, x03] =
        std::forward<Aggregate>(aggregate);
    return std::forward_as_tuple(FWD(x00), FWD(x01), FWD(x02), FWD(x03));
  }

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&aggregate, lib::size_constant<5>) {
    using T = decltype(std::forward<Aggregate>(aggregate));
    auto && [x00, x01, x02, x03, x04] =
        std::forward<Aggregate>(aggregate);
    return std::forward_as_tuple(FWD(x00), FWD(x01), FWD(x02), FWD(x03),
                                 FWD(x04));
  }

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&aggregate, lib::size_constant<6>) {
    using T = decltype(std::forward<Aggregate>(aggregate));
    auto && [x00, x01, x02, x03, x04, x05] =
        std::forward<Aggregate>(aggregate);
    return std::forward_as_tuple(FWD(x00), FWD(x01), FWD(x02), FWD(x03),
                                 FWD(x04), FWD(x05));
  }

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&aggregate, lib::size_constant<7>) {
    using T = decltype(std::forward<Aggregate>(aggregate));
    auto && [x00, x01, x02, x03, x04, x05, x06] =
        std::forward<Aggregate>(aggregate);
    return std::forward_as_tuple(FWD(x00), FWD(x01), FWD(x02), FWD(x03),
                                 FWD(x04), FWD(x05), FWD(x06));
  }

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&aggregate, lib::size_constant<8>) {
    using T = decltype(std::forward<Aggregate>(aggregate));
    auto && [x00, x01, x02, x03, x04, x05, x06, x07] =
        std::forward<Aggregate>(aggregate);
    return std::forward_as_tuple(FWD(x00), FWD(x01), FWD(x02), FWD(x03),
                                 FWD(x04), FWD(x05), FWD(x06), FWD(x07));
  }

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&aggregate, lib::size_constant<9>) {
    using T = decltype(std::forward<Aggregate>(aggregate));
    auto && [x00, x01, x02, x03, x04, x05, x06, x07,
             x08] =
        std::forward<Aggregate>(aggregate);
    return std::forward_as_tuple(FWD(x00), FWD(x01), FWD(x02), FWD(x03),
                                 FWD(x04), FWD(x05), FWD(x06), FWD(x07),
                                 FWD(x08));
  }

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&aggregate, lib::size_constant<10>) {
    using T = decltype(std::forward<Aggregate>(aggregate));
    auto && [x00, x01, x02, x03, x04, x05, x06, x07,
             x08, x09] =
        std::forward<Aggregate>(aggregate);
    return std::forward_as_tuple(FWD(x00), FWD(x01), FWD(x02), FWD(x03),
                                 FWD(x04), FWD(x05), FWD(x06), FWD(x07),
                                 FWD(x08), FWD(x09));
  }

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&aggregate, lib::size_constant<11>) {
    using T = decltype(std::forward<Aggregate>(aggregate));
    auto && [x00, x01, x02, x03, x04, x05, x06, x07,
             x08, x09, x10] =
        std::forward<Aggregate>(aggregate);
    return std::forward_as_tuple(FWD(x00), FWD(x01), FWD(x02), FWD(x03),
                                 FWD(x04), FWD(x05), FWD(x06), FWD(x07),
                                 FWD(x08), FWD(x09), FWD(x10));
  }

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&aggregate, lib::size_constant<12>) {
    using T = decltype(std::forward<Aggregate>(aggregate));
    auto && [x00, x01, x02, x03, x04, x05, x06, x07,
             x08, x09, x10, x11] =
        std::forward<Aggregate>(aggregate);
    return std::forward_as_tuple(FWD(x00), FWD(x01), FWD(x02), FWD(x03),
                                 FWD(x04), FWD(x05), FWD(x06), FWD(x07),
                                 FWD(x08), FWD(x09), FWD(x10), FWD(x11));
  }

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&aggregate, lib::size_constant<13>) {
    using T = decltype(std::forward<Aggregate>(aggregate));
    auto && [x00, x01, x02, x03, x04, x05, x06, x07,
             x08, x09, x10, x11, x12] =
        std::forward<Aggregate>(aggregate);
    return std::forward_as_tuple(FWD(x00), FWD(x01), FWD(x02), FWD(x03),
                                 FWD(x04), FWD(x05), FWD(x06), FWD(x07),
                                 FWD(x08), FWD(x09), FWD(x10), FWD(x11),
                                 FWD(x12));
  }

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&aggregate, lib::size_constant<14>) {
    using T = decltype(std::forward<Aggregate>(aggregate));
    auto && [x00, x01, x02, x03, x04, x05, x06, x07,
             x08, x09, x10, x11, x12, x13] =
        std::forward<Aggregate>(aggregate);
    return std::forward_as_tuple(FWD(x00), FWD(x01), FWD(x02), FWD(x03),
                                 FWD(x04), FWD(x05), FWD(x06), FWD(x07),
                                 FWD(x08), FWD(x09), FWD(x10), FWD(x11),
                                 FWD(x12), FWD(x13));
  }

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&aggregate, lib::size_constant<15>) {
    using T = decltype(std::forward<Aggregate>(aggregate));
    auto && [x00, x01, x02, x03, x04, x05, x06, x07,
             x08, x09, x10, x11, x12, x13, x14] =
        std::forward<Aggregate>(aggregate);
    return std::forward_as_tuple(FWD(x00), FWD(x01), FWD(x02), FWD(x03),
                                 FWD(x04), FWD(x05), FWD(x06), FWD(x07),
                                 FWD(x08), FWD(x09), FWD(x10), FWD(x11),
                                 FWD(x12), FWD(x13), FWD(x14));
  }

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&aggregate, lib::size_constant<16>) {
    using T = decltype(std::forward<Aggregate>(aggregate));
    auto && [x00, x01, x02, x03, x04, x05, x06, x07,
             x08, x09, x10, x11, x12, x13, x14, x15] =
        std::forward<Aggregate>(aggregate);
    return std::forward_as_tuple(FWD(x00), FWD(x01), FWD(x02), FWD(x03),
                                 FWD(x04), FWD(x05), FWD(x06), FWD(x07),
                                 FWD(x08), FWD(x09), FWD(x10), FWD(x11),
                                 FWD(x12), FWD(x13), FWD(x14), FWD(x15));
  }

#undef FWD

  template <typename Aggregate>
  auto as_tuple(Aggregate &&aggregate) {
    return as_tuple_impl(std::forward<Aggregate>(aggregate),
                         aggregate_size<std::decay_t<Aggregate>>{});
  }

}  // namespace mpark::patterns::detail

#endif  // MPARK_PATTERNS_DETAIL_AS_TUPLE_HPP

// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_DETAIL_FORWARDER_HPP
#define MPARK_PATTERNS_DETAIL_FORWARDER_HPP

#include <type_traits>
#include <utility>

namespace mpark::patterns::detail {

  struct Void {};

  template <typename T>
  class Forwarder {
    private:
    template <typename U, typename Qualified>
    static constexpr bool is_enabled() {
      return std::is_constructible_v<T, Qualified> &&
             !(std::is_constructible_v<T, Forwarder<U> &> ||
               std::is_constructible_v<T, const Forwarder<U> &> ||
               std::is_constructible_v<T, Forwarder<U> &&> ||
               std::is_constructible_v<T, const Forwarder<U> &&> ||
               std::is_convertible_v<Forwarder<U> &, T> ||
               std::is_convertible_v<const Forwarder<U> &, T> ||
               std::is_convertible_v<Forwarder<U> &&, T> ||
               std::is_convertible_v<const Forwarder<U> &&, T>);
    }

    template <typename U, typename Qualified>
    static constexpr bool enable_explicit() {
      return is_enabled<U, Qualified>() && !std::is_convertible_v<Qualified, T>;
    }

    template <typename U, typename Qualified>
    static constexpr bool enable_implicit() {
      return is_enabled<U, Qualified>() && std::is_convertible_v<Qualified, T>;
    }

    public:
    constexpr Forwarder(T &&value) : value_(std::forward<T>(value)) {}

    template <typename U,
              std::enable_if_t<enable_explicit<U, const U &>(), int> = 0>
    explicit Forwarder(const Forwarder<U> &that) : value_(that.forward()) {}

    template <typename U,
              std::enable_if_t<enable_implicit<U, const U &>(), int> = 0>
    Forwarder(const Forwarder<U> &that) : value_(that.forward()) {}

    template <typename U, std::enable_if_t<enable_explicit<U, U &&>(), int> = 0>
    explicit Forwarder(Forwarder<U> &&that)
        : value_(std::move(that).forward()) {}

    template <typename U, std::enable_if_t<enable_implicit<U, U &&>(), int> = 0>
    Forwarder(Forwarder<U> &&that) : value_(std::move(that).forward()) {}

    Forwarder(const Forwarder &) = default;
    Forwarder(Forwarder &&) = default;

    Forwarder &operator=(const Forwarder &) = delete;
    Forwarder &operator=(Forwarder &&) = delete;

    constexpr T forward() && { return std::forward<T>(value_); }

    private:
    T value_;
  };

  template <>
  class Forwarder<void> {
    public:
    constexpr Forwarder(Void) noexcept {}

    template <typename U>
    Forwarder(const Forwarder<U> &) noexcept {}

    template <typename U>
    Forwarder(Forwarder<U> &&) noexcept {}

    Forwarder(const Forwarder &) noexcept = default;
    Forwarder(Forwarder &&) noexcept = default;

    Forwarder &operator=(const Forwarder &) noexcept = delete;
    Forwarder &operator=(Forwarder &&) noexcept = delete;

    constexpr void forward() && noexcept {}
  };

}  // namespace mpark::patterns::detail

#endif  // MPARK_PATTERNS_DETAIL_FORWARDER_HPP


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

    template <typename Pattern>
    inline constexpr bool is_variadic_v = false;

    template <typename Pattern>
    inline constexpr bool is_variadic_v<Variadic<Pattern>> = true;

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
    auto matches_impl(const Anyof<Patterns...> &anyof,
                      Value &&value,
                      F &&f,
                      std::index_sequence<I>) {
      return matches(std::get<I>(anyof.patterns),
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
    auto matches_impl(const Anyof<Patterns...> &anyof,
                      Value &&value,
                      F &&f,
                      std::index_sequence<I, J, Js...>) {
      auto result = matches(std::get<I>(anyof.patterns),
                            std::forward<Value>(value),
                            [&](auto &&... args) {
                              return match_invoke(
                                  std::forward<F>(f),
                                  std::forward<decltype(args)>(args)...);
                            });
      return result ? std::move(result)
                    : matches_impl(anyof,
                                   std::forward<Value>(value),
                                   std::forward<F>(f),
                                   std::index_sequence<J, Js...>{});
    }

  }  // namespace detail

  template <typename... Patterns, typename Value, typename F>
  auto matches(const Anyof<Patterns...> &anyof, Value &&value, F &&f) {
    return detail::matches_impl(anyof,
                                std::forward<Value>(value),
                                std::forward<F>(f),
                                std::index_sequence_for<Patterns...>{});
  }

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_ANYOF_HPP

// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_BIND_HPP
#define MPARK_PATTERNS_BIND_HPP

#include <utility>

// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_WILDCARD_HPP
#define MPARK_PATTERNS_WILDCARD_HPP

#include <utility>

namespace mpark::patterns {

  inline constexpr struct Wildcard {} _{};

  template <typename Value, typename F>
  auto matches(Wildcard, Value &&, F &&f) {
    return match_invoke(std::forward<F>(f));
  }

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_WILDCARD_HPP


namespace mpark::patterns {

  template <typename Pattern>
  struct Arg { const Pattern &pattern; };

  template <>
  struct Arg<Wildcard> {
    template <typename Pattern>
    auto operator()(const Pattern &pattern_) const noexcept {
      return Arg<Pattern>{pattern_};
    }

    const Wildcard &pattern = _;
  };

  using placeholder = Arg<Wildcard>;

  inline constexpr Arg<Wildcard> arg{};

  template <typename Pattern, typename Value, typename F>
  auto matches(const Arg<Pattern> &arg_, Value &&value, F &&f) {
    return matches(
        arg_.pattern, std::forward<Value>(value), [&](auto &&... args) {
          return match_invoke(std::forward<F>(f),
                              std::forward<Value>(value),
                              std::forward<decltype(args)>(args)...);
        });
  }

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_BIND_HPP

// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_OPTIONAL_HPP
#define MPARK_PATTERNS_OPTIONAL_HPP

#include <utility>

namespace mpark::patterns {

  inline constexpr struct None {} none{};

  template <typename Value, typename F>
  auto matches(None, Value &&value, F &&f) {
    return value ? no_match : match_invoke(std::forward<F>(f));
  }

  template <typename Pattern>
  struct Some { const Pattern &pattern; };

  template <typename Pattern>
  auto some(const Pattern &pattern) { return Some<Pattern>{pattern}; }

  template <typename Pattern, typename Value, typename F>
  auto matches(const Some<Pattern> &some, Value &&value, F &&f) {
    return value ? matches(some.pattern,
                           *std::forward<Value>(value),
                           std::forward<F>(f))
                 : no_match;
  }

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_OPTIONAL_HPP

// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_REGEX_HPP
#define MPARK_PATTERNS_REGEX_HPP

#include <cstddef>
#include <iterator>
#include <regex>
#include <string_view>
#include <tuple>
#include <utility>

namespace mpark::patterns {

  struct RegexMatch {
    template <typename... Args>
    bool operator()(Args &&... args) const {
      return std::regex_match(std::forward<Args>(args)...);
    }
  };

  struct RegexSearch {
    template <typename... Args>
    bool operator()(Args &&... args) const {
      return std::regex_search(std::forward<Args>(args)...);
    }
  };

  inline constexpr struct SubMatches {} sub_matches{};

  /*
  template <typename... Patterns>
  struct Groups { std::tuple<const Patterns &...> patterns; };

  template <typename... Patterns>
  auto groups(const Patterns &... patterns) noexcept {
    return Groups<Patterns...>{std::tie(patterns...)};
  }
  */

  template <typename RegexF, typename = void>
  struct Regex { std::regex regex; };

  template <typename RegexF>
  struct Regex<RegexF, SubMatches> { std::regex regex; };

  /*
  template <typename RegexF, typename... Patterns>
  struct Regex<RegexF, Groups<Patterns...>> {
    std::regex regex;
    const Groups<Patterns...> &groups;
  };
  */

  struct Re {
    auto match(std::regex regex) const {
      return Regex<RegexMatch>{std::move(regex)};
    }

    auto match(std::regex regex, SubMatches) const {
      return Regex<RegexMatch, SubMatches>{std::move(regex)};
    }

    /*
    template <typename... Patterns>
    auto match(std::regex regex, const Groups<Patterns...> &groups) const {
      return Regex<RegexMatch, Groups<Patterns...>>{std::move(regex), groups};
    }
    */

    template <typename... Args>
    auto match(std::string_view sv, Args &&... args) const {
      return match(std::regex(std::begin(sv), std::end(sv)),
                   std::forward<Args>(args)...);
    }

    auto search(std::regex regex) const {
      return Regex<RegexSearch>{std::move(regex)};
    }

    auto search(std::regex regex, SubMatches) const {
      return Regex<RegexSearch, SubMatches>{std::move(regex)};
    }

    /*
    template <typename... Patterns>
    auto search(std::regex regex, const Groups<Patterns...> &groups) const {
      return Regex<RegexSearch, Groups<Patterns...>>{std::move(regex), groups};
    }
    */

    template <typename... Args>
    auto search(std::string_view sv, Args &&... args) const {
      return search(std::regex(std::begin(sv), std::end(sv)),
                    std::forward<Args>(args)...);
    }
  };

  inline constexpr Re re{};

  template <typename RegexF, typename F>
  auto matches(const Regex<RegexF> &r, const char *s, F &&f) {
    return RegexF{}(s, r.regex) ? match_invoke(std::forward<F>(f)) : no_match;
  }

  template <typename RegexF,
            typename Value,
            typename F,
            std::enable_if_t<!std::is_same_v<std::decay_t<Value>, const char *>,
                             int> = 0>
  auto matches(const Regex<RegexF> &r, Value &&value, F &&f) {
    return RegexF{}(std::cbegin(value), std::cend(value), r.regex)
               ? match_invoke(std::forward<F>(f))
               : no_match;
  }

  template <typename RegexF, typename F>
  auto matches(const Regex<RegexF, SubMatches> &r, const char *s, F &&f) {
    std::cmatch results;
    return RegexF{}(s, results, r.regex)
               ? match_invoke(std::forward<F>(f), std::move(results))
               : no_match;
  }

  template <typename RegexF,
            typename Value,
            typename F,
            std::enable_if_t<!std::is_same_v<std::decay_t<Value>, const char *>,
                             int> = 0>
  auto matches(const Regex<RegexF, SubMatches> &r, Value &&value, F &&f) {
    std::match_results<typename std::decay_t<Value>::const_iterator> results;
    return RegexF{}(std::cbegin(value), std::cend(value), results, r.regex)
               ? match_invoke(std::forward<F>(f), std::move(results))
               : no_match;
  }

  /*
  namespace detail {

    template <typename BiIter, std::size_t... Is>
    auto as_tuple(std::match_results<BiIter> &&results,
                  std::index_sequence<Is...>) {
      // Ignore the first element which is the full match.
      return std::make_tuple(std::move(results)[Is + 1]...);
    }

  }  // namespace detail

  template <typename RegexF, typename... Patterns, typename F>
  auto matches(const Regex<RegexF, Groups<Patterns...>> &r,
               const char *s,
               F &&f) {
    std::cmatch results;
    return RegexF{}(s, results, r.regex)
               ? match(Prod{r.groups.patterns},
                       as_tuple(std::move(results),
                                std::index_sequence_for<Patterns...>{}),
                       std::forward<F>(f))
               : no_match;
  }

  template <typename RegexF,
            typename... Patterns,
            typename Value,
            typename F,
            std::enable_if_t<!std::is_same_v<std::decay_t<Value>, const char *>,
                             int> = 0>
  auto matches(const Regex<RegexF, Groups<Patterns...>> &r,
               Value &&value,
               F &&f) {
    std::match_results<typename std::decay_t<Value>::const_iterator> results;
    return RegexF{}(std::cbegin(value), std::cend(value), results, r.regex)
               ? match(Prod{r.groups.patterns},
                       as_tuple(std::move(results),
                                std::index_sequence_for<Patterns...>{}),
                       std::forward<F>(f))
               : no_match;
  }
  */

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_REGEX_HPP

// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_SUM_HPP
#define MPARK_PATTERNS_SUM_HPP

#include <type_traits>
#include <utility>
#include <variant>

namespace mpark::patterns {

  namespace detail { struct Visit; }

  template <typename T, typename Pattern>
  struct SumByType { const Pattern &pattern; };

  template <typename Pattern>
  struct SumByVisit { const Pattern &pattern; };

  template <typename T = detail::Visit, typename Pattern>
  auto sum(const Pattern &pattern) noexcept {
    using Sum = std::conditional_t<std::is_same_v<T, detail::Visit>,
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
        [&](auto &&v) {
          return matches(
              sum.pattern, std::forward<decltype(v)>(v), std::forward<F>(f));
        },
        std::forward<Value>(value));
  }

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_SUM_HPP

// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_WHEN_HPP
#define MPARK_PATTERNS_WHEN_HPP

#include <utility>

namespace mpark::patterns {

  struct When { bool condition; };

  template <typename F>
  auto operator>>=(When when, F &&f) {
    return when.condition ? match_invoke(std::forward<F>(f)) : no_match;
  }

#define WHEN(condition) return When{condition} >>= [&]

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_WHEN_HPP


#endif  // MPARK_PATTERNS_HPP
