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
    /* inline */ constexpr bool is_invocable_v = is_invocable<F, Args...>::value;

    template <typename R, typename F, typename... Args>
    using is_invocable_r = detail::is_invocable_r<void, R, F, Args...>;

    template <typename R, typename F, typename... Args>
    /* inline */ constexpr bool is_invocable_r_v = is_invocable_r<R, F, Args...>::value;

  }  // namespace cpp17

  template <std::size_t I>
  using size_constant = std::integral_constant<std::size_t, I>;

  template <std::size_t N = 0>
  struct priority : priority<N + 1> {};

  template <>
  struct priority<4> {};

}  // namespace mpark::patterns::lib

#endif  // MPARK_PATTERNS_LIB_HPP
