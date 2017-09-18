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

  template <typename F, typename Tuple, std::size_t... Is>
  decltype(auto) apply(F &&f, Tuple &&tuple, std::index_sequence<Is...>) {
    return std::invoke(std::forward<F>(f),
                       std::get<Is>(std::forward<Tuple>(tuple))...);
  }

  // `string_view::find` is not `constexpr` on GCC 7.
  inline constexpr std::size_t find(std::string_view this_,
                                    char ch,
                                    std::size_t begin = 0) {
    for (std::size_t i = begin; i < this_.size(); ++i) {
      if (this_[i] == ch) {
        return i;
      }
    }
    return std::string_view::npos;
  }

  // `string_view::find_first_not_of` is not `constexpr` on GCC 7.
  inline constexpr std::size_t find_first_not_of(std::string_view this_,
                                                 std::string_view sv) {
    for (std::size_t i = 0; i < this_.size(); ++i) {
      std::size_t index = find(sv, this_[i]);
      if (index == std::string_view::npos) {
        return i;
      }
    }
    return std::string_view::npos;
  }

  template <typename... Ts>
  inline constexpr bool false_v = false;

  template <typename... Ts>
  struct list {};

  template <std::size_t I, typename T>
  struct indexed_type : identity<T> {};

  template <std::size_t I>
  using size_constant = std::integral_constant<std::size_t, I>;

  template <std::size_t N = 0>
  struct priority : priority<N + 1> {};

  template <>
  struct priority<4> {};

}  // namespace mpark::patterns::lib

#endif  // MPARK_PATTERNS_LIB_HPP
