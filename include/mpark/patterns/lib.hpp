// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_LIB_HPP
#define MPARK_PATTERNS_LIB_HPP

#include <utility>

namespace mpark {
  namespace patterns {
    namespace lib {

      template <typename T>
      struct identity { using type = T; };

      inline namespace cpp17 {

        namespace detail {

          template <typename Void, typename, typename...>
          struct invoke_result {};

          template <typename F, typename... Args>
          struct invoke_result<
              std::void_t<decltype(
                  std::invoke(std::declval<F>(), std::declval<Args>()...))>,
              F,
              Args...>
              : identity<decltype(std::invoke(std::declval<F>(),
                                              std::declval<Args>()...))> {};

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

        template <typename R, typename F, typename... Args>
        using is_invocable_r = detail::is_invocable_r<void, R, F, Args...>;

      }  // namespace cpp17

    }  // namespace lib
  }  // namespace patterns
}  // namespace mpark

#endif  // MPARK_PATTERNS_LIB_HPP
