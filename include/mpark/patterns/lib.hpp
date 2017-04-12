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

        template <typename...>
        using void_t = void;

        // <functional>
#define RETURN(...) -> decltype(__VA_ARGS__) { return __VA_ARGS__; }

        template <typename F, typename... As>
        inline constexpr auto invoke(F &&f, As &&... as)
            RETURN(std::forward<F>(f)(std::forward<As>(as)...))

        template <typename B, typename T, typename D>
        inline constexpr auto invoke(T B::*pmv, D &&d)
            RETURN(std::forward<D>(d).*pmv)

        template <typename Pmv, typename Ptr>
        inline constexpr auto invoke(Pmv pmv, Ptr &&ptr)
            RETURN((*std::forward<Ptr>(ptr)).*pmv)

        template <typename B, typename T, typename D, typename... As>
        inline constexpr auto invoke(T B::*pmf, D &&d, As &&... as)
            RETURN((std::forward<D>(d).*pmf)(std::forward<As>(as)...))

        template <typename Pmf, typename Ptr, typename... As>
        inline constexpr auto invoke(Pmf pmf, Ptr &&ptr, As &&... as)
            RETURN(((*std::forward<Ptr>(ptr)).*pmf)(std::forward<As>(as)...))

        template <class F, class Tuple, std::size_t... Is>
        inline constexpr auto apply_impl(F &&f,
                                         Tuple &&tuple,
                                         std::index_sequence<Is...>)
            RETURN(lib::invoke(std::forward<F>(f),
                               std::get<Is>(std::forward<Tuple>(tuple))...))

        template <class F, class Tuple>
        inline constexpr auto apply(F &&f, Tuple &&tuple)
            RETURN(apply_impl(std::forward<F>(f),
                              std::forward<Tuple>(tuple),
                              std::make_index_sequence<
                                  std::tuple_size<std::decay_t<Tuple>>::value>{}))
  #undef RETURN

      }  // namespace cpp17

      struct Deduce;

      template <typename R>
      struct Apply {
        template <typename F, typename Tuple>
        inline R operator()(F &&f, Tuple &&tuple) const {
          return lib::apply(std::forward<F>(f), std::forward<Tuple>(tuple));
        }
      };

      template <>
      struct Apply<Deduce> {
        template <typename F, typename Tuple>
        inline decltype(auto) operator()(F &&f, Tuple &&tuple) const {
          return lib::apply(std::forward<F>(f), std::forward<Tuple>(tuple));
        }
      };

      template <typename R = Deduce, typename F, typename Tuple>
      inline decltype(auto) apply_r(F &&f, Tuple &&tuple) {
        return Apply<R>{}(std::forward<F>(f), std::forward<Tuple>(tuple));
      }

      /*
      namespace detail {

        template <typename Void, typename, typename>
        struct apply_result {};

        template <typename F, typename Tuple>
        struct apply_result<void_t<decltype(lib::apply(std::declval<F>(),
                                                       std::declval<Tuple>()))>,
                            F,
                            Tuple>
            : identity<decltype(
                  lib::apply(std::declval<F>(), std::declval<Tuple>()))> {};

      }  // namespace detail

      template <typename F, typename Tuple>
      using apply_result = detail::apply_result<void, F, Tuple>;

      template <typename F, typename Tuple>
      using apply_result_t = typename apply_result<F, Tuple>::type;

      namespace detail {

        template <typename Void, typename, typename>
        struct is_applicable : std::false_type {};

        template <typename F, typename Tuple>
        struct is_applicable<void_t<apply_result_t<F, Tuple>>, F, Tuple>
            : std::true_type {};

        template <typename Void, typename, typename, typename>
        struct is_applicable_r : std::false_type {};

        template <typename R, typename F, typename Tuple>
        struct is_applicable_r<void_t<apply_result_t<F, Tuple>>, R, F, Tuple>
            : std::is_convertible<apply_result_t<F, Tuple>, R> {};

      }  // namespace detail

      template <typename F, typename Tuple>
      using is_applicable = detail::is_applicable<void, F, Tuple>;

      template <typename R, typename F, typename Tuple>
      using is_applicable_r = detail::is_applicable_r<void, R, F, Tuple>;
      */

    }  // namespace lib
  }  // namespace patterns
}  // namespace mpark

#endif  // MPARK_PATTERNS_LIB_HPP
