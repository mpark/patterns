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
#define RETURN(...)                                          \
  noexcept(noexcept(__VA_ARGS__)) -> decltype(__VA_ARGS__) { \
    return __VA_ARGS__;                                      \
  }

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

#undef RETURN

      }  // namespace cpp17

    }  // namespace lib
  }  // namespace patterns
}  // namespace mpark

#endif  // MPARK_PATTERNS_LIB_HPP
