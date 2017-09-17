// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_AS_HPP
#define MPARK_PATTERNS_AS_HPP

#include <type_traits>
#include <utility>
#include <variant>

#include "lib.hpp"

namespace mpark::patterns {

  namespace detail {

    template <typename T,
              typename V,
              typename = decltype(std::declval<V>().template get_if<T>())>
    constexpr bool has_get_if_member(lib::priority<0>) noexcept { return true; }

    template <typename T, typename V>
    constexpr bool has_get_if_member(lib::priority<1>) noexcept { return false; }

    template <typename T, typename V>
    inline constexpr bool has_get_if_member_v =
        has_get_if_member<T, V>(lib::priority<>{});

  }  // namespace detail

  template <typename T, typename Pattern>
  struct As { const Pattern &pattern; };

  template <typename T, typename Pattern>
  auto as(const Pattern &pattern) noexcept { return As<T, Pattern>{pattern}; }

  template <typename T,
            std::enable_if_t<std::is_class_v<T>, int> = 0,
            std::size_t = sizeof(std::variant_size<T>)>
  constexpr bool is_variant_like(lib::priority<0>) noexcept { return true; }

  template <typename T>
  constexpr bool is_variant_like(lib::priority<1>) noexcept { return false; }

  template <typename T>
  inline constexpr bool is_variant_like_v =
      is_variant_like<T>(lib::priority<>{});

  template <typename T, typename Pattern, typename Value, typename F>
  auto try_match(const As<T, Pattern> &as, Value &&value, F &&f) {
    auto &&v = [&]() -> decltype(auto) {
      if constexpr (std::is_polymorphic_v<std::decay_t<Value>>) {
        return dynamic_cast<
            std::add_pointer_t<detail::qualify_as_t<T, Value &&>>>(
            std::addressof(value));
      } else if constexpr (is_variant_like_v<std::decay_t<Value>>) {
        if constexpr (detail::has_get_if_member_v<T, Value>) {
          return std::forward<Value>(value).template get_if<T>();
        } else {
          using std::get_if;
          return get_if<T>(std::addressof(value));
        }
      } else {
        static_assert(lib::false_v<Value>,
                      "The value attempting to be matched against an `as` "
                      "pattern is not polymorphic nor variant-like.");
      }
    }();
    return v ? try_match(as.pattern,
                         static_cast<detail::qualify_as_t<
                             decltype(*std::forward<decltype(v)>(v)),
                             Value &&>>(*std::forward<decltype(v)>(v)),
                         std::forward<F>(f))
             : no_match;
  }

}  // namespace mpark::patterns

#endif  // MPARK_PATTERNS_AS_HPP
