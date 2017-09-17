// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_AS_HPP
#define MPARK_PATTERNS_AS_HPP

#include <any>
#include <type_traits>
#include <utility>
#include <variant>

#include "lib.hpp"

namespace mpark::patterns {

  namespace detail {

    using std::get_if;

    template <typename T,
              typename V,
              typename = decltype(std::declval<V>().template get_if<T>())>
    constexpr DetectResult detect_get_if(lib::priority<0>) noexcept {
      return DetectResult::Member;
    }

    template <
        typename T,
        typename V,
        typename = decltype(get_if<T>(std::addressof(std::declval<V &>())))>
    constexpr DetectResult detect_get_if(lib::priority<1>) noexcept {
      return DetectResult::NonMember;
    }

    template <typename T, typename V>
    constexpr DetectResult detect_get_if(lib::priority<2>) noexcept {
      return DetectResult::None;
    }

    template <typename T, typename V>
    inline constexpr DetectResult detect_get_if_v =
        detect_get_if<T, V>(lib::priority<>{});

    using std::any_cast;

    template <typename T,
              typename V,
              typename = decltype(std::declval<V>().template any_cast<T>())>
    constexpr DetectResult detect_any_cast(lib::priority<0>) noexcept {
      return DetectResult::Member;
    }

    template <
        typename T,
        typename V,
        typename = decltype(any_cast<T>(std::addressof(std::declval<V &>())))>
    constexpr DetectResult detect_any_cast(lib::priority<1>) noexcept {
      return DetectResult::NonMember;
    }

    template <typename T, typename V>
    constexpr DetectResult detect_any_cast(lib::priority<2>) noexcept {
      return DetectResult::None;
    }

    template <typename T, typename V>
    inline constexpr DetectResult detect_any_cast_v =
        detect_any_cast<T, V>(lib::priority<>{});

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
        constexpr auto result = detail::detect_get_if_v<T, Value>;
        if constexpr (result == detail::DetectResult::Member) {
          return std::forward<Value>(value).template get_if<T>();
        } else if constexpr (result == detail::DetectResult::NonMember) {
          using std::get_if;
          return get_if<T>(std::addressof(value));
        } else {
          static_assert(lib::false_v<Value>,
                        "The value attempting to be matched against an `as` "
                        "pattern has a specialization for `std::variant_size`, "
                        "but does not have a member nor non-member `get_if` "
                        "function available.");
        }
      } else {
        constexpr auto result = detail::detect_any_cast_v<T, Value>;
        if constexpr (result == detail::DetectResult::Member) {
          return std::forward<Value>(value).template any_cast<T>();
        } else if constexpr (result == detail::DetectResult::NonMember) {
          using std::any_cast;
          return any_cast<T>(std::addressof(value));
        } else {
          static_assert(
              lib::false_v<Value>,
              "The value attempting to be matched against an `as` "
              "pattern is not polymorphic, variant-like, nor any-like.");
        }
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
