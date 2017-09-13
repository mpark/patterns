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

#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/preprocessor/repetition/enum_params.hpp>
#include <boost/preprocessor/repetition/repeat_from_to.hpp>

#include "../lib.hpp"
#include "qualify_as.hpp"

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

  template <typename T, std::size_t B, std::size_t E>
  constexpr std::optional<std::size_t> aggregate_size_impl() {
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
      return aggregate_size_impl<T, M, E>();
    } else if constexpr (constexpr auto lhs = aggregate_size_impl<T, B, M>()) {
      return lhs;
    } else if constexpr (constexpr auto rhs = aggregate_size_impl<T, M, E>()) {
      return rhs;
    }
  }

  template <typename T>
  struct aggregate_size
      : lib::size_constant<*aggregate_size_impl<T, 0, sizeof(T) + 1>()> {};

  template <typename T>
  inline constexpr std::size_t aggregate_size_v = aggregate_size<T>::value;

  template <typename Aggregate>
  auto as_tuple_impl(Aggregate &&, lib::size_constant<0>) {
    return std::forward_as_tuple();
  }

#define FWD(z, n, text) static_cast<qualify_as_t<decltype(text##n), T>>(text##n)

#define AS_TUPLE_IMPL(z, n, text)                                              \
  template <typename Aggregate>                                                \
  auto as_tuple_impl(Aggregate &&aggregate, lib::size_constant<n>) {           \
    using T = decltype(std::forward<Aggregate>(aggregate));                    \
    auto && [BOOST_PP_ENUM_PARAMS(n, x)] = std::forward<Aggregate>(aggregate); \
    return std::forward_as_tuple(BOOST_PP_ENUM(n, FWD, x));                    \
  }

  BOOST_PP_REPEAT_FROM_TO(1, 32, AS_TUPLE_IMPL, _)

#undef FWD
#undef AS_TUPLE_IMPL

  template <typename Aggregate>
  auto as_tuple(Aggregate &&aggregate) {
    return as_tuple_impl(std::forward<Aggregate>(aggregate),
                         aggregate_size<std::decay_t<Aggregate>>{});
  }

}  // namespace mpark::patterns::detail

#endif  // MPARK_PATTERNS_DETAIL_AS_TUPLE_HPP
