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
  constexpr bool is_n_constructible(std::index_sequence<Is...>,
                                    lib::priority<0>) {
    return true;
  }
#pragma GCC diagnostic pop

  template <typename T, std::size_t... Is>
  constexpr bool is_n_constructible(std::index_sequence<Is...>,
                                    lib::priority<1>) {
    return false;
  }

  template <typename T, std::size_t N>
  inline constexpr bool is_n_constructible_v =
      is_n_constructible<T>(std::make_index_sequence<N>{}, lib::priority<>{});

  template <typename T>
  struct aggregate_size_impl {
    template <std::size_t B, std::size_t E>
    static constexpr std::optional<std::size_t> impl() {
      constexpr std::size_t M = B + ((E - B) / 2);
      constexpr bool is_mid_constructible = is_n_constructible_v<T, M>;
      if constexpr (B == M) {
        if constexpr (is_mid_constructible) {
          return M;
        } else {
          return std::nullopt;
        }
      } else if constexpr (is_mid_constructible) {
        // We recursve into `[M, E)` rather than `[M + 1, E)`
        // since `M` could be the answer.
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
      *aggregate_size_impl<T>::template impl<0, sizeof(T) + 1>();

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
