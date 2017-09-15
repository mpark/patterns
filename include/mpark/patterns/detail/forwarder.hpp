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
