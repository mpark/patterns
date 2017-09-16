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

  struct void_ {};

  template <typename T>
  class forwarder {
    private:
    template <typename U, typename Qualified>
    static constexpr bool is_enabled() {
      return std::is_constructible_v<T, Qualified> &&
             !(std::is_constructible_v<T, forwarder<U> &> ||
               std::is_constructible_v<T, const forwarder<U> &> ||
               std::is_constructible_v<T, forwarder<U> &&> ||
               std::is_constructible_v<T, const forwarder<U> &&> ||
               std::is_convertible_v<forwarder<U> &, T> ||
               std::is_convertible_v<const forwarder<U> &, T> ||
               std::is_convertible_v<forwarder<U> &&, T> ||
               std::is_convertible_v<const forwarder<U> &&, T>);
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
    constexpr forwarder(T &&value) : value_(std::forward<T>(value)) {}

    template <typename U, std::enable_if_t<enable_explicit<U, U &&>(), int> = 0>
    explicit forwarder(forwarder<U> &&that)
        : value_(std::move(that).forward()) {}

    template <typename U, std::enable_if_t<enable_implicit<U, U &&>(), int> = 0>
    forwarder(forwarder<U> &&that) : value_(std::move(that).forward()) {}

    forwarder(const forwarder &) = default;
    forwarder(forwarder &&) = default;

    forwarder &operator=(const forwarder &) = delete;
    forwarder &operator=(forwarder &&) = delete;

    constexpr T forward() && { return std::forward<T>(value_); }

    private:
    T value_;
  };

  template <>
  class forwarder<void> {
    public:
    constexpr forwarder(void_) noexcept {}

    template <typename U>
    forwarder(forwarder<U> &&) noexcept {}

    forwarder(const forwarder &) noexcept = default;
    forwarder(forwarder &&) noexcept = default;

    forwarder &operator=(const forwarder &) noexcept = delete;
    forwarder &operator=(forwarder &&) noexcept = delete;

    constexpr void forward() && noexcept {}
  };

}  // namespace mpark::patterns::detail

#endif  // MPARK_PATTERNS_DETAIL_FORWARDER_HPP
