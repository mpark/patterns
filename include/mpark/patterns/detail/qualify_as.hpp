// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_DETAIL_QUALIFY_AS_HPP
#define MPARK_PATTERNS_DETAIL_QUALIFY_AS_HPP

namespace mpark::patterns::detail {

  template <typename T, typename U>
  struct qualify_as : lib::identity<T> {};

  template <typename T, typename U>
  using qualify_as_t = typename qualify_as<T, U>::type;

  template <typename T, typename U>
  struct qualify_as<T, U &> : lib::identity<qualify_as_t<T, U> &> {};

  template <typename T, typename U>
  struct qualify_as<T, U &&> : lib::identity<qualify_as_t<T, U> &&> {};

  template <typename T, typename U>
  struct qualify_as<T, const U> : lib::identity<const qualify_as_t<T, U>> {};

  template <typename T, typename U>
  struct qualify_as<T, U volatile>
      : lib::identity<qualify_as_t<T, U> volatile> {};

  template <typename T, typename U>
  struct qualify_as<T, const U volatile>
      : lib::identity<const qualify_as_t<T, U> volatile> {};

}  // namespace mpark::patterns::detail

#endif  // MPARK_PATTERNS_DETAIL_QUALIFY_AS_HPP
