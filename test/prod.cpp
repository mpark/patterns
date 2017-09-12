// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#include <mpark/patterns.hpp>

#include <cstddef>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

#include <gtest/gtest.h>

namespace N {

  struct S {
    S(int x_, std::string y_) : x(x_), y(std::move(y_)) {}

    int x;
    std::string y;
  };

  template <std::size_t I>
  auto &&get(const S &s) {
    if      constexpr (I == 0) return s.x;
    else if constexpr (I == 1) return s.y;
  }

}  // namespace N

namespace std {

  template <> class tuple_size<N::S> : public integral_constant<size_t, 2> {};
  template <> class tuple_element<0, N::S> { public: using type = int; };
  template <> class tuple_element<1, N::S> { public: using type = string; };

}  // namespace std

TEST(Prod, Pair) {
  std::vector<std::pair<int, int>> points = {{0, 0}, {1, 0}, {1, 1}, {2, 0}};

  int origin = 0;
  int y_zero = 0;
  int otherwise = 0;
  for (const auto &point : points) {
    using namespace mpark::patterns;
    match(point)(pattern(prod(0, 0)) = [&origin] { ++origin; },
                 pattern(prod(_, 0)) = [&y_zero] { ++y_zero; },
                 pattern(prod(_, _)) = [&otherwise] { ++otherwise; });
  }

  EXPECT_EQ(1, origin);
  EXPECT_EQ(2, y_zero);
  EXPECT_EQ(1, otherwise);
}

TEST(Prod, Custom) {
  N::S s(101, "world");

  using namespace mpark::patterns;
  int result = match(s)(
      pattern(prod(0, "")) = [] { return 0; },
      pattern(arg(prod(101, arg))) = [](const auto &x, const auto &y) {
        static_assert(std::is_same<N::S, std::decay_t<decltype(x)>>::value, "");
        static_assert(
            std::is_same<std::string, std::decay_t<decltype(y)>>::value, "");
        return 1;
      });

  EXPECT_EQ(1, result);
}
