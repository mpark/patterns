// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#include <cstdio>

#include <mpark/patterns.hpp>

void fizzbuzz() {
  using namespace mpark::patterns;
  for (int i = 1; i <= 100; ++i) {
    match(i % 3, i % 5)(
        pattern(0, 0) = [] { std::printf("fizzbuzz\n"); },
        pattern(0, _) = [] { std::printf("fizz\n"); },
        pattern(_, 0) = [] { std::printf("buzz\n"); },
        pattern(_, _) = [i] { std::printf("%d\n", i); });
  }
}

int main() {
  fizzbuzz();
}
