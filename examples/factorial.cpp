#include <iostream>
#include <cstdio>

#include <mpark/patterns.hpp>

int factorial(int n) {
    using namespace mpark::patterns;
    return match(n)(pattern(0) = [] { return 1; },
                    pattern(arg) = [](auto n) { return n * factorial(n - 1); });
}

int main() {
    std::cout << factorial(5) << " == 120" << std::endl;
}