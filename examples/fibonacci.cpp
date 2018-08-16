#include <iostream>
#include <cstdio>

#include <mpark/patterns.hpp>

int fibonacci(int n) {
    using namespace mpark::patterns;

    return match(n)(
        pattern(0)  = []        { return 0; },
        pattern(1)  = []        { return 1; },
        pattern(arg)= [](auto n){ return fibonacci(n - 1) + fibonacci(n - 2); }
    );
}

int main() {
    std::cout << fibonacci(10) << " == 55" << std::endl;
}