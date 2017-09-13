# MPark.Patterns

> Pattern Matching in __C++__

[![release][badge.release]][release]
[![travis][badge.travis]][travis]
[![license][badge.license]][license]
[![godbolt][badge.godbolt]][godbolt]
[![wandbox][badge.wandbox]][wandbox]

[badge.release]: https://img.shields.io/github/release/mpark/patterns.svg
[badge.travis]: https://travis-ci.org/mpark/patterns.svg?branch=master
[badge.license]: http://img.shields.io/badge/license-boost-blue.svg
[badge.godbolt]: https://img.shields.io/badge/try%20it-on%20godbolt-222266.svg
[badge.wandbox]: https://img.shields.io/badge/try%20it-on%20wandbox-5cb85c.svg

[release]: https://github.com/mpark/patterns/releases/latest
[travis]: https://travis-ci.org/mpark/patterns
[license]: https://github.com/mpark/patterns/blob/master/LICENSE.md
[godbolt]: https://godbolt.org/g/xWYuHJ
[wandbox]: https://wandbox.org/permlink/XfuP5Pu5dHmQHBtq

## Introduction

__MPark.Patterns__ is an experimental pattern matching library for __C++17__.

It determines whether a given value __matches__ a __pattern__ and, if it does,
__binds__ the desired portions of the value to a __handler__.

Pattern matching has been introduced to many programming languages outside of
the functional world, and this library draws inspiration from languages such as
Haskell, OCaml, Rust, Scala, and Swift.

```cpp
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
```

## Basic Syntax

```cpp
using namespace mpark::patterns;
match(<expr>...)(
  pattern(<pattern>...) = [](<binding>...) { /* ... */ },
  pattern(<pattern>...) = [](<binding>...) { /* ... */ },
  // ...
);
```

## Types of Patterns

### Wildcard Pattern

A _wildcard pattern_ matches and ignores any value.

#### Requirements

None.

#### Syntax

  - `_` (underscore)

#### Examples

```cpp
int factorial(int n) {
  using namespace mpark::patterns;
  return match(n)(pattern(0) = [] { return 1; },
                  pattern(_) = [n] { return n * factorial(n - 1); });
}
```

### Bind Pattern

A _bind pattern_ passes any value matched by `<pattern>` to the handler.

#### Requirements

None.

#### Syntax

  - `arg(<pattern>)`
  - `arg` -- alias for `arg(_)`

#### Examples

```cpp
int factorial(int n) {
  using namespace mpark::patterns;
  return match(n)(pattern(0) = [] { return 1; },
                  pattern(arg) = [](auto n) { return n * factorial(n - 1); });
}
```

### Product Pattern

A _product pattern_ matches values that holds multiple values.

#### Requirements

The type `T` satisfies `Product` if given a variable `x` of type `T`,
  - If `std::tuple_size<T>` is a complete type, `x.get<I>()` is valid for all
    `I` in `[0, std::tuple_size<T>::value)`. Otherwise, `get<I>(x)` is valid
    for all `I` in `[0, std::tuple_size<T>::value)`.
  - `std::tuple_size<T>::value` is a well-formed integer constant expression.

__NOTE__: These requirements are very similar to the requirements for
          [C++17 Structured Bindings][structured-bindings].

[structured-bindings]: http://en.cppreference.com/w/cpp/language/declarations#Structured_binding_declaration

#### Syntax

  - `prod(<pattern>...)`

#### Examples

```cpp
auto t = std::make_tuple(101, "hello", 1.1);

// C++17 Structured Bindings:
const auto& [x, y, z] = t;
// ...

// C++14 MPark.Patterns:
using namespace mpark::patterns;
placeholder x, y, z;
match(t)(
    pattern(prod(x, y, z)) = [](const auto& x, const auto& y, const auto& z) {
      // ...
    });
```

__NOTE__: The top-level is wrapped by a `tuple`, allowing us to write:

```cpp
void fizzbuzz() {
  for (int i = 1; i <= 100; ++i) {
    using namespace mpark::patterns;
    match(i % 3, i % 5)(
        pattern(0, 0) = [] { std::cout << "fizzbuzz\n"; },
        pattern(0, _) = [] { std::cout << "fizz\n"; },
        pattern(_, 0) = [] { std::cout << "buzz\n"; },
        pattern(_, _) = [i] { std::cout << i << std::endl; });
  }
}
```

### Sum Pattern

A _sum pattern_ matches values that holds one of a set of alternatives.

The `sum<T>` pattern matches if the given value holds an instance of `T`.
The `sum` pattern matches values of a sum type,

#### Requirements

The type `T` satisfies `Sum<U>` if given a variable `x` of type `T`,
  - If `std::variant_size<T>` is a complete type, `x.get_if<U>()` and `x.get<U>()`
    are valid. Otherwise, `get_if<U>(&x)` and `get<U>(x)` are valid.
  - `std::variant_size<T>::value` is a well-formed integer constant expression.

The type `T` satisfies `Sum` if given a variable `x` of type `T`,
  - If `std::variant_size<T>` is a complete type, `visit([](auto&&) {}, x)` is valid.
  - `std::variant_size<T>::value` is a well-formed integer constant expression.

#### Syntax

  - `sum<U>(<pattern>)`
  - `sum(<pattern>)`

#### Examples

```cpp
using str = std::string;
std::variant<int, str> v = 42;

using namespace mpark::patterns;
match(v)(pattern(sum<int>(_)) = [] { std::cout << "int\n"; },
         pattern(sum<str>(_)) = [] { std::cout << "str\n"; });
// prints: "int".
```

```cpp
using str = std::string;
std::variant<int, str> v = "hello world!";

struct {
  void operator()(int n) const { std::cout << "int: " << n << '\n'; }
  void operator()(const str& s) const { std::cout << "str: " << s << '\n'; }
} handler;

using namespace mpark::patterns;
match(v)(pattern(sum(arg)) = handler);
// prints: "str: hello world!".
```

### Optional Pattern

An _optional pattern_ matches values that can be dereferenced, and tested as a `bool`.

#### Requirements

The type `T` satisfies `Optional` if given a variable `x` of type `T`,
  - `*x` is a valid expression.
  - `x` is contextually convertible to `bool`.

#### Syntax

  - `some(<pattern>)`
  - `none`

#### Examples

```cpp
int *p = nullptr;

using namespace mpark::patterns;
match(p)(pattern(some(_)) = [] { std::cout << "some\n"; },
         pattern(none) = [] { std::cout << "none\n"; });
// prints: "none".
```

```cpp
std::optional<int> o = 42;

using namespace mpark::patterns;
match(o)(
    pattern(some(arg)) = [](auto x) { std::cout << "some(" << x << ")\n"; },
    pattern(none) = [] { std::cout << "none\n"; });
// prints: "some(42)".
```

### Variadic Pattern

A _variadic pattern_ matches 0 or more values that match a given pattern.

#### Requirements

None.

#### Syntax

  - `variadic(<pattern>)`

#### Examples

```cpp
auto x = std::make_tuple(101, "hello", 1.1);

using namespace mpark::patterns;
match(x)(
    pattern(prod(variadic(arg))) = [](const auto&... xs) {
      int dummy[] = { (std::cout << xs << ' ', 0)... };
      (void)dummy;
    });
// prints: "101 hello 1.1 "
```

This could also be used to implement [C++17 `std::apply`][apply]:

[apply]: http://en.cppreference.com/w/cpp/utility/apply

```cpp
template <typename F, typename Tuple>
decltype(auto) apply(F &&f, Tuple &&t) {
  using namespace mpark::patterns;
  return match(std::forward<T>(t))(
      pattern(prod(variadic(arg))) = std::forward<F>(f));
}
```

and even [C++17 `std::visit`][visit]:

[visit]: http://en.cppreference.com/w/cpp/utility/variant/visit

```cpp
template <typename F, typename... Vs>
decltype(auto) visit(F &&f, Vs &&... vs) {
  using namespace mpark::patterns;
  return match(std::forward<Vs>(vs)...)(
      pattern(variadic(sum(arg))) = std::forward<F>(f));
}
```

We can even get a little fancier:

```cpp
int x = 42;
auto y = std::make_tuple(101, "hello", 1.1);

using namespace mpark::patterns;
match(x, y)(
    pattern(arg, prod(variadic(arg))) = [](const auto&... xs) {
      int dummy[] = { (std::cout << xs << ' ', 0)... };
      (void)dummy;
    });
// prints: "42 101 hello 1.1 "
```

### Alternation Pattern

An _alternation pattern_ matches values that match any of the given patterns.

#### Requirements

None.

#### Syntax

  - `anyof(<pattern>...)`

#### Examples

```cpp
std::string s = "large";

using namespace mpark::patterns;
match(s)(
    pattern(anyof("big", "large", "huge")) = [] { std::cout << "big!\n"; },
    pattern(anyof("little", "small", "tiny")) = [] { std::cout << "small.\n"; },
    pattern(_) = [] { std::cout << "unknown size.\n"; });
```

## Pattern Guards

While pattern matching is used to match values against patterns and bind the
desired portions, pattern guards are used to test whether the bound values
satisfy some predicate.

#### Syntax

  - `WHEN(<condition>) { /* ... */ };`

#### Examples

```cpp
using namespace mpark::patterns;
placeholder lhs, rhs;
match(101, 202)(
    pattern(lhs, rhs) = [](auto&& lhs, auto&& rhs) {
      WHEN(lhs > rhs) { std::cout << "GT\n"; };
    },
    pattern(lhs, rhs) = [](auto&& lhs, auto&& rhs) {
      WHEN(lhs < rhs) { std::cout << "LT\n"; };
    },
    pattern(lhs, rhs) = [](auto&& lhs, auto&& rhs) {
      WHEN(lhs == rhs) { std::cout << "EQ\n"; };
    });
// prints: "LT".
```

## Requirements

This library requires a standard conformant __C++17__ compiler.
The following compilers are continously tested:

| Compiler                               | Operating System                            | Version String                                                                          |
|----------------------------------------|---------------------------------------------|-----------------------------------------------------------------------------------------|
| GCC 7.2.0                              | Ubuntu 14.04.5 LTS                          | g++-7 (Ubuntu 7.2.0-1ubuntu1~14.04) 7.2.0                                               |
| Clang 5.0.0                            | Ubuntu 14.04.5 LTS                          | clang version 5.0.0-svn312333-1~exp1 (branches/release_50)                              |

## CMake Variables

  -  __`MPARK_PATTERNS_INCLUDE_TESTS`__:`BOOL` (__default__: `OFF`)

     Build tests.

## Unit Tests

Refer to [test/README.md](test/README.md).

## License

Distributed under the [Boost Software License, Version 1.0](LICENSE.md).

## Related Work

  - [solodon4/Mach7](https://github.com/solodon4/Mach7)
  - [jbandela/simple_match](https://github.com/jbandela/simple_match/)
