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
[godbolt]: https://godbolt.org/g/QXqY1o
[wandbox]: https://wandbox.org/permlink/xAgD8i8R43Ge6k9I

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
IDENTIFIERS(<identifier>...);  // optional
match(<expr>...)(
    pattern(<pattern>...) = [](<binding>...) { /* ... */ },
    pattern(<pattern>...) = [](<binding>...) { /* ... */ },
    // ...
);
```

## Types of Patterns

### Expression Pattern

An _expression pattern_ matches if the value compares equal to
the given expression.

#### Requirements

Given an expression `e` and a value `x`, `e == x` must be a valid expression.

#### Syntax

  - `<expr>`

#### Examples

```cpp
int factorial(int n) {
  using namespace mpark::patterns;
  return match(n)(pattern(0) = [] { return 1; },
                  //      ^ expression
                  pattern(_) = [n] { return n * factorial(n - 1); });
}
```

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
                  //      ^ wildcard
}
```

### Binding Patterns

A _binding pattern_ passes matching values to corresponding handler.

There are two types of binding patterns:
  * Arg Pattern
  * Identifier Pattern

### Arg Pattern

A _arg pattern_ passes any matching values to the corresponding handler.
It can be used multiple times, and they match independent of each other.

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
                  //      ^^^ arg
}
```

```cpp
using namespace mpark::patterns;
match(202, 101)(pattern(101, arg) = [](auto) { /* ... */ },
                //           ^^^ arg
                pattern(arg, arg) = [](auto, auto) { /* OK. */ });
                //      ^^^  ^^^ arg
```

### Identifier Pattern

An _identifier pattern_ is similar to the _arg pattern_ in that it is a binding
pattern. It matches any values and passes them to the corresponding handler.

It can be also be used multiple times, with an additional constraint that
the values bound to the repeated identifiers must compare equal in order
for the enclosing pattern to match.

#### Requirements

If no identifiers are repeated, none.

Otherwise, let `x, xs...` be the values matched by a repeated identifier.
Then `(... && (x == xs))` must be a valid expression for each repeated identifier.

#### Syntax

  - IDENTIFIERS(<identifier>...);
  - `<identifier>(<pattern>)`
  - `<identifier>` -- alias for `<identifier>(_)`

#### Examples

```cpp
using namespace mpark::patterns;
IDENTIFIERS(x, y);
match(101, 202)(
    pattern(x, x) = [](auto x) { std::cout << "same\n"; },
    //      ^  ^ identifier
    pattern(x, y) = [](auto x, auto y) { std::cout << "diff\n"; });
    //      ^  ^ identifier
// prints: "diff"
```

```cpp
using namespace mpark::patterns;
IDENTIFIERS(x, y);
match(42, 42)(
    pattern(x, x) = [](auto x) { std::cout << "same\n"; },
    //      ^  ^ identifier
    pattern(x, y) = [](auto x, auto y) { std::cout << "diff\n"; });
    //      ^  ^ identifier
// prints: "same"
```

### Destructure Pattern

A _destructure pattern_ matches values that holds multiple values.

#### Requirements

Given a value `x` of type `T`, `T` must satisfy one of the following conditions:
  1. `std::is_array_v<T> == true`, or
  2. `std::is_class_v<T> == true` and `std::tuple_size<T>` is a complete type, or
  3. `std::is_aggregate_v<T> == true`

If (2), the following conditions must also be satisfied.
  - `std::tuple_size<T>::value` must be a well-formed
    integer constant expression, and
  - `x.get<I>()` or `get<I>(x)` must be a valid expression for all `I` in
    `[0, std::tuple_size<T>::value)`

For aggregate types (3), the current implementation only supports types that
contain upto 16 non-static data members.

__NOTE__: These requirements are very similar to the requirements for
          [C++17 Structured Bindings][structured-bindings].

[structured-bindings]: http://en.cppreference.com/w/cpp/language/declarations#Structured_binding_declaration

#### Syntax

  - `ds(<pattern>...)`

#### Examples

```cpp
auto t = std::make_tuple(101, "hello", 1.1);

// C++17 Structured Bindings:
const auto& [x, y, z] = t;
// ...

// MPark.Patterns:
using namespace mpark::patterns;
placeholder x, y, z;
match(t)(
    pattern(ds(x, y, z)) = [](const auto& x, const auto& y, const auto& z) {
    //      ^^^^^^^^^^^ destructure
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
        pattern(_, _) = [i] { std::cout << i << '\n'; });
  }
}
```

### Optional Pattern

An _optional pattern_ matches values that can be dereferenced,
and tested as a `bool`.

#### Requirements

Given a value `x` of type `T`, `T` must satisfy all of the following conditions:
  - `*x` is a valid expression, and
  - `x` is contextually convertible to `bool`

#### Syntax

  - `some(<pattern>)`
  - `none`

#### Examples

```cpp
int *p = nullptr;

using namespace mpark::patterns;
match(p)(pattern(some(_)) = [] { std::cout << "some\n"; },
         pattern(none) = [] { std::cout << "none\n"; });
// prints: "none"
```

```cpp
std::optional<int> o = 42;

using namespace mpark::patterns;
match(o)(
    pattern(some(arg)) = [](auto x) { std::cout << "some(" << x << ")\n"; },
    pattern(none) = [] { std::cout << "none\n"; });
// prints: "some(42)"
```

### As Pattern

An _as pattern_ matches values that is capable of holding a value of multiple types.

#### Requirements

Given a value `x` of type `T`, and a pattern `as<U>(<pattern>)`,
`T` must satisfy one of the following conditions:
  1. `std::is_polymorphic_v<T> == true`, or
  2. `std::is_class_v<T> == true` and `std::variant_size<T>` is a complete type, or
  3. `x.any_cast<U>()` or `any_cast<U>(&x)` is a valid expression

- If (1), `dynamic_cast<U'>(&x)` must be a valid expression,
  where `U'` is pointer to `U` with the same cv-qualifiers as `decltype(x)`
- If (2), `x.get_if<U>()` or `get_if<U>(&x)` must be a valid expression

#### Syntax

  - `as<U>(<pattern>)`

#### Examples

```cpp
struct Shape { virtual ~Shape() = default; };
struct Circle : Shape {};
struct Square : Shape {};

std::unique_ptr<Shape> shape = std::make_unique<Square>();

using namespace mpark::patterns;
match(shape)(pattern(some(as<Circle>(_))) = [] { std::cout << "Circle\n"; },
             pattern(some(as<Square>(_))) = [] { std::cout << "Square\n"; });
// prints: "Square"
```

```cpp
using str = std::string;
std::variant<int, str> v = 42;

using namespace mpark::patterns;
match(v)(pattern(as<int>(_)) = [] { std::cout << "int\n"; },
         pattern(as<str>(_)) = [] { std::cout << "str\n"; });
// prints: "int"
```

```cpp
using str = std::string;
std::any a = str("hello");

using namespace mpark::patterns;
match(a)(pattern(as<int>(_)) = [] { std::cout << "int\n"; },
         pattern(as<str>(_)) = [] { std::cout << "str\n"; });
// prints: "str"
```

### Visit Pattern

A _visit pattern_ matches values that is capable of holding a value of
closed set of types, and can be visited.

#### Requirements

Given a value `x`, The following code must be valid.

```
using std::visit;
visit([](auto&&) {}, x);`
```

#### Syntax

  - `vis(<pattern>)`

#### Examples

```cpp
using str = std::string;
std::variant<int, str> v = "hello world!";

struct Visitor {
  void operator()(int n) const { std::cout << "int: " << n << '\n';
}
void operator()(const str &s) const { std::cout << "str: " << s << '\n'; }
};

using namespace mpark::patterns;
match(v)(pattern(vis(arg)) = Visitor{});
// prints: "str: hello world!".
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

### Variadic Pattern

A _variadic pattern_ matches 0 or more values that match a given pattern.

#### Requirements

  - A _variadic pattern_ can only appear within a destructure pattern.
  - A _variadic pattern_ can only appear once.

#### Syntax

  - `variadic(<pattern>)`

#### Examples

```cpp
auto x = std::make_tuple(101, "hello", 1.1);

using namespace mpark::patterns;
match(x)(
    pattern(ds(variadic(arg))) = [](const auto&... xs) {
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
      pattern(ds(variadic(arg))) = std::forward<F>(f));
}
```

and even [C++17 `std::visit`][visit]:

[visit]: http://en.cppreference.com/w/cpp/utility/variant/visit

```cpp
template <typename F, typename... Vs>
decltype(auto) visit(F &&f, Vs &&... vs) {
  using namespace mpark::patterns;
  return match(std::forward<Vs>(vs)...)(
      pattern(variadic(vis(arg))) = std::forward<F>(f));
}
```

We can even get a little fancier:

```cpp
int x = 42;
auto y = std::make_tuple(101, "hello", 1.1);

using namespace mpark::patterns;
match(x, y)(
    pattern(arg, ds(variadic(arg))) = [](const auto&... xs) {
      int dummy[] = { (std::cout << xs << ' ', 0)... };
      (void)dummy;
    });
// prints: "42 101 hello 1.1 "
```

## Pattern Guards

While pattern matching is used to match values against patterns and bind the
desired portions, pattern guards are used to test whether the bound values
satisfy some predicate.

#### Requirements

A _pattern guard_ must be the only statement in a handler.

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
// prints: "LT"
```

## `let` Statements

### `if_let`

This construct is simply syntactic sugar for the following situation:

```cpp
std::optional<int> o = 42;

using namespace mpark::patterns;
match(o)(
    pattern(some(arg)) = [](auto x) { ... },
    pattern(_) = [] {});
```

For this situation, `if_let` provides a cleaner interface:

```cpp
std::optional<int> o = 42;

using namespace mpark::patterns;
if_let (pattern(some(arg)) = o) = [](auto x) {
  // ...
};
```

### `for_let`

Suppose we want to match each element in a range-based `for` loop with pattern.
We could imagine being able to write something like:

```cpp
std::vector<std::pair<int, int>> pairs = {{0, 1}, {1, 1}, {0, 0}, {0, 2}};

for ((0, x) : pairs) {
  std::cout << x << ' ';
}
// prints: "1 0 2 "
```

With `if_let` we can say something close-ish:

```cpp
std::vector<std::pair<int, int>> pairs = {{0, 1}, {1, 1}, {0, 0}, {0, 2}};

for (const auto &p : pairs) {
  using namespace mpark::patterns;
  if_let (pattern(ds(0, arg)) = p) = [](auto x) {
    std::cout << x << ' ';
  };
}
// prints: "1 0 2 "
```

`for_let` allows the above example to be written more concisely:

```cpp
std::vector<std::pair<int, int>> pairs = {{0, 1}, {1, 1}, {0, 0}, {0, 2}};

using namespace mpark::patterns;
for_let (pattern(ds(0, arg)) = pairs) = [](auto x) {
  std::cout << x << ' ';
};
// prints: "1 0 2 "
```

`for_let` also provides the `break` and `continue` statements with
`return Break;` and `return Continue;` respectively.

```cpp
std::vector<std::pair<int, int>> pairs = {{0, 1}, {1, 1}, {0, 0}, {0, 2}};

using namespace mpark::patterns;
for_let (pattern(ds(0, arg)) = pairs) = [](auto x) {
  if (x == 1) return Break;
  std::cout << x << ' ';
  return Continue;
};
// prints: "0 "
```

```cpp
std::vector<std::pair<int, int>> pairs = {{0, 1}, {1, 1}, {0, 0}, {0, 2}};

using namespace mpark::patterns;
for_let (pattern(ds(0, arg)) = pairs) = [](auto x) {
  if (x == 0) return Continue;
  std::cout << x << ' ';
  return Continue;
};
// prints: "1 2 "
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
