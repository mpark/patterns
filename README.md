# MPark.Patterns

> Pattern Matching in __C++14__.

## Introduction

## Basic Syntax

```cpp
using namespace mpark;
match(<expr>...)(
  pattern(<pattern>...) = <handler>,
  pattern(<pattern>...) = <handler>,
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
  using namespace mpark;
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
  using namespace mpark;
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

#### Syntax

  - `prod(<pattern>...)`

#### Examples

```cpp
auto t = std::make_tuple(101, "hello", 1.1);

// C++17 Structured Bindings:
const auto& [x, y, z] = t;
// ...

// C++14 MPark.Patterns:
using namespace mpark;
match(t)(
    pattern(prod(arg, arg, arg)) = [](const auto& x, const auto& y, const auto& z) {
      // ...
    });
```

```cpp
void fizzbuzz() {
  using namespace mpark;
  for (int i = 1; i <= 100; ++i) {
    match(std::make_pair(i % 3, i % 5))(
        pattern(prod(0, 0)) = [] { std::cout << "fizzbuzz\n"; },
        pattern(prod(0, _)) = [] { std::cout << "fizz\n"; },
        pattern(prod(_, 0)) = [] { std::cout << "buzz\n"; },
        pattern(prod(_, _)) = [i] { std::cout << i << std::endl; });
  }
}
```

__NOTE__: The top-level is wrapped by a `tuple`, allowing us to write:

```cpp
void fizzbuzz() {
  using namespace mpark;
  for (int i = 1; i <= 100; ++i) {
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

#### Requirements

The type `T` satisfies `Sum` if given a variable `x` of type `T`,
  - If `mpark::variant_size<T>` is a complete type, `x.get<T>()` and
    `x.get_if<T>()` are valid. Otherwise, `get<T>(x)` and `get_if<T>(x)` are valid.
  - `mpark::variant_size<T>::value` is a well-formed integer constant expression.

#### Syntax

  - `sum<T>(<pattern>)`

#### Examples

```cpp
using str = std::string;
mpark::variant<int, str> v = 42;

using namespace mpark;
match(v)(pattern(sum<int>(_)) = [] { std::cout << "int\n"; },
         pattern(sum<str>(_)) = [] { std::cout << "str\n"; });
// prints "int".
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

using namespace mpark;
match(p)(pattern(some(_)) = [] { std::cout << "some\n"; },
         pattern(none) = [] { std::cout << "none\n"; });
// prints "none".
```

```cpp
boost::optional<int> o = 42;

using namespace mpark;
match(o)(
    pattern(some(arg)) = [](auto x) { std::cout << "some(" << x << ")\n"; },
    pattern(none) = [] { std::cout << "none\n"; });
// prints "some(42)".
```

### Variadic Pattern

A _variadic pattern_ matches 0 or more values that match a pattern.

#### Requirements

None.

#### Syntax

  - `variadic(<pattern>)`

#### Examples

```cpp
auto x = std::make_tuple(101, "hello", 1.1);

using namespace mpark;
match(x)(
    pattern(prod(variadic(arg))) = [](const auto&... xs) {
      int dummy[] = { (std::cout << xs << ' ', 0)... };
      (void)dummy;
    });
// prints: "101 hello 1.1 "
```

This could also be used to implement [C++17 `std::apply`][apply]:

```cpp
template <class F, class Tuple>
constexpr decltype(auto) apply(F &&f, Tuple &&t) {
  return match(std::forward<T>(t))(
      pattern(prod(variadic(arg))) = std::forward<F>(f));
}
```

We can even get a little fancier:

```cpp
int x = 42;
auto y = std::make_tuple(101, "hello", 1.1);

using namespace mpark;
match(x, y)(
    pattern(arg, prod(variadic(arg))) = [](const auto&... xs) {
      int dummy[] = { (std::cout << xs << ' ', 0)... };
      (void)dummy;
    });
// prints: "42 101 hello 1.1 "
```

[apply]: http://en.cppreference.com/w/cpp/utility/apply
[structured-bindings]: http://en.cppreference.com/w/cpp/language/declarations#Structured_binding_declaration
