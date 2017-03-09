# MPark.Patterns

> Pattern matching in __C++14__.

## Introduction

## Basic Syntax

```cpp
using namespace mpark;
match(<expr>)(
  pattern(<pattern_0>) = <handler_0>,
  pattern(<pattern_1>) = <handler_1>,
  /* ... */
);
```

## Types of Patterns

### Wildcard Pattern

A _wildcard pattern_ matches and ignores any value.

#### Requirements

None.

#### Syntax

  - `_` (underscore)

#### Example

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

#### Example

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

#### Example

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

### Sum Pattern

A _sum pattern_ matches values that holds one of a set of alternatives.

#### Requirements

The type `T` satisfies `Sum` if given a variable `x` of type `T`,
  - If `mpark::variant_size<T>` is a complete type, `x.get<T>()` and
    `x.get_if<T>()` are valid. Otherwise, `get<T>(x)` and `get_if<T>(x)` are valid.
  - `mpark::variant_size<T>::value` is a well-formed integer constant expression.

#### Syntax

  - `sum<T>(<pattern>)`

#### Example

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

#### Example

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

[structured-bindings]: http://en.cppreference.com/w/cpp/language/declarations#Structured_binding_declaration
