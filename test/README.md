# MPark.Patterns

> Pattern Matching in __C++__

[![release][badge.release]][release]
[![travis][badge.travis]][travis]
[![license][badge.license]][license]
[![wandbox][badge.wandbox]][wandbox]

[badge.release]: https://img.shields.io/github/release/mpark/variant.svg
[badge.travis]: https://travis-ci.org/mpark/patterns.svg?branch=master
[badge.license]: http://img.shields.io/badge/license-boost-blue.svg
[badge.wandbox]: https://img.shields.io/badge/try%20it-on%20wandbox-5cb85c.svg

[release]: https://github.com/mpark/variant/releases/latest
[travis]: https://travis-ci.org/mpark/patterns
[license]: https://github.com/mpark/patterns/blob/master/LICENSE.md
[wandbox]: https://wandbox.org/permlink/G46QnPBB0OiV5m0N

## Test

This directory contains the tests for __MPark.Patterns__.

## CMake Variables

  -  __`MPARK_PATTERNS_EXCEPTIONS`__:`BOOL` (__default__: `ON`)

     Build the tests with exceptions support.

## Build / Run

Execute the following commands from the top-level directory:

```bash
mkdir build
cd build
cmake -DMPARK_PATTERNS_INCLUDE_TESTS=ON ..
cmake --build .
ctest -V
```
