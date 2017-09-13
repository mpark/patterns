# MPark.Patterns

> Pattern Matching in __C++__.

[![stability][badge.stability]][stability]
[![travis][badge.travis]][travis]
[![license][badge.license]][license]
[![wandbox][badge.wandbox]][wandbox]

[badge.stability]: https://img.shields.io/badge/stability-experimental-orange.svg
[badge.travis]: https://travis-ci.org/mpark/patterns.svg?branch=master
[badge.license]: http://img.shields.io/badge/license-boost-blue.svg
[badge.wandbox]: https://img.shields.io/badge/try%20it-on%20wandbox-5cb85c.svg

[stability]: http://github.com/badges/stability-badges
[travis]: https://travis-ci.org/mpark/patterns
[license]: https://github.com/mpark/patterns/blob/master/LICENSE.md
[wandbox]: https://wandbox.org/permlink/G46QnPBB0OiV5m0N

## Test

This directory contains the tests for __MPark.Patterns__.

## CMake Variables

  -  __`MPARK_PATTERNS_EXCEPTIONS`__:`BOOL` (__default__: `ON`)

     Build the tests with exceptions support.

### Building and Running Tests

Execute the following commands from the top-level directory:

```bash
mkdir build
cd build
cmake -DMPARK_PATTERNS_INCLUDE_TESTS=ON ..
cmake --build .
ctest -V
```
