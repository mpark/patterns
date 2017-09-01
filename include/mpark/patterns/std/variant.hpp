// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_STD_VARIANT_HPP
#define MPARK_PATTERNS_STD_VARIANT_HPP

#if defined(__cpp_lib_variant)
#include <variant>
#else
#define mpark std
#include "mpark_variant.hpp"
#undef mpark
#endif

#endif  // MPARK_PATTERNS_STD_VARIANT_HPP
