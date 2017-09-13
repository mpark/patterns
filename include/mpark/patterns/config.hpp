// MPark.Patterns
//
// Copyright Michael Park, 2017
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

#ifndef MPARK_PATTERNS_CONFIG_HPP
#define MPARK_PATTERNS_CONFIG_HPP

#ifndef __has_feature
#define __has_feature(x) 0
#endif

#if __has_feature(cxx_exceptions) || defined(__cpp_exceptions) || \
    (defined(_MSC_VER) && defined(_CPPUNWIND))
#define MPARK_PATTERNS_EXCEPTIONS
#endif

#if defined(__cpp_lib_is_aggregate)
#define MPARK_PATTERNS_IS_AGGREGATE
#endif

#endif  // MPARK_PATTERNS_CONFIG_HPP
