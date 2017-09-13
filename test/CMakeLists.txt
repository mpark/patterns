# MPark.Patterns
#
# Copyright Michael Park, 2017
#
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

cmake_minimum_required(VERSION 3.8.2)

option(MPARK_PATTERNS_EXCEPTIONS
       "Build the tests with exceptions-handling support." ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang" OR
   "${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" OR
   "${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
  add_compile_options(-Wall)
  add_compile_options(-Werror)
  add_compile_options(-Wextra)
  add_compile_options(-pedantic)
  if(NOT ${MPARK_PATTERNS_EXCEPTIONS})
    add_compile_options(-fno-exceptions)
  endif()
endif()

set(MPARK_TESTS
  aggregate
  array
  balance
  calc
  intro
  optional
  prod
  regex
  sum
  variadic)

foreach(test ${MPARK_TESTS})
  add_executable(${test} ${test}.cpp)
  set_target_properties(${test} PROPERTIES COMPILE_FLAGS "${cxx_strict}")
  target_link_libraries(${test} gtest_main mpark_patterns)
  add_test(${test} ${test} --gtest_color=yes)
endforeach()
