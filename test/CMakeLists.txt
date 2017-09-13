# MPark.Patterns
#
# Copyright Michael Park, 2017
#
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

cmake_minimum_required(VERSION 3.6.3)

option(MPARK_PATTERNS_EXCEPTIONS
       "Build the tests with exceptions support." ON)

if(CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang" OR
   CMAKE_CXX_COMPILER_ID STREQUAL "Clang" OR
   CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  add_compile_options("-std=c++17")

  add_compile_options(-Wall)
  add_compile_options(-Werror)
  add_compile_options(-Wextra)
  add_compile_options(-pedantic)
  add_compile_options(-Wno-shadow)
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang" AND
   CMAKE_CXX_SIMULATE_ID STREQUAL "MSVC")
  add_compile_options(-Qunused-arguments)
  add_compile_options(-Wno-deprecated-declarations)
  add_compile_options(-Wno-unknown-argument)
endif()

add_definitions(-DGTEST_HAS_TR1_TUPLE=0)
add_definitions(-DGTEST_HAS_STD_TUPLE=1)

add_subdirectory(${CMAKE_SOURCE_DIR}/3rdparty/googletest/googletest
                 ${CMAKE_BINARY_DIR}/3rdparty/googletest/googletest)

config_compiler_and_linker()

if(MPARK_VARIANT_EXCEPTIONS)
  set(compile_flags ${cxx_strict})
else()
  set(compile_flags ${cxx_no_exception})
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
  set_target_properties(${test} PROPERTIES COMPILE_FLAGS "${compile_flags}")
  target_link_libraries(${test} gtest_main mpark_patterns)
  add_test(${test} ${test} --gtest_color=yes)
endforeach()
