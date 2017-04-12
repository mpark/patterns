# MPark.Patterns
#
# Copyright Michael Park, 2017
#
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE_1_0.txt or copy at
# http://www.boost.org/LICENSE_1_0.txt)

cmake_minimum_required(VERSION 3.6.3)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" AND
   "${CMAKE_CXX_SIMULATE_ID}" STREQUAL "MSVC")
  add_compile_options(-Qunused-arguments)
  add_compile_options(-Wno-deprecated-declarations)
  add_compile_options(-Wno-unknown-argument)
endif()

if(MPARK_PATTERNS_INCLUDE_TESTS)

  function(add_gtest name)
    add_executable(${name} ${name}.cpp)
    set_target_properties(${name} PROPERTIES COMPILE_FLAGS "${cxx_strict}")
    target_link_libraries(${name} gtest_main mpark_patterns mpark_variant)
    add_test(${name} ${name} --gtest_color=yes)
  endfunction(add_gtest)

  add_gtest(intro)
  add_gtest(calc)

endif()
