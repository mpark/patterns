# MPark.Patterns
#
# Copyright Michael Park, 2017
#
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

cmake_minimum_required(VERSION 3.8.2)

project(MPark.Patterns VERSION 1.0.0 LANGUAGES CXX)

# Option.
option(MPARK_PATTERNS_INCLUDE_TESTS "Build tests." OFF)

# Target.
add_library(mpark_patterns INTERFACE)
target_include_directories(mpark_patterns INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>)

# Config.
include(CMakePackageConfigHelpers)

configure_package_config_file(
  cmake/mpark_patterns-config.cmake.in
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/mpark_patterns-config.cmake"
  INSTALL_DESTINATION lib/cmake/mpark_patterns
  NO_CHECK_REQUIRED_COMPONENTS_MACRO)

write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/mpark_patterns-config-version.cmake"
  COMPATIBILITY AnyNewerVersion)

# Export.
export(
  TARGETS mpark_patterns
  FILE "${CMAKE_CURRENT_BINARY_DIR}/cmake/mpark_patterns-targets.cmake")

# Install.
install(TARGETS mpark_patterns EXPORT mpark_patterns)

install(
  EXPORT mpark_patterns
  FILE mpark_patterns-targets.cmake
  DESTINATION lib/cmake/mpark_patterns)

install(DIRECTORY include/mpark DESTINATION include)

install(
  FILES
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/mpark_patterns-config.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake/mpark_patterns-config-version.cmake"
  DESTINATION lib/cmake/mpark_patterns)

if(MPARK_PATTERNS_INCLUDE_TESTS)
  if("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" AND
     "${CMAKE_CXX_SIMULATE_ID}" STREQUAL "MSVC")
    add_compile_options(-Qunused-arguments)
    add_compile_options(-Wno-deprecated-declarations)
    add_compile_options(-Wno-unknown-argument)
  endif()

  add_definitions(-DGTEST_HAS_TR1_TUPLE=0)
  add_definitions(-DGTEST_HAS_STD_TUPLE=1)
  add_subdirectory(3rdparty/googletest/googletest)
  config_compiler_and_linker()

  enable_testing()
  add_subdirectory(test)
endif()
