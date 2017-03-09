# MPark.Patterns
#
# Copyright Michael Park, 2017
#
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE_1_0.txt or copy at
# http://www.boost.org/LICENSE_1_0.txt)

cmake_minimum_required(VERSION 3.6.3)

project(MPark.Patterns VERSION 1.0.0 LANGUAGES CXX)

# Option.
option(MPARK_PATTERNS_INCLUDE_TESTS "Build tests." OFF)

# Dependency on `mpark/variant`.
add_subdirectory(3rdparty/variant)

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
  enable_testing()
  add_subdirectory(3rdparty/googletest/googletest)
  add_subdirectory(test)
endif()
