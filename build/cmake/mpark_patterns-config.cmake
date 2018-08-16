# MPark.Patterns
#
# Copyright Michael Park, 2017
#
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE.md or copy at http://boost.org/LICENSE_1_0.txt)

# Config file for MPark.Patterns
#
#   `MPARK_PATTERNS_INCLUDE_DIRS` - include directories
#   `MPARK_PATTERNS_LIBRARIES`    - libraries to link against
#
# The following `IMPORTED` target is also defined:
#
#   `mpark_patterns`


####### Expanded from @PACKAGE_INIT@ by configure_package_config_file() #######
####### Any changes to this file will be overwritten by the next CMake run ####
####### The input file was mpark_patterns-config.cmake.in                            ########

get_filename_component(PACKAGE_PREFIX_DIR "${CMAKE_CURRENT_LIST_DIR}/../../../" ABSOLUTE)

macro(set_and_check _var _file)
  set(${_var} "${_file}")
  if(NOT EXISTS "${_file}")
    message(FATAL_ERROR "File or directory ${_file} referenced by variable ${_var} does not exist !")
  endif()
endmacro()

####################################################################################

include("${CMAKE_CURRENT_LIST_DIR}/mpark_patterns-targets.cmake")

get_target_property(
  MPARK_PATTERNS_INCLUDE_DIRS
  mpark_patterns INTERFACE_INCLUDE_DIRECTORIES)

set_and_check(MPARK_PATTERNS_INCLUDE_DIRS "${MPARK_PATTERNS_INCLUDE_DIRS}")
set(MPARK_PATTERNS_LIBRARIES mpark_patterns)
