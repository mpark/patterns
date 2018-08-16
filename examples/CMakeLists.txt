cmake_minimum_required(VERSION 3.6.3)

set (CMAKE_CXX_STANDARD 17)

add_executable(fizzbuzz fizzbuzz.cpp)
target_link_libraries(fizzbuzz mpark_patterns)

add_executable(fibonacci fibonacci.cpp)
target_link_libraries(fibonacci mpark_patterns)

add_executable(factorial factorial.cpp)
target_link_libraries(factorial mpark_patterns)

