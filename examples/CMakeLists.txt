cmake_minimum_required(VERSION 3.18)

project(example C CXX)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_executable(example main.cpp)
target_link_libraries(example PRIVATE compiler_lib SDL3::SDL3)
file(COPY script DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
add_custom_command(TARGET example POST_BUILD COMMAND "${CMAKE_COMMAND}" -E copy "script" "build/examples/script")
