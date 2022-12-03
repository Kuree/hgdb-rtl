cmake_minimum_required(VERSION 3.24)
project(hgdb_rtl)

set(CMAKE_CXX_STANDARD 17)

if (CMAKE_BUILD_TYPE MATCHES "Debug")
    add_compile_options(-DDEBUG)
endif ()

add_subdirectory(extern/slang)

add_subdirectory(src)
add_subdirectory(tools)
