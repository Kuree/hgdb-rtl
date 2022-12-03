add_library(hgdb-rtl rtl.cc)
target_link_libraries(hgdb-rtl slang)
target_include_directories(hgdb-rtl PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../extern/slang/include
        ${CMAKE_CURRENT_SOURCE_DIR}/../extern/hgdb/include
        ${CMAKE_CURRENT_SOURCE_DIR}/../extern/slang/external
        ${CMAKE_BINARY_DIR}/extern/slang/source/
        ${CMAKE_BINARY_DIR}/_deps/unordered_dense-src/include/
        ${CMAKE_BINARY_DIR}/_deps/fmt-src/include)