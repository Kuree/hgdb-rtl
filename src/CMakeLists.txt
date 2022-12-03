add_library(hgdb-rtl rtl.cc)
target_link_libraries(hgdb-rtl slang::slang)
target_include_directories(hgdb-rtl PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/../extern/hgdb/include
        ${CMAKE_BINARY_DIR}/_deps/fmt-src/include)
