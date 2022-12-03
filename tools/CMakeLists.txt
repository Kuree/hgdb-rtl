add_executable(hgdb-rtl-bin hgdb-rtl.cc)
target_link_libraries(hgdb-rtl-bin hgdb-rtl)
set_target_properties(hgdb-rtl-bin PROPERTIES OUTPUT_NAME "hgdb-rtl")
target_include_directories(hgdb-rtl-bin PRIVATE ${CMAKE_SOURCE_DIR}/src)