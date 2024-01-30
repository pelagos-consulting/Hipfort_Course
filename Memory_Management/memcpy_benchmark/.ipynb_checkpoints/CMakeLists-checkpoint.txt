add_executable (memcpy_bench ${CMAKE_CURRENT_SOURCE_DIR}/memcpy_bench.f90)

target_link_libraries(memcpy_bench
                      hip::device)

install(TARGETS memcpy_bench DESTINATION bin)
