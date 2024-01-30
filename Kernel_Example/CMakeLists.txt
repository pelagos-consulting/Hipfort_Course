add_executable (tensoradd_hip_cptr
    ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel_code.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tensor_lib_hip_cptr.f90
    )
target_link_libraries(tensoradd_hip_cptr
                      hip::device)
install(TARGETS tensoradd_hip_cptr DESTINATION bin)








