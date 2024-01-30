add_executable (tensoradd_hip_cptr 
    ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd.f90 
    ${CMAKE_CURRENT_SOURCE_DIR}/tensor_lib_c_ptr.f90 
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel_code.cpp)
target_link_libraries(tensoradd_module
                      hip::device)
install(TARGETS tensoradd_hip_cptr DESTINATION bin)








