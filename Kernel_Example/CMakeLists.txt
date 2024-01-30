add_executable (tensoradd_hip_cptr
    ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd_hip_cptr.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel_code.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/tensor_lib_hip_cptr.f90
    )
target_link_libraries(tensoradd_hip_cptr
                      hip::device)

# This is needed so that the linker used is the Fortran compiler
set_target_properties(tensoradd_hip_cptr
    PROPERTIES LINKER_LANGUAGE Fortran
)
                      
install(TARGETS tensoradd_hip_cptr DESTINATION bin)








