
# Set the language for any kernel files
set(KERNEL_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel_code.cpp
)
set_source_files_properties(${KERNEL_FILES} PROPERTIES LANGUAGE ${kernel_lang})


# Tensor addition with HIP and C pointers
add_executable (tensoradd_hip_cptr
    ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd_hip_cptr.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel_code.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/hip_utils.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/math_utils.f90
    )
target_link_libraries(tensoradd_hip_cptr ${kernel_libs})

# This is needed so that the linker used is the Fortran compiler
set_target_properties(tensoradd_hip_cptr
    PROPERTIES LINKER_LANGUAGE Fortran
)
                      
install(TARGETS tensoradd_hip_cptr DESTINATION bin)


# Tensor addition with HIP and Fortran pointers
add_executable (tensoradd_hip_fptr
    ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd_hip_fptr.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel_code.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/hip_utils.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/math_utils.f90
    )
target_link_libraries(tensoradd_hip_fptr ${kernel_libs})

# This is needed so that the linker used is the Fortran compiler
set_target_properties(tensoradd_hip_fptr
    PROPERTIES LINKER_LANGUAGE Fortran
)
                      
install(TARGETS tensoradd_hip_fptr DESTINATION bin)

# Tensor addition with HIP and Fortran object-oriented types
add_executable (tensoradd_hip_oo
    ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd_hip_oo.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel_code.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/hip_utils.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/math_utils.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/tensor_hip.f90
    )
target_link_libraries(tensoradd_hip_oo ${kernel_libs})

# This is needed so that the linker used is the Fortran compiler
set_target_properties(tensoradd_hip_oo
    PROPERTIES LINKER_LANGUAGE Fortran
)
                      
install(TARGETS tensoradd_hip_oo DESTINATION bin)




