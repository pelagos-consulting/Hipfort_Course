
# Set all kernel sources 
set(KERNEL_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel_code.cpp
)
# Set the language of kernel sources
set_source_files_properties(${KERNEL_FILES} PROPERTIES LANGUAGE ${kernel_lang})
add_library(kernel OBJECT "${KERNEL_FILES}")
# Make sure kinds.h in src directory is included
target_include_directories(kernel PRIVATE ${COURSE_MATERIAL_DIR}/src)
    
# tensoradd_hip_cptr - Tensor addition with HIP and C pointers
add_executable (tensoradd_hip_cptr
    ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd_hip_cptr.f90
    $<TARGET_OBJECTS:kernel>
    $<TARGET_OBJECTS:common_lib>
)
# Link in other libraries
target_link_libraries(tensoradd_hip_cptr ${kernel_libs})
# This is needed so that the linker used is the Fortran compiler
set_target_properties(tensoradd_hip_cptr
    PROPERTIES LINKER_LANGUAGE Fortran
)                      
install(TARGETS tensoradd_hip_cptr DESTINATION bin)


# tensoradd_hip_fptr - Tensor addition with HIP and Fortran pointers
add_executable (tensoradd_hip_fptr
    ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd_hip_fptr.f90
    $<TARGET_OBJECTS:kernel>
    $<TARGET_OBJECTS:common_lib>
)
# Link in other libraries
target_link_libraries(tensoradd_hip_fptr ${kernel_libs})
# This is needed so that the linker used is the Fortran compiler
set_target_properties(tensoradd_hip_fptr
    PROPERTIES LINKER_LANGUAGE Fortran
)                   
install(TARGETS tensoradd_hip_fptr DESTINATION bin)


# Tensor addition with HIP and Fortran object-oriented types
add_executable (tensoradd_hip_oo
    ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd_hip_oo.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/tensor_hip.f90
    $<TARGET_OBJECTS:kernel>
    $<TARGET_OBJECTS:common_lib>
    )
# Link in other libraries
##target_link_libraries(tensoradd_hip_oo ${kernel_libs} $<TARGET_OBJECTS:kinds_lib>)
target_link_libraries(tensoradd_hip_oo ${kernel_libs})


# This is needed so that the linker used is the Fortran compiler
set_target_properties(tensoradd_hip_oo
    PROPERTIES LINKER_LANGUAGE Fortran
)
install(TARGETS tensoradd_hip_oo DESTINATION bin)


# tensoradd_hip_fptr_managed - Tensor addition with HIP and Fortran pointers and managed memory
add_executable (tensoradd_hip_fptr_managed
    ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd_hip_fptr_managed.f90
    $<TARGET_OBJECTS:kernel>
    $<TARGET_OBJECTS:common_lib>
)
# Link in other libraries
target_link_libraries(tensoradd_hip_fptr_managed ${kernel_libs})
# This is needed so that the linker used is the Fortran compiler
set_target_properties(tensoradd_hip_fptr_managed
    PROPERTIES LINKER_LANGUAGE Fortran
)                   
install(TARGETS tensoradd_hip_fptr_managed DESTINATION bin)




