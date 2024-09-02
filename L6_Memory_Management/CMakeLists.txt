
# Set all kernel sources 
##set(KERNEL_FILES
#    ${CMAKE_CURRENT_SOURCE_DIR}/kernel_code.cpp
#)
# Set the language of kernel sources
##set_source_files_properties(${KERNEL_FILES} PROPERTIES LANGUAGE ${kernel_lang})

## Files common to all examples - language is determined by file extension
set(COMMON_FILES
    ${PROJECT_SOURCE_DIR}/src/hip_utils.f90
    ${PROJECT_SOURCE_DIR}/src/math_utils.f90
)

# Paged memory example
add_executable (paged_mem
    ${CMAKE_CURRENT_SOURCE_DIR}/paged_mem.f90
    #${KERNEL_FILES}
    ${COMMON_FILES}
)
# Link in other libraries
target_link_libraries(paged_mem ${kernel_libs})
# This is needed so that the linker used is the Fortran compiler
set_target_properties(paged_mem
    PROPERTIES LINKER_LANGUAGE Fortran
)                      
install(TARGETS paged_mem DESTINATION bin)

# Pinned memory example
add_executable (pinned_mem
    ${CMAKE_CURRENT_SOURCE_DIR}/pinned_mem.f90
    #${KERNEL_FILES}
    ${COMMON_FILES}
)
# Link in other libraries
target_link_libraries(pinned_mem ${kernel_libs})
# This is needed so that the linker used is the Fortran compiler
set_target_properties(pinned_mem
    PROPERTIES LINKER_LANGUAGE Fortran
)                      
install(TARGETS pinned_mem DESTINATION bin)







