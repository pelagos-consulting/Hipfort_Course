
# Paged memory example
add_executable (paged_mem
    ${CMAKE_CURRENT_SOURCE_DIR}/paged_mem.f90
    $<TARGET_OBJECTS:common_lib>
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
    $<TARGET_OBJECTS:common_lib>
)
# Link in other libraries
target_link_libraries(pinned_mem ${kernel_libs})
# This is needed so that the linker used is the Fortran compiler
set_target_properties(pinned_mem
    PROPERTIES LINKER_LANGUAGE Fortran
)                      
install(TARGETS pinned_mem DESTINATION bin)

# Managed memory example
add_executable (managed_mem
    ${CMAKE_CURRENT_SOURCE_DIR}/managed_mem.f90
    $<TARGET_OBJECTS:common_lib>
)
# Link in other libraries
target_link_libraries(managed_mem ${kernel_libs})
# This is needed so that the linker used is the Fortran compiler
set_target_properties(managed_mem
    PROPERTIES LINKER_LANGUAGE Fortran
)                      
install(TARGETS managed_mem DESTINATION bin)


# Memory transfers using synchronous interface
add_executable (memcpy_sync
    ${CMAKE_CURRENT_SOURCE_DIR}/memcpy_sync.f90
    $<TARGET_OBJECTS:common_lib>
)
# Link in other libraries
target_link_libraries(memcpy_sync ${kernel_libs})
# This is needed so that the linker used is the Fortran compiler
set_target_properties(memcpy_sync
    PROPERTIES LINKER_LANGUAGE Fortran
)                      
install(TARGETS memcpy_sync DESTINATION bin)

# Memory transfers using asynchronous interface
add_executable (memcpy_async
    ${CMAKE_CURRENT_SOURCE_DIR}/memcpy_async.f90
    $<TARGET_OBJECTS:common_lib>
)
# Link in other libraries
target_link_libraries(memcpy_async ${kernel_libs})
# This is needed so that the linker used is the Fortran compiler
set_target_properties(memcpy_async
    PROPERTIES LINKER_LANGUAGE Fortran
)                      
install(TARGETS memcpy_async DESTINATION bin)

# tensoradd_hip_pinned - tensor addition with HIP and pinned memory
add_executable (tensoradd_hip_pinned
    ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd_hip_pinned.f90
    # Use same kernel object library from Lesson 5
    $<TARGET_OBJECTS:kernels_hipfort_example>
    $<TARGET_OBJECTS:common_lib>
)
# Link in other libraries
target_link_libraries(tensoradd_hip_pinned ${kernel_libs})
# This is needed so that the linker used is the Fortran compiler
set_target_properties(tensoradd_hip_pinned
    PROPERTIES LINKER_LANGUAGE Fortran
)                   
install(TARGETS tensoradd_hip_pinned DESTINATION bin)


# tensoradd_hip_managed - Tensor addition with HIP and managed memory
add_executable (tensoradd_hip_managed
    ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd_hip_managed.f90
    # Use same kernel object library from Lesson 5
    $<TARGET_OBJECTS:kernels_hipfort_example>
    $<TARGET_OBJECTS:common_lib>
)
# Link in other libraries
target_link_libraries(tensoradd_hip_managed ${kernel_libs})
# This is needed so that the linker used is the Fortran compiler
set_target_properties(tensoradd_hip_managed
    PROPERTIES LINKER_LANGUAGE Fortran
)                   
install(TARGETS tensoradd_hip_managed DESTINATION bin)

