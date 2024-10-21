# Chessboard on the GPU, the exercise

# Header files for the shortcut answer
set(HEADER_FILES
    step0_kernel.h
    step1_init.h
    step2_malloc.h
    step3_launch.h
    step4_copy.h
    step5_free.h
    step6_reset.h)

# Kernels for the example
set(KERNEL_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel_code.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel_code_answer.cpp
)
# Use this switch to make sure either the NVIDIA or AMD compilers are used to compile the kernel code.
set_source_files_properties(${KERNEL_FILES} PROPERTIES LANGUAGE ${kernel_lang})

# Target for the chessboard_GPU code
add_executable (chessboard_GPU
    ${CMAKE_CURRENT_SOURCE_DIR}/chessboard_GPU.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel_code.cpp
    ${HEADER_FILES}
    $<TARGET_OBJECTS:common_lib>
    )   
target_link_libraries(chessboard_GPU
                      ${kernel_libs})
                      
# This is needed so that the linker used is the Fortran compiler
set_target_properties(chessboard_GPU
    PROPERTIES LINKER_LANGUAGE Fortran
)                 
target_include_directories(chessboard_GPU PRIVATE ${COURSE_MATERIAL_DIR}/src)
install(TARGETS chessboard_GPU DESTINATION bin)


# Chessboard on the GPU, the answer
add_executable (chessboard_GPU_answer
    ${CMAKE_CURRENT_SOURCE_DIR}/chessboard_GPU_answer.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel_code_answer.cpp
    $<TARGET_OBJECTS:common_lib>
    )
target_link_libraries(chessboard_GPU_answer
                      ${kernel_libs})

# This is needed so that the linker used is the Fortran compiler
set_target_properties(chessboard_GPU_answer
    PROPERTIES LINKER_LANGUAGE Fortran
)
target_include_directories(chessboard_GPU_answer PRIVATE ${COURSE_MATERIAL_DIR}/src)
install(TARGETS chessboard_GPU_answer DESTINATION bin)







