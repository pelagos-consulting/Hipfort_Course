# Chessboard on the GPU, the exercise
set(KERNEL_FILES
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel_code.cpp
)
# Use this switch to make sure either the NVIDIA or AMD compilers are used to compile the kernel code.
set_source_files_properties(${KERNEL_FILES} PROPERTIES LANGUAGE ${kernel_lang})

# Chessboard on the GPU
add_executable (chessboard_mm
    ${CMAKE_CURRENT_SOURCE_DIR}/chessboard_mm.f90
    ${KERNEL_FILES}
    ${PROJECT_SOURCE_DIR}/src/hip_utils.f90
    )
target_link_libraries(chessboard_mm
                      ${kernel_libs})

# This is needed so that the linker used is the Fortran compiler
set_target_properties(chessboard_mm
    PROPERTIES LINKER_LANGUAGE Fortran
)
                      
install(TARGETS chessboard_mm DESTINATION bin)

# Chessboard on the GPU, the answer
add_executable (chessboard_mm_answer
    ${CMAKE_CURRENT_SOURCE_DIR}/chessboard_mm_answer.f90
    ${KERNEL_FILES}
    ${PROJECT_SOURCE_DIR}/src/hip_utils.f90
    )
target_link_libraries(chessboard_mm_answer
                      ${kernel_libs})

# This is needed so that the linker used is the Fortran compiler
set_target_properties(chessboard_mm_answer
    PROPERTIES LINKER_LANGUAGE Fortran
)
                      
install(TARGETS chessboard_mm_answer DESTINATION bin)





