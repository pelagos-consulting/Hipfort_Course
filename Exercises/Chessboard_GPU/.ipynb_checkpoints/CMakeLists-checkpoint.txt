# Chessboard on the GPU, the exercise
add_executable (chessboard_GPU
    ${CMAKE_CURRENT_SOURCE_DIR}/chessboard_GPU.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel_code.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/hip_utils.f90
    )
target_link_libraries(chessboard_GPU
                      hip::device)

# This is needed so that the linker used is the Fortran compiler
set_target_properties(chessboard_GPU
    PROPERTIES LINKER_LANGUAGE Fortran
)
                      
install(TARGETS chessboard_GPU DESTINATION bin)


# Chessboard on the GPU, the answer
add_executable (chessboard_GPU_answer
    ${CMAKE_CURRENT_SOURCE_DIR}/chessboard_GPU_answer.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/kernel_code_answer.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/hip_utils.f90
    )
target_link_libraries(chessboard_GPU_answer
                      hip::device)

# This is needed so that the linker used is the Fortran compiler
set_target_properties(chessboard_GPU_answer
    PROPERTIES LINKER_LANGUAGE Fortran
)
                      
install(TARGETS chessboard_GPU_answer DESTINATION bin)







