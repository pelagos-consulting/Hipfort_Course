add_executable (chessboard_CPU_answer 
    ${CMAKE_CURRENT_SOURCE_DIR}/chessboard_CPU_answer.f90) 
install(TARGETS chessboard_CPU_answer DESTINATION bin)

add_executable (chessboard_CPU 
    ${CMAKE_CURRENT_SOURCE_DIR}/chessboard_CPU.f90) 
install(TARGETS chessboard_CPU DESTINATION bin)






