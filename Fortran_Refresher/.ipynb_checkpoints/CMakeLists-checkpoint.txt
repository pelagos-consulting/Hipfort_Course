add_executable (tensoradd_simple ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd_simple.f90)
##target_link_libraries(tensoradd_simple 
##                     hip::device)
install(TARGETS tensoradd_simple DESTINATION bin)

add_executable (tensoradd_allocatable ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd_allocatable.f90)
##target_link_libraries(tensoradd_allocatable 
##                      hip::device)
install(TARGETS tensoradd_allocatable DESTINATION bin)

add_executable (tensoradd_pointer ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd_pointer.f90)
##target_link_libraries(tensoradd_pointer 
##                      hip::device)
install(TARGETS tensoradd_pointer DESTINATION bin)

add_executable (tensoradd_function ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd_function.f90)
##target_link_libraries(tensoradd_function 
##                      hip::device)
install(TARGETS tensoradd_function DESTINATION bin)                      

add_executable (tensoradd_module 
    ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd_module.f90 
    ${CMAKE_CURRENT_SOURCE_DIR}/tensor_lib.f90 
    ${CMAKE_CURRENT_SOURCE_DIR}/c_functions.cpp)
##target_link_libraries(tensoradd_module
##                      hip::device)
install(TARGETS tensoradd_module DESTINATION bin)

add_executable (tensoradd_cfun 
    ${CMAKE_CURRENT_SOURCE_DIR}/tensoradd_cfun.f90 
    ${CMAKE_CURRENT_SOURCE_DIR}/tensor_lib_c.f90
    ${CMAKE_CURRENT_SOURCE_DIR}/c_functions.cpp)
##target_link_libraries(tensoradd_cfun
##                      hip::device)
set_source_files_properties(${CMAKE_CURRENT_SOURCE_DIR}/c_functions.cpp PROPERTIES LANGUAGE HIP)
install(TARGETS tensoradd_cfun DESTINATION bin)






