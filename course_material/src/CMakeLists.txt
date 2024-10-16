
# Make the kinds_lib library
add_library(kinds_lib OBJECT kinds.f90)

# Make output module from kinds_lib accessible in CMAKE_Fortran_MODULE_DIRECTORY
set_target_properties(kinds_lib PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY})

# Make the common_lib library
add_library(common_lib OBJECT
    hip_utils.f90
    math_utils.f90
)

# Put the output module from common_lib accessible in CMAKE_Fortran_MODULE_DIRECTORY
set_target_properties(common_lib PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY})

# Establish that common_lib depends on kinds_lib
target_link_libraries(common_lib PUBLIC $<TARGET_OBJECTS:kinds_lib>)


