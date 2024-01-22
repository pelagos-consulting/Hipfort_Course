#!/bin/bash

# Source this file to get the installation directory
source ./env

mkdir -p build
cd build

# Set Fortran compiler
export FC=hipfc 

echo "cmake -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR/ -DCMAKE_BUILD_TYPE=RELEASE $COURSE_DIR/"

make clean
make install

