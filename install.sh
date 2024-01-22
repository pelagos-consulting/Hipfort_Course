#!/bin/bash

# Source this file to get the installation directory
source ./env

# Make the directory to build in
mkdir -p build
cd build

# Run cmake
cmake -DCMAKE_INSTALL_PREFIX=$INSTALL_DIR/ -DCMAKE_BUILD_TYPE=$RELEASE $COURSE_DIR/

#make clean
make install

