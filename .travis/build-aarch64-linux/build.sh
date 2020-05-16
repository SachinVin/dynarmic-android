#!/bin/sh

set -e
set -x

export CC=aarch64-linux-gnu-gcc-8
export CXX=aarch64-linux-gnu-g++-8

mkdir build && cd build
cmake .. -DBoost_INCLUDE_DIRS=${PWD}/../externals/ext-boost -DCMAKE_BUILD_TYPE=Release -G Ninja
ninja

qemu-aarch64 -L /usr/aarch64-linux-gnu ./tests/dynarmic_tests -d yes
