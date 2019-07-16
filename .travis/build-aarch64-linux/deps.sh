#!/bin/sh

set -e
set -x

apt-get update
apt-get install -y git cmake gcc python ninja-build g++-8-aarch64-linux-gnu qemu-user

# TODO: This isn't ideal.
cd dynarmic/externals
git clone https://github.com/MerryMage/ext-boost
git clone https://github.com/unicorn-engine/unicorn.git

cd unicorn
UNICORN_ARCHS="arm aarch64" CC=aarch64-linux-gnu-gcc-8 ./make.sh
cd ../..
