#!/bin/sh

set -e
set -x

# TODO: This isn't ideal.
cd externals
git clone https://github.com/MerryMage/ext-boost
git clone https://github.com/unicorn-engine/unicorn.git

cd unicorn
UNICORN_ARCHS="arm aarch64" CC=aarch64-linux-gnu-gcc-8 ./make.sh
cd ../..
