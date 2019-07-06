#!/bin/sh

set -e
set -x

apt-get update
apt-get install -y git cmake gcc ninja-build g++-8-aarch64-linux-gnu qemu-user

# TODO: This isn't ideal.
cd dynarmic/externals
git clone https://github.com/MerryMage/ext-boost
cd ..
