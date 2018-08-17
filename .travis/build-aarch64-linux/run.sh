#!/bin/sh

 docker pull ubuntu:18.04
 docker run -v $(pwd):/dynarmic ubuntu:18.04 dynarmic/.travis/build-aarch64-linux/docker.sh
