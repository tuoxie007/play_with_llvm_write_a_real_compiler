#!/bin/sh

#  build.sh
#  play
#
#  Created by Jason Hsu on 2020/2/14.
#  Copyright © 2020 Jason Hsu<tuoxie007@gmail.com>. All rights reserved.

export PATH=/usr/local/Cellar/llvm/9.0.0/bin:$PATH
export PROJECT_DIR=`pwd`/..

clang++ -g -O3 *.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core mcjit native OrcJIT` -std=c++14 -DPROJECT_DIR=\"`pwd`/..\" -o play
