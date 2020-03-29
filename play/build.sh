#!/bin/sh

#  build.sh
#  play
#
#  Created by Jason Hsu on 2020/2/14.
#  Copyright © 2020 Jason Hsu<tuoxie007@gmail.com>. All rights reserved.

llvm-config --cxxflags --ldflags --system-libs --libs core mcjit native OrcJIT
