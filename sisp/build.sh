#!/bin/sh

#  build.sh
#  sisp
#
#  Created by 徐可 on 2020/2/14.
#  Copyright © 2020 Beibei Inc. All rights reserved.

llvm-config --cxxflags --ldflags --system-libs --libs core mcjit native OrcJIT
