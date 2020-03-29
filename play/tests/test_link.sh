#!/bin/sh

#  test_link.sh
#  play
#
#  Created by Jason Hsu on 2020/2/27.
#  Copyright © 2020 Jason Hsu<tuoxie007@gmail.com>. All rights reserved.

xcrun cc call_triple.c link.o
./a.out
if [[ "$?" == "42" ]]; then
    echo "Pass"
else
    echo "Fail"
fi
