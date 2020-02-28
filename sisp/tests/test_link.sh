#!/bin/sh

#  test_link.sh
#  sisp
#
#  Created by 徐可 on 2020/2/27.
#  Copyright © 2020 Beibei Inc. All rights reserved.

xcrun cc call_triple.c link.o -o call_triple
./call_triple
if [[ "$?" == "42" ]]; then
    echo "Pass"
else
    echo "Fail"
fi
