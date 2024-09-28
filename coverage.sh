#!/bin/sh
NAME=util.cov
alr exec -- lcov --quiet --base-directory . --directory . \
   --no-external  --ignore-errors gcov,unused \
   --exclude '*/b__*.adb' \
   --exclude '*/<unknown>' \
   --exclude '*/samples/*' \
   --exclude '*/regtests*' \
   --exclude '*/src/tests/*' -c -o $NAME
rm -rf cover
genhtml --quiet -o ./cover -t "test coverage" --num-spaces 4 $NAME
 
