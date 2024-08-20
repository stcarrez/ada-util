#!/bin/sh
NAME=util.cov
alr exec -- lcov --quiet --base-directory . --directory . \
   --no-external \
   --exclude '*/b__*.adb' \
   --exclude '*/samples/*' \
   --exclude '*/regtests*' \
   --exclude '*/src/tests/*' -c -o $NAME
rm -rf cover
genhtml --quiet -o ./cover -t "test coverage" --num-spaces 4 $NAME
 
