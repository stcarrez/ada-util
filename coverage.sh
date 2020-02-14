#!/bin/sh
NAME=util.cov
lcov --quiet --base-directory . --directory . -c --include "*/ada-util/src/*" -o $NAME
lcov --quiet --remove $NAME "*/src/tests/*" -o $NAME
rm -rf cover
genhtml --quiet -o ./cover -t "test coverage" --num-spaces 4 $NAME

 
