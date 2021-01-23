#!/bin/sh
NAME=util.cov
lcov --quiet --base-directory . --directory . -c -o $NAME
lcov --quiet --remove $NAME "/usr*" -o $NAME
lcov --quiet --remove $NAME "/build*" -o $NAME
lcov --quiet --remove $NAME "/opt*" -o $NAME
lcov --quiet --remove $NAME "*/regtests*" -o $NAME
lcov --quiet --remove $NAME "*/src/tests/*" -o $NAME
lcov --quiet --remove $NAME "*/b__util_harness.adb" -o $NAME
lcov --quiet --remove $NAME "*/b__util_test_process.adb" -o $NAME
rm -rf cover
genhtml --quiet -o ./cover -t "test coverage" --num-spaces 4 $NAME
 
