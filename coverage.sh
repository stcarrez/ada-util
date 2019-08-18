#!/bin/sh
lcov --base-directory . --directory . -c -o util.cov
bin/util_harness -xml util-aunit.xml
lcov --base-directory . --directory . -c -o util.cov
lcov --remove util.cov "/usr*" -o util.cov
lcov --remove util.cov "/build*" -o util.cov
lcov --remove util.cov "/opt*" -o util.cov
lcov --remove util.cov "regtests*" -o util.cov
lcov --remove util.cov "*/b__util_harness.adb" -o util.cov
lcov --remove util.cov "*/b__util_test_process.adb" -o util.cov
rm -rf cover
genhtml -o ./cover -t "test coverage" --num-spaces 4 util.cov
 
