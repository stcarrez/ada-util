#!/bin/bash
#  add-junit-result.xht -- Add a junit test result in an XML junit file
#  Copyright (C) 2011 Stephane Carrez
#  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#---------------------------------------------------------------------
#
# Usage: add-junit-result.sh junit-result.xml [-error msg] test-case-name
#
usage () {
   echo "Usage: add-junit-result.sh junit-result.xml [-error msg] test-case-name"
   exit 2
}

DIRNAME=$(readlink -f $0)
DIRNAME=`dirname $DIRNAME`

DIR=
FILE=
TEST=
TNAME=
FILE=$1
shift
while [ $# -gt 0 ]; do
    case $1 in
        -error)
	    shift
	    ERROR=$1
	    ;;

        -name)
	    shift
	    TNAME=$1
	    ;;

        -*)
	    usage
	    ;;

	*)
	    TEST=$1
	    ;;

   esac
   shift
done
if [ "$FILE" = "" ]; then
   usage
fi
if [ "$TEST" = "" ]; then
   usage
fi

# Create an empty Junit XML file
if test ! -f $FILE; then
  cat <<EOF > $FILE
<?xml version="1.0"?>
<testsuite errors="0" failures="0" tests="0" name="$TNAME" time="0">
</testsuite>
EOF

fi

if [ "$ERROR" = "" ]; then
  xsltproc --stringparam test "$TEST" -o t.xml $DIRNAME/add-junit.xsl $FILE
else
  xsltproc --stringparam test "$TEST" --stringparam error "$ERROR" -o t.xml $DIRNAME/add-junit.xsl $FILE
fi
if test $? -eq 0; then
  mv t.xml $FILE
fi
