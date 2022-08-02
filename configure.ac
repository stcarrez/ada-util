dnl Autoconf configure script for Ada Util
dnl Copyright (C) 2001 - 2021 Stephane Carrez
dnl Written by Stephane Carrez (Stephane.Carrez@gmail.com)
dnl
dnl  Licensed under the Apache License, Version 2.0 (the "License");
dnl  you may not use this file except in compliance with the License.
dnl  You may obtain a copy of the License at
dnl
dnl     http://www.apache.org/licenses/LICENSE-2.0
dnl
dnl  Unless required by applicable law or agreed to in writing, software
dnl  distributed under the License is distributed on an "AS IS" BASIS,
dnl  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
dnl  See the License for the specific language governing permissions and
dnl  limitations under the License.
dnl 
dnl Process this file with autoconf to produce a configure script.

AC_INIT
AC_CONFIG_SRCDIR([src/core/util.ads])

# Current release settings
UTIL_MAJOR_VERSION=2
UTIL_MINOR_VERSION=5
UTIL_MICRO_VERSION=0
UTIL_VERSION=$UTIL_MAJOR_VERSION.$UTIL_MINOR_VERSION.$UTIL_MICRO_VERSION

# checking for local tools
AC_PROG_CC

# Set the version number of Ada Util
AC_SUBST(UTIL_VERSION)
AC_SUBST(UTIL_MAJOR_VERSION)
AC_SUBST(UTIL_MINOR_VERSION)
AC_SUBST(UTIL_MICRO_VERSION)

AM_ADA_UTIL_HARDWARE()

AM_GNAT_LIBRARY_PROJECT(utilada_core)
AC_CHECK_PROG(HAVE_PANDOC,pandoc,yes)
AC_CHECK_PROG(HAVE_DYNAMO,dynamo,yes)

AM_GNAT_LIBRARY_SETUP(utilada_base)
AM_GNAT_LIBRARY_SETUP(utilada_sys)

ac_enable_utilgen=no
AC_ARG_ENABLE(utilgen,
[  --enable-utilgen        Enable the support to port Ada Util on other platforms (disable)],
[case "${enableval}" in
  no|none)  ac_enable_utilgen=no ;;
  *)        ac_enable_utilgen=yes ;;
esac])dnl

AC_MSG_CHECKING([whether utilgen support is required])
HAVE_UTILGEN=$ac_enable_utilgen
AC_MSG_RESULT(${HAVE_UTILGEN})
AC_SUBST(HAVE_UTILGEN)

if test T$ac_enable_utilgen = Tyes; then

  AC_CHECK_HEADERS(dlfcn.h)
  AC_CHECK_LIB(dl,dlopen,[
    CFLAGS="$CFLAGS -DHAVE_DLOPEN=1"
  ])
  UTIL_OS_VERSION=$os_base
else
  UTIL_OS_VERSION=$os_version
fi
AC_SUBST(UTIL_OS_VERSION)

AM_GNAT_FIND_XML_ADA([
    AM_GNAT_LIBRARY_SETUP(utilada_xml)
    WITH_UTILADA_XML="with \"utilada_xml\";"
], [
    WITH_UTILADA_XML=""
])
AC_SUBST(WITH_UTILADA_XML)

##########################################
# Ada LZMA library
##########################################

AM_GNAT_CHECK_PROJECT([lzmada],[lzmada])

if test T$ac_cv_gnat_project_lzmada = Tno; then
  ac_enable_lzma=no
else
  ac_enable_lzma=yes
fi

AC_MSG_CHECKING([Ada LZMA support is enabled])
AC_ARG_ENABLE(lzma,
[  --enable-lzma           Enable the Ada LZMA support in Ada Util (enabled)],
[case "${enableval}" in
  no|none)  ac_enable_lzma=no ;;
  *)        ac_enable_lzma=yes ;;
esac])dnl

AC_MSG_RESULT(${ac_enable_lzma})

AC_ARG_WITH(ada-lzma,
AS_HELP_STRING([--with-ada-lzma=x], [Path for Ada LZMA Library]),
[
  if test T${withval} = Tno ; then
    UTIL_WITH_LZMA=no;
  else
    UTIL_WITH_LZMA='yes';
  fi
],
[
  UTIL_USE_LZMA=yes;
])

if test T$ac_enable_lzma = Tyes; then

AM_GNAT_FIND_PROJECT([ada-lzma],[Ada LZMA Library],[lzmada],
  [git@github.com:stcarrez/ada-lzma.git],
  [Building Ada Utility Library with LZMA support.],
  [
    LZMA_DIR=${ac_cv_gnat_project_dir_lzmada}
  ])
  WITH_LZMA_ADA="with \"${LZMA_DIR}lzmada\";";
  WITH_UTILADA_LZMA="with \"utilada_lzma\";";
  UTIL_HAVE_LZMA='yes';
else
  WITH_LZMA_ADA=''
  WITH_UTILADA_LZMA="";
  UTIL_HAVE_LZMA=no;
fi
AC_SUBST(WITH_LZMA_ADA)
AC_SUBST(WITH_UTILADA_LZMA)
AC_SUBST(UTIL_HAVE_LZMA)

##########################################
# Check for GNAT traceback in Util.Log.Loggers
##########################################

AC_MSG_CHECKING([traceback in log exceptions])
ac_enable_traceback=yes
AC_ARG_ENABLE(traceback,
[  --enable-traceback      Enable the symbolic traceback dump in logged exceptions (enabled)],
[case "${enableval}" in
  no|none)  ac_enable_traceback=no ;;
  *)        ac_enable_traceback=yes ;;
esac])dnl

AC_MSG_RESULT(${ac_enable_traceback})
UTIL_GNAT_TRACEBACK=$ac_enable_traceback
AC_SUBST(UTIL_GNAT_TRACEBACK)

##########################################
# Test framework selection: aunit or ahven
##########################################

ac_enable_aunit=no

ac_enable_ahven=yes
AC_ARG_ENABLE(ahven,
[  --enable-ahven          Enable internal Ahven support for testutil library and Ada util unit tests (enabled)],
[case "${enableval}" in
  no|none)  ac_enable_ahven= ;;
  *)        ac_enable_ahven=yes
            ac_enable_aunit=no ;;
esac])dnl

AC_ARG_ENABLE(aunit,
[  --enable-aunit          Enable AUnit support for testutil library and Ada util unit tests (disabled)],
[case "${enableval}" in
  no|none)  ac_enable_aunit=no ;;
  *)        ac_enable_aunit=yes ;;
esac])dnl

AC_MSG_CHECKING(whether AUnit support is enabled)
AC_MSG_RESULT($ac_enable_aunit)

AC_MSG_CHECKING(whether Ahven internal support is enabled)
AC_MSG_RESULT($ac_enable_ahven)

if test T$ac_enable_aunit = Tyes; then
   AM_GNAT_CHECK_PROJECT([aunit],[aunit])
   if test T$ac_cv_gnat_project_aunit = Tno; then
          if test T$ac_enable_ahven = Tno; then
             AC_MSG_WARN(AUnit is not installed and Ahven is not enabled.)
             AC_MSG_ERROR(Please install AUnit or enable Ahven with --enable-ahven.)
          fi
   fi
fi

if test x$ac_enable_ahven = 'xyes' -a x$ac_enable_aunit = 'xyes'; then
   AC_MSG_ERROR([Both Ahven and AUnit testing framework are selected.  Please, choose one of them.])
fi

UTIL_XUNIT_TYPE=
WITH_AUNIT=

if test x$ac_enable_aunit = 'xyes'; then
  WITH_AUNIT="with \"aunit\";"
  UTIL_XUNIT_TYPE="aunit"
fi
if test x$ac_enable_ahven = 'xyes'; then
  UTIL_AHVEN_DIR=testutil/ahven
  UTIL_XUNIT_TYPE="ahven-${os_base}"
fi

##########################################
# Check for libcurl
##########################################

UTIL_HAVE_CURL=no
WITH_UTIL_CURL=""
UTIL_CURL_LIB=curl

AC_MSG_CHECKING([curl HTTP client support in Ada Util])
ac_enable_curl=yes
AC_ARG_ENABLE(curl,
[  --enable-curl           Enable the CURL HTTP client support in Ada Util (enabled)],
[case "${enableval}" in
  no|none)  ac_enable_curl=no ;;
  *)        ac_enable_curl=yes ;;
esac])dnl

AC_MSG_RESULT(${ac_enable_curl})
if test T$ac_enable_curl = Tyes; then
  AC_CHECK_LIB($UTIL_CURL_LIB,curl_easy_perform,[
    UTIL_HAVE_CURL=yes
    CFLAGS="$CFLAGS -DHAVE_CURL"
    WITH_UTIL_CURL="with \"utilada_curl\";";
  ])

  AM_GNAT_LIBRARY_SETUP(utilada_curl)
fi

##########################################
# Check for Ada Web Server
##########################################
AM_ADA_UTIL_AWS_VERSION()

AC_ARG_ENABLE(link-options-util,
[  --enable-link-options-util=opts
                          Add some linker options when building the Ada Util shared library (disabled)],
[
  UTIL_LIBRARY_OPTIONS=${enableval}
])
AC_MSG_CHECKING([for additional linker options for libutilada])
if test "T${UTIL_LIBRARY_OPTIONS}" != T; then
    UTIL_LIBRARY_OPTIONS="-Wl,${UTIL_LIBRARY_OPTIONS}"
    opts=${UTIL_LIBRARY_OPTIONS}
else
    opts=none
fi
AC_MSG_RESULT($opts)
AC_SUBST(UTIL_LIBRARY_OPTIONS)

AC_ARG_ENABLE(link-options-curl,
[  --enable-link-options-curl=opts
                          Add some linker options when building the Ada Util Curl shared library (disabled)],
[
  UTIL_CURL_LIBRARY_OPTIONS=${enableval}
])
AC_MSG_CHECKING([for additional linker options for libutilada_curl])
if test "T${UTIL_CURL_LIBRARY_OPTIONS}" != T; then
    UTIL_CURL_LIBRARY_OPTIONS="-Wl,${UTIL_CURL_LIBRARY_OPTIONS}"
    opts=${UTIL_CURL_LIBRARY_OPTIONS}
else
    opts=none
fi
AC_MSG_RESULT($opts)
AC_SUBST(UTIL_CURL_LIBRARY_OPTIONS)

AM_GNAT_CHECK_INSTALL

AC_SUBST(UTIL_HAVE_AWS)
AC_SUBST(WITH_UTIL_AWS)
AC_SUBST(WITH_SERVER)

AC_SUBST(UTIL_HAVE_CURL)
AC_SUBST(WITH_UTIL_CURL)
AC_SUBST(WITH_AUNIT)
AC_SUBST(UTIL_XUNIT_TYPE)

dnl AC_MSG_NOTICE(  XML support:   $WITH_XML_ADA)
dnl AC_MSG_NOTICE(  CURL support:  $WITH_UTIL_CURL)
dnl AC_MSG_NOTICE(  AWS support:   $WITH_UTIL_AWS)

AC_CONFIG_FILES([Makefile.conf
utilada.gpr
utilada_http.gpr
utilada_unit.gpr
utilada_xml.gpr
utilada_lzma.gpr
utilada_tests.gpr
utilada_conf.gpr
])
AC_OUTPUT
