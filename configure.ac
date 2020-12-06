dnl Autoconf configure script for Ada Util
dnl Copyright (C) 2001 - 2020 Stephane Carrez
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

AC_INIT(src/core/util.ads)

# Current release settings
UTIL_MAJOR_VERSION=2
UTIL_MINOR_VERSION=3
UTIL_MICRO_VERSION=0
UTIL_VERSION=$UTIL_MAJOR_VERSION.$UTIL_MINOR_VERSION.$UTIL_MICRO_VERSION

# checking for local tools
AC_PROG_CC

# Set the version number of Ada Util
AC_SUBST(UTIL_VERSION)
AC_SUBST(UTIL_MAJOR_VERSION)
AC_SUBST(UTIL_MINOR_VERSION)
AC_SUBST(UTIL_MICRO_VERSION)

AC_CHECK_PROG(HAVE_PANDOC,pandoc,yes)
AC_CHECK_PROG(HAVE_DYNAMO,dynamo,yes)

AM_GNAT_LIBRARY_PROJECT(utilada_core)
AM_GNAT_LIBRARY_SETUP(utilada_base)
AM_GNAT_LIBRARY_SETUP(utilada_sys)

os_base='unix'
os_version='none'
AC_MSG_CHECKING([operating system])
case "${target_os}" in
     linux|linux-*|solaris*|sysv-*)
        os_version='linux'
        ;;

     netbsdelf*|dragonfly*)
        os_version='netbsd'
        ;;

     openbsd*|freebsd*)
        # Let OpenBSD people cry 
        os_version='freebsd'
        ;;

     macos*|darwin*)
        os_version='macos'
        ;;

     mingw32*|cygwin*|mingw64*|msys)
        os_version='win'
        os_base='windows'
        ;;

     mingw32*|cygwin*|mingw64*|msys)
        os_version='win'
        os_base='windows'
        ;;

     *)
        # Be authoritative
        os_version='linux'
        ;;
esac
AC_MSG_RESULT($os_version)

AC_MSG_CHECKING([hardware platform])
case "${target_cpu}" in
     x86_64)
        os_version="${os_version}64"
        HARDWARE_PLATFORM=${target_cpu}
        ;;

     i386|i486|i586|i686)
        os_version="${os_version}32"
        HARDWARE_PLATFORM='x86'
        ;;

     armv7*)
        os_version="${os_version}32"
        HARDWARE_PLATFORM=${target_cpu}
        ;;

     armv8*)
        os_version="${os_version}64"
        HARDWARE_PLATFORM=${target_cpu}
        ;;

     *)
        HARDWARE_PLATFORM=${target_cpu}
        ;;
esac
AC_MSG_RESULT($HARDWARE_PLATFORM)
AC_SUBST(HARDWARE_PLATFORM)

# Check for gcc intrinsics
AM_HAS_INTRINSIC_SYNC_COUNTERS(src_asm='intrinsic',src_asm='')
AC_MSG_CHECKING([specific processor support])
if test T$src_asm = T; then
   case "${target}" in
   ## Intel 386 machines where we don't care about the manufacturer
     i[[34567]]86-*-* | x86_* | x86-*)
       src_asm='x86'
       ;;

     *)
       src_asm='none'
       ;;

   esac
fi
AC_MSG_RESULT(using $src_asm)
UTIL_ASM_TYPE="$src_asm"
AC_SUBST(UTIL_ASM_TYPE)

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

fi
UTIL_OS_VERSION=$os_version
AC_SUBST(UTIL_OS_VERSION)

AM_GNAT_FIND_XML_ADA([
    AM_GNAT_LIBRARY_SETUP(utilada_xml)
], [
])

AC_MSG_CHECKING([Ada LZMA Library])
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
AC_MSG_RESULT(${UTIL_WITH_LZMA})

##########################################
# Ada LZMA library
##########################################
if test T${UTIL_WITH_LZMA} = Tyes; then
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
AM_GNAT_CHECK_AWS(
  [
    UTIL_HAVE_AWS=no
    WITH_SERVER="";
    WITH_UTIL_AWS="";
  ], [
    UTIL_HAVE_AWS=yes
    WITH_UTIL_AWS="with \"utilada_aws\";";
    WITH_SERVER=$ac_cv_gnat_project_with_aws
    AM_GNAT_LIBRARY_SETUP(utilada_aws)
  ])
AM_GNAT_AWS_VERSION

AC_MSG_CHECKING([using AWS http client])
UTIL_AWS_VERSION=1
case $AWS_VERSION in
  *2017*|*2018*|*2019*|*202*|20.0)
     UTIL_AWS_VERSION=2
     ;;

  3.3.2)
     UTIL_AWS_VERSION=2
     ;;

  *)
     UTIL_AWS_VERSION=1
     ;;
esac
AC_MSG_RESULT(${UTIL_AWS_VERSION})
AC_SUBST(UTIL_AWS_VERSION)

##########################################
# Check for Util.Serialize.Mappers.Vector_Mappers support
##########################################

ac_enable_vector_mappers=yes
AC_MSG_CHECKING([for gcc bug compiling Util.Serialize.Mappers.Vector_Mappers (Bug 53737)])
version=`$GNATMAKE --version | grep GNATMAKE | sed -e 's,GNATMAKE ,,'`
case "$version" in
   4.7.0*|4.7.1*|4.7.2*|4.7.3*)
      AC_MSG_RESULT(yes)
      AC_MSG_WARN(Your compiler is known to fail when compiling a generic package.)
      AC_MSG_WARN(See Bug 53737: Assert_Failure on a generic package with a package as parameter)
      AC_MSG_WARN(The Util.Serialize.Mappers.Vector_Mappers and Util.Http.Rest packages are excluded.)
      ac_enable_vector_mappers=no
      ;;

   *)
      AC_MSG_RESULT(no)
      ac_enable_vector_mappers=yes
      ;;
esac

AC_MSG_CHECKING([whether the Util.Serialize.Mappers.Vector_Mappers package must be compiled])
AC_ARG_ENABLE(vector-mappers,
[  --enable-vector-mappers Enable the Util.Serialize.Mappers.Vector_Mappers package (enabled)],
[case "${enableval}" in
  no|none)  ac_enable_vector_mappers=no ;;
  *)        ac_enable_vector_mappers=yes ;;
esac])dnl

AC_MSG_RESULT(${ac_enable_vector_mappers})
UTIL_HAVE_VECTOR_MAPPERS=$ac_enable_vector_mappers

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

AC_SUBST(UTIL_HAVE_VECTOR_MAPPERS)
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

AC_OUTPUT(
Makefile.conf
utilada.gpr
utilada_http.gpr
utilada_unit.gpr
utilada_xml.gpr
utilada_lzma.gpr
utilada_tests.gpr
utilada_conf.gpr
)
