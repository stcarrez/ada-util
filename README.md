# Ada Utility Library

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada.json)](https://alire.ada.dev/crates/utilada)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_lzma.json)](https://alire.ada.dev/crates/utilada_lzma)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_curl.json)](https://alire.ada.dev/crates/utilada_curl)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_unit.json)](https://alire.ada.dev/crates/utilada_unit)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_xml.json)](https://alire.ada.dev/crates/utilada_xml)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_aws.json)](https://alire.ada.dev/crates/utilada_aws)
[![Build Status](https://img.shields.io/jenkins/s/https/jenkins.vacs.fr/Bionic-Ada-Util.svg)](https://jenkins.vacs.fr/job/Bionic-Ada-Util/)
[![Test Status](https://img.shields.io/jenkins/t/https/jenkins.vacs.fr/Bionic-Ada-Util.svg)](https://jenkins.vacs.fr/job/Bionic-Ada-Util/)
[![codecov](https://codecov.io/gh/stcarrez/ada-util/branch/master/graph/badge.svg)](https://codecov.io/gh/stcarrez/ada-util)
[![Documentation Status](https://readthedocs.org/projects/ada-util/badge/?version=latest)](https://ada-util.readthedocs.io/en/latest/?badge=latest)
[![Download](https://img.shields.io/badge/download-2.3.0-brightgreen.svg)](http://download.vacs.fr/ada-util/ada-util-2.3.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
[![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-util/2.3.0.svg)](Commits)


This Ada05 library contains various utility packages for building
Ada05 applications.  This includes:

* A logging framework close to Java log4j framework,
* Support for properties
* A serialization/deserialization framework for XML, JSON, CSV
* Ada beans framework
* Encoding/decoding framework (Base16, Base64, SHA, HMAC-SHA, AES-256)
* A composing stream framework (raw, files, buffers, pipes, sockets)
* Several concurrency tools (reference counters, counters, pools, fifos, arrays)
* Process creation and pipes
* Support for loading shared libraries (on Windows or Unix)
* HTTP client library on top of CURL or AWS

Ada Util also provides a small test utility library on top of
Ahven or AUnit to help in writing unit tests.  Ahven is the default testing
framework as it provides better reports.

## Version 2.4.0  - 
  - Add support to customize and provide application specific log appenders
  - Fix compilation with GNAT 10
  - New package Util.Properties.Form to help in parsing application/x-www-form-urlencoded

[List all versions](https://github.com/stcarrez/ada-util/blob/master/NEWS.md)

## Build

For a detailed description on how you can configure, build and install the library
refer to the [Installation](https://ada-util.readthedocs.io/en/latest/Installation/) guide.
Otherwise, you can easily configure and build the library with the steps described below.

To use Ahven testing framework, configure and build as follows:

```
./configure
make
```

To use AUnit, build with the following commands:
```
./configure --enable-aunit
make
```
   
The unit tests are built and executed with:
```
   make test
```

And unit tests are executed with (256 success, 0 failed):
```
   bin/util_harness
```
For the installation, use the following command:
```
   make install
```

## Build without configure

Since the integration with Alire, you can build without running configure.
However, there are a number of checks and dependencies which are not verified
and you must run several commands manually, choosing the correct values for
`UTIL_ASM_TYPE` and `UTIL_OS` build variables.  The trick is to give `gprbuild`
the Alire configuration project in `.alire/utilada_conf.gpr`.

For a simple Linux 64-bit build, use:

```
gprbuild -aP.alire -Putilada_core -p
gprbuild -aP.alire -Putilada_base -p
gprbuild -aP.alire -Putilada_sys -p
gprbuild -aP.alire -Putilada_aws -p
gprbuild -aP.alire -Putilada_curl -p
gprbuild -aP.alire -Putilada_xml -p
```

For a Linux 32-bit build, use
```
gprbuild -aP.alire -Putilada_sys -p -XUTIL_OS=linux32
```

For Windows-64 try:
```
gprbuild -aP.alire -Putilada_sys -p -XUTIL_OS=win64
```

Checkout the file `.alire/utilada_conf.gpr` for other alternatives.

You can then use `gprinstall` for the installation:

```
gprinstall -aP.alire -Putilada_core -p
gprinstall -aP.alire -Putilada_base -p
gprinstall -aP.alire -Putilada_sys -p
gprinstall -aP.alire -Putilada_aws -p
gprinstall -aP.alire -Putilada_curl -p
gprinstall -aP.alire -Putilada_xml -p
```

I've never tried to build on Windows without msys2.
I don't know if it works. I suspect there will be problems.

# Samples

The samples can be built using:
```
   gnatmake -Psamples
```   

# Documentation

* [Ada Utility Library Programmer's Guide](https://ada-util.readthedocs.io/en/latest/intro/) [PDF](https://github.com/stcarrez/ada-util/blob/master/docs/utilada-book.pdf)
* [Wiki Documentation](https://github.com/stcarrez/ada-util/wiki)

# Articles

* [Easy reading and writing files with Ada Utility Library](https://blog.vacs.fr/vacs/blogs/post.html?post=2020/08/09/Easy-reading-and-writing-files-with-Ada-Utility-Library)
* [Process creation in Java and Ada](https://blog.vacs.fr/vacs/blogs/post.html?post=2012/03/16/Process-creation-in-Java-and-Ada)
* [Ada perfect hash generation with gperfhash](https://blog.vacs.fr/vacs/blogs/post.html?post=2012/01/16/Ada-perfect-hash-generation)
* [Aunit vs Ahven](https://blog.vacs.fr/vacs/blogs/post.html?post=2011/11/27/Aunit-vs-Ahven)
