# Ada Utility Library

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada.json)](https://alire.ada.dev/crates/utilada)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_lzma.json)](https://alire.ada.dev/crates/utilada_lzma)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_curl.json)](https://alire.ada.dev/crates/utilada_curl)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_unit.json)](https://alire.ada.dev/crates/utilada_unit)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_xml.json)](https://alire.ada.dev/crates/utilada_xml)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_aws.json)](https://alire.ada.dev/crates/utilada_aws)
[![Build Status](https://img.shields.io/jenkins/s/https/jenkins.vacs.fr/Ada-Util.svg)](https://jenkins.vacs.fr/job/Ada-Util/)
[![Test Status](https://img.shields.io/jenkins/t/https/jenkins.vacs.fr/Ada-Util.svg)](https://jenkins.vacs.fr/job/Ada-Util/)
[![codecov](https://codecov.io/gh/stcarrez/ada-util/branch/master/graph/badge.svg)](https://codecov.io/gh/stcarrez/ada-util)
[![Documentation Status](https://readthedocs.org/projects/ada-util/badge/?version=latest)](https://ada-util.readthedocs.io/en/latest/?badge=latest)
[![Download](https://img.shields.io/badge/download-2.0.0-brightgreen.svg)](http://download.vacs.fr/ada-util/ada-util-2.0.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
[![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-util/2.0.0.svg)](Commits)


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

# Build

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

# Samples

The samples can be built using:
```
   gnatmake -Psamples
```   

# Documentation

* [Ada Utility Library Programmer's Guide](https://ada-util.readthedocs.io/en/latest/intro/)
* [Wiki Documentation](https://github.com/stcarrez/ada-util/wiki)
