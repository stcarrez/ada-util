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
[![Download](https://img.shields.io/badge/download-2.5.0-brightgreen.svg)](http://download.vacs.fr/ada-util/ada-util-2.5.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
[![GitLab](https://img.shields.io/badge/repo-GitLab-6C488A.svg)](https://gitlab.com/stcarrez/ada-util)
[![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-util/2.5.0.svg)](Commits)


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

## Version 2.5.0  - Aug 2022
  - New examples to illustrate the IO stream composition
  - New examples for JSON parser and Util.Beans.Objects
  - Add support to set environment variables when launching a process (without changing the current process environment!)
  - Add support to indent XML output streams
  - New package Util.Files.Rolling to provide a rolling file manager
  - New package Util.Beans.Objects.Iterators to easily iterate over objects
  - Add a new log appender to support rolling log files with size and time based policies
  - New operation Util.Files.Delete_Tree to delete a directory tree and work arround
    for GNAT bug gcc/63222 and gcc/56055
  - New operation Util.Files.Realpath to find the canonicalized absolute path of a file
  - New package Util.Blobs to hold binary content with reference counting
  - New package Util.Http.Headers to provide some HTTP helper operations
  - Add support for Blob in bean objects
  - Fix compilation on NetBSD 9.2
  - Fix compilation with AWS >= 22.0

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

| Package              | Example                                                                      |
|----------------------|------------------------------------------------------------------------------|
| `Util.Dates.Formats` | [date.adb](https://github.com/stcarrez/ada-util/tree/master/samples/date.adb)|
| `Util.Beans.Objects` | [objcalc.adb](https://github.com/stcarrez/ada-util/tree/master/samples/objcalc.adb), [jsonobj.adb](https://github.com/stcarrez/ada-util/tree/master/samples/jsonobj.adb), [jsonread.adb](https://github.com/stcarrez/ada-util/tree/master/samples/jsonread.adb)|
| `Util.Beans.Objects.Maps` | [jsonobj.adb](https://github.com/stcarrez/ada-util/tree/master/samples/jsonobj.adb), [genentities.adb](https://github.com/stcarrez/ada-util/tree/master/samples/genentities.adb)|
| `Util.Beans.Objects.Vectors` | [datasets.adb](https://github.com/stcarrez/ada-util/tree/master/samples/datasets.adb), [jsonobj.adb](https://github.com/stcarrez/ada-util/tree/master/samples/jsonobj.adb)|
| `Util.Beans.Objects.Datasets` | [datasets.adb](https://github.com/stcarrez/ada-util/tree/master/samples/datasets.adb)|
| `Util.Beans.Objects.Iterators` | [jsonread.adb](https://github.com/stcarrez/ada-util/tree/master/samples/jsonread.adb)|
| `Util.Encoders` | [encodes.adb](https://github.com/stcarrez/ada-util/tree/master/samples/encodes.adb) |
| `Util.Encoders.AES` | [decrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/decrypt.adb), [encrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/encrypt.adb) |
| `Util.Encoders.SHA256` | [sha256.adb](https://github.com/stcarrez/ada-util/tree/master/samples/sha256.adb) |
| `Util.Files` | [realpath.adb](https://github.com/stcarrez/ada-util/tree/master/samples/realpath.adb), [gperfhash.adb](https://github.com/stcarrez/ada-util/tree/master/samples/gperfhash.adb) |
| `Util.Files.Rolling` | [rolling_file.adb](https://github.com/stcarrez/ada-util/tree/master/samples/rolling_file.adb) |
| `Util.Http.Clients` | [wget.adb](https://github.com/stcarrez/ada-util/tree/master/samples/wget.adb), [facebook.adb](https://github.com/stcarrez/ada-util/tree/master/samples/facebook.adb) |
| `Util.Log.Loggers` | [log.adb](https://github.com/stcarrez/ada-util/tree/master/samples/log.adb), [multipro_refs.adb](https://github.com/stcarrez/ada-util/tree/master/samples/multipro_refs.adb), [date.adb](https://github.com/stcarrez/ada-util/tree/master/samples/date.adb), [xmlrd.adb](https://github.com/stcarrez/ada-util/tree/master/samples/xmlrd.adb), [gperfhash.adb](https://github.com/stcarrez/ada-util/tree/master/samples/gperfhash.adb), [multipro.adb](https://github.com/stcarrez/ada-util/tree/master/samples/multipro.adb), [mapping.adb](https://github.com/stcarrez/ada-util/tree/master/samples/mapping.adb), [csv_city.adb](https://github.com/stcarrez/ada-util/tree/master/samples/csv_city.adb), [bundles.adb](https://github.com/stcarrez/ada-util/tree/master/samples/bundles.adb) |
| `Util.Measures` | [measures.adb](https://github.com/stcarrez/ada-util/tree/master/samples/measures.adb) |
| `Util.Processes` | [launch.adb](https://github.com/stcarrez/ada-util/tree/master/samples/launch.adb), [env.adb](https://github.com/stcarrez/ada-util/tree/master/samples/env.adb), [popen.adb](https://github.com/stcarrez/ada-util/tree/master/samples/popen.adb) |
| `Util.Properties` | [properties.adb](https://github.com/stcarrez/ada-util/tree/master/samples/properties.adb), [proplist.adb](https://github.com/stcarrez/ada-util/tree/master/samples/proplist.adb), [bundles.adb](https://github.com/stcarrez/ada-util/tree/master/samples/bundles.adb) |
| `Util.Refs` | [multipro_refs.adb](https://github.com/stcarrez/ada-util/tree/master/samples/multipro_refs.adb) |
| `Util.Streams.AES` | [decrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/decrypt.adb), [encrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/encrypt.adb) |
| `Util.Streams.Base64` | [lzma_encrypt_b64.adb](https://github.com/stcarrez/ada-util/tree/master/samples/lzma_encrypt_b64.adb), [lzma_decrypt_b64.adb](https://github.com/stcarrez/ada-util/tree/master/samples/lzma_decrypt_b64.adb) |
| `Util.Streams.Files` | [copy.adb](https://github.com/stcarrez/ada-util/tree/master/samples/copy.adb), [sha256.adb](https://github.com/stcarrez/ada-util/tree/master/samples/sha256.adb), [compress.adb](https://github.com/stcarrez/ada-util/tree/master/samples/compress.adb), [decompress.adb](https://github.com/stcarrez/ada-util/tree/master/samples/decompress.adb), [decrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/decrypt.adb), [encrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/encrypt.adb), [lzma_encrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/lzma_encrypt.adb), [lzma_decrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/lzma_decrypt.adb) |
| `Util.Streams.Pipes` | [launch.adb](https://github.com/stcarrez/ada-util/tree/master/samples/launch.adb), [popen.adb](https://github.com/stcarrez/ada-util/tree/master/samples/popen.adb) |
| `Util.Serialize.IO.CSV` | [csv_city.adb](https://github.com/stcarrez/ada-util/tree/master/samples/csv_city.adb) |
| `Util.Serialize.IO.JSON` | [serialize.adb](https://github.com/stcarrez/ada-util/tree/master/samples/serialize.adb), [json.adb](https://github.com/stcarrez/ada-util/tree/master/samples/json.adb), [jsonobj.adb](https://github.com/stcarrez/ada-util/tree/master/samples/jsonobj.adb), [jsonread.adb](https://github.com/stcarrez/ada-util/tree/master/samples/jsonread.adb), [genentities.adb](https://github.com/stcarrez/ada-util/tree/master/samples/genentities.adb) |
| `Util.Serialize.IO.XML` | [serialize_xml.adb](https://github.com/stcarrez/ada-util/tree/master/samples/serialize_xml.adb), [xmi.adb](https://github.com/stcarrez/ada-util/tree/master/samples/xmi.adb), [xmlrd.adb](https://github.com/stcarrez/ada-util/tree/master/samples/xmlrd.adb) |
| `Util.Serialize.Mappers` | [csv_reader.adb](https://github.com/stcarrez/ada-util/tree/master/samples/csv_reader.adb), [csv_city.adb](https://github.com/stcarrez/ada-util/tree/master/samples/csv_city.adb), [xmi.adb](https://github.com/stcarrez/ada-util/tree/master/samples/xmi.adb), [xmlrd.adb](https://github.com/stcarrez/ada-util/tree/master/samples/xmlrd.adb) |
| `Util.Serialize.Mappers.Record_Mapper` | [xmi.adb](https://github.com/stcarrez/ada-util/tree/master/samples/xmi.adb), [xmlrd.adb](https://github.com/stcarrez/ada-util/tree/master/samples/xmlrd.adb) |
| `Util.Serialize.Mappers.Vector_Mapper` | [json.adb](https://github.com/stcarrez/ada-util/tree/master/samples/json.adb), [xmlrd.adb](https://github.com/stcarrez/ada-util/tree/master/samples/xmlrd.adb) |
| `Util.Strings` | [cut.adb](https://github.com/stcarrez/ada-util/tree/master/samples/cut.adb), [escape.adb](https://github.com/stcarrez/ada-util/tree/master/samples/escape.adb) |
| `Util.Strings.Tokenizers` | [cut.adb](https://github.com/stcarrez/ada-util/tree/master/samples/cut.adb), [escape.adb](https://github.com/stcarrez/ada-util/tree/master/samples/escape.adb) |
| `Util.Strings.Transforms` | [escape.adb](https://github.com/stcarrez/ada-util/tree/master/samples/escape.adb), [gperfhash.adb](https://github.com/stcarrez/ada-util/tree/master/samples/gperfhash.adb) |
| `Util.Strings.Vectors` | [gperfhash.adb](https://github.com/stcarrez/ada-util/tree/master/samples/gperfhash.adb) |

# Documentation

* [Ada Utility Library Programmer's Guide](https://ada-util.readthedocs.io/en/latest/) [PDF](https://github.com/stcarrez/ada-util/blob/master/docs/utilada-book.pdf)
* [Wiki Documentation](https://github.com/stcarrez/ada-util/wiki)

# Articles

* [IO stream composition and serialization with Ada Utility Library](https://blog.vacs.fr/vacs/blogs/post.html?post=2022/03/05/IO-stream-composition-and-serialization-with-Ada-Utility-Library)
* [Easy reading and writing files with Ada Utility Library](https://blog.vacs.fr/vacs/blogs/post.html?post=2020/08/09/Easy-reading-and-writing-files-with-Ada-Utility-Library)
* [Process creation in Java and Ada](https://blog.vacs.fr/vacs/blogs/post.html?post=2012/03/16/Process-creation-in-Java-and-Ada)
* [Ada perfect hash generation with gperfhash](https://blog.vacs.fr/vacs/blogs/post.html?post=2012/01/16/Ada-perfect-hash-generation)
* [Aunit vs Ahven](https://blog.vacs.fr/vacs/blogs/post.html?post=2011/11/27/Aunit-vs-Ahven)
