# Ada Utility Library

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada.json)](https://alire.ada.dev/crates/utilada)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_lzma.json)](https://alire.ada.dev/crates/utilada_lzma)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_curl.json)](https://alire.ada.dev/crates/utilada_curl)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_unit.json)](https://alire.ada.dev/crates/utilada_unit)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_xml.json)](https://alire.ada.dev/crates/utilada_xml)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_aws.json)](https://alire.ada.dev/crates/utilada_aws)
[![Build Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-util/badges/build.json)](https://porion.vacs.fr/porion/projects/view/ada-util/summary)
[![Test Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-util/badges/tests.json)](https://porion.vacs.fr/porion/projects/view/ada-util/xunits)
[![Coverage](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-util/badges/coverage.json)](https://porion.vacs.fr/porion/projects/view/ada-util/summary)
[![Documentation Status](https://readthedocs.org/projects/ada-util/badge/?version=latest)](https://ada-util.readthedocs.io/en/latest/?badge=latest)
[![Download](https://img.shields.io/badge/download-2.6.0-brightgreen.svg)](http://download.vacs.fr/ada-util/ada-util-2.6.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
[![GitLab](https://img.shields.io/badge/repo-GitLab-6C488A.svg)](https://gitlab.com/stcarrez/ada-util)
[![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-util/2.6.0.svg)](Commits)


This Ada library contains various utility packages for building
Ada applications.  This includes:

* A logging framework close to Java log4j framework,
* Support for INI and property files,
* A serialization/deserialization framework for XML, JSON, CSV, Forms
* Ada beans framework,
* Encoding/decoding framework (Base16, Base32, Base64, SHA, HMAC-SHA, AES-256),
* A composing stream framework (raw, files, buffers, pipes, sockets, encryption, decryption, LZMA compression, LZMA decompression),
* Several concurrency tools (reference counters, counters, pools, fifos, arrays),
* Process creation and pipes,
* Support for loading shared libraries (on Windows or Unix),
* HTTP client library on top of CURL or AWS.

Ada Util also provides a small test utility library on top of
Ahven or AUnit to help in writing unit tests.  Ahven is the default testing
framework as it provides better reports.

## Version 2.7.0  - Under development
  - New package Util.Files.Walk to iterate over directory trees and honor .gitignore
  - Feature #48: Change the log time from UTC to Local Time (configurable)
  - Fix #49: Perf report generates incorrect XML title attributes
  - Fix #50: 128Bit AES-CTR Encoding doesn't work (thanks Adam Jasinski)

## Version 2.6.0  - Jul 2023
  - New encoder/decoder for Base32
  - Feature #32: Custom log appender
  - Feature #36: Add HOTP algorithm
  - Feature #38: Allow to read stdout and stderr as separate streams when spawning a process
  - Feature #40: Input stream reader to read parts from another stream
  - Feature #41: Support to ignore line breaks when decoding Base64 streams
  - Feature #42: Improvement of test framework to launch and verify external program execution and output
  - Fix #37: Support to build with -gnatW8
  - Fix #35: utilada_curl crate uses wrong curl dependency

[List all versions](https://github.com/stcarrez/ada-util/blob/master/NEWS.md)

## Build with Alire

```
alr with utilada
alr with utilada_aws
alr with utilada_curl
alr with utilada_lzma
alr with utilada_unit
alr with utilada_xml
```

## Build with configure

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
   gprbuild -Psamples
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
| `Util.Files.Walk` | [tree.adb](https://github.com/stcarrez/ada-util/tree/master/samples/tree.adb) |
| `Util.Http.Clients` | [wget.adb](https://github.com/stcarrez/ada-util/tree/master/samples/wget.adb), [facebook.adb](https://github.com/stcarrez/ada-util/tree/master/samples/facebook.adb) |
| `Util.Log.Loggers` | [log.adb](https://github.com/stcarrez/ada-util/tree/master/samples/log.adb), [syslog_appenders.adb](https://github.com/stcarrez/ada-util/tree/master/samples/syslog_appenders.adb), [multipro_refs.adb](https://github.com/stcarrez/ada-util/tree/master/samples/multipro_refs.adb), [date.adb](https://github.com/stcarrez/ada-util/tree/master/samples/date.adb), [xmlrd.adb](https://github.com/stcarrez/ada-util/tree/master/samples/xmlrd.adb), [gperfhash.adb](https://github.com/stcarrez/ada-util/tree/master/samples/gperfhash.adb), [multipro.adb](https://github.com/stcarrez/ada-util/tree/master/samples/multipro.adb), [mapping.adb](https://github.com/stcarrez/ada-util/tree/master/samples/mapping.adb), [csv_city.adb](https://github.com/stcarrez/ada-util/tree/master/samples/csv_city.adb), [bundles.adb](https://github.com/stcarrez/ada-util/tree/master/samples/bundles.adb) |
| `Util.Measures` | [measures.adb](https://github.com/stcarrez/ada-util/tree/master/samples/measures.adb) |
| `Util.Processes` | [launch.adb](https://github.com/stcarrez/ada-util/tree/master/samples/launch.adb), [env.adb](https://github.com/stcarrez/ada-util/tree/master/samples/env.adb), [popen.adb](https://github.com/stcarrez/ada-util/tree/master/samples/popen.adb) |
| `Util.Properties` | [properties.adb](https://github.com/stcarrez/ada-util/tree/master/samples/properties.adb), [proplist.adb](https://github.com/stcarrez/ada-util/tree/master/samples/proplist.adb), [bundles.adb](https://github.com/stcarrez/ada-util/tree/master/samples/bundles.adb) |
| `Util.Refs` | [multipro_refs.adb](https://github.com/stcarrez/ada-util/tree/master/samples/multipro_refs.adb) |
| `Util.Streams.AES` | [decrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/decrypt.adb), [encrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/encrypt.adb) |
| `Util.Streams.Base64` | [lzma_encrypt_b64.adb](https://github.com/stcarrez/ada-util/tree/master/samples/lzma_encrypt_b64.adb), [lzma_decrypt_b64.adb](https://github.com/stcarrez/ada-util/tree/master/samples/lzma_decrypt_b64.adb), [dumpcert.adb](https://github.com/stcarrez/ada-util/tree/master/samples/dumpcert.adb) |
| `Util.Streams.Buffered.Parts` | [multipart.adb](https://github.com/stcarrez/ada-util/tree/master/samples/multipart.adb), [dumpcert.adb](https://github.com/stcarrez/ada-util/tree/master/samples/dumpcert.adb) |
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
