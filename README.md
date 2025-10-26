# Ada Utility Library

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada.json)](https://alire.ada.dev/crates/utilada)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_lzma.json)](https://alire.ada.dev/crates/utilada_lzma)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_curl.json)](https://alire.ada.dev/crates/utilada_curl)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_unit.json)](https://alire.ada.dev/crates/utilada_unit)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_xml.json)](https://alire.ada.dev/crates/utilada_xml)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/utilada_aws.json)](https://alire.ada.dev/crates/utilada_aws)
[![Ada 2012](https://img.shields.io/badge/2012-inside-green?logo=ada&logoColor=white&logoSize=auto)](https://adaic.org/ada-resources/standards/ada12)
[![Build Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-util/badges/build.json)](https://porion.vacs.fr/porion/projects/view/ada-util/summary)
[![Test Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-util/badges/tests.json)](https://porion.vacs.fr/porion/projects/view/ada-util/xunits)
[![Coverage](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/ada-util/badges/coverage.json)](https://porion.vacs.fr/porion/projects/view/ada-util/summary)
[![Documentation Status](https://readthedocs.org/projects/ada-util/badge/?version=latest)](https://ada-util.readthedocs.io/en/latest/?badge=latest)
[![Download](https://img.shields.io/badge/download-2.8.0-brightgreen.svg)](http://download.vacs.fr/ada-util/ada-util-2.8.0.tar.gz)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
[![GitLab](https://img.shields.io/badge/repo-GitLab-6C488A.svg)](https://gitlab.com/stcarrez/ada-util)
[![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-util/2.8.0.svg)](Commits)

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

## Version 2.8.2  - Oct 2025
  - Fix #61: MacOS build is broken

## Version 2.8.1  - Jul 2025
  - Fix #59: Encryption of non-aligned array raises constraint error

[List all versions](https://github.com/stcarrez/ada-util/blob/master/NEWS.md)

## Using with Alire

If you are using [Alire](https://alire.ada.dev/) in your project, run the following command
within your [Alire](https://alire.ada.dev/) project to use the library:

```
alr with utilada
```

Depending on your project, you may need one or some of the following other components:

```
alr with utilada_aws
alr with utilada_curl
alr with utilada_lzma
alr with utilada_unit
alr with utilada_xml
```

## Using without Alire

If you don't have [Alire](https://alire.ada.dev/) or want to build and install the library
on a specific place, run a `setup` command to configure the build as well as installation
directory.
For a detailed description on how you can configure, build and install the library
refer to the [Installation](https://ada-util.readthedocs.io/en/latest/Installation/) guide.
Otherwise, you can easily configure and build the library with the steps described below.

The support for AWS, Curl, LZMA and XML/Ada are enabled only when a `HAVE_XXX=yes` configuration
variable has defined.  Run the setup command that records in the `Makefile.conf` the configuration
you want to build.

The `HAVE_ALIRE` configuration allows you to build with [Alire](https://alire.ada.dev/) or not.

The `UTIL_OS` configuration is mandatory for the setup to indicate the build host configuration.
It must one of the allowed values defined in `utilada_conf.gpr` GNAT project in the
`Os_Version_Type` declaration:

```
   type Os_Version_Type is ("none", "unix", "windows",
                            "linux32", "linux64", "win32", "win64", "macos64",
                            "netbsd32", "netbsd64", "freebsd32", "freebsd64",
                            "openbsd32", "openbsd64");
```

The example below enables the XML/Ada and AWS components but disables
the Curl and LZMA support and disables the use of [Alire](https://alire.ada.dev/) to build
the library.

```
make setup BUILD=debug PREFIX=/build/install \
  UTIL_OS=linux64 \
  HAVE_XML_ADA=yes HAVE_AWS=yes \
  HAVE_CURL=no HAVE_LZMA=no HAVE_ALIRE=no
```

Then build, run the unit tests and install by using:

```
make
make test
make install
```

To use the installed libraries, make sure your `ADA_PROJECT_PATH` contains the directory
where you installed the libraries (configured by the `PREFIX=<path>` option in the setup phase).
The installed GNAT projects are the same as those used when using [Alire](https://alire.ada.dev/).

# Samples

The samples are built by using:
```
cd samples
alr build
```   

or by running:

```
make samples
```


| Package              | Example                                                                      |
|----------------------|------------------------------------------------------------------------------|
| `Util.Dates.Formats` | [date.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/date.adb)|
| `Util.Beans.Objects` | [objcalc.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/objcalc.adb), [jsonobj.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/jsonobj.adb), [jsonread.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/jsonread.adb)|
| `Util.Beans.Objects.Maps` | [jsonobj.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/jsonobj.adb), [genentities.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/genentities.adb)|
| `Util.Beans.Objects.Vectors` | [datasets.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/datasets.adb), [jsonobj.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/jsonobj.adb)|
| `Util.Beans.Objects.Datasets` | [datasets.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/datasets.adb)|
| `Util.Beans.Objects.Iterators` | [jsonread.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/jsonread.adb)|
| `Util.Encoders` | [encodes.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/encodes.adb) |
| `Util.Encoders.AES` | [decrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/decrypt.adb), [encrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/encrypt.adb), [decrypt_array.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/decrypt_array.adb), [encrypt_array.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/encrypt_array.adb) |
| `Util.Encoders.SHA256` | [sha256.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/sha256.adb) |
| `Util.Files` | [realpath.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/realpath.adb), [gperfhash.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/gperfhash.adb) |
| `Util.Files.Rolling` | [rolling_file.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/rolling_file.adb) |
| `Util.Files.Walk` | [tree.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/tree.adb) |
| `Util.Http.Clients` | [wget.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/wget.adb) |
| `Util.Log.Loggers` | [log.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/log.adb), [syslog_appenders.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/syslog_appenders.adb), [multipro_refs.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/multipro_refs.adb), [date.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/date.adb), [xmlrd.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/xmlrd.adb), [gperfhash.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/gperfhash.adb), [multipro.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/multipro.adb), [mapping.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/mapping.adb), [csv_city.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/csv_city.adb), [bundles.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/bundles.adb) |
| `Util.Measures` | [measures.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/measures.adb) |
| `Util.Processes` | [launch.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/launch.adb), [env.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/env.adb), [popen.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/popen.adb) |
| `Util.Properties` | [properties.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/properties.adb), [proplist.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/proplist.adb), [bundles.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/bundles.adb) |
| `Util.Refs` | [multipro_refs.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/multipro_refs.adb) |
| `Util.Streams.AES` | [decrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/decrypt.adb), [encrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/encrypt.adb) |
| `Util.Streams.Base64` | [lzma_encrypt_b64.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/lzma_encrypt_b64.adb), [lzma_decrypt_b64.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/lzma_decrypt_b64.adb), [dumpcert.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/dumpcert.adb) |
| `Util.Streams.Buffered.Parts` | [multipart.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/multipart.adb), [dumpcert.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/dumpcert.adb) |
| `Util.Streams.Files` | [copy.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/copy.adb), [sha256.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/sha256.adb), [compress.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/compress.adb), [decompress.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/decompress.adb), [decrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/decrypt.adb), [encrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/encrypt.adb), [lzma_encrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/lzma_encrypt.adb), [lzma_decrypt.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/lzma_decrypt.adb) |
| `Util.Streams.Pipes` | [launch.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/launch.adb), [popen.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/popen.adb) |
| `Util.Serialize.IO.CSV` | [csv_city.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/csv_city.adb) |
| `Util.Serialize.IO.JSON` | [serialize.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/serialize.adb), [json.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/json.adb), [jsonobj.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/jsonobj.adb), [jsonread.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/jsonread.adb), [genentities.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/genentities.adb) |
| `Util.Serialize.IO.XML` | [serialize_xml.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/serialize_xml.adb), [xmi.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/xmi.adb), [xmlrd.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/xmlrd.adb) |
| `Util.Serialize.Mappers` | [csv_reader.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/csv_reader.adb), [csv_city.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/csv_city.adb), [xmi.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/xmi.adb), [xmlrd.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/xmlrd.adb) |
| `Util.Serialize.Mappers.Record_Mapper` | [xmi.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/xmi.adb), [xmlrd.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/xmlrd.adb) |
| `Util.Serialize.Mappers.Vector_Mapper` | [json.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/json.adb), [xmlrd.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/xmlrd.adb) |
| `Util.Strings` | [cut.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/cut.adb), [escape.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/escape.adb) |
| `Util.Strings.Tokenizers` | [cut.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/cut.adb), [escape.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/escape.adb) |
| `Util.Strings.Transforms` | [escape.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/escape.adb), [gperfhash.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/gperfhash.adb) |
| `Util.Strings.Vectors` | [gperfhash.adb](https://github.com/stcarrez/ada-util/tree/master/samples/src/gperfhash.adb) |

# Documentation

* [Ada Utility Library Programmer's Guide](https://ada-util.readthedocs.io/en/latest/) [PDF](https://github.com/stcarrez/ada-util/blob/master/docs/utilada-book.pdf)

# Articles

* [IO stream composition and serialization with Ada Utility Library](https://blog.vacs.fr/vacs/blogs/post.html?post=2022/03/05/IO-stream-composition-and-serialization-with-Ada-Utility-Library)
* [Easy reading and writing files with Ada Utility Library](https://blog.vacs.fr/vacs/blogs/post.html?post=2020/08/09/Easy-reading-and-writing-files-with-Ada-Utility-Library)
* [Process creation in Java and Ada](https://blog.vacs.fr/vacs/blogs/post.html?post=2012/03/16/Process-creation-in-Java-and-Ada)
* [Ada perfect hash generation with gperfhash](https://blog.vacs.fr/vacs/blogs/post.html?post=2012/01/16/Ada-perfect-hash-generation)
* [Aunit vs Ahven](https://blog.vacs.fr/vacs/blogs/post.html?post=2011/11/27/Aunit-vs-Ahven)
