# Ada Utility Library

[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Ada-Util.svg)](http://jenkins.vacs.fr/job/Ada-Util/)
[![Test Status](https://img.shields.io/jenkins/t/http/jenkins.vacs.fr/Ada-Util.svg)](http://jenkins.vacs.fr/job/Ada-Util/)
[![Download](https://img.shields.io/badge/download-1.8.0-brightgreen.svg)](http://download.vacs.fr/ada-util/ada-util-1.8.0.tar.gz)
[![License](http://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/ada-util/ada-util-1.8.0.svg)


This Ada05 library contains various utility packages for building
Ada05 applications.  This includes:

* A logging framework close to Java log4j framework,
* Support for properties
* A serialization/deserialization framework for XML, JSON, CSV
* Ada beans framework
* Encoding/decoding framework (Base16, Base64, SHA, HMAC-SHA)
* A composing stream framework (raw, files, buffers, pipes, sockets)
* Several concurrency tools (reference counters, counters, pools, fifos, arrays)
* Process creation and pipes
* Support for loading shared libraries (on Windows or Unix)
* HTTP client library on top of CURL or AWS

Ada Util also provides a small test utility library on top of
Ahven or AUnit to help in writing unit tests.
To use Ahven testing framework, configure as follows:

```
./configure --enable-ahven
make
```

To use AUnit, build with the following commands:
```
./configure
make
```
The samples can be built using:
```
   gnatmake -Psamples
```   
   
The unit tests are built and executed with:
```
   make test
```
Or manually build with:
```
   gnatmake -Ptests
```

And unit tests are executed with (216 success, 0 failed):
```
   bin/util_harness
```
For the installation, use the following command:
```
   make install
```

# Documentation

The Ada Util sources as well as a wiki documentation is provided on:

   https://github.com/stcarrez/ada-util/wiki
