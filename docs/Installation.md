# Installation

This chapter explains how to build and install the library.

## Before Building

Before building the library, you will need:

* [XML/Ada](http://libre.adacore.com/libre/tools/xmlada/)
* [AWS](http://libre.adacore.com/libre/tools/aws/)

First get, build and install the [XML/Ada](http://libre.adacore.com/libre/tools/xmlada/)
and then get, build and install the [Ada Utility Library](https://github.com/stcarrez/ada-util).

## Configuration

The library uses the `configure` script to detect the build environment, check whether XML/Ada,
AWS, Curl support are available and configure everything before building.  If some component is missing, the
`configure` script will report an error or it will disable the feature.
The `configure` script provides several standard options
and you may use:

  * `--prefix=DIR` to control the installation directory,
  * `--enable-shared` to enable the build of shared libraries,
  * `--disable-static` to disable the build of static libraries,
  * `--enable-distrib` to build for a distribution and strip symbols,
  * `--disable-distrib` to build with debugging support,
  * `--enable-coverage` to build with code coverage support (`-fprofile-arcs -ftest-coverage`),
  * `--disable-traceback` to disable the support for symbolic traceback by the logging framework,
  * `--disable-ahven` to disable building the Ahven support used by the Ada utility testing framework,
  * `--enable-aunit` to enable building the AUnit support used by the Ada utility testing framework,
  * `--disable-curl` to disable the support for CURL,
  * `--disable-aws` to disable the support for AWS,
  * `--disable-lzma` to disable the support for LZMA,
  * `--with-xmlada=PATH` to control the installation path of [XML/Ada](http://libre.adacore.com/libre/tools/xmlada/),
  * `--with-aws=PATH` to control the installation path of [AWS](http://libre.adacore.com/libre/tools/aws/),
  * `--with-ada-lzma=PATH` to control the installation path of [Ada LZMA](https://github.com/stcarrez/ada-lzma),
  * `--enable-link-options-util=opts` to add some linker options when building the Ada Util shared library,
  * `--enable-link-options-curl=opts` to add some linker options when building the Ada Util Curl shared library,
  * `--help` to get a detailed list of supported options.

In most cases you will configure with the following command:
```
./configure
```

Building to get a shared library can sometimes be a real challenge.  With GNAT 2018, you
can configure as follows:

```
./configure --enable-shared
```

But with some other versions of the Ada compiler, you may need to add some linker options
to make sure that the generated shared library is useable.  Basically, it happens that
the `-ldl` is not passed correctly when the shared library is created and when it is used
you end up with missing symbols such as `dlopen`, `dlclose`, `dlsym` and `dlerror`.
When this happens, you can fix by re-configuring and adding the missing option
with the following command:

```
./configure --enable-shared --enable-link-options-util=--no-as-needed,-ldl,--as-needed
```

## Build

After configuration is successful, you can build the library by running:
```
make
```

After building, it is good practice to run the unit tests before installing the library.
The unit tests are built and executed using:
```
make test
```
And unit tests are executed by running the `bin/util_harness` test program.

## Installation
The installation is done by running the `install` target:

```
make install
```

If you want to install on a specific place, you can change the `prefix` and indicate the installation
direction as follows:

```
make install prefix=/opt
```

## Using

To use the library in an Ada project, add the following line at the beginning of your
GNAT project file:

```
with "utilada";
```

If you use only a subset of the library, you may use the following GNAT projects:

| GNAT project | Description                                          |
| ------------ | ---------------------------------------------------- |
| utilada_core | Provides: Util.Concurrent, Util.Strings, Util.Texts, |
|              | Util.Locales, Util.Refs, Util.Stacks, Util.Listeners |
|              | Util.Executors                                       |
| utilada_base | Provides: Util.Beans, Util.Commands, Util.Dates,     |
|              | Util.Events, Util.Files, Util.Log, Util.Properties,  |
|              | Util.Systems                                         |
| utilada_sys  | Provides: Util.Encoders, Util.Measures,              |
|              | Util.Processes, Util.Serialize, Util.Streams         |
| utilada_lzma | Provides: Util.Encoders.Lzma, Util.Streams.Buffered.Lzma    |
| utilada_aws  | Provides HTTP client support using AWS               |
| utilada_curl | Provides HTTP client support using CURL              |
| utilada_http | Provides Util.Http                                          |
| utilada      | Uses all utilada GNAT projects except the unit test library |
| utilada_unit | Support to write unit tests on top of Ahven or AUnit |

