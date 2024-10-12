# Installation

This chapter explains how to build and install the library.

## Using Alire

The Ada Utility Library is available as several Alire crates to simplify the installation
and setup your project.  Run the following commands to setup your Alire project to use the library:

```
alr index --update-all
alr with utilada
```

Depending on your project, you may need one or some of the following other components:

```
alr with utilada_xml
alr with utilada_unit
alr with utilada_curl
alr with utilada_aws
alr with utilada_lzma
```

## Without Alire

If you don't have [Alire](https://alire.ada.dev/) or want to build and install the library
on a specific place, run a `setup` command to configure the build as well as installation
directory.

The support for AWS, Curl, LZMA and XML/Ada are enabled only when a `HAVE_XXX=yes` configuration
variable has defined.  Run the setup command that records in the `Makefile.conf` the configuration
you want to build.  The example below enables the XML/Ada and AWS components but disables
the Curl and LZMA support.

```
make setup BUILD=debug PREFIX=/build/install \
  HAVE_XML_ADA=yes HAVE_AWS=yes \
  HAVE_CURL=no HAVE_LZMA=no
```

Then, build:

```
make
```

After building, it is good practice to run the unit tests before installing the library.
The unit tests are built and executed using:

```
make test
```

And unit tests are executed by running the `bin/util_harness` test program.

The installation is done by running the `install` target:

```
make install
```

To use the installed libraries, make sure your `ADA_PROJECT_PATH` contains the directory
where you installed the libraries (configured by the `PREFIX=<path>` option in the setup phase).
The installed GNAT projects are the same as those used when using [Alire](https://alire.ada.dev/).

If you want to install on a specific place, you can change the `prefix` and indicate the installation
direction as follows:

```
make install PREFIX=/opt
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

