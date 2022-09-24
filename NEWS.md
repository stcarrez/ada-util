Version 2.5.0  - Aug 2022
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

Version 2.4.1  - Jul 2021
  - Fix compilation issues with GNAT 2021
  - Fix serialization of Util.Beans.Objects holding arrays or maps

Version 2.4.0  - Feb 2021
  - Add support to customize and provide application specific log appenders (example in ada-keystore)
  - Improvement of read/write streams to chain LZMA, AES, Base64
  - Add examples to show LZMA compress+AES encryption, AES decryption+LZMA decompress
  - Fix compilation with GNAT 10
  - New package Util.Properties.Form to help in parsing application/x-www-form-urlencoded
  - Fixed the Util.Tests.Get_Test_Path semantic and use the results directory
  - Drop detection of buggy gcc 4.7.2

Version 2.3.0  - Nov 2020
  - New stream operations to read/write UTF-8 sequences in Wide_Wide character
  - Fix AES encryption in CFB, OFB and CTR modes
  - Add HTTP support for HEAD, OPTIONS, PATCH requests

Version 2.2.0  - May 2020
  - New Wait_Empty operation on fifo.
  - Add Get_Count and Wait operation on executors

Version 2.1.0  - Feb 2020
  - Improvement to allow custom Util.Properties implementation

Version 2.0.1  - Jan 2019
  - Fixed Util.Processes mixed pipe and file redirections

Version 2.0.0  - Dec 2019
  - Rename GNAT projects and split the library in several parts
  - New samples to show compression, decompression, encryption, decryption streams
  - Added AES encryption and decryption
  - Added password based key derivation function (PBKDF2)
  - Added encoders error correction code (ECC)
  - Added generic concurrent sequence queues
  - Added generic work queue executor

Version 1.9.0   - Jul 2018
  - Improvement and fixes of the JSON, XML, CSV serialization
  - Improvement of properties to also read and describe INI files
  - Add encoders to support SHA256 and HMAC-SHA256
  - Added a command package for implementation of command line tools
  - Added event timer list management
  - Fix on the HTTP curl support
  - Implementation of x-www-form-urlencoded serialization
  - Added localized date parsing

Version 1.8.0   - Dec 2015
  - Added support for immediate flush and file appending to the file logger
  - Added support for RFC7231/RFC2616 date conversion
  - Improvement of configure and installation process with gprinstall (if available)
  - Added file system stat/fstat support
  - Use gcc intrinsics for atomic counters (Intel, Arm)

Version 1.7.2   - May 2015
  - Fix end condition when reading stream buffer

Version 1.7.1   - Jul 2014
  - Support XmlAda 2014
  - Fixed Get_Week_Start/Get_Week_End when the system timezone is
    different than the asked timezone

Version 1.7     - Feb 2014
  - Added a text and string builder
  - Added date helper operations to get the start of day, week or month time
  - Support XmlAda 2013
  - Added Objects.Datasets to provide list beans (lists of row/column objects)
  - Added support for shared library loading
  - Support for the creation of Debian packages
  - Update Ahven integration to 2.3
  - New option -r <test> option for the unit test harness to execute a single test
  - Port on FreeBSD

Version 1.6.1   - Feb 2013
  - Fix compilation with gcc 4.7.2 (workarround to avoid gcc bug 53737)

Version 1.6     - Dec 2012
  - Support for HTTP clients (curl, AWS, ...)
  - Support for REST APIs using JSON
  - New operations To_JSON and From_JSON for easy object map serialization
  - Added a listeners to help implementing the observer/listener design patterns
  - Added support for wildcard mapping in serializaton framework
  - New option -d <dir> for the unit test harness to change the working directory

Version 1.5     - May 2012
  - Added concurrent fifo queues and arrays
  - Changed Objects.Maps to use a String instead of an Unbounded_String as the key
  - Support for shared or static build configuration
  - Implement input/output/error redirection to a file for process launch
  - Added a string tokenizer

Version 1.4     - Jan 2012
  - Support for localized date format
  - Support for process creation and pipe streams (on Unix and Windows)
  - Support for CSV in the serialization framework
  - Integrate Ahven 2.1 for the unit tests (activate with --enable-ahven)
  - Add a tool to generate perfect hash function

Version 1.3     - Sep 2011
  - Added concurrent object pools
  - Support XmlAda 4.1 (GNAT 2011)
  - Better support to report errors in the serialization framework
  - Optimize Util.Log implementation

Version 1.2	- May 2011
  - Integrates the Ada EL beans framework
  - Added encoders (base16, base64, SHA1, HMAC-SHA1)
  - Added serialization framework (JSON, XML) - EXPERIMENTAL
  - Added reference counters

Version 1.1	- Aug 2010
  - Added text transformations
  - Added Locales
  - Added property bundles

Version 1.0	- Jul 2010
  - Supports logs, properties, concurrent counters
