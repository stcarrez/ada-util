# Introduction

The Ada Utility Library provides a collection of utility packages which includes:

  * A logging framework close to Java log4j framework,
  * A support for properties,
  * A serialization/deserialization framework for XML, JSON, CSV,
  * Ada beans framework,
  * Encoding/decoding framework (Base16, Base64, SHA, HMAC-SHA, AES256, PBKDF2, ECC),
  * A composing stream framework (raw, files, buffers, pipes, sockets, compress),
  * Several concurrency tools (reference counters, counters, pools, fifos, arrays, sequences, executors),
  * Process creation and pipes,
  * Support for loading shared libraries (on Windows or Unix),
  * HTTP client library on top of CURL or AWS.

This document describes how to build the library and how you can use
the different features to simplify and help you in your Ada application.

