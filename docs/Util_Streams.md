# Streams
The `Util.Streams` package provides several types and operations to allow the
composition of input and output streams.  Input streams can be chained together so that
they traverse the different stream objects when the data is read from them.  Similarly,
output streams can be chained and the data that is written will traverse the different
streams from the first one up to the last one in the chain.  During such traversal, the
stream object is able to bufferize the data or make transformations on the data.

The `Input_Stream` interface represents the stream to read data.  It only provides a
`Read` procedure.  The `Output_Stream` interface represents the stream to write data.
It provides a `Write`, `Flush` and `Close` operation.

To use the packages described here, use the following GNAT project:

```Ada
with "utilada_sys";
```

## Buffered Streams
The `Output_Buffer_Stream` and `Input_Buffer_Stream` implement an output
and input stream respectively which manages an output or input buffer.  The data is
first written to the buffer and when the buffer is full or flushed, it gets written
to the target output stream.

The `Output_Buffer_Stream` must be initialized to indicate the buffer size as well
as the target output stream onto which the data will be flushed.  For example, a
pipe stream could be created and configured to use the buffer as follows:

```Ada
with Util.Streams.Buffered;
with Util.Streams.Pipes;
...
   Pipe   : aliased Util.Streams.Pipes.Pipe_Stream;
   Buffer : Util.Streams.Buffered.Output_Buffer_Stream;
   ...
      Buffer.Initialize (Output => Pipe'Unchecked_Access,
                         Size => 1024);
```

In this example, the buffer of 1024 bytes is configured to flush its content to the
pipe input stream so that what is written to the buffer will be received as input by
the program.
The `Output_Buffer_Stream` provides write operation that deal only with binary data
(`Stream_Element`).  To write text, it is best to use the `Print_Stream` type from
the `Util.Streams.Texts` package as it extends the `Output_Buffer_Stream` and provides
several operations to write character and strings.

The `Input_Buffer_Stream` must also be initialized to also indicate the buffer size
and either an input stream or an input content.  When configured, the input stream is used
to fill the input stream buffer.  The buffer configuration is very similar as the
output stream:

```Ada
with Util.Streams.Buffered;
with Util.Streams.Pipes;
...
   Pipe   : aliased Util.Streams.Pipes.Pipe_Stream;
   Buffer : Util.Streams.Buffered.Input_Buffer_Stream;
   ...
      Buffer.Initialize (Input => Pipe'Unchecked_Access, Size => 1024);
```

In this case, the buffer of 1024 bytes is filled by reading the pipe stream, and thus
getting the program's output.

## Texts
The `Util.Streams.Texts` package implements text oriented input and output streams.
The `Print_Stream` type extends the `Output_Buffer_Stream` to allow writing
text content.

The `Reader_Stream` type extends the `Input_Buffer_Stream` and allows to
read text content.

## File streams
The `Util.Streams.Files` package provides input and output streams that access
files on top of the Ada `Stream_IO` standard package.

## Pipes
The `Util.Streams.Pipes` package defines a pipe stream to or from a process.
It allows to launch an external program while getting the program standard output or
providing the program standard input.  The `Pipe_Stream` type represents the input or
output stream for the external program.  This is a portable interface that works on
Unix and Windows.

The process is created and launched by the `Open` operation.  The pipe allows
to read or write to the process through the `Read` and `Write` operation.
It is very close to the *popen* operation provided by the C stdio library.
First, create the pipe instance:

```Ada
with Util.Streams.Pipes;
...
   Pipe : aliased Util.Streams.Pipes.Pipe_Stream;
```

The pipe instance can be associated with only one process at a time.
The process is launched by using the `Open` command and by specifying the command
to execute as well as the pipe redirection mode:

* `READ` to read the process standard output,
* `WRITE` to write the process standard input.

For example to run the `ls -l` command and read its output, we could run it by using:

```Ada
Pipe.Open (Command => "ls -l", Mode => Util.Processes.READ);
```

The `Pipe_Stream` is not buffered and a buffer can be configured easily by using the
`Input_Buffer_Stream` type and connecting the buffer to the pipe so that it reads
the pipe to fill the buffer.  The initialization of the buffer is the following:

```Ada
with Util.Streams.Buffered;
...
   Buffer : Util.Streams.Buffered.Input_Buffer_Stream;
   ...
   Buffer.Initialize (Input => Pipe'Unchecked_Access, Size => 1024);
```

And to read the process output, one can use the following:

```Ada
 Content : Ada.Strings.Unbounded.Unbounded_String;
 ...
 Buffer.Read (Into => Content);
```

The pipe object should be closed when reading or writing to it is finished.
By closing the pipe, the caller will wait for the termination of the process.
The process exit status can be obtained by using the `Get_Exit_Status` function.

```Ada
 Pipe.Close;
 if Pipe.Get_Exit_Status /= 0 then
    Ada.Text_IO.Put_Line ("Command exited with status "
                          & Integer'Image (Pipe.Get_Exit_Status));
 end if;
```

You will note that the `Pipe_Stream` is a limited type and thus cannot be copied.
When leaving the scope of the `Pipe_Stream` instance, the application will wait for
the process to terminate.

Before opening the pipe, it is possible to have some control on the process that
will be created to configure:

  * The shell that will be used to launch the process,
  * The process working directory,
  * Redirect the process output to a file,
  * Redirect the process error to a file,
  * Redirect the process input from a file.

All these operations must be made before calling the `Open` procedure.

## Sockets
The <b>Util.Streams.Sockets</b> package defines a socket stream.

## Raw files
The <b>Util.Streams.Raw</b> package provides a stream directly on top of
file system operations <b>read</b> and <b>write</b>.

## Encoder Streams
The `Util.Streams.Buffered.Encoders` is a generic package which implements an
encoding or decoding stream through the `Transformer` interface.  The generic
package must be instantiated with a transformer type.  The stream passes the data
to be written to the `Transform` method of that interface and it makes
transformations on the data before being written.

The AES encoding stream is created as follows:

```Ada
package Encoding is
  new Util.Streams.Buffered.Encoders (Encoder => Util.Encoders.AES.Encoder);
```

and the AES decoding stream is created with:

```Ada
package Decoding is
  new Util.Streams.Buffered.Encoders (Encoder => Util.Encoders.AES.Decoder);
```

The encoding stream instance is declared:

```Ada
  Encode : Util.Streams.Buffered.Encoders.Encoder_Stream;
```

The encoding stream manages a buffer that is used to hold the encoded data before it is
written to the target stream.  The `Initialize` procedure must be called to indicate
the target stream, the size of the buffer and the encoding format to be used.

```Ada
 Encode.Initialize (Output => File'Access, Size => 4096, Format => "base64");
```

The encoding stream provides a `Produces` procedure that reads the encoded
stream and write the result in another stream.  It also provides a `Consumes`
procedure that encodes a stream by reading its content and write the encoded
result to another stream.

## Base16 Encoding Streams
The `Util.Streams.Base16` package provides streams to encode and decode the stream
using Base16.

## Base64 Encoding Streams
The `Util.Streams.Base64` package provides streams to encode and decode the stream
using Base64.

## AES Encoding Streams
The `Util.Streams.AES` package define the `Encoding_Stream` and `Decoding_Stream` types to
encrypt and decrypt using the AES cipher.  Before using these streams, you must use
the `Set_Key` procedure to setup the encryption or decryption key and define the AES
encryption mode to be used.  The following encryption modes are supported:

* AES-ECB
* AES-CBC
* AES-PCBC
* AES-CFB
* AES-OFB
* AES-CTR

The encryption and decryption keys are represented by the `Util.Encoders.Secret_Key` limited
type.  The key cannot be copied, has its content protected and will erase the memory once
the instance is deleted.  The size of the encryption key defines the AES encryption level
to be used:

* Use 16 bytes, or `Util.Encoders.AES.AES_128_Length` for AES-128,
* Use 24 bytes, or `Util.Encoders.AES.AES_192_Length` for AES-192,
* Use 32 bytes, or `Util.Encoders.AES.AES_256_Length` for AES-256.

Other key sizes will raise a pre-condition or constraint error exception.
The recommended key size is 32 bytes to use AES-256.  The key could be declared as follows:

```Ada
Key : Util.Encoders.Secret_Key
          (Length => Util.Encoders.AES.AES_256_Length);
```

The encryption and decryption key are initialized by using the `Util.Encoders.Create`
operations or by using one of the key derivative functions provided by the
`Util.Encoders.KDF` package.  A simple string password is created by using:

```Ada
Password_Key : constant Util.Encoders.Secret_Key
          := Util.Encoders.Create ("mysecret");
```

Using a password key like this is not the good practice and it may be useful to generate
a stronger key by using one of the key derivative function.  We will use the
PBKDF2 HMAC-SHA256 with 20000 loops (see [RFC 8018](https://tools.ietf.org/html/rfc8018)):

```Ada
Util.Encoders.KDF.PBKDF2_HMAC_SHA256 (Password => Password_Key,
                                      Salt     => Password_Key,
                                      Counter  => 20000,
                                      Result   => Key);
```

To write a text, encrypt the content and save the file, we can chain several stream objects
together.  Because they are chained, the last stream object in the chain must be declared
first and the first element of the chain will be declared last.  The following declaration
is used:

```Ada
  Out_Stream   : aliased Util.Streams.Files.File_Stream;
  Cipher       : aliased Util.Streams.AES.Encoding_Stream;
  Printer      : Util.Streams.Texts.Print_Stream;
```

The stream objects are chained together by using their `Initialize` procedure.
The `Out_Stream` is configured to write on the `encrypted.aes` file.
The `Cipher` is configured to write in the `Out_Stream` with a 32Kb buffer.
The `Printer` is configured to write in the `Cipher` with a 4Kb buffer.

```Ada
  Out_Stream.Initialize (Mode => Ada.Streams.Stream_IO.In_File,
                         Name => "encrypted.aes");
  Cipher.Initialize (Output => Out_Stream'Unchecked_Access,
                     Size   => 32768);
  Printer.Initialize (Output => Cipher'Unchecked_Access,
                      Size   => 4096);
```

The last step before using the cipher is to configure the encryption key and modes:

```Ada
  Cipher.Set_Key (Secret => Key, Mode => Util.Encoders.AES.ECB);
```

It is now possible to write the text by using the `Printer` object:

```Ada
Printer.Write ("Hello world!");

```

