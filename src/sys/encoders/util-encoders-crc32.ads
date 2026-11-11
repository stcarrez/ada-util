-----------------------------------------------------------------------
--  util-encoders-crc32 -- CRC32
--  Copyright (C) 2025-2026 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--  SPDX-License-Identifier: Apache-2.0
-----------------------------------------------------------------------

with Ada.Streams;
with Interfaces;

--  == CRC32 ==
--  The `Util.Encoders.CRC32` package implements the standard CRC-32
--  checksum (CRC-32/ISO-HDLC) as used by zlib, gzip, ZIP, PNG and Ethernet:
--
--  - polynomial    : 16#04C11DB7# (reflected form: 16#EDB88320#)
--  - initial value : 16#FFFFFFFF#
--  - reflected input and output
--  -  final XOR     : 16#FFFFFFFF#
--
--  With these settings, the CRC-32 of the ASCII string "123456789" is 16#CBF43926#.
--  To compute the CRC32, create a context object and feed it with the content
--  by calling `Update`.  At the end, get the result by calling `Finish`:
--
--    Context : Util.Encoders.CRC32.Context;
--    Digest  : Util.Encoders.Digest;
--    ...
--      Context.Update ("data...");
--      Context.Finish (Digest);
--
--  If you want to use a different polynomial, call `Init` before feeding the
--  context:
--
--      Context.Init (16#82F6_3B78#); -- CRC-32C (Castagnoli)
--
package Util.Encoders.CRC32 with Preelaborate is

   subtype Unsigned_32 is Interfaces.Unsigned_32;
   subtype Digest is Interfaces.Unsigned_32;

   --  The standard CRC-32 polynomial 16#04C11DB7# in reflected (reversed) form.
   CRC32_Poly : constant Unsigned_32 := 16#EDB8_8320#;

   --  ------------------------------
   --  CRC32 Context
   --  ------------------------------
   type Context is tagged limited private;

   --  Initialize the context to compute the CRC32 with the given polynomial.
   --  The polynomial is given in reflected form (see `CRC32_Poly`).
   --  The standard CRC-32 polynomial is used by default so that a context
   --  can be used directly with `Update` without calling `Init`.
   procedure Init (E    : in out Context;
                   Poly : in Unsigned_32 := CRC32_Poly);

   --  Update the crc32 with the string.
   procedure Update (E : in out Context;
                     S : in String);

   --  Update the crc32 with the array.
   procedure Update (E : in out Context;
                     S : in Ada.Streams.Stream_Element_Array);

   --  Computes the CRC32 value.
   procedure Finish (E      : in out Context;
                     Result : out Digest);

private

   type Crc_Table is array (0 .. 255) of Unsigned_32;

   --  CRC32 table for the standard polynomial 16#EDB88320# (see samples/crc32table.adb)
   Crc32_Table : constant Crc_Table :=
      [16#00000000#, 16#77073096#, 16#EE0E612C#, 16#990951BA#,
       16#076DC419#, 16#706AF48F#, 16#E963A535#, 16#9E6495A3#,
       16#0EDB8832#, 16#79DCB8A4#, 16#E0D5E91E#, 16#97D2D988#,
       16#09B64C2B#, 16#7EB17CBD#, 16#E7B82D07#, 16#90BF1D91#,
       16#1DB71064#, 16#6AB020F2#, 16#F3B97148#, 16#84BE41DE#,
       16#1ADAD47D#, 16#6DDDE4EB#, 16#F4D4B551#, 16#83D385C7#,
       16#136C9856#, 16#646BA8C0#, 16#FD62F97A#, 16#8A65C9EC#,
       16#14015C4F#, 16#63066CD9#, 16#FA0F3D63#, 16#8D080DF5#,
       16#3B6E20C8#, 16#4C69105E#, 16#D56041E4#, 16#A2677172#,
       16#3C03E4D1#, 16#4B04D447#, 16#D20D85FD#, 16#A50AB56B#,
       16#35B5A8FA#, 16#42B2986C#, 16#DBBBC9D6#, 16#ACBCF940#,
       16#32D86CE3#, 16#45DF5C75#, 16#DCD60DCF#, 16#ABD13D59#,
       16#26D930AC#, 16#51DE003A#, 16#C8D75180#, 16#BFD06116#,
       16#21B4F4B5#, 16#56B3C423#, 16#CFBA9599#, 16#B8BDA50F#,
       16#2802B89E#, 16#5F058808#, 16#C60CD9B2#, 16#B10BE924#,
       16#2F6F7C87#, 16#58684C11#, 16#C1611DAB#, 16#B6662D3D#,
       16#76DC4190#, 16#01DB7106#, 16#98D220BC#, 16#EFD5102A#,
       16#71B18589#, 16#06B6B51F#, 16#9FBFE4A5#, 16#E8B8D433#,
       16#7807C9A2#, 16#0F00F934#, 16#9609A88E#, 16#E10E9818#,
       16#7F6A0DBB#, 16#086D3D2D#, 16#91646C97#, 16#E6635C01#,
       16#6B6B51F4#, 16#1C6C6162#, 16#856530D8#, 16#F262004E#,
       16#6C0695ED#, 16#1B01A57B#, 16#8208F4C1#, 16#F50FC457#,
       16#65B0D9C6#, 16#12B7E950#, 16#8BBEB8EA#, 16#FCB9887C#,
       16#62DD1DDF#, 16#15DA2D49#, 16#8CD37CF3#, 16#FBD44C65#,
       16#4DB26158#, 16#3AB551CE#, 16#A3BC0074#, 16#D4BB30E2#,
       16#4ADFA541#, 16#3DD895D7#, 16#A4D1C46D#, 16#D3D6F4FB#,
       16#4369E96A#, 16#346ED9FC#, 16#AD678846#, 16#DA60B8D0#,
       16#44042D73#, 16#33031DE5#, 16#AA0A4C5F#, 16#DD0D7CC9#,
       16#5005713C#, 16#270241AA#, 16#BE0B1010#, 16#C90C2086#,
       16#5768B525#, 16#206F85B3#, 16#B966D409#, 16#CE61E49F#,
       16#5EDEF90E#, 16#29D9C998#, 16#B0D09822#, 16#C7D7A8B4#,
       16#59B33D17#, 16#2EB40D81#, 16#B7BD5C3B#, 16#C0BA6CAD#,
       16#EDB88320#, 16#9ABFB3B6#, 16#03B6E20C#, 16#74B1D29A#,
       16#EAD54739#, 16#9DD277AF#, 16#04DB2615#, 16#73DC1683#,
       16#E3630B12#, 16#94643B84#, 16#0D6D6A3E#, 16#7A6A5AA8#,
       16#E40ECF0B#, 16#9309FF9D#, 16#0A00AE27#, 16#7D079EB1#,
       16#F00F9344#, 16#8708A3D2#, 16#1E01F268#, 16#6906C2FE#,
       16#F762575D#, 16#806567CB#, 16#196C3671#, 16#6E6B06E7#,
       16#FED41B76#, 16#89D32BE0#, 16#10DA7A5A#, 16#67DD4ACC#,
       16#F9B9DF6F#, 16#8EBEEFF9#, 16#17B7BE43#, 16#60B08ED5#,
       16#D6D6A3E8#, 16#A1D1937E#, 16#38D8C2C4#, 16#4FDFF252#,
       16#D1BB67F1#, 16#A6BC5767#, 16#3FB506DD#, 16#48B2364B#,
       16#D80D2BDA#, 16#AF0A1B4C#, 16#36034AF6#, 16#41047A60#,
       16#DF60EFC3#, 16#A867DF55#, 16#316E8EEF#, 16#4669BE79#,
       16#CB61B38C#, 16#BC66831A#, 16#256FD2A0#, 16#5268E236#,
       16#CC0C7795#, 16#BB0B4703#, 16#220216B9#, 16#5505262F#,
       16#C5BA3BBE#, 16#B2BD0B28#, 16#2BB45A92#, 16#5CB36A04#,
       16#C2D7FFA7#, 16#B5D0CF31#, 16#2CD99E8B#, 16#5BDEAE1D#,
       16#9B64C2B0#, 16#EC63F226#, 16#756AA39C#, 16#026D930A#,
       16#9C0906A9#, 16#EB0E363F#, 16#72076785#, 16#05005713#,
       16#95BF4A82#, 16#E2B87A14#, 16#7BB12BAE#, 16#0CB61B38#,
       16#92D28E9B#, 16#E5D5BE0D#, 16#7CDCEFB7#, 16#0BDBDF21#,
       16#86D3D2D4#, 16#F1D4E242#, 16#68DDB3F8#, 16#1FDA836E#,
       16#81BE16CD#, 16#F6B9265B#, 16#6FB077E1#, 16#18B74777#,
       16#88085AE6#, 16#FF0F6A70#, 16#66063BCA#, 16#11010B5C#,
       16#8F659EFF#, 16#F862AE69#, 16#616BFFD3#, 16#166CCF45#,
       16#A00AE278#, 16#D70DD2EE#, 16#4E048354#, 16#3903B3C2#,
       16#A7672661#, 16#D06016F7#, 16#4969474D#, 16#3E6E77DB#,
       16#AED16A4A#, 16#D9D65ADC#, 16#40DF0B66#, 16#37D83BF0#,
       16#A9BCAE53#, 16#DEBB9EC5#, 16#47B2CF7F#, 16#30B5FFE9#,
       16#BDBDF21C#, 16#CABAC28A#, 16#53B39330#, 16#24B4A3A6#,
       16#BAD03605#, 16#CDD70693#, 16#54DE5729#, 16#23D967BF#,
       16#B3667A2E#, 16#C4614AB8#, 16#5D681B02#, 16#2A6F2B94#,
       16#B40BBE37#, 16#C30C8EA1#, 16#5A05DF1B#, 16#2D02EF8D#];

   type Context is tagged limited record
      Value : Digest := 16#FFFF_FFFF#;
      Crc32 : Crc_Table := Crc32_Table;
   end record;

end Util.Encoders.CRC32;
