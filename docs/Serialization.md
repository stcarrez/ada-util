# Serialization of data structures in CSV/JSON/XML

## Introduction

The `Util.Serialize` package provides a customizable
framework to serialize and de-serialize data structures in CSV, JSON and XML.
It is inspired from the Java [XStream](https://x-stream.github.io/) library.

## Record Mapping

The serialization relies on a mapping that must be provided
for each data structure that must be read.  Basically,
it consists in writing an enum type, a procedure and instantiating
a mapping package.  Let's assume we have a record declared as follows:

```Ada
type Address is record       
  City      : Unbounded_String;
  Street    : Unbounded_String;
  Country   : Unbounded_String;
  Zip       : Natural;
end record;  
```

The enum type shall define one value for each record member that has
to be serialized/deserialized.

```Ada
 type Address_Fields is (FIELD_CITY, FIELD_STREET, FIELD_COUNTRY, FIELD_ZIP);
```

The de-serialization uses a specific procedure to fill the record member.
The procedure that must be written is in charge of writing one field in the record. For that it gets the record as an in out parameter, the field identification and the value.

```Ada
procedure Set_Member (Addr  : in out Address;
                      Field : in Address_Fields;
                      Value : in Util.Beans.Objects.Object) is
begin
   case Field is
     when FIELD_CITY =>
       Addr.City := To_Unbounded_String (Value);

     when FIELD_STREET =>
       Addr.Street := To_Unbounded_String (Value);

     when FIELD_COUNTRY =>
       Addr.Country := To_Unbounded_String (Value);
     
     when FIELD_ZIP =>
        Addr.Zip := To_Integer (Value);
   end case;    
end Set_Member; 
```

The procedure will be called by the CSV, JSON or XML reader when a field
is recognized.

The serialization to JSON or XML needs a function that returns the field
value from the record value and the field identification.  The value is
returned as a **Util.Beans.Objects.Object** type which can hold a string,
a wide wide string, a boolean, a date, an integer or a float.

```Ada
function Get_Member (Addr  : in Address;
                     Field : in Address_Fields) return Util.Beans.Objects.Object is
begin
   case Field is
      when FIELD_CITY =>
         return Util.Beans.Objects.To_Object (Addr.City);

      when FIELD_STREET =>
         return Util.Beans.Objects.To_Object (Addr.Street);

      when FIELD_COUNTRY =>
         return Util.Beans.Objects.To_Object (Addr.Country);

      when FIELD_ZIP =>
         return Util.Beans.Objects.To_Object (Addr.Zip);

   end case;
end Get_Member;
```

A mapping package has to be instantiated to provide the necessary
glue to tie the set procedure to the framework.

```Ada
package Address_Mapper is
  new Util.Serialize.Mappers.Record_Mapper
     (Element_Type        => Address,    
      Element_Type_Access => Address_Access,
      Fields              => Address_Fields,
      Set_Member          => Set_Member);  
```

**Note**: a bug in the gcc compiler does not allow to specify the
**!Get_Member** function in the generic package.  As a work-arround, the
function must be associated with the mapping using the **Bind** procedure.

## Mapping Definition

The mapping package defines a `Mapper` type which holds the
mapping definition.  The mapping definition tells a mapper what name correspond to the different fields.
It is possible to define several mappings
for the same record type.  The mapper object is declared as follows:

```Ada
Address_Mapping : Address_Mapper.Mapper;  
```

Then, each field is bound to a name as follows:

```Ada
Address_Mapping.Add_Mapping ("city", FIELD_CITY);
Address_Mapping.Add_Mapping ("street", FIELD_STREET);
Address_Mapping.Add_Mapping ("country", FIELD_COUNTRY);
Address_Mapping.Add_Mapping ("zip", FIELD_ZIP);
```

Once initialized, the same mapper can be used read several files in several
threads at the same time (the mapper is only read by the JSON/XML parsers).

## De-serialization

To de-serialize a JSON object, a parser object is created and one or several mappings are defined:
```Ada
Reader : Util.Serialize.IO.JSON.Parser;
...
   Reader.Add_Mapping ("address", Address_Mapping'Access);
```

For an XML de-serialize, we just have to use another parser:

```Ada
Reader : Util.Serialize.IO.XML.Parser;
...
   Reader.Add_Mapping ("address", Address_Mapping'Access);
```

For a CSV de-serialize, we just have to use another parser:

```Ada
Reader : Util.Serialize.IO.CSV.Parser;
...
   Reader.Add_Mapping ("", Address_Mapping'Access);
```

The next step is to indicate the object that the de-serialization will write into.  For this, the generic package provided the `!Set_Context` procedure
to register the root object that will be
filled according to the mapping.

```Ada
Addr : aliased Address;
...
  Address_Mapper.Set_Context (Reader, Addr'Access);
```

The `Parse` procedure parses a file using a CSV, JSON or XML parser.  It uses the mappings registered by `Add_Mapping` and fills the objects registered by `Set_Context`.  When the parsing is successful, the `Addr` object will hold the values.
```Ada
  Reader.Parse (File);
```

## Parser Specificities

### XML

XML has attributes and entities both of them being associated with a name.
For the mapping, to specify that a value is stored in an XML attribute, the
name must be prefixed by the **@** sign (this is very close to an XPath expression).  For example if the `city` XML entity has an `id` attribute,
we could map it to a field `FIELD_CITY_ID` as follows:

```Ada
Address_Mapping.Add_Mapping ("city/@id", FIELD_CITY_ID);
```

### CSV

A CSV file is flat and each row is assumed to contain the same kind of entities.  By default the CSV file contains as first row a column header which is used
by the de-serialization to make the column field association.  The mapping
defined through `Add_Mapping` uses the column header name to indicate
which column correspond to which field.

If a CSV file does not contain a column header, the mapping must be created
by using the default column header names (Ex: A, B, C, ..., AA, AB, ...).
The parser must be told about this lack of column header:

```Ada
   Parser.Set_Default_Headers;
```
