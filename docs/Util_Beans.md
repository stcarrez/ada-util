# Ada Beans
A [Java Bean](https://en.wikipedia.org/wiki/JavaBean)(http://en.wikipedia.org/wiki/JavaBean) is an object that
allows to access its properties through getters and setters. Java Beans
rely on the use of Java introspection to discover the [Java Bean](https://en.wikipedia.org/wiki/JavaBean) object properties.

An Ada Bean has some similarities with the [Java Bean](https://en.wikipedia.org/wiki/JavaBean) as it tries to expose
an object through a set of common interfaces.  Since Ada does not have introspection,
some developer work is necessary. The Ada Bean framework consists of:

  * An `Object` concrete type that allows to hold any data type such
  as boolean, integer, floats, strings, dates and Ada bean objects.
  * A `Bean` interface that exposes a `Get_Value` and `Set_Value`
  operation through which the object properties can be obtained and modified.
  * A `Method_Bean` interface that exposes a set of method bindings
  that gives access to the methods provided by the Ada Bean object.

The benefit of Ada beans comes when you need to get a value or invoke
a method on an object but you don't know at compile time the object or method.
That step being done later through some external configuration or presentation file.

The Ada Bean framework is the basis for the implementation of
[Ada Server Faces](https://github.com/stcarrez/ada-asf) and [Ada EL](https://github.com/stcarrez/ada-el).  It allows the presentation layer to
access information provided by Ada beans.

To use the packages described here, use the following GNAT project:

```Ada
with "utilada_base";
```

## Objects
The `Util.Beans.Objects` package provides a data type to manage entities of different types
by using the same abstraction.  The `Object` type allows to hold various values of different
types.

An `Object` can hold one of the following values:

  * a boolean,
  * a long long integer,
  * a date,
  * a string,
  * a wide wide string,
  * an array of objects,
  * a generic data bean,
  * a map of objects,
  * a vector of object

Several operations are provided to convert a value into an `Object`.

```Ada
with Util.Beans.Objects;
  Value : Util.Beans.Objects.Object
     := Util.Beans.Objects.To_Object (String '("something"));
  Value := Value + To_Object (String '("12"));
  Value := Value - To_Object (Integer (3));

```

The package provides various operations to check, convert and use the `Object`
type.

| Name      | Description                              |
| --------- | ---------------------------------------- |
| Is_Empty  | Returns true if the object is the empty string or empty list |
| Is_Null   | Returns true if the object does not contain any value |
| Is_Array  | Returns true if the object is an array |
| Get_Type  | Get the type of the object |
| To_String | Converts the object to a string |
| To_Wide_Wide_String | Convert to a wide wide string |
| To_Unbounded_String | Convert to an unbounded string |
| To_Boolean | Convert to a boolean |
| To_Integer | Convert to an integer |
| To_Long_Integer | Convert to a long integer |
| To_Long_Long_Integer | Convert to a long long integer |
| To_Float   | Convert to a float |
| To_Long_Float | Convert to a long float |
| To_Long_Long_Float | Convert to a long long float |
| To_Duration  | Convert to a duration |
| To_Bean | Convert to an access to the Read_Only_Bean'Class |

Conversion to a time or enumeration is provided by specific packages.

The support for enumeration is made by the generic package
`Util.Beans.Objects.Enums` which must be instantiated with the enumeration
type.  Example of instantiation:

```Ada
 with Util.Beans.Objects.Enum;
 ...
    type Color_Type is (GREEN, BLUE, RED, BROWN);
    package Color_Enum is
       new Util.Beans.Objects.Enum (Color_Type);
```

Then, two functions are available to convert the enum value into an `Object`
or convert back the `Object` in the enum value:

```Ada
 Color : Object := Color_Enum.To_Object (BLUE);
 C : Color_Type := Color_Enum.To_Value (Color);
```

## Object maps
The `Util.Beans.Objects.Maps` package provides a map of objects with a `String`
as key.  This allows to associated names to objects.
To create an instance of the map, it is possible to use the `Create` function
as follows:

```Ada
with Util.Beans.Objects.Maps;
...
   Person : Util.Beans.Objects.Object := Util.Beans.Objects.Maps.Create;
```

Then, it becomes possible to populate the map with objects by using
the `Set_Value` procedure as follows:

```Ada
Util.Beans.Objects.Set_Value (Person, "name",
                              To_Object (Name));
Util.Beans.Objects.Set_Value (Person, "last_name",
                              To_Object (Last_Name));
Util.Beans.Objects.Set_Value (Person, "age",
                              To_Object (Age));
```

Getting a value from the map is done by using the `Get_Value` function:

```Ada
Name : Util.Beans.Objects.Object := Get_Value (Person, "name");
```

It is also possible to iterate over the values of the map by using
the `Iterate` procedure or by using the iterator support provided by
the `Util.Beans.Objects.Iterators` package.

## Object vectors
The `Util.Beans.Objects.Vectors` package provides a vector of objects.
To create an instance of the vector, it is possible to use the `Create` function
as follows:

```Ada
with Util.Beans.Objects.Vectors;
...
   List : Util.Beans.Objects.Object := Util.Beans.Objects.Vectors.Create;
```

## Datasets
The `Datasets` package implements the `Dataset` list bean which
defines a set of objects organized in rows and columns.  The `Dataset`
implements the `List_Bean` interface and allows to iterate over its rows.
Each row defines a `Bean` instance and allows to access each column value.
Each column is associated with a unique name.  The row `Bean` allows to
get or set the column by using the column name.

```Ada
 with Util.Beans.Objects.Datasets;
 ...
    Set : Util.Beans.Objects.Datasets.Dataset_Access
        := new Util.Beans.Objects.Datasets.Dataset;
```

After creation of the dataset instance, the first step is to define
the columns that composed the list.  This is done by using the `Add_Column`
procedure:

```Ada
 Set.Add_Column ("name");
 Set.Add_Column ("email");
 Set.Add_Column ("age");
```

To populate the dataset, the package only provide the `Append` procedure
which adds a new row and calls a procedure whose job is to fill the columns
of the new row.  The procedure gets the row as an array of `Object`:

```Ada
 procedure Fill (Row : in out Util.Beans.Objects.Object_Array) is
 begin
    Row (Row'First) := To_Object (String '("Yoda"));
    Row (Row'First + 1) := To_Object (String '("Yoda@Dagobah"));
    Row (Row'First + 2) := To_Object (Integer (900));
 end Fill;
 Set.Append (Fill'Access);

```

The dataset instance is converted to an `Object` by using the `To_Object`
function.  Note that the default behavior of `To_Object` is to take
the ownership of the object and hence it will be released automatically.

```Ada
 List : Util.Beans.Objects.Object
    := Util.Beans.Objects.To_Object (Set);
```

## Object iterator
Iterators are provided by the `Util.Beans.Objects.Iterators` package.
The iterator instance is created by using either the `First` or `Last`
function on the object to iterate.

```Ada
with Util.Beans.Objects.Iterators;
...
   Iter : Util.Beans.Objects.Iterators.Iterator
      := Util.Beans.Objects.Iterators.First (Object);
```

The iterator is used in conjunction with its `Has_Element` function
and either its `Next` or `Previous` procedure.  The current element
is obtained by using the `Element` function.  When the object being
iterated is a map, a key can be associated with the element and
is obtained by the `Key` function.

```Ada
while Util.Beans.Objects.Iterators.Has_Element (Iter) loop
   declare
      Item : Object := Util.Beans.Objects.Iterators.Element (Iter);
      Key  : String := Util.Beans.Objects.Iterators.Key (Iter);
   begin
      ...
      Util.Beans.Objects.Iterators.Next (Iter);
   end;
end loop;
```

## Bean Interface
An Ada Bean is an object which implements the `Util.Beans.Basic.Readonly_Bean` or the
`Util.Beans.Basic.Bean` interface.  By implementing these interface, the object provides
a behavior that is close to the Java Beans: a getter and a setter operation are available.


