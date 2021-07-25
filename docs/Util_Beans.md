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

  * a generic data

Several operations are provided to convert a value into an `Object`.

```Ada
Value : Util.Beans.Objects.Object := Util.Beans.Objects.To_Object ("something");
Value := Value + To_Object ("12");
```
## Datasets
The `Datasets` package implements the `Dataset` list bean which
defines a set of objects organized in rows and columns.  The `Dataset`
implements the `List_Bean` interface and allows to iterate over its rows.
Each row defines a `Bean` instance and allows to access each column value.
Each column is associated with a unique name.  The row `Bean` allows to
get or set the column by using the column name.
## Bean Interface
An Ada Bean is an object which implements the `Util.Beans.Basic.Readonly_Bean` or the
`Util.Beans.Basic.Bean` interface.  By implementing these interface, the object provides
a behavior that is close to the Java Beans: a getter and a setter operation are available.


