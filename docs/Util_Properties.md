# Property Files
The `Util.Properties` package and children implements support to read, write and use
property files either in the Java property file format or the Windows INI configuration file.
Each property is assigned a key and a value.  The list of properties are stored in the
`Util.Properties.Manager` tagged record and they are indexed by the key name.  A property
is therefore unique in the list.  Properties can be grouped together in sub-properties so
that a key can represent another list of properties.  To use the packages described here,
use the following GNAT project:

```Ada
with "utilada_base";
```

## File formats
The property file consists of a simple name and value pair separated by the `=` sign.
Thanks to the Windows INI file format, list of properties can be grouped together
in sections by using the `[section-name]` notation.

```Ada
test.count=20
test.repeat=5
[FileTest]
test.count=5
test.repeat=2
```

## Using property files
An instance of the `Util.Properties.Manager` tagged record must be declared and it provides
various operations that can be used.  When created, the property manager is empty.  One way
to fill it is by using the `Load_Properties` procedure to read the property file.  Another
way is by using the `Set` procedure to insert or change a property by giving its name
and its value.

In this example, the property file `test.properties` is loaded and assuming that it contains
the above configuration example, the `Get ("test.count")` will return the string `"20"`.
The property `test.repeat` is then modified to have the value `"23"` and the properties are
then saved in the file.

```Ada
with Util.Properties;
...
   Props : Util.Properties.Manager;
   ...
      Props.Load_Properties (Path => "test.properties");
      Ada.Text_IO.Put_Line ("Count: " & Props.Get ("test.count");
      Props.Set ("test.repeat", "23");
      Props.Save_Properties (Path => "test.properties");
```

To be able to access a section from the property manager, it is necessary to retrieve it
by using the `Get` function and giving the section name.  For example, to retrieve the
`test.count` property of the `FileTest` section, the following code is used:

```Ada
   FileTest : Util.Properties.Manager := Props.Get ("FileTest");
   ...
      Ada.Text_IO.Put_Line ("[FileTest] Count: "
                            & FileTest.Get ("test.count");
```

When getting or removing a property, the `NO_PROPERTY` exception is raised if the property
name was not found in the map.  To avoid that exception, it is possible to check whether
the name is known by using the `Exists` function.

```Ada
   if Props.Exists ("test.old_count") then
      ... --  Property exist
   end if;
```

## Reading JSON property files
The `Util.Properties.JSON` package provides operations to read a JSON
content and put the result in a property manager.  The JSON content is flattened
into a set of name/value pairs.  The JSON structure is reflected in the name.
Example:

```Ada
{ "id": "1",                                 id         -> 1
  "info": { "name": "search",                info.name  -> search
            "count": "12",                   info.count -> 12
            "data": { "value": "empty" }},   info.data.value  -> empty
  "count": 1                                 count      -> 1
}
```

To get the value of a JSON property, the user can use the flatten name.  For example:

```Ada
 Value : constant String := Props.Get ("info.data.value");
```

The default separator to construct a flatten name is the dot (`.`) but this can be
changed easily when loading the JSON file by specifying the desired separator:

```Ada
 Util.Properties.JSON.Read_JSON (Props, "config.json", "|");
```

Then, the property will be fetch by using:

```Ada
 Value : constant String := Props.Get ("info|data|value");
```

## Property bundles
Property bundles represent several property files that share some overriding rules and
capabilities.  Their introduction comes from Java resource bundles which allow to
localize easily some configuration files or some message.  When loading a property bundle
a locale is defined to specify the target language and locale.  If a specific property
file for that locale exists, it is used first.  Otherwise, the property bundle will use
the default property file.

A rule exists on the name of the specific property locale file: it must start with the
bundle name followed by `_` and the name of the locale.  The default property file must
be the bundle name.  For example, the bundle `dates` is associated with the following
property files:

```Ada
dates.properties           Default values (English locale)
dates_fr.properties        French locale
dates_de.properties        German locale
dates_es.properties        Spain locale
```

Because a bundle can be associated with one or several property files, a specific loader is
used.  The loader instance must be declared and configured to indicate one or several search
directories that contain property files.

```Ada
with Util.Properties.Bundles;
...
   Loader : Util.Properties.Bundles.Loader;
   Bundle : Util.Properties.Bundles.Manager;
   ...
   Util.Properties.Bundles.Initialize (Loader,
                                       "bundles;/usr/share/bundles");
   Util.Properties.Bundles.Load_Bundle (Loader, "dates", "fr", Bundle);
   Ada.Text_IO.Put_Line (Bundle.Get ("util.month1.long");
```

In this example, the `util.month1.long` key is first searched in the `dates_fr` French locale
and if it is not found it is searched in the default locale.

The restriction when using bundles is that they don't allow changing any value and the
`NOT_WRITEABLE` exception is raised when one of the `Set` operation is used.

When a bundle cannot be loaded, the `NO_BUNDLE` exception is raised by the `Load_Bundle`
operation.

## Advance usage of properties
The property manager holds the name and value pairs by using an Ada Bean object.

It is possible to iterate over the properties by using the `Iterate` procedure that
accepts as parameter a `Process` procedure that gets the property name as well as the
property value.  The value itself is passed as an `Util.Beans.Objects.Object` type.


