# Other utilities



## Text Builders
The `Util.Texts.Builders` generic package was designed to provide string builders.
The interface was designed to reduce memory copies as much as possible.

  * The `Builder` type holds a list of chunks into which texts are appended.
  * The builder type holds an initial chunk whose capacity is defined when the builder
  instance is declared.
  * There is only an `Append` procedure which allows to append text to the builder.
  This is the only time when a copy is made.
  * The package defines the `Iterate` operation that allows to get the content
  collected by the builder.  When using the `Iterate` operation, no copy is
  performed since chunks data are passed passed by reference.
  * The type is limited to forbid copies of the builder instance.

First, instantiate the package for the element type (eg, String):

```Ada
package String_Builder is new Util.Texts.Builders (Character, String);
```

Declare the string builder instance with its initial capacity:

```Ada
Builder : String_Builder.Builder (256);
```

And append to it:

```Ada
String_Builder.Append (Builder, "Hello");
```

To get the content collected in the builder instance, write a procedure that receives
the chunk data as parameter:

```Ada
procedure Collect (Item : in String) is ...
```

And use the `Iterate` operation:

```Ada
String_Builder.Iterate (Builder, Collect'Access);
```

## Listeners
The `Listeners` package implements a simple observer/listener design pattern.
A subscriber registers to a list.  When a change is made on an object, the
application can notify the subscribers which are then called with the object.

### Creating the listener list
The listeners list contains a list of listener interfaces.

```Ada
L : Util.Listeners.List;
```

The list is heterogeneous meaning that several kinds of listeners could
be registered.

### Creating the observers
First the `Observers` package must be instantiated with the type being
observed.  In the example below, we will observe a string:

```Ada
package String_Observers is new Util.Listeners.Observers (String);
```

### Implementing the observer
Now we must implement the string observer:

```Ada
type String_Observer is new String_Observer.Observer with null record;
procedure Update (List : in String_Observer; Item : in String);
```

### Registering the observer
An instance of the string observer must now be registered in the list.

```Ada
O : aliased String_Observer;
L.Append (O'Access);
```

### Publishing
Notifying the listeners is done by invoking the `Notify` operation
provided by the `String_Observers` package:

```Ada
String_Observer.Notify (L, "Hello");
```

## Timer Management
The <tt>Util.Events.Timers</tt> package provides a timer list that allows to have
operations called on regular basis when a deadline has expired.  It is very close to
the <tt>Ada.Real_Time.Timing_Events</tt> package but it provides more flexibility
by allowing to have several timer lists that run independently.  Unlike the GNAT
implementation, this timer list management does not use tasks at all.  The timer list
can therefore be used in a mono-task environment by the main process task.  Furthermore
you can control your own task priority by having your own task that uses the timer list.

The timer list is created by an instance of <tt>Timer_List</tt>:

```Ada
Manager : Util.Events.Timers.Timer_List;
```

The timer list is protected against concurrent accesses so that timing events can be
setup by a task but the timer handler is executed by another task.

### Timer Creation
A timer handler is defined by implementing the <tt>Timer</tt> interface with the
<tt>Time_Handler</tt> procedure.  A typical timer handler could be declared as follows:

```Ada
type Timeout is new Timer with null record;
overriding procedure Time_Handler (T : in out Timeout);
My_Timeout : aliased Timeout;

```

The timer instance is represented by the <tt>Timer_Ref</tt> type that describes the handler
to be called as well as the deadline time.  The timer instance is initialized as follows:

```Ada
T : Util.Events.Timers.Timer_Ref;
Manager.Set_Timer (T, My_Timeout'Access, Ada.Real_Time.Seconds (1));
```

### Timer Main Loop
Because the implementation does not impose any execution model, the timer management must
be called regularly by some application's main loop.  The <tt>Process</tt> procedure
executes the timer handler that have elapsed and it returns the deadline to wait for the
next timer to execute.

```Ada
Deadline : Ada.Real_Time.Time;
loop
   ...
   Manager.Process (Deadline);
   delay until Deadline;
end loop;
```

## Executors
The `Util.Executors` generic package defines a queue of work that will be executed
by one or several tasks.  The `Work_Type` describes the type of the work and the
`Execute` procedure will be called by the task to execute the work.  After instantiation
of the package, an instance of the `Executor_Manager` is created with a number of desired
tasks.  The tasks are then started by calling the `Start` procedure.

A work object is added to the executor's queue by using the `Execute` procedure.
The work object is added in a concurrent fifo queue.  One of the task managed by the
executor manager will pick the work object and run it.


