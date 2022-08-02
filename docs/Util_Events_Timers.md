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

