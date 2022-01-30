-----------------------------------------------------------------------
--  util-events-timers -- Timer list management
--  Copyright (C) 2017 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Ada.Real_Time;
with Ada.Exceptions;
private with Ada.Finalization;
private with Util.Concurrent.Counters;

--  == Timer Management ==
--  The <tt>Util.Events.Timers</tt> package provides a timer list that allows to have
--  operations called on regular basis when a deadline has expired.  It is very close to
--  the <tt>Ada.Real_Time.Timing_Events</tt> package but it provides more flexibility
--  by allowing to have several timer lists that run independently.  Unlike the GNAT
--  implementation, this timer list management does not use tasks at all.  The timer list
--  can therefore be used in a mono-task environment by the main process task.  Furthermore
--  you can control your own task priority by having your own task that uses the timer list.
--
--  The timer list is created by an instance of <tt>Timer_List</tt>:
--
--    Manager : Util.Events.Timers.Timer_List;
--
--  The timer list is protected against concurrent accesses so that timing events can be
--  setup by a task but the timer handler is executed by another task.
--
--  === Timer Creation ===
--  A timer handler is defined by implementing the <tt>Timer</tt> interface with the
--  <tt>Time_Handler</tt> procedure.  A typical timer handler could be declared as follows:
--
--    type Timeout is new Timer with null record;
--    overriding procedure Time_Handler (T : in out Timeout);
--
--    My_Timeout : aliased Timeout;
--
--  The timer instance is represented by the <tt>Timer_Ref</tt> type that describes the handler
--  to be called as well as the deadline time.  The timer instance is initialized as follows:
--
--    T : Util.Events.Timers.Timer_Ref;
--    Manager.Set_Timer (T, My_Timeout'Access, Ada.Real_Time.Seconds (1));
--
--  === Timer Main Loop ===
--  Because the implementation does not impose any execution model, the timer management must
--  be called regularly by some application's main loop.  The <tt>Process</tt> procedure
--  executes the timer handler that have elapsed and it returns the deadline to wait for the
--  next timer to execute.
--
--    Deadline : Ada.Real_Time.Time;
--    loop
--       ...
--       Manager.Process (Deadline);
--       delay until Deadline;
--    end loop;
--
package Util.Events.Timers is

   type Timer_Ref is tagged private;

   --  The timer interface that must be implemented by applications.
   type Timer is limited interface;
   type Timer_Access is access all Timer'Class;

   --  The timer handler executed when the timer deadline has passed.
   procedure Time_Handler (T     : in out Timer;
                           Event : in out Timer_Ref'Class) is abstract;

   --  Repeat the timer.
   procedure Repeat (Event   : in out Timer_Ref;
                     In_Time : in Ada.Real_Time.Time_Span);

   --  Cancel the timer.
   procedure Cancel (Event : in out Timer_Ref);

   --  Check if the timer is ready to be executed.
   function Is_Scheduled (Event : in Timer_Ref) return Boolean;

   --  Returns the deadline time for the timer execution.
   --  Returns Time'Last if the timer is not scheduled.
   function Time_Of_Event (Event : in Timer_Ref) return Ada.Real_Time.Time;

   type Timer_List is tagged limited private;

   --  Set a timer to be called at the given time.
   procedure Set_Timer (List    : in out Timer_List;
                        Handler : in Timer_Access;
                        Event   : in out Timer_Ref'Class;
                        At_Time : in Ada.Real_Time.Time);

   --  Set a timer to be called after the given time span.
   procedure Set_Timer (List    : in out Timer_List;
                        Handler : in Timer_Access;
                        Event   : in out Timer_Ref'Class;
                        In_Time : in Ada.Real_Time.Time_Span);

   --  Process the timer handlers that have passed the deadline and return the next
   --  deadline.  The <tt>Max_Count</tt> parameter allows to limit the number of timer handlers
   --  that are called by operation.  The default is not limited.
   procedure Process (List      : in out Timer_List;
                      Timeout   : out Ada.Real_Time.Time;
                      Max_Count : in Natural := Natural'Last);

   --  Procedure called when a timer handler raises an exception.
   --  The default operation reports an error in the logs.  This procedure can be
   --  overridden to implement specific error handling.
   procedure Error (List : in out Timer_List;
                    Handler : in Timer_Access;
                    E       : in Ada.Exceptions.Exception_Occurrence);

private

   type Timer_Manager;
   type Timer_Manager_Access is access all Timer_Manager;

   type Timer_Node;
   type Timer_Node_Access is access all Timer_Node;

   type Timer_Ref is new Ada.Finalization.Controlled with record
      Value : Timer_Node_Access;
   end record;

   overriding
   procedure Adjust (Object : in out Timer_Ref);

   overriding
   procedure Finalize (Object : in out Timer_Ref);

   type Timer_Node is limited record
      Next     : Timer_Node_Access;
      Prev     : Timer_Node_Access;
      List     : Timer_Manager_Access;
      Counter  : Util.Concurrent.Counters.Counter := Util.Concurrent.Counters.ONE;
      Handler  : Timer_Access;
      Deadline : Ada.Real_Time.Time;
   end record;

   protected type Timer_Manager is

      --  Add a timer.
      procedure Add (Timer    : in Timer_Node_Access;
                     Deadline : in Ada.Real_Time.Time);

      --  Cancel a timer.
      procedure Cancel (Timer : in out Timer_Node_Access);

      --  Find the next timer to be executed before the given time or return the next deadline.
      procedure Find_Next (Before   : in Ada.Real_Time.Time;
                           Deadline : out Ada.Real_Time.Time;
                           Timer    : in out Timer_Ref);

   private
      List : Timer_Node_Access;
   end Timer_Manager;

   type Timer_List is new Ada.Finalization.Limited_Controlled with record
      Manager : aliased Timer_Manager;
   end record;

   overriding
   procedure Finalize (Object : in out Timer_List);

end Util.Events.Timers;
