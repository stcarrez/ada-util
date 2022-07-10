-----------------------------------------------------------------------
--  util-events-timers -- Timer list management
--  Copyright (C) 2017, 2022 Stephane Carrez
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
with Ada.Unchecked_Deallocation;
with Util.Log.Loggers;
package body Util.Events.Timers is

   use type Ada.Real_Time.Time;

   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Util.Events.Timers");

   procedure Free is
     new Ada.Unchecked_Deallocation (Object => Timer_Node,
                                     Name   => Timer_Node_Access);

   --  -----------------------
   --  Repeat the timer.
   --  -----------------------
   procedure Repeat (Event   : in out Timer_Ref;
                     In_Time : in Ada.Real_Time.Time_Span) is
      Timer : constant Timer_Node_Access := Event.Value;
   begin
      if Timer /= null and then Timer.List /= null then
         Timer.List.Add (Timer, Timer.Deadline + In_Time);
      end if;
   end Repeat;

   --  -----------------------
   --  Cancel the timer.
   --  -----------------------
   procedure Cancel (Event : in out Timer_Ref) is
   begin
      if Event.Value /= null and then Event.Value.List /= null then
         Event.Value.List.all.Cancel (Event.Value);
         Event.Value.List := null;
      end if;
   end Cancel;

   --  -----------------------
   --  Check if the timer is ready to be executed.
   --  -----------------------
   function Is_Scheduled (Event : in Timer_Ref) return Boolean is
   begin
      return Event.Value /= null and then Event.Value.List /= null;
   end Is_Scheduled;

   --  -----------------------
   --  Returns the deadline time for the timer execution.
   --  Returns Time'Last if the timer is not scheduled.
   --  -----------------------
   function Time_Of_Event (Event : in Timer_Ref) return Ada.Real_Time.Time is
   begin
      return (if Event.Value /= null then Event.Value.Deadline else Ada.Real_Time.Time_Last);
   end Time_Of_Event;

   --  -----------------------
   --  Set a timer to be called at the given time.
   --  -----------------------
   procedure Set_Timer (List    : in out Timer_List;
                        Handler : in Timer_Access;
                        Event   : in out Timer_Ref'Class;
                        At_Time : in Ada.Real_Time.Time) is
      Timer : Timer_Node_Access := Event.Value;
   begin
      if Timer = null then
         Event.Value := new Timer_Node;
         Timer := Event.Value;
      end if;
      Timer.Handler := Handler;

      --  Cancel the timer if it is part of another timer manager.
      if Timer.List /= null and then Timer.List /= List.Manager'Unchecked_Access then
         Timer.List.Cancel (Timer);
      end if;

      --  Update the timer.
      Timer.List := List.Manager'Unchecked_Access;
      List.Manager.Add (Timer, At_Time);
   end Set_Timer;

   --  -----------------------
   --  Set a timer to be called after the given time span.
   --  -----------------------
   procedure Set_Timer (List    : in out Timer_List;
                        Handler : in Timer_Access;
                        Event   : in out Timer_Ref'Class;
                        In_Time : in Ada.Real_Time.Time_Span) is
   begin
      List.Set_Timer (Handler, Event, Ada.Real_Time.Clock + In_Time);
   end Set_Timer;

   --  -----------------------
   --  Process the timer handlers that have passed the deadline and return the next
   --  deadline.  The <tt>Max_Count</tt> parameter allows to limit the number of timer handlers
   --  that are called by operation.  The default is not limited.
   --  -----------------------
   procedure Process (List      : in out Timer_List;
                      Timeout   : out Ada.Real_Time.Time;
                      Max_Count : in Natural := Natural'Last) is
      Timer : Timer_Ref;
      Now   : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      for Count in 1 .. Max_Count loop
         List.Manager.Find_Next (Now, Timeout, Timer);
         exit when Timer.Value = null;
         begin
            Timer.Value.Handler.Time_Handler (Timer);

         exception
            when E : others =>
               Timer_List'Class (List).Error (Timer.Value.Handler, E);

         end;
         Timer.Finalize;
      end loop;
   end Process;

   --  -----------------------
   --  Procedure called when a timer handler raises an exception.
   --  The default operation reports an error in the logs.  This procedure can be
   --  overridden to implement specific error handling.
   --  -----------------------
   procedure Error (List : in out Timer_List;
                    Handler : in Timer_Access;
                    E       : in Ada.Exceptions.Exception_Occurrence) is
      pragma Unreferenced (List, Handler);
   begin
      Log.Error ("Timer handler raised an exception", E, True);
   end Error;

   overriding
   procedure Adjust (Object : in out Timer_Ref) is
   begin
      if Object.Value /= null then
         Util.Concurrent.Counters.Increment (Object.Value.Counter);
      end if;
   end Adjust;

   overriding
   procedure Finalize (Object : in out Timer_Ref) is
      Is_Zero : Boolean;
   begin
      if Object.Value /= null then
         Util.Concurrent.Counters.Decrement (Object.Value.Counter, Is_Zero);
         if Is_Zero then
            Free (Object.Value);
         else
            Object.Value := null;
         end if;
      end if;
   end Finalize;

   protected body Timer_Manager is

      procedure Remove (Timer : in Timer_Node_Access) is
      begin
         if List = Timer then
            List := Timer.Next;
            Timer.Prev := null;
            if List /= null then
               List.Prev := null;
            end if;
         elsif Timer.Prev /= null then
            Timer.Prev.Next := Timer.Next;
            Timer.Next.Prev := Timer.Prev;
         else
            return;
         end if;
         Timer.Next := null;
         Timer.Prev := null;
         Timer.List := null;
      end Remove;

      --  -----------------------
      --  Add a timer.
      --  -----------------------
      procedure Add (Timer    : in Timer_Node_Access;
                     Deadline : in Ada.Real_Time.Time) is
         Current : Timer_Node_Access := List;
         Prev    : Timer_Node_Access;
      begin
         Util.Concurrent.Counters.Increment (Timer.Counter);
         if Timer.List /= null then
            Remove (Timer);
         end if;
         Timer.Deadline := Deadline;
         while Current /= null loop
            if Current.Deadline > Deadline then
               if Prev = null then
                  List := Timer;
               else
                  Prev.Next := Timer;
               end if;
               Timer.Next := Current;
               Current.Prev := Timer;
               return;
            end if;
            Prev := Current;
            Current := Current.Next;
         end loop;
         if Prev = null then
            List := Timer;
            Timer.Prev := null;
         else
            Prev.Next := Timer;
            Timer.Prev := Prev;
         end if;
         Timer.Next := null;
      end Add;

      --  -----------------------
      --  Cancel a timer.
      --  -----------------------
      procedure Cancel (Timer : in out Timer_Node_Access) is
         Is_Zero : Boolean;
      begin
         if Timer.List = null then
            return;
         end if;
         Remove (Timer);
         Util.Concurrent.Counters.Decrement (Timer.Counter, Is_Zero);
         if Is_Zero then
            Free (Timer);
         end if;
      end Cancel;

      --  -----------------------
      --  Find the next timer to be executed before the given time or return the next deadline.
      --  -----------------------
      procedure Find_Next (Before   : in Ada.Real_Time.Time;
                           Deadline : out Ada.Real_Time.Time;
                           Timer    : in out Timer_Ref) is
      begin
         if List = null then
            Deadline := Ada.Real_Time.Time_Last;
         elsif List.Deadline < Before then
            Timer.Value := List;
            List := List.Next;
            if List /= null then
               List.Prev := null;
               Deadline := List.Deadline;
            else
               Deadline := Ada.Real_Time.Time_Last;
            end if;
         else
            Deadline := List.Deadline;
         end if;
      end Find_Next;

   end Timer_Manager;

   overriding
   procedure Finalize (Object : in out Timer_List) is
      Timer    : Timer_Ref;
      Timeout  : Ada.Real_Time.Time;
   begin
      loop
         Object.Manager.Find_Next (Ada.Real_Time.Time_Last, Timeout, Timer);
         exit when Timer.Value = null;
         Timer.Finalize;
      end loop;
   end Finalize;

end Util.Events.Timers;
