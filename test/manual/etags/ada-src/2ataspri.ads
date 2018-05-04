------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                S Y S T E M . T A S K _ P R I M I T I V E S               --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.1 $                             --
--                                                                          --
--   Copyright (C) 1991,1992,1993,1994,1995,1996 Florida State University   --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C;
--  Used for Size_t;

with Interfaces.C.Pthreads;
--  Used for, size_t,
--            pthread_mutex_t,
--            pthread_cond_t,
--            pthread_t

with Interfaces.C.POSIX_RTE;
--  Used for, Signal,
--            siginfo_ptr,

with System.Task_Clock;
--  Used for, Stimespec

with Unchecked_Conversion;

pragma Elaborate_All (Interfaces.C.Pthreads);

with System.Task_Info;
package System.Task_Primitives is

   --  Low level Task size and state definition

   type LL_Task_Procedure_Access is access procedure (Arg : System.Address);

   type Pre_Call_State is new System.Address;

   type Task_Storage_Size is new Interfaces.C.size_t;

   type Machine_Exceptions is new Interfaces.C.POSIX_RTE.Signal;

   type Error_Information is new Interfaces.C.POSIX_RTE.siginfo_ptr;

   type Lock is private;
   type Condition_Variable is private;

   --  The above types should both be limited. They are not due to a hack in
   --  ATCB allocation which allocates a block of the correct size and then
   --  assigns an initialized ATCB to it. This won't work with limited types.
   --  When allocation is done with new, these can become limited once again.
   --  ???

   type Task_Control_Block is record
      LL_Entry_Point : LL_Task_Procedure_Access;
      LL_Arg         : System.Address;
      Thread         : aliased Interfaces.C.Pthreads.pthread_t;
      Stack_Size     : Task_Storage_Size;
      Stack_Limit    : System.Address;
   end record;

   type TCB_Ptr is access all Task_Control_Block;

   --  Task ATCB related and variables.

   function Address_To_TCB_Ptr is new
     Unchecked_Conversion (System.Address, TCB_Ptr);

   procedure Initialize_LL_Tasks (T : TCB_Ptr);
   --  Initialize GNULLI. T points to the Task Control Block that should
   --  be initialized for use by the environment task.

   function Self return TCB_Ptr;
   --  Return a pointer to the Task Control Block of the calling task.

   procedure Initialize_Lock (Prio : System.Any_Priority; L : in out Lock);
   --  Initialize a lock object. Prio is the ceiling priority associated
   --  with the lock.

   procedure Finalize_Lock (L : in out Lock);
   --  Finalize a lock object, freeing any resources allocated by the
   --  corresponding Initialize_Lock.

   procedure Write_Lock (L : in out Lock; Ceiling_Violation : out Boolean);
   pragma Inline (Write_Lock);
   --  Lock a lock object for write access to a critical section. After
   --  this operation returns, the calling task owns the lock, and
   --  no other Write_Lock or Read_Lock operation on the same object will
   --  return the owner executes an Unlock operation on the same object.

   procedure Read_Lock (L : in out Lock; Ceiling_Violation : out Boolean);
   pragma Inline (Read_Lock);
   --  Lock a lock object for read access to a critical section. After
   --  this operation returns, the calling task owns the lock, and
   --  no other Write_Lock operation on the same object will return until
   --  the owner(s) execute Unlock operation(s) on the same object.
   --  A Read_Lock to an owned lock object may return while the lock is
   --  still owned, though an implementation may also implement
   --  Read_Lock to have the same semantics.

   procedure Unlock (L : in out Lock);
   pragma Inline (Unlock);
   --  Unlock a locked lock object. The results are undefined if the
   --  calling task does not own the lock. Lock/Unlock operations must
   --  be nested, that is, the argument to Unlock must be the object
   --  most recently locked.

   procedure Initialize_Cond (Cond : in out Condition_Variable);
   --  Initialize a condition variable object.

   procedure Finalize_Cond (Cond : in out Condition_Variable);
   --  Finalize a condition variable object, recovering any resources
   --  allocated for it by Initialize_Cond.

   procedure Cond_Wait (Cond : in out Condition_Variable; L : in out Lock);
   pragma Inline (Cond_Wait);
   --  Wait on a condition variable. The mutex object L is unlocked
   --  atomically, such that another task that is able to lock the mutex
   --  can be assured that the wait has actually commenced, and that
   --  a Cond_Signal operation will cause the waiting task to become
   --  eligible for execution once again. Before Cond_Wait returns,
   --  the waiting task will again lock the mutex. The waiting task may become
   --  eligible for execution at any time, but will become eligible for
   --  execution when a Cond_Signal operation is performed on the
   --  same condition variable object. The effect of more than one
   --  task waiting on the same condition variable is unspecified.

   procedure Cond_Timed_Wait
     (Cond      : in out Condition_Variable;
      L         : in out Lock; Abs_Time : System.Task_Clock.Stimespec;
      Timed_Out : out Boolean);
   pragma Inline (Cond_Timed_Wait);
   --  Wait on a condition variable, as for Cond_Wait, above. In addition,
   --  the waiting task will become eligible for execution again
   --  when the absolute time specified by Timed_Out arrives.

   procedure Cond_Signal (Cond : in out Condition_Variable);
   pragma Inline (Cond_Signal);
   --  Wake up a task waiting on the condition variable object specified
   --  by Cond, making it eligible for execution once again.

   procedure Set_Priority (T : TCB_Ptr; Prio : System.Any_Priority);
   pragma Inline (Set_Priority);
   --  Set the priority of the task specified by T to P.

   procedure Set_Own_Priority (Prio : System.Any_Priority);
   pragma Inline (Set_Own_Priority);
   --  Set the priority of the calling task to P.

   function Get_Priority (T : TCB_Ptr) return System.Any_Priority;
   pragma Inline (Get_Priority);
   --  Return the priority of the task specified by T.

   function Get_Own_Priority return System.Any_Priority;
   pragma Inline (Get_Own_Priority);
   --  Return the priority of the calling task.

   procedure Create_LL_Task
     (Priority       : System.Any_Priority;
      Stack_Size     : Task_Storage_Size;
      Task_Info      : System.Task_Info.Task_Info_Type;
      LL_Entry_Point : LL_Task_Procedure_Access;
      Arg            : System.Address;
      T              : TCB_Ptr);
   --  Create a new low-level task with priority Priority. A new thread
   --  of control is created with a stack size of at least Stack_Size,
   --  and the procedure LL_Entry_Point is called with the argument Arg
   --  from this new thread of control. The Task Control Block pointed
   --  to by T is initialized to refer to this new task.

   procedure Exit_LL_Task;
   --  Exit a low-level task. The resources allocated for the task
   --  by Create_LL_Task are recovered. The task no longer executes, and
   --  the effects of further operations on task are unspecified.

   procedure Abort_Task (T : TCB_Ptr);
   --  Abort the task specified by T (the target task). This causes
   --  the target task to asynchronously execute the handler procedure
   --  installed by the target task using Install_Abort_Handler. The
   --  effect of this operation is unspecified if there is no abort
   --  handler procedure for the target task.

   procedure Test_Abort;
   --  ??? Obsolete?  This is intended to allow implementation of
   --      abortion and ATC in the absence of an asynchronous Abort_Task,
   --      but I think that we decided that GNARL can handle this on
   --      its own by making sure that there is an Undefer_Abortion at
   --      every abortion synchronization point.

   type Abort_Handler_Pointer is access procedure (Context : Pre_Call_State);

   procedure Install_Abort_Handler (Handler : Abort_Handler_Pointer);
   --  Install an abort handler procedure. This procedure is called
   --  asynchronously by the calling task whenever a call to Abort_Task
   --  specifies the calling task as the target. If the abort handler
   --  procedure is asynchronously executed during a GNULLI operation
   --  and then calls some other GNULLI operation, the effect is unspecified.

   procedure Install_Error_Handler (Handler : System.Address);
   --  Install an error handler for the calling task. The handler will
   --  be called synchronously if an error is encountered during the
   --  execution of the calling task.

   procedure LL_Assert (B : Boolean; M : String);
   --  If B is False, print the string M to the console and halt the
   --  program.

   Task_Wrapper_Frame : constant Integer := 72;
   --  This is the size of the frame for the Pthread_Wrapper procedure.

   type Proc is access procedure (Addr : System.Address);


   --  Test and Set support
   type TAS_Cell is private;
   --  On some systems we can not assume that an arbitrary memory location
   --  can be used in an atomic test and set instruction (e.g. on some
   --  multiprocessor machines, only memory regions are cache interlocked).
   --  TAS_Cell is private to facilitate adaption to a variety of
   --  implementations.

   procedure Initialize_TAS_Cell (Cell :    out TAS_Cell);
   pragma Inline (Initialize_TAS_Cell);
   --  Initialize a Test And Set Cell.  On some targets this will allocate
   --  a system-level lock object from a special pool.  For most systems,
   --  this is a nop.

   procedure Finalize_TAS_Cell   (Cell : in out TAS_Cell);
   pragma Inline (Finalize_TAS_Cell);
   --  Finalize a Test and Set cell, freeing any resources allocated by the
   --  corresponding Initialize_TAS_Cell.

   procedure Clear        (Cell : in out TAS_Cell);
   pragma Inline (Clear);
   --  Set the state of the named TAS_Cell such that a subsequent call to
   --  Is_Set will return False.  This operation must be atomic with
   --  respect to the Is_Set and Test_And_Set operations for the same
   --  cell.

   procedure Test_And_Set (Cell : in out TAS_Cell; Result : out Boolean);
   pragma Inline (Test_And_Set);
   --  Modify the state of the named TAS_Cell such that a subsequent call
   --  to Is_Set will return True.  Result is set to True if Is_Set
   --  was False prior to the call, False otherwise.  This operation must
   --  be atomic with respect to the Clear and Is_Set operations for the
   --  same cell.

   function  Is_Set       (Cell : in     TAS_Cell) return Boolean;
   pragma Inline (Is_Set);
   --  Returns the current value of the named TAS_Cell.  This operation
   --  must be atomic with respect to the Clear and Test_And_Set operations
   --  for the same cell.

private

   type Lock is
      record
         mutex : aliased Interfaces.C.Pthreads.pthread_mutex_t;
      end record;

   type Condition_Variable is
      record
         CV : aliased Interfaces.C.Pthreads.pthread_cond_t;
      end record;

   type TAS_Cell is
      record
         Value : aliased Interfaces.C.unsigned := 0;
      end record;

end System.Task_Primitives;
