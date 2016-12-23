------------------------------------------------------------------------------
--                                                                          --
--                 GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                S Y S T E M . T A S K _ P R I M I T I V E S               --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.1 $                             --
--                                                                          --
--     Copyright (C) 1991,1992,1993,1994,1996 Florida State University      --
--                                                                          --
-- GNARL is free software; you can redistribute it  and/or modify it  under --
-- terms  of  the  GNU  Library General Public License  as published by the --
-- Free Software  Foundation;  either version 2, or (at  your  option)  any --
-- later  version.  GNARL is distributed  in the hope that  it will be use- --
-- ful, but but WITHOUT ANY WARRANTY;  without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Gen- --
-- eral Library Public License  for more details.  You should have received --
-- a  copy of the GNU Library General Public License along with GNARL;  see --
-- file COPYING.LIB.  If not,  write to the  Free Software Foundation,  675 --
-- Mass Ave, Cambridge, MA 02139, USA.                                      --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.IO;
with Interfaces.C.POSIX_timers;

with Interfaces.C.POSIX_Error;
use  Interfaces.C.POSIX_Error;

with Interfaces.C.POSIX_RTE;
use  Interfaces.C.POSIX_RTE;

with Interfaces.C.Pthreads;
use  Interfaces.C.Pthreads;

with Interfaces.C;
use  Interfaces.C;

with System.Tasking;
use  System.Tasking;

with System.Storage_Elements;
use  System.Storage_Elements;

with System.Compiler_Exceptions;
use  System.Compiler_Exceptions;

with System.Task_Specific_Data;
use  System.Task_Specific_Data;

with System.Secondary_Stack;
use  System.Secondary_Stack;

with System.Tasking_Soft_Links;

with System.Task_Clock;
use  System.Task_Clock;

with Unchecked_Conversion;
with Interfaces.C.System_Constants;

package body System.Task_Primitives is

   use Interfaces.C.Pthreads;
   use Interfaces.C.System_Constants;

   package RTE renames Interfaces.C.POSIX_RTE;
   package TSL renames System.Tasking_Soft_Links;

   Test_And_Set_Mutex : Lock;

   Abort_Signal : constant := 6;

   Abort_Handler : Abort_Handler_Pointer;

   ATCB_Key : aliased pthread_key_t;

   Unblocked_Signal_Mask : aliased RTE.Signal_Set;
   --  The set of signals that should be unblocked in a task.
   --  This is in general the signals that can be generated synchronously,
   --  and which should therefore be converted into Ada exceptions.
   --  It also includes the Abort_Signal, to allow asynchronous abortion.

   function To_void_ptr is new
     Unchecked_Conversion (TCB_Ptr, void_ptr);

   function To_TCB_Ptr is new
     Unchecked_Conversion (void_ptr, TCB_Ptr);

   function pthread_mutexattr_setprotocol
     (attr : access pthread_attr_t; priority : integer) return int;
   pragma Import (C,
                  pthread_mutexattr_setprotocol,
                  "pthread_mutexattr_setprotocol",
                  "pthread_mutexattr_setprotocol");

   function pthread_mutexattr_setprio_ceiling
     (attr : access pthread_attr_t; priority : int) return int;
   pragma Import (C,
                  pthread_mutexattr_setprio_ceiling,
                  "pthread_mutexattr_setprio_ceiling",
                  "pthread_mutexattr_setprio_ceiling");

   pthread_mutexattr_default : pthread_mutexattr_t;
   pragma Import (C, pthread_mutexattr_default,
                  "pthread_mutexattr_default",
                  "pthread_mutexattr_default");

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Abort_Wrapper
     (signo   : Integer;
      info    : RTE.siginfo_ptr;
      context : System.Address);
   --  This is a signal handler procedure which calls the user-specified
   --  abort handler procedure.

   procedure LL_Wrapper (T : TCB_Ptr);
   --  A wrapper procedure that is called from a new low-level task.
   --  It performs initializations for the new task and calls the
   --  user-specified startup procedure.

   -------------------------
   -- Initialize_LL_Tasks --
   -------------------------

   procedure Initialize_LL_Tasks (T : TCB_Ptr) is
      Result : int;
   begin
      T.LL_Entry_Point := null;
      T.Thread := pthread_self;

      Result := pthread_key_create (ATCB_Key'Access, null);

      if Result = FUNC_ERR then
         raise Storage_Error;               --  Insufficient resources.
      end if;

      T.Thread := pthread_self;

      Result := pthread_setspecific (ATCB_Key, To_void_ptr (T));

      if Result = FUNC_ERR then
         GNAT.IO.Put_Line ("Get specific failed");
         raise Storage_Error;               --  Insufficient resources.
      end if;
      pragma Assert (Result /= FUNC_ERR,
         "GNULLI failure---pthread_setspecific");

   end Initialize_LL_Tasks;

   ----------
   -- Self --
   ----------

   function Self return TCB_Ptr is
      Temp   : aliased void_ptr;
      Result : int;
   begin
      Result := pthread_getspecific (ATCB_Key, Temp'Access);
      pragma Assert (Result /= FUNC_ERR,
          "GNULLI failure---pthread_getspecific");
      return To_TCB_Ptr (Temp);
   end Self;

   ---------------------
   -- Initialize_Lock --
   ---------------------

   procedure Initialize_Lock
     (Prio : System.Any_Priority;
      L    : in out Lock)
   is

      Attributes : aliased pthread_mutexattr_t;
      Result     : int;
      MUTEX_NONRECURSIVE_NP : constant := 2;

   begin
      Result := pthread_mutexattr_init (Attributes'Access);
      if Result = FUNC_ERR then
         raise STORAGE_ERROR;  --  should be ENOMEM
      end if;

      Result := pthread_mutexattr_setkind
        (Attributes'Access, MUTEX_NONRECURSIVE_NP);
      if Result = FUNC_ERR then
         raise STORAGE_ERROR;  --  should be ENOMEM
      end if;

      Result := pthread_mutex_init (L.mutex'Access, Attributes);

      if Result = FUNC_ERR then
         Result := pthread_mutexattr_destroy (Attributes'Access);
         raise STORAGE_ERROR;  --  should be ENOMEM ???
      end if;

      Result := pthread_mutexattr_destroy (Attributes'Access);

   end Initialize_Lock;

   -------------------
   -- Finalize_Lock --
   -------------------

   procedure Finalize_Lock (L : in out Lock) is
      Result : int;
   begin
      Result := pthread_mutex_destroy (L.mutex'Access);
      pragma Assert
         (Result /= FUNC_ERR, "GNULLI failure---pthread_mutex_destroy");
   end Finalize_Lock;

   ----------------
   -- Write_Lock --
   ----------------

   --
   --  The current pthreads implementation does not check for Ceiling
   --  violations.
   --
   procedure Write_Lock (L : in out Lock; Ceiling_Violation : out Boolean) is
      Result : int;
   begin
      Ceiling_Violation := False;
      Result := pthread_mutex_lock (L.mutex'Access);
      pragma Assert
        (Result /= FUNC_ERR, "GNULLI FUNC_ERR---pthread_mutex_lock");
   end Write_Lock;

   ---------------
   -- Read_Lock --
   ---------------

   procedure Read_Lock (L : in out Lock; Ceiling_Violation : out Boolean)
      renames Write_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (L : in out Lock) is
      Result : int;
   begin
      Result := pthread_mutex_unlock (L.mutex'Access);
      pragma Assert
         (Result /= FUNC_ERR, "GNULLI FUNC_ERR---pthread_mutex_unlock");
   end Unlock;

   ---------------------
   -- Initialize_Cond --
   ---------------------

   procedure Initialize_Cond (Cond : in out Condition_Variable) is
      Attributes : aliased Pthreads.pthread_condattr_t;
      Result     : int;
   begin
      Result := pthread_condattr_init (Attributes'Access);

      if Result = FUNC_ERR then
         raise STORAGE_ERROR;  --  should be ENOMEM ???
      end if;

      --  Result := pthread_cond_init (Cond.CV'Access, Attributes'Access);
      Result := pthread_cond_init (Cond.CV'Access, Attributes);


      if Result = FUNC_ERR then
         raise STORAGE_ERROR;  --  should be ENOMEM  ???
      end if;

      Result := pthread_condattr_destroy (Attributes'Access);
      pragma Assert
         (Result /= FUNC_ERR, "GNULLI FUNC_ERR---pthread_condattr_destroy");

   end Initialize_Cond;

   -------------------
   -- Finalize_Cond --
   -------------------

   procedure Finalize_Cond (Cond : in out Condition_Variable) is
      Result : int;

   begin
      Result := pthread_cond_destroy (Cond.CV'Access);
      pragma Assert
         (Result /= FUNC_ERR, "GNULLI failure---pthread_cond_destroy");
   end Finalize_Cond;


   ---------------
   -- Cond_Wait --
   ---------------

   procedure Cond_Wait (Cond : in out Condition_Variable; L : in out Lock) is
      Result : int;
   begin
      Result := pthread_cond_wait (Cond.CV'Access, L.mutex'Access);
      pragma Assert
        (Result /= FUNC_ERR, "GNULLI failure---pthread_cond_wait");
   end Cond_Wait;

   ---------------------
   -- Cond_Timed_Wait --
   ---------------------

   procedure Cond_Timed_Wait
     (Cond      : in out Condition_Variable;
      L         : in out Lock;
      Abs_Time  : System.Task_Clock.Stimespec;
      Timed_Out : out Boolean) is

      Result : int;
      TV     : aliased timespec;

      use POSIX_Error;

   begin
      Timed_Out := False;  --  Assume success until we know otherwise

      TV.tv_sec := int (Interfaces.C.POSIX_timers.time_t
        (Task_Clock.Stimespec_Seconds (Abs_Time)));

      TV.tv_nsec := long (Interfaces.C.POSIX_timers.Nanoseconds
        (Task_Clock.Stimespec_NSeconds (Abs_Time)));

      Result := pthread_cond_timedwait
        (Cond.CV'Access, L.mutex'Access, TV'Access);
      pragma Assert
         (Result /= FUNC_ERR, "GNULLI failure---pthread_cond_timedwait");

   end Cond_Timed_Wait;

   -----------------
   -- Cond_Signal --
   -----------------

   procedure Cond_Signal (Cond : in out Condition_Variable) is
      Result : int;
   begin
      Result :=  pthread_cond_signal (Cond.CV'Access);
      pragma Assert
         (Result /= FUNC_ERR, "GNULLI failure---pthread_cond_signal");
   end Cond_Signal;

   ------------------
   -- Set_Priority --
   ------------------

   procedure Set_Priority
     (T : TCB_Ptr;
      Prio : System.Any_Priority) is

      Result : int;
      Thread : Pthreads.pthread_t renames T.Thread;

   begin
      Result := pthread_setprio (Thread, int (Prio));
      pragma Assert
         (Result /= FUNC_ERR, "GNULLI failure---pthread_setprio");
   end Set_Priority;

   ----------------------
   -- Set_Own_Priority --
   ----------------------

   procedure Set_Own_Priority (Prio : System.Any_Priority) is
   begin
      null;
      --  ENOSYS Result :=
      --     pthread_setprio (pthread_self, int (Prio));
      --  pragma Assert
      --     (Result /= FUNC_ERR, "GNULLI failure---pthread_setprio");
   end Set_Own_Priority;

   ------------------
   -- Get_Priority --
   ------------------

   function Get_Priority (T : TCB_Ptr) return System.Any_Priority is
      Priority : aliased int := 0;
   begin
      --  ENOSYS Result := pthread_getprio (T.Thread, Priority'Access);
      --  pragma Assert
      --     (Result /= FUNC_ERR, "GNULLI failure---pthread_getprio");
      return System.Priority (Priority);
   end Get_Priority;

   -----------------------
   --  Get_Own_Priority --
   -----------------------

   function Get_Own_Priority return System.Any_Priority is
      Result : int;
      Priority : aliased int := 0;
   begin
      Result := pthread_getprio (pthread_self, Priority'Access);
      pragma Assert
         (Result /= FUNC_ERR, "GNULLI failure---pthread_getprio");
      return System.Priority (Priority);
   end Get_Own_Priority;

   --------------------
   -- Create_LL_Task --
   --------------------

   procedure Create_LL_Task
     (Priority       : System.Any_Priority;
      Stack_Size     : Task_Storage_Size;
      Task_Info      : System.Task_Info.Task_Info_Type;
      LL_Entry_Point : LL_Task_Procedure_Access;
      Arg            : System.Address;
      T              : TCB_Ptr) is

      use Pthreads;

      Attributes : aliased pthread_attr_t;
      Result     : int;
      L_Priority : System.Any_Priority := Priority;

      function To_Start_Addr is new
        Unchecked_Conversion (System.Address, start_addr);

   begin
      T.LL_Entry_Point := LL_Entry_Point;
      T.LL_Arg := Arg;
      T.Stack_Size := Stack_Size;

      Result := pthread_attr_init (Attributes'Access);
      pragma Assert (Result /= FUNC_ERR, "GNULLI failure---pthread_attr_init");

--      Result := pthread_attr_setdetachstate (Attributes'Access, 1);
--      pragma Assert
--        (Result /= FUNC_ERR, "GNULLI failure---pthread_setdetachstate");

      Result := pthread_attr_setstacksize
        (Attributes'Access, size_t (Stack_Size));
      pragma Assert
        (Result /= FUNC_ERR, "GNULLI failure---pthread_setstacksize");

      Result := pthread_attr_setinheritsched
        (Attributes'Access, PTHREAD_DEFAULT_SCHED);
      pragma Assert
        (Result /= FUNC_ERR, "GNULLI failure---pthread_setinheritsched");

      Result := pthread_attr_setsched
        (Attributes'Access, SCHED_FIFO);
      pragma Assert
        (Result /= FUNC_ERR, "GNULLI failure---pthread_setinheritsched");

      --  The following priority adjustment is a kludge to get around needing
      --  root privileges to run at higher than 18 for FIFO or 19 for OTHER.

      if (L_Priority > 18) then
         L_Priority := 18;
      elsif (L_Priority < 14) then
         L_Priority := 14;
      end if;

      Result := pthread_attr_setprio
        (Attributes'Access, int (L_Priority));
      pragma Assert
        (Result /= FUNC_ERR, "GNULLI failure---pthread_attr_setprio");

      Result := pthread_create
        (T.Thread'Access,
         Attributes,
         To_Start_Addr (LL_Wrapper'Address),
         T.all'Address);
      if Result = FUNC_ERR then
         GNAT.IO.Put_Line ("pthread create failed");
         raise Storage_Error;
      end if;
      pragma Assert (Result /= FUNC_ERR, "GNULLI failure---pthread_create");

      Result := pthread_attr_destroy (Attributes'Access);
      pragma Assert
        (Result /= FUNC_ERR, "GNULLI failure---pthread_attr_destroy");

   end Create_LL_Task;

   -----------------
   -- Exit_LL_Task --
   ------------------

   procedure Exit_LL_Task is
   begin
      pthread_exit (System.Null_Address);
   end Exit_LL_Task;

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : TCB_Ptr) is
      Result     : int;
   begin
--      Result := pthread_kill (T.Thread);
--      pragma Assert
--         (Result /= FUNC_ERR, "GNULLI failure---pthread_kill");
      null;
   end Abort_Task;

   ----------------
   -- Test_Abort --
   ----------------

   --  This procedure does nothing.  It is intended for systems without
   --  asynchronous abortion, where the runtime system would have to
   --  synchronously poll for pending abortions.  This should be done
   --  at least at every synchronization point.

   procedure Test_Abort is
   begin
      null;
   end Test_Abort;

   ---------------------------
   -- Install_Abort_Handler --
   ---------------------------

   procedure Install_Abort_Handler (Handler : Abort_Handler_Pointer) is
      act     : aliased RTE.struct_sigaction;
      old_act : aliased RTE.struct_sigaction;
      Result  : POSIX_Error.Return_Code;
      SA_SIGINFO : constant := 64;

      use type POSIX_Error.Return_Code;

   begin
      Abort_Handler := Handler;

      act.sa_flags := SA_SIGINFO;
      act.sa_handler := Abort_Wrapper'Address;
      RTE.sigemptyset (act.sa_mask'Access, Result);
      pragma Assert (Result /= FUNC_ERR, "GNULLI failure---sigemptyset");

      RTE.sigaction (Abort_Signal, act'Access, old_act'Access, Result);
      pragma Assert (Result /= FUNC_ERR, "GNULLI failure---sigaction");
   end Install_Abort_Handler;

   -------------------
   -- Abort_Wrapper --
   -------------------

   --  This is the handler called by the OS when an abort signal is
   --  received; it in turn calls the handler installed by the client.
   --  This procedure serves to isolate the client from the
   --  implementation-specific calling conventions of asynchronous
   --  handlers.

   procedure Abort_Wrapper
     (signo   : Integer;
      info    : RTE.siginfo_ptr;
      context : System.Address)
   is
      function Address_To_Call_State is new
        Unchecked_Conversion (System.Address, Pre_Call_State);

   begin
      Abort_Handler (Address_To_Call_State (context));
   end Abort_Wrapper;

   ---------------------------
   -- Install_Error_Handler --
   ---------------------------

   procedure Install_Error_Handler (Handler : System.Address) is

      Temp : Address;

      use Pthreads;

   begin
      --  Set up the soft links to tasking services used in the absence of
      --  tasking.  These replace tasking-free defaults.

      Temp := TSL.Get_Jmpbuf_Address.all;
      --  pthread_set_jumpbuf_address (Temp);

      Temp := TSL.Get_Sec_Stack_Addr.all;
      --  pthread_set_sec_stack_addr  (Temp);

      --  TSL.Get_Jmpbuf_Address := pthread_get_jumpbuf_address'Access;
      --  TSL.Set_Jmpbuf_Address := pthread_set_jumpbuf_address'Access;
      --  TSL.Get_Gnat_Exception := pthread_get_exception'Access;
      --  TSL.Set_Gnat_Exception := pthread_set_exception'Access;
   end Install_Error_Handler;

   ---------------
   -- LL_Assert --
   ---------------

   procedure LL_Assert (B : Boolean; M : String) is
   begin
      null;
   end LL_Assert;

   ----------------
   -- LL_Wrapper --
   ----------------

   procedure LL_Wrapper (T : TCB_Ptr) is
      Result  : POSIX_Error.Return_Code;
      Result1 : int;
      Exc_Stack : String (1 .. 256);
      Exc_Base  : Address := Exc_Stack (Exc_Stack'Last)'Address + 1;
      Old_Set : aliased RTE.Signal_Set;
   begin
      Result1 := pthread_setspecific (ATCB_Key, T.all'Address);

      RTE.sigprocmask (
        RTE.SIG_UNBLOCK, Unblocked_Signal_Mask'Access, Old_Set'Access, Result);
      pragma Assert (
        Result /= Failure, "GNULLI failure---sigprocmask");

      --  Note that the following call may not return!
      T.LL_Entry_Point (T.LL_Arg);
   end LL_Wrapper;

   --------------------------
   -- Test and Set support --
   --------------------------

   procedure Initialize_TAS_Cell (Cell : out TAS_Cell) is
   begin
      Cell.Value := 0;
   end Initialize_TAS_Cell;

   procedure Finalize_TAS_Cell (Cell : in out TAS_Cell) is
   begin
      null;
   end Finalize_TAS_Cell;

   procedure Clear (Cell : in out TAS_Cell) is
   begin
      Cell.Value := 1;
   end Clear;

   procedure Test_And_Set (Cell : in out TAS_Cell; Result : out Boolean) is
      Error   : Boolean;
   begin
      Write_Lock (Test_And_Set_Mutex, Error);

      if Cell.Value = 1 then
         Result := False;
      else
         Result :=  True;
         Cell.Value := 1;
      end if;
      Unlock (Test_And_Set_Mutex);
   end Test_And_Set;

   function  Is_Set (Cell : in TAS_Cell) return Boolean is
   begin
      return Cell.Value = 1;
   end Is_Set;
begin
   Initialize_Lock (System.Any_Priority'Last, Test_And_Set_Mutex);
end System.Task_Primitives;
