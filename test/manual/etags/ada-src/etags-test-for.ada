   type LL_Task_Procedure_Access is access procedure (Arg : System.Address);

   function Body_Required
      (N : Node_Id) return Boolean is
   begin
      pragma Assert (False
          or else NT (N).Nkind = N_Compilation_Unit);
      return Flag13 (N);
   end Body_Required;

   type Type_Specific_Data is record
      Idepth        : Natural;
      Expanded_Name : Cstring_Ptr;
      External_Tag  : Cstring_Ptr;
      HT_Link       : Tag;
      Ancestor_Tags : Tag_Table (Natural);
   end record;

   function "abs"   (Right : Complex) return Real'Base renames Modulus;

   type Barrier_Function_Pointer is access
     function
       (O : System.Address;
        E : Protected_Entry_Index)
        return Boolean;

   function "=" (L, R : System.Address) return Boolean
     renames System."=";

   type usfreelock_ptr is access
     procedure (lock : ulock_t; arena : usptr_t_ptr);

   function p pragma Import (C,
                  "pthread_mutexattr_setprio_ceiling",
                  "pthread_mutexattr_setprio_ceiling");
   pragma Import ()
   procedure LL_Wrapper (T : TCB_Ptr);

function p ("p");

-- This file is an Ada file containing test data
-- for etags (Ada83 and Ada95 support).

package Pkg1 is

  type Private_T is private;

  package Inner1 is
    procedure Private_T;
  end Inner1;

  package Inner2 is
    task Private_T;
  end Inner2;

  type Public_T is
    record
      A : Integer;
      B : Integer;
    end record;

  procedure Pkg1_Proc1;

  procedure Pkg1_Proc2 (I : Integer);

  function Pkg1_Func1 return Boolean;

  function Pkg1_Func2 (Ijk : Integer; Z : Integer) return Natural;


  package Pkg1_Pkg1 is
    procedure Pkg1_Pkg1_Proc1;
  end Pkg1_Pkg1;

  task type Task_Type is
    entry Entry1;
    entry Entry2 (I : Integer);
  end;

private

  type Private_T is
    record
      Z : Integer;
      W : Boolean;
    end record;
end Pkg1;

package body Pkg1 is

  procedure Pkg1_Proc1 is
  begin
    null;
  end;

  package body Inner1 is
    procedure Private_T is
    begin
      null;
    end;
  end Inner1;

  package body Inner2 is
    task body Private_T is
    begin
      loop
        null;
      end loop;
    end;
  end Inner2;

  task body Task_Type is
  begin
    select
      accept Entry1 do
        null;
      end;
    or
      accept Entry2 (I : Integer) do
        null;
      end;
    end select;
  end;


  procedure Pkg1_Proc2 (I : Integer) is
  begin
    null;
  end Pkg1_Proc2;


  function Pkg1_Func1 return Boolean is separate;

  function Pkg1_Func2 (Ijk : Integer; Z : Integer) return Natural is
  begin
    return 1;
  end;


  package body Pkg1_Pkg1 is separate;


end Pkg1;

separate (Pkg1)
package body Pkg1_Pkg1 is
  procedure Pkg1_Pkg1_Proc1 is
  begin
    null;
  end;
end Pkg1_Pkg1;


separate (Pkg1)
function  Pkg1_Func1 return Boolean is
begin
  return False;
end;


-- from now on, this is Ada 95 specific.
package Truc is
  I : Integer;
end Truc;

package Truc.Bidule is

  protected Bidule is
    entry Basar;
  end Bidule;

  protected type Machin_T is
    entry Truc;
  end Machin_T;

end Truc.Bidule;

package body Truc.Bidule is
  protected body Bidule is
    entry Basar is
    begin
      null;
    end;
  end Bidule;

  protected Machin_T is
    entry Truc is
    begin
      null;
    end;
  end Machin_T;

end Truc.Bidule;
