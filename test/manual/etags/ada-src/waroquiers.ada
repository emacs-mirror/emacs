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
with Pkg1;
package Truc.Bidule is
  use type Pkg1.Public_T;
  use Pkg1;
  use
    type Pkg1.Public_T;
  use -- comment
    type -- comment
    Pkg1.Public_T;

  protected Bidule is
    entry Basar;
  private
    Ok : Boolean;
  end Bidule;

  protected type Machin_T is
    entry Truc;
  private
    Ok : Boolean;
  end Machin_T;

end Truc.Bidule;
package body Truc.Bidule is
  protected body Bidule is
    entry Basar when Ok is
    begin
      null;
    end;
  end Bidule;

  protected body Machin_T is
    entry Truc when Ok is
    begin
      null;
    end;
  end Machin_T;

end Truc.Bidule;
