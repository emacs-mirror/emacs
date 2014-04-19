// Test parsing of local variables

class foo {
  foo *member;
  char anArray[10];
};

void func()
{
  foo local1;
  foo* local2 = localvar.member;
  foo* local3 = new foo();
  foo local4[10];
  char local5[5] = {'1','2','3','4','5'};
  char *local6 = "12345";
  char local7 = local.anArray[0];
  char local8 = true ? 10 : 11 ;

  // Check that all of the above was parsed
  local//-1-
    ; //#1# ("local1" "local2" "local3" "local4" "local5" "local6" "local7" "local8" )

  local1.//-2-
    ; //#2# ("anArray" "member")

  local2->//-3-
    ; //#3# ("anArray" "member")

  local3->//-4-
    ; //#4# ("anArray" "member")

  local4[0].//-5-
    ; //#5# ("anArray" "member")
}
