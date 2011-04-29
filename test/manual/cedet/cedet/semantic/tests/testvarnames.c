/*
 * Test variable and function names, lists of variables on one line, etc.
 */

struct independent {
  int indep_1;
  int indep_2;
};

struct independent var_indep_struct;

struct {
  int unnamed_1;
  int unnamed_2;
} var_unamed_struct;

struct {
  int unnamed_3;
  int unnamed_4;
} var_un_2, var_un_3;

struct inlinestruct {
  int named_1;
  int named_2;
} var_named_struct;

struct inline2struct {
  int named_3;
  int named_4;
} var_n_2, var_n_3;

/* Structures with names that then declare variables
 * should also be completable.
 *
 * Getting this to work is the bugfix in semantic/bovine/c.el CVS v 1.122
 */
struct inlinestruct in_var1;
struct inline2struct in_var2;

int test_1(int var_arg1) {

  var_// -1-
    ; // #1# ("var_arg1" "var_indep_struct" "var_n_2" "var_n_3" "var_named_struct" "var_un_2" "var_un_3" "var_unamed_struct")

  var_indep_struct.// -2-
    ; // #2# ( "indep_1" "indep_2" )

  var_unamed_struct.// -3-
    ; // #3# ( "unnamed_1" "unnamed_2" )

  var_named_struct.// -4-
    ; // #4# ( "named_1" "named_2" )

  var_un_2.// -5-
    ; // #5# ( "unnamed_3" "unnamed_4" )
  var_un_3.// -6-
    ; // #6# ( "unnamed_3" "unnamed_4" )

  var_n_2.// -7-
    ; // #7# ( "named_3" "named_4" )
  var_n_3.// -8-
    ; // #8# ( "named_3" "named_4" )

  in_// -9-
    ; // #9# ( "in_var1" "in_var2" )

  in_var1.// -10-
    ; // #10# ( "named_1" "named_2")
  in_var2.// -11-
    ; // #11# ( "named_3" "named_4")
}
