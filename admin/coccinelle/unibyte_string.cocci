// make_unibyte_string (str, strlen (str)) -> build_unibyte_string (str)
@@
identifier I;
@@
- make_unibyte_string (I, strlen (I))
+ build_unibyte_string (I)

@@
constant C;
@@
- make_unibyte_string (C, strlen (C))
+ build_unibyte_string (C)
