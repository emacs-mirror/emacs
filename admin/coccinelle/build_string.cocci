// Convert simple cases to build_string.
@@
identifier I;
@@
- make_string (I, strlen (I))
+ build_string (I)

@@
constant C;
@@
- make_string (C, strlen (C))
+ build_string (C)
