// Use the ARRAYELTS macro where possible.
@@
type T;
T[] E;
@@
- (sizeof (E) / sizeof (E[...]))
+ ARRAYELTS (E)

@@
type T;
T[] E;
@@
- (sizeof (E) / sizeof (T))
+ ARRAYELTS (E)

@@
type T;
T[] E;
@@
- (sizeof (E) / sizeof (*E))
+ ARRAYELTS (E)
