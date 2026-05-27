// Use the countof macro where possible.
@@
type T;
T[] E;
@@
- (sizeof (E) / sizeof (E[...]))
+ countof (E)

@@
type T;
T[] E;
@@
- (sizeof (E) / sizeof (T))
+ countof (E)

@@
type T;
T[] E;
@@
- (sizeof (E) / sizeof (*E))
+ countof (E)
