@@
expression x;
expression E;
@@
 x = 
- xmalloc 
+ xzalloc
  (E)
  ... 
- memset (x, 0, E);
