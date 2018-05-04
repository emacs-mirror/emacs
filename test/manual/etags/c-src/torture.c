/* Date: Thu, 05 Aug 1993 20:28:03 +0200
   From: "Tom R.Hageman" <tom@basil.icce.rug.nl>
   Subject: more etags torture;-) [etags 7.3 patch#3]
   To: pot@CNUCE.CNR.IT

   Hi,

   This test file illustrates some more problems with etags (7.3):
 

   1. parentheses are confusing,
   2. preprocessor directives can disrupt other state machines. */

/* A small torture test for etags. */

/* The classic parenthesis nightmare, based on signal(). */
void
(*tag1 (sig, handler)) ()
  int sig;
  void (*handler) ();
{
  (*handler)(sig);
  return handler;
}

#define notag2 void
/* The classic, with user-defined return type. */
notag2
(*tag2 (sig, handler)) ()
  int sig;
  void (*handler) ();
{
  (*handler)(sig);
  return handler;
}

/* The classic, in ANSI C style. */
void
(*tag3 (int sig, void (*handler) (int))) (int)
{
  (*handler)(sig);
  return handler;
}

#define notag4 void
/* The classic, with user-defined return type, in ANSI C style. */
notag4
(*tag4 (int sig, void (*handler) (int))) (int)
{
  (*handler)(sig);
  return handler;
}


/* A less tortuous example. */
void
tag5 (handler, arg)
void (*handler)();
void *arg;
{
  (*handler)(arg);
}

/* A less tortuous example, in ANSI C style. */
void
tag6 (void (*handler) (void *), void *arg)
{
  (*handler)(arg);
}


/* Interfering preprocessing torture */

int pp1(
#if (__STDC__)
	int
#endif
	bar)
#if (!__STDC__)
     int bar;
#endif
{
  return bar;
}

int
pp2
#if __STDC__
  (int bar)
#else
  (bar)
    int bar;
#endif
{
  return bar;
}

int
#if __STDC__
pp3(int bar)
#else
pp3(bar)
  int bar;
#endif
{
  return bar;
}
