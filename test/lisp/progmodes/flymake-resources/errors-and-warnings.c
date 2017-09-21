 int main()
{
  char c = 1000;
  int bla;
  /* The following line should have one warning and one error. The
     warning spans the full line because gcc (at least 6.3.0) points
     places the error at the =, which isn't a sexp.*/
  char c; if (bla == (void*)3);
  return c;
}
