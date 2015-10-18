// Test disambiguation of polymorphic methods.

class xx {
public:
  int int_field;
  int char_field;
  char foo(char x);
  int foo (int x);
};

int whatever() {
  xx x;

  char same_char;
  int same_int;
  int i;
  char c;

  i = x.foo(same // -1-
	    // #1# ( "same_int" )
	    );

  c = x.foo(same // -2-
	    // #2# ( "same_char" )
	    );

}

// Example from Dmitry
struct foo {
  int a;
  int b;
};

struct bar {
  int c;
  int d;
};

foo tee(int i) {
  foo f;
  return f;
}

bar tee(long i) {
  bar b;
  return b;
}

int main() {
  int i = 1;
  long l = 2;

  tee(i).//-3-
    ; // #3# ( "a" "b" )

  tee(l).//-4-
    ; // #4# ( "c" "d" )
}
