/* Example provided by Hannes Janetzek */

struct Test { int test; };

#define BLA(_type) \
  _type *bla = (_type*) malloc(sizeof(_type));

#define BLUB(_type)				\
  (_type*)malloc(sizeof(_type));

#define FOO(_type)				\
  _type *foo = BLUB(_type);

#define BAR(_type)				\
  _type *bar = (*_type)BLUB(_type);

int main(int argc, char *argv[]) {
  BLA(Test);
  bla->// -1-
    ; // #1# ( "test" )

  FOO(Test);
  foo->// -2-
    ; // #2# ( "test" )

  BAR(Test);
  bar->// -3-
    ; // #3# ( "test" )
}

/* arch-tag: f4a9fe26-9035-4378-b951-9f06d6554599
   (do not change this comment) */
