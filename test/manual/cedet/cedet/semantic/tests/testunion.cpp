// Test unions and completion.

struct myStruct {
  int base1;
  int base2;

  union { // Named
    char *named_str;
    void *named_ptr;
  } allocated;

  union { // Anonymous
    char *anon_str;
    void *anon_ptr;
  };

};

int main() {
  struct myStruct S;

  S.//-1-
    ; // #1# ( "allocated" "base1" "base2" "anon_str" "anon_ptr" )

  S.allocated.//-2-
    ; // #2# ( "named_str" "named_ptr" )

}
