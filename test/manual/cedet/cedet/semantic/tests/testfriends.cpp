// Test parsing of friends and how they are used in completion.
/*
  >>  Thanks Damien Profeta for the nice example.
  >
  >  I paste a small example.
  >  It would be great if friend can be well parsed and even greater if
  >  class B can access to all the members of A.
*/

class Af // %2% ( ( "testfriends.cpp" ) ( "Af" "B::testB" ) )
{
public:
  int pubVar;
private:
  int privateVar;

  friend class B; // Full class friend format.

  friend C; // Abbreviated friend format.

};

class B
{
public:
  int testB();
  int testAB();

};

class C
{
public:
  int testC();
  int testAC();

};

int B::testB() {
  Af classA;
  classA.//-1-
    ; //#1# ( "privateVar" "pubVar" )
}

int B::testAB() { // %1% ( ( "testfriends.cpp" ) ( "B" "B::testAB" ) )
}


// Test friends when subclassing.
class scA : public scB, public scC, public scD
{
public:
  friend class scB;
  friend scC;

private:

  int data;

public:

  int PublicMethod();

};

class scB
{
public:
  int b_pub_method();
protected:
  int b_prot_method();
private:
  int b_priv_method();
};


class scC
{
public:
  int c_pub_method();
protected:
  int c_prot_method();
private:
  int c_priv_method();
};

class scD // Not a friend
{
public:
  int d_pub_method();
protected:
  int d_prot_method();
private:
  int d_priv_method();
};


int main()
{
  scA aclass;

  aclass. //-2-
    ; //#2# ( "PublicMethod" "b_pub_method" "c_pub_method" "d_pub_method")
}


int scA::PublicMethod() {
  this. // -3-
    ; // #3# ( "PublicMethod" "b_prot_method" "b_pub_method" "c_prot_method" "c_pub_method" "d_prot_method" "d_pub_method" "data")
}

int scB::b_pub_method() {
  scA myfriend;
  myfriend. // -4-
    ; // #4# ( "PublicMethod" "b_priv_method" "b_prot_method" "b_pub_method" "c_pub_method" "d_pub_method" "data")
}

int scC::c_pub_method() {
  scA myfriend;
  myfriend. // -5-
    ; // #5# ( "PublicMethod" "b_pub_method" "c_priv_method" "c_prot_method" "c_pub_method" "d_pub_method" "data")
}

int scD::d_pub_method() {
  scA myfriend;
  // The NOT FRIEND can't see 'data' which is private to scA
  myfriend. // -6-
    ; // #6# ( "PublicMethod" "b_pub_method" "c_pub_method" "d_priv_method" "d_prot_method" "d_pub_method")
}
