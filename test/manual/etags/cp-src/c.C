template <typename ipc3dIslandHierarchy, typename ipc3dChannelType, unsigned numOfChannels, typename ipc3dLinkControl, typename ipc3dLinkControlSetup>
class CMultiChannelCSC19_3D
{
private:
        ipc3dLinkControlSetup setup;
        ipc3dCSC19<ipc3dIslandHierarchy,ipcMultiChannel<ipc3dChannelType,numOfChannels>,ipcMultiChannel<ipc3dChannelType,numOfChannels>,ipc3dLinkControl> mcCSC;
        advTimer cscInitTime;
        advTimer cscSegmentationTime;
        advTimer outputTime;
public:
        void execute(CPluginCSCState& p, int w, int h, int d, const ipcMultiChannel<ipc3dChannelType,numOfChannels>* orgImage, ipcMultiChannel<ipc3dChannelType,numOfChannels>* regionImage, unsigned int* mapImage, ipc3dBlockCompressedLabelImage* compressedMapImage=NULL)
        {
                if (orgImage!=NULL)
                {
                                  //do something
                }
        }

class foo {
  int const_func() const;
  int b;
  int non_const_func2(void);
};
static void my_function1(int var1) const;
int main (void) { my_function0(0); my_function1(1); return; }
double base (void) const { return rng_base;  }

template <typename T> MDiagArray2<T>&
operator += (MDiagArray2<T>& a, const MDiagArray2<T>& b);

class TestRecord;
typedef struct s1 {
   int counter;
} t1;
struct s2 {
   int counter;
};
typedef struct s2 t2;
class A {
  enum { rosso, giallo, verde } colori;
  const A& operator+(const A&);
};
const A& A::operator+(const A&) { }
void operator+(int, int) {}
void operator -(int, int) {}
void operator int(int, int) {}

A<int>* f() {}
int f(A<int> x) {}
int A<int>::f(A<int>* x) {}
A<float,B<int> > A<B<float>,int>::f(A<int>* x) {}
template <class C, int n> class AT { C t[n]; };
class AU { T x; };
class B<int> { void f() {} };
const A::B::T& abt = abc;
class A { class B { int f(); }; };
class A {
  int get_data() const;
  A operator+(A& a) {};
};
is_muldiv_operation(pc)
{
}
#ifdef __cplusplus
extern "C" {
#endif

domain foo {
     void f() {}
};

void A::A() {}
struct A { A(); }
struct B { B(); };
void B::B() {}
void BE_Node::BE_Node() {}
class BE_Node {};

struct foo {
  int x;
};

#ifdef __cplusplus
}
#endif
class test {
  int f(){return 0;};		// first comment
				// second comment
  int ff(){return 1;};
  int g(){return 2;};
}
class	AST_Root : public virtual AST_Module
{
};

class	AST_Root;			// The root of an AST 112,3888
// etags finds

AST_ConcreteType::AST_ConcreteType(AST_Decl::NodeType nt, UTL_ScopedName *n,
				     UTL_StrList *p)
		 : AST_Decl(nt, n, p)
{
}

// and

AST_Array::AST_Array(UTL_ScopedName *n, unsigned long nd, UTL_ExprList *ds)
	 : pd_n_dims(nd), pd_base_type(NULL),
	   AST_Decl(AST_Decl::NT_array, n, NULL)
{
}

// as definitions of AST_Decl.
class {
     void f() {}
};
struct A {
    ~A();
};
A::~A() {}

struct B {
    ~B() {};
};

enum {dog, cat} animals;
struct {int teats;} cow;

class Boo {
    enum {dog, cat} animals;
    struct {int treats;} cow;
    int i,a,b;
    foo() {
        cout << "hi";
    }

    Boo(int _i, int _a, int _b) : i(_i), a(_a), b(_b) {}
    Boo(Boo);
};

Boo::Boo(Boo) :
    i(i),
    a(a),
    b(b)
{}

/* extern "C" grot: */
extern "C" {
typedef int should_see_this_one_enclosed_in_extern_C;
}

/* Typedefs: */
typedef int (*should_see_this_function_pointer) (
	void *but_not_this_argument);

typedef int should_see_this_array_type[but_not_this_subscript];
