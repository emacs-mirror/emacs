/* Examples provided by Sam Kendall <kendall@mv.mv.com>, Jan 1997 */

// check use of references with nested/local classes

// This example causes etags 13 to abort when compiled with -DDEBUG.
// Etags 13 cannot deal with nested structures after the first level.
struct A {
	   struct B {
		   struct C {
			   int x;
			   C(int i) {x = i;}
			   operator int() const {return x;}
		   };
		   typedef C T;
	   };
	   typedef B T2;
};


class String;


class A {
	class B {
		class C {};
		int f() { return 5; }
	};
};


int A::B::f() { return 2; }


A::B::C abc(-37);


main()
{
	if (abc != -37 || abt != -37) return 1;

	class D : public A::B::C {
	public:
		D() : ::A::T2::T(97), x(1066) {}
		int x;
	} &d = D();

	if (d.x != 1066 || d.A::T2::T::x != 97) return 2;
	return 0;
}


template <class T>
