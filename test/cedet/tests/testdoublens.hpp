//
// Header file used in one of the ia-utest tests.
//
namespace Name1 {
  namespace Name2 {

    class Foo
    {
      typedef unsigned int Mumble;
    public:
      Foo();
      ~Foo();
      int get();

    private:
      void publishStuff(int a, int b);

      void sendStuff(int a, int b);
    
      Mumble* pMumble;
    };

    typedef Foo stage1_Foo;

  } // namespace Name2

  typedef Name2::stage1_Foo stage2_Foo;

  typedef Name2::Foo decl_stage1_Foo;

} // namespace Name1

typedef Name1::stage2_Foo stage3_Foo;


// Double namespace from Hannu Koivisto
namespace a {
  namespace b {

    class Foo
    {
      struct Dum {
        int diDum;
      };

    protected:
      mutable a::b::Foo::Dum dumdum;
    };

  } // namespace b
} // namespace a
