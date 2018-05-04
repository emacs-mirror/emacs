class XX
{
public:
     int foo();
     void bar();
};

int
XX::foo()
{
     return 1;
}

void
XX::bar()
{
     foo();
}

int
main(int argc, char *argv[])
{
     XX xx;
     xx.bar();
     return 0;
}
