# -------- bug#19709: input --------
my $a = func1(
    Module::test()
  );

my $b = func2(
    test()
);

my $c = func3(
    Module::test(),
);
# -------- bug#19709: expected output --------
my $a = func1(
    Module::test()
);

my $b = func2(
    test()
);

my $c = func3(
    Module::test(),
);
# -------- bug#19709: end --------
