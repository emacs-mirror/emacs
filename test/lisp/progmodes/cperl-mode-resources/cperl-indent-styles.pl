#!/usr/bin/env perl
use strict;
use warnings;
use 5.020;

# This file contains test input and expected output for the tests in
# cperl-mode-tests.el, cperl-mode-test-indent-exp.  The code is
# syntactically valid, but doesn't make much sense.

# -------- PBP indent: input --------
for my $foo (@ARGV)
{
...;
}
# -------- PBP indent: expected output --------
for my $foo (@ARGV) {
    ...;
}
# -------- PBP indent: end --------

# -------- PBP uncuddle else: input --------
{
if (1 < 2)
{
say "Seems ok";
} elsif (1 == 2) {
say "Strange things are happening";
} else {
die "This world is backwards";
}
}
# -------- PBP uncuddle else: expected output --------
{
    if (1 < 2) {
	say "Seems ok";
    }
    elsif (1 == 2) {
	say "Strange things are happening";
    }
    else {
	die "This world is backwards";
    }
}
# -------- PBP uncuddle else: end --------

# -------- PBP closing paren offset: input --------
my $a = func1(
    Module::test()
  );
# -------- PBP closing paren offset: expected output --------
my $a = func1(
    Module::test()
);
# -------- PBP closing paren offset: end --------
