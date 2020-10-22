#!/usr/bin/env perl
use strict;
use warnings;
use 5.020;

# This file contains test input and expected output for the tests in
# cperl-mode-tests.el, cperl-mode-test-indent-exp.  The code is
# syntactically valid, but doesn't make much sense.

# -------- for loop: input --------
for my $foo (@ARGV)
{
...;
}
# -------- for loop: expected output --------
for my $foo (@ARGV) {
  ...;
}
# -------- for loop: end --------

# -------- while loop: input --------
{
while (1)
{
say "boring loop";
}
continue
{
last;
}
}
# -------- while loop: expected output --------
{
  while (1) {
    say "boring loop";
  } continue {
    last;
  }
}
# -------- while loop: end --------

# -------- if-then-else: input --------
if (my $foo) { bar() } elsif (quux()) { baz() } else { quuux }
# -------- if-then-else: expected output --------
if (my $foo) {
  bar();
} elsif (quux()) {
  baz();
} else {
  quuux;
}
# -------- if-then-else: end --------
