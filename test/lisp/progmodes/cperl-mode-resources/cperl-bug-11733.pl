# This resource file can be run with cperl--run-testcases from
# cperl-tests.el and works with both perl-mode and cperl-mode.

# -------- Multiline declaration: input -------
#!/usr/bin/env perl
# -*- mode: cperl -*-

sub foo
  {
  }

sub bar
  {
  }
# -------- Multiline declaration: expected output -------
#!/usr/bin/env perl
# -*- mode: cperl -*-

sub foo
{
}

sub bar
{
}
# -------- Multiline declaration: end -------

# -------- Fred Colon at work: input --------
#!/usr/bin/env perl
# -*- mode: cperl -*-

while (<>)
{
m:^  \d+ p:
or die;
m:^  \d+ :
or die;
}
# -------- Fred Colon at work: expected output --------
#!/usr/bin/env perl
# -*- mode: cperl -*-

while (<>)
  {
    m:^  \d+ p:
      or die;
    m:^  \d+ :
      or die;
  }
# -------- Fred Colon at work: end --------
