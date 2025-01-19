# This resource file can be run with cperl--run-testcases from
# cperl-tests.el and works with both perl-mode and cperl-mode.

# -------- signature where last parameter is ignored: input -------
package P {
use v5.36;
sub ignore ($first, $) {}
ignore(qw(first second));
}
# -------- signature where last parameter is ignored: expected output -------
package P {
  use v5.36;
  sub ignore ($first, $) {}
  ignore(qw(first second));
}
# -------- signature where last parameter is ignored: end -------
