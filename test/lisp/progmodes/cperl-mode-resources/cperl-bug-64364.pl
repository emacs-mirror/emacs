# This resource file can be run with cperl--run-testcases from
# cperl-tests.el and works with both perl-mode and cperl-mode.

# -------- Bug#64364: input -------
package P {
sub way { ...; }
#
sub bus
:lvalue
($sig,$na,@ture)
{
...;
}
}
# -------- Bug#64364: expected output -------
package P {
    sub way { ...; }
    #
    sub bus
	:lvalue
	($sig,$na,@ture)
    {
	...;
    }
}
# -------- Bug#64364: end -------
