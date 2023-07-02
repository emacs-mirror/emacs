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

# Now do this with multiline initializers
# -------- signature with init: input -------
package P {
sub way { ...; }
# perl 5.38 or newer
sub bus
:lvalue
($sig,
$na //= 42,
@ture)
{
...;
}
}
# -------- signature with init: expected output -------
package P {
    sub way { ...; }
    # perl 5.38 or newer
    sub bus
	:lvalue
	($sig,
	 $na //= 42,
	 @ture)
    {
	...;
    }
}
# -------- signature with init: end -------
