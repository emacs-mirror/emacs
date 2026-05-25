use 5.038;
use feature 'class';
use warnings;
no warnings 'experimental';

class C {
    # "method" is not yet understood by perl-mode, but it isn't
    # relevant here: We can use "sub" because what matters is the
    # name, which collides with a builtin.
    sub m {
	"m called"
    }
}

say C->new->m;

# This comment has a method name in it, and we don't want "method"
# to be fontified as a keyword, nor "name" fontified as a name.

# Next is a variable named "$method" followed by a keyword.  This
# keyword is not a subroutine name and should not be fontified
# accordingly.  Reported by Branislav Zahradnik,
# https://github.com/HaraldJoerg/cperl-mode/issues/24

push @abstract, $method
	unless defined &$method
	;

__END__

=head1 Test using the keywords POD

This piece of POD has a method name in it, and we don't want "method"
to be fontified as a keyword, nor "name" fontified as a name.
