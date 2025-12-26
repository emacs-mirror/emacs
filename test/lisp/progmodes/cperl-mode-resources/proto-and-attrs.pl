# The next two lines are required as of 2022, but obsolescent
# as soon as signatures leave their "experimental" state
use feature 'signatures';
no warnings 'experimental::signatures';

# Tests for subroutine prototypes, signatures and the like

# Prototypes have syntactical properties different from "normal" Perl:
# Perl has a variable $), so ($)) is not an unbalanced parenthesis.
# On the other hand, in a prototype ($) is _not_ an open paren
# followed by the variable $), so the parens are balanced.  Prototypes
# are somewhat frowned upon most of the times, but they are required
# for some Perl magic

# Part 1: Named subroutines
# A plain named subroutine without any optional stuff
sub sub_0 { ...; }

# A prototype and a trivial subroutine attribute
{
    no feature 'signatures'; # that's a prototype, not a signature
    sub sub_1 ($) :lvalue { local $); }
}

# A prototype as an attribute (how it should be written these days)
sub sub_2 :prototype($) { ...; }

# A signature (these will soon-ish leave the experimental state)
sub sub_3 ($foo,$bar) { ...; }

# Attribute plus signature
sub sub_4 :prototype($$$) ($foo,$bar,$baz) { ...; }

# A signature with a trailing comma (weird, but legal)
sub sub_5 ($foo,$bar,) { ...; }

# Perl 5.38-style initializer
sub sub_6
    ($foo,
     $bar //= "baz")
{
}

# Braces in initializers (Bug79269)
sub sub_7
    ($foo = { },
     $bar //= "baz")
{
}


# Part 2: Same constructs for anonymous subs
# A plain named subroutine without any optional stuff
my $subref_0 = sub { ...; };

# A prototype and a trivial subroutine attribute
{
    no feature 'signatures'; # that's a prototype, not a signature
    my $subref_1 = sub ($) :lvalue { local $); };
}

# A prototype as an attribute (how it should be written these days)
my $subref_2 = sub :prototype($) { ...; };

# A signature (these will soon-ish leave the experimental state)
my $subref_3 = sub ($foo,$bar) { ...; };

# Attribute plus signature
my $subref_4 = sub :prototype($$$) ($foo,$bar,$baz) { ...; };
