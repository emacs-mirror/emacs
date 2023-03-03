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

# FIXME: 2022-02-02 CPerl mode does not handle subroutine signatures.
# In simple cases it mistakes them as prototypes, when attributes are
# present, it doesn't handle them at all.  Variables in signatures
# SHOULD be fontified like variable declarations.

# Part 1: Named subroutines
# A prototype and a trivial subroutine attribute
{
    no feature 'signatures'; # that's a prototype, not a signature
    sub sub_1 ($) :lvalue { local $); }
}

# A prototype as an attribute (how it should be written these days)
sub sub_2 :prototype($) { ...; }

# A signature (these will soon-ish leave the experimental state)
sub sub_3 ($foo,$bar) { ...; }

# Attribute plus signature FIXME: Not yet supported
sub bad_sub_4 :prototype($$$) ($foo,$bar,$baz) { ...; }

# Part 2: Same constructs for anonymous subs
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
