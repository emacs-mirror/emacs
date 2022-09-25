use 5.024;
use strict;
use warnings;
use utf8;

sub outside {
    say "Line @{[__LINE__]}: package '@{[__PACKAGE__]}'";
}

package Package;

=head1 NAME

grammar - A Test resource for regular expressions

=head1 SYNOPSIS

A Perl file showing a variety of declarations

=head1 DESCRIPTION

This file offers several syntactical constructs for packages,
subroutines, and POD to test the imenu capabilities of CPerl mode.

Perl offers syntactical variations for package and subroutine
declarations.  Packages may, or may not, have a version and may, or
may not, have a block of code attached to them.  Subroutines can have
old-style prototypes, attributes, and signatures which are still
experimental but widely accepted.

Various Extensions and future Perl versions will probably add new
keywords for "class" and "method", both with syntactical extras of
their own.

This test file tries to keep up with them.

=head2 Details

The code is supposed to identify and exclude false positives,
e.g. declarations in a string or in POD, as well as POD in a string.
These should not go into the imenu index.

=cut

our $VERSION = 3.1415;
say "Line @{[__LINE__]}: package '@{[__PACKAGE__]}', version $VERSION";

sub in_package {
    # Special test for POD: A line which looks like POD, but actually
    # is part of a multiline string.  In the case shown here, the
    # semicolon is not part of the string, but POD headings go to the
    # end of the line.  The code needs to distinguish between a POD
    # heading "This Is Not A Pod/;" and a multiline string.
    my $not_a_pod = q/Another false positive:

=head1 This Is Not A Pod/;

}

sub Shoved::elsewhere {
    say "Line @{[__LINE__]}: package '@{[__PACKAGE__]}', sub Shoved::elsewhere";
}

sub prototyped ($$) {
    ...;
}

package Versioned::Package 0.07;
say "Line @{[__LINE__]}: package '@{[__PACKAGE__]}', version $VERSION";

sub versioned {
    # This sub is in package Versioned::Package
    say "sub 'versioned' in package '", __PACKAGE__, "'";
}

versioned();

my $false_positives = <<'EOH';
The following declarations are not supposed to be recorded for imenu.
They are in a HERE-doc, which is a generic comment in CPerl mode.

package Don::T::Report::This;
sub this_is_no_sub {
    my $self = shuffle;
}

And this is not a POD heading:

=head1 Not a POD heading, just a string.

EOH

package Block {
    our $VERSION = 2.7182;
    say "Line @{[__LINE__]}: package '@{[__PACKAGE__]}', version $VERSION";

    sub attr:lvalue {
        say "sub 'attr' in package '", __PACKAGE__, "'";
    }

    attr();

    package Block::Inner {
        # This hopefully doesn't happen too often.
        say "Line @{[__LINE__]}: package '@{[__PACKAGE__]}', version $VERSION";
    }

    # Now check that we're back to package "Block"
    say "Line @{[__LINE__]}: package '@{[__PACKAGE__]}', version $VERSION";
}

sub outer {
    # This is in package Versioned::Package
    say "Line @{[__LINE__]}: package '@{[__PACKAGE__]}', version $VERSION";
}

outer();

package Versioned::Block 42 {
    say "Line @{[__LINE__]}: package '@{[__PACKAGE__]}', version $VERSION";

    my sub lexical {
        say "sub 'lexical' in package '", __PACKAGE__, "'";
    }

    lexical();

    use experimental 'signatures';
    sub signatured :prototype($@) ($self,@rest)
    {
        ...;
    }
}

# After all is said and done, we're back in package Versioned::Package.
say "We're in package '", __PACKAGE__, "' now.";
say "Now try to call a subroutine which went out of scope:";
eval { lexical() };
say $@ if $@;

# Now back to Package. This must not appear separately in the
# hierarchy list.
package Package;

our sub in_package_again {
    say "Line @{[__LINE__]}: package '@{[__PACKAGE__]}', version $VERSION";
}


package :: {
    # This is just a weird, but legal, package name.
    say "Line @{[__LINE__]}: package '@{[__PACKAGE__]}', version $VERSION";

    in_package_again(); # weird, but calls the sub from above
}

Shoved::elsewhere();

# Finally, try unicode identifiers.
package Erdős::Number;

sub erdős_number {
    my $name = shift;
    if ($name eq  "Erdős Pál") {
	return 0;
    }
    else {
        die "No access to the database. Sorry.";
    }
}

1;
