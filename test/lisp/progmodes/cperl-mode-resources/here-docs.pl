use 5.020;

=head1 NAME

here-docs.pl - resource file for cperl-test-here-docs

=head1 DESCRIPTION

This file holds a couple of HERE documents, with a variety of normal
and edge cases.  For a formatted view of this description, run:

   (cperl-perldoc "here-docs.pl")

For each of the HERE documents, the following checks will done:

=over 4

=item *

All occurrences of the string "look-here" are fontified correctly.
Note that we deliberately test the face, not the syntax property:
Users won't care for the syntax property, but they see the face.
Different implementations with different syntax properties have been
seen in the past.

=item *

Indentation of the line(s) containing "look-here" is 0, i.e. there are no
leading spaces.

=item *

Indentation of the following perl statement containing "indent" should
be 0 if the statement contains "noindent", and according to the mode's
continued-statement-offset otherwise.

=back

=cut

# Prologue to make the test file valid without warnings

my $text;
my $any;
my $indentation;
my $anywhere = 'back again';
my $noindent;

=head1 The Tests

=head2 Test Case 1

We have two HERE documents in one line with different quoting styles.

=cut

## test case

$text = <<"HERE" . <<'THERE' . $any;
#look-here and
HERE
$tlook-here and
THERE

$noindent = "This should be left-justified";

=head2 Test case 2

A HERE document followed by a continuation line

=cut

## test case

$text = <<HERE
look-here
HERE

. 'indent-level'; # Continuation, should be indented

=head2 Test case 3

A here document with a line-end comment in the starter line,
after a complete statement

=cut

## test case

$text = <<HERE; # start here
look-here
HERE

$noindent = "New statement in this line";

=head2 Test case 4

A HERE document with a to-be-continued statement and a comment in the
starter line.

=cut

## test case

$text = <<HERE # start here
look-here
HERE

. 'indent-level'; # Continuation, should be indented

=head2 Test case 5

A HERE document with a comment sign, but no comment to follow.


=cut

## test case

$text = <<HERE; #
look-here
HERE

$noindent = "New statement in this line";

=head2 Test case 6

A HERE document with a comment sign, but no comment to follow, with a
statement to be continued.  Also, the character before the comment
sign has a relevant syntax property (end of string in our case) which
must be preserved.

=cut

## test case

$text = <<"HERE"#
look-here
HERE

. 'indent-level'; # Continuation, should be indented

=head2 Test case 7

An indented HERE document using a bare identifier.

=cut

## test case

$text = <<~HERE;
look-here
HERE

$noindent = "New statement in this line";

=head2 Test case 8

A HERE document as an argument to print when printing to a filehandle.

=cut

## test case

print $fh <<~HERE;
look-here
HERE

$noindent = "New statement in this line";

=head2 Test case 9

A HERE document as a hash value.

=cut

my %foo = (
    text => <<~HERE
look-here
HERE
    );

$noindent = "New statement in this line";

=head2 Test case 10

A HERE document as an argument to die.

=cut

1 or die <<HERE;
look-here
HERE

$noindent = "New statement in this line";

=head2 Test case 11

A HERE document as an argument to warn.

=cut

1 or warn <<HERE;
look-here
HERE

$noindent = "New statement in this line";

__END__
