# Code from the bug report Bug#25098

my $good = XML::LibXML->load_xml( string => q{<div class="clearfix">});
my $bad  = XML::LibXML->load_xml( string =>q{<div class="clearfix">});

# Related: Method calls are no quotelike operators.  That's why you
# can't just add '>' to the character class.

my $method_call  = $object->q(argument);

# Also related, still not fontified correctly:
#
#     my $method_call  = $object -> q (argument);
#
# perl-mode interprets the method call as a quotelike op (because it
# is preceded by a space).
# cperl-mode gets the argument right, but marks q as a quotelike op.
#
#     my $greater = 2>q/1/;
#
# perl-mode doesn't identify this as a quotelike op.
