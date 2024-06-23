use utf8;

my $string_with_strange_delimiters  = q«a»;
my $printed = 0;

label:
print $string_with_strange_delimiters;
$printed = 1;

# With cperl-extra-delimiters-mode=on the previous lines are a label
# and a print statement.  This line here is a comment.  Without
# cperl-extra-delimiters-mode, all this is part of the variable
# declaration.

# Perl will print hist an "a" if called like this:
#   perl -M5.040 extra.pl
# ...and, if called without that -M switch,
#   perl extra.pl
# will print everything until here: «;

$printed  or print $string_with_strange_delimiters;

my $sanity = "eventually recovered.";
