# The following Perl punctuation variables contain characters which
# are classified as string delimiters in the syntax table.  The mode
# should not be confused by these.
# The corresponding tests check that two consecutive '#' characters
# are seen as comments, not as strings.
my $pre = $`;  ##  $PREMATCH,      use another ` # to balance out
my $pos = $';  ##  $POSTMATCH,     use another ' # to balance out
my $lsp = $";  ##  $LIST_SEPARATOR use another " # to balance out

# In the second level, we use the reference constructor \ on these
# variables.  The backslash is an escape character *only* in strings.
my $ref = \$`; ## \$PREMATCH,      use another ` # to balance out
my $rif = \$'; ## \$POSTMATCH,     use another ' # to balance out
my $raf = \$"; ## \$LIST_SEPARATOR use another " # to balance out

my $opt::s = 0;       ## s is no substitution here
my $opt_s  = 0;       ## s is no substitution here
my %opt = (s => 0);   ## s is no substitution here
$opt{s} = 0;          ## s is no substitution here
$opt_s =~ /\s+.../    ## s is no substitution here
