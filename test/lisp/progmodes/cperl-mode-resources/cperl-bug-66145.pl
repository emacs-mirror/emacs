# The original code, from the bug report, with variables renamed

sub foo {
  # Here we do something like
  # this: $array_comment [ num_things ]->{key_comment}
}

# --------------------------------------------------
# Comments containing hash and array sigils

# This is an @array, and this is a %hash
# $array_comment[$index] = $hash_comment{key_comment}
# The last element has the index $#array_comment
# my @a_slice = @array_comment[1,2,3];
# my @h_slice = @hash_comment{qw(a b c)};
# my %a_set   = %array_comment[1,2,3];
# my %h_set   = %hash_comment{qw(a b c)};

# --------------------------------------------------
# in POD

=head1 NAME

cperl-bug-66145 - don't fontify arrays and hashes in POD

=head1 SYNOPSIS

   $array_comment[$index] = $hash_comment{key_comment};
   @array_comment = qw(in pod);
   %hash_comment  = key_comment => q(pod);
   @array_comment = @array_comment[1,2,3];
   @array_comment = @hash_comment{qw(a b c)};
   %hash_comment  = %array_comment[1,2,3];
   %hash_comment  = %hash_comment{qw(a b c)};

=cut

# --------------------------------------------------
# in strings

my @strings = (
    q/$array_string[$index] = $hash_string{key_string};/,
    q/my @array_string = qw(in unquoted string);/,
    q/my %hash_string  = (key_string => q(pod);)/,
    q/@array_string    = @array_string[1,2,3];/,
    q/@array_string    = @hash_string{qw(a b c)};/,
    q/%hash_string     = %array_string[1,2,3];/,
    q/%hash_string     = %hash_string{qw(a b c)};/,
);

# --------------------------------------------------
# in a HERE-document (perl-mode has an extra face for that)

my $here = <<DONE;
   $array_here[$index_here] = $hash_here{key_here};
   @array_here = qw(in a hrere-document);
   %hash_here  = key_here => q(pod);
   @array_here = @array_here[1,2,3];
   @array_here = @hash_here{qw(a b c)};
   %hash_here  = %array_here[1,2,3];
   %hash_here  = %hash_here{qw(a b c)};
DONE
