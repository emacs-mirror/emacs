#!/usr/bin/perl
# -*- eval: (bug-reference-mode 1) -*-

sub add_funds($) {
    return 0;
}

# qw(...) is a quoted list of words, so we can and should indent its content!
my @tutu = qw[
    tata
    titi
    ];

my $hash = {
    foo => 'bar',
    format => 'some',
};

sub some_code {
    print "will not indent :(";
};

use v5.14;

my $str= <<END;
Hello
END

my $a = $';

my $b=3;

print $str;
if ($c && /====/){xyz;}

print "a" . <<EOF . s/he"llo/th'ere/;
It's a surprise!
EOF

print <<\EOF1 . s/he"llo/th'ere/;
foo
EOF2
bar
EOF1

$config = {
    b  =>
        [
         "123",
        ],
    c => "123",
};

print <<"EOF1" . <<\EOF2 . s/he"llo/th'ere/;
foo
EOF2
bar
EOF1
bar
EOF2

print <<~"EOF1" . <<\EOF2 . s/he"llo/th'ere/;
foo
EOF2
   bar
   EOF1
bar
EOF2

print $'; # This should not start a string!

print "hello" for /./;

$fileType_filesButNot           # bug#12373?
    = join( '|', map { quotemeta($_).'$' } @{$fileType->{filesButNot}} );

# There can be a comment between an if/when/while and a /<re>/ matcher!
return 'W' if               #/^Not Available on Mobile/m;    #W=Web only
    /This video is not available on mobile devices./m;       #bug#20800

# A "y|abc|def|" shouldn't interfere when inside a string!
$toto = " x \" string\"";
$toto = " y \" string\"";       # This is not the `y' operator!


# Tricky cases from Harald Jörg <haj@posteo.de>
$_ = "abcabc\n";
s:abc:def:g;  # FIXME: the initial s is fontified like a label, and indented

s'def'ghi'g;  # The middle ' should not end the quoting.
s"ghi"ijk"g;  # The middle ' should not end the quoting.

s#ijk#lmn#g;  # This is a regular expression substitution.

s #lmn#opq#g; # FIXME: this should be a comment starting with "#lmn"
  /lmn/rst/g; # and this is the actual regular expression
print;        # prints "rstrst\n"

given ($num) {
    when ($num>10) {
	printf "number is greater than 10\n";
    }
    when ($num<10) {
	printf "number is less than 10\n";
    }
    default {
	printf "number is equal to 10\n";
    }
}
