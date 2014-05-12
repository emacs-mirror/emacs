#!/usr/bin/perl
# -*- eval: (bug-reference-mode 1) -*-

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


print <<"EOF1" . <<\EOF2 . s/he"llo/th'ere/;
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
