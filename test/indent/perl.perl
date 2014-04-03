#!/usr/bin/perl
# -*- eval: (bug-reference-mode 1) -*-

print <<"EOF1" . s/he"llo/th'ere/;
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
