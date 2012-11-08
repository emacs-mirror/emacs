#!/usr/bin/perl
# -*- eval: (bug-reference-mode 1) -*-

$fileType_filesButNot           # bug#12373?
    = join( '|', map { quotemeta($_).'$' } @{$fileType->{filesButNot}} );
