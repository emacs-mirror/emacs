#!/usr/bin/perl
sub f1 {
        print "f1\n";
}

sub main::f2 {
        print "f2\n";
}

package Foo;

sub f3 {
        print "f3\n";
}

sub Bar::f4 {
        print "f4\n";
}

package Bar;

sub f5 {
        print "f5\n";
}

package Foo::Bar;

sub f6 {
        print "f6\n";
}

package main;

sub f7 {
        print "f7\n";
}

exit 0;
# end of Perl code
