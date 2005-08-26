#!/usr/bin/perl

# b2m.pl - Script to convert a Babyl file to an mbox file

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
# USA.

# Maintained by Jonathan Kamens <jik@kamens.brookline.ma.us>.

# Requires CPAN modules: MailTools (for Mail::Address), TimeDate (for
# Date::Parse).

use warnings;
use strict;
use File::Basename;
use Getopt::Long;
use Mail::Address;
use Date::Parse;

my($whoami) = basename $0;
my($version) = '$Revision$';
my($usage) = "Usage: $whoami [--help] [--version] [--[no]full-headers] [Babyl-file]
\tBy default, full headers are printed.\n";

my($opt_help, $opt_version);
my($opt_full_headers) = 1;

die $usage if (! GetOptions(
			    'help' => \$opt_help,
			    'version' => \$opt_version,
			    'full-headers!' => \$opt_full_headers,
			    ));

if ($opt_help) {
    print $usage;
    exit;
}
elsif ($opt_version) {
    print "$whoami version: $version\n";
    exit;
}

die $usage if (@ARGV > 1);

$/ = "\n\037";

if (<> !~ /^BABYL OPTIONS:/) {
    die "$whoami: $ARGV is not a Babyl file\n$usage";
}

while (<>) {
    my($msg_num) = $. - 1;
    my($labels, $pruned, $full_header, $header);
    my($from_line, $from_addr);
    my($time);

    # This will strip the initial form feed, any whitespace that may
    # be following it, and then a newline
    s/^\s+//;
    # This will strip the ^_ off of the end of the message
    s/\037$//;

    if (! s/(.*)\n//) {
      malformatted:
	warn "$whoami: message $msg_num in $ARGV is malformatted\n";
	next;
    }
    $labels = $1;

    # Strip the integer indicating whether the header is pruned
    $labels =~ s/^(\d+)[,\s]*//;
    $pruned = $1;

    s/(?:((?:.+\n)+)\n*)?\*\*\* EOOH \*\*\*\n+// || goto malformatted;
    $full_header = $1;

    if (s/((?:.+\n)+)\n+//) {
	$header = $1;
    }
    else {
	# Message has no body
	$header = $_;
	$_ = '';
    }

    # "$pruned eq '0'" is different from "! $pruned".  We want to make
    # sure that we found a valid label line which explicitly indicated
    # that the header was not pruned.
    if ((! $full_header) || ($pruned eq '0')) {
	$full_header = $header;
    }

    # End message with two newlines (some mbox parsers require a blank
    # line before the next "From " line).
    s/\s+$/\n\n/;

    # Quote "^From "
    s/(^|\n)From /$1>From /g;

    # Strip extra commas and whitespace from the end
    $labels =~ s/[,\s]+$//;
    # Now collapse extra commas and whitespace in the remaining label string
    $labels =~ s/[,\s]+/, /g;

    foreach my $rmail_header qw(summary-line x-coding-system) {
	$full_header =~ s/(^|\n)$rmail_header:.*\n/$1/i;
    }

    if ($full_header =~ s/(^|\n)mail-from:\s*(From .*)\n/$1/i) {
	($from_line = $2) =~ s/\s*$/\n/;
    }
    else {
	foreach my $addr_header qw(return-path from really-from sender) {
	    if ($full_header =~ /(?:^|\n)$addr_header:\s*(.*\n(?:\B.*\n)*)/i) {
		my($addr) = Mail::Address->parse($1);
		$from_addr = $addr->address($addr);
		last;
	    }
	}

	if (! $from_addr) {
	    $from_addr = "Babyl_to_mail_by_$whoami\@localhost";
	}

	if ($full_header =~ /(?:^|\n)date:\s*(\S.*\S)/i) {
	    $time = str2time($1);
	}

	if (! $time) {
	    # No Date header or we failed to parse it
	    $time = time;
	}

	$from_line = "From " . $from_addr . " " . localtime($time) . "\n";
    }

    print($from_line, ($opt_full_headers ? $full_header : $header),
	  ($labels ? "X-Babyl-Labels: $labels\n" : ""), "\n",
	  $_) || die "$whoami: error writing to stdout: $!\n";
}

close(STDOUT) || die "$whoami: Error closing stdout: $!\n";

# arch-tag: 8c7c8ab0-721c-46d7-ba3e-139801240aa8
