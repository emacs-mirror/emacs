#!/usr/staff/bin/perl
#
# efaq2texinfo : Convert Emacs FAQ to texinfo format
#
# George Ferguson, ferguson@cs.rochester.edu, 24 Jan 1992.
#
# Usage, assuming the list is in the file FAQ:
#	% efaq2texinfo <FAQ >efaq.texinfo
# If you store your FAQ compressed, you can use
#	% zcat FAQ.Z | efaq2texinfo >efaq.texinfo
# In any case, you then need to convert to info format:
#	% makeinfo +fill-column 80 efaq.texinfo
# The result will be files efaq-info and efaq-info-[1-4]. If you don't
# have makeinfo(1), you can try using "texinfo-format-buffer" within
# emacs.
#
# The FAQ can be obtained from pit-manager.mit.edu.
#

$chapter = 0;
$item = 0;
$itemizing = 0;
$enumerating = 0;
$displaying = 0;

# Now process each line
while (<>) {
    if ($. == 1 || /^---Continued---$/) {
	&skipJunk;
    }
    s/^ +$//;					# blanks only -> newline
    s/\t/        /;				# tabs -> spaces (stupidly)
    if (/^([^0-9 \n].*)$/) {			# new chapter heading
	&endDisplay;
	&endLists;
	$chapter += 1;
	$text = $1;
	$text =~ s/ +[-!+]$//;			# remove change marker
	$chapterTitle[$chapter] = $text;
	$item = 0;
#	print "CHAPTER $chapter: \"$text\"\n";
    } elsif (/^([0-9]+): (.*)$/) {
	&endDisplay;
	&endLists;
	$num = $1;
	$text = $2;
	$text =~ s/ +[-!+]$//;			# remove change marker
	$menu[$chapter] .= "* Question $num::\t$text\n";
	$item = $num;
	$itemTitle[$item] = "$text\n";
#	print "QUESTION $item: \"$text\"\n";
    } else {
	if (/^ +\* Topic/) {		# hack for info example
	    &addText("@example\n");
	    &addText("$_");
	    &addText("@end example\n");
	    next;
	} elsif (/^ +\*/) {		# asterisk marks list element
	    if (!$itemizing) {
		&addText("\@itemize \@bullet\n");
		$itemizing = 1;
	    }
	    &endDisplay;
	    &addText("\@item\n");
	    s/^ +\* *//;
	} elsif (/^ +[0-9]\./) {	# digit-period marks list element
	    if (!$enumerating) {
		&addText("\@enumerate\n");
		$enumerating = 1;
	    }
	    &endDisplay;
	    &addText("\@item\n");
	    s/^ +[0-9]+\. *//;
	} elsif (/^ {0,2}[^ \n]/) {	# less whitespace marks end of list
	    &endLists;
	}
	s/  //;				# automatically remove 2 spaces
	if ($itemizing) {		# adjust leading whitespace in lists
	    s/^  //;
	} elsif ($enumerating) {
	    s/^   //;
	}
	if (/^ +/ && !$displaying) {	# extra leading spaces -> display
	    &addText("\@example\n");
	    $displaying = 1;
	}
	if (/^[^ ]/ && $displaying) {	# no leading spaces -> display end
	    &addText("\@end example\n");
	    $displaying = 0;
	}
	s/ +[-!+]$//;			# remove "change" markers (sorry)
	$_ = &formatText;
	&addText($_);
    }
}
# Just in case
&endDisplay;
&endLists;

# Output it all

# Print texinfo header
print "\\input texinfo\n";
print "@setfilename efaq.info\n";
print "@settitle Frequently Asked Questions about Emacs\n";
print "\n";
print "@node Top\n";
print "@top Frequently Asked Questions about Emacs\n";
print "\n";

print $chapterText[0];
print "\@menu\n";
for ($i = 1; $i <= $chapter; $i++) {
    print "* Chapter $i::\t$chapterTitle[$i]\n";
}
print "* Index::\tHosts, usernames, files, etc.\n";
print "\@end menu\n\n";
for ($i = 1; $i <= $chapter; $i++) {
    print "\@node Chapter $i\n";
    print "\@chapter $chapterTitle[$i]\n";
    print $chapterText[$i];
    print "\@menu\n";
    print $menu[$i];
    print "\@end menu\n\n";
}
for ($i = 1; $i <= $item; $i++) {
    print "\@node Question $i\n";
    print "\@section $itemTitle[$i]";	# too many newlines already
    print $itemText[$i];
}

# Print texinfo trailer
print "\n";
print "@node Index\n";
print "@appendix Index\n";
print "\n";
print "@printindex fn\n";
print "\n";
print "@bye\n";

############################################################################

sub addText {
    if ($item == 0) {
	$chapterText[$chapter] .= "$_[0]";
    } else {
	$itemText[$item] .= "$_[0]";
    }
}

sub endLists {
    if ($itemizing) {
	&addText("\@end itemize\n");
    }
    if ($enumerating) {
	&addText("\@end enumerate\n");
    }
    $enumerating = 0;
    $itemizing = 0;
}

sub endDisplay {
    if ($displaying) {
	&addText("\@end example\n");
	$displaying = 0;
    }
}

sub skipJunk {
    local($blank) = 0;
    # skip until double newline (ie. mail headers, etc.)
    while ($_ ne "\n" || !$blank) {
	$blank = ($_ eq "\n");
	$_ = <>;
    }
    # skip next paragraph: "If you are viewing..."
    $_ = <>;
    while ($_ ne "\n") { $_ = <>; }
    # skip next paragraph: "To search for..."
    $_ = <>;
    while ($_ ne "\n") { $_ = <>; }
    # skip next paragraph: "A `+' in the..."
    $_ = <>;
    while ($_ ne "\n") { $_ = <>; }
}

#
# For each line of text in the document, we generate an index entry for
# anything that looks like a hostname, email address, newsgroup, filename
# or emacs command (only long command recognized). This function is complicated
# by the fact that the index entries have to come on lines of their own,
# after the text that contains them.
#
# To recognize host:file as a single entity, add ":?" inside the first
# nested set of brackets but before the multiple-alternatives pattern.
#
# This function also escapes any characters with special meaning to
# texinfo. Returns the line of text.
#
sub formatText {
    # convert special chars to texinfo escapes
    s/@/@@/g;
    s/\{/@{/g;
    s/\}/@}/g;
    # remove trailing spaces
    s/ +$//g;
    # gather the index entries
    local($t) = $_;
    local($tt) = "";
    while ($t =~ /([-a-zA-Z0-9]{2,}((\.|@@|%|!|-|\/)[-@a-zA-Z0-9]+){2,})/) {
	$t = $';
	$tt .= "@findex $1\n";
    }
    # Flag cross-references
    s/(q|Q)uestion ([0-9]+)/@ref\{Question $2\}/g;
    # print the line of text
    return("$_$tt");
}

__END__
