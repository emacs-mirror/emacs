#Yet Another Getopt Routine In Perl
# jgreely@cis.ohio-state.edu, 89/11/1
#usage:
#&getopt("f:bar") ||
#	die &usage("script","f:bar","oo","[files ...]");
#
sub getopt {
	local($_,$flag,$opt,$f,$r,@temp) = @_;
	@temp = split(/(.):/);
	while ($#temp >= $[) {
		$flag .= shift(@temp);
		$opt .= shift(@temp);
	}
	while ($_ = $ARGV[0], /^-(.)(.*)/ && shift(@ARGV)) {
		($f,$r) = ($1,$2);
		last if $f eq '-';
		if (index($flag,$f) >= $[) {
			eval "\$opt_$f++;";
			$r =~ /^(.)(.*)/,redo if $r ne '';
		}elsif (index($opt,$f) >= $[) {
			$r = $r eq '' ? shift(@ARGV) : $r;
			eval "\$opt_$f = \$r;";
		}else{
			print STDERR "Unrecognized switch \"-$f\".\n";
			return 0;
		}
	}
	return 1;
}

#usage: usage:
# &usage(progname,arglist,@names,@last);
#ex:
# &usage("script","f:bar","oo","[file ...]");
#would return
# "usage: script [-f oo] [-bar] [file ...]"
#
sub usage {
	local($prog,$_,@list) = @_;
	local($string,$flag,@string,@temp,@last) = ();
	@temp = split(/(.):/);
	push(@string,"usage:",$prog);
	while ($#temp >= $[) {
		if (($flag = shift(@temp)) ne '') {
			push(@string,"[-$flag]");
		}
		if (($flag = shift(@temp)) ne '') {
			push(@string,sprintf("[-%s %s]",$flag,shift(@list)));
		}
	}
	push(@string,@list) if $#list >= $[;
	return join(' ',@string) . "\n";
}
1;
