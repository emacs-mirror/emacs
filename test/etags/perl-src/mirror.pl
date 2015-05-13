#!/usr/bin/perl
# Make local directories mirror images of a remote sites
#
#
# Copyright (C) 1990 - 1998   Lee McLoughlin
#
# Permission to use, copy, and distribute this software and its
# documentation for any purpose with or without fee is hereby granted,
# provided that the above copyright notice appear in all copies and
# that both that copyright notice and this permission notice appear
# in supporting documentation.
#
# Permission to modify the software is granted, but not the right to
# distribute the modified code.  Modifications are to be distributed
# as patches to released version.
#
# This software is provided "as is" without express or implied warranty.
#
#
# The Debian system patched this file after installation to add:
# ls-lR file patching 2001/09/29
#	Copyright (C) 1999-2001 Ian Maclaine-cross <iml@debian.org>
# and other changes.
# Debian patches are copyright by their authors and you may use them only
# under the conditions of the General Public License in file GPL.
#
# $Id: mirror.pl,v 2.9 1998/05/29 19:01:07 lmjm Exp lmjm $
# $Log: mirror.pl,v $
# Revision 2.9  1998/05/29 19:01:07  lmjm
# Lots of changes.  See CHANGES since 2.8 file.
#
# Revision 2.8  1995/08/06  14:03:52  lmjm
# Trap a wider range of signals to aid in debugging under perl5
# Avoid looping processing symlinks.
#
# Revision 2.7  1995/08/06  13:59:00  lmjm
# No longer require socket.ph in this module.
# Escape at signs and dollars for perl5
# Make sure  proxy_gateway is at least null.
# Added ls_fix_mappings, failed_gets_excl, store_remote_listing, passive_ftp
#  and using_socks.
# Stop using dollar star as perl5 has dropped it.
# Process local directory listing before connecting to remote.
# Allow for remote_account pasword.
# Only one arg to undef, for early perl5's
# Use all capitals for file descriptors.
# Use ftp'close not ftp'quit
# Avoid file renaming under MACos
# Corrected file deleting.
#
# Revision 2.6  1994/06/10  18:28:27  lmjm
# Dropped debug statement.
#
# Revision 2.5  1994/06/06  18:39:21  lmjm
# Don't have . in the extra_path.
# Have 'internet-gateway' as the default proxy_gateway when INTERNET_HOST unset.
# Allow strip_cr (from Andrew).
# More symlink handling...
# Set type for vms correctly.
# Changed response from ftp'delete, also corrected path used.
#
# Revision 2.4  1994/04/29  20:11:09  lmjm
# Use correct variable for hostname
#
# Revision 2.3  1994/01/31  18:31:22  lmjm
# Allow for funny chars in filenames when calling the shell (Erez).
# Added compress_size_floor to avoid compressing small files (David).
# Added get_missing to just delete files not on remote system (Pieter).
# Don't try to delete old dirs if no time set (Pieter).
# Zap .dir$$ files, and keep then in $big_temp.
# Pretty print time in comparisons.
# Move the large comparision conditionals into routines (David).
# Allow for sites with limited filename lengths.
# Allow for deleted files when doing deletes.
# Don't delete dirs that are really symlinks.
#
# Revision 2.2  1993/12/14  11:09:15  lmjm
# Allow for no flock.
# Use installed socket.ph.
# Allow for system 5.
# Use percentage defaults on max_delete_*
# Checkout regexps before using.
# Allow for extra leading | in local_ignore.
# Return better exit codes.
# Fixups for recurse_hard.
# Smarter symlink handling.
#
# Revision 2.1  1993/06/28  14:59:00  lmjm
# Full 2.1 release
#
#

$#ARGV >= 0 or die("Try `man mirror` for help.\n");  

# Make sure we don't go recursive processing signals
$sigs = 0;
$max_sigs = 10;

# Am I on windoze?
$on_win = ($^O =~ /mswin/i);
$path_sep = $on_win ? ';' : ':';
$file_sep = $on_win ? '\\' : '/';
# Internally mirror uses / but when looking at names from environment allow
# for either
$file_sep_pat = $on_win ? "[\\/]" : "/"; # allow for c:\win/fred on windoze


# Default settings file loaded from a directory in PERLLIB
$defaults_file = 'mirror.defaults';
$load_defaults = 1;

# Try to find the default location of various programs via
# the users PATH then using $extra_path
if( ! $on_win ){
	$extra_path = '/usr/local/bin:/usr/new/bin:/usr/public/bin:/usr/ucb:/usr/bin:/bin:/etc:/usr/etc:/usr/local/etc';
}
if( $extra_path ne '' ){
	$ENV{ 'PATH' } .= $path_sep . $extra_path;
}

&trap_signals();

# If compressing a local file to send need somewhere to store the temp
# compressed version.
$big_temp = '/var/tmp';

# Hopefully we have flock.
$can_flock = 1;

# no debugging by default
$debug = 0;

# NOTE:
#  It is not an error for a program not to be found in the path as the user
# may be setting it as part of the package details or defaults.

# Used by the save_deletes option
$mv_prog = 'mv -f';

# compress must be able to take the -d arg to cause it to uncompress.
$sys_compress_prog = &find_prog( 'compress' );
$sys_compress_suffix = 'Z';

# Like compress gzip must be able to take -d
if( $gzip_prog = &find_prog( 'gzip' ) ){
	# Force maximum compression with gzip
	$gzip_level = ' -9';
	$gzip_prog .= $gzip_level;
	$gzip_suffix = 'gz';
	$old_gzip_suffix = 'z';
}

# For remote systems with gzipped patches to their ls-lR.gz file.  The
# gzipped patch file is a unified diff between old and new ls-lR.  The
# times file has modification times in decimal epoch seconds of the
# old and new ls-lR file on its first and second lines respectively.
if( $patch_prog = &find_prog( 'patch' ) ){
	$patch_local = '-usNT';
	$patch_UTC = '-usNZ';
}
$patch_suffix = '.pth';            # These suffices distinct
$patch_gzip_suffix = '.patch.gz';  # if truncated locally.
$times_suffix = '.times';

# A mail program that can be called as: "$mail_prog person_list'
# Can be overridden with the mail_prog keyword.
# If you use $mail_subject to pass extra arguments then remember that
# the mail program will need to know how to handle them.
$mail_prog = &find_prog( 'mailx' );
if( ! $mail_prog ){
	$mail_prog = &find_prog( 'Mail' );
}
if( ! $mail_prog ){
	$mail_prog = &find_prog( 'mail' );
}

# Used to remove directory heirarchies.  This program is passed the -rf
# arguments.
$rm_prog = &find_prog( 'rm' );

# Generate checksums
$sum_prog = &find_prog( 'sum' );

# SPECIAL NOTE: This is eval'd, so DONT put double-quotes (") in it.
# You can get local variables to appear as in the second example:
$mail_subject = '-s \'mirror update\'';
# $mail_subject = ' -s \'mirror update of $package\'';

# When scanning the local directory, how often to prod the remote
# system to keep the connection alive
$prod_interval = 60;

# Put the directory that mirror is actually in at the start of PERLLIB.
$dir = &real_dir_from_path( $0 );
unshift( @INC, $dir );

# Debian GNU/Linux stores mirror.defaults in /etc/mirror
$debian_defs = '/etc/mirror';
unshift( @INC, $debian_defs ) if -d $debian_defs;

# This, when eval'd, will get the current dir under windows NT/95
$win_getcwd = 'Win32::GetCwd';
	
# Make sure that your PERLLIB environment variable can get you
# all these or that they are installed.
require 'ftp.pl';
require 'lsparse.pl';
require 'dateconv.pl';

# Find some local details
# The current directory
$home = &cwd();
# The hostname
$hostname_cmd = &find_prog( 'hostname' );
if( $hostname_cmd ne '' ){
	chop( $hostname = `$hostname_cmd` );
}
if( $hostname eq '' ){
	$hostname_cmd = &find_prog( 'uname' );
	if( $hostname_cmd ne '' ){
		chop( $hostname = `$hostname_cmd -n` );
		if( $hostname eq '' ){
			chop( $hostname = `$hostname_cmd -l` );
		}
	}
}
if( $hostname eq '' ){
	$hostname = 'localhost';
}

if( $hn = (gethostbyname( "$hostname" ))[ 0 ] ){
	$hostname = $hn;
}

# Some systems hold the username in $USER, some in $LOGNAME.
$me = $ENV{'USER'} || $ENV{'LOGNAME'};

# Files matching this pattern are usually compressed.
$squished = '\.(Z|z|gz)$';

# special /bin/sh chars that must be escaped.
$shell_metachars = '\"|\$|`|\\\\';

# Remote directory parsing fail if not given input every readtime seconds.
$parse_time = 600;

# Timeout are not fatal unless you get more than this number of them.
$max_timeouts = 20;

# If connected to a site then this holds the site name.
$connected = '';

# Umask setting.
$curr_umask = sprintf( "0%o", umask );

# mapping from a pathname to a number - just to make the keys to assoc arrays
# shorter.
$map_init = 1;	# just so I know 0 is invalid

$tmp = "/tmp";
@assocs = ( 'local_map', 'remote_map' );

# A reasonable set of defaults
#   these are in the same order as the documentation - except where order is
#   important
$default{ 'hostname' } = $hostname; # The LOCAL hostname

$default{ 'package' } = '';	# should be a unique handle for the "lump" to be mirrored
$default{ 'site' } = '';	# site to connect to
$default{ 'remote_dir' } = '';	# remote directory to mirror
$default{ 'local_dir' } = '';	# local directory to copy into

$default{ 'remote_user' } = 'anonymous';  # the remote guest account name
$default{ 'remote_password' } = "$me\@$hostname";
$default{ 'remote_account' } = ''; # remote account name/passwd (for systems
				   # that use it.)
# Used for group and gpass.  (As in ftp.1 site group/gpass commands.)
$default{ 'remote_group' } = '';
$default{ 'remote_gpass' } = '';
$default{ 'timeout' } = 120;	# timeout ftp requests after this many seconds
$default{ 'failed_gets_excl' } = ''; # failed messages to ignore while getting,
				# if failed to ftp'get
$default{ 'ftp_port' } = 21;	# port number of remote ftp daemon
$default{ 'proxy' } = 0;	# normally use regular ftp
$default{ 'proxy_ftp_port' } = 4514; # default from Sun
$default{ 'proxy_gateway' } = "$ENV{ 'INTERNET_HOST' }";
				# Gateway to use for proxy
				# (Null if environment not set.)
$default{ 'using_socks' } = 0;	# Set the default perl version to the non-SOCKS one.
$default{ 'passive_ftp' } = 0;	# Set the default ftp usage not to use passive (PASV) ftp.
$default{ 'retry_call' } = 1;	# Retry the call if it fails first time
$default{ 'disconnect' } = 0;	# Force close at end of package EVEN if
				# next package is to the same site
$default{ 'remote_idle' } = '';	# Set the remote idle timer to this

$default{ 'get_patt' } = ".";	# regex of pathnames to retrieve
$default{ 'exclude_patt' } = ''; # regex of pathnames to ignore
$default{ 'local_ignore' } = ''; # regex of local pathnames to totally ignore
$default{ 'get_newer' } = 1;	# get remote file if its date is newer than local
$default{ 'get_size_change' } = 1; # get the file if size if different than local
$default{ 'make_bad_symlinks' } = 0; # prevent symlinks to non-existant files
$default{ 'follow_local_symlinks' } = ''; # Follow symlinks to pathnames matching this regexp.
$default{ 'get_symlink_files' } = 0; # If true gets file and makes symlink otherwise bad. 
$default{ 'get_missing' } = 1;	# Set get_missing to 0 to just delete files not on remote system
$default{ 'get_file' } = 1;	# perform get, not put by default
$default{ 'text_mode' } = 0;	# transfer in binary mode by default
$default{ 'strip_cr' } = 0;	# Delete \r (usefull when transfering from
				# mainframes -- set text_mode and strip_cr)
$default{ 'vms_keep_versions' } = 1; # Keep multiple VMS versions
$default{ 'vms_xfer_text' } = 'readme$|info$|listing$|\.c$';
				# pattern of VMS files to xfer in TEXT mode
				# (Case insensitive)
$default{ 'name_mappings' } = '';# remote to local pathname mappings
				# used to change layout or zap weird chars
	 			# (eg s:old:new)
$default{ 'external_mapping' } = '';# remote to local mapping by external routine
$default{ 'update_local' } = 0;	# Don't just update local dirs
$default{ 'max_days' } = 0;	# Ignore age of file
$default{ 'max_size' } = 0;	# If non-zero dont get files larger than this
$default{ 'chmod' } = 1;	# perform a chmod after an xfer

$default{ 'user' } = '';	# UID/user name to give to local pathnames
$default{ 'group' } = '';	# GID/group name to give to local pathnames
$default{ 'mode_copy' } = 0;	# true indicates to copy the mode bits
$default{ 'file_mode' } = 0444;	# Mode to give files created locally
$default{ 'dir_mode' } = 0755;	# mode to give directories created locally
$default{ 'force' } = 0;	# don't force by default
$default{ 'umask' } = 07000; # DONT allow setuid things by default
$default{ 'use_timelocal' }  = 1; # Use local time NOT gmt to timestamp files. (See also the -T flag.)
$default{ 'force_times' } = 1;	# Force local file times to match the original

$default{ 'do_deletes' } = 0;	# delete dest files if not in src tree
$default{ 'delete_patt' } = '.';# delete only files which match this pattern
$default{ 'delete_get_patt' } = 0;# true: set delete_patt to get_patt
$default{ 'delete_excl' } = ''; # regex of local pathnames to ignore when deleting
$default{ 'max_delete_files' } = '10%'; # Any more than this and DONT delete
$default{ 'max_delete_dirs' } = '10%'; # Any more than this and DONT delete
$default{ 'save_deletes' } = 0;	# save local files if not in remote
$default{ 'save_dir' } = 'Old';	# directory in which to create tree for keeping
				# files no longer in remote
$default{ 'store_remote_listing' } = ''; # Where to store remote site listings on local system

$default{ 'compress_patt' } = ''; # compress files matching this pattern
$default{ 'compress_excl' } = $squished; # dont compress regexp (case insensitive)
$default{ 'compress_prog' } = $sys_compress_prog; # Program to compress files.
$default{ 'compress_suffix' } = $sys_compress_suffix; # Suffix on compressed files
$default{ 'compress_conv_patt' } = '(\.Z|\.taz)$';
	# compress->gzip files matching this pattern
$default{ 'compress_conv_expr' } = 's/\.Z$/.gz/;s/\.taz$/.tgz/';
	# perl expressions to convert names of files from compress->gzip
$default{ 'compress_size_floor' } = 0;  # don't compress files < this size

$default{ 'split_max' } = 0;	# Files > this size can be split up.
$default{ 'split_patt' } = '';  # Files must match this pattern to be split
$default{ 'split_chunk' } = 100 * 1024; # Size of split-up chunks

$default{ 'remote_fs' } = 'unix'; # Remote filestore
	# Other posibilies dls, netware and vms
$default{ 'ls_lR_file' } = '';	# remote file containing ls-lR, patch or 
				# times - else use remote ls
$default{ 'local_ls_lR_file' } = ''; # local file containing ls-lR
				# used when first copying a large remote package
				# or ls_lR_file is a remote ls-lR patch or times
$default{ 'recursive' } = 1;	# true indicates to do recursive processing
$default{ 'recurse_hard' } = 0;	# true indicates have to cwd+ls for each remote
				# subdirectory - AVOID wherever possible.
$default{ 'flags_recursive' } = '-lRat'; # Flags passed to remote dir
$default{ 'flags_nonrecursive' } = '-lat'; # Flags passed to remote dir
$default{ 'ls_fix_mappings' } = '';# Correct pathnames in remote listings
				# (eg s:usr/spool/pub/::) to match reality

$default{ 'update_log' } = '';	# Filename where an update report is to be kept
$default{ 'mail_to' } = '';	# Mail a report to these addresses
$default{ 'mail_prog' } = $mail_prog; # the mail program (see $mail_prog)
$default{ 'mail_subject' } = $mail_subject; # Subject passed to mail_prog

$default{ 'comment' } = '';	# General comment used in report
# If mirroring a VERY large directory then it is best to put the assoc
# arrays in files (use command line switch -F. to turn on).
$default{ 'use_files' }  = 0;
$default{ 'interactive' } = 0;	# noninteractive copy default
$default{ 'skip' } = '';	# If set then skip this entry giving value as reason
$default{ 'verbose' } = 0;	# Verbose messages
$default{ 'algorithm' } = 0;	# The mirror algorithm to use
$default{ 'local_dir_check' } = 0; # Check local_dir exists before mirroring?

# I really want to drop this option.
$default{ 'delete_source' } = 0;# delete source after xfer (default = NO!!!)

@boolean_values = ( 'get_newer', 'get_size_change', 'do_deletes',
	'update_local',	'force_times', 'retry_call', 'recursive',
	'mode_copy', 'disconnect', 'interactive', 'text_mode',
	'force', 'get_file', 'verbose', 'proxy', 'delete_get_patt',
	'delete_source', 'save_deletes', 'use_files', 'use_timelocal',
	'make_bad_symlinks', 'get_symlink_files', 'recurse_hard', 
	'get_missing', 'strip_cr', 'passive_ftp', 'using_socks', 
	'local_dir_check' );

%boolean_values = ();
&set_assoc_from_array( *boolean_values );

@regexp_values = ( 'get_patt', 'exclude_patt', 'local_ignore',
		  'delete_patt', 'delete_excl', 'split_patt', 'save_deletes',
		  'compress_patt', 'compress_excl', 'compress_conv_patt',
		  'failed_gets_excl', 'store_remote_listing' );

#
# message levels used by &msg( level, msg )
# if you call msg as &msg( msg ) the level is presumed to be just $pr.
$pr = 0;	# Always print out messages
$log = 1;	# push this messages onto @log

# The max number of directory failures under algorithm 1 before giving up.
$max_failed_dirs = 5;

#
# Exit status
$exit_status = 0;
$exit_status_xfers = 0;

# "#defines" for the above
$exit_xfers = 16;  # Add this to the exit code to show xfers took place
$exit_ok = 0;
$exit_fail = 1;
$exit_fail_noconnect = 2;

# -d		Turn on debugging - more -d's means more debugging.
# -ppattern	Just do packages matching pattern.
# -Rpattern	Skip till the first package name matches pattern then do all.
#		it and following packages.
# -n		Do nothing, just show what would be done.
# -F		Use files for assoc arrays (see also the variable use_files).
# -gsite:path
#		Get all files on given site.  If path matches .*/.+ then
#		it is the name of the directory and the last part is the
#		pattern of filenames to get.  If path matches .*/ then
#		it is the name of a directory and all its contents are retrieved.
#		Otherwise path is the pattern to be used in '/'.
# -r		Same as "-krecursive=false".
# -kvar=val	set variable to value.
# -uusername	Same as "-kremote_user=username", prompts for remote_password.
# -v		Print version and exit.
# -T		Dont transfer just force local timestamps to match remote.
# -N		Don't load mirror.defaults.
# -L		Generate a pretty list of what is being mirrored.
# -m 		Same as "-kmode_copy=true".

# -Cconfig_file
# -P 		Same as "-kget_file=false -kinteractive=true".
# -G		Same as "-kget_file=true -kinteractive=true".
# -t 		Same as "-ktext_mode=true".
# -f		Same as "-kforce=true".
# -sSITENAME	Same as "-ksite=SITENAME.
# -ULOGFILE	Set the upload log to LOGILE - if none given uses
#		the file $home/upload_log.$mday.$mon.$year

# -DUMP		Dump perl - to be later undumped --  THIS DOES NOT YET WORK!!!

sub msg_version
{
	&msg( '$Id: mirror.pl,v 2.9 1998/05/29 19:01:07 lmjm Exp lmjm $' . "\n" );
	&msg( 'Debian patch version: mirror (2.9-38)  Tue Jan 29 07:06:25 2002 UTC.' . "\n" );
	&msg( 'Copyright conditions are in file /usr/share/doc/mirror/copyright.' . "\n" );
}

parse_args:
while( $ARGV[ 0 ] =~ /^-/ ){
	local( $arg ) = shift;

	if( $arg eq '-d' ){
		if( $debug == 2 ){
			&msg_version();
		}
		$| = 1;
		$debug++;
		next;
	}

	if( $arg =~ /^-(p)(.*)/ || $arg =~ /^-(R)(.*)/ ){
		local( $flag, $p ) = ($1, $2);
		if( $flag eq 'R' ){
			# Skip all packages till a match is made
			# then process ALL further packages
			$skip_till = 1;
		}
		if( ! $p ){
			# Must be -p/-R space arg
			$p = shift;
		}
		if( $p !~ /[a-zA-Z0-9]/ ){
			die "Invalid package name to -p of: $p\n";
			next;
		}
		# Only mirror the named packages
		$do_packages{ $p } = 1;
		$limit_packages = 1;
		next;
	}

	if( $arg eq '-n' ){
		# Do nothing - just show what would be done
		$dont_do = 1;
		$debug += 2;
		$| = 1;
		next;
	}

	if( $arg eq '-F' ){
		# Use files for the dir listings assoc lookups
		$use_files = 1;
		$command_line{ 'use_files' } = 1;
		next;
	}

	if( $arg eq '-T' ){
		# Don't actually get any files but just force
		# local timestamps to be the same on the remote system
		$timestamp = 1;
		$command_line{ 'force_times' } = 'true';
		next;
	}

	if( $arg =~ /^-g(.*)$/ ){
		# the next arg is the site:path to get
		local( $site_path ) = $1;

		if( ! $site_path ){
			# Must be -g space arg
			$site_path = shift;
		}
		
		# DONT use the system defaults!
		$load_defaults = 0;
		
		# This is probably interactive so print interactively
		$| = 1;
		
		if( $site_path =~ /(.*):(.*)?/ ){
			local( $site, $path ) = ($1, $2);
			push( @get_sites, $site );
			# Find the directory and files
			if( $path =~ m|^(.*)/([^/]*)$| ){
				if( $1 eq '' && $2 eq '' ){
					push( @get_paths, '/' );
					push( @get_patt, '.' );
				}
				elsif( $1 eq '' ){
					push( @get_paths, '/' );
				}
				else {
					push( @get_paths, $1 );
				}
				if( $2 eq '' ){
					push( @get_patt, '.' );
				}
				else {
					push( @get_patt, "^$2\$" );
				}
			}
			else {
				push( @get_paths, '.' );
				push( @get_patt, "^$path\$" );
			}
		}
		else {
			die "expected -gsite:path got $arg";
		}
		next;
	}

	if( $arg eq "-r" ){
		# no recursive copy
		$command_line{ 'recursive' } = 0;
		next;
	}
# Debian bug #93853, -k keyword=value did not work, jkn@softavenue.fi 
	if( $arg =~ /^-k(.*)/ ){
	        local( $key_val ) = $1;
		if( ! $key_val ){
			# Must be -k space key=val
			$key_val = shift;
		}
		if( $key_val =~ /(.*)=(.*)/ ){ 
		        # set the keyword = value
		        if( !defined( $default{ "$1" } ) ){
			    warn "Invalid keyword $1\n";
			} else {
			    $command_line{ "$1" } = $2;
			}
		}
		next;
	}

	if( $arg =~ /^-u(.*)/ ){
		local( $user ) = $1;

		if( ! $user ){
			# must be -u space user
			$user = shift;
		}

		# override the user name
	        $command_line{ 'remote_user' } = $user;
		# and ask for a password
		$command_line{ 'remote_password' } = &get_passwd( $user );
		next;
	}

	if( $arg eq '-N' ){
		$load_defaults = 0;
		next;
	}

	if( $arg eq '-v' ){
		&msg_version();
		exit( 0 );
	}

        if( $arg eq '-L' ){
                # Generate a pretty list of what is being mirrored
                $pretty_print = 1;
                next;
        }

        if( $arg eq '-m' ){
                # propagate the mode
		$command_line{ 'mode_copy' } = 'true';
		next;
        }

	# Old command line interface flags
	if( $arg =~ /^-C(.*)/ ){
		# specify the config file
		local( $c ) = $1;
		if( $c !~ /./ ){
			die "Must give config file name -Cname ($arg)\n";
		}
		# Only mirror the named packages
	        push( @config_files, $c);
		next;
	}

        if( $arg eq '-P' ){
                # put files
		$command_line{ 'get_file' } = 'false';
		$command_line{ 'interactive' } = 'true';
		next;
        }

        if( $arg eq '-G' ){
                # get files
		$command_line{ 'get_file' } = 'true';
		$command_line{ 'interactive' } = 'true';
		next;
        }

        if( $arg eq '-t' ){
                # set the file mode to text
		$command_line{ 'text_mode' } = 'true';
		next;
        }

        if( $arg eq '-f' ){
                # force file transfers irregardless of date/size matches
		$command_line{ 'force' } = 'true';
		next;
        }

	if( $arg =~ /^-s(.*)/ ){
		# override the site name
		$command_line{ 'site' } = $1;
		next;
	}

	if( $arg =~ /^-U(.*)/ ){
		$upload_log = $1;
		if( $upload_log eq '' ){
			local( $sec,$min,$hour,$mday,$mon,$year,
				$wday,$yday,$isdst ) 
				= localtime( time );
			$mon++;
			$upload_log = "$home/upload_log.$mday.$mon.$year";
		}
			
		next;
	}

	if( $arg eq '-DUMP' ){
		# THIS DOES NOT YET WORK!!!!!
		$dumped_version = 1;
		warn "Dumping perl\n";
		dump parse_args;
	}

	warn "Unknown arg $arg, skipping\n";
}

# Handle multiline buffers in a sane way
# This is deprecated in perl-5.  Someone should add "/m" modifiers to any
# regexps that *really* need it, not all.
# $* = 1;

$interactive = $command_line{ 'interactive' };

if( ! $interactive ){
	local( $c );

	# The remainder of ARGV are package names
	foreach $c ( @ARGV ){
		push( @config_files, $c );
	}
}

if( $interactive && $limit_packages){
	die "Can not mix -p and interactive";
}

$value{ 'remote_user' } = $default{ 'remote_user' };
%value = ();
&set_defaults();

if( $load_defaults ){
	local( $dir, $mp );
	foreach $dir ( @INC ){
		local( $f ) = "$dir/$defaults_file";
		if( -f $f ){
			$mp = $f;
			last;
		}
	}
	if( $mp ){
		&msg( "defaults from $mp\n" ) if $debug > 2;
		splice( @config_files, 0, 0, $mp );
	}
	else {
		warn "No $defaults_file found in perl library path\n";
	}
}
elsif( $debug > 1 ){
	&msg( "not loading $defaults_file\n" );
}
		

&interpret_config_files();

# Shut down any remaining ftp session
&disconnect();

&msg( "All done, Exiting\n" ) if $debug;
exit( $exit_status + $exit_status_xfers );


$key = ''; # The current keyword
$value = ''; # the value for the keyword

sub interpret_config_files
{
	local( $fname );

	if( $#get_sites >= 0 ){
		while( $#get_sites >= 0 ){
			$value{ 'site' } = pop( @get_sites );
			$value{ 'remote_dir' } = pop( @get_paths );
			$value{ 'get_patt' } = pop( @get_patt );
			$value{ 'local_dir' } = '.';
			$value{ 'remote_user' } = 'anonymous';
			$exit_status = &do_mirror();
		}
		return;
	}
		

	if( $command_line{ 'interactive' } ){
		# No config file to read
		$value{ 'package' } = 'interactive';
		$exit_status = &do_mirror();
		return;
	}

	# if no configuration files were specified use standard input
	@ARGV = @config_files;
	&interpret_config();
}

sub interpret_config
{
	while( <> ){
		# Ignore comment and blank lines
		next if /^\s*#/ || /^\s*$/;
		
		&parse_line();
		
		# Is this a new package?
		if( $value{ 'package' } && $key eq 'package' ){
			# mirror the existing package
			$exit_status = &do_mirror();
			
			# reset
			&set_defaults();

			# Make sure I'm at the right place for <> to work!
			chdir $home;
		}
		
		if( $debug > 3 ){
			&msg( "$key \"$value\"\n" );
		}

		$value{ $key } = $value;

		# do an explicit close for each file so $. gets reset
		if( eof( ARGV ) ){
			if( $debug > 3 ){
				&msg( "-- end of config file \"$ARGV\"\n" );
			}
			close( ARGV );
		}
	}

	# Mirror the last package in the file
	if( $value{ 'package' } ){
		$exit_status = &do_mirror();
	}
}

# parse each line for keyword=value
sub parse_line
{
	local( $eqpl );
	local( $cont ) = '&';

	chop;
	if( /^\s*([^\s=+]+)\s*([=+])(.*)?$/ ){
		($key, $eqpl, $value) = ($1, $2, $3);
		# If the value ends in the continuation character then
		# tag the next line on the end (ignoring any leading ws).
		while( $value =~ /^(.*)$cont$/o && !eof ){
			$_ = <>;
			local( $v ) = $1;
			if( /^\s*(.*)$/ ){
				$value = $v . $1;
			}
		}
		if( $debug > 3 ){
			&msg( "read: $key$eqpl$value\n" );
		}
	}
	else {
		warn "unknown input in \"$ARGV\" line $. of: $_\n";
	}
	if( ! defined( $default{ "$key" } ) ){
		die "unknown keyword in \"$ARGV\" line $. of: $key\n";
	}
	if( $eqpl eq '+' ){
		$value = $value{ $key } . $value;
	}
}

# Initialise the key values to the default settings
sub set_defaults
{
	%value = %default;
	undef( $uid );
	undef( $gid );
}

# Override the current settings with command line values
sub command_line_override
{
	local( $key, $val, $overrides );

	while( ($key, $val) = each %command_line ){
		$overrides++;
		if( $boolean_values{ $key } ){
			# a boolean value
			$value{ $key } = &istrue( $val );
		} else {
			# not a boolean value
			$value{ $key } = $val;
		}
	}

	if( $debug > 4 ){
		if( $overrides ){
			&pr_variables( "keywords after command line override\n" );
		}
		else {
			&msg( "No command line overrides\n" );
		}
	}
}

# set each variable $key = $value{ $key }
sub set_variables
{
	local( $key, $val );

	while( ($key, $val) = each %value ){
		# for things like passwords it is nice to have the
		# real value in a file
		if( $val =~ /^\<(.*)$/ ){
			local( $val_name ) = $1;
			open( VAL_FILE, $val_name ) ||
				die "can't open value file $val_name\n";
			$val = <VAL_FILE>;
			close( VAL_FILE );
			chop $val if $val =~ /\n$/;
		}

		if( $boolean_values{ $key } ){
			# a boolean value
			eval "\$$key = &istrue( $val )";
		}
		else {
			# not a boolan value
			# Change all \ to \\ since \'s will be escaped in
			# the following string used in the eval.
			$val =~ s/([^\\])(')/$1\\$2/g;
			eval "\$$key = '$val'";
		}
		if( $key eq 'compress_prog' ){
			if( $val eq 'compress' ){
				$compress_prog = $sys_compress_prog;
				$compress_suffix = $sys_compress_suffix;
			}
			elsif( $val eq 'gzip' ){
				if( ! $gzip_prog ){
					die "Trying to use gzip but not found in PATH\n";
				}
				$compress_prog = $gzip_prog;
				$compress_suffix = $gzip_suffix;
			}
			elsif( $debug > 2 && $compress_prog ne $gzip_prog &&
			       $compress_prog ne $sys_compress_prog ){
				&msg( "compress_prog ($compress_prog) not compress or gzip, presuming program name\n" .
				      "- user must set compress_suffix\n" );
			}
			&upd_val( 'compress_prog' );
			&upd_val( 'compress_suffix' );
		}
	}
	if( $compress_patt ne '' && $compress_prog eq '' ){
		&msg( "compress_patt set but no compress_prog so compress_patt reset to nothing" );
		$compress_patt = '';
	}
		

	# Reset the umask if needed.
	# Do it here to try and get it done as early as possible.
	# If the user doesn't use octal umasks this will cause umask
	# to be called again unnecessarily - but that is pretty cheap.
	if( $umask && $umask != $curr_umask ){
		local( $val ) = $umask;
		$val = oct( $val ) if $val =~ /^0/;
		umask( $val );
		$curr_umask = sprintf( "0%o", umask );
	}

	&map_user_group();
}

sub upd_val
{
	local( $key ) = @_;
	if( $package eq 'defaults' ){
		$default{ $key } = $value{ $key };
	}
}

sub pr_variables
{
	local( $msg ) = @_;
	local( $nle ) = 60;
	local( $out ) = 0;
	local( $key, $val, $str );

	&msg( $msg );
	if( $get_file ){
		&msg( "package=$package  $site:$remote_dir -> $local_dir\n\t" );
	}
	else {
		&msg( "package=$package  $local_dir -> $site:$remote_dir\n\t" );
	}

	for $key ( sort keys( %value ) ){
		next if $key eq 'package' ||
			$key eq 'site' ||
			$key eq 'remote_dir' ||
			# Don't show passwords when interactive
			($interactive && $key eq 'remote_password') ||
			($interactive && $key eq 'remote_gpass');
		# Report the value in the actual variable
		$val = eval "\$$key";
		$str = "$key=\"$val\" ";
		&msg( $str );
		$out += length( $str );
		# Output newlines when a line is full
		if( $out > $nle ){
			$out = 0;
			&msg( "\n\t" );
		}
	}
	&msg( "\n" );
}

# Mirror the package, return exit_status
sub do_mirror
{
	local( $get_one_package ) = 0;

	$package = $value{ 'package' };
	
	if( $package eq 'defaults' ){
		# This isn't a real site - just a way to change the defaults
		%default = %value;

		return $exit_ok;
	}

	# Only do this package if given by a -Ppack argument
	if( $limit_packages && ! $do_packages{ $package } ){
		return;
	}

	if( $skip_till ){
		# Found a package so process all packages from now on
		$skip_till = $limit_packages = 0;
	}
	
	local( $exit_status ) = $exit_fail_noconnect;  # Presume the worse.
	$timeouts = 0;

	# set things from the command line arguments
	&command_line_override();

	if( ! &checkout_regexps() ){
		&msg( "skipping package\n\n" );
		return $exit_status;
	}

	# set each variable $key = $value{ $key }
	&set_variables();

	# don't trash locally glossed over things with stuff from the remote
	if( $local_ignore ){
		if( $exclude_patt ){
			$exclude_patt .= '|' . $local_ignore;
		}
		else {
			$exclude_patt = $local_ignore;
		}
	}

	if( $debug > 3 ){
		&pr_variables( "\n" );
	}
	elsif( $package && ! $pretty_print ){
		if( $get_patt ){
			&msg( "package=$package $site:$remote_dir -> $local_dir\n");
		}
		else {
			&msg( "package=$package $local_dir -> $site:$remote_dir\n" );
		}
		&msg( "algorithm=$algorithm\n") if $algorithm != 0;
	}
	
	# Don't bother if trying to mirror here!
	if( !$interactive && !$force && ((gethostbyname( $site ))[0] eq $hostname) ){
		&msg( "Skipping $site as it is this local site!\n\n" );
		return $exit_ok;
	}

	chdir $home;

	$max_age = 0;
	if( $value{ 'max_days' } ne '0' ){
		$max_age = time - ($value{ 'max_days' } * 24 * 60 * 60);
		&msg( "max_age = $max_age\n" ) if $debug > 1;
	}

	# pull in external code, if required
	if( $external_mapping ){
		&msg( "Loading external mapping from $external_mapping.\n" ) if $debug > 0 ;
		do $external_mapping || die "Cannot load from $external_mapping";
	}

	if( $debug ){
		# Keep the ftp debugging lower than the rest.
		&ftp'debug( $debug - 1);
	}
	else {
		&ftp'debug( $verbose );
	}

	if( $recurse_hard ){
		$recursive = 1;
	}
	if( $algorithm == 1 ){
		$recursive = 0;
		$make_bad_symlinks = 1;
		$rem_start_len = length( $remote_dir );
	}

	if( ! $interactive ){
		$ftp'showfd = 'STDOUT';
	}
	&ftp'set_timeout( $timeout );
	&ftp'set_signals( "main'msg" );

	# set passive ftp mode
	if( $passive_ftp ){
		$ftp'use_pasv = 1;
	}

	# Are we using the SOCKS version of perl?
	if( $using_socks ){
		$chat'using_socks = 1;
	}

	# Useful string in prints
	$XFER = $get_file ? "get" : "put";

	# create the list of items to copy
	@transfer_list = ();
	if( $interactive ){
		if( $algorithm == 1 ){
			warn "Cannot use algorithm 1 with interactive, using 0\n";
			$algorithm = 0;
		}
		# copy the remainder of items from argv to the transfer list
		while( @ARGV ){
			# copy the local directory
			if( @ARGV ){
				push( @transfer_list, shift( @ARGV ) );
			} 
	
			# copy the remote directory
			if( @ARGV ){
				push( @transfer_list, shift( @ARGV ) );
			}
			else {
				die "remote directory must be specified\n";
			}
	
			# copy the pattern, if available
			if( @ARGV ){
				push( @transfer_list, shift( @ARGV ) );
			} else {
				push( @transfer_list, $default{ 'get_patt' } );
			}
		}
	
		if( $debug > 1 ){
			local( @t );
			@t  = @transfer_list;
	
			while( @t ){
				printf( "local_dir=%s remote_dir=%s patt=%s\n",
					shift( @t ), shift( @t ), shift( @t ) );
			}
		}
	}
	else {
		push( @transfer_list, $local_dir );
		push( @transfer_list, $remote_dir );
		push( @transfer_list, $get_patt );
		if( $algorithm != 1 ){
			$get_one_package = 1;
		}
        }
		

	if( $update_local && $get_patt ){
		if( $get_patt ne $default{ 'get_patt' } ){
			&msg( "Cannot mix get_patt and update_local.  get_patt ignored\n" );
		}
		$get_patt = '';
	}

	
	if( !$site || (!$interactive && (!$local_dir || !$remote_dir)) ){
		&msg( "Insufficient details for package to be fetched\n" );
		&msg( "Must give at least: site, remote_user, remote_dir and local_dir\n\n" );
		return $exit_status;
	}

        if( $pretty_print ){
                # Don't actually mirror just print a pretty list
                # of what would be mirrored.  This is for mailing to
                # people
		if( $skip ){
			return $exit_ok;
		}
                &msg( "$package  \"$comment\"\n" );
                &msg( "  $site:$remote_dir  -->  $local_dir\n\n" );
                return $exit_ok;
        }

	if( $skip ){
		&msg( "Skipping $site:$package because $skip\n\n" );
		return $exit_ok;
	}

	$split_max = &to_bytes( $split_max );
	$split_chunk = &to_bytes( $split_chunk );

	if( $split_max && $split_max <= $split_chunk ){
		&msg( "split_max <= split_chunk - skipping package\n" );
		&msg( " $split_max <= $split_chunk\n\n" );
		return $exit_status;
	}

	if( $split_chunk && ($split_chunk & 511) ){
		&msg( "split_chunk bad size - skipping package\n" );
		&msg( " $split_chunk should be a multiple of 512 bytes\n\n" );
		return $exit_status;
	}

	if( $local_dir_check && ! -d $local_dir ){
		&msg( "local_dir $local_dir does not exist - skipping package\n" );
		return $exit_status;
	}

	if( $get_one_package && $algorithm != 1 ){
    		# If only getting one package may as well parse the
		# local directory listings before connecting to the
		# remote site.  (With the status_file stuff this info
		# can then be reused if something goes wrong.)
		if( $use_files ){
			&create_assocs();
		}

		if( !&get_local_directory_details() ){
			&msg( "Cannot get local directory details ($local_dir)\n" );
			&disconnect();
			&msg( "\n" );
			return $exit_status;
		}
	}

	local( $con ) = &connect();
	if( $con <= 0 ){
		&msg( "Cannot connect, skipping package\n" );
		&disconnect();
		&msg( "\n" );
		return $exit_status;
	}

	if( $con == 1 ){
		&msg( "login as $remote_user\n" ) if $debug > 1;
		$curr_remote_user = $remote_user;
		if( ! &ftp'login( $remote_user, $remote_password, $remote_account ) ){
			&msg( "Cannot login, skipping package\n" );
			&disconnect();
			&msg( "\n" );
			return $exit_status;
		}
		$can_restart = (&ftp'restart(0) == 1);
		if( $debug > 1 ){
			&msg( "Can " . ($can_restart ? '' : "not ") . "do restarts\n" );

		}
	}
	else {
		# Already connected to this site - so no need to login again
		&msg( "Already connected to site $site\n" ) if $debug;
	}

	if( ! &ftp'type( $text_mode ? 'A' : 'I' ) ){
		&msg( "Cannot set type\n" );
	}

	$exit_status = $exit_fail; # ok this is now the worse case

	# Mirror thinks in terms of Unix pathnames.
	# Ask ftp.pl to map any remote name it is about to use by
	# setting the namemap functions.
	if( $remote_fs =~ /vms/i ){
		$vms = 1;
		&ftp'set_namemap( "main'unix2vms", "main'vms2unix" );
	}
	else {
		$vms = 0;
		# No mapping necessary
		&ftp'set_namemap( '' );
	}

	if( ! $get_file || $remote_idle ){
		local( @rhelp ) = &ftp'site_commands();
		$remote_has_chmod = grep( $_ eq 'CHMOD', @rhelp);
		$remote_has_rename = grep( $_ eq 'RNFR', @rhelp) && grep( $_ eq 'RNTO', @rhelp);
		$remote_has_idle = grep( $_ eq 'IDLE', @rhelp);
		if( $debug > 2 ){
			&msg( "remote site " . ($remote_has_chmod ? "has" : "hasn't") . " got chmod\n" );
			&msg( "remote site " . ($remote_has_idle ? "has" : "hasn't") . " got idle\n" );
		}
	}
	
	if( $remote_has_idle && $remote_idle ){
		if( ! &ftp'quote( "site idle $remote_idle" ) ){
			&msg( "Cannot set remote idle\n" );
		}
		elsif( $debug > 2 ){
		 	&msg( "remote idle has been set to $remote_idle\n" );
		}
	}

	if( $remote_group ){
		if( ! &ftp'quote( "site group $remote_group" ) ){
			&msg( "Cannot set remote group\n" );
		}
		elsif( $debug > 2 ){
		 	&msg( "remote group has been set to $remote_group\n" );
		}
	}
	
	if( $remote_gpass ){
		if( ! &ftp'quote( "site gpass $remote_gpass" ) ){
			&msg( "Cannot set remote gpass\n" );
		}
		elsif( $debug > 2 ){
		 	&msg( "remote gpass has been set\n" );
		}
	}

	@log = ();
	$cannot = 0;

	local( @sub_dirs );
	while( @transfer_list ){
		# get files
		$local_dir = shift( @transfer_list );
		$remote_dir = shift( @transfer_list );
		$get_patt = shift( @transfer_list );

		# Clear all details
		undef( @xfer_dest );
		undef( @xfer_src );
		undef( @xfer_attribs );
		undef( @things_to_make );
		undef( @sub_dirs );

		if( ! $get_one_package ){
			if( $use_files ){
				&create_assocs();
			}

			if( !&get_local_directory_details() ){
				&msg( "Cannot get local directory details ($local_dir)\n" );
				&disconnect();
				&msg( "\n" );
				return $exit_status;
			}
		}

		# Create a get_patt from the contents of the local directory
		if( $update_local && $#get_top >= 0 ){
			$get_patt = '^' . join( '|^', @get_top );
			$get_patt =~ s/$squished//g;
			&msg( "get_patt = $get_patt\n" ) if $debug;
		}
	
		if( !&get_remote_directory_details() ){
			&msg( "Cannot get remote directory details ($remote_dir)\n" );
			if( $algorithm == 1 ){
				# Skip this directory.
				$cannot++;
				if( $cannot < $max_failed_dirs ){
					next;
				}
				# Too many failed directories.  Fall thru'
				# into disconnect.
			}
			&disconnect();
			&msg( "\n" );
			return $exit_status;
		}
	
		if( $get_file ){
			&compare_dirs(
				*remote_sorted,
				 *remote_map, *remote_time,
				  *remote_size, *remote_type,
				*local_sorted,
				 *local_map, *local_time,
				  *local_size, *local_type,
				   *local_keep, *local_keep_totals );
		} else {
			&compare_dirs(
				*local_sorted,
				 *local_map, *local_time,
				  *local_size, *local_type,
				*remote_sorted,
				 *remote_map, *remote_time,
				  *remote_size, *remote_type,
				   *remote_keep, *remote_keep_totals );
		}

		if( $timestamp ){
			&set_timestamps();
			if( $algorithm == 1 ){
				foreach $sd ( @sub_dirs ){
					push( @transfer_list, "$local_dir/$sd" );
					push( @transfer_list, "$remote_dir/$sd" );
					push( @transfer_list, $get_patt );
				}
			}

			next;
		}

		&make_dirs();
		&do_all_transfers();

		$exit_status = $exit_ok;	# Everything went ok.

		if( $get_file ){
			# I must have finished with the remote information
			# so clear it out.
			&clear_remote();
		}
		else {
			# clear out local info.
			&clear_local();
		}
		
		if( $save_deletes ){
			# If $save_dir is null, make $save_dir to be
			# subdirectory 'Old' under 
			# current path
			if( ( ! defined( $save_dir ) ) || ( $save_dir eq '' ) ){
				$save_dir = "$cwd/Old";
			}

			# If $save_dir is not absolute, take it as
			# subdirectory of current path
			if( $save_dir !~ m,^/, ){
               			$save_dir = "$cwd/$save_dir";
       			}
		}

		if( $do_deletes || $save_deletes ){
			if( $get_file ){
				&do_deletes(
					*local_sorted,
					 *local_map,
					  *local_type, *local_keep,
					   *local_totals, *local_keep_totals );
			}
			else {
				&do_deletes(
					*remote_sorted,
					 *remote_map,
					  *remote_type, *remote_keep,
					   *remote_totals, *remote_keep_totals );
			}
		}

		&make_symlinks();
		undef( @things_to_make );

		if( $algorithm == 1 ){
			foreach $sd ( @sub_dirs ){
				push( @transfer_list, "$local_dir/$sd" );
				push( @transfer_list, "$remote_dir/$sd" );
				push( @transfer_list, $get_patt );
			}
		}

		# No more transfers if the connection has died.
		last if ! $connected;
	}

	&clear_local();
	&clear_remote();
	
	if( $use_files ){
		# Close and zap.
		&delete_assocs();
	}

	# Should I force a disconnect now?
	if( $connected && $disconnect ){
		&disconnect();
	}

	if( $dont_do || $timestamp ){
		# Don't generate logs/email
		&msg( "\n" );
		return $exit_status;
	}

	local( $now ) = &time_to_standard( time );
	if( $update_log ){
		if( ! open( LOGG, ">>$update_log" ) ){
			&msg( "Cannot append to $update_log because: $!\n\n" );
			# Serious but this shouldn't stop mirroring.
			# return $exit_fail;
		}
		print LOGG "mirroring $package ($site:$remote_dir) completed successfully \@ $now\n";
		print LOGG @log;
		close( LOGG );
	}

	if( $#log >= 0 && $mail_prog ne '' && $mail_to =~ /./ ){
		local( $arg );
		eval "\$arg = \"$mail_subject\"";
		if( ! open( MAIL, "|$mail_prog $arg $mail_to" ) ){
			&msg( "Cannot run: $com\n\n" );
			return $exit_fail;
		}
		if( $get_patt ){
			print MAIL "Mirrored $package ($site:$remote_dir -> $local_dir) $comment \@ $now\n";
		}
		else {
			print MAIL "Mirrored $package ($local_dir -> $site:$remote_dir) $comment \@ $now\n";
		}
		print MAIL @log;
		close( MAIL );
	}
	undef( @log );

	&msg( "\n" );
	return $exit_status;
}


sub disconnect
{
	if( $connected ){
		&msg( "disconnecting from $connected\n" ) if $debug;
		if( ! $ftp'fatalerror ){
			&ftp'close();
		}
		else {
			&ftp'service_closed();
		}
	}
	$connected = '';
}

# Connect to the site
# Return 0 on a fail,
# 1 if a connection was successfully made,
# 2 if already connected to the site
sub connect
{
	local( $attempts ) = 1; # Retry ONCE! Be friendly.
	local( $res );

	if( $connected eq $site && $curr_remote_user eq $remote_user ){
		# Already connected to this site!
		return 2;
	}

	# Clear out any session active session
	&disconnect();

	if( $proxy ){
	    $ftp'proxy = $proxy;
	    $ftp'proxy_gateway = $proxy_gateway;
	    $ftp'proxy_ftp_port = $proxy_ftp_port;
	}
	$res = &ftp'open( $site, $ftp_port, $retry_call, $attempts );
	if( $res == 1 ){
		# Connected
		$connected = $site;
	}
	return $res;
}	

# This just prods the remote ftpd to prevent time-outs
sub prod
{
	return unless $connected;

	if( $debug > 2 ){
		&msg( " prodding remote ftpd\n" );
	}
	&ftp'pwd();
}

# checkout and fixup any regexps.
# return 0 on an error
sub checkout_regexps
{
	local( $ret ) = 1;
	# Check out the regexps
	local( $t ) = 'x';
	foreach $r ( @regexp_values ){
		# regexps should never begin or end with a | or have
		# two in a row otherwise the pattern matches everything.
		# Use null to match everything if thats what you mean.
		$value{ $r } =~ s/\|+/|/g;
		$value{ $r } =~ s/^\|//;
		$value{ $r } =~ s/\|$//;
		local( $val ) = $value{ $r };
		next if ! $val;
		eval '$t =~ /$val/';
		if( $@ ){
			local( $err );
			chop( $err = $@ );
			&msg( "Problem with regexp $r ($err)\n" );
			$ret = 0;
		}
	}
	return $ret;
}

sub clear_local
{
	if( ! $use_files ){
		undef( %local_map );
	}
	undef( @local_sorted );
	undef( @local_time );
	undef( @local_size );
	undef( @local_type );
	undef( @local_mode );
	undef( @local_keep );
	undef( @local_totals );
	undef( @local_keep_totals );
}

sub clear_remote
{
	if( ! $use_files ){
		undef( %remote_map );
	}
	undef( @remote_sorted );
	undef( @remote_time );
	undef( @remote_size );
	undef( @remote_type );
	undef( @remote_mode );
	undef( @remote_keep );
	undef( @remote_totals );
	undef( @remote_keep_totals );
}

sub get_local_directory_details
{
	local( @dirs, $dir );
	local( $last_prodded ) = time; # when I last prodded the remote ftpd

	$next_local_mapi = $map_init;
	
	&clear_local();
	
	# Make sure the first elem is 0.
	$local_time[ 0 ] = 0;
	$local_size[ 0 ] = 0;
	$local_type[ 0 ] = 0;
	$local_mode[ 0 ] = 0;

	@get_top = ();

	&msg( "Scanning local directory $local_dir\n" ) if $debug;
	
	if( ! -d $local_dir ){
		&msg( "$local_dir no such directory - creating it\n" );
		if( $dont_do || $timestamp ){
			return 1;
		}
		if( &mkdirs( $local_dir ) ){
			&msg( $log, "Created local dir $local_dir\n" );
			$exit_xfer_status |= $exit_xfers;
		}
		else {
			&msg( $log, "FAILED to create local dir $local_dir\n" );
		}
	}
	if( !chdir( $local_dir ) ){
		&msg( "Cannot change directory to $local_dir\n" );
		return 0;
	}

	if( $local_dir =~ m,^/, ){
		$cwd = $local_dir;
	}
	else {
		&cwd();
	}

	# @dirs is the list of all directories to scan
	# As subdirs are found they are added to the end of the list
	# and as 
	@dirs = ( "." );
	# Most of these variables should be locals in blocks below but
	# that seems to tickle a perl bug and causes a lot of memory to
	# be wasted.
	local( $dir_level ) = 0;
	local( $i ) = 0;
	local( $path, $time, $size, $type, $mode, $name, $isdir, $value, $follow );
	local( $dev,$ino,$fmode,$nlink,$uid,$gid,$rdev,$ssize,
		      $atime,$mtime,$ctime,$blksize,$blocks );
	while( defined( $dir = shift( @dirs ) ) ){

		if( !opendir( DIR, $dir ) ){
			&msg( "Cannot open local directory $dir, skipping it\n" );
			next;
		}

		while( defined( $name = readdir( DIR ) ) ){
			$isdir = 0;

			# Prod the remote system from time to time
			# To prevent time outs.  Only look once every 50 files
			# to save on unnecessary systems calls.
			if( ($i % 50 == 0) && time > ($last_prodded + $prod_interval) ){
				$last_prodded = time;
				&prod();
			}
			$i ++;

			$path = "$dir/$name";
			$path =~ s,(^|/)\./,,;
			next if $name eq '.' || $name eq '..' ||
				($local_ignore && $path =~ /$local_ignore/);

			$follow = ($follow_local_symlinks ne '' && $path =~ /$follow_local_symlinks/);
			if( !$follow && -l $path ){
				$value = readlink( $path );
				( $dev,$ino,$fmode,$nlink,$uid,$gid,$rdev,$ssize,
				      $atime,$mtime,$ctime,$blksize,$blocks ) =
					lstat( _ );
				$size = $ssize;
				$time = $mtime;
				$type = "l $value";
				$mode = $fmode;
			}
			elsif( ($isdir = ($follow ? (-d $path) : (-d _))) ||
			         -f _ ){
				( $dev,$ino,$fmode,$nlink,$uid,$gid,$rdev,$ssize,
				      $atime,$mtime,$ctime,$blksize,$blocks ) =
					stat( _ );
				$size = $ssize;
				$time = $mtime;
				$mode = $fmode;
				if( $isdir ){
					push( @dirs, $path ) if $recursive;
					$type = 'd';
				}
				else {
					$type = 'f';
				}
				if( $dir_level == 0 && $update_local ){
					push( @get_top, $path );
				}
			}
			else {
				&msg( "unknown file type $path, skipping\n" );
				next;
			}
			if( $debug > 2){
				printf "local: %s %s %s %s 0%o\n",
					$path, $size, $time, $type, $mode;
			}
			if( $max_age && $time != 0 && $time < $max_age ){
				&msg( "   too old: $path\n" ) if $debug > 1;
				next;
			}

			local( $mapi ) = $next_local_mapi++;
			# push( @local_sorted, $path );
			$local_sorted[ $mapi - 1 ] = $path;
			$local_map{ $path } = $mapi;
			$local_time[ $mapi ] = $time;
			$local_size[ $mapi ] = $size;
			$local_type[ $mapi ] = $type;
			$local_mode[ $mapi ] = $mode;
			if( $type eq 'd' ){
				$local_totals[ 0 ]++;
			}
			else {
				$local_totals[ 1 ]++;
			}
		}
		closedir( DIR );
		$dir_level++;

		if( ! $recursive ){
			last;
		}
	}
	return 1;
}

# Return true if the remote directory listing was brought back safely.
sub get_remote_directory_details
{
	local( $type_changed ) = 0;
	local( $udirtmp );
	local( $storename ) = "/dev/null";

	&msg( "Scanning remote directory $remote_dir\n" ) if $debug;
	
	if( $store_remote_listing ){
		eval "\$storename = \"$store_remote_listing\"";
	}

	$next_remote_mapi = $map_init;
	&clear_remote();

	# Make sure the first elem is 0.
	$remote_time[ 0 ] = 0;
	$remote_size[ 0 ] = 0;
	$remote_type[ 0 ] = 0;
	$remote_mode[ 0 ] = 0;

	if( $remote_fs !~ /cms/ && ! &ftp'cwd( $remote_dir ) ){
		if( $get_file ){
			# no files to get
			return 0;
		}

		&msg( "Failed to change to remote directory ($remote_dir) trying to create it\n" );
		&mkdirs( $remote_dir );

		if( ! &ftp'cwd( $remote_dir ) ){
			&msg( "Cannot change to remote directory ($remote_dir) because: $ftp'response\n" );
			return 0;
		}
	}

	local( $rls );

	$use_ls = 0;

	if( $local_ls_lR_file ){
	    	local( $dirtmp ) = $local_ls_lR_file;
		&msg( " Using local file $local_ls_lR_file for remote dir listing\n" ) if $debug;
		local( $unsquish );
		if( $dirtmp =~ /\.$sys_compress_suffix$/ ){
			$unsquish = $sys_compress_prog;
		}
		elsif( $dirtmp =~ /\.($gzip_suffix|$old_gzip_suffix)$/ ){
			$unsquish = $gzip_prog;
		}
  		if( defined( $unsquish ) ){
  			local( $f );
  			$f = $dirtmp;
			$f =~ s/($shell_metachars)/\\$1/g;
			$dirtmp = "$unsquish -d < \"$f\" |";
  		}
		elsif( $ls_lR_file =~ /($times_suffix|$patch_gzip_suffix)$/ ){
			return 0 if &patch_ls_lR_file()==0;    
		}
		if( ! open( DIRTMP, $dirtmp ) ){
			&msg( "Cannot open $dirtmp\n" );
			return 0;
		}
		$rls = "main'DIRTMP";
		# Now we don't want to overwrite our input... better test?
		if( $local_ls_lR_file eq $storename ){
			$storename = "/dev/null";
		}
	}
	elsif( $ls_lR_file ){
		local( $dirtmp );
		$dirtmp = "$big_temp/.dir$$";
		if( $ls_lR_file =~ /\.($sys_compress_suffix|$gzip_suffix|$old_gzip_suffix)$/ ){
			$dirtmp .= ".$1";
		}

		&msg( " Getting directory listing from remote file $ls_lR_file\n" ) if $debug;
		if( ! &ftp'get( $ls_lR_file, $dirtmp, 0 ) ){
			&msg( "Cannot get dir listing file\n" );
			return 0;
		}
		local( $unsquish );
		if( $dirtmp =~ /\.$sys_compress_suffix$/ ){
			$unsquish = $sys_compress_prog;
		}
		elsif( $dirtmp =~ /\.($gzip_suffix|$old_gzip_suffix)$/ ){
			$unsquish = $gzip_prog;
		}
  		if( defined( $unsquish ) ){
  			local( $f, $uf );
			$uf = $udirtmp = $dirtmp;
			$dirtmp =~ s/($shell_metachars)/\\$1/g;
  			$f = $dirtmp;
  			$dirtmp =~ s/\.($sys_compress_suffix|$gzip_suffix|$old_gzip_suffix)$//;
  			$udirtmp =~ s/\.($sys_compress_suffix|$gzip_suffix|$ol_gzip_suffix)$//;
			if( &sys( "$unsquish -d < \"$f\" > \"$dirtmp\"" ) != 0 ){
				&msg( "Cannot uncompress directory listing\n" );
				return 0;
			}
  			unlink( $uf );
  		}
		else {
			$udirtmp = $dirtmp;
		}

		open( DIRTMP, $dirtmp ) || die "Cannot open $dirtmp";
		$rls = "main'DIRTMP";
	}
	else {
		$use_ls = 1;
		if( ! &ftp'type( 'A' ) ){
			&msg( "Cannot set type to ascii for dir listing, ignored\n" );
			$type_changed = 0;
		}
		else {
			$type_changed = 1;
		}
	}
	
	$lsparse'fstype = $remote_fs;
	$lsparse'name = "$site:$package";
	
	if( $use_ls ){
		local( $flags ) = $flags_nonrecursive;
		if( $recursive && ! $recurse_hard ){
			$flags = $flags_recursive;
		}
		$lsparse'report_subdirs = (! $recurse_hard && $algorithm == 0);
	 	if( !&ftp'dir_open( $flags ) ){
			&msg( "Cannot get remote directory listing because: $ftp'response\n" );
			return 0;
		}
		
		$rls = "ftp'NS";
	}
		
	$rcwd = '';
	if( $vms ){
		# Strip this off all pathnames to make them
		# relative to the remote_dir
		$rcwd = $remote_dir;
	}
	$dateconv'use_timelocal = $use_timelocal;
	if( !&lsparse'reset( $rcwd ) ){
		&msg( "$remote_fs: unknown fstype\n" );
		return 0;
	}
	if( $vms ){
		# Need to get in terms of the full pathname
		# so add it back in - see unix2vms at end of mirror
		$vms_dir = $remote_dir;
	}
	
	if( $storename ne "/dev/null" ){
		open( STORE, ">$storename" ) || die( "Cannot write to $storename\n" );
	}

	local( $parse_state ) = &parse_remote_details();

	close( STORE );

	
	if( $local_ls_lR_file ){
		close( DIRTMP );
	}
	elsif( $ls_lR_file ){
		close( DIRTMP );
		unlink( $udirtmp );
	}
	else {
		# Could optimise this out - but it makes sure that
		# the other end gets a command straight after a possibly
		# long dir listing.
		if( ! &ftp'type( $text_mode ? 'A' : 'I' ) ){
			local( $msg ) = "Cannot reset type after dir listing, ";
			if( $type_changed ){
				# I changed it before - so I must be able to
				# change back unless something is wrong
				$msg .= "aborting\n";
				&msg( $msg );
				return 0;
			}
			else {
				$msg .= "ignoring\n";
				&msg( $msg );
			}
		}
	}

	# If the other end dropped part way thru make sure the
	# higher routines know!
	return $parse_state;
}

# Get remote ls-lR times or mirror gzipped patch files.
sub patch_ls_lR_file
{
	if( ! $patch_prog ){
		&msg( "No patch program on PATH\n" );
		return 0;
	}
	local( $f, $fr, $flb, $flt, $flp, $flz, $frb, $frt );
	local( $to , $tn );
	$frb = $frt = $ls_lR_file;
	$flb = $dirtmp;
	&msg( "Patching $flb using $frb\n" ) if $debug;
	local( $tlb ) = -f $flb?(stat($flb))[9]:0;
	$dateconv'use_timelocal = $use_timelocal;
	$flp = "$flb$patch_suffix";
	$flz = "$flb$patch_gzip_suffix";
	# Get times and patch.
	if( $frt =~ /$times_suffix$/ ){
		# Use remote times file.
		$frb =~ s/$times_suffix$//;
		$flt = "$flb$times_suffix";
		&ftp'get( $frt, $flt, 0 ) ?  
			&msg( "Got $frt\n" ):
			return 0;
		open( FT, $flt );
		for( $to, $tn ){
			$f = gmtime( <FT> );
			$_ = &lstime_to_time( $f );
		}
		close( FT );
		$f = "$frb$patch_gzip_suffix";
		if( $tlb == $to && &ftp'get( $f, $flz, 0 ) &&
			! &sys("$gzip_prog -df <$flz >$flp") ){
			&msg( $log, "Got $f\n" );
			unlink $flz if ! $debug;
		}
	}
	else {
		# Get time of remote patch file.
		$lsparse'fstype = $remote_fs;
		$lsparse'name = "$site:$package";
		&lsparse'reset( $remote_dir );
		if( ! &ftp'dir_open( "$flags_nonrecursive $frb" ) ){
			&msg( "List remote ls-lR patch: $ftp'response\n" );
			&ftp'dir_close();
			return 0;
		}
		local( $p, $s, $trz, $t, $m ) = &lsparse'line( ftp'NS );
		&msg( "Remote ls-lR patch:\n$p $s $trz $t $m\n" ) if $debug;
		if( ! &ftp'dir_close() ){
			&msg( "List remote ls-lR patch: $ftp'response\n" );
			return 0;
		}
		# If remote time does not match local get remote patch file.
		local( $tlz ) = -f $flz?(stat($flz))[9]:0;
		if( $trz == $tlz ){
			&msg( "No new $frb\n" );
			&msg( "age $trz same as $flz\n" ) if $debug;
		}
		else {
			&ftp'get( $frb, $flz, 0 )?
			&msg( $log, "Got $frb $s\n" ):
			return 0;
			&utime( $trz, $trz, $flz );
		}
		# unzip patch and read times.
		$frb =~ s/$patch_gzip_suffix$//;
		&sys( "$gzip_prog -df <$flz >$flp" ) ?
			return 0:
			open( FT, $flp );
		for( $to, $tn ){
			( $fr, $f ) = split( /\t/, <FT> );
			$_ = &lstime_to_time( $f );
		}
		close( FT );
	}
	# Patch or leave or get new local ls-lR file?
	$f = "$patch_prog ";
	$f .= $use_timelocal?$patch_local:$patch_UTC;
	if( $tlb == $to && ! &sys( "$f $flb $flp" ) ){
		&msg( "$flb patched\n" );
	}
	elsif( $tlb == $tn ){
		&msg( "$flb up to date\n" );
	}
	else {
		$fr = "$frb.$gzip_suffix";
		$f = "$flb.$gzip_suffix";
		if( &ftp'get( $fr, $f, 0 ) &&
			! &sys( "$gzip_prog -df $f" ) ){
			&utime( $tn, $tn, $flb );
			&msg( $log, "Got $fr for $flb\n" );
		}
		else {
			&msg( "Did not get $fr\nand $ftp'response\n" );
			return 0;
		}
	}
	unlink $flp, $flt if ! $debug;
	if( ! $do_deletes && $exclude_patt =~ /^\.($|\|)/ ){
		&msg( "$flb check complete\n" );
		next;
	}
	return 1;
}

sub parse_timeout
{
	$parse_timed_out = 1;
	die "timeout: parse_remote_details";
}

sub parse_remote_details
{
	local( $ret );
	local( $old_sig );

	$parse_timed_out = 0;
	
	if( ! $use_ls ){
		# No need to bother with the timers
		return &parse_remote_details_real();
	}
	
	# This may timeout
	$old_sig = $SIG{ 'ALRM' };
	$SIG{ 'ALRM' } = "main\'parse_timeout";
	
	$ret = eval '&parse_remote_details_real()';
	
	&alarm( 0 );

	$SIG{ 'ALRM' } = $old_sig;

	if( $@ =~ /^timeout/ ){
		&msg( "timed out parsing directory details\n" );
		return 0;
	}
	return $ret;
}


sub parse_remote_details_real
{
	local( $path, $size, $time, $type, $mode, $rdir, $rcwd );
	local( @dir_list );
	local( $i ) = 0;
	local( $old_path );
	
	if( $use_ls ){
		&alarm( $parse_time );
	}
	
	# Need to loop in case $recurse_hard
	while( 1 ){
		while( !eof( $rls ) ){
			( $path, $size, $time, $type, $mode ) =
				&lsparse'line( $rls );
			last if $path eq '';
			if( $ls_fix_mappings ){
				local( $old_path ) = $path;
				$_ = $path;
				eval $ls_fix_mappings;
				if( $_ ne $old_path ){
					$path = $_;
				}
			}
			next if $name eq '.' || $name eq '..';
			if( $debug > 2 ){
				printf "remote: %s %s %s %s 0%o\n",
 					$path, $size, $time, $type, $mode;
			}
			if( $use_ls ){
				# I just got something so shouldn't timeout
				&alarm( $parse_time );
			}
			else {
				# Prod the remote system from time to time
				# To prevent time outs.  Only look once every
				# 50 files
				# to save on unnecessary systems calls.
				if( ($i % 50 == 0) &&
				    time > ($last_prodded + $prod_interval) ){
					$last_prodded = time;
					&prod();
				}
				$i ++;
			}
			
			if( $algorithm == 1 ){
				$path0 = substr( $remote_dir, $rem_start_len );
				if( $path0 ne '' ){
					$path0 .= "/";
				}
				$path0 .= $path;
				$path0 =~ s,^/,,;
				# &msg( "debug: $path0, $remote_dir, $rem_start_len\n" );
			}
			else {
			    	$path0 = $path;
			}

			if( $exclude_patt && $path0 =~ /$exclude_patt/ ){
				&msg( "   exclude: $path0\n" ) if $debug > 1;
				next;
			}

			if( $type eq 'd' ){
				push( @dir_list, $path0 );
			}
			
			if( $max_age && $time != 0 && $time < $max_age ){
				&msg( "   too old: $path0\n" ) if $debug > 1;
				next;
			}
			
			# If vms and only keeping the latest version
			if( $vms && !$vms_keep_versions ){
				# If we already have a file, pick the newer
				# TODO: pick the greatest version number
				local( $ri ) = $remote_map{ $path };
				if( $ri &&
				    $time > $remote_time[ $ri ] ){
					$remote_time[ $ri ] = $time;
					$remote_size[ $ri ] = $size;
					$remote_type[ $ri ] = $type;
					$remote_mode[ $ri ] = $mode;
					next;
				}
			}
			
			local( $mapi ) = $next_remote_mapi++;
			# push( @remote_sorted, $path );
			$remote_sorted[ $mapi - 1 ] = $path;
			$remote_map{ $path } = $mapi;
			$remote_time[ $mapi ] = $time;
			$remote_size[ $mapi ] = $size;
			$remote_type[ $mapi ] = $type;
			$remote_mode[ $mapi ] = $mode;
			if( $type eq 'd' ){
				$remote_totals[ 0 ]++;
			}
			else {
				$remote_totals[ 1 ]++;
			}
		}

		if( $use_ls ){
			if( ! &ftp'dir_close() ){
				&msg( "Failure at end of remote directory" .
				 " ($rdir) because: $ftp'response\n" );
				return 0;
			}
		}
		
		if( $recurse_hard ){
			local( $done ) = 0;
			while( 1 ){
				if( $#dir_list < 0 ){
					# Make sure we end in the right directory.
					if( ! &ftp'cwd( $remote_dir ) ){
						&msg( "Cannot change to remote directory" .
						 " ($rdir) because: $ftp'response\n" );
						return 0;
					}
					$done = 1;
					last;
				}
				$rcwd = shift( @dir_list );
				$rdir = "$remote_dir/$rcwd";
				if( $debug > 2 ){
					print "scanning: $remote_dir / $rcwd\n";
				}
				if( ! &ftp'cwd( $rdir ) ){
					&msg( "Cannot change to remote directory" .
					 " ($rdir) because: $ftp'response\n" );
					next;
				}
				last;
			}
			if( $done ){
				last;
			}
		 	if( !&ftp'dir_open( $flags_nonrecursive ) ){
				&msg( "Cannot get remote directory" .
				  	" listing because: $ftp'response\n" );
				return 0;
			}
			&lsparse'reset( $rcwd );
			
			# round the loop again.
			next;
		}
		
		# All done - snap the loop
		last;
	}
	return 1;
}

sub compare_dirs
{
	# This declaration must be "local()" because it modifies global data.
	local( *src_paths,
		*src_map, *src_time,
		 *src_size, *src_type, 
	       *dest_paths,
		*dest_map, *dest_time,
		 *dest_size, *dest_type,
		  *dest_keep, *dest_keep_totals ) = @_;
	local( $src_path, $dest_path, $i );
	local( $last_prodded ) = time; # when I last prodded the remote ftpd

	# Most of these variables should be locals in blocks below but
	# that seems to tickle a perl bug and causes a lot of memory to
	# be wasted.
	local( $desti, $srci, $compress, $srciZ, $srcigz, $split, $dest_path_real );
	local( $old_dest_path, $existing_path, $tmp, $restart );
	local( $sp, $dp ) = ($#src_paths + 1, $#dest_paths + 1);
	
	&msg( "compare directories (src $sp, dest $dp)\n" ) if $debug;
	$total_src_size = 0;

	for( $i = 0; $i <= $#src_paths; $i++ ){
		$dest_path = $src_path = $src_paths[ $i ];
		
		$desti = $dest_map{ $dest_path };
		$srci = $i + 1;

		# Prod the remote system from time to time
		# To prevent time outs.  Only look once every 50 files
		# to save on unnecessary systems calls.
		if( ($i % 50 == 0) && time > ($last_prodded + $prod_interval) ){
			$last_prodded = time;
			&prod();
		}

		if( $debug > 2 ){
			&msg( "Compare src $src_path ($srci): " .
				&t2str( $src_time[ $srci ] ) );
			&msg( " $src_size[ $srci ] $src_type[ $srci ]\n" );
		}

		# I'm about to do a lot of matching on this
		study( $src_path );

		# Should I compress this file?
		#  Don't compress this file if trying to do a compress->gzip
		# conversion.
		$compress = 0;
		if( &will_compress( $src_path, $srci ) ){
			if( $dest_path !~ /$squished/o ){
				$srciZ = $src_map{ "$src_path.$sys_compress_suffix" };
				$srcigz = $src_map{ "$src_path.$gzip_suffix" };
				if( $srciZ || $srcigz ){
					# There is a compressed version
					# too!  Skip the uncompressed one
					&msg( "   do not xfer, compressed version exists: $src_path\n" ) if $debug > 1;
					next;
				}

				$compress = 1;
				$dest_path .= '.' . $compress_suffix;
				$desti = $dest_map{ $dest_path };
			}
		}
		# need to adjust the symlink pointer?
		elsif( $src_type[ $srci ] =~ /^l (.*)/ ){
		   # Am I going to squish the file this points to?
		   local( $real, $reali, $reali1 );
		   local( $count ) = 0;
		   while( $count++ <= 10 ){
			$real = &expand_symlink( $src_path, $1 );
			$reali = $src_map{ $real };
			# Look out for when the symlink loops on itself
			if( defined( $reali1 ) && $reali == $reali1 ){
				last;
			}
			$reali1 = $reali;
			last if $src_type[ $reali ] !~ /^l (.*)$/;
		   }
		   if( &will_compress( $real, $reali ) ){
			# real is going to be (at least) squished so
			# suffix the dest
			$dest_path .= '.' . $compress_suffix;
			$desti = $dest_map{ $dest_path };
			$src_type[ $srci ] .= '.' . $compress_suffix;
			&msg( "  symlink pointer is now $dest_path\n" ) if $debug > 1;
			if( $src_map{ $dest_path } ){
				&msg( "do not xfer, $dest_path exists\n" ) if $debug > 1;
				next;
			}
		   }
		   if( &will_split( $real, $reali ) ){
			$src_type[ $srci ] .= '-split/README';
			&msg( "  symlink pointer now to $real-split/README'\n" ) if $debug > 1;
		   }
		}
		
		# If this is a file that I decided not to compress but the
		# remote file is compressed and I want a gziped local version
		# then force compression.
		# This ignores any compress_excl flags.
		if( ! $compress &&
		    $compress_suffix eq $gzip_suffix &&
		    $compress_conv_patt && $src_path =~ /$compress_conv_patt/ ){
			$_ = $dest_path;
			eval $compress_conv_expr;
			$dest_path = $_;
			# check if dest_path exists in the sources.  If it
			# does, ignore this file.  This is to avoid the
			# double mirroring problem if you are using gzip and
			# the source site has both foo.Z and foo.gz.
			if( $src_map{ $dest_path } ){
				&msg( "Skipping $src_path because remote site also has $dest_path\n" ) if $debug > 2;
				next;
			}
			&msg( "   $src_path -> $dest_path\n" ) if $debug > 2;
			$desti = $dest_map{ $dest_path };
			$compress = 1;
		}

		# Am I converting the compression on the file this points to?
		if( $src_type[ $srci ] =~ /^l (.*)/ &&
		      $compress_suffix eq $gzip_suffix ){
		        local( $value ) = $1;
			local( $real ) = &expand_symlink( $src_path, $value );
		        local( $reali ) = $src_map{ $real };
		        if( $src_type[ $reali ] ne 'd' &&
			    $src_type[ $reali ] ne /^l .*/ &&
			    $compress_conv_patt && $real =~ /$compress_conv_patt/ ){
			    $dest_path =~ s/$sys_compress_suffix$/$gzip_suffix/;
			    $desti = $dest_map{ $dest_path };
			    $value =~ s/$sys_compress_suffix$/$gzip_suffix/;
			    &msg( "  symlink pointer is now $dest_path (conv)\n")
				 if $debug > 1;
			}
			if( $name_mappings || $external_mapping ){
			        local( $old ) = $value;
				$value = &map_name( $value );
				if( $value ne $old ){
				    &msg( "   Mapped symlink value is $value\n" ) if $debug > 2;
				}
				    
			}
			$src_type[ $srci ] = "l ".$value;
		}

		if( $name_mappings || $external_mapping ){
			local( $old_dest_path ) = $dest_path;
			$dest_path = &map_name( $dest_path );
			if( $dest_path ne $old_dest_path ){
				$desti = $dest_map{ $dest_path };
				&msg( "   Mapped name is $dest_path\n" ) if $debug > 2;
			}
		}
		
		# Should this file be split?
		$split = 0;
		$dest_path_real = undef;
		if( &will_split( $src_path, $srci ) ){
			$split = 1;
			$dest_path_real = $dest_path;
			$dest_path .= "-split/part01";
			$desti = $dest_map{ $dest_path };
		}

		if( $debug > 2 ){
			&msg( "       dest $dest_path ($desti): " .
				&t2str( $dest_time[ $desti ] ) );
			&msg( " $dest_size[ $desti ] $dest_type[ $desti ]" );
			&msg( " (->$compress_suffix)" ) if $compress;
			&msg( " (split)" ) if $split;
			&msg( "\n" );
		}
		
		if( $algorithm == 1 ){
			$src_path0 = substr( $remote_dir, $rem_start_len );
			if( $src_path0 ne '' ){
				$src_path0 .= "/";
			}
		 	$src_path0 .= $src_path;
			$src_path0 =~ s,^/,,;
			#&msg( "debug: $src_path0, $remote_dir, $rem_start_len\n" );
		}
		else {
			$src_path0 = $src_path;
		}
			
		if( $get_patt && $src_path0 !~ /$get_patt/ ){
			&msg( "   do not xfer: $src_path0\n" ) if $debug > 1;
			next;
		}

		# Just create any needed directories (the timestamps
		# should be ignored)
		if( $src_type[ $srci ] eq 'd' ){
			if( $algorithm == 1 ){
				if( $exclude_patt && $src_path0 =~ /$exclude_patt/ ){
					&msg( "   exclude: $src_path0\n" ) if $debug > 1;
				}
				else {
					$rel_src_path = $src_path;
					$rel_src_path =~ s,.*/,,;
					push( @sub_dirs, $rel_src_path );
					&msg( "   adding $rel_src_path\n" ) if $debug;
				}
			}
			if( $dest_type[ $desti ] ne 'd' ){
				push( @things_to_make, "d $dest_path" );
				&msg( "   need to mkdir $dest_path\n" ) if $debug > 1;
			}
			# keep the directory once made
			# (Also if local is really a symlink elsewhere
			#  it will be kept.)
			&keep( $desti, $dest_path, *dest_keep, *dest_keep_totals, *dest_map, 0 );
			next;
		}

		# Well that just leaves files and symlinks.
		# Do various checks on them.

		if( $desti && ! $dest_keep[ $desti ] ){
			&keep( $desti, $dest_path, *dest_keep, *dest_keep_totals, *dest_map, 1 );
			if( $split ){
				# Mark all the split parts as kept
				local( $dpp, $dps );
				($dpp, $dps) = ($dest_path =~ m,^(.*/)(part[0-9]+)$,);
				while( 1 ){
					$dps++;
					if( !($di = $dest_map{ $dpp . $dps }) ){
						last;
					}
					&keep( $di, $dpp . $dps, *dest_keep, *dest_keep_totals, *dest_map, 1 );
				}
				# And the README
				$dps = 'README';
				$di = $dest_map{ $dpp . $dps };
				if( $di ){
					&keep( $di, $dpp . $dps, *dest_keep, *dest_keep_totals, *dest_map, 1 );
				}
				# And the directory
				chop( $dpp );
				$dps = '';
				$di = $dest_map{ $dpp . $dps };
				if( $di ){
					&keep( $di, $dpp . $dps, *dest_keep, *dest_keep_totals, *dest_map, 0 );
				}
			}
		}
		
		local( $update ) = 0;

		if( ! $get_missing ){
			next;
		}

		if( ($max_size > 0) && ($src_size[ $srci ] > $max_size) ){
			&msg( "   src is too big, no need to xfer it\n" ) if $debug > 2;
			next;
		}

		if( $force || ! $dest_type[ $desti ] || $timestamp ){
			# Either I'm forcing xfers or the file doesn't exist
			# either way I should update
			$update = 1;
		}
		else {
			# Maybe the src is newer?
			if( $get_newer &&
			   &compare_times( $src_time[ $srci ], $dest_time[ $desti ] ) ){
				&msg( "   src is newer, xfer it\n" ) if $debug > 2;
				$update = 1;
			}
			# or maybe its size has changed?
			# don't bother if file was compressed or split as the
			# size will have changed anyway
			if( !$update &&
			   !$compress && !$split &&
			   $get_size_change &&
			   ($src_type[ $srci ] eq 'f') &&
			   ($src_size[ $srci ] != $dest_size[ $desti ]) ){
				$update = 1;
				if( $debug > 2 ){
				    &msg( "   src is different size, xfer it\n" );
				}
			}
			# Maybe it has changed type!
			if( !$update &&
			   $src_type[ $srci ] ne $dest_type[ $desti ] ){
				$update = 1;
				if( $debug > 2 ){
				    &msg( "   src has different type, xfer it\n" );
				}
			}
		}

		if( ! $update ){
			next;
		}

		if( $src_type[ $srci ] =~ /^l (.*)/ ){
			# If the symlink hasn't changed then may as well 
			# leave it alone
			if( $src_type[ $srci ] eq $dest_type[ $desti ] ){
				next;
			}
			# DONT FORGET TO NAME MAP!!!!
			$existing_path = $1;

			if( $compress_suffix eq $gzip_suffix &&
			    $compress_conv_patt && $existing_path =~ /$compress_conv_patt/ ){
				$_ = $existing_path;
				eval $compress_conv_expr;
				$existing_path = $_;
			}

			push( @things_to_make, "l $dest_path -> $existing_path" );
			&msg( "   need to symlink $dest_path -> $existing_path\n" ) if $debug > 2;
			next;
		}

		# Now that the tests are complete use the real dest.
		if( defined( $dest_path_real ) ){
			$dest_path = $dest_path_real;
			$desti = $dest_map{ $dest_path };
		}

		$total_src_size += $src_size[ $srci ];
		if( $dont_do ){
			&msg("Should ");
		}
		&msg( "$XFER file $src_path as $dest_path ($src_size[ $srci ])".
			($compress ? " (->$compress_suffix)" : "") .
			($split ? " (split)" : "") . "\n" ) if $debug > 1;
		push( @xfer_dest, $dest_path );
		push( @xfer_src, $src_path );

		# If xfers can be restarted AND
		# a temporary file exists from a previous attempt at a
		# transfer  AND
		# the timestamps of the exising temp file and the original
		# src file match then flag a restart.
		$tmp = &filename_to_tempname( '', $dest_path );
		$tmpi = $dest_map{ $tmp };
		$restart = '';
#warn "get_file = $get_file, can_restart = $can_restart, dest_size = $dest_size[ $tmpi ], dest_time = $dest_time[ $tmpi ], src_time = $src_time[ $srci ]\n";
		if( $get_file &&
		   $can_restart &&
# Debian bug #24243, mirror-2.9 does not restart, adam@usa.net
		   $dest_size[ $tmpi ] != 0 ){
		    if ($dest_time[ $tmpi ] eq $src_time[ $srci ]) {
			# Then this is an xfer of the same file
			# so just restart where I left off
			$restart = 'r';
		    } elsif ( $debug > 1 ){
			&msg ( "Timestamp useless on $tmp\n" );
		    }
		}
		# x for xfer, c for compress, s for split
		push( @xfer_attribs,
		     "x$restart" .
		     ($compress ? "c" : "") .
		     ($split ? "s" : "") );
	}
	&msg( "to $XFER $total_src_size bytes\n" ) if $debug > 2;
}

sub map_name
{
	local( $name ) = @_;

	if( $name_mappings ){
		local( $old_name ) = $name;
		$_ = $name;
		eval $name_mappings;
		if( $_ ne $old_name ){
			$name = $_;
		}
	}
	
	if( $external_mapping ){
		$old_name = $name;
		local( $tmp ) = &extmap'map( $name );
		if( $tmp ne $old_name ){
			$name = $tmp;
		}
	}
	return $name;
}


sub set_timestamps
{
	local( $src_path );
	
	&msg( "setting timestamps\n" );
	if( ! $get_file ){
		&msg( "Cannot set remote timestamps\n" );
		return;
	}

	local( $dest_path, $dest_loc_mapi, $src_rem_mapi,  $rtime );
	
	foreach $src_path ( @xfer_src ){
		$dest_path = shift( @xfer_dest );
		$dest_loc_mapi = $local_map{ $dest_path };
		$src_rem_mapi = $remote_map{ $src_path };

		$rtime = $remote_time[ $src_rem_mapi ];
		if( $dest_loc_mapi && $local_time[ $dest_loc_mapi ] ne $rtime ){
			&set_timestamp( $dest_path, $rtime );
		}
	}
}

sub set_timestamp
{
	local( $path, $time ) =  @_;
	
	local( $pr_time ) = &t2str( $time );

	if( $dont_do ){
		&msg( "Should set time of $path to $pr_time\n" );
		return;
	}

	if( $timestamp || $debug > 2 ){
		&msg( "Setting time of $path to $pr_time\n" );
	}

	if( ! &utime( $time, $time, $path ) ){
		&msg( $log, "Cannot set file times for $path to $pr_time because: $!\n" );
	}
}

sub make_dirs
{
	local( $thing );

	foreach $thing ( @things_to_make ){
		if( $thing !~ /^d (.*)/ ){
			next;
		}
		if( $dont_do ){
			&msg( "Should mkdir $1\n" );
		}
		else {
			&mkdirs( $1 );
		}
	}
}

sub make_symlinks
{
	local( $thing );

	thing:
	foreach $thing ( @things_to_make ){
		if( $thing !~ /^l (.*) -> (.*)/ ){
			next;
		}
		local( $dest, $existing ) = ($1, $2);
		local( $dirpart ) = &dirpart( $dest );
		local( $ft ) = &expand_symlink( $dest, $existing ); 
		if( -e $ft ){
			&mkdirs( $dirpart ) if ! -d $dirpart;
			# symlink to existing file.
# Debian bug #85353 "bad symlink stops listing with -n" <sizif@pier.botik.ru> 
		        &mksymlink( $dest, $existing );
			next;
		}

		# The existing file doesn't actually exist!
		# Has it been compressed, gzipped, split? or worse
		# compressed/gzipped AND split.  (OK so it could
		# be another problem, bad symlink on remote host, file
		# that hasn't been xfer'd yet... but this is as good as
		# it gets.)
		local( $p );
		foreach $p (
			"\%s.$sys_compress_suffix",
			"\%s.$gzip_suffix",
			"\%s/README",
			"\%s-split/README",
			"\%s-split.$sys_compress_suffix/README",
			"\%s-split.$gzip_suffix/README" ){
			local( $f ) = sprintf( $p, $existing );
			if( -e $f ){
				&msg( "using $p\n" ) if $debug > 2;
				&mksymlink( $dest, $f );
				next thing;
			}
		}
		if( $make_bad_symlinks ){
			&msg( "symlink to non-existant file: $dest -> $existing\n" );
			&mksymlink( $dest, $existing );
		}
		elsif ( $get_symlink_files ){
# Get file within $local_dir tree and make symlink, iml@debian.org, 2001/09/22.
			if( $ft =~ m|\.\./| ){
				&msg( "Not getting path $ft\nas not in remote_dir $remote_dir\n" );
				&msg( "and not symlinking $dest -> $existing\n" );
				next thing;
			}
			local( $dl ) = &dirpart( $ft );
			&mkdirs( $dl ) if ! -d $dl;
			if( &ftp'get( $ft, $ft, 0 ) ){
				&msg( $log, "Got $ft\n" );
				&mksymlink( $dest, $existing );
			}
			else {
				&msg( "Did not get $ft\nbecause $ftp'response\n" );
				&msg( "so not symlinking $dest -> $existing\n" );
			}
		}
		else {
			&msg( "Not symlinking $dest -> $existing\n" );
			&msg( "as no path $ft\n" );
		}
	}
}

sub do_all_transfers
{
	local( $src_path );
	local( $dest_path, $attribs );
	local( $srci );
	
	if( $#xfer_src < 0 ){
		&msg( "No files to transfer\n" ) if $algorithm == 0;
		return;
	}

	# The Macos ftpd cannot reliably rename files
	$no_rename = (! $remote_has_rename) || ($remote_fs eq 'macos' && ! $get_file);

	foreach $src_path ( @xfer_src ){
		if( $get_file ){
			$srci = $remote_map{ $src_path };
		}
		else {
			$srci = $local_map{ $src_path };
		}

		$dest_path = shift( @xfer_dest );
		$attribs = shift( @xfer_attribs );
		
		if( $dont_do ){
			# Skip trying to get the file.
			next;
		}

		&msg( "Need to $XFER file $src_path as $dest_path ($attribs)\n" ) if $debug > 1;

		local( $newpath ) =
			&transfer_file( $src_path, $dest_path,
				       $attribs, $remote_time[ $srci ] );
		if( $get_file && $newpath eq '' ){
			&msg( $log, "Failed to $XFER file $ftp'response\n" );
			if( $ftp'response =~ /timeout|timed out/i ){
				$timeouts++;
			}
			if( $ftp'fatalerror || $timeouts > $max_timeouts ){
				&msg( $log, "Fatal error talking to site, skipping rest of transfers\n" );
				&disconnect();
				return;
			}
			next;
		}

		# File will now have been split up.
		if( $attribs =~ /s/ ){
			next;
		}

		&set_attribs( $newpath, $src_path, 'f' );

		# we can only force time for local files
		if( $force_times && $get_file ){
			&set_timestamp( $newpath, $remote_time[ $srci ] );
		}
	}
}


sub transfer_file
{
	local( $src_path, $dest_path, $attribs, $timestamp ) = @_;
	local( $dir, $file, $temp, $compress, $split, $restart, $mesg, $got_mesg );
	
	# Make sure the required directory exists
	$dir = "";
	if( $dest_path =~ /^(.+\/)([^\/]+)$/ ){
		($dir, $file) = ($1, $2);
		if( $dest_type[ $dir ] ne 'd' && &mkdirs( $dir ) ){
			&msg( $log, "Created dir $dir\n" );
		}
	}
	else {
		$file = $dest_path;
	}
	
	$temp = &filename_to_tempname( $dir, $file );
	
	# Interpret the attrib characters
	if( $attribs !~ /x/ ){
		# Not an xfer!
		return '';
	}
	if( $attribs =~ /c/ ){
		$compress = 1;
		$mesg = " and compress";
	}
	if( $attribs =~ /s/ ){
		$split = 1;
		$mesg = " and split";
	}
	if( $attribs =~ /r/ ){
		$restart = 1;
	}
	
	if( $vms ){
		&ftp'type( ($src_path =~ /$vms_xfer_text/i) ? 'A' : 'I' );
	}
	
	if( $remote_fs eq 'macos' && ! $get_file ){
		&ftp'type( 'A' );
	}
	
	if( ! $get_file ){
		# put the file remotely
		local( $src_file ) = $src_path;
		local( $comptemp ) = '';

		if( $compress ){
  			# No easy way to tell wether this was compressed or not
  			# for now just presume that it is.
  			local( $f ) = $src_file;
			$f =~ s/($shell_metachars)/\\$1/g;
  			$comptemp = "$big_temp/.out$$";
			&sys( "$compress_prog < \"$f\" > \"$comptemp\"" );
  			$src_file = $comptemp;
		}
		
		if( $no_rename ){
			$temp = $dest_path;
		}

		if( ! &ftp'put( $src_file, $temp, $restart ) ){
			&msg( $log, "Failed to put $src_file: $ftp'response\n" );
			unlink( $comptemp ) if $comptemp;
			return '';
		}
	
		unlink( $comptemp ) if $comptemp;
		if( !$no_rename && ! &ftp'rename( $temp, $dest_path ) ){
			&msg( $log, "Failed to remote rename $temp to $dest_path: $ftp'response\n" );
			return '';
		}

		local($filesize) = &filesize( $src_file );
		&msg( $log, "Put $src_file $filesize bytes\n" );

		&log_upload( $src_file, $dest_path, "", $filesize );

		# Some transfers done
		$exit_xfer_status |= $exit_xfers;

		if( $delete_source ){
			unlink( $src_file );
		}
		
		return $dest_path;
	}

	# Maybe TODO: Paul Szabo suggest that if recurse_hard is set then
	# mirror should chdir to the directory the file is in before getting
	# it.

	# Get a file
	&ftp'dostrip( $strip_cr );
	$start_time = time;
	if( ! &ftp'get( $src_path, $temp, $restart ) ){
		if( !$failed_gets_excl || $ftp'response !~ /$failed_gets_excl/ ){
			&msg( $log, "Failed to get $src_path: $ftp'response\n" );
		}

		# Time stamp the temp file to allow for a restart
		if( -f $temp ){
			&utime( $timestamp, $timestamp, $temp );
			# Make sure this file is kept
			local( $ti ) = $local_map{ $temp };
			&keep( $ti, $temp, *local_keep, *local_keep_totals, *local_map, 0 );
		}

		return '';
	}
	
	# Some transfers done
	$exit_xfer_status |= $exit_xfers;

	# delete source file after successful transfer
	if( $delete_source ){
		if( &ftp'delete( $src_path ) ){
			&msg( $log, "Deleted remote $src_path\n");
		}
		else {
			&msg( $log, "Failed to delete remote $src_path\n");
		}
	}

	if( $compress ){
		# Prevent the shell from expanding characters
  		local( $f ) = $temp;
  		local( $comp );
		$f =~ s/($shell_metachars)/\\$1/g;
  		$temp = "$f.$compress_suffix";
  		# Am I doing compress to gzip conversion?
   		if( $compress_conv_patt && $src_path =~ /$compress_conv_patt/ &&
  		    $compress_suffix eq $gzip_suffix ){
			$comp = "$sys_compress_prog -d < \"$f\" | $gzip_prog > \"$temp\"";
  		}
  		else {
			$comp = "$compress_prog < \"$f\" > \"$temp\"";
  		}
  		&sys( $comp );
		$temp =~ s/\\($shell_metachars)/$1/g;
		$f =~ s/\\($shell_metachars)/$1/g;
  		unlink( $f );
  	}

	local( $filesize ) = &filesize( $temp );
	local( $sizemsg ) = $filesize;
	local( $srcsize ) = $remote_size[ $remote_map{ $src_path } ];
	if( $srcsize > $sizemsg && !$compress ){
		# should never happen, right?  right ...
		$sizemsg .= " (file shrunk from $srcsize!)";
	}
	elsif( $srcsize < $sizemsg ){
		# compression wasn't such a great idea
		$sizemsg .= " (file grew from $srcsize!)";
	}

	# Ok - chop it up into bits!
	if( $split ){
		local( $time ) = 0;
		if( $force_times ){
			$time = $remote_time[ $remote_map{ $src_path } ];
		}
		&bsplit( $temp, $dest_path, $time );
		unlink( $temp );
		$got_mesg .= " and split";
	}
	else {
		if( -f $dest_path ){
			unlink( $dest_path );
		}
		if( ! rename( $temp, $dest_path ) ){
			&msg( $log, "Cannot rename $temp to $dest_path: $!\n" );
		}
	}

	local( $as ) = '';
	if( $src_path ne $dest_path ){
		$as = " as $dest_path";
	}
	$time_taken = time - $start_time;
	&msg( $log, "Got $src_path$as$got_mesg $sizemsg $time_taken\n" );
	# Make sure to keep what you just got!  It may/may not have
	# been compressed or gzipped..
	local( $locali ) = $local_map{ $dest_path };
	&keep( $locali, $dest_path, *local_keep, *local_keep_totals, *local_map, 1 );

	&log_upload( $src_path, $dest_path, $got_mesg, $filesize );

	return( $dest_path );
}

sub filename_to_tempname
{
	local( $dir, $file ) = @_;

	local ( $dest_path ) = $file;

	if( $dir eq '' ){
		if( $dest_path =~ /^(.+\/)([^\/]+)$/ ){
			($dir, $file) = ($1, $2);
		}
	}
	else {
		$file = $dest_path;
	}

	# dir 
# LIMITED NAMELEN
# if you are really limited in pathname length then
# change the .in. to just .
	if( $remote_fs eq 'macos' && ! $get_file ){
		return $dir . "tmp.$file";
	}
	return "$dir.in.$file.";
}


# Open, write, close - to try and ensure that the log will allways be filled
# in.
sub log_upload
{
	local( $src_path, $dest_path, $got_mesg, $size ) = @_;

	if( ! $upload_log ){
		return;
	}

	if( ! open( ULOG, ">>$upload_log" ) ){
		print STDERR "Cannot write to $upload_log\n";
		return;
	}

	&myflock( 'ULOG', $LOCK_EX );
	if( $get_files ){
		print ULOG "$site:$remote_dir/$src_path -> $local_dir/$dest_path $size ";
	}
	else {
		print ULOG "$local_dir/$dest_path -> $site:$remote_dir/$src_path $size ";
	}
	if( $got_mesg ){
		print ULOG "($got_mesg)";
	}
	print ULOG "\n";
	&myflock( 'ULOG', $LOCK_UN );
	close( ULOG );
}

sub do_deletes
{
	# This declaration must be "local()" because it modifies global data.
	local( *src_paths,
		*src_map,
		 *src_type, *src_keep,
		  *src_totals, *src_keep_totals ) = @_;
	local( $files_to_go, $dirs_to_go );
	
	if( ! ($do_deletes || $save_deletes) ){
		return;
	}
	
	local( $src_path, $i );
	local( $orig_do_deletes ) = $do_deletes;
	local( $orig_save_deletes ) = $save_deletes;

	local( $del_patt ) = $delete_patt;
	if( $delete_get_patt ){
		$del_patt = $get_patt;
	}
	
	$files_to_go = $src_totals[ 1 ] - $src_keep_totals[ 1 ];
	$dirs_to_go = $src_totals[ 0 ] - $src_keep_totals[ 0 ];

	# Adjust totals by considering del_patt
	for( $i = $#src_paths; $i >= 0; $i-- ){
		$src_path = $src_paths[ $i ];
		$srci = $i + 1;

		if( !$src_keep[ $srci ] && $src_path !~ /$del_patt/
		    || $delete_excl && $src_path =~ /$delete_excl/ ){
			if( $src_type[ $srci ] =~ "d" ){
				$dirs_to_go--;
			}
			else {
				$files_to_go--;
			}
		}
	}

	# Check out file deletions
	if( $max_delete_files =~ /^(\d+)\%$/ ){
		# There is a % in the value - so its a percentage
		local( $per ) = $1;
		if( $per <= 0 || 100 < $per ){
			&msg( "silly percentage $max_delete_files, not deleting\n" );
			$do_deletes = 0;
			$save_deletes = 0;
		}
		else {
			# Don't do more than this percentage of files
			# Convert max_delete_files into the number of files
			$max_delete_files =
				int( $src_totals[ 1 ] * $max_delete_files /100 );
		}
	}
	if( $files_to_go > $max_delete_files ){
		&msg( "Too many files to delete, not actually deleting ($files_to_go > $max_delete_files)\n" );
		$do_deletes = 0;
		$save_deletes = 0;
	}

	# Check out directory deletions
	if( $max_delete_dirs =~ /^(\d+)%$/ ){
		# There is a % in the value - so its a percentage
		local( $per ) = $1;
		if( $per <= 0 || 100 < $per ){
			&msg( "silly percentage $max_delete_dirs, not deleting\n" );
			$do_deletes = 0;
			$save_deletes = 0;
		}
		else {
			# Don't do more than this percentage of dirs
			# Convert max_delete_dirs into the number of dirs
			$max_delete_dirs =
				int( $src_totals[ 0 ] * $max_delete_dirs / 100 );
		}
	}

	if( $dirs_to_go > $max_delete_dirs ){
		&msg( "Too many directories to delete, not actually deleting ($dirs_to_go > $max_delete_dirs)\n" );
		$do_deletes = 0;
		$save_deletes = 0;
	}

	# Scan the list backwards so subdirectories are dealt with first
	for( $i = $#src_paths; $i >= 0; $i-- ){
		$src_path = $src_paths[ $i ];
		$srci = $i + 1;
	
		if( $src_keep[ $srci ] ){
			# Keep this for sure;
			&msg( "Keeping: $src_path\n" ) if $debug > 3;
			next;
		}

		if( $src_path !~ /$del_patt/ ){
			&msg( "   not in del_patt: $src_path\n" ) if $debug > 1;
			next;
		}

		if( $delete_excl && $src_path =~ /$delete_excl/ ){
			&msg( "   do not delete: $src_path\n" ) if $debug > 1;
			next;
		}

		if( $save_deletes && $save_dir =~ m,$cwd/(.*), ){
			local( $save_dir_tail ) = $1;
			if( $save_dir_tail && $src_path =~ m,$save_dir_tail/*, ){
				next;
			}
		}

		if( $save_deletes ){
			&save_delete( $src_path, $src_type[ $srci ] );
		}
		else {
			&do_delete( $src_path, $src_type[ $srci ] );
		}
	}
	
	$do_deletes = $orig_do_deletes;
	$save_deletes = $orig_save_deletes;
}
		
# Move aside the given file.  Kind is 'd' for dirs and 'f' for files.
sub save_delete
{
	local( $save, $kind ) = @_;

	local( $real_save_dir, $save_dest );
	eval "\$real_save_dir = \"$save_dir\"";


	if( ! $get_file ){
		&msg( "NEED TO implement remote save_deletes\n" );
		return;
	}
	
	$save_dest = "$real_save_dir/$save";

	if( $dont_do ){
		&msg( "Should save_delete $save to $save_dest\n" );
		return;
	}

	if( $kind eq 'd' ){
		$save_dest =~ s,/+$,,;
		
		# Make sure it exists
		&save_mkdir( $save_dest );
			
		# Zap the original
		if( rmdir( $save ) == 1 ){
			&msg( $log, "Removed directory $save\n" );
		}
		else {
			&msg( $log, "UNABLE TO REMOVE DIRECTORY $save\n" );
		}
		return;
	}

	# Save a file

	# Make the directories under $save_dir
	local( $dirname );
	$dirname = $save_dest;
	$dirname =~ s/\/[^\/]+$//;
	# Make sure the directory exists to mv the file into.
	&save_mkdir( $dirname );
		
	if( rename( $save, $save_dest ) == 1 ){
		&msg( $log, "Moved $save to $save_dest\n" );
	}
	else {
		system "$mv_prog '$save' '$save_dest'";
		if( ( $? >> 8 ) == 0 ){
			&msg( $log, "Moved $save to $save_dest\n" );
		}
		else {
			&msg( $log, "UNABLE TO MOVE $save TO $save_dest\n" );
		}
	}
}

sub save_mkdir
{
	local( $dir ) = @_;
	
	if( ! -d $dir ){
		if( &mkdirs( $dir ) ){
			&msg( $log, "Created save directory $dir\n" );
		}
		else {
			&msg( $log, "UNABLE TO CREATE $dir, aborting saves\n" );
			$save_deletes = 0;
		}
	}
}

# Delete the given file.  Kind is 'd' for dirs and 'f' for files.
sub do_delete
{
	local( $del, $kind ) = @_;
	
	if( $dont_do ){
		&msg( "Should delete $del\n" );
		return;
	}

	if( $kind eq 'd' ){
		$del =~ s,/+$,,;
		if( $do_deletes ){
			if( $get_file ){
				&msg( $log, "rmdir $cwd/$del\n" );
				rmdir( "$cwd/$del" ) ||
					&msg( $log, "rmdir $cwd/$del failed: $!\n" );
			}
			else {
				&msg( $log, "delete DIR $del\n" );
				&ftp'deldir( "$del" ) ||
					&msg( $log, "ftp delete DIR $del failed\n" );
			}
		}
		else {
			if( $get_file ){
				&msg( $log, "NEED TO rmdir $cwd/$del\n" );
			}
			else {
				&msg( $log, "NEED TO ftp'deldir $del\n" );
			}
		}
		return;
	}	

	# Deleting a file.
	if( $do_deletes ){
		if( $get_file ){
			&msg( $log, "unlink $cwd/$del\n" );
			unlink( "$cwd/$del" ) ||
				&msg( $log, "unlink $cwd/$del failed: $!\n" );
		}
		else {
			&msg( $log, "delete FILE $del\n" );
			&ftp'delete( "$del" ) ||
				&msg( $log, "ftp delete FILE $del failed\n" );
		}
	}
	else {
		if( $get_file ){
			&msg( $log, "NEED TO unlink $cwd/$del\n" );
		}
		else {
			&msg( $log, "NEED TO ftp'delete $del\n" );
		}
	}
}

sub filesize
{
	local( $fname ) = @_;

	if( ! -f $fname ){
		return -1;
	}

	return (stat( _ ))[ 7 ];
	
}

# Is the value
sub istrue
{
	local( $val ) = @_;
	
	return $val eq '1' || $val eq 'yes' || $val eq 'ok' ||
	       $val eq 'true';
}

sub mksymlink
{
	local( $dest_path, $existing_path ) = @_;

	if( ! $get_file ){
		&msg( "Cannot create symlinks on remote systems ($dest_path -> $existing_path)\n" );
		return;
	}
	
# Debian bug #85353 "bad symlink stops listing with -n" <sizif@pier.botik.ru> 
	if( $dont_do ){
		&msg( "Should symlink $dest_path to $existing_path\n" );
        	return;
	}

	# make the symlink locally

	# Zap any exiting file/symlink of that name
	if( -d $dest_path && ! -l $dest_path ){
		local( $msg ) = "rmdir( $dest_path ) before symlink";
		if( ! rmdir( $dest_path ) ){
			if( $algorithm == 1 ){
				$msg = "rmdir( $local_dir/$dest_path ) before symlink";
				&msg( "$msg failed: $!\n" );
			}
			&msg( "$msg failed: $!\n" );
			return;
		}
		elsif( $debug ){
			&msg( "$msg\n" );
		}
	}
	if( -e $dest_path || -l $dest_path ){
		local( $msg ) = "unlink( $dest_path ) before symlink";
		if( ! unlink( $dest_path ) ){
			&msg( "$msg failed: $!\n" );
			return;
		}
		elsif( $debug ){
			&msg( "$msg\n" );
		}
	}

	if( (eval 'symlink("","")', $@ eq '') ){
		local( $status ) = '';
		if( ! symlink( $existing_path, $dest_path ) ){
			$status = "Failed to ";
		}
		&msg( $log, $status . "symlink $existing_path to $dest_path\n" );
		&chown( $uid, $gid, $dest_path );
	}
	else {
		&msg( $log, "Tried to create symlink - but not supported locally\n" );
	}
}


# Make a full directory heirarchy
# returns true if the directory doesn't exist
sub mkdirs
{
	local( $dir ) = @_;
	local( @dir, $d, $path );

	# If the target directory already exists but is a symlink then
	# zap the symlink to recreate it as a directory
	if( $get_file && -l $dir ){
		unlink( $dir );
	}

	# Very often the directory does exist - so return now
	return 0 if &dir_exists( $dir );
	
	# Make sure that the target directory exists
	@dirs = split( '/', $dir );
	
	# the root directory always exists
	$path = '';
	if( $dirs[ 0 ] eq '' ){ 
		shift( @dirs ); 
		$path = '/';
	}

	foreach $d ( @dirs ){
		$path = $path . $d;
		if( ! &dir_exists( $path ) ){
			&msg( "mkdir $path\n" ) if $debug > 2;
			if( ! &make_dir( $path, 0755 ) ){
				&msg( "make_dir($path,0755) failed with $err\n" );
				return 0;
			}
			&set_attribs( $path, $path, 'd' );
		}
		$path .= "/";
	}
	return 1;
}

# return 0 on error, 1 on success
sub make_dir
{
	local( $dir, $mode ) = @_;
	local( $val );

	if( $get_file ){
		if( $on_win && $dir =~ /^[a-z]:$/i ){
			return 1;
		}
		# make a local directory
		if( -e $dir || -l $dir ){
			unlink( $dir );
		}
		$val = mkdir( $dir, $mode );
		$err = "$!";
	}
	else {
		# make a remote directory
		$val = &ftp'mkdir( $dir );

		# The mkdir might have failed due to bad mode
		# So try to chmod it anyway
		if( $remote_has_chmod ){
			$val = &ftp'chmod( $dir, $mode );
		}
	}

	return $val;
}

# return 1 if $dir exists, 0 if not
sub dir_exists
{
	local( $dir ) = @_;
	local( $val );

	if( $get_file ){
		# check if local directory exists
		$val = (-d $dir);
	}
	else {
		# check if remote directory exists
		local($old_dir) = &ftp'pwd();		
		
		$val = &ftp'cwd($dir);

		# If I didn't manage to change dir should be where I was!
		if( $val ){
			# go back to the original directory
			&ftp'cwd($old_dir) || die "Cannot cd to original remote directory";
		}
	}
	return $val;
}

# Set file/directory attributes
sub set_attribs
{
	local( $path, $src_path, $type ) = @_;
	local( $mode );
	
	if( ! $chmod ){
		&msg( "dont chmod \"$path\"\n" ) if $debug > 2;
		return;
	}

	if( $get_file ){
		local( $pathi ) = $remote_map{ $src_path };
		$mode = $remote_mode[ $pathi ];
	}
	else {
		local( $pathi ) = $local_map{ $path };
		$mode = $local_mode[ $pathi ];
	}

	# If I can't figure out the mode or I'm not copying it
	# use the default
	if( !$mode_copy || !$mode ){
		if( $type eq 'f' ){
			$mode = $file_mode;
		}
		elsif( $type eq 'd' ){
			$mode = $dir_mode;
		}
	}

	# Convert from octal
	# Suggested patch to limit bits being set
	# $mode = $mode & 0777;
	$mode = oct( $mode ) if $mode =~ /^0/;

	if( $get_file ){
		# Change local

		chmod $mode, $path;

		if( $user ne '' && $group ne '' ){
			&chown( $uid, $gid, $path );
		}
	}
	else {
		# change the remote file
		if( $remote_has_chmod ){
			&ftp'chmod( $path, $mode );
		}
	}
}


sub get_passwd
{
	local( $user ) = @_;
	local( $pass );
	local( $| ) = 1;

	# prompt for a password
	$SIG{ 'INT' } = 'IGNORE';
	$SIG{ 'QUIT' } = 'IGNORE';

	system "stty -echo </dev/tty >/dev/tty 2>&1";
	print "Password for $user: ";

	$pass = <STDIN>;
	print "\n";
	chop( $pass );

	system "stty echo </dev/tty >/dev/tty 2>&1";

	$SIG{ 'INT' } = 'DEFAULT';
	$SIG{ 'QUIT' } = 'DEFAULT';
	
	return $pass;
}

sub compare_times
{
	# Try and allow for time zone changes (eg when a site
	# switches from daylight saving to non daylight saving)
	# by ignoring differences of exactly one hour

	local( $t1, $t2 ) = @_;
	local( $diff ) = ($t1 > $t2 ? $t1 - $t2 : $t2 - $t1);

	return ($t1 > $t2) && ($diff != 3600);
}

sub create_assocs
{
	local( $map );

	&delete_assocs();

	&msg( "creating assocs ...\n" ) if $debug > 2;
	foreach $map ( @assocs ){
		eval "\$$map = \"\$big_temp/$map.$$\"";
		eval "dbmopen( $map, \$$map, 0644 )";
	}
	&msg( "creating assocs done\n" ) if $debug > 2;
}

sub delete_assocs
{
	local( $map );

	&msg( "deleting assocs ...\n" ) if $debug > 2;
	foreach $map ( @assocs ){
		eval "\$$map = \"\$big_temp/$map.$$\"";
		eval "dbmclose( $map )";
		&unlink_dbm( eval "\$$map" );
		eval "\%$map = ()";
	}
	&msg( "deleting assocs done\n" ) if $debug > 2;
}

sub unlink_dbm
{
	local( $file ) = @_;
	unlink "$file.pag" if -e "$file.pag";
	unlink "$file.dir" if -e "$file.dir";
	unlink "$file.gdbm" if -e "$file.gdbm";
	unlink "$file" if -e "$file";
}

# Chop the tmp file up
sub bsplit
{
	local( $temp, $dest_path, $time ) = @_;
	local( $dest_dir ) = "$dest_path-split";
	local( $bufsiz ) = 512;
	local( $buffer, $in, $sofar );

	&msg( "Splitting up $temp into $dest_dir/ ($time)\n" ) if $debug;

	# Stomp on the original directories
	local( $d ) = $dest_dir;
	$d =~ s/($shell_metachars)/\\$1/g;
	&sys( "$rm_prog -rf \"$d\"" );

	&mkdirs( $dest_dir );

	local( $index ) = "00";
	local( $part );
	open( TMP, $temp ) || die "Cannot open $temp!";
	$sofar = $split_chunk; # Force a new file
	while( ($in = sysread( TMP, $buffer, $bufsiz )) > 0 ){
		if( $sofar >= $split_chunk ){
			if( $part ){
				close( PART );
				if( $time ){
					&set_timestamp( $part, $time );
				}
			}
			$index++;
			$part = "$dest_dir/part$index";
			&msg( "creating $part\n" ) if $debug;
			open( PART, ">$part" ) || die "Cannot create $part";
			# Make sure to keep this!
			local( $locali ) = $local_map{ $part };
			&keep( $locali, $part, *local_keep, *local_keep_totals, *local_map, 1 );
			$sofar = 0;
		}
		if( ($out = syswrite( PART, $buffer, $in )) != $in ){
			die "Failed to write data to $part";
		}
		$sofar += $in;
	}
	close( PART );
	if( $time ){
		&set_timestamp( $part, $time );
	}
	close( TMP );

	# Generate a readme file about what is in the split directory
	local( $readme ) = "$dest_dir/README";
	open( README, ">$readme" ) || die "Cannot create $readme";
	print README "This directory contains a splitup version of $dest_path\n";
	print README "to recreate the original simply concatenate all the\n";
	print README "parts back together.\n\nChecksums are:\n\n";
	close README;
	&sys( "(cd \"$d\" ; $sum_prog part*)>> $readme" );
}

sub sys
{
	local( $com ) = @_;
	&msg( "$com\n" ) if $debug > 2;
	return system( $com ) / 256;
}

# Set up an associative array given all an array of keys.
# @fred = ( 'a' );
# &set_assoc_from_array( *fred )
# Creates => $fred{ 'a' } = 1
#
sub set_assoc_from_array
{
	# This declaration must be "local()" because it modifies global data.
	local( *things ) = @_;
	foreach $thing ( @things ){
		$things{ $thing } = 1;
	}
}

sub find_prog
{
	local( $prog ) = @_;
	local( $path ) = $ENV{ 'PATH' };

	foreach $dir ( split( /$path_sep/, $path ) ){
		local( $path ) = $dir . $file_sep . $prog;
		if( -x $path ){
			return $path;
		}
		if( $on_win ){
			$path .= ".exe";
			if( -x $path ){
				return $path;
			}
		}
	}
	return '';
}

sub real_dir_from_path
{
	local( $program ) = @_;
	local( @prog_path ) = split( m:$file_sep_pat: , $program );	# dir collection
	local( $dir );

	while( -l $program ){				# follow symlink
		$program = readlink( $program );
		if( $program =~ m:^$file_sep_pat: ){	# full path?
			@prog_path = ();		# start dir collection anew
		}
		else {
			pop( @prog_path );		# discard file name
		}
		push( @prog_path, split( m:$file_sep_pat:, $program ) );# add new parts
		$program = join( $file_sep, @prog_path );  # might be a symlink again...
	}
	pop( @prog_path );
	$dir = join( $file_sep, @prog_path );

	if( ! $dir ){
		$dir = '.';
	}
	
	return $dir;
}

sub msg
{
	local( $todo, $msg ) = (0, "");

	if( $#_ == 1 ){
		($todo, $msg) = @_;
	}
	else {
		$todo = 0;
		$msg = $_[ 0 ];
	}

	# Assign to $0 so when you do a 'ps' it says this!
	if( defined $package && 
		defined $site &&
		defined $remote_dir ){
		$0 =  "mirror $package:$site:$remote_dir $msg";
	}
	else {
		$0 = "mirror $msg";
	}

	if( $todo & $log ){
		push( @log, $msg );
	}
# Not sure about this one.  always print the message even if its a log msg.
#	else {
		print $msg;
#	}
}

sub to_bytes
{
	local( $size ) = @_;
	if( $size =~ /^(\d+)\s*(k|b|m)s*$/i ){
		$size = $1;
		if( $2 =~ /[mM]/ ){
			$size *= (1024*1024);
		}
		elsif( $2 =~ /[bB]/ ){
			$size *= 512;
		}
		elsif( $2 =~ /[kK]/ ){
			$size *= 1024;
		}
	}
	return $size;
}

# Given a unix filename map it into a vms name.
# $kind is 'f' for files and 'd' for directories
sub unix2vms
{
	local( $v, $kind ) = @_;

	if( $v eq '.' || $v eq '/' ){
		return "[]";
	}

	if( $vms_dir ){
		$v = $vms_dir . '/' . $v;
	}

	if( $kind eq 'f' ){
		# Map a/b/c.txt into [a.b]c.txt
		if( $v =~ m,(.*)/([^/]+), ){
			local( $dir, $rest ) = ($1, $2);
			$dir =~ s,/,.,g;
			$v = "[$dir]$rest";
		}
	}
	else {
		# Map a/b/c into [a.b.c]
		$v =~ s,/,.,g;
		$v = "[$v]";
	}
	return $v;
}

sub dirpart
{
	local( $path ) = @_;
	if( $path =~ m:/: ){
		$path =~ s:^(.*)/[^/]+$:$1:;
	}
	else {
		$path = '.';
	}
	return $path;
}

# Given a filename (not a directory) and what path it symlinks to
# return a, hopefully, non-relative pathname that the symlink
# really points to.  This is so it can be used to index into the $src_path
# map.
sub expand_symlink
{
	local( $orig_path, $points_to ) = @_;
	local( $dirpart ) = &dirpart( $orig_path );

	return &flatten_path( "$dirpart/$points_to" );
}

# flatten out the effects of dir/.. and /./
# The problem is not flattening out ../.. into nothing!  Hence
# the contortions below.
sub flatten_path
{
	local( $path ) = @_;
	local( $changed ) = 1;
	local( $i );
	
	local( $rooted ) = $path =~ m:^/:;
	local( $count ) = 0;
	local( $orig_path ) = $path;
	
	$path =~ s:^/::;
	$path =~ s:(^|/)\.(/|$)::g;
	$path =~ s:/+:/:g;

	while( $changed ){
		if( $count++ > 100 ){
			&msg( $log, "LOOPING in flatten_path orig = $orig_path, path now $path\n" );
			last;
		}
		local( $in ) = $path;
		local( @parts ) = split( /\//, $path );
		for( $i = 0; $i <= $#parts; $i++ ){
			if( $parts[ $i ] eq '.' ){
				$parts[ $i ] = undef;
				next;
			}
			if( $i > 0 && $parts[ $i ] eq '..' &&
			   $parts[ $i - 1 ] && $parts[ $i - 1 ] ne '..' ){
				$parts[ $i - 1 ] = $parts[ $i ] = undef;
				next;
			}
		}
		$path = '';
		for( $i = 0; $i <= $#parts; $i++ ){
			next unless $parts[ $i ];
			$path .= '/' if $path ne '';
			$path .= $parts[ $i ];
		}
		$changed = $in ne $path;
	}
	if( $rooted ){
		$path = "/$path";
	}
	return $path;
}


# Fix up a package name.
# strip trailing and leading ws and replace awkward characters
# This doesn't guarentee a unique filename.
sub fix_package
{
	local( $package ) = @_;
	$package =~ s:[\s/']:_:g;
	return $package;
}

sub will_compress
{
	$src_type[ $_[1] ] eq 'f' &&
	$compress_patt && $_[0] =~ /$compress_patt/ &&
	( ! $compress_size_floor ||
	  $compress_size_floor < $src_size[ $_[1] ] ) &&
	!($compress_excl && $_[0] =~ /$compress_excl/i) &&
	!($compress_suffix eq $gzip_suffix &&
	  $compress_conv_patt && $_[0] =~ /$compress_conv_patt/);
}

sub will_split
{
 	$split_max &&
	$src_size[ $_[1] ] > $split_max &&
	$src_type[ $_[1] ] eq 'f' &&
	$split_patt && $_[0] =~ /$split_patt/;
}

sub myflock
{
	local( $file, $kind ) = @_;

	if( ! $can_flock ){
		return;
	}

	eval( "flock( \$file, $kind )" );
	if( $@ =~ /unimplemented/ ){
		$can_flock = 0;
		warn "flock not unavialable, running unlocked\n";
	}
}

sub t2str
{
	local( @t );
	if( $use_timelocal ){
		@t = localtime( $_[0] );
	}
	else {
		@t = gmtime( $_[0] );
	}
	local($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = @t;

# Debian bug #48611, 1969 appeared as 2069, dsb@smart.net.
	$year += 1900;
	    
	return sprintf( "%04d/%02d/%02d-%02d:%02d:%02d",
		$year, $mon + 1, $mday, $hour, $min, $sec );
}

sub handler
{
	$sigs ++;
	if( $sigs > $max_sigs ){
		exit( 0 );
	}

        local( $sig ) = @_;
        local( $msg ) = "Caught a SIG$sig shutting down";
        local( $package, $filename, $line ) = caller;
        warn "$package:$filename:$line $msg";
        exit( 0 );
}

sub trap_signals
{
	local( $sig );
        foreach $sig ( 'HUP', 'INT', 'QUIT', 'ILL',
                        'TRAP', 'IOT', 'BUS', 'FPE',
                        'USR1', 'SEGV', 'USR2',
                        'PIPE', 'ALRM', 'TERM' ){
                $SIG{ $sig } = "main\'handler";
        }
}

sub map_user_group
{
	if( ! defined( $uid ) ){
		if( $user =~ /^\d+$/ ){
			# User is just a number - presume it is the uid
			$uid = $user;
		}
		else {
			$uid = (getpwnam( $user ))[ 2 ];
		}
	}
	if( ! defined( $gid ) ){
		if( $group =~ /\d+$/ ){
			# Group is just a number - presume it is the gid
			$gid = $group;
		}
		else {
			$gid = (getgrnam( $group ))[ 2 ];
		}
	}
}

sub keep
{
	local( $pathi, $path, *keep, *keep_totals, *keep_map, $kind ) = @_;
	# If pathi is already kept nothing to do
	if( $pathi eq '' ){
		&msg( "   keep $path NOTE null pathi\n" ) if $debug > 3;
		return;
	}
	if( $keep[ $pathi ] ){
		&msg( "   keep $path [$pathi] NOTE already kept\n" ) if $debug > 3;
		return;
	}

	$keep[ $pathi ] = 1;
	$keep_totals[ $kind ]++;
	&msg( "   keep $path\n" ) if $debug > 3;

	# Keep all the parent directories
	while( $path =~ m,^(.*)/([^/]+)$, ){
		$path = $1;
		$pathi = $keep_map{ $path };
		if( $pathi eq '' ){
			&msg( "   keep $path NOTE null pathi\n" ) if $debug > 3;
			return;
		}
		if( $keep[ $pathi ] ){
			&msg( "   keep $path [$pathi] NOTE already kept\n" ) if $debug > 3;
			return;
		}

		$keep[ $pathi ] = 1;
		$keep_totals[ 0 ]++;
	}
}

sub alarm
{
	local( $time_to_sig ) = @_;
	eval "alarm( $time_to_sig )";
}

sub chown
{
	local( $uid, $gid, $path ) = @_;
	eval "chown ( \$uid, \$gid, \$path )";
}

sub utime
{
	local( $atime, $mtime, $path ) = @_;
	if( ! $on_win ){
		return utime( $atime, $mtime, $path );
	}
	
	# On windoze I might have set attribs to allow the time to be changed first
	local( $old_mode ) = (stat( $path ))[ 2 ]; 
	local( $tmp_mode ) = $old_mode;
	local( $ret );

	$tmp_mode |= 0700;
	chmod( $tmp_mode, $path );
	$ret = utime( $atime, $mtime, $path );
	chmod( $old_mode, $path );
	return $ret;
}

sub cwd
{
	local( $lcwd ) = '';
	eval "\$lcwd = $win_getcwd";
	
	if( ! ($lcwd eq '' || $lcwd eq $win_getcwd) ){
		# Must be on windoze!
		$cwd = $lcwd;
	}
	else {
		# didn't manage it try and run the pwd command instead
		chop( $cwd = `pwd` );
	}
	return $cwd;
}
