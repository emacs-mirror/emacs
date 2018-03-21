#!/bin/sh
### autogen.sh - tool to help build Emacs from a repository checkout

## Copyright (C) 2011-2018 Free Software Foundation, Inc.

## Author: Glenn Morris <rgm@gnu.org>
## Maintainer: emacs-devel@gnu.org

## This file is part of GNU Emacs.

## GNU Emacs is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## GNU Emacs is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

### Commentary:

## The Emacs repository does not include the configure script (and
## associated helpers).  The first time you fetch Emacs from the repo,
## run this script to generate the necessary files.
## For more details, see the file INSTALL.REPO.

### Code:

## Tools we need:
## Note that we respect the values of AUTOCONF etc, like autoreconf does.
progs="autoconf"

## Minimum versions we need:
autoconf_min=`sed -n 's/^ *AC_PREREQ(\([0-9\.]*\)).*/\1/p' configure.ac`


## $1 = program, eg "autoconf".
## Echo the version string, eg "2.59".
## FIXME does not handle things like "1.4a", but AFAIK those are
## all old versions, so it is OK to fail there.
## Also note that we do not handle micro versions.
get_version ()
{
    vers=`($1 --version) 2> /dev/null` && expr "$vers" : '[^
]* \([0-9][0-9.]*\).*'
}

## $1 = version string, eg "2.59"
## Echo the major version, eg "2".
major_version ()
{
    echo $1 | sed -e 's/\([0-9][0-9]*\)\..*/\1/'
}

## $1 = version string, eg "2.59"
## Echo the minor version, eg "59".
minor_version ()
{
    echo $1 | sed -e 's/[0-9][0-9]*\.\([0-9][0-9]*\).*/\1/'
}

## $1 = program
## $2 = minimum version.
## Return 0 if program is present with version >= minimum version.
## Return 1 if program is missing.
## Return 2 if program is present but too old.
## Return 3 for unexpected error (eg failed to parse version).
check_version ()
{
    ## Respect, e.g., $AUTOCONF if it is set, like autoreconf does.
    uprog0=`echo $1 | sed -e 's/-/_/g' -e 'y/abcdefghijklmnopqrstuvwxyz/ABCDEFGHIJKLMNOPQRSTUVWXYZ/'`

    eval uprog=\$${uprog0}

    if [ x"$uprog" = x ]; then
        uprog=$1
    else
        printf '%s' "(using $uprog0=$uprog) "
    fi

    ## /bin/sh should always define the "command" builtin, but
    ## sometimes it does not on hydra.nixos.org.
    ## /bin/sh = "BusyBox v1.27.2", "built-in shell (ash)".
    ## It seems to be an optional compile-time feature in that shell:
    ## see ASH_CMDCMD in <https://git.busybox.net/busybox/tree/shell/ash.c>.
    if command -v command > /dev/null 2>&1; then
        command -v $uprog > /dev/null || return 1
    else
        $uprog --version > /dev/null 2>&1 || return 1
    fi
    have_version=`get_version $uprog` || return 4

    have_maj=`major_version $have_version`
    need_maj=`major_version $2`

    [ x"$have_maj" != x ] && [ x"$need_maj" != x ] || return 3

    [ $have_maj -gt $need_maj ] && return 0
    [ $have_maj -lt $need_maj ] && return 2

    have_min=`minor_version $have_version`
    need_min=`minor_version $2`

    [ x"$have_min" != x ] && [ x"$need_min" != x ] || return 3

    [ $have_min -ge $need_min ] && return 0
    return 2
}

do_check=true
do_autoconf=false
do_git=false

for arg; do
    case $arg in
      --help)
	exec echo "$0: usage: $0 [--no-check] [target...]
  Targets are: all autoconf git";;
      --no-check)
        do_check=false;;
      all)
	do_autoconf=true
	test -r .git && do_git=true;;
      autoconf)
	do_autoconf=true;;
      git)
	do_git=true;;
      *)
	echo >&2 "$0: $arg: unknown argument"; exit 1;;
    esac
done

case $do_autoconf,$do_git in
  false,false)
    do_autoconf=true
    test -r .git && do_git=true;;
esac

# Generate Autoconf-related files, if requested.

if $do_autoconf; then

  if $do_check; then

    echo 'Checking whether you have the necessary tools...
(Read INSTALL.REPO for more details on building Emacs)'

    missing=

    for prog in $progs; do

      sprog=`echo "$prog" | sed 's/-/_/g'`

      eval min=\$${sprog}_min

      printf '%s' "Checking for $prog (need at least version $min) ... "

      check_version $prog $min

      retval=$?

      case $retval in
          0) stat="ok" ;;
          1) stat="missing" ;;
          2) stat="too old" ;;
          4) stat="broken?" ;;
          *) stat="unable to check" ;;
      esac

      echo $stat

      if [ $retval -ne 0 ]; then
          missing="$missing $prog"
          eval ${sprog}_why=\""$stat"\"
      fi

    done


    if [ x"$missing" != x ]; then

      echo '
Building Emacs from the repository requires the following specialized programs:'

      for prog in $progs; do
          sprog=`echo "$prog" | sed 's/-/_/g'`

          eval min=\$${sprog}_min

          echo "$prog (minimum version $min)"
      done


      echo '
Your system seems to be missing the following tool(s):'

      for prog in $missing; do
          sprog=`echo "$prog" | sed 's/-/_/g'`

          eval why=\$${sprog}_why

          echo "$prog ($why)"
      done

      echo '
If you think you have the required tools, please add them to your PATH
and re-run this script.

Otherwise, please try installing them.
On systems using rpm and yum, try: "yum install PACKAGE"
On systems using dpkg and apt, try: "apt-get install PACKAGE"
Then re-run this script.

If you do not have permission to do this, or if the version provided
by your system is too old, it is normally straightforward to build
these packages from source.  You can find the sources at:

https://ftp.gnu.org/gnu/PACKAGE/

Download the package (make sure you get at least the minimum version
listed above), extract it using tar, then run configure, make,
make install.  Add the installation directory to your PATH and re-run
this script.

If you know that the required versions are in your PATH, but this
script has made an error, then you can simply re-run this script with
the --no-check option.

Please report any problems with this script to bug-gnu-emacs@gnu.org .'

      exit 1
    fi

    echo 'Your system has the required tools.'

  fi                            # do_check

  # Build aclocal.m4 here so that autoreconf need not use aclocal.
  # aclocal is part of Automake and might not be installed, and
  # autoreconf skips aclocal if aclocal.m4 is already supplied.
  ls m4/*.m4 | LC_ALL=C sort | sed 's,.*\.m4$,m4_include([&]),' \
    > aclocal.m4.tmp || exit
  if cmp -s aclocal.m4.tmp aclocal.m4; then
    rm -f aclocal.m4.tmp
  else
    echo "Building aclocal.m4 ..."
    mv aclocal.m4.tmp aclocal.m4
  fi || exit

  echo "Running 'autoreconf -fi -I m4' ..."

  ## Let autoreconf figure out what, if anything, needs doing.
  ## Use autoreconf's -f option in case autoreconf itself has changed.
  autoreconf -fi -I m4 || exit
fi


# True if the Git setup was OK before autogen.sh was run.

git_was_ok=true

if $do_git; then
    case `cp --help 2>/dev/null` in
      *--backup*--verbose*)
	cp_options='--backup=numbered --verbose';;
      *)
	cp_options='-f';;
    esac
fi


# Like 'git config NAME VALUE' but verbose on change and exiting on failure.
# Also, do not configure unless requested.

git_config ()
{
    $do_git || return

    name=$1
    value=$2

    ovalue=`git config --get "$name"` && test "$ovalue" = "$value" || {
       if $git_was_ok; then
	   echo 'Configuring local git repository...'
	   case $cp_options in
	       --backup=*)
		   config=$git_common_dir/config
		   cp $cp_options --force -- "$config" "$config" || exit;;
	   esac
       fi
       echo "git config $name '$value'"
       git config "$name" "$value" || exit
       git_was_ok=false
    }
}

## Configure Git, if requested.

# Get location of Git's common configuration directory.  For older Git
# versions this is just '.git'.  Newer Git versions support worktrees.

{ test -r .git &&
  git_common_dir=`git rev-parse --no-flags --git-common-dir 2>/dev/null` &&
  test -n "$git_common_dir"
} || git_common_dir=.git
hooks=$git_common_dir/hooks

# Check hashes when transferring objects among repositories.

git_config transfer.fsckObjects true


# Configure 'git diff' hunk header format.

git_config diff.elisp.xfuncname \
	   '^\(def[^[:space:]]+[[:space:]]+([^()[:space:]]+)'
git_config 'diff.m4.xfuncname' '^((m4_)?define|A._DEFUN(_ONCE)?)\([^),]*'
git_config 'diff.make.xfuncname' \
	   '^([$.[:alnum:]_].*:|[[:alnum:]_]+[[:space:]]*([*:+]?[:?]?|!?)=|define .*)'
git_config 'diff.shell.xfuncname' \
	   '^([[:space:]]*[[:alpha:]_][[:alnum:]_]*[[:space:]]*\(\)|[[:alpha:]_][[:alnum:]_]*=)'
git_config diff.texinfo.xfuncname \
	   '^@node[[:space:]]+([^,[:space:]][^,]+)'


# Install Git hooks.

tailored_hooks=
sample_hooks=

for hook in commit-msg pre-commit; do
    cmp -- build-aux/git-hooks/$hook "$hooks/$hook" >/dev/null 2>&1 ||
	tailored_hooks="$tailored_hooks $hook"
done

git_sample_hook_src ()
{
    hook=$1
    src=$hooks/$hook.sample
    if test ! -r "$src"; then
	case $hook in
	    applypatch-msg) src=build-aux/git-hooks/commit-msg;;
	    pre-applypatch) src=build-aux/git-hooks/pre-commit;;
	esac
    fi
}
for hook in applypatch-msg pre-applypatch; do
    git_sample_hook_src $hook
    cmp -- "$src" "$hooks/$hook" >/dev/null 2>&1 ||
	sample_hooks="$sample_hooks $hook"
done

if test -n "$tailored_hooks$sample_hooks"; then
    if $do_git; then
	echo "Installing git hooks..."

	if test ! -d "$hooks"; then
	    printf "mkdir -p -- '%s'\\n" "$hooks"
	    mkdir -p -- "$hooks" || exit
	fi

	if test -n "$tailored_hooks"; then
	    for hook in $tailored_hooks; do
		dst=$hooks/$hook
		cp $cp_options -- build-aux/git-hooks/$hook "$dst" || exit
		chmod -- a-w "$dst" || exit
	    done
	fi

	if test -n "$sample_hooks"; then
	    for hook in $sample_hooks; do
		git_sample_hook_src $hook
		dst=$hooks/$hook
		cp $cp_options -- "$src" "$dst" || exit
		chmod -- a-w "$dst" || exit
	    done
	fi
    else
	git_was_ok=false
    fi
fi

if test ! -f configure; then
    echo "You can now run '$0 autoconf'."
elif test -r .git && test $git_was_ok = false && test $do_git = false; then
    echo "You can now run '$0 git'."
elif test ! -f config.status ||
	test -n "`find configure src/config.in -newer config.status`"; then
    echo "You can now run './configure'."
fi

exit 0

### autogen.sh ends here
