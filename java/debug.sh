#!/bin/bash
### Run Emacs under GDB or JDB on Android.

## Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

set -m
oldpwd=`pwd`
cd `dirname $0`

devices=`adb devices | grep device | awk -- '/device\y/ { print $1 }' -`
device=
progname=$0
package=org.gnu.emacs
activity=org.gnu.emacs.EmacsActivity
gdb_port=5039
jdb_port=64013
jdb=no
attach_existing=no
gdbserver=
gdb=gdb

while [ $# -gt 0 ]; do
    case "$1" in
	## This option specifies the serial number of a device to use.
	"--device" )
	    device="$2"
	    if [ -z device ]; then
		echo "You must specify an argument to --device"
		exit 1
	    fi
	    shift
	    ;;
	"--help" )
	    echo "Usage: $progname [options] -- [gdb options]"
	    echo ""
	    echo "  --device DEVICE	run Emacs on the specified device"
	    echo "  --port PORT		run the GDB server on a specific port"
	    echo "  --jdb-port PORT	run the JDB server on a specific port"
	    echo "  --jdb		run JDB instead of GDB"
	    echo "  --gdb		use specified GDB binary"
	    echo "  --attach-existing	attach to an existing process"
	    echo "  --gdbserver BINARY	upload and use the specified gdbserver binary"
	    echo "  --help		print this message"
	    echo ""
	    echo "Available devices:"
	    for device in $devices; do
		echo "  " $device
	    done
	    echo ""
	    exit 0
	    ;;
	"--jdb" )
	    jdb=yes
	    ;;
	"--gdb" )
	    shift
	    gdb=$1
	    ;;
	"--gdbserver" )
	    shift
	    gdbserver=$1
	    ;;
	"--port" )
	    shift
	    gdb_port=$1
	    ;;
	"--jdb-port" )
	    shift
	    jdb_port=$1
	    ;;
	"--attach-existing" )
	    attach_existing=yes
	    ;;
	"--" )
	    shift
	    gdbargs=$@
	    break;
	    ;;
	* )
	    echo "$progname: Unrecognized argument $1"
	    exit 1
	    ;;
    esac
    shift
done

if [ -z "$devices" ]; then
    echo "No devices are available."
    exit 1
fi

if [ `wc -w <<< "$devices"` -gt 1 ] && [ -z $device ]; then
    echo "Multiple devices are available.  Please specify one with"
    echo "the option --device and try again."
    exit 1
fi

if [ -z $device ]; then
    device=$devices
fi

echo "Looking for $package on device $device"

# Find the application data directory
app_data_dir=`adb -s $device shell run-as $package sh -c 'pwd 2> /dev/null'`

if [ -z $app_data_dir ]; then
   echo "The data directory for the package $package was not found."
   echo "Is it installed?"
fi

echo "Found application data directory at" "$app_data_dir"

# Generate an awk script to extract PIDs from Android ps output.  It
# is enough to run `ps' as the package user on newer versions of
# Android, but that doesn't work on Android 2.3.
cat << EOF > tmp.awk
BEGIN {
  pid = 0;
  pid_column = 2;
}

{
  # Remove any trailing carriage return from the input line.
  gsub ("\r", "", \$NF)

  # If this is line 1, figure out which column contains the PID.
  if (NR == 1)
    {
      for (n = 1; n <= NF; ++n)
	{
	  if (\$n == "PID")
	    pid_column=n;
	}
    }
  else if (\$NF == "$package")
   print \$pid_column
}
EOF

# Make sure that file disappears once this script exits.
trap "rm -f $(pwd)/tmp.awk" 0

# First, run ps to fetch the list of process IDs.
package_pids=`adb -s $device shell ps`

# Next, extract the list of PIDs currently running.
package_pids=`awk -f tmp.awk <<< $package_pids`

if [ "$attach_existing" != "yes" ]; then
    # Finally, kill each existing process.
    for pid in $package_pids; do
	echo "Killing existing process $pid..."
	adb -s $device shell run-as $package kill -9 $pid &> /dev/null
    done

    # Now run the main activity.  This must be done as the adb user and
    # not as the package user.
    echo "Starting activity $activity and attaching debugger"

    # Exit if the activity could not be started.
    adb -s $device shell am start -D -n "$package/$activity"
    if [ ! $? ]; then
	exit 1;
    fi

    # Sleep for a bit.  Otherwise, the process may not have started
    # yet.
    sleep 1

    # Now look for processes matching the package again.
    package_pids=`adb -s $device shell ps`

    # Next, remove lines matching "ps" itself.
    package_pids=`awk -f tmp.awk <<< $package_pids`
fi

rm tmp.awk

pid=$package_pids
num_pids=`wc -w <<< "$package_pids"`

if [ $num_pids -gt 1 ]; then
    echo "More than one process was started:"
    echo ""
    adb -s $device shell run-as $package ps | awk -- "{
      if (!match (\$0, /ps/) && match (\$0, /$package/))
        print \$0
    }"
    echo ""
    printf "Which one do you want to attach to? "
    read pid
elif [ -z $package_pids ]; then
    echo "No processes were found to attach to."
    exit 1
fi

# If either --jdb was specified or debug.sh is not connecting to an
# existing process, then store a suitable JDB invocation in
# jdb_command.  GDB will then run JDB to unblock the application from
# the wait dialog after startup.

if [ "$jdb" = "yes" ] || [ "$attach_existing" != yes ]; then
    adb -s $device forward --remove-all
    adb -s $device forward "tcp:$jdb_port" "jdwp:$pid"

    if [ ! $? ]; then
	echo "Failed to forward jdwp:$pid to $jdb_port!"
	echo "Perhaps you need to specify a different port with --port?"
	exit 1;
    fi

    jdb_command="jdb -connect \
		 com.sun.jdi.SocketAttach:hostname=localhost,port=$jdb_port"

    if [ $jdb = "yes" ]; then
	# Just start JDB and then exit
	$jdb_command
	exit 1
    fi
fi

if [ -n "$jdb_command" ]; then
    echo "Starting JDB to unblock application."

    # Start JDB to unblock the application.
    coproc JDB { $jdb_command; }

    # Tell JDB to first suspend all threads.
    echo "suspend" >&${JDB[1]}

    # Tell JDB to print a magic string once the program is
    # initialized.
    echo "print \"__verify_jdb_has_started__\"" >&${JDB[1]}

    # Now wait for JDB to give the string back.
    line=
    while :; do
	read -u ${JDB[0]} line
	if [ ! $? ]; then
	    echo "Failed to read JDB output"
	    exit 1
	fi

	case "$line" in
	    *__verify_jdb_has_started__*)
		# Android only polls for a Java debugger every 200ms, so
		# the debugger must be connected for at least that long.
		echo "Pausing 1 second for the program to continue."
		sleep 1
		break
		;;
	esac
    done

    # Note that JDB does not exit until GDB is fully attached!
fi

# See if gdbserver has to be uploaded
gdbserver_cmd=
is_root=
if [ -z "$gdbserver" ]; then
    gdbserver_bin=/system/bin/gdbserver64
else
    gdbserver_bin=/data/local/tmp/gdbserver
    gdbserver_cat="cat $gdbserver_bin | run-as $package sh -c \
		   \"tee gdbserver > /dev/null\""

    # Upload the specified gdbserver binary to the device.
    adb -s $device push "$gdbserver" "$gdbserver_bin"

    if (adb -s $device shell ls /system/bin | grep -G tee); then
	# Copy it to the user directory.
	adb -s $device shell "$gdbserver_cat"
	adb -s $device shell "run-as $package chmod 777 gdbserver"
	gdbserver_cmd="./gdbserver"
    else
	# Hopefully this is an old version of Android which allows
	# execution from /data/local/tmp.  Its `chmod' doesn't support
	# `+x' either.
	adb -s $device shell "chmod 777 $gdbserver_bin"
	gdbserver_cmd="$gdbserver_bin"

	# If the user is root, then there is no need to open any kind
	# of TCP socket.
	if (adb -s $device shell id | grep -G root); then
	    gdbserver=
	    is_root=yes
	fi
    fi
fi

# Now start gdbserver on the device asynchronously.

echo "Attaching gdbserver to $pid on $device..."
exec 5<> /tmp/file-descriptor-stamp
rm -f /tmp/file-descriptor-stamp

if [ -z "$gdbserver" ]; then
    if [ "$is_root" = "yes" ]; then
	adb -s $device shell $gdbserver_bin --multi \
	    "0.0.0.0:7564" --attach $pid >&5 &
	gdb_socket="tcp:7564"
    else
	adb -s $device shell $gdbserver_bin --multi \
	    "0.0.0.0:7564" --attach $pid >&5 &
	gdb_socket="tcp:7564"
    fi
else
    # Normally the program cannot access $gdbserver_bin when it is
    # placed in /data/local/tmp.
    adb -s $device shell run-as $package $gdbserver_cmd --multi \
	"+debug.$package.socket" --attach $pid >&5 &
    gdb_socket="localfilesystem:$app_data_dir/debug.$package.socket"
fi

# In order to allow adb to forward to the gdbserver socket, make the
# app data directory a+x.
adb -s $device shell run-as $package chmod a+x $app_data_dir

# Wait until gdbserver successfully runs.
line=
while read -u 5 line; do
    case "$line" in
	*Attached* )
	    break;
	    ;;
	*error* | *Error* | failed )
	    echo "GDB error:" $line
	    exit 1
	    ;;
	* )
	    ;;
    esac
done

# Now that GDB is attached, tell the Java debugger to resume execution
# and then exit.

if [ -n "$jdb_command" ]; then
    echo "resume" >&${JDB[1]}
    echo "exit" >&${JDB[1]}
fi

# Forward the gdb server port here.
adb -s $device forward "tcp:$gdb_port" $gdb_socket
if [ ! $? ]; then
    echo "Failed to forward $app_data_dir/debug.$package.socket"
    echo "to $gdb_port!  Perhaps you need to specify a different port"
    echo "with --port?"
    exit 1;
fi

# Finally, start gdb with any extra arguments needed.
cd "$oldpwd"
$gdb --eval-command "target remote localhost:$gdb_port" $gdbargs
