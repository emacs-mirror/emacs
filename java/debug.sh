#!/bin/bash
### Run Emacs under GDB or JDB on Android.

## Copyright (C) 2023 Free Software Foundation, Inc.

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

if [ -z $devices ]; then
    echo "No devices are available."
    exit 1
fi

if [ -z $device ]; then
    device=$devices
fi

if [ `wc -w <<< "$devices"` -gt 1 ] && [ -z device ]; then
    echo "Multiple devices are available.  Please pick one using"
    echo "--device and try again."
fi

echo "Looking for $package on device $device"

# Find the application data directory
app_data_dir=`adb -s $device shell run-as $package sh -c 'pwd 2> /dev/null'`

if [ -z $app_data_dir ]; then
   echo "The data directory for the package $package was not found."
   echo "Is it installed?"
fi

echo "Found application data directory at $app_data_dir..."

# Find which PIDs are associated with org.gnu.emacs
package_uid=`adb -s $device shell run-as $package id -u`

if [ -z $package_uid ]; then
    echo "Failed to obtain UID of packages named $package"
    exit 1
fi

# First, run ps -u $package_uid -o PID,CMD to fetch the list of
# process IDs.
package_pids=`adb -s $device shell run-as $package ps -u $package_uid -o PID,CMD`

# Next, remove lines matching "ps" itself.
package_pids=`awk -- '{
  if (!match ($0, /(PID|ps)/))
    print $1
}' <<< $package_pids`

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
    adb -s $device shell am start -D "$package/$activity"
    if [ ! $? ]; then
	exit 1;
    fi

    # Now look for processes matching the package again.
    package_pids=`adb -s $device shell run-as $package ps -u $package_uid -o PID,CMD`

    # Next, remove lines matching "ps" itself.
    package_pids=`awk -- '{
  if (!match ($0, /(PID|ps)/))
    print $1
}' <<< $package_pids`
fi

pid=$package_pids
num_pids=`wc -w <<< "$package_pids"`

if [ $num_pids -gt 1 ]; then
    echo "More than one process was started:"
    echo ""
    adb -s $device shell run-as $package ps -u $package_uid | awk -- '{
      if (!match ($0, /ps/))
        print $0
    }'
    echo ""
    printf "Which one do you want to attach to? "
    read pid
elif [ -z $package_pids ]; then
    echo "No processes were found to attach to."
    exit 1
fi

# This isn't necessary when attaching gdb to an existing process.
if [ "$jdb" = "yes" ] || [ "$attach_existing" != yes ]; then
    # Start JDB to make the wait dialog disappear.
    echo "Attaching JDB to unblock the application."
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

    exec 4<> /tmp/file-descriptor-stamp

    # Now run JDB with IO redirected to file descriptor 4 in a subprocess.
    $jdb_command <&4 >&4 &

    character=
    # Next, wait until the prompt is found.
    while read -n1 -u 4 character; do
	if [ "$character" = ">" ]; then
	    echo "JDB attached successfully"
	    break;
	fi
    done
fi

# See if gdbserver has to be uploaded
if [ -z "$gdbserver" ]; then
    gdbserver_bin=/system/bin/gdbserver
else
    gdbserver_bin=/data/local/tmp/gdbserver

    # Upload the specified gdbserver binary to the device.
    adb -s $device push "$gdbserver" "$gdbserver_bin"
    adb -s $device shell chmod +x "$gdbserver_bin"
fi

# Now start gdbserver on the device asynchronously.

echo "Attaching gdbserver to $pid on $device..."
exec 5<> /tmp/file-descriptor-stamp

if [ -z "$gdbserver" ]; then
    adb -s $device shell run-as $package $gdbserver_bin --once \
	"+debug.$package_uid.socket" --attach $pid >&5 &
    gdb_socket="localfilesystem:$app_data_dir/debug.$package_uid.socket"
else
    # Normally the program cannot access $gdbserver_bin when it is
    # placed in /data/local/tmp.
    adb -s $device shell $gdbserver_bin --once \
	"+/data/local/tmp/debug.$package_uid.socket" \
	--attach $pid >&5 &
    gdb_socket="localfilesystem:/data/local/tmp/debug.$package_uid.socket"
fi

# Wait until gdbserver successfully runs.
line=
while read -u 5 line; do
    case "$line" in
	*Attached* )
	    break;
	    ;;
	*error* | *Error* | failed )
	    echo $line
	    exit 1
	    ;;
	* )
	    ;;
    esac
done

if [ "$attach_existing" != "yes" ]; then
    # Send EOF to JDB to make it go away.  This will also cause
    # Android to allow Emacs to continue executing.
    echo "Making JDB go away..."
    echo "exit" >&4
    read -u 4 line
    echo "JDB has gone away with $line"
fi

# Forward the gdb server port here.
adb -s $device forward "tcp:$gdb_port" $gdb_socket
if [ ! $? ]; then
    echo "Failed to forward $app_data_dir/debug.$package_uid.socket"
    echo "to $gdb_port!  Perhaps you need to specify a different port"
    echo "with --port?"
    exit 1;
fi

# Finally, start gdb with any extra arguments needed.
cd "$oldpwd"
gdb --eval-command "" --eval-command "target remote localhost:$gdb_port" $gdbargs
