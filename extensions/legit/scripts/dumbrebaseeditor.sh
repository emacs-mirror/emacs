#!/bin/bash
# Helper for a Git interactive rebase.
#
# git --interactive creates the rebase file we need in .git/rebase-merge/git-rebase-todo
# but the git-rebase process opens $EDITOR and waits for it to exit gracefully.
#
# This dumb editor is not an editor, so we can edit the rebase file with Lem,
# but this script catches a SIGTERM signal and exits successfulyl, so git-rebase
# is happy and terminates the rebase and all is well (on Unix).

function ok {
    exit 0
}

trap ok SIGTERM
echo "dumbrebaseeditor_pid:$$"

while :
do
        sleep 0.1
done
