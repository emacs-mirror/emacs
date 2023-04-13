# Check the file list of GNU Emacs change log entries for each commit SHA.

# Copyright 2023 Free Software Foundation, Inc.

# This file is part of GNU Emacs.

# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

### Commentary:

# This script accepts a list of (unabbreviated) Git commit SHAs, and
# will then iterate over them to check that any files mentioned in the
# commit message are actually present in the commit's diff.  If not,
# it will print out the incorrect file names and return 1.

# You can also pass "-v reason=pre-push", which will add more-verbose
# output, indicating the abbreviated commit SHA and first line of the
# commit message for any improper commits.

### Code:

function get_commit_changes(commit_sha, changes,    cmd, i, j, len, \
                            bits, filename) {
  # Collect all the files touched in the specified commit.
  cmd = ("git log -1 --name-status --format= " commit_sha)
  while ((cmd | getline) > 0) {
    for (i = 2; i <= NF; i++) {
      len = split($i, bits, "/")
      for (j = 1; j <= len; j++) {
        if (j == 1)
          filename = bits[j]
        else
          filename = filename "/" bits[j]
        changes[filename] = 1
      }
    }
  }
  close(cmd)
}

function check_commit_msg_files(commit_sha, verbose,    changes, good, \
                                cmd, msg, filenames_str, filenames, i) {
  get_commit_changes(commit_sha, changes)
  good = 1

  cmd = ("git log -1 --format=%B " commit_sha)
  while ((cmd | getline) > 0) {
    if (verbose && ! msg)
      msg = $0

    # Find lines that reference files.  We look at any line starting
    # with "*" (possibly prefixed by "; ") where the file part starts
    # with an alphanumeric character.  The file part ends if we
    # encounter any of the following characters: [ ( < { :
    if (/^(; )?\*[ \t]+[[:alnum:]]/ && match($0, /[[:alnum:]][^[(<{:]*/)) {
      # There might be multiple files listed on this line, separated
      # by spaces (and possibly a comma).  Iterate over each of them.
      split(substr($0, RSTART, RLENGTH), filenames, ",?([[:blank:]]+|$)")

      for (i in filenames) {
        # Remove trailing slashes from any directory entries.
        sub(/\/$/, "", filenames[i])

        if (length(filenames[i]) && ! (filenames[i] in changes)) {
          if (good) {
            # Print a header describing the error.
            if (verbose)
              printf("In commit %s \"%s\"...\n", substr(commit_sha, 1, 10), msg)
            printf("Files listed in commit message, but not in diff:\n")
          }
          printf("  %s\n", filenames[i])
          good = 0
        }
      }
    }
  }
  close(cmd)

  return good
}

BEGIN {
  if (reason == "pre-push")
    verbose = 1
}

/^[a-z0-9]{40}$/ {
  if (! check_commit_msg_files($0, verbose)) {
    status = 1
  }
}

END {
  if (status != 0) {
    if (reason == "pre-push")
      error_msg = "Push aborted"
    else
      error_msg = "Bad commit message"
    printf("%s; please see the file 'CONTRIBUTE'\n", error_msg)
  }
  exit status
}
