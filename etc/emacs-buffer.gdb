# emacs-buffer.gdb --- gdb macros for recovering buffers from emacs coredumps

# Copyright (C) 2005-2018 Free Software Foundation, Inc.

# Author: Noah Friedman <friedman@splode.com>
# Created: 2005-04-28

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

# Commentary:

# This is a set of gdb macros for recovering the contents of buffers from
# an Emacs coredump; they may not always be file-backed or have a recent
# autosave.
#
# The Emacs executable must have debugging symbols for this to work.
# But you never strip Emacs, right?
#
# The main commands of interest are 'ybuffer-list', 'yfile-buffers',
# 'ysave-buffer', and 'ybuffer-contents'.  The 'y' prefix avoids any
# namespace collisions with emacs/src/.gdbinit.

# Since the internal data structures in Emacs change from time to
# time, you should use the version of this file that came with your
# particular Emacs version; older versions might not work anymore.

# Example usage:
#
#     $ gdb /export/src/emacs/2005-05-02--03-17/src/emacs core.emacs.6.9845
#     Current directory is /u/noah/
#     GNU gdb (6.1post-1.20040607.43rh)
#     ...
#     #0  0x400007a2 in _dl_sysinfo_int80 () from /lib/ld-linux.so.2
#     (gdb) source emacs-buffer.gdb
#     (gdb) ybuffer-list
#     B# M       Size Name                 Mode       File
#     -- -       ---- ----                 ----       ----
#      0 *        556 mail to emacs-devel@gnu.org Mail
#      1 *          0  *Minibuf-1*         Fundamental
#      2       145769 ChangeLog            Change Log /u/noah/lib/elisp/noahf/ChangeLog
#      3         6619 ascii-table.el       Elisp      /u/noah/lib/elisp/noahf/ascii-table.el
#      4 *      48396 *Messages*           Fundamental
#      5         3191 *Apropos*            Apropos
#      6        17642 init-21.el           Elisp      /u/noah/etc/init/emacs/init-21.el
#      7          333 cpuid.c              C          /u/noah/cpuid.c
#      8          230 src                  Dired
#      9          218 noah                 Dired
#     10 *         21  *Echo Area 0*       Fundamental
#     11 *          0  *Echo Area 1*       Fundamental
#     12       319952  *bbdb data*         Text       /u/noah/.bbdb
#     (gdb) ysave-buffer 0 mail.save
#     [Wrote buffer "mail to emacs-devel@gnu.org" to file mail.save]
#     (gdb) quit
#     $ ls -l mail.save
#     -rw-rw-rw-  1 noah user 556 May  2 04:05 mail.save
#     $

# Code:

# Force loading of symbols, enough to give us VALBITS etc.
set $dummy = main + 8
# With some compilers, we need this to give us struct Lisp_Symbol etc.:
set $dummy = Fmake_symbol + 8

# When nonzero, display some extra diagnostics in various commands
set $yverbose = 1
set $yfile_buffers_only = 0

define ygetptr
  set $ptr = $arg0
  set $ptr = (EMACS_INT) (CHECK_LISP_OBJECT_TYPE ? $ptr.i : $ptr) & VALMASK
end

# Get the value of Qnil for comparison.  Needed when
# CHECK_LISP_OBJECT_TYPE is non-zero.
ygetptr Qnil
set $qnil = $ptr

define ybuffer-list
  set $files_only         = $yfile_buffers_only
  set $yfile_buffers_only = 0

  if $yverbose
    printf "B# M       Size Name                 Mode       File\n"
    printf "-- -       ---- ----                 ----       ----\n"
  end

  set $i = 0
  set $alist = Vbuffer_alist
  ygetptr $alist
  set $alist = $ptr
  while $alist != $qnil
    set $this  = ((struct Lisp_Cons *) $ptr)->u.s.car
    set $alist = ((struct Lisp_Cons *) $ptr)->u.s.u.cdr

    # Vbuffer_alist elts are pairs of the form (name . buffer)
    ygetptr $this
    set $buf  = ((struct Lisp_Cons *) $ptr)->u.s.u.cdr
    ygetptr $buf
    set $buf = (struct buffer *) $ptr

    ygetptr $buf->filename_
    set $fname = $ptr
    if ! ($files_only && $fname == $qnil)
      ygetptr $buf->name_
      set $name = ((struct Lisp_String *) $ptr)->u.s.data
      set $modp = ($buf->text->modiff > $buf->text->save_modiff) ? '*' : ' '

      ygetptr $buf->mode_name_
      set $mode = ((struct Lisp_String *) $ptr)->u.s.data

      if $fname != $qnil
        ygetptr $buf->filename_
        printf "%2d %c  %9d %-20s %-10s %s\n", \
               $i, $modp, ($buf->text->z_byte - 1), $name, $mode, \
               ((struct Lisp_String *) $fname)->u.s.data
      else
        printf "%2d %c  %9d %-20s %-10s\n", \
               $i, $modp, ($buf->text->z_byte - 1), $name, $mode
      end
    end

    set $i++
    ygetptr $alist
    set $alist = $ptr
  end
end
document ybuffer-list
  Display a list of buffer names, sizes, and other attributes.
  The buffer number in the first column is used as an argument
  to some other emacs-buffer recovery commands, e.g. 'ysave-buffer'.
end

define yfile-buffers
  set $yfile_buffers_only = 1
  ybuffer-list
end
document yfile-buffers
  Display a list of buffers which are associated with files.
  This is like 'ybuffer-list', but only buffers that were visiting files
  are displayed.
end

define yset-buffer
  set $i = $arg0

  set $alist = Vbuffer_alist
  ygetptr $alist
  set $alist = $ptr
  while ($alist != $qnil && $i > 0)
    set $alist = ((struct Lisp_Cons *) $ptr)->u.s.u.cdr
    ygetptr $alist
    set $alist = $ptr
    set $i--
  end

  # Get car of alist; this is a pair (name . buffer)
  set $this = ((struct Lisp_Cons *) $alist)->u.s.car

  # Get the buffer object
  ygetptr $this
  set $this = ((struct Lisp_Cons *) $ptr)->u.s.u.cdr

  ygetptr $this
  set $ycurrent_buffer = (struct buffer *) $ptr
end
document yset-buffer
  Set current buffer (for other emacs-buffer recovery commands) to the ARG'th
  buffer as displayed by 'ybuffer-list'.
end

define yget-buffer-pointers
  yset-buffer $arg0
  set $buf = $ycurrent_buffer->text

  set $beg     = $buf->beg
  set $gap     = $beg + $buf->gpt_byte
  set $gap_end = $gap + $buf->gap_size - 1
  set $end     = $gap_end + ($buf->z_byte - $buf->gpt_byte)

  set $modp    = $buf->modiff > $buf->save_modiff

  #print *$beg@($gap - $beg)
  #print *$gap_end@($end - $gap_end)
end
document yget-buffer-pointers
  Update convenience variables with address pointers for the ARG'th buffer
  as displayed by 'ybuffer-list'.

  This also sets the current buffer using 'yset-buffer' (which see).
end

define yget-current-buffer-name
  set $this = $ycurrent_buffer->name_
  ygetptr $this
  set $ycurrent_buffer_name = ((struct Lisp_String *) $ptr)->u.s.data
end
document yget-current-buffer-name
  Set $ycurrent_buffer_name to the name of the currently selected buffer.
end

define ycurrent-buffer
  yget-current-buffer-name
  printf "%s\n", $ycurrent_buffer_name
end
document ycurrent-buffer
  Display the currently selected buffer.
end

define ydump-buffer
  yget-buffer-pointers $arg0
  if $buf->z_byte > 1
    if $buf->z_byte <= $buf->gpt_byte
      set $endptr = $beg + $buf->gpt_byte - 1
      dump binary memory $arg1 $beg $endptr
    else
      if $gap - $beg > 1
        dump   binary memory $arg1 $beg $gap-1
        append binary memory $arg1 $gap_end $end
      else
        dump   binary memory $arg1 $gap_end $end
      end
      set $endptr = $end
    end
  end
end
document ydump-buffer
  Write contents of buffer N (as numbered according to 'ybuffer-list') to
  file FILE.

  This is mainly used as an internal subroutine for 'ysave-buffer' and
  'ybuffer-contents', which see.
end

define ysave-buffer
  ydump-buffer $arg0 $arg1
  if $yverbose
    yget-current-buffer-name
    if $buf->z_byte <= 1
      printf "[Buffer \"%s\" is empty.]\n", $ycurrent_buffer_name
    else
      # Output string broken into separate calls as necessary to avoid
      # requiring a running process for evaluation.
      printf "[Wrote buffer \"%s\" to file ", $ycurrent_buffer_name
      echo $arg1]\n
    end
  end
end
document ysave-buffer
  Save contents of buffer N (as numbered according to 'ybuffer-list') to
  file FILE.
end

define ybuffer-contents
  ydump-buffer $arg0 /dev/stdout
  if $yverbose && $buf->z_byte <= 1
    yget-current-buffer-name
    printf "[Buffer \"%s\" is empty.]\n", $ycurrent_buffer_name
  else
    if *($endptr-1) != '\n'
      echo \n
    end
  end
end
document ybuffer-contents
  Write contents of buffer N (numbered according to 'ybuffer-list') to stdout.
end

# local variables:
# mode: gdb-script
# end:
