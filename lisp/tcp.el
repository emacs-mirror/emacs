;;; TCP/IP stream emulation for GNU Emacs
;; Copyright (C) 1988, 1989, 1993 Free Software Foundation, Inc.

;;; Author: Masanobu Umeda
;;; Maintainer: umerin@mse.kyutech.ac.jp

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Notes on TCP package:
;;
;; This package provides a TCP/IP stream emulation for GNU Emacs. If
;; the function `open-network-stream' is not defined in Emacs, but
;; your operating system has a capability of network stream
;; connection, this tcp package can be used for communicating with
;; NNTP server.
;;
;; The tcp package runs inferior process which actually does the role
;; of `open-network-stream'.  The program `tcp' provided with this
;; package can be used for such purpose.  Before loading the package,
;; compile `tcp.c' and install it as `tcp' in a directory in the emacs
;; search path. If you modify `tcp.c', please send diffs to the author
;; of GNUS.  I'll include some of them in the next releases.

(provide 'tcp)

(defvar tcp-program-name "tcp"
  "*The name of the program emulating open-network-stream function.")

(defun open-network-stream (name buffer host service)
  "Open a TCP connection for a service to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.
Args are NAME BUFFER HOST SERVICE.
NAME is name for process.  It is modified if necessary to make it unique.
BUFFER is the buffer (or buffer-name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 BUFFER may be also nil, meaning that this process is not associated
 with any buffer
Third arg is name of the host to connect to.
Fourth arg SERVICE is name of the service desired, or an integer
 specifying a port number to connect to."
  (let ((proc (start-process name buffer 
			     tcp-program-name
			     host 
			     (if (stringp service)
				 service
			       (int-to-string service))
			     )))
    (process-kill-without-query proc)
    ;; Return process
    proc
    ))
