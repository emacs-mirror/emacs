;;; cmacexp.el --- C macro expansion

;; Copyright (C) 1988 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: c

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

(defvar c-macro-preprocessor "/lib/cpp"
  "*Command to be used for C preprocessing.")

(defvar c-macro-options nil
  "*List of options to use in C preprocessing.
Each string in the list becomes a separate argument to the preprocessor.
These arguments precede the filename.
Use the `-I' option here to specify directories for header files.")

(defun c-macro-expand (beg end)
  "Display the result of expanding all C macros occurring in the region.
The expansion is entirely correct because it uses the C preprocessor.
You can use the variables `c-macro-preprocessor' and `c-macro-options'
to customize how preprocessing is done, or specify header file directories
and macros to predefine."
  (interactive "r")
  (let ((outbuf (get-buffer-create "*Macroexpansion*"))
	(tempfile "%%macroexpand%%")
	expanded
	process
	last-needed)
    (setq expanded (expand-file-name tempfile))
    (save-excursion
      (set-buffer outbuf)
      (erase-buffer))
    (setq process (apply 'start-process "macros" outbuf c-macro-preprocessor
			 c-macro-options))
    (set-process-sentinel process '(lambda (&rest x)))
    (save-restriction
      (widen)
      (save-excursion
	(goto-char beg)
	(beginning-of-line)
	(setq last-needed (point))
	(if (re-search-backward "^[ \t]*#" nil t)
	    (progn
	      ;; Skip continued lines.
	      (while (progn (end-of-line) (= (preceding-char) ?\\))
		(forward-line 1))
	      ;; Skip the last line of the macro definition we found.
	      (forward-line 1)
	      (setq last-needed (point)))))
      (write-region (point-min) last-needed expanded nil 'nomsg)
      ;; Output comment ender in case last #-directive is inside a comment.
      ;; Also, terminate any string that we are in.
      (write-region "*//*\"*/\n" nil expanded t 'nomsg)
      (write-region beg end (concat expanded "x") nil 'nomsg)
      (process-send-string process (concat "#include \"" tempfile "\"\n"))
      (process-send-string process "\n")
      (process-send-string process (concat "#include \"" tempfile "x\"\n"))
      (process-send-eof process)
      ;; HPUX seems to want two eofs.
      (process-send-eof process))
    (while (eq (process-status process) 'run)
      (accept-process-output))
    (delete-file expanded)
    (delete-file (concat expanded "x"))
    (display-buffer outbuf)
    (save-excursion
      (set-buffer outbuf)
      (goto-char (point-max))
      (forward-line -1)
      (delete-region (point) (point-max))
      (re-search-backward "\n# 1 ")
      (forward-line 2)
      (while (eolp) (delete-char 1))
      (delete-region (point-min) (point)))
    (display-buffer outbuf)))

;;; cmacexp.el ends here
