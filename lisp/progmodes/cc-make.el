;;; cc-make.el --- Simplifies compilation.

;; Copyright (C) 1985,87,92,93,94,95,96,97,98 Free Software Foundation, Inc.

;; Authors:    1998 Barry A. Warsaw and Martin Stjernholm
;;             1997 Barry A. Warsaw
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Version:    See cc-mode.el
;; Keywords:   c languages oop

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defconst cc-path-to-the-custom-library
  ;; In Emacs 19.34, change the following line to the directory that
  ;; contains Per Abrahamsen's new Custom library, which you must
  ;; download first.  You can get Custom from:
  ;;
  ;; http://www.dina.kvl.dk/~abraham/custom/
  ;;
  ;; See the CC Mode homepage for installation details:
  ;;
  ;; http://www.python.org/emacs/cc-mode/
  nil
  )

(if cc-path-to-the-custom-library
    (setq load-path (cons cc-path-to-the-custom-library load-path)))

(if (not (and (condition-case nil
		  (require 'custom)
		(error nil))
	      ;; Stock Emacs 19.34 doesn't have this
	      (fboundp 'defcustom)))
    (error "STOP! STOP! STOP! STOP!

The Custom library was not found or is out of date.  A more current
version is required to use CC Mode 5.  You MUST fix cc-make.el.  See
that file or the CC Mode Web site for details:

    <http://www.python.org/emacs/cc-mode/"))

(setq load-path (cons default-directory load-path))

(batch-byte-compile)

;;; cc-make.el ends here
