;;; em-xtra.el --- extra alias functions  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2018 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'esh-util)
(eval-when-compile
  (require 'eshell))
;; Strictly speaking, should only be needed at compile time.
;; Require at run-time too to silence compiler.
(require 'pcomplete)
(require 'compile)

;; There are no items in this custom group, but eshell modules (ab)use
;; custom groups.
;;;###autoload
(progn
(defgroup eshell-xtra nil
  "This module defines some extra alias functions which are entirely
optional.  They can be viewed as samples for how to write Eshell alias
functions, or as aliases which make some of Emacs's behavior more
naturally accessible within Emacs."
  :tag "Extra alias functions"
  :group 'eshell-module))

;;; Functions:

(autoload 'eshell-parse-command "esh-cmd")

(defun eshell/expr (&rest args)
  "Implementation of expr, using the calc package."
  (if (not (fboundp 'calc-eval))
      (throw 'eshell-replace-command
	     (eshell-parse-command "*expr" (eshell-flatten-list args)))
    ;; to fool the byte-compiler...
    (let ((func 'calc-eval))
      (funcall func (eshell-flatten-and-stringify args)))))

(defun eshell/substitute (&rest args)
  "Easy front-end to `intersection', for comparing lists of strings."
  (apply 'substitute (car args) (cadr args) :test 'equal
	 (cddr args)))

(defun eshell/count (&rest args)
  "Easy front-end to `intersection', for comparing lists of strings."
  (apply 'count (car args) (cadr args) :test 'equal
	 (cddr args)))

(defun eshell/mismatch (&rest args)
  "Easy front-end to `intersection', for comparing lists of strings."
  (apply 'mismatch (car args) (cadr args) :test 'equal
	 (cddr args)))

(defun eshell/union (&rest args)
  "Easy front-end to `intersection', for comparing lists of strings."
  (apply 'union (car args) (cadr args) :test 'equal
	 (cddr args)))

(defun eshell/intersection (&rest args)
  "Easy front-end to `intersection', for comparing lists of strings."
  (apply 'intersection (car args) (cadr args) :test 'equal
	 (cddr args)))

(defun eshell/set-difference (&rest args)
  "Easy front-end to `intersection', for comparing lists of strings."
  (apply 'set-difference (car args) (cadr args) :test 'equal
	 (cddr args)))

(defun eshell/set-exclusive-or (&rest args)
  "Easy front-end to `intersection', for comparing lists of strings."
  (apply 'set-exclusive-or (car args) (cadr args) :test 'equal
	 (cddr args)))

(defalias 'eshell/ff 'find-name-dired)
(defalias 'eshell/gf 'find-grep-dired)

(defun pcomplete/bcc32 ()
  "Completion function for Borland's C++ compiler."
  (let ((cur (pcomplete-arg 0)))
    (cond
     ((string-match "\\`-w\\([^;]+;\\)*\\([^;]*\\)\\'" cur)
      (pcomplete-here
       '("ali" "amb" "amp" "asc" "asm" "aus" "bbf" "bei" "big" "ccc"
	 "cln" "cod" "com" "cpt" "csu" "def" "dig" "dpu" "dsz" "dup"
	 "eas" "eff" "ext" "hch" "hid" "ias" "ibc" "ifr" "ill" "nil"
	 "lin" "lvc" "mcs" "mes" "mpc" "mpd" "msg" "nak" "ncf" "nci"
	 "ncl" "nfd" "ngu" "nin" "nma" "nmu" "nod" "nop" "npp" "nsf"
	 "nst" "ntd" "nto" "nvf" "obi" "obs" "ofp" "osh" "ovf" "par"
	 "pch" "pck" "pia" "pin" "pow" "prc" "pre" "pro" "rch" "ret"
	 "rng" "rpt" "rvl" "sig" "spa" "stl" "stu" "stv" "sus" "tai"
	 "tes" "thr" "ucp" "use" "voi" "zdi") (match-string 2 cur)))
     ((string-match "\\`-[LIn]\\([^;]+;\\)*\\([^;]*\\)\\'" cur)
      (pcomplete-here (pcomplete-dirs) (match-string 2 cur)))
     ((string-match "\\`-[Ee]\\(.*\\)\\'" cur)
      (pcomplete-here (pcomplete-dirs-or-entries "\\.[Ee][Xx][Ee]\\'")
		      (match-string 1 cur)))
     ((string-match "\\`-o\\(.*\\)\\'" cur)
      (pcomplete-here (pcomplete-dirs-or-entries "\\.[Oo][Bb][Jj]\\'")
		      (match-string 1 cur)))
     (t
      (pcomplete-opt "3456ABCDEHIKLMNOPRSTUVXabcdefgijklnoptuvwxyz"))))
  (while (pcomplete-here
	  (pcomplete-dirs-or-entries "\\.[iCc]\\([Pp][Pp]\\)?\\'"))))

(defalias 'pcomplete/bcc 'pcomplete/bcc32)

(provide 'em-xtra)

;; Local Variables:
;; generated-autoload-file: "esh-groups.el"
;; End:

;;; em-xtra.el ends here
