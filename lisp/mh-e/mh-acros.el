;;; mh-acros.el --- Macros used in MH-E

;; Copyright (C) 2004 Free Software Foundation, Inc.

;; Author: Satyaki Das <satyaki@theforce.stanford.edu>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains macros that would normally be in mh-utils.el except that
;; their presence there would cause a dependency loop with mh-customize.el.
;; This file must always be included like this:
;;
;;   (eval-when-compile (require 'mh-acros))
;;
;; It is so named with a silent `m' so that it is compiled first. Otherwise,
;; "make recompile" in Emacs 21.4 fails.

;;; Change Log:

;;; Code:

(require 'cl)
(require 'advice)

;; The Emacs coding conventions require that the cl package not be required at
;; runtime. However, the cl package in versions of Emacs prior to 21.4 left cl
;; routines in their macro expansions. Use mh-require-cl to provide the cl
;; routines in the best way possible.
(defmacro mh-require-cl ()
  "Macro to load `cl' if needed.
Some versions of `cl' produce code for the expansion of
\(setf (gethash ...) ...) that uses functions in `cl' at run time.  This macro
recognizes that and loads `cl' where appropriate."
  (if (eq (car (macroexpand '(setf (gethash foo bar) baz))) 'cl-puthash)
      `(require 'cl)
    `(eval-when-compile (require 'cl))))

;;; Macros to generate correct code for different emacs variants

(defmacro mh-do-in-gnu-emacs (&rest body)
  "Execute BODY if in GNU Emacs."
  (unless (featurep 'xemacs) `(progn ,@body)))
(put 'mh-do-in-gnu-emacs 'lisp-indent-hook 'defun)

(defmacro mh-do-in-xemacs (&rest body)
  "Execute BODY if in GNU Emacs."
  (when (featurep 'xemacs) `(progn ,@body)))
(put 'mh-do-in-xemacs 'lisp-indent-hook 'defun)

(defmacro mh-funcall-if-exists (function &rest args)
  "Call FUNCTION with ARGS as parameters if it exists."
  (when (fboundp function)
    `(when (fboundp ',function)
       (funcall ',function ,@args))))

(defmacro mh-make-local-hook (hook)
  "Make HOOK local if needed.
XEmacs and versions of GNU Emacs before 21.1 require `make-local-hook' to be
called."
  (when (and (fboundp 'make-local-hook)
             (not (get 'make-local-hook 'byte-obsolete-info)))
    `(make-local-hook ,hook)))

(defmacro mh-mark-active-p (check-transient-mark-mode-flag)
  "A macro that expands into appropriate code in XEmacs and nil in GNU Emacs.
In GNU Emacs if CHECK-TRANSIENT-MARK-MODE-FLAG is non-nil then check if
variable `transient-mark-mode' is active."
  (cond ((featurep 'xemacs)             ;XEmacs
         `(and (boundp 'zmacs-regions) zmacs-regions (region-active-p)))
        ((not check-transient-mark-mode-flag) ;GNU Emacs
         `(and (boundp 'mark-active) mark-active))
        (t                              ;GNU Emacs
         `(and (boundp 'transient-mark-mode) transient-mark-mode
               (boundp 'mark-active) mark-active))))

(defmacro mh-defstruct (name-spec &rest fields)
  "Replacement for `defstruct' from the `cl' package.
The `defstruct' in the `cl' library produces compiler warnings, and generates
code that uses functions present in `cl' at run-time.  This is a partial
replacement, that avoids these issues.

NAME-SPEC declares the name of the structure, while FIELDS describes the
various structure fields. Lookup `defstruct' for more details."
  (let* ((struct-name (if (atom name-spec) name-spec (car name-spec)))
         (conc-name (or (and (consp name-spec)
                             (cadr (assoc :conc-name (cdr name-spec))))
                        (format "%s-" struct-name)))
         (predicate (intern (format "%s-p" struct-name)))
         (constructor (or (and (consp name-spec)
                               (cadr (assoc :constructor (cdr name-spec))))
                          (intern (format "make-%s" struct-name))))
         (field-names (mapcar #'(lambda (x) (if (atom x) x (car x))) fields))
         (field-init-forms (mapcar #'(lambda (x) (and (consp x) (cadr x)))
                                   fields))
         (struct (gensym "S"))
         (x (gensym "X"))
         (y (gensym "Y")))
    `(progn
       (defun* ,constructor (&key ,@(mapcar* #'(lambda (x y) (list x y))
                                             field-names field-init-forms))
         (list (quote ,struct-name) ,@field-names))
       (defun ,predicate (arg)
         (and (consp arg) (eq (car arg) (quote ,struct-name))))
       ,@(loop for x from 1
               for y in field-names
               collect `(defmacro ,(intern (format "%s%s" conc-name y)) (z)
                          (list 'nth ,x z)))
       (quote ,struct-name))))

(defadvice require (around mh-prefer-el activate)
  "Modify `require' to load uncompiled MH-E files."
  (or (featurep (ad-get-arg 0))
      (and (string-match "^mh-" (symbol-name (ad-get-arg 0)))
           (load (format "%s.el" (ad-get-arg 0)) t t))
      ad-do-it))

(provide 'mh-acros)

;;; Local Variables:
;;; no-byte-compile: t
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;; arch-tag: b383b49a-494f-4ed0-a30a-cb6d5d2da4ff
;;; mh-acros.el ends here
