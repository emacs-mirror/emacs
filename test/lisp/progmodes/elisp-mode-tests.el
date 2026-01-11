;;; elisp-mode-tests.el --- Tests for emacs-lisp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Free Software Foundation, Inc.

;; Author: Dmitry Gutov <dgutov@yandex.ru>
;; Author: Stephen Leake <stephen_leake@member.fsf.org>

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

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'xref)
(eval-when-compile (require 'cl-lib))
(require 'ert-x)

;;; Completion

(defun elisp--test-completions ()
  (let ((data (elisp-completion-at-point)))
    (all-completions (buffer-substring (nth 0 data) (nth 1 data))
                     (nth 2 data)
                     (plist-get (nthcdr 3 data) :predicate))))

(ert-deftest elisp-completes-functions ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(ba")
    (let ((comps (elisp--test-completions)))
      (should (member "backup-buffer" comps))
      (should-not (member "backup-inhibited" comps)))))

(ert-deftest elisp-completes-variables ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo ba")
    (let ((comps (elisp--test-completions)))
      (should (member "backup-inhibited" comps))
      (should-not (member "backup-buffer" comps)))))

(ert-deftest elisp-completes-anything-quoted ()
  (dolist (text '("`(foo ba" "(foo 'ba"
                  "`(,foo ba" "`,(foo `ba"
                  "'(foo (ba"))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert text)
      (let ((comps (elisp--test-completions)))
        (should (member "backup-inhibited" comps))
        (should (member "backup-buffer" comps))
        (should (member "backup" comps))))))

(ert-deftest elisp-completes-variables-unquoted ()
  (dolist (text '("`(foo ,ba" "`(,(foo ba" "`(,ba"))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert text)
      (let ((comps (elisp--test-completions)))
        (should (member "backup-inhibited" comps))
        (should-not (member "backup-buffer" comps))))))

(ert-deftest elisp-completes-functions-in-special-macros ()
  (dolist (text '("(declare-function ba" "(cl-callf2 ba"))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert text)
      (let ((comps (elisp--test-completions)))
        (should (member "backup-buffer" comps))
        (should-not (member "backup-inhibited" comps))))))

(ert-deftest elisp-completes-functions-after-hash-quote ()
  (ert-deftest elisp-completes-functions-after-let-bindings ()
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "#'ba")
      (let ((comps (elisp--test-completions)))
        (should (member "backup-buffer" comps))
        (should-not (member "backup-inhibited" comps))))))

(ert-deftest elisp-completes-local-variables ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(let ((bar 1) baz) (foo ba")
    (let ((comps (elisp--test-completions)))
      (should (member "backup-inhibited" comps))
      (should (member "bar" comps))
      (should (member "baz" comps)))))

(ert-deftest elisp-completest-variables-in-let-bindings ()
  (dolist (text '("(let (ba" "(let* ((ba"))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert text)
      (let ((comps (elisp--test-completions)))
        (should (member "backup-inhibited" comps))
        (should-not (member "backup-buffer" comps))))))

(ert-deftest elisp-completes-functions-after-empty-let-bindings ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(let () (ba")
    (let ((comps (elisp--test-completions)))
      (should (member "backup-buffer" comps))
      (should-not (member "backup-inhibited" comps)))))

(ert-deftest elisp-completes-functions-after-let-bindings-2 ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(let ((bar 1) (baz 2)) (ba")
    (let ((comps (elisp--test-completions)))
      (should (member "backup-buffer" comps))
      (should-not (member "backup-inhibited" comps)))))

;;; eval-last-sexp

(ert-deftest eval-last-sexp-print-format-sym ()
  (with-temp-buffer
    (let ((current-prefix-arg '(4)))
      (erase-buffer) (insert "t")
      (call-interactively #'eval-last-sexp)
      (should (equal (buffer-string) "tt")))))

(ert-deftest eval-last-sexp-print-format-sym-echo ()
  ;; We can only check the echo area when running interactive.
  (skip-when noninteractive)
  (with-temp-buffer
    (let ((current-prefix-arg nil))
      (erase-buffer) (insert "t") (message nil)
      (call-interactively #'eval-last-sexp)
      (should (equal (current-message) "t")))))

(ert-deftest eval-last-sexp-print-format-small-int ()
  (with-temp-buffer
    (let ((current-prefix-arg '(4)))
      (erase-buffer) (insert "?A")
      (call-interactively #'eval-last-sexp)
      (should (equal (buffer-string) "?A65")))
    (let ((current-prefix-arg 0))
      (erase-buffer) (insert "?A")
      (call-interactively #'eval-last-sexp)
      (should (equal (buffer-string) "?A65 (#o101, #x41, ?A)")))))

(ert-deftest eval-last-sexp-print-format-small-int-echo ()
  (skip-when noninteractive)
  (with-temp-buffer
    (let ((current-prefix-arg nil))
      (erase-buffer) (insert "?A") (message nil)
      (call-interactively #'eval-last-sexp)
      (should (equal (current-message) "65 (#o101, #x41, ?A)")))))

(ert-deftest eval-last-sexp-print-format-large-int ()
  (with-temp-buffer
    (let ((eval-expression-print-maximum-character ?A))
      (let ((current-prefix-arg '(4)))
        (erase-buffer) (insert "?B")
        (call-interactively #'eval-last-sexp)
        (should (equal (buffer-string) "?B66")))
      (let ((current-prefix-arg 0))
        (erase-buffer) (insert "?B")
        (call-interactively #'eval-last-sexp)
        (should (equal (buffer-string) "?B66 (#o102, #x42)")))
      (let ((current-prefix-arg -1))
        (erase-buffer) (insert "?B")
        (call-interactively #'eval-last-sexp)
        (should (equal (buffer-string) "?B66 (#o102, #x42, ?B)"))))))

(ert-deftest eval-last-sexp-print-format-large-int-echo ()
  (skip-when noninteractive)
  (with-temp-buffer
    (let ((eval-expression-print-maximum-character ?A))
      (let ((current-prefix-arg nil))
        (erase-buffer) (insert "?B") (message nil)
        (call-interactively #'eval-last-sexp)
        (should (equal (current-message) "66 (#o102, #x42)")))
      (let ((current-prefix-arg '-))
        (erase-buffer) (insert "?B") (message nil)
        (call-interactively #'eval-last-sexp)
        (should (equal (current-message) "66 (#o102, #x42, ?B)"))))))

;;; eval-defun

(ert-deftest eval-defun-prints-edebug-when-instrumented ()
  (skip-when noninteractive)
  (with-temp-buffer
    (let ((current-prefix-arg '(4)))
      (erase-buffer) (insert "(defun foo ())") (message nil)
      (call-interactively #'eval-defun)
      (should (equal (current-message) "Edebug: foo")))))

;;; eldoc

(defun elisp-mode-tests--face-propertized-string (string)
  "Return substring of STRING with a non-nil `face' property."
  (let* ((start (next-single-property-change 0 'face string))
         (end (and start (next-single-property-change start 'face string))))
    (and end
         (substring string start end))))

(ert-deftest elisp--highlight-function-argument-indexed ()
  (dotimes (i 3)
    (should
     (equal (elisp-mode-tests--face-propertized-string
             (elisp--highlight-function-argument 'foo "(A B C)" (1+ i)))
            (propertize (nth i '("A" "B" "C"))
                        'face 'eldoc-highlight-function-argument)))))

(ert-deftest elisp--highlight-function-argument-keyed-1 ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo prompt bar :b 2)")
    (goto-char (1+ (point-min)))
    (cl-flet ((bold-arg (i)
               (elisp-mode-tests--face-propertized-string
                (elisp--highlight-function-argument
                 'foo "(PROMPT LST &key A B C)" i))))
      (should-not (bold-arg 0))
      (progn (forward-sexp) (forward-char))
      (should (equal (bold-arg 1) "PROMPT"))
      (progn (forward-sexp) (forward-char))
      (should (equal (bold-arg 2) "LST"))
      ;; Both `:b' and `2' should highlight the `B' arg.
      (progn (forward-sexp) (forward-char))
      (should (equal (bold-arg 3) "B"))
      (progn (forward-sexp) (forward-char))
      (should (equal (bold-arg 4) "B")))))

(ert-deftest elisp--highlight-function-argument-keyed-2 ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo :b :a 1)")
    (goto-char (1+ (point-min)))
    (cl-flet ((bold-arg (i)
               (elisp-mode-tests--face-propertized-string
                (elisp--highlight-function-argument
                 'foo "(X &key A B C)" i))))
      (should-not (bold-arg 0))
      ;; The `:b' specifies positional arg `X'.
      (progn (forward-sexp) (forward-char))
      (should (equal (bold-arg 1) "X"))
      (progn (forward-sexp) (forward-char))
      (should (equal (bold-arg 2) "A"))
      (progn (forward-sexp) (forward-char))
      (should (equal (bold-arg 3) "A")))))

;;; xref

(defun xref-elisp-test-descr-to-target (xref)
  "Return an appropriate `looking-at' match string for XREF."
  (let* ((loc (xref-item-location xref))
	 (type (or (xref-elisp-location-type loc)
		  'defun)))

    (cl-case type
      (defalias
       ;; summary: "(defalias xref)"
       ;; target : "(defalias 'xref"
       (concat "(defalias '" (substring (xref-item-summary xref) 10 -1)))

      (defun
       (let ((summary (xref-item-summary xref))
	     (file (xref-elisp-location-file loc)))
	 (cond
	  ((string= "c" (file-name-extension file))
	   ;; summary: "(defun buffer-live-p)"
	   ;; target : "DEFUN (buffer-live-p"
	   (concat
	    (upcase (substring summary 1 6))
	    " (\""
	    (substring summary 7 -1)
	    "\""))

	  (t
	   (substring summary 0 -1))
	  )))

      (defvar
       (let ((summary (xref-item-summary xref))
	     (file (xref-elisp-location-file loc)))
	 (cond
	  ((string= "c" (file-name-extension file))
	   ;; summary: "(defvar system-name)"
	   ;; target : "DEFVAR_LISP ("system-name", "
           ;; summary: "(defvar abbrev-mode)"
           ;; target : DEFVAR_PER_BUFFER ("abbrev-mode"
	   (concat
	    (upcase (substring summary 1 7))
            (if (bufferp (variable-binding-locus (xref-elisp-location-symbol loc)))
                "_PER_BUFFER (\""
              "_LISP (\"")
	    (substring summary 8 -1)
	    "\""))

	  (t
	   (substring summary 0 -1))
	  )))

      (feature
       ;; summary: "(feature xref)"
       ;; target : "(provide 'xref)"
       (concat "(provide '" (substring (xref-item-summary xref) 9 -1)))

      (otherwise
       (substring (xref-item-summary xref) 0 -1))
      )))


;; tmp may be on a different filesystem to the tests, but, ehh.
(defvar xref--case-insensitive
  (ert-with-temp-directory dir
    (with-temp-file (expand-file-name "hElLo" dir) "hello")
    (file-exists-p (expand-file-name "HELLO" dir)))
  "Non-nil if file system seems to be case-insensitive.")

(defun xref-elisp-test-run (xrefs expected-xrefs)
  (should (= (length xrefs) (length expected-xrefs)))
  (while xrefs
    (let* ((xref (pop xrefs))
           (expected (pop expected-xrefs))
           (expected-xref (or (when (consp expected) (car expected)) expected))
           (expected-source (when (consp expected) (cdr expected)))
           (xref-file (xref-elisp-location-file (xref-item-location xref)))
           (expected-file (xref-elisp-location-file
                           (xref-item-location expected-xref))))

      ;; Make sure file names compare as strings.
      (when (file-name-absolute-p xref-file)
        (setf (xref-elisp-location-file (xref-item-location xref))
              (file-truename (xref-elisp-location-file (xref-item-location xref)))))
      (when (file-name-absolute-p expected-file)
        (setf (xref-elisp-location-file (xref-item-location expected-xref))
              (file-truename (xref-elisp-location-file
                              (xref-item-location expected-xref)))))

      ;; Downcase the filenames for case-insensitive file systems.
      (when xref--case-insensitive
        (setf (xref-elisp-location-file (xref-item-location xref))
              (downcase (xref-elisp-location-file (xref-item-location xref))))

        (setf (xref-elisp-location-file (xref-item-location expected-xref))
              (downcase (xref-elisp-location-file
                         (xref-item-location expected-xref)))))

      (should (equal xref expected-xref))

      (xref--goto-location (xref-item-location xref))
      (back-to-indentation)
      (should (looking-at (or expected-source
                              (xref-elisp-test-descr-to-target expected)))))
    ))

(defmacro xref-elisp-deftest (name computed-xrefs expected-xrefs)
  "Define an ert test for an xref-elisp feature.
COMPUTED-XREFS and EXPECTED-XREFS are lists of xrefs, except if
an element of EXPECTED-XREFS is a cons (XREF . TARGET), TARGET is
matched to the found location; otherwise, match
to (xref-elisp-test-descr-to-target xref)."
  (declare (indent defun)
           (debug (symbolp "name")))
  `(ert-deftest ,(intern (concat "xref-elisp-test-" (symbol-name name))) ()
     (let ((find-file-suppress-same-file-warnings t))
       (xref-elisp-test-run ,computed-xrefs ,expected-xrefs)
       )))

;; When tests are run from the Makefile, 'default-directory' is $HOME,
;; so we must provide this dir to expand-file-name in the expected
;; results. This also allows running these tests from other
;; directories.
;;
;; We add 'downcase' here to deliberately cause a potential problem on
;; case-insensitive file systems. On such systems, `load-file-name'
;; may not have the same case as the real file system, since the user
;; can set `load-path' to have the wrong case (on my Windows system,
;; `load-path' has the correct case, so this causes the expected test
;; values to have the wrong case). This is handled in
;; `xref-elisp-test-run'.
(defvar emacs-test-dir
  (funcall (if xref--case-insensitive 'downcase 'identity)
           (file-truename (file-name-directory
                           (or load-file-name (buffer-file-name))))))


;; alphabetical by test name

;; Autoloads require no special support; they are handled as functions.

;; FIXME: defalias-defun-c cmpl-prefix-entry-head
;; FIXME: defalias-defvar-el allout-mode-map

(xref-elisp-deftest find-defs-constructor
  (elisp--xref-find-definitions 'xref-make-elisp-location)
  ;; 'xref-make-elisp-location' is just a name for the default
  ;; constructor created by the cl-defstruct, so the location is the
  ;; cl-defstruct location.
  (list
   (cons
    (xref-make "(cl-defstruct xref-elisp-location (:constructor xref-make-elisp-location))"
               (xref-make-elisp-location
                'xref-elisp-location 'define-type
                (expand-file-name "../../../lisp/progmodes/elisp-mode.el" emacs-test-dir)))
    ;; It's not worth adding another special case to `xref-elisp-test-descr-to-target' for this
    "(cl-defstruct (xref-elisp-location")
   ))

(require 'em-xtra)
(require 'find-dired)
(xref-elisp-deftest find-defs-defalias-defun-el
  (elisp--xref-find-definitions 'eshell/ff)
  (list
   (xref-make "(defalias eshell/ff)"
	      (xref-make-elisp-location
	       'eshell/ff 'defalias
	       (expand-file-name "../../../lisp/eshell/em-xtra.elc"
                                 emacs-test-dir)))
   (xref-make "(defun find-name-dired)"
	      (xref-make-elisp-location
	       'find-name-dired nil
	       (expand-file-name "../../../lisp/find-dired.el"
                                 emacs-test-dir)))))

;; FIXME: defconst

;; Possible ways of defining the default method implementation for a
;; generic function. We declare these here, so we know we cover all
;; cases, and we don't rely on other code not changing.
;;
;; When the generic and default method are declared in the same place,
;; elisp--xref-find-definitions only returns one.

(cl-defstruct (xref-elisp-root-type)
  slot-1)

(cl-defgeneric xref-elisp-generic-no-methods (arg1 arg2)
  "Doc string generic no-methods."
  ;; No default implementation, no methods, but fboundp is true for
  ;; this symbol; it calls cl-no-applicable-method
  )

;; WORKAROUND: ‘this’ is unused, and the byte compiler complains, so
;; it should be spelled ‘_this’. But for some unknown reason, that
;; causes the batch mode test to fail; the symbol shows up as
;; ‘this’. It passes in interactive tests, so I haven't been able to
;; track down the problem.
(cl-defmethod xref-elisp-generic-no-default ((this xref-elisp-root-type) arg2)
  "Doc string generic no-default xref-elisp-root-type."
  "non-default for no-default"
  (list this arg2)) ; silence byte-compiler

;; defgeneric after defmethod in file to ensure the fallback search
;; method of just looking for the function name will fail.
(cl-defgeneric xref-elisp-generic-no-default (arg1 arg2)
  "Doc string generic no-default generic."
  ;; No default implementation; this function calls the cl-generic
  ;; dispatching code.
  )

(with-no-warnings ; FIXME: Make more specific.
  (cl-defgeneric xref-elisp-generic-co-located-default (arg1 arg2)
    "Doc string generic co-located-default."
    "co-located default"))

(with-no-warnings ; FIXME: Make more specific.
  (cl-defmethod xref-elisp-generic-co-located-default ((this xref-elisp-root-type) arg2)
    "Doc string generic co-located-default xref-elisp-root-type."
    "non-default for co-located-default"))

(cl-defgeneric xref-elisp-generic-separate-default (arg1 arg2)
  "Doc string generic separate-default."
  ;; default implementation provided separately
  )

(cl-defmethod xref-elisp-generic-separate-default (arg1 arg2)
  "Doc string generic separate-default default."
  "separate default"
  (list arg1 arg2)) ; silence byte-compiler

(cl-defmethod xref-elisp-generic-separate-default ((this xref-elisp-root-type) arg2)
  "Doc string generic separate-default xref-elisp-root-type."
  "non-default for separate-default"
  (list this arg2)) ; silence byte-compiler

(cl-defmethod xref-elisp-generic-implicit-generic (arg1 arg2)
  "Doc string generic implicit-generic default."
  "default for implicit generic"
  (list arg1 arg2)) ; silence byte-compiler

(cl-defmethod xref-elisp-generic-implicit-generic ((this xref-elisp-root-type) arg2)
  "Doc string generic implicit-generic xref-elisp-root-type."
  "non-default for implicit generic"
  (list this arg2)) ; silence byte-compiler


(xref-elisp-deftest find-defs-defgeneric-no-methods
  (elisp--xref-find-definitions 'xref-elisp-generic-no-methods)
  (list
   (xref-make "(cl-defgeneric xref-elisp-generic-no-methods)"
	      (xref-make-elisp-location
	       'xref-elisp-generic-no-methods 'cl-defgeneric
	       (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   ))

(xref-elisp-deftest find-defs-defgeneric-no-default
  (elisp--xref-find-definitions 'xref-elisp-generic-no-default)
  (list
   (xref-make "(cl-defgeneric xref-elisp-generic-no-default)"
	      (xref-make-elisp-location
	       'xref-elisp-generic-no-default 'cl-defgeneric
	       (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   (xref-make "(cl-defmethod xref-elisp-generic-no-default ((this xref-elisp-root-type) arg2))"
	      (xref-make-elisp-location
	       (cl--generic-load-hist-format
                'xref-elisp-generic-no-default nil '(xref-elisp-root-type t))
               'cl-defmethod
	       (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   ))

(xref-elisp-deftest find-defs-defgeneric-co-located-default
  (elisp--xref-find-definitions 'xref-elisp-generic-co-located-default)
  (list
   (xref-make "(cl-defgeneric xref-elisp-generic-co-located-default)"
	      (xref-make-elisp-location
	       'xref-elisp-generic-co-located-default 'cl-defgeneric
	       (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   (xref-make "(cl-defmethod xref-elisp-generic-co-located-default ((this xref-elisp-root-type) arg2))"
	      (xref-make-elisp-location
	       (cl--generic-load-hist-format
                'xref-elisp-generic-co-located-default nil
                '(xref-elisp-root-type t))
               'cl-defmethod
	       (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   ))

(xref-elisp-deftest find-defs-defgeneric-separate-default
  (elisp--xref-find-definitions 'xref-elisp-generic-separate-default)
  (list
   (xref-make "(cl-defgeneric xref-elisp-generic-separate-default)"
	      (xref-make-elisp-location
	       'xref-elisp-generic-separate-default 'cl-defgeneric
	       (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   (xref-make "(cl-defmethod xref-elisp-generic-separate-default (arg1 arg2))"
              (xref-make-elisp-location
               (cl--generic-load-hist-format
                'xref-elisp-generic-separate-default nil '(t t))
               'cl-defmethod
               (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   (xref-make "(cl-defmethod xref-elisp-generic-separate-default ((this xref-elisp-root-type) arg2))"
	      (xref-make-elisp-location
               (cl--generic-load-hist-format
                'xref-elisp-generic-separate-default nil
                '(xref-elisp-root-type t))
               'cl-defmethod
	       (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   ))

(xref-elisp-deftest find-defs-defgeneric-implicit-generic
  (elisp--xref-find-definitions 'xref-elisp-generic-implicit-generic)
  (list
   (xref-make "(cl-defmethod xref-elisp-generic-implicit-generic (arg1 arg2))"
	      (xref-make-elisp-location
               (cl--generic-load-hist-format
                'xref-elisp-generic-implicit-generic nil '(t t))
               'cl-defmethod
	       (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   (xref-make "(cl-defmethod xref-elisp-generic-implicit-generic ((this xref-elisp-root-type) arg2))"
	      (xref-make-elisp-location
               (cl--generic-load-hist-format
                'xref-elisp-generic-implicit-generic nil
                '(xref-elisp-root-type t))
               'cl-defmethod
	       (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   ))

;; Test that we handle more than one method

;; When run from the Makefile, etags is not loaded at compile time,
;; but it is by the time this test is run.  interactively; don't fail
;; for that.
(require 'etags)
(xref-elisp-deftest find-defs-defgeneric-el
  (elisp--xref-find-definitions 'xref-location-marker)
  (list
   (xref-make "(cl-defgeneric xref-location-marker)"
	      (xref-make-elisp-location
	       'xref-location-marker 'cl-defgeneric
	       (expand-file-name "../../../lisp/progmodes/xref.el" emacs-test-dir)))
   (xref-make "(cl-defmethod xref-location-marker ((l xref-elisp-location)))"
	      (xref-make-elisp-location
	       (cl--generic-load-hist-format
                'xref-location-marker nil '(xref-elisp-location))
               'cl-defmethod
	       (expand-file-name "../../../lisp/progmodes/elisp-mode.el" emacs-test-dir)))
   (xref-make "(cl-defmethod xref-location-marker ((l xref-file-location)))"
	      (xref-make-elisp-location
	       (cl--generic-load-hist-format
                'xref-location-marker nil '(xref-file-location))
               'cl-defmethod
	       (expand-file-name "../../../lisp/progmodes/xref.el" emacs-test-dir)))
   (xref-make "(cl-defmethod xref-location-marker ((l xref-buffer-location)))"
	      (xref-make-elisp-location
	       (cl--generic-load-hist-format
                'xref-location-marker nil '(xref-buffer-location))
               'cl-defmethod
	       (expand-file-name "../../../lisp/progmodes/xref.el" emacs-test-dir)))
   (xref-make "(cl-defmethod xref-location-marker ((l xref-bogus-location)))"
	      (xref-make-elisp-location
	       (cl--generic-load-hist-format
                'xref-location-marker nil '(xref-bogus-location))
               'cl-defmethod
	       (expand-file-name "../../../lisp/progmodes/xref.el" emacs-test-dir)))
   (xref-make "(cl-defmethod xref-location-marker ((l xref-etags-location)))"
              (xref-make-elisp-location
               (cl--generic-load-hist-format
                'xref-location-marker nil '(xref-etags-location))
               'cl-defmethod
               (expand-file-name "../../../lisp/progmodes/etags.el" emacs-test-dir)))
   (xref-make "(cl-defmethod xref-location-marker ((l xref-etags-apropos-location)))"
              (xref-make-elisp-location
               (cl--generic-load-hist-format
                'xref-location-marker nil '(xref-etags-apropos-location))
               'cl-defmethod
               (expand-file-name "../../../lisp/progmodes/etags.el" emacs-test-dir)))
   ))

(xref-elisp-deftest find-defs-defgeneric-eval
  (elisp--xref-find-definitions (eval '(cl-defgeneric stephe-leake-cl-defgeneric ()) t))
  nil)

;; Define some mode-local overloadable/overridden functions for xref to find
(require 'mode-local)

(declare-function xref-elisp-overloadable-no-methods-default "elisp-mode-tests")
(declare-function xref-elisp-overloadable-no-default-default "elisp-mode-tests")

(define-overloadable-function xref-elisp-overloadable-no-methods ()
  "Doc string overloadable no-methods.")

(define-overloadable-function xref-elisp-overloadable-no-default ()
  "Doc string overloadable no-default.")

(define-mode-local-override xref-elisp-overloadable-no-default c-mode
  (_start _end &optional _nonterminal _depth _returnonerror)
  "Doc string overloadable no-default c-mode."
  "result overloadable no-default c-mode.")

(define-overloadable-function xref-elisp-overloadable-co-located-default ()
  "Doc string overloadable co-located-default."
  "result overloadable co-located-default.")

(define-mode-local-override xref-elisp-overloadable-co-located-default c-mode
  (_start _end &optional _nonterminal _depth _returnonerror)
  "Doc string overloadable co-located-default c-mode."
  "result overloadable co-located-default c-mode.")

(define-overloadable-function xref-elisp-overloadable-separate-default ()
  "Doc string overloadable separate-default.")

(defun xref-elisp-overloadable-separate-default-default ()
  "Doc string overloadable separate-default default."
  "result overloadable separate-default.")

(define-mode-local-override xref-elisp-overloadable-separate-default c-mode
  (_start _end &optional _nonterminal _depth _returnonerror)
  "Doc string overloadable separate-default c-mode."
  "result overloadable separate-default c-mode.")

(xref-elisp-deftest find-defs-define-overload-no-methods
  (elisp--xref-find-definitions 'xref-elisp-overloadable-no-methods)
  (list
   (xref-make "(define-overloadable-function xref-elisp-overloadable-no-methods)"
              (xref-make-elisp-location
               'xref-elisp-overloadable-no-methods 'define-overloadable-function
               (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   ))

(xref-elisp-deftest find-defs-define-overload-no-default
  (elisp--xref-find-definitions 'xref-elisp-overloadable-no-default)
  (list
   (xref-make "(define-overloadable-function xref-elisp-overloadable-no-default)"
              (xref-make-elisp-location
               'xref-elisp-overloadable-no-default 'define-overloadable-function
               (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   (xref-make "(define-mode-local-override xref-elisp-overloadable-no-default c-mode)"
              (xref-make-elisp-location
               '(xref-elisp-overloadable-no-default-c-mode . c-mode) 'define-mode-local-override
               (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   ))

(xref-elisp-deftest find-defs-define-overload-co-located-default
  (elisp--xref-find-definitions 'xref-elisp-overloadable-co-located-default)
  (list
   (xref-make "(define-overloadable-function xref-elisp-overloadable-co-located-default)"
              (xref-make-elisp-location
               'xref-elisp-overloadable-co-located-default 'define-overloadable-function
               (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   (xref-make "(define-mode-local-override xref-elisp-overloadable-co-located-default c-mode)"
              (xref-make-elisp-location
               '(xref-elisp-overloadable-co-located-default-c-mode . c-mode) 'define-mode-local-override
               (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   ))

(xref-elisp-deftest find-defs-define-overload-separate-default
  (elisp--xref-find-definitions 'xref-elisp-overloadable-separate-default)
  (list
   (xref-make "(define-overloadable-function xref-elisp-overloadable-separate-default)"
              (xref-make-elisp-location
               'xref-elisp-overloadable-separate-default 'define-overloadable-function
               (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   (xref-make "(defun xref-elisp-overloadable-separate-default-default)"
              (xref-make-elisp-location
               'xref-elisp-overloadable-separate-default-default nil
               (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   (xref-make "(define-mode-local-override xref-elisp-overloadable-separate-default c-mode)"
              (xref-make-elisp-location
               '(xref-elisp-overloadable-separate-default-c-mode . c-mode) 'define-mode-local-override
               (expand-file-name "elisp-mode-tests.el" emacs-test-dir)))
   ))

(xref-elisp-deftest find-defs-defun-el
  (elisp--xref-find-definitions 'xref-find-definitions)
  (list
   (xref-make "(defun xref-find-definitions)"
	      (xref-make-elisp-location
	       'xref-find-definitions nil
	       (expand-file-name "../../../lisp/progmodes/xref.el" emacs-test-dir)))))

(xref-elisp-deftest find-defs-defun-eval
  (elisp--xref-find-definitions (eval '(defun stephe-leake-defun ()) t))
  nil)

(xref-elisp-deftest find-defs-defun-c
  (elisp--xref-find-definitions 'buffer-live-p)
  (list
   (xref-make "(defun buffer-live-p)"
	      (xref-make-elisp-location 'buffer-live-p nil "src/buffer.c"))))

;; FIXME: deftype

(xref-elisp-deftest find-defs-defun-c-defvar-c
  (xref-backend-definitions 'elisp "system-name")
  (list
   (xref-make "(defvar system-name)"
	      (xref-make-elisp-location 'system-name 'defvar "src/editfns.c"))
   (xref-make "(defun system-name)"
              (xref-make-elisp-location 'system-name nil "src/editfns.c")))
  )

(xref-elisp-deftest find-defs-defun-el-defvar-c
  (xref-backend-definitions 'elisp "abbrev-mode")
  ;; It's a minor mode, but the variable is defined in buffer.c
  (list
   (xref-make "(defvar abbrev-mode)"
	      (xref-make-elisp-location 'abbrev-mode 'defvar "src/buffer.c"))
   (cons
    (xref-make "(defun abbrev-mode)"
               (xref-make-elisp-location
                'abbrev-mode nil
                (expand-file-name "../../../lisp/abbrev.el" emacs-test-dir)))
    "(define-minor-mode abbrev-mode"))
  )

;; Source for both variable and defun is "(define-minor-mode
;; compilation-minor-mode". There is no way to tell that directly from
;; the symbol, but we can use (memq sym minor-mode-list) to detect
;; that the symbol is a minor mode. In non-filtering mode we only
;; return the function.
(require 'compile) ;; not loaded by default at test time
(xref-elisp-deftest find-defs-defun-defvar-el
  (xref-backend-definitions 'elisp "compilation-minor-mode")
  (list
   (cons
    (xref-make "(defun compilation-minor-mode)"
               (xref-make-elisp-location
                'compilation-minor-mode nil
                (expand-file-name "../../../lisp/progmodes/compile.el" emacs-test-dir)))
    "(define-minor-mode compilation-minor-mode")
   ))

;; Returning only defvar because source near point indicates the user
;; is searching for a variable, not a function.
(xref-elisp-deftest find-defs-minor-defvar-c
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(foo overwrite-mode")
    (xref-backend-definitions 'elisp
                              (xref-backend-identifier-at-point 'elisp)))
  (list
   (cons
    (xref-make "(defvar overwrite-mode)"
               (xref-make-elisp-location 'overwrite-mode 'defvar "src/buffer.c"))
    "DEFVAR_PER_BUFFER (\"overwrite-mode\"")
   ))

(xref-elisp-deftest find-defs-defvar-el
  (elisp--xref-find-definitions 'xref--history)
  (list
   (xref-make "(defvar xref--history)"
	      (xref-make-elisp-location
	       'xref--history 'defvar
	       (expand-file-name "../../../lisp/progmodes/xref.el" emacs-test-dir)))
    ))

(xref-elisp-deftest find-defs-defvar-c
  (elisp--xref-find-definitions 'default-directory)
  (list
   (cons
    (xref-make "(defvar default-directory)"
               (xref-make-elisp-location 'default-directory 'defvar "src/buffer.c"))
    ;; IMPROVEME: we might be able to compute this target
    "DEFVAR_PER_BUFFER (\"default-directory\"")))

(xref-elisp-deftest find-defs-defvar-eval
  (elisp--xref-find-definitions (eval '(defvar stephe-leake-defvar nil) t))
  nil)

(xref-elisp-deftest find-defs-face-el
  (elisp--xref-find-definitions 'font-lock-keyword-face)
  ;; 'font-lock-keyword-face is both a face and a var
  (list
   (xref-make "(defvar font-lock-keyword-face)"
	      (xref-make-elisp-location
	       'font-lock-keyword-face 'defvar
	       (expand-file-name "../../../lisp/font-lock.el" emacs-test-dir)))
   (xref-make "(defface font-lock-keyword-face)"
	      (xref-make-elisp-location
	       'font-lock-keyword-face 'defface
	       (expand-file-name "../../../lisp/font-lock.el" emacs-test-dir)))
   ))

(xref-elisp-deftest find-defs-face-eval
  (elisp--xref-find-definitions (eval '(defface stephe-leake-defface nil "") t))
  nil)

(xref-elisp-deftest find-defs-feature-el
  (elisp--xref-find-definitions 'xref)
  (list
   (cons
    (xref-make "(feature xref)"
	      (xref-make-elisp-location
	       'xref 'feature
	       (expand-file-name "../../../lisp/progmodes/xref.el" emacs-test-dir)))
    ";;; Code:")
   ))

(xref-elisp-deftest find-defs-feature-eval
  (elisp--xref-find-definitions (eval '(provide 'stephe-leake-feature) t))
  nil)

(ert-deftest elisp--preceding-sexp--char-name ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "?\\N{HEAVY CHECK MARK}")
    (should (equal (elisp--preceding-sexp) ?\N{HEAVY CHECK MARK}))))

(defun test--font (form search)
  (with-temp-buffer
    (emacs-lisp-mode)
    (if (stringp form)
        (insert form)
      (pp form (current-buffer)))
    (with-suppressed-warnings ((interactive-only font-lock-debug-fontify))
      (font-lock-debug-fontify))
    (goto-char (point-min))
    (and (re-search-forward search nil t)
         (get-text-property (match-beginning 1) 'face))))

(ert-deftest test-elisp-font-keywords-1 ()
  ;; Special form.
  (should (eq (test--font '(if foo bar) "(\\(if\\)")
              'font-lock-keyword-face))
  ;; Macro.
  (should (eq (test--font '(when foo bar) "(\\(when\\)")
              'font-lock-keyword-face))
  (should (eq (test--font '(condition-case nil
                               (foo)
                             (error (if a b)))
                          "(\\(if\\)")
              'font-lock-keyword-face))
  (should (eq (test--font '(condition-case nil
                               (foo)
                             (when (if a b)))
                          "(\\(when\\)")
              'nil)))

(ert-deftest test-elisp-font-keywords-2 ()
  (should (eq (test--font '(condition-case nil
                               (foo)
                             (error (when a b)))
                          "(\\(when\\)")
              'font-lock-keyword-face)))

(ert-deftest test-elisp-font-keywords-3 ()
  (should (eq (test--font '(setq a '(if when zot))
                          "(\\(if\\)")
              nil)))

(ert-deftest test-elisp-font-keywords-4 ()
  :expected-result :failed ; FIXME bug#43265
  (should (eq (test--font '(condition-case nil
                               (foo)
                             ((if foo) (when a b)))
                          "(\\(if\\)")
              nil)))

(ert-deftest test-elisp-font-keywords-5 ()
  (should (eq (test--font '(condition-case (when a)
                               (foo)
                             (error t))
                          "(\\(when\\)")
              nil)))

(defmacro elisp-mode-test--with-buffer (text-with-pos &rest body)
  "Eval BODY with buffer and variables from TEXT-WITH-POS.
All occurrences of {NAME} are removed from TEXT-WITH-POS and
the remaining text put in a buffer in `elisp-mode'.
Each NAME is then bound to its position in the text during the
evaluation of BODY."
  (declare (indent 1))
  (let* ((annot-text (eval text-with-pos t))
         (pieces nil)
         (positions nil)
         (tlen (length annot-text))
         (ofs 0)
         (text-ofs 0))
    (while
        (and (< ofs tlen)
             (let ((m (string-match (rx "{" (group (+ (not "}"))) "}")
                                    annot-text ofs)))
               (and m
                    (let ((var (intern (match-string 1 annot-text))))
                      (push (substring annot-text ofs m) pieces)
                      (setq text-ofs (+ text-ofs (- m ofs)))
                      (push (list var (1+ text-ofs)) positions)
                      (setq ofs (match-end 0))
                      t)))))
    (push (substring annot-text ofs tlen) pieces)
    (let ((text (apply #'concat (nreverse pieces)))
          (bindings (nreverse positions)))
      `(with-temp-buffer
         (ert-info (,text :prefix "text: ")
           (emacs-lisp-mode)
           (insert ,text)
           (let ,bindings . ,body))))))

(ert-deftest elisp-mode-with-buffer ()
  ;; Sanity test of macro, also demonstrating how it works.
  (elisp-mode-test--with-buffer
      "{a}123{b}45{c}6"
    (should (equal a 1))
    (should (equal b 4))
    (should (equal c 6))
    (should (equal (buffer-string) "123456"))))

(ert-deftest elisp-mode-infer-namespace ()
  (elisp-mode-test--with-buffer
      (concat " ({p1}alphaX {p2}beta {p3}gamma '{p4}delta\n"
              "    #'{p5}epsilon `{p6}zeta `(,{p7}eta ,@{p8}theta))\n")
    (should (equal (elisp--xref-infer-namespace p1) 'function))
    (should (equal (elisp--xref-infer-namespace p2) 'maybe-variable))
    (should (equal (elisp--xref-infer-namespace p3) 'maybe-variable))
    (should (equal (elisp--xref-infer-namespace p4) 'any))
    (should (equal (elisp--xref-infer-namespace p5) 'function))
    (should (equal (elisp--xref-infer-namespace p6) 'any))
    (should (equal (elisp--xref-infer-namespace p7) 'variable))
    (should (equal (elisp--xref-infer-namespace p8) 'variable)))

  (elisp-mode-test--with-buffer
      (concat "(let ({p1}alpha {p2}beta ({p3}gamma {p4}delta))\n"
              "  ({p5}epsilon {p6}zeta)\n"
              "  {p7}eta)\n")
    (should (equal (elisp--xref-infer-namespace p1) 'variable))
    (should (equal (elisp--xref-infer-namespace p2) 'variable))
    (should (equal (elisp--xref-infer-namespace p3) 'variable))
    (should (equal (elisp--xref-infer-namespace p4) 'variable))
    (should (equal (elisp--xref-infer-namespace p5) 'function))
    (should (equal (elisp--xref-infer-namespace p6) 'maybe-variable))
    (should (equal (elisp--xref-infer-namespace p7) 'variable)))

  (elisp-mode-test--with-buffer
      (concat "(let (({p1}alpha {p2}beta)\n"
              "      ({p3}gamma ({p4}delta {p5}epsilon)))\n"
              "  ({p6}zeta))\n")
    (should (equal (elisp--xref-infer-namespace p1) 'variable))
    (should (equal (elisp--xref-infer-namespace p2) 'variable))
    (should (equal (elisp--xref-infer-namespace p3) 'variable))
    (should (equal (elisp--xref-infer-namespace p4) 'function))
    (should (equal (elisp--xref-infer-namespace p5) 'maybe-variable))
    (should (equal (elisp--xref-infer-namespace p6) 'function)))

  (elisp-mode-test--with-buffer
      (concat "(defun {p1}alpha () {p2}beta)\n"
              "(defface {p3}gamma ...)\n"
              "(defvar {p4}delta {p5}epsilon)\n"
              "(function {p6}zeta)\n")
    (should (equal (elisp--xref-infer-namespace p1) 'function))
    (should (equal (elisp--xref-infer-namespace p2) 'variable))
    (should (equal (elisp--xref-infer-namespace p3) 'face))
    (should (equal (elisp--xref-infer-namespace p4) 'variable))
    (should (equal (elisp--xref-infer-namespace p5) 'variable))
    (should (equal (elisp--xref-infer-namespace p6) 'function)))

  (elisp-mode-test--with-buffer
      (concat "(defclass child-class ({p1}parent-1 {p2}parent-2))\n")
    (should (equal (elisp--xref-infer-namespace p1) 'function))
    (should (equal (elisp--xref-infer-namespace p2) 'function)))

  (elisp-mode-test--with-buffer
      (concat "(require '{p1}alpha)\n"
              "(fboundp '{p2}beta)\n"
              "(boundp '{p3}gamma)\n"
              "(facep '{p4}delta)\n"
              "(define-key map [f1] '{p5}epsilon)\n")
    (should (equal (elisp--xref-infer-namespace p1) 'feature))
    (should (equal (elisp--xref-infer-namespace p2) 'function))
    (should (equal (elisp--xref-infer-namespace p3) 'variable))
    (should (equal (elisp--xref-infer-namespace p4) 'face))
    (should (equal (elisp--xref-infer-namespace p5) 'function)))

  (elisp-mode-test--with-buffer
      (concat "(list {p1}alpha {p2}beta)\n"
              "(progn {p3}gamma {p4}delta)\n"
              "(lambda ({p5}epsilon {p6}zeta) {p7}eta)\n")
    (should (equal (elisp--xref-infer-namespace p1) 'variable))
    (should (equal (elisp--xref-infer-namespace p2) 'variable))
    (should (equal (elisp--xref-infer-namespace p3) 'variable))
    (should (equal (elisp--xref-infer-namespace p4) 'variable))
    (should (equal (elisp--xref-infer-namespace p5) 'variable))
    (should (equal (elisp--xref-infer-namespace p6) 'variable))
    (should (equal (elisp--xref-infer-namespace p7) 'variable)))

  (elisp-mode-test--with-buffer
      (concat "'({p1}alpha {p2}beta\n"
              "  ({p3}gamma ({p4}delta)))\n")
    (should (equal (elisp--xref-infer-namespace p1) 'any))
    (should (equal (elisp--xref-infer-namespace p2) 'any))
    (should (equal (elisp--xref-infer-namespace p3) 'any))
    (should (equal (elisp--xref-infer-namespace p4) 'any))))


(ert-deftest elisp-shorthand-read-buffer ()
  (let* ((gsym (downcase (symbol-name (gensym "sh-"))))
         (shorthand-sname (format "s-%s" gsym))
         (expected (intern (format "shorthand-longhand-%s" gsym))))
    (cl-assert (not (intern-soft shorthand-sname)))
    (should (equal (let ((read-symbol-shorthands
                          '(("s-" . "shorthand-longhand-"))))
                     (with-temp-buffer
                       (insert shorthand-sname)
                       (goto-char (point-min))
                       (read (current-buffer))))
                   expected))
    (should (not (intern-soft shorthand-sname)))))

(ert-deftest elisp-shorthand-read-from-string ()
  (let* ((gsym (downcase (symbol-name (gensym "sh-"))))
         (shorthand-sname (format "s-%s" gsym))
         (expected (intern (format "shorthand-longhand-%s" gsym))))
    (cl-assert (not (intern-soft shorthand-sname)))
    (should (equal (let ((read-symbol-shorthands
                          '(("s-" . "shorthand-longhand-"))))
                     (car (read-from-string shorthand-sname)))
                   expected))
    (should (not (intern-soft shorthand-sname)))))

(ert-deftest elisp-shorthand-load-a-file ()
  (let ((test-file (ert-resource-file "simple-shorthand-test.el")))
    (mapatoms (lambda (s)
                (when (string-match "^elisp--foo-" (symbol-name s))
                  (unintern s obarray))))
    (load test-file nil t)
    (should (intern-soft "elisp--foo-test"))
    (should-not (intern-soft "f-test"))))

(ert-deftest elisp-shorthand-byte-compile-a-file ()

  (let ((test-file (ert-resource-file "simple-shorthand-test.el"))
        (byte-compiled (ert-resource-file "simple-shorthand-test.elc")))
    (mapatoms (lambda (s)
                (when (string-match "^elisp--foo-" (symbol-name s))
                  (unintern s obarray))))
    (byte-compile-file test-file)
    (should-not (intern-soft "f-test"))
    (should (intern-soft "elisp--foo-test"))
    (should-not (fboundp (intern-soft "elisp--foo-test")))
    (load byte-compiled nil t)
    (should (intern-soft "elisp--foo-test"))
    (should-not (intern-soft "f-test"))))

(ert-deftest elisp-shorthand-completion-at-point ()
  (let ((test-file (ert-resource-file "simple-shorthand-test.el")))
    (load test-file nil t)
    (with-current-buffer (find-file-noselect test-file)
      (revert-buffer t t)
      (goto-char (point-min))
      (insert "f-test-compl")
      (completion-at-point)
      (goto-char (point-min))
      (should (search-forward "f-test-complete-me" (pos-eol) t))
      (goto-char (point-min))
      (should (string= (symbol-name (read (current-buffer)))
                       "elisp--foo-test-complete-me"))
      (revert-buffer t t))))

(ert-deftest elisp-shorthand-escape ()
  (let ((test-file (ert-resource-file "simple-shorthand-test.el")))
    (load test-file nil t)
    (should (intern-soft "f-test4---"))
    (should-not (intern-soft "elisp--foo-test4---"))
    (should (= 84 (funcall (intern-soft "f-test4---"))))
    (should (unintern "f-test4---" obarray))))

(ert-deftest elisp-dont-shadow-punctuation-only-symbols ()
  (let* ((shorthanded-form '(/= 42 (-foo 42)))
         (expected-longhand-form '(/= 42 (fooey-foo 42)))
         (observed (let ((read-symbol-shorthands
                          '(("-" . "fooey-"))))
                     (car (read-from-string
                           (with-temp-buffer
                             (print shorthanded-form (current-buffer))
                             (buffer-string)))))))
    (should (equal observed expected-longhand-form))))

(ert-deftest test-indentation ()
  (ert-test-erts-file (ert-resource-file "elisp-indents.erts"))
  (ert-test-erts-file (ert-resource-file "flet.erts")
                      (lambda ()
                        (emacs-lisp-mode)
                        (indent-region (point-min) (point-max)))))

(ert-deftest elisp-tests-syntax-propertize ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(a '@)")                   ;bug#24542
    (should (equal (scan-sexps (+ (point-min) 3) 1) (1- (point-max))))
    (erase-buffer)
    (insert "(a ,@)")
    (should-error (scan-sexps (+ (point-min) 3) 1))))

(ert-deftest elisp-test-font-lock ()
  (ert-font-lock-test-file (ert-resource-file "semantic-highlighting.el")
                           (lambda ()
                             (emacs-lisp-mode)
                             (setq-local trusted-content :all
                                         elisp-fontify-semantically t
                                         read-symbol-shorthands
                                         '(("e-s-" . "elisp-scope-"))))))

(provide 'elisp-mode-tests)
;;; elisp-mode-tests.el ends here
