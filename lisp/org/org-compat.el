;;; org-compat.el --- Compatibility Code for Older Emacsen -*- lexical-binding: t; -*-

;; Copyright (C) 2004-2024 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, text
;; URL: https://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains code needed for compatibility with older
;; versions of GNU Emacs and integration with other packages.

;;; Code:


(require 'cl-lib)
(require 'seq)
(require 'org-macs)

(eval-when-compile (require 'subr-x))  ; Emacs < 28

;; We rely on org-compat when generating Org version.  Checking Org
;; version here will interfere with Org build process.
;; (org-assert-version)

(declare-function org-agenda-diary-entry "org-agenda")
(declare-function org-agenda-maybe-redo "org-agenda" ())
(declare-function org-agenda-set-restriction-lock "org-agenda" (&optional type))
(declare-function org-agenda-remove-restriction-lock "org-agenda" (&optional noupdate))
(declare-function org-calendar-goto-agenda "org-agenda" ())
(declare-function org-align-tags "org" (&optional all))
(declare-function org-at-heading-p "org" (&optional ignored))
(declare-function org-at-table.el-p "org-table" ())
(declare-function org-back-to-heading "org" (&optional invisible-ok))
(declare-function org-element-at-point "org-element" (&optional pom cached-only))
(declare-function org-element-at-point-no-context "org-element" (&optional pom))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-lineage "org-element-ast" (blob &optional types with-self))
(declare-function org-element-type "org-element-ast" (node &optional anonymous))
(declare-function org-element-type-p "org-element-ast" (node types))
(declare-function org-element-property "org-element-ast" (property node))
(declare-function org-element-begin "org-element" (node))
(declare-function org-element-end "org-element" (node))
(declare-function org-element-contents-begin "org-element" (node))
(declare-function org-element-contents-end "org-element" (node))
(declare-function org-element-post-affiliated "org-element" (node))
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading))
(declare-function org-get-heading "org" (&optional no-tags no-todo no-priority no-comment))
(declare-function org-get-tags "org" (&optional pos local))
(declare-function org-fold-hide-block-toggle "org-fold" (&optional force no-error element))
(declare-function org-link-display-format "ol" (s))
(declare-function org-link-set-parameters "ol" (type &rest rest))
(declare-function org-log-into-drawer "org" ())
(declare-function org-make-tag-string "org" (tags))
(declare-function org-next-visible-heading "org" (arg))
(declare-function org-reduced-level "org" (l))
(declare-function org-return "org" (&optional indent arg interactive))
(declare-function org-fold-show-context "org-fold" (&optional key))
(declare-function org-table-end "org-table" (&optional table-type))
(declare-function outline-next-heading "outline" ())
(declare-function speedbar-line-directory "speedbar" (&optional depth))
(declare-function table--at-cell-p "table" (position &optional object at-column))
(declare-function ob-clojure-eval-with-cmd "ob-clojure" (cmd expanded))
(declare-function org-fold-folded-p "org-fold" (&optional pos spec-or-alias))
(declare-function org-fold-hide-sublevels "org-fold" (levels))
(declare-function org-fold-hide-subtree "org-fold" ())
(declare-function org-fold-region "org-fold" (from to flag &optional spec))
(declare-function org-fold-show-all "org-fold" (&optional types))
(declare-function org-fold-show-children "org-fold" (&optional level))
(declare-function org-fold-show-entry "org-fold" (&optional hide-drawers))
;; `org-string-equal-ignore-case' is in _this_ file but isn't at the
;; top-level.
(declare-function org-string-equal-ignore-case "org-compat" (string1 string2))

(defvar calendar-mode-map)
(defvar org-complex-heading-regexp)
(defvar org-agenda-diary-file)
(defvar org-agenda-overriding-restriction)
(defvar org-agenda-restriction-lock-overlay)
(defvar org-table-any-border-regexp)
(defvar org-table-dataline-regexp)
(defvar org-table-tab-recognizes-table.el)
(defvar org-table1-hline-regexp)
(defvar org-fold-core-style)


;;; Emacs < 29 compatibility

(if (fboundp 'display-buffer-full-frame)
    (defalias 'org-display-buffer-full-frame #'display-buffer-full-frame)
  (defun org-display-buffer-full-frame (buffer alist)
    "Display BUFFER in the current frame, taking the entire frame.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
    (when-let ((window (or (display-buffer-reuse-window buffer alist)
                           (display-buffer-same-window buffer alist)
                           (display-buffer-pop-up-window buffer alist)
                           (display-buffer-use-some-window buffer alist))))
      (delete-other-windows window)
      window)))

(defvar org-file-has-changed-p--hash-table (make-hash-table :test #'equal)
  "Internal variable used by `org-file-has-changed-p'.")

(if (fboundp 'file-has-changed-p)
    (defalias 'org-file-has-changed-p #'file-has-changed-p)
  (defun org-file-has-changed-p (file &optional tag)
    "Return non-nil if FILE has changed.
The size and modification time of FILE are compared to the size
and modification time of the same FILE during a previous
invocation of `org-file-has-changed-p'.  Thus, the first invocation
of `org-file-has-changed-p' always returns non-nil when FILE exists.
The optional argument TAG, which must be a symbol, can be used to
limit the comparison to invocations with identical tags; it can be
the symbol of the calling function, for example."
    (let* ((file (directory-file-name (expand-file-name file)))
           (remote-file-name-inhibit-cache t)
           (fileattr (file-attributes file 'integer))
	   (attr (and fileattr
                      (cons (file-attribute-size fileattr)
		            (file-attribute-modification-time fileattr))))
	   (sym (concat (symbol-name tag) "@" file))
	   (cachedattr (gethash sym org-file-has-changed-p--hash-table)))
      (when (not (equal attr cachedattr))
        (puthash sym attr org-file-has-changed-p--hash-table)))))

(if (fboundp 'string-equal-ignore-case)
    (defalias 'org-string-equal-ignore-case #'string-equal-ignore-case)
  ;; From Emacs subr.el.
  (defun org-string-equal-ignore-case (string1 string2)
    "Like `string-equal', but case-insensitive.
Upper-case and lower-case letters are treated as equal.
Unibyte strings are converted to multibyte for comparison."
    (eq t (compare-strings string1 0 nil string2 0 nil t))))

(defun org-buffer-text-pixel-width ()
  "Return pixel width of text in current buffer.
This function uses `buffer-text-pixel-size', when available, and falls
back to `window-text-pixel-size' otherwise."
  (if (fboundp 'buffer-text-pixel-size)
      (car (buffer-text-pixel-size nil nil t))
    (if (get-buffer-window (current-buffer))
        ;; FIXME: 10000 because `most-positive-fixnum' ain't working
        ;; (tests failing) and this call will be removed after we drop
        ;; Emacs 28 support anyway.
        (car (window-text-pixel-size
              nil (point-min) (point-max) 10000))
      (let ((dedicatedp (window-dedicated-p))
            (oldbuffer (window-buffer)))
        (unwind-protect
            (progn
              ;; Do not throw error in dedicated windows.
              (set-window-dedicated-p nil nil)
              (set-window-buffer nil (current-buffer))
              (car (window-text-pixel-size
                    nil (point-min) (point-max) 10000)))
          (set-window-buffer nil oldbuffer)
          (set-window-dedicated-p nil dedicatedp))))))


;;; Emacs < 28.1 compatibility

(if (= 2 (cdr (subr-arity (symbol-function 'get-buffer-create))))
    ;; Emacs >27.
    (defalias 'org-get-buffer-create #'get-buffer-create)
  (defun org-get-buffer-create (buffer-or-name &optional _)
    "Call `get-buffer-create' with BUFFER-OR-NAME argument.
Ignore optional argument."
    (get-buffer-create buffer-or-name)))

(if (fboundp 'file-name-concat)
    (defalias 'org-file-name-concat #'file-name-concat)
  (defun org-file-name-concat (directory &rest components)
    "Append COMPONENTS to DIRECTORY and return the resulting string.

Elements in COMPONENTS must be a string or nil.
DIRECTORY or the non-final elements in COMPONENTS may or may not end
with a slash -- if they don't end with a slash, a slash will be
inserted before concatenating."
    (save-match-data
      (mapconcat
       #'identity
       (delq nil
             (mapcar
              (lambda (str)
                (when (and str (not (seq-empty-p str))
                           (string-match "\\(.+\\)/?" str))
                  (match-string 1 str)))
              (cons directory components)))
       "/"))))

(if (fboundp 'directory-empty-p)
    (defalias 'org-directory-empty-p #'directory-empty-p)
  (defun org-directory-empty-p (dir)
    "Return t if DIR names an existing directory containing no other files."
    (and (file-directory-p dir)
         (null (directory-files dir nil directory-files-no-dot-files-regexp t)))))

(if (fboundp 'string-clean-whitespace)
    (defalias 'org-string-clean-whitespace #'string-clean-whitespace)
  ;; From Emacs subr-x.el.
  (defun org-string-clean-whitespace (string)
    "Clean up whitespace in STRING.
All sequences of whitespaces in STRING are collapsed into a
single space character, and leading/trailing whitespace is
removed."
    (let ((blank "[[:blank:]\r\n]+"))
      (string-trim (replace-regexp-in-string blank " " string t t)
                   blank blank))))

(if (fboundp 'format-prompt)
    (defalias 'org-format-prompt #'format-prompt)
  ;; From Emacs minibuffer.el, inlining
  ;; `minibuffer-default-prompt-format' value and replacing `length<'
  ;; (both new in Emacs 28.1).
  (defun org-format-prompt (prompt default &rest format-args)
    "Compatibility substitute for `format-prompt'."
    (concat
     (if (null format-args)
         prompt
       (apply #'format prompt format-args))
     (and default
          (or (not (stringp default))
              (> (length default) 0))
          (format " (default %s)"
                  (if (consp default)
                      (car default)
                    default)))
     ": ")))


;;; Emacs < 27.1 compatibility

(if (version< emacs-version "29")
    ;; A stub when `combine-change-calls' was not yet there or had
    ;; critical bugs (see Emacs bug#60467).
    (defmacro org-combine-change-calls (_beg _end &rest body)
      (declare (debug (form form def-body)) (indent 2))
      `(progn ,@body))
  (defalias 'org-combine-change-calls 'combine-change-calls))

;; `flatten-tree' was added in Emacs 27.1.
(if (fboundp 'flatten-tree)
    (defalias 'org--flatten-tree #'flatten-tree)
  ;; The implementation is taken from Emacs subr.el 8664ba18c7c5.
  (defun org--flatten-tree (tree)
    "Return a \"flattened\" copy of TREE.

A `flatten-tree' polyfill for compatibility with Emacs versions
older than 27.1"
    (let (elems)
      (while (consp tree)
        (let ((elem (pop tree)))
          (while (consp elem)
            (push (cdr elem) tree)
            (setq elem (car elem)))
          (if elem (push elem elems))))
      (if tree (push tree elems))
      (nreverse elems))))

(if (version< emacs-version "27.1")
    (defsubst org-replace-buffer-contents (source &optional _max-secs _max-costs)
      (replace-buffer-contents source))
  (defalias 'org-replace-buffer-contents #'replace-buffer-contents))

(unless (fboundp 'proper-list-p)
  ;; `proper-list-p' was added in Emacs 27.1.  The function below is
  ;; taken from Emacs subr.el 200195e824b^.
  (defun proper-list-p (object)
    "Return OBJECT's length if it is a proper list, nil otherwise.
A proper list is neither circular nor dotted (i.e., its last cdr
is nil)."
    (and (listp object) (ignore-errors (length object)))))

(if (fboundp 'xor)
    ;; `xor' was added in Emacs 27.1.
    (defalias 'org-xor #'xor)
  (defsubst org-xor (a b)
    "Exclusive `or'."
    (if a (not b) b)))

(unless (fboundp 'pcomplete-uniquify-list)
  ;; The misspelled variant was made obsolete in Emacs 27.1
  (defalias 'pcomplete-uniquify-list 'pcomplete-uniqify-list))

(if (fboundp 'time-convert)
    (progn
      (defsubst org-time-convert-to-integer (time)
	(time-convert time 'integer))
      (defsubst org-time-convert-to-list (time)
	(time-convert time 'list)))
  (defun org-time-convert-to-integer (time)
    (floor (float-time time)))
  (defun org-time-convert-to-list (time)
    (seconds-to-time (float-time time))))

;; `newline-and-indent' did not take a numeric argument before 27.1.
(if (version< emacs-version "27")
    (defsubst org-newline-and-indent (&optional _arg)
      (newline-and-indent))
  (defalias 'org-newline-and-indent #'newline-and-indent))

(defun org--set-faces-extend (faces extend-p)
  "Set the :extend attribute of FACES to EXTEND-P.

This is a no-op for Emacs versions lower than 27, since face
extension beyond end of line was not controllable."
  (when (fboundp 'set-face-extend)
    (mapc (lambda (f) (set-face-extend f extend-p)) faces)))

(if (fboundp 'string-distance)
    (defalias 'org-string-distance 'string-distance)
  (defun org-string-distance (s1 s2)
    "Return the edit (levenshtein) distance between strings S1 S2."
    (let* ((l1 (length s1))
	   (l2 (length s2))
	   (dist (vconcat (mapcar (lambda (_) (make-vector (1+ l2) nil))
				  (number-sequence 1 (1+ l1)))))
	   (in (lambda (i j) (aref (aref dist i) j))))
      (setf (aref (aref dist 0) 0) 0)
      (dolist (j (number-sequence 1 l2))
        (setf (aref (aref dist 0) j) j))
      (dolist (i (number-sequence 1 l1))
        (setf (aref (aref dist i) 0) i)
        (dolist (j (number-sequence 1 l2))
	  (setf (aref (aref dist i) j)
	        (min
	         (1+ (funcall in (1- i) j))
	         (1+ (funcall in i (1- j)))
	         (+ (if (equal (aref s1 (1- i)) (aref s2 (1- j))) 0 1)
		    (funcall in (1- i) (1- j)))))))
      (funcall in l1 l2))))

(define-obsolete-function-alias 'org-babel-edit-distance 'org-string-distance
  "9.5")

(unless (fboundp 'with-connection-local-variables)
  ;; Added in Emacs 27: commit:21f54feee8, 2019-03-09.
  ;; Redefining it using the old function `with-connection-local-profiles'.
  (defmacro with-connection-local-variables (&rest body)
    "Apply connection-local variables according to `default-directory'.
Execute BODY, and unwind connection-local variables."
    (declare (debug t))
    `(with-connection-local-profiles (connection-local-get-profiles nil)
       ,@body)))

;; assoc-delete-all missing from 26.1
(if (fboundp 'assoc-delete-all)
    (defalias 'org-assoc-delete-all 'assoc-delete-all)
  ;; from compat/compat-27.el
  (defun org-assoc-delete-all (key alist &optional test)
    "Delete all matching key from alist, default test equal"
    (unless test (setq test #'equal))
    (while (and (consp (car alist))
		(funcall test (caar alist) key))
      (setq alist (cdr alist)))
    (let ((tail alist) tail-cdr)
      (while (setq tail-cdr (cdr tail))
	(if (and (consp (car tail-cdr))
		 (funcall test (caar tail-cdr) key))
            (setcdr tail (cdr tail-cdr))
          (setq tail tail-cdr))))
    alist))


;;; Emacs < 26.1 compatibility

(if (fboundp 'line-number-display-width)
    (defalias 'org-line-number-display-width 'line-number-display-width)
  (defun org-line-number-display-width (&rest _) 0))

(if (fboundp 'buffer-hash)
    (defalias 'org-buffer-hash 'buffer-hash)
  (defun org-buffer-hash () (md5 (current-buffer))))

(unless (fboundp 'file-attribute-modification-time)
  (defsubst file-attribute-modification-time (attributes)
    "The modification time in ATTRIBUTES returned by `file-attributes'.
This is the time of the last change to the file's contents, and
is a Lisp timestamp in the same style as `current-time'."
    (nth 5 attributes)))

(unless (fboundp 'file-attribute-size)
  (defsubst file-attribute-size (attributes)
    "The size (in bytes) in ATTRIBUTES returned by `file-attributes'.
This is a floating point number if the size is too large for an integer."
    (nth 7 attributes)))


;;; Obsolete aliases (remove them after the next major release).

;;;; XEmacs compatibility, now removed.
(define-obsolete-function-alias 'org-activate-mark 'activate-mark "9.0")
(define-obsolete-function-alias 'org-add-hook 'add-hook "9.0")
(define-obsolete-function-alias 'org-bound-and-true-p 'bound-and-true-p "9.0")
(define-obsolete-function-alias 'org-decompose-region 'decompose-region "9.0")
(define-obsolete-function-alias 'org-defvaralias 'defvaralias "9.0")
(define-obsolete-function-alias 'org-detach-overlay 'delete-overlay "9.0")
(define-obsolete-function-alias 'org-file-equal-p 'file-equal-p "9.0")
(define-obsolete-function-alias 'org-float-time 'float-time "9.0")
(define-obsolete-function-alias 'org-indent-line-to 'indent-line-to "9.0")
(define-obsolete-function-alias 'org-indent-to-column 'indent-to-column "9.0")
(define-obsolete-function-alias 'org-looking-at-p 'looking-at-p "9.0")
(define-obsolete-function-alias 'org-looking-back 'looking-back "9.0")
(define-obsolete-function-alias 'org-match-string-no-properties 'match-string-no-properties "9.0")
(define-obsolete-function-alias 'org-propertize 'propertize "9.0")
(define-obsolete-function-alias 'org-select-frame-set-input-focus 'select-frame-set-input-focus "9.0")
(define-obsolete-function-alias 'org-file-remote-p 'file-remote-p "9.2")

(define-obsolete-function-alias 'org-show-context 'org-fold-show-context "9.6")
(define-obsolete-function-alias 'org-show-entry 'org-fold-show-entry "9.6")
(define-obsolete-function-alias 'org-show-children 'org-fold-show-children "9.6")


(defmacro org-re (s)
  "Replace posix classes in regular expression S."
  (declare (debug (form))
           (obsolete "you can safely remove it." "9.0"))
  s)

;;;; Functions from cl-lib that Org used to have its own implementation of.
(define-obsolete-function-alias 'org-count 'cl-count "9.0")
(define-obsolete-function-alias 'org-every 'cl-every "9.0")
(define-obsolete-function-alias 'org-find-if 'cl-find-if "9.0")
(define-obsolete-function-alias 'org-reduce 'cl-reduce "9.0")
(define-obsolete-function-alias 'org-remove-if 'cl-remove-if "9.0")
(define-obsolete-function-alias 'org-remove-if-not 'cl-remove-if-not "9.0")
(define-obsolete-function-alias 'org-some 'cl-some "9.0")
(define-obsolete-function-alias 'org-floor* 'cl-floor "9.0")

(defun org-sublist (list start end)
  "Return a section of LIST, from START to END.
Counting starts at 1."
  (cl-subseq list (1- start) end))
(make-obsolete 'org-sublist
               "use cl-subseq (note the 0-based counting)."
               "9.0")

;;;; Functions available since Emacs 25.1
(define-obsolete-function-alias 'org-string-collate-lessp 'string-collate-lessp "9.6")
(define-obsolete-function-alias 'org-decode-time 'decode-time "9.6")
(define-obsolete-function-alias 'org-format-time-string 'format-time-string "9.6")
(define-obsolete-function-alias 'org-time-add 'time-add "9.6")
(define-obsolete-function-alias 'org-time-subtract 'time-subtract "9.6")
(define-obsolete-function-alias 'org-time-since 'time-since "9.6")
(define-obsolete-function-alias 'org-time-less-p 'time-less-p "9.6")

;;;; Functions available since Emacs 24.3
(define-obsolete-function-alias 'org-buffer-narrowed-p 'buffer-narrowed-p "9.0")
(define-obsolete-function-alias 'org-called-interactively-p 'called-interactively-p "9.0")
(define-obsolete-function-alias 'org-char-to-string 'char-to-string "9.0")
(define-obsolete-function-alias 'org-delete-directory 'delete-directory "9.0")
(define-obsolete-function-alias 'org-format-seconds 'format-seconds "9.0")
(define-obsolete-function-alias 'org-link-escape-browser 'url-encode-url "9.0")
(define-obsolete-function-alias 'org-no-warnings 'with-no-warnings "9.0")
(define-obsolete-function-alias 'org-number-sequence 'number-sequence "9.0")
(define-obsolete-function-alias 'org-pop-to-buffer-same-window 'pop-to-buffer-same-window "9.0")
(define-obsolete-function-alias 'org-string-match-p 'string-match-p "9.0")

;;;; Functions and variables from previous releases now obsolete.
(define-obsolete-variable-alias 'org-export-ignored-local-variables
  'org-element-ignored-local-variables "Org 9.7")
(define-obsolete-function-alias 'org-habit-get-priority
  'org-habit-get-urgency "Org 9.7")
(define-obsolete-function-alias 'org-timestamp-format
  'org-format-timestamp "Org 9.6")
(define-obsolete-variable-alias 'org-export-before-processing-hook
  'org-export-before-processing-functions "Org 9.6")
(define-obsolete-variable-alias 'org-export-before-parsing-hook
  'org-export-before-parsing-functions "Org 9.6")
(define-obsolete-function-alias 'org-element-remove-indentation
  'org-remove-indentation "9.0")
(define-obsolete-variable-alias 'org-latex-create-formula-image-program
  'org-preview-latex-default-process "9.0")
(define-obsolete-variable-alias 'org-latex-preview-ltxpng-directory
  'org-preview-latex-image-directory "9.0")
(define-obsolete-variable-alias 'org-latex-listings
  'org-latex-src-block-backend "9.6")
(define-obsolete-function-alias 'org-table-p 'org-at-table-p "9.0")
(define-obsolete-function-alias 'org-on-heading-p 'org-at-heading-p "9.0")
(define-obsolete-function-alias 'org-at-regexp-p 'org-in-regexp "8.3")
(define-obsolete-function-alias 'org-image-file-name-regexp
  'image-file-name-regexp "9.0")
(define-obsolete-function-alias 'org-completing-read-no-i
  'completing-read "9.0")
(define-obsolete-function-alias 'org-icompleting-read
  'completing-read "9.0")
(define-obsolete-function-alias 'org-iread-file-name 'read-file-name "9.0")
(define-obsolete-function-alias 'org-days-to-time
  'org-timestamp-to-now "8.2")
(define-obsolete-variable-alias 'org-agenda-ignore-drawer-properties
  'org-agenda-ignore-properties "9.0")
(define-obsolete-function-alias 'org-preview-latex-fragment
  'org-toggle-latex-fragment "8.3")
(define-obsolete-function-alias 'org-export-get-genealogy
  'org-element-lineage "9.0")
(define-obsolete-variable-alias 'org-latex-with-hyperref
  'org-latex-hyperref-template "9.0")
(define-obsolete-variable-alias 'hfy-optimisations 'hfy-optimizations "9.0")
(define-obsolete-variable-alias 'org-export-htmlized-org-css-url
  'org-org-htmlized-css-url "8.2")
(define-obsolete-function-alias 'org-list-parse-list 'org-list-to-lisp "9.0")
(define-obsolete-function-alias 'org-agenda-todayp
  'org-agenda-today-p "9.0")
(define-obsolete-function-alias 'org-babel-examplize-region
  'org-babel-examplify-region "9.0")
(define-obsolete-variable-alias 'org-babel-capitalize-example-region-markers
  'org-babel-uppercase-example-markers "9.1")

(define-obsolete-function-alias 'org-babel-trim 'org-trim "9.0")
(define-obsolete-variable-alias 'org-html-style 'org-html-head "24.4")
(define-obsolete-function-alias 'org-insert-columns-dblock
  'org-columns-insert-dblock "9.0")
(define-obsolete-variable-alias 'org-export-babel-evaluate
  'org-export-use-babel "9.1")
(define-obsolete-function-alias 'org-activate-bracket-links
  'org-activate-links "9.0")
(define-obsolete-function-alias 'org-activate-plain-links 'ignore "9.0")
(define-obsolete-function-alias 'org-activate-angle-links 'ignore "9.0")
(define-obsolete-function-alias 'org-remove-double-quotes 'org-strip-quotes "9.0")
(define-obsolete-function-alias 'org-get-indentation
  'current-indentation "9.2")
(define-obsolete-function-alias 'org-capture-member 'org-capture-get "9.2")
(define-obsolete-function-alias 'org-remove-from-invisibility-spec
  'remove-from-invisibility-spec "9.2")

(define-obsolete-variable-alias 'org-effort-durations 'org-duration-units
  "9.2")

(define-obsolete-function-alias 'org-toggle-latex-fragment 'org-latex-preview
  "9.3")

(define-obsolete-function-alias 'org-remove-latex-fragment-image-overlays
  'org-clear-latex-preview "9.3")

(define-obsolete-function-alias 'org-hide-archived-subtrees
  'org-fold-hide-archived-subtrees "9.6")

(define-obsolete-function-alias 'org-flag-region
  'org-fold-region "9.6")

(define-obsolete-function-alias 'org-flag-subtree
  'org-fold-subtree "9.6")

(define-obsolete-function-alias 'org-hide-entry
  'org-fold-hide-entry "9.6")

(define-obsolete-function-alias 'org-show-subtree
  'org-fold-show-subtree "9.6")

(define-obsolete-function-alias 'org--hide-wrapper-toggle
  'org-fold--hide-wrapper-toggle "9.6")

(define-obsolete-function-alias 'org-hide-block-toggle
  'org-fold-hide-block-toggle "9.6")

(define-obsolete-function-alias 'org-hide-drawer-toggle
  'org-fold-hide-drawer-toggle "9.6")

(define-obsolete-function-alias 'org--hide-drawers
  'org-fold--hide-drawers "9.6")

(define-obsolete-function-alias 'org-hide-block-all
  'org-fold-hide-block-all "9.6")

(define-obsolete-function-alias 'org-hide-drawer-all
  'org-fold-hide-drawer-all "9.6")

(define-obsolete-function-alias 'org-show-all
  'org-fold-show-all "9.6")

(define-obsolete-function-alias 'org-set-startup-visibility
  'org-cycle-set-startup-visibility "9.6")

(define-obsolete-function-alias 'org-show-set-visibility
  'org-fold-show-set-visibility "9.6")

(define-obsolete-function-alias 'org-check-before-invisible-edit
  'org-fold-check-before-invisible-edit "9.6")

(define-obsolete-function-alias 'org-flag-above-first-heading
  'org-fold-flag-above-first-heading "9.6")

(define-obsolete-function-alias 'org-show-branches-buffer
  'org-fold-show-branches-buffer "9.6")

(define-obsolete-function-alias 'org-show-siblings
  'org-fold-show-siblings "9.6")

(define-obsolete-function-alias 'org-show-hidden-entry
  'org-fold-show-hidden-entry "9.6")

(define-obsolete-function-alias 'org-flag-heading
  'org-fold-heading "9.6")

(define-obsolete-function-alias 'org-set-startup-visibility
  'org-cycle-set-startup-visibility "9.6")

(define-obsolete-function-alias 'org-set-visibility-according-to-property
  'org-cycle-set-visibility-according-to-property "9.6")

(define-obsolete-variable-alias 'org-scroll-position-to-restore
  'org-cycle-scroll-position-to-restore "9.6")
(define-obsolete-function-alias 'org-optimize-window-after-visibility-change
  'org-cycle-optimize-window-after-visibility-change "9.6")

(define-obsolete-function-alias 'org-force-cycle-archived
  'org-cycle-force-archived "9.6")

(define-obsolete-variable-alias 'org-attach-directory
  'org-attach-id-dir "9.3")
(make-obsolete 'org-attach-store-link "No longer used" "9.4")
(make-obsolete 'org-attach-expand-link "No longer used" "9.4")

(define-obsolete-function-alias 'org-file-url-p 'org-url-p "9.5")

(define-obsolete-variable-alias 'org-show-context-detail
  'org-fold-show-context-detail "9.6")

(define-obsolete-variable-alias 'org-catch-invisible-edits
  'org-fold-catch-invisible-edits "9.6")

(define-obsolete-variable-alias 'org-reveal-start-hook
  'org-fold-reveal-start-hook "9.6")
(define-obsolete-function-alias 'org-file-url-p 'org-url-p "9.6")
(define-obsolete-variable-alias 'org-plantuml-executable-args 'org-plantuml-args
  "Org 9.6")

(defvar org-cached-props nil)
(defvar org-use-property-inheritance)
(declare-function org-entry-get "org" (epom property &optional inherit literal-nil))
(declare-function org-entry-properties "org" (&optional epom which))
(defun org-cached-entry-get (pom property)
  (if (or (eq t org-use-property-inheritance)
	  (and (stringp org-use-property-inheritance)
	       (let ((case-fold-search t))
		 (string-match-p org-use-property-inheritance property)))
	  (and (listp org-use-property-inheritance)
	       (member-ignore-case property org-use-property-inheritance)))
      ;; Caching is not possible, check it directly.
      (org-entry-get pom property 'inherit)
    ;; Get all properties, so we can do complicated checks easily.
    (cdr (assoc-string property
		       (or org-cached-props
			   (setq org-cached-props (org-entry-properties pom)))
		       t))))

(make-obsolete 'org-cached-entry-get
               "Performs badly.  Instead use `org-entry-get' with the argument INHERIT set to `selective'"
               "9.7")

(defconst org-latex-line-break-safe "\\\\[0pt]"
  "Linebreak protecting the following [...].

Without \"[0pt]\" it would be interpreted as an optional argument to
the \\\\.

This constant, for example, makes the below code not err:

\\begin{tabular}{c|c}
    [t] & s\\\\[0pt]
    [I] & A\\\\[0pt]
    [m] & kg
\\end{tabular}")
(make-obsolete 'org-latex-line-break-safe
               "should not be used - it is not safe in all the scenarios."
               "9.7")

(defun org-in-fixed-width-region-p ()
  "Non-nil if point in a fixed-width region."
  (save-match-data
    (org-element-type-p (org-element-at-point) 'fixed-width)))
(make-obsolete 'org-in-fixed-width-region-p
               "use `org-element' library"
               "9.0")

;; FIXME: Unused; obsoleted; to be removed.
(defun org-let (list &rest body) ;FIXME: So many kittens are suffering here.
  (declare (indent 1) (obsolete cl-progv "2021"))
  (eval (cons 'let (cons list body))))

;; FIXME: Unused; obsoleted; to be removed.
(defun org-let2 (list1 list2 &rest body) ;FIXME: Where did our karma go?
  (declare (indent 2) (obsolete cl-progv "2021"))
  (eval (cons 'let (cons list1 (list (cons 'let (cons list2 body)))))))

(make-obsolete 'org-let "to be removed" "9.6")
(make-obsolete 'org-let2 "to be removed" "9.6")

(define-obsolete-function-alias 'org--math-always-on
  'org--math-p "9.7")

(defmacro org-no-popups (&rest body)
  "Suppress popup windows and evaluate BODY."
  `(let (pop-up-frames pop-up-windows)
     ,@body))
(make-obsolete 'org-no-popups "no longer used" "9.7")

(defun org-switch-to-buffer-other-window (&rest args)
  "Switch to buffer in a second window on the current frame.
In particular, do not allow pop-up frames.
Returns the newly created buffer."
  (let (pop-up-frames pop-up-windows)
    (apply #'switch-to-buffer-other-window args)))
  (make-obsolete 'org-switch-to-buffer-other-window "no longer used" "9.7")

(make-obsolete 'org-refresh-category-properties "no longer used" "9.7")
(make-obsolete 'org-refresh-effort-properties "no longer used" "9.7")

(defun org-compatible-face (inherits specs)
  "Make a compatible face specification.
If INHERITS is an existing face and if the Emacs version supports
it, just inherit the face.  If INHERITS is not given and SPECS
is, use SPECS to define the face."
  (declare (indent 1))
  (if (facep inherits)
      (list (list t :inherit inherits))
    specs))
(make-obsolete 'org-compatible-face "you can remove it." "9.0")

(defun org-add-link-type (type &optional follow export)
  "Add a new TYPE link.
FOLLOW and EXPORT are two functions.

FOLLOW should take the link path as the single argument and do whatever
is necessary to follow the link, for example find a file or display
a mail message.

EXPORT should format the link path for export to one of the export formats.
It should be a function accepting three arguments:

  path    the path of the link, the text after the prefix (like \"http:\")
  desc    the description of the link, if any
  format  the export format, a symbol like `html' or `latex' or `ascii'.

The function may use the FORMAT information to return different values
depending on the format.  The return value will be put literally into
the exported file.  If the return value is nil, this means Org should
do what it normally does with links which do not have EXPORT defined.

Org mode has a built-in default for exporting links.  If you are happy with
this default, there is no need to define an export function for the link
type.  For a simple example of an export function, see `org-bbdb.el'.

If TYPE already exists, update it with the arguments.
See `org-link-parameters' for documentation on the other parameters."
  (org-link-set-parameters type :follow follow :export export)
  (message "Created %s link." type))

(make-obsolete 'org-add-link-type "use `org-link-set-parameters' instead." "9.0")

;;;; Functions unused in Org core.
(defun org-table-recognize-table.el ()
  "If there is a table.el table nearby, recognize it and move into it."
  (when (org-at-table.el-p)
    (forward-line 0)
    (unless (or (looking-at org-table-dataline-regexp)
                (not (looking-at org-table1-hline-regexp)))
      (forward-line)
      (when (looking-at org-table-any-border-regexp)
        (forward-line -2)))
    (if (re-search-forward "|" (org-table-end t) t)
        (progn
          (require 'table)
          (if (table--at-cell-p (point)) t
            (message "recognizing table.el table...")
            (table-recognize-table)
            (message "recognizing table.el table...done")))
      (error "This should not happen"))))

;; Not used since commit 6d1e3082, Feb 2010.
(make-obsolete 'org-table-recognize-table.el
               "please notify Org mailing list if you use this function."
               "9.0")

(defmacro org-preserve-lc (&rest body)
  (declare (debug (body))
	   (obsolete "please notify Org mailing list if you use this function."
		     "9.2"))
  (org-with-gensyms (line col)
    `(let ((,line (org-current-line))
	   (,col (current-column)))
       (unwind-protect
	   (progn ,@body)
	 (org-goto-line ,line)
	 (org-move-to-column ,col)))))

(defun org-version-check (version &rest _)
  "Non-nil if VERSION is lower (older) than `emacs-version'."
  (declare (obsolete "use `version<' or `fboundp' instead."
		     "9.2"))
  (version< version emacs-version))

(defun org-remove-angle-brackets (s)
  (org-unbracket-string "<" ">" s))
(make-obsolete 'org-remove-angle-brackets 'org-unbracket-string "9.0")

(defcustom org-capture-bookmark t
  "When non-nil, add bookmark pointing at the last stored position when capturing."
  :group 'org-capture
  :version "24.3"
  :type 'boolean)
(make-obsolete-variable
 'org-capture-bookmark
 "use `org-bookmark-names-plist' instead."
 "9.7")

(defcustom org-publish-sitemap-file-entry-format "%t"
  "Format string for site-map file entry.
You could use brackets to delimit on what part the link will be.

%t is the title.
%a is the author.
%d is the date."
  :group 'org-export-publish
  :type 'string)
(make-obsolete-variable
 'org-publish-sitemap-file-entry-format
 "set `:sitemap-format-entry' in `org-publish-project-alist' instead."
 "9.1")

(defvar org-agenda-skip-regexp)
(defun org-agenda-skip-entry-when-regexp-matches ()
  "Check if the current entry contains match for `org-agenda-skip-regexp'.
If yes, it returns the end position of this entry, causing agenda commands
to skip the entry but continuing the search in the subtree.  This is a
function that can be put into `org-agenda-skip-function' for the duration
of a command."
  (declare (obsolete "use `org-agenda-skip-if' instead." "9.1"))
  (let ((end (save-excursion (org-end-of-subtree t)))
	skip)
    (save-excursion
      (setq skip (re-search-forward org-agenda-skip-regexp end t)))
    (and skip end)))

(defun org-agenda-skip-subtree-when-regexp-matches ()
  "Check if the current subtree contains match for `org-agenda-skip-regexp'.
If yes, it returns the end position of this tree, causing agenda commands
to skip this subtree.  This is a function that can be put into
`org-agenda-skip-function' for the duration of a command."
  (declare (obsolete "use `org-agenda-skip-if' instead." "9.1"))
  (let ((end (save-excursion (org-end-of-subtree t)))
	skip)
    (save-excursion
      (setq skip (re-search-forward org-agenda-skip-regexp end t)))
    (and skip end)))

(defun org-agenda-skip-entry-when-regexp-matches-in-subtree ()
  "Check if the current subtree contains match for `org-agenda-skip-regexp'.
If yes, it returns the end position of the current entry (NOT the tree),
causing agenda commands to skip the entry but continuing the search in
the subtree.  This is a function that can be put into
`org-agenda-skip-function' for the duration of a command.  An important
use of this function is for the stuck project list."
  (declare (obsolete "use `org-agenda-skip-if' instead." "9.1"))
  (let ((end (save-excursion (org-end-of-subtree t)))
	(entry-end (save-excursion (outline-next-heading) (1- (point))))
	skip)
    (save-excursion
      (setq skip (re-search-forward org-agenda-skip-regexp end t)))
    (and skip entry-end)))

(define-obsolete-function-alias 'org-minutes-to-clocksum-string
  'org-duration-from-minutes "9.1")

(define-obsolete-function-alias 'org-hh:mm-string-to-minutes
  'org-duration-to-minutes "9.1")

(define-obsolete-function-alias 'org-duration-string-to-minutes
  'org-duration-to-minutes "9.1")

(make-obsolete-variable 'org-time-clocksum-format
                        "set `org-duration-format' instead." "9.1")

(make-obsolete-variable 'org-time-clocksum-use-fractional
                        "set `org-duration-format' instead." "9.1")

(make-obsolete-variable 'org-time-clocksum-fractional-format
                        "set `org-duration-format' instead." "9.1")

(make-obsolete-variable 'org-time-clocksum-use-effort-durations
                        "set `org-duration-units' instead." "9.1")

(define-obsolete-function-alias 'org-babel-number-p
  'org-babel--string-to-number "9.0")

(define-obsolete-variable-alias 'org-usenet-links-prefer-google
  'org-gnus-prefer-web-links "9.1")

(define-obsolete-variable-alias 'org-texinfo-def-table-markup
  'org-texinfo-table-default-markup "9.1")

(define-obsolete-variable-alias 'org-agenda-overriding-columns-format
  'org-overriding-columns-format "9.2.2")

(define-obsolete-variable-alias 'org-doi-server-url
  'org-link-doi-server-url "9.3")

(define-obsolete-variable-alias 'org-email-link-description-format
  'org-link-email-description-format "9.3")

(define-obsolete-variable-alias 'org-make-link-description-function
  'org-link-make-description-function "9.3")

(define-obsolete-variable-alias 'org-from-is-user-regexp
  'org-link-from-user-regexp "9.3")

(define-obsolete-variable-alias 'org-descriptive-links
  'org-link-descriptive "9.3")

(define-obsolete-variable-alias 'org-context-in-file-links
  'org-link-context-for-files "9.3")

(define-obsolete-variable-alias 'org-keep-stored-link-after-insertion
  'org-link-keep-stored-after-insertion "9.3")

(define-obsolete-variable-alias 'org-display-internal-link-with-indirect-buffer
  'org-link-use-indirect-buffer-for-internals "9.3")

(define-obsolete-variable-alias 'org-confirm-shell-link-function
  'org-link-shell-confirm-function "9.3")

(define-obsolete-variable-alias 'org-confirm-shell-link-not-regexp
  'org-link-shell-skip-confirm-regexp "9.3")

(define-obsolete-variable-alias 'org-confirm-elisp-link-function
  'org-link-elisp-confirm-function "9.3")

(define-obsolete-variable-alias 'org-confirm-elisp-link-not-regexp
  'org-link-elisp-skip-confirm-regexp "9.3")

(define-obsolete-function-alias 'org-file-complete-link
  'org-link-complete-file "9.3")

(define-obsolete-function-alias 'org-email-link-description
  'org-link-email-description "9.3")

(define-obsolete-function-alias 'org-make-link-string
  'org-link-make-string "9.3")

(define-obsolete-function-alias 'org-store-link-props
  'org-link-store-props "9.3")

(define-obsolete-function-alias 'org-add-link-props
  'org-link-add-props "9.3")

(define-obsolete-function-alias 'org-make-org-heading-search-string
  'org-link-heading-search-string "9.3")

(define-obsolete-function-alias 'org-make-link-regexps
  'org-link-make-regexps "9.3")

(define-obsolete-function-alias 'org-property-global-value
  'org-property-global-or-keyword-value "9.3")

(make-obsolete-variable 'org-file-properties 'org-keyword-properties "9.3")

(define-obsolete-variable-alias 'org-angle-link-re
  'org-link-angle-re "9.3")

(define-obsolete-variable-alias 'org-plain-link-re
  'org-link-plain-re "9.3")

(define-obsolete-variable-alias 'org-bracket-link-regexp
  'org-link-bracket-re "9.3")

(define-obsolete-variable-alias 'org-bracket-link-analytic-regexp
  'org-link-bracket-re "9.3")

(define-obsolete-variable-alias 'org-any-link-re
  'org-link-any-re "9.3")

(define-obsolete-function-alias 'org-open-link-from-string
  'org-link-open-from-string "9.3")

(define-obsolete-function-alias 'org-add-angle-brackets
  'org-link-add-angle-brackets "9.3")

;; The function was made obsolete by commit 65399674d5 of 2013-02-22.
;; This make-obsolete call was added 2016-09-01.
(make-obsolete 'org-capture-import-remember-templates
	       "use the `org-capture-templates' variable instead."
	       "9.0")

(defun org-show-block-all ()
  "Unfold all blocks in the current buffer."
  (interactive)
  (org-fold-show-all '(blocks)))

(make-obsolete 'org-show-block-all
	       "use `org-show-all' instead."
	       "9.2")

(define-obsolete-function-alias 'org-get-tags-at 'org-get-tags "9.2")

(defun org-get-local-tags ()
  "Get a list of tags defined in the current headline."
  (declare (obsolete "use `org-get-tags' instead." "9.2"))
  (org-get-tags nil 'local))

(defun org-get-local-tags-at (&optional pos)
  "Get a list of tags defined in the current headline."
  (declare (obsolete "use `org-get-tags' instead." "9.2"))
  (org-get-tags pos 'local))

(defun org-get-tags-string ()
  "Get the TAGS string in the current headline."
  (declare (obsolete "use `org-make-tag-string' instead." "9.2"))
  (org-make-tag-string (org-get-tags nil t)))

(define-obsolete-function-alias 'org-set-tags-to 'org-set-tags "9.2")

(defun org-align-all-tags ()
  "Align the tags in all headings."
  (declare (obsolete "use `org-align-tags' instead." "9.2"))
  (org-align-tags t))

(define-obsolete-function-alias
  'org-at-property-block-p 'org-at-property-drawer-p "9.4")

(defun org-flag-drawer (flag &optional element beg end)
  "When FLAG is non-nil, hide the drawer we are at.
Otherwise make it visible.

When optional argument ELEMENT is a parsed drawer, as returned by
`org-element-at-point', hide or show that drawer instead.

When buffer positions BEG and END are provided, hide or show that
region as a drawer without further ado."
  (declare (obsolete "use `org-hide-drawer-toggle' instead." "9.4"))
  (if (and beg end) (org-fold-region beg end flag 'drawer)
    (let ((drawer
	   (or element
	       (and (save-excursion
		      (forward-line 0)
		      (looking-at-p "^[ \t]*:\\(\\(?:\\w\\|[-_]\\)+\\):[ \t]*$"))
		    (org-element-at-point)))))
      (when (org-element-type-p drawer '(drawer property-drawer))
	(let ((post (org-element-post-affiliated drawer)))
	  (org-fold-region
	   (save-excursion (goto-char post) (line-end-position))
	   (save-excursion (goto-char (org-element-end drawer))
			   (skip-chars-backward " \t\n")
			   (line-end-position))
	   flag 'drawer)
	  ;; When the drawer is hidden away, make sure point lies in
	  ;; a visible part of the buffer.
	  (when (invisible-p (max (1- (point)) (point-min)))
	    (goto-char post)))))))

(defun org-hide-block-toggle-maybe ()
  "Toggle visibility of block at point.
Unlike to `org-hide-block-toggle', this function does not throw
an error.  Return a non-nil value when toggling is successful."
  (declare (obsolete "use `org-hide-block-toggle' instead." "9.4"))
  (interactive)
  (org-fold-hide-block-toggle nil t))

(defun org-hide-block-toggle-all ()
  "Toggle the visibility of all blocks in the current buffer."
  (declare (obsolete "please notify Org mailing list if you use this function."
		     "9.4"))
  (let ((start (point-min))
        (end (point-max)))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end)
		  (re-search-forward "^[ \t]*#\\+begin_?\
\\([^ \n]+\\)\\(\\([^\n]+\\)\\)?\n\\(\\(?:.\\|\n\\)+?\\)#\\+end_?\\1[ \t]*$" end t))
	(save-excursion
	  (save-match-data
            (goto-char (match-beginning 0))
            (org-fold-hide-block-toggle)))))))

(defun org-return-indent ()
  "Goto next table row or insert a newline and indent.
Calls `org-table-next-row' or `newline-and-indent', depending on
context.  See the individual commands for more information."
  (declare (obsolete "use `org-return' with INDENT set to t instead."
		     "9.4"))
  (interactive)
  (org-return t))

(defmacro org-with-silent-modifications (&rest body)
  (declare (obsolete "use `with-silent-modifications' instead." "9.2")
	   (debug (body)))
  `(with-silent-modifications ,@body))

(define-obsolete-function-alias 'org-babel-strip-quotes
  'org-strip-quotes "9.2")

(define-obsolete-variable-alias 'org-sort-agenda-notime-is-late
  'org-agenda-sort-notime-is-late "9.4")

(define-obsolete-variable-alias 'org-sort-agenda-noeffort-is-high
  'org-agenda-sort-noeffort-is-high "9.4")

(defconst org-maybe-keyword-time-regexp
  (concat "\\(\\<\\(\\(?:CLO\\(?:CK\\|SED\\)\\|DEADLINE\\|SCHEDULED\\):\\)\\)?"
	  " *\\([[<][0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [^]\r\n>]*[]>]"
	  "\\|"
	  "<%%([^\r\n>]*>\\)")
  "Matches a timestamp, possibly preceded by a keyword.")
(make-obsolete-variable
 'org-maybe-keyword-time-regexp
 "use `org-planning-line-re', followed by `org-ts-regexp-both' instead."
 "9.4")

(define-obsolete-function-alias 'org-copy 'org-refile-copy "9.4")

(define-obsolete-function-alias 'org-get-last-sibling 'org-get-previous-sibling "9.4")

(define-obsolete-function-alias 'org-publish-cache-ctime-of-src
  'org-publish-cache-mtime-of-src "9.6")

(define-obsolete-function-alias 'org-truely-invisible-p
  'org-truly-invisible-p "9.6"
  "Compatibility alias for legacy misspelling of `org-truly-invisible-p'.")


(defconst org-latex-babel-language-alist
  '(("af" . "afrikaans")
    ("bg" . "bulgarian")
    ("ca" . "catalan")
    ("cs" . "czech")
    ("cy" . "welsh")
    ("da" . "danish")
    ("de" . "germanb")
    ("de-at" . "naustrian")
    ("de-de" . "ngerman")
    ("el" . "greek")
    ("en" . "english")
    ("en-au" . "australian")
    ("en-ca" . "canadian")
    ("en-gb" . "british")
    ("en-ie" . "irish")
    ("en-nz" . "newzealand")
    ("en-us" . "american")
    ("es" . "spanish")
    ("et" . "estonian")
    ("eu" . "basque")
    ("fi" . "finnish")
    ("fr" . "french")
    ("fr-ca" . "canadien")
    ("gl" . "galician")
    ("hr" . "croatian")
    ("hu" . "hungarian")
    ("id" . "indonesian")
    ("is" . "icelandic")
    ("it" . "italian")
    ("la" . "latin")
    ("ms" . "malay")
    ("nl" . "dutch")
    ("nb" . "norsk")
    ("nn" . "nynorsk")
    ("no" . "norsk")
    ("pl" . "polish")
    ("pt" . "portuguese")
    ("pt-br" . "brazilian")
    ("ro" . "romanian")
    ("ru" . "russian")
    ("sa" . "sanskrit")
    ("sb" . "uppersorbian")
    ("sk" . "slovak")
    ("sl" . "slovene")
    ("sq" . "albanian")
    ("sr" . "serbian")
    ("sv" . "swedish")
    ("ta" . "tamil")
    ("tr" . "turkish")
    ("uk" . "ukrainian"))
  "Alist between language code and corresponding Babel option.")

(defconst org-latex-polyglossia-language-alist
  '(("am" "amharic")
    ("ar" "arabic")
    ("ast" "asturian")
    ("bg" "bulgarian")
    ("bn" "bengali")
    ("bo" "tibetan")
    ("br" "breton")
    ("ca" "catalan")
    ("cop" "coptic")
    ("cs" "czech")
    ("cy" "welsh")
    ("da" "danish")
    ("de" "german" "german")
    ("de-at" "german" "austrian")
    ("de-de" "german" "german")
    ("dsb" "lsorbian")
    ("dv" "divehi")
    ("el" "greek")
    ("en" "english" "usmax")
    ("en-au" "english" "australian")
    ("en-gb" "english" "uk")
    ("en-nz" "english" "newzealand")
    ("en-us" "english" "usmax")
    ("eo" "esperanto")
    ("es" "spanish")
    ("et" "estonian")
    ("eu" "basque")
    ("fa" "farsi")
    ("fi" "finnish")
    ("fr" "french")
    ("fu" "friulan")
    ("ga" "irish")
    ("gd" "scottish")
    ("gl" "galician")
    ("he" "hebrew")
    ("hi" "hindi")
    ("hr" "croatian")
    ("hsb" "usorbian")
    ("hu" "magyar")
    ("hy" "armenian")
    ("ia" "interlingua")
    ("id" "bahasai")
    ("is" "icelandic")
    ("it" "italian")
    ("kn" "kannada")
    ("la" "latin" "modern")
    ("la-classic" "latin" "classic")
    ("la-medieval" "latin" "medieval")
    ("la-modern" "latin" "modern")
    ("lo" "lao")
    ("lt" "lithuanian")
    ("lv" "latvian")
    ("ml" "malayalam")
    ("mr" "maranthi")
    ("nb" "norsk")
    ("nko" "nko")
    ("nl" "dutch")
    ("nn" "nynorsk")
    ("no" "norsk")
    ("oc" "occitan")
    ("pl" "polish")
    ("pms" "piedmontese")
    ("pt" "portuges")
    ("pt-br" "brazilian")
    ("rm" "romansh")
    ("ro" "romanian")
    ("ru" "russian")
    ("sa" "sanskrit")
    ("se" "samin")
    ("sk" "slovak")
    ("sl" "slovenian")
    ("sq" "albanian")
    ("sr" "serbian")
    ("sv" "swedish")
    ("syr" "syriac")
    ("ta" "tamil")
    ("te" "telugu")
    ("th" "thai")
    ("tk" "turkmen")
    ("tr" "turkish")
    ("uk" "ukrainian")
    ("ur" "urdu")
    ("vi" "vietnamese"))
  "Alist between language code and corresponding Polyglossia option.")

(make-obsolete-variable 'org-latex-babel-language-alist
                        "set `org-latex-language-alist' instead." "9.6")

(make-obsolete-variable 'org-latex-polyglossia-language-alist
                        "set `org-latex-language-alist' instead." "9.6")

(defconst org-babel-python-mode 'python
  "Python mode for use in running python interactively.")

(make-obsolete-variable
 'org-babel-python-mode
 "Only the built-in Python mode is supported in ob-python now."
 "9.7")

(define-obsolete-function-alias 'ob-clojure-eval-with-babashka
  #'ob-clojure-eval-with-cmd "9.7")

(define-obsolete-function-alias 'org-export-get-parent
  'org-element-parent "9.7")
(define-obsolete-function-alias 'org-export-get-parent-element
  'org-element-parent-element "9.7")

(define-obsolete-function-alias 'org-print-speed-command
  'org--print-speed-command "9.7"
  "Internal function.  Subject of unannounced changes.")

;;;; Obsolete link types

(eval-after-load 'ol
  '(progn
     (org-link-set-parameters "file+emacs") ;since Org 9.0
     (org-link-set-parameters "file+sys"))) ;since Org 9.0





;;; Miscellaneous functions

(defun org-get-x-clipboard (value)
  "Get the value of the X or Windows clipboard."
  (cond ((and (eq window-system 'x)
              (fboundp 'gui-get-selection)) ;Silence byte-compiler.
         (org-no-properties
          (ignore-errors
            (or (gui-get-selection value 'UTF8_STRING)
                (gui-get-selection value 'COMPOUND_TEXT)
                (gui-get-selection value 'STRING)
                (gui-get-selection value 'TEXT)))))
        ((and (eq window-system 'w32) (fboundp 'w32-get-clipboard-data))
         (w32-get-clipboard-data))))


;;; Region compatibility

(defvar org-ignore-region nil
  "Non-nil means temporarily disable the active region.")

(defun org-region-active-p ()
  "Non-nil when the region active.
Unlike to `use-region-p', this function also checks
`org-ignore-region'."
  (and (not org-ignore-region) (use-region-p)))

(defun org-cursor-to-region-beginning ()
  (when (and (org-region-active-p)
             (> (point) (region-beginning)))
    (exchange-point-and-mark)))


;;; Invisibility compatibility

(defun org-in-invisibility-spec-p (arg)
  "Is ARG a member of `buffer-invisibility-spec'?"
  (when (consp buffer-invisibility-spec)
    (member arg buffer-invisibility-spec)))

(defun org-move-to-column (column &optional force _buffer)
  "Move to column COLUMN.
Pass COLUMN and FORCE to `move-to-column'."
  (let ((buffer-invisibility-spec
         (if (listp buffer-invisibility-spec)
             (remove '(org-filtered) buffer-invisibility-spec)
           buffer-invisibility-spec)))
    (move-to-column column force)))

(defmacro org-find-library-dir (library)
  `(file-name-directory (or (locate-library ,library) "")))

(defun org-count-lines (s)
  "How many lines in string S?"
  (let ((start 0) (n 1))
    (while (string-match "\n" s start)
      (setq start (match-end 0) n (1+ n)))
    (when (and (> (length s) 0) (= (aref s (1- (length s))) ?\n))
      (setq n (1- n)))
    n))

(defun org-kill-new (string &rest args)
  (remove-text-properties 0 (length string) '(line-prefix t wrap-prefix t)
                          string)
  (apply 'kill-new string args))

;; `file-local-name' was added in Emacs 26.1.
(defalias 'org-babel-local-file-name
  (if (fboundp 'file-local-name)
      'file-local-name
    (lambda (file)
      "Return the local name component of FILE."
      (or (file-remote-p file 'localname) file))))

;;;###autoload
(defmacro org-check-version ()
  "Try very hard to provide sensible version strings."
  (let* ((org-dir        (org-find-library-dir "org"))
         (org-version.el (concat org-dir "org-version.el"))
         (org-fixup.el   (concat org-dir "../mk/org-fixup.el")))
    (if (require 'org-version org-version.el 'noerror)
        '(progn
           (autoload 'org-release     "org-version.el")
           (autoload 'org-git-version "org-version.el"))
      (if (require 'org-fixup org-fixup.el 'noerror)
          '(org-fixup)
        ;; provide fallback definitions and complain
        (warn "Could not define org version correctly.  Check installation!")
        '(progn
           (defun org-release () "N/A")
           (defun org-git-version () "N/A !!check installation!!"))))))

(define-obsolete-function-alias 'org-define-error #'define-error "9.6")
(define-obsolete-function-alias 'org-without-partial-completion 'progn "9.6")


;;; Integration with and fixes for other packages

(defgroup org-imenu-and-speedbar nil
  "Options concerning imenu and speedbar in Org mode."
  :tag "Org Imenu and Speedbar"
  :group 'org-structure)

(defcustom org-imenu-depth 2
  "The maximum level for Imenu access to Org headlines.
This also applied for speedbar access."
  :type 'integer)

;;;; Imenu

(defvar-local org-imenu-markers nil
  "All markers currently used by Imenu.")

(defun org-imenu-get-tree ()
  "Produce the index for Imenu."
  (dolist (x org-imenu-markers) (move-marker x nil))
  (setq org-imenu-markers nil)
  (org-with-wide-buffer
   (goto-char (point-max))
   (let* ((re (concat "^" (org-get-limited-outline-regexp)))
	  (subs (make-vector (1+ org-imenu-depth) nil))
	  (last-level 0))
     (while (re-search-backward re nil t)
       (let ((level (org-reduced-level (funcall outline-level)))
	     (headline (org-no-properties
			(org-link-display-format (org-get-heading t t t t)))))
	 (when (and (<= level org-imenu-depth) (org-string-nw-p headline))
	   (let* ((m (point-marker))
		  (item (propertize headline 'org-imenu-marker m 'org-imenu t)))
	     (push m org-imenu-markers)
	     (if (>= level last-level)
		 (push (cons item m) (aref subs level))
	       (push (cons item
			   (cl-mapcan #'identity (cl-subseq subs (1+ level))))
		     (aref subs level))
	       (cl-loop for i from (1+ level) to org-imenu-depth
			do (aset subs i nil)))
	     (setq last-level level)))))
     (aref subs 1))))

(eval-after-load 'imenu
  '(progn
     (add-hook 'imenu-after-jump-hook
	       (lambda ()
		 (when (derived-mode-p 'org-mode)
		   (org-fold-show-context 'org-goto))))
     (add-hook 'org-mode-hook
	       (lambda ()
		 (setq imenu-create-index-function 'org-imenu-get-tree)))))

;;;; Speedbar

(defvar org-speedbar-restriction-lock-overlay (make-overlay 1 1)
  "Overlay marking the agenda restriction line in speedbar.")
(overlay-put org-speedbar-restriction-lock-overlay
	     'face 'org-agenda-restriction-lock)
(overlay-put org-speedbar-restriction-lock-overlay
	     'help-echo "Agendas are currently limited to this item.")
(delete-overlay org-speedbar-restriction-lock-overlay)

(defun org-speedbar-set-agenda-restriction ()
  "Restrict future agenda commands to the location at point in speedbar.
If there is already a restriction lock at the location, remove it.

To get rid of the restriction, use `\\[org-agenda-remove-restriction-lock]'."
  (interactive)
  (require 'org-agenda)
  (let (p m tp np dir txt)
    (cond
     ((setq p (text-property-any (line-beginning-position) (line-end-position)
				 'org-imenu t))
      (setq m (get-text-property p 'org-imenu-marker))
      (with-current-buffer (marker-buffer m)
	(goto-char m)
	(if (and org-agenda-overriding-restriction
		 (member org-agenda-restriction-lock-overlay
			 (overlays-at (point))))
	    (org-agenda-remove-restriction-lock 'noupdate)
	  (org-agenda-set-restriction-lock 'subtree))))
     ((setq p (text-property-any (line-beginning-position) (line-end-position)
				 'speedbar-function 'speedbar-find-file))
      (setq tp (previous-single-property-change
		(1+ p) 'speedbar-function)
	    np (next-single-property-change
		tp 'speedbar-function)
	    dir (speedbar-line-directory)
	    txt (buffer-substring-no-properties (or tp (point-min))
						(or np (point-max))))
      (with-current-buffer (find-file-noselect
			    (let ((default-directory dir))
			      (expand-file-name txt)))
	(unless (derived-mode-p 'org-mode)
	  (user-error "Cannot restrict to non-Org mode file"))
	(org-agenda-set-restriction-lock 'file)))
     (t (user-error "Don't know how to restrict Org mode agenda")))
    (move-overlay org-speedbar-restriction-lock-overlay
                  (line-beginning-position) (line-end-position))
    (setq current-prefix-arg nil)
    (org-agenda-maybe-redo)))

(defvar speedbar-file-key-map)
(declare-function speedbar-add-supported-extension "speedbar" (extension))
(eval-after-load 'speedbar
  '(progn
     (speedbar-add-supported-extension ".org")
     (define-key speedbar-file-key-map "<" 'org-speedbar-set-agenda-restriction)
     (define-key speedbar-file-key-map "\C-c\C-x<" 'org-speedbar-set-agenda-restriction)
     (define-key speedbar-file-key-map ">" 'org-agenda-remove-restriction-lock)
     (define-key speedbar-file-key-map "\C-c\C-x>" 'org-agenda-remove-restriction-lock)
     (add-hook 'speedbar-visiting-tag-hook
	       (lambda () (and (derived-mode-p 'org-mode) (org-fold-show-context 'org-goto))))))

;;;; Add Log

(defun org-add-log-current-headline ()
  "Return current headline or nil.
This function ignores inlinetasks.  It is meant to be used as
`add-log-current-defun-function' value."
  (org-with-limited-levels (org-get-heading t t t t)))

;;;; Flyspell

(defun org--flyspell-object-check-p (element)
  "Non-nil when Flyspell can check object at point.
ELEMENT is the element at point."
  (let ((object (save-excursion
		  (when (looking-at-p "\\>") (backward-char))
		  (org-element-context element))))
    (cl-case (org-element-type object)
      ;; Prevent checks in links due to keybinding conflict with
      ;; Flyspell.
      ((citation citation-reference code entity export-snippet inline-babel-call
	         inline-src-block line-break latex-fragment link macro
	         statistics-cookie target timestamp verbatim)
       nil)
      (footnote-reference
       ;; Only in inline footnotes, within the definition.
       (and (eq (org-element-property :type object) 'inline)
	    (< (save-excursion
		 (goto-char (org-element-begin object))
		 (search-forward ":" nil t 2))
	       (point))))
      (otherwise t))))

(defun org-mode-flyspell-verify ()
  "Function used for `flyspell-generic-check-word-predicate'."
  (if (org-at-heading-p)
      ;; At a headline or an inlinetask, check title only.
      (and (save-excursion (forward-line 0)
			   (and (let ((case-fold-search t))
				  (not (looking-at-p "\\*+ END[ \t]*$")))
				(let ((case-fold-search nil))
				  (looking-at org-complex-heading-regexp))))
	   (match-beginning 4)
	   (>= (point) (match-beginning 4))
	   (or (not (match-beginning 5))
	       (< (point) (match-beginning 5)))
           ;; Ignore checks in code, verbatim and others.
           (org--flyspell-object-check-p (org-element-at-point-no-context)))
    (let* ((element (org-element-at-point-no-context))
	   (post-affiliated (org-element-post-affiliated element)))
      (cond
       ;; Ignore checks in all affiliated keywords but captions.
       ((< (point) post-affiliated)
	(and (save-excursion
	       (forward-line 0)
	       (let ((case-fold-search t)) (looking-at "[ \t]*#\\+CAPTION:")))
	     (> (point) (match-end 0))
	     (org--flyspell-object-check-p element)))
       ;; Ignore checks in LOGBOOK (or equivalent) drawer.
       ((let ((log (org-log-into-drawer)))
	  (and log
	       (let ((drawer (org-element-lineage element 'drawer)))
		 (and drawer
		      (org-string-equal-ignore-case
		       log (org-element-property :drawer-name drawer))))))
	nil)
       (t
	(cl-case (org-element-type element)
	  ((comment quote-section) t)
	  (comment-block
	   ;; Allow checks between block markers, not on them.
	   (and (> (line-beginning-position) post-affiliated)
		(save-excursion
		  (end-of-line)
		  (skip-chars-forward " \r\t\n")
		  (< (point) (org-element-end element)))))
	  ;; Arbitrary list of keywords where checks are meaningful.
	  ;; Make sure point is on the value part of the element.
	  (keyword
	   (and (member (org-element-property :key element)
			'("DESCRIPTION" "TITLE"))
		(save-excursion
		  (search-backward ":" (line-beginning-position) t))))
	  ;; Check is globally allowed in paragraphs verse blocks and
	  ;; table rows (after affiliated keywords) but some objects
	  ;; must not be affected.
	  ((paragraph table-row verse-block)
	   (let ((cbeg (org-element-contents-begin element))
		 (cend (org-element-contents-end element)))
	     (and cbeg (>= (point) cbeg) (< (point) cend)
		  (org--flyspell-object-check-p element))))))))))
(put 'org-mode 'flyspell-mode-predicate 'org-mode-flyspell-verify)

(defun org-remove-flyspell-overlays-in (beg end)
  "Remove flyspell overlays in region."
  (and (bound-and-true-p flyspell-mode)
       (fboundp 'flyspell-delete-region-overlays)
       (flyspell-delete-region-overlays beg end)))

(defvar flyspell-delayed-commands)
(eval-after-load 'flyspell
  '(add-to-list 'flyspell-delayed-commands 'org-self-insert-command))

;;;; Bookmark

(defun org-bookmark-jump-unhide (&rest _)
  "Unhide the current position, to show the bookmark location."
  (and (derived-mode-p 'org-mode)
       (or (org-invisible-p)
	   (save-excursion (goto-char (max (point-min) (1- (point))))
			   (org-invisible-p)))
       (org-fold-show-context 'bookmark-jump)))

;; Make `bookmark-jump' shows the jump location if it was hidden.
(add-hook 'bookmark-after-jump-hook #'org-bookmark-jump-unhide)

;;;; Calendar

(defcustom org-calendar-to-agenda-key 'default
  "Key to be installed in `calendar-mode-map' for switching to the agenda.

The command `org-calendar-goto-agenda' will be bound to this key.

When set to `default', bind the function to `c', but only if it is
available in the Calendar keymap.  This is the default choice because
`c' can then be used to switch back and forth between agenda and calendar.

When nil, `org-calendar-goto-agenda' is not bound to any key."
  :group 'org-agenda
  :type '(choice
	  (const :tag "Bind to `c' if available" default)
	  (key-sequence :tag "Other binding")
	  (const :tag "No binding" nil))
  :safe (lambda (v) (or (symbolp v) (stringp v)))
  :package-version '(Org . "9.2"))

(defcustom org-calendar-insert-diary-entry-key [?i]
  "The key to be installed in `calendar-mode-map' for adding diary entries.
This option is irrelevant until `org-agenda-diary-file' has been configured
to point to an Org file.  When that is the case, the command
`org-agenda-diary-entry' will be bound to the key given here, by default
`i'.  In the calendar, `i' normally adds entries to `diary-file'.  So
if you want to continue doing this, you need to change this to a different
key."
  :group 'org-agenda
  :type 'sexp)

(defun org--setup-calendar-bindings ()
  "Bind Org functions in Calendar keymap."
  (pcase org-calendar-to-agenda-key
    (`nil nil)
    ((and key (pred stringp))
     (local-set-key (kbd key) #'org-calendar-goto-agenda))
    ((guard (not (lookup-key calendar-mode-map "c")))
     (local-set-key "c" #'org-calendar-goto-agenda))
    (_ nil))
  (when (and (boundp 'org-agenda-diary-file)
	     (not (eq org-agenda-diary-file 'diary-file)))
    (local-set-key org-calendar-insert-diary-entry-key
		   #'org-agenda-diary-entry)))

(eval-after-load 'calendar
  '(add-hook 'calendar-mode-hook #'org--setup-calendar-bindings))

;;;; Saveplace

;; Make sure saveplace shows the location if it was hidden
(advice-add 'save-place-find-file-hook :after #'org-bookmark-jump-unhide)

;;;; Ecb

;; Make sure ecb shows the location if it was hidden
(advice-add 'ecb-method-clicked :after #'org--ecb-show-context)
(defun org--ecb-show-context (&rest _)
  "Make hierarchy visible when jumping into location from ECB tree buffer."
  (when (derived-mode-p 'org-mode)
    (org-fold-show-context)))

;;;; Simple

(defun org-mark-jump-unhide (&rest _)
  "Make the point visible with `org-show-context' after jumping to the mark."
  (when (and (derived-mode-p 'org-mode)
	     (org-invisible-p))
    (org-fold-show-context 'mark-goto)))

(advice-add 'pop-to-mark-command :after #'org-mark-jump-unhide)

(advice-add 'exchange-point-and-mark :after #'org-mark-jump-unhide)
(advice-add 'pop-global-mark :after #'org-mark-jump-unhide)

;;;; Session

;; Make "session.el" ignore our circular variable.
(defvar session-globals-exclude)
(eval-after-load 'session
  '(add-to-list 'session-globals-exclude 'org-mark-ring))

;;;; outline-mode

;; Folding in outline-mode is not compatible with org-mode folding
;; anymore. Working around to avoid breakage of external packages
;; assuming the compatibility.
(define-advice outline-flag-region (:around (oldfun from to flag &rest extra) fix-for-org-fold)
  "Run `org-fold-region' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (org-fold-region (max from (point-min)) (min to (point-max)) flag 'headline)
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun from to flag extra)))

(define-advice outline-next-visible-heading (:around (oldfun arg &rest extra) fix-for-org-fold)
  "Run `org-next-visible-heading' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (org-next-visible-heading arg)
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun arg extra)))

(define-advice outline-back-to-heading (:around (oldfun &optional invisible-ok &rest extra) fix-for-org-fold)
  "Run `org-back-to-heading' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (progn
        (forward-line 0)
        (or (org-at-heading-p (not invisible-ok))
            (let (found)
	      (save-excursion
	        (while (not found)
	          (or (re-search-backward (concat "^\\(?:" outline-regexp "\\)")
				          nil t)
                      (signal 'outline-before-first-heading nil))
	          (setq found (and (or invisible-ok (not (org-fold-folded-p)))
			           (point)))))
	      (goto-char found)
	      found)))
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun invisible-ok extra)))

(define-advice outline-on-heading-p (:around (oldfun &optional invisible-ok &rest extra) fix-for-org-fold)
  "Run `org-at-heading-p' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (org-at-heading-p (not invisible-ok))
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun invisible-ok extra)))

(define-advice outline-hide-sublevels (:around (oldfun levels &rest extra) fix-for-org-fold)
  "Run `org-fold-hide-sublevels' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (org-fold-hide-sublevels levels)
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun levels extra)))

(define-advice outline-toggle-children (:around (oldfun &rest extra) fix-for-org-fold)
  "Run `org-fold-hide-sublevels' when in org-mode."
  (if (derived-mode-p 'org-mode)
      (save-excursion
        (org-back-to-heading)
        (if (not (org-fold-folded-p (line-end-position)))
            (org-fold-hide-subtree)
          (org-fold-show-children)
          (org-fold-show-entry 'hide-drawers)))
    ;; Apply EXTRA to avoid breakages if advised function definition
    ;; changes.
    (apply oldfun extra)))

;; TODO: outline-headers-as-kill

;;;; Speed commands

(make-obsolete-variable 'org-speed-commands-user
                        "configure `org-speed-commands' instead." "9.5")
(provide 'org-compat)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-compat.el ends here
