;;; assess.el --- Test support functions -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@russet.org.uk>
;; Maintainer: Phillip Lord <phillip.lord@russet.org.uk>
;; Version: 0.4
;; Package-Requires: ((emacs "24.1")(m-buffer "0.15"))

;; The contents of this file are subject to the GPL License, Version 3.0.

;; Copyright (C) 2015, 2016, Phillip Lord

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides functions to support ert, the Emacs Regression Test
;; framework. It includes:

;;  - a set of predicates for comparing strings, buffers and file contents.
;;  - explainer functions for all predicates giving useful output
;;  - macros for creating many temporary buffers at once, and for restoring the
;;    buffer list.
;;  - methods for testing indentation, by comparison or "round-tripping".
;;  - methods for testing fontification.

;; Assess aims to be a stateless as possible, leaving Emacs unchanged whether
;; the tests succeed or fail, with respect to buffers, open files and so on; this
;; helps to keep tests independent from each other. Violations of this will be
;; considered a bug.

;; Assess aims also to be as noiseless as possible, reducing and suppressing
;; extraneous messages where possible, to leave a clean ert output in batch mode.


;;; Status:

;; Assess is currently a work in progress; the API is not currently stable. I
;; may also considering winding this into ert-x, because then it can be used
;; to test core.

;; Assess used to be called sisyphus which seemed like a good idea when I
;; started, but I kept spelling it wrong.


;;; Code:

;; ** Preliminaries

;; #+begin_src emacs-lisp
(require 'pp)
(require 'ert)
(require 'm-buffer-at)
(require 'm-buffer)
(require 'seq)
;; #+end_src

;; ** Advice

;; Emacs-24 insists on printing out results on a single line with escaped
;; newlines. This does not work so well with the explainer functions in assess
;; and, probably, does not make sense anywhere. So, we advice here. The use of
;; nadvice.el limits this package to Emacs 24.4. Emacs 25 has this fixed.

;; #+begin_src emacs-lisp
(when (fboundp 'advice-add)

  (defun assess--ert-pp-with-indentation-and-newline (orig object)
    (let ((pp-escape-newlines nil))
      (funcall orig object)))

  (advice-add
   'ert--pp-with-indentation-and-newline
   :around
   #'assess--ert-pp-with-indentation-and-newline))
;; #+end_src

;; ** Deliberate Errors

;; Sometimes during testing, we need to throw an "error" deliberately. Assess'
;; own test cases do this to check that state is preserved with this form of
;; non-local exit. Throwing `error' itself is a bit dangerous because we might
;; get that for other reasons; so we create a new symbol here for general use.

;; #+begin_src emacs-lisp
(if (fboundp 'define-error)
    (define-error 'assess-deliberate-error
      "An error deliberately caused during testing."
      'error)
  (put 'assess-deliberate-error
       'error-conditions
       '(error assess-deliberate-error))
  (put 'assess-deliberate-error
       'error-message
       "A error deliberately caused during testing."))
;; #+end_src

;; ** Buffer creation

;; For tests, it is often better to use temporary buffers, as it is much less
;; affected by the existing state of Emacs, and much less likely to affect future
;; state; this is particularly the case where tests are being developed as the
;; developer may be trying to change or write test files at the same time as
;; Emacs is trying to use them for testing.

;; Emacs really only provides a single primitive `with-temp-buffer' for this
;; situation, and that only creates a single temporary buffer at a time. Nesting
;; of these forms sometimes works, but fails if we need to operate on two buffers
;; at once.

;; So, we provide an environment for restoring the buffer list. This allows any
;; creation of buffers we need for testing, followed by clean up afterwards. For
;; example, a trivial usage would be to remove buffers explicitly created.

;; #+begin_src elisp
;;   (assess-with-preserved-buffer-list
;;    (get-buffer-create "a")
;;    (get-buffer-create "b")
;;    (get-buffer-create "c"))
;; #+end_src

;; Any buffer created in this scope is removed, whether this is as a direct or
;; indirect result of the function. For example, this usage creates a ~*Help*~
;; buffer which then gets removed again.

;; #+begin_src elisp
;;   (assess-with-preserved-buffer-list
;;    (describe-function 'self-insert-command))
;; #+end_src

;; This does not prevent changes to existing buffers of course. If ~*Help*~ is
;; already open before evaluation, it will remain open afterwards but with
;; different content.

;; Sometimes, it is useful to create several temporary buffers at once.
;; `assess-with-temp-buffers' provides an easy mechanism for doing this, as
;; well as evaluating content in these buffers. For example, this returns true
;; (actually three killed buffers which were live when the `mapc' form runs).

;; #+begin_src elisp
;;   (assess-with-temp-buffers
;;       (a b c)
;;     (mapc #'buffer-live-p (list a b c)))
;; #+end_src

;; While this creates two buffers, puts "hellogoodbye" into one and "goodbye"
;; into the other, then compares the contents of these buffers with `assess='.

;; #+begin_src elisp
;;   (assess-with-temp-buffers
;;       ((a (insert "hello")
;;           (insert "goodbye"))
;;        (b (insert "goodbye")))
;;     (assess= a b))
;; #+end_src

;; Finally, we provide a simple mechanism for converting any assess type into a
;; buffer. The following form, for example, returns the contents of the ~.emacs~
;; file.

;; #+begin_src elisp
;;   (assess-as-temp-buffer
;;       (assess-file "~/.emacs")
;;     (buffer-string))
;; #+end_src

;; *** Implementation

;; #+begin_src emacs-lisp
(defmacro assess-with-preserved-buffer-list (&rest body)
  "Evaluate BODY, but delete any buffers that have been created."
  (declare (debug t))
  `(let ((before-buffer-list
          (buffer-list)))
     (unwind-protect
         (progn
           ,@body)
       (seq-map
        (lambda (it)
          (with-current-buffer it
            (set-buffer-modified-p nil)
            (kill-buffer)))
        (seq-difference (buffer-list)
                        before-buffer-list)))))

(defun assess--temp-buffer-let-form (item)
  (if (not (listp item))
      (assess--temp-buffer-let-form
       (list item))
    `(,(car item)
      (with-current-buffer
          (generate-new-buffer " *assess-with-temp-buffers*")
        ,@(cdr item)
        (current-buffer)))))
;; #+end_src

;; The implementation of `assess-with-temp-buffers' currently uses
;; `assess-with-preserved-buffer-list' to remove buffers which means that it
;; will also delete any buffers created by the user; this may be a mistake, and
;; it might be better to delete the relevant buffers explicitly.

;; #+begin_src emacs-lisp
(defmacro assess-with-temp-buffers (varlist &rest body)
  "Bind variables in varlist to temp buffers, then eval BODY.

VARLIST is (nearly) of the same form as a `let' binding. Each
element is a symbol or a list (symbol valueforms). Each symbol is
bound to a buffer generated with `generate-new-buffer'.
VALUEFORMS are evaluated with the buffer current. Any buffers
created inside this form (and not just by this form!) are
unconditionally killed at the end of the form.

Unlike `let' there can be multiple valueforms which are,
effectively, placed within an impicit `progn'."
  (declare (indent 1)
           (debug
            ((&rest (symbolp &rest form))
             body)))
  (let ((let-form
         (seq-map
          #'assess--temp-buffer-let-form
          varlist)))
    `(assess-with-preserved-buffer-list
      (let* ,let-form
        ,@body))))

(defmacro assess-as-temp-buffer (x &rest body)
  "Insert X in a type-appropriate way into a temp buffer and eval
BODY there.

See `assess-to-string' for the meaning of type-appropriate."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert (assess-to-string ,x))
     ,@body))
;; #+end_src

;; ** Types

;; Many tests on files or buffers actually end up being string comparison.
;; In many cases, we want to compare the *contents* of a buffer to, for example,
;; the *contents* of a file.

;; Emacs normally uses strings to represent files (i.e. file names) and can also
;; use them to represent buffers (i.e. buffer names). So, here, we introduce a
;; set of "types" where so that we can distinguish between strings, buffer names
;; and file names, passing them at a single parameter. This will allow us to make
;; later parts of the API use overloaded methods based on their type.

;; "Types" are either a Emacs core type (as with buffers and strings), or an 2
;; element list (I haven't used cons cells in case I want to add more elements),
;; with a keyword at the head. This allows assess to distinguish between a
;; simple string and a file or buffer name.

;; #+begin_src elisp
;; ;; Identify "~/.emacs" as a file name
;; (assess-file "~/.emacs")

;; ;; Identify "*Messages*" as a buffer
;; (assess-buffer "*Messages*")
;; #+end_src

;; *** Implementation

;; #+begin_src emacs-lisp
(defun assess-to-string (x)
  "Turn X into a string in a type appropriate way.

If X is identified as a file, returns the file contents.
If X is identified as a buffer, returns the buffer contents.
If X is a string, returns that.

See also `assess-buffer' and `assess-file' which turn a
string into something that will identified appropriately."
  (pcase x
    ((pred stringp) x)
    ((pred bufferp) (m-buffer-at-string x))
    (`(:buffer ,b) (assess-to-string (get-buffer-create b)))
    (`(:file ,f)
     (with-temp-buffer
       (insert-file-contents f)
       (buffer-string)))
    ;; error condition
    (_ (error "Type not recognised"))))

(defun assess-buffer (b)
  "Add type data to the string B marking it as a buffer."
  `(:buffer ,b))

(defun assess-file (f)
  "Add type data to the string F marking it as a file."
  `(:file ,f))

(defun assess-to-file-name (file)
  "Return file name for FILE.

FILE can be either a string, or a plist returned by
`assess-file' or `assess-make-related-file'."
  (pcase file
    ((pred stringp) file)
    (`(:file ,f) f)
    (_ (error "Type not recognised"))))
;; #+end_src

;; ** Entity Comparison

;; In this section, we provide support for comparing strings, buffer or file
;; contents. The main entry point is `assess=', which works like `string=' but
;; on any of the three data types, in any order.

;; #+begin_src elisp
;;   ;; Compare Two Strings
;;   (assess= "hello" "goodbye")

;;   ;; Compare the contents of Two Buffers
;;   (assess=
;;    (assess-buffer "assess.el")
;;    (assess-buffer "assess-previous.el"))

;;   ;; Compare the contents of Two files
;;   (assess=
;;    (assess-file "~/.emacs")
;;    (assess-file "~/.emacs"))

;;   ;; We can use core Emacs types also
;;   (assess=
;;    (assess-buffer "assess.el")
;;    (get-buffer "assess-previous.el"))

;;   ;; And in any combination; here we compare a string and the contents of a
;;   ;; file.
;;   (assess=
;;    ";; This is an empty .emacs file"
;;    (assess-file "~/.emacs"))
;; #+end_src

;; In addition, `assess=' has an "explainer" function attached which produces a
;; richer output when `assess=' returns false, showing diffs of the string
;; comparison. Compare, for example, the results of running these two tests, one
;; using `string=' and one using `assess='.

;; #+BEGIN_EXAMPLE
;; F temp
;;     (ert-test-failed
;;      ((should
;;        (string= "a" "b"))
;;       :form
;;       (string= "a" "b")
;;       :value nil))

;; F test-assess=
;;     (ert-test-failed
;;      ((should
;;        (assess= "a" "b"))
;;       :form
;;       (assess= "a" "b")
;;       :value nil :explanation "Strings:
;; a
;; and
;; b
;; Differ at:*** /tmp/a935uPW	2016-01-20 13:25:47.373076381 +0000
;; --- /tmp/b9357Zc	2016-01-20 13:25:47.437076381 +0000
;;  ***************
;;  *** 1 ****
;; ! a
;; \\ No newline at end of file
;; --- 1 ----
;; ! b
;; \\ No newline at end of file

;; "))
;; #+END_EXAMPLE

;; As `assess=' has a compatible interface with `string=' it is also possible
;; to add this explainer function to `string=' for use with tests which do not
;; otherwise use assess, like so:

;; #+begin_src elisp
;; (put 'string= 'ert-explainer 'assess-explain=)
;; #+end_src

;; Currently, `assess' uses the ~diff~ program to do the comparison if it is
;; available, or falls back to just reporting a difference -- this could do with
;; improving, but it is at least no worse than the existing behaviour for string
;; comparison.

;; *** Implementation

;; We start by writing a file silently -- this is important because the
;; ~*Messages*~ buffer should not be affected by the machinary of a failing test,
;; as it hides what is happening from the test code.

;; #+begin_src emacs-lisp
(defun assess--write-file-silently (filename)
  "Write current buffer into FILENAME.
Unlike most other ways of saving a file, this should not
print any messages!"
  (write-region
   (point-min) (point-max)
   filename nil
   'dont-display-wrote-file-message))
;; #+end_src

;; Diff does a nicer comparison than anything in Emacs, although a lisp
;; implementation would have been more portable. Diff is used by quite a few
;; other tools in Emacs, so probably most people will have access to diff.

;; #+begin_src emacs-lisp
(defun assess--explainer-diff-string= (a b)
  "Compare strings A and B using diff output.

We assume that diff exists. Temporary files are left
afterwards for cleanup by the operating system."
  (assess-with-preserved-buffer-list
   (let* ((diff
           (executable-find "diff"))
          (a-buffer
           (generate-new-buffer "a"))
          (b-buffer
           (generate-new-buffer "b"))
          (a-file
           (make-temp-file
            (buffer-name a-buffer)))
          (b-file
           (make-temp-file
            (buffer-name b-buffer))))
     (with-current-buffer
         a-buffer
       (insert a)
       (assess--write-file-silently a-file))
     (with-current-buffer
         b-buffer
       (insert b)
       (assess--write-file-silently b-file))
     (progn
         (format "Strings:\n%s\nand\n%s\nDiffer at:%s\n"
                 a b
                 (with-temp-buffer
                   (call-process
                    diff
                    ;; no infile
                    nil
                    ;; dump to current buffer
                    t
                    nil
                    "-c"
                    a-file
                    b-file)
                   (buffer-string)))))))

(defun assess--explainer-simple-string= (a b)
  "Compare strings for first difference."
  ;; We could do a bit more here.
  (format "String :%s:%s: are not equal." a b))
;; #+end_src

;; And the actual predicate function and explainer. We do a simple string
;; comparison on the contents of each entity.

;; #+begin_src emacs-lisp
(defun assess= (a b)
  "Compare A and B to see if they are the same.

Equality in this sense means compare the contents in a way which
is appropriate for the type of the two arguments. So, if they are
strings, the compare strings, if buffers, then compare the buffer
contents and so on.

Text properties in strings or buffers are ignored."
  (string=
   (assess-to-string a)
   (assess-to-string b)))

(defun assess-explain= (a b)
  "Compare A and B and return an explanation.

This function is called by ERT as an explainer function
automatically. See `assess=' for more information."
  (let ((a (assess-to-string a))
        (b (assess-to-string b)))
    (cond
     ((assess= a b)
      t)
     ((executable-find "diff")
      (assess--explainer-diff-string= a b))
     (t
      (assess--explainer-simple-string= a b)))))

(put 'assess= 'ert-explainer 'assess-explain=)
;; #+end_src

;; ** Opening files

;; Opening files presents a particular problem for testing, particularly if we
;; open a file that is already open in the same or a different Emacs. For batch
;; use of Emacs with parallelisation, the situation becomes intractable.

;; A solution is to copy files before we open them, which means that they can be
;; changed freely. Largely, the copied file will behave the same as the main file;
;; the only notable exception to this is those features which depend on the
;; current working directory (dir-local variables, for example).

;; ~assess-make-related-file~ provides a simple method for doing this. For
;; example, this form will return exactly the contents of ~my-test-file.el~, even
;; if that file is current open in the current Emacs (even if the buffer has not
;; been saved). Likewise, a test opening this file could be run in a batch Emacs
;; without interfering with an running interactive Emacs.

;; #+begin_src elisp
;;   (assess-as-temp-buffer
;;       (assess-make-related-file "dev-resources/my-test-file.el")
;;     (buffer-substring))
;; #+end_src

;; We also add support for opening a file, as if it where opened interactively,
;; with all the appropriate hooks being run, in the form of the
;; `assess-with-find-file' macro. Combined with `assess-make-related-file',
;; we can write the following expression without removing our ~.emacs~.

;; #+begin_src elisp
;;   (assess-with-find-file
;;       (assess-make-related-file "~/.emacs")
;;     (erase-buffer)
;;     (save-buffer))
;; #+end_src

;; #+RESULTS:

;; *** Implementation

;; All of the functions here support the file type introduced earlier, but
;; interpret raw strings as a file also.

;; #+begin_src emacs-lisp
(defun assess--make-related-file-1 (file &optional directory)
  (make-temp-file
   (concat
    (or directory
        temporary-file-directory)
    (file-name-nondirectory file))
   nil
   (concat "."
           (file-name-extension file))))

(defun assess-make-related-file (file &optional directory)
  "Open a copy of FILE in DIRECTORY.

FILE is copied to a temporary file in DIRECTORY or
`temporary-file-directory'. The copy has a unique name but shares
the same file extension.

This is useful for making test changes to FILE without actually
altering it."
  (let* ((file (assess-to-file-name file))
         (related-file
          (assess--make-related-file-1 file directory)))
    (copy-file file related-file t)
    (assess-file
     related-file)))

(defmacro assess-with-find-file (file &rest body)
  "Open FILE and evaluate BODY in resultant buffer.

FILE is opened with `find-file-noselect' so all the normal hooks
for file opening should occur. The buffer is killed after the
macro exits, unless it was already open. This happens
unconditionally, even if the buffer has changed.

See also `assess-make-related-file'."
  (declare (debug t) (indent 1))
  (let ((temp-buffer (make-symbol "temp-buffer"))
        (file-has-buffer-p (make-symbol "file-has-buffer-p"))
        (file-s (make-symbol "file")))
    `(let* ((,file-s ,file)
            (,file-s (assess-to-file-name ,file-s))
            (,file-has-buffer-p
             (find-buffer-visiting ,file-s))
            (,temp-buffer))
       (unwind-protect
           (with-current-buffer
               (setq ,temp-buffer
                     (find-file-noselect ,file-s))
             ,@body)
         (when
          ;; kill the buffer unless it was already open.
             (and (not ,file-has-buffer-p)
                  (buffer-live-p ,temp-buffer))
           ;; kill unconditionally
           (with-current-buffer ,temp-buffer
             (set-buffer-modified-p nil))
           (kill-buffer ,temp-buffer))))))
;; #+end_src

;; ** Creating Files and Directories
;; I can write some documentation here if Phil wants to merge code below.
;; *** Implementation
;; #+BEGIN_SRC emacs-lisp
(defun assess-with-filesystem--make-parent (spec path)
  "If SPEC is a file name, create its parent directory rooted at PATH."
  (save-match-data
    (when (string-match "\\(.*\\)/" spec)
      (make-directory (concat path "/" (match-string 1 spec)) t))))

(defun assess-with-filesystem--init (spec &optional path)
  "Interpret the SPEC inside PATH."
  (setq path (or path "."))
  (cond
   ((listp spec)
    (cond
     ;; non-empty file
     ((and (stringp (car spec))
           (stringp (cadr spec)))
      (when (string-match-p "/\\'" (car spec))
        (error "Invalid syntax: `%s' - cannot create a directory with text content" (car spec)))
      (assess-with-filesystem--make-parent (car spec) path)
      (with-temp-file (concat path "/" (car spec))
        (insert (cadr spec))))
     ;; directory
     ((and (stringp (car spec))
           (consp (cadr spec)))
      (make-directory (concat path "/" (car spec)) t)
      (mapc (lambda (s) (assess-with-filesystem--init
                    s (concat path "/" (car spec)))) (cadr spec)))
     ;; recursive spec, this should probably never happen
     (t (mapc (lambda (s) (assess-with-filesystem--init s path)) spec))))
   ;; directory specified using a string
   ((and (stringp spec)
         (string-match-p "/\\'" spec))
    (make-directory (concat path "/" spec) t))
   ;; empty file
   ((stringp spec)
    (assess-with-filesystem--make-parent spec path)
    (write-region "" nil (concat path "/" spec) nil 'no-message))
   (t (error "Invalid syntax: `%s'" spec))))

(defmacro assess-with-filesystem (spec &rest forms)
  "Create temporary file hierarchy according to SPEC and run FORMS.

SPEC is a list of specifications for file system entities which
are to be created.

File system entities are specified as follows:

1. a string FILE is the name of file to be created
  - if the string contains \"/\", parent directories are created
    automatically
  - if the string ends with \"/\", a directory is created
2. a list of two elements (FILE CONTENT) specifies filename and the
  content to put in the file
  - the \"/\" rules apply in the same way as in 1., except you can not
    create a directory this way
3. a list where car is a string and cadr is a list (DIR SPEC) is a
  recursive specification evaluated with DIR as current directory
  - the \"/\" rules apply in the same way as in 1., except you can not
    create a file this way, a directory is always created

An example showing all the possibilities:

  (\"empty_file\"
   \"dir/empty_file\"
   \"dir/subdir/\"
   (\"non_empty_file\" \"content\")
   (\"dir/anotherdir/non_empty_file\" \"tralala\")
   (\"big_dir\" (\"empty_file\"
              (\"non_empty_file\" \"content\")
              \"subdir/empty_file\")))

If we want to run some code in a directory with an empty file
\"foo.txt\" present, we call:

  (assess-with-filesystem '(\"foo\")
    (code-here)
    (and-some-more-forms))

You should *not* depend on where exactly the hierarchy is created.
By default, a new directory in `temporary-file-directory' is
created and the specification is evaluated there, but this is up
for change."
  (declare (indent 1))
  (let ((temp-root (make-symbol "temp-root"))
        (old-dd (make-symbol "old-dd")))
    `(let ((,temp-root (make-temp-file "temp-fs-" t))
           (,old-dd default-directory))
       (unwind-protect
           (progn
             (setq default-directory ,temp-root)
             (mapc (lambda (s) (assess-with-filesystem--init s ".")) ,spec)
             ,@forms)
         (delete-directory ,temp-root t)
         (setq default-directory ,old-dd)))))
;; #+END_SRC
;; ** Indentation functions

;; There are two main ways to test indentation -- we can either take unindented
;; text, indent it, and then compare it to something else; or, we can roundtrip
;; -- take indented code, unindent it, re-indent it again and see whether we end
;; up with what we started. Assess supports both of these.

;; Additionally, there are two different ways to specific a mode -- we can either
;; define it explicitly or, if we are opening from a file, we can use the normal
;; `auto-mode-alist' functionality to determine the mode. Assess supports both
;; of these also.

;; The simplest function is `assess-indentation=' which we can use as follows.
;; In this case, we have mixed a multi-line string and a single line with
;; control-n characters; this is partly to show that we can, and partly to make
;; sure that the code works both in an `org-mode' buffer and an ~*Org Src*~ buffer.

;; #+begin_src elisp
;;   (assess-indentation=
;;    'emacs-lisp-mode
;;    "(assess-with-find-file
;;   \"~/.emacs\"
;;   (buffer-string))"
;;    "(assess-with-find-file\n    \"~/.emacs\"\n  (buffer-string))")
;; #+end_src

;; #+RESULTS:
;; : t

;; Probably more useful is `assess-roundtrip-indentation=' which allows us to
;; just specify the indented form; in this case, the string is first unindented
;; (every line starts at the first position) and then reindented. This saves the
;; effort of keeping the text in both the indented and unindent forms in sync
;; (but without the indentation).

;; #+begin_src elisp
;;   (assess-roundtrip-indentation=
;;    'emacs-lisp-mode
;;    "(assess-with-find-file\n    \"~/.emacs\"\n  (buffer-string))")
;; #+end_src

;; #+RESULTS:
;; : t

;; While these are useful for simple forms of indentation checking, they have
;; the significant problem of writing indented code inside an Emacs string. An
;; easier solution for longer pieces of code is to use
;; `assess-file-roundtrip-indentation='. This opens a file (safely using
;; `assess-make-related-file'), unindents, and reindents. The mode must be set
;; up automatically by the file type.

;; #+begin_src elisp
;;   (assess-file-roundtrip-indentation=
;;     "assess.el")
;; #+end_src

;; #+RESULTS:

;; All of these methods are fully supported with ert explainer functions -- as
;; before they use diff where possible to compare the two forms.


;; *** Implementation

;; We start with some functionality for making Emacs quiet while indenting,
;; otherwise we will get a large amount of spam on the command line. Emacs needs
;; to have a better technique for shutting up `message'.

;; #+begin_src emacs-lisp
(defun assess--indent-buffer (&optional column)
  (cond
   (column
    (indent-region (point-min) (point-max) column))
   ;; if indent-region-function is set, use it, and hope that it is not
   ;; noisy.
   (indent-region-function
    (funcall indent-region-function (point-min) (point-max)))
   (t
    (seq-map
     (lambda (m)
       (goto-char m)
       (indent-according-to-mode))
     (m-buffer-match-line-start (current-buffer))))))

(defun assess--indent-in-mode (mode unindented)
  (with-temp-buffer
    (insert
     (assess-to-string unindented))
    (funcall mode)
    (assess--indent-buffer)
    (buffer-string)))
;; #+end_src

;; Now for the basic indentation= comparison.

;; #+begin_src emacs-lisp
(defun assess-indentation= (mode unindented indented)
  "Return non-nil if UNINDENTED indents in MODE to INDENTED.
Both UNINDENTED and INDENTED can be any value usable by
`assess-to-string'. Indentation is performed using
`indent-region', which MODE should set up appropriately.

See also `assess-file-roundtrip-indentation=' for an
alternative mechanism."
  (assess=
   (assess--indent-in-mode
    mode
    unindented)
   indented))

(defun assess-explain-indentation= (mode unindented indented)
  "Explanation function for `assess-indentation='."
  (assess-explain=
   (assess--indent-in-mode
    mode
    unindented)
   indented))

(put 'assess-indentation= 'ert-explainer 'assess-explain-indentation=)
;; #+end_src

;; Roundtripping.

;; #+begin_src emacs-lisp
(defun assess--buffer-unindent (buffer)
  (with-current-buffer
      buffer
    (assess--indent-buffer 0)))

(defun assess--roundtrip-1 (comp mode indented)
  (with-temp-buffer
    (funcall comp
             mode
             (progn
               (insert
                (assess-to-string indented))
               (assess--buffer-unindent (current-buffer))
               (buffer-string))
             indented)))

(defun assess-roundtrip-indentation= (mode indented)
  "Return t if in MODE, text in INDENTED is corrected indented.

This is checked by unindenting the text, then reindenting it according
to MODE.

See also `assess-indentation=' and
`assess-file-roundtrip-indentation=' for alternative
mechanisms of checking indentation."
  (assess--roundtrip-1
   #'assess-indentation=
   mode indented))

(defun assess-explain-roundtrip-indentation= (mode indented)
  "Explanation function for `assess-roundtrip-indentation='."
  (assess--roundtrip-1
   #'assess-explain-indentation=
   mode indented))

(put 'assess-roundtrip-indentation=
     'ert-explainer
     'assess-explain-roundtrip-indentation=)
;; #+end_src

;; And file based checking.

;; #+begin_src emacs-lisp
(defun assess--file-roundtrip-1 (comp file)
  (funcall
   comp
   (assess-with-find-file
       (assess-make-related-file file)
     (assess--buffer-unindent (current-buffer))
     (assess--indent-buffer)
     (buffer-string))
   file))

(defun assess-file-roundtrip-indentation= (file)
  "Return t if text in FILE is indented correctly.

FILE is copied with `assess-make-related-file', so this
function should be side-effect free whether or not FILE is
already open. The file is opened with `find-file-noselect', so
hooks associated with interactive visiting of a file should all
be called, with the exception of directory local variables, as
the copy of FILE will be in a different directory."
  (assess--file-roundtrip-1
   #'assess= file))

(defun assess-explain-file-roundtrip-indentation= (file)
  "Explanation function for `assess-file-roundtrip-indentation=."
  (assess--file-roundtrip-1
   #'assess-explain= file))

(put 'assess-file-roundtrip-indentation=
     'ert-explainer
     'assess-explain-file-roundtrip-indentation=)
;; #+end_src

;; ** Font-Lock

;; Here we define two predicates that can be used to checking
;; fontification/syntax highlighting; as with indentation, one accepts strings
;; but requires an explicit mode, while the other reads from file and depends on
;; the normal Emacs mechanisms for defining the mode. These two are
;; `assess-font-at=' and `assess-file-font-at='. Both of these have the same
;; interface and have attached explainer functions. Here, we show examples with
;; `assess-face-at='.

;; The simplest use is to specify a point location and a face. This returns true
;; if at least that face is present at the location.

;; #+begin_src elisp
;;    (assess-face-at=
;;     "(defun x ())"
;;     'emacs-lisp-mode
;;     2
;;     'font-lock-keyword-face)
;; #+end_src

;; It is also possible to specify several locations in a list, with a single
;; face. This checks that the given font is present at every location.

;; #+begin_src elisp
;;    (assess-face-at=
;;     "(defun x ())
;; (defun y ())
;; (defun z ())"
;;     'emacs-lisp-mode
;;     '(2 15 28)
;;     'font-lock-keyword-face)
;; #+end_src

;; Or, we can specify a list of faces in which case the locations and faces are
;; checked in a pairwise manner.

;; #+begin_src elisp
;;    (assess-face-at=
;;     "(defun x ())"
;;     'emacs-lisp-mode
;;     '(2 8)
;;     '(font-lock-keyword-face font-lock-function-name-face))
;; #+end_src

;; It is also possible to define locations with regexps; again either one or
;; multiple regexps can be used. With a single string, all matches are checked,
;; with the first match to the first is checked, then the next match to the
;; second, incrementally.

;; #+begin_src elisp
;;    (assess-face-at=
;;     "(defun x ())\n(defun y ())\n(defun z ())"
;;     'emacs-lisp-mode
;;     "defun"
;;     'font-lock-keyword-face)

;;    (assess-face-at=
;;     "(defun x ())\n(defmacro y ())\n(defun z ())"
;;     'emacs-lisp-mode
;;     '("defun" "defmacro" "defun")
;;     'font-lock-keyword-face)
;; #+end_src


;; The locations can also be specified as a `lambda' which takes a single
;; argument of a buffer. The return result can be any form of location accepted
;; by `assess-face-at=', including a list of match data generated, as in this
;; case, by the `m-buffer' package.

;; #+begin_src elisp
;;    (assess-face-at=
;;     "(defun x ())\n(defun y ())\n(defun z ())"
;;     'emacs-lisp-mode
;;     (lambda(buf)
;;       (m-buffer-match buf "defun"))
;;     'font-lock-keyword-face)
;; #+end_src


;; *** Implementation

;; First, `assess-face-at='.


;; #+begin_src emacs-lisp
(defun assess--face-at-location=
    (location face property throw-on-nil)
  ;; it's match data
  (if (listp location)
      ;; We need to test every point but not the last because the match is
      ;; passed the end.
      (let ((all nil))
        (cl-loop for i from
                 (marker-position (car location))
                 below
                 (marker-position (cadr location))
                 do
                 (setq all
                       (cons (assess--face-at-location=
                              i face
                              property throw-on-nil)
                             all)))
        (seq-every-p #'identity all))
    (let* ((local-faces
            (get-text-property location property))
           (rtn
            ;; for face this can be one of -- a face name (a symbol or string)
            ;; a list of faces, or a plist of face attributes
            (pcase local-faces
              ;; compare directly
              ((pred symbolp)
               (eq face local-faces))
              ;; give up -- we should probably be able to compare the plists here.
              ((and `(,s . ,_)
                    (guard (keywordp s)))
               nil)
              ;; compare that we have at least this.
              ((and `(,s . ,_)
                    (guard (symbolp s)))
               (member face s)))))
      (if (and throw-on-nil
               (not rtn))
          (throw
           'face-non-match
           (format "Face does not match expected value
\tExpected: %s
\tActual: %s
\tLocation: %s
\tLine Context: %s
\tbol Position: %s
"
                   face local-faces location
                   (thing-at-point 'line)
                   (m-buffer-at-line-beginning-position
                    (current-buffer) location)))
        rtn))))


(defun assess--face-at=
    (buffer locations faces property throw-on-nil)
  (let* (
         ;; default property
         (property (or property 'face))
         ;; make sure we have a list of locations
         (locations
          (pcase locations
            ((pred functionp)
             (funcall locations buffer))
            ((pred listp)
             locations)
            (_ (list locations))))
         (first-location
          (car locations))
         ;; make sure we have a list of markers
         (locations
          (cond
           ((integerp first-location)
            (m-buffer-pos-to-marker buffer locations))
           ((stringp first-location)
            (m-buffer-match-multi locations :buffer buffer))
           ;; markers
           ((markerp first-location)
            locations)
           ;; match data
           ((and (listp first-location)
                 (markerp (car first-location)))
            locations)))
          ;; make sure we have a list of faces
          (faces
           (if (and (listp faces)
                    ;; but not nil
                    (not (eq nil faces)))
               faces
             (list faces)))
          ;; make sure faces is as long as locations
          (faces
           (progn
             (while (> (length locations)
                       (length faces))
               ;; cycle faces if needed
               (setq faces (append faces (seq-copy faces))))
             faces)))
    (seq-every-p
     (lambda (it)
       (assess--face-at-location=
        (car it) (cdr it) property throw-on-nil))
     (seq-mapn #'cons locations faces))))

(defun assess--face-at=-1 (x mode locations faces property throw-on-nil)
  (with-temp-buffer
    (insert (assess-to-string x))
    (funcall mode)
    (font-lock-fontify-buffer)
    (assess--face-at= (current-buffer) locations faces property throw-on-nil)))

(defun assess-face-at=
    (x mode locations faces &optional property)
  "Return non-nil if in X with MODE at MARKERS, FACES are present on PROPERTY.

This function tests if one or more faces are present at specific
locations in some text. It operates over single or multiple
values for both locations and faces; if there are more locations
than faces, then faces will be cycled over. If locations are
match data, then each the beginning and end of each match are
tested against each face.

X can be a buffer, file name or string -- see
`assess-to-string' for details.

MODE is the major mode with which to fontify X -- actually, it
will just be a function called to initialize the buffer.

LOCATIONS can be either one or a list of the following things:
integer positions in X; markers in X (or nil!); match data in X;
or strings which match X. If this is a list, all items in list
should be of the same type.

FACES can be one or more faces.

PROPERTY is the text property on which to check the faces.

See also `assess-to-string' for treatment of the parameter X.

See `assess-file-face-at=' for a similar function which
operates over files and takes the mode from that file."
  (assess--face-at=-1 x mode locations faces property nil))

(defun assess-explain-face-at=
    (x mode locations faces &optional property)
  (catch 'face-non-match
    (assess--face-at=-1 x mode locations faces property t)))

(put 'assess-face-at=
     'ert-explainer
     'assess-explain-face-at=)
;; #+end_src

;; Followed by `assess-file-face-at='.

;; #+begin_src emacs-lisp
(defun assess--file-face-at=-1 (file locations faces property throw-on-nil)
  (assess-with-find-file
      (assess-make-related-file file)
    (font-lock-fontify-buffer)
    (assess--face-at= (current-buffer) locations faces property throw-on-nil)))

(defun assess-file-face-at= (file locations faces &optional property)
  (assess--file-face-at=-1 file locations faces property nil))

(defun assess-explain-file-face-at= (file locations faces &optional property)
  (catch 'face-non-match
    (assess--file-face-at=-1 file locations faces property t)))

(put 'assess-file-face-at=
     'ert-explainer
     'assess-explain-file-face-at=)
;; #+end_src


;; #+begin_src emacs-lisp
(defmacro assess-with-safe-user-directory (&rest body)
  (declare (debug t))
  `(let ((user-emacs-directory (make-temp-file "emacs-assess" t)))
     ,@body))
;; #+end_src


;; #+begin_src emacs-lisp
(provide 'assess)
;;; assess.el ends here
;; #+end_src
