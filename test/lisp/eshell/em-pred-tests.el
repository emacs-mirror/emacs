;;; em-pred-tests.el --- em-pred test suite  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

;; Tests for Eshell's argument predicates/modifiers.

;;; Code:

(require 'ert)
(require 'esh-mode)
(require 'eshell)
(require 'em-glob)
(require 'em-pred)
(require 'ert-x)

(require 'eshell-tests-helpers
         (ert-resource-file "eshell-tests-helpers"))

(defvar eshell-test-value nil)

(defun eshell-eval-predicate (initial-value predicate)
  "Evaluate PREDICATE on INITIAL-VALUE, returning the result.
PREDICATE is an Eshell argument predicate/modifier."
  (let ((eshell-test-value initial-value))
    (ignore-errors
      (eshell-test-command-result
       (format "echo $eshell-test-value(%s)" predicate)))))

(defun eshell-parse-file-name-attributes (file)
  "Parse a fake FILE name to determine its attributes.
Fake file names are file names beginning with \"/fake/\".  This
allows defining file names for fake files with various properties
to query via predicates.  Attributes are written as a
comma-separate list of ATTR=VALUE pairs as the file's base name,
like:

  /fake/type=-,modes=0755.el

The following attributes are recognized:

  * \"type\": A single character describing the file type;
    accepts the same values as the first character of the file
    modes in `ls -l'.
  * \"modes\": The file's permission modes, in octal.
  * \"links\": The number of links to this file.
  * \"uid\": The UID of the file's owner.
  * \"gid\": The UID of the file's group.
  * \"atime\": The time the file was last accessed, in seconds
    since the UNIX epoch.
  * \"mtime\": As \"atime\", but for modification time.
  * \"ctime\": As \"atime\", but for inode change time.
  * \"size\": The file's size in bytes."
  (mapcar (lambda (i)
            (pcase (split-string i "=")
              (`("modes" ,modes)
               (cons 'modes (string-to-number modes 8)))
              (`(,(and (or "links" "uid" "gid" "size") key) ,value)
               (cons (intern key) (string-to-number value)))
              (`(,(and (or "atime" "mtime" "ctime") key) ,value)
               (cons (intern key) (time-convert (string-to-number value) t)))
              (`(,key ,value)
               (cons (intern key) value))
              (_ (error "invalid format %S" i))))
          (split-string (file-name-base file) ",")))

(defmacro eshell-partial-let-func (overrides &rest body)
  "Temporarily bind to FUNCTION-NAMEs and evaluate BODY.
This is roughly analogous to advising functions, but only does so
while BODY is executing, and only calls NEW-FUNCTION if its first
argument is a string beginning with \"/fake/\".

This allows selectively overriding functions to test file
properties with fake files without altering the functions'
behavior for real files.

\(fn ((FUNCTION-NAME NEW-FUNCTION) ...) BODY...)"
  (declare (indent 1))
  `(cl-letf
       ,(mapcar
         (lambda (override)
           `((symbol-function #',(car override))
             (let ((orig-function (symbol-function #',(car override))))
               (lambda (file &rest rest)
                 (apply
                  (if (and (stringp file) (string-prefix-p "/fake/" file))
                      ,(cadr override)
                    orig-function)
                  file rest)))))
         overrides)
     ,@body))

(defmacro eshell-with-file-attributes-from-name (&rest body)
  "Temporarily override file attribute functions and evaluate BODY."
  (declare (indent 0))
  `(eshell-partial-let-func
       ((file-attributes
         (lambda (file &optional _id-format)
           (let ((attrs (eshell-parse-file-name-attributes file)))
             (list (equal (alist-get 'type attrs) "d")
                   (or (alist-get 'links attrs) 1)
                   (or (alist-get 'uid attrs) 0)
                   (or (alist-get 'gid attrs) 0)
                   (or (alist-get 'atime attrs) nil)
                   (or (alist-get 'mtime attrs) nil)
                   (or (alist-get 'ctime attrs) nil)
                   (or (alist-get 'size attrs) 0)
                   (format "%s---------" (or (alist-get 'type attrs) "-"))
                   nil 0 0))))
        (file-modes
         (lambda (file _nofollow)
           (let ((attrs (eshell-parse-file-name-attributes file)))
             (or (alist-get 'modes attrs) 0))))
        (file-exists-p #'always)
        (file-regular-p
         (lambda (file)
           (let ((attrs (eshell-parse-file-name-attributes file)))
             (member (or (alist-get 'type attrs) "-") '("-" "l")))))
        (file-symlink-p
         (lambda (file)
           (let ((attrs (eshell-parse-file-name-attributes file)))
             (equal (alist-get 'type attrs) "l"))))
        (file-executable-p
         (lambda (file)
           (let ((attrs (eshell-parse-file-name-attributes file)))
             ;; For simplicity, just return whether the file is
             ;; world-executable.
             (oddp (or (alist-get 'modes attrs) 0))))))
     ,@body))

;;; Tests:


;; Argument predicates

(ert-deftest em-pred-test/predicate-file-types ()
  "Test file type predicates."
  (eshell-with-file-attributes-from-name
    (let ((files (mapcar (lambda (i) (format "/fake/type=%s" i))
                         '("b" "c" "d/" "p" "s" "l" "-"))))
      (should (equal (eshell-eval-predicate files "%")
                     '("/fake/type=b" "/fake/type=c")))
      (should (equal (eshell-eval-predicate files "%b") '("/fake/type=b")))
      (should (equal (eshell-eval-predicate files "%c") '("/fake/type=c")))
      (should (equal (eshell-eval-predicate files "/")  '("/fake/type=d/")))
      (should (equal (eshell-eval-predicate files ".")  '("/fake/type=-")))
      (should (equal (eshell-eval-predicate files "p")  '("/fake/type=p")))
      (should (equal (eshell-eval-predicate files "=")  '("/fake/type=s")))
      (should (equal (eshell-eval-predicate files "@")  '("/fake/type=l"))))))

(ert-deftest em-pred-test/predicate-executable ()
  "Test that \"*\" matches only regular, non-symlink executable files."
  (eshell-with-file-attributes-from-name
    (let ((files '("/fake/modes=0777" "/fake/modes=0666"
                   "/fake/type=d,modes=0777" "/fake/type=l,modes=0777")))
      (should (equal (eshell-eval-predicate files "*")
                     '("/fake/modes=0777"))))))

(defmacro em-pred-test--file-modes-deftest (name mode-template predicates
                                                 &optional docstring)
  "Define NAME as a file-mode test.
MODE-TEMPLATE is a format string to convert an integer from 0 to
7 to an octal file mode.  PREDICATES is a list of strings for the
read, write, and execute predicates to query the file's modes."
  (declare (indent 4) (doc-string 4))
  `(ert-deftest ,name ()
     ,docstring
     (eshell-with-file-attributes-from-name
       (let ((file-template (concat "/fake/modes=" ,mode-template)))
         (cl-flet ((make-files (perms)
                               (mapcar (lambda (i) (format file-template i))
                                       perms)))
           (pcase-let ((files (make-files (number-sequence 0 7)))
                       (`(,read ,write ,exec) ,predicates))
             (should (equal (eshell-eval-predicate files read)
                            (make-files '(4 5 6 7))))
             (should (equal (eshell-eval-predicate files (concat "^" read))
                            (make-files '(0 1 2 3))))
             (should (equal (eshell-eval-predicate files write)
                            (make-files '(2 3 6 7))))
             (should (equal (eshell-eval-predicate files (concat "^" write))
                            (make-files '(0 1 4 5))))
             (should (equal (eshell-eval-predicate files exec)
                            (make-files '(1 3 5 7))))
             (should (equal (eshell-eval-predicate files (concat "^" exec))
                            (make-files '(0 2 4 6))))))))))

(em-pred-test--file-modes-deftest em-pred-test/predicate-file-modes-owner
    "0%o00" '("r" "w" "x")
    "Test predicates for file permissions for the owner.")

(em-pred-test--file-modes-deftest em-pred-test/predicate-file-modes-group
    "00%o0" '("A" "I" "E")
    "Test predicates for file permissions for the group.")

(em-pred-test--file-modes-deftest em-pred-test/predicate-file-modes-world
    "000%o" '("R" "W" "X")
    "Test predicates for file permissions for the world.")

(em-pred-test--file-modes-deftest em-pred-test/predicate-file-modes-flags
    "%o000" '("s" "S" "t")
    "Test predicates for \"s\" (setuid), \"S\" (setgid), and \"t\" (sticky).")

(ert-deftest em-pred-test/predicate-effective-uid ()
  "Test that \"U\" matches files owned by the effective UID."
  (eshell-with-file-attributes-from-name
    (cl-letf (((symbol-function 'user-uid) (lambda () 1)))
      (let ((files '("/fake/uid=1" "/fake/uid=2")))
        (should (equal (eshell-eval-predicate files "U")
                       '("/fake/uid=1")))))))

(ert-deftest em-pred-test/predicate-effective-gid ()
  "Test that \"G\" matches files owned by the effective GID."
  (eshell-with-file-attributes-from-name
    (cl-letf (((symbol-function 'group-gid) (lambda () 1)))
      (let ((files '("/fake/gid=1" "/fake/gid=2")))
        (should (equal (eshell-eval-predicate files "G")
                       '("/fake/gid=1")))))))

(ert-deftest em-pred-test/predicate-links ()
  "Test that \"l\" filters by number of links."
  (eshell-with-file-attributes-from-name
    (let ((files '("/fake/links=1" "/fake/links=2" "/fake/links=3")))
      (should (equal (eshell-eval-predicate files "l1")
                     '("/fake/links=1")))
      (should (equal (eshell-eval-predicate files "l+1")
                     '("/fake/links=2" "/fake/links=3")))
      (should (equal (eshell-eval-predicate files "l-3")
                     '("/fake/links=1" "/fake/links=2"))))))

(ert-deftest em-pred-test/predicate-uid ()
  "Test that \"u\" filters by UID/user name."
  (eshell-with-file-attributes-from-name
    (let ((files '("/fake/uid=1" "/fake/uid=2"))
          (user-names '("root" "one" "two")))
      (should (equal (eshell-eval-predicate files "u1")
                     '("/fake/uid=1")))
      (cl-letf (((symbol-function 'eshell-user-id)
                 (lambda (name) (seq-position user-names name))))
        (should (equal (eshell-eval-predicate files "u'one'")
                       '("/fake/uid=1")))))))

(ert-deftest em-pred-test/predicate-gid ()
  "Test that \"g\" filters by GID/group name."
  (eshell-with-file-attributes-from-name
    (let ((files '("/fake/gid=1" "/fake/gid=2"))
          (group-names '("root" "one" "two")))
      (should (equal (eshell-eval-predicate files "g1")
                     '("/fake/gid=1")))
      (cl-letf (((symbol-function 'eshell-group-id)
                 (lambda (name) (seq-position group-names name))))
        (should (equal (eshell-eval-predicate files "g'one'")
                       '("/fake/gid=1")))))))

(defmacro em-pred-test--time-deftest (name file-attribute predicate
                                           &optional docstring)
  "Define NAME as a file-time test.
FILE-ATTRIBUTE is the file's attribute to set (e.g. \"atime\").
PREDICATE is the predicate used to query that attribute."
  (declare (indent 4) (doc-string 4))
  `(ert-deftest ,name ()
     ,docstring
     (eshell-with-file-attributes-from-name
       (cl-flet ((make-file (time)
                            (format "/fake/%s=%d" ,file-attribute time)))
         (let* ((now (time-convert nil 'integer))
                (yesterday (- now 86400))
                (files (mapcar #'make-file (list now yesterday))))
           ;; Test comparison against a number of days.
           (should (equal (eshell-eval-predicate
                           files (concat ,predicate "-1"))
                          (mapcar #'make-file (list now))))
           (should (equal (eshell-eval-predicate
                           files (concat ,predicate "+1"))
                          (mapcar #'make-file (list yesterday))))
           (should (equal (eshell-eval-predicate
                           files (concat ,predicate "+2"))
                          nil))
           ;; Test comparison against a number of hours.
           (should (equal (eshell-eval-predicate
                           files (concat ,predicate "h-1"))
                          (mapcar #'make-file (list now))))
           (should (equal (eshell-eval-predicate
                           files (concat ,predicate "h+1"))
                          (mapcar #'make-file (list yesterday))))
           (should (equal (eshell-eval-predicate
                           files (concat ,predicate "+48"))
                          nil))
           ;; Test comparison against another file.
           (should (equal (eshell-eval-predicate
                           files (format "%s-'%s'" ,predicate (make-file now)))
                          nil))
           (should (equal (eshell-eval-predicate
                           files (format "%s+'%s'" ,predicate (make-file now)))
                          (mapcar #'make-file (list yesterday)))))))))

(em-pred-test--time-deftest em-pred-test/predicate-access-time
    "atime" "a"
    "Test that \"a\" filters by access time.")

(em-pred-test--time-deftest em-pred-test/predicate-modification-time
    "mtime" "m"
    "Test that \"m\" filters by change time.")

(em-pred-test--time-deftest em-pred-test/predicate-change-time
    "ctime" "c"
    "Test that \"c\" filters by change time.")

(ert-deftest em-pred-test/predicate-size ()
  "Test that \"L\" filters by file size."
  (eshell-with-file-attributes-from-name
    (let ((files '("/fake/size=0"
                   ;; 1 and 2 KiB.
                   "/fake/size=1024" "/fake/size=2048"
                   ;; 1 and 2 MiB.
                   "/fake/size=1048576" "/fake/size=2097152")))
      ;; Size in bytes.
      (should (equal (eshell-eval-predicate files "L2048")
                     '("/fake/size=2048")))
      (should (equal (eshell-eval-predicate files "L+2048")
                     '("/fake/size=1048576" "/fake/size=2097152")))
      (should (equal (eshell-eval-predicate files "L-2048")
                     '("/fake/size=0" "/fake/size=1024")))
      ;; Size in blocks.
      (should (equal (eshell-eval-predicate files "Lp4")
                     '("/fake/size=2048")))
      (should (equal (eshell-eval-predicate files "Lp+4")
                     '("/fake/size=1048576" "/fake/size=2097152")))
      (should (equal (eshell-eval-predicate files "Lp-4")
                     '("/fake/size=0" "/fake/size=1024")))
      ;; Size in KiB.
      (should (equal (eshell-eval-predicate files "Lk2")
                     '("/fake/size=2048")))
      (should (equal (eshell-eval-predicate files "Lk+2")
                     '("/fake/size=1048576" "/fake/size=2097152")))
      (should (equal (eshell-eval-predicate files "Lk-2")
                     '("/fake/size=0" "/fake/size=1024")))
      ;; Size in MiB.
      (should (equal (eshell-eval-predicate files "LM1")
                     '("/fake/size=1048576")))
      (should (equal (eshell-eval-predicate files "LM+1")
                     '("/fake/size=2097152")))
      (should (equal (eshell-eval-predicate files "LM-1")
                     '("/fake/size=0" "/fake/size=1024" "/fake/size=2048"))))))


;; Argument modifiers

(ert-deftest em-pred-test/modifier-eval ()
  "Test that \":E\" re-evaluates the value."
  (should (equal (eshell-eval-predicate "${echo hi}" ":E") "hi"))
  (should (equal (eshell-eval-predicate
                  '("${echo hi}" "$(upcase \"bye\")") ":E")
                 '("hi" "BYE"))))

(ert-deftest em-pred-test/modifier-downcase ()
  "Test that \":L\" downcases values."
  (should (equal (eshell-eval-predicate "FOO" ":L") "foo"))
  (should (equal (eshell-eval-predicate '("FOO" "BAR") ":L")
                 '("foo" "bar"))))

(ert-deftest em-pred-test/modifier-upcase ()
  "Test that \":U\" upcases values."
  (should (equal (eshell-eval-predicate "foo" ":U") "FOO"))
  (should (equal (eshell-eval-predicate '("foo" "bar") ":U")
                 '("FOO" "BAR"))))

(ert-deftest em-pred-test/modifier-capitalize ()
  "Test that \":C\" capitalizes values."
  (should (equal (eshell-eval-predicate "foo bar" ":C") "Foo Bar"))
  (should (equal (eshell-eval-predicate '("foo bar" "baz") ":C")
                 '("Foo Bar" "Baz"))))

(ert-deftest em-pred-test/modifier-dirname ()
  "Test that \":h\" returns the dirname."
  (should (equal (eshell-eval-predicate "/path/to/file.el" ":h") "/path/to/"))
  (should (equal (eshell-eval-predicate
                  '("/path/to/file.el" "/other/path/") ":h")
                 '("/path/to/" "/other/path/"))))

(ert-deftest em-pred-test/modifier-basename ()
  "Test that \":t\" returns the basename."
  (should (equal (eshell-eval-predicate "/path/to/file.el" ":t") "file.el"))
  (should (equal (eshell-eval-predicate
                  '("/path/to/file.el" "/other/path/") ":t")
                 '("file.el" ""))))

(ert-deftest em-pred-test/modifier-extension ()
  "Test that \":e\" returns the extension."
  (should (equal (eshell-eval-predicate "/path/to/file.el" ":e") "el"))
  (should (equal (eshell-eval-predicate
                  '("/path/to/file.el" "/other/path/") ":e")
                 '("el" nil))))

(ert-deftest em-pred-test/modifier-sans-extension ()
  "Test that \":r\" returns the file name san extension."
  (should (equal (eshell-eval-predicate "/path/to/file.el" ":r")
                 "/path/to/file"))
  (should (equal (eshell-eval-predicate
                  '("/path/to/file.el" "/other/path/") ":r")
                 '("/path/to/file" "/other/path/"))))

(ert-deftest em-pred-test/modifier-substitute ()
  "Test that \":s/PAT/REP/\" replaces PAT with REP once."
  (should (equal (eshell-eval-predicate "bar" ":s/a/*/") "b*r"))
  (should (equal (eshell-eval-predicate "bar" ":s|a|*|") "b*r"))
  (should (equal (eshell-eval-predicate "bar" ":s{a}{*}") "b*r"))
  (should (equal (eshell-eval-predicate "bar" ":s{a}'*'") "b*r"))
  (should (equal (eshell-eval-predicate '("foo" "bar" "baz") ":s/[ao]/*/")
                 '("f*o" "b*r" "b*z")))
  (should (equal (eshell-eval-predicate '("foo" "bar" "baz") ":s|[ao]|*|")
                 '("f*o" "b*r" "b*z"))))

(ert-deftest em-pred-test/modifier-global-substitute ()
  "Test that \":s/PAT/REP/\" replaces PAT with REP for all occurrences."
  (should (equal (eshell-eval-predicate "foo" ":gs/a/*/") "foo"))
  (should (equal (eshell-eval-predicate "foo" ":gs|a|*|") "foo"))
  (should (equal (eshell-eval-predicate "bar" ":gs/a/*/") "b*r"))
  (should (equal (eshell-eval-predicate "bar" ":gs|a|*|") "b*r"))
  (should (equal (eshell-eval-predicate "foo" ":gs/o/O/") "fOO"))
  (should (equal (eshell-eval-predicate '("foo" "bar" "baz") ":gs/[aeiou]/*/")
                 '("f**" "b*r" "b*z")))
  (should (equal (eshell-eval-predicate '("foo" "bar" "baz") ":gs|[aeiou]|*|")
                 '("f**" "b*r" "b*z"))))

(ert-deftest em-pred-test/modifier-include ()
  "Test that \":i/PAT/\" filters elements to include only ones matching PAT."
  (should (equal (eshell-eval-predicate "foo" ":i/a/") nil))
  (should (equal (eshell-eval-predicate "bar" ":i/a/") "bar"))
  (should (equal (eshell-eval-predicate '("foo" "bar" "baz") ":i/a/")
                 '("bar" "baz"))))

(ert-deftest em-pred-test/modifier-exclude ()
  "Test that \":x/PAT/\" filters elements to exclude any matching PAT."
  (should (equal (eshell-eval-predicate "foo" ":x/a/") "foo"))
  (should (equal (eshell-eval-predicate "bar" ":x/a/") nil))
  (should (equal (eshell-eval-predicate '("foo" "bar" "baz") ":x/a/")
                 '("foo"))))

(ert-deftest em-pred-test/modifier-split ()
  "Test that \":S\" and \":S/PAT/\" split elements by spaces (or PAT)."
  (should (equal (eshell-eval-predicate "foo bar baz" ":S")
                 '("foo" "bar" "baz")))
  (should (equal (eshell-eval-predicate '("foo bar" "baz") ":S")
                 '(("foo" "bar") ("baz"))))
  (should (equal (eshell-eval-predicate "foo-bar-baz" ":S/-/")
                 '("foo" "bar" "baz")))
  (should (equal (eshell-eval-predicate '("foo-bar" "baz") ":S/-/")
                 '(("foo" "bar") ("baz")))))

(ert-deftest em-pred-test/modifier-join ()
  "Test that \":j\" and \":j/DELIM/\" join elements by spaces (or DELIM)."
  (should (equal (eshell-eval-predicate "foo" ":j") "foo"))
  (should (equal (eshell-eval-predicate '("foo" "bar" "baz") ":j")
                 "foo bar baz"))
  (should (equal (eshell-eval-predicate "foo" ":j/-/") "foo"))
  (should (equal (eshell-eval-predicate '("foo" "bar" "baz") ":j/-/")
                 "foo-bar-baz")))

(ert-deftest em-pred-test/modifier-sort ()
  "Test that \":o\" sorts elements in lexicographic order."
  (should (equal (eshell-eval-predicate "foo" ":o") "foo"))
  (should (equal (eshell-eval-predicate '("foo" "bar" "baz") ":o")
                 '("bar" "baz" "foo"))))

(ert-deftest em-pred-test/modifier-sort-reverse ()
  "Test that \":o\" sorts elements in reverse lexicographic order."
  (should (equal (eshell-eval-predicate "foo" ":O") "foo"))
  (should (equal (eshell-eval-predicate '("foo" "bar" "baz") ":O")
                 '("foo" "baz" "bar"))))

(ert-deftest em-pred-test/modifier-unique ()
  "Test that \":u\" filters out duplicate elements."
  (should (equal (eshell-eval-predicate "foo" ":u") "foo"))
  (should (equal (eshell-eval-predicate '("foo" "bar" "baz") ":u")
                 '("foo" "bar" "baz")))
  (should (equal (eshell-eval-predicate '("foo" "bar" "baz" "foo") ":u")
                 '("foo" "bar" "baz"))))

(ert-deftest em-pred-test/modifier-reverse ()
  "Test that \":r\" reverses the order of elements."
  (should (equal (eshell-eval-predicate "foo" ":R") "foo"))
  (should (equal (eshell-eval-predicate '("foo" "bar" "baz") ":R")
                 '("baz" "bar" "foo"))))


;; Miscellaneous

(ert-deftest em-pred-test/combine-predicate-and-modifier ()
  "Test combination of predicates and modifiers."
  (eshell-with-file-attributes-from-name
    (let ((files '("/fake/type=-.el" "/fake/type=-.txt" "/fake/type=s.el"
                   "/fake/subdir/type=-.el")))
      (should (equal (eshell-eval-predicate files ".:e:u")
                     '("el" "txt"))))))

(ert-deftest em-pred-test/predicate-delimiters ()
  "Test various delimiter pairs with predicates and modifiers."
  (dolist (delims eshell-pred-delimiter-pairs)
    (eshell-with-file-attributes-from-name
     (let ((files '("/fake/uid=1" "/fake/uid=2"))
           (user-names '("root" "one" "two")))
       (cl-letf (((symbol-function 'eshell-user-id)
                  (lambda (name) (seq-position user-names name))))
         (should (equal (eshell-eval-predicate
                         files (format "u%cone%c" (car delims) (cdr delims)))
                        '("/fake/uid=1"))))))
    (should (equal (eshell-eval-predicate
                    '("foo" "bar" "baz")
                    (format ":j%c-%c" (car delims) (cdr delims)))
                   "foo-bar-baz"))))

(ert-deftest em-pred-test/predicate-escaping ()
  "Test string escaping in predicate and modifier parameters."
  ;; Escaping the delimiter should remove the backslash.
  (should (equal (eshell-eval-predicate '("foo" "bar" "baz") ":j'\\''")
                 "foo'bar'baz"))
  ;; Escaping a backlash should remove the first backslash.
  (should (equal (eshell-eval-predicate '("foo" "bar" "baz") ":j'\\\\'")
                 "foo\\bar\\baz"))
  ;; Escaping a different character should keep the backslash.
  (should (equal (eshell-eval-predicate '("foo" "bar" "baz") ":j'\\\"'")
                 "foo\\\"bar\\\"baz")))

(ert-deftest em-pred-test/no-matches ()
  "Test behavior when a predicate fails to match any files."
  (eshell-with-file-attributes-from-name
    (let ((files '("/fake/modes=0666" "/fake/type=d,modes=0777"
                   "/fake/type=l,modes=0777")))
      (should (equal (eshell-eval-predicate files "*") nil))
      (let ((eshell-error-if-no-glob t))
        ;; Don't signal an error if the original list is empty.
        (should (equal (eshell-eval-predicate nil "*") nil))
        ;; Ensure this signals an error.  This test case is a bit
        ;; clumsy, since `eshell-do-eval' makes it hard to catch
        ;; errors otherwise.
        (let ((modifiers (with-temp-eshell
                          (eshell-with-temp-command "*"
                            (eshell-parse-modifiers)))))
          (should-error (eshell-apply-modifiers files (car modifiers)
                                                (cdr modifiers) "*")))))))

;; em-pred-tests.el ends here
