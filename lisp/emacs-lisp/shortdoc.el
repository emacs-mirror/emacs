;;; shortdoc.el --- Short function summaries  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Free Software Foundation, Inc.

;; Keywords: lisp, help
;; Package: emacs

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

(require 'seq)
(require 'text-property-search)
(eval-when-compile (require 'cl-lib))

(defgroup shortdoc nil
  "Short documentation."
  :group 'lisp)

(defface shortdoc-heading
  '((t :inherit variable-pitch :height 1.3 :weight bold))
  "Face used for a heading."
  :version "28.1")

(defface shortdoc-section
  '((t :inherit variable-pitch))
  "Face used for a section.")

(defvar shortdoc--groups nil)

(defmacro define-short-documentation-group (group &rest functions)
  "Add GROUP to the list of defined documentation groups.
FUNCTIONS is a list of elements on the form:

  (fun
   :no-manual BOOL
   :args ARGS
   :eval EXAMPLE-FORM
   :no-eval EXAMPLE-FORM
   :no-eval* EXAMPLE-FORM
   :no-value EXAMPLE-FORM
   :result RESULT-FORM
   :result-string RESULT-FORM
   :eg-result RESULT-FORM
   :eg-result-string RESULT-FORM)

BOOL should be non-nil if the function isn't documented in the
manual.

ARGS is optional; the function's signature is displayed if ARGS
is not present.

If EVAL isn't a string, it will be printed with `prin1', and then
evaluated to give a result, which is also printed.  If it's a
string, it'll be inserted as is, then the string will be `read',
and then evaluated.

There can be any number of :example/:result elements."
  `(progn
     (setq shortdoc--groups (delq (assq ',group shortdoc--groups)
                                  shortdoc--groups))
     (push (cons ',group ',functions) shortdoc--groups)))

(define-short-documentation-group alist
  "Alist Basics"
  (assoc
   :eval (assoc 'foo '((foo . bar) (zot . baz))))
  (rassoc
   :eval (rassoc 'bar '((foo . bar) (zot . baz))))
  (assq
   :eval (assq 'foo '((foo . bar) (zot . baz))))
  (rassq
   :eval (rassq 'bar '((foo . bar) (zot . baz))))
  (assoc-string
   :eval (assoc-string "foo" '(("foo" . "bar") ("zot" "baz"))))
  "Manipulating Alists"
  (assoc-delete-all
   :eval (assoc-delete-all "foo" '(("foo" . "bar") ("zot" . "baz")) #'equal))
  (assq-delete-all
   :eval (assq-delete-all 'foo '((foo . bar) (zot . baz))))
  (rassq-delete-all
   :eval (rassq-delete-all 'bar '((foo . bar) (zot . baz))))
  (alist-get
   :eval (let ((foo '((bar . baz))))
           (setf (alist-get 'bar foo) 'zot)
           foo))
  "Misc"
  (assoc-default
   :eval (assoc-default "foobar" '(("foo" . baz)) #'string-match))
  (copy-alist
   :eval (let* ((old '((foo . bar)))
                (new (copy-alist old)))
           (eq old new)))
  ;; FIXME: Outputs "\.rose" for the symbol `.rose'.
  ;; (let-alist
  ;;     :eval (let ((colors '((rose . red)
  ;;                           (lily . white))))
  ;;             (let-alist colors
  ;;               (if (eq .rose 'red)
  ;;                   .lily))))
  )

(define-short-documentation-group string
  "Making Strings"
  (make-string
   :args (length init)
   :eval "(make-string 5 ?x)")
  (string
   :eval "(string ?a ?b ?c)")
  (concat
   :eval (concat "foo" "bar" "zot"))
  (string-join
   :no-manual t
   :eval (string-join '("foo" "bar" "zot") " "))
  (mapconcat
   :eval (mapconcat (lambda (a) (concat "[" a "]"))
                    '("foo" "bar" "zot") " "))
  (string-pad
   :eval (string-pad "foo" 5)
   :eval (string-pad "foobar" 5)
   :eval (string-pad "foo" 5 ?- t))
  (mapcar
   :eval (mapcar #'identity "123"))
  (format
   :eval (format "This number is %d" 4))
  "Manipulating Strings"
  (substring
   :eval (substring "foobar" 0 3)
   :eval (substring "foobar" 3))
  (string-limit
   :eval (string-limit "foobar" 3)
   :eval (string-limit "foobar" 3 t)
   :eval (string-limit "foobar" 10)
   :eval (string-limit "fo好" 3 nil 'utf-8))
  (truncate-string-to-width
   :eval (truncate-string-to-width "foobar" 3)
   :eval (truncate-string-to-width "你好bar" 5))
  (split-string
   :eval (split-string "foo bar")
   :eval (split-string "|foo|bar|" "|")
   :eval (split-string "|foo|bar|" "|" t))
  (split-string-and-unquote
   :eval (split-string-and-unquote "foo \"bar zot\""))
  (split-string-shell-command
   :eval (split-string-shell-command "ls /tmp/'foo bar'"))
  (string-lines
   :eval (string-lines "foo\n\nbar")
   :eval (string-lines "foo\n\nbar" t))
  (string-replace
   :eval (string-replace "foo" "bar" "foozot"))
  (replace-regexp-in-string
   :eval (replace-regexp-in-string "[a-z]+" "_" "*foo*"))
  (string-trim
   :args (string)
   :doc "Trim STRING of leading and trailing white space."
   :eval (string-trim " foo "))
  (string-trim-left
   :eval (string-trim-left "oofoo" "o+"))
  (string-trim-right
   :eval (string-trim-right "barkss" "s+"))
  (string-truncate-left
   :no-manual t
   :eval (string-truncate-left "longstring" 8))
  (string-remove-suffix
   :no-manual t
   :eval (string-remove-suffix "bar" "foobar"))
  (string-remove-prefix
   :no-manual t
   :eval (string-remove-prefix "foo" "foobar"))
  (string-chop-newline
   :eval (string-chop-newline "foo\n"))
  (string-clean-whitespace
   :eval (string-clean-whitespace " foo   bar   "))
  (string-fill
   :eval (string-fill "Three short words" 12)
   :eval (string-fill "Long-word" 3))
  (reverse
   :eval (reverse "foo"))
  (substring-no-properties
   :eval (substring-no-properties (propertize "foobar" 'face 'bold) 0 3))
  (try-completion
   :eval (try-completion "foo" '("foobar" "foozot" "gazonk")))
  "Predicates for Strings"
  (string-equal
   :eval (string-equal "foo" "foo"))
  (eq
   :eval (eq "foo" "foo"))
  (eql
   :eval (eql "foo" "foo"))
  (equal
   :eval (equal "foo" "foo"))
  (cl-equalp
   :eval (cl-equalp "Foo" "foo"))
  (stringp
   :eval "(stringp ?a)")
  (string-empty-p
   :no-manual t
   :eval (string-empty-p ""))
  (string-blank-p
   :no-manual t
   :eval (string-blank-p " \n"))
  (string-lessp
   :eval (string-lessp "foo" "bar"))
  (string-greaterp
   :eval (string-greaterp "foo" "bar"))
  (string-version-lessp
   :eval (string-version-lessp "pic4.png" "pic32.png"))
  (string-prefix-p
   :eval (string-prefix-p "foo" "foobar"))
  (string-suffix-p
   :eval (string-suffix-p "bar" "foobar"))
  "Case Manipulation"
  (upcase
   :eval (upcase "foo"))
  (downcase
   :eval (downcase "FOObar"))
  (capitalize
   :eval (capitalize "foo bar zot"))
  (upcase-initials
   :eval (upcase-initials "The CAT in the hAt"))
  "Converting Strings"
  (string-to-number
   :eval (string-to-number "42")
   :eval (string-to-number "deadbeef" 16))
  (number-to-string
   :eval (number-to-string 42))
  "Data About Strings"
  (length
   :eval (length "foo"))
  (string-search
   :eval (string-search "bar" "foobarzot"))
  (assoc-string
   :eval (assoc-string "foo" '(("a" 1) (foo 2))))
  (seq-position
   :eval "(seq-position \"foobarzot\" ?z)"))

(define-short-documentation-group file-name
  "File Name Manipulation"
  (file-name-directory
   :eval (file-name-directory "/tmp/foo")
   :eval (file-name-directory "/tmp/foo/"))
  (file-name-nondirectory
   :eval (file-name-nondirectory "/tmp/foo")
   :eval (file-name-nondirectory "/tmp/foo/"))
  (file-name-sans-versions
   :args (filename)
   :eval (file-name-sans-versions "/tmp/foo~"))
  (file-name-extension
   :eval (file-name-extension "/tmp/foo.txt"))
  (file-name-sans-extension
   :eval (file-name-sans-extension "/tmp/foo.txt"))
  (file-name-with-extension
   :eval (file-name-with-extension "foo.txt" "bin")
   :eval (file-name-with-extension "foo" "bin"))
  (file-name-base
   :eval (file-name-base "/tmp/foo.txt"))
  (file-relative-name
   :eval (file-relative-name "/tmp/foo" "/tmp"))
  (make-temp-name
   :eval (make-temp-name "/tmp/foo-"))
  (file-name-concat
   :eval (file-name-concat "/tmp/" "foo")
   :eval (file-name-concat "/tmp" "foo")
   :eval (file-name-concat "/tmp" "foo" "bar/" "zot")
   :eval (file-name-concat "/tmp" "~"))
  (expand-file-name
   :eval (expand-file-name "foo" "/tmp/")
   :eval (expand-file-name "foo" "/tmp///")
   :eval (expand-file-name "foo" "/tmp/foo/.././")
   :eval (expand-file-name "~" "/tmp/"))
  (substitute-in-file-name
   :eval (substitute-in-file-name "$HOME/foo"))
  "Directory Functions"
  (file-name-as-directory
   :eval (file-name-as-directory "/tmp/foo"))
  (directory-file-name
   :eval (directory-file-name "/tmp/foo/"))
  (abbreviate-file-name
   :no-eval (abbreviate-file-name "/home/some-user")
   :eg-result "~some-user")
  "Quoted File Names"
  (file-name-quote
   :args (name)
   :eval (file-name-quote "/tmp/foo"))
  (file-name-unquote
   :args (name)
   :eval (file-name-unquote "/:/tmp/foo"))
  "Predicates"
  (file-name-absolute-p
   :eval (file-name-absolute-p "/tmp/foo")
   :eval (file-name-absolute-p "foo"))
  (directory-name-p
   :eval (directory-name-p "/tmp/foo/"))
  (file-name-quoted-p
   :eval (file-name-quoted-p "/:/tmp/foo")))

(define-short-documentation-group file
  "Inserting Contents"
  (insert-file-contents
   :no-eval (insert-file-contents "/tmp/foo")
   :eg-result ("/tmp/foo" 6))
  (insert-file-contents-literally
   :no-eval (insert-file-contents-literally "/tmp/foo")
   :eg-result ("/tmp/foo" 6))
  (find-file
   :no-eval (find-file "/tmp/foo")
   :eg-result-string "#<buffer foo>")
  "Predicates"
  (file-symlink-p
   :no-eval (file-symlink-p "/tmp/foo")
   :eg-result t)
  (file-directory-p
   :no-eval (file-directory-p "/tmp")
   :eg-result t)
  (file-regular-p
   :no-eval (file-regular-p "/tmp/foo")
   :eg-result t)
  (file-exists-p
   :no-eval (file-exists-p "/tmp/foo")
   :eg-result t)
  (file-readable-p
   :no-eval (file-readable-p "/tmp/foo")
   :eg-result t)
  (file-writeable-p
   :no-eval (file-writeable-p "/tmp/foo")
   :eg-result t)
  (file-accessible-directory-p
   :no-eval (file-accessible-directory-p "/tmp")
   :eg-result t)
  (file-executable-p
   :no-eval (file-executable-p "/bin/cat")
   :eg-result t)
  (file-newer-than-file-p
   :no-eval (file-newer-than-file-p "/tmp/foo" "/tmp/bar")
   :eg-result nil)
  (file-equal-p
   :no-eval (file-equal-p "/tmp/foo" "/tmp/bar")
   :eg-result nil)
  (file-in-directory-p
   :no-eval (file-in-directory-p "/tmp/foo" "/tmp/")
   :eg-result t)
  (file-locked-p
   :no-eval (file-locked-p "/tmp/foo")
   :eg-result nil)
  "Information"
  (file-attributes
   :no-eval* (file-attributes "/tmp"))
  (file-truename
   :no-eval (file-truename "/tmp/foo/bar")
   :eg-result "/tmp/foo/zot")
  (file-chase-links
   :no-eval (file-chase-links "/tmp/foo/bar")
   :eg-result "/tmp/foo/zot")
  (vc-responsible-backend
   :args (file &optional no-error)
   :no-eval (vc-responsible-backend "/src/foo/bar.c")
   :eg-result Git)
  (file-acl
   :no-eval (file-acl "/tmp/foo")
   :eg-result "user::rw-\ngroup::r--\nother::r--\n")
  (file-extended-attributes
   :no-eval* (file-extended-attributes "/tmp/foo"))
  (file-selinux-context
   :no-eval* (file-selinux-context "/tmp/foo"))
  (locate-file
   :no-eval (locate-file "syslog" '("/var/log" "/usr/bin"))
   :eg-result "/var/log/syslog")
  (executable-find
   :no-eval (executable-find "ls")
   :eg-result "/usr/bin/ls")
  "Creating"
  (make-temp-file
   :no-eval (make-temp-file "/tmp/foo-")
   :eg-result "/tmp/foo-ZcXFMj")
  (make-nearby-temp-file
   :no-eval (make-nearby-temp-file "/tmp/foo-")
   :eg-result "/tmp/foo-xe8iON")
  (write-region
   :no-value (write-region (point-min) (point-max) "/tmp/foo"))
  "Directories"
  (make-directory
   :no-value (make-directory "/tmp/bar/zot/" t))
  (directory-files
   :no-eval (directory-files "/tmp/")
   :eg-result ("." ".." ".ICE-unix" ".Test-unix"))
  (directory-files-recursively
   :no-eval (directory-files-recursively "/tmp/" "\\.png\\'")
   :eg-result ("/tmp/foo.png" "/tmp/zot.png" "/tmp/bar/foobar.png"))
  (directory-files-and-attributes
   :no-eval* (directory-files-and-attributes "/tmp/foo"))
  (file-expand-wildcards
   :no-eval (file-expand-wildcards "/tmp/*.png")
   :eg-result ("/tmp/foo.png" "/tmp/zot.png"))
  (locate-dominating-file
   :no-eval (locate-dominating-file "foo.png" "/tmp/foo/bar/zot")
   :eg-result "/tmp/foo.png")
  (copy-directory
   :no-value (copy-directory "/tmp/bar/" "/tmp/barcopy"))
  (delete-directory
   :no-value (delete-directory "/tmp/bar/"))
  "File Operations"
  (rename-file
   :no-value (rename-file "/tmp/foo" "/tmp/newname"))
  (copy-file
   :no-value (copy-file "/tmp/foo" "/tmp/foocopy"))
  (delete-file
   :no-value (delete-file "/tmp/foo"))
  (make-empty-file
   :no-value (make-empty-file "/tmp/foo"))
  (make-symbolic-link
   :no-value (make-symbolic-link "/tmp/foo" "/tmp/foosymlink"))
  (add-name-to-file
   :no-value (add-name-to-file "/tmp/foo" "/tmp/bar"))
  (set-file-modes
   :no-value "(set-file-modes \"/tmp/foo\" #o644)")
  (set-file-times
   :no-value (set-file-times "/tmp/foo" (current-time)))
  "File Modes"
  (set-default-file-modes
   :no-value "(set-default-file-modes #o755)")
  (default-file-modes
    :no-eval (default-file-modes)
    :eg-result-string "#o755")
  (file-modes-symbolic-to-number
   :no-eval (file-modes-symbolic-to-number "a+r")
   :eg-result-string "#o444")
  (file-modes-number-to-symbolic
   :eval "(file-modes-number-to-symbolic #o444)")
  (set-file-extended-attributes
   :no-eval (set-file-extended-attributes
             "/tmp/foo" '((acl . "group::rxx")))
   :eg-result t)
  (set-file-selinux-context
   :no-eval (set-file-selinux-context
             "/tmp/foo" '(unconfined_u object_r user_home_t s0))
   :eg-result t)
  (set-file-acl
   :no-eval (set-file-acl "/tmp/foo" "group::rxx")
   :eg-result t))

(define-short-documentation-group hash-table
  "Hash Table Basics"
  (make-hash-table
   :no-eval (make-hash-table)
   :result-string "#s(hash-table ...)")
  (puthash
   :no-eval (puthash 'key "value" table))
  (gethash
   :no-eval (gethash 'key table)
   :eg-result "value")
  (remhash
   :no-eval (remhash 'key table)
   :result nil)
  (clrhash
   :no-eval (clrhash table)
   :result-string "#s(hash-table ...)")
  (maphash
   :no-eval (maphash (lambda (key value) (message value)) table)
   :result nil)
  "Other Hash Table Functions"
  (hash-table-p
   :eval (hash-table-p 123))
  (copy-hash-table
   :no-eval (copy-hash-table table)
   :result-string "#s(hash-table ...)")
  (hash-table-count
   :no-eval (hash-table-count table)
   :eg-result 15)
  (hash-table-size
   :no-eval (hash-table-size table)
   :eg-result 65))

(define-short-documentation-group list
  "Making Lists"
  (make-list
   :eval (make-list 5 'a))
  (cons
   :eval (cons 1 '(2 3 4)))
  (list
   :eval (list 1 2 3))
  (number-sequence
   :eval (number-sequence 5 8))
  (ensure-list
   :eval (ensure-list "foo")
   :eval (ensure-list '(1 2 3)))
  "Operations on Lists"
  (append
   :eval (append '("foo" "bar") '("zot")))
  (copy-tree
   :eval (copy-tree '(1 (2 3) 4)))
  (flatten-tree
   :eval (flatten-tree '(1 (2 3) 4)))
  (car
   :eval (car '(one two three))
   :eval (car '(one . two))
   :eval (car nil))
  (cdr
   :eval (cdr '(one two three))
   :eval (cdr '(one . two))
   :eval (cdr nil))
  (last
   :eval (last '(one two three)))
  (butlast
   :eval (butlast '(one two three)))
  (nbutlast
   :eval (nbutlast (list 'one 'two 'three)))
  (nth
   :eval (nth 1 '(one two three)))
  (nthcdr
   :eval (nthcdr 1 '(one two three)))
  (elt
   :eval (elt '(one two three) 1))
  (car-safe
   :eval (car-safe '(one two three)))
  (cdr-safe
   :eval (cdr-safe '(one two three)))
  (push
   :no-eval* (push 'a list))
  (pop
   :no-eval* (pop list))
  (setcar
   :no-eval (setcar list 'c)
   :result c)
  (setcdr
   :no-eval (setcdr list (list c))
   :result '(c))
  (nconc
   :eval (nconc (list 1) (list 2 3 4)))
  (delq
   :eval (delq 2 (list 1 2 3 4))
   :eval (delq "a" (list "a" "b" "c" "d")))
  (delete
   :eval (delete 2 (list 1 2 3 4))
   :eval (delete "a" (list "a" "b" "c" "d")))
  (remove
   :eval (remove 2 '(1 2 3 4))
   :eval (remove "a" '("a" "b" "c" "d")))
  (delete-dups
   :eval (delete-dups (list 1 2 4 3 2 4)))
  "Mapping Over Lists"
  (mapcar
   :eval (mapcar #'list '(1 2 3)))
  (mapcan
   :eval (mapcan #'list '(1 2 3)))
  (mapc
   :eval (mapc #'insert '("1" "2" "3")))
  (reduce
   :eval (reduce #'+ '(1 2 3)))
  (mapconcat
   :eval (mapconcat #'identity '("foo" "bar") "|"))
  "Predicates"
  (listp
   :eval (listp '(1 2 3))
   :eval (listp nil)
   :eval (listp '(1 . 2)))
  (consp
   :eval (consp '(1 2 3))
   :eval (consp nil))
  (proper-list-p
   :eval (proper-list-p '(1 2 3))
   :eval (proper-list-p nil)
   :eval (proper-list-p '(1 . 2)))
  (null
   :eval (null nil))
  (atom
   :eval (atom 'a))
  (nlistp
   :eval (nlistp '(1 2 3))
   :eval (nlistp t)
   :eval (nlistp '(1 . 2)))
  "Finding Elements"
  (memq
   :eval (memq 2 '(1 2 3))
   :eval (memq 2.0 '(1.0 2.0 3.0))
   :eval (memq "b" '("a" "b" "c")))
  (member
   :eval (member 2 '(1 2 3))
   :eval (member "b" '("a" "b" "c")))
  (remq
   :eval (remq 2 '(1 2 3 2 4 2))
   :eval (remq "b" '("a" "b" "c")))
  (memql
   :eval (memql 2.0 '(1.0 2.0 3.0)))
  (member-ignore-case
   :eval (member-ignore-case "foo" '("bar" "Foo" "zot")))
  "Association Lists"
  (assoc
   :eval (assoc 'b '((a 1) (b 2))))
  (rassoc
   :eval (rassoc '2 '((a . 1) (b . 2))))
  (assq
   :eval (assq 'b '((a 1) (b 2)))
   :eval (assq "a" '(("a" 1) ("b" 2))))
  (rassq
   :eval (rassq '2 '((a . 1) (b . 2))))
  (assoc-string
   :eval (assoc-string "foo" '(("a" 1) (foo 2))))
  (alist-get
   :eval (alist-get 2 '((1 . a) (2 . b))))
  (assoc-default
   :eval (assoc-default 2 '((1 . a) (2 . b) #'=)))
  (copy-alist
   :eval (copy-alist '((1 . a) (2 . b))))
  (assq-delete-all
   :eval (assq-delete-all 2 (list '(1 . a) '(2 . b) '(2 . c))))
  (assoc-delete-all
   :eval (assoc-delete-all "b" (list '("a" . a) '("b" . b) '("b" . c))))
  "Property Lists"
  (plist-get
   :eval (plist-get '(a 1 b 2 c 3) 'b))
  (plist-put
   :no-eval (setq plist (plist-put plist 'd 4))
   :eq-result (a 1 b 2 c 3 d 4))
  (lax-plist-get
   :eval (lax-plist-get '("a" 1 "b" 2 "c" 3) "b"))
  (lax-plist-put
   :no-eval (setq plist (lax-plist-put plist "d" 4))
   :eq-result '("a" 1 "b" 2 "c" 3 "d" 4))
  (plist-member
   :eval (plist-member '(a 1 b 2 c 3) 'b))
  "Data About Lists"
  (length
   :eval (length '(a b c)))
  (length<
   :eval (length< '(a b c) 1))
  (length>
   :eval (length> '(a b c) 1))
  (length=
   :eval (length= '(a b c) 3))
  (safe-length
   :eval (safe-length '(a b c))))


(define-short-documentation-group vector
  "Making Vectors"
  (make-vector
   :eval (make-vector 5 "foo"))
  (vector
   :eval (vector 1 "b" 3))
  "Operations on Vectors"
  (vectorp
   :eval (vectorp [1])
   :eval (vectorp "1"))
  (vconcat
   :eval (vconcat '(1 2) [3 4]))
  (append
   :eval (append [1 2] nil))
  (length
   :eval (length [1 2 3]))
  (seq-reduce
   :eval (seq-reduce #'+ [1 2 3] 0))
  (seq-subseq
   :eval (seq-subseq [1 2 3 4 5] 1 3)
   :eval (seq-subseq [1 2 3 4 5] 1))
  "Mapping Over Vectors"
  (mapcar
   :eval (mapcar #'identity [1 2 3]))
  (mapc
   :eval (mapc #'insert ["1" "2" "3"])))

(define-short-documentation-group regexp
  "Matching Strings"
  (replace-regexp-in-string
   :eval (replace-regexp-in-string "[a-z]+" "_" "*foo*"))
  (string-match-p
   :eval (string-match-p "^[fo]+" "foobar"))
  "Looking in Buffers"
  (re-search-forward
   :no-eval (re-search-forward "^foo$" nil t)
   :eg-result 43)
  (re-search-backward
   :no-eval (re-search-backward "^foo$" nil t)
   :eg-result 43)
  (looking-at-p
   :no-eval (looking-at-p "f[0-9]")
   :eg-result t)
  "Match Data"
  (match-string
   :eval (and (string-match "^\\([fo]+\\)b" "foobar")
              (match-string 0 "foobar")))
  (match-beginning
   :no-eval (match-beginning 1)
   :eg-result 0)
  (match-end
   :no-eval (match-end 1)
   :eg-result 3)
  (save-match-data
    :no-eval (save-match-data ...))
  "Replacing Match"
  (replace-match
   :no-eval (replace-match "new")
   :eg-result nil)
  (match-substitute-replacement
   :no-eval (match-substitute-replacement "new")
   :eg-result "new")
  (replace-regexp-in-region
   :no-value (replace-regexp-in-region "[0-9]+" "Num \\&"))
  "Utilities"
  (regexp-quote
   :eval (regexp-quote "foo.*bar"))
  (regexp-opt
   :eval (regexp-opt '("foo" "bar")))
  (regexp-opt-depth
   :eval (regexp-opt-depth "\\(a\\(b\\)\\)"))
  (regexp-opt-charset
   :eval (regexp-opt-charset '(?a ?b ?c ?d ?e)))
  "The `rx' Structured Regexp Notation"
  (rx
   :eval (rx "IP=" (+ digit) (= 3 "." (+ digit))))
  (rx-to-string
   :eval (rx-to-string '(| "foo" "bar")))
  (rx-define
   :no-eval "(and (rx-define haskell-comment (seq \"--\" (zero-or-more nonl)))
       (rx haskell-comment))"
   :result "--.*")
  (rx-let
   :eval "(rx-let ((comma-separated (item) (seq item (0+ \",\" item)))
           (number (1+ digit))
           (numbers (comma-separated number)))
    (rx \"(\" numbers \")\"))"
   :result "([[:digit:]]+\\(?:,[[:digit:]]+\\)*)")
  (rx-let-eval
   :eval "(rx-let-eval
      '((ponder (x) (seq \"Where have all the \" x \" gone?\")))
    (rx-to-string
     '(ponder (or \"flowers\" \"cars\" \"socks\"))))"
   :result "\\(?:Where have all the \\(?:\\(?:car\\|flower\\|sock\\)s\\) gone\\?\\)"))

(define-short-documentation-group sequence
  "Sequence Predicates"
  (seq-contains-p
   :eval (seq-contains-p '(a b c) 'b)
   :eval (seq-contains-p '(a b c) 'd))
  (seq-every-p
   :eval (seq-every-p #'numberp '(1 2 3)))
  (seq-empty-p
   :eval (seq-empty-p []))
  (seq-set-equal-p
   :eval (seq-set-equal-p '(1 2 3) '(3 1 2)))
  (seq-some
   :eval (seq-some #'cl-evenp '(1 2 3)))
  "Building Sequences"
  (seq-concatenate
   :eval (seq-concatenate 'vector '(1 2) '(c d)))
  (seq-copy
   :eval (seq-copy '(a 2)))
  (seq-into
   :eval (seq-into '(1 2 3) 'vector))
  "Utility Functions"
  (seq-count
   :eval (seq-count #'numberp '(1 b c 4)))
  (seq-elt
   :eval (seq-elt '(a b c) 1))
  (seq-random-elt
   :no-eval (seq-random-elt '(a b c))
   :eg-result c)
  (seq-find
   :eval (seq-find #'numberp '(a b 3 4 f 6)))
  (seq-position
   :eval (seq-position '(a b c) 'c))
  (seq-length
   :eval (seq-length "abcde"))
  (seq-max
   :eval (seq-max [1 2 3]))
  (seq-min
   :eval (seq-min [1 2 3]))
  (seq-first
   :eval (seq-first [a b c]))
  (seq-rest
   :eval (seq-rest '[1 2 3]))
  (seq-reverse
   :eval (seq-reverse '(1 2 3)))
  (seq-sort
   :eval (seq-sort #'> '(1 2 3)))
  (seq-sort-by
   :eval (seq-sort-by (lambda (a) (/ 1.0 a)) #'< '(1 2 3)))
  "Mapping Over Sequences"
  (seq-map
   :eval (seq-map #'1+ '(1 2 3)))
  (seq-map-indexed
   :eval (seq-map-indexed (lambda (a i) (cons i a)) '(a b c)))
  (seq-mapcat
   :eval (seq-mapcat #'upcase '("a" "b" "c") 'string))
  (seq-do
   :no-eval (seq-do (lambda (a) (insert a)) '("foo" "bar"))
   :eg-result ("foo" "bar"))
  (seq-do-indexed
   :no-eval (seq-do-indexed
             (lambda (a index) (message "%s:%s" index a))
             '("foo" "bar"))
   :eg-result nil)
  (seq-reduce
   :eval (seq-reduce #'* [1 2 3] 2))
  "Excerpting Sequences"
  (seq-drop
   :eval (seq-drop '(a b c) 2))
  (seq-drop-while
   :eval (seq-drop-while #'numberp '(1 2 c d 5)))
  (seq-filter
   :eval (seq-filter #'numberp '(a b 3 4 f 6)))
  (seq-remove
   :eval (seq-remove #'numberp '(1 2 c d 5)))
  (seq-group-by
   :eval (seq-group-by #'cl-plusp '(-1 2 3 -4 -5 6)))
  (seq-union
   :eval (seq-union '(1 2 3) '(3 5)))
  (seq-difference
   :eval (seq-difference '(1 2 3) '(2 3 4)))
  (seq-intersection
   :eval (seq-intersection '(1 2 3) '(2 3 4)))
  (seq-partition
   :eval (seq-partition '(a b c d e f g h) 3))
  (seq-subseq
   :eval (seq-subseq '(a b c d e) 2 4))
  (seq-take
   :eval (seq-take '(a b c d e) 3))
  (seq-take-while
   :eval (seq-take-while #'cl-evenp [2 4 9 6 5]))
  (seq-uniq
   :eval (seq-uniq '(a b d b a c))))

(define-short-documentation-group buffer
  "Buffer Basics"
  (current-buffer
   :no-eval (current-buffer)
   :eg-result-string "#<buffer shortdoc.el>")
  (bufferp
   :eval (bufferp 23))
  (buffer-live-p
   :no-eval (buffer-live-p some-buffer)
   :eg-result t)
  (buffer-modified-p
   :eval (buffer-modified-p (current-buffer)))
  (buffer-name
   :eval (buffer-name))
  (window-buffer
   :eval (window-buffer))
  "Selecting Buffers"
  (get-buffer-create
   :no-eval (get-buffer-create "*foo*")
   :eg-result-string "#<buffer *foo*>")
  (pop-to-buffer
   :no-eval (pop-to-buffer "*foo*")
   :eg-result-string "#<buffer *foo*>")
  (with-current-buffer
      :no-eval* (with-current-buffer buffer (buffer-size)))
  "Points and Positions"
  (point
   :eval (point))
  (point-min
   :eval (point-min))
  (point-max
   :eval (point-max))
  (line-beginning-position
   :eval (line-beginning-position))
  (line-end-position
   :eval (line-end-position))
  (buffer-size
   :eval (buffer-size))
  "Moving Around"
  (goto-char
   :no-eval (goto-char (point-max))
   :eg-result 342)
  (search-forward
   :no-eval (search-forward "some-string" nil t)
   :eg-result 245)
  (re-search-forward
   :no-eval (re-search-forward "some-s.*g" nil t)
   :eg-result 245)
  (forward-line
   :no-eval (forward-line 1)
   :eg-result 0
   :no-eval (forward-line -2)
   :eg-result 0)
  "Strings from Buffers"
  (buffer-string
   :no-eval* (buffer-string))
  (buffer-substring
   :eval (buffer-substring (point-min) (+ (point-min) 10)))
  (buffer-substring-no-properties
   :eval (buffer-substring-no-properties (point-min) (+ (point-min) 10)))
  (following-char
   :no-eval (following-char)
   :eg-result 67)
  (char-after
   :eval (char-after 45))
  (get-byte
   :no-eval (get-byte 45)
   :eg-result-string "#xff")
  "Altering Buffers"
  (delete-region
   :no-value (delete-region (point-min) (point-max)))
  (erase-buffer
   :no-value (erase-buffer))
  (insert
   :no-value (insert "This string will be inserted in the buffer\n"))
  (subst-char-in-region
   :no-eval "(subst-char-in-region (point-min) (point-max) ?+ ?-)")
  (replace-string-in-region
   :no-value (replace-string-in-region "foo" "bar"))
  "Locking"
  (lock-buffer
   :no-value (lock-buffer "/tmp/foo"))
  (unlock-buffer
   :no-value (unlock-buffer)))

(define-short-documentation-group overlay
  "Predicates"
  (overlayp
   :no-eval (overlayp some-overlay)
   :eg-result t)
  "Creation and Deletion"
  (make-overlay
   :args (beg end &optional buffer)
   :no-eval (make-overlay 1 10)
   :eg-result-string "#<overlay from 1 to 10 in *foo*>")
  (delete-overlay
   :no-eval (delete-overlay foo)
   :eg-result t)
  "Searching Overlays"
  (overlays-at
   :no-eval (overlays-at 15)
   :eg-result-string "(#<overlay from 1 to 10 in *foo*>)")
  (overlays-in
   :no-eval (overlays-in 1 30)
   :eg-result-string "(#<overlay from 1 to 10 in *foo*>)")
  (next-overlay-change
   :no-eval (next-overlay-change 1)
   :eg-result 20)
  (previous-overlay-change
   :no-eval (previous-overlay-change 30)
   :eg-result 20)
  "Overlay Properties"
  (overlay-start
   :no-eval (overlay-start foo)
   :eg-result 1)
  (overlay-end
   :no-eval (overlay-end foo)
   :eg-result 10)
  (overlay-put
   :no-eval (overlay-put foo 'happy t)
   :eg-result t)
  (overlay-get
   :no-eval (overlay-get foo 'happy)
   :eg-result t)
  (overlay-buffer
   :no-eval (overlay-buffer foo))
  "Moving Overlays"
  (move-overlay
   :no-eval (move-overlay foo 5 20)
   :eg-result-string "#<overlay from 5 to 20 in *foo*>"))

(define-short-documentation-group process
  (make-process
   :no-eval (make-process :name "foo" :command '("cat" "/tmp/foo"))
   :eg-result-string "#<process foo>")
  (processp
   :eval (processp t))
  (delete-process
   :no-value (delete-process process))
  (kill-process
   :no-value (kill-process process))
  (set-process-sentinel
   :no-value (set-process-sentinel process (lambda (proc string))))
  (process-buffer
   :no-eval (process-buffer process)
   :eg-result-string "#<buffer *foo*>")
  (get-buffer-process
   :no-eval (get-buffer-process buffer)
   :eg-result-string "#<process foo>")
  (process-live-p
   :no-eval (process-live-p process)
   :eg-result t))

(define-short-documentation-group number
  "Arithmetic"
  (+
   :args (&rest numbers)
   :eval (+ 1 2)
   :eval (+ 1 2 3 4))
  (-
   :args (&rest numbers)
   :eval (- 3 2)
   :eval (- 6 3 2))
  (*
   :args (&rest numbers)
   :eval (* 3 4 5))
  (/
   :eval (/ 10 5)
   :eval (/ 10 6)
   :eval (/ 10.0 6)
   :eval (/ 10.0 3 3))
  (%
   :eval (% 10 5)
   :eval (% 10 6))
  (mod
   :eval (mod 10 5)
   :eval (mod 10 6)
   :eval (mod 10.5 6))
  (1+
   :eval (1+ 2))
  (1-
   :eval (1- 4))
  "Predicates"
  (=
   :args (number &rest numbers)
   :eval (= 4 4)
   :eval (= 4.0 4.0)
   :eval (= 4 5 6 7))
  (eq
   :eval (eq 4 4)
   :eval (eq 4.0 4.0))
  (eql
   :eval (eql 4 4)
   :eval (eql 4 "4")
   :eval (eql 4.0 4.0))
  (/=
   :eval (/= 4 4))
  (<
   :args (number &rest numbers)
   :eval (< 4 4)
   :eval (< 1 2 3))
  (<=
   :args (number &rest numbers)
   :eval (<= 4 4)
   :eval (<= 1 2 3))
  (>
   :args (number &rest numbers)
   :eval (> 4 4)
   :eval (> 1 2 3))
  (>=
   :args (number &rest numbers)
   :eval (>= 4 4)
   :eval (>= 1 2 3))
  (zerop
   :eval (zerop 0))
  (cl-plusp
   :eval (cl-plusp 0)
   :eval (cl-plusp 1))
  (cl-minusp
   :eval (cl-minusp 0)
   :eval (cl-minusp -1))
  (cl-oddp
   :eval (cl-oddp 3))
  (cl-evenp
   :eval (cl-evenp 6))
  (natnump
   :eval (natnump -1)
   :eval (natnump 23))
  (bignump
   :eval (bignump 4)
   :eval (bignump (expt 2 90)))
  (fixnump
   :eval (fixnump 4)
   :eval (fixnump (expt 2 90)))
  (floatp
   :eval (floatp 5.4))
  (integerp
   :eval (integerp 5.4))
  (numberp
   :eval (numberp "5.4"))
  (cl-digit-char-p
   :eval (cl-digit-char-p ?5 10)
   :eval (cl-digit-char-p ?f 16))
  "Operations"
  (max
   :args (number &rest numbers)
   :eval (max 7 9 3))
  (min
   :args (number &rest numbers)
   :eval (min 7 9 3))
  (abs
   :eval (abs -4))
  (float
   :eval (float 2))
  (truncate
   :eval (truncate 1.2)
   :eval (truncate -1.2)
   :eval (truncate 5.4 2))
  (floor
   :eval (floor 1.2)
   :eval (floor -1.2)
   :eval (floor 5.4 2))
  (ceiling
   :eval (ceiling 1.2)
   :eval (ceiling -1.2)
   :eval (ceiling 5.4 2))
  (round
   :eval (round 1.2)
   :eval (round -1.2)
   :eval (round 5.4 2))
  (random
   :eval (random 6))
  "Bit Operations"
  (ash
   :eval (ash 1 4)
   :eval (ash 16 -1))
  (lsh
   :eval (lsh 1 4)
   :eval (lsh 16 -1))
  (logand
   :no-eval "(logand #b10 #b111)"
   :result-string "#b10")
  (logior
   :eval (logior 4 16))
  (logxor
   :eval (logxor 4 16))
  (lognot
   :eval (lognot 5))
  (logcount
   :eval (logcount 5))
  "Floating Point"
  (isnan
   :eval (isnan 5.0))
  (frexp
   :eval (frexp 5.7))
  (ldexp
   :eval (ldexp 0.7125 3))
  (logb
   :eval (logb 10.5))
  (ffloor
   :eval (ffloor 1.2))
  (fceiling
   :eval (fceiling 1.2))
  (ftruncate
   :eval (ftruncate 1.2))
  (fround
   :eval (fround 1.2))
  "Standard Math Functions"
  (sin
   :eval (sin float-pi))
  (cos
   :eval (cos float-pi))
  (tan
   :eval (tan float-pi))
  (asin
   :eval (asin float-pi))
  (acos
   :eval (acos float-pi))
  (atan
   :eval (atan float-pi))
  (exp
   :eval (exp 4))
  (log
   :eval (log 54.59))
  (expt
   :eval (expt 2 16))
  (sqrt
   :eval (sqrt -1)))

(define-short-documentation-group text-properties
  "Examining Text Properties"
  (get-text-property
   :eval (get-text-property 0 'foo (propertize "x" 'foo t)))
  (get-char-property
   :eval (get-char-property 0 'foo (propertize "x" 'foo t)))
  (get-pos-property
   :eval (get-pos-property 0 'foo (propertize "x" 'foo t)))
  (get-char-property-and-overlay
   :eval (get-char-property-and-overlay 0 'foo (propertize "x" 'foo t)))
  (text-properties-at
   :eval (text-properties-at (point)))
  "Changing Text Properties"
  (put-text-property
   :eval (let ((s "abc")) (put-text-property 0 1 'foo t s) s)
   :no-eval (put-text-property (point) (1+ (point)) 'face 'error))
  (add-text-properties
   :no-eval (add-text-properties (point) (1+ (point)) '(face error)))
  (remove-text-properties
   :no-eval (remove-text-properties (point) (1+ (point)) '(face nil)))
  (remove-list-of-text-properties
   :no-eval (remove-list-of-text-properties (point) (1+ (point)) '(face font-lock-face)))
  (set-text-properties
   :no-eval (set-text-properties (point) (1+ (point)) '(face error)))
  (add-face-text-property
   (add-face-text-property START END '(:foreground "green")))
  (propertize
   :eval (propertize "foo" 'face 'italic 'mouse-face 'bold-italic))
  "Searching for Text Properties"
  (next-property-change
   :no-eval (next-property-change (point) (current-buffer)))
  (previous-property-change
   :no-eval (previous-property-change (point) (current-buffer)))
  (next-single-property-change
   :no-eval (next-single-property-change (point) 'face (current-buffer)))
  (previous-single-property-change
   :no-eval (previous-single-property-change (point) 'face (current-buffer)))
  ;; TODO: There are some more that could be added here.
  (text-property-search-forward
   :no-eval (text-property-search-forward 'face nil t))
  (text-property-search-backward
   :no-eval (text-property-search-backward 'face nil t)))

;;;###autoload
(defun shortdoc-display-group (group &optional function)
  "Pop to a buffer with short documentation summary for functions in GROUP.
If FUNCTION is non-nil, place point on the entry for FUNCTION (if any)."
  (interactive (list (completing-read "Show summary for functions in: "
                                      (mapcar #'car shortdoc--groups))))
  (when (stringp group)
    (setq group (intern group)))
  (unless (assq group shortdoc--groups)
    (error "No such documentation group %s" group))
  (pop-to-buffer (format "*Shortdoc %s*" group))
  (let ((inhibit-read-only t)
        (prev nil))
    (erase-buffer)
    (shortdoc-mode)
    (button-mode)
    (mapc
     (lambda (data)
       (cond
        ((stringp data)
         (setq prev nil)
         (unless (bobp)
           (insert "\n"))
         (insert (propertize
                  (concat (substitute-command-keys data) "\n\n")
                  'face 'shortdoc-heading
                  'shortdoc-section t)))
        ;; There may be functions not yet defined in the data.
        ((fboundp (car data))
         (when prev
           (insert (make-separator-line)))
         (setq prev t)
         (shortdoc--display-function data))))
     (cdr (assq group shortdoc--groups))))
  (goto-char (point-min))
  (when function
    (text-property-search-forward 'shortdoc-function function t)
    (beginning-of-line)))

(defun shortdoc--display-function (data)
  (let ((function (pop data))
        (start-section (point))
        arglist-start)
    ;; Function calling convention.
    (insert (propertize "(" 'shortdoc-function function))
    (if (plist-get data :no-manual)
        (insert-text-button
         (symbol-name function)
         'face 'button
         'action (lambda (_)
                   (describe-function function))
         'follow-link t
         'help-echo (purecopy "mouse-1, RET: describe function"))
      (insert-text-button
       (symbol-name function)
       'face 'button
       'action (lambda (_)
                 (info-lookup-symbol function 'emacs-lisp-mode))
       'follow-link t
       'help-echo (purecopy "mouse-1, RET: show \
function's documentation in the Info manual")))
    (setq arglist-start (point))
    (insert ")\n")
    ;; Doc string.
    (insert "  "
            (or (plist-get data :doc)
                (car (split-string (documentation function) "\n"))))
    (insert "\n")
    (add-face-text-property start-section (point) 'shortdoc-section t)
    (let ((print-escape-newlines t)
          (double-arrow (if (char-displayable-p ?⇒)
                            "⇒"
                          "=>"))
          (single-arrow (if (char-displayable-p ?→)
                            "→"
                          "->")))
      (cl-loop for (type value) on data by #'cddr
               do
               (cl-case type
                 (:eval
                  (if (stringp value)
                      (insert "  " value "\n")
                    (insert "  ")
                    (prin1 value (current-buffer))
                    (insert "\n")
                    (insert "    " double-arrow " ")
                    (prin1 (eval value) (current-buffer))
                    (insert "\n")))
                 (:no-eval*
                  (if (stringp value)
                      (insert "  " value "\n")
                    (insert "  ")
                    (prin1 value (current-buffer)))
                  (insert "\n    " single-arrow " "
                          (propertize "[it depends]"
                                      'face 'shortdoc-section)
                          "\n"))
                 (:no-value
                  (if (stringp value)
                      (insert "  " value)
                    (insert "  ")
                    (prin1 value (current-buffer)))
                  (insert "\n"))
                 (:no-eval
                  (if (stringp value)
                      (insert "  " value)
                    (insert "  ")
                    (prin1 value (current-buffer)))
                  (insert "\n"))
                 (:result
                  (insert "    " double-arrow " ")
                  (prin1 value (current-buffer))
                  (insert "\n"))
                 (:result-string
                  (insert "    " double-arrow " ")
                  (princ value (current-buffer))
                  (insert "\n"))
                 (:eg-result
                  (insert "    e.g. " double-arrow " ")
                  (prin1 value (current-buffer))
                  (insert "\n"))
                 (:eg-result-string
                  (insert "    e.g. " double-arrow " ")
                  (princ value (current-buffer))
                  (insert "\n")))))
    ;; Insert the arglist after doing the evals, in case that's pulled
    ;; in the function definition.
    (save-excursion
      (goto-char arglist-start)
      (dolist (param (or (plist-get data :args)
                         (help-function-arglist function t)))
        (insert " " (symbol-name param)))
      (add-face-text-property arglist-start (point) 'shortdoc-section t))))

(defun shortdoc-function-groups (function)
  "Return all shortdoc groups FUNCTION appears in."
  (cl-loop for group in shortdoc--groups
           when (assq function (cdr group))
           collect (car group)))

(defun shortdoc-add-function (group section elem)
  "Add ELEM to shortdoc GROUP in SECTION.
If GROUP doesn't exist, it will be created.
If SECTION doesn't exist, it will be added.

Example:

  (shortdoc-add-function
    'file \"Predicates\"
    '(file-locked-p :no-eval (file-locked-p \"/tmp\")))"
  (let ((glist (assq group shortdoc--groups)))
    (unless glist
      (setq glist (list group))
      (push glist shortdoc--groups))
    (let ((slist (member section glist)))
      (unless slist
        (setq slist (list section))
        (nconc glist slist))
      (while (and (cdr slist)
                  (not (stringp (cadr slist))))
        (setq slist (cdr slist)))
      (setcdr slist (cons elem (cdr slist))))))

(defvar shortdoc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'shortdoc-next)
    (define-key map (kbd "p") 'shortdoc-previous)
    (define-key map (kbd "C-c C-n") 'shortdoc-next-section)
    (define-key map (kbd "C-c C-p") 'shortdoc-previous-section)
    map)
  "Keymap for `shortdoc-mode'.")

(define-derived-mode shortdoc-mode special-mode "shortdoc"
  "Mode for shortdoc."
  :interactive nil)

(defun shortdoc--goto-section (arg sym &optional reverse)
  (unless (natnump arg)
    (setq arg 1))
  (while (> arg 0)
    (funcall
     (if reverse 'text-property-search-backward
       'text-property-search-forward)
     sym nil t t)
    (setq arg (1- arg))))

(defun shortdoc-next (&optional arg)
  "Move cursor to the next function.
With ARG, do it that many times."
  (interactive "p" shortdoc-mode)
  (shortdoc--goto-section arg 'shortdoc-function))

(defun shortdoc-previous (&optional arg)
  "Move cursor to the previous function.
With ARG, do it that many times."
  (interactive "p" shortdoc-mode)
  (shortdoc--goto-section arg 'shortdoc-function t)
  (backward-char 1))

(defun shortdoc-next-section (&optional arg)
  "Move cursor to the next section.
With ARG, do it that many times."
  (interactive "p" shortdoc-mode)
  (shortdoc--goto-section arg 'shortdoc-section))

(defun shortdoc-previous-section (&optional arg)
  "Move cursor to the previous section.
With ARG, do it that many times."
  (interactive "p" shortdoc-mode)
  (shortdoc--goto-section arg 'shortdoc-section t)
  (forward-line -2))

(provide 'shortdoc)

;;; shortdoc.el ends here
