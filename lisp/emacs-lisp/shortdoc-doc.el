;;; shortdoc-doc.el --- Builtin shortdoc groups  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

;; This file defines builtin Emacs shortdoc groups.
;;
;; If a shortdoc group describes builtin functions, functions from
;; subr.el or simple.el or otherwise preloaded files, or functions from
;; different files, then you should probably define it in this file.
;; Otherwise, you might as well define the shortdoc group in the file
;; where the documented functions live, like treesit.el does it.

;;; Code:

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
   :eval (assoc-delete-all "b" (list '("a" . a) '("b" . b) '("b" . c))))
  (assq-delete-all
   :eval (assq-delete-all 2 (list '(1 . a) '(2 . b) '(2 . c))))
  (rassq-delete-all
   :eval (rassq-delete-all 'b (list '(1 . a) '(2 . b) '(2 . c))))
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
  ;; FIXME: Outputs "\.rose" for the symbol `.rose'.  It would be
  ;; better if that could be cleaned up.
  (let-alist
      :eval (let ((colors '((rose . red)
                            (lily . white))))
              (let-alist colors
                (if (eq .rose 'red)
                    .lily)))))

(define-short-documentation-group map
  "Map Basics"
  (mapp
   :eval (mapp (list 'bar 1 'foo 2 'baz 3))
   :eval (mapp (list '(bar . 1) '(foo . 2) '(baz . 3)))
   :eval (mapp [bar foo baz])
   :eval (mapp "this is a string")
   :eval (mapp #s(hash-table data (bar 1 foo 2 baz 3)))
   :eval (mapp '())
   :eval (mapp nil)
   :eval (mapp (make-char-table 'shortdoc-test)))
  (map-empty-p
   :args (map)
   :eval (map-empty-p nil)
   :eval (map-empty-p [])
   :eval (map-empty-p '()))
  (map-elt
   :args (map key)
   :eval (map-elt (list 'bar 1 'foo 2 'baz 3) 'foo)
   :eval (map-elt (list '(bar . 1) '(foo . 2) '(baz . 3)) 'foo)
   :eval (map-elt [bar foo baz] 1)
   :eval (map-elt #s(hash-table data (bar 1 foo 2 baz 3)) 'foo))
  (map-contains-key
   :args (map key)
   :eval (map-contains-key (list 'bar 1 'foo 2 'baz 3) 'foo)
   :eval (map-contains-key (list '(bar . 1) '(foo . 2) '(baz . 3)) 'foo)
   :eval (map-contains-key [bar foo baz] 1)
   :eval (map-contains-key #s(hash-table data (bar 1 foo 2 baz 3)) 'foo))
  (map-put!
   (map key value)
   :eval
"(let ((map (list 'bar 1 'baz 3)))
    (map-put! map 'foo 2)
    map)"
;; This signals map-not-inplace when used in shortdoc.el :-(
;;    :eval
;; "(let ((map (list '(bar . 1) '(baz . 3))))
;;     (map-put! map 'foo 2)
;;     map)"
    :eval
"(let ((map [bar bot baz]))
    (map-put! map 1 'foo)
    map)"
   :eval
"(let ((map #s(hash-table data (bar 1 baz 3))))
    (map-put! map 'foo 2)
    map)")
  (map-insert
   :args (map key value)
   :eval (map-insert (list 'bar 1 'baz 3 'foo 7) 'foo 2)
   :eval (map-insert (list '(bar . 1) '(baz . 3) '(foo . 7)) 'foo 2)
   :eval (map-insert [bar bot baz] 1 'foo)
   :eval (map-insert #s(hash-table data (bar 1 baz 3 foo 7)) 'foo 2))
  (map-delete
   :args (map key)
   :eval (map-delete (list 'bar 1 'foo 2 'baz 3) 'foo)
   :eval (map-delete (list '(bar . 1) '(foo . 2) '(baz . 3)) 'foo)
   :eval (map-delete [bar foo baz] 1)
   :eval (map-delete #s(hash-table data (bar 1 foo 2 baz 3)) 'foo))
  (map-keys
   :eval (map-keys (list 'bar 1 'foo 2 'baz 3))
   :eval (map-keys (list '(bar . 1) '(foo . 2) '(baz . 3)))
   :eval (map-keys [bar foo baz])
   :eval (map-keys #s(hash-table data (bar 1 foo 2 baz 3))))
  (map-values
   :args (map)
   :eval (map-values (list 'bar 1 'foo 2 'baz 3))
   :eval (map-values (list '(bar . 1) '(foo . 2) '(baz . 3)))
   :eval (map-values [bar foo baz])
   :eval (map-values #s(hash-table data (bar 1 foo 2 baz 3))))
  (map-pairs
   :eval (map-pairs (list 'bar 1 'foo 2 'baz 3))
   :eval (map-pairs (list '(bar . 1) '(foo . 2) '(baz . 3)))
   :eval (map-pairs [bar foo baz])
   :eval (map-pairs #s(hash-table data (bar 1 foo 2 baz 3))))
  (map-length
   :args (map)
   :eval (map-length (list 'bar 1 'foo 2 'baz 3))
   :eval (map-length (list '(bar . 1) '(foo . 2) '(baz . 3)))
   :eval (map-length [bar foo baz])
   :eval (map-length #s(hash-table data (bar 1 foo 2 baz 3))))
  (map-copy
   :args (map)
   :eval (map-copy (list 'bar 1 'foo 2 'baz 3))
   :eval (map-copy (list '(bar . 1) '(foo . 2) '(baz . 3)))
   :eval (map-copy [bar foo baz])
   :eval (map-copy #s(hash-table data (bar 1 foo 2 baz 3))))
  "Doing things to maps and their contents"
  (map-apply
   :args (function map)
   :eval (map-apply #'+ (list '(1 . 2)  '(3 . 4))))
  (map-do
   :args (function map)
   :eval
"(let ((map (list '(1 . 1) '(2 . 3)))
      acc)
  (map-do (lambda (k v) (push (+ k v) acc)) map)
  (nreverse acc))")
  (map-keys-apply
   :eval (map-keys-apply #'1+ (list '(1 . 2)  '(3 . 4))))
  (map-values-apply
   :args (function map)
   :eval (map-values-apply #'1+ (list '(1 . 2)  '(3 . 4))))
  (map-filter
   :eval (map-filter (lambda (k _) (oddp k)) (list '(1 . 2) '(4 . 6)))
   :eval (map-filter (lambda (k v) (evenp (+ k v))) (list '(1 . 2) '(4 . 6))))
  (map-remove
   :eval (map-remove (lambda (k _) (oddp k)) (list '(1 . 2) '(4 . 6)))
   :eval (map-remove (lambda (k v) (evenp (+ k v))) (list '(1 . 2) '(4 . 6))))
  (map-some
   :eval (map-some (lambda (k _) (oddp k)) (list '(1 . 2) '(4 . 6)))
   :eval (map-some (lambda (k v) (evenp (+ k v))) (list '(1 . 2) '(4 . 6))))
  (map-every-p
   :eval (map-every-p (lambda (k _) (oddp k)) (list '(1 . 2) '(4 . 6)))
   :eval (map-every-p (lambda (k v) (evenp (+ k v))) (list '(1 . 3) '(4 . 6))))
  "Combining and changing maps"
  (map-merge
   :eval (map-merge 'alist '(1 2 3 4) #s(hash-table data (5 6 7 8)))
   :eval (map-merge 'list '(1 2 3 4) #s(hash-table data (5 6 7 8)))
   :eval (map-merge 'plist '(1 2 3 4) #s(hash-table data (5 6 7 8)))
   :eval (map-merge 'hash-table '(1 2 3 4) #s(hash-table data (5 6 7 8))))
  (map-merge-with
   :eval (map-merge-with 'alist #'max '(1 2 3 4) #s(hash-table data (1 1 3 5)))
   :eval (map-merge-with 'alist #'min '(1 2 3 4) #s(hash-table data (1 1 3 5)))
   :eval (map-merge-with 'hash-table #'min '(1 2 3 4) #s(hash-table data (1 1 3 5))))
  (map-into
   :args (map type)
   :eval (map-into #s(hash-table data '(5 6 7 8)) 'list)
   :eval (map-into '((5 . 6) (7 . 8)) 'plist)
   :eval (map-into '((5 . 6) (7 . 8)) 'hash-table)))

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
   :eval (substring "abcde" 1 3)
   :eval (substring "abcde" 2)
   :eval (substring "abcde" 1 -1)
   :eval (substring "abcde" -4 4))
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
  "Unicode Strings"
  (string-glyph-split
   :eval (string-glyph-split "Hello, 👼🏻🧑🏼‍🤝‍🧑🏻"))
  (string-glyph-compose
   :eval (string-glyph-compose "Å"))
  (string-glyph-decompose
   :eval (string-glyph-decompose "Å"))
  "Predicates for Strings"
  (string-equal
   :eval (string-equal "abc" "abc")
   :eval (string-equal "abc" "ABC"))
  (string-equal-ignore-case
   :eval (string-equal-ignore-case "foo" "FOO"))
  (equal
   :eval (equal "foo" "foo"))
  (cl-equalp
   :eval (cl-equalp "Foo" "foo"))
  (stringp
   :eval (stringp "a")
   :eval (stringp 'a)
   :eval "(stringp ?a)")
  (string-or-null-p
   :eval (string-or-null-p "a")
   :eval (string-or-null-p nil))
  (char-or-string-p
   :eval "(char-or-string-p ?a)"
   :eval (char-or-string-p "a"))
  (string-empty-p
   :no-manual t
   :eval (string-empty-p ""))
  (string-blank-p
   :no-manual t
   :eval (string-blank-p " \n"))
  (string-lessp
   :eval (string-lessp "abc" "def")
   :eval (string-lessp "pic4.png" "pic32.png")
   :eval (string-lessp "1.1" "1.2"))
  (string-greaterp
   :eval (string-greaterp "foo" "bar"))
  (string-version-lessp
   :eval (string-version-lessp "pic4.png" "pic32.png")
   :eval (string-version-lessp "1.9.3" "1.10.2"))
  (string-collate-lessp
   :eval (string-collate-lessp "abc" "abd"))
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
   :eval (string-to-number "deadbeef" 16)
   :eval (string-to-number "2.5e+03"))
  (number-to-string
   :eval (number-to-string 42))
  (char-uppercase-p
   :eval "(char-uppercase-p ?A)"
   :eval "(char-uppercase-p ?a)")
  "Data About Strings"
  (length
   :eval (length "foo")
   :eval (length "avocado: 🥑"))
  (string-width
   :eval (string-width "foo")
   :eval (string-width "avocado: 🥑"))
  (string-pixel-width
   :eval (string-pixel-width "foo")
   :eval (string-pixel-width "avocado: 🥑"))
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
  (file-name-split
   :eval (file-name-split "/tmp/foo")
   :eval (file-name-split "foo/bar"))
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
  (file-name-parent-directory
   :eval (file-name-parent-directory "/foo/bar")
   :eval (file-name-parent-directory "/foo/")
   :eval (file-name-parent-directory "foo/bar")
   :eval (file-name-parent-directory "foo"))
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
  (file-writable-p
   :no-eval (file-writable-p "/tmp/foo")
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
  (file-has-changed-p
   :no-eval (file-has-changed-p "/tmp/foo")
   :eg-result t)
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
   :eg-result ("/tmp/foo.png" "/tmp/zot.png")
   :no-eval (file-expand-wildcards "/*/foo.png")
   :eg-result ("/tmp/foo.png" "/var/foo.png"))
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
   :no-value (set-file-times "/tmp/foo"))
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
  (hash-table-contains-p
   :no-eval (hash-table-contains-p 'key table))
  (copy-hash-table
   :no-eval (copy-hash-table table)
   :result-string "#s(hash-table ...)")
  (hash-table-count
   :no-eval (hash-table-count table)
   :eg-result 15))

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
   :eval (ensure-list '(1 2 3))
   :eval (ensure-list '(1 . 2)))
  (ensure-proper-list
   :eval (ensure-proper-list "foo")
   :eval (ensure-proper-list '(1 2 3))
   :eval (ensure-proper-list '(1 . 2)))
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
  (take
   :eval (take 3 '(one two three four)))
  (ntake
   :eval (ntake 3 (list 'one 'two 'three 'four)))
  (take-while
   :eval (take-while #'numberp '(1 2 three 4 five)))
  (drop-while
   :eval (drop-while #'numberp '(1 2 three 4 five)))
  (any
   :eval (any #'symbolp '(1 2 three 4 five)))
  (all
   :eval (all #'symbolp '(one 2 three))
   :eval (all #'symbolp '(one two three)))
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
   :eval (delq 'a (list 'a 'b 'c 'd)))
  (delete
   :eval (delete 2 (list 1 2 3 4))
   :eval (delete "a" (list "a" "b" "c" "d")))
  (remq
   :eval (remq 'b '(a b c)))
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
  (seq-reduce
   :eval (seq-reduce #'+ '(1 2 3) 0))
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
   :eval (memq 'b '(a b c)))
  (memql
   :eval (memql 2.0 '(1.0 2.0 3.0)))
  (member
   :eval (member 2 '(1 2 3))
   :eval (member "b" '("a" "b" "c")))
  (member-ignore-case
   :eval (member-ignore-case "foo" '("bar" "Foo" "zot")))
  "Association Lists"
  (assoc
   :eval (assoc "b" '(("a" . 1) ("b" . 2))))
  (rassoc
   :eval (rassoc "b" '((1 . "a") (2 . "b"))))
  (assq
   :eval (assq 'b '((a . 1) (b . 2))))
  (rassq
   :eval (rassq 'b '((1 . a) (2 . b))))
  (assoc-string
   :eval (assoc-string "foo" '(("a" 1) (foo 2))))
  (alist-get
   :eval (alist-get 2 '((1 . a) (2 . b))))
  (assoc-default
   :eval (assoc-default 2 '((1 . a) (2 . b) #'=)))
  (copy-alist
   :eval (copy-alist '((1 . a) (2 . b))))
  (assoc-delete-all
   :eval (assoc-delete-all "b" (list '("a" . a) '("b" . b) '("b" . c))))
  (assq-delete-all
   :eval (assq-delete-all 2 (list '(1 . a) '(2 . b) '(2 . c))))
  (rassq-delete-all
   :eval (rassq-delete-all 'b (list '(1 . a) '(2 . b) '(2 . c))))
  "Property Lists"
  (plist-get
   :eval (plist-get '(a 1 b 2 c 3) 'b))
  (plist-put
   :no-eval (setq plist (plist-put plist 'd 4))
   :eg-result (a 1 b 2 c 3 d 4))
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

(define-short-documentation-group symbol
  "Making symbols"
  (intern
   :eval (intern "abc"))
  (intern-soft
   :eval (intern-soft "list")
   :eval (intern-soft "Phooey!"))
  (make-symbol
   :eval (make-symbol "abc"))
  (gensym
   :no-eval (gensym)
   :eg-result g37)
  "Comparing symbols"
  (eq
   :eval (eq 'abc 'abc)
   :eval (eq 'abc 'abd))
  (eql
   :eval (eql 'abc 'abc))
  (equal
   :eval (equal 'abc 'abc))
  "Name"
  (symbol-name
   :eval (symbol-name 'abc))
  "Obarrays"
  (obarray-make
   :eval (obarray-make))
  (obarrayp
   :eval (obarrayp (obarray-make))
   :eval (obarrayp nil))
  (unintern
   :no-eval (unintern "abc" my-obarray)
   :eg-result t)
  (mapatoms
   :no-eval (mapatoms (lambda (symbol) (print symbol)) my-obarray))
  (obarray-clear
   :no-eval (obarray-clear my-obarray)))

(define-short-documentation-group comparison
  "General-purpose"
  (eq
   :eval (eq 'a 'a)
   :eval "(eq ?A ?A)"
   :eval (let ((x (list 'a "b" '(c) 4 5.0)))
           (eq x x)))
  (eql
   :eval (eql 2 2)
   :eval (eql 2.0 2.0)
   :eval (eql 2.0 2))
  (equal
   :eval (equal "abc" "abc")
   :eval (equal 2.0 2.0)
   :eval (equal 2.0 2)
   :eval (equal '(a "b" (c) 4.0) '(a "b" (c) 4.0)))
  (cl-equalp
   :eval (cl-equalp 2 2.0)
   :eval (cl-equalp "ABC" "abc"))
  "Numeric"
  (=
   :args (number &rest numbers)
   :eval (= 2 2)
   :eval (= 2.0 2.0)
   :eval (= 2.0 2)
   :eval (= 4 4 4 4))
  (/=
   :eval (/= 4 4))
  (<
   :args (number &rest numbers)
   :eval (< 4 4)
   :eval (< 1 2 3))
  (<=
   :args (number &rest numbers)
   :eval (<= 4 4)
   :eval (<= 1 2 2 3))
  (>
   :args (number &rest numbers)
   :eval (> 4 4)
   :eval (> 3 2 1))
  (>=
   :args (number &rest numbers)
   :eval (>= 4 4)
   :eval (>= 3 2 2 1))
  "String"
  (string-equal
   :eval (string-equal "abc" "abc")
   :eval (string-equal "abc" "ABC"))
  (string-equal-ignore-case
   :eval (string-equal-ignore-case "abc" "ABC"))
  (string-lessp
   :eval (string-lessp "abc" "abd")
   :eval (string-lessp "abc" "abc")
   :eval (string-lessp "pic4.png" "pic32.png"))
  (string-greaterp
   :eval (string-greaterp "abd" "abc")
   :eval (string-greaterp "abc" "abc"))
  (string-version-lessp
   :eval (string-version-lessp "pic4.png" "pic32.png")
   :eval (string-version-lessp "1.9.3" "1.10.2"))
  (string-collate-lessp
   :eval (string-collate-lessp "abc" "abd")))

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
  (copy-tree
   :eval (copy-tree [1 (2 3) [4 5]] t))
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
   :eval (seq-some #'floatp '(1 2.0 3)))
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
  (seq-positions
   :eval (seq-positions '(a b c a d) 'a)
   :eval (seq-positions '(a b c a d) 'z)
   :eval (seq-positions '(11 5 7 12 9 15) 10 #'>=))
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
  (seq-doseq
   :no-eval (seq-doseq (a '("foo" "bar")) (insert a))
   :eg-result ("foo" "bar"))
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
  (seq-keep
   :eval (seq-keep #'car-safe '((1 2) 3 t (a . b))))
  (seq-remove
   :eval (seq-remove #'numberp '(1 2 c d 5)))
  (seq-remove-at-position
   :eval (seq-remove-at-position '(a b c d e) 3)
   :eval (seq-remove-at-position [a b c d e] 0))
  (seq-group-by
   :eval (seq-group-by #'natnump '(-1 2 3 -4 -5 6)))
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
  (seq-split
   :eval (seq-split [0 1 2 3 5] 2))
  (seq-take-while
   :eval (seq-take-while #'integerp [1 2 3.0 4]))
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
  (pos-bol
   :eval (pos-bol))
  (pos-eol
   :eval (pos-eol))
  (bolp
   :eval (bolp))
  (eolp
   :eval (eolp))
  (line-beginning-position
   :eval (line-beginning-position))
  (line-end-position
   :eval (line-end-position))
  (buffer-size
   :eval (buffer-size))
  (bobp
   :eval (bobp))
  (eobp
   :eval (eobp))
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
  (preceding-char
   :no-eval (preceding-char)
   :eg-result 38)
  (char-after
   :eval (char-after 45))
  (char-before
   :eval (char-before 13))
  (get-byte
   :no-eval (get-byte 45)
   :eg-result-string "#xff")
  "Altering Buffers"
  (delete-region
   :no-value (delete-region (point-min) (point-max)))
  (erase-buffer
   :no-value (erase-buffer))
  (delete-line
   :no-value (delete-line))
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
  (process-status
   :no-eval (process-status process)
   :eg-result exit)
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
   :eval (1+ 2)
   :eval (let ((x 2)) (1+ x) x))
  (1-
   :eval (1- 4)
   :eval (let ((x 4)) (1- x) x))
  (incf
   :eval (let ((x 2)) (incf x) x)
   :eval (let ((x 2)) (incf x 2) x))
  (decf
   :eval (let ((x 4)) (decf x) x)
   :eval (let ((x 4)) (decf x 2)) x)
  "Predicates"
  (=
   :args (number &rest numbers)
   :eval (= 4 4)
   :eval (= 4.0 4.0)
   :eval (= 4 4.0)
   :eval (= 4 4 4 4))
  (eql
   :eval (eql 4 4)
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
   :eval (<= 1 2 2 3))
  (>
   :args (number &rest numbers)
   :eval (> 4 4)
   :eval (> 3 2 1))
  (>=
   :args (number &rest numbers)
   :eval (>= 4 4)
   :eval (>= 3 2 2 1))
  (zerop
   :eval (zerop 0))
  (natnump
   :eval (natnump -1)
   :eval (natnump 0)
   :eval (natnump 23))
  (plusp
   :eval (plusp 0)
   :eval (plusp 1))
  (minusp
   :eval (minusp 0)
   :eval (minusp -1))
  (oddp
   :eval (oddp 3))
  (evenp
   :eval (evenp 6))
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
   :eval (let ((s (copy-sequence "abc"))) (put-text-property 0 1 'foo t s) s)
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
   :no-eval (add-face-text-property START END '(:foreground "green")))
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

(define-short-documentation-group keymaps
  "Defining keymaps or adding bindings to existing keymaps"
  (define-keymap
    :no-eval (define-keymap "C-c C-c" #'quit-buffer)
    :no-eval (define-keymap :keymap ctl-x-map
               "C-r"      #'recentf-open
               "k"        #'kill-current-buffer))
  (defvar-keymap
      :no-eval (defvar-keymap my-keymap "C-c C-c" #'quit-buffer))
  "Setting keys"
  (keymap-set
   :no-eval (keymap-set map "C-c C-c" #'quit-buffer))
  (keymap-local-set
   :no-eval (keymap-local-set "C-c C-c" #'quit-buffer))
  (keymap-global-set
   :no-eval (keymap-global-set "C-c C-c" #'quit-buffer))
  (keymap-unset
   :no-eval (keymap-unset map "C-c C-c"))
  (keymap-local-unset
   :no-eval (keymap-local-unset "C-c C-c"))
  (keymap-global-unset
   :no-eval (keymap-global-unset "C-c C-c"))
  (keymap-substitute
   :no-eval (keymap-substitute map "C-c C-c" "M-a"))
  (keymap-set-after
   :no-eval (keymap-set-after map "<separator-2>" menu-bar-separator))
  "Predicates"
  (keymapp
   :eval (keymapp (define-keymap)))
  (key-valid-p
   :eval (key-valid-p "C-c C-c")
   :eval (key-valid-p "C-cC-c"))
  "Lookup"
  (keymap-lookup
   :eval (keymap-lookup (current-global-map) "C-x x g")))

(provide 'shortdoc-doc)

;;; shortdoc-doc.el ends here
