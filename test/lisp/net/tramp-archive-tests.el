;;; tramp-archive-tests.el --- Tests of file archive access  -*- lexical-binding:t -*-

;; Copyright (C) 2017-2024 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A testsuite for testing file archives.

;;; Code:

;; The `tramp-archive-testnn-*' tests correspond to the respective
;; tests in tramp-tests.el.

(require 'ert)
(require 'ert-x)
(require 'tramp-archive)
(defvar tramp-persistency-file-name)

;; `ert-resource-file' was introduced in Emacs 28.1.
(unless (macrop 'ert-resource-file)
  (eval-and-compile
    (defvar ert-resource-directory-format "%s-resources/"
      "Format for `ert-resource-directory'.")
    (defvar ert-resource-directory-trim-left-regexp ""
      "Regexp for `string-trim' (left) used by `ert-resource-directory'.")
    (defvar ert-resource-directory-trim-right-regexp
      (rx (? "-test" (? "s")) ".el")
      "Regexp for `string-trim' (right) used by `ert-resource-directory'.")

    (defmacro ert-resource-directory ()
      "Return absolute file name of the resource directory for this file.

The path to the resource directory is the \"resources\" directory
in the same directory as the test file.

If that directory doesn't exist, use the directory named like the
test file but formatted by `ert-resource-directory-format' and trimmed
using `string-trim' with arguments
`ert-resource-directory-trim-left-regexp' and
`ert-resource-directory-trim-right-regexp'.  The default values mean
that if called from a test file named \"foo-tests.el\", return
the absolute file name for \"foo-resources\"."
      `(let* ((testfile ,(or (bound-and-true-p byte-compile-current-file)
                             (and load-in-progress load-file-name)
                             buffer-file-name))
              (default-directory (file-name-directory testfile)))
	 (file-truename
	  (if (file-accessible-directory-p "resources/")
              (expand-file-name "resources/")
            (expand-file-name
             (format
	      ert-resource-directory-format
              (string-trim testfile
			   ert-resource-directory-trim-left-regexp
			   ert-resource-directory-trim-right-regexp)))))))

    (defmacro ert-resource-file (file)
      "Return file name of resource file named FILE.
A resource file is in the resource directory as per
`ert-resource-directory'."
      `(expand-file-name ,file (ert-resource-directory)))))

(defvar tramp-archive-test-file-archive (ert-resource-file "foo.tar.gz")
  "The test file archive.")

(defun tramp-archive-test-file-archive-hexlified ()
    "Return hexlified `tramp-archive-test-file-archive'.
Do not hexlify \"/\".  This hexlified string is used in `file:///' URLs."
  (let* ((url-unreserved-chars (cons ?/ url-unreserved-chars)))
    (url-hexify-string tramp-archive-test-file-archive)))

(defvar tramp-archive-test-archive
  (file-name-as-directory tramp-archive-test-file-archive)
  "The test archive.")

(defconst tramp-archive-test-directory
  (file-truename (ert-resource-file "foo.iso"))
  "A directory file name, which looks like an archive.")

(setq password-cache-expiry nil
      tramp-cache-read-persistent-data t ;; For auth-sources.
      tramp-persistency-file-name nil
      tramp-verbose 0)

(defun tramp-archive--test-make-temp-name ()
  "Return a temporary file name for test.
The temporary file is not created."
  (expand-file-name
   (make-temp-name "tramp-archive-test") temporary-file-directory))

(defun tramp-archive--test-delete (tmpfile)
  "Delete temporary file or directory TMPFILE.
This needs special support, because archive file names, which are
the origin of the temporary TMPFILE, have no write permissions."
  (unless (file-writable-p (file-name-directory tmpfile))
    (set-file-modes
     (file-name-directory tmpfile)
     (logior (file-modes (file-name-directory tmpfile)) #o0700)))
  (set-file-modes tmpfile #o0700)
  (if (file-regular-p tmpfile)
      (delete-file tmpfile)
    (mapc
     #'tramp-archive--test-delete
     (directory-files tmpfile 'full directory-files-no-dot-files-regexp))
    (delete-directory tmpfile)))

(defun tramp-archive--test-emacs28-p ()
  "Check for Emacs version >= 28.1.
Some semantics has been changed for there, without new functions or
variables, so we check the Emacs version directly."
  (>= emacs-major-version 28))

(ert-deftest tramp-archive-test00-availability ()
  "Test availability of archive file name functions."
  :expected-result (if tramp-archive-enabled :passed :failed)
  (should
   (and
    tramp-archive-enabled
    (file-exists-p tramp-archive-test-file-archive)
    (tramp-archive-file-name-p tramp-archive-test-archive))))

(ert-deftest tramp-archive-test01-file-name-syntax ()
  "Check archive file name syntax."
  (should-not (tramp-archive-file-name-p tramp-archive-test-file-archive))
  (should (tramp-archive-file-name-p tramp-archive-test-archive))
  (should
   (string-equal
    (tramp-archive-file-name-archive tramp-archive-test-archive)
    tramp-archive-test-file-archive))
  (should
   (string-equal
    (tramp-archive-file-name-localname tramp-archive-test-archive) "/"))
  (should (tramp-archive-file-name-p (concat tramp-archive-test-archive "foo")))
  (should
   (string-equal
    (tramp-archive-file-name-localname
     (concat tramp-archive-test-archive "foo"))
    "/foo"))
  (should
   (tramp-archive-file-name-p (concat tramp-archive-test-archive "foo/bar")))
  (should
   (string-equal
    (tramp-archive-file-name-localname
     (concat tramp-archive-test-archive "foo/bar"))
    "/foo/bar"))
  ;; A file archive inside a file archive.
  (should
   (tramp-archive-file-name-p (concat tramp-archive-test-archive "baz.tar")))
  (should
   (string-equal
    (tramp-archive-file-name-archive
     (concat tramp-archive-test-archive "baz.tar"))
    tramp-archive-test-file-archive))
  (should
   (string-equal
    (tramp-archive-file-name-localname
     (concat tramp-archive-test-archive "baz.tar"))
    "/baz.tar"))
  (should
   (tramp-archive-file-name-p (concat tramp-archive-test-archive "baz.tar/")))
  (should
   (string-equal
    (tramp-archive-file-name-archive
     (concat tramp-archive-test-archive "baz.tar/"))
    (concat tramp-archive-test-archive "baz.tar")))
  (should
   (string-equal
    (tramp-archive-file-name-localname
     (concat tramp-archive-test-archive "baz.tar/"))
    "/")))

(ert-deftest tramp-archive-test02-file-name-dissect ()
  "Check archive file name components."
  (skip-unless tramp-archive-enabled)

  ;; Suppress method name check.
  (let ((non-essential t))
    (with-parsed-tramp-archive-file-name tramp-archive-test-archive nil
      (should (string-equal method tramp-archive-method))
      (should-not user)
      (should-not domain)
      (should
       (string-equal
	host
	(file-remote-p
	 (tramp-archive-gvfs-file-name tramp-archive-test-archive) 'host)))
      (should
       (string-equal
	host
	(url-hexify-string
	 (concat "file://" (tramp-archive-test-file-archive-hexlified)))))
      (should-not port)
      (should (string-equal localname "/"))
      (should (string-equal archive tramp-archive-test-file-archive)))

    ;; Localname.
    (with-parsed-tramp-archive-file-name
	(concat tramp-archive-test-archive "foo") nil
      (should (string-equal method tramp-archive-method))
      (should-not user)
      (should-not domain)
      (should
       (string-equal
	host
	(file-remote-p
	 (tramp-archive-gvfs-file-name tramp-archive-test-archive) 'host)))
      (should
       (string-equal
	host
	(url-hexify-string
	 (concat "file://" (tramp-archive-test-file-archive-hexlified)))))
      (should-not port)
      (should (string-equal localname "/foo"))
      (should (string-equal archive tramp-archive-test-file-archive)))

    ;; File archive in file archive.
    (let* ((tramp-archive-test-file-archive
	    (concat tramp-archive-test-archive "baz.tar"))
	   (tramp-archive-test-archive
	    (file-name-as-directory tramp-archive-test-file-archive))
	   (tramp-methods (cons `(,tramp-archive-method) tramp-methods))
	   (tramp-gvfs-methods tramp-archive-all-gvfs-methods))
      (unwind-protect
	  (with-parsed-tramp-archive-file-name
	      (expand-file-name "bar" tramp-archive-test-archive) nil
	    (should (string-equal method tramp-archive-method))
	    (should-not user)
	    (should-not domain)
	    (should
	     (string-equal
	      host
	      (file-remote-p
	       (tramp-archive-gvfs-file-name tramp-archive-test-archive)
	       'host)))
	    ;; We reimplement the logic of tramp-archive.el here.
	    ;; Don't know, whether it is worth the test.
	    (should
	     (string-equal
	      host
	      (url-hexify-string
	       (concat
		(tramp-gvfs-url-file-name
		 (tramp-make-tramp-file-name
		  (make-tramp-file-name
		   :method tramp-archive-method
		   :host
		   (url-hexify-string
		    (concat
		     "file://"
		     ;; `directory-file-name' does not leave file
		     ;; archive boundaries.  So we must cut the
		     ;; trailing slash ourselves.
		     (substring
		      (file-name-directory
		       (tramp-archive-test-file-archive-hexlified))
		      0 -1)))
		   :localname "/")))
		(file-name-nondirectory tramp-archive-test-file-archive)))))
	    (should-not port)
	    (should (string-equal localname "/bar"))
	    (should (string-equal archive tramp-archive-test-file-archive)))

	;; Cleanup.
	(tramp-archive-cleanup-hash)))))

(ert-deftest tramp-archive-test05-expand-file-name ()
  "Check `expand-file-name'."
  (should
   (string-equal
    (expand-file-name (concat tramp-archive-test-archive "path/./file"))
    (concat tramp-archive-test-archive "path/file")))
  (should
   (string-equal
    (expand-file-name (concat tramp-archive-test-archive "path/../file"))
    (concat tramp-archive-test-archive "file")))
  ;; `expand-file-name' does not care "~/" in archive file names.
  (should
   (string-equal
    (expand-file-name (concat tramp-archive-test-archive "~/file"))
    (concat tramp-archive-test-archive "~/file")))
  ;; `expand-file-name' does not care file archive boundaries.
  (should
   (string-equal
    (expand-file-name (concat tramp-archive-test-archive "./file"))
    (concat tramp-archive-test-archive "file")))
  (should
   (string-equal
    (expand-file-name (concat tramp-archive-test-archive "../file"))
    (concat (ert-resource-directory) "file"))))

;; This test is inspired by Bug#30293.
(ert-deftest tramp-archive-test05-expand-file-name-non-archive-directory ()
  "Check existing directories with archive file name syntax.
They shall still be supported"
  (should (file-directory-p tramp-archive-test-directory))
  ;; `tramp-archive-file-name-p' tests only for file name syntax.  It
  ;; doesn't test, whether it is really a file archive.
  (should
   (tramp-archive-file-name-p
    (file-name-as-directory tramp-archive-test-directory)))
  (should
   (file-directory-p (file-name-as-directory tramp-archive-test-directory)))
  (should
   (file-exists-p (expand-file-name "foo" tramp-archive-test-directory))))

(ert-deftest tramp-archive-test06-directory-file-name ()
  "Check `directory-file-name'.
This checks also `file-name-as-directory', `file-name-directory',
`file-name-nondirectory' and `unhandled-file-name-directory'."
  (skip-unless tramp-archive-enabled)

  (should
   (string-equal
    (directory-file-name (concat tramp-archive-test-archive "path/to/file"))
    (concat tramp-archive-test-archive "path/to/file")))
  (should
   (string-equal
    (directory-file-name (concat tramp-archive-test-archive "path/to/file/"))
    (concat tramp-archive-test-archive "path/to/file")))
  ;; `directory-file-name' does not leave file archive boundaries.
  (should
   (string-equal
    (directory-file-name tramp-archive-test-archive) tramp-archive-test-archive))

  (should
   (string-equal
    (file-name-as-directory (concat tramp-archive-test-archive "path/to/file"))
    (concat tramp-archive-test-archive "path/to/file/")))
  (should
   (string-equal
    (file-name-as-directory (concat tramp-archive-test-archive "path/to/file/"))
    (concat tramp-archive-test-archive "path/to/file/")))
  (should
   (string-equal
    (file-name-as-directory tramp-archive-test-archive)
    tramp-archive-test-archive))
  (should
   (string-equal
    (file-name-as-directory tramp-archive-test-file-archive)
    tramp-archive-test-archive))

  (should
   (string-equal
    (file-name-directory (concat tramp-archive-test-archive "path/to/file"))
    (concat tramp-archive-test-archive "path/to/")))
  (should
   (string-equal
    (file-name-directory (concat tramp-archive-test-archive "path/to/file/"))
    (concat tramp-archive-test-archive "path/to/file/")))
  (should
   (string-equal
    (file-name-directory tramp-archive-test-archive) tramp-archive-test-archive))

  (should
   (string-equal
    (file-name-nondirectory (concat tramp-archive-test-archive "path/to/file"))
    "file"))
  (should
   (string-equal
    (file-name-nondirectory (concat tramp-archive-test-archive "path/to/file/"))
    ""))
  (should (string-equal (file-name-nondirectory tramp-archive-test-archive) ""))

  (should-not
   (unhandled-file-name-directory
    (concat tramp-archive-test-archive "path/to/file"))))

(ert-deftest tramp-archive-test07-file-exists-p ()
  "Check `file-exist-p', `write-region' and `delete-file'."
  :tags '(:expensive-test)
  (skip-unless tramp-archive-enabled)

  (unwind-protect
      (let ((default-directory tramp-archive-test-archive))
	(should (file-exists-p tramp-archive-test-file-archive))
	(should (file-exists-p tramp-archive-test-archive))
	(should (file-exists-p "foo.txt"))
	(should (file-exists-p "foo.lnk"))
	(should (file-exists-p "bar"))
	(should (file-exists-p "bar/bar"))
	(should-error
	 (write-region "foo" nil "baz")
	 :type 'file-error)
	(should-error
	 (delete-file "baz")
	 :type 'file-error))

    ;; Cleanup.
    (tramp-archive-cleanup-hash)))

(ert-deftest tramp-archive-test08-file-local-copy ()
  "Check `file-local-copy'."
  :tags '(:expensive-test)
  (skip-unless tramp-archive-enabled)

  (let (tmp-name)
    (unwind-protect
	(progn
	  (should
	   (setq tmp-name
		 (file-local-copy
		  (expand-file-name "bar/bar" tramp-archive-test-archive))))
	  (with-temp-buffer
	    (insert-file-contents tmp-name)
	    (should (string-equal (buffer-string) "bar\n")))
	    ;; Error case.
	    (tramp-archive--test-delete tmp-name)
	    (should-error
	     (setq tmp-name
		   (file-local-copy
		    (expand-file-name "what" tramp-archive-test-archive)))
	     :type 'file-missing))

      ;; Cleanup.
      (ignore-errors (tramp-archive--test-delete tmp-name))
      (tramp-archive-cleanup-hash))))

(ert-deftest tramp-archive-test09-insert-file-contents ()
  "Check `insert-file-contents'."
  :tags '(:expensive-test)
  (skip-unless tramp-archive-enabled)

  (let ((tmp-name (expand-file-name "bar/bar" tramp-archive-test-archive)))
    (unwind-protect
	(with-temp-buffer
	  (insert-file-contents tmp-name)
	  (should (string-equal (buffer-string) "bar\n"))
	  (insert-file-contents tmp-name)
	  (should (string-equal (buffer-string) "bar\nbar\n"))
	  ;; Insert partly.
	  (insert-file-contents tmp-name nil 1 3)
	  (should (string-equal (buffer-string) "arbar\nbar\n"))
	  ;; Replace.
	  (insert-file-contents tmp-name nil nil nil 'replace)
	  (should (string-equal (buffer-string) "bar\n"))
	  ;; Error case.
	  (should-error
	   (insert-file-contents
	    (expand-file-name "what" tramp-archive-test-archive))
	   :type 'file-missing))

	;; Cleanup.
	(tramp-archive-cleanup-hash))))

(ert-deftest tramp-archive-test11-copy-file ()
  "Check `copy-file'."
  :tags '(:expensive-test)
  (skip-unless tramp-archive-enabled)

  ;; Copy simple file.
  (let ((tmp-name1 (expand-file-name "bar/bar" tramp-archive-test-archive))
	(tmp-name2 (tramp-archive--test-make-temp-name)))
    (unwind-protect
	(progn
	  (copy-file tmp-name1 tmp-name2)
	  (should (file-exists-p tmp-name2))
	  (with-temp-buffer
	    (insert-file-contents tmp-name2)
	    (should (string-equal (buffer-string) "bar\n")))
	  (should-error
	   (copy-file tmp-name1 tmp-name2)
	   :type 'file-already-exists)
	  (copy-file tmp-name1 tmp-name2 'ok)
	  ;; The file archive is not writable.
	  (should-error
	   (copy-file tmp-name2 tmp-name1 'ok)
	   :type 'file-error))

      ;; Cleanup.
      (ignore-errors (tramp-archive--test-delete tmp-name2))
      (tramp-archive-cleanup-hash)))

  ;; Copy directory to existing directory.
  (let ((tmp-name1 (expand-file-name "bar" tramp-archive-test-archive))
	(tmp-name2 (tramp-archive--test-make-temp-name)))
    (unwind-protect
	(progn
	  (make-directory tmp-name2)
	  (should (file-directory-p tmp-name2))
	  ;; Directory `tmp-name2' exists already, so we must use
	  ;; `file-name-as-directory'.
	  (copy-file tmp-name1 (file-name-as-directory tmp-name2))
	  (should
	   (file-exists-p
	    (expand-file-name
	     (concat (file-name-nondirectory tmp-name1) "/bar") tmp-name2))))

      ;; Cleanup.
      (ignore-errors (tramp-archive--test-delete tmp-name2))
      (tramp-archive-cleanup-hash)))

  ;; Copy directory/file to non-existing directory.
  (let ((tmp-name1 (expand-file-name "bar" tramp-archive-test-archive))
	(tmp-name2 (tramp-archive--test-make-temp-name)))
    (unwind-protect
	(progn
	  (make-directory tmp-name2)
	  (should (file-directory-p tmp-name2))
	  (copy-file
	   tmp-name1
	   (expand-file-name (file-name-nondirectory tmp-name1) tmp-name2))
	  (should
	   (file-exists-p
	    (expand-file-name
	     (concat (file-name-nondirectory tmp-name1) "/bar") tmp-name2))))

      ;; Cleanup.
      (ignore-errors (tramp-archive--test-delete tmp-name2))
      (tramp-archive-cleanup-hash))))

(ert-deftest tramp-archive-test15-copy-directory ()
  "Check `copy-directory'."
  :tags '(:expensive-test)
  (skip-unless tramp-archive-enabled)

  (let* ((tmp-name1 (expand-file-name "bar" tramp-archive-test-archive))
	 (tmp-name2 (tramp-archive--test-make-temp-name))
	 (tmp-name3 (expand-file-name
		     (file-name-nondirectory tmp-name1) tmp-name2))
	 (tmp-name4 (expand-file-name "bar" tmp-name2))
	 (tmp-name5 (expand-file-name "bar" tmp-name3)))

    ;; Copy complete directory.
    (unwind-protect
	(progn
	  ;; Copy empty directory.
	  (copy-directory tmp-name1 tmp-name2)
	  (should (file-directory-p tmp-name2))
	  (should (file-exists-p tmp-name4))
	  ;; Target directory does exist already.
	  (should-error
	   (copy-directory tmp-name1 tmp-name2)
	   :type 'file-error)
	  (tramp-archive--test-delete tmp-name4)
	  (copy-directory tmp-name1 (file-name-as-directory tmp-name2))
	  (should (file-directory-p tmp-name3))
	  (should (file-exists-p tmp-name5)))

      ;; Cleanup.
      (ignore-errors (tramp-archive--test-delete tmp-name2))
      (tramp-archive-cleanup-hash))

    ;; Copy directory contents.
    (unwind-protect
        (progn
          ;; Copy empty directory.
          (copy-directory tmp-name1 tmp-name2 nil 'parents 'contents)
          (should (file-directory-p tmp-name2))
          (should (file-exists-p tmp-name4))
          ;; Target directory does exist already.
          (tramp-archive--test-delete tmp-name4)
          (copy-directory
           tmp-name1 (file-name-as-directory tmp-name2)
           nil 'parents 'contents)
          (should (file-directory-p tmp-name2))
          (should (file-exists-p tmp-name4))
          (should-not (file-directory-p tmp-name3))
          (should-not (file-exists-p tmp-name5)))

      ;; Cleanup.
      (ignore-errors (tramp-archive--test-delete tmp-name2))
      (tramp-archive-cleanup-hash))))

(ert-deftest tramp-archive-test16-directory-files ()
  "Check `directory-files'."
  :tags '(:expensive-test)
  (skip-unless tramp-archive-enabled)

  (let ((tmp-name tramp-archive-test-archive)
	(files '("." ".." "bar" "baz.tar" "foo.hrd" "foo.lnk" "foo.txt")))
    (unwind-protect
	(progn
	  (should (file-directory-p tmp-name))
	  (should (equal (directory-files tmp-name) files))
	  (should (equal (directory-files tmp-name 'full)
			 (mapcar (lambda (x) (concat tmp-name x)) files)))
	  (should (equal (directory-files
			  tmp-name nil directory-files-no-dot-files-regexp)
			 (remove "." (remove ".." files))))
	  (should (equal (directory-files
			  tmp-name 'full directory-files-no-dot-files-regexp)
			 (mapcar (lambda (x) (concat tmp-name x))
				 (remove "." (remove ".." files))))))

      ;; Cleanup.
      (tramp-archive-cleanup-hash))))

(ert-deftest tramp-archive-test17-insert-directory ()
  "Check `insert-directory'."
  :tags '(:expensive-test)
  (skip-unless tramp-archive-enabled)

  (let (;; We test for the summary line.  Keyword "total" could be localized.
	(process-environment
	 (append '("LANG=C" "LANGUAGE=C" "LC_ALL=C") process-environment)))
    (unwind-protect
	(progn
	  (with-temp-buffer
	    (insert-directory tramp-archive-test-archive nil)
	    (goto-char (point-min))
	    (should (looking-at-p (rx (literal tramp-archive-test-archive)))))
	  (with-temp-buffer
	    (insert-directory tramp-archive-test-archive "-al")
	    (goto-char (point-min))
	    (should
	     (looking-at-p
	      (rx bol (+ nonl) blank (literal tramp-archive-test-archive) eol))))
	  (with-temp-buffer
	    (insert-directory
	     (file-name-as-directory tramp-archive-test-archive)
	     "-al" nil 'full-directory-p)
	    (goto-char (point-min))
	    (should
	     (looking-at-p
	      (rx-to-string
	       `(:
		 ;; There might be a summary line.
		 (? "total" (+ nonl) (+ digit) (? blank)
		    (? (any "EGKMPTYZk")) (? "i") (? "B") "\n")
		 ;; We don't know in which order the files appear.
		 (= ,(length (directory-files tramp-archive-test-archive))
		    (+ nonl) blank
		    (regexp
		     ,(regexp-opt (directory-files tramp-archive-test-archive)))
		    (? " ->" (+ nonl)) "\n"))))))
	  ;; Check error case.
	  (with-temp-buffer
	    (should-error
	     (insert-directory
	      (expand-file-name "baz" tramp-archive-test-archive) nil)
	     :type 'file-missing)))

      ;; Cleanup.
      (tramp-archive-cleanup-hash))))

(ert-deftest tramp-archive-test18-file-attributes ()
  "Check `file-attributes'.
This tests also `access-file', `file-readable-p' and `file-regular-p'."
  :tags '(:expensive-test)
  (skip-unless tramp-archive-enabled)

  (let ((tmp-name1 (expand-file-name "foo.txt" tramp-archive-test-archive))
	(tmp-name2 (expand-file-name "foo.lnk" tramp-archive-test-archive))
	(tmp-name3 (expand-file-name "bar" tramp-archive-test-archive))
	(tmp-name4 (expand-file-name "baz" tramp-archive-test-archive))
	attr)
    (unwind-protect
	(progn
	  (should (file-exists-p tmp-name1))
	  (should (file-readable-p tmp-name1))
	  (should (file-regular-p tmp-name1))
	  (should-not (access-file tmp-name1 "error"))

	  ;; We do not test inodes and device numbers.
	  (setq attr (file-attributes tmp-name1))
	  (should (consp attr))
	  (should (null (car attr)))
	  (should (numberp (nth 1 attr))) ;; Link.
	  (should (numberp (nth 2 attr))) ;; Uid.
	  (should (numberp (nth 3 attr))) ;; Gid.
	  ;; Last access time.
	  (should (stringp (current-time-string (nth 4 attr))))
	  ;; Last modification time.
	  (should (stringp (current-time-string (nth 5 attr))))
	  ;; Last status change time.
	  (should (stringp (current-time-string (nth 6 attr))))
	  (should (numberp (nth 7 attr))) ;; Size.
	  (should (stringp (nth 8 attr))) ;; Modes.

	  (setq attr (file-attributes tmp-name1 'string))
	  (should (stringp (nth 2 attr))) ;; Uid.
	  (should (stringp (nth 3 attr))) ;; Gid.

	  ;; Symlink.
	  (should (file-exists-p tmp-name2))
	  (should (file-symlink-p tmp-name2))
	  (should (file-regular-p tmp-name2))
	  (setq attr (file-attributes tmp-name2))
	  (should (string-equal (car attr) (file-name-nondirectory tmp-name1)))

	  ;; Directory.
	  (should (file-exists-p tmp-name3))
	  (should (file-readable-p tmp-name3))
	  (should-not (file-regular-p tmp-name3))
	  (setq attr (file-attributes tmp-name3))
	  (should (eq (car attr) t))
	  (should-not (access-file tmp-name3 "error"))

	  ;; Check error case.
	  (should-error
	   (access-file tmp-name4  "error")
	   :type 'file-missing))

      ;; Cleanup.
      (tramp-archive-cleanup-hash))))

(ert-deftest tramp-archive-test19-directory-files-and-attributes ()
  "Check `directory-files-and-attributes'."
  :tags '(:expensive-test)
  (skip-unless tramp-archive-enabled)

  (let ((tmp-name (expand-file-name "bar" tramp-archive-test-archive))
	attr)
    (unwind-protect
	(progn
	  (should (file-directory-p tmp-name))
	  (setq attr (directory-files-and-attributes tmp-name))
	  (should (consp attr))
	  (dolist (elt attr)
	    (should
	     (equal (file-attributes (expand-file-name (car elt) tmp-name))
		    (cdr elt))))
	  (setq attr (directory-files-and-attributes tmp-name 'full))
	  (dolist (elt attr)
	    (should (equal (file-attributes (car elt)) (cdr elt))))
	  (setq attr (directory-files-and-attributes tmp-name nil (rx bos "b")))
	  (should (equal (mapcar #'car attr) '("bar"))))

      ;; Cleanup.
      (tramp-archive-cleanup-hash))))

(ert-deftest tramp-archive-test20-file-modes ()
  "Check `file-modes'.
This tests also `file-executable-p', `file-writable-p' and `set-file-modes'."
  :tags '(:expensive-test)
  (skip-unless tramp-archive-enabled)

  (let ((tmp-name1 (expand-file-name "foo.txt" tramp-archive-test-archive))
	(tmp-name2 (expand-file-name "bar" tramp-archive-test-archive)))
    (unwind-protect
	(progn
	  (should (file-exists-p tmp-name1))
	  ;; `set-file-modes' is not implemented.
	  (should-error
	   (set-file-modes tmp-name1 #o777)
	   :type 'file-error)
	  (should (= (file-modes tmp-name1) #o400))
	  (should-not (file-executable-p tmp-name1))
	  (should-not (file-writable-p tmp-name1))

	  (should (file-exists-p tmp-name2))
	  ;; `set-file-modes' is not implemented.
	  (should-error
	   (set-file-modes tmp-name2 #o777)
	   :type 'file-error)
	  (should (= (file-modes tmp-name2) #o500))
	  (should (file-executable-p tmp-name2))
	  (should-not (file-writable-p tmp-name2)))

      ;; Cleanup.
      (tramp-archive-cleanup-hash))))

(ert-deftest tramp-archive-test21-file-links ()
  "Check `file-symlink-p' and `file-truename'"
  :tags '(:expensive-test)
  (skip-unless tramp-archive-enabled)

  ;; We must use `file-truename' for the file archive, because it
  ;; could be located on a symlinked directory.  This would let the
  ;; test fail.
  (let* ((tramp-archive-test-archive (file-truename tramp-archive-test-archive))
	 (tmp-name1 (expand-file-name "foo.txt" tramp-archive-test-archive))
	 (tmp-name2 (expand-file-name "foo.lnk" tramp-archive-test-archive)))

    (unwind-protect
	(progn
	  (should (file-exists-p tmp-name1))
	  (should (file-regular-p tmp-name1))
	  (should (string-equal tmp-name1 (file-truename tmp-name1)))
	  ;; `make-symbolic-link' is not implemented.
	  (should-error
	   (make-symbolic-link tmp-name1 tmp-name2)
	   :type 'file-error)
	  (should (file-symlink-p tmp-name2))
	  (should (file-regular-p tmp-name2))
	  (should
	   (string-equal
	    ;; This is "/foo.txt".
	    (with-parsed-tramp-archive-file-name tmp-name1 nil localname)
	    ;; `file-symlink-p' returns "foo.txt".  Wer must expand, therefore.
	    (with-parsed-tramp-archive-file-name
		(expand-file-name
		 (file-symlink-p tmp-name2) tramp-archive-test-archive)
		nil
	      localname)))
	  (should-not (string-equal tmp-name2 (file-truename tmp-name2)))
	  (should
	   (string-equal (file-truename tmp-name1) (file-truename tmp-name2)))
	  (should (file-equal-p tmp-name1 tmp-name2)))

      ;; Cleanup.
      (tramp-archive-cleanup-hash))))

(ert-deftest tramp-archive-test26-file-name-completion ()
  "Check `file-name-completion' and `file-name-all-completions'."
  :tags '(:expensive-test)
  (skip-unless tramp-archive-enabled)

  (let ((tmp-name tramp-archive-test-archive))
    (unwind-protect
	(progn
	  ;; Local files.
	  (should (equal (file-name-completion "fo" tmp-name) "foo."))
	  (should (equal (file-name-completion "foo.txt" tmp-name) t))
	  (should (equal (file-name-completion "b" tmp-name) "ba"))
	  (should-not (file-name-completion "a" tmp-name))
	  (should
	   (equal
	    (file-name-completion "b" tmp-name #'file-directory-p) "bar/"))
	  (should
	   (equal
	    (sort (file-name-all-completions "fo" tmp-name) #'string-lessp)
	    '("foo.hrd" "foo.lnk" "foo.txt")))
	  (should
	   (equal
	    (sort (file-name-all-completions "b" tmp-name) #'string-lessp)
	    '("bar/" "baz.tar")))
	  (should-not (file-name-all-completions "a" tmp-name))
	  ;; `completion-regexp-list' restricts the completion to
	  ;; files which match all expressions in this list.
	  (let ((completion-regexp-list
		 `(,directory-files-no-dot-files-regexp "b")))
	    (should
	     (equal (file-name-completion "" tmp-name) "ba"))
	    (should
	     (equal
	      (sort (file-name-all-completions "" tmp-name) #'string-lessp)
	      '("bar/" "baz.tar")))))

      ;; Cleanup.
      (tramp-archive-cleanup-hash))))

(ert-deftest tramp-archive-test40-make-nearby-temp-file ()
  "Check `make-nearby-temp-file' and `temporary-file-directory'."
  (skip-unless tramp-archive-enabled)

  (let ((default-directory tramp-archive-test-archive)
	tmp-file)
    ;; The file archive shall know a temporary file directory.  It is
    ;; not in the archive itself.
    (should (stringp (temporary-file-directory)))
    (should-not (tramp-archive-file-name-p (temporary-file-directory)))

    ;; A temporary file or directory shall not be located in the
    ;; archive itself.
    (setq tmp-file (make-nearby-temp-file "tramp-archive-test"))
    (should (file-exists-p tmp-file))
    (should (file-regular-p tmp-file))
    (should-not (tramp-archive-file-name-p tmp-file))
    (delete-file tmp-file)
    (should-not (file-exists-p tmp-file))

    (setq tmp-file (make-nearby-temp-file "tramp-archive-test" 'dir))
    (should (file-exists-p tmp-file))
    (should (file-directory-p tmp-file))
    (should-not (tramp-archive-file-name-p tmp-file))
    (delete-directory tmp-file)
    (should-not (file-exists-p tmp-file))))

(ert-deftest tramp-archive-test43-file-system-info ()
  "Check that `file-system-info' returns proper values."
  (skip-unless tramp-archive-enabled)

  (let ((fsi (file-system-info tramp-archive-test-archive)))
    (skip-unless fsi)
    (should (and (consp fsi)
		 (tramp-compat-length= fsi 3)
		 (numberp (nth 0 fsi))
		 ;; FREE and AVAIL are always 0.
		 (zerop (nth 1 fsi))
		 (zerop (nth 2 fsi))))))

;; `file-user-uid' and `file-group-gid' were introduced in Emacs 30.1.
(ert-deftest tramp-archive-test44-user-group-ids ()
  "Check results of user/group functions.
`file-user-uid' and `file-group-gid' should return proper values."
  (skip-unless tramp-archive-enabled)
  (skip-unless (and (fboundp 'file-user-uid)
                    (fboundp 'file-group-gid)))

  ;; `file-user-uid' and `file-group-gid' exist since Emacs 30.1.
  ;; We don't want to see compiler warnings for older Emacsen.
  (let* ((default-directory tramp-archive-test-archive)
	 (uid (with-no-warnings (file-user-uid)))
	 (gid (with-no-warnings (file-group-gid))))
    (should (integerp uid))
    (should (integerp gid))
    (let ((default-directory tramp-archive-test-file-archive))
      (should (equal uid (with-no-warnings (file-user-uid))))
      (should (equal gid (with-no-warnings (file-group-gid)))))))

(ert-deftest tramp-archive-test48-auto-load ()
  "Check that `tramp-archive' autoloads properly."
  :tags '(:expensive-test)
  (skip-unless tramp-archive-enabled)

  ;; tramp-archive is neither loaded at Emacs startup, nor when
  ;; loading a file like "/mock::foo" (which loads Tramp).
  (let ((code
	 "(progn \
	    (message \"tramp-archive loaded: %%s\" \
              (featurep 'tramp-archive)) \
	    (let ((inhibit-message t)) \
              (file-attributes %S \"/\")) \
	    (message \"tramp-archive loaded: %%s\" \
              (featurep 'tramp-archive))))"))
    (dolist (enabled '(t nil))
      (dolist (default-directory
		(append
		 `(,temporary-file-directory)
		 ;;  Starting Emacs in a directory which has
		 ;; `tramp-archive-file-name-regexp' syntax is
		 ;; supported only with Emacs > 27.2 (sigh!).
		 ;; (Bug#48476)
                 (and (tramp-archive--test-emacs28-p)
		      `(,(file-name-as-directory
			  tramp-archive-test-directory)))))
	(dolist (file `("/mock::foo" ,(concat tramp-archive-test-archive "foo")))
          (should
           (string-match
	    (rx
	     "tramp-archive loaded: "
	     (literal (symbol-name
		       (tramp-archive-file-name-p default-directory)))
	     (+ ascii)
	     "tramp-archive loaded: "
	     (literal (symbol-name
		       (or (tramp-archive-file-name-p default-directory)
			   (and enabled (tramp-archive-file-name-p file))))))
	    (shell-command-to-string
	     (format
	      "%s -batch -Q -L %s --eval %s --eval %s"
	      (shell-quote-argument
	       (expand-file-name invocation-name invocation-directory))
	      (mapconcat #'shell-quote-argument load-path " -L ")
	      (shell-quote-argument
	       (format "(setq tramp-archive-enabled %s)" enabled))
	      (shell-quote-argument (format code file)))))))))))

(ert-deftest tramp-archive-test48-delay-load ()
  "Check that `tramp-archive' is loaded lazily, only when needed."
  :tags '(:expensive-test)
  (skip-unless tramp-archive-enabled)

  ;; tramp-archive is neither loaded at Emacs startup, nor when
  ;; loading a file like "/foo.tar".  It is loaded only when
  ;; `tramp-archive-enabled' is t.
  (let ((default-directory (expand-file-name temporary-file-directory))
	(code
	 "(progn \
            (setq tramp-archive-enabled %s) \
	    (message \"tramp-archive loaded: %%s\" \
              (featurep 'tramp-archive)) \
	    (file-attributes %S \"/\") \
	    (message \"tramp-archive loaded: %%s\" \
              (featurep 'tramp-archive)) \
	    (file-attributes %S \"/\") \
	    (message \"tramp-archive loaded: %%s\" \
              (featurep 'tramp-archive)))"))
    ;; tramp-archive doesn't load when `tramp-archive-enabled' is nil.
    (dolist (tae '(t nil))
      (should
       (string-match
	(rx
	 "tramp-archive loaded: nil" (+ ascii)
	 "tramp-archive loaded: nil" (+ ascii)
	 "tramp-archive loaded: " (literal (symbol-name tae)))
        (shell-command-to-string
         (format
	  "%s -batch -Q -L %s --eval %s"
	  (shell-quote-argument
	   (expand-file-name invocation-name invocation-directory))
	  (mapconcat #'shell-quote-argument load-path " -L ")
	  (shell-quote-argument
           (format
            code tae tramp-archive-test-file-archive
            (concat tramp-archive-test-archive "foo"))))))))))

(ert-deftest tramp-archive-test49-without-remote-files ()
  "Check that Tramp can be suppressed."
  (skip-unless tramp-archive-enabled)

  (should (file-exists-p tramp-archive-test-archive))
  (should-not (without-remote-files (file-exists-p tramp-archive-test-archive)))
  (should (file-exists-p tramp-archive-test-archive))

  (inhibit-remote-files)
  (should-not (file-exists-p tramp-archive-test-archive))
  (tramp-register-file-name-handlers)
  (setq tramp-mode t)
  (should (file-exists-p tramp-archive-test-archive)))

(ert-deftest tramp-archive-test99-libarchive-tests ()
  "Run tests of libarchive test files."
  :tags '(:expensive-test :unstable)
  (skip-unless tramp-archive-enabled)
  ;; We do not want to run unless chosen explicitly.  This test makes
  ;; sense only in my local environment.  Michael Albinus.
  (skip-unless
   (equal
    (ert--stats-selector ert--current-run-stats)
    (ert-test-name (ert-running-test))))

  (url-handler-mode)
  (unwind-protect
      (dolist (dir
	       '("~/Downloads" "/sftp::~/Downloads" "/ssh::~/Downloads"
		 "http://ftp.debian.org/debian/pool/main/c/coreutils"))
	(dolist
	    (file
	     '("coreutils_8.26-3_amd64.deb"
	       "coreutils_8.26-3ubuntu3_amd64.deb"))
	  (setq file (expand-file-name file dir))
	  (when (file-exists-p file)
	    (setq file (expand-file-name "control.tar.gz/control" file))
	    (message "%s" file)
	    (should (file-attributes (file-name-as-directory file))))))

    ;; Cleanup.
    (tramp-archive-cleanup-hash))

  (unwind-protect
      (dolist (dir '("" "/sftp::" "/ssh::"))
	(dolist
	    (file
	     (apply
	      'append
	      (mapcar
	       (lambda (x)
		 (directory-files (concat dir x) 'full (rx "uu" eos) 'sort))
	       '("~/src/libarchive-3.2.2/libarchive/test"
		 "~/src/libarchive-3.2.2/cpio/test"
		 "~/src/libarchive-3.2.2/tar/test"))))
	  (setq file (file-name-as-directory file))
	  (cond
	   ((not (tramp-archive-file-name-p file))
	    (message "skipped: %s" file))
	   ((file-attributes file)
	    (message "%s" file))
	   (t (message "failed: %s" file)))
	  (tramp-archive-cleanup-hash)))

    ;; Cleanup.
    (tramp-archive-cleanup-hash)))

(defun tramp-archive-test-all (&optional interactive)
  "Run all tests for \\[tramp-archive].
If INTERACTIVE is non-nil, the tests are run interactively."
  (interactive "p")
  (funcall
   (if interactive #'ert-run-tests-interactively #'ert-run-tests-batch)
   "^tramp-archive"))

(provide 'tramp-archive-tests)

;;; tramp-archive-tests.el ends here
