;;; tests-ispell.el --- Test ispell.el.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Lockywolf

;; Author: Lockywolf <for_emacs_1@lockywolf.net>
;; Keywords: languages, text

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for ispell.el.

;;; Code:

(require 'ispell)
(load (expand-file-name "test/lisp/textmodes/ispell-tests/ispell-tests-common.el" source-directory))


(defun warnings-buffer-exists-p ()
  "Check if a buffer named \"*Warnings*\" exists."
  (if (get-buffer "*Warnings*")
      t
    nil))

(ert-deftest ispell/ispell-program-name/nil ()
  "Sanity check.  Setting a non-string should produce a warning.
Give ispell-program-name a wrong type."
  (should (unwind-protect
              (progn
                (setq ispell-program-name "ispell")
                (when (warnings-buffer-exists-p)
                  (kill-buffer "*Warnings*"))
                (setopt ispell-program-name nil)
                (if (warnings-buffer-exists-p)
                    t
                  nil))
            (when (warnings-buffer-exists-p)
              (kill-buffer "*Warnings*")))))

(ert-deftest ispell/ispell-program-name/noncommand ()
  "Sanity check.  Should error or at least warn.
Give ispell-program-name a meaningless string."
  :expected-result :failed
  (should-error
   (setopt ispell-program-name "6c628ac4-63a0-11f0-b37c-e38fc166e3fc") ;; random ispellnonexistent name
   ))

(ert-deftest ispell/ispell-program-name/noncommand/interactive ()
  "Sanity check.  Should error or at least warn.
Give ispell-program-name a meaningless string."
  (should-error
   (progn
     (setopt ispell-program-name "6c628ac4-63a0-11f0-b37c-e38fc166e3fc") ;; random ispellnonexistent name
     (ispell-check-version)
     )))

(ert-deftest ispell/ispell-program-name/non-executable ()
  "Sanity check.  Should error or at least warn.
Give ispell-program-name a path to a non-executable.
I personally think that this should always fail, but
at the moment only an interactive call fails."
  :expected-result :failed
  (should-error
   (progn
     (setopt ispell-program-name null-device))))

(ert-deftest ispell/ispell-program-name/non-executable/interactive ()
  "Sanity check.  Should error or at least warn.
Give ispell-program-name a path to a non-executable."
  (should-error
   (progn
     (setopt ispell-program-name null-device)
     (ispell-check-version t))))

(ert-deftest ispell/ispell-program-name/non-spellchecker ()
  "Sanity check.  Give ispell-program-name a path to a non-spellchecker.
Fails because for non-interactive runs, `ispell-check-version' does
not actually err."
  :expected-result :failed
  (skip-unless (executable-find "etags"))
  (should-error (string-equal "etags" (setopt ispell-ispell-program "etags"))))

(ert-deftest ispell/ispell-program-name/non-spellchecker/interactive ()
  "Sanity check.  Give ispell-program-name a path to a non-spellchecker."
  (skip-unless (executable-find "etags"))
  (should-error
   (progn (setopt ispell-ispell-program "etags")
          (ispell-check-version t))
   ))

(ert-deftest ispell/ispell-program-name/ispell ()
  "Sanity check.  If at least some ispell is available, should pass.
Give ispell-program-name a real spellchecker"
  (skip-unless (and (executable-find "ispell")
                    (with-temp-buffer
                      (call-process "ispell" nil t nil "-vv")
                      (search-backward "Ispell"))))
  ;; should not throw
  (should (string-equal "ispell" (setopt ispell-ispell-program "ispell"))))

(ert-deftest ispell/ispell-with-safe-default-directory/bad ()
  "Try doing something with a bad default directory."
  (should (with-temp-buffer
            (let ((default-directory "c296752a-7d7b-4769-a2d4-4bfd96c7ca71"))
              (ispell-with-safe-default-directory
                (equal default-directory (expand-file-name "~/")))))))

(ert-deftest ispell/ispell-with-safe-default-directory/good ()
  "Try doing something with a bad default directory."
  (should (with-temp-buffer
            (let ((default-directory temporary-file-directory))
              (ispell-with-safe-default-directory
                (equal default-directory temporary-file-directory))))))

(ert-deftest ispell/ispell-call-process/simple ()
  "Check that ispell-call-process works.
This test fails, because HOME is not defined.
This should not be the case, because ispell-call-process
whould be making sure that the directory for running
the backend's process exists."
  :expected-result :failed
  (should
   (with-temp-buffer
     (let ((default-directory "86e44985-cfba-43ba-98dc-73be46addbc2"))
       (ispell-call-process "emacs" nil t nil '("--batch" "-Q" "--eval" "(progn (message default-directory) (kill-emacs))"))
       (search-backward (expand-file-name "~"))))))

(ert-deftest ispell/ispell-call-process/simple-writable ()
  "Check that ispell-call-process works."
  (should
   (with-temp-buffer
     (let ((default-directory temporary-file-directory))
       (ispell-call-process "emacs" nil t nil "--batch" "-Q" "--eval" "(message default-directory)")
       (search-backward (directory-file-name temporary-file-directory))))))

(ert-deftest ispell/ispell-call-process-region/cat-empty ()
  "Check ispell-call-process-region works with unrelated process.
This test is expected to fail, because at the moment, there is
a construction (let ((default-directory default-directory))...) in
the `ispell-with-safe-default-directory' function, which effectively
makes it useless."
  :expected-result :failed
  (should
   (with-temp-buffer
     (let* ((string-to-send "")
            (dir (concat temporary-file-directory
                         "86e44985-cfba-43ba-98dc-73be46addbc2")))
       (make-directory dir t)
       (chmod dir 000)
       (let ((default-directory dir))
         ;; (ispell-call-process-region string-to-send nil "cat" nil t nil)
         (ispell-call-process-region "emacs" nil t nil "--batch" "-Q" "--eval" "(progn (setq this-read (ignore-errors (read-from-minibuffer \"\"))) (message \"%s\" this-read))")
         ;; emacs --batch --eval '(progn (setq this-read (ignore-errors (read-from-minibuffer ""))) (message "%s" this-read))'
         (equal (buffer-string) string-to-send))))))

(ert-deftest ispell/ispell-call-process-region/cat-random ()
  "Check ispell-call-process-region works with unrelad process.
This test is expected to fail, because at the moment, there is
a construction (let ((default-directory default-directory))...) in
the `ispell-with-safe-default-directory' function, which effectively
makes it useless."
  :expected-result :failed
  (should
   (with-temp-buffer
     (let ((string-to-send (format "%s" (random)))
           (default-directory "86e44985-cfba-43ba-98dc-73be46addbc2"))
       (ispell-call-process-region "emacs" nil t nil "--batch" "-Q" "--eval" "(progn (setq this-read (ignore-errors (read-from-minibuffer \"\"))) (message \"%s\" this-read))")
       (equal (buffer-string) string-to-send)))))

(ert-deftest ispell/ispell-create-debug-buffer ()
  "Make sure that debug buffer creation works."
  (when (bufferp (get-buffer "*ispell-debug*"))
    (with-current-buffer "*ispell-debug*"
      (rename-buffer "*ispell-debug*-test")))
  (unwind-protect
      (progn
        (ispell-create-debug-buffer)
        (should (bufferp (get-buffer "*ispell-debug*")))
        (kill-buffer "*ispell-debug*") ;; should not error
        )
    (when (bufferp (get-buffer "*ispell-debug*-test"))
      (with-current-buffer "*ispell-debug*-test"
        (rename-buffer "*ispell-debug*"))))
  )

;; FIXME: this test should probably go into a separate file, dedicated
;; to the hunspell backend, but so far there is not partition between
;; backends, so let us add it here.  It is easy to move it.
(ert-deftest ispell/ispell-valid-dictionary-list/hunspell/no-library-directory ()
  "If hunspell, `ispell-valid-dictionary-list' returns default.
This function only works for aspell and ispell, for hunspell and
enchant-2 it always returns either default or everything.
I think this is an issue in itself, but this test is added to verify
that changes to third-party code do not break existing behaviour."
  (skip-unless (executable-find "hunspell"))
  (skip-unless (equal 0 (call-process "hunspell" nil nil nil)))
  (let ((old-ispell ispell-program-name)
        (old-library-directory ispell-library-directory))
    (unwind-protect
        (progn
          (setopt ispell-program-name "hunspell")
          (setopt ispell-library-directory nil)
          (ispell-check-version t)
          (should
           (equal
            (sort (ispell-valid-dictionary-list) 'string<)
            (sort (cl-substitute "default" nil (mapcar #'car ispell-dictionary-alist)) 'string<))))
      (setopt ispell-library-directory old-library-directory)
      (setopt ispell-program-name old-ispell)))
  )

;; FIXME: this test should probably go into a separate file, dedicated
;; to the hunspell backend, but so far there is not partition between
;; backends, so let us add it here.  It is easy to move it.
(ert-deftest ispell/ispell-valid-dictionary-list/hunspell/library-directory ()
  "If hunspell, `ispell-valid-dictionary-list' returns default.
This function only works for aspell and ispell, for hunspell and
enchant-2 it always returns either default or everything.
I think this is an issue in itself, but this test is added to verify
that changes to third-party code do not break existing behaviour."
  (skip-unless (executable-find "hunspell"))
  (skip-unless (equal 0 (call-process "hunspell" nil nil nil)))
  (let ((old-ispell ispell-program-name)
        (old-library-directory ispell-library-directory))
    (unwind-protect
        (progn
          (setopt ispell-program-name "hunspell")
          (ispell-check-version t)
          (setopt ispell-library-directory temporary-file-directory)
          (should
           (equal
            (ispell-valid-dictionary-list)
            '("default"))))
      (setopt ispell-library-directory old-library-directory)
      (setopt ispell-program-name old-ispell)))
  )

;; FIXME: this test should probably go into a separate file, dedicated
;; to the enchant-2 backend, but so far there is not partition between
;; backends, so let us add it here.  It is easy to move it.
(ert-deftest ispell/ispell-valid-dictionary-list/enchant-2/no-library-directory ()
  "If enchant-2, `ispell-valid-dictionary-list' returns default.
This function only works for aspell and ispell, for hunspell and
enchant-2 it always returns either default or everything.
I think this is an issue in itself, but this test is added to verify
that changes to third-party code do not break existing behaviour."
  (skip-unless (executable-find "enchant-2"))
  (let ((old-ispell ispell-program-name)
        (old-library-directory ispell-library-directory))
    (unwind-protect
        (progn
          (setopt ispell-program-name "enchant-2")
          (setopt ispell-library-directory nil)
          (ispell-check-version t)
          (should
           (equal
            (sort (ispell-valid-dictionary-list) 'string<)
            (sort (cl-substitute "default" nil (mapcar #'car ispell-dictionary-alist)) 'string<))))
      (setopt ispell-library-directory old-library-directory)
      (setopt ispell-program-name old-ispell)))
  )

;; FIXME: this test should probably go into a separate file, dedicated
;; to the enchant-2 backend, but so far there is not partition between
;; backends, so let us add it here.  It is easy to move it.
(ert-deftest ispell/ispell-valid-dictionary-list/enchant-2/library-directory ()
  "If enchant-2, `ispell-valid-dictionary-list' returns default.
This function only works for aspell and ispell, for hunspell and
enchant-2 it always returns either default or everything.
I think this is an issue in itself, but this test is added to verify
that changes to third-party code do not break existing behaviour."
  (skip-unless (executable-find "enchant-2"))
  (let ((old-ispell ispell-program-name)
        (old-library-directory ispell-library-directory))
    (unwind-protect
        (progn
          (setopt ispell-program-name "enchant-2")
          (setopt ispell-library-directory temporary-file-directory)
          (ispell-check-version t)
          (should
           (equal
            (ispell-valid-dictionary-list)
            '("default"))))
      (setopt ispell-library-directory old-library-directory)
      (setopt ispell-program-name old-ispell)))
  )

(ert-deftest ispell/ispell-valid-dictionary-list/international-ispell ()
  "Check that ispell-valid-dictionary-list does something useful for ispell.
For ispell, `ispell-valid-dictionary-list' checks that a corresponding
file is present in `ispell-library-directory'."
  (skip-unless (executable-find "ispell"))
  (skip-unless (let ((libdir (with-temp-buffer
                               (call-process "ispell" nil t nil "-vv")
                               (goto-char (point-min))
                               (when (re-search-forward
                                      "LIBDIR *= *\"\\([^\"]+\\)\"" nil t)
                                 (match-string 1)))))
                 (file-readable-p (expand-file-name "english.hash" libdir))))
  (let ((old-ispell ispell-program-name)
        (old-library-directory ispell-library-directory)
        (old-ispell-local-dictionary-alist ispell-local-dictionary-alist))
    (unwind-protect
        (progn
          (setopt ispell-program-name "ispell") ;; this should set ispell-library-directory
          (ispell-check-version t) ;; sets ispell-library-directory
          (should (not (null ispell-library-directory)))
          ;; english is always shipped with international ispell,
          ;; other languages not necessarily
          (setopt ispell-local-dictionary-alist
                  '(("english" "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil utf-8)))
          (should (member "english" (ispell-valid-dictionary-list)))
          (should (member "default" (ispell-valid-dictionary-list)))
          )
      (setopt ispell-library-directory old-library-directory)
      (setopt ispell-program-name old-ispell)
      (setopt ispell-local-dictionary-alist old-ispell-local-dictionary-alist))))

(ert-deftest ispell/ispell-valid-dictionary-list/aspell ()
  "Check that ispell-valid-dictionary-list does something useful for aspell.
For aspell, `ispell-valid-dictionary-list' computes an intersection of
`ispell-dictionary-alist' and `ispell--aspell-found-dictionaries'."
  (skip-unless (executable-find "aspell"))
  (skip-unless (with-temp-buffer
                 (call-process "aspell" nil t nil "dicts")
                 (> (length (buffer-string)) 2)))
  (let ((old-ispell ispell-program-name)
        (old-library-directory ispell-library-directory)
        (old-ispell-local-dictionary-alist ispell-local-dictionary-alist)
        (old-ispell-dictionary-alist ispell-dictionary-alist))
    (unwind-protect
        (progn
          (setopt ispell-program-name "aspell") ;; this should set ispell-library-directory
          (ispell-check-version t) ;; sets ispell-library-directory
          ;; english is always shipped with international ispell,
          ;; other languages not necessarily
          (setopt ispell-local-dictionary-alist
                  '(("english" "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-B") nil utf-8)))
          (should
           (> (length (ispell-valid-dictionary-list))
              (length ispell--aspell-found-dictionaries))))
      (setopt ispell-library-directory old-library-directory)
      (setopt ispell-dictionary-alist old-ispell-dictionary-alist)
      (setopt ispell-program-name old-ispell)
      (setopt ispell-local-dictionary-alist old-ispell-local-dictionary-alist))))

;; Adding file-local words into the file.  (They are _not_ sent to the
;; backend in this function.)

(ert-deftest ispell/ispell-add-per-file-word-list/simple ()
  "Adding a per-file word to an empty buffer.  No comment
syntax expected."
  (with-temp-buffer
    (let ((testword (format "%s" (random))))
      (ispell-add-per-file-word-list testword)
      (should (equal (buffer-string)
                     (concat "
" ispell-words-keyword " " testword "
"))))))

(ert-deftest ispell/ispell-add-per-file-word-list/comments ()
  "Adding a per-file word to an empty buffer.  Uses default
emacs-lisp comment syntax."
  (with-temp-buffer
    (let ((testword (format "%s" (random))))
      (emacs-lisp-mode)
      (ispell-add-per-file-word-list testword)
      (should (equal (buffer-string)
                     (concat "
; " ispell-words-keyword " " testword "
"))))))

(ert-deftest ispell/ispell-add-per-file-word-list/nxml ()
  "Adding a per-file word to an empty buffer.  Uses default
xml comment syntax, which has an opening and a closing
marker."
  (with-temp-buffer
    (let ((testword (format "%s" (random))))
      (nxml-mode)
      (ispell-add-per-file-word-list testword)
      (should (equal (buffer-string)
                     (concat "
<!-- " ispell-words-keyword " " testword "
-->
"))))))

(ert-deftest ispell/ispell-add-per-file-word-list/keyword-there-space ()
  "Adding a per-file word to buffer with keyword.  Uses default
xml comment syntax, which has an opening and a closing
marker. "
  (with-temp-buffer
    (let ((testword (format "%s" (random))))
      (nxml-mode)
      (insert "
<!-- " ispell-words-keyword "
-->
")
      (ispell-add-per-file-word-list testword)
      (should (equal (buffer-string)
                     (concat "
<!-- " ispell-words-keyword " " testword "
-->
"))))))

(ert-deftest ispell/ispell-add-per-file-word-list/longline ()
  "Adding a per-file word to buffer with keyword.  Uses default
xml comment syntax, which has an opening and a closing
marker.
This test fails, because ispell.el does not work well with
nXML comments."
  :expected-result :failed
  (letopt ((ispell-program-name "ispell"))
          (with-temp-buffer
            (let* ((testword (format "%s" (random)))
                   (fill-column 50))
              (nxml-mode)
              (insert "
<!-- " ispell-words-keyword (make-string fill-column ?a) "
-->
")
              (ispell-add-per-file-word-list testword)
              (should (equal (buffer-string)
                             (concat "
<!-- " ispell-words-keyword (make-string fill-column ?a) "
-->
"
"
<!-- " ispell-words-keyword " " testword "
-->
")))))))

;; Adding file-local words from the file's cellar into the backend
;; (@-prefixed, see *man ispell*).  (They _are_ sent to the backend in
;; this function.)

(ert-deftest ispell/ispell-buffer-local-words/ispell-words-keyword ()
  "Send some words prefixed by @ from the file's cellar to backend.
Should pass regardless of the backend and the dictionary, because
presumably nobody will have `hellooooooo' in their dictionary."
  (skip-unless (ispell-tests--some-backend-available-p))
  (letopt ((ispell-program-name (ispell-tests--some-backend)))
          (with-temp-buffer
            (nxml-mode)
            (ignore-errors (ispell-kill-ispell))
            (with-environment-variables (("HOME" temporary-file-directory))
              (ispell-init-process)
              (let ((test-output (ispell--run-on-word "hellooooooo")))
                (should (listp test-output))
                (should-not (equal t test-output)))
              (ispell-add-per-file-word-list "hellooooooo")
              (ispell-buffer-local-words)
              (should (equal t (ispell--run-on-word "hellooooooo")))))))


(ert-deftest
    ispell/ispell-buffer-local-words/ispell-buffer-session-localwords ()
  "Send some words prefixed by @ from the file's cellar to backend.
Should pass regardless of the backend and the dictionary, because
presumably nobody will have `hellooooooo' in their dictionary."
  (skip-unless (ispell-tests--some-backend-available-p))
  (letopt ((ispell-program-name (ispell-tests--some-backend)))
          (cd temporary-file-directory)
          (with-temp-buffer
            (nxml-mode)
            (ignore-errors (ispell-kill-ispell))
            (with-environment-variables (("HOME" temporary-file-directory))
              (ispell-init-process)
              (let ((test-output (ispell--run-on-word "hellooooooo")))
                (should (listp test-output))
                (should-not (equal t test-output)))
              (let ((ispell-buffer-session-localwords (list "hellooooooo")))
                (ispell-buffer-local-words)
                (should (equal t (ispell--run-on-word "hellooooooo"))))))))

(ert-deftest ispell/ispell-init-process/works-nohome ()
  "Simple test to check that ispell-init-process works."
  :expected-result :failed
  (skip-unless (ispell-tests--some-backend-available-p))
  (letopt ((ispell-program-name (ispell-tests--some-backend)))
          (with-temp-buffer
            (ispell-init-process))))

(ert-deftest ispell/ispell-init-process/works-withhome ()
  "Simple test to check that ispell-init-process works."
  (skip-unless (ispell-tests--some-backend-available-p))
  (letopt ((ispell-program-name (ispell-tests--some-backend)))
          (with-temp-buffer
            (with-environment-variables (("HOME" temporary-file-directory))
              (ispell-init-process)))))

;; Some more tests for buffer-local stuff.
;; `ispell-buffer-local-dict'
(let ((possible-pdict-paths (list "/tmp/lispellnonexistent.txt"
                                  "Q:\\ispellnonexistent\\ispellnonexistent.pdict"
                                  "https://example.text"
                                  "(my-favourite-function)"
                                  (format "%s" (random))
                                  (expand-file-name
                                   (format "%s" (random))
                                   temporary-file-directory))))
  (ert-deftest ispell/ispell-buffer-local-dict/no-reload+no-overriden ()
    "ispell.el can recognise keyword-defined dictionary and keyword-defined
personal-dictionary."
    (with-temp-buffer
      (nxml-mode)
      (let ((test-dict "ispellnonexistent"))
        (seq-map (lambda (test-pdict)
                   (insert
                    "hello\n\n\n"
                    "<!-- " ispell-dictionary-keyword test-dict " -->"
                    "<!-- " ispell-pdict-keyword test-pdict " -->")
                   (ispell-buffer-local-dict t)
                   (should (equal ispell-local-dictionary test-dict))
                   (should (equal ispell-local-pdict test-pdict)))
                 possible-pdict-paths))))

  (ert-deftest ispell/ispell-buffer-local-dict/reload+no-overriden ()
    "ispell.el can recognise keyword-defined dictionary and keyword-defined
personal-dictionary."
    :expected-result :failed
    (with-temp-buffer
      (nxml-mode)
      (let ((test-dict "ispellnonexistent"))
        (seq-map (lambda (test-pdict)
                   (insert
                    "hello\n\n\n"
                    "<!-- " ispell-dictionary-keyword test-dict " -->"
                    "<!-- " ispell-pdict-keyword test-pdict " -->")
                   (letopt ((ispell-current-dictionary "ispellnonexistent2"))
                     (ispell-buffer-local-dict)
                     (should (equal ispell-current-dictionary test-dict))
                     (should (equal ispell-current-personal-dictionary test-pdict))))
                 possible-pdict-paths))))

  (ert-deftest ispell/ispell-buffer-local-dict/no-reload+overriden ()
    "ispell.el can recognise keyword-defined dictionary and keyword-defined
personal-dictionary.  With no-reload it needs no backend at all."
    (with-temp-buffer
      (nxml-mode)
      (let ((test-dict "ispellnonexistent"))
        (seq-map (lambda (test-pdict)
                   (insert
                    "hello\n\n\n"
                    "<!-- " ispell-dictionary-keyword test-dict " -->"
                    "<!-- " ispell-pdict-keyword test-pdict " -->")
                   (letopt ((ispell-current-dictionary "ispellnonexistent2"))
                     (let ((ispell-local-dictionary-overridden t))
                       (ispell-buffer-local-dict t))
                     (should-not (equal ispell-local-dictionary test-dict))
                     (should (equal ispell-local-pdict test-pdict))))
                 possible-pdict-paths))))

  (ert-deftest ispell/ispell-buffer-local-dict/reload+overriden ()
    "ispell.el can recognise keyword-defined dictionary and keyword-defined
personal-dictionary.  With no-reload it needs no backend at all."
    :expected-result :failed
    (with-temp-buffer
      (nxml-mode)
      (let ((test-dict "ispellnonexistent"))
        (seq-map (lambda (test-pdict)
                   (insert
                    "hello\n\n\n"
                    "<!-- " ispell-dictionary-keyword test-dict " -->"
                    "<!-- " ispell-pdict-keyword test-pdict " -->")
                   (letopt ((ispell-current-dictionary "ispellnonexistent2"))
                     (let ((ispell-local-dictionary-overridden t))
                       (ispell-buffer-local-dict t))
                     (should-not (equal ispell-current-dictionary test-dict))
                     (should (equal ispell-current-personal-dictionary
                                    test-pdict))))
                 possible-pdict-paths)))))

;; parsing

(ert-deftest ispell/ispell-buffer-local-parsing/local-keyword ()
  "Check that ispell.el can suscessfully pick up a tex parser
from a buffer-local keyword."
  ;; FIXME: what if default dictionary sets
  ;; (ispell-get-extended-character-mode) ?
  (with-temp-buffer
    (let ((test-parser  "~tex")
          (test-dictname "testdict")
          (test-extcharmode "~latin3"))
      (letopt ((ispell-parser 'ispellnonexistent)
               (ispell-local-dictionary-alist
                `((,test-dictname "[A-Za-z]" "[^A-Za-z]" "[']"
                                  nil ("-B") ,test-extcharmode utf-8)))
               (ispell-current-dictionary test-dictname))

              (insert
               "hello\n\n\n" ispell-parsing-keyword test-parser)
              (let* ((counter 0))
                (cl-labels ((checker (s)
                              (setq counter (+ 1 counter))
                              (when (equal counter 1)
                                (should (string-equal s "!\n")))
                              (when (equal counter 2)
                                (should (string-equal s "-\n")))
                              (when (equal counter 3)
                                (should (string-equal s (concat test-extcharmode "\n"))))
                              (when (equal counter 4)
                                (should (string-equal s (concat test-parser "\n"))))
                              t))
                  (unwind-protect (progn
                                    (advice-add 'ispell-send-string :override
                                                #'checker)
                                    (let ((ispell-really-hunspell nil))
                                      (ispell-buffer-local-parsing)))
                    (advice-remove 'ispell-send-string #'checker)))))))
  )

(ert-deftest ispell/ispell-buffer-local-parsing/local-keyword/hunspell-bug ()
  "Check that ispell.el can suscessfully pick up a tex parser
from a buffer-local keyword."
  ;; FIXME: what if default dictionary sets
  ;; (ispell-get-extended-character-mode) ?
  :expected-result :failed
  (with-temp-buffer
    (let ((test-parser  "~tex")
          (test-dictname "testdict")
          (test-extcharmode "~latin3"))
      (letopt ((ispell-parser 'ispellnonexistent)
               (ispell-local-dictionary-alist
                `((,test-dictname "[A-Za-z]" "[^A-Za-z]" "[']"
                                  nil ("-B") ,test-extcharmode utf-8)))
               (ispell-current-dictionary test-dictname))

              (insert
               "hello\n\n\n" ispell-parsing-keyword test-parser)
              (let* ((counter 0))
                (cl-labels ((checker (s)
                              (setq counter (+ 1 counter))
                              (when (equal counter 1)
                                (should (string-equal s "!\n")))
                              (when (equal counter 2)
                                (should (string-equal s "-\n")))
                              (when (equal counter 3)
                                (should (string-equal s (concat test-extcharmode "\n"))))
                              (when (equal counter 4)
                                (should (string-equal s (concat test-parser "\n"))))
                              t))
                  (unwind-protect (progn
                                    (advice-add 'ispell-send-string :override
                                                #'checker)
                                    (let ((ispell-really-hunspell t))
                                      (ispell-buffer-local-parsing)))
                    (advice-remove 'ispell-send-string #'checker)))))))
  )

(ert-deftest ispell/ispell-buffer-local-parsing/mode-tex ()
  "Check that ispell.el can suscessfully pick up a tex parser
from tex-based mode-name.
There is another implicit check here: explicit-character-mode
(argument 7 from the ispell.el dictionary structure) is nil."
  (with-temp-buffer
    (let ((test-dictname "testdict")
          (test-extcharmode nil))
      (letopt ((ispell-check-comments t)
               (ispell-parser 'use-mode-name)
               (ispell-local-dictionary-alist
                `((,test-dictname "[A-Za-z]" "[^A-Za-z]" "[']"
                                  nil ("-B") ,test-extcharmode utf-8)))
               (ispell-current-dictionary test-dictname))
              (insert
               "hello\n\n\n")
              (tex-mode)
              (let* ((counter 0))
                (cl-labels ((checker (s)
                              (setq counter (+ 1 counter))
                              (when (equal counter 1)
                                (should (string-equal s "!\n")))
                              (when (equal counter 2)
                                (should (string-equal s "+\n")))
                              (when (equal counter 3)
                                (error "Should not have a third call to `ispell-send-string'"))
                              t))
                  (unwind-protect (progn
                                    (advice-add 'ispell-send-string :override
                                                #'checker)
                                    (ispell-buffer-local-parsing))
                    (advice-remove 'ispell-send-string #'checker)))))))
  )

(ert-deftest ispell/ispell-buffer-local-parsing/extended-character-mode ()
  "Check that ispell.el can suscessfully pick up an extended character
mode from the dictionary."
  (with-temp-buffer
    (insert
     "hello\n\n\n")
    (let ((test-extcharmode "~latin3"))
      (letopt ((ispell-check-comments t)
               (ispell-parser 'use-mode-name)
               ;; FIXME: what if default dictionary sets
               ;; (ispell-get-extended-character-mode)?
               (ispell-local-dictionary-alist
                `(("english" "[A-Za-z]" "[^A-Za-z]" "[']"
                   nil ("-B") ,test-extcharmode utf-8)))
               )
              (tex-mode)
              (let* ((counter 0))
                (cl-labels ((checker (s)
                              (setq counter (+ 1 counter))
                              (when (equal counter 1)
                                (should (string-equal s "!\n")))
                              (when (equal counter 2)
                                (should (string-equal s "+\n")))
                              (when (equal counter 3)
                                (should (string-equal s (concat test-extcharmode "\n"))))
                              (when (equal counter 4)
                                (error "Should not have a third call to `ispell-send-string'"))
                              t))
                  (unwind-protect (progn
                                    (advice-add 'ispell-send-string :override
                                                #'checker)
                                    (ispell-buffer-local-parsing))
                    (advice-remove 'ispell-send-string #'checker)))))))
  )

;; Let us now test the most important state-related function:
;; `ispell-accept-buffer-local-defs'.
;; Why is it important?
;; Because it is used in emacs' own CI for testing documentation
;; in checkdoc.
;; Indeed, when we are running the checker in batch mode,
;; we do not want to have any global state.


(ert-deftest ispell/ispell-accept-buffer-local-defs/simple ()
  "Check that `ispell-accept-buffer-local-defs' works for a
batch mode.
1. local words
2. dictionary and pdict
3. parser and extcharmode"
  (skip-unless (executable-find "ispell"))
  (setq old-engine ispell-program-name)
  (setopt ispell-program-name "ispell")
  (ispell-check-version t)
  (skip-unless (and (null ispell-really-aspell)
                    (null ispell-really-hunspell)
                    (null ispell-really-enchant)))
  (setq ispell-program-name old-engine)
  (with-environment-variables (("HOME" temporary-file-directory))
    (with-temp-buffer
      (letopt ((ispell-program-name "ispell"))
        (let ((test-dictname "english")
              (test-extcharmode "~latin3")
              (test-parser "~testparser")
              (test-localword1 "aaaaaaaaaaaaa")
              (test-localword2 "bbbbbbbbbbb")
              (test-pdict "test-pdict.pdict"))
          (insert
           "hello\n\n\n"
           ispell-dictionary-keyword test-dictname "\n"
           ispell-pdict-keyword (expand-file-name test-pdict temporary-file-directory) "\n"
           ispell-parsing-keyword test-parser "\n"
           ispell-words-keyword " " test-localword1 " " test-localword2 "\n")
          (letopt ((ispell-check-comments t)
                   (ispell-parser 'tex)
                   ;; FIXME: what if default dictionary sets
                   ;; (ispell-get-extended-character-mode)?
                   (ispell-local-dictionary-alist
                    `((,test-dictname "[A-Za-z]" "[^A-Za-z]" "[']"
                                      nil ("-B") ,test-extcharmode utf-8))))
            (tex-mode)
            (let* ((counter 0))
              (cl-labels ((checker-ispell-send-string (s)
                            (let ((references
                                   (list nil
                                         (concat test-extcharmode "\n")
                                         (concat "@" test-localword1 "\n")
                                         (concat "@" test-localword2 "\n")
                                         "!\n"
                                         "+\n"
                                         (concat test-extcharmode "\n")
                                         (concat test-parser "\n"))))
                              (setq counter (+ 1 counter))
                              (should (<= counter (length references)))
                              (should (string-equal
                                       (concat s)
                                       (concat (nth counter references))))
                              t)))
                (unwind-protect (progn
                                  (advice-add 'ispell-send-string :before
                                              #'checker-ispell-send-string)
                                  (ignore-errors (ispell-kill-ispell))
                                  (ispell-accept-buffer-local-defs)
                                  (should (equal ispell-local-dictionary test-dictname))
                                  (should (equal ispell-local-pdict (expand-file-name test-pdict temporary-file-directory)))
                                  )
                  (advice-remove 'ispell-send-string #'checker-ispell-send-string)))))))))
  )

(ert-deftest ispell/ispell--run-on-word/default ()
  "`ispell--run-on-word' should be the simplest interface
for checking a word."
  (skip-unless (ispell-tests--some-backend-available-p))
  (skip-unless (equal
                0
                (call-process (ispell-tests--some-backend) nil nil nil "-vv")))
  (letopt ((ispell-program-name (ispell-tests--some-backend))
           (ispell-dictionary "default"))
    (let ((default-directory temporary-file-directory))
      (with-temp-buffer
        (with-environment-variables (("HOME" temporary-file-directory))
          (nxml-mode)
          ;; t t kills regardless and clears buffer-local words
          (ignore-errors (ispell-kill-ispell t t))
          (ispell-init-process)

          (let ((test-output (ispell--run-on-word "hellooooooo")))
            (should (listp test-output))
            (should-not (equal t test-output))
            (setq ispell-filter nil)
            (setq ispell-filter-continue nil))

          (let ((test-output (ispell--run-on-word "hello")))
            (should-not (listp test-output))
            (should (equal t test-output))
            (setq ispell-filter nil)
            (setq ispell-filter-continue nil))

          (let ((test-output (ispell--run-on-word "fail")))
            (should-not (listp test-output))
            (should (equal t test-output))
            (setq ispell-filter nil)
            (setq ispell-filter-continue nil))

          (let ((test-output (ispell--run-on-word "tail")))
            (should-not (listp test-output))
            (should (equal t test-output))
            (setq ispell-filter nil)
            (setq ispell-filter-continue nil))
          ))))
  )

(ert-deftest ispell/ispell--run-on-word/default/fails ()
  "`ispell--run-on-word' should be the simplest interface
for checking a word.  This test fails due to what I consider
to be a bug.  I am quite convinced that `ispell--run-on-word'
should work twice in a row, without having to call
(`ispell-init-process') or (setq ispell-filter nil)
before each call.
"
  :expected-result :failed
  (skip-unless (ispell-tests--some-backend-available-p))
  (skip-unless (equal
                0
                (call-process (ispell-tests--some-backend) nil nil nil "-vv")))
  (letopt ((ispell-program-name (ispell-tests--some-backend))
           (ispell-dictionary "default"))
    (let ((default-directory temporary-file-directory))
      (with-temp-buffer
        (with-environment-variables (("HOME" temporary-file-directory))
          (nxml-mode)
          ;; t t kills regardless and clears buffer-local words
          (ignore-errors (ispell-kill-ispell t t))
          (ispell-init-process)

          (let ((test-output (ispell--run-on-word "hellooooooo")))
            (should (listp test-output))
            (should-not (equal t test-output)))

          (let ((test-output (ispell--run-on-word "hello")))
            (should-not (listp test-output))
            (should (equal t test-output)))

          ))))
  )

(ert-deftest ispell/ispell-word/default/check-only/correct ()
  "Check that `ispell-word' works with a default
dictionary, which we expect to be english, as
Ispell ships it.  This is probably wrong and should
be rewritten with a mock."
  (skip-unless (ispell-tests--some-backend-available-p))
  (skip-unless (equal
                0
                (call-process (ispell-tests--some-backend) nil nil nil "-vv")))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (letopt ((ispell-program-name (ispell-tests--some-backend)))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           "hello\n")
          (goto-char 0)
          (let ((ispell-check-only t)
                (current-point
                 (with-current-buffer "*Messages*"
                   (point))))
            (ispell-word)
            (with-current-buffer "*Messages*"
              (goto-char (point-max))
              (should ( > (search-backward "is correct" nil t)
                        current-point)))))))))

(ert-deftest ispell/ispell-word/default/check-only/correct/add-init ()
  "Check that `ispell-word' works with a default
dictionary, which we expect to be english, as
Ispell ships it.  This is probably wrong and should
be rewritten with a mock.
This test is different from the previous one in that an explicit init
call to (ispell-init-process) is added. I had issues with it, so I would
like to test it explicitly."
  (skip-unless (ispell-tests--some-backend-available-p))
  (skip-unless (equal
                0
                (call-process (ispell-tests--some-backend) nil nil nil "-vv")))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (letopt ((ispell-program-name (ispell-tests--some-backend)))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (ispell-init-process) ;; this is added
          (insert
           "hello\n")
          (goto-char 0)
          (let ((ispell-check-only t)
                (current-point
                 (with-current-buffer "*Messages*"
                   (point))))
            (ispell-word)
            (with-current-buffer "*Messages*"
              (goto-char (point-max))
              (should (> (search-backward "is correct" nil t)
                         current-point)))
            ))))))

(ert-deftest ispell/ispell-word/default/check-only/incorrect ()
  "Check that `ispell-word' works with a default
dictionary, which we expect to be english, as
Ispell ships it.  This is probably wrong and should
be rewritten with a mock.
This test gives it a word which does not exist."
  (skip-unless (ispell-tests--some-backend-available-p))
  (skip-unless (equal
                0
                (call-process (ispell-tests--some-backend) nil nil nil "-vv")))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let ((default-directory temporary-file-directory))
      (letopt ((ispell-program-name (ispell-tests--some-backend)))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           "helloooo\n")
          (goto-char 0)
          (let ((ispell-check-only t)
                (current-point
                 (with-current-buffer "*Messages*"
                   (point))))
            (ispell-word)
            (with-current-buffer "*Messages*"
              (goto-char (point-max))
              (should (> (search-backward "is incorrect" nil t)
                         current-point)))
            ))))))

(ert-deftest ispell/ispell-region/correct ()
  "The simplest test for `ispell-region'."
  (skip-unless (ispell-tests--some-backend-available-p))
  (skip-unless (equal
                0
                (call-process (ispell-tests--some-backend) nil nil nil "-vv")))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let* ((default-directory temporary-file-directory)
           (fake-aspell-path (expand-file-name
                                          "./fake-aspell-new.bash"
                                          tests-ispell-data-directory))
           (words '("hello" "test" "test" "more" "obvious" "word"))
           (text (string-join words " ")))
      (letopt ((ispell-program-name fake-aspell-path))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           text)
          (goto-char (length (nth 0 words)))
          (let (;(ispell-check-only t)
                (current-point
                 (with-current-buffer "*Messages*"
                   (point))))
            (ispell-region (point) (point-max))
            (with-current-buffer "*Messages*"
              (goto-char (point-max))
              (should (> (re-search-backward "Spell-checking region using .* with .* dictionary...done" nil t) current-point))
              'passed)
            )))))
  )

(ert-deftest ispell/ispell-region/incorrect ()
  "The simplest test for `ispell-region'."
  (skip-unless (ispell-tests--some-backend-available-p))
  (skip-unless (equal
                0
                (call-process (ispell-tests--some-backend) nil nil nil "-vv")))
  (with-environment-variables (("HOME" temporary-file-directory))
    (let* ((default-directory temporary-file-directory)
           (fake-aspell-path "aspell")
           (words '("hello" "tarampampamtararam" "world"))
           (text (string-join words " ")))
      (letopt ((ispell-program-name fake-aspell-path))
        (ignore-errors (ispell-kill-ispell t t))
        (with-temp-buffer
          (insert
           text)
          (goto-char (length (nth 0 words)))
          (cl-labels ((checker ()
                        (user-error "expected error")))
            (unwind-protect
                (progn
                  (advice-add 'ispell-show-choices :override
                              #'checker)
                  (should-error (ispell-region (point) (point-max)))
                  'passed)
              (advice-remove 'ispell-show-choices #'checker)))
          ))))
  )

(provide 'tests-ispell)
;;; tests-ispell.el ends here
