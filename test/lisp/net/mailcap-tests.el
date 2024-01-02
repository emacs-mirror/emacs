;;; mailcap-tests.el --- tests for mailcap.el -*- lexical-binding: t -*-

;; Copyright (C) 2017-2024 Free Software Foundation, Inc.

;; Author: Mark Oteiza <mvoteiza@udel.edu>

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

(require 'ert)
(require 'ert-x)
(require 'mailcap)

(defconst mailcap-tests-path (ert-resource-file "mime.types")
  "String used as PATH argument of `mailcap-parse-mimetypes'.")

(defconst mailcap-tests-mime-extensions (copy-alist mailcap-mime-extensions))

(defconst mailcap-tests-path-extensions
  '((".wav" . "audio/x-wav")
    (".flac" . "audio/flac")
    (".opus" . "audio/ogg"))
  "Alist of MIME associations in `mailcap-tests-path'.")

(ert-deftest mailcap-mimetypes-parsed-p ()
  (should (null mailcap-mimetypes-parsed-p)))

(ert-deftest mailcap-parse-empty-path ()
  "If PATH is empty, this should be a noop."
  (mailcap-parse-mimetypes "file/that/should/not/exist" t)
  (should mailcap-mimetypes-parsed-p)
  (should (equal mailcap-mime-extensions mailcap-tests-mime-extensions)))

(ert-deftest mailcap-parse-path ()
  (let ((mimetypes (getenv "MIMETYPES")))
    (unwind-protect
        (progn
          (setenv "MIMETYPES" mailcap-tests-path)
          (mailcap-parse-mimetypes nil t))
      (setenv "MIMETYPES" mimetypes)))
  (should (equal mailcap-mime-extensions
                 (append mailcap-tests-path-extensions
                         mailcap-tests-mime-extensions)))
  ;; Already parsed this, should be a noop
  (mailcap-parse-mimetypes mailcap-tests-path)
  (should (equal mailcap-mime-extensions
                 (append mailcap-tests-path-extensions
                         mailcap-tests-mime-extensions))))

(defmacro with-pristine-mailcap (&rest body)
  ;; We only want the mailcap info we define ourselves.
  `(let (mailcap--computed-mime-data
         mailcap-mime-data
         mailcap-user-mime-data)
     ;; `mailcap-mime-info' calls `mailcap-parse-mailcaps' which parses
     ;; the system's mailcaps.  We don't want that for our test.
     (cl-letf (((symbol-function 'mailcap-parse-mailcaps) #'ignore))
       ,@body)))

(ert-deftest mailcap-parsing-and-mailcap-mime-info ()
  (with-pristine-mailcap
   ;; One mailcap entry has a test=false field.  The shell command
   ;; execution errors when running the tests from the Makefile
   ;; because then HOME=/nonexistent.
   (ert-with-temp-directory home
     (with-environment-variables (("HOME" home))
       ;; Now parse our resource mailcap file.
       (mailcap-parse-mailcap (ert-resource-file "mailcap"))

       ;; Assert that we get what we have defined.
       (dolist (type '("audio/ogg" "audio/flac"))
         (should (string= "mpv %s" (mailcap-mime-info type))))
       (should (string= "aplay %s" (mailcap-mime-info "audio/x-wav")))
       (should (string= "emacsclient -t %s"
                        (mailcap-mime-info "text/plain")))
       ;; evince is chosen because acroread has test=false and okular
       ;; comes later.
       (should (string= "evince %s"
                        (mailcap-mime-info "application/pdf")))
       (should (string= "inkscape %s"
                        (mailcap-mime-info "image/svg+xml")))
       (should (string= "eog %s"
                        (mailcap-mime-info "image/jpg")))
       ;; With REQUEST being a number, all fields of the selected entry
       ;; should be returned.
       (should (equal '((viewer . "evince %s")
                        (type . "application/pdf"))
                      (mailcap-mime-info "application/pdf" 1)))
       ;; With 'all, all applicable entries should be returned.
       (should (equal '(((viewer . "evince %s")
                         (type . "application/pdf"))
                        ((viewer . "okular %s")
                         (type . "application/pdf")))
                      (mailcap-mime-info "application/pdf" 'all)))
       (let* ((c nil)
              (toggle (lambda (_) (setq c (not c)))))
         (mailcap-add "audio/ogg" "toggle %s" toggle)
         (should (string= "toggle %s" (mailcap-mime-info "audio/ogg")))
         ;; The test results are cached, so in order to have the test
         ;; re-evaluated, one needs to clear the cache.
         (setq mailcap-viewer-test-cache nil)
         (should (string= "mpv %s" (mailcap-mime-info "audio/ogg")))
         (setq mailcap-viewer-test-cache nil)
         (should (string= "toggle %s" (mailcap-mime-info "audio/ogg"))))))))

(defvar mailcap--test-result nil)
(defun mailcap--test-viewer ()
  (setq mailcap--test-result (string= (buffer-string) "test\n")))

(ert-deftest mailcap-view-file ()
  (with-pristine-mailcap
   ;; Try using a lambda as viewer and check whether
   ;; `mailcap-view-file' works correctly.
   (let* ((mailcap-mime-extensions '((".test" . "test/test"))))
     (mailcap-add "test/test" 'mailcap--test-viewer)
     (save-window-excursion
       (mailcap-view-file (ert-resource-file "test.test")))
     (should mailcap--test-result))))



(ert-deftest mailcap-add-mailcap-entry-new-major ()
  "Add a major entry not yet in ‘mailcap-mime-data’."
  (let ((mailcap-mime-data))

    ;; Add a new major entry to a empty ‘mailcap-mime-data’.
    (mailcap-add-mailcap-entry "major1" "minor1"
                               (list (cons 'viewer "viewer1"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major1"
                      ("minor1" . ((viewer . "viewer1")))))))

    ;; Add a new major entry to a non-empty ‘mailcap-mime-data’.
    (mailcap-add-mailcap-entry "major2" "minor2"
                               (list (cons 'viewer "viewer2"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major2"
                      ("minor2" . ((viewer . "viewer2"))))
                     ("major1"
                      ("minor1" . ((viewer . "viewer1"))))))))

  ;; Same spiel but with extra entries in INFO.
  (let ((mailcap-mime-data))
    ;; Add a new major entry to an empty ‘mailcap-mime-data’.
    (mailcap-add-mailcap-entry "major1" "minor1"
                               (list (cons 'viewer "viewer1")
                                     (cons 'print "print1"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major1"
                      ("minor1" . ((viewer . "viewer1")
                                   (print . "print1")))))))

    ;; Add a new major entry to a non-empty ‘mailcap-mime-data’.
    (mailcap-add-mailcap-entry "major2" "minor2"
                               (list (cons 'viewer "viewer2")
                                     (cons 'print "print2")
                                     (cons 'compose "compose2"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major2"
                      ("minor2" . ((viewer . "viewer2")
                                   (print . "print2")
                                   (compose . "compose2"))))
                     ("major1"
                      ("minor1" . ((viewer . "viewer1")
                                   (print . "print1")))))))))


(ert-deftest mailcap-add-mailcap-entry-new-minor-to-empty-major ()
  "Add a minor entry to a an empty major entry."
  (let ((mailcap-mime-data (list (list "major"))))
    (mailcap-add-mailcap-entry "major" "minor1"
                               (list (cons 'viewer "viewer1")
                                     (cons 'print "print1"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major"
                      ("minor1" . ((viewer . "viewer1")
                                   (print . "print1")))))))))

(ert-deftest mailcap-add-mailcap-entry-new-minor-to-non-empty-major ()
  "Add a minor to a major entry containing already minor entries."
  (let ((mailcap-mime-data
         (list
          (list "major"
                (list "minor1"
                      (cons 'viewer "viewer1")
                      (cons 'test "test1")
                      (cons 'print "print1"))))))

    (mailcap-add-mailcap-entry "major" "minor2"
                               (list (cons 'viewer "viewer2")
                                     (cons 'test "test2")
                                     (cons 'print "print2"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major"
                      ("minor2" . ((viewer . "viewer2")
                                   (test . "test2")
                                   (print . "print2")))
                      ("minor1" . ((viewer . "viewer1")
                                   (test . "test1")
                                   (print . "print1")))))))

    (mailcap-add-mailcap-entry "major" "minor3"
                               (list (cons 'viewer "viewer3")
                                     (cons 'test "test3")
                                     (cons 'compose "compose3"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major"
                      ("minor3" . ((viewer . "viewer3")
                                   (test . "test3")
                                   (compose . "compose3")))
                      ("minor2" . ((viewer . "viewer2")
                                   (test . "test2")
                                   (print . "print2")))
                      ("minor1" . ((viewer . "viewer1")
                                   (test . "test1")
                                   (print . "print1")))))))))

(ert-deftest mailcap-add-mailcap-entry-new-minor-to-various-major-positions ()
  "Add a new minor entry to major entries at various positions
in ‘mailcap-mime-data’."
  (let ((mailcap-mime-data
         (list
          (list "major1"
                (list "minor1.1"
                      (cons 'viewer "viewer1.1")
                      (cons 'print "print1.1")))
          (list "major2"
                (list "minor2.1"
                      (cons 'viewer "viewer2.1")
                      (cons 'print "print2.1")
                      (cons 'compose "compose2.1")))
          (list "major3"
                (list "minor3.1"
                      (cons 'viewer "viewer3.1")
                      (cons 'compose "compose3.1")))
          (list "major4"
                (list "minor4.1"
                      (cons 'viewer "viewer4.1")
                      (cons 'edit "edit4.1"))))))

    ;; Add a minor entry to a major mode at the front of
    ;; ‘mailcap-mime-data’.
    (mailcap-add-mailcap-entry "major1" "minor1.2"
                               (list (cons 'viewer "viewer1.2")
                                     (cons 'test "test1.2"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major1"
                      ("minor1.2" . ((viewer . "viewer1.2")
                                     (test . "test1.2")))
                      ("minor1.1" . ((viewer . "viewer1.1")
                                     (print . "print1.1"))))
                     ("major2"
                      ("minor2.1" . ((viewer . "viewer2.1")
                                     (print . "print2.1")
                                     (compose . "compose2.1"))))
                     ("major3"
                      ("minor3.1" . ((viewer . "viewer3.1")
                                     (compose . "compose3.1"))))
                     ("major4"
                      ("minor4.1" . ((viewer . "viewer4.1")
                                     (edit . "edit4.1")))))))

    ;; Add a minor entry to a major mode in the middle of
    ;; ‘mailcap-mime-data’.
    (mailcap-add-mailcap-entry "major3" "minor3.2"
                               (list (cons 'viewer "viewer3.2")
                                     (cons 'test "test3.2")
                                     (cons 'compose "compose3.2"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major1"
                      ("minor1.2" . ((viewer . "viewer1.2")
                                     (test . "test1.2")))
                      ("minor1.1" . ((viewer . "viewer1.1")
                                     (print . "print1.1"))))
                     ("major2"
                      ("minor2.1" . ((viewer . "viewer2.1")
                                     (print . "print2.1")
                                     (compose . "compose2.1"))))
                     ("major3"
                      ("minor3.2" . ((viewer . "viewer3.2")
                                     (test . "test3.2")
                                     (compose . "compose3.2")))
                      ("minor3.1" . ((viewer . "viewer3.1")
                                     (compose . "compose3.1"))))
                     ("major4"
                      ("minor4.1" . ((viewer . "viewer4.1")
                                     (edit . "edit4.1")))))))

    ;; Add a minor entry to a major mode at the end of
    ;; ‘mailcap-mime-data’.
    (mailcap-add-mailcap-entry "major4" "minor4.2"
                               (list (cons 'viewer "viewer4.2")
                                     (cons 'test "test4.2")
                                     (cons 'print "print4.2")
                                     (cons 'compose "compose4.2"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major1"
                      ("minor1.2" . ((viewer . "viewer1.2")
                                     (test . "test1.2")))
                      ("minor1.1" . ((viewer . "viewer1.1")
                                     (print . "print1.1"))))
                     ("major2"
                      ("minor2.1" . ((viewer . "viewer2.1")
                                     (print . "print2.1")
                                     (compose . "compose2.1"))))
                     ("major3"
                      ("minor3.2" . ((viewer . "viewer3.2")
                                     (test . "test3.2")
                                     (compose . "compose3.2")))
                      ("minor3.1" . ((viewer . "viewer3.1")
                                     (compose . "compose3.1"))))
                     ("major4"
                      ("minor4.2" . ((viewer . "viewer4.2")
                                     (test . "test4.2")
                                     (print . "print4.2")
                                     (compose . "compose4.2")))
                      ("minor4.1" . ((viewer . "viewer4.1")
                                     (edit . "edit4.1")))))))))

(ert-deftest mailcap-add-mailcap-entry-existing-with-test-differing-viewer ()
  "Add a new entry for an already existing major/minor entry."

  ;; The new and the existing entry have each a test info field.
  (let ((mailcap-mime-data
         (list
          (list "major"
                (list "minor"
                      (cons 'viewer "viewer1")
                      (cons 'test "test1")
                      (cons 'print "print1"))))))
    (mailcap-add-mailcap-entry "major" "minor"
                               (list (cons 'viewer "viewer2")
                                     (cons 'test "test2"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major"
                      ("minor" . ((viewer . "viewer2")
                                  (test . "test2")))
                      ("minor" . ((viewer . "viewer1")
                                  (test . "test1")
                                  (print . "print1"))))))))

  ;; Only the new entry has a test info field.
  (let ((mailcap-mime-data
         (list
          (list "major"
                (list "minor"
                      (cons 'viewer "viewer1")
                      (cons 'print "print1"))))))
    (mailcap-add-mailcap-entry "major" "minor"
                               (list (cons 'viewer "viewer2")
                                     (cons 'test "test2"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major"
                      ("minor" . ((viewer . "viewer2")
                                  (test . "test2")))
                      ("minor" . ((viewer . "viewer1")
                                  (print . "print1"))))))))

  ;; Only the existing entry has a test info field.
  (let ((mailcap-mime-data
         (list
          (list "major"
                (list "minor"
                      (cons 'viewer "viewer1")
                      (cons 'test "test1")
                      (cons 'print "print1"))))))
    (mailcap-add-mailcap-entry "major" "minor"
                               (list (cons 'viewer "viewer2"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major"
                      ("minor" . ((viewer . "viewer2")))
                      ("minor" . ((viewer . "viewer1")
                                  (test . "test1")
                                  (print . "print1")))))))))

(ert-deftest mailcap-add-mailcap-entry-existing-with-test-same-viewer ()
  "Add a new entry for an already existing major/minor entry."
  ;; Both the new and the existing entry have each a test info field.
  (let ((mailcap-mime-data
         (list
          (list "major"
                (list "minor"
                      (cons 'viewer "viewer")
                      (cons 'test "test1")
                      (cons 'print "print1"))))))
    (mailcap-add-mailcap-entry "major" "minor"
                               (list (cons 'viewer "viewer")
                                     (cons 'test "test2"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major"
                      ("minor" . ((viewer . "viewer")
                                  (test . "test2")))
                      ("minor" . ((viewer . "viewer")
                                  (test . "test1")
                                  (print . "print1"))))))))

  ;; Only the new entry has a test field.
  (let ((mailcap-mime-data
         (list
          (list "major"
                (list "minor"
                      (cons 'viewer "viewer")
                      (cons 'print "print1"))))))
    (mailcap-add-mailcap-entry "major" "minor"
                               (list (cons 'viewer "viewer")
                                     (cons 'test "test2"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major"
                      ("minor" . ((viewer . "viewer")
                                  (test . "test2")))
                      ("minor" . ((viewer . "viewer")
                                  (print . "print1"))))))))

  ;; Only the existing entry has a test info field.
  (let ((mailcap-mime-data
         (list
          (list "major"
                (list "minor"
                      (cons 'viewer "viewer")
                      (cons 'test "test1")
                      (cons 'print "print1"))))))
    (mailcap-add-mailcap-entry "major" "minor"
                               (list (cons 'viewer "viewer"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major"
                      ("minor" . ((viewer . "viewer")))
                      ("minor" . ((viewer . "viewer")
                                  (test . "test1")
                                  (print . "print1")))))))))

(ert-deftest mailcap-add-mailcap-entry-existing-without-test-differing-viewer ()
  "Add a new entry for an already existing major/minor entry."
  ;; Both entries do not have test fields.
  (let ((mailcap-mime-data
        (list
         (list "major"
               (list "minor"
                     (cons 'viewer "viewer1")
                     (cons 'print "print1"))))))
    (mailcap-add-mailcap-entry "major" "minor"
                               (list (cons 'viewer "viewer2")
                                     (cons 'compose "print2"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major"
                      ("minor" . ((viewer . "viewer2")
                                     (compose . "print2")))
                      ("minor" . ((viewer . "viewer1")
                                  (print . "print1")))))))))

(ert-deftest mailcap-add-mailcap-entry-simple-merge ()
  "Merge entries without tests (no extra info fields in the existing entry)."
  (let ((mailcap-mime-data
        (list
         (list "major"
               (list "minor"
                     (cons 'viewer "viewer"))))))
    (mailcap-add-mailcap-entry "major" "minor"
                               (list (cons 'viewer "viewer"))
                               'mailcap-mime-data)
    (should (equal mailcap-mime-data
                   '(("major"
                      ("minor" . ((viewer . "viewer"))))))))

  (let ((mailcap-mime-data
        (list
         (list "major"
               (list "minor"
                     (cons 'viewer "viewer"))))))
    (mailcap-add-mailcap-entry "major" "minor"
                               (list (cons 'viewer "viewer")
                                     (cons 'print "print"))
                               'mailcap-mime-data)

    (should (equal mailcap-mime-data
                   '(("major"
                      ("minor" . ((viewer . "viewer")
                                  (print . "print")))))))))

(ert-deftest mailcap-add-mailcap-entry-erroneous-merge ()
  "Merge entries without tests (extra info fields in existing entry).

In its current implementation ‘mailcap-add-mailcap-entry’ loses
extra fields of an entry already existing in ‘mailcap-mime-data’.
This test does not actually verify a correct result; it merely
checks whether ‘mailcap-add-mailcap-entry’ behavior is still the
incorrect one.  As such, it can be satisfied by any other result
than the expected and known wrong one, and its success does not
help to verify the correct addition and merging of an entry."
  :expected-result :failed

  (let ((mailcap-mime-data
        (list
         (list "major"
               (list "minor"
                     (cons 'viewer "viewer")
                     (cons 'print "print"))))))
    (mailcap-add-mailcap-entry "major" "minor"
                               (list (cons 'viewer "viewer")
                                     (cons 'edit "edit"))
                               'mailcap-mime-data)
    ;; Has the print field been lost?
    (should-not (equal mailcap-mime-data
                       '(("major"
                          ("minor" . ((viewer . "viewer")
                                      (edit . "edit")))))))))


;;; mailcap-tests.el ends here
