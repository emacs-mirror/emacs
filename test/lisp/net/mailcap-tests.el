;;; mailcap-tests.el --- tests for mailcap.el -*- lexical-binding: t -*-

;; Copyright (C) 2017-2022 Free Software Foundation, Inc.

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
   ;; Try using a lambda as viewer and check wether
   ;; `mailcap-view-file' works correctly.
   (let* ((mailcap-mime-extensions '((".test" . "test/test"))))
     (mailcap-add "test/test" 'mailcap--test-viewer)
     (save-window-excursion
       (mailcap-view-file (ert-resource-file "test.test")))
     (should mailcap--test-result))))

;;; mailcap-tests.el ends here
