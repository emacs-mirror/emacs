;;; shr-tests.el --- tests for shr.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2026 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@gnus.org>

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
(require 'shr)

(declare-function libxml-parse-html-region "xml.c")

(defvar shr-test--max-wait-time 5
  "The maximum amount of time to wait for a condition to resolve, in seconds.
See `shr-test-wait-for'.")

(defun shr-test-wait-for (predicate &optional message)
  "Wait until PREDICATE returns non-nil.
If this takes longer than `shr-test--max-wait-time', raise an error.
MESSAGE is an optional message to use if this times out."
  (let ((start (current-time))
        (message (or message "timed out waiting for condition")))
    (while (not (funcall predicate))
      (when (> (float-time (time-since start))
               shr-test--max-wait-time)
        (error message))
      (sit-for 0.1))))

(defun shr-test--rendering-check (name &optional context)
  "Render NAME.html and compare it to NAME.txt.
Raise a test failure if the rendered buffer does not match NAME.txt.
Append CONTEXT to the failure data, if non-nil."
  (let ((text-file (file-name-concat (ert-resource-directory) (concat name ".txt")))
        (html-file (file-name-concat (ert-resource-directory) (concat name ".html")))
        (description (if context (format "%s (%s)" name context) name))
        (coding-system-for-read 'utf-8))
    (with-temp-buffer
      (insert-file-contents html-file)
      (let ((dom (libxml-parse-html-region (point-min) (point-max)))
            (shr-width 80)
            (shr-use-fonts nil))
        (erase-buffer)
        (shr-insert-document dom)
        (let ((result (buffer-substring-no-properties (point-min) (point-max)))
              (expected
               (with-temp-buffer
                 (insert-file-contents text-file)
                 (while (re-search-forward "%\\([0-9A-F][0-9A-F]\\)" nil t)
                   (replace-match (string (string-to-number (match-string 1) 16))
                                  t t))
                 (buffer-string))))
          (unless (equal result expected)
            (ert-fail (list description result expected))))))))

(defconst shr-test--rendering-extra-configs
  '(("blockquote"
     ;; Make sure blockquotes remain indented even when filling is
     ;; disabled (bug#69555).
     . ((shr-fill-text . nil))))
  "Extra customizations which can impact rendering.
This is a list of (NAME . SETTINGS) pairs.  NAME is the basename of a
set of txt/html files under shr-resources/, as passed to `shr-test'.
SETTINGS is a list of (OPTION . VALUE) pairs that are interesting to
validate for the NAME testcase.

The `rendering' testcase will test NAME once without altering any
settings, then once more for each (OPTION . VALUE) pair.")

;;; Tests:

(ert-deftest rendering ()
  (skip-unless (fboundp 'libxml-parse-html-region))
  (dolist (file (directory-files (ert-resource-directory) nil "\\.html\\'"))
    (let* ((name (string-remove-suffix ".html" file))
           (extra-options (alist-get name shr-test--rendering-extra-configs
                                     nil nil 'string=)))
      ;; Test once with default settings.
      (shr-test--rendering-check name)
      ;; Test once more for every extra option for this specific NAME.
      (pcase-dolist (`(,option-sym ,option-val)
                     extra-options)
        (let ((option-old (symbol-value option-sym)))
          (set option-sym option-val)
          (unwind-protect
              (shr-test--rendering-check
               name (format "with %s %s" option-sym option-val))
            (set option-sym option-old)))))))

(ert-deftest use-cookies ()
  (let ((shr-cookie-policy 'same-origin))
    (should
     (shr--use-cookies-p "http://images.fsf.org" '("http://www.fsf.org")))
    (should
     (shr--use-cookies-p "http://www.fsf.org" '("https://www.fsf.org")))
    (should
     (shr--use-cookies-p "http://www.fsf.org" '("https://www.fsf.org")))
    (should
     (shr--use-cookies-p "http://www.fsf.org" '("http://fsf.org")))
    (should-not
     (shr--use-cookies-p "http://www.gnu.org" '("http://www.fsf.org")))))

(ert-deftest shr-srcset ()
  (should (equal (shr--parse-srcset "") nil))

  (should (equal (shr--parse-srcset "a 10w, b 20w")
                 '(("b" 20) ("a" 10))))

  (should (equal (shr--parse-srcset "a 10w b 20w")
                 '(("a" 10))))

  (should (equal (shr--parse-srcset "https://example.org/1\n\n 10w , https://example.org/2 20w      ")
	         '(("https://example.org/2" 20) ("https://example.org/1" 10))))

  (should (equal (shr--parse-srcset "https://example.org/1,2\n\n 10w , https://example.org/2 20w      ")
	         '(("https://example.org/2" 20) ("https://example.org/1,2" 10)))))

(ert-deftest shr-test/zoom-image ()
  "Test that `shr-zoom-image' properly replaces the original image."
  (skip-unless (and (bound-and-true-p image-types)
                    (image-type-available-p 'png)
                    (fboundp 'libxml-parse-html-region)))
  (let* ((image (expand-file-name "data/image/blank-100x200.png"
                                  (getenv "EMACS_TEST_DIRECTORY")))
         (image-url (concat "file://" (if (string-prefix-p "/" image)
                                          image (concat "/" image)))))
    (dolist (alt '(nil "" "nothing to see here"))
      (with-temp-buffer
        (ert-info ((format "image with alt=%S" alt))
          (let ((attrs (if alt (format " alt=\"%s\">" alt) ">")))
            (insert (format "<img src=\"%s\"%s" image-url attrs)))
          (cl-letf* (;; Pretend we're a graphical display.
                     ((symbol-function 'display-graphic-p) #'always)
                     ((symbol-function 'url-queue-retrieve)
                      (lambda (&rest args)
                        (apply #'run-at-time 0 nil #'url-retrieve args)))
                     (put-image-calls 0)
                     (shr-put-image-function
                      (lambda (&rest args)
                        (incf put-image-calls)
                        (apply #'shr-put-image args)))
                     (shr-width 80)
                     (shr-use-fonts nil)
                     (shr-image-animate nil)
                     (shr-sliced-image-height nil)
                     (inhibit-message t)
                     (dom (libxml-parse-html-region (point-min) (point-max))))
            ;; Render the document.
            (erase-buffer)
            (shr-insert-document dom)
            (shr-test-wait-for (lambda () (= put-image-calls 1))
                               "Timed out waiting for initial load")
            ;; Now zoom the image.
            (goto-char (point-min))
            (shr-zoom-image)
            (shr-test-wait-for (lambda () (= put-image-calls 2))
                               "Timed out waiting to zoom image")
            ;; Check that we have a single image at original size.
            (let (image-zooms)
              (goto-char (point-min))
              (while (< (point) (point-max))
                (when (get-text-property (point) 'display)
                  (push (get-text-property (point) 'image-zoom) image-zooms))
                (goto-char (or (next-single-property-change (point) 'display)
                               (point-max))))
              (should (equal image-zooms '(original))))))))))

(require 'shr)

;;; shr-tests.el ends here
