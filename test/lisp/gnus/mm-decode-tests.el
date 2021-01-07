;;; mm-decode-tests.el ---  -*- lexical-binding:t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

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
(require 'mm-decode)

(ert-deftest test-mm-dissect-buffer ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally (ert-resource-file "8bit-multipart.bin"))
    (while (search-forward "\r\n" nil t)
      (replace-match "\n"))
    (let ((handle (mm-dissect-buffer)))
      (should (equal (mm-handle-media-type handle) "multipart/alternative"))
      ;; Skip multipart type.
      (pop handle)
      (let ((part (pop handle)))
        (should (equal (mm-handle-media-type part) "text/plain"))
        (should (eq (mm-handle-encoding part) '8bit))
        (with-current-buffer (mm-handle-buffer part)
          (should (equal (decode-coding-string
                          (buffer-string)
                          (intern (mail-content-type-get (mm-handle-type part)
                                                         'charset)))
                         "ääää\n"))))
      (let ((part (pop handle)))
        (should (equal (mm-handle-media-type part) "text/html"))
        (should (eq (mm-handle-encoding part) '8bit))
        (with-current-buffer (mm-handle-buffer part)
          (should (equal (decode-coding-string
                          (buffer-string)
                          (intern (mail-content-type-get (mm-handle-type part)
                                                         'charset)))
                         "<!doctype html><html><head><meta http-equiv=\"content-type\" content=\"text/html; charset=UTF-8\"></head><body>ääää</body></html>\n")))))))

(ert-deftest test-mm-with-part-unibyte ()
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally (ert-resource-file "8bit-multipart.bin"))
    (while (search-forward "\r\n" nil t)
      (replace-match "\n"))
    (let ((handle (mm-dissect-buffer)))
      (pop handle)
      (let ((part (pop handle)))
        (should (equal (decode-coding-string
                        (mm-with-part part
                          (buffer-string))
                        (intern (mail-content-type-get (mm-handle-type part)
                                                       'charset)))
                       "ääää\n"))))))

(ert-deftest test-mm-with-part-multibyte ()
  (with-temp-buffer
    (set-buffer-multibyte t)
    (nnheader-insert-file-contents (ert-resource-file "8bit-multipart.bin"))
    (while (search-forward "\r\n" nil t)
      (replace-match "\n"))
    (let ((handle (mm-dissect-buffer)))
      (pop handle)
      (let ((part (pop handle)))
        (should (equal (decode-coding-string
                        (mm-with-part part
                          (buffer-string))
                        (intern (mail-content-type-get (mm-handle-type part)
                                                       'charset)))
                       "ääää\n"))))))

;;; mm-decode-tests.el ends here
