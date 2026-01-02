;;; image-tests.el --- tests for image.c  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>
;; Keywords: internal

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

;; These tests will only run in a GUI session.  You must run them
;; manually in an interactive session with, for example, `M-x
;; eval-buffer' followed by `M-x ert'.
;;
;; To run them from the command line instead, try:
;;     ./src/emacs -Q -l test/manual/image-tests.el -eval "(ert t)"

;;; Code:

(require 'ert)

(defmacro image-skip-unless (format &rest condition)
  `(skip-unless (and (and (display-images-p)
                          (image-type-available-p ,format))
                     ,@condition)))

(defconst image-tests--images
  `((gif . ,(expand-file-name "test/data/image/black.gif"
                              source-directory))
    (jpeg . ,(expand-file-name "test/data/image/black.jpg"
                               source-directory))
    (svg . ,(find-image '((:file "splash.svg" :type svg))))
    (png . ,(find-image '((:file "splash.png" :type png))))
    (pbm . ,(find-image '((:file "splash.pbm" :type pbm))))
    (tiff . ,(expand-file-name
              "nextstep/GNUstep/Emacs.base/Resources/emacs.tiff"
              source-directory))
    (webp . ,(expand-file-name "test/data/image/black.webp"
                               source-directory))
    (xbm . ,(find-image '((:file "gnus/gnus.xbm" :type xbm))))
    (xpm . ,(find-image '((:file "splash.xpm" :type xpm))))))


;;;; Load image

(defmacro image-tests-make-load-image-test (type)
  `(ert-deftest ,(intern (format "image-tests-load-image/%s"
                                 (eval type t)))
       ()
     (image-skip-unless ,type)
     (let* ((img (cdr (assq ,type image-tests--images)))
            (file (if (listp img)
                      (plist-get (cdr img) :file)
                    img)))
       (find-file file))
     (should (equal major-mode 'image-mode))
     ;; Cleanup
     (kill-buffer (current-buffer))))

(image-tests-make-load-image-test 'gif)
(image-tests-make-load-image-test 'jpeg)
(image-tests-make-load-image-test 'pbm)
(image-tests-make-load-image-test 'png)
(image-tests-make-load-image-test 'svg)
(image-tests-make-load-image-test 'tiff)
(image-tests-make-load-image-test 'webp)
(image-tests-make-load-image-test 'xbm)
(image-tests-make-load-image-test 'xpm)

(ert-deftest image-tests-load-image/svg-too-big ()
  (image-skip-unless svg)
  (with-temp-buffer
    (let* ((max-image-size 0)
           (messages-buffer-name (buffer-name (current-buffer)))
           (img (cdr (assq 'svg image-tests--images)))
           (file (if (listp img)
                     (plist-get (cdr img) :file)
                   img)))
      (save-excursion (find-file file))
      (should (string-match-p "invalid image size" (buffer-string)))
      ;; no annoying newlines
      (should-not (string-match-p "^[ \t\n\r]+$" (buffer-string)))
      ;; no annoying double error reporting
      (should-not (string-match-p "error parsing" (buffer-string))))))

(ert-deftest image-tests-load-image/svg-invalid ()
  (image-skip-unless svg)
  (with-temp-buffer
    (let ((messages-buffer-name (buffer-name (current-buffer))))
      (with-temp-buffer
        (pop-to-buffer (current-buffer))
        (insert (propertize " "
                            'display '(image :data
                                             "invalid foo bar"
                                             :type svg)))
        (redisplay))
      ;; librsvg error: "... Start tag expected, '<' not found [3 times]"
      (should (string-match-p "[Ee]rror.+Start tag expected" (buffer-string)))
      ;; no annoying newlines
      (should-not (string-match-p "^[ \t\n\r]+$" (buffer-string))))))


;;;; image-test-size

(declare-function image-size "image.c" (spec &optional pixels frame))

(ert-deftest image-tests-image-size/gif ()
  (image-skip-unless 'gif)
  (pcase (image-size (create-image (cdr (assq 'gif image-tests--images))))
    (`(,a . ,b)
     (should (floatp a))
     (should (floatp b)))))

(ert-deftest image-tests-image-size/jpeg ()
  (image-skip-unless 'jpeg)
  (pcase (image-size (create-image (cdr (assq 'jpeg image-tests--images))))
    (`(,a . ,b)
     (should (floatp a))
     (should (floatp b)))))

(ert-deftest image-tests-image-size/pbm ()
  (image-skip-unless 'pbm)
  (pcase (image-size (cdr (assq 'pbm image-tests--images)))
    (`(,a . ,b)
     (should (floatp a))
     (should (floatp b)))))

(ert-deftest image-tests-image-size/png ()
  (image-skip-unless 'png)
  (pcase (image-size (cdr (assq 'png image-tests--images)))
    (`(,a . ,b)
     (should (floatp a))
     (should (floatp b)))))

(ert-deftest image-tests-image-size/svg ()
  (image-skip-unless 'svg)
  (pcase (image-size (cdr (assq 'svg image-tests--images)))
    (`(,a . ,b)
     (should (floatp a))
     (should (floatp b)))))

(ert-deftest image-tests-image-size/tiff ()
  (image-skip-unless 'tiff)
  (pcase (image-size (create-image (cdr (assq 'tiff image-tests--images))))
    (`(,a . ,b)
     (should (floatp a))
     (should (floatp b)))))

(ert-deftest image-tests-image-size/webp ()
  (image-skip-unless 'webp)
  (pcase (image-size (create-image (cdr (assq 'webp image-tests--images))))
    (`(,a . ,b)
     (should (floatp a))
     (should (floatp b)))))

(ert-deftest image-tests-image-size/xbm ()
  (image-skip-unless 'xbm)
  (pcase (image-size (cdr (assq 'xbm image-tests--images)))
    (`(,a . ,b)
     (should (floatp a))
     (should (floatp b)))))

(ert-deftest image-tests-image-size/xpm ()
  (image-skip-unless 'xpm)
  (pcase (image-size (cdr (assq 'xpm image-tests--images)))
    (`(,a . ,b)
     (should (floatp a))
     (should (floatp b)))))

(ert-deftest image-tests-image-size/error-on-invalid-spec ()
  (skip-unless (display-images-p))
  (should-error (image-size 'invalid-spec)))


;;;; image-mask-p

(declare-function image-mask-p "image.c" (spec &optional frame))

(ert-deftest image-tests-image-mask-p/gif ()
  (image-skip-unless 'gif)
  (should-not (image-mask-p (create-image
                             (cdr (assq 'gif image-tests--images))))))

(ert-deftest image-tests-image-mask-p/jpeg ()
  (image-skip-unless 'jpeg)
  (should-not (image-mask-p (create-image
                             (cdr (assq 'jpeg image-tests--images))))))

(ert-deftest image-tests-image-mask-p/pbm ()
  (image-skip-unless 'pbm)
  (should-not (image-mask-p (cdr (assq 'pbm image-tests--images)))))

(ert-deftest image-tests-image-mask-p/png ()
  (image-skip-unless 'png)
  (should-not (image-mask-p (cdr (assq 'png image-tests--images)))))

(ert-deftest image-tests-image-mask-p/svg ()
  (image-skip-unless 'svg)
  (should-not (image-mask-p (cdr (assq 'svg image-tests--images)))))

(ert-deftest image-tests-image-mask-p/tiff ()
  (image-skip-unless 'tiff)
  (should-not (image-mask-p (create-image
                             (cdr (assq 'tiff image-tests--images))))))

(ert-deftest image-tests-image-mask-p/webp ()
  (image-skip-unless 'webp)
  (should-not (image-mask-p (create-image
                             (cdr (assq 'webp image-tests--images))))))

(ert-deftest image-tests-image-mask-p/xbm ()
  (image-skip-unless 'xbm)
  (should-not (image-mask-p (cdr (assq 'xbm image-tests--images)))))

(ert-deftest image-tests-image-mask-p/xpm ()
  (image-skip-unless 'xpm)
  (should-not (image-mask-p (cdr (assq 'xpm image-tests--images)))))

(ert-deftest image-tests-image-mask-p/error-on-invalid-spec ()
  (skip-unless (display-images-p))
  (should-error (image-mask-p 'invalid-spec)))


;;;; image-metadata

(declare-function image-metadata "image.c" (spec &optional frame))

;; TODO: These tests could be expanded with files that actually
;;       contain metadata.

(ert-deftest image-tests-image-metadata/gif ()
  (image-skip-unless 'gif
                ;; FIXME: Why is this failing on macOS?
                (not (eq system-type 'darwin))
                (not (bound-and-true-p w32-use-native-image-API)))
  (should (memq 'delay
                (image-metadata
                 (create-image (cdr (assq 'gif image-tests--images)))))))

(ert-deftest image-tests-image-metadata/jpeg ()
  (image-skip-unless 'jpeg)
  (should-not (image-metadata
               (create-image (cdr (assq 'jpeg image-tests--images))))))

(ert-deftest image-tests-image-metadata/pbm ()
  (image-skip-unless 'pbm)
  (should-not (image-metadata (cdr (assq 'pbm image-tests--images)))))

(ert-deftest image-tests-image-metadata/png ()
  (image-skip-unless 'png)
  (should-not (image-metadata (cdr (assq 'png image-tests--images)))))

(ert-deftest image-tests-image-metadata/svg ()
  (image-skip-unless 'svg)
  (should-not (image-metadata (cdr (assq 'svg image-tests--images)))))

(ert-deftest image-tests-image-metadata/tiff ()
  (image-skip-unless 'tiff)
  (should-not (image-metadata
               (create-image (cdr (assq 'tiff image-tests--images))))))

(ert-deftest image-tests-image-metadata/webp ()
  (image-skip-unless 'webp
                ;; FIXME: Why is this failing on macOS?
                (not (eq system-type 'darwin)))
  (should (memq 'delay
                (image-metadata
                 (create-image (cdr (assq 'webp image-tests--images)))))))

(ert-deftest image-tests-image-metadata/xbm ()
  (image-skip-unless 'xbm)
  (should-not (image-metadata (cdr (assq 'xbm image-tests--images)))))

(ert-deftest image-tests-image-metadata/xpm ()
  (image-skip-unless 'xpm)
  (should-not (image-metadata (cdr (assq 'xpm image-tests--images)))))

(ert-deftest image-tests-image-metadata/nil-on-invalid-spec ()
  (skip-unless (display-images-p))
  (should-not (image-metadata 'invalid-spec)))

;;; image-size-tests.el ends here
