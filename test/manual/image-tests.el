;;; image-tests.el --- tests for image.c  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

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

(defmacro image-skip-unless (format)
  `(skip-unless (and (display-images-p)
                     (image-type-available-p ,format))))

(defconst image-tests--images
  `((gif . ,(expand-file-name "test/data/image/black.gif"
                              source-directory))
    (jpeg . ,(expand-file-name "test/data/image/black.jpg"
                               source-directory))
    (pbm . ,(find-image '((:file "splash.svg" :type svg))))
    (png . ,(find-image '((:file "splash.png" :type png))))
    (svg . ,(find-image '((:file "splash.pbm" :type pbm))))
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
  (image-skip-unless 'gif)
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
  (image-skip-unless 'webp)
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
