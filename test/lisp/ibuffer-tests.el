;;; ibuffer-tests.el --- Test suite. -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

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

;;; Code:
(require 'ert)
(require 'ibuffer)
(eval-when-compile
  (require 'ibuf-macs))

(defvar ibuffer-filter-groups)
(defvar ibuffer-filtering-alist)
(defvar ibuffer-filtering-qualifiers)
(defvar ibuffer-save-with-custom)
(defvar ibuffer-saved-filter-groups)
(defvar ibuffer-saved-filters)
(declare-function ibuffer-format-qualifier "ibuf-ext" (qualifier))
(declare-function ibuffer-unary-operand "ibuf-ext" (filter))

(ert-deftest ibuffer-0autoload ()       ; sort first
  "Tests to see whether ibuffer has been autoloaded"
  (skip-unless (not (featurep 'ibuf-ext)))
  (should
   (fboundp 'ibuffer-mark-unsaved-buffers))
  (should
   (autoloadp
    (symbol-function
     'ibuffer-mark-unsaved-buffers))))

(ert-deftest ibuffer-test-Bug24997 ()
  "Test for https://debbugs.gnu.org/24997 ."
  (ibuffer)
  (let ((orig ibuffer-filtering-qualifiers))
    (unwind-protect
        (progn
          (setq ibuffer-filtering-qualifiers
                '((size-gt . 10)
                  (used-mode . lisp-interaction-mode)))
          (ibuffer-update nil t)
          (ignore-errors (ibuffer-decompose-filter))
          (should (cdr ibuffer-filtering-qualifiers)))
      (setq ibuffer-filtering-qualifiers orig)
      (ibuffer-update nil t))))

(ert-deftest ibuffer-test-Bug25000 ()
  "Test for https://debbugs.gnu.org/25000 ."
  (let ((case-fold-search t)
        (buf1 (generate-new-buffer "ibuffer-test-Bug25000-buf1"))
        (buf2 (generate-new-buffer "ibuffer-test-Bug25000-buf2")))
    (ibuffer)
    (unwind-protect
        (ibuffer-save-marks
          (ibuffer-unmark-all-marks)
          (ibuffer-mark-by-name-regexp (buffer-name buf1))
          (ibuffer-change-marks ibuffer-marked-char ?L)
          (ibuffer-mark-by-name-regexp (buffer-name buf2))
          (ibuffer-change-marks ibuffer-marked-char ?l)
          (should-not (cdr (ibuffer-buffer-names-with-mark ?l))))
      (mapc (lambda (buf) (when (buffer-live-p buf)
                            (kill-buffer buf))) (list buf1 buf2)))))

(ert-deftest ibuffer-save-filters ()
  "Tests that `ibuffer-save-filters' saves in the proper format."
  (require 'ibuf-ext)
  (let ((ibuffer-save-with-custom nil)
        (ibuffer-saved-filters nil)
        (test1 '((mode . org-mode)
                 (or (size-gt . 10000)
                     (and (not (starred-name))
                          (directory . "\<org\>")))))
        (test2 '((or (mode . emacs-lisp-mode) (file-extension . "elc?")
                     (and (starred-name) (name . "elisp"))
                     (mode . lisp-interaction-mode))))
        (test3 '((size-lt . 100) (derived-mode . prog-mode)
                 (or (filename . "scratch")
                     (filename . "bonz")
                     (filename . "temp")))))
    (ibuffer-save-filters "test1" test1)
    (should (equal (car ibuffer-saved-filters) (cons "test1" test1)))
    (ibuffer-save-filters "test2" test2)
    (should (equal (car ibuffer-saved-filters) (cons "test2" test2)))
    (should (equal (cadr ibuffer-saved-filters) (cons "test1" test1)))
    (ibuffer-save-filters "test3" test3)
    (should (equal (car ibuffer-saved-filters) (cons "test3" test3)))
    (should (equal (cadr ibuffer-saved-filters) (cons "test2" test2)))
    (should (equal (car (cddr ibuffer-saved-filters)) (cons "test1" test1)))
    (should (equal (cdr (assoc "test1" ibuffer-saved-filters)) test1))
    (should (equal (cdr (assoc "test2" ibuffer-saved-filters)) test2))
    (should (equal (cdr (assoc "test3" ibuffer-saved-filters)) test3))))

(ert-deftest ibuffer-test-Bug25058 ()
  "Test for https://debbugs.gnu.org/25058 ."
  (ibuffer)
  (let ((orig-filters ibuffer-saved-filter-groups)
        (tmp-filters '(("saved-filters"
                        ("Shell"
                         (used-mode . shell-mode))
                        ("Elisp"
                         (or
                          (used-mode . emacs-lisp-mode)
                          (used-mode . lisp-interaction-mode)))
                        ("Dired"
                         (used-mode . dired-mode))
                        ("Info"
                         (or
                          (used-mode . help-mode)
                          (used-mode . debugger-mode)
                          (used-mode . Custom-mode)
                          (used-mode . completion-list-mode)
                          (name . "\\`[*]Messages[*]\\'")))))))
    (unwind-protect
        (progn
          (setq ibuffer-saved-filter-groups tmp-filters)
	  (ibuffer-switch-to-saved-filter-groups "saved-filters")
          (ibuffer-decompose-filter-group "Elisp")
          (ibuffer-filter-disable)
          (ibuffer-switch-to-saved-filter-groups "saved-filters")
          (should (assoc "Elisp" (cdar ibuffer-saved-filter-groups))))
      (setq ibuffer-saved-filter-groups orig-filters)
      (ibuffer-awhen (get-buffer "*Ibuffer*")
        (and (buffer-live-p it) (kill-buffer it))))))


(ert-deftest ibuffer-test-Bug25042 ()
  "Test for https://debbugs.gnu.org/25042 ."
  (ibuffer)
  (let ((filters ibuffer-filtering-qualifiers))
    (unwind-protect
        (progn
          (ignore-errors ; Mistyped `match-string' instead of `string-match'.
            (setq ibuffer-filtering-qualifiers nil)
            (ibuffer-filter-by-predicate '(match-string "foo" (buffer-name))))
          (should-not ibuffer-filtering-qualifiers))
      (setq ibuffer-filtering-qualifiers filters))))

;; Test Filter Inclusion
(let* (test-buffer-list  ; accumulated buffers to clean up
       test-file-list
       ;; Utility functions without polluting the environment
       (set-buffer-mode
        (lambda (buffer mode)
          "Set BUFFER's major mode to MODE, a mode function, or fundamental."
          (with-current-buffer buffer
            (funcall (or mode #'fundamental-mode)))))
       (set-buffer-contents
        (lambda (buffer size include-content)
          "Add exactly SIZE bytes to BUFFER, including INCLUDE-CONTENT."
          (when (or size include-content)
            (let* ((unit "\n")
                   (chunk "ccccccccccccccccccccccccccccccc\n")
                   (chunk-size (length chunk))
                   (size (if (and size include-content (stringp include-content))
                             (- size (length include-content))
                           size)))
              (unless (or (null size) (> size 0))
                (error "size argument must be nil or positive"))
              (with-current-buffer buffer
                (when include-content
                  (insert include-content))
                (when size
                  (dotimes (_ (floor size chunk-size))
                    (insert chunk))
                  (dotimes (_ (mod size chunk-size))
                    (insert unit)))
                ;; prevent query on cleanup
                (set-buffer-modified-p nil))))))
       (create-file-buffer
        (lambda (prefix &rest args-plist)
          "Create a file and buffer with designated properties.
        PREFIX is a string giving the beginning of the name, and ARGS-PLIST
        is a series of keyword-value pairs, with allowed keywords
        :suffix STRING, :size NUMBER, :mode MODE-FUNC, :include-content STRING.
        Returns the created buffer."
          (let* ((suffix  (plist-get args-plist :suffix))
                 (size    (plist-get args-plist :size))
                 (include (plist-get args-plist :include-content))
                 (mode    (plist-get args-plist :mode))
                 (file    (make-temp-file prefix nil suffix))
                 (buf     (find-file-noselect file t)))
            (push buf test-buffer-list) ; record for cleanup
            (push file test-file-list)
            (funcall set-buffer-mode buf mode)
            (funcall set-buffer-contents buf size include)
            buf)))
       (create-non-file-buffer
        (lambda (prefix &rest args-plist)
          "Create a non-file and buffer with designated properties.
        PREFIX is a string giving the beginning of the name, and ARGS-PLIST
        is a series of keyword-value pairs, with allowed keywords
        :size NUMBER, :mode MODE-FUNC, :include-content STRING.
        Returns the created buffer."
          (let* ((size    (plist-get args-plist :size))
                 (include (plist-get args-plist :include-content))
                 (mode    (plist-get args-plist :mode))
                 (buf     (generate-new-buffer prefix)))
            (push buf test-buffer-list) ; record for cleanup
            (funcall set-buffer-mode buf mode)
            (funcall set-buffer-contents buf size include)
            buf)))
       (clean-up
        (lambda ()
          "Restore all emacs state modified during the tests"
          (dolist (f test-file-list)
            (and f (file-exists-p f) (delete-file f)))
          (while test-buffer-list       ; created temporary buffers
            (let ((buf (pop test-buffer-list)))
              (with-current-buffer buf (bury-buffer)) ; ensure not selected
              (kill-buffer buf))))))
  ;; Tests
  (ert-deftest ibuffer-filter-inclusion-1 ()
    "Tests inclusion using basic filter combinators with a single buffer."
    (require 'ibuf-ext)
    (unwind-protect
        (let ((buf
               (funcall create-file-buffer "ibuf-test-1" :size 100
                        :include-content "One ring to rule them all\n")))
          (should (ibuffer-included-in-filters-p buf '((size-gt . 99))))
          (should (ibuffer-included-in-filters-p buf '((size-lt . 101))))
          (should (ibuffer-included-in-filters-p
                   buf '((mode . fundamental-mode))))
          (should (ibuffer-included-in-filters-p
                   buf '((content . "ring to rule them all"))))
          (should (ibuffer-included-in-filters-p
                   buf '((and (content . "ring to rule them all")))))
          (should (ibuffer-included-in-filters-p
                   buf '((and (and (content . "ring to rule them all"))))))
          (should (ibuffer-included-in-filters-p
                   buf '((and (and (and (content . "ring to rule them all")))))))
          (should (ibuffer-included-in-filters-p
                   buf '((or (content . "ring to rule them all")))))
          (should (ibuffer-included-in-filters-p
                   buf '((not (not (content . "ring to rule them all"))))))
          (should (ibuffer-included-in-filters-p
                   buf '((and (size-gt . 99)
                              (content . "ring to rule them all")
                              (mode . fundamental-mode)
                              (basename . "\\`ibuf-test-1")))))
          (should (ibuffer-included-in-filters-p
                   buf '((not (or (not (size-gt . 99))
                                  (not (content . "ring to rule them all"))
                                  (not (mode . fundamental-mode))
                                  (not (basename . "\\`ibuf-test-1")))))))
          (should (ibuffer-included-in-filters-p
                   buf '((and (or (size-gt . 99) (size-lt . 10))
                              (and (content . "ring.*all")
                                   (content . "rule")
                                   (content . "them all")
                                   (content . "One"))
                              (not (mode . text-mode))
                              (basename . "\\`ibuf-test-1"))))))
      (funcall clean-up)))

  (ert-deftest ibuffer-filter-inclusion-2 ()
    "Tests inclusion of basic filters in combination on a single buffer."
    (require 'ibuf-ext)
    (unwind-protect
        (let ((buf
               (funcall create-file-buffer "ibuf-test-2" :size 200
                        :mode #'text-mode
                        :include-content "and in the darkness find them\n")))
          (should (ibuffer-included-in-filters-p buf '((size-gt . 199))))
          (should (ibuffer-included-in-filters-p buf '((size-lt . 201))))
          (should (ibuffer-included-in-filters-p buf '((not size-gt . 200))))
          (should (ibuffer-included-in-filters-p buf '((not (size-gt . 200)))))
          (should (ibuffer-included-in-filters-p
                   buf '((and (size-gt . 199) (size-lt . 201)))))
          (should (ibuffer-included-in-filters-p
                   buf '((or (size-gt . 199) (size-gt . 201)))))
          (should (ibuffer-included-in-filters-p
                   buf '((or (size-gt . 201) (size-gt . 199)))))
          (should (ibuffer-included-in-filters-p
                   buf '((size-gt . 199) (mode . text-mode)
                         (content . "darkness find them"))))
          (should (ibuffer-included-in-filters-p
                   buf '((and (size-gt . 199) (mode . text-mode)
                              (content . "darkness find them")))))
          (should (ibuffer-included-in-filters-p
                   buf '((not (or (not (size-gt . 199)) (not (mode . text-mode))
                                  (not (content . "darkness find them")))))))
          (should (ibuffer-included-in-filters-p
                   buf '((or (size-gt . 200) (content . "darkness find them")
                             (derived-mode . emacs-lisp-mode)))))
          (should-not (ibuffer-included-in-filters-p
                       buf '((or (size-gt . 200) (content . "rule them all")
                                 (derived-mode . emacs-lisp-mode))))))
      (funcall clean-up)))

  (ert-deftest ibuffer-filter-inclusion-3 ()
    "Tests inclusion with filename filters on specified buffers."
    (require 'ibuf-ext)
    (unwind-protect
        (let* ((bufA
                (funcall create-file-buffer "ibuf-test-3.a" :size 50
                         :mode #'text-mode
                         :include-content "...but a multitude of drops?\n"))
               (bufB
                (funcall create-non-file-buffer "ibuf-test-3.b" :size 50
                         :mode #'text-mode
                         :include-content "...but a multitude of drops?\n"))
               (dirA (with-current-buffer bufA default-directory))
               (dirB (with-current-buffer bufB default-directory)))
          (should (ibuffer-included-in-filters-p
                   bufA '((basename . "ibuf-test-3"))))
          (should (ibuffer-included-in-filters-p
                   bufA '((basename . "test-3\\.a"))))
          (should (ibuffer-included-in-filters-p
                   bufA '((file-extension . "a"))))
          (should (ibuffer-included-in-filters-p
                   bufA (list (cons 'directory dirA))))
          (should-not (ibuffer-included-in-filters-p
                       bufB '((basename . "ibuf-test-3"))))
          (should-not (ibuffer-included-in-filters-p
                       bufB '((file-extension . "b"))))
          (should (ibuffer-included-in-filters-p
                   bufB (list (cons 'directory dirB))))
          (should (ibuffer-included-in-filters-p
                   bufA '((name . "ibuf-test-3"))))
          (should (ibuffer-included-in-filters-p
                   bufB '((name . "ibuf-test-3")))))
      (funcall clean-up)))

  (ert-deftest ibuffer-filter-inclusion-4 ()
    "Tests inclusion with various filters on a single buffer."
    (require 'ibuf-ext)
    (unwind-protect
        (let ((buf
               (funcall create-file-buffer "ibuf-test-4"
                        :mode #'emacs-lisp-mode :suffix ".el"
                        :include-content "(message \"--%s--\" 'emacs-rocks)\n")))
          (should (ibuffer-included-in-filters-p
                   buf '((file-extension . "el"))))
          (should (ibuffer-included-in-filters-p
                   buf '((derived-mode . prog-mode))))
          (should (ibuffer-included-in-filters-p
                   buf '((used-mode . emacs-lisp-mode))))
          (should (ibuffer-included-in-filters-p
                   buf '((mode . emacs-lisp-mode))))
          (with-current-buffer buf (set-buffer-modified-p t))
          (should (ibuffer-included-in-filters-p buf '((modified))))
          (with-current-buffer buf (set-buffer-modified-p nil))
          (should (ibuffer-included-in-filters-p buf '((not modified))))
          (should (ibuffer-included-in-filters-p
                   buf '((and (file-extension . "el")
                              (derived-mode . prog-mode)
                              (not modified)))))
          (should (ibuffer-included-in-filters-p
                   buf '((or (file-extension . "tex")
                             (derived-mode . prog-mode)
                             (modified)))))
          (should (ibuffer-included-in-filters-p
                   buf '((file-extension . "el")
                         (derived-mode . prog-mode)
                         (not modified)))))
      (funcall clean-up)))

  (ert-deftest ibuffer-filter-inclusion-5 ()
    "Tests inclusion with various filters on a single buffer."
    (require 'ibuf-ext)
    (unwind-protect
        (let ((buf
               (funcall create-non-file-buffer "ibuf-test-5.el"
                        :mode #'emacs-lisp-mode
                        :include-content
                        "(message \"--%s--\" \"It really does!\")\n")))
          (should-not (ibuffer-included-in-filters-p
                       buf '((file-extension . "el"))))
          (should (ibuffer-included-in-filters-p
                   buf '((size-gt . 18))))
          (should (ibuffer-included-in-filters-p
                   buf '((predicate . (lambda ()
                                        (> (- (point-max) (point-min)) 18))))))
          (should (ibuffer-included-in-filters-p
                   buf '((and (mode . emacs-lisp-mode)
                              (or (starred-name)
                                  (size-gt . 18))
                              (and (not (size-gt . 100))
                                   (content . "[Ii]t  *really does!")
                                   (or (name . "test-5")
                                       (not (filename . "test-5")))))))))
      (funcall clean-up)))

  (ert-deftest ibuffer-filter-inclusion-6 ()
    "Tests inclusion using saved filters and DeMorgan's laws."
    (require 'ibuf-ext)
    (unwind-protect
        (let ((buf
               (funcall create-non-file-buffer "*ibuf-test-6*" :size 65
                        :mode #'text-mode))
              (buf2
               (funcall create-file-buffer "ibuf-test-6a" :suffix ".html"
                        :mode #'html-mode
                        :include-content
                        "<HTML><BODY><H1>Hello, World!</H1></BODY></HTML>")))
          (should (ibuffer-included-in-filters-p buf '((starred-name))))
          (should-not (ibuffer-included-in-filters-p
                       buf '((saved . "text document"))))
          (should (ibuffer-included-in-filters-p buf2 '((saved . "web"))))
          (should (ibuffer-included-in-filters-p
                   buf2 '((not (and (not (derived-mode . sgml-mode))
                                    (not (derived-mode . css-mode))
                                    (not (mode         . javascript-mode))
                                    (not (mode         . js2-mode))
                                    (not (mode         . scss-mode))
                                    (not (derived-mode . haml-mode))
                                    (not (mode         . sass-mode)))))))
          (should (ibuffer-included-in-filters-p
                   buf '((and (starred-name)
                              (or (size-gt . 50) (filename . "foo"))))))
          (should (ibuffer-included-in-filters-p
                   buf '((not (or (not starred-name)
                                  (and (size-lt . 51)
                                       (not (filename . "foo")))))))))
      (funcall clean-up)))

  (ert-deftest ibuffer-filter-inclusion-7 ()
    "Tests inclusion with various filters on a single buffer."
    (require 'ibuf-ext)
    (unwind-protect
        (let ((buf
               (funcall create-non-file-buffer "ibuf-test-7"
                        :mode #'artist-mode)))
          (should (ibuffer-included-in-filters-p
                   buf '((not (starred-name)))))
          (should (ibuffer-included-in-filters-p
                   buf '((not starred-name))))
          (should (ibuffer-included-in-filters-p
                   buf '((not (not (not starred-name))))))
          (should (ibuffer-included-in-filters-p
                   buf '((not (modified)))))
          (should (ibuffer-included-in-filters-p
                   buf '((not modified))))
          (should (ibuffer-included-in-filters-p
                   buf '((not (not (not modified)))))))
      (funcall clean-up)))

  (ert-deftest ibuffer-filter-inclusion-8 ()
    "Tests inclusion with various filters."
    (require 'ibuf-ext)
    (unwind-protect
        (let ((bufA
               (funcall create-non-file-buffer "ibuf-test-8a"
                        :mode #'artist-mode))
              (bufB (funcall create-non-file-buffer "*ibuf-test-8b*" :size 32))
              (bufC (or (memq system-type '(ms-dos windows-nt))
                        (funcall create-file-buffer "ibuf-test8c" :suffix "*"
                                 :size 64)))
              (bufD (or (memq system-type '(ms-dos windows-nt))
                        (funcall create-file-buffer "*ibuf-test8d" :size 128)))
              (bufE (or (memq system-type '(ms-dos windows-nt))
                        (funcall create-file-buffer "*ibuf-test8e"
                                 :suffix "*<2>" :size 16)))
              (bufF (and (funcall create-non-file-buffer "*ibuf-test8f*")
                         (funcall create-non-file-buffer "*ibuf-test8f*"
                                  :size 8))))
          (with-current-buffer bufA (set-buffer-modified-p t))
          (should (ibuffer-included-in-filters-p
                   bufA '((and (not starred-name)
                               (modified)
                               (name . "test-8")
                               (not (size-gt . 100))
                               (mode . picture-mode)))))
          (with-current-buffer bufA (set-buffer-modified-p nil))
          (should-not (ibuffer-included-in-filters-p
                       bufA '((or (starred-name) (visiting-file) (modified)))))
          (should (ibuffer-included-in-filters-p
                   bufB '((and (starred-name)
                               (name . "test.*8b")
                               (size-gt . 31)
                               (not visiting-file)))))
          ;; MS-DOS and MS-Windows don't allow "*" in file names.
          (or (memq system-type '(ms-dos windows-nt))
              (should (ibuffer-included-in-filters-p
                       bufC '((and (not (starred-name))
                                   (visiting-file)
                                   (name . "8c[^*]*\\*")
                                   (size-lt . 65))))))
          ;; MS-DOS and MS-Windows don't allow "*" in file names.
          (or (memq system-type '(ms-dos windows-nt))
              (should (ibuffer-included-in-filters-p
                       bufD '((and (not (starred-name))
                                   (visiting-file)
                                   (name . "\\`\\*.*test8d")
                                   (size-lt . 129)
                                   (size-gt . 127))))))
          ;; MS-DOS and MS-Windows don't allow "*" in file names.
          (or (memq system-type '(ms-dos windows-nt))
              (should (ibuffer-included-in-filters-p
                       bufE '((and (starred-name)
                                   (visiting-file)
                                   (name . "8e.*?\\*<[[:digit:]]+>")
                                   (size-gt . 10))))))
          (should (ibuffer-included-in-filters-p
                   bufF '((and (starred-name)
                               (not (visiting-file))
                               (name . "8f\\*<[[:digit:]]>")
                               (size-lt . 10))))))
      (funcall clean-up))))

;; Test Filter Combination and Decomposition
(let* (ibuffer-to-kill       ; if non-nil, kill this buffer at cleanup
       (ibuffer-already 'check) ; existing ibuffer buffer to use but not kill
       ;; Utility functions without polluting the environment
       (get-test-ibuffer
        (lambda ()
          "Returns a test ibuffer-mode buffer, creating one if necessary.
        If a new buffer is created, it is named  \"*Test-Ibuffer*\" and is
        saved to `ibuffer-to-kill' for later cleanup."
          (when (eq ibuffer-already 'check)
            (setq ibuffer-already
                  (catch 'found-buf
                    (dolist (buf (buffer-list) nil)
                      (when (with-current-buffer buf
                              (derived-mode-p 'ibuffer-mode))
                        (throw 'found-buf buf))))))
          (or ibuffer-already
              ibuffer-to-kill
              (let ((test-ibuf-name "*Test-Ibuffer*"))
                (ibuffer nil test-ibuf-name nil t)
                (setq ibuffer-to-kill (get-buffer test-ibuf-name))))))
       (clean-up
        (lambda ()
          "Restore all emacs state modified during the tests"
          (when ibuffer-to-kill         ; created ibuffer
            (with-current-buffer ibuffer-to-kill
              (set-buffer-modified-p nil)
              (bury-buffer))
            (kill-buffer ibuffer-to-kill)
            (setq ibuffer-to-kill nil))
          (when (and ibuffer-already (not (eq ibuffer-already 'check)))
            ;; restore existing ibuffer state
            (ibuffer-update nil t)))))
  ;; Tests
  (ert-deftest ibuffer-decompose-filter ()
    "Tests `ibuffer-decompose-filter' for and, or, not, and saved."
    (require 'ibuf-ext)
    (unwind-protect
        (let ((ibuf (funcall get-test-ibuffer)))
          (with-current-buffer ibuf
            (let ((ibuffer-filtering-qualifiers nil)
                  (ibuffer-filter-groups nil)
                  (filters '((size-gt . 100) (not (starred-name))
                             (name . "foo"))))
              (progn
                (push (cons 'or filters) ibuffer-filtering-qualifiers)
                (ibuffer-decompose-filter)
                (should (equal filters ibuffer-filtering-qualifiers))
                (setq ibuffer-filtering-qualifiers nil))
              (progn
                (push (cons 'and filters) ibuffer-filtering-qualifiers)
                (ibuffer-decompose-filter)
                (should (equal filters ibuffer-filtering-qualifiers))
                (setq ibuffer-filtering-qualifiers nil))
              (progn
                (push (list 'not (car filters)) ibuffer-filtering-qualifiers)
                (ibuffer-decompose-filter)
                (should (equal (list (car filters))
                               ibuffer-filtering-qualifiers))
                (setq ibuffer-filtering-qualifiers nil))
              (progn
                (push (cons 'not (car filters)) ibuffer-filtering-qualifiers)
                (ibuffer-decompose-filter)
                (should (equal (list (car filters))
                               ibuffer-filtering-qualifiers))
                (setq ibuffer-filtering-qualifiers nil))
              (let ((gnus (assoc "gnus" ibuffer-saved-filters)))
                (push '(saved . "gnus") ibuffer-filtering-qualifiers)
                (ibuffer-decompose-filter)
                (should (equal (cdr gnus) ibuffer-filtering-qualifiers))
                (ibuffer-decompose-filter)
                (should (equal (cdr (cadr gnus)) ibuffer-filtering-qualifiers))
                (setq ibuffer-filtering-qualifiers nil))
              (when (not (assoc "__unknown__" ibuffer-saved-filters))
                (push '(saved . "__uknown__") ibuffer-filtering-qualifiers)
                (should-error (ibuffer-decompose-filter) :type 'error)
                (setq ibuffer-filtering-qualifiers nil))
              (progn
                (push (car filters) ibuffer-filtering-qualifiers)
                (should-error (ibuffer-decompose-filter) :type 'error)
                (setq ibuffer-filtering-qualifiers nil)))))
      (funcall clean-up)))

  (ert-deftest ibuffer-and-filter ()
    "Tests `ibuffer-and-filter' in an Ibuffer buffer."
    (require 'ibuf-ext)
    (unwind-protect
        (let ((ibuf (funcall get-test-ibuffer)))
          (with-current-buffer ibuf
            (let ((ibuffer-filtering-qualifiers nil)
                  (ibuffer-filter-groups nil)
                  (filters [(size-gt . 100) (not (starred-name))
                            (filename . "A") (mode . text-mode)]))
              (should-error (ibuffer-and-filter) :type 'error)
              (progn
                (push (aref filters 1) ibuffer-filtering-qualifiers)
                (should-error (ibuffer-and-filter) :type 'error))
              (should (progn
                        (push (aref filters 0) ibuffer-filtering-qualifiers)
                        (ibuffer-and-filter)
                        (and (equal (list 'and (aref filters 0) (aref filters 1))
                                    (car ibuffer-filtering-qualifiers))
                             (null (cdr ibuffer-filtering-qualifiers)))))
              (should (progn
                        (ibuffer-and-filter 'decompose)
                        (and (equal (aref filters 0)
                                    (pop ibuffer-filtering-qualifiers))
                             (equal (aref filters 1)
                                    (pop ibuffer-filtering-qualifiers))
                             (null ibuffer-filtering-qualifiers))))
              (should (progn
                        (push (list 'and (aref filters 2) (aref filters 3))
                              ibuffer-filtering-qualifiers)
                        (push (list 'and (aref filters 0) (aref filters 1))
                              ibuffer-filtering-qualifiers)
                        (ibuffer-and-filter)
                        (and (equal (list 'and (aref filters 0) (aref filters 1)
                                          (aref filters 2) (aref filters 3))
                                    (car ibuffer-filtering-qualifiers))
                             (null (cdr ibuffer-filtering-qualifiers)))))
              (pop ibuffer-filtering-qualifiers)
              (should (progn
                        (push (list 'or (aref filters 2) (aref filters 3))
                              ibuffer-filtering-qualifiers)
                        (push (list 'and (aref filters 0) (aref filters 1))
                              ibuffer-filtering-qualifiers)
                        (ibuffer-and-filter)
                        (and (equal (list 'and (aref filters 0) (aref filters 1)
                                          (list 'or (aref filters 2)
                                                (aref filters 3)))
                                    (car ibuffer-filtering-qualifiers))
                             (null (cdr ibuffer-filtering-qualifiers)))))
              (pop ibuffer-filtering-qualifiers)
              (should (progn
                        (push (list 'and (aref filters 2) (aref filters 3))
                              ibuffer-filtering-qualifiers)
                        (push (list 'or (aref filters 0) (aref filters 1))
                              ibuffer-filtering-qualifiers)
                        (ibuffer-and-filter)
                        (and (equal (list 'and (list 'or (aref filters 0)
                                                    (aref filters 1))
                                          (aref filters 2) (aref filters 3))
                                    (car ibuffer-filtering-qualifiers))
                             (null (cdr ibuffer-filtering-qualifiers)))))
              (pop ibuffer-filtering-qualifiers)
              (should (progn
                        (push (list 'or (aref filters 2) (aref filters 3))
                              ibuffer-filtering-qualifiers)
                        (push (list 'or (aref filters 0) (aref filters 1))
                              ibuffer-filtering-qualifiers)
                        (ibuffer-and-filter)
                        (and (equal (list 'and
                                          (list 'or (aref filters 0)
                                                (aref filters 1))
                                          (list 'or (aref filters 2)
                                                (aref filters 3)))
                                    (car ibuffer-filtering-qualifiers))
                             (null (cdr ibuffer-filtering-qualifiers))))))))
      (funcall clean-up)))

  (ert-deftest ibuffer-or-filter ()
    "Tests `ibuffer-or-filter' in an Ibuffer buffer."
    (require 'ibuf-ext)
    (unwind-protect
        (let ((ibuf (funcall get-test-ibuffer)))
          (with-current-buffer ibuf
            (let ((ibuffer-filtering-qualifiers nil)
                  (ibuffer-filter-groups nil)
                  (filters [(size-gt . 100) (not (starred-name))
                            (filename . "A") (mode . text-mode)]))
              (should-error (ibuffer-or-filter) :type 'error)
              (progn
                (push (aref filters 1) ibuffer-filtering-qualifiers)
                (should-error (ibuffer-or-filter) :type 'error))
              (should (progn
                        (push (aref filters 0) ibuffer-filtering-qualifiers)
                        (ibuffer-or-filter)
                        (and (equal (list 'or (aref filters 0) (aref filters 1))
                                    (car ibuffer-filtering-qualifiers))
                             (null (cdr ibuffer-filtering-qualifiers)))))
              (should (progn
                        (ibuffer-or-filter 'decompose)
                        (and (equal (aref filters 0)
                                    (pop ibuffer-filtering-qualifiers))
                             (equal (aref filters 1)
                                    (pop ibuffer-filtering-qualifiers))
                             (null ibuffer-filtering-qualifiers))))
              (should (progn
                        (push (list 'or (aref filters 2) (aref filters 3))
                              ibuffer-filtering-qualifiers)
                        (push (list 'or (aref filters 0) (aref filters 1))
                              ibuffer-filtering-qualifiers)
                        (ibuffer-or-filter)
                        (and (equal (list 'or (aref filters 0) (aref filters 1)
                                          (aref filters 2) (aref filters 3))
                                    (car ibuffer-filtering-qualifiers))
                             (null (cdr ibuffer-filtering-qualifiers)))))
              (pop ibuffer-filtering-qualifiers)
              (should (progn
                        (push (list 'and (aref filters 2) (aref filters 3))
                              ibuffer-filtering-qualifiers)
                        (push (list 'or (aref filters 0) (aref filters 1))
                              ibuffer-filtering-qualifiers)
                        (ibuffer-or-filter)
                        (and (equal (list 'or (aref filters 0) (aref filters 1)
                                          (list 'and (aref filters 2)
                                                (aref filters 3)))
                                    (car ibuffer-filtering-qualifiers))
                             (null (cdr ibuffer-filtering-qualifiers)))))
              (pop ibuffer-filtering-qualifiers)
              (should (progn
                        (push (list 'or (aref filters 2) (aref filters 3))
                              ibuffer-filtering-qualifiers)
                        (push (list 'and (aref filters 0) (aref filters 1))
                              ibuffer-filtering-qualifiers)
                        (ibuffer-or-filter)
                        (and (equal (list 'or (list 'and (aref filters 0)
                                                    (aref filters 1))
                                          (aref filters 2) (aref filters 3))
                                    (car ibuffer-filtering-qualifiers))
                             (null (cdr ibuffer-filtering-qualifiers)))))
              (pop ibuffer-filtering-qualifiers)
              (should (progn
                        (push (list 'and (aref filters 2) (aref filters 3))
                              ibuffer-filtering-qualifiers)
                        (push (list 'and (aref filters 0) (aref filters 1))
                              ibuffer-filtering-qualifiers)
                        (ibuffer-or-filter)
                        (and (equal (list 'or
                                          (list 'and (aref filters 0)
                                                (aref filters 1))
                                          (list 'and (aref filters 2)
                                                (aref filters 3)))
                                    (car ibuffer-filtering-qualifiers))
                             (null (cdr ibuffer-filtering-qualifiers))))))))
      (funcall clean-up))))

(ert-deftest ibuffer-format-qualifier ()
  "Tests string recommendation of filter from `ibuffer-format-qualifier'."
  (require 'ibuf-ext)
  (let ((test1 '(mode . org-mode))
        (test2 '(size-lt . 100))
        (test3 '(derived-mode . prog-mode))
        (test4 '(or (size-gt . 10000)
                    (and (not (starred-name))
                         (directory . "\\<org\\>"))))
        (test5 '(or (filename . "scratch")
                    (filename . "bonz")
                    (filename . "temp")))
        (test6 '(or (mode . emacs-lisp-mode) (file-extension . "elc?")
                    (and (starred-name) (name . "elisp"))
                    (mode . lisp-interaction-mode)))
        (description (lambda (q)
                       (cadr (assq q ibuffer-filtering-alist))))
        (tag (lambda (&rest args )
               (concat " [" (apply #'concat args) "]"))))
    (should (equal (ibuffer-format-qualifier test1)
                   (funcall tag (funcall description 'mode)
                            ": " "org-mode")))
    (should (equal (ibuffer-format-qualifier test2)
                   (funcall tag (funcall description 'size-lt)
                            ": " "100")))
    (should (equal (ibuffer-format-qualifier test3)
                   (funcall tag (funcall description 'derived-mode)
                            ": " "prog-mode")))
    (should (equal (ibuffer-format-qualifier test4)
                   (funcall tag "OR"
                            (funcall tag (funcall description 'size-gt)
                                     ": " (format "%s" 10000))
                            (funcall tag "AND"
                                     (funcall tag "NOT"
                                              (funcall tag
                                                       (funcall description
                                                                'starred-name)
                                                       ": " "nil"))
                                     (funcall tag
                                              (funcall description 'directory)
                                              ": " "\\<org\\>")))))
    (should (equal (ibuffer-format-qualifier test5)
                   (funcall tag "OR"
                            (funcall tag (funcall description 'filename)
                                     ": "  "scratch")
                            (funcall tag (funcall description 'filename)
                                     ": " "bonz")
                            (funcall tag (funcall description 'filename)
                                     ": " "temp"))))
    (should (equal (ibuffer-format-qualifier test6)
                   (funcall tag "OR"
                            (funcall tag (funcall description 'mode)
                                     ": " "emacs-lisp-mode")
                            (funcall tag (funcall description 'file-extension)
                                     ": " "elc?")
                            (funcall tag "AND"
                                     (funcall tag
                                              (funcall description 'starred-name)
                                              ": " "nil")
                                     (funcall tag
                                              (funcall description 'name)
                                              ": " "elisp"))
                            (funcall tag (funcall description 'mode)
                                     ": " "lisp-interaction-mode"))))))

(ert-deftest ibuffer-unary-operand ()
  "Tests `ibuffer-unary-operand': (not cell) or (not . cell) -> cell."
  (require 'ibuf-ext)
  (should (equal (ibuffer-unary-operand '(not . (mode "foo")))
                 '(mode "foo")))
  (should (equal (ibuffer-unary-operand '(not (mode "foo")))
                 '(mode "foo")))
  (should (equal (ibuffer-unary-operand '(not "cdr"))
                 '("cdr")))
  (should (equal (ibuffer-unary-operand '(not)) nil))
  (should (equal (ibuffer-unary-operand '(not . a)) 'a)))

(provide 'ibuffer-tests)
;; ibuffer-tests.el ends here
