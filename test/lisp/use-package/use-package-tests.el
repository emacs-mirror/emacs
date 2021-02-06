;;; use-package-tests.el --- Tests for use-package.el  -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;


;;; Code:

(require 'cl)
(require 'ert)
(require 'use-package)

(setq use-package-always-ensure nil
      use-package-verbose 'errors
      use-package-expand-minimally t
      ;; These are needed for certain tests below where the `pcase' match
      ;; expression is large and contains holes, such as the :after tests.
      max-lisp-eval-depth 8000
      max-specpdl-size 8000)

(unless (fboundp 'macroexpand-1)
  (defun macroexpand-1 (form &optional environment)
    "Perform (at most) one step of macroexpansion."
    (cond
     ((consp form)
      (let* ((head (car form))
             (env-expander (assq head environment)))
        (if env-expander
            (if (cdr env-expander)
                (apply (cdr env-expander) (cdr form))
              form)
          (if (not (and (symbolp head) (fboundp head)))
              form
            (let ((def (autoload-do-load (symbol-function head) head 'macro)))
              (cond
               ;; Follow alias, but only for macros, otherwise we may end up
               ;; skipping an important compiler-macro (e.g. cl--block-wrapper).
               ((and (symbolp def) (macrop def)) (cons def (cdr form)))
               ((not (consp def)) form)
               (t
                (if (eq 'macro (car def))
                    (apply (cdr def) (cdr form))
                  form))))))))
     (t form))))

(defmacro expand-minimally (form)
  `(let ((use-package-verbose 'errors)
         (use-package-expand-minimally t))
     (macroexpand-1 ',form)))

(defmacro expand-maximally (form)
  `(let ((use-package-verbose 'debug)
         (use-package-expand-minimally nil))
     (macroexpand-1 ',form)))

(defmacro match-expansion (form &rest value)
  `(should (pcase (expand-minimally ,form)
             ,@(mapcar #'(lambda (x) (list x t)) value))))

(defun fix-expansion ()
  (interactive)
  (save-excursion
    (unless (looking-at "(match-expansion")
      (backward-up-list))
    (when (looking-at "(match-expansion")
      (re-search-forward "(\\(use-package\\|bind-key\\)")
      (goto-char (match-beginning 0))
      (let ((decl (read (current-buffer))))
        (kill-sexp)
        (let (vars)
          (catch 'exit
            (save-excursion
              (while (ignore-errors (backward-up-list) t)
                (when (looking-at "(let\\s-+")
                  (goto-char (match-end 0))
                  (setq vars (read (current-buffer)))
                  (throw 'exit t)))))
          (eval
           `(let (,@ (append vars
                             '((use-package-verbose 'errors)
                               (use-package-expand-minimally t))))
              (insert ?\n ?\` (pp-to-string (macroexpand-1 decl))))))))))

(bind-key "C-c C-u" #'fix-expansion emacs-lisp-mode-map)

(eval-when-compile
  (defun plist-delete (plist property)
    "Delete PROPERTY from PLIST"
    (let (p)
      (while plist
        (if (not (eq property (car plist)))
            (setq p (plist-put p (car plist) (nth 1 plist))))
        (setq plist (cddr plist)))
      p))

  ;; `cl-flet' does not work for some of the mocking we do below, while `flet'
  ;; always does.
  (setplist 'flet (plist-delete (symbol-plist 'flet) 'byte-obsolete-info)))

(ert-deftest use-package-test-recognize-function ()
  (should (use-package-recognize-function nil t))
  (should-not (use-package-recognize-function nil))
  (should (use-package-recognize-function t))
  (should (use-package-recognize-function 'sym))
  (should (use-package-recognize-function #'sym))
  (should (use-package-recognize-function (lambda () ...)))
  (should (use-package-recognize-function '(lambda () ...)))
  (should (use-package-recognize-function #'(lambda () ...)))

  (should-not (use-package-recognize-function 1))
  (should-not (use-package-recognize-function "Hello"))
  (should-not (use-package-recognize-function '(nil . nil))))

(ert-deftest use-package-test-normalize-function ()
  (should (equal (use-package-normalize-function nil) nil))
  (should (equal (use-package-normalize-function t) t))
  (should (equal (use-package-normalize-function 'sym) 'sym))
  (should (equal (use-package-normalize-function #'sym) 'sym))
  (should (equal (use-package-normalize-function '(lambda () ...)) '(lambda () ...)))
  (should (equal (use-package-normalize-function ''(lambda () ...)) '(lambda () ...)))
  (should (equal (use-package-normalize-function '#'(lambda () ...)) '(lambda () ...)))

  (should (equal (use-package-normalize-function 1) 1))
  (should (equal (use-package-normalize-function "Hello") "Hello"))
  (should (equal (use-package-normalize-function '(nil . nil)) '(nil . nil))))

(ert-deftest use-package-test/:disabled-1 ()
  (match-expansion
   (use-package foo :disabled t)
   `()))

(ert-deftest use-package-test/:preface-1 ()
  (match-expansion
   (use-package foo :preface (t))
   `(progn
      (eval-and-compile
        (t))
      (require 'foo nil nil))))

(ert-deftest use-package-test/:preface-2 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :preface (t))
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors
                "Cannot load foo: %S" nil
                (unless (featurep 'foo)
                  (load "foo" nil t))))
          (t))
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:preface-3 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo
       :preface (preface)
       :init (init)
       :config (config)
       :functions func
       :defines def)
     `(progn
        (eval-and-compile
          (defvar def)
          (declare-function func "foo")
          (eval-when-compile
            (with-demoted-errors
                "Cannot load foo: %S" nil
                (unless (featurep 'foo)
                  (load "foo" nil t))))
          (preface))
        (init)
        (require 'foo nil nil)
        (config)
        t))))

(ert-deftest use-package-test/:preface-4 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo
       :preface (preface)
       :init (init)
       :config (config)
       :functions func
       :defines def
       :defer t)
     `(progn
        (eval-and-compile
          (defvar def)
          (declare-function func "foo")
          (eval-when-compile
            (with-demoted-errors
                "Cannot load foo: %S" nil
                (unless (featurep 'foo)
                  (load "foo" nil t))))
          (preface))
        (init)
        (eval-after-load 'foo
          '(progn
             (config)
             t))))))

(ert-deftest use-package-test/:pin-1 ()
  (match-expansion
   (use-package foo :pin foo)
   `(progn
      (use-package-pin-package 'foo "foo")
      (require 'foo nil nil))))

(ert-deftest use-package-test/:pin-2 ()
  (match-expansion
   (use-package foo :pin "foo")
   `(progn
      (use-package-pin-package 'foo "foo")
      (require 'foo nil nil))))

(ert-deftest use-package-test-normalize/:ensure ()
  (flet ((norm (&rest args)
               (apply #'use-package-normalize/:ensure
                      'foopkg :ensure args)))
    (should (equal (norm '(t)) '(t)))
    (should (equal (norm '(nil)) '(nil)))
    (should (equal (norm '(sym)) '(sym)))
    (should-error (norm '(1)))
    (should-error (norm '("Hello")))))

(ert-deftest use-package-test/:ensure-1 ()
  (let ((use-package-always-ensure nil))
    (match-expansion
     (use-package foo :ensure t)
     `(progn
        (use-package-ensure-elpa 'foo '(t) 'nil)
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-2 ()
  (let ((use-package-always-ensure t))
    (match-expansion
     (use-package foo :ensure t)
     `(progn
        (use-package-ensure-elpa 'foo '(t) 'nil)
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-3 ()
  (let ((use-package-always-ensure nil))
    (match-expansion
     (use-package foo :ensure nil)
     `(progn
        (use-package-ensure-elpa 'foo '(nil) 'nil)
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-4 ()
  (let ((use-package-always-ensure t))
    (match-expansion
     (use-package foo :ensure nil)
     `(progn
        (use-package-ensure-elpa 'foo '(nil) 'nil)
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-5 ()
  (let ((use-package-always-ensure nil))
    (match-expansion
     (use-package foo :load-path "foo")
     `(progn
        (eval-and-compile
          (add-to-list 'load-path ,(pred stringp)))
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-6 ()
  (let ((use-package-always-ensure t))
    (match-expansion
     (use-package foo :load-path "foo")
     `(progn
        (eval-and-compile
          (add-to-list 'load-path ,(pred stringp)))
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-7 ()
  (let ((use-package-always-ensure nil))
    (match-expansion
     (use-package foo :ensure nil :load-path "foo")
     `(progn
        (use-package-ensure-elpa 'foo '(nil) 'nil)
        (eval-and-compile
          (add-to-list 'load-path ,(pred stringp)))
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-8 ()
  (let ((use-package-always-ensure t))
    (match-expansion
     (use-package foo :ensure nil :load-path "foo")
     `(progn
        (use-package-ensure-elpa 'foo '(nil) 'nil)
        (eval-and-compile
          (add-to-list 'load-path ,(pred stringp)))
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-9 ()
  (let ((use-package-always-ensure nil))
    (match-expansion
     (use-package foo :ensure t :load-path "foo")
     `(progn
        (use-package-ensure-elpa 'foo '(t) 'nil)
        (eval-and-compile
          (add-to-list 'load-path ,(pred stringp)))
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-10 ()
  (let ((use-package-always-ensure t))
    (match-expansion
     (use-package foo :ensure t :load-path "foo")
     `(progn
        (use-package-ensure-elpa 'foo '(t) 'nil)
        (eval-and-compile
          (add-to-list 'load-path ,(pred stringp)))
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-11 ()
  (let (tried-to-install)
    (flet ((use-package-ensure-elpa
            (name ensure state &optional no-refresh)
            (when ensure
              (setq tried-to-install name)))
           (require (&rest ignore)))
      (use-package foo :ensure t)
      (should (eq tried-to-install 'foo)))))

(ert-deftest use-package-test/:ensure-12 ()
  (let ((use-package-always-ensure t))
    (match-expansion
     (use-package foo :ensure bar)
     `(progn
        (use-package-ensure-elpa 'foo '(bar) 'nil)
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-13 ()
  (let ((use-package-always-ensure t))
    (match-expansion
     (use-package foo :ensure bar :ensure quux)
     `(progn
        (use-package-ensure-elpa 'foo '(bar quux) 'nil)
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-14 ()
  (match-expansion
   (use-package ess-site
     :ensure ess1
     :ensure ess2
     :ensure (ess3 :pin "melpa-unstable")
     :pin melpa-stable)
   `(progn
      (use-package-pin-package 'ess-site "melpa-stable")
      (use-package-ensure-elpa 'ess-site
                               '(ess1 ess2
                                      (ess3 . "melpa-unstable"))
                               'nil)
      (require 'ess-site nil nil))))

(ert-deftest use-package-test/:ensure-15 ()
  (let ((use-package-always-ensure t))
    (match-expansion
     (use-package foo
       :pin "elpa"
       :ensure bar
       :ensure (quux :pin "melpa"))
     `(progn
        (use-package-pin-package 'foo "elpa")
        (use-package-ensure-elpa 'foo
                                 '(bar
                                   (quux . "melpa"))
                                 'nil)
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:if-1 ()
  (match-expansion
   (use-package foo :if t)
   `(when t
      (require 'foo nil nil))))

(ert-deftest use-package-test/:if-2 ()
  (match-expansion
   (use-package foo :if (and t t))
   `(when (and t t)
      (require 'foo nil nil))))

(ert-deftest use-package-test/:if-3 ()
  (match-expansion
   (use-package foo :if nil)
   `(when nil
      (require 'foo nil nil))))

(ert-deftest use-package-test/:when-1 ()
  (match-expansion
   (use-package foo :when t)
   `(when t
      (require 'foo nil nil))))

(ert-deftest use-package-test/:when-2 ()
  (match-expansion
   (use-package foo :when (and t t))
   `(when (and t t)
      (require 'foo nil nil))))

(ert-deftest use-package-test/:when-3 ()
  (match-expansion
   (use-package foo :when nil)
   `(when nil
      (require 'foo nil nil))))

(ert-deftest use-package-test/:unless-1 ()
  (match-expansion
   (use-package foo :unless t)
   `(when (not t)
      (require 'foo nil nil))))

(ert-deftest use-package-test/:unless-2 ()
  (match-expansion
   (use-package foo :unless (and t t))
   `(when (not (and t t))
      (require 'foo nil nil))))

(ert-deftest use-package-test/:unless-3 ()
  (match-expansion
   (use-package foo :unless nil)
   `(unless nil
      (require 'foo nil nil))))

(ert-deftest use-package-test/:requires-1 ()
  (match-expansion
   (use-package foo :requires bar)
   `(when (featurep 'bar)
      (require 'foo nil nil))))

(ert-deftest use-package-test/:requires-2 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :requires bar)
     `(when (featurep 'bar)
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors
                "Cannot load foo: %S" nil
                (unless (featurep 'foo)
                  (load "foo" nil t)))))
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:requires-3 ()
  (match-expansion
   (use-package foo :requires (bar quux))
   `(when (not (member nil (mapcar #'featurep '(bar quux))))
      (require 'foo nil nil))))

(ert-deftest use-package-test/:requires-4 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :requires bar)
     `(when (featurep 'bar)
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (unless (featurep 'foo)
                                   (load "foo" nil t)))))
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:load-path-1 ()
  (match-expansion
   (use-package foo :load-path "bar")
   `(progn
      (eval-and-compile
        (add-to-list 'load-path
                     ,(pred (apply-partially
                             #'string=
                             (expand-file-name
                              "bar" user-emacs-directory)))))
      (require 'foo nil nil))))

(ert-deftest use-package-test/:load-path-2 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :load-path "bar")
     `(progn
        (eval-and-compile
          (add-to-list 'load-path
                       ,(pred (apply-partially
                               #'string=
                               (expand-file-name
                                "bar" user-emacs-directory)))))
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (unless (featurep 'foo)
                                   (load "foo" nil t)))))
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:load-path-3 ()
  (match-expansion
   (use-package foo :load-path ("bar" "quux"))
   `(progn
      (eval-and-compile
        (add-to-list 'load-path
                     ,(pred (apply-partially
                             #'string=
                             (expand-file-name
                              "bar" user-emacs-directory)))))
      (eval-and-compile
        (add-to-list 'load-path
                     ,(pred (apply-partially
                             #'string=
                             (expand-file-name
                              "quux" user-emacs-directory)))))
      (require 'foo nil nil))))

(ert-deftest use-package-test/:load-path-4 ()
  (match-expansion
   (use-package foo :load-path (lambda () (list "bar" "quux")))
   `(progn
      (eval-and-compile
        (add-to-list 'load-path
                     ,(pred (apply-partially
                             #'string=
                             (expand-file-name
                              "bar" user-emacs-directory)))))
      (eval-and-compile
        (add-to-list 'load-path
                     ,(pred (apply-partially
                             #'string=
                             (expand-file-name
                              "quux" user-emacs-directory)))))
      (require 'foo nil nil))))

(ert-deftest use-package-test/:no-require-1 ()
  (match-expansion
   (use-package foo :no-require t)
   `nil))

(ert-deftest use-package-test/:no-require-2 ()
  (match-expansion
   (use-package foo :no-require t :config (config))
   `(progn
      (config)
      t)))

(ert-deftest use-package-test/:no-require-3 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :no-require t)
     `(eval-and-compile
        (eval-when-compile
          (with-demoted-errors "Cannot load foo: %S" nil nil))))))

(defun use-package-test-normalize-bind (&rest args)
  (apply #'use-package-normalize-binder 'foo :bind args))

(ert-deftest use-package-test-normalize/:bind-1 ()
  (should (equal (use-package-test-normalize-bind
                  '(("C-a" . alpha)))
                 '(("C-a" . alpha)))))

(ert-deftest use-package-test-normalize/:bind-2 ()
  (should (equal (use-package-test-normalize-bind
                  '(("C-a" . alpha)
                    :map foo-map
                    ("C-b" . beta)))
                 '(("C-a" . alpha)
                   :map foo-map
                   ("C-b" . beta)))))

(ert-deftest use-package-test-normalize/:bind-3 ()
  (should (equal (use-package-test-normalize-bind
                  '(:map foo-map
                         ("C-a" . alpha)
                         ("C-b" . beta)))
                 '(:map foo-map
                        ("C-a" . alpha)
                        ("C-b" . beta)))))

(ert-deftest use-package-test/:bind-1 ()
  (match-expansion
   (use-package foo :bind ("C-k" . key1) ("C-u" . key2))
   `(progn
      (unless
          (fboundp 'key1)
        (autoload #'key1 "foo" nil t))
      (unless
          (fboundp 'key2)
        (autoload #'key2 "foo" nil t))
      (bind-keys :package foo
                 ("C-k" . key1)
                 ("C-u" . key2)))))

(ert-deftest use-package-test/:bind-2 ()
  (match-expansion
   (use-package foo :bind (("C-k" . key1) ("C-u" . key2)))
   `(progn
      (unless (fboundp 'key1)
        (autoload #'key1 "foo" nil t))
      (unless (fboundp 'key2)
        (autoload #'key2 "foo" nil t))
      (bind-keys :package foo
                 ("C-k" . key1)
                 ("C-u" . key2)))))

(ert-deftest use-package-test/:bind-3 ()
  (match-expansion
   (use-package foo :bind (:map my-map ("C-k" . key1) ("C-u" . key2)))
   `(progn
      (unless
          (fboundp 'key1)
        (autoload #'key1 "foo" nil t))
      (unless
          (fboundp 'key2)
        (autoload #'key2 "foo" nil t))
      (bind-keys :package foo :map my-map
                 ("C-k" . key1)
                 ("C-u" . key2)))))

(ert-deftest use-package-test/:bind-4 ()
  (should-error
   (match-expansion
    (use-package foo :bind :map my-map ("C-k" . key1) ("C-u" . key2))
    `(bind-keys :package foo))))

(ert-deftest use-package-test/:bind-5 ()
  (match-expansion
   (use-package foo :bind ("C-k" . key1) (:map my-map ("C-u" . key2)))
   `(progn
      (unless (fboundp 'key1)
        (autoload #'key1 "foo" nil t))
      (unless (fboundp 'key2)
        (autoload #'key2 "foo" nil t))
      (bind-keys :package foo
                 ("C-k" . key1)
                 :map my-map
                 ("C-u" . key2)))))

(ert-deftest use-package-test/:bind-6 ()
  (match-expansion
   (use-package foo
     :bind
     ("C-k" . key1)
     (:map my-map ("C-u" . key2))
     (:map my-map2 ("C-u" . key3)))
   `(progn
      (unless (fboundp 'key1)
        (autoload #'key1 "foo" nil t))
      (unless (fboundp 'key2)
        (autoload #'key2 "foo" nil t))
      (unless (fboundp 'key3)
        (autoload #'key3 "foo" nil t))
      (bind-keys :package foo
                 ("C-k" . key1)
                 :map my-map ("C-u" . key2)
                 :map my-map2 ("C-u" . key3)))))

(ert-deftest use-package-test/:bind-7 ()
  (match-expansion
   (use-package foo
     :ensure
     :bind ("C-c r" . browse-at-remote))
   `(progn
      (use-package-ensure-elpa 'foo '(t) 'nil)
      (unless (fboundp 'browse-at-remote)
        (autoload #'browse-at-remote "foo" nil t))
      (bind-keys :package foo ("C-c r" . browse-at-remote)))))

(ert-deftest use-package-test/:bind-8 ()
  (match-expansion
   (use-package foo
     :ensure
     :bind (:map foo-map
                 (("C-c r" . foo)
                  ("C-c r" . bar))))
   `(progn
      (use-package-ensure-elpa 'foo '(t) 'nil)
      (unless (fboundp 'foo)
        (autoload #'foo "foo" nil t))
      (unless (fboundp 'bar)
        (autoload #'bar "foo" nil t))
      (bind-keys :package foo :map foo-map
                 ("C-c r" . foo)
                 ("C-c r" . bar)))))

(ert-deftest use-package-test/:bind*-1 ()
  (match-expansion
   (use-package foo :bind* ("C-k" . key))
   `(progn
      (unless (fboundp 'key)
        (autoload #'key "foo" nil t))
      (bind-keys* :package foo ("C-k" . key)))))

(ert-deftest use-package-test/:bind-keymap-1 ()
  (match-expansion
   (use-package foo :bind-keymap ("C-k" . key))
   `(bind-key "C-k"
              #'(lambda nil
                  (interactive)
                  (use-package-autoload-keymap 'key 'foo nil)))))

(ert-deftest use-package-test/:bind-keymap*-1 ()
  (match-expansion
   (use-package foo :bind-keymap* ("C-k" . key))
   `(bind-key* "C-k"
               #'(lambda ()
                   (interactive)
                   (use-package-autoload-keymap 'key 'foo t)))))

(ert-deftest use-package-test/:interpreter-1 ()
  (match-expansion
   (use-package foo :interpreter "interp")
   `(progn
      (unless (fboundp 'foo)
        (autoload #'foo "foo" nil t))
      (add-to-list 'interpreter-mode-alist '("interp" . foo)))))

(ert-deftest use-package-test/:interpreter-2 ()
  (match-expansion
   (use-package foo :interpreter ("interp" . fun))
   `(progn
      (unless (fboundp 'fun)
        (autoload #'fun "foo" nil t))
      (add-to-list 'interpreter-mode-alist '("interp" . fun)))))

(ert-deftest use-package-test-normalize/:mode ()
  (flet ((norm (&rest args)
               (apply #'use-package-normalize/:mode
                      'foopkg :mode args)))
    (should (equal (norm '(".foo"))
                   '((".foo" . foopkg))))
    (should (equal (norm '(".foo" ".bar"))
                   '((".foo" . foopkg) (".bar" . foopkg))))
    (should (equal (norm '((".foo" ".bar")))
                   '((".foo" . foopkg) (".bar" . foopkg))))
    (should (equal (norm '((".foo")))
                   '((".foo" . foopkg))))
    (should (equal (norm '((".foo" . foo) (".bar" . bar)))
                   '((".foo" . foo) (".bar" . bar))))))

(ert-deftest use-package-test/:mode-1 ()
  (match-expansion
   (use-package foo :mode "interp")
   `(progn
      (unless (fboundp 'foo)
        (autoload #'foo "foo" nil t))
      (add-to-list 'auto-mode-alist '("interp" . foo)))))

(ert-deftest use-package-test/:mode-2 ()
  (match-expansion
   (use-package foo :mode ("interp" . fun))
   `(progn
      (unless (fboundp 'fun)
        (autoload #'fun "foo" nil t))
      (add-to-list 'auto-mode-alist '("interp" . fun)))))

(ert-deftest use-package-test/:magic-1 ()
  (match-expansion
   (use-package foo :magic "interp")
   `(progn
      (unless (fboundp 'foo)
        (autoload #'foo "foo" nil t))
      (add-to-list 'magic-mode-alist '("interp" . foo)))))

(ert-deftest use-package-test/:magic-2 ()
  (match-expansion
   (use-package foo :magic ("interp" . fun))
   `(progn
      (unless (fboundp 'fun)
        (autoload #'fun "foo" nil t))
      (add-to-list 'magic-mode-alist '("interp" . fun)))))

(ert-deftest use-package-test/:magic-fallback-1 ()
  (match-expansion
   (use-package foo :magic-fallback "interp")
   `(progn
      (unless (fboundp 'foo)
        (autoload #'foo "foo" nil t))
      (add-to-list 'magic-fallback-mode-alist '("interp" . foo)))))

(ert-deftest use-package-test/:magic-fallback-2 ()
  (match-expansion
   (use-package foo :magic-fallback ("interp" . fun))
   `(progn
      (unless (fboundp 'fun)
        (autoload #'fun "foo" nil t))
      (add-to-list 'magic-fallback-mode-alist '("interp" . fun)))))

(ert-deftest use-package-test/:commands-1 ()
  (match-expansion
   (use-package foo :commands bar)
   `(unless (fboundp 'bar)
      (autoload #'bar "foo" nil t))))

(ert-deftest use-package-test/:commands-2 ()
  (match-expansion
   (use-package foo :commands (bar quux))
   `(progn
      (unless (fboundp 'bar)
        (autoload #'bar "foo" nil t))
      (unless (fboundp 'quux)
        (autoload #'quux "foo" nil t)))))

(ert-deftest use-package-test/:commands-3 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :commands (bar quux))
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (unless (featurep 'foo)
                                   (load "foo" nil t)))))
        (unless (fboundp 'bar)
          (autoload #'bar "foo" nil t))
        (eval-when-compile
          (declare-function bar "foo"))
        (unless (fboundp 'quux)
          (autoload #'quux "foo" nil t))
        (eval-when-compile
          (declare-function quux "foo"))))))

(ert-deftest use-package-test/:commands-4 ()
  (match-expansion
   (use-package foo :commands bar :init (bar))
   `(progn
      (unless
          (fboundp 'bar)
        (autoload #'bar "foo" nil t))
      (bar))))

(ert-deftest use-package-test/:commands-5 ()
  (match-expansion
   (use-package gnus-harvest
     :load-path "foo"
     :commands gnus-harvest-install
     :demand t
     :config
     (if (featurep 'message-x)
         (gnus-harvest-install 'message-x)
       (gnus-harvest-install)))
   `(progn
      (eval-and-compile
        (add-to-list 'load-path ,(pred stringp)))
      (require 'gnus-harvest nil nil)
      (if (featurep 'message-x)
          (gnus-harvest-install 'message-x)
        (gnus-harvest-install))
      t)))

(ert-deftest use-package-test/:commands-6 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package gnus-harvest
       :load-path "foo"
       :commands gnus-harvest-install
       :demand t
       :config
       (if (featurep 'message-x)
           (gnus-harvest-install 'message-x)
         (gnus-harvest-install)))
     `(progn
        (eval-and-compile
          (add-to-list 'load-path ,(pred stringp)))
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load gnus-harvest: %S" nil
                                 (unless (featurep 'gnus-harvest)
                                   (load "gnus-harvest" nil t)))))
        (eval-when-compile
          (declare-function gnus-harvest-install "gnus-harvest"))
        (require 'gnus-harvest nil nil)
        (if
            (featurep 'message-x)
            (gnus-harvest-install 'message-x)
          (gnus-harvest-install))
        t))))

(ert-deftest use-package-test/:defines-1 ()
  (match-expansion
   (use-package foo :defines bar)
   `(require 'foo nil nil)))

(ert-deftest use-package-test/:defines-2 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :defines bar)
     `(progn
        (eval-and-compile
          (defvar bar)
          (eval-when-compile
            (with-demoted-errors
                "Cannot load foo: %S" nil
                (unless (featurep 'foo)
                  (load "foo" nil t)))))
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:functions-1 ()
  (match-expansion
   (use-package foo :functions bar)
   `(require 'foo nil nil)))

(ert-deftest use-package-test/:functions-2 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :functions bar)
     `(progn
        (eval-and-compile
          (declare-function bar "foo")
          (eval-when-compile
            (with-demoted-errors
                "Cannot load foo: %S" nil
                (unless (featurep 'foo)
                  (load "foo" nil t)))))
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:functions-3 ()
  (match-expansion
   (use-package foo :defer t :functions bar)
   `nil))

(ert-deftest use-package-test/:functions-4 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :defer t :functions bar)
     `(eval-and-compile
        (declare-function bar "foo")
        (eval-when-compile
          (with-demoted-errors "Cannot load foo: %S" nil
                               (unless (featurep 'foo)
                                 (load "foo" nil t))))))))

(ert-deftest use-package-test/:functions-5 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :defer t :config (config) :functions bar)
     `(progn
        (eval-and-compile
          (declare-function bar "foo")
          (eval-when-compile
            (with-demoted-errors
                "Cannot load foo: %S" nil
                (unless (featurep 'foo)
                  (load "foo" nil t)))))
        (eval-after-load 'foo
          '(progn
             (config)
             t))))))

(ert-deftest use-package-test/:defer-1 ()
  (match-expansion
   (use-package foo)
   `(require 'foo nil nil)))

(ert-deftest use-package-test/:defer-2 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo)
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (unless (featurep 'foo)
                                   (load "foo" nil t)))))
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:defer-3 ()
  (match-expansion
   (use-package foo :defer t)
   `nil))

(ert-deftest use-package-test/:defer-4 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :defer t)
     `(eval-and-compile
        (eval-when-compile
          (with-demoted-errors "Cannot load foo: %S" nil
                               (unless (featurep 'foo)
                                 (load "foo" nil t))))))))

(ert-deftest use-package-test-normalize/:hook ()
  (flet ((norm (&rest args)
               (apply #'use-package-normalize/:hook
                      'foopkg :hook args)))
    (should-error (norm nil))
    (should (equal (norm '(bar))
                   '((bar . foopkg))))
    (should (equal (norm '((bar . baz)))
                   '((bar . baz))))
    (should (equal (norm '(((bar baz) . quux)))
                   '(((bar baz) . quux))))
    (should (equal (norm '(bar baz))
                   '(((bar baz) . foopkg))))
    (should (equal (norm '((bar baz) (quux bow)))
                   '(((bar baz) . foopkg) ((quux bow) . foopkg))))
    (should (equal (norm '((bar . baz) (quux . bow)))
                   '((bar . baz) (quux . bow))))
    (should (equal (norm '(((bar1 bar2) . baz) ((quux1 quux2) . bow)))
                   '(((bar1 bar2) . baz) ((quux1 quux2) . bow))))))

(ert-deftest use-package-test/:hook-1 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo
       :bind (("C-a" . key))
       :hook (hook . fun))
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors
                "Cannot load foo: %S" nil
                (unless (featurep 'foo)
                  (load "foo" nil t)))))
        (unless
            (fboundp 'key)
          (autoload #'key "foo" nil t))
        (eval-when-compile
          (declare-function key "foo"))
        (unless
            (fboundp 'fun)
          (autoload #'fun "foo" nil t))
        (eval-when-compile
          (declare-function fun "foo"))
        (add-hook 'hook-hook #'fun)
        (bind-keys :package foo ("C-a" . key))))))

(ert-deftest use-package-test/:hook-2 ()
  (match-expansion
   (use-package foo
     :hook (hook . fun))
   `(progn
      (unless (fboundp 'fun)
        (autoload #'fun "foo" nil t))
      (add-hook 'hook-hook #'fun))))

(ert-deftest use-package-test/:hook-3 ()
  (let ((use-package-hook-name-suffix nil))
    (match-expansion
     (use-package foo
       :hook (hook . fun))
     `(progn
        (unless (fboundp 'fun)
          (autoload #'fun "foo" nil t))
        (add-hook 'hook #'fun)))))

(ert-deftest use-package-test/:hook-4 ()
  (let ((use-package-hook-name-suffix "-special"))
    (match-expansion
     (use-package foo
       :hook (hook . fun))
     `(progn
        (unless (fboundp 'fun)
          (autoload #'fun "foo" nil t))
        (add-hook 'hook-special #'fun)))))

(ert-deftest use-package-test/:hook-5 ()
  (match-expansion
   (use-package erefactor
     :load-path "foo"
     :after elisp-mode
     :load t
     :hook (emacs-lisp-mode
            . (lambda ()
                (bind-key "\C-c\C-v" erefactor-map emacs-lisp-mode-map))))
   `(progn
      (eval-and-compile
        (add-to-list 'load-path ,(pred stringp)))
      (eval-after-load 'elisp-mode
        '(progn
           (require 'erefactor nil nil)
           (add-hook
            'emacs-lisp-mode-hook
            #'(lambda nil
                (bind-key "" erefactor-map emacs-lisp-mode-map))))))))

(ert-deftest use-package-test/:hook-6 ()
  (match-expansion
   (use-package erefactor
     :load-path "foo"
     :after elisp-mode
     :hook (emacs-lisp-mode . function))
   `(progn
      (eval-and-compile
        (add-to-list 'load-path ,(pred stringp)))
      (eval-after-load 'elisp-mode
        '(progn
           (unless (fboundp 'function)
             (autoload #'function "erefactor" nil t))
           (add-hook 'emacs-lisp-mode-hook #'function))))))

(ert-deftest use-package-test/:hook-7 ()
  (match-expansion
   (use-package erefactor
     :load-path "foo"
     :after elisp-mode
     :hook (emacs-lisp-mode . (lambda () (function))))
   `(progn
      (eval-and-compile
        (add-to-list 'load-path ,(pred stringp)))
      (eval-after-load 'elisp-mode
        '(progn
           (require 'erefactor nil nil)
           (add-hook 'emacs-lisp-mode-hook #'(lambda nil (function))))))))

(ert-deftest use-package-test-normalize/:custom ()
  (flet ((norm (&rest args)
               (apply #'use-package-normalize/:custom
                      'foopkg :custom args)))
    (should-error (norm nil))
    (should-error (norm '(bar)))
    ;; (should-error (norm '((foo bar baz quux))))
    (should (equal (norm '(foo bar)) '((foo bar))))
    ;; (should-error (norm '(foo bar baz)))
    ;; (should (equal (norm '(foo bar "baz"))
    ;;                '((foo bar baz))))
    ))


(ert-deftest use-package-test/:custom-1 ()
  (match-expansion
   (use-package foo :custom (foo bar))
   `(progn
      (let
          ((custom--inhibit-theme-enable nil))
        (unless (memq 'use-package custom-known-themes)
          (deftheme use-package)
          (enable-theme 'use-package)
          (setq custom-enabled-themes (remq 'use-package custom-enabled-themes)))
        (custom-theme-set-variables 'use-package
                                    '(foo bar nil nil "Customized with use-package foo")))
      (require 'foo nil nil))))

(ert-deftest use-package-test/:custom-with-comment1 ()
  (match-expansion
   (use-package foo :custom (foo bar "commented"))
   `(progn
      (let
          ((custom--inhibit-theme-enable nil))
        (unless (memq 'use-package custom-known-themes)
          (deftheme use-package)
          (enable-theme 'use-package)
          (setq custom-enabled-themes (remq 'use-package custom-enabled-themes)))
        (custom-theme-set-variables 'use-package
                                    '(foo bar nil nil "commented")))
      (require 'foo nil nil))))

(ert-deftest use-package-test/:custom-face-1 ()
  (match-expansion
   (use-package foo :custom-face (foo ((t (:background "#e4edfc")))))
   `(progn
      (custom-set-faces (backquote (foo ((t (:background "#e4edfc"))))))
      (require 'foo nil nil))))

(ert-deftest use-package-test/:init-1 ()
  (match-expansion
   (use-package foo :init (init))
   `(progn
      (init)
      (require 'foo nil nil))))

(ert-deftest use-package-test/:init-2 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :init (init))
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (unless (featurep 'foo)
                                   (load "foo" nil t)))))
        (init)
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:catch-1 ()
  (match-expansion
   (use-package foo :catch t)
   `(progn
      (defvar ,_
        #'(lambda (keyword err)
            (let ((msg (format "%s/%s: %s" 'foo keyword
                               (error-message-string err))))
              (display-warning 'use-package msg :error))))
      (condition-case-unless-debug err
          (require 'foo nil nil)
        (error
         (funcall ,_ :catch err))))))

(ert-deftest use-package-test/:catch-2 ()
  (match-expansion
   (use-package foo :catch nil)
   `(require 'foo nil nil)))

(ert-deftest use-package-test/:catch-3 ()
  (match-expansion
   (use-package foo :catch (lambda (keyword error)))
   `(progn
      (defvar ,_ (lambda (keyword error)))
      (condition-case-unless-debug err
          (require 'foo nil nil)
        (error
         (funcall ,_ :catch err))))))

(ert-deftest use-package-test/:after-1 ()
  (match-expansion
   (use-package foo :after bar)
   `(eval-after-load 'bar
      '(require 'foo nil nil))))

(ert-deftest use-package-test/:after-2 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :after bar)
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (unless (featurep 'foo)
                                   (load "foo" nil t)))))
        (eval-after-load 'bar
          '(require 'foo nil nil))))))

(ert-deftest use-package-test/:after-3 ()
  (match-expansion
   (use-package foo :after (bar quux))
   `(eval-after-load 'quux
      '(eval-after-load 'bar
         '(require 'foo nil nil)))))

(ert-deftest use-package-test/:after-4 ()
  (match-expansion
   (use-package foo :after (:all bar quux))
   `(eval-after-load 'quux
      '(eval-after-load 'bar
         '(require 'foo nil nil)))))

(ert-deftest use-package-test/:after-5 ()
  (match-expansion
   (use-package foo :after (:any bar quux))
   `(progn
      (defvar ,_ nil)
      (defvar ,_ nil)
      (defvar ,_
        #'(lambda nil
            (if ,_ ,_
              (setq ,_ t ,_
                    (require 'foo nil nil)))))
      (eval-after-load 'bar
        '(funcall ,_))
      (eval-after-load 'quux
        '(funcall ,_)))))

(ert-deftest use-package-test/:after-6 ()
  (match-expansion
   (use-package foo :after (:all (:any bar quux) bow))
   `(progn
      (defvar ,_ nil)
      (defvar ,_ nil)
      (defvar ,_
        #'(lambda nil
            (if ,_ ,_
              (setq ,_ t ,_
                    (require 'foo nil nil)))))
      (eval-after-load 'bow
        '(progn
           (eval-after-load 'bar
             '(funcall ,_))
           (eval-after-load 'quux
             '(funcall ,_)))))))

(ert-deftest use-package-test/:after-7 ()
  (match-expansion
   (use-package foo :after (:any (:all bar quux) bow))
   `(progn
      (defvar ,_ nil)
      (defvar ,_ nil)
      (defvar ,_
        #'(lambda nil
            (if ,_ ,_
              (setq ,_ t ,_
                    (require 'foo nil nil)))))
      (eval-after-load 'quux
        '(eval-after-load 'bar
           '(funcall ,_)))
      (eval-after-load 'bow
        '(funcall ,_)))))

(ert-deftest use-package-test/:after-8 ()
  (match-expansion
   (use-package foo :after (:all (:any bar quux) (:any bow baz)))
   `(progn
      (defvar ,_ nil)
      (defvar ,_ nil)
      (defvar ,_
        #'(lambda nil
            (if ,_ ,_
              (setq ,_ t ,_
                    (require 'foo nil nil)))))
      (eval-after-load 'bow
        '(progn
           (eval-after-load 'bar
             '(funcall ,_))
           (eval-after-load 'quux
             '(funcall ,_))))
      (eval-after-load 'baz
        '(progn
           (eval-after-load 'bar
             '(funcall ,_))
           (eval-after-load 'quux
             '(funcall ,_)))))))

(ert-deftest use-package-test/:after-9 ()
  (match-expansion
   (use-package foo :after (:any (:all bar quux) (:all bow baz)))
   `(progn
      (defvar ,_ nil)
      (defvar ,_ nil)
      (defvar ,_
        #'(lambda nil
            (if ,_ ,_
              (setq ,_ t ,_
                    (require 'foo nil nil)))))
      (eval-after-load 'quux
        '(eval-after-load 'bar
           '(funcall ,_)))
      (eval-after-load 'baz
        '(eval-after-load 'bow
           '(funcall ,_))))))

(ert-deftest use-package-test/:after-10 ()
  (match-expansion
   (use-package foo :after (:any (:all bar quux) (:any bow baz)))
   `(progn
      (defvar ,_ nil)
      (defvar ,_ nil)
      (defvar ,_
        #'(lambda nil
            (if ,_ ,_
              (setq ,_ t ,_
                    (require 'foo nil nil)))))
      (eval-after-load 'quux
        '(eval-after-load 'bar
           '(funcall ,_)))
      (eval-after-load 'bow
        '(funcall ,_))
      (eval-after-load 'baz
        '(funcall ,_)))))

(ert-deftest use-package-test/:demand-1 ()
  (match-expansion
   (use-package foo :demand t)
   `(require 'foo nil nil)))

(ert-deftest use-package-test/:demand-2 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :demand t)
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (unless (featurep 'foo)
                                   (load "foo" nil t)))))
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:demand-3 ()
  (match-expansion
   (use-package foo :demand t :config (config))
   `(progn
      (require 'foo nil nil)
      (config)
      t)))

(ert-deftest use-package-test/:demand-4 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :demand t :config (config))
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (unless (featurep 'foo)
                                   (load "foo" nil t)))))
        (require 'foo nil nil)
        (config)
        t))))

(ert-deftest use-package-test/:demand-5 ()
  ;; #529 - :demand should not override an explicit use of :after
  (match-expansion
   (use-package foo :demand t :after bar)
   `(eval-after-load 'bar
      '(require 'foo nil nil))))

(ert-deftest use-package-test/:demand-6 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :demand t :after bar)
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (unless (featurep 'foo)
                                   (load "foo" nil t)))))
        (eval-after-load 'bar
          '(require 'foo nil nil))))))

(ert-deftest use-package-test/:demand-7 ()
  (match-expansion
   (use-package counsel
     :load-path "foo"
     :after ivy
     :demand t
     :diminish
     :bind (("C-*" . counsel-org-agenda-headlines)
            ("M-x" . counsel-M-x))
     :commands (counsel-minibuffer-history
                counsel-find-library
                counsel-unicode-char)
     :preface (preface-code)
     :init
     ;; This is actually wrong, but it's just part of the example.
     (define-key minibuffer-local-map (kbd "M-r")
       'counsel-minibuffer-history))
   `(progn
      (eval-and-compile
        (add-to-list 'load-path ,(pred stringp)))
      (eval-and-compile
        (preface-code))
      (eval-after-load 'ivy
        '(progn
           (define-key minibuffer-local-map (kbd "M-r")
             'counsel-minibuffer-history)
           (require 'counsel nil nil)
           (if (fboundp 'diminish)
               (diminish 'counsel-mode))
           (bind-keys :package counsel
                      ("C-*" . counsel-org-agenda-headlines)
                      ("M-x" . counsel-M-x)))))))

(ert-deftest use-package-test/:config-1 ()
  (match-expansion
   (use-package foo :config (config))
   `(progn
      (require 'foo nil nil)
      (config)
      t)))

(ert-deftest use-package-test/:config-2 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :config (config))
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (unless (featurep 'foo)
                                   (load "foo" nil t)))))
        (require 'foo nil nil)
        (config)
        t))))

(ert-deftest use-package-test/:config-3 ()
  (match-expansion
   (use-package foo :defer t :config (config))
   `(eval-after-load 'foo
      '(progn
         (config)
         t))))

(ert-deftest use-package-test/:config-4 ()
  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :defer t :config (config))
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (unless (featurep 'foo)
                                   (load "foo" nil t)))))
        (eval-after-load 'foo
          '(progn
             (config)
             t))))))

(ert-deftest use-package-test/pre-post-hooks-with-:config ()
  (let ((use-package-inject-hooks t))
    (match-expansion
     (use-package foo :config (config))
     `(progn
       (when
           (run-hook-with-args-until-failure 'use-package--foo--pre-init-hook)
         (run-hooks 'use-package--foo--post-init-hook))
       (require 'foo nil nil)
       (when
           (run-hook-with-args-until-failure 'use-package--foo--pre-config-hook)
         (config)
         (run-hooks 'use-package--foo--post-config-hook))
       t))))

(ert-deftest use-package-test/pre-post-hooks-without-:config ()
  ;; https://github.com/jwiegley/use-package/issues/785
  (let ((use-package-inject-hooks t))
    (match-expansion
     (use-package foo)
     `(progn
        (when
            (run-hook-with-args-until-failure 'use-package--foo--pre-init-hook)
          (run-hooks 'use-package--foo--post-init-hook))
        (require 'foo nil nil)
        (when
            (run-hook-with-args-until-failure 'use-package--foo--pre-config-hook)
          t
          (run-hooks 'use-package--foo--post-config-hook))
        t))))

(ert-deftest use-package-test-normalize/:diminish ()
  (should (equal (use-package-normalize-diminish 'foopkg :diminish nil)
                 '(foopkg-mode)))
  (should (equal (use-package-normalize-diminish 'foopkg :diminish 'bar)
                 '(bar)))
  (should (equal (use-package-normalize-diminish 'foopkg :diminish "bar")
                 '((foopkg-mode . "bar"))))
  (should (equal (use-package-normalize-diminish 'foopkg :diminish 'foo-mode)
                 '(foo-mode)))
  (should (equal (use-package-normalize-diminish 'foopkg :diminish '(foo . "bar"))
                 '((foo . "bar")))))

(ert-deftest use-package-test/:diminish-1 ()
  (match-expansion
   (use-package foo :diminish nil)
   `(progn
      (require 'foo nil nil)
      (if (fboundp 'diminish)
          (diminish 'foo-mode)))))

(ert-deftest use-package-test/:diminish-2 ()
  (match-expansion
   (use-package foo :diminish bar)
   `(progn
      (require 'foo nil nil)
      (if (fboundp 'diminish)
          (diminish 'bar)))))

(ert-deftest use-package-test/:diminish-3 ()
  (match-expansion
   (use-package foo :diminish "bar")
   `(progn
      (require 'foo nil nil)
      (if (fboundp 'diminish)
          (diminish 'foo-mode "bar")))))

(ert-deftest use-package-test/:diminish-4 ()
  (match-expansion
   (use-package foo :diminish (foo . "bar"))
   `(progn
      (require 'foo nil nil)
      (if (fboundp 'diminish)
          (diminish 'foo "bar")))))

(ert-deftest use-package-test-normalize/:delight ()
  (should (equal `((foo-mode nil foo))
                 (use-package-normalize/:delight 'foo :delight nil)))
  (should (equal `((foo-mode nil foo-mode))
                 (use-package-normalize/:delight 'foo-mode :delight nil)))
  (should (equal `((bar-mode nil foo))
                 (use-package-normalize/:delight 'foo :delight '(bar-mode))))
  (should (equal `((bar-mode nil :major))
                 (use-package-normalize/:delight 'foo :delight '((bar-mode nil :major)))))
  (should (equal `((foo-mode "abc" foo))
                 (use-package-normalize/:delight 'foo :delight '("abc"))))
  (should (equal `((foo-mode (:eval 1) foo))
                 (use-package-normalize/:delight 'foo :delight '('(:eval 1)))))
  (should (equal (use-package-normalize/:delight 'foo :delight '((a-mode) (b-mode " b")))
                 `((a-mode nil foo) (b-mode " b" foo))))
  (should-error (use-package-normalize/:delight 'foo :delight '((:eval 1)))))

(ert-deftest use-package-test/:delight-1 ()
  (match-expansion
   (use-package foo :delight)
   `(progn
      (require 'foo nil nil)
      (if (fboundp 'delight)
          (delight '((foo-mode nil foo)))))))

(ert-deftest use-package-test/:delight-2 ()
  (should-error
   (match-expansion
    (use-package foo :delight nil)
    `(progn
       (require 'foo nil nil)
       (if (fboundp 'diminish)
           (diminish 'foo-mode))))))

(ert-deftest use-package-test/:delight-3 ()
  (match-expansion
   (use-package foo :delight bar)
   `(progn
      (require 'foo nil nil)
      (if (fboundp 'delight)
          (delight '((bar nil foo)))))))

(ert-deftest use-package-test/:delight-4 ()
  (match-expansion
   (use-package foo :delight "bar")
   `(progn
      (require 'foo nil nil)
      (if (fboundp 'delight)
          (delight '((foo-mode "bar" foo)))))))

(ert-deftest use-package-test/:delight-5 ()
  (should-error
   (match-expansion
    (use-package foo :delight (foo . "bar"))
    `(progn
       (require 'foo nil nil)
       (if (fboundp 'diminish)
           (diminish 'foo "bar"))))))

(ert-deftest use-package-test/:delight-6 ()
  (match-expansion
   (use-package foo :delight (foo "bar"))
   `(progn
      (require 'foo nil nil)
      (if (fboundp 'delight)
          (delight '((foo "bar" foo)))))))

(ert-deftest use-package-test/334-1 ()
  (let (foo1-map foo2-map
                 bar1-func1
                 bar1-func2
                 bar2-func1
                 bar2-func2
                 bar3-func1
                 bar3-func2
                 bar4-func1
                 bar4-func2)
    (match-expansion
     (bind-keys :map foo1-map
                ("Y" . foo1)
                :prefix "y"
                :prefix-map bar1-prefix-map
                ("y" . bar1-func1)
                ("f" . bar1-func2)
                :prefix "y"
                :prefix-map bar2-prefix-map
                ("y" . bar2-func1)
                ("f" . bar2-func2)
                :map foo2-map
                ("Y" . foo2)
                :prefix "y"
                :prefix-map bar3-prefix-map
                ("y" . bar3-func1)
                ("f" . bar3-func2)
                :prefix "y"
                :prefix-map bar4-prefix-map
                ("y" . bar4-func1)
                ("f" . bar4-func2))
     `(progn
        (bind-key "Y" #'foo1 foo1-map nil)
        (defvar bar1-prefix-map)
        (define-prefix-command 'bar1-prefix-map)
        (bind-key "y" 'bar1-prefix-map foo1-map nil)
        (bind-key "y" #'bar1-func1 bar1-prefix-map nil)
        (bind-key "f" #'bar1-func2 bar1-prefix-map nil)
        (defvar bar2-prefix-map)
        (define-prefix-command 'bar2-prefix-map)
        (bind-key "y" 'bar2-prefix-map foo1-map nil)
        (bind-key "y" #'bar2-func1 bar2-prefix-map nil)
        (bind-key "f" #'bar2-func2 bar2-prefix-map nil)
        (bind-key "Y" #'foo2 foo2-map nil)
        (defvar bar3-prefix-map)
        (define-prefix-command 'bar3-prefix-map)
        (bind-key "y" 'bar3-prefix-map foo2-map nil)
        (bind-key "y" #'bar3-func1 bar3-prefix-map nil)
        (bind-key "f" #'bar3-func2 bar3-prefix-map nil)
        (defvar bar4-prefix-map)
        (define-prefix-command 'bar4-prefix-map)
        (bind-key "y" 'bar4-prefix-map foo2-map nil)
        (bind-key "y" #'bar4-func1 bar4-prefix-map nil)
        (bind-key "f" #'bar4-func2 bar4-prefix-map nil)))))

(ert-deftest use-package-test/334-2 ()
  (let (w3m-lnum-mode-map
        w3m-print-current-url
        w3m-lnum-print-this-url
        w3m-print-this-url)
    (match-expansion
     (bind-keys :map w3m-lnum-mode-map
                :prefix "y"
                :prefix-map w3m-y-prefix-map
                ("y" . w3m-print-current-url)
                ("f" . w3m-lnum-print-this-url)
                ("t" . w3m-print-this-url))
     `(progn
        (defvar w3m-y-prefix-map)
        (define-prefix-command 'w3m-y-prefix-map)
        (bind-key "y" 'w3m-y-prefix-map w3m-lnum-mode-map nil)
        (bind-key "y" #'w3m-print-current-url w3m-y-prefix-map nil)
        (bind-key "f" #'w3m-lnum-print-this-url w3m-y-prefix-map nil)
        (bind-key "t" #'w3m-print-this-url w3m-y-prefix-map nil)))))

(ert-deftest use-package-test/482-1 ()
  (match-expansion
   (use-package simple
     :bind-keymap ("C-t " . my/transpose-map)
     :bind (:map my/transpose-map
	         ("w" . transpose-words)))
   `(progn
      (unless (fboundp 'transpose-words)
        (autoload #'transpose-words "simple" nil t))
      (bind-key "C-t "
                #'(lambda nil
                    (interactive)
                    (use-package-autoload-keymap 'my/transpose-map 'simple nil)))
      (bind-keys :package simple :map my/transpose-map
                 ("w" . transpose-words)))))

(ert-deftest use-package-test/482-2 ()
  (match-expansion
   (use-package simple
     :bind (:prefix-map my/transpose-map
                        :prefix "C-t"
                        ("w" . transpose-words)))
   `(progn
      (unless (fboundp 'transpose-words)
        (autoload #'transpose-words "simple" nil t))
      (bind-keys :package simple
                 :prefix-map my/transpose-map
                 :prefix "C-t"
                 ("w" . transpose-words)))))

(ert-deftest use-package-test/482-3 ()
  (match-expansion
   (bind-keys :package simple
              :prefix-map my/transpose-map
              :prefix "C-t"
              ("w" . transpose-words))
   `(progn
      (defvar my/transpose-map)
      (define-prefix-command 'my/transpose-map)
      (bind-key "C-t" 'my/transpose-map nil nil)
      (bind-key "w" #'transpose-words my/transpose-map nil))))

(ert-deftest use-package-test/538 ()
  (match-expansion
   (use-package mu4e
     :commands (mu4e)
     :bind (("<f9>" . mu4e))
     :init
     :config
     (config))
   `(progn
      (unless (fboundp 'mu4e)
        (autoload #'mu4e "mu4e" nil t))
      (eval-after-load 'mu4e
        '(progn (config) t))
      (bind-keys :package mu4e ("<f9>" . mu4e)))))

(ert-deftest use-package-test/543 ()
  (match-expansion
   (use-package hydra
     :ensure)
   `(progn
      (use-package-ensure-elpa 'hydra '(t) 'nil)
      (require 'hydra nil nil))))

(ert-deftest use-package-test/545 ()
  (match-expansion
   (use-package spacemacs-theme
     :ensure t
     :init                                 ; or :config
     (load-theme 'spacemacs-dark t)
     )
   `(progn
      (use-package-ensure-elpa 'spacemacs-theme '(t) 'nil)
      (load-theme 'spacemacs-dark t)
      (require 'spacemacs-theme nil nil))
   ))

(ert-deftest use-package-test/550 ()
  (match-expansion
   (use-package company-try-hard
     :ensure t
     :bind
     ("C-c M-/" . company-try-hard)
     (:map company-active-map
           ("C-c M-/" . company-try-hard)))
   `(progn
      (use-package-ensure-elpa 'company-try-hard
                               '(t)
                               'nil)
      (unless
          (fboundp 'company-try-hard)
        (autoload #'company-try-hard "company-try-hard" nil t))
      (bind-keys :package company-try-hard
                 ("C-c M-/" . company-try-hard)
                 :map company-active-map
                 ("C-c M-/" . company-try-hard)))))

(ert-deftest use-package-test/558 ()
  (match-expansion
   (bind-keys* :package org-ref
               ("C-c C-r" . org-ref-helm-insert-cite-link))
   `(bind-key "C-c C-r" #'org-ref-helm-insert-cite-link override-global-map nil)))

(ert-deftest use-package-test/560 ()
  (flet ((executable-find (name)))
    (let (notmuch-command)
      (match-expansion
       (use-package notmuch
         :preface (setq-default notmuch-command (executable-find "notmuch"))
         :if notmuch-command
         :requires foo
         :load-path "foo"
         :defines var)
       `(progn
          (eval-and-compile
            (add-to-list 'load-path ,(pred stringp)))
          (when (featurep 'foo)
            (eval-and-compile
              (setq-default notmuch-command
                            (executable-find "notmuch")))
            (when (symbol-value 'notmuch-command)
              (require 'notmuch nil nil))))))))

(ert-deftest use-package-test/572-1 ()
  (let ((use-package-always-defer t))
    (match-expansion
     (use-package auth-password-store
       :after auth-source
       :init
       (setq auth-sources '(password-store)))
     `(eval-after-load 'auth-source
        '(setq auth-sources '(password-store))))))

(ert-deftest use-package-test/572-2 ()
  (let ((use-package-always-defer t))
    (match-expansion
     (use-package ivy-hydra :after ivy)
     `nil)))

(ert-deftest use-package-test/572-3 ()
  (let ((use-package-always-defer t)
        (use-package-defaults
         (let ((defaults (copy-alist use-package-defaults)))
           (setcdr (assq :defer defaults)
                   '(use-package-always-defer
                     (lambda (name args)
                       (and use-package-always-defer
                            (not (plist-member args :after))
                            (not (plist-member args :defer))
                            (not (plist-member args :demand))))))
           defaults)))
    (match-expansion
     (use-package ivy-hydra :after ivy)
     `(eval-after-load 'ivy
        '(require 'ivy-hydra nil nil)))))

(ert-deftest use-package-test/575-1 ()
  (match-expansion
   (use-package helm
     :defer t
     :after (:any ido dired)
     :config
     (message "test. helm start"))
   `(progn
      (defvar ,_ nil)
      (defvar ,_ nil)
      (defvar ,_
        #'(lambda nil
            (if ,_ ,_
              (setq ,_ t ,_
                    (eval-after-load 'helm
                      '(progn
                         (message "test. helm start")
                         t))))))
      (eval-after-load 'ido
        '(funcall ,_))
      (eval-after-load 'dired
        '(funcall ,_)))))

(ert-deftest use-package-test/575-2 ()
  (match-expansion
   (use-package helm
     :defer t
     :bind ("C-c d" . helm-mini)
     :config
     (message "test. helm start"))
   `(progn
      (unless (fboundp 'helm-mini)
        (autoload #'helm-mini "helm" nil t))
      (eval-after-load 'helm
        '(progn
           (message "test. helm start")
           t))
      (bind-keys :package helm ("C-c d" . helm-mini)))))

(ert-deftest use-package-test/585 ()
  (match-expansion
   (use-package bug
     :bind (:map bug-map ("C-a" . alpha))
     :bind (("C-b" . beta)))
   `(progn
      (unless (fboundp 'alpha)
        (autoload #'alpha "bug" nil t))
      (unless (fboundp 'beta)
        (autoload #'beta "bug" nil t))
      (bind-keys :package bug :map bug-map
                 ("C-a" . alpha))
      (bind-keys :package bug
                 ("C-b" . beta)))))

(ert-deftest use-package-test/589 ()
  (let ((use-package-verbose t)
        (use-package-expand-minimally t)
        debug-on-error
        warnings)
    (flet ((display-warning (_ msg _) (push msg warnings)))
      (progn
        (macroexpand-1
         '(use-package ediff :defer t (setq my-var t)))
        (should (= (and (> (length warnings) 0)
                        (string-match ":defer wants exactly one argument"
                                      (car warnings))) 44))))))

(ert-deftest use-package-test/591 ()
  (let ((use-package-defaults
         (cons '(:if (lambda (name _) `(locate-library ,name)) t)
               use-package-defaults)))
    (match-expansion
     (use-package nonexistent
       :hook lisp-mode)
     `(when (locate-library nonexistent)
        (unless (fboundp 'nonexistent)
          (autoload #'nonexistent "nonexistent" nil t))
        (add-hook 'lisp-mode-hook #'nonexistent)))))

(ert-deftest bind-key/:prefix-map ()
  (match-expansion
   (bind-keys :prefix "<f1>"
              :prefix-map my/map)
   `(progn
      (defvar my/map)
      (define-prefix-command 'my/map)
      (bind-key "<f1>" 'my/map nil nil))))


(ert-deftest bind-key/845 ()
  (defvar test-map (make-keymap))
  (bind-key "<f1>" 'ignore 'test-map)
  (should (eq (lookup-key test-map (kbd "<f1>")) 'ignore))
  (let ((binding (cl-find "<f1>" personal-keybindings :test 'string= :key 'caar)))
    (message "test-map %s" test-map)
    (message "binding %s" binding)
    (should (eq (cdar binding) 'test-map))
    (should (eq (nth 1 binding) 'ignore))
    (should (eq (nth 2 binding) nil))))

;; Local Variables:
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;;; use-package-tests.el ends here
