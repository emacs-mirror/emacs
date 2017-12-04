;;; use-package-tests.el --- Tests for use-package.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
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

(require 'ert)
(require 'use-package)

(setq use-package-always-ensure nil
      use-package-verbose 'errors
      use-package-expand-minimally t
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

(defmacro match-expansion (form &rest value)
  `(should (pcase (expand-minimally ,form)
             ,@(mapcar #'(lambda (x) (list x t)) value))))

(defun fix-expansion ()
  (interactive)
  (save-excursion
    (unless (looking-at "(match-expansion")
      (backward-up-list))
    (when (looking-at "(match-expansion")
      (search-forward "(use-package")
      (goto-char (match-beginning 0))
      (let ((decl (read (current-buffer))))
        (kill-sexp)
        (let ((use-package-verbose 'errors)
              (use-package-expand-minimally t))
          (insert ?\n ?\` (pp-to-string (macroexpand-1 decl))))))))

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
  (should (equal (use-package-normalize-function (lambda () ...)) (lambda () ...)))
  (should (equal (use-package-normalize-function '(lambda () ...)) (lambda () ...)))
  (should (equal (use-package-normalize-function #'(lambda () ...)) (lambda () ...)))

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
                (load "foo" nil t)))
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
                (load "foo" nil t)))
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
                (load "foo" nil t)))
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
    (should (equal (norm '(t)) t))
    (should (equal (norm '(nil)) nil))
    (should (equal (norm '(sym)) 'sym))
    (should-error (norm '(1)))
    (should-error (norm '("Hello")))))

(ert-deftest use-package-test/:ensure-1 ()
  (let ((use-package-always-ensure nil))
    (match-expansion
     (use-package foo :ensure t)
     `(progn
        (use-package-ensure-elpa 'foo 't 'nil)
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-2 ()
  (let ((use-package-always-ensure t))
    (match-expansion
     (use-package foo :ensure t)
     `(progn
        (use-package-ensure-elpa 'foo 't 'nil)
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-3 ()
  (let ((use-package-always-ensure nil))
    (match-expansion
     (use-package foo :ensure nil)
     `(progn
        (use-package-ensure-elpa 'foo 'nil 'nil)
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-4 ()
  (let ((use-package-always-ensure t))
    (match-expansion
     (use-package foo :ensure nil)
     `(progn
        (use-package-ensure-elpa 'foo 'nil 'nil)
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
        (use-package-ensure-elpa 'foo 'nil 'nil)
        (eval-and-compile
          (add-to-list 'load-path ,(pred stringp)))
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-8 ()
  (let ((use-package-always-ensure t))
    (match-expansion
     (use-package foo :ensure nil :load-path "foo")
     `(progn
        (use-package-ensure-elpa 'foo 'nil 'nil)
        (eval-and-compile
          (add-to-list 'load-path ,(pred stringp)))
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-9 ()
  (let ((use-package-always-ensure nil))
    (match-expansion
     (use-package foo :ensure t :load-path "foo")
     `(progn
        (use-package-ensure-elpa 'foo 't 'nil)
        (eval-and-compile
          (add-to-list 'load-path ,(pred stringp)))
        (require 'foo nil nil)))))

(ert-deftest use-package-test/:ensure-10 ()
  (let ((use-package-always-ensure t))
    (match-expansion
     (use-package foo :ensure t :load-path "foo")
     `(progn
        (use-package-ensure-elpa 'foo 't 'nil)
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
                (load "foo" nil t))))
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
                                 (load "foo" nil t))))
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
                                 (load "foo" nil t))))
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

(ert-deftest use-package-test-normalize/:bind ()
  (flet ((norm (&rest args)
               (apply #'use-package-normalize-binder
                      'foopkg :bind args)))
    (let ((good-values '(:map map-sym
                              ("str" . sym) ("str" . "str")
                              ([vec] . sym) ([vec] . "str"))))
      (should (equal (norm good-values) good-values)))
    (should-error (norm '("foo")))
    (should-error (norm '("foo" . 99)))
    (should-error (norm '(99 . sym)))))

(ert-deftest use-package-test/:bind-1 ()
  (match-expansion
   (use-package foo :bind ("C-k" . key))
   `(progn
      (unless (fboundp 'key)
        (autoload #'key "foo" nil t))
      (ignore
       (bind-keys :package foo ("C-k" . key))))))

(ert-deftest use-package-test/:bind*-1 ()
  (match-expansion
   (use-package foo :bind* ("C-k" . key))
   `(progn
      (unless (fboundp 'key)
        (autoload #'key "foo" nil t))
      (ignore
       (bind-keys* :package foo ("C-k" . key))))))

(ert-deftest use-package-test/:bind-keymap-1 ()
  (match-expansion
   (use-package foo :bind-keymap ("C-k" . key))
   `(ignore
     (bind-key "C-k"
               #'(lambda nil
                   (interactive)
                   (use-package-autoload-keymap 'key 'foo nil))))))

(ert-deftest use-package-test/:bind-keymap*-1 ()
  (match-expansion
   (use-package foo :bind-keymap* ("C-k" . key))
   `(ignore
     (bind-key* "C-k"
                #'(lambda ()
                    (interactive)
                    (use-package-autoload-keymap 'key 'foo t))))))

(ert-deftest use-package-test/:interpreter-1 ()
  (match-expansion
   (use-package foo :interpreter "interp")
   `(progn
      (unless (fboundp 'foo)
        (autoload #'foo "foo" nil t))
      (ignore
       (add-to-list 'interpreter-mode-alist '("interp" . foo))))))

(ert-deftest use-package-test/:interpreter-2 ()
  (match-expansion
   (use-package foo :interpreter ("interp" . fun))
   `(progn
      (unless (fboundp 'fun)
        (autoload #'fun "foo" nil t))
      (ignore
       (add-to-list 'interpreter-mode-alist '("interp" . fun))))))

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
      (ignore
       (add-to-list 'auto-mode-alist '("interp" . foo))))))

(ert-deftest use-package-test/:mode-2 ()
  (match-expansion
   (use-package foo :mode ("interp" . fun))
   `(progn
      (unless (fboundp 'fun)
        (autoload #'fun "foo" nil t))
      (ignore
       (add-to-list 'auto-mode-alist '("interp" . fun))))))

(ert-deftest use-package-test/:magic-1 ()
  (match-expansion
   (use-package foo :magic "interp")
   `(progn
      (unless (fboundp 'foo)
        (autoload #'foo "foo" nil t))
      (ignore
       (add-to-list 'magic-mode-alist '("interp" . foo))))))

(ert-deftest use-package-test/:magic-2 ()
  (match-expansion
   (use-package foo :magic ("interp" . fun))
   `(progn
      (unless (fboundp 'fun)
        (autoload #'fun "foo" nil t))
      (ignore
       (add-to-list 'magic-mode-alist '("interp" . fun))))))

(ert-deftest use-package-test/:magic-fallback-1 ()
  (match-expansion
   (use-package foo :magic-fallback "interp")
   `(progn
      (unless (fboundp 'foo)
        (autoload #'foo "foo" nil t))
      (ignore
       (add-to-list 'magic-fallback-mode-alist '("interp" . foo))))))

(ert-deftest use-package-test/:magic-fallback-2 ()
  (match-expansion
   (use-package foo :magic-fallback ("interp" . fun))
   `(progn
      (unless (fboundp 'fun)
        (autoload #'fun "foo" nil t))
      (ignore
       (add-to-list 'magic-fallback-mode-alist '("interp" . fun))))))

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
                                 (load "foo" nil t))))
        (unless (fboundp 'bar)
          (autoload #'bar "foo" nil t))
        (eval-when-compile
          (declare-function bar "foo"))
        (unless (fboundp 'quux)
          (autoload #'quux "foo" nil t))
        (eval-when-compile
          (declare-function quux "foo"))))))

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
                (load "foo" nil t))))
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
                (load "foo" nil t))))
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
                               (load "foo" nil t)))))))

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
                (load "foo" nil t))))
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
                                 (load "foo" nil t))))
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
                               (load "foo" nil t)))))))

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
    (should
     (equal
      (expand-minimally
       (use-package foo
         :bind (("C-a" . key))
         :hook (hook . fun)))
      '(progn
         (eval-and-compile
           (eval-when-compile
             (with-demoted-errors
                 "Cannot load foo: %S" nil
                 (load "foo" nil t))))
         (unless (fboundp 'fun)
           (autoload #'fun "foo" nil t))
         (eval-when-compile
           (declare-function fun "foo"))
         (unless (fboundp 'key)
           (autoload #'key "foo" nil t))
         (eval-when-compile
           (declare-function key "foo"))
         (ignore
          (add-hook 'hook-hook #'fun))
         (ignore
          (bind-keys :package foo ("C-a" . key))))))))

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
      (customize-set-variable 'foo bar "Customized with use-package foo")
      (require 'foo nil nil))))

(ert-deftest use-package-test/:custom-face-1 ()
  (match-expansion
   (use-package foo :custom-face (foo ((t (:background "#e4edfc")))))
   `(progn
      (custom-set-faces '(foo ((t (:background "#e4edfc")))))
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
                                 (load "foo" nil t))))
        (init)
        (require 'foo nil nil)))))

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
                                 (load "foo" nil t))))
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
   `(lexical-let ,_
      (lexical-let ,_
        (progn
          (eval-after-load 'bar
            `(funcall ,_))
          (eval-after-load 'quux
            `(funcall ,_)))))))

(ert-deftest use-package-test/:after-6 ()
  (match-expansion
   (use-package foo :after (:all (:any bar quux) bow))
   `(lexical-let ,_
      (lexical-let ,_
        (eval-after-load 'bow
          '(progn
             (eval-after-load 'bar
               `(funcall ,_))
             (eval-after-load 'quux
               `(funcall ,_))))))))

(ert-deftest use-package-test/:after-7 ()
  (match-expansion
   (use-package foo :after (:any (:all bar quux) bow))
   `(lexical-let ,_
      (lexical-let ,_
        (progn
          (eval-after-load 'quux
            '(eval-after-load 'bar
               `(funcall ,_)))
          (eval-after-load 'bow
            `(funcall ,_)))))))

(ert-deftest use-package-test/:after-8 ()
  (match-expansion
   (use-package foo :after (:all (:any bar quux) (:any bow baz)))
   `(lexical-let ,_
      (lexical-let ,_
        (progn
          (eval-after-load 'bow
            '(progn
               (eval-after-load 'bar
                 `(funcall ,_))
               (eval-after-load 'quux
                 `(funcall ,_))))
          (eval-after-load 'baz
            '(progn
               (eval-after-load 'bar
                 `(funcall ,_))
               (eval-after-load 'quux
                 `(funcall ,_)))))))))

(ert-deftest use-package-test/:after-9 ()
  (match-expansion
   (use-package foo :after (:any (:all bar quux) (:all bow baz)))
   `(lexical-let ,_
      (lexical-let ,_
        (progn
          (eval-after-load 'quux
            '(eval-after-load 'bar
               `(funcall ,_)))
          (eval-after-load 'baz
            '(eval-after-load 'bow
               `(funcall ,_))))))))

(ert-deftest use-package-test/:after-10 ()
  (match-expansion
   (use-package foo :after (:any (:all bar quux) (:any bow baz)))
   `(lexical-let ,_
      (lexical-let ,_
        (progn
          (eval-after-load 'quux
            '(eval-after-load 'bar
               `(funcall ,_)))
          (progn
            (eval-after-load 'bow
              `(funcall ,_))
            (eval-after-load 'baz
              `(funcall ,_))))))))

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
                                 (load "foo" nil t))))
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
                                 (load "foo" nil t))))
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
                                 (load "foo" nil t))))
        (eval-after-load 'bar
          '(require 'foo nil nil))))))

;; (ert-deftest use-package-test/:demand-7 ()
;;   (match-expansion
;;    (use-package counsel
;;      :load-path "site-lisp/swiper"
;;      :after ivy
;;      :demand t
;;      :diminish
;;      :bind (("C-*" . counsel-org-agenda-headlines)
;;             ("M-x" . counsel-M-x))
;;      :commands (counsel-minibuffer-history
;;                 counsel-find-library
;;                 counsel-unicode-char)
;;      :preface (preface-code)
;;      :init
;;      ;; This is actually wrong, but it's just part of the example.
;;      (define-key minibuffer-local-map (kbd "M-r")
;;        'counsel-minibuffer-history))
;;    `(progn
;;       (eval-and-compile
;;         (add-to-list 'load-path "/Users/johnw/.emacs.d/site-lisp/swiper"))
;;       (eval-and-compile
;;         (preface-code))
;;       (eval-after-load 'ivy
;;         '(progn
;;            (define-key minibuffer-local-map (kbd "M-r")
;;              'counsel-minibuffer-history)
;;            (require 'counsel nil nil)
;;            (if (fboundp 'diminish)
;;                (diminish 'counsel-mode))
;;            (ignore
;;             (bind-keys :package counsel
;;                        ("C-*" . counsel-org-agenda-headlines)
;;                        ("M-x" . counsel-M-x))))))))

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
                                 (load "foo" nil t))))
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
                                 (load "foo" nil t))))
        (eval-after-load 'foo
          '(progn
             (config)
             t))))))

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

;; Local Variables:
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;;; use-package-tests.el ends here
