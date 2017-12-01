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
      use-package-verbose nil
      use-package-expand-minimally t
      max-lisp-eval-depth 8000)

;; (let ((byte-compile-current-file nil)) (expand-minimally ()))
(fset 'insert-expansion
      [?\C-\M-  ?\M-w ?\M-: ?\M-p ?\C-e ?\C-b ?\C-b ?\C-\M-b ?\C-y ?\C-\M-k return ?\C-\M-  ?\M-w C-return ?\C-z ?\C-n ?\C-f ?\C-y ?\C-\M-k])

(defmacro expand-minimally (form)
  `(let ((use-package-verbose nil)
         (use-package-expand-minimally t))
     (macroexpand ',form)))

(defmacro match-expansion (form &rest value)
  `(should (pcase (expand-minimally ,form)
             ,@(mapcar #'(lambda (x) (list x t)) value))))

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
  (should (use-package--recognize-function nil t))
  (should-not (use-package--recognize-function nil))
  (should (use-package--recognize-function t))
  (should (use-package--recognize-function 'sym))
  (should (use-package--recognize-function #'sym))
  (should (use-package--recognize-function (lambda () ...)))
  (should (use-package--recognize-function '(lambda () ...)))
  (should (use-package--recognize-function #'(lambda () ...)))

  (should-not (use-package--recognize-function 1))
  (should-not (use-package--recognize-function "Hello"))
  (should-not (use-package--recognize-function '(nil . nil))))

(ert-deftest use-package-test-normalize-function ()
  (should (equal (use-package--normalize-function nil) nil))
  (should (equal (use-package--normalize-function t) t))
  (should (equal (use-package--normalize-function 'sym) 'sym))
  (should (equal (use-package--normalize-function #'sym) 'sym))
  (should (equal (use-package--normalize-function (lambda () ...)) (lambda () ...)))
  (should (equal (use-package--normalize-function '(lambda () ...)) (lambda () ...)))
  (should (equal (use-package--normalize-function #'(lambda () ...)) (lambda () ...)))

  (should (equal (use-package--normalize-function 1) 1))
  (should (equal (use-package--normalize-function "Hello") "Hello"))
  (should (equal (use-package--normalize-function '(nil . nil)) '(nil . nil))))

(ert-deftest use-package-test/:disabled ()
  (match-expansion
   (use-package foo :disabled t)
   `())

  (match-expansion
   ;; jww (2017-11-30): Should :disabled ignore its argument?
   (use-package foo :disabled nil)
   `()))

(ert-deftest use-package-test/:preface ()
  (match-expansion
   (use-package foo :preface (t))
   `(progn
      (eval-and-compile
        (t))
      (require 'foo nil 'nil)))

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
        (require 'foo nil 'nil))))

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
        (require 'foo nil 'nil)
        (config)
        t)))

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

(ert-deftest use-package-test/:pin ()
  (match-expansion
   (use-package foo :pin foo)
   `(progn
      (use-package-pin-package 'foo "foo")
      (require 'foo nil 'nil)))

  (match-expansion
   (use-package foo :pin "foo")
   `(progn
      (use-package-pin-package 'foo "foo")
      (require 'foo nil 'nil))))

(ert-deftest use-package-test/:defer-install ()
  (match-expansion
   (use-package foo :defer-install t)
   `(progn
      (require 'foo nil 'nil))))

(ert-deftest use-package-test-normalize/:ensure ()
  (flet ((norm (&rest args)
               (apply #'use-package-normalize/:ensure
                      'foopkg :ensure args)))
    (should (equal (norm '(t)) t))
    (should (equal (norm '(nil)) nil))
    (should (equal (norm '(sym)) 'sym))
    (should-error (norm '(1)))
    (should-error (norm '("Hello")))))

(ert-deftest use-package-test/:ensure ()
  (let ((use-package-always-ensure nil))
    (match-expansion
     (use-package foo :ensure t)
     `(progn
        (use-package-ensure-elpa 'foo 't 'nil :ensure)
        (require 'foo nil 'nil))))

  (let ((use-package-always-ensure t))
    (match-expansion
     (use-package foo :ensure t)
     `(progn
        (use-package-ensure-elpa 'foo 't 'nil :ensure)
        (require 'foo nil 'nil))))

  (let ((use-package-always-ensure nil))
    (match-expansion
     (use-package foo :ensure nil)
     `(progn
        (use-package-ensure-elpa 'foo 'nil 'nil :ensure)
        (require 'foo nil 'nil))))

  (let ((use-package-always-ensure t))
    (match-expansion
     (use-package foo :ensure nil)
     `(progn
        (use-package-ensure-elpa 'foo 'nil 'nil :ensure)
        (require 'foo nil 'nil))))

  (let ((use-package-always-ensure nil))
    (match-expansion
     (use-package foo :load-path "foo")
     `(progn
        (eval-and-compile
          (add-to-list 'load-path ,(pred stringp)))
        (require 'foo nil 'nil))))

  (let ((use-package-always-ensure t))
    (match-expansion
     (use-package foo :load-path "foo")
     `(progn
        (use-package-ensure-elpa 'foo 'nil 'nil :ensure)
        (eval-and-compile
          (add-to-list 'load-path ,(pred stringp)))
        (require 'foo nil 'nil))))

  (let ((use-package-always-ensure nil))
    (match-expansion
     (use-package foo :ensure nil :load-path "foo")
     `(progn
        (use-package-ensure-elpa 'foo 'nil 'nil :ensure)
        (eval-and-compile
          (add-to-list 'load-path ,(pred stringp)))
        (require 'foo nil 'nil))))

  (let ((use-package-always-ensure t))
    (match-expansion
     (use-package foo :ensure nil :load-path "foo")
     `(progn
        (use-package-ensure-elpa 'foo 'nil 'nil :ensure)
        (eval-and-compile
          (add-to-list 'load-path ,(pred stringp)))
        (require 'foo nil 'nil))))

  (let ((use-package-always-ensure nil))
    (match-expansion
     (use-package foo :ensure t :load-path "foo")
     `(progn
        (use-package-ensure-elpa 'foo 't 'nil :ensure)
        (eval-and-compile
          (add-to-list 'load-path ,(pred stringp)))
        (require 'foo nil 'nil))))

  (let ((use-package-always-ensure t))
    (match-expansion
     (use-package foo :ensure t :load-path "foo")
     `(progn
        (use-package-ensure-elpa 'foo 't 'nil :ensure)
        (eval-and-compile
          (add-to-list 'load-path ,(pred stringp)))
        (require 'foo nil 'nil))))

  (let (tried-to-install)
    (flet ((use-package-ensure-elpa
            (name ensure state context &optional no-refresh)
            (when ensure
              (setq tried-to-install name)))
           (require (&rest ignore)))
      (use-package foo :ensure t)
      (should (eq tried-to-install 'foo)))))

(ert-deftest use-package-test/:if ()
  (match-expansion
   (use-package foo :if t)
   `(progn
      (when (symbol-value 't)
        (require 'foo nil 'nil))))

  (match-expansion
   (use-package foo :if (and t t))
   `(progn
      (when (and t t)
        (require 'foo nil 'nil))))

  (match-expansion
   (use-package foo :if nil)
   `(progn
      (when nil
        (require 'foo nil 'nil)))))

(ert-deftest use-package-test/:when ()
  (match-expansion
   (use-package foo :when t)
   `(progn
      (when (symbol-value 't)
        (require 'foo nil 'nil))))

  (match-expansion
   (use-package foo :when (and t t))
   `(progn
      (when (and t t)
        (require 'foo nil 'nil))))

  (match-expansion
   (use-package foo :when nil)
   `(progn
      (when nil
        (require 'foo nil 'nil)))))

(ert-deftest use-package-test/:unless ()
  (match-expansion
   (use-package foo :unless t)
   `(progn
      (unless (symbol-value 't)
        (require 'foo nil 'nil))))

  (match-expansion
   (use-package foo :unless (and t t))
   `(progn
      (unless (and t t)
        (require 'foo nil 'nil))))

  (match-expansion
   (use-package foo :unless nil)
   `(progn
      (unless nil
        (require 'foo nil 'nil)))))

(ert-deftest use-package-test/:requires ()
  (match-expansion
   (use-package foo :requires bar)
   `(progn
      (when (not (member nil (mapcar #'featurep '(bar))))
        (require 'foo nil 'nil))))

  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :requires bar)
     `(progn
        (when (not (member nil (mapcar #'featurep '(bar))))
          (eval-and-compile
            (eval-when-compile
              (with-demoted-errors "Cannot load foo: %S" nil
                                   (load "foo" nil t))))
          (require 'foo nil 'nil))))))

(ert-deftest use-package-test/:load-path ()
  (match-expansion
   (use-package foo :load-path "bar")
   `(progn
      (eval-and-compile
        (add-to-list 'load-path
                     ,(pred (apply-partially
                             #'string=
                             (expand-file-name
                              "bar" user-emacs-directory)))))
      (require 'foo nil 'nil)))

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
        (require 'foo nil 'nil))))

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
      (require 'foo nil 'nil)))

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
      (require 'foo nil 'nil))))

(ert-deftest use-package-test/:no-require ()
  (match-expansion
   (use-package foo :no-require t)
   `(progn))

  (match-expansion
   (use-package foo :no-require t :config (config))
   `(progn
      (config)
      t))

  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :no-require t)
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil nil)))))))

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

(ert-deftest use-package-test/:bind ()
  (match-expansion
   (use-package foo :bind ("C-k" . key))
   `(progn
      (unless (fboundp 'key)
        (autoload #'key "foo" nil t))
      (ignore
       (bind-keys :package foo ("C-k" . key))))))

(ert-deftest use-package-test/:bind* ()
  (match-expansion
   (use-package foo :bind* ("C-k" . key))
   `(progn
      (unless (fboundp 'key)
        (autoload #'key "foo" nil t))
      (ignore
       (bind-keys* :package foo ("C-k" . key))))))

(ert-deftest use-package-test/:bind-keymap ()
  (match-expansion
   (use-package foo :bind-keymap ("C-k" . key))
   `(progn
      (ignore
       (bind-key "C-k"
                 #'(lambda ()
                     (interactive)
                     (use-package-autoload-keymap 'key 'foo nil)))))))

(ert-deftest use-package-test/:bind-keymap* ()
  (match-expansion
   (use-package foo :bind-keymap* ("C-k" . key))
   `(progn
      (ignore
       (bind-key* "C-k"
                  #'(lambda ()
                      (interactive)
                      (use-package-autoload-keymap 'key 'foo t)))))))

(ert-deftest use-package-test/:interpreter ()
  (match-expansion
   (use-package foo :interpreter "interp")
   `(progn
      (unless (fboundp 'foo)
        (autoload #'foo "foo" nil t))
      (ignore
       (add-to-list 'interpreter-mode-alist
                    '("interp" . foo)))))

  (match-expansion
   (use-package foo :interpreter ("interp" . fun))
   `(progn
      (unless (fboundp 'fun)
        (autoload #'fun "foo" nil t))
      (ignore
       (add-to-list 'interpreter-mode-alist
                    '("interp" . fun))))))

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

(ert-deftest use-package-test/:mode ()
  (match-expansion
   (use-package foo :mode "interp")
   `(progn
      (unless (fboundp 'foo)
        (autoload #'foo "foo" nil t))
      (ignore
       (add-to-list 'auto-mode-alist
                    '("interp" . foo)))))

  (match-expansion
   (use-package foo :mode ("interp" . fun))
   `(progn
      (unless (fboundp 'fun)
        (autoload #'fun "foo" nil t))
      (ignore
       (add-to-list 'auto-mode-alist
                    '("interp" . fun))))))

(ert-deftest use-package-test/:magic ()
  (match-expansion
   (use-package foo :magic "interp")
   `(progn
      (unless (fboundp 'foo)
        (autoload #'foo "foo" nil t))
      (ignore
       (add-to-list 'magic-mode-alist
                    '("interp" . foo)))))

  (match-expansion
   (use-package foo :magic ("interp" . fun))
   `(progn
      (unless (fboundp 'fun)
        (autoload #'fun "foo" nil t))
      (ignore
       (add-to-list 'magic-mode-alist
                    '("interp" . fun))))))

(ert-deftest use-package-test/:magic-fallback ()
  (match-expansion
   (use-package foo :magic-fallback "interp")
   `(progn
      (unless (fboundp 'foo)
        (autoload #'foo "foo" nil t))
      (ignore
       (add-to-list 'magic-fallback-mode-alist
                    '("interp" . foo)))))

  (match-expansion
   (use-package foo :magic-fallback ("interp" . fun))
   `(progn
      (unless (fboundp 'fun)
        (autoload #'fun "foo" nil t))
      (ignore
       (add-to-list 'magic-fallback-mode-alist
                    '("interp" . fun))))))

(ert-deftest use-package-test/:commands ()
  (match-expansion
   (use-package foo :commands bar)
   `(progn
      (unless (fboundp 'bar)
        (autoload #'bar "foo" nil t))))

  (match-expansion
   (use-package foo :commands (bar quux))
   `(progn
      (unless (fboundp 'bar)
        (autoload #'bar "foo" nil t))
      (unless (fboundp 'quux)
        (autoload #'quux "foo" nil t))))

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

(ert-deftest use-package-test/:defines ()
  (match-expansion
   (use-package foo :defines bar)
   `(progn
      (require 'foo nil 'nil)))

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
        (require 'foo nil 'nil)))))

(ert-deftest use-package-test/:functions ()
  (match-expansion
   (use-package foo :functions bar)
   `(progn
      (require 'foo nil 'nil)))

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
        (require 'foo nil 'nil))))

  (match-expansion
   (use-package foo :defer t :functions bar)
   `(progn))

  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :defer t :functions bar)
     `(progn
        (eval-and-compile
          (declare-function bar "foo")
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (load "foo" nil t)))))))

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

(ert-deftest use-package-test/:defer ()
  (match-expansion
   (use-package foo)
   `(progn
      (require 'foo nil 'nil)))

  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo)
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (load "foo" nil t))))
        (require 'foo nil 'nil))))

  (match-expansion
   (use-package foo :defer t)
   `(progn))

  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :defer t)
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
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

(ert-deftest use-package-test/:hook ()
  (let ((byte-compile-current-file t))
    (should
     (equal                             ; pcase crashes
      (expand-minimally
       (use-package foo
         :bind (("C-a" . key))
         :hook (hook . fun)))
      '(progn
         (eval-and-compile
           (eval-when-compile
             (with-demoted-errors "Cannot load foo: %S" nil
                                  (load "foo" nil t))))
         (unless (fboundp 'fun)
           (autoload #'fun "foo" nil t))
         (eval-when-compile
           (declare-function fun "foo"))
         (unless (fboundp 'key)
           (autoload #'key "foo" nil t))
         (eval-when-compile
           (declare-function key "foo"))
         (add-hook 'hook-hook #'fun)
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

(ert-deftest use-package-test/:custom ()
  (match-expansion
   (use-package foo :custom (foo bar))
   `(progn
      (customize-set-variable 'foo bar "Customized with use-package foo")
      (require 'foo nil 'nil))))

(ert-deftest use-package-test/:custom-face ()
  (match-expansion
   (use-package foo :custom-face (foo ((t (:background "#e4edfc")))))
   `(progn
      (custom-set-faces '(foo ((t (:background "#e4edfc")))))
      (require 'foo nil 'nil))))

(ert-deftest use-package-test/:init ()
  (match-expansion
   (use-package foo :init (init))
   `(progn
      (init)
      (require 'foo nil 'nil)))

  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :init (init))
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (load "foo" nil t))))
        (init)
        (require 'foo nil 'nil)))))

(ert-deftest use-package-test/:after ()
  (match-expansion
   (use-package foo :after bar)
   `(progn
      (eval-after-load 'bar
        '(require 'foo nil t))))

  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :after bar)
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (load "foo" nil t))))
        (eval-after-load 'bar
          '(require 'foo nil t)))))

  (match-expansion
   (use-package foo :after (bar quux))
   `(progn
      (eval-after-load 'quux
        '(eval-after-load 'bar
           '(require 'foo nil t)))))

  (match-expansion
   (use-package foo :after (:all bar quux))
   `(progn
      (eval-after-load 'quux
        '(eval-after-load 'bar
           '(require 'foo nil t)))))

  (match-expansion
   (use-package foo :after (:any bar quux))
   `(progn
      (progn
        (eval-after-load 'bar
          '(require 'foo nil t))
        (eval-after-load 'quux
          '(require 'foo nil t)))))

  (match-expansion
   (use-package foo :after (:all (:any bar quux) bow))
   `(progn
      (eval-after-load 'bow
        '(progn
           (eval-after-load 'bar
             '(require 'foo nil t))
           (eval-after-load 'quux
             '(require 'foo nil t))))))

  (match-expansion
   (use-package foo :after (:any (:all bar quux) bow))
   `(progn
      (progn
        (eval-after-load 'quux
          '(eval-after-load 'bar
             '(require 'foo nil t)))
        (eval-after-load 'bow
          '(require 'foo nil t)))))

  (match-expansion
   (use-package foo :after (:all (:any bar quux) (:any bow baz)))
   `(progn
      (progn
        (eval-after-load 'bow
          '(progn
             (eval-after-load 'bar
               '(require 'foo nil t))
             (eval-after-load 'quux
               '(require 'foo nil t))))
        (eval-after-load 'baz
          '(progn
             (eval-after-load 'bar
               '(require 'foo nil t))
             (eval-after-load 'quux
               '(require 'foo nil t)))))))

  (match-expansion
   (use-package foo :after (:any (:all bar quux) (:all bow baz)))
   `(progn
      (progn
        (eval-after-load 'quux
          '(eval-after-load 'bar
             '(require 'foo nil t)))
        (eval-after-load 'baz
          '(eval-after-load 'bow
             '(require 'foo nil t))))))

  (match-expansion
   (use-package foo :after (:any (:all bar quux) (:any bow baz)))
   `(progn
      (progn
        (eval-after-load 'quux
          '(eval-after-load 'bar
             '(require 'foo nil t)))
        (progn
          (eval-after-load 'bow
            '(require 'foo nil t))
          (eval-after-load 'baz
            '(require 'foo nil t)))))))

(ert-deftest use-package-test/:demand ()
  (match-expansion
   (use-package foo :demand t)
   `(progn
      (require 'foo nil 'nil)))

  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :demand t)
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (load "foo" nil t))))
        (require 'foo nil 'nil))))

  (match-expansion
   (use-package foo :demand t :config (config))
   `(progn
      (require 'foo nil 'nil)
      (config)
      t))

  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :demand t :config (config))
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (load "foo" nil t))))
        (require 'foo nil 'nil)
        (config)
        t)))

  ;; #529 - :demand should not override an explicit use of :after
  (match-expansion
   (use-package foo :demand t :after bar)
   `(progn
      (eval-after-load 'bar
        '(require 'foo nil t))))

  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :demand t :after bar)
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (load "foo" nil t))))
        (eval-after-load 'bar
          '(require 'foo nil t))))))

(ert-deftest use-package-test/:config ()
  (match-expansion
   (use-package foo :config (config))
   `(progn
      (require 'foo nil 'nil)
      (config)
      t))

  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :config (config))
     `(progn
        (eval-and-compile
          (eval-when-compile
            (with-demoted-errors "Cannot load foo: %S" nil
                                 (load "foo" nil t))))
        (require 'foo nil 'nil)
        (config)
        t)))

  (match-expansion
   (use-package foo :defer t :config (config))
   `(progn
      (eval-after-load 'foo
        '(progn
           (config)
           t))))

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

(ert-deftest use-package-test/:diminish ()
  (match-expansion
   (use-package foo :diminish nil)
   `(progn
      (require 'foo nil 'nil)
      (if (fboundp 'diminish)
          (diminish 'foo-mode))))

  (match-expansion
   (use-package foo :diminish bar)
   `(progn
      (require 'foo nil 'nil)
      (if (fboundp 'diminish)
          (diminish 'bar))))

  (match-expansion
   (use-package foo :diminish "bar")
   `(progn
      (require 'foo nil 'nil)
      (if (fboundp 'diminish)
          (diminish 'foo-mode "bar"))))


  (match-expansion
   (use-package foo :diminish (foo . "bar"))
   `(progn
      (require 'foo nil 'nil)
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

(ert-deftest use-package-test/:delight ()
  (match-expansion
   (use-package foo :delight)
   `(progn
      (require 'foo nil 'nil)
      (if (fboundp 'delight)
          (delight '((foo-mode nil foo))))))

  (should-error
   (match-expansion
    (use-package foo :delight nil)
    `(progn
       (require 'foo nil 'nil)
       (if (fboundp 'diminish)
           (diminish 'foo-mode)))))

  (match-expansion
   (use-package foo :delight bar)
   `(progn
      (require 'foo nil 'nil)
      (if (fboundp 'delight)
          (delight '((bar nil foo))))))

  (match-expansion
   (use-package foo :delight "bar")
   `(progn
      (require 'foo nil 'nil)
      (if (fboundp 'delight)
          (delight '((foo-mode "bar" foo))))))

  (should-error
   (match-expansion
    (use-package foo :delight (foo . "bar"))
    `(progn
       (require 'foo nil 'nil)
       (if (fboundp 'diminish)
           (diminish 'foo "bar")))))

  (match-expansion
   (use-package foo :delight (foo "bar"))
   `(progn
      (require 'foo nil 'nil)
      (if (fboundp 'delight)
          (delight '((foo "bar" foo)))))))

;; Local Variables:
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;;; use-package-tests.el ends here
