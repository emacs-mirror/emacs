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
      use-package-expand-minimally t)

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

;; (ert-deftest use-package-test/:pin ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

;; (ert-deftest use-package-test/:defer-install ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

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
   `(if (symbol-value 't)
        (progn
          (require 'foo nil 'nil))))

  (match-expansion
   (use-package foo :if (and t t))
   `(if (and t t)
        (progn
          (require 'foo nil 'nil))))

  (match-expansion
   (use-package foo :if nil)
   `(if nil
        (progn
          (require 'foo nil 'nil)))))

(ert-deftest use-package-test/:when ()
  (match-expansion
   (use-package foo :when t)
   `(if (symbol-value 't)
        (progn
          (require 'foo nil 'nil))))

  (match-expansion
   (use-package foo :when (and t t))
   `(if (and t t)
        (progn
          (require 'foo nil 'nil))))

  (match-expansion
   (use-package foo :when nil)
   `(if nil
        (progn
          (require 'foo nil 'nil)))))

(ert-deftest use-package-test/:when ()
  (match-expansion
   (use-package foo :unless t)
   `(if (symbol-value 't)
        nil
      (require 'foo nil 'nil)))

  (match-expansion
   (use-package foo :unless (and t t))
   `(if (and t t)
        nil
      (require 'foo nil 'nil)))

  (match-expansion
   (use-package foo :unless nil)
   `(if nil
        nil
      (require 'foo nil 'nil))))

;; (ert-deftest use-package-test/:requires ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

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
   `nil)

  (let ((byte-compile-current-file t))
    (match-expansion
     (use-package foo :no-require t)
     `'nil)))

(ert-deftest use-package-test-normalize/:bind ()
  (let ((good-values '(:map map-sym
                            ("str" . sym) ("str" . "str")
                            ([vec] . sym) ([vec] . "str"))))
    (should (equal (use-package-normalize-binder
                    'foopkg :bind good-values)
                   good-values)))
  (should-error (use-package-normalize-binder
                 'foopkg :bind '("foo")))
  (should-error (use-package-normalize-binder
                 'foopkg :bind '("foo" . 99)))
  (should-error (use-package-normalize-binder
                 'foopkg :bind '(99 . sym))))

;; (ert-deftest use-package-test/:bind ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

;; (ert-deftest use-package-test/:bind* ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

;; (ert-deftest use-package-test/:bind-keymap ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

;; (ert-deftest use-package-test/:bind-keymap* ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

;; (ert-deftest use-package-test/:interpreter ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

(ert-deftest use-package-test-normalize/:mode ()
  (should (equal (use-package-normalize-mode 'foopkg :mode '(".foo"))
                 '((".foo" . foopkg))))
  (should (equal (use-package-normalize-mode 'foopkg :mode '(".foo" ".bar"))
                 '((".foo" . foopkg) (".bar" . foopkg))))
  (should (equal (use-package-normalize-mode 'foopkg :mode '((".foo" ".bar")))
                 '((".foo" . foopkg) (".bar" . foopkg))))
  (should (equal (use-package-normalize-mode 'foopkg :mode '((".foo")))
                 '((".foo" . foopkg))))
  (should (equal (use-package-normalize-mode 'foopkg :mode '((".foo" . foo) (".bar" . bar)))
                 '((".foo" . foo) (".bar" . bar)))))

;; (ert-deftest use-package-test/:mode ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

;; (ert-deftest use-package-test/:magic ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

;; (ert-deftest use-package-test/:magic-fallback ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

;; (ert-deftest use-package-test/:commands ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

(ert-deftest use-package-test/:defines ()
  (match-expansion
   (use-package foo :defines bar)
   `(require 'foo nil 'nil))

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
   `(require 'foo nil 'nil))

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
   `nil)

  ;; jww (2017-12-01): This exposes a bug.
  ;; (let ((byte-compile-current-file t))
  ;;   (match-expansion
  ;;    (use-package foo :defer t :functions bar)
  ;;    `'nil))

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

;; (ert-deftest use-package-test/:defer ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

(ert-deftest use-package-test-normalize/:hook ()
  (should-error (use-package-normalize/:hook 'foopkg :hook nil))
  (should (equal (use-package-normalize/:hook 'foopkg :hook '(bar))
                 '((bar . foopkg))))
  (should (equal (use-package-normalize/:hook 'foopkg :hook '((bar . baz)))
                 '((bar . baz))))
  (should (equal (use-package-normalize/:hook 'foopkg :hook '(((bar baz) . quux)))
                 '(((bar baz) . quux))))
  (should (equal (use-package-normalize/:hook 'foopkg :hook '(bar baz))
                 '(((bar baz) . foopkg))))
  (should (equal (use-package-normalize/:hook 'foopkg :hook '((bar baz) (quux bow)))
                 '(((bar baz) . foopkg) ((quux bow) . foopkg))))
  (should (equal (use-package-normalize/:hook 'foopkg :hook '((bar . baz) (quux . bow)))
                 '((bar . baz) (quux . bow))))
  (should (equal (use-package-normalize/:hook 'foopkg :hook '(((bar1 bar2) . baz)
                                                              ((quux1 quux2) . bow)))
                 '(((bar1 bar2) . baz)
                   ((quux1 quux2) . bow)))))

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
  (should-error (use-package-normalize/:custom 'foopkg :custom nil))
  (should-error (use-package-normalize/:custom 'foopkg :custom '(bar)))
  ;; (should-error (use-package-normalize/:custom 'foopkg :custom '((foo bar baz quux))))
  (should (equal (use-package-normalize/:custom 'foopkg :custom '(foo bar))
                 '((foo bar))))
  ;; (should-error (use-package-normalize/:custom 'foopkg :custom '(foo bar baz)))
  ;; (should (equal (use-package-normalize/:custom 'foopkg :custom '(foo bar "baz"))
  ;;                '((foo bar baz))))
  )

;; (ert-deftest use-package-test/:custom ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

;; (ert-deftest use-package-test/:custom-face ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

;; (ert-deftest use-package-test/:init ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

(ert-deftest use-package-test/:after ()
  (match-expansion
   (use-package foo :after bar)
   `(eval-after-load 'bar
      '(require 'foo nil t))))

;; (ert-deftest use-package-test/:demand ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

;; (ert-deftest use-package-test/:config ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

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

;; (ert-deftest use-package-test/:diminish ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

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
  (should (equal `((a-mode nil foo)
                   (b-mode " b" foo))
                 (use-package-normalize/:delight 'foo :delight '((a-mode)
                                                                 (b-mode " b")))))
  (should-error (use-package-normalize/:delight 'foo :delight '((:eval 1)))))

;; (ert-deftest use-package-test/:delight ()
;;   (should (equal (macroexpand (use-package))
;;                  '())))

;; Local Variables:
;; indent-tabs-mode: nil
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; use-package-tests.el ends here
