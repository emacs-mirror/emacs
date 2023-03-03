;;; use-package-bind-key.el --- Support for the :bind/:bind-keymap keywords  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2023 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>

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

;; Provides support for the :bind, :bind*, :bind-keymap and :bind-keymap*
;; keywords.  Note that these are currently still baked into
;; `use-package-keywords' and `use-package-deferring-keywords', although this
;; is harmless if they are never used.
;;
;; These keywords are made available by default by requiring `use-package'.
;;
;; See the `use-package' info manual for more information.

;;; Code:

(require 'use-package-core)
(require 'bind-key)

;;;###autoload
(defun use-package-autoload-keymap (keymap-symbol package override)
  "Load PACKAGE and bind key sequence invoking this function to KEYMAP-SYMBOL.
Then simulate pressing the same key sequence a again, so that the
next key pressed is routed to the newly loaded keymap.

This function supports use-package's :bind-keymap keyword.  It
works by binding the given key sequence to an invocation of this
function for a particular keymap.  The keymap is expected to be
defined by the package.  In this way, loading the package is
deferred until the prefix key sequence is pressed."
  (if (not (require package nil t))
      (use-package-error (format "Cannot load package.el: %s" package))
    (if (and (boundp keymap-symbol)
             (keymapp (symbol-value keymap-symbol)))
        (let* ((kv (this-command-keys-vector))
               (key (key-description kv))
               (keymap (symbol-value keymap-symbol)))
          (if override
              (bind-key* key keymap)
            (bind-key key keymap))
          (setq unread-command-events
                (mapcar (lambda (ev) (cons t ev))
                        (listify-key-sequence kv))))
      (use-package-error
       (format "package.el %s failed to define keymap %s"
               package keymap-symbol)))))

;;;###autoload
(defun use-package-normalize-binder (name keyword args)
  (let ((arg args)
        args*)
    (while arg
      (let ((x (car arg)))
        (cond
         ;; (KEY . COMMAND)
         ((and (consp x)
               (or (stringp (car x))
                   (vectorp (car x)))
               (or (use-package-recognize-function (cdr x) t #'stringp)))
          (setq args* (nconc args* (list x)))
          (setq arg (cdr arg)))
         ;; KEYWORD
         ;;   :map KEYMAP
         ;;   :prefix-docstring STRING
         ;;   :prefix-map SYMBOL
         ;;   :prefix STRING
	 ;;   :repeat-docstring STRING
         ;;   :repeat-map SYMBOL
         ;;   :filter SEXP
         ;;   :menu-name STRING
         ;;   :package SYMBOL
	 ;;   :continue and :exit are used within :repeat-map
         ((or (and (eq x :map) (symbolp (cadr arg)))
              (and (eq x :prefix) (stringp (cadr arg)))
              (and (eq x :prefix-map) (symbolp (cadr arg)))
              (and (eq x :prefix-docstring) (stringp (cadr arg)))
	      (and (eq x :repeat-map) (symbolp (cadr arg)))
	      (eq x :continue)
	      (eq x :exit)
              (and (eq x :repeat-docstring) (stringp (cadr arg)))
              (eq x :filter)
              (and (eq x :menu-name) (stringp (cadr arg)))
              (and (eq x :package) (symbolp (cadr arg))))
          (setq args* (nconc args* (list x (cadr arg))))
          (setq arg (cddr arg)))
         ((listp x)
          (setq args*
                (nconc args* (use-package-normalize-binder name keyword x)))
          (setq arg (cdr arg)))
         (t
          ;; Error!
          (use-package-error
           (concat (symbol-name name)
                   " wants arguments acceptable to the `bind-keys' macro,"
                   " or a list of such values"))))))
    args*))

;;;; :bind, :bind*

;;;###autoload
(defalias 'use-package-normalize/:bind 'use-package-normalize-binder)
;;;###autoload
(defalias 'use-package-normalize/:bind* 'use-package-normalize-binder)

;; jww (2017-12-07): This is too simplistic. It will fail to determine
;; autoloads in this situation:
;;   (use-package foo
;;     :bind (:map foo-map (("C-a" . func))))
;;;###autoload
(defalias 'use-package-autoloads/:bind 'use-package-autoloads-mode)
;;;###autoload
(defalias 'use-package-autoloads/:bind* 'use-package-autoloads-mode)

;;;###autoload
(defun use-package-handler/:bind
    (name _keyword args rest state &optional bind-macro)
  (use-package-concat
   (use-package-process-keywords name rest state)
   `(,@(mapcar
        #'(lambda (xs)
            `(,(if bind-macro bind-macro 'bind-keys)
              :package ,name ,@(use-package-normalize-commands xs)))
        (use-package-split-list-at-keys :break args)))))

(defun use-package-handler/:bind* (name keyword arg rest state)
  (use-package-handler/:bind name keyword arg rest state 'bind-keys*))

;;;; :bind-keymap, :bind-keymap*

;;;###autoload
(defalias 'use-package-normalize/:bind-keymap 'use-package-normalize-binder)
;;;###autoload
(defalias 'use-package-normalize/:bind-keymap* 'use-package-normalize-binder)

;;;###autoload
(defun use-package-handler/:bind-keymap
    (name _keyword args rest state &optional override)
  (use-package-concat
   (use-package-process-keywords name rest state)
   (mapcar
    #'(lambda (binding)
        `(,(if override 'bind-key* 'bind-key)
          ,(car binding)
          #'(lambda ()
              (interactive)
              (use-package-autoload-keymap
               ',(cdr binding) ',(use-package-as-symbol name)
               ,override))))
    args)))

;;;###autoload
(defun use-package-handler/:bind-keymap* (name keyword arg rest state)
  (use-package-handler/:bind-keymap name keyword arg rest state t))

(provide 'use-package-bind-key)

;;; use-package-bind-key.el ends here
