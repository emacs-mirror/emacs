;;; use-package-bind-key.el --- Support for the :bind/:bind-keymap keywords

;; Copyright (C) 2012-2017 John Wiegley

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: 17 Jun 2012
;; Modified: 4 Dec 2017
;; Version: 1.0
;; Package-Requires: ((emacs "24.3") (use-package "2.4")  (bind-key "2.4"))
;; Keywords: dotemacs startup speed config package
;; URL: https://github.com/jwiegley/use-package

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

;; Provides support for the :bind, :bind*, :bind-keymap and :bind-keymap*
;; keywords. Note that these are currently still baked into
;; `use-package-keywords' and `use-package-deferring-keywords', although this
;; is harmless if they are never used.

;;; Code:

(require 'use-package-core)
(require 'bind-key)

;;;###autoload
(defun use-package-autoload-keymap (keymap-symbol package override)
  "Loads PACKAGE and then binds the key sequence used to invoke
this function to KEYMAP-SYMBOL. It then simulates pressing the
same key sequence a again, so that the next key pressed is routed
to the newly loaded keymap.

This function supports use-package's :bind-keymap keyword. It
works by binding the given key sequence to an invocation of this
function for a particular keymap. The keymap is expected to be
defined by the package. In this way, loading the package is
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
                (listify-key-sequence kv)))
      (use-package-error
       (format "package.el %s failed to define keymap %s"
               package keymap-symbol)))))

(defun use-package-normalize-binder (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    #'(lambda (label arg)
        (unless (consp arg)
          (use-package-error
           (concat label " a (<string or vector> . <symbol, string or function>)"
                   " or list of these")))
        (use-package-normalize-pairs
         #'(lambda (k)
             (pcase k
               ((pred stringp) t)
               ((pred vectorp) t)))
         #'(lambda (v) (use-package-recognize-function v t #'stringp))
         name label arg))))

;;;; :bind, :bind*

(defalias 'use-package-normalize/:bind 'use-package-normalize-binder)
(defalias 'use-package-normalize/:bind* 'use-package-normalize-binder)

(defun use-package-handler/:bind
    (name keyword args rest state &optional bind-macro)
  (cl-destructuring-bind (nargs . commands)
      (use-package-normalize-commands args)
    (use-package-concat
     (use-package-process-keywords name
       (use-package-sort-keywords
        (use-package-plist-append rest :commands commands))
       state)
     `((ignore
        (,(if bind-macro bind-macro 'bind-keys)
         :package ,name ,@nargs))))))

(defun use-package-handler/:bind* (name keyword arg rest state)
  (use-package-handler/:bind name keyword arg rest state 'bind-keys*))

;;;; :bind-keymap, :bind-keymap*

(defalias 'use-package-normalize/:bind-keymap 'use-package-normalize-binder)
(defalias 'use-package-normalize/:bind-keymap* 'use-package-normalize-binder)

(defun use-package-handler/:bind-keymap
    (name keyword arg rest state &optional override)
  (use-package-concat
   (use-package-process-keywords name rest state)
   `((ignore
      ,@(mapcar
         #'(lambda (binding)
             `(,(if override
                    'bind-key*
                  'bind-key)
               ,(car binding)
               #'(lambda ()
                   (interactive)
                   (use-package-autoload-keymap
                    ',(cdr binding) ',(use-package-as-symbol name)
                    ,override)))) arg)))))

(defun use-package-handler/:bind-keymap* (name keyword arg rest state)
  (use-package-handler/:bind-keymap name keyword arg rest state t))

(provide 'use-package-bind-key)

;;; use-package-bind-key.el ends here
