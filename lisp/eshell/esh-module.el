;;; esh-module.el --- Eshell modules  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2000, 2002-2026 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Keywords: processes

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

;;; Code:

(require 'esh-util)

(defgroup eshell-module nil
  "The `eshell-module' group is for Eshell extension modules, which
provide optional behavior which the user can enable or disable by
customizing the variable `eshell-modules-list'."
  :tag "Extension modules"
  :group 'eshell)

;; load the defgroup's for the standard extension modules, so that
;; documentation can be provided when the user customize's
;; `eshell-modules-list'.  We use "(progn (defgroup ..." in each file
;; to force the autoloader into including the entire defgroup, rather
;; than an abbreviated version.
(load "esh-module-loaddefs" nil 'nomessage)

;;; User Variables:

(defcustom eshell-module-unload-hook
  '(eshell-unload-extension-modules)
  "A hook run when `eshell-module' is unloaded."
  :type 'hook
  :group 'eshell-module)
(make-obsolete-variable 'eshell-module-unload-hook nil "30.1")

(defcustom eshell-module-loading-messages t
  "If non-nil, display messages when loading/unloading Eshell modules."
  :type 'boolean
  :group 'eshell-module
  :version "30.1")

(defcustom eshell-modules-list
  '(eshell-alias
    eshell-banner
    eshell-basic
    eshell-cmpl
    eshell-dirs
    eshell-extpipe
    eshell-glob
    eshell-hist
    eshell-ls
    eshell-pred
    eshell-prompt
    eshell-script
    eshell-term
    eshell-unix)
  "A list of optional add-on modules to be loaded by Eshell.
Changes will only take effect in future Eshell buffers."
  :type (append
	 (list 'set ':tag "Supported modules")
	 (mapcar
          (lambda (modname)
            (let ((modsym (intern modname)))
              (list 'const
                    ':tag (format "%s -- %s" modname
                                  (get modsym 'custom-tag))
                    ':link (caar (get modsym 'custom-links))
                    ':doc (concat "\n" (get modsym 'group-documentation)
                                  "\n ")
                    modsym)))
	  (sort (mapcar 'symbol-name
			(eshell-subgroups 'eshell-module))
		'string-lessp))
	 '((repeat :inline t :tag "Other modules" symbol)))
  :group 'eshell-module)

;;; Code:

(defsubst eshell-module--feature-name (module &optional kind)
  "Get the feature name for the specified Eshell MODULE.
KIND can be either `core' for a core module or `extension' for an
extension module; if nil, KIND defaults to `extension'."
  (let ((module-name (symbol-name module))
        (prefix (cond ((eq kind 'core) "esh-")
                      ((memq kind '(extension nil)) "em-")
                      (t (error "Unknown module kind %s" kind)))))
    (if (string-match "^eshell-\\(.*\\)" module-name)
	(concat prefix (match-string 1 module-name))
      (error "Invalid Eshell module name: %s" module))))

(defsubst eshell-using-module (module)
  "Return non-nil if a certain Eshell MODULE is in use.
The MODULE should be a symbol corresponding to that module's
customization group.  Example: `eshell-cmpl' for that module."
  (memq module eshell-modules-list))

(defun eshell-load-modules (modules)
  "Load Eshell MODULES into memory.
This will cause any global variables they define to be visible so
that other modules can take advantage of their functionality if
desired."
  (let ((verbose eshell-module-loading-messages))
    (dolist (module modules)
      (let ((module-feature-name (eshell-module--feature-name module)))
        (unless (featurep (intern module-feature-name))
          (when verbose (message "Loading %s..." module))
          (condition-case-unless-debug nil
              (progn
                (load module-feature-name nil t)
                (when verbose (message "Loading %s...done" module)))
            (error (when verbose (message "Loading %s...failed" module))
                   (lwarn 'eshell :error
                          "Unable to load Eshell module `%s'"
                          module))))))))

(defun eshell-initialize-modules (modules)
  "Initialize Eshell MODULES.
This calls `MODULE-load-hook' and `MODULE-initialize' for each
MODULE, if they're defined."
  (dolist (module modules)
    (let ((load-hook (intern-soft (format "%s-load-hook" module)))
          (initfunc (intern-soft (format "%s-initialize" module))))
      (when (and load-hook (boundp load-hook))
        (if (memq initfunc (symbol-value load-hook)) (setq initfunc nil))
        (run-hooks load-hook))
      ;; So we don't need the -initialize functions on the hooks (bug#5375).
      (and initfunc (fboundp initfunc) (funcall initfunc)))))

(defun eshell-unload-modules (modules &optional kind)
  "Try to unload the specified Eshell MODULES.
KIND can be either `core' for core modules or `extension' for
extension modules; if nil, KIND defaults to `extension'."
  ;; We're about to unload this module, but we need to remember whether
  ;; to print messages.
  (let ((verbose eshell-module-loading-messages))
    (dolist (module modules)
      (let ((module-feature (intern (eshell-module--feature-name module kind))))
        (when (featurep module-feature)
          (when verbose (message "Unloading %s..." module))
          (condition-case-unless-debug nil
              (progn
                (unload-feature module-feature)
                (when verbose (message "Unloading %s...done" module)))
            (error (when verbose (message "Unloading %s...failed" module))
                   (lwarn 'eshell :error
                          "Unable to unload Eshell module `%s'"
                          module))))))))

(defun eshell-unload-extension-modules ()
  "Try to unload all currently-loaded Eshell extension modules."
  (eshell-unload-modules (eshell-subgroups 'eshell-module)))

(provide 'esh-module)
;;; esh-module.el ends here
