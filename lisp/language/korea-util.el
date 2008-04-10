;;; korea-util.el --- utilities for Korean

;; Copyright (C) 1997, 1999, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
;;   Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
;;   2007, 2008
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: mule, multilingual, Korean

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

;;;###autoload
(defvar default-korean-keyboard
  (if (string-match "3" (or (getenv "HANGUL_KEYBOARD_TYPE") ""))
      "3"
    "")
  "*The kind of Korean keyboard for Korean input method.
\"\" for 2, \"3\" for 3.")

;; functions useful for Korean text input

(defun toggle-korean-input-method ()
  "Turn on or off a Korean text input method for the current buffer."
  (interactive)
  (if current-input-method
      (inactivate-input-method)
    (activate-input-method
     (concat "korean-hangul" default-korean-keyboard))))

(defun quail-hangul-switch-symbol-ksc (&rest ignore)
  "Swith to/from Korean symbol package."
  (interactive "i")
  (and current-input-method
       (if (string-equal current-input-method "korean-symbol")
	   (activate-input-method (concat "korean-hangul"
					  default-korean-keyboard))
	 (activate-input-method "korean-symbol"))))

(defun quail-hangul-switch-hanja (&rest ignore)
  "Swith to/from Korean hanja package."
  (interactive "i")
  (and current-input-method
       (if (string-match "korean-hanja" current-input-method)
	   (activate-input-method (concat "korean-hangul"
					  default-korean-keyboard))
	 (activate-input-method (concat "korean-hanja"
					default-korean-keyboard)))))

;; The following three commands are set in isearch-mode-map.

(defun isearch-toggle-korean-input-method ()
  (interactive)
  (let ((overriding-terminal-local-map nil))
    (toggle-korean-input-method))
  (setq isearch-input-method-function input-method-function
	isearch-input-method-local-p t)
  (setq input-method-function nil)
  (isearch-update))

(defun isearch-hangul-switch-symbol-ksc ()
  (interactive)
  (let ((overriding-terminal-local-map nil))
    (quail-hangul-switch-symbol-ksc))
  (setq isearch-input-method-function input-method-function
	isearch-input-method-local-p t)
  (setq input-method-function nil)
  (isearch-update))

(defun isearch-hangul-switch-hanja ()
  (interactive)
  (let ((overriding-terminal-local-map nil))
    (quail-hangul-switch-hanja))
  (setq isearch-input-method-function input-method-function
	isearch-input-method-local-p t)
  (setq input-method-function nil)
  (isearch-update))

;; Information for setting and exiting Korean environment.
(defvar korean-key-bindings
  `((global [?\S- ] toggle-korean-input-method nil)
    (global [C-f9] quail-hangul-switch-symbol-ksc nil)
    (global [f9]  quail-hangul-switch-hanja nil)
    (,isearch-mode-map [?\S- ] isearch-toggle-korean-input-method nil)
    (,isearch-mode-map [C-f9] isearch-hangul-switch-symbol-ksc nil)
    (,isearch-mode-map [f9] isearch-hangul-switch-hanja nil)))

;;;###autoload
(defun setup-korean-environment-internal ()
  (let ((key-bindings korean-key-bindings))
    (while key-bindings
      (let* ((this (car key-bindings))
	     (key (nth 1 this))
	     (new-def (nth 2 this))
	     old-def)
	(if (eq (car this) 'global)
	    (progn
	      (setq old-def (global-key-binding key))
	      (global-set-key key new-def))
	  (setq old-def (lookup-key (car this) key))
	  (define-key (car this) key new-def))
	(setcar (nthcdr 3 this) old-def))
      (setq key-bindings (cdr key-bindings)))))

(defun exit-korean-environment ()
  "Exit Korean language environment."
  (let ((key-bindings korean-key-bindings))
    (while key-bindings
      (let* ((this (car key-bindings))
	     (key (nth 1 this))
	     (new-def (nth 2 this))
	     (old-def (nth 3 this)))
	(if (eq (car this) 'global)
	    (if (eq (global-key-binding key) new-def)
		(global-set-key key old-def))
	  (if (eq (lookup-key (car this) key) new-def)
	      (define-key (car this) key old-def))))
      (setq key-bindings (cdr key-bindings)))))

;;
(provide 'korea-util)

;; arch-tag: b17d0981-05da-4577-99f8-1db87fff8b44
;;; korea-util.el ends here
