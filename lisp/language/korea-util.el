;;; korea-util.el --- utilities for Korean

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Keywords: mule, multilingual, Korean

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

;;;###autoload
(defvar default-korean-keyboard ""
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

;; Information for exiting Korean environment.
(defvar exit-korean-environment-data nil)

;;;###autoload
(defun setup-korean-environment ()
  "Setup multilingual environment (MULE) for Korean."
  (interactive)
  (setup-english-environment)
  (setq coding-category-iso-8-2 'korean-iso-8bit)

  (set-coding-priority
   '(coding-category-iso-7
     coding-category-iso-8-2
     coding-category-iso-8-1))

  (set-default-coding-systems 'korean-iso-8bit)

  (setq default-input-method "korean-hangul")

  (let ((key-bindings '(([?\S- ] . toggle-korean-input-method)
			([C-f9] . quail-hangul-switch-symbol-ksc)
			([f9] . quail-hangul-switch-hanja))))
    (while key-bindings
      (let ((prev-binding (global-key-binding (car (car key-bindings)))))
	(setq exit-korean-environment-data
	      (cons (cons (car (car key-bindings)) prev-binding)
		    exit-korean-environment-data)))
      (global-set-key (car (car key-bindings)) (cdr (car key-bindings)))
      (setq key-bindings (cdr key-bindings)))))

(defun exit-korean-environment ()
  "Exit Korean language environment."
  (while exit-korean-environment-data
    (global-set-key (car (car exit-korean-environment-data))
		    (cdr (car exit-korean-environment-data)))
    (setq exit-korean-environment-data
	  (cdr exit-korean-environment-data))))

;;
(provide 'korea-util)

;;; korean-util.el ends here
