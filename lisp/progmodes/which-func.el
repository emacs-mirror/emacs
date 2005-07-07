;;; which-func.el --- print current function in mode line

;; Copyright (C) 1994, 1997, 1998, 2001, 2003, 2005
;;           Free Software Foundation, Inc.

;; Author:   Alex Rezinsky <alexr@msil.sps.mot.com>
;;           (doesn't seem to be responsive any more)
;; Keywords: mode-line, imenu, tools

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package prints name of function where your current point is
;; located in mode line. It assumes that you work with imenu package
;; and imenu--index-alist is up to date.

;; KNOWN BUGS
;; ----------
;; Really this package shows not "function where the current point is
;; located now", but "nearest function which defined above the current
;; point". So if your current point is located after end of function
;; FOO but before begin of function BAR, FOO will be displayed in mode
;; line.
;; - if two windows display the same buffer, both windows
;;   show the same `which-func' information.

;; TODO LIST
;; ---------
;;     1. Dependence on imenu package should be removed.  Separate
;; function determination mechanism should be used to determine the end
;; of a function as well as the beginning of a function.
;;     2. This package should be realized with the help of overlay
;; properties instead of imenu--index-alist variable.

;;; History:

;; THANKS TO
;; ---------
;; Per Abrahamsen   <abraham@iesd.auc.dk>
;;     Some ideas (inserting  in mode-line,  using of post-command  hook
;;     and toggling this  mode) have  been   borrowed from  his  package
;;     column.el
;; Peter Eisenhauer <pipe@fzi.de>
;;     Bug fixing in case nested indexes.
;; Terry Tateyama   <ttt@ursa0.cs.utah.edu>
;;     Suggestion to use find-file-hook for first imenu
;;     index building.

;;; Code:

;; Variables for customization
;; ---------------------------
;;
(defvar which-func-unknown "???"
  "String to display in the mode line when current function is unknown.")

(defgroup which-func nil
  "Mode to display the current function name in the modeline."
  :group 'tools
  :version "20.3")

(defcustom which-func-modes
  '(emacs-lisp-mode c-mode c++-mode perl-mode cperl-mode makefile-mode
		    sh-mode fortran-mode f90-mode ada-mode)
  "List of major modes for which Which Function mode should be used.
For other modes it is disabled.  If this is equal to t,
then Which Function mode is enabled in any major mode that supports it."
  :group 'which-func
  :type '(choice (const :tag "All modes" t)
		 (repeat (symbol :tag "Major mode"))))

(defcustom which-func-non-auto-modes nil
  "List of major modes where Which Function mode is inactive till Imenu is used.
This means that Which Function mode won't really do anything
until you use Imenu, in these modes.  Note that files
larger than `which-func-maxout' behave in this way too;
Which Function mode doesn't do anything until you use Imenu."
  :group 'which-func
  :type '(repeat (symbol :tag "Major mode")))

(defcustom which-func-maxout 500000
  "Don't automatically compute the Imenu menu if buffer is this big or bigger.
Zero means compute the Imenu menu regardless of size."
  :group 'which-func
  :type 'integer)

(defvar which-func-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'beginning-of-defun)
    (define-key map [mode-line mouse-2]
      (lambda ()
	(interactive)
	(if (eq (point-min) 1)
	    (narrow-to-defun)
	  (widen))))
    (define-key map [mode-line mouse-3] 'end-of-defun)
    map)
  "Keymap to display on mode line which-func.")

(defface which-func
  ;; Whether `font-lock-function-name-face' is an appropriate face to
  ;; inherit depends on the mode-line face; define several variants based
  ;; on the default mode-line face.
  '(;; The default mode-line face on a high-color display is a relatively
    ;; light color ("grey75"), and only the light-background variant of
    ;; `font-lock-function-name-face' is visible against it.
    (((class color) (min-colors 88) (background light))
     :inherit font-lock-function-name-face)
    ;; The default mode-line face on other display types is inverse-video;
    ;; it seems that only in the dark-background case is
    ;; `font-lock-function-name-face' visible against it.
    (((class grayscale mono) (background dark))
     :inherit font-lock-function-name-face)
    (((class color) (background light))
     :inherit font-lock-function-name-face)
    ;; If none of the above cases, use an explicit color chosen to contrast
    ;; well with the default mode-line face.
    (((class color) (min-colors 88) (background dark))
     :foreground "Blue1")
    (((background dark))
     :foreground "Blue1")
    (t
     :foreground "LightSkyBlue"))
  "Face used to highlight mode line function names."
  :group 'which-func)
;; backward-compatibility alias
(put 'which-func-face 'face-alias 'which-func)

(defcustom which-func-format
  `("["
    (:propertize which-func-current
		 local-map ,which-func-keymap
		 face which-func
		 ;;mouse-face highlight	; currently not evaluated :-(
		 help-echo "mouse-1: go to beginning, mouse-2: toggle rest visibility, mouse-3: go to end")
    "]")
  "Format for displaying the function in the mode line."
  :group 'which-func
  :type 'sexp)
;;;###autoload (put 'which-func-format 'risky-local-variable t)

(defvar which-func-cleanup-function nil
  "Function to transform a string before displaying it in the mode line.
The function is called with one argument, the string to display.
Its return value is displayed in the modeline.
If nil, no function is called.  The default value is nil.

This feature can be useful if Imenu is set up to make more
detailed entries (e.g., containing the argument list of a function),
and you want to simplify them for the mode line
\(e.g., removing the parameter list to just have the function name.)")

;;; Code, nothing to customize below here
;;; -------------------------------------
;;;
(require 'imenu)

(defvar which-func-table (make-hash-table :test 'eq :weakness 'key))

(defconst which-func-current
  '(:eval (gethash (selected-window) which-func-table which-func-unknown)))
;;;###autoload (put 'which-func-current 'risky-local-variable t)

(defvar which-func-mode nil
  "Non-nil means display current function name in mode line.
This makes a difference only if `which-function-mode' is non-nil.")
(make-variable-buffer-local 'which-func-mode)
;;(put 'which-func-mode 'permanent-local t)

(add-hook 'find-file-hook 'which-func-ff-hook t)

(defun which-func-ff-hook ()
  "File find hook for Which Function mode.
It creates the Imenu index for the buffer, if necessary."
  (setq which-func-mode
	(and which-function-mode
	     (or (eq which-func-modes t)
		 (member major-mode which-func-modes))))

  (condition-case nil
      (if (and which-func-mode
	       (not (member major-mode which-func-non-auto-modes))
	       (or (null which-func-maxout)
		   (< buffer-saved-size which-func-maxout)
		   (= which-func-maxout 0)))
	  (setq imenu--index-alist
		(save-excursion (funcall imenu-create-index-function))))
    (error
     (setq which-func-mode nil))))

(defun which-func-update ()
  ;; "Update the Which-Function mode display for all windows."
  ;; (walk-windows 'which-func-update-1 nil 'visible))
  (which-func-update-1 (selected-window)))

(defun which-func-update-1 (window)
  "Update the Which Function mode display for window WINDOW."
  (with-selected-window window
    (when which-func-mode
      (condition-case info
	  (let ((current (which-function)))
	    (unless (equal current (gethash window which-func-table))
	      (puthash window current which-func-table)
	      (force-mode-line-update)))
	(error
	 (setq which-func-mode nil)
	 (error "Error in which-func-update: %s" info))))))

;;;###autoload
(defalias 'which-func-mode 'which-function-mode)

(defvar which-func-update-timer nil)

;; This is the name people would normally expect.
;;;###autoload
(define-minor-mode which-function-mode
  "Toggle Which Function mode, globally.
When Which Function mode is enabled, the current function name is
continuously displayed in the mode line, in certain major modes.

With prefix ARG, turn Which Function mode on iff arg is positive,
and off otherwise."
  :global t :group 'which-func
  (if which-function-mode
      ;;Turn it on
      (progn
        (setq which-func-update-timer
              (run-with-idle-timer idle-update-delay t 'which-func-update))
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (setq which-func-mode
                  (or (eq which-func-modes t)
                      (member major-mode which-func-modes))))))
    ;; Turn it off
    (when (timerp which-func-update-timer)
      (cancel-timer which-func-update-timer))
    (setq which-func-update-timer nil)
    (dolist (buf (buffer-list))
      (with-current-buffer buf (setq which-func-mode nil)))))

(defvar which-function-imenu-failed nil
  "Locally t in a buffer if `imenu--make-index-alist' found nothing there.")

(defvar which-func-functions nil
  "List of functions for `which-function' to call with no arguments.
It calls them sequentially, and if any returns non-nil,
`which-function' uses that name and stops looking for the name.")

(defun which-function ()
  "Return current function name based on point.
Uses `which-function-functions', `imenu--index-alist'
or `add-log-current-defun-function'.
If no function name is found, return nil."
  (let ((name
	 ;; Try the `which-function-functions' functions first.
	 (run-hook-with-args-until-success 'which-func-functions)))

    ;; If Imenu is loaded, try to make an index alist with it.
    (when (and (null name)
	       (boundp 'imenu--index-alist) (null imenu--index-alist)
	       (null which-function-imenu-failed))
      (imenu--make-index-alist t)
      (unless imenu--index-alist
	(make-local-variable 'which-function-imenu-failed)
	(setq which-function-imenu-failed t)))
    ;; If we have an index alist, use it.
    (when (and (null name)
	       (boundp 'imenu--index-alist) imenu--index-alist)
      (let ((alist imenu--index-alist)
            (minoffset (point-max))
            offset elem pair mark)
        (while alist
          (setq elem  (car-safe alist)
                alist (cdr-safe alist))
          ;; Elements of alist are either ("name" . marker), or
          ;; ("submenu" ("name" . marker) ... ).
          (unless (listp (cdr elem))
              (setq elem (list elem)))
          (while elem
            (setq pair (car elem)
                  elem (cdr elem))
            (and (consp pair)
                 (number-or-marker-p (setq mark (cdr pair)))
                 (if (>= (setq offset (- (point) mark)) 0)
                     (if (< offset minoffset) ; find the closest item
                         (setq minoffset offset
                               name (car pair)))
                   ;; Entries in order, so can skip all those after point.
                   (setq elem nil)))))))
    ;; Try using add-log support.
    (when (and (null name) (boundp 'add-log-current-defun-function)
	       add-log-current-defun-function)
      (setq name (funcall add-log-current-defun-function)))
    ;; Filter the name if requested.
    (when name
      (if which-func-cleanup-function
	  (funcall which-func-cleanup-function name)
	name))))

(provide 'which-func)

;; arch-tag: fa8a55c7-bfe3-4ffc-95ab-01bf21796827
;;; which-func.el ends here
