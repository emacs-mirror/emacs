;;; rfn-eshadow.el --- Highlight `shadowed' part of read-file-name input text
;;
;; Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.
;;
;; Author: Miles Bader <miles@gnu.org>
;; Keywords: convenience

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

;;; Commentary:
;;
;; Defines the mode `read-file-name-electric-shadow-mode'.
;;
;; The `read-file-name' function passes its result through
;; `substitute-in-file-name', so any part of the string preceding
;; multiple slashes (or a drive indicator on MS-DOS/MS-Windows) is
;; ignored.
;;
;; If `read-file-name-electric-shadow-mode' is active, any part of the
;; minibuffer text that would be ignored because of this is given the
;; properties in `read-file-name-electric-shadow-properties', which may
;; be used to make the ignored text invisible, dim, etc.
;;

;;; Code:


;;; Customization

(defconst read-file-name-electric-shadow-properties-custom-type
  '(list
    (checklist :inline t
	       (const :tag "Invisible"
		      :doc "Make shadowed part of filename invisible"
		      :format "%t%n%h"
		      :inline t
		      (invisible t intangible t))
	       (list :inline t
		     :format "%v"
		     :tag "Face"
		     :doc "Display shadowed part of filename using a different face"
		     (const :format "" face)
		     (face :value read-file-name-electric-shadow))
	       (list :inline t
		     :format "%t: %v%h"
		     :tag "Brackets"
		     ;; Note the 4 leading spaces in the doc string;
		     ;; this is hack to get around the fact that the
		     ;; newline after the second string widget comes
		     ;; from the string widget, and doesn't indent
		     ;; correctly.  We could use a :size attribute to
		     ;; make the second string widget not have a
		     ;; terminating newline, but this makes it impossible
		     ;; to enter trailing whitespace, and it's desirable
		     ;; that it be possible.
		     :doc "    Surround shadowed part of filename with brackets"
		     (const :format "" before-string)
		     (string :format "%v" :size 4 :value "{")
		     (const :format "" after-string)
		     ;; see above about why the 2nd string doesn't use :size
		     (string :format " and: %v" :value "} "))
	       (list :inline t
		     :format "%t: %v%n%h"
		     :tag "String"
		     :doc "Display a string instead of the shadowed part of filename"
		     (const :format "" display)
		     (string :format "%v" :size 15 :value "<...ignored...>"))
	       (const :tag "Avoid"
		      :doc "Try to keep cursor out of shadowed part of filename"
		      :format "%t%n%h"
		      :inline t
		      (field shadow)))
    (repeat :inline t
	    :tag "Other Properties"
	    (list :inline t
		  :format "%v"
		  (symbol :tag "Property")
		  (sexp :tag "Value")))))

;;;###autoload
(defcustom read-file-name-electric-shadow-properties
  '(face read-file-name-electric-shadow field shadow)
  "Properties given to the `shadowed' part of a filename in the minibuffer.
Only used when `read-file-name-electric-shadow-mode' is active.
If emacs is not running under a window system,
`read-file-name-electric-shadow-tty-properties' is used instead."
  :type read-file-name-electric-shadow-properties-custom-type
  :group 'minibuffer)

;;;###autoload
(defcustom read-file-name-electric-shadow-tty-properties
  '(before-string "{" after-string "} " field shadow)
  "Properties given to the `shadowed' part of a filename in the minibuffer.
Only used when `read-file-name-electric-shadow-mode' is active and emacs
is not running under a window-system; if emacs is running under a window
system, `read-file-name-electric-shadow-properties' is used instead."
  :type read-file-name-electric-shadow-properties-custom-type
  :group 'minibuffer)

(defface read-file-name-electric-shadow
  '((((background dark))
     :foreground "grey50")
    (t
     :foreground "grey70"))
  "Face used by `read-file-name-electric-shadow-mode' for the shadow."
  :group 'minibuffer)


;;; Internal variables

;; Regexp to locate dividing point between shadow and real pathname
(defconst rfn-eshadow-regexp
  (cond ((memq system-type '(ms-dos windows-nt))
	 ;; This horrible regexp considers the following patterns as
	 ;; starting an absolute pathname, when following a `/' or an `\':
	 ;;   L:  /  //  ~  $  \\  \\\\
	 "\\(.*[^/]+/+?\\|/*?\\|\\)\\(~\\|$[^$]\\|$\\'\\|[][\\^a-z]:\\|//?\\([^][\\^a-z/$~]\\|[^/$~][^:]\\|[^/$~]?\\'\\)\\)")
	(t
	 ;; default is for unix-style filenames
	 "\\(.*/\\)\\([/~]\\|$[^$]\\|$\\'\\)"))
  "Regular expression used to match shadowed filenames.
There should be at least one regexp group; the end of the first one
is used as the end of the shadowed portion of the filename.")

;; A list of minibuffers to which we've added a post-command-hook.
(defvar rfn-eshadow-frobbed-minibufs nil)

;; An overlay covering the shadowed part of the filename (local to the
;; minibuffer).
(defvar rfn-eshadow-overlay)
(make-variable-buffer-local 'rfn-eshadow-overlay)


;;; Hook functions

;; This function goes on minibuffer-setup-hook
(defun rfn-eshadow-setup-minibuffer ()
  "Set up a minibuffer for `read-file-name-electric-shadow-mode'.
The prompt and initial input should already have been inserted."
  (when minibuffer-completing-file-name
    (setq rfn-eshadow-overlay
	  (make-overlay (minibuffer-prompt-end) (minibuffer-prompt-end)))
    ;; Give rfn-eshadow-overlay the user's props.
    (let ((props
	   (if window-system
	       read-file-name-electric-shadow-properties
	     read-file-name-electric-shadow-tty-properties)))
      (while props
	(overlay-put rfn-eshadow-overlay (pop props) (pop props))))
    ;; Turn on overlay evaporation so that we don't have to worry about
    ;; odd effects when the overlay sits empty at the beginning of the
    ;; minibuffer.
    (overlay-put rfn-eshadow-overlay 'evaporate t)
    ;; Add our post-command hook, and make sure can remove it later.
    (add-to-list 'rfn-eshadow-frobbed-minibufs (current-buffer))
    (add-hook 'post-command-hook #'rfn-eshadow-update-overlay nil t)))

;; post-command-hook to update overlay
(defun rfn-eshadow-update-overlay ()
  "Update `rfn-eshadow-overlay' to cover shadowed part of minibuffer input.
This is intended to be used as a minibuffer post-command-hook for
`read-file-name-electric-shadow-mode'; the minibuffer should have already
been set up by `rfn-eshadow-setup-minibuffer'."
  ;; This is not really a correct implementation; it won't always do the
  ;; right thing in the presence of environment variables that
  ;; substitute-in-file-name would expand; currently it just assumes any
  ;; environment variable contains an absolute filename.
  (save-excursion
    (let ((inhibit-point-motion-hooks t))
      (goto-char (minibuffer-prompt-end))
      ;; Update the overlay (which will evaporate if it's empty).
      (move-overlay rfn-eshadow-overlay
		    (point)
		    (if (looking-at rfn-eshadow-regexp)
			(match-end 1)
		      (point))))))


;;; Note this definition must be at the end of the file, because
;;; `define-minor-mode' actually calls the mode-function if the
;;; associated variable is non-nil, which requires that all needed
;;; functions be already defined.  [This is arguably a bug in d-m-m]
;;;###autoload
(define-minor-mode read-file-name-electric-shadow-mode
  "Toggle Read-File-Name Electric Shadow mode.
When active, any part of the filename being read in the minibuffer
that would be ignored because the result is passed through
`substitute-in-file-name' is given the properties in
`read-file-name-electric-shadow-properties', which can be used to make
that portion dim, invisible, or otherwise less visually noticeable.

With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled."
  :global t
  :group 'minibuffer
  (if read-file-name-electric-shadow-mode
      ;; Enable the mode
      (add-hook 'minibuffer-setup-hook 'rfn-eshadow-setup-minibuffer)
    ;; Disable the mode
    (remove-hook 'minibuffer-setup-hook 'rfn-eshadow-setup-minibuffer)
    ;; Remove our entry from any post-command-hook variable's it's still in
    (dolist (minibuf rfn-eshadow-frobbed-minibufs)
      (with-current-buffer minibuf
	(remove-hook 'post-command-hook #'rfn-eshadow-update-overlay t)))
    (setq rfn-eshadow-frobbed-minibufs nil)))


(provide 'rfn-eshadow)

;;; rfn-eshadow.el ends here
