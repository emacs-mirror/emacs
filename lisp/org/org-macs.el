;;; org-macs.el --- Top-level definitions for Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
;;   Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.35i
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains macro definitions, defsubst definitions, other
;; stuff needed for compilation and top-level forms in Org-mode, as well
;; lots of small functions that are not org-mode specific but simply
;; generally useful stuff.

;;; Code:

(eval-and-compile
  (unless (fboundp 'declare-function)
    (defmacro declare-function (fn file &optional arglist fileonly))))

(declare-function org-add-props "org-compat" (string plist &rest props))

(defmacro org-bound-and-true-p (var)
  "Return the value of symbol VAR if it is bound, else nil."
  `(and (boundp (quote ,var)) ,var))

(defmacro org-unmodified (&rest body)
  "Execute body without changing `buffer-modified-p'.
Also, do not record undo information."
  `(set-buffer-modified-p
    (prog1 (buffer-modified-p)
      (let ((buffer-undo-list t)
	    before-change-functions after-change-functions)
	,@body))))

(defmacro org-re (s)
  "Replace posix classes in regular expression."
  (if (featurep 'xemacs)
      (let ((ss s))
	(save-match-data
	  (while (string-match "\\[:alnum:\\]" ss)
	    (setq ss (replace-match "a-zA-Z0-9" t t ss)))
	  (while (string-match "\\[:word:\\]" ss)
	    (setq ss (replace-match "a-zA-Z0-9" t t ss)))
	  (while (string-match "\\[:alpha:\\]" ss)
	    (setq ss (replace-match "a-zA-Z" t t ss)))
	  (while (string-match "\\[:punct:\\]" ss)
	    (setq ss (replace-match "\001-@[-`{-~" t t ss)))
	  ss))
    s))

(defmacro org-preserve-lc (&rest body)
  `(let ((_line (org-current-line))
	 (_col (current-column)))
     (unwind-protect
	 (progn ,@body)
       (org-goto-line _line)
       (org-move-to-column _col))))

(defmacro org-without-partial-completion (&rest body)
  `(let ((pc-mode (and (boundp 'partial-completion-mode)
		       partial-completion-mode)))
     (unwind-protect
	 (progn
	   (if pc-mode (partial-completion-mode -1))
	   ,@body)
       (if pc-mode (partial-completion-mode 1)))))

(defmacro org-maybe-intangible (props)
  "Add '(intangible t) to PROPS if Emacs version is earlier than Emacs 22.
In emacs 21, invisible text is not avoided by the command loop, so the
intangible property is needed to make sure point skips this text.
In Emacs 22, this is not necessary.  The intangible text property has
led to problems with flyspell.  These problems are fixed in flyspell.el,
but we still avoid setting the property in Emacs 22 and later.
We use a macro so that the test can happen at compilation time."
  (if (< emacs-major-version 22)
      `(append '(intangible t) ,props)
    props))

(defmacro org-with-point-at (pom &rest body)
  "Move to buffer and point of point-or-marker POM for the duration of BODY."
  `(save-excursion
     (if (markerp ,pom) (set-buffer (marker-buffer ,pom)))
     (save-excursion
       (goto-char (or ,pom (point)))
       ,@body)))
(put 'org-with-point-at 'lisp-indent-function 1)

(defmacro org-no-warnings (&rest body)
  (cons (if (fboundp 'with-no-warnings) 'with-no-warnings 'progn) body))

(defmacro org-if-unprotected (&rest body)
  "Execute BODY if there is no `org-protected' text property at point."
  `(unless (get-text-property (point) 'org-protected)
     ,@body))

(defmacro org-if-unprotected-1 (&rest body)
  "Execute BODY if there is no `org-protected' text property at point-1."
  `(unless (get-text-property (1- (point)) 'org-protected)
     ,@body))

(defmacro org-if-unprotected-at (pos &rest body)
  "Execute BODY if there is no `org-protected' text property at POS."
  `(unless (get-text-property ,pos 'org-protected)
     ,@body))
(put 'org-if-unprotected-at 'lisp-indent-function 1)

(defun org-re-search-forward-unprotected (&rest args)
  "Like re-search-forward, but stop only in unprotected places."
  (catch 'exit
    (while t
      (unless (apply 're-search-forward args)
	(throw 'exit nil))
      (unless (get-text-property (match-beginning 0) 'org-protected)
	(throw 'exit (point))))))

(defmacro org-with-remote-undo (_buffer &rest _body)
  "Execute BODY while recording undo information in two buffers."
  `(let ((_cline (org-current-line))
	 (_cmd this-command)
	 (_buf1 (current-buffer))
	 (_buf2 ,_buffer)
	 (_undo1 buffer-undo-list)
	 (_undo2 (with-current-buffer ,_buffer buffer-undo-list))
	 _c1 _c2)
     ,@_body
     (when org-agenda-allow-remote-undo
       (setq _c1 (org-verify-change-for-undo
		  _undo1 (with-current-buffer _buf1 buffer-undo-list))
	     _c2 (org-verify-change-for-undo
		  _undo2 (with-current-buffer _buf2 buffer-undo-list)))
       (when (or _c1 _c2)
	 ;; make sure there are undo boundaries
	 (and _c1 (with-current-buffer _buf1 (undo-boundary)))
	 (and _c2 (with-current-buffer _buf2 (undo-boundary)))
	 ;; remember which buffer to undo
	 (push (list _cmd _cline _buf1 _c1 _buf2 _c2)
	       org-agenda-undo-list)))))

(defmacro org-no-read-only (&rest body)
  "Inhibit read-only for BODY."
  `(let ((inhibit-read-only t)) ,@body))

(defconst org-rm-props '(invisible t face t keymap t intangible t mouse-face t
				   rear-nonsticky t mouse-map t fontified t)
  "Properties to remove when a string without properties is wanted.")

(defsubst org-match-string-no-properties (num &optional string)
  (if (featurep 'xemacs)
      (let ((s (match-string num string)))
	(and s (remove-text-properties 0 (length s) org-rm-props s))
	s)
    (match-string-no-properties num string)))

(defsubst org-no-properties (s)
  (if (fboundp 'set-text-properties)
      (set-text-properties 0 (length s) nil s)
    (remove-text-properties 0 (length s) org-rm-props s))
  s)

(defsubst org-get-alist-option (option key)
  (cond ((eq key t) t)
	((eq option t) t)
	((assoc key option) (cdr (assoc key option)))
	(t (cdr (assq 'default option)))))

(defsubst org-check-external-command (cmd &optional use no-error)
  "Check if external program CMD for USE exists, error if not.
When the program does exist, return its path.
When it does not exist and NO-ERROR is set, return nil.
Otherwise, throw an error.  The optional argument USE can describe what this
program is needed for, so that the error message can be more informative."
  (or (executable-find cmd)
      (if no-error
	  nil
	(error "Can't find `%s'%s" cmd
	       (if use (format " (%s)" use) "")))))

(defsubst org-inhibit-invisibility ()
  "Modified `buffer-invisibility-spec' for Emacs 21.
Some ops with invisible text do not work correctly on Emacs 21.  For these
we turn off invisibility temporarily.  Use this in a `let' form."
  (if (< emacs-major-version 22) nil buffer-invisibility-spec))

(defsubst org-set-local (var value)
  "Make VAR local in current buffer and set it to VALUE."
  (set (make-local-variable var) value))

(defsubst org-mode-p ()
  "Check if the current buffer is in Org-mode."
  (eq major-mode 'org-mode))

(defsubst org-last (list)
  "Return the last element of LIST."
  (car (last list)))

(defun org-let (list &rest body)
  (eval (cons 'let (cons list body))))
(put 'org-let 'lisp-indent-function 1)

(defun org-let2 (list1 list2 &rest body)
  (eval (cons 'let (cons list1 (list (cons 'let (cons list2 body)))))))
(put 'org-let2 'lisp-indent-function 2)

(defsubst org-call-with-arg (command arg)
  "Call COMMAND interactively, but pretend prefix arg was ARG."
  (let ((current-prefix-arg arg)) (call-interactively command)))

(defsubst org-current-line (&optional pos)
  (save-excursion
    (and pos (goto-char pos))
    ;; works also in narrowed buffer, because we start at 1, not point-min
    (+ (if (bolp) 1 0) (count-lines 1 (point)))))

(defsubst org-goto-line (N)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- N))))

(defsubst org-current-line-string (&optional to-here)
  (buffer-substring (point-at-bol) (if to-here (point) (point-at-eol))))

(defsubst org-pos-in-match-range (pos n)
  (and (match-beginning n)
       (<= (match-beginning n) pos)
       (>= (match-end n) pos)))

(defun org-autoload (file functions)
  "Establish autoload for all FUNCTIONS in FILE, if not bound already."
  (let ((d (format "Documentation will be available after `%s.el' is loaded."
		   file))
	f)
    (while (setq f (pop functions))
      (or (fboundp f) (autoload f file d t)))))

(defun org-match-line (re)
  "Looking-at at the beginning of the current line."
  (save-excursion
    (goto-char (point-at-bol))
    (looking-at re)))

(defun org-plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
	  (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))


(defun org-replace-match-keep-properties (newtext &optional fixedcase
						  literal string)
  "Like `replace-match', but add the text properties found original text."
  (setq newtext (org-add-props newtext (text-properties-at
					(match-beginning 0) string)))
  (replace-match newtext fixedcase literal string))

(defmacro org-with-limited-levels (&rest body)
  "Execute BODY with limited number of outline levels."
  `(let* ((outline-regexp (org-get-limited-outline-regexp)))
     ,@body))

(defvar org-odd-levels-only) ; defined in org.el
(defvar org-inlinetask-min-level) ; defined in org-inlinetask.el
(defun org-get-limited-outline-regexp ()
  "Return outline-regexp with limited number of levels.
The number of levels is controlled by "
  (if (or (not (org-mode-p)) (not (featurep 'org-inlinetask)))

      outline-regexp
    (let* ((limit-level (1- org-inlinetask-min-level))
	   (nstars (if org-odd-levels-only (1- (* limit-level 2)) limit-level)))
      (format "\\*\\{1,%d\\} " nstars))))

(provide 'org-macs)

;; arch-tag: 7e6a73ce-aac9-4fc0-9b30-ce6f89dc6668

;;; org-macs.el ends here
