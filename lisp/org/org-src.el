;;; org-src.el --- Source code examples in Org
;;
;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009
;;   Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten at orgmode dot org>
;;	   Bastien Guerry <bzg AT altern DOT org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.30c
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code dealing with source code examples in Org-mode.

;;; Code:

(require 'org-macs)
(require 'org-compat)

(declare-function org-do-remove-indentation "org" (&optional n))
(declare-function org-get-indentation "org" (&optional line))

(defcustom org-edit-src-region-extra nil
  "Additional regexps to identify regions for editing with `org-edit-src-code'.
For examples see the function `org-edit-src-find-region-and-lang'.
The regular expression identifying the begin marker should end with a newline,
and the regexp marking the end line should start with a newline, to make sure
there are kept outside the narrowed region."
  :group 'org-edit-structure
  :type '(repeat
	  (list
	   (regexp :tag "begin regexp")
	   (regexp :tag "end regexp")
	   (choice :tag "language"
		   (string :tag "specify")
		   (integer :tag "from match group")
		   (const :tag "from `lang' element")
		   (const :tag "from `style' element")))))

(defcustom org-coderef-label-format "(ref:%s)"
  "The default coderef format.
This format string will be used to search for coderef labels in literal
examples (EXAMPLE and SRC blocks).  The format can be overwritten in
an individual literal example with the -f option, like

#+BEGIN_SRC pascal +n -r -l \"((%s))\"
...
#+END_SRC

If you want to use this for HTML export, make sure that the format does
not introduce special font-locking, and avoid the HTML special
characters `<', `>', and `&'.  The reason for this restriction is that
the labels are searched for only after htmlize has done its job."
  :group 'org-edit-structure ; FIXME this is not in the right group
  :type 'string)

(defcustom org-edit-fixed-width-region-mode 'artist-mode
  "The mode that should be used to edit fixed-width regions.
These are the regions where each line starts with a colon."
  :group 'org-edit-structure
  :type '(choice
	  (const artist-mode)
	  (const picture-mode)
	  (const fundamental-mode)
	  (function :tag "Other (specify)")))

(defcustom org-edit-src-content-indentation 2
  "Indentation for the content is a source code block.
This should be the number of spaces added to the indentation of the #+begin
line in order to compute the indentation of the block content after
editing it with \\[org-edit-src-code]."
  :group 'org-edit-structure
  :type 'integer)

(defcustom org-edit-src-persistent-message t
  "Non-nil means show persistent exit help message while editing src examples.
The message is shown in the header-line, which will be created in the
first line of the window showing the editing buffer.
When nil, the message will only be shown intermittently in the echo area."
  :group 'org-edit-structure
  :type 'boolean)


(defvar org-src-mode-hook nil
  "Hook  run after Org switched a source code snippet to its Emacs mode.
This hook will run

- when editing a source code snippet with \"C-c '\".
- When formatting a source code snippet for export with htmlize.

You may want to use this hook for example to turn off `outline-minor-mode'
or similar things which you want to have when editing a source code file,
but which mess up the display of a snippet in Org exported files.")

(defcustom org-src-lang-modes
  '(("ocaml" . tuareg) ("elisp" . emacs-lisp) ("ditaa" . artist))
  "Alist mapping languages to their major mode.
The key is the language name, the value is the string that should
be inserted as the name of the major mode.  For many languages this is
simple, but for language where this is not the case, this variable
provides a way to simplify things on the user side.
For example, there is no ocaml-mode in Emacs, but the mode to use is
`tuareg-mode'."
  :group 'org-edit-structure
  :type '(repeat
	  (cons
	   (string "Language name")
	   (symbol "Major mode"))))

;;; Editing source examples

(defvar org-src-mode-map (make-sparse-keymap))
(define-key org-src-mode-map "\C-c'" 'org-edit-src-exit)
(defvar org-edit-src-force-single-line nil)
(defvar org-edit-src-from-org-mode nil)
(defvar org-edit-src-picture nil)
(defvar org-edit-src-beg-marker nil)
(defvar org-edit-src-end-marker nil)
(defvar org-edit-src-overlay nil)
(defvar org-edit-src-nindent nil)

(define-minor-mode org-src-mode
  "Minor mode for language major mode buffers generated by org.
This minor mode is turned on in two situations:
- when editing a source code snippet with \"C-c '\".
- When formatting a source code snippet for export with htmlize.
There is a mode hook, and keybindings for `org-edit-src-exit' and
`org-edit-src-save'")

(defun org-edit-src-code ()
  "Edit the source code example at point.
The example is copied to a separate buffer, and that buffer is switched
to the correct language mode.  When done, exit with \\[org-edit-src-exit].
This will remove the original code in the Org buffer, and replace it with
the edited version."
  (interactive)
  (let ((line (org-current-line))
	(case-fold-search t)
	(msg (substitute-command-keys
	      "Edit, then exit with C-c ' (C-c and single quote)"))
	(info (org-edit-src-find-region-and-lang))
	(org-mode-p (eq major-mode 'org-mode))
	(beg (make-marker))
	(end (make-marker))
	nindent	ovl lang lang-f single lfmt code begline buffer)
    (if (not info)
	nil
      (setq beg (move-marker beg (nth 0 info))
	    end (move-marker end (nth 1 info))
	    code (buffer-substring-no-properties beg end)
	    lang (or (cdr (assoc (nth 2 info) org-src-lang-modes))
                     (nth 2 info))
	    lang (if (symbolp lang) (symbol-name lang) lang)
	    single (nth 3 info)
	    lfmt (nth 4 info)
	    nindent (nth 5 info)
	    lang-f (intern (concat lang "-mode"))
	    begline (save-excursion (goto-char beg) (org-current-line)))
      (unless (functionp lang-f)
	(error "No such language mode: %s" lang-f))
      (org-goto-line line)
      (if (and (setq buffer (org-edit-src-find-buffer beg end))
	       (y-or-n-p "Return to existing edit buffer? [n] will revert changes: "))
	  (switch-to-buffer buffer)
	(when buffer
	  (with-current-buffer buffer
	    (if (boundp 'org-edit-src-overlay)
		(org-delete-overlay org-edit-src-overlay)))
	  (kill-buffer buffer))
	(setq buffer (generate-new-buffer
		      (concat "*Org Src " (file-name-nondirectory buffer-file-name) "[" lang "]*")))
	(setq ovl (org-make-overlay beg end))
	(org-overlay-put ovl 'face 'secondary-selection)
	(org-overlay-put ovl 'edit-buffer buffer)
	(org-overlay-put ovl 'help-echo "Click with mouse-1 to switch to buffer editing this segment")
	(org-overlay-put ovl 'face 'secondary-selection)
	(org-overlay-put ovl
			 'keymap
			 (let ((map (make-sparse-keymap)))
			   (define-key map [mouse-1] 'org-edit-src-continue)
			   map))
	(org-overlay-put ovl :read-only "Leave me alone")
	(switch-to-buffer buffer)
	(insert code)
	(remove-text-properties (point-min) (point-max)
				'(display nil invisible nil intangible nil))
	(org-do-remove-indentation)
	(let ((org-inhibit-startup t))
	  (funcall lang-f))
	(set (make-local-variable 'org-edit-src-force-single-line) single)
	(set (make-local-variable 'org-edit-src-from-org-mode) org-mode-p)
	(when lfmt
	  (set (make-local-variable 'org-coderef-label-format) lfmt))
	(when org-mode-p
	  (goto-char (point-min))
	  (while (re-search-forward "^," nil t)
	    (replace-match "")))
	(org-goto-line (1+ (- line begline)))
	(org-set-local 'org-edit-src-beg-marker beg)
	(org-set-local 'org-edit-src-end-marker end)
	(org-set-local 'org-edit-src-overlay ovl)
	(org-set-local 'org-edit-src-nindent nindent)
	(org-src-mode)
	(set-buffer-modified-p nil)
	(and org-edit-src-persistent-message
	     (org-set-local 'header-line-format msg)))
      (message "%s" msg)
      t)))

(defun org-edit-src-continue (e)
  (interactive "e")
  (mouse-set-point e)
  (let ((buf (get-char-property (point) 'edit-buffer)))
    (if buf (switch-to-buffer buf)
      (error "Something is wrong here"))))

(defun org-edit-src-find-buffer (beg end)
  "Find a source editing buffer that is already editing the region BEG to END."
  (catch 'exit
    (mapc
     (lambda (b)
       (with-current-buffer b
	 (if (and (string-match "\\`*Org Edit " (buffer-name))
		  (local-variable-p 'org-edit-src-beg-marker (current-buffer))
		  (local-variable-p 'org-edit-src-end-marker (current-buffer))
		  (equal beg org-edit-src-beg-marker)
		  (equal end org-edit-src-end-marker))
	     (throw 'exit (current-buffer)))))
     (buffer-list))
    nil))

(defun org-edit-fixed-width-region ()
  "Edit the fixed-width ascii drawing at point.
This must be a region where each line starts with a colon followed by
a space character.
An new buffer is created and the fixed-width region is copied into it,
and the buffer is switched into `artist-mode' for editing.  When done,
exit with \\[org-edit-src-exit].  The edited text will then replace
the fragment in the Org-mode buffer."
  (interactive)
  (let ((line (org-current-line))
	(case-fold-search t)
	(msg (substitute-command-keys
	      "Edit, then exit with C-c ' (C-c and single quote)"))
	(org-mode-p (eq major-mode 'org-mode))
	(beg (make-marker))
	(end (make-marker))
	nindent ovl beg1 end1 code begline buffer)
    (beginning-of-line 1)
    (if (looking-at "[ \t]*[^:\n \t]")
	nil
      (if (looking-at "[ \t]*\\(\n\\|\\'\\)")
	  (setq beg1 (point) end1 beg1)
	(save-excursion
	  (if (re-search-backward "^[ \t]*[^: \t]" nil 'move)
	      (setq beg1 (point-at-bol 2))
	    (setq beg1 (point))))
	(save-excursion
	  (if (re-search-forward "^[ \t]*[^: \t]" nil 'move)
	      (setq end1 (1- (match-beginning 0)))
	    (setq end1 (point))))
	(org-goto-line line))
      (setq beg (move-marker beg beg1)
	    end (move-marker end end1)
	    code (buffer-substring-no-properties beg end)
	    begline (save-excursion (goto-char beg) (org-current-line)))
      (if (and (setq buffer (org-edit-src-find-buffer beg end))
	       (y-or-n-p "Return to existing edit buffer? [n] will revert changes: "))
	  (switch-to-buffer buffer)
	(when buffer
	  (with-current-buffer buffer
	    (if (boundp 'org-edit-src-overlay)
		(org-delete-overlay org-edit-src-overlay)))
	  (kill-buffer buffer))
	(setq buffer (generate-new-buffer "*Org Edit Src Example*"))
	(setq ovl (org-make-overlay beg end))
	(org-overlay-put ovl 'face 'secondary-selection)
	(org-overlay-put ovl 'edit-buffer buffer)
	(org-overlay-put ovl 'help-echo "Click with mouse-1 to switch to buffer editing this segment")
	(org-overlay-put ovl 'face 'secondary-selection)
	(org-overlay-put ovl
			 'keymap
			 (let ((map (make-sparse-keymap)))
			   (define-key map [mouse-1] 'org-edit-src-continue)
			   map))
	(org-overlay-put ovl :read-only "Leave me alone")
	(switch-to-buffer buffer)
	(insert code)
	(remove-text-properties (point-min) (point-max)
				'(display nil invisible nil intangible nil))
	(setq nindent (org-do-remove-indentation))
	(cond
	 ((eq org-edit-fixed-width-region-mode 'artist-mode)
	  (fundamental-mode)
	  (artist-mode 1))
       (t (funcall org-edit-fixed-width-region-mode)))
	(set (make-local-variable 'org-edit-src-force-single-line) nil)
	(set (make-local-variable 'org-edit-src-from-org-mode) org-mode-p)
	(set (make-local-variable 'org-edit-src-picture) t)
	(goto-char (point-min))
	(while (re-search-forward "^[ \t]*: ?" nil t)
	  (replace-match ""))
	(org-goto-line (1+ (- line begline)))
	(org-set-local 'org-edit-src-beg-marker beg)
	(org-set-local 'org-edit-src-end-marker end)
	(org-set-local 'org-edit-src-overlay ovl)
	(org-set-local 'org-edit-src-nindent nindent)
	(org-src-mode)
	(set-buffer-modified-p nil)
	(and org-edit-src-persistent-message
	     (org-set-local 'header-line-format msg)))
      (message "%s" msg)
      t)))

(defun org-edit-src-find-region-and-lang ()
  "Find the region and language for a local edit.
Return a list with beginning and end of the region, a string representing
the language, a switch telling of the content should be in a single line."
  (let ((re-list
	 (append
	  org-edit-src-region-extra
	  '(
	    ("<src\\>[^<]*>[ \t]*\n?" "\n?[ \t]*</src>" lang)
	    ("<literal\\>[^<]*>[ \t]*\n?" "\n?[ \t]*</literal>" style)
	    ("<example>[ \t]*\n?" "\n?[ \t]*</example>" "fundamental")
	    ("<lisp>[ \t]*\n?" "\n?[ \t]*</lisp>" "emacs-lisp")
	    ("<perl>[ \t]*\n?" "\n?[ \t]*</perl>" "perl")
	    ("<python>[ \t]*\n?" "\n?[ \t]*</python>" "python")
	    ("<ruby>[ \t]*\n?" "\n?[ \t]*</ruby>" "ruby")
	    ("^[ \t]*#\\+begin_src\\( \\([^ \t\n]+\\)\\)?.*\n" "\n[ \t]*#\\+end_src" 2)
	    ("^[ \t]*#\\+begin_example.*\n" "\n[ \t]*#\\+end_example" "fundamental")
	    ("^[ \t]*#\\+html:" "\n" "html" single-line)
	    ("^[ \t]*#\\+begin_html.*\n" "\n[ \t]*#\\+end_html" "html")
	    ("^[ \t]*#\\+latex:" "\n" "latex" single-line)
	    ("^[ \t]*#\\+begin_latex.*\n" "\n[ \t]*#\\+end_latex" "latex")
	    ("^[ \t]*#\\+ascii:" "\n" "fundamental" single-line)
	    ("^[ \t]*#\\+begin_ascii.*\n" "\n[ \t]*#\\+end_ascii" "fundamental")
	    ("^[ \t]*#\\+docbook:" "\n" "xml" single-line)
	    ("^[ \t]*#\\+begin_docbook.*\n" "\n[ \t]*#\\+end_docbook" "xml")
	    )))
	(pos (point))
	re1 re2 single beg end lang lfmt match-re1 ind entry)
    (catch 'exit
      (while (setq entry (pop re-list))
	(setq re1 (car entry) re2 (nth 1 entry) lang (nth 2 entry)
	      single (nth 3 entry))
	(save-excursion
	  (if (or (looking-at re1)
		  (re-search-backward re1 nil t))
	      (progn
		(setq match-re1 (match-string 0))
		(setq beg (match-end 0)
		      lang (org-edit-src-get-lang lang)
		      lfmt (org-edit-src-get-label-format match-re1)
		      ind (org-edit-src-get-indentation (match-beginning 0)))
		(if (and (re-search-forward re2 nil t)
			 (>= (match-end 0) pos))
		    (throw 'exit (list beg (match-beginning 0)
				       lang single lfmt ind))))
	    (if (or (looking-at re2)
		    (re-search-forward re2 nil t))
		(progn
		  (setq end (match-beginning 0))
		  (if (and (re-search-backward re1 nil t)
			   (<= (match-beginning 0) pos))
		      (progn
			(setq lfmt (org-edit-src-get-label-format
				    (match-string 0))
			      ind (org-edit-src-get-indentation
				   (match-beginning 0)))
			(throw 'exit
			       (list (match-end 0) end
				     (org-edit-src-get-lang lang)
				     single lfmt ind))))))))))))

(defun org-edit-src-get-lang (lang)
  "Extract the src language."
  (let ((m (match-string 0)))
    (cond
     ((stringp lang) lang)
     ((integerp lang) (match-string lang))
     ((and (eq lang 'lang)
	   (string-match "\\<lang=\"\\([^ \t\n\"]+\\)\"" m))
      (match-string 1 m))
     ((and (eq lang 'style)
	   (string-match "\\<style=\"\\([^ \t\n\"]+\\)\"" m))
      (match-string 1 m))
     (t "fundamental"))))

(defun org-edit-src-get-label-format (s)
  "Extract the label format."
  (save-match-data
    (if (string-match "-l[ \t]+\\\\?\"\\([^\t\r\n\"]+\\)\\\\?\"" s)
	(match-string 1 s))))

(defun org-edit-src-get-indentation (pos)
  "Extract the label format."
  (save-match-data
    (goto-char pos)
    (org-get-indentation)))

(defun org-edit-src-exit ()
  "Exit special edit and protect problematic lines."
  (interactive)
  (unless org-edit-src-from-org-mode
    (error "This is not a sub-editing buffer, something is wrong..."))
  (let ((beg org-edit-src-beg-marker)
	(end org-edit-src-end-marker)
	(ovl org-edit-src-overlay)
	(buffer (current-buffer))
	(nindent org-edit-src-nindent)
	code line)
    (untabify (point-min) (point-max))
    (save-excursion
      (goto-char (point-min))
      (if (looking-at "[ \t\n]*\n") (replace-match ""))
      (if (re-search-forward "\n[ \t\n]*\\'" nil t) (replace-match "")))
    (setq line (if (org-bound-and-true-p org-edit-src-force-single-line)
		   1
		 (org-current-line)))
    (when (org-bound-and-true-p org-edit-src-force-single-line)
      (goto-char (point-min))
      (while (re-search-forward "\n" nil t)
	(replace-match " "))
      (goto-char (point-min))
      (if (looking-at "\\s-*") (replace-match " "))
      (if (re-search-forward "\\s-+\\'" nil t)
	  (replace-match "")))
    (when (org-bound-and-true-p org-edit-src-from-org-mode)
      (goto-char (point-min))
      (while (re-search-forward
	      (if (org-mode-p) "^\\(.\\)" "^\\([*]\\|[ \t]*#\\+\\)") nil t)
	(replace-match ",\\1")))
    (when (org-bound-and-true-p org-edit-src-picture)
      (untabify (point-min) (point-max))
      (goto-char (point-min))
      (while (re-search-forward "^" nil t)
	(replace-match ": ")))
    (when nindent
      (setq nindent (make-string (+ org-edit-src-content-indentation nindent)
				 ?\ ))
      (goto-char (point-min))
      (while (re-search-forward "^" nil t)
      (replace-match nindent)))
    (setq code (buffer-string))
    (set-buffer-modified-p nil)
    (switch-to-buffer (marker-buffer beg))
    (kill-buffer buffer)
    (goto-char beg)
    (delete-region beg end)
    (insert code)
    (goto-char beg)
    (org-goto-line (1- (+ (org-current-line) line)))
    (move-marker beg nil)
    (move-marker end nil)))

(defun org-edit-src-save ()
  "Save parent buffer with current state source-code buffer."
  (interactive)
  (let ((p (point)) (m (mark)) msg)
    (org-edit-src-exit)
    (save-buffer)
    (setq msg (current-message))
    (org-edit-src-code)
    (push-mark m 'nomessage)
    (goto-char (min p (point-max)))
    (message (or msg ""))))

(defun org-src-mode-configure-edit-buffer ()
  (when org-edit-src-from-org-mode
    (setq buffer-offer-save t)
    (setq buffer-file-name
	  (concat (buffer-file-name (marker-buffer org-edit-src-beg-marker))
		  "[" (buffer-name) "]"))
    (set (if (featurep 'xemacs) 'write-contents-hooks 'write-contents-functions)
	 '(org-edit-src-save))
    (org-add-hook 'kill-buffer-hook
		  '(lambda () (org-delete-overlay org-edit-src-overlay)) nil 'local)))

(org-add-hook 'org-src-mode-hook 'org-src-mode-configure-edit-buffer)

(provide 'org-src)

;; arch-tag: 6a1fc84f-dec7-47be-a416-64be56bea5d8
;;; org-src.el ends here
