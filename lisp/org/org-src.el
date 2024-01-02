;;; org-src.el --- Source code examples in Org       -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2004-2024 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;;	   Bastien Guerry <bzg@gnu.org>
;;         Dan Davison <davison at stats dot ox dot ac dot uk>
;; Keywords: outlines, hypermedia, calendar, wp
;; URL: https://orgmode.org
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code dealing with source code examples in
;; Org mode.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'cl-lib)
(require 'ob-comint)
(require 'org-macs)
(require 'org-compat)
(require 'org-keys)

(declare-function org--get-expected-indentation "org" (element contentsp))
(declare-function org-mode "org" ())
(declare-function org--get-expected-indentation "org" (element contentsp))
(declare-function org-fold-region "org-fold" (from to flag &optional spec-or-alias))
(declare-function org-element-at-point "org-element" (&optional pom cached-only))
(declare-function org-element-class "org-element" (datum &optional parent))
(declare-function org-element-context "org-element" (&optional element))
(declare-function org-element-lineage "org-element"
		  (blob &optional types with-self))
(declare-function org-element--parse-paired-brackets "org-element" (char))
(declare-function org-element-property "org-element" (property element))
(declare-function org-element-type "org-element" (element))
(declare-function org-footnote-goto-definition "org-footnote"
		  (label &optional location))

(defvar org-inhibit-startup)

(defcustom org-edit-src-turn-on-auto-save nil
  "Non-nil means turn `auto-save-mode' on when editing a source block.
This will save the content of the source code editing buffer into
a newly created file, not the base buffer for this source block.

If you want to regularly save the base buffer instead of the source
code editing buffer, see `org-edit-src-auto-save-idle-delay' instead."
  :group 'org-edit-structure
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-edit-src-auto-save-idle-delay 0
  "Delay before saving a source code buffer back into its base buffer.
When a positive integer N, save after N seconds of idle time.
When 0 (the default), don't auto-save.

If you want to save the source code buffer itself, don't use this.
Check `org-edit-src-turn-on-auto-save' instead."
  :group 'org-edit-structure
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'integer)

(defcustom org-coderef-label-format "(ref:%s)"
  "The default coderef format.
This format string will be used to search for coderef labels in literal
examples (EXAMPLE and SRC blocks).  The format can be overwritten in
an individual literal example with the -l option, like

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

(defcustom org-src-preserve-indentation nil
  "If non-nil preserve leading whitespace characters on export.
\\<org-mode-map>
If non-nil leading whitespace characters in source code blocks
are preserved on export, and when switching between the org
buffer and the language mode edit buffer.

When this variable is nil, after editing with `\\[org-edit-src-code]',
the minimum (across-lines) number of leading whitespace characters
are removed from all lines, and the code block is uniformly indented
according to the value of `org-edit-src-content-indentation'."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-edit-src-content-indentation 2
  "Indentation for the content of a source code block.

This should be the number of spaces added to the indentation of the #+begin
line in order to compute the indentation of the block content after
editing it with `\\[org-edit-src-code]'.

It has no effect if `org-src-preserve-indentation' is non-nil."
  :group 'org-edit-structure
  :type 'integer
  :safe #'wholenump)

(defcustom org-edit-src-persistent-message t
  "Non-nil means show persistent exit help message while editing src examples.
The message is shown in the header-line, which will be created in the
first line of the window showing the editing buffer."
  :group 'org-edit-structure
  :type 'boolean)

(defcustom org-src-ask-before-returning-to-edit-buffer t
  "Non-nil means ask before switching to an existing edit buffer.
If nil, when `org-edit-src-code' is used on a block that already
has an active edit buffer, it will switch to that edit buffer
immediately; otherwise it will ask whether you want to return to
the existing edit buffer."
  :group 'org-edit-structure
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'boolean)

(defcustom org-src-window-setup 'reorganize-frame
  "How the source code edit buffer should be displayed.
Possible values for this option are:

plain              Show edit buffer using `display-buffer'.  Users can
                   further control the display behavior by modifying
                   `display-buffer-alist' and its relatives.
current-window     Show edit buffer in the current window, keeping all other
                   windows.
split-window-below Show edit buffer below the current window, keeping all
                   other windows.
split-window-right Show edit buffer to the right of the current window,
                   keeping all other windows.
other-window       Use `switch-to-buffer-other-window' to display edit buffer.
reorganize-frame   Show only two windows on the current frame, the current
                   window and the edit buffer.
other-frame        Use `switch-to-buffer-other-frame' to display edit buffer.
                   Also, when exiting the edit buffer, kill that frame.

Values that modify the window layout (reorganize-frame, split-window-below,
split-window-right) will restore the layout after exiting the edit buffer."
  :group 'org-edit-structure
  :type '(choice
          (const plain)
	  (const current-window)
	  (const split-window-below)
	  (const split-window-right)
	  (const other-frame)
	  (const other-window)
	  (const reorganize-frame)))

(defvar org-src-mode-hook nil
  "Hook run after Org switched a source code snippet to its Emacs mode.
\\<org-mode-map>
This hook will run:
- when editing a source code snippet with `\\[org-edit-special]'
- when formatting a source code snippet for export with htmlize.

You may want to use this hook for example to turn off `outline-minor-mode'
or similar things which you want to have when editing a source code file,
but which mess up the display of a snippet in Org exported files.")

(defcustom org-src-lang-modes
  '(("C" . c)
    ("C++" . c++)
    ("asymptote" . asy)
    ("bash" . sh)
    ("beamer" . latex)
    ("calc" . fundamental)
    ("cpp" . c++)
    ("ditaa" . artist)
    ("desktop" . conf-desktop)
    ("dot" . fundamental)
    ("elisp" . emacs-lisp)
    ("ocaml" . tuareg)
    ("screen" . shell-script)
    ("shell" . sh)
    ("sqlite" . sql)
    ("toml" . conf-toml))
  "Alist mapping languages to their major mode.

The key is the language name.  The value is the mode name, as
a string or a symbol, without the \"-mode\" suffix.

For many languages this is simple, but for language where this is
not the case, this variable provides a way to simplify things on
the user side.  For example, there is no `ocaml-mode' in Emacs,
but the mode to use is `tuareg-mode'."
  :group 'org-edit-structure
  :package-version '(Org . "9.6")
  :type '(repeat
	  (cons
	   (string "Language name")
	   (symbol "Major mode"))))

(defcustom org-src-block-faces nil
  "Alist of faces to be used for source-block.
Each element is a cell of the format

     (\"language\" FACE)

Where FACE is either a defined face or an anonymous face.

For instance, the following would color the background of
emacs-lisp source blocks and python source blocks in purple and
green, respectability.

  (setq org-src-block-faces
        \\='((\"emacs-lisp\" (:background \"#EEE2FF\"))
          (\"python\" (:background \"#e5ffb8\"))))"
  :group 'org-edit-structure
  :type '(repeat (list (string :tag "language")
                       (choice
                        (face :tag "Face")
                        (sexp :tag "Anonymous face"))))
  :version "26.1"
  :package-version '(Org . "9.0"))

(defcustom org-src-tab-acts-natively t
  "If non-nil, TAB uses the language's major-mode binding in code blocks."
  :type 'boolean
  :package-version '(Org . "9.4")
  :group 'org-babel)



;;; Internal functions and variables

(defvar org-src--auto-save-timer nil
  "Idle Timer auto-saving remote editing buffers.")

(defvar-local org-src--allow-write-back t)
(put 'org-src--allow-write-back 'permanent-local t)

(defvar-local org-src--babel-info nil)
(put 'org-src--babel-info 'permanent-local t)

(defvar-local org-src--beg-marker nil)
(put 'org-src--beg-marker 'permanent-local t)

(defvar-local org-src--block-indentation nil)
(put 'org-src--block-indentation 'permanent-local t)

(defvar-local org-src--content-indentation nil)
(put 'org-src--content-indentation 'permanent-local t)

(defvar-local org-src--end-marker nil)
(put 'org-src--end-marker 'permanent-local t)

(defvar-local org-src--from-org-mode nil)
(put 'org-src--from-org-mode 'permanent-local t)

(defvar-local org-src--overlay nil)
(put 'org-src--overlay 'permanent-local t)

(defvar-local org-src--preserve-indentation nil)
(put 'org-src--preserve-indentation 'permanent-local t)

(defvar-local org-src--remote nil)
(put 'org-src--remote 'permanent-local t)

(defvar-local org-src--saved-temp-window-config nil)
(put 'org-src--saved-temp-window-config 'permanent-local t)

(defvar-local org-src--source-type nil
  "Type of element being edited, as a symbol.")
(put 'org-src--source-type 'permanent-local t)

(defvar-local org-src--tab-width nil
  "Contains `tab-width' value from Org source buffer.
However, if `indent-tabs-mode' is nil in that buffer, its value
is 0.")
(put 'org-src--tab-width 'permanent-local t)

(defvar-local org-src-source-file-name nil
  "File name associated to Org source buffer, or nil.")
(put 'org-src-source-file-name 'permanent-local t)

(defvar-local org-src--preserve-blank-line nil)
(put 'org-src--preserve-blank-line 'permanent-local t)

(defun org-src--construct-edit-buffer-name (org-buffer-name lang)
  "Construct the buffer name for a source editing buffer.
Format is \"*Org Src ORG-BUFFER-NAME[ LANG ]*\"."
  (concat "*Org Src " org-buffer-name "[ " lang " ]*"))

(defun org-src--edit-buffer (beg end)
  "Return buffer editing area between BEG and END.
Return nil if there is no such buffer."
  (catch 'exit
    (dolist (b (buffer-list))
      (with-current-buffer b
	(and (org-src-edit-buffer-p)
	     (= beg org-src--beg-marker)
	     (eq (marker-buffer beg) (marker-buffer org-src--beg-marker))
	     (= end org-src--end-marker)
	     (eq (marker-buffer end) (marker-buffer org-src--end-marker))
	     (throw 'exit b))))))

(defun org-src--coordinates (pos beg end)
  "Return coordinates of POS relatively to BEG and END.
POS, BEG and END are buffer positions.  Return value is either
a cons cell (LINE . COLUMN) or symbol `end'.  See also
`org-src--goto-coordinates'."
  (if (>= pos end) 'end
    (org-with-wide-buffer
     (goto-char (max beg pos))
     (cons (count-lines (save-excursion (goto-char beg) (line-beginning-position))
                        (line-beginning-position))
	   ;; Column is relative to the end of line to avoid problems of
	   ;; comma escaping or colons appended in front of the line.
	   (- (point) (min end (line-end-position)))))))

(defun org-src--goto-coordinates (coord beg end)
  "Move to coordinates COORD relatively to BEG and END.
COORD are coordinates, as returned by `org-src--coordinates',
which see.  BEG and END are buffer positions."
  (goto-char
   (if (eq coord 'end) (max (1- end) beg)
     ;; If BEG happens to be located outside of the narrowed part of
     ;; the buffer, widen it first.
     (org-with-wide-buffer
      (goto-char beg)
      (forward-line (car coord))
      (max (point)
           (+ (min end (line-end-position))
              (cdr coord)))))))

(defun org-src--contents-area (datum)
  "Return contents boundaries of DATUM.
DATUM is an element or object.  Return a list (BEG END CONTENTS)
where BEG and END are buffer positions and CONTENTS is a string."
  (let ((type (org-element-type datum)))
    (org-with-wide-buffer
     (cond
      ((eq type 'footnote-definition)
       (let* ((beg (progn
		     (goto-char (org-element-property :post-affiliated datum))
		     (search-forward "]")))
	      (end (or (org-element-property :contents-end datum) beg)))
	 (list beg end (buffer-substring-no-properties beg end))))
      ((eq type 'inline-src-block)
       (let ((beg (progn (goto-char (org-element-property :begin datum))
			 (search-forward "{" (line-end-position) t)))
	     (end (progn (goto-char (org-element-property :end datum))
			 (search-backward "}" (line-beginning-position) t))))
	 (list beg end (buffer-substring-no-properties beg end))))
      ((eq type 'latex-fragment)
       (let ((beg (org-element-property :begin datum))
	     (end (org-with-point-at (org-element-property :end datum)
		    (skip-chars-backward " \t")
		    (point))))
	 (list beg end (buffer-substring-no-properties beg end))))
      ((org-element-property :contents-begin datum)
       (let ((beg (org-element-property :contents-begin datum))
	     (end (org-element-property :contents-end datum)))
	 (list beg end (buffer-substring-no-properties beg end))))
      ((memq type '(example-block export-block src-block comment-block))
       (list (progn (goto-char (org-element-property :post-affiliated datum))
		    (line-beginning-position 2))
	     (progn (goto-char (org-element-property :end datum))
		    (skip-chars-backward " \r\t\n")
		    (line-beginning-position 1))
	     (org-element-property :value datum)))
      ((memq type '(fixed-width latex-environment table))
       (let ((beg (org-element-property :post-affiliated datum))
	     (end (progn (goto-char (org-element-property :end datum))
			 (skip-chars-backward " \r\t\n")
			 (line-beginning-position 2))))
	 (list beg
	       end
	       (if (eq type 'fixed-width) (org-element-property :value datum)
		 (buffer-substring-no-properties beg end)))))
      (t (error "Unsupported element or object: %s" type))))))

(defun org-src--make-source-overlay (beg end edit-buffer)
  "Create overlay between BEG and END positions and return it.
EDIT-BUFFER is the buffer currently editing area between BEG and
END."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'secondary-selection)
    (overlay-put overlay 'edit-buffer edit-buffer)
    (overlay-put overlay 'help-echo
		 "Click with mouse-1 to switch to buffer editing this segment")
    (overlay-put overlay 'face 'secondary-selection)
    (overlay-put overlay 'keymap
		 (let ((map (make-sparse-keymap)))
		   (define-key map [mouse-1] 'org-edit-src-continue)
		   map))
    (let ((read-only
	   (list
	    (lambda (&rest _)
	      (user-error
	       "Cannot modify an area being edited in a dedicated buffer")))))
      (overlay-put overlay 'modification-hooks read-only)
      (overlay-put overlay 'insert-in-front-hooks read-only)
      (overlay-put overlay 'insert-behind-hooks read-only))
    overlay))

(defun org-src--remove-overlay ()
  "Remove overlay from current source buffer."
  (when (overlayp org-src--overlay) (delete-overlay org-src--overlay)))

(defun org-src--on-datum-p (datum)
  "Non-nil when point is on DATUM.
DATUM is an element or an object.  Consider blank lines or white
spaces after it as being outside."
  (and (>= (point) (org-element-property :begin datum))
       (<= (point)
	   (org-with-wide-buffer
	    (goto-char (org-element-property :end datum))
	    (skip-chars-backward " \r\t\n")
	    (if (eq (org-element-class datum) 'element)
		(line-end-position)
	      (point))))))

(defun org-src--contents-for-write-back (write-back-buf)
  "Populate WRITE-BACK-BUF with contents in the appropriate format.
Assume point is in the corresponding edit buffer."
  (let ((indentation-offset
	 (if org-src--preserve-indentation 0
	   (+ (or org-src--block-indentation 0)
	      (if (memq org-src--source-type '(example-block src-block))
		  org-src--content-indentation
		0))))
	(use-tabs? (and (> org-src--tab-width 0) t))
        (preserve-fl (eq org-src--source-type 'latex-fragment))
	(source-tab-width org-src--tab-width)
	(contents (org-with-wide-buffer
                   (let ((eol (line-end-position)))
                     (list (buffer-substring (point-min) eol)
                           (buffer-substring eol (point-max))))))
	(write-back org-src--allow-write-back)
        (preserve-blank-line org-src--preserve-blank-line)
        marker)
    (with-current-buffer write-back-buf
      ;; Reproduce indentation parameters from source buffer.
      (setq indent-tabs-mode use-tabs?)
      (when (> source-tab-width 0) (setq tab-width source-tab-width))
      ;; Apply WRITE-BACK function on edit buffer contents.
      (insert (org-no-properties (car contents)))
      (setq marker (point-marker))
      (insert (org-no-properties (car (cdr contents))))
      (goto-char (point-min))
      (when (functionp write-back) (save-excursion (funcall write-back)))
      ;; Add INDENTATION-OFFSET to every line in buffer,
      ;; unless indentation is meant to be preserved.
      (when (> indentation-offset 0)
	(when preserve-fl (forward-line))
        (while (not (eobp))
	  (skip-chars-forward " \t")
          (when (or (not (eolp))                               ; not a blank line
                    (and (eq (point) (marker-position marker)) ; current line
                         preserve-blank-line))
	    (let ((i (current-column)))
	      (delete-region (line-beginning-position) (point))
	      (indent-to (+ i indentation-offset))))
	  (forward-line)))
      (set-marker marker nil))))

(defun org-src--edit-element
    (datum name &optional initialize write-back contents remote)
  "Edit DATUM contents in a dedicated buffer NAME.

INITIALIZE is a function to call upon creating the buffer.

When WRITE-BACK is non-nil, assume contents will replace original
region.  Moreover, if it is a function, apply it in the edit
buffer, from point min, before returning the contents.

When CONTENTS is non-nil, display them in the edit buffer.
Otherwise, show DATUM contents as specified by
`org-src--contents-area'.

When REMOTE is non-nil, do not try to preserve point or mark when
moving from the edit area to the source.

Leave point in edit buffer."
  (when (memq org-src-window-setup '(reorganize-frame
				     split-window-below
				     split-window-right))
    (setq org-src--saved-temp-window-config (current-window-configuration)))
  (let* ((area (org-src--contents-area datum))
	 (beg (copy-marker (nth 0 area)))
	 (end (copy-marker (nth 1 area) t))
	 (old-edit-buffer (org-src--edit-buffer beg end))
	 (contents (or contents (nth 2 area))))
    (if (and old-edit-buffer
	     (or (not org-src-ask-before-returning-to-edit-buffer)
		 (y-or-n-p "Return to existing edit buffer ([n] will revert changes)? ")))
	;; Move to existing buffer.
	(org-src-switch-to-buffer old-edit-buffer 'return)
      ;; Discard old edit buffer.
      (when old-edit-buffer
	(with-current-buffer old-edit-buffer (org-src--remove-overlay))
	(kill-buffer old-edit-buffer))
      (let* ((org-mode-p (derived-mode-p 'org-mode))
	     (source-file-name (buffer-file-name (buffer-base-buffer)))
	     (source-tab-width (if indent-tabs-mode tab-width 0))
	     (type (org-element-type datum))
	     (block-ind (org-with-point-at (org-element-property :begin datum)
                          (cond
                           ((save-excursion (skip-chars-backward " \t") (bolp))
			    (org-current-text-indentation))
                           ((org-element-property :parent datum)
                            (org--get-expected-indentation
                             (org-element-property :parent datum) nil))
                           (t (org-current-text-indentation)))))
	     (content-ind org-edit-src-content-indentation)
             (blank-line (save-excursion (beginning-of-line)
                                         (looking-at-p "^[[:space:]]*$")))
             (empty-line (and blank-line (looking-at-p "^$")))
             (preserve-blank-line (or (and blank-line (not empty-line))
                                      (and empty-line (= (+ block-ind content-ind) 0))))
	     (preserve-ind
	      (and (memq type '(example-block src-block))
		   (or (org-element-property :preserve-indent datum)
		       org-src-preserve-indentation)))
	     ;; Store relative positions of mark (if any) and point
	     ;; within the edited area.
	     (point-coordinates (and (not remote)
				     (org-src--coordinates (point) beg end)))
	     (mark-coordinates (and (not remote)
				    (org-region-active-p)
				    (let ((m (mark)))
				      (and (>= m beg) (>= end m)
					   (org-src--coordinates m beg end)))))
	     ;; Generate a new edit buffer.
	     (buffer (generate-new-buffer name))
	     ;; Add an overlay on top of source.
	     (overlay (org-src--make-source-overlay beg end buffer)))
	;; Switch to edit buffer.
	(org-src-switch-to-buffer buffer 'edit)
	;; Insert contents.
	(insert contents)
	(remove-text-properties (point-min) (point-max)
				'(display nil invisible nil intangible nil))
	(let ((lf (eq type 'latex-fragment)))
          (unless preserve-ind (org-do-remove-indentation (and lf block-ind) lf)))
	(set-buffer-modified-p nil)
	(setq buffer-file-name nil)
	;; Initialize buffer.
	(when (functionp initialize)
	  (let ((org-inhibit-startup t))
	    (condition-case e
		(funcall initialize)
	      (error (message "Initialization fails with: %S"
			      (error-message-string e))))))
	;; Transmit buffer-local variables for exit function.  It must
	;; be done after initializing major mode, as this operation
	;; may reset them otherwise.
	(setq org-src--tab-width source-tab-width)
	(setq org-src--from-org-mode org-mode-p)
	(setq org-src--beg-marker beg)
	(setq org-src--end-marker end)
	(setq org-src--remote remote)
	(setq org-src--source-type type)
	(setq org-src--block-indentation block-ind)
	(setq org-src--content-indentation content-ind)
	(setq org-src--preserve-indentation preserve-ind)
	(setq org-src--overlay overlay)
	(setq org-src--allow-write-back write-back)
	(setq org-src-source-file-name source-file-name)
        (setq org-src--preserve-blank-line preserve-blank-line)
	;; Start minor mode.
	(org-src-mode)
	;; Clear undo information so we cannot undo back to the
	;; initial empty buffer.
	(buffer-disable-undo (current-buffer))
	(buffer-enable-undo)
	;; Move mark and point in edit buffer to the corresponding
	;; location.
	(if remote
	    (progn
	      ;; Put point at first non read-only character after
	      ;; leading blank.
	      (goto-char
	       (or (text-property-any (point-min) (point-max) 'read-only nil)
		   (point-max)))
	      (skip-chars-forward " \r\t\n"))
	  ;; Set mark and point.
	  (when mark-coordinates
	    (org-src--goto-coordinates mark-coordinates (point-min) (point-max))
	    (push-mark (point) 'no-message t)
	    (setq deactivate-mark nil))
	  (org-src--goto-coordinates
	   point-coordinates (point-min) (point-max)))))))



;;; Fontification of source blocks

(defvar org-src-fontify-natively) ; Defined in org.el
(defun org-src-font-lock-fontify-block (lang start end)
  "Fontify code block between START and END using LANG's syntax.
This function is called by Emacs' automatic fontification, as long
as `org-src-fontify-natively' is non-nil."
  (let ((modified (buffer-modified-p)))
    (remove-text-properties start end '(face nil))
    (let ((lang-mode (org-src-get-lang-mode lang)))
      (when (fboundp lang-mode)
        (let ((string (buffer-substring-no-properties start end))
	      (org-buffer (current-buffer)))
	  (with-current-buffer
	      (get-buffer-create
	       (format " *org-src-fontification:%s*" lang-mode))
	    (let ((inhibit-modification-hooks nil))
	      (erase-buffer)
	      ;; Add string and a final space to ensure property change.
	      (insert string " "))
	    (unless (eq major-mode lang-mode) (funcall lang-mode))
            (font-lock-ensure)
	    (let ((pos (point-min)) next)
	      (while (setq next (next-property-change pos))
	        ;; Handle additional properties from font-lock, so as to
	        ;; preserve, e.g., composition.
                ;; FIXME: We copy 'font-lock-face property explicitly because
                ;; `font-lock-mode' is not enabled in the buffers starting from
                ;; space and the remapping between 'font-lock-face and 'face
                ;; text properties may thus not be set.  See commit
                ;; 453d634bc.
	        (dolist (prop (append '(font-lock-face face) font-lock-extra-managed-props))
		  (let ((new-prop (get-text-property pos prop)))
                    (when new-prop
                      (if (not (eq prop 'invisible))
		          (put-text-property
		           (+ start (1- pos)) (1- (+ start next)) prop new-prop
		           org-buffer)
                        ;; Special case.  `invisible' text property may
                        ;; clash with Org folding.  Do not assign
                        ;; `invisible' text property directly.  Use
                        ;; property alias instead.
                        (let ((invisibility-spec
                               (or
                                ;; ATOM spec.
                                (and (memq new-prop buffer-invisibility-spec)
                                     new-prop)
                                ;; (ATOM . ELLIPSIS) spec.
                                (assq new-prop buffer-invisibility-spec))))
                          (with-current-buffer org-buffer
                            ;; Add new property alias.
                            (unless (memq 'org-src-invisible
                                          (cdr (assq 'invisible char-property-alias-alist)))
                              (setq-local
                               char-property-alias-alist
                               (cons (cons 'invisible
			                   (nconc (cdr (assq 'invisible char-property-alias-alist))
                                                  '(org-src-invisible)))
		                     (remove (assq 'invisible char-property-alias-alist)
			                     char-property-alias-alist))))
                            ;; Carry over the invisibility spec, unless
                            ;; already present.  Note that there might
                            ;; be conflicting invisibility specs from
                            ;; different major modes.  We cannot do much
                            ;; about this then.
                            (when invisibility-spec
                              (add-to-invisibility-spec invisibility-spec))
                            (put-text-property
		             (+ start (1- pos)) (1- (+ start next))
                             'org-src-invisible new-prop
		             org-buffer)))))))
	        (setq pos next)))
            (set-buffer-modified-p nil)))))
    ;; Add Org faces.
    (let ((src-face (nth 1 (assoc-string lang org-src-block-faces t))))
      (when (or (facep src-face) (listp src-face))
        (font-lock-append-text-property start end 'face src-face))
      (font-lock-append-text-property start end 'face 'org-block))
    ;; Clear abbreviated link folding.
    (org-fold-region start end nil 'org-link)
    (add-text-properties
     start end
     '(font-lock-fontified t fontified t font-lock-multiline t))
    (set-buffer-modified-p modified)))

(defun org-fontify-inline-src-blocks (limit)
  "Try to apply `org-fontify-inline-src-blocks-1'."
  (condition-case nil
      (org-fontify-inline-src-blocks-1 limit)
    (error (message "Org mode fontification error in %S at %d"
                    (current-buffer)
                    (line-number-at-pos)))))

(defun org-fontify-inline-src-blocks-1 (limit)
  "Fontify inline src_LANG blocks, from `point' up to LIMIT."
  (let ((case-fold-search t))
    ;; The regexp below is copied from `org-element-inline-src-block-parser'.
    (while (re-search-forward "\\_<src_\\([^ \t\n[{]+\\)[{[]?" limit t)
      (let ((beg (match-beginning 0))
            (lang-beg (match-beginning 1))
            (lang-end (match-end 1))
            pt)
        (font-lock-append-text-property
         lang-beg lang-end 'face 'org-meta-line)
        (font-lock-append-text-property
         beg lang-beg 'face 'shadow)
        (font-lock-append-text-property
         beg lang-end 'face 'org-inline-src-block)
        (setq pt (goto-char lang-end))
        ;; `org-element--parse-paired-brackets' doesn't take a limit, so to
        ;; prevent it searching the entire rest of the buffer we temporarily
        ;; narrow the active region.
        (save-restriction
          (narrow-to-region beg
                            (min limit (or (save-excursion
                                             (and (search-forward"\n" limit t 2)
                                                  (point)))
                                           (point-max))))
          (when (ignore-errors (org-element--parse-paired-brackets ?\[))
            (font-lock-append-text-property
             pt (point) 'face 'org-inline-src-block)
            (setq pt (point)))
          (when (ignore-errors (org-element--parse-paired-brackets ?\{))
            (remove-text-properties pt (point) '(face nil))
            (font-lock-append-text-property
             pt (1+ pt) 'face '(org-inline-src-block shadow))
            (unless (= (1+ pt) (1- (point)))
              (if org-src-fontify-natively
                  (org-src-font-lock-fontify-block
                   (buffer-substring-no-properties lang-beg lang-end)
                   (1+ pt) (1- (point)))
                (font-lock-append-text-property
                 (1+ pt) (1- (point)) 'face 'org-inline-src-block)))
            (font-lock-append-text-property
             (1- (point)) (point) 'face '(org-inline-src-block shadow))
            (setq pt (point)))))
      t)))


;;; Escape contents

(defun org-escape-code-in-region (beg end)
  "Escape lines between BEG and END.
Escaping happens when a line starts with \"*\", \"#+\", \",*\" or
\",#+\" by appending a comma to it."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (while (re-search-backward "^[ \t]*\\(,*\\(?:\\*\\|#\\+\\)\\)" beg t)
      (save-excursion (replace-match ",\\1" nil nil nil 1)))))

(defun org-escape-code-in-string (s)
  "Escape lines in string S.
Escaping happens when a line starts with \"*\", \"#+\", \",*\" or
\",#+\" by appending a comma to it."
  (replace-regexp-in-string "^[ \t]*\\(,*\\(?:\\*\\|#\\+\\)\\)" ",\\1"
			    s nil nil 1))

(defun org-unescape-code-in-region (beg end)
  "Un-escape lines between BEG and END.
Un-escaping happens by removing the first comma on lines starting
with \",*\", \",#+\", \",,*\" and \",,#+\"."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (while (re-search-backward "^[ \t]*,*\\(,\\)\\(?:\\*\\|#\\+\\)" beg t)
      (save-excursion (replace-match "" nil nil nil 1)))))

(defun org-unescape-code-in-string (s)
  "Un-escape lines in string S.
Un-escaping happens by removing the first comma on lines starting
with \",*\", \",#+\", \",,*\" and \",,#+\"."
  (replace-regexp-in-string
   "^[ \t]*,*\\(,\\)\\(?:\\*\\|#\\+\\)" "" s nil nil 1))



;;; Org src minor mode

(defvar org-src-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c'" 'org-edit-src-exit)
    (define-key map "\C-c\C-k" 'org-edit-src-abort)
    (define-key map "\C-x\C-s" 'org-edit-src-save)
    map))

(define-minor-mode org-src-mode
  "Minor mode for language major mode buffers generated by Org.
\\<org-mode-map>
This minor mode is turned on in two situations:
  - when editing a source code snippet with `\\[org-edit-special]'
  - when formatting a source code snippet for export with htmlize.

\\{org-src-mode-map}

See also `org-src-mode-hook'."
  :lighter " OrgSrc"
  (when org-edit-src-persistent-message
    (setq header-line-format
	  (substitute-command-keys
	   (if org-src--allow-write-back
	       "Edit, then exit with `\\[org-edit-src-exit]' or abort with \
`\\[org-edit-src-abort]'"
	     "Exit with `\\[org-edit-src-exit]' or abort with \
`\\[org-edit-src-abort]'"))))
  ;; Possibly activate various auto-save features (for the edit buffer
  ;; or the source buffer).
  (when org-edit-src-turn-on-auto-save
    (setq buffer-auto-save-file-name
	  (concat (make-temp-name "org-src-")
		  (format-time-string "-%Y-%d-%m")
		  ".txt")))
  (unless (or org-src--auto-save-timer
	      (= 0 org-edit-src-auto-save-idle-delay))
    (setq org-src--auto-save-timer
	  (run-with-idle-timer
	   org-edit-src-auto-save-idle-delay t
	   (lambda ()
	     (save-excursion
	       (let (edit-flag)
		 (dolist (b (buffer-list))
		   (with-current-buffer b
		     (when (org-src-edit-buffer-p)
		       (unless edit-flag (setq edit-flag t))
		       (when (buffer-modified-p) (org-edit-src-save)))))
		 (unless edit-flag
		   (cancel-timer org-src--auto-save-timer)
		   (setq org-src--auto-save-timer nil)))))))))

(defun org-src-mode-configure-edit-buffer ()
  "Configure the src edit buffer."
  (when (bound-and-true-p org-src--from-org-mode)
    (add-hook 'kill-buffer-hook #'org-src--remove-overlay nil 'local)
    (if (bound-and-true-p org-src--allow-write-back)
	(progn
	  (setq buffer-offer-save t)
	  (setq write-contents-functions '(org-edit-src-save)))
      (setq buffer-read-only t))))

(add-hook 'org-src-mode-hook #'org-src-mode-configure-edit-buffer)



;;; Babel related functions

(defun org-src-associate-babel-session (info)
  "Associate edit buffer with comint session.
INFO should be a list similar in format to the return value of
`org-babel-get-src-block-info'."
  (interactive)
  (let ((session (cdr (assq :session (nth 2 info)))))
    (and session (not (string= session "none"))
	 (org-babel-comint-buffer-livep session)
	 (let ((f (intern (format "org-babel-%s-associate-session"
                                  (nth 0 info)))))
           (and (fboundp f) (funcall f session))))))

(defun org-src-babel-configure-edit-buffer ()
  "Configure src editing buffer."
  (when org-src--babel-info
    (org-src-associate-babel-session org-src--babel-info)))

(add-hook 'org-src-mode-hook #'org-src-babel-configure-edit-buffer)


;;; Public API

(defmacro org-src-do-at-code-block (&rest body)
  "Execute BODY from an edit buffer in the Org mode buffer."
  (declare (debug (body)))
  `(let ((beg-marker org-src--beg-marker))
     (when beg-marker
       (with-current-buffer (marker-buffer beg-marker)
	 (goto-char beg-marker)
	 ,@body))))

(defun org-src-do-key-sequence-at-code-block (&optional key)
  "Execute key sequence at code block in the source Org buffer.
The command bound to KEY in the Org-babel key map is executed
remotely with point temporarily at the start of the code block in
the Org buffer.

This command is not bound to a key by default, to avoid conflicts
with language major mode bindings.  To bind it to C-c @ in all
language major modes, you could use

  (add-hook \\='org-src-mode-hook
            (lambda () (define-key org-src-mode-map \"\\C-c@\"
                    \\='org-src-do-key-sequence-at-code-block)))

In that case, for example, C-c @ t issued in code edit buffers
would tangle the current Org code block, C-c @ e would execute
the block and C-c @ h would display the other available
Org-babel commands."
  (interactive "kOrg-babel key: ")
  (if (equal key (kbd "C-g")) (keyboard-quit)
    (org-edit-src-save)
    (org-src-do-at-code-block
     (call-interactively (lookup-key org-babel-map key)))))

(defun org-src-get-lang-mode (lang)
  "Return major mode that should be used for LANG.
LANG is a string, and the returned major mode is a symbol."
  (intern
   (concat
    (let ((l (or (cdr (assoc lang org-src-lang-modes)) lang)))
      (if (symbolp l) (symbol-name l) l))
    "-mode")))

(defun org-src-edit-buffer-p (&optional buffer)
  "Non-nil when current buffer is a source editing buffer.
If BUFFER is non-nil, test it instead."
  (let ((buffer (org-base-buffer (or buffer (current-buffer)))))
    (and (buffer-live-p buffer)
	 (local-variable-p 'org-src--beg-marker buffer)
	 (local-variable-p 'org-src--end-marker buffer))))

(defun org-src-source-buffer ()
  "Return source buffer edited in current buffer.
Raise an error when current buffer is not a source editing buffer."
  (unless (org-src-edit-buffer-p) (error "Not in a source buffer"))
  (or (marker-buffer org-src--beg-marker)
      (error "No source buffer available for current editing session")))

(defun org-src-source-type ()
  "Return type of element edited in current buffer.
Raise an error when current buffer is not a source editing buffer."
  (unless (org-src-edit-buffer-p) (error "Not in a source buffer"))
  org-src--source-type)

(defun org-src-switch-to-buffer (buffer context)
  "Switch to BUFFER considering CONTEXT and `org-src-window-setup'."
  (pcase org-src-window-setup
    (`plain
     (when (eq context 'exit) (quit-restore-window))
     (pop-to-buffer buffer))
    (`current-window (pop-to-buffer-same-window buffer))
    (`other-window
     (let ((cur-win (selected-window)))
       (org-switch-to-buffer-other-window buffer)
       (when (eq context 'exit) (quit-restore-window cur-win))))
    (`split-window-below
     (if (eq context 'exit)
	 (delete-window)
       (select-window (split-window-vertically)))
     (pop-to-buffer-same-window buffer))
    (`split-window-right
     (if (eq context 'exit)
	 (delete-window)
       (select-window (split-window-horizontally)))
     (pop-to-buffer-same-window buffer))
    (`other-frame
     (pcase context
       (`exit
	(let ((frame (selected-frame)))
	  (switch-to-buffer-other-frame buffer)
	  (delete-frame frame)))
       (`save
	(kill-buffer (current-buffer))
	(pop-to-buffer-same-window buffer))
       (_ (switch-to-buffer-other-frame buffer))))
    (`reorganize-frame
     (when (eq context 'edit) (delete-other-windows))
     (org-switch-to-buffer-other-window buffer)
     (when (eq context 'exit) (delete-other-windows)))
    (`switch-invisibly (set-buffer buffer))
    (_
     (message "Invalid value %s for `org-src-window-setup'"
	      org-src-window-setup)
     (pop-to-buffer-same-window buffer))))

(defun org-src-coderef-format (&optional element)
  "Return format string for block at point.

When optional argument ELEMENT is provided, use that block.
Otherwise, assume point is either at a source block, at an
example block.

If point is in an edit buffer, retrieve format string associated
to the remote source block."
  (cond
   ((and element (org-element-property :label-fmt element)))
   ((org-src-edit-buffer-p) (org-src-do-at-code-block (org-src-coderef-format)))
   ((org-element-property :label-fmt (org-element-at-point)))
   (t org-coderef-label-format)))

(defun org-src-coderef-regexp (fmt &optional label)
  "Return regexp matching a coderef format string FMT.

When optional argument LABEL is non-nil, match coderef for that
label only.

Match group 1 contains the full coderef string with surrounding
white spaces.  Match group 2 contains the same string without any
surrounding space.  Match group 3 contains the label.

A coderef format regexp can only match at the end of a line."
  (format "\\([ \t]*\\(%s\\)[ \t]*\\)$"
	  (replace-regexp-in-string
	   "%s"
	   (if label (regexp-quote label) "\\([-a-zA-Z0-9_][-a-zA-Z0-9_ ]*\\)")
	   (regexp-quote fmt)
	   nil t)))

(defun org-edit-footnote-reference ()
  "Edit definition of footnote reference at point."
  (interactive)
  (let* ((context (org-element-context))
	 (label (org-element-property :label context)))
    (unless (and (eq (org-element-type context) 'footnote-reference)
		 (org-src--on-datum-p context))
      (user-error "Not on a footnote reference"))
    (unless label (user-error "Cannot edit remotely anonymous footnotes"))
    (let* ((definition (org-with-wide-buffer
			(org-footnote-goto-definition label)
			(backward-char)
			(org-element-context)))
	   (inline? (eq 'footnote-reference (org-element-type definition)))
	   (contents
	    (org-with-wide-buffer
	     (buffer-substring-no-properties
	      (or (org-element-property :post-affiliated definition)
		  (org-element-property :begin definition))
	      (cond
	       (inline? (1+ (org-element-property :contents-end definition)))
	       ((org-element-property :contents-end definition))
	       (t (goto-char (org-element-property :post-affiliated definition))
		  (line-end-position)))))))
      (add-text-properties
       0
       (progn (string-match (if inline? "\\`\\[fn:.*?:" "\\`.*?\\]") contents)
	      (match-end 0))
       '(read-only "Cannot edit footnote label" front-sticky t rear-nonsticky t)
       contents)
      (when inline?
	(let ((l (length contents)))
	  (add-text-properties
	   (1- l) l
	   '(read-only "Cannot edit past footnote reference"
		       front-sticky nil rear-nonsticky nil)
	   contents)))
      (org-src--edit-element
       definition
       (format "*Edit footnote [%s]*" label)
       (let ((source (current-buffer)))
	 (lambda ()
	   (org-mode)
	   (org-clone-local-variables source)))
       (lambda ()
	 (if (not inline?) (delete-region (point) (search-forward "]"))
	   (delete-region (point) (search-forward ":" nil t 2))
	   (delete-region (1- (point-max)) (point-max))
	   (when (re-search-forward "\n[ \t]*\n" nil t)
	     (user-error "Inline definitions cannot contain blank lines"))
	   ;; If footnote reference belongs to a table, make sure to
	   ;; remove any newline characters in order to preserve
	   ;; table's structure.
	   (when (org-element-lineage definition '(table-cell))
	     (while (search-forward "\n" nil t) (replace-match " ")))))
       contents
       'remote))
    ;; Report success.
    t))

(defun org-edit-table.el ()
  "Edit \"table.el\" table at point.
\\<org-src-mode-map>
A new buffer is created and the table is copied into it.  Then
the table is recognized with `table-recognize'.  When done
editing, exit with `\\[org-edit-src-exit]'.  The edited text will \
then replace
the area in the Org mode buffer.

Throw an error when not at such a table."
  (interactive)
  (let ((element (org-element-at-point)))
    (unless (and (eq (org-element-type element) 'table)
		 (eq (org-element-property :type element) 'table.el)
		 (org-src--on-datum-p element))
      (user-error "Not in a table.el table"))
    (org-src--edit-element
     element
     (org-src--construct-edit-buffer-name (buffer-name) "Table")
     #'text-mode t)
    (when (bound-and-true-p flyspell-mode) (flyspell-mode -1))
    (table-recognize)
    t))

(defun org-edit-latex-fragment ()
  "Edit LaTeX fragment at point."
  (interactive)
  (let ((context (org-element-context)))
    (unless (and (eq 'latex-fragment (org-element-type context))
		 (org-src--on-datum-p context))
      (user-error "Not on a LaTeX fragment"))
    (let* ((contents
	    (buffer-substring-no-properties
	     (org-element-property :begin context)
	     (- (org-element-property :end context)
		(org-element-property :post-blank context))))
	   (delim-length (if (string-match "\\`\\$[^$]" contents) 1 2)))
      ;; Make the LaTeX deliminators read-only.
      (add-text-properties 0 delim-length
			   (list 'read-only "Cannot edit LaTeX deliminator"
				 'front-sticky t
				 'rear-nonsticky t)
			   contents)
      (let ((l (length contents)))
	(add-text-properties (- l delim-length) l
			     (list 'read-only "Cannot edit LaTeX deliminator"
				   'front-sticky nil
				   'rear-nonsticky nil)
			     contents))
      (org-src--edit-element
       context
       (org-src--construct-edit-buffer-name (buffer-name) "LaTeX fragment")
       (org-src-get-lang-mode "latex")
       (lambda ()
	 ;; Blank lines break things, replace with a single newline.
	 (while (re-search-forward "\n[ \t]*\n" nil t) (replace-match "\n"))
	 ;; If within a table a newline would disrupt the structure,
	 ;; so remove newlines.
	 (goto-char (point-min))
	 (when (org-element-lineage context '(table-cell))
	   (while (search-forward "\n" nil t) (replace-match " "))))
       contents))
    t))

(defun org-edit-latex-environment ()
  "Edit LaTeX environment at point.
\\<org-src-mode-map>
The LaTeX environment is copied into a new buffer.  Major mode is
set to the one associated to \"latex\" in `org-src-lang-modes',
or to `latex-mode' if there is none.

When done, exit with `\\[org-edit-src-exit]'.  The edited text \
will then replace
the LaTeX environment in the Org mode buffer."
  (interactive)
  (let ((element (org-element-at-point)))
    (unless (and (eq (org-element-type element) 'latex-environment)
		 (org-src--on-datum-p element))
      (user-error "Not in a LaTeX environment"))
    (org-src--edit-element
     element
     (org-src--construct-edit-buffer-name (buffer-name) "LaTeX environment")
     (org-src-get-lang-mode "latex")
     t)
    t))

(defun org-edit-export-block ()
  "Edit export block at point.
\\<org-src-mode-map>
A new buffer is created and the block is copied into it, and the
buffer is switched into an appropriate major mode.  See also
`org-src-lang-modes'.

When done, exit with `\\[org-edit-src-exit]'.  The edited text \
will then replace
the area in the Org mode buffer.

Throw an error when not at an export block."
  (interactive)
  (let ((element (org-element-at-point)))
    (unless (and (eq (org-element-type element) 'export-block)
		 (org-src--on-datum-p element))
      (user-error "Not in an export block"))
    (let* ((type (downcase (or (org-element-property :type element)
			       ;; Missing export-block type.  Fallback
			       ;; to default mode.
			       "fundamental")))
	   (mode (org-src-get-lang-mode type)))
      (unless (functionp mode) (error "No such language mode: %s" mode))
      (org-src--edit-element
       element
       (org-src--construct-edit-buffer-name (buffer-name) type)
       mode
       (lambda () (org-escape-code-in-region (point-min) (point-max)))))
    t))

(defun org-edit-comment-block ()
  "Edit comment block at point.
\\<org-src-mode-map>
A new buffer is created and the block is copied into it, and the
buffer is switched into Org mode.

When done, exit with `\\[org-edit-src-exit]'.  The edited text will
then replace the area in the Org mode buffer.

Throw an error when not at a comment block."
  (interactive)
  (let ((element (org-element-at-point)))
    (unless (and (eq (org-element-type element) 'comment-block)
		 (org-src--on-datum-p element))
      (user-error "Not in a comment block"))
    (org-src--edit-element
     element
     (org-src--construct-edit-buffer-name (buffer-name) "org")
     'org-mode
     (lambda () (org-escape-code-in-region (point-min) (point-max)))
     (org-unescape-code-in-string (org-element-property :value element)))
    t))

(defun org-edit-src-code (&optional code edit-buffer-name)
  "Edit the source or example block at point.
\\<org-src-mode-map>
The code is copied to a separate buffer and the appropriate mode
is turned on.  When done, exit with `\\[org-edit-src-exit]'.  This \
will remove the
original code in the Org buffer, and replace it with the edited
version.  See `org-src-window-setup' to configure the display of
windows containing the Org buffer and the code buffer.

When optional argument CODE is a string, edit it in a dedicated
buffer instead.

When optional argument EDIT-BUFFER-NAME is non-nil, use it as the
name of the sub-editing buffer."
  (interactive)
  (let* ((element (org-element-at-point))
	 (type (org-element-type element)))
    (unless (and (memq type '(example-block src-block))
		 (org-src--on-datum-p element))
      (user-error "Not in a source or example block"))
    (let* ((lang
	    (if (eq type 'src-block) (org-element-property :language element)
	      "example"))
	   (lang-f (and (eq type 'src-block) (org-src-get-lang-mode lang)))
	   (babel-info (and (eq type 'src-block)
			    (org-babel-get-src-block-info 'no-eval)))
	   deactivate-mark)
      (when (and (eq type 'src-block) (not (functionp lang-f)))
	(error "No such language mode: %s" lang-f))
      (org-src--edit-element
       element
       (or edit-buffer-name
	   (org-src--construct-edit-buffer-name (buffer-name) lang))
       lang-f
       (and (null code)
	    (lambda () (org-escape-code-in-region (point-min) (point-max))))
       (and code (org-unescape-code-in-string code)))
      ;; Finalize buffer.
      (setq-local org-coderef-label-format
		  (or (org-element-property :label-fmt element)
		      org-coderef-label-format))
      (when (eq type 'src-block)
	(setq org-src--babel-info babel-info)
	(let ((edit-prep-func (intern (concat "org-babel-edit-prep:" lang))))
	  (when (fboundp edit-prep-func)
	    (funcall edit-prep-func babel-info))))
      t)))

(defun org-edit-inline-src-code ()
  "Edit inline source code at point."
  (interactive)
  (let ((context (org-element-context)))
    (unless (and (eq (org-element-type context) 'inline-src-block)
		 (org-src--on-datum-p context))
      (user-error "Not on inline source code"))
    (let* ((lang (org-element-property :language context))
	   (lang-f (org-src-get-lang-mode lang))
	   (babel-info (org-babel-get-src-block-info 'no-eval))
	   deactivate-mark)
      (unless (functionp lang-f) (error "No such language mode: %s" lang-f))
      (org-src--edit-element
       context
       (org-src--construct-edit-buffer-name (buffer-name) lang)
       lang-f
       (lambda ()
	 ;; Inline source blocks are limited to one line.
	 (while (re-search-forward "\n[ \t]*" nil t) (replace-match " "))
	 ;; Trim contents.
	 (goto-char (point-min))
	 (skip-chars-forward " \t")
	 (delete-region (point-min) (point))
	 (goto-char (point-max))
	 (skip-chars-backward " \t")
	 (delete-region (point) (point-max))))
      ;; Finalize buffer.
      (setq org-src--babel-info babel-info)
      (setq org-src--preserve-indentation t)
      (let ((edit-prep-func (intern (concat "org-babel-edit-prep:" lang))))
	(when (fboundp edit-prep-func) (funcall edit-prep-func babel-info)))
      ;; Return success.
      t)))

(defun org-edit-fixed-width-region ()
  "Edit the fixed-width ASCII drawing at point.
\\<org-src-mode-map>
This must be a region where each line starts with a colon
followed by a space or a newline character.

A new buffer is created and the fixed-width region is copied into
it, and the buffer is switched into the major mode defined in
`org-edit-fixed-width-region-mode', which see.

When done, exit with `\\[org-edit-src-exit]'.  The edited text \
will then replace
the area in the Org mode buffer."
  (interactive)
  (let ((element (org-element-at-point)))
    (unless (and (eq (org-element-type element) 'fixed-width)
		 (org-src--on-datum-p element))
      (user-error "Not in a fixed-width area"))
    (org-src--edit-element
     element
     (org-src--construct-edit-buffer-name (buffer-name) "Fixed Width")
     org-edit-fixed-width-region-mode
     (lambda () (while (not (eobp)) (insert ": ") (forward-line))))
    ;; Return success.
    t))

(defun org-edit-src-abort ()
  "Abort editing of the src code and return to the Org buffer."
  (interactive)
  (let (org-src--allow-write-back) (org-edit-src-exit)))

(defun org-edit-src-continue (event)
  "Unconditionally return to buffer editing area under point.
Throw an error if there is no such buffer.
EVENT is passed to `mouse-set-point'."
  (interactive "e")
  (mouse-set-point event)
  (let ((buf (get-char-property (point) 'edit-buffer)))
    (if buf (org-src-switch-to-buffer buf 'continue)
      (user-error "No sub-editing buffer for area at point"))))

(defun org-edit-src-save ()
  "Save parent buffer with current state source-code buffer."
  (interactive)
  (unless (org-src-edit-buffer-p) (user-error "Not in a sub-editing buffer"))
  (set-buffer-modified-p nil)
  (let ((write-back-buf (generate-new-buffer "*org-src-write-back*"))
	(beg org-src--beg-marker)
	(end org-src--end-marker)
	(overlay org-src--overlay))
    (org-src--contents-for-write-back write-back-buf)
    (with-current-buffer (org-src-source-buffer)
      (undo-boundary)
      (goto-char beg)
      ;; Temporarily disable read-only features of OVERLAY in order to
      ;; insert new contents.
      (delete-overlay overlay)
      (let ((expecting-bol (bolp)))
	(if (version< emacs-version "27.1")
	    (progn (delete-region beg end)
		   (insert (with-current-buffer write-back-buf (buffer-string))))
	  (save-restriction
	    (narrow-to-region beg end)
	    (org-replace-buffer-contents write-back-buf 0.1 nil)
	    (goto-char (point-max))))
	(when (and expecting-bol (not (bolp))) (insert "\n")))
      (kill-buffer write-back-buf)
      (save-buffer)
      (move-overlay overlay beg (point))))
  ;; `write-contents-functions' requires the function to return
  ;; a non-nil value so that other functions are not called.
  t)

(defun org-edit-src-exit ()
  "Kill current sub-editing buffer and return to source buffer."
  (interactive)
  (unless (org-src-edit-buffer-p)
    (error "Not in a sub-editing buffer"))
  (let* ((beg org-src--beg-marker)
	 (end org-src--end-marker)
	 (write-back org-src--allow-write-back)
	 (remote org-src--remote)
	 (coordinates (and (not remote)
			   (org-src--coordinates (point) 1 (point-max))))
	 (write-back-buf
          (and write-back (generate-new-buffer "*org-src-write-back*"))))
    (when write-back (org-src--contents-for-write-back write-back-buf))
    (set-buffer-modified-p nil)
    ;; Switch to source buffer.  Kill sub-editing buffer.
    (let ((edit-buffer (current-buffer))
	  (source-buffer (marker-buffer beg)))
      (unless source-buffer
        (when write-back-buf (kill-buffer write-back-buf))
        (error "Source buffer disappeared.  Aborting"))
      (org-src-switch-to-buffer source-buffer 'exit)
      (kill-buffer edit-buffer))
    ;; Insert modified code.  Ensure it ends with a newline character.
    (org-with-wide-buffer
     (when (and write-back
                (not (equal (buffer-substring beg end)
			  (with-current-buffer write-back-buf
                            (buffer-string)))))
       (undo-boundary)
       (goto-char beg)
       (let ((expecting-bol (bolp)))
	 (if (version< emacs-version "27.1")
	     (progn (delete-region beg end)
		    (insert (with-current-buffer write-back-buf
                              (buffer-string))))
	   (save-restriction
	     (narrow-to-region beg end)
	     (org-replace-buffer-contents write-back-buf 0.1 nil)
	     (goto-char (point-max))))
	 (when (and expecting-bol (not (bolp))) (insert "\n")))))
    (when write-back-buf (kill-buffer write-back-buf))
    ;; If we are to return to source buffer, put point at an
    ;; appropriate location.  In particular, if block is hidden, move
    ;; to the beginning of the block opening line.
    (unless remote
      (goto-char beg)
      (cond
       ;; Block is hidden; move at start of block.
       ((if (eq org-fold-core-style 'text-properties)
            (org-fold-folded-p nil 'block)
          (cl-some (lambda (o) (eq (overlay-get o 'invisible) 'org-hide-block))
		   (overlays-at (point))))
	(beginning-of-line 0))
       (write-back (org-src--goto-coordinates coordinates beg end))))
    ;; Clean up left-over markers and restore window configuration.
    (set-marker beg nil)
    (set-marker end nil)
    (when org-src--saved-temp-window-config
      (unwind-protect
	  (set-window-configuration org-src--saved-temp-window-config)
	(setq org-src--saved-temp-window-config nil)))))

(provide 'org-src)

;;; org-src.el ends here
