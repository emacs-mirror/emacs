;;; flyspell.el --- on-the-fly spell checker

;; Copyright (C) 1998, 2000, 2001, 2002 Free Software Foundation, Inc.

;; Author: Manuel Serrano <Manuel.Serrano@unice.fr>
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
;; Flyspell is a minor Emacs mode performing on-the-fly spelling
;; checking.
;;
;; To enable Flyspell minor mode, type M-x flyspell-mode.
;; This applies only to the current buffer.
;;
;; To enable Flyspell in text representing computer programs, type
;; M-x flyspell-prog-mode.
;; In that mode only text inside comments is checked.
;;
;; Note: consider setting the variable ispell-parser to `tex' to
;; avoid TeX command checking; use `(setq ispell-parser 'tex)'.
;;
;; Some user variables control the behavior of flyspell.  They are
;; those defined under the `User variables' comment.

;;; Code:

(require 'ispell)

;*---------------------------------------------------------------------*/
;*    Group ...                                                        */
;*---------------------------------------------------------------------*/
(defgroup flyspell nil
  "Spell checking on the fly."
  :tag "FlySpell"
  :prefix "flyspell-"
  :group 'ispell
  :group 'processes)

;*---------------------------------------------------------------------*/
;*    User configuration ...                                           */
;*---------------------------------------------------------------------*/
(defcustom flyspell-highlight-flag t
  "*How Flyspell should indicate misspelled words.
Non-nil means use highlight, nil means use minibuffer messages."
  :group 'flyspell
  :type 'boolean)

(defcustom flyspell-mark-duplications-flag t
  "*Non-nil means Flyspell reports a repeated word as an error."
  :group 'flyspell
  :type 'boolean)

(defcustom flyspell-sort-corrections nil
  "*Non-nil means, sort the corrections alphabetically before popping them."
  :group 'flyspell
  :version "21.1"
  :type 'boolean)

(defcustom flyspell-duplicate-distance -1
  "*The maximum distance for finding duplicates of unrecognized words.
This applies to the feature that when a word is not found in the dictionary,
if the same spelling occurs elsewhere in the buffer,
Flyspell uses a different face (`flyspell-duplicate-face') to highlight it.
This variable specifies how far to search to find such a duplicate.
-1 means no limit (search the whole buffer).
0 means do not search for duplicate unrecognized spellings."
  :group 'flyspell
  :version "21.1"
  :type 'number)

(defcustom flyspell-delay 3
  "*The number of seconds to wait before checking, after a \"delayed\" command."
  :group 'flyspell
  :type 'number)

(defcustom flyspell-persistent-highlight t
  "*Non-nil means misspelled words remain highlighted until corrected.
If this variable is nil, only the most recently detected misspelled word
is highlighted."
  :group 'flyspell
  :type 'boolean)

(defcustom flyspell-highlight-properties t
  "*Non-nil means highlight incorrect words even if a property exists for this word."
  :group 'flyspell
  :type 'boolean)

(defcustom flyspell-default-delayed-commands
  '(self-insert-command
    delete-backward-char
    backward-or-forward-delete-char
    delete-char
    scrollbar-vertical-drag)
  "The standard list of delayed commands for Flyspell.
See `flyspell-delayed-commands'."
  :group 'flyspell
  :version "21.1"
  :type '(repeat (symbol)))

(defcustom flyspell-delayed-commands nil
  "List of commands that are \"delayed\" for Flyspell mode.
After these commands, Flyspell checking is delayed for a short time,
whose length is specified by `flyspell-delay'."
  :group 'flyspell
  :type '(repeat (symbol)))

(defcustom flyspell-default-deplacement-commands
  '(next-line
    previous-line
    scroll-up
    scroll-down)
  "The standard list of deplacement commands for Flyspell.
See `flyspell-deplacement-commands'."
  :group 'flyspell
  :version "21.1"
  :type '(repeat (symbol)))

(defcustom flyspell-deplacement-commands nil
  "List of commands that are \"deplacement\" for Flyspell mode.
After these commands, Flyspell checking is performed only if the previous
command was not the very same command."
  :group 'flyspell
  :version "21.1"
  :type '(repeat (symbol)))

(defcustom flyspell-issue-welcome-flag t
  "*Non-nil means that Flyspell should display a welcome message when started."
  :group 'flyspell
  :type 'boolean)

(defcustom flyspell-issue-message-flag t
  "*Non-nil means that Flyspell emits messages when checking words."
  :group 'flyspell
  :type 'boolean)

(defcustom flyspell-incorrect-hook nil
  "*List of functions to be called when incorrect words are encountered.
Each function is given three arguments: the beginning and the end
of the incorrect region.  The third is either the symbol 'doublon' or the list
of possible corrections as returned by 'ispell-parse-output'.

If any of the functions return non-Nil, the word is not highlighted as
incorrect."
  :group 'flyspell
  :version "21.1"
  :type 'hook)

(defcustom flyspell-default-dictionary nil
  "A string that is the name of the default dictionary.
This is passed to the `ispell-change-dictionary' when flyspell is started.
If the variable `ispell-local-dictionary' or `ispell-dictionary' is non-nil
when flyspell is started, the value of that variable is used instead
of `flyspell-default-dictionary' to select the default dictionary.
Otherwise, if `flyspell-default-dictionary' is nil, it means to use
Ispell's ultimate default dictionary."
  :group 'flyspell
  :version "21.1"
  :type '(choice string (const :tag "Default" nil)))

(defcustom flyspell-tex-command-regexp
  "\\(\\(begin\\|end\\)[ \t]*{\\|\\(cite[a-z*]*\\|label\\|ref\\|eqref\\|usepackage\\|documentclass\\)[ \t]*\\(\\[[^]]*\\]\\)?{[^{}]*\\)"
  "A string that is the regular expression that matches TeX commands."
  :group 'flyspell
  :version "21.1"
  :type 'string)

(defcustom flyspell-check-tex-math-command nil
  "*Non nil means check even inside TeX math environment.
TeX math environments are discovered by the TEXMATHP that implemented
inside the texmathp.el Emacs package.  That package may be found at:
http://strw.leidenuniv.nl/~dominik/Tools"
  :group 'flyspell
  :type 'boolean)

(defcustom flyspell-dictionaries-that-consider-dash-as-word-delimiter
  '("francais" "deutsch8" "norsk")
  "List of dictionary names that consider `-' as word delimiter."
  :group 'flyspell
  :version "21.1"
  :type '(repeat (string)))

(defcustom flyspell-abbrev-p
  t
  "*If true, add correction to abbreviation table."
  :group 'flyspell
  :version "21.1"
  :type 'boolean)

(defcustom flyspell-use-global-abbrev-table-p
  nil
  "*If true, prefer global abbrev table to local abbrev table."
  :group 'flyspell
  :version "21.1"
  :type 'boolean)

;;;###autoload
(defcustom flyspell-mode-line-string " Fly"
  "*String displayed on the modeline when flyspell is active.
Set this to nil if you don't want a modeline indicator."
  :group 'flyspell
  :type '(choice string (const :tag "None" nil)))

(defcustom flyspell-large-region 1000
  "*The threshold that determines if a region is small.
The `flyspell-region' function is invoked if the region is small, the
word are checked one after the other using regular flyspell check
means.  If the region is large, a new Ispell process is spawned to get
speed."
  :group 'flyspell
  :version "21.1"
  :type 'number)

(defcustom flyspell-insert-function (function insert)
  "*Function for inserting word by flyspell upon correction."
  :group 'flyspell
  :type 'function)

(defcustom flyspell-before-incorrect-word-string nil
  "String used to indicate an incorrect word starting."
  :group 'flyspell
  :type '(choice string (const nil)))

(defcustom flyspell-after-incorrect-word-string nil
  "String used to indicate an incorrect word ending."
  :group 'flyspell
  :type '(choice string (const nil)))

;*---------------------------------------------------------------------*/
;*    Mode specific options                                            */
;*    -------------------------------------------------------------    */
;*    Mode specific options enable users to disable flyspell on        */
;*    certain word depending of the emacs mode. For instance, when     */
;*    using flyspell with mail-mode add the following expression       */
;*    in your .emacs file:                                             */
;*       (add-hook 'mail-mode                                          */
;*    	     '(lambda () (setq flyspell-generic-check-word-p           */
;*    			       'mail-mode-flyspell-verify)))           */
;*---------------------------------------------------------------------*/
(defvar flyspell-generic-check-word-p nil
  "Function providing per-mode customization over which words are flyspelled.
Returns t to continue checking, nil otherwise.
Flyspell mode sets this variable to whatever is the `flyspell-mode-predicate'
property of the major mode name.")
(make-variable-buffer-local 'flyspell-generic-check-word-p)

;*--- mail mode -------------------------------------------------------*/
(put 'mail-mode 'flyspell-mode-predicate 'mail-mode-flyspell-verify)
(put 'message-mode 'flyspell-mode-predicate 'mail-mode-flyspell-verify)
(defun mail-mode-flyspell-verify ()
  "This function is used for `flyspell-generic-check-word-p' in Mail mode."
  (let ((in-headers (save-excursion
		      (re-search-forward mail-header-separator nil t)))
	(in-signature (save-excursion
			(re-search-backward message-signature-separator nil t))))
    (cond (in-headers
	   (and (save-excursion (beginning-of-line)
				(looking-at "^Subject:"))
		(> (point) (match-end 0))))
	  (in-signature
	   nil)
	  (t
	   (save-excursion
	     (beginning-of-line)
	     (not (looking-at "[>}|]\\To:")))))))

;*--- texinfo mode ----------------------------------------------------*/
(put 'texinfo-mode 'flyspell-mode-predicate 'texinfo-mode-flyspell-verify)
(defun texinfo-mode-flyspell-verify ()
  "This function is used for `flyspell-generic-check-word-p' in Texinfo mode."
  (save-excursion
    (forward-word -1)
    (not (looking-at "@"))))

;*--- tex mode --------------------------------------------------------*/
(put 'tex-mode 'flyspell-mode-predicate 'tex-mode-flyspell-verify)
(defun tex-mode-flyspell-verify ()
  "This function is used for `flyspell-generic-check-word-p' in LaTeX mode."
  (and
   (not (save-excursion
	  (re-search-backward "^[ \t]*%%%[ \t]+Local" (point-min) t)))
   (not (save-excursion
	  (let ((this (point-marker))
		(e (progn (end-of-line) (point-marker))))
	    (beginning-of-line)
	    (if (re-search-forward "\\\\\\(cite\\|label\\|ref\\){[^}]*}" e t)
		(and (>= this (match-beginning 0))
		     (<= this (match-end 0)) )))))))

;*--- sgml mode -------------------------------------------------------*/
(put 'sgml-mode 'flyspell-mode-predicate 'sgml-mode-flyspell-verify)
(put 'html-mode 'flyspell-mode-predicate 'sgml-mode-flyspell-verify)

(defun sgml-mode-flyspell-verify ()
  "This function is used for `flyspell-generic-check-word-p' in SGML mode."
  (not (save-excursion
	 (let ((this (point-marker))
	       (s (progn (beginning-of-line) (point-marker)))
	       (e (progn (end-of-line) (point-marker))))
	   (or (progn
		 (goto-char this)
		 (and (re-search-forward  "[^<]*>" e t)
		      (= (match-beginning 0) this)))
	       (progn
		 (goto-char this)
		 (and (re-search-backward "<[^>]*" s t)
		      (= (match-end 0) this)))
	       (and (progn
		      (goto-char this)
		      (and (re-search-forward  "[^&]*;" e t)
			   (= (match-beginning 0) this)))
		    (progn
		      (goto-char this)
		      (and (re-search-backward "&[^;]*" s t)
			   (= (match-end 0) this)))))))))

;*---------------------------------------------------------------------*/
;*    Programming mode                                                 */
;*---------------------------------------------------------------------*/
(defvar flyspell-prog-text-faces
  '(font-lock-string-face font-lock-comment-face font-lock-doc-face)
  "Faces corresponding to text in programming-mode buffers.")

(defun flyspell-generic-progmode-verify ()
  "Used for `flyspell-generic-check-word-p' in programming modes."
  (let ((f (get-text-property (point) 'face)))
    (memq f flyspell-prog-text-faces)))

;;;###autoload
(defun flyspell-prog-mode ()
  "Turn on `flyspell-mode' for comments and strings."
  (interactive)
  (setq flyspell-generic-check-word-p 'flyspell-generic-progmode-verify)
  (flyspell-mode 1))

;*---------------------------------------------------------------------*/
;*    Overlay compatibility                                            */
;*---------------------------------------------------------------------*/
(autoload 'make-overlay            "overlay" "Overlay compatibility kit." t)
(autoload 'overlayp                "overlay" "Overlay compatibility kit." t)
(autoload 'overlays-in             "overlay" "Overlay compatibility kit." t)
(autoload 'delete-overlay          "overlay" "Overlay compatibility kit." t)
(autoload 'overlays-at             "overlay" "Overlay compatibility kit." t)
(autoload 'overlay-put             "overlay" "Overlay compatibility kit." t)
(autoload 'overlay-get             "overlay" "Overlay compatibility kit." t)
(autoload 'previous-overlay-change "overlay" "Overlay compatibility kit." t)

;*---------------------------------------------------------------------*/
;*    Which emacs are we currently running                             */
;*---------------------------------------------------------------------*/
(defvar flyspell-emacs
  (cond
   ((string-match "XEmacs" emacs-version)
    'xemacs)
   (t
    'emacs))
  "The type of Emacs we are currently running.")

(defvar flyspell-use-local-map
  (or (eq flyspell-emacs 'xemacs)
      (not (string< emacs-version "20"))))

;*---------------------------------------------------------------------*/
;*    The minor mode declaration.                                      */
;*---------------------------------------------------------------------*/
(eval-when-compile (defvar flyspell-local-mouse-map))

;;;###autoload
(defvar flyspell-mode nil)
(make-variable-buffer-local 'flyspell-mode)

(defvar flyspell-mouse-map
  (let ((map (make-sparse-keymap)))
    (cond
     ((eq flyspell-emacs 'xemacs)
      (define-key map [(button2)] #'flyspell-correct-word)
      (define-key map "\M-\t" #'flyspell-auto-correct-word))
     (flyspell-use-local-map
      (define-key map [(mouse-2)] #'flyspell-correct-word)
      (define-key map "\M-\t" #'flyspell-auto-correct-word)))
    map))

;;;###autoload
(defvar flyspell-mode-map (make-sparse-keymap))

;; mouse, keyboard bindings and misc definition
(when (or (assoc 'flyspell-mode minor-mode-map-alist)
	  (setq minor-mode-map-alist
		(cons (cons 'flyspell-mode flyspell-mode-map)
		      minor-mode-map-alist)))
  (define-key flyspell-mode-map "\M-\t" 'flyspell-auto-correct-word))

;; the name of the overlay property that defines the keymap
(defvar flyspell-overlay-keymap-property-name 'keymap)

;; dash character machinery
(defvar flyspell-consider-dash-as-word-delimiter-flag nil
   "*Non-nil means that the `-' char is considered as a word delimiter.")
(make-variable-buffer-local 'flyspell-consider-dash-as-word-delimiter-flag)
(defvar flyspell-dash-dictionary nil)
(make-variable-buffer-local 'flyspell-dash-dictionary)
(defvar flyspell-dash-local-dictionary nil)
(make-variable-buffer-local 'flyspell-dash-local-dictionary)

;*---------------------------------------------------------------------*/
;*    Highlighting                                                     */
;*---------------------------------------------------------------------*/
(defface flyspell-incorrect-face
  (if (eq flyspell-emacs 'xemacs)
      '((((class color)) (:foreground "OrangeRed" :bold t :underline t))
	(t (:bold t)))
    '((((class color)) (:foreground "OrangeRed" :weight bold :underline t))
      (t (:weight bold))))
  "Face used for marking a misspelled word in Flyspell."
  :group 'flyspell)

(defface flyspell-duplicate-face
  (if (eq flyspell-emacs 'xemacs)
      '((((class color)) (:foreground "Gold3" :bold t :underline t))
	(t (:bold t)))
    '((((class color)) (:foreground "Gold3" :weight bold :underline t))
      (t (:weight bold))))
  "Face used for marking a misspelled word that appears twice in the buffer.
See also `flyspell-duplicate-distance'."
  :group 'flyspell)

(defvar flyspell-overlay nil)

;*---------------------------------------------------------------------*/
;*    flyspell-mode ...                                                */
;*---------------------------------------------------------------------*/
;;;###autoload
(defun flyspell-mode (&optional arg)
  "Minor mode performing on-the-fly spelling checking.
Ispell is automatically spawned on background for each entered words.
The default flyspell behavior is to highlight incorrect words.
With no argument, this command toggles Flyspell mode.
With a prefix argument ARG, turn Flyspell minor mode on iff ARG is positive.

Bindings:
\\[ispell-word]: correct words (using Ispell).
\\[flyspell-auto-correct-word]: automatically correct word.
\\[flyspell-correct-word] (or mouse-2): popup correct words.

Hooks:
This runs `flyspell-mode-hook' after flyspell is entered.

Remark:
`flyspell-mode' uses `ispell-mode'.  Thus all Ispell options are
valid.  For instance, a personal dictionary can be used by
invoking `ispell-change-dictionary'.

Consider using the `ispell-parser' to check your text.  For instance
consider adding:
\(add-hook 'tex-mode-hook (function (lambda () (setq ispell-parser 'tex))))
in your .emacs file.

\\[flyspell-region] checks all words inside a region.
\\[flyspell-buffer] checks the whole buffer."
  (interactive "P")
  (let ((old-flyspell-mode flyspell-mode))
    ;; Mark the mode as on or off.
    (setq flyspell-mode (not (or (and (null arg) flyspell-mode)
				 (<= (prefix-numeric-value arg) 0))))
    ;; Do the real work.
    (unless (eq flyspell-mode old-flyspell-mode)
      (if flyspell-mode
	  (flyspell-mode-on)
	(flyspell-mode-off))
      ;; Force modeline redisplay.
      (set-buffer-modified-p (buffer-modified-p)))))

;*---------------------------------------------------------------------*/
;*    Autoloading                                                      */
;*---------------------------------------------------------------------*/
;;;###autoload
(add-minor-mode 'flyspell-mode
		'flyspell-mode-line-string
		flyspell-mode-map
		nil
		'flyspell-mode)

;*---------------------------------------------------------------------*/
;*    flyspell-buffers ...                                             */
;*    -------------------------------------------------------------    */
;*    For remembering buffers running flyspell                         */
;*---------------------------------------------------------------------*/
(defvar flyspell-buffers nil)

;*---------------------------------------------------------------------*/
;*    flyspell-minibuffer-p ...                                        */
;*---------------------------------------------------------------------*/
(defun flyspell-minibuffer-p (buffer)
  "Is BUFFER a minibuffer?"
  (let ((ws (get-buffer-window-list buffer t)))
    (and (consp ws) (window-minibuffer-p (car ws)))))

;*---------------------------------------------------------------------*/
;*    flyspell-version ...                                             */
;*---------------------------------------------------------------------*/
;;;###autoload
(defun flyspell-version ()
  "The flyspell version"
  (interactive)
  "1.6h")

;*---------------------------------------------------------------------*/
;*    flyspell-accept-buffer-local-defs ...                            */
;*---------------------------------------------------------------------*/
(defun flyspell-accept-buffer-local-defs ()
  (ispell-accept-buffer-local-defs)
  (if (not (and (eq flyspell-dash-dictionary ispell-dictionary)
		(eq flyspell-dash-local-dictionary ispell-local-dictionary)))
      ;; the dictionary has changed
      (progn
	(setq flyspell-dash-dictionary ispell-dictionary)
	(setq flyspell-dash-local-dictionary ispell-local-dictionary)
	(if (member (or ispell-local-dictionary ispell-dictionary)
		    flyspell-dictionaries-that-consider-dash-as-word-delimiter)
	    (setq flyspell-consider-dash-as-word-delimiter-flag t)
	  (setq flyspell-consider-dash-as-word-delimiter-flag nil)))))

;*---------------------------------------------------------------------*/
;*    flyspell-mode-on ...                                             */
;*---------------------------------------------------------------------*/
(defun flyspell-mode-on ()
  "Turn Flyspell mode on.  Do not use this; use `flyspell-mode' instead."
  (setq ispell-highlight-face 'flyspell-incorrect-face)
  ;; local dictionaries setup
  (ispell-change-dictionary
   (or ispell-local-dictionary ispell-dictionary flyspell-default-dictionary))
  ;; we have to force ispell to accept the local definition or
  ;; otherwise it could be too late, the local dictionary may
  ;; be forgotten!
  (flyspell-accept-buffer-local-defs)
  ;; we put the `flyspell-delayed' property on some commands
  (flyspell-delay-commands)
  ;; we put the `flyspell-deplacement' property on some commands
  (flyspell-deplacement-commands)
  ;; we bound flyspell action to post-command hook
  (add-hook 'post-command-hook (function flyspell-post-command-hook) t t)
  ;; we bound flyspell action to pre-command hook
  (add-hook 'pre-command-hook (function flyspell-pre-command-hook) t t)
  ;; we bound flyspell action to after-change hook
  (make-local-variable 'after-change-functions)
  (setq after-change-functions
	(cons 'flyspell-after-change-function after-change-functions))
  ;; set flyspell-generic-check-word-p based on the major mode
  (let ((mode-predicate (get major-mode 'flyspell-mode-predicate)))
    (if mode-predicate
	(setq flyspell-generic-check-word-p mode-predicate)))
  ;; the welcome message
  (if (and flyspell-issue-message-flag
	   flyspell-issue-welcome-flag
	   (interactive-p))
      (let ((binding (where-is-internal 'flyspell-auto-correct-word
					nil 'non-ascii)))
	(message
	 (if binding
	     (format "Welcome to flyspell. Use %s or Mouse-2 to correct words."
		     (key-description binding))
	   "Welcome to flyspell. Use Mouse-2 to correct words."))))
  ;; we end with the flyspell hooks
  (run-hooks 'flyspell-mode-hook))

;*---------------------------------------------------------------------*/
;*    flyspell-delay-commands ...                                      */
;*---------------------------------------------------------------------*/
(defun flyspell-delay-commands ()
  "Install the standard set of Flyspell delayed commands."
  (mapcar 'flyspell-delay-command flyspell-default-delayed-commands)
  (mapcar 'flyspell-delay-command flyspell-delayed-commands))

;*---------------------------------------------------------------------*/
;*    flyspell-delay-command ...                                       */
;*---------------------------------------------------------------------*/
(defun flyspell-delay-command (command)
  "Set COMMAND to be delayed, for Flyspell.
When flyspell `post-command-hook' is invoked because a delayed command
as been used the current word is not immediately checked.
It will be checked only after `flyspell-delay' seconds."
  (interactive "SDelay Flyspell after Command: ")
  (put command 'flyspell-delayed t))

;*---------------------------------------------------------------------*/
;*    flyspell-deplacement-commands ...                                */
;*---------------------------------------------------------------------*/
(defun flyspell-deplacement-commands ()
  "Install the standard set of Flyspell deplacement commands."
  (mapcar 'flyspell-deplacement-command flyspell-default-deplacement-commands)
  (mapcar 'flyspell-deplacement-command flyspell-deplacement-commands))

;*---------------------------------------------------------------------*/
;*    flyspell-deplacement-command ...                                 */
;*---------------------------------------------------------------------*/
(defun flyspell-deplacement-command (command)
  "Set COMMAND that implement cursor movements, for Flyspell.
When flyspell `post-command-hook' is invoked because of a deplacement command
as been used the current word is checked only if the previous command was
not the very same deplacement command."
  (interactive "SDeplacement Flyspell after Command: ")
  (put command 'flyspell-deplacement t))

;*---------------------------------------------------------------------*/
;*    flyspell-word-cache ...                                          */
;*---------------------------------------------------------------------*/
(defvar flyspell-word-cache-start  nil)
(defvar flyspell-word-cache-end    nil)
(defvar flyspell-word-cache-word   nil)
(make-variable-buffer-local 'flyspell-word-cache-start)
(make-variable-buffer-local 'flyspell-word-cache-end)
(make-variable-buffer-local 'flyspell-word-cache-word)

;*---------------------------------------------------------------------*/
;*    The flyspell pre-hook, store the current position. In the        */
;*    post command hook, we will check, if the word at this position   */
;*    has to be spell checked.                                         */
;*---------------------------------------------------------------------*/
(defvar flyspell-pre-buffer     nil)
(defvar flyspell-pre-point      nil)
(defvar flyspell-pre-column     nil)
(defvar flyspell-pre-pre-buffer nil)
(defvar flyspell-pre-pre-point  nil)

;*---------------------------------------------------------------------*/
;*    flyspell-previous-command ...                                    */
;*---------------------------------------------------------------------*/
(defvar flyspell-previous-command nil
  "The last interactive command checked by Flyspell.")

;*---------------------------------------------------------------------*/
;*    flyspell-pre-command-hook ...                                    */
;*---------------------------------------------------------------------*/
(defun flyspell-pre-command-hook ()
  "Save the current buffer and point for Flyspell's post-command hook."
  (interactive)
  (setq flyspell-pre-buffer (current-buffer))
  (setq flyspell-pre-point  (point))
  (setq flyspell-pre-column (current-column)))

;*---------------------------------------------------------------------*/
;*    flyspell-mode-off ...                                            */
;*---------------------------------------------------------------------*/
;;;###autoload
(defun flyspell-mode-off ()
  "Turn Flyspell mode off."
  ;; we remove the hooks
  (remove-hook 'post-command-hook (function flyspell-post-command-hook) t)
  (remove-hook 'pre-command-hook (function flyspell-pre-command-hook) t)
  (setq after-change-functions (delq 'flyspell-after-change-function
				     after-change-functions))
  ;; we remove all the flyspell hilightings
  (flyspell-delete-all-overlays)
  ;; we have to erase pre cache variables
  (setq flyspell-pre-buffer nil)
  (setq flyspell-pre-point  nil)
  ;; we mark the mode as killed
  (setq flyspell-mode nil))

;*---------------------------------------------------------------------*/
;*    flyspell-check-pre-word-p ...                                    */
;*---------------------------------------------------------------------*/
(defun flyspell-check-pre-word-p ()
  "Return non-nil if we should to check the word before point.
More precisely, it applies to the word that was before point
before the current command."
  (cond
   ((or (not (numberp flyspell-pre-point))
	(not (bufferp flyspell-pre-buffer))
	(not (buffer-live-p flyspell-pre-buffer)))
    nil)
   ((and (eq flyspell-pre-pre-point flyspell-pre-point)
	 (eq flyspell-pre-pre-buffer flyspell-pre-buffer))
    nil)
   ((or (and (= flyspell-pre-point (- (point) 1))
	     (eq (char-syntax (char-after flyspell-pre-point)) ?w))
	(= flyspell-pre-point (point))
	(= flyspell-pre-point (+ (point) 1)))
    nil)
   ((and (symbolp this-command)
	 (not executing-kbd-macro)
	 (or (get this-command 'flyspell-delayed)
	     (and (get this-command 'flyspell-deplacement)
		  (eq flyspell-previous-command this-command)))
	 (or (= (current-column) 0)
	     (= (current-column) flyspell-pre-column)
	     (eq (char-syntax (char-after flyspell-pre-point)) ?w)))
    nil)
   ((not (eq (current-buffer) flyspell-pre-buffer))
    t)
   ((not (and (numberp flyspell-word-cache-start)
	      (numberp flyspell-word-cache-end)))
    t)
   (t
    (or (< flyspell-pre-point flyspell-word-cache-start)
	(> flyspell-pre-point flyspell-word-cache-end)))))

;*---------------------------------------------------------------------*/
;*    The flyspell after-change-hook, store the change position. In    */
;*    the post command hook, we will check, if the word at this        */
;*    position has to be spell checked.                                */
;*---------------------------------------------------------------------*/
(defvar flyspell-changes nil)

;*---------------------------------------------------------------------*/
;*    flyspell-after-change-function ...                               */
;*---------------------------------------------------------------------*/
(defun flyspell-after-change-function (start stop len)
  "Save the current buffer and point for Flyspell's post-command hook."
  (interactive)
  (setq flyspell-changes (cons (cons start stop) flyspell-changes)))

;*---------------------------------------------------------------------*/
;*    flyspell-check-changed-word-p ...                                */
;*---------------------------------------------------------------------*/
(defun flyspell-check-changed-word-p (start stop)
  "Return t when the changed word has to be checked.
The answer depends of several criteria.
Mostly we check word delimiters."
  (cond
   ((and (eq (char-after start) ?\n) (> stop start))
    t)
   ((not (numberp flyspell-pre-point))
    t)
   ((and (>= flyspell-pre-point start) (<= flyspell-pre-point stop))
    nil)
   ((let ((pos (point)))
      (or (>= pos start) (<= pos stop) (= pos (1+ stop))))
    nil)
   (t
    t)))

;*---------------------------------------------------------------------*/
;*    flyspell-check-word-p ...                                        */
;*---------------------------------------------------------------------*/
(defun flyspell-check-word-p ()
  "Return t when the word at `point' has to be checked.
The answer depends of several criteria.
Mostly we check word delimiters."
  (cond
   ((<= (- (point-max) 1) (point-min))
    ;; the buffer is not filled enough
    nil)
   ((and (and (> (current-column) 0)
	      (not (eq (current-column) flyspell-pre-column)))
	 (save-excursion
	   (backward-char 1)
	   (and (looking-at (flyspell-get-not-casechars))
		(or flyspell-consider-dash-as-word-delimiter-flag
		    (not (looking-at "\\-"))))))
    ;; yes because we have reached or typed a word delimiter.
    t)
   ((symbolp this-command)
    (cond
     ((get this-command 'flyspell-deplacement)
      (not (eq flyspell-previous-command this-command)))
     ((get this-command 'flyspell-delayed)
      ;; the current command is not delayed, that
      ;; is that we must check the word now
      (if (fboundp 'about-xemacs)
	  (sit-for flyspell-delay nil)
	(sit-for flyspell-delay 0 nil)))
     (t t)))
   (t t)))

;*---------------------------------------------------------------------*/
;*    flyspell-debug-signal-no-check ...                               */
;*---------------------------------------------------------------------*/
(defun flyspell-debug-signal-no-check (msg obj)
  (setq debug-on-error t)
  (save-excursion
    (let ((buffer (get-buffer-create "*flyspell-debug*")))
      (set-buffer buffer)
      (erase-buffer)
      (insert "NO-CHECK:\n")
      (insert (format "    %S : %S\n" msg obj)))))

;*---------------------------------------------------------------------*/
;*    flyspell-debug-signal-pre-word-checked ...                       */
;*---------------------------------------------------------------------*/
(defun flyspell-debug-signal-pre-word-checked ()
  (setq debug-on-error t)
  (save-excursion
    (let ((buffer (get-buffer-create "*flyspell-debug*")))
      (set-buffer buffer)
      (insert "PRE-WORD:\n")
      (insert (format "  pre-point  : %S\n" flyspell-pre-point))
      (insert (format "  pre-buffer : %S\n" flyspell-pre-buffer))
      (insert (format "  cache-start: %S\n" flyspell-word-cache-start))
      (insert (format "  cache-end  : %S\n" flyspell-word-cache-end))
      (goto-char (point-max)))))

;*---------------------------------------------------------------------*/
;*    flyspell-debug-signal-word-checked ...                           */
;*---------------------------------------------------------------------*/
(defun flyspell-debug-signal-word-checked ()
  (setq debug-on-error t)
  (save-excursion
    (let ((oldbuf (current-buffer))
	  (buffer (get-buffer-create "*flyspell-debug*"))
	  (point  (point)))
      (set-buffer buffer)
      (insert "WORD:\n")
      (insert (format "  this-cmd   : %S\n" this-command))
      (insert (format "  delayed    : %S\n" (and (symbolp this-command)
						 (get this-command 'flyspell-delayed))))
      (insert (format "  point      : %S\n" point))
      (insert (format "  prev-char  : [%c] %S\n"
		      (progn
			(set-buffer oldbuf)
			(let ((c (if (> (point) (point-min))
				     (save-excursion
				       (backward-char 1)
				       (char-after (point)))
				   ? )))
			  (set-buffer buffer)
			  c))
		      (progn
			(set-buffer oldbuf)
			(let ((c (if (> (point) (point-min))
				     (save-excursion
				       (backward-char 1)
				       (and (and (looking-at (flyspell-get-not-casechars)) 1)
					    (and (or flyspell-consider-dash-as-word-delimiter-flag
						     (not (looking-at "\\-"))) 2))))))
			  (set-buffer buffer)
			  c))))
      (insert (format "  because    : %S\n"
		      (cond
		       ((not (and (symbolp this-command)
				  (get this-command 'flyspell-delayed)))
			;; the current command is not delayed, that
			;; is that we must check the word now
			'not-delayed)
		       ((progn
			  (set-buffer oldbuf)
			  (let ((c (if (> (point) (point-min))
				       (save-excursion
					 (backward-char 1)
					 (and (looking-at (flyspell-get-not-casechars))
					      (or flyspell-consider-dash-as-word-delimiter-flag
						  (not (looking-at "\\-"))))))))
			    (set-buffer buffer)
			    c))
			;; yes because we have reached or typed a word delimiter.
			'separator)
		       ((not (integerp flyspell-delay))
			;; yes because the user had set up a no-delay configuration.
			'no-delay)
		       (t
			'sit-for))))
      (goto-char (point-max)))))

;*---------------------------------------------------------------------*/
;*    flyspell-debug-signal-changed-checked ...                        */
;*---------------------------------------------------------------------*/
(defun flyspell-debug-signal-changed-checked ()
  (setq debug-on-error t)
  (save-excursion
    (let ((buffer (get-buffer-create "*flyspell-debug*"))
	  (point  (point)))
      (set-buffer buffer)
      (insert "CHANGED WORD:\n")
      (insert (format "  point   : %S\n" point))
      (goto-char (point-max)))))

;*---------------------------------------------------------------------*/
;*    flyspell-post-command-hook ...                                   */
;*    -------------------------------------------------------------    */
;*    It is possible that we check several words:                      */
;*    1- the current word is checked if the predicate                  */
;*       FLYSPELL-CHECK-WORD-P is true                                 */
;*    2- the word that used to be the current word before the          */
;*       THIS-COMMAND is checked if:                                   */
;*        a- the previous word is different from the current word      */
;*        b- the previous word as not just been checked by the         */
;*           previous FLYSPELL-POST-COMMAND-HOOK                       */
;*    3- the words changed by the THIS-COMMAND that are neither the    */
;*       previous word nor the current word                            */
;*---------------------------------------------------------------------*/
(defun flyspell-post-command-hook ()
  "The `post-command-hook' used by flyspell to check a word in-the-fly."
  (interactive)
  (let ((command this-command))
    (if (flyspell-check-pre-word-p)
	(save-excursion
	  '(flyspell-debug-signal-pre-word-checked)
	  (set-buffer flyspell-pre-buffer)
	  (save-excursion
	    (goto-char flyspell-pre-point)
	    (flyspell-word))))
    (if (flyspell-check-word-p)
	(progn
	  '(flyspell-debug-signal-word-checked)
	  (flyspell-word)
	  ;; we remember which word we have just checked.
	  ;; this will be used next time we will check a word
	  ;; to compare the next current word with the word
	  ;; that as been registered in the pre-command-hook
	  ;; that is these variables are used within the predicate
	  ;; FLYSPELL-CHECK-PRE-WORD-P
	  (setq flyspell-pre-pre-buffer (current-buffer))
	  (setq flyspell-pre-pre-point  (point)))
      (progn
	(setq flyspell-pre-pre-buffer nil)
	(setq flyspell-pre-pre-point  nil)
	;; when a word is not checked because of a delayed command
	;; we do not disable the ispell cache.
	(if (and (symbolp this-command) (get this-command 'flyspell-delayed))
	    (setq flyspell-word-cache-end -1))))
    (while (consp flyspell-changes)
      (let ((start (car (car flyspell-changes)))
	    (stop  (cdr (car flyspell-changes))))
	(if (flyspell-check-changed-word-p start stop)
	    (save-excursion
	      '(flyspell-debug-signal-changed-checked)
	      (goto-char start)
	      (flyspell-word)))
	(setq flyspell-changes (cdr flyspell-changes))))
    (setq flyspell-previous-command command)))

;*---------------------------------------------------------------------*/
;*    flyspell-notify-misspell ...                                     */
;*---------------------------------------------------------------------*/
(defun flyspell-notify-misspell (start end word poss)
  (let ((replacements (if (stringp poss)
			  poss
			(if flyspell-sort-corrections
			    (sort (car (cdr (cdr poss))) 'string<)
			  (car (cdr (cdr poss)))))))
    (if flyspell-issue-message-flag
	(message (format "mispelling `%s'  %S" word replacements)))))

;*---------------------------------------------------------------------*/
;*    flyspell-word ...                                                */
;*---------------------------------------------------------------------*/
(defun flyspell-word (&optional following)
  "Spell check a word."
  (interactive (list current-prefix-arg))
  (if (interactive-p)
      (setq following ispell-following-word))
  (save-excursion
    ;; use the correct dictionary
    (flyspell-accept-buffer-local-defs)
    (let* ((cursor-location (point))
	  (flyspell-word (flyspell-get-word following))
	  start end poss word)
      (if (or (eq flyspell-word nil)
 	      (and (fboundp flyspell-generic-check-word-p)
 		   (not (funcall flyspell-generic-check-word-p))))
	  '()
	(progn
	  ;; destructure return flyspell-word info list.
	  (setq start (car (cdr flyspell-word))
		end (car (cdr (cdr flyspell-word)))
		word (car flyspell-word))
	  ;; before checking in the directory, we check for doublons.
	  (cond
	   ((and (or (not (eq ispell-parser 'tex))
		     (not (eq (char-after start) ?\\)))
		 flyspell-mark-duplications-flag
		 (save-excursion
		   (goto-char start)
		   (word-search-backward word
					 (- start
					    (+ 1 (- end start)))
					 t)))
	    ;; yes, this is a doublon
	    (flyspell-highlight-incorrect-region start end 'doublon))
	   ((and (eq flyspell-word-cache-start start)
		 (eq flyspell-word-cache-end end)
		 (string-equal flyspell-word-cache-word word))
	    ;; this word had been already checked, we skip
	    nil)
	   ((and (eq ispell-parser 'tex)
		 (flyspell-tex-command-p flyspell-word))
	    ;; this is a correct word (because a tex command)
	    (flyspell-unhighlight-at start)
	    (if (> end start)
		(flyspell-unhighlight-at (- end 1)))
	    t)
	   (t
	    ;; we setup the cache
	    (setq flyspell-word-cache-start start)
	    (setq flyspell-word-cache-end end)
	    (setq flyspell-word-cache-word word)
	    ;; now check spelling of word.
	    (process-send-string ispell-process "%\n")
	    ;; put in verbose mode
	    (process-send-string ispell-process
				 (concat "^" word "\n"))
	    ;; we mark the ispell process so it can be killed
	    ;; when emacs is exited without query
	    (if (fboundp 'process-kill-without-query)
		(process-kill-without-query ispell-process))
	    ;; wait until ispell has processed word
	    (while (progn
		     (accept-process-output ispell-process)
		     (not (string= "" (car ispell-filter)))))
	    ;; (process-send-string ispell-process "!\n")
	    ;; back to terse mode.
	    (setq ispell-filter (cdr ispell-filter))
	    (if (consp ispell-filter)
		(setq poss (ispell-parse-output (car ispell-filter))))
	    (cond ((eq poss t)
		   ;; correct
		   (flyspell-unhighlight-at start)
		   (if (> end start)
		       (flyspell-unhighlight-at (- end 1)))
		   t)
		  ((and (stringp poss) flyspell-highlight-flag)
		   ;; correct
		   (flyspell-unhighlight-at start)
		   (if (> end start)
		       (flyspell-unhighlight-at (- end 1)))
		   t)
		  ((null poss)
		   (flyspell-unhighlight-at start)
		   (if (> end start)
		       (flyspell-unhighlight-at (- end 1))))
		  ((or (and (< flyspell-duplicate-distance 0)
			    (or (save-excursion
				  (goto-char start)
				  (word-search-backward word
							(point-min)
							t))
				(save-excursion
				  (goto-char end)
				  (word-search-forward word
						       (point-max)
						       t))))
		       (and (> flyspell-duplicate-distance 0)
			    (or (save-excursion
				  (goto-char start)
				  (word-search-backward
				   word
				   (- start
				      flyspell-duplicate-distance)
				   t))
				(save-excursion
				  (goto-char end)
				  (word-search-forward
				   word
				   (+ end
				      flyspell-duplicate-distance)
				   t)))))
		   (if flyspell-highlight-flag
		       (flyspell-highlight-duplicate-region start end)
		     (message (format "duplicate `%s'" word))))
		  (t
		   ;; incorrect highlight the location
		   (if flyspell-highlight-flag
		       (flyspell-highlight-incorrect-region start end poss)
		     (flyspell-notify-misspell start end word poss))))
	    ;; return to original location
	    (goto-char cursor-location)
	    (if ispell-quit (setq ispell-quit nil)))))))))

;*---------------------------------------------------------------------*/
;*    flyspell-tex-math-initialized ...                                */
;*---------------------------------------------------------------------*/
(defvar flyspell-tex-math-initialized nil)

;*---------------------------------------------------------------------*/
;*    flyspell-math-tex-command-p ...                                  */
;*    -------------------------------------------------------------    */
;*    This function uses the texmathp package to check if (point)      */
;*    is within a tex command. In order to avoid using                 */
;*    condition-case each time we use the variable                     */
;*    flyspell-tex-math-initialized to make a special case the first   */
;*    time that function is called.                                    */
;*---------------------------------------------------------------------*/
(defun flyspell-math-tex-command-p ()
  (cond
   (flyspell-check-tex-math-command
    nil)
   ((eq flyspell-tex-math-initialized t)
    (texmathp))
   ((eq flyspell-tex-math-initialized 'error)
    nil)
   (t
    (setq flyspell-tex-math-initialized t)
    (condition-case nil
	(texmathp)
      (error (progn
	       (setq flyspell-tex-math-initialized 'error)
	       nil))))))

;*---------------------------------------------------------------------*/
;*    flyspell-tex-command-p ...                                       */
;*---------------------------------------------------------------------*/
(defun flyspell-tex-command-p (word)
  "Return t if WORD is a TeX command."
  (or (save-excursion
	(let ((b  (car (cdr word))))
	  (and (re-search-backward "\\\\" (- (point) 100) t)
	       (or (= (match-end 0) b)
		   (and (goto-char (match-end 0))
			(looking-at flyspell-tex-command-regexp)
			(>= (match-end 0) b))))))
      (flyspell-math-tex-command-p)))

;*---------------------------------------------------------------------*/
;*    flyspell-casechars-cache ...                                     */
;*---------------------------------------------------------------------*/
(defvar flyspell-casechars-cache nil)
(defvar flyspell-ispell-casechars-cache nil)
(make-variable-buffer-local 'flyspell-casechars-cache)
(make-variable-buffer-local 'flyspell-ispell-casechars-cache)

;*---------------------------------------------------------------------*/
;*    flyspell-get-casechars ...                                       */
;*---------------------------------------------------------------------*/
(defun flyspell-get-casechars ()
  "This function builds a string that is the regexp of word chars.
In order to avoid one useless string construction,
this function changes the last char of the `ispell-casechars' string."
  (let ((ispell-casechars (ispell-get-casechars)))
    (cond
     ((eq ispell-parser 'tex)
      (setq flyspell-ispell-casechars-cache ispell-casechars)
      (setq flyspell-casechars-cache
	    (concat (substring ispell-casechars
			       0
			       (- (length ispell-casechars) 1))
		    "]"))
      flyspell-casechars-cache)
     (t
      (setq flyspell-ispell-casechars-cache ispell-casechars)
      (setq flyspell-casechars-cache ispell-casechars)
      flyspell-casechars-cache))))

;*---------------------------------------------------------------------*/
;*    flyspell-get-not-casechars-cache ...                             */
;*---------------------------------------------------------------------*/
(defvar flyspell-not-casechars-cache nil)
(defvar flyspell-ispell-not-casechars-cache nil)
(make-variable-buffer-local 'flyspell-not-casechars-cache)
(make-variable-buffer-local 'flyspell-ispell-not-casechars-cache)

;*---------------------------------------------------------------------*/
;*    flyspell-get-not-casechars ...                                   */
;*---------------------------------------------------------------------*/
(defun flyspell-get-not-casechars ()
  "This function builds a string that is the regexp of non-word chars."
  (let ((ispell-not-casechars (ispell-get-not-casechars)))
    (cond
     ((eq ispell-parser 'tex)
      (setq flyspell-ispell-not-casechars-cache ispell-not-casechars)
      (setq flyspell-not-casechars-cache
	    (concat (substring ispell-not-casechars
			       0
			       (- (length ispell-not-casechars) 1))
		    "]"))
      flyspell-not-casechars-cache)
     (t
      (setq flyspell-ispell-not-casechars-cache ispell-not-casechars)
      (setq flyspell-not-casechars-cache ispell-not-casechars)
      flyspell-not-casechars-cache))))

;*---------------------------------------------------------------------*/
;*    flyspell-get-word ...                                            */
;*---------------------------------------------------------------------*/
(defun flyspell-get-word (following)
  "Return the word for spell-checking according to Ispell syntax.
If argument FOLLOWING is non-nil or if `ispell-following-word'
is non-nil when called interactively, then the following word
\(rather than preceding\) is checked when the cursor is not over a word.
Optional second argument contains other chars that can be included in word
many times.

Word syntax described by `ispell-dictionary-alist' (which see)."
  (let* ((flyspell-casechars (flyspell-get-casechars))
	 (flyspell-not-casechars (flyspell-get-not-casechars))
	 (ispell-otherchars (ispell-get-otherchars))
	 (ispell-many-otherchars-p (ispell-get-many-otherchars-p))
	 (word-regexp (if (string< "" ispell-otherchars)
			  (concat flyspell-casechars
				  "+\\("
				  ispell-otherchars
				  "?"
				  flyspell-casechars
				  "+\\)"
				  (if ispell-many-otherchars-p
				      "*" "?"))
			(concat flyspell-casechars "+")))
	 did-it-once
	 start end word)
    ;; find the word
    (if (not (looking-at flyspell-casechars))
	(if following
	    (re-search-forward flyspell-casechars (point-max) t)
	  (re-search-backward flyspell-casechars (point-min) t)))
    ;; move to front of word
    (re-search-backward flyspell-not-casechars (point-min) 'start)
    (let ((pos nil))
      (if (string< "" ispell-otherchars)
	  (while (and (looking-at ispell-otherchars)
		      (not (bobp))
		      (or (not did-it-once)
			  ispell-many-otherchars-p)
		      (not (eq pos (point))))
	    (setq pos (point))
	    (setq did-it-once t)
	    (backward-char 1)
	    (if (looking-at flyspell-casechars)
		(re-search-backward flyspell-not-casechars (point-min) 'move)
	      (backward-char -1)))))
    ;; Now mark the word and save to string.
    (if (eq (re-search-forward word-regexp (point-max) t) nil)
	nil
      (progn
	(setq start (match-beginning 0)
	      end (point)
	      word (buffer-substring-no-properties start end))
	(list word start end)))))

;*---------------------------------------------------------------------*/
;*    flyspell-small-region ...                                        */
;*---------------------------------------------------------------------*/
(defun flyspell-small-region (beg end)
  "Flyspell text between BEG and END."
  (save-excursion
    (if (> beg end)
	(let ((old beg))
	  (setq beg end)
	  (setq end old)))
    (goto-char beg)
    (let ((count 0))
      (while (< (point) end)
	(if (and flyspell-issue-message-flag (= count 100))
	    (progn
	      (message "Spell Checking...%d%%"
		       (* 100 (/ (float (- (point) beg)) (- end beg))))
	      (setq count 0))
	  (setq count (+ 1 count)))
	(flyspell-word)
	(sit-for 0)
	(let ((cur (point)))
	  (forward-word 1)
	  (if (and (< (point) end) (> (point) (+ cur 1)))
	      (backward-char 1)))))
    (backward-char 1)
    (if flyspell-issue-message-flag (message "Spell Checking completed."))
    (flyspell-word)))

;*---------------------------------------------------------------------*/
;*    flyspell-external-ispell-process ...                             */
;*---------------------------------------------------------------------*/
(defvar flyspell-external-ispell-process '()
  "The external Flyspell Ispell process.")

;*---------------------------------------------------------------------*/
;*    flyspell-external-ispell-buffer ...                              */
;*---------------------------------------------------------------------*/
(defvar flyspell-external-ispell-buffer '())
(defvar flyspell-large-region-buffer '())
(defvar flyspell-large-region-beg (point-min))
(defvar flyspell-large-region-end (point-max))

;*---------------------------------------------------------------------*/
;*    flyspell-external-point-words ...                                */
;*---------------------------------------------------------------------*/
(defun flyspell-external-point-words ()
  (let ((buffer flyspell-external-ispell-buffer))
    (set-buffer buffer)
    (beginning-of-buffer)
    (let ((size (- flyspell-large-region-end flyspell-large-region-beg))
	  (start flyspell-large-region-beg))
      ;; now we are done with ispell, we have to find the word in
      ;; the initial buffer
      (while (< (point) (- (point-max) 1))
	;; we have to fetch the incorrect word
	(if (re-search-forward "\\([^\n]+\\)\n" (point-max) t)
	    (let ((word (match-string 1)))
	      (goto-char (match-end 0))
	      (set-buffer flyspell-large-region-buffer)
	      (goto-char flyspell-large-region-beg)
	      (if flyspell-issue-message-flag
		  (message "Spell Checking...%d%% [%s]"
			   (* 100 (/ (float (- (point) start)) size))
			   word))
	      (if (search-forward word flyspell-large-region-end t)
		  (progn
		    (setq flyspell-large-region-beg (point))
		    (goto-char (- (point) 1))
		    (flyspell-word)))
	      (set-buffer buffer))
	  (goto-char (point-max)))))
    ;; we are done
    (if flyspell-issue-message-flag (message "Spell Checking completed."))
    ;; ok, we are done with pointing out incorrect words, we just
    ;; have to kill the temporary buffer
    (kill-buffer flyspell-external-ispell-buffer)
    (setq flyspell-external-ispell-buffer nil)))

;*---------------------------------------------------------------------*/
;*    flyspell-large-region ...                                        */
;*---------------------------------------------------------------------*/
(defun flyspell-large-region (beg end)
  (let* ((curbuf  (current-buffer))
	 (buffer  (get-buffer-create "*flyspell-region*")))
    (setq flyspell-external-ispell-buffer buffer)
    (setq flyspell-large-region-buffer curbuf)
    (setq flyspell-large-region-beg beg)
    (setq flyspell-large-region-end end)
    (set-buffer buffer)
    (erase-buffer)
    ;; this is done, we can start checking...
    (if flyspell-issue-message-flag (message "Checking region..."))
    (set-buffer curbuf)
    (let ((c (apply 'call-process-region beg
		    end
		    ispell-program-name
		    nil
		    buffer
		    nil
		    "-l"
		    (let (args)
		      ;; Local dictionary becomes the global dictionary in use.
		      (if ispell-local-dictionary
			  (setq ispell-dictionary ispell-local-dictionary))
		      (setq args (ispell-get-ispell-args))
		      (if ispell-dictionary ; use specified dictionary
			  (setq args
				(append (list "-d" ispell-dictionary) args)))
		      (if ispell-personal-dictionary ; use specified pers dict
			  (setq args
				(append args
					(list "-p"
					      (expand-file-name
					       ispell-personal-dictionary)))))
		      (setq args (append args ispell-extra-args))
		      args))))
      (if (= c 0)
	  (flyspell-external-point-words)
	(error "Can't check region...")))))

;*---------------------------------------------------------------------*/
;*    flyspell-region ...                                              */
;*    -------------------------------------------------------------    */
;*    Because `ispell -a' is too slow, it is not possible to use       */
;*    it on large region. Then, when ispell is invoked on a large      */
;*    text region, a new `ispell -l' process is spawned. The           */
;*    pointed out words are then searched in the region a checked with */
;*    regular flyspell means.                                          */
;*---------------------------------------------------------------------*/
;;;###autoload
(defun flyspell-region (beg end)
  "Flyspell text between BEG and END."
  (interactive "r")
  (if (= beg end)
      ()
    (save-excursion
      (if (> beg end)
	  (let ((old beg))
	    (setq beg end)
	    (setq end old)))
      (if (> (- end beg) flyspell-large-region)
	  (flyspell-large-region beg end)
	(flyspell-small-region beg end)))))

;*---------------------------------------------------------------------*/
;*    flyspell-buffer ...                                              */
;*---------------------------------------------------------------------*/
;;;###autoload
(defun flyspell-buffer ()
  "Flyspell whole buffer."
  (interactive)
  (flyspell-region (point-min) (point-max)))

;*---------------------------------------------------------------------*/
;*    old next error position ...                                      */
;*---------------------------------------------------------------------*/
(defvar flyspell-old-buffer-error nil)
(defvar flyspell-old-pos-error nil)

;*---------------------------------------------------------------------*/
;*    flyspell-goto-next-error ...                                     */
;*---------------------------------------------------------------------*/
(defun flyspell-goto-next-error ()
  "Go to the next previously detected error.
In general FLYSPELL-GOTO-NEXT-ERROR must be used after
FLYSPELL-BUFFER."
  (interactive)
  (let ((pos (point))
	(max (point-max)))
    (if (and (eq (current-buffer) flyspell-old-buffer-error)
	     (eq pos flyspell-old-pos-error))
	(progn
	  (if (= flyspell-old-pos-error max)
	      ;; goto beginning of buffer
	      (progn
		(message "Restarting from beginning of buffer")
		(goto-char (point-min)))
	    (forward-word 1))
	  (setq pos (point))))
    ;; seek the next error
    (while (and (< pos max)
		(let ((ovs (overlays-at pos))
		      (r '()))
		  (while (and (not r) (consp ovs))
		    (if (flyspell-overlay-p (car ovs))
			(setq r t)
		      (setq ovs (cdr ovs))))
		  (not r)))
      (setq pos (1+ pos)))
    ;; save the current location for next invocation
    (setq flyspell-old-pos-error pos)
    (setq flyspell-old-buffer-error (current-buffer))
    (goto-char pos)
    (if (= pos max)
	(message "No more miss-spelled word!"))))

;*---------------------------------------------------------------------*/
;*    flyspell-overlay-p ...                                           */
;*---------------------------------------------------------------------*/
(defun flyspell-overlay-p (o)
  "A predicate that return true iff O is an overlay used by flyspell."
  (and (overlayp o) (overlay-get o 'flyspell-overlay)))

;*---------------------------------------------------------------------*/
;*    flyspell-delete-all-overlays ...                                 */
;*    -------------------------------------------------------------    */
;*    Remove all the overlays introduced by flyspell.                  */
;*---------------------------------------------------------------------*/
(defun flyspell-delete-all-overlays ()
  "Delete all the overlays used by flyspell."
  (let ((l (overlays-in (point-min) (point-max))))
    (while (consp l)
      (progn
	(if (flyspell-overlay-p (car l))
	    (delete-overlay (car l)))
	(setq l (cdr l))))))

;*---------------------------------------------------------------------*/
;*    flyspell-unhighlight-at ...                                      */
;*---------------------------------------------------------------------*/
(defun flyspell-unhighlight-at (pos)
  "Remove the flyspell overlay that are located at POS."
  (if flyspell-persistent-highlight
      (let ((overlays (overlays-at pos)))
	(while (consp overlays)
	  (if (flyspell-overlay-p (car overlays))
	      (delete-overlay (car overlays)))
	  (setq overlays (cdr overlays))))
    (if (flyspell-overlay-p flyspell-overlay)
	(delete-overlay flyspell-overlay))))

;*---------------------------------------------------------------------*/
;*    flyspell-properties-at-p ...                                     */
;*    -------------------------------------------------------------    */
;*    Is there an highlight properties at position pos?                */
;*---------------------------------------------------------------------*/
(defun flyspell-properties-at-p (pos)
  "Return t if there is a text property at POS, not counting `local-map'.
If variable `flyspell-highlight-properties' is set to nil,
text with properties are not checked.  This function is used to discover
if the character at POS has any other property."
  (let ((prop (text-properties-at pos))
	(keep t))
    (while (and keep (consp prop))
      (if (and (eq (car prop) 'local-map) (consp (cdr prop)))
	  (setq prop (cdr (cdr prop)))
	(setq keep nil)))
    (consp prop)))

;*---------------------------------------------------------------------*/
;*    make-flyspell-overlay ...                                        */
;*---------------------------------------------------------------------*/
(defun make-flyspell-overlay (beg end face mouse-face)
  "Allocate an overlay to highlight an incorrect word.
BEG and END specify the range in the buffer of that word.
FACE and MOUSE-FACE specify the `face' and `mouse-face' properties
for the overlay."
  (let ((flyspell-overlay (make-overlay beg end nil t nil)))
    (overlay-put flyspell-overlay 'face face)
    (overlay-put flyspell-overlay 'mouse-face mouse-face)
    (overlay-put flyspell-overlay 'flyspell-overlay t)
    (overlay-put flyspell-overlay 'evaporate t)
    (overlay-put flyspell-overlay 'help-echo "mouse-2: correct word at point")
    (if flyspell-use-local-map
        (overlay-put flyspell-overlay
                     flyspell-overlay-keymap-property-name
                     flyspell-mouse-map))
    (when (eq face 'flyspell-incorrect-face)
      (and (stringp flyspell-before-incorrect-word-string)
           (overlay-put flyspell-overlay 'before-string
                        flyspell-before-incorrect-word-string))
      (and (stringp flyspell-after-incorrect-word-string)
           (overlay-put flyspell-overlay 'after-string
                        flyspell-after-incorrect-word-string)))
    flyspell-overlay))

;*---------------------------------------------------------------------*/
;*    flyspell-highlight-incorrect-region ...                          */
;*---------------------------------------------------------------------*/
(defun flyspell-highlight-incorrect-region (beg end poss)
  "Set up an overlay on a misspelled word, in the buffer from BEG to END."
  (unless (run-hook-with-args-until-success
           'flyspell-incorrect-hook beg end poss)
    (if (or flyspell-highlight-properties (not (flyspell-properties-at-p beg)))
        (progn
          ;; we cleanup current overlay at the same position
          (if (and (not flyspell-persistent-highlight)
                   (overlayp flyspell-overlay))
              (delete-overlay flyspell-overlay)
            (let ((overlays (overlays-at beg)))
              (while (consp overlays)
                (if (flyspell-overlay-p (car overlays))
                    (delete-overlay (car overlays)))
                (setq overlays (cdr overlays)))))
          ;; now we can use a new overlay
          (setq flyspell-overlay
                (make-flyspell-overlay beg end
				       'flyspell-incorrect-face
				       'highlight))))))

;*---------------------------------------------------------------------*/
;*    flyspell-highlight-duplicate-region ...                          */
;*---------------------------------------------------------------------*/
(defun flyspell-highlight-duplicate-region (beg end)
  "Set up an overlay on a duplicated word, in the buffer from BEG to END."
  (if (or flyspell-highlight-properties (not (flyspell-properties-at-p beg)))
      (progn
	;; we cleanup current overlay at the same position
	(if (and (not flyspell-persistent-highlight)
		 (overlayp flyspell-overlay))
	    (delete-overlay flyspell-overlay)
	  (let ((overlays (overlays-at beg)))
	    (while (consp overlays)
	      (if (flyspell-overlay-p (car overlays))
		  (delete-overlay (car overlays)))
	      (setq overlays (cdr overlays)))))
	;; now we can use a new overlay
	(setq flyspell-overlay
	      (make-flyspell-overlay beg end
				     'flyspell-duplicate-face
				     'highlight)))))

;*---------------------------------------------------------------------*/
;*    flyspell-auto-correct-cache ...                                  */
;*---------------------------------------------------------------------*/
(defvar flyspell-auto-correct-pos nil)
(defvar flyspell-auto-correct-region nil)
(defvar flyspell-auto-correct-ring nil)
(defvar flyspell-auto-correct-word nil)
(make-variable-buffer-local 'flyspell-auto-correct-pos)
(make-variable-buffer-local 'flyspell-auto-correct-region)
(make-variable-buffer-local 'flyspell-auto-correct-ring)
(make-variable-buffer-local 'flyspell-auto-correct-word)

;*---------------------------------------------------------------------*/
;*    flyspell-check-previous-highlighted-word ...                     */
;*---------------------------------------------------------------------*/
(defun flyspell-check-previous-highlighted-word (&optional arg)
  "Correct the closer misspelled word.
This function scans a mis-spelled word before the cursor. If it finds one
it proposes replacement for that word. With prefix arg, count that many
misspelled words backwards."
  (interactive)
  (let ((pos1 (point))
	(pos  (point))
	(arg  (if (or (not (numberp arg)) (< arg 1)) 1 arg))
	ov ovs)
    (if (catch 'exit
	  (while (and (setq pos (previous-overlay-change pos))
		      (not (= pos pos1)))
	    (setq pos1 pos)
	    (if (> pos (point-min))
		(progn
		  (setq ovs (overlays-at (1- pos)))
		  (while (consp ovs)
		    (setq ov (car ovs))
		    (setq ovs (cdr ovs))
		    (if (and (overlay-get ov 'flyspell-overlay)
			     (= 0 (setq arg (1- arg))))
			(throw 'exit t)))))))
	(save-excursion
	  (goto-char pos)
	  (ispell-word))
      (error "No word to correct before point"))))

;*---------------------------------------------------------------------*/
;*    flyspell-display-next-corrections ...                            */
;*---------------------------------------------------------------------*/
(defun flyspell-display-next-corrections (corrections)
  (let ((string "Corrections:")
	(l corrections)
	(pos '()))
    (while (< (length string) 80)
      (if (equal (car l) flyspell-auto-correct-word)
	  (setq pos (cons (+ 1 (length string)) pos)))
      (setq string (concat string " " (car l)))
      (setq l (cdr l)))
    (while (consp pos)
      (let ((num (car pos)))
	(put-text-property num
			   (+ num (length flyspell-auto-correct-word))
			   'face
			   'flyspell-incorrect-face
			   string))
      (setq pos (cdr pos)))
    (if (fboundp 'display-message)
	(display-message 'no-log string)
      (message string))))

;*---------------------------------------------------------------------*/
;*    flyspell-abbrev-table ...                                        */
;*---------------------------------------------------------------------*/
(defun flyspell-abbrev-table ()
  (if flyspell-use-global-abbrev-table-p
      global-abbrev-table
    local-abbrev-table))

;*---------------------------------------------------------------------*/
;*    flyspell-define-abbrev ...                                       */
;*---------------------------------------------------------------------*/
(defun flyspell-define-abbrev (name expansion)
  (let ((table (flyspell-abbrev-table)))
    (when table
      (define-abbrev table name expansion))))

;*---------------------------------------------------------------------*/
;*    flyspell-auto-correct-word ...                                   */
;*---------------------------------------------------------------------*/
(defun flyspell-auto-correct-word ()
  "Correct the current word.
This command proposes various successive corrections for the current word."
  (interactive)
  (let ((pos     (point))
	(old-max (point-max)))
    ;; use the correct dictionary
    (flyspell-accept-buffer-local-defs)
    (if (and (eq flyspell-auto-correct-pos pos)
	     (consp flyspell-auto-correct-region))
	;; we have already been using the function at the same location
	(let* ((start (car flyspell-auto-correct-region))
	       (len   (cdr flyspell-auto-correct-region)))
	  (flyspell-unhighlight-at start)
	  (delete-region start (+ start len))
	  (setq flyspell-auto-correct-ring (cdr flyspell-auto-correct-ring))
	  (let* ((word (car flyspell-auto-correct-ring))
		 (len  (length word)))
	    (rplacd flyspell-auto-correct-region len)
	    (goto-char start)
	    (if flyspell-abbrev-p
		(if (flyspell-already-abbrevp (flyspell-abbrev-table)
					      flyspell-auto-correct-word)
		    (flyspell-change-abbrev (flyspell-abbrev-table)
					    flyspell-auto-correct-word
					    word)
		  (flyspell-define-abbrev flyspell-auto-correct-word word)))
	    (funcall flyspell-insert-function word)
	    (flyspell-word)
	    (flyspell-display-next-corrections flyspell-auto-correct-ring))
	  (flyspell-ajust-cursor-point pos (point) old-max)
	  (setq flyspell-auto-correct-pos (point)))
      ;; fetch the word to be checked
      (let ((word (flyspell-get-word nil))
	    start end poss)
	;; destructure return word info list.
	(setq start (car (cdr word))
	      end (car (cdr (cdr word)))
	      word (car word))
	(setq flyspell-auto-correct-word word)
	;; now check spelling of word.
	(process-send-string ispell-process "%\n") ;put in verbose mode
	(process-send-string ispell-process (concat "^" word "\n"))
	;; wait until ispell has processed word
	(while (progn
		 (accept-process-output ispell-process)
		 (not (string= "" (car ispell-filter)))))
	(setq ispell-filter (cdr ispell-filter))
	(if (consp ispell-filter)
	    (setq poss (ispell-parse-output (car ispell-filter))))
	(cond ((or (eq poss t) (stringp poss))
	       ;; don't correct word
	       t)
	      ((null poss)
	       ;; ispell error
	       (error "Ispell: error in Ispell process"))
	      (t
	       ;; the word is incorrect, we have to propose a replacement
	       (let ((replacements (if flyspell-sort-corrections
				       (sort (car (cdr (cdr poss))) 'string<)
				     (car (cdr (cdr poss))))))
		 (setq flyspell-auto-correct-region nil)
		 (if (consp replacements)
		     (progn
		       (let ((replace (car replacements)))
			 (let ((new-word replace))
			   (if (not (equal new-word (car poss)))
			       (progn
				 ;; the save the current replacements
				 (setq flyspell-auto-correct-region
				       (cons start (length new-word)))
				 (let ((l replacements))
				   (while (consp (cdr l))
				     (setq l (cdr l)))
				   (rplacd l (cons (car poss) replacements)))
				 (setq flyspell-auto-correct-ring
				       replacements)
				 (flyspell-unhighlight-at start)
				 (delete-region start end)
				 (funcall flyspell-insert-function new-word)
				 (if flyspell-abbrev-p
				     (if (flyspell-already-abbrevp
					  (flyspell-abbrev-table) word)
					 (flyspell-change-abbrev
					  (flyspell-abbrev-table)
					  word
					  new-word)
				       (flyspell-define-abbrev word new-word)))
				 (flyspell-word)
				 (flyspell-display-next-corrections
				  (cons new-word flyspell-auto-correct-ring))
				 (flyspell-ajust-cursor-point pos
							      (point)
							      old-max))))))))))
	(setq flyspell-auto-correct-pos (point))
	(ispell-pdict-save t)))))

;*---------------------------------------------------------------------*/
;*    flyspell-auto-correct-previous-pos ...                           */
;*---------------------------------------------------------------------*/
(defvar flyspell-auto-correct-previous-pos nil
  "Holds the start of the first incorrect word before point.")

;*---------------------------------------------------------------------*/
;*    flyspell-auto-correct-previous-hook ...                          */
;*---------------------------------------------------------------------*/
(defun flyspell-auto-correct-previous-hook ()
  "Hook to track successive calls to `flyspell-auto-correct-previous-word'.
Sets flyspell-auto-correct-previous-pos to nil"
  (interactive)
  (remove-hook 'pre-command-hook (function flyspell-auto-correct-previous-hook) t)
  (unless (eq this-command (function flyspell-auto-correct-previous-word))
    (setq flyspell-auto-correct-previous-pos nil)))

;*---------------------------------------------------------------------*/
;*    flyspell-auto-correct-previous-word ...                          */
;*---------------------------------------------------------------------*/
(defun flyspell-auto-correct-previous-word (position)
  "*Auto correct the first mispelled word that occurs before point."
  (interactive "d")

  (add-hook 'pre-command-hook
	    (function flyspell-auto-correct-previous-hook) t t)

  (save-excursion
    (unless flyspell-auto-correct-previous-pos
      ;; only reset if a new overlay exists
      (setq flyspell-auto-correct-previous-pos nil)

      (let ((overlay-list (overlays-in (point-min) position))
	    (new-overlay 'dummy-value))

	;; search for previous (new) flyspell overlay
	(while (and new-overlay
		    (or (not (flyspell-overlay-p new-overlay))
			;; check if its face has changed
			(not (eq (get-char-property
				  (overlay-start new-overlay) 'face)
				 'flyspell-incorrect-face))))
	  (setq new-overlay (car-safe overlay-list))
	  (setq overlay-list (cdr-safe overlay-list)))

	;; if nothing new exits new-overlay should be nil
	(if new-overlay;; the length of the word may change so go to the start
	    (setq flyspell-auto-correct-previous-pos
		  (overlay-start new-overlay)))))

    (when flyspell-auto-correct-previous-pos
      (save-excursion
	(goto-char flyspell-auto-correct-previous-pos)
	(let ((ispell-following-word t));; point is at start
	  (if (numberp flyspell-auto-correct-previous-pos)
	      (goto-char flyspell-auto-correct-previous-pos))
	  (flyspell-auto-correct-word))
	;; the point may have moved so reset this
	(setq flyspell-auto-correct-previous-pos (point))))))

;*---------------------------------------------------------------------*/
;*    flyspell-correct-word ...                                        */
;*---------------------------------------------------------------------*/
(defun flyspell-correct-word (event)
  "Pop up a menu of possible corrections for a misspelled word.
The word checked is the word at the mouse position."
  (interactive "e")
  ;; use the correct dictionary
  (flyspell-accept-buffer-local-defs)
  ;; retain cursor location (I don't know why but save-excursion here fails).
  (let ((save (point)))
    (mouse-set-point event)
    (let ((cursor-location (point))
	  (word (flyspell-get-word nil))
	  start end poss replace)
      ;; destructure return word info list.
      (setq start (car (cdr word))
	    end (car (cdr (cdr word)))
	    word (car word))
      ;; now check spelling of word.
      (process-send-string ispell-process "%\n") ;put in verbose mode
      (process-send-string ispell-process (concat "^" word "\n"))
      ;; wait until ispell has processed word
      (while (progn
	       (accept-process-output ispell-process)
	       (not (string= "" (car ispell-filter)))))
      (setq ispell-filter (cdr ispell-filter))
      (if (consp ispell-filter)
	  (setq poss (ispell-parse-output (car ispell-filter))))
      (cond ((or (eq poss t) (stringp poss))
	     ;; don't correct word
	     t)
	    ((null poss)
	     ;; ispell error
	     (error "Ispell: error in Ispell process"))
	    ((string-match "GNU" (emacs-version))
	     ;; the word is incorrect, we have to propose a replacement
	     (setq replace (flyspell-emacs-popup event poss word))
	     (cond ((eq replace 'ignore)
		    (goto-char save)
		    nil)
		   ((eq replace 'save)
		    (goto-char save)
		    (process-send-string ispell-process (concat "*" word "\n"))
		    (flyspell-unhighlight-at cursor-location)
		    (setq ispell-pdict-modified-p '(t)))
		   ((or (eq replace 'buffer) (eq replace 'session))
		    (process-send-string ispell-process (concat "@" word "\n"))
		    (if (null ispell-pdict-modified-p)
			(setq ispell-pdict-modified-p
			      (list ispell-pdict-modified-p)))
		    (flyspell-unhighlight-at cursor-location)
		    (goto-char save)
		    (if (eq replace 'buffer)
			(ispell-add-per-file-word-list word)))
		   (replace
		    (flyspell-unhighlight-at cursor-location)
		    (let ((new-word (if (atom replace)
					replace
				      (car replace)))
			  (cursor-location (+ (- (length word) (- end start))
					      cursor-location)))
		      (if (not (equal new-word (car poss)))
			  (let ((old-max (point-max)))
			    (delete-region start end)
			    (funcall flyspell-insert-function new-word)
			    (if flyspell-abbrev-p
				(flyspell-define-abbrev word new-word))
			    (flyspell-ajust-cursor-point save
							 cursor-location
							 old-max)))))
		   (t
		    (goto-char save)
		    nil)))
	    ((eq flyspell-emacs 'xemacs)
	     (flyspell-xemacs-popup
	      event poss word cursor-location start end save)
	     (goto-char save)))
      (ispell-pdict-save t))))

;*---------------------------------------------------------------------*/
;*    flyspell-xemacs-correct ...                                      */
;*---------------------------------------------------------------------*/
(defun flyspell-xemacs-correct (replace poss word cursor-location start end save)
  "The xemacs popup menu callback."
  (cond ((eq replace 'ignore)
	 nil)
	((eq replace 'save)
	 (process-send-string ispell-process (concat "*" word "\n"))
	 (process-send-string ispell-process "#\n")
	 (flyspell-unhighlight-at cursor-location)
	 (setq ispell-pdict-modified-p '(t)))
	((or (eq replace 'buffer) (eq replace 'session))
	 (process-send-string ispell-process (concat "@" word "\n"))
	 (flyspell-unhighlight-at cursor-location)
	 (if (null ispell-pdict-modified-p)
	     (setq ispell-pdict-modified-p
		   (list ispell-pdict-modified-p)))
	 (if (eq replace 'buffer)
	     (ispell-add-per-file-word-list word)))
	(replace
	 (let ((old-max (point-max))
	       (new-word (if (atom replace)
			     replace
			   (car replace)))
	       (cursor-location (+ (- (length word) (- end start))
				   cursor-location)))
	   (if (not (equal new-word (car poss)))
	       (progn
		 (delete-region start end)
		 (goto-char start)
		 (funcall flyspell-insert-function new-word)
		 (if flyspell-abbrev-p
		     (flyspell-define-abbrev word new-word))))
	   (flyspell-ajust-cursor-point save cursor-location old-max)))))

;*---------------------------------------------------------------------*/
;*    flyspell-ajust-cursor-point ...                                  */
;*---------------------------------------------------------------------*/
(defun flyspell-ajust-cursor-point (save cursor-location old-max)
  (if (>= save cursor-location)
      (let ((new-pos (+ save (- (point-max) old-max))))
	(goto-char (cond
		    ((< new-pos (point-min))
		     (point-min))
		    ((> new-pos (point-max))
		     (point-max))
		    (t new-pos))))
    (goto-char save)))

;*---------------------------------------------------------------------*/
;*    flyspell-emacs-popup ...                                         */
;*---------------------------------------------------------------------*/
(defun flyspell-emacs-popup (event poss word)
  "The Emacs popup menu."
  (if (not event)
      (let* ((mouse-pos  (mouse-position))
	     (mouse-pos  (if (nth 1 mouse-pos)
			     mouse-pos
			   (set-mouse-position (car mouse-pos)
				 	       (/ (frame-width) 2) 2)
			   (unfocus-frame)
			   (mouse-position))))
	(setq event (list (list (car (cdr mouse-pos))
				(1+ (cdr (cdr mouse-pos))))
			  (car mouse-pos)))))
  (let* ((corrects   (if flyspell-sort-corrections
			 (sort (car (cdr (cdr poss))) 'string<)
		       (car (cdr (cdr poss)))))
	 (cor-menu   (if (consp corrects)
			 (mapcar (lambda (correct)
				   (list correct correct))
				 corrects)
		       '()))
	 (affix      (car (cdr (cdr (cdr poss)))))
	 (base-menu  (let ((save (if (consp affix)
				     (list
				      (list (concat "Save affix: " (car affix))
					    'save)
				      '("Accept (session)" session)
				      '("Accept (buffer)" buffer))
				   '(("Save word" save)
				     ("Accept (session)" session)
				     ("Accept (buffer)" buffer)))))
		       (if (consp cor-menu)
			   (append cor-menu (cons "" save))
			 save)))
	 (menu       (cons "flyspell correction menu" base-menu)))
    (car (x-popup-menu event
		       (list (format "%s [%s]" word (or ispell-local-dictionary
							ispell-dictionary))
			     menu)))))

;*---------------------------------------------------------------------*/
;*    flyspell-xemacs-popup ...                                        */
;*---------------------------------------------------------------------*/
(defun flyspell-xemacs-popup (event poss word cursor-location start end save)
  "The XEmacs popup menu."
  (let* ((corrects   (if flyspell-sort-corrections
			 (sort (car (cdr (cdr poss))) 'string<)
		       (car (cdr (cdr poss)))))
	 (cor-menu   (if (consp corrects)
			 (mapcar (lambda (correct)
				   (vector correct
					   (list 'flyspell-xemacs-correct
						 correct
						 (list 'quote poss)
						 word
						 cursor-location
						 start
						 end
						 save)
					   t))
				 corrects)
		       '()))
	 (affix      (car (cdr (cdr (cdr poss)))))
	 (menu       (let ((save (if (consp affix)
				     (vector
				      (concat "Save affix: " (car affix))
				      (list 'flyspell-xemacs-correct
					    ''save
					    (list 'quote poss)
					    word
					    cursor-location
					    start
					    end
					    save)
				      t)
				   (vector
				    "Save word"
				    (list 'flyspell-xemacs-correct
					  ''save
					  (list 'quote poss)
					  word
					  cursor-location
					  start
					  end
					  save)
				    t)))
			   (session (vector "Accept (session)"
					    (list 'flyspell-xemacs-correct
						  ''session
						  (list 'quote poss)
						  word
						  cursor-location
						  start
						  end
						  save)
					    t))
			   (buffer  (vector "Accept (buffer)"
					    (list 'flyspell-xemacs-correct
						  ''buffer
						  (list 'quote poss)
						  word
						  cursor-location
						  start
						  end
						  save)
					    t)))
		       (if (consp cor-menu)
			   (append cor-menu (list "-" save session buffer))
			 (list save session buffer)))))
    (popup-menu (cons (format "%s [%s]" word (or ispell-local-dictionary
						 ispell-dictionary))
		      menu))))

;*---------------------------------------------------------------------*/
;*    Some example functions for real autocorrecting                   */
;*---------------------------------------------------------------------*/
(defun flyspell-maybe-correct-transposition (beg end poss)
  "Check replacements for transposed characters.

If the text between BEG and END is equal to a correction suggested by
Ispell, after transposing two adjacent characters, correct the text,
and return t.

The third arg POSS is either the symbol 'doublon' or a list of
possible corrections as returned by 'ispell-parse-output'.

This function is meant to be added to 'flyspell-incorrect-hook'."
  (when (consp poss)
    (let ((temp-buffer (get-buffer-create " *flyspell-temp*"))
	  found)
    (save-excursion
      (copy-to-buffer temp-buffer beg end)
      (set-buffer temp-buffer)
      (goto-char (1+ (point-min)))
      (while (and (not (eobp)) (not found))
	  (transpose-chars 1)
	  (if (member (buffer-string) (nth 2 poss))
	      (setq found (point))
	    (transpose-chars -1)
	    (forward-char))))
    (when found
      (save-excursion
	(goto-char (+ beg found -1))
	(transpose-chars -1)
	t)))))

(defun flyspell-maybe-correct-doubling (beg end poss)
  "Check replacements for doubled characters.

If the text between BEG and END is equal to a correction suggested by
Ispell, after removing a pair of doubled characters, correct the text,
and return t.

The third arg POSS is either the symbol 'doublon' or a list of
possible corrections as returned by 'ispell-parse-output'.

This function is meant to be added to 'flyspell-incorrect-hook'."
  (when (consp poss)
    (let ((temp-buffer (get-buffer-create " *flyspell-temp*"))
	  found)
    (save-excursion
      (copy-to-buffer temp-buffer beg end)
      (set-buffer temp-buffer)
      (goto-char (1+ (point-min)))
      (while (and (not (eobp)) (not found))
	(when (char-equal (char-after) (char-before))
	  (delete-char 1)
	  (if (member (buffer-string) (nth 2 poss))
	      (setq found (point))
	    (insert-char (char-before) 1)))
	(forward-char)))
    (when found
      (save-excursion
	(goto-char (+ beg found -1))
	(delete-char 1)
	t)))))

;*---------------------------------------------------------------------*/
;*    flyspell-already-abbrevp ...                                     */
;*---------------------------------------------------------------------*/
(defun flyspell-already-abbrevp (table word)
  (let ((sym (abbrev-symbol word table)))
    (and sym (symbolp sym))))

;*---------------------------------------------------------------------*/
;*    flyspell-change-abbrev ...                                       */
;*---------------------------------------------------------------------*/
(defun flyspell-change-abbrev (table old new)
  (set (abbrev-symbol old table) new))

(provide 'flyspell)

;;; flyspell.el ends here
