;;; flyspell.el --- On-the-fly spell checker

;; Copyright (C) 1998, 2000 Free Software Foundation, Inc.

;; Author: Manuel Serrano <Manuel.Serrano@unice.fr>
;; Keywords: convenience

;;; This file is part of GNU Emacs.

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

;;; commentary:
;;
;; Flyspell is a minor Emacs mode performing on-the-fly spelling
;; checking.
;;                                                                  
;; To enable Flyspell minor mode, type Meta-x flyspell-mode.
;; This applies only to the current buffer.
;;                                                                  
;; Note: consider setting the variable ispell-parser to `tex' to
;; avoid TeX command checking; use `(setq ispell-parser 'tex)'
;; _before_ entering flyspell.
;;                                                                  
;; Some user variables control the behavior of flyspell.  They are
;; those defined under the `User variables' comment.
;; 
;; Note: as suggested by Yaron M. Minsky, if you use flyspell when
;; sending mails, you should add the following:
;;    (add-hook 'mail-send-hook 'flyspell-mode-off)

;;; Code:
(require 'ispell)

;*---------------------------------------------------------------------*/
;*    Group ...                                                        */
;*---------------------------------------------------------------------*/
(defgroup flyspell nil
  "Spellchecking on the fly."
  :tag "FlySpell"
  :prefix "flyspell-"
  :group 'processes
  :version "20.3")

;*---------------------------------------------------------------------*/
;*    User variables ...                                               */
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

(defcustom flyspell-sort-corrections t
  "*Non-nil means, sort the corrections alphabetically before popping them."
  :group 'flyspell
  :type 'boolean)

(defcustom flyspell-duplicate-distance 10000
  "*The maximum distance for finding duplicates of unrecognized words.
This applies to the feature that when a word is not found in the dictionary,
if the same spelling occurs elsewhere in the buffer,
Flyspell uses a different face (`flyspell-duplicate-face') to highlight it.
This variable specifies how far to search to find such a duplicate.
-1 means no limit (search the whole buffer).
0 means do not search for duplicate unrecognized spellings."
  :group 'flyspell
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
    delete-char)
  "The standard list of delayed commands for Flyspell.
See `flyspell-delayed-commands'."
  :group 'flyspell
  :type '(repeat (symbol)))

(defcustom flyspell-delayed-commands nil
  "List of commands that are \"delayed\" for Flyspell mode.
After these commands, Flyspell checking is delayed for a short time,
whose length is specified by `flyspell-delay'."
  :group 'flyspell
  :type '(repeat (symbol)))

(defcustom flyspell-issue-welcome-flag t
  "*Non-nil means that Flyspell should display a welcome message when started."
  :group 'flyspell
  :type 'boolean)

(defcustom flyspell-consider-dash-as-word-delimiter-flag nil
  "*Non-nil means that the `-' char is considered as a word delimiter."
  :group 'flyspell
  :type 'boolean)

(defcustom flyspell-incorrect-hook nil
  "*List of functions to be called when incorrect words are encountered.
Each function is given two arguments: the beginning and the end
of the incorrect region."
  :group 'flyspell)

(defcustom flyspell-multi-language-p nil
  "*Non-nil means that Flyspell can be used with multiple languages.
This mode works by starting a separate Ispell process for each buffer,
so that each buffer can use its own language."
  :group 'flyspell
  :type 'boolean)

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

(put 'mail-mode 'flyspell-mode-predicate 'mail-mode-flyspell-verify)
(put 'message-mode 'flyspell-mode-predicate 'mail-mode-flyspell-verify)
(defun mail-mode-flyspell-verify ()
  "This function is used for `flyspell-generic-check-word-p' in Mail mode."
  (save-excursion
    (or (progn
	  (beginning-of-line)
	  (looking-at "Subject:"))
	(not (or (re-search-forward mail-header-separator nil t)
		 (re-search-backward message-signature-separator nil t)
		 (progn
		   (beginning-of-line)
		   (looking-at "[>}|]")))))))

(put 'texinfo-mode 'flyspell-mode-predicate 'texinfo-mode-flyspell-verify)
(defun texinfo-mode-flyspell-verify ()
  "This function is used for `flyspell-generic-check-word-p' in Texinfo mode."
  (save-excursion
    (forward-word -1)
    (not (looking-at "@"))))

;*---------------------------------------------------------------------*/
;*    Overlay compatibility                                            */
;*---------------------------------------------------------------------*/
(autoload 'make-overlay        "overlay" "" t)
(autoload 'move-overlay        "overlay" "" t)
(autoload 'overlayp            "overlay" "" t)
(autoload 'overlay-properties  "overlay" "" t)
(autoload 'overlays-in         "overlay" "" t)
(autoload 'delete-overlay      "overlay" "" t)
(autoload 'overlays-at         "overlay" "" t)
(autoload 'overlay-put         "overlay" "" t)
(autoload 'overlay-get         "overlay" "" t)

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
(defvar flyspell-mode nil)
(make-variable-buffer-local 'flyspell-mode)

(defvar flyspell-mouse-map
  (let ((map (make-sparse-keymap)))
    (cond
     ((eq flyspell-emacs 'xemacs)
      (define-key map [(button2)]
	#'flyspell-correct-word/mouse-keymap)
      (define-key flyspell-mouse-map "\M-\t" #'flyspell-auto-correct-word))
     (flyspell-use-local-map
      (define-key map [(mouse-2)] #'flyspell-correct-word/mouse-keymap)
      (define-key map "\M-\t" #'flyspell-auto-correct-word)))
    map))
(defvar flyspell-mode-map (make-sparse-keymap))

(or (assoc 'flyspell-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(flyspell-mode " Fly") minor-mode-alist)))

;; mouse or local-map bindings
(when (or (assoc 'flyspell-mode minor-mode-map-alist)
	   (setq minor-mode-map-alist
		 (cons (cons 'flyspell-mode flyspell-mode-map)
		       minor-mode-map-alist)))
   (define-key flyspell-mode-map "\M-\t" 'flyspell-auto-correct-word)
   (define-key flyspell-mode-map [(mouse-2)]
     (function flyspell-correct-word/local-keymap)))

;; the name of the overlay property that defines the keymap
(defvar flyspell-overlay-keymap-property-name
  (if (string-match "19.*XEmacs" emacs-version)
      'keymap
    'local-map))
   
;*---------------------------------------------------------------------*/
;*    Highlighting                                                     */
;*---------------------------------------------------------------------*/
(defface flyspell-incorrect-face
  '((((class color)) (:foreground "OrangeRed" :bold t :underline t))
    (t (:bold t)))
  "Face used for marking a misspelled word in Flyspell."
  :group 'flyspell)

(defface flyspell-duplicate-face
  '((((class color)) (:foreground "Gold3" :bold t :underline t))
    (t (:bold t)))
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
flyspell-mode-hook is run after flyspell is entered.

Remark:
`flyspell-mode' uses `ispell-mode'.  Thus all Ispell options are
valid.  For instance, a personal dictionary can be used by
invoking `ispell-change-dictionary'.

Consider using the `ispell-parser' to check your text.  For instance
consider adding:
\(add-hook 'tex-mode-hook (function (lambda () (setq ispell-parser 'tex))))
in your .emacs file.

flyspell-region checks all words inside a region.

flyspell-buffer checks the whole buffer."
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
;*    flyspell-mode-on ...                                             */
;*---------------------------------------------------------------------*/
(eval-when-compile (defvar flyspell-local-mouse-map))

(defun flyspell-mode-on ()
  "Turn Flyspell mode on.  Do not use this; use `flyspell-mode' instead."
  (setq ispell-highlight-face 'flyspell-incorrect-face)
  ;; ispell initialization
  (if flyspell-multi-language-p
      (progn
	(make-variable-buffer-local 'ispell-dictionary)
	(make-variable-buffer-local 'ispell-process)
	(make-variable-buffer-local 'ispell-filter)
	(make-variable-buffer-local 'ispell-filter-continue)
	(make-variable-buffer-local 'ispell-process-directory)
	(make-variable-buffer-local 'ispell-parser)
	(put 'ispell-dictionary 'permanent-local t)
	(put 'ispell-process 'permanent-local t)
	(put 'ispell-filter 'permanent-local t)
	(put 'ispell-filter-continue 'permanent-local t)
	(put 'ispell-process-directory 'permanent-local t)
	(put 'ispell-parser 'permanent-local t)))
  ;; We put the `flyspell-delayed' property on some commands.
  (flyspell-delay-commands)
  ;; we bound flyspell action to post-command hook
  (make-local-hook 'post-command-hook)
  (add-hook 'post-command-hook (function flyspell-post-command-hook) t t)
  ;; we bound flyspell action to pre-command hook
  (make-local-hook 'pre-command-hook)
  (add-hook 'pre-command-hook (function flyspell-pre-command-hook) t t)

  ;; Set flyspell-generic-check-word-p based on the major mode.
  (let ((mode-predicate (get major-mode 'flyspell-mode-predicate)))
    (if mode-predicate
	(setq flyspell-generic-check-word-p mode-predicate)))
 
  ;; the welcome message
  (if flyspell-issue-welcome-flag
      (let ((binding (where-is-internal 'flyspell-auto-correct-word
					nil 'non-ascii)))
	(message
	 (if binding
	     (format "Welcome to flyspell.  Use %s or Mouse-2 to correct words."
		     (key-description binding))
	   "Welcome to flyspell.  Use Mouse-2 to correct words."))))
  ;; we have to kill the flyspell process when the buffer is deleted.
  ;; (thanks to Jeff Miller and Roland Rosenfeld who sent me this
  ;; improvement).
  (add-hook 'kill-buffer-hook
	    (lambda ()
	      (if (and flyspell-multi-language-p ispell-process)
		  (ispell-kill-ispell t))))
  (make-local-hook 'change-major-mode-hook)
  (add-hook 'change-major-mode-hook 'flyspell-mode-off)
  ;; Use this so that we can still get major mode bindings at a
  ;; misspelled word (unless they're overridden by
  ;; `flyspell-mouse-map').
  (set (make-local-variable 'flyspell-local-mouse-map)
       (let ((map (copy-keymap flyspell-mouse-map)))
	 (if (eq flyspell-emacs 'xemacs)
	     (set-keymap-parents (list (current-local-map)))
	   (set-keymap-parent map (current-local-map)))
	 map))
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
as been used the current word is not immediatly checked.
It will be checked only after `flyspell-delay' seconds."
  (interactive "SDelay Flyspell after Command: ")
  (put command 'flyspell-delayed t))

;*---------------------------------------------------------------------*/
;*    flyspell-ignore-commands ...                                     */
;*---------------------------------------------------------------------*/
(defun flyspell-ignore-commands ()
  "This is an obsolete function, use `flyspell-delay-commands' instead."
  (flyspell-delay-commands))

;*---------------------------------------------------------------------*/
;*    flyspell-ignore-command ...                                      */
;*---------------------------------------------------------------------*/
(defun flyspell-ignore-command (command)
  "This is an obsolete function, use `flyspell-delay-command' instead.
COMMAND is the name of the command to be delayed."
  (flyspell-delay-command command))

(make-obsolete 'flyspell-ignore-commands 'flyspell-delay-commands)
(make-obsolete 'flyspell-ignore-command 'flyspell-delay-command)

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
(defvar flyspell-pre-buffer nil)
(defvar flyspell-pre-point  nil)

;*---------------------------------------------------------------------*/
;*    flyspell-pre-command-hook ...                                    */
;*---------------------------------------------------------------------*/
(defun flyspell-pre-command-hook ()
  "Save the current buffer and point for Flyspell's post-command hook."
  (interactive)
  (setq flyspell-pre-buffer (current-buffer))
  (setq flyspell-pre-point  (point)))

;*---------------------------------------------------------------------*/
;*    flyspell-mode-off ...                                            */
;*---------------------------------------------------------------------*/
;;;###autoload
(defun flyspell-mode-off ()
  "Turn Flyspell mode off."
  ;; If we have an Ispell process for each buffer,
  ;; kill the one for this buffer.
  (if flyspell-multi-language-p
      (ispell-kill-ispell t))
  ;; we remove the hooks
  (remove-hook 'post-command-hook (function flyspell-post-command-hook) t)
  (remove-hook 'pre-command-hook (function flyspell-pre-command-hook) t)
  ;; we remove all the flyspell hilightings
  (flyspell-delete-all-overlays)
  ;; we have to erase pre cache variables
  (setq flyspell-pre-buffer nil)
  (setq flyspell-pre-point  nil)
  ;; we mark the mode as killed
  (setq flyspell-mode nil))

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
   ((not (and (symbolp this-command) (get this-command 'flyspell-delayed)))
    ;; the current command is not delayed, that
    ;; is that we must check the word now
    t)
   ((and (> (point) (point-min))
	 (save-excursion
	   (backward-char 1)
	   (and (looking-at (flyspell-get-not-casechars))
		(or flyspell-consider-dash-as-word-delimiter-flag
		    (not (looking-at "\\-"))))))
    ;; yes because we have reached or typed a word delimiter.
    t)
   ((not (integerp flyspell-delay))
    ;; yes because the user had set up a no-delay configuration.
    t)
   (executing-kbd-macro
    ;; Don't delay inside a keyboard macro.
    t)
   (t
    (if (fboundp 'about-xemacs)
	(sit-for flyspell-delay nil)
      (sit-for flyspell-delay 0 nil)))))

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
   ((or (and (= flyspell-pre-point (- (point) 1))
	     (eq (char-syntax (char-after flyspell-pre-point)) ?w))
	(= flyspell-pre-point (point))
	(= flyspell-pre-point (+ (point) 1)))
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
;*    flyspell-post-command-hook ...                                   */
;*---------------------------------------------------------------------*/
(defun flyspell-post-command-hook ()
  "The `post-command-hook' used by flyspell to check a word in-the-fly."
  (interactive)
  (if (flyspell-check-word-p)
      (flyspell-word))
  (if (flyspell-check-pre-word-p)
      (save-excursion
	(set-buffer flyspell-pre-buffer)
	(save-excursion
	  (goto-char flyspell-pre-point)
	  (flyspell-word)))))

;*---------------------------------------------------------------------*/
;*    flyspell-word ...                                                */
;*---------------------------------------------------------------------*/
(defun flyspell-word (&optional following)
  "Spell check a word."
  (interactive (list current-prefix-arg))
  (if (interactive-p)
      (setq following ispell-following-word))
  (save-excursion
    (ispell-accept-buffer-local-defs)	; use the correct dictionary
    (let ((cursor-location (point))	; retain cursor location
	  (word (flyspell-get-word following))
	  start end poss)
      (if (or (eq word nil)
 	      (and (fboundp flyspell-generic-check-word-p)
 		   (not (funcall flyspell-generic-check-word-p))))
	  t
	(progn
	  ;; destructure return word info list.
	  (setq start (car (cdr word))
		end (car (cdr (cdr word)))
		word (car word))
	  ;; before checking in the directory, we check for doublons.
	  (cond
	   ((and flyspell-mark-duplications-flag
		 (save-excursion
		   (goto-char start)
		   (word-search-backward word
					 (- start
					    (+ 1 (- end start)))
					 t)))
	    ;; yes, this is a doublon
	    (flyspell-highlight-incorrect-region start end))
	   ((and (eq flyspell-word-cache-start start)
		 (eq flyspell-word-cache-end end)
		 (string-equal flyspell-word-cache-word word))
	    ;; this word had been already checked, we skip
	    nil)
	   ((and (eq ispell-parser 'tex)
		 (flyspell-tex-command-p word))
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
	    (if (listp ispell-filter)
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
		       (flyspell-unhighlight-at (- end 1)))
		   (message "Error in ispell process"))
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
		       (flyspell-highlight-incorrect-region start end)
		     (message (format "mispelling `%s'" word)))))
	    (goto-char cursor-location) ; return to original location
	    (if ispell-quit (setq ispell-quit nil)))))))))

;*---------------------------------------------------------------------*/
;*    flyspell-tex-command-p ...                                       */
;*---------------------------------------------------------------------*/
(defun flyspell-tex-command-p (word)
  "Return t if WORD is a TeX command."
  (eq (aref word 0) ?\\))

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
     ((eq ispell-casechars flyspell-ispell-casechars-cache)
      flyspell-casechars-cache)
     ((not (eq ispell-parser 'tex))
      (setq flyspell-ispell-casechars-cache ispell-casechars)
      (setq flyspell-casechars-cache
	    (concat (substring ispell-casechars
			       0
			       (- (length ispell-casechars) 1))
		    "{}]"))
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
     ((eq ispell-not-casechars flyspell-ispell-not-casechars-cache)
      flyspell-not-casechars-cache)
     ((not (eq ispell-parser 'tex))
      (setq flyspell-ispell-not-casechars-cache ispell-not-casechars)
      (setq flyspell-not-casechars-cache
	    (concat (substring ispell-not-casechars
			       0
			       (- (length ispell-not-casechars) 1))
		    "{}]"))
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
If optional argument FOLLOWING is non-nil or if `ispell-following-word'
is non-nil when called interactively, then the following word
\(rather than preceding\) is checked when the cursor is not over a word.
Optional second argument contains otherchars that can be included in word
many times.

Word syntax described by `ispell-dictionary-alist' (which see)."
  (let* ((flyspell-casechars (flyspell-get-casechars))
	 (flyspell-not-casechars (flyspell-get-not-casechars))
	 (ispell-otherchars (ispell-get-otherchars))
	 (ispell-many-otherchars-p (ispell-get-many-otherchars-p))
	 (word-regexp (if (not (string= "" ispell-otherchars))
			  (concat 
			   flyspell-casechars
			   "+\\("
			   ispell-otherchars
			   "?"
			   flyspell-casechars
			   "+\\)"
			   (if ispell-many-otherchars-p
			       "*" "?"))
			(concat flyspell-casechars "+")))
	 (tex-prelude "[\\\\{]")
	 (tex-regexp  (if (eq ispell-parser 'tex)
			  (concat tex-prelude "?" word-regexp "}?")
			word-regexp))
		      
	 did-it-once
	 start end word)
    ;; find the word
    (if (not (or (looking-at flyspell-casechars)
		 (and (eq ispell-parser 'tex)
		      (looking-at tex-prelude))))
	(if following
	    (re-search-forward flyspell-casechars (point-max) t)
	  (re-search-backward flyspell-casechars (point-min) t)))
    ;; move to front of word
    (re-search-backward flyspell-not-casechars (point-min) 'start)
    (if (not (string= "" ispell-otherchars))
	(let ((pos nil))
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
    (if (eq (re-search-forward tex-regexp (point-max) t) nil)
	nil
      (progn
	(setq start (match-beginning 0)
	      end (point)
	      word (buffer-substring start end))
	(list word start end)))))

;*---------------------------------------------------------------------*/
;*    flyspell-region ...                                              */
;*---------------------------------------------------------------------*/
(defun flyspell-region (beg end)
  "Flyspell text between BEG and END."
  (interactive "r")
  (save-excursion
  (if (> beg end)
      (let ((old beg))
	(setq beg end)
	(setq end old)))
    (goto-char beg)
    (let ((count 0))
      (while (< (point) end)
	(if (= count 100)
	    (progn
	      (message "Spell Checking...%d%%"
		       (* 100 (/ (float (- (point) beg)) (- end beg))))
	      (setq count 0))
	  (setq count (+ 1 count)))
	(flyspell-word)
	(let ((cur (point)))
	  (forward-word 1)
	  (if (and (< (point) end) (> (point) (+ cur 1)))
	      (backward-char 1)))))
    (backward-char 1)
    (message "Spell Checking...done")
    (flyspell-word)))

;*---------------------------------------------------------------------*/
;*    flyspell-buffer ...                                              */
;*---------------------------------------------------------------------*/
(defun flyspell-buffer ()
  "Flyspell whole buffer."
  (interactive)
  (flyspell-region (point-min) (point-max)))

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
    (delete-overlay flyspell-overlay)))

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
    (if flyspell-use-local-map
	(overlay-put flyspell-overlay
		     flyspell-overlay-keymap-property-name
		     flyspell-local-mouse-map))))
    
;*---------------------------------------------------------------------*/
;*    flyspell-highlight-incorrect-region ...                          */
;*---------------------------------------------------------------------*/
(defun flyspell-highlight-incorrect-region (beg end)
  "Set up an overlay on a misspelled word, in the buffer from BEG to END."
  (run-hook-with-args 'flyspell-incorrect-hook beg end)
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
				     'flyspell-incorrect-face 'highlight)))))

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
				     'flyspell-duplicate-face 'highlight)))))

;*---------------------------------------------------------------------*/
;*    flyspell-auto-correct-cache ...                                  */
;*---------------------------------------------------------------------*/
(defvar flyspell-auto-correct-pos nil)
(defvar flyspell-auto-correct-region nil)
(defvar flyspell-auto-correct-ring nil)

;*---------------------------------------------------------------------*/
;*    flyspell-auto-correct-word ...                                   */
;*---------------------------------------------------------------------*/
(defun flyspell-auto-correct-word (pos)
  "Correct the word at POS.
This command proposes various successive corrections for the word at POS.
The variable `flyspell-auto-correct-binding' specifies the key to bind
to this command."
  (interactive "d")
  ;; use the correct dictionary
  (ispell-accept-buffer-local-defs)
  (if (eq flyspell-auto-correct-pos pos)
      ;; we have already been using the function at the same location
      (progn
	(save-excursion
	  (let ((start (car flyspell-auto-correct-region))
		(len   (cdr flyspell-auto-correct-region)))
	    (delete-region start (+ start len))
	    (setq flyspell-auto-correct-ring (cdr flyspell-auto-correct-ring))
	    (let* ((word (car flyspell-auto-correct-ring))
		   (len  (length word)))
	      (rplacd flyspell-auto-correct-region len)
	      (goto-char start)
	      (insert word))))
	(setq flyspell-auto-correct-pos (point)))
    ;; retain cursor location
    (let ((cursor-location pos)
	  (word (flyspell-get-word nil))
	  start end poss)
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
      (if (listp ispell-filter)
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
	       (if (consp replacements)
		   (progn
		     (let ((replace (car replacements)))
		       (setq word replace)
		       (setq cursor-location (+ (- (length word) (- end start))
						cursor-location))
		       (if (not (equal word (car poss)))
			   (progn
			     ;; the save the current replacements
			     (setq flyspell-auto-correct-pos cursor-location)
			     (setq flyspell-auto-correct-region
				   (cons start (length word)))
			     (let ((l replacements))
			       (while (consp (cdr l))
				 (setq l (cdr l)))
			       (rplacd l (cons (car poss) replacements)))
			     (setq flyspell-auto-correct-ring
				   (cdr replacements))
			     (delete-region start end)
			     (insert word)))))))))
      ;; return to original location
      (goto-char cursor-location)
      (ispell-pdict-save t))))

;*---------------------------------------------------------------------*/
;*    flyspell-correct-word ...                                        */
;*---------------------------------------------------------------------*/
(defun flyspell-correct-word (event)
  "Check spelling of word under or before the cursor.
If the word is not found in dictionary, display possible corrections
in a popup menu allowing you to choose one.

Word syntax described by `ispell-dictionary-alist' (which see).

This will check or reload the dictionary.  Use \\[ispell-change-dictionary]
or \\[ispell-region] to update the Ispell process."
  (interactive "e")
  (if flyspell-use-local-map
      (flyspell-correct-word/mouse-keymap event)
    (flyspell-correct-word/local-keymap event)))
    
;*---------------------------------------------------------------------*/
;*    flyspell-correct-word/local-keymap ...                           */
;*---------------------------------------------------------------------*/
(defun flyspell-correct-word/local-keymap (event)
  "emacs 19.xx seems to be buggous. Overlay keymap does not seems
to work correctly with local map. That is, if a key is not
defined for the overlay keymap, the current local map, is not
checked. The binding is resolved with the global map. The
consequence is that we can not use overlay map with flyspell."
  (interactive "e")
  (save-window-excursion
    (let ((save (point)))
      (mouse-set-point event)
      ;; we look for a flyspell overlay here
      (let ((overlays (overlays-at (point)))
	    (overlay  nil))
	(while (consp overlays)
	  (if (flyspell-overlay-p (car overlays))
	      (progn
		(setq overlay (car overlays))
		(setq overlays nil))
	    (setq overlays (cdr overlays))))
	;; we return to the correct location
	(goto-char save)
	;; we check to see if button2 has been used overlay a
	;; flyspell overlay
	(if overlay
	    ;; yes, so we use the flyspell function
	    (flyspell-correct-word/mouse-keymap event)
	  ;; no so we have to use the non flyspell binding
	  (let ((flyspell-mode nil))
	    (if (key-binding (this-command-keys))
		(command-execute (key-binding (this-command-keys))))))))))
  
;*---------------------------------------------------------------------*/
;*    flyspell-correct-word ...                                        */
;*---------------------------------------------------------------------*/
(defun flyspell-correct-word/mouse-keymap (event)
  "Pop up a menu of possible corrections for a misspelled word.
The word checked is the word at the mouse position."
  (interactive "e")
  ;; use the correct dictionary
  (ispell-accept-buffer-local-defs)
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
      (if (listp ispell-filter)
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
		    nil)
		   ((eq replace 'save)
		    (process-send-string ispell-process (concat "*" word "\n"))
		    (flyspell-unhighlight-at cursor-location)
		    (setq ispell-pdict-modified-p '(t)))
		   ((or (eq replace 'buffer) (eq replace 'session))
		    (process-send-string ispell-process (concat "@" word "\n"))
		    (if (null ispell-pdict-modified-p)
			(setq ispell-pdict-modified-p
			      (list ispell-pdict-modified-p)))
		    (flyspell-unhighlight-at cursor-location)
		    (if (eq replace 'buffer)
			(ispell-add-per-file-word-list word)))
		   (replace
		    (setq word (if (atom replace) replace (car replace))
			  cursor-location (+ (- (length word) (- end start))
					     cursor-location))
		    (if (not (equal word (car poss)))
			(progn
			  (delete-region start end)
			  (insert word))))))
	    ((eq flyspell-emacs 'xemacs)
	     (flyspell-xemacs-popup
	      event poss word cursor-location start end)))
      (ispell-pdict-save t))
    (if (< save (point-max))
	(goto-char save)
      (goto-char (point-max)))))

;*---------------------------------------------------------------------*/
;*    flyspell-xemacs-correct ...                                      */
;*---------------------------------------------------------------------*/
(defun flyspell-xemacs-correct (replace poss word cursor-location start end)
  "The xemacs popup menu callback."
  (cond ((eq replace 'ignore)
	 nil)
	((eq replace 'save)
	 (process-send-string ispell-process (concat "*" word "\n"))
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
	 (setq word (if (atom replace) replace (car replace))
	       cursor-location (+ (- (length word) (- end start))
				  cursor-location))
	 (if (not (equal word (car poss)))
	     (save-excursion
	       (delete-region start end)
	       (goto-char start)
	       (insert word))))))

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
				      '("Accept (session)" accept)
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
(defun flyspell-xemacs-popup (event poss word cursor-location start end)
  "The xemacs popup menu."
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
						 end)
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
					    end)
				      t)
				   (vector
				    "Save word"
				    (list 'flyspell-xemacs-correct
					  ''save
					  (list 'quote poss)
					  word
					  cursor-location
					  start
					  end)
				    t)))
			   (session (vector "Accept (session)"
					    (list 'flyspell-xemacs-correct
						  ''session
						  (list 'quote poss)
						  word
						  cursor-location
						  start
						  end)
					    t))
			   (buffer  (vector "Accept (buffer)"
					    (list 'flyspell-xemacs-correct
						  ''buffer
						  (list 'quote poss)
						  word
						  cursor-location
						  start
						  end)
					    t)))
		       (if (consp cor-menu)
			   (append cor-menu (list "-" save session buffer))
			 (list save session buffer)))))
    (popup-menu (cons (format "%s [%s]" word (or ispell-local-dictionary
						 ispell-dictionary))
		      menu))))

(provide 'flyspell)

;;; flyspell.el ends here
