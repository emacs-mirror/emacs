;;; quail.el --- Provides simple input method for multilingual text

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Author: Kenichi HANDA <handa@etl.go.jp>
;;	   Naoto TAKAHASHI <ntakahas@etl.go.jp>
;; Maintainer: Kenichi HANDA <handa@etl.go.jp>
;; Keywords: mule, multilingual, input method

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

;; In Quail minor mode, you can input multilingual text easily.  By
;; defining a translation table (named Quail map) which maps ASCII key
;; string to multilingual character or string, you can input any text
;; from ASCII keyboard.
;;
;; We use words "translation" and "conversion" differently.  The
;; former is done by Quail package itself, the latter is the further
;; process of converting a translated text to some more desirable
;; text.  For instance, Quail package for Japanese (`quail-jp')
;; translates Roman text (transliteration of Japanese in Latin
;; alphabets) to Hiragana text, which is then converted to
;; Kanji-and-Kana mixed text or Katakana text by commands specified in
;; CONVERSION-KEYS argument of the Quail package.

;;; Code:

(require 'faces)

;; Buffer local variables

(defvar quail-current-package nil
  "The current Quail package, which depends on the current input method.
See the documentation of `quail-package-alist' for the format.")
(make-variable-buffer-local 'quail-current-package)
(put 'quail-current-package 'permanent-local t)

;; Quail uses the following two buffers to assist users.
;; A buffer to show available key sequence or translation list.
(defvar quail-guidance-buf nil)
;; A buffer to show completion list of the current key sequence.
(defvar quail-completion-buf nil)

;; Each buffer in which Quail is activated should use different
;; guidance buffers.
(make-variable-buffer-local 'quail-guidance-buf)
(put 'quail-guidance-buf 'permanent-local t)

;; A main window showing Quail guidance buffer.
(defvar quail-guidance-win nil)
(make-variable-buffer-local 'quail-guidance-win)

(defvar quail-mode nil
  "Non-nil if in Quail minor mode.")
(make-variable-buffer-local 'quail-mode)
(put 'quail-mode 'permanent-local t)

(defvar quail-overlay nil
  "Overlay which covers the current translation region of Quail.")
(make-variable-buffer-local 'quail-overlay)

(defvar quail-conv-overlay nil
  "Overlay which covers the text to be converted in Quail mode.")
(make-variable-buffer-local 'quail-conv-overlay)

(defvar quail-current-key nil
  "Current key for translation in Quail mode.")

(defvar quail-current-str nil
  "Currently selected translation of the current key.")

(defvar quail-current-translations nil
  "Cons of indices and vector of possible translations of the current key.
Indices is a list of (CURRENT START END BLOCK BLOCKS), where
CURRENT is an index of the current translation,
START and END are indices of the start and end of the current block,
BLOCK is the current block index,
BLOCKS is a number of  blocks of translation.")

(defvar quail-current-data nil
  "Any Lisp object holding information of current translation status.
When a key sequence is mapped to TRANS and TRANS is a cons
of actual translation and some Lisp object to be refered
for translating the longer key sequence, this variable is set
to that Lisp object.")
(make-variable-buffer-local 'quail-current-data)

;; A flag to control conversion region.  Normally nil, but if set to
;; t, it means we must start the new conversion region if new key to
;; be translated is input.
(defvar quail-reset-conversion-region nil)

;; Quail package handlers.

(defvar quail-package-alist nil
  "List of Quail packages.
A Quail package is a list of these elements:
  NAME, TITLE, QUAIL-MAP, GUIDANCE, DOCSTRING, TRANSLATION-KEYS,
  FORGET-LAST-SELECTION, DETERMINISTIC, KBD-TRANSLATE, SHOW-LAYOUT,
  DECODE-MAP, MAXIMUM-SHORTEST, OVERLAY-PLIST, UPDATE-TRANSLATION-FUNCTION,
  CONVERSION-KEYS, SIMPLE.

QUAIL-MAP is a data structure to map key strings to translations.  For
the format, see the documentation of `quail-map-p'.

DECODE-MAP is an alist of translations and corresponding keys.

See the documentation of `quail-define-package' for the other elements.")

;; Return various slots in the current quail-package.

(defsubst quail-name ()
  "Return the name of the current Quail package."
  (nth 0 quail-current-package))
(defsubst quail-title ()
  "Return the title of the current Quail package."
  (nth 1 quail-current-package))
(defsubst quail-map ()
  "Return the translation map of the current Quail package."
  (nth 2 quail-current-package))
(defsubst quail-guidance ()
  "Return an object used for `guidance' feature of the current Quail package.
See also the documentation of `quail-define-package'."
  (nth 3 quail-current-package))
(defsubst quail-docstring ()
  "Return the documentation string of the current Quail package."
  (nth 4 quail-current-package))
(defsubst quail-translation-keymap ()
  "Return translation keymap in the current Quail package.
Translation keymap is a keymap used while translation region is active."
  (nth 5 quail-current-package))
(defsubst quail-forget-last-selection ()
  "Return `forget-last-selection' flag of the current Quail package.
See also the documentation of `quail-define-package'."
  (nth 6 quail-current-package))
(defsubst quail-deterministic ()
  "Return `deterministic' flag of the current Quail package.
See also the documentation of `quail-define-package'."
  (nth 7 quail-current-package))
(defsubst quail-kbd-translate ()
  "Return `kbd-translate' flag of the current Quail package.
See also the documentation of `quail-define-package'."
  (nth 8 quail-current-package))
(defsubst quail-show-layout ()
  "Return `show-layout' flag of the current Quail package.
See also the documentation of `quail-define-package'."
  (nth 9 quail-current-package))
(defsubst quail-decode-map ()
  "Return decode map of the current Quail package.
It is an alist of translations and corresponding keys."
  (nth 10 quail-current-package))
(defsubst quail-maximum-shortest ()
  "Return `maximum-shortest' flag of the current Quail package.
See also the documentation of `quail-define-package'."
  (nth 11 quail-current-package))
(defsubst quail-overlay-plist ()
  "Return property list of an overly used in the current Quail package."
  (nth 12 quail-current-package))
(defsubst quail-update-translation-function ()
  "Return a function for updating translation in the current Quail package."
  (nth 13 quail-current-package))
(defsubst quail-conversion-keymap ()
  "Return conversion keymap in the current Quail package.
Conversion keymap is a keymap used while conversion region is active
 but translation region is not active."
  (nth 14 quail-current-package))
(defsubst quail-simple ()
  "Return t if the current Quail package is simple."
  (nth 15 quail-current-package))

(defsubst quail-package (name)
  "Return Quail package named NAME."
  (assoc name quail-package-alist))

(defun quail-add-package (package)
  "Add Quail package PACKAGE to `quail-package-alist'."
  (let ((pac (quail-package (car package))))
    (if pac
	(setcdr pac (cdr package))
      (setq quail-package-alist (cons package quail-package-alist)))))

(defun quail-select-package (name)
  "Select Quail package named NAME as the current Quail package."
  (let ((package (quail-package name)))
    (if (null package)
	(error "No Quail package `%s'" name))
    (setq quail-current-package package)
    (setq-default quail-current-package package)
    name))

;;;###autoload
(defun quail-use-package (package-name &rest libraries)
  "Start using Quail package PACKAGE-NAME.
The remaining arguments are libraries to be loaded before using the package."
  (let ((package (quail-package package-name)))
    (if (null package)
	;; Perhaps we have not yet loaded necessary libraries.
	(while libraries
	  (if (not (load (car libraries) t))
	      (progn
		(with-output-to-temp-buffer "*Help*"
		  (princ "Quail package \"")
		  (princ package-name)
		  (princ "\" can't be activated\n  because library \"")
		  (princ (car libraries))
		  (princ "\" is not in `load-path'.

The most common case is that you have not yet installed appropriate
libraries in LEIM (Libraries of Emacs Input Method) which is
distributed separately from Emacs.

LEIM is available from the same ftp directory as Emacs."))
		(error "Can't use the Quail package `%s'" package-name))
	    (setq libraries (cdr libraries))))))
  (quail-select-package package-name)
  (setq current-input-method-title (quail-title))
  (quail-mode 1))

(defun quail-inactivate ()
  "Turn off Quail input method."
  (interactive)
  (setq overriding-terminal-local-map nil)
  (quail-mode -1))

(or (assq 'quail-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(quail-mode " Quail") minor-mode-alist)))

(defvar quail-mode-map 
  (let ((map (make-keymap))
	(i ? ))
    (while (< i 127)
      (define-key map (char-to-string i) 'quail-start-translation)
      (setq i (1+ i)))
    (setq i 128)
    (while (< i 256)
      (define-key map (vector i) 'quail-start-translation)
      (setq i (1+ i)))
    map)
  "Keymap for Quail mode.")

(or (assq 'quail-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'quail-mode quail-mode-map) minor-mode-map-alist)))

;; Since some Emacs Lisp programs (e.g. viper.el) make
;; minor-mode-map-alist buffer-local, we must be sure to register
;; quail-mode-map in default-value of minor-mode-map-alist.
(if (local-variable-p 'minor-mode-map-alist)
    (let ((map (default-value 'minor-mode-map-alist)))
      (or (assq 'quail-mode map)
	  (set-default 'minor-mode-map-alist (cons 'quail-mode map)))))

(defvar quail-translation-keymap
  (let ((map (make-keymap))
	(i 0))
    (while (< i ?\ )
      (define-key map (char-to-string i) 'quail-execute-non-quail-command)
      (setq i (1+ i)))
    (while (< i 127)
      (define-key map (char-to-string i) 'quail-self-insert-command)
      (setq i (1+ i)))
    (setq i 128)
    (while (< i 256)
      (define-key map (vector i) 'quail-self-insert-command)
      (setq i (1+ i)))
    (define-key map "\177" 'quail-delete-last-char)
    (define-key map "\C-f" 'quail-next-translation)
    (define-key map "\C-b" 'quail-prev-translation)
    (define-key map "\C-n" 'quail-next-translation-block)
    (define-key map "\C-p" 'quail-prev-translation-block)
    (define-key map "\C-i" 'quail-completion)
    (define-key map "\C-@" 'quail-select-current)
    (define-key map "\C-h" 'quail-translation-help)
;;; This interferes with handling of escape sequences on non-X terminals.
;;;    (define-key map "\e" '(keymap (t . quail-execute-non-quail-command)))
    (define-key map [?\C-\ ] 'quail-select-current)
    (define-key map [tab] 'quail-completion)
    (define-key map [delete] 'quail-delete-last-char)
    (define-key map [backspace] 'quail-delete-last-char)
    (let ((meta-map (make-sparse-keymap)))
      (define-key map (char-to-string meta-prefix-char) meta-map)
      (define-key map [escape] meta-map))
    (define-key map (vector meta-prefix-char t)
      'quail-execute-non-quail-command)
    ;; At last, define default key binding.
    (define-key map [t] 'quail-execute-non-quail-command)
    map)
  "Keymap used processing translation in complex Quail modes.
Only a few especially complex input methods use this map;
most use `quail-simple-translation-keymap' instead.
This map is activated while translation region is active.")

(defvar quail-simple-translation-keymap
  (let ((map (make-keymap))
	(i 0))
    (while (< i ?\ )
      (define-key map (char-to-string i) 'quail-execute-non-quail-command)
      (setq i (1+ i)))
    (while (< i 127)
      (define-key map (char-to-string i) 'quail-self-insert-command)
      (setq i (1+ i)))
    (define-key map "\177" 'quail-delete-last-char)
    (define-key map [delete] 'quail-delete-last-char)
    (define-key map [backspace] 'quail-delete-last-char)
;;; This interferes with handling of escape sequences on non-X terminals.
;;;    (define-key map "\e" '(keymap (t . quail-execute-non-quail-command)))
    (let ((meta-map (make-sparse-keymap)))
      (define-key map (char-to-string meta-prefix-char) meta-map)
      (define-key map [escape] meta-map))
    (define-key map (vector meta-prefix-char t)
      'quail-execute-non-quail-command)
    ;; At last, define default key binding.
    (define-key map [t]
      'quail-execute-non-quail-command)
    map)
  "Keymap used while processing translation in simple Quail modes.
A few especially complex input methods use `quail--translation-keymap' instead.
This map is activated while translation region is active.")

(defvar quail-conversion-keymap
  (let ((map (make-keymap))
	(i 0))
    (while (< i ?\ )
      (define-key map (char-to-string i) 'quail-execute-non-quail-command)
      (setq i (1+ i)))
    (while (< i 127)
      (define-key map (char-to-string i)
	'quail-start-translation-in-conversion-mode)
      (setq i (1+ i)))
    (setq i 128)
    (while (< i 256)
      (define-key map (vector i) 'quail-start-translation-in-conversion-mode)
      (setq i (1+ i)))
    (define-key map "\C-b" 'quail-conversion-backward-char)
    (define-key map "\C-f" 'quail-conversion-forward-char)
    (define-key map "\C-a" 'quail-conversion-beginning-of-region)
    (define-key map "\C-e" 'quail-conversion-end-of-region)
    (define-key map "\C-d" 'quail-conversion-delete-char)
    (define-key map "\C-h" 'quail-conversion-help)
;;; This interferes with handling of escape sequences on non-X terminals.
;;;    (define-key map "\e" '(keymap (t . quail-execute-non-quail-command)))
    (define-key map "\177" 'quail-conversion-backward-delete-char)
    (define-key map [delete] 'quail-conversion-backward-delete-char)
    (define-key map [backspace] 'quail-conversion-backward-delete-char)
    (let ((meta-map (make-sparse-keymap)))
      (define-key map (char-to-string meta-prefix-char) meta-map)
      (define-key map [escape] meta-map))
    (define-key map (vector meta-prefix-char t)
      'quail-execute-non-quail-command)
    ;; At last, define default key binding.
    (define-key map [t] 'quail-execute-non-quail-command)
    map)
  "Keymap used for processing conversion in Quail mode.
This map is activated while convesion region is active but translation
region is not active.")

;;;###autoload
(defun quail-define-package (name language title
				  &optional guidance docstring translation-keys
				  forget-last-selection deterministic
				  kbd-translate show-layout create-decode-map
				  maximum-shortest overlay-plist
				  update-translation-function
				  conversion-keys simple)
  "Define NAME as a new Quail package for input LANGUAGE.
TITLE is a string to be displayed at mode-line to indicate this package.
Optional arguments are GUIDANCE, DOCSTRING, TRANLSATION-KEYS,
 FORGET-LAST-SELECTION, DETERMINISTIC, KBD-TRANSLATE, SHOW-LAYOUT,
 CREATE-DECODE-MAP, MAXIMUM-SHORTEST, OVERLAY-PLIST,
 UPDATE-TRANSLATION-FUNCTION, CONVERSION-KEYS and SIMPLE.

GUIDANCE specifies how a guidance string is shown in echo area.
If it is t, list of all possible translations for the current key is shown
 with the currently selected translation being highlighted.
If it is an alist, the element has the form (CHAR . STRING).  Each character
 in the current key is searched in the list and the corresponding string is
 shown.
If it is nil, the current key is shown.

DOCSTRING is the documentation string of this package.

TRANSLATION-KEYS specifies additional key bindings used while translation
region is active.  It is an alist of single key character vs. corresponding
command to be called.

FORGET-LAST-SELECTION non-nil means a selected translation is not kept
for the future to translate the same key.  If this flag is nil, a
translation selected for a key is remembered so that it can be the
first candidate when the same key is entered later.

DETERMINISTIC non-nil means the first candidate of translation is
selected automatically without allowing users to select another
translation for a key.  In this case, unselected translations are of
no use for an interactive use of Quail but can be used by some other
programs.  If this flag is non-nil, FORGET-LAST-SELECTION is also set
to t.

KBD-TRANSLATE non-nil means input characters are translated from a
user's keyboard layout to the standard keyboard layout.  See the
documentation of `quail-keyboard-layout' and
`quail-keyboard-layout-standard' for more detail.

SHOW-LAYOUT non-nil means the `quail-help' command should show
the user's keyboard layout visually with translated characters.
If KBD-TRANSLATE is set, it is desirable to set also this flag unless
this package defines no translations for single character keys.

CREATE-DECODE-MAP non-nil means decode map is also created.  A decode
map is an alist of translations and corresponding original keys.
Although this map is not used by Quail itself, it can be used by some
other programs.  For instance, Vietnamese supporting needs this map to
convert Vietnamese text to VIQR format which uses only ASCII
characters to represent Vietnamese characters.

MAXIMUM-SHORTEST non-nil means break key sequence to get maximum
length of the shortest sequence.  When we don't have a translation of
key \"..ABCD\" but have translations of \"..AB\" and \"CD..\", break
the key at \"..AB\" and start translation of \"CD..\".  Hangul
packages, for instance, use this facility.  If this flag is nil, we
break the key just at \"..ABC\" and start translation of \"D..\".

OVERLAY-PLIST if non-nil is a property list put on an overlay which
covers Quail translation region.

UPDATE-TRANSLATION-FUNCTION if non-nil is a function to call to update
the current translation region accoding to a new translation data.  By
default, a tranlated text or a user's key sequence (if no transltion
for it) is inserted.

CONVERSION-KEYS specifies additional key bindings used while
conversion region is active.  It is an alist of single key character
vs. corresponding command to be called.

If SIMPLE is non-nil, then we do not alter the meanings of
commands such as C-f, C-b, C-n, C-p and TAB; they are treated as
non-Quail commands."
  (let (translation-keymap conversion-keymap)
    (if deterministic (setq forget-last-selection t))
    (if translation-keys
	(progn
	  (setq translation-keymap (copy-keymap
				    (if simple quail-simple-translation-keymap
				      quail-translation-keymap)))
	  (while translation-keys
	    (define-key translation-keymap
	      (car (car translation-keys)) (cdr (car translation-keys)))
	    (setq translation-keys (cdr translation-keys))))
      (setq translation-keymap
	    (if simple quail-simple-translation-keymap
	      quail-translation-keymap)))
    (when conversion-keys
      (setq conversion-keymap (copy-keymap quail-conversion-keymap))
      (while conversion-keys
	(define-key conversion-keymap
	  (car (car conversion-keys)) (cdr (car conversion-keys)))
	(setq conversion-keys (cdr conversion-keys))))
    (quail-add-package
     (list name title (list nil) guidance (or docstring "")
	   translation-keymap
	   forget-last-selection deterministic kbd-translate show-layout
	   (if create-decode-map (list 'decode-map) nil)
	   maximum-shortest overlay-plist update-translation-function
	   conversion-keymap simple))

    ;; Update input-method-alist.
    (let ((slot (assoc name input-method-alist))
	  (val (list language 'quail-use-package title docstring)))
      (if slot (setcdr slot val)
	(setq input-method-alist (cons (cons name val) input-method-alist)))))

  (quail-select-package name))

;; Quail minor mode handlers.

;; Setup overlays used in Quail mode.
(defun quail-setup-overlays (conversion-mode)
  (let ((pos (point)))
    (if (overlayp quail-overlay)
	(move-overlay quail-overlay pos pos)
      (setq quail-overlay (make-overlay pos pos nil nil t))
      (if input-method-highlight-flag
	  (overlay-put quail-overlay 'face 'underline))
      (let ((l (quail-overlay-plist)))
	(while l
	  (overlay-put quail-overlay (car l) (car (cdr l)))
	  (setq l (cdr (cdr l))))))
    (if conversion-mode
	(if (overlayp quail-conv-overlay)
	    (if (not (overlay-start quail-conv-overlay))
		(move-overlay quail-conv-overlay pos pos))
	  (setq quail-conv-overlay (make-overlay pos pos nil nil t))
	  (if input-method-highlight-flag
	      (overlay-put quail-conv-overlay 'face 'underline))))))

;; Delete overlays used in Quail mode.
(defun quail-delete-overlays ()
  (if (overlayp quail-overlay)
      (delete-overlay quail-overlay))
  (if (overlayp quail-conv-overlay)
      (delete-overlay quail-conv-overlay)))

;; Kill Quail guidance buffer.  Set in kill-buffer-hook.
(defun quail-kill-guidance-buf ()
  (if (buffer-live-p quail-guidance-buf)
      (kill-buffer quail-guidance-buf)))

(defun quail-mode (&optional arg)
  "Toggle Quail minor mode.
With arg, turn Quail mode on if and only if arg is positive.

You should not turn on and off Quail mode manually, instead use
the commands `toggle-input-method' or `select-input-methods' (which
see).  They automatically turn on or off this mode.

Try \\[describe-bindings] in Quail mode to see the available key bindings.
The command \\[describe-input-method] describes the current Quail package."
  (setq quail-mode
	(if (null arg) (null quail-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (null quail-mode)
      ;; Let's turn off Quail mode.
      (progn
	(quail-hide-guidance-buf)
	(quail-delete-overlays)
	(setq describe-current-input-method-function nil)
	(run-hooks 'quail-mode-exit-hook))
    ;; Let's turn on Quail mode.
    ;; At first, be sure that quail-mode is at the first element of
    ;; minor-mode-map-alist.
    (or (eq (car minor-mode-map-alist) 'quail-mode)
	(let ((l minor-mode-map-alist))
	  (while (cdr l)
	    (if (eq (car (cdr l)) 'quail-mode)
		(progn
		  (setcdr l (cdr (cdr l)))
		  (setq l nil))
	      (setq l (cdr l))))
	  (setq minor-mode-map-alist (cons 'quail-mode minor-mode-map-alist))))
    (if (null quail-current-package)
	;; Quail package is not yet selected.  Select one now.
	(let (name)
	  (if quail-package-alist
	      (setq name (car (car quail-package-alist)))
	    (setq quail-mode nil)
	    (error "No Quail package loaded"))
	  (quail-select-package name)))
    (setq inactivate-current-input-method-function 'quail-inactivate)
    (setq describe-current-input-method-function 'quail-help)
    (quail-delete-overlays)
    (quail-show-guidance-buf)
    ;; If we are in minibuffer, turn off the current input method
    ;; before exiting.
    (if (eq (selected-window) (minibuffer-window))
	(add-hook 'minibuffer-exit-hook 'quail-exit-from-minibuffer))
    (make-local-hook 'post-command-hook)
    (make-local-hook 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook 'quail-kill-guidance-buf nil t)
    (run-hooks 'quail-mode-hook))
  (force-mode-line-update))

(defun quail-exit-from-minibuffer ()
  (inactivate-input-method)
  (if (<= (minibuffer-depth) 1)
      (remove-hook 'minibuffer-exit-hook 'quail-exit-from-minibuffer)))

(defvar quail-saved-current-map nil)
(defvar quail-saved-current-buffer nil)

;; Toggle Quail mode.  This function is added to `post-command-hook'
;; in Quail mode, to turn Quail mode temporarily off, or back on after
;; one non-Quail command.
(defun quail-toggle-mode-temporarily ()
  (if quail-mode
      ;; We are going to handle following events out of Quail mode.
      (setq quail-saved-current-buffer (current-buffer)
	    quail-saved-current-map overriding-terminal-local-map
	    quail-mode nil
	    overriding-terminal-local-map nil)
    ;; We have just executed one non-Quail command.  We don't need
    ;; this hook any more.
    (remove-hook 'post-command-hook 'quail-toggle-mode-temporarily t)
    (if (eq (current-buffer) quail-saved-current-buffer)
	;; We should go back to Quail mode only when the current input
	;; method was not turned off by the last command.
	(when current-input-method
	  (setq quail-mode t
		overriding-terminal-local-map quail-saved-current-map)
	  (if input-method-exit-on-invalid-key
	      (inactivate-input-method)))
      ;; The last command changed the current buffer, we should not go
      ;; back to Quail mode in this new buffer, but should turn on
      ;; Quail mode in the original buffer.
      (save-excursion
	(set-buffer quail-saved-current-buffer)
	(setq quail-mode t)
	(quail-delete-overlays)))))

(defun quail-execute-non-quail-command ()
  "Execute one non-Quail command out of Quail mode.
The current translation and conversion are terminated."
  (interactive)
  (let* ((key (this-command-keys))
	 (keylist (listify-key-sequence key)))
    (setq unread-command-events (append keylist unread-command-events)))
  (reset-this-command-lengths)
  (quail-terminate-translation)
  (quail-delete-overlays)
  (setq overriding-terminal-local-map nil)
  (if (buffer-live-p quail-guidance-buf)
      (save-excursion
	(set-buffer quail-guidance-buf)
	(erase-buffer)))
  (add-hook 'post-command-hook 'quail-toggle-mode-temporarily nil t))

;; Keyboard layout translation handlers.

;; Some Quail packages provide localized keyboard simulation which
;; requires a particular keyboard layout.  In this case, what we need
;; is locations of keys the user entered, not character codes
;; generated by those keys.  However, for the moment, there's no
;; common way to get such information.  So, we ask a user to give
;; information of his own keyboard layout, then translate it to the
;; standard layout which we defined so that all Quail packages depend
;; just on it.

(defconst quail-keyboard-layout-standard
  "\
                              \
  1!2@3#4$5%6^7&8*9(0)-_=+`~  \
  qQwWeErRtTyYuUiIoOpP[{]}    \
  aAsSdDfFgGhHjJkKlL;:'\"\\|    \
  zZxXcCvVbBnNmM,<.>/?        \
                              "
  "Standard keyboard layout of printable characters Quail assumes.
See the documentation of `quail-keyboard-layout' for this format.
This layout is almost the same as that of VT100,
 but the location of key \\ (backslash) is just right of key ' (single-quote),
 not right of RETURN key.")

(defvar quail-keyboard-layout quail-keyboard-layout-standard
  "A string which represents physical key layout of a particular keyboard.
We assume there are six rows and each row has 15 keys (columns),
	the first row is above the `1' - `0' row,
	the first column of the second row is left of key `1',
	the first column of the third row is left of key `q',
	the first column of the fourth row is left of key `a',
	the first column of the fifth row is left of key `z',
	the sixth row is below the `z' - `/' row.
Nth (N is even) and (N+1)th characters in the string are non-shifted
 and shifted characters respectively at the same location.
The location of Nth character is row (N / 30) and column ((N mod 30) / 2).
The command `quail-set-keyboard-layout' usually sets this variable.")

(defconst quail-keyboard-layout-len 180)

;; Here we provide several examples of famous keyboard layouts.

(defvar quail-keyboard-layout-alist
  (list
   '("sun-type3" . "\
                              \
  1!2@3#4$5%6^7&8*9(0)-_=+\\|`~\
  qQwWeErRtTyYuUiIoOpP[{]}    \
  aAsSdDfFgGhHjJkKlL;:'\"      \
  zZxXcCvVbBnNmM,<.>/?        \
                              ")
   '("atari-german" . "\
                              \
  1!2\"3\2474$5%6&7/8(9)0=\337?'`#^  \
  qQwWeErRtTzZuUiIoOpP\374\334+*    \
  aAsSdDfFgGhHjJkKlL\366\326\344\304~|    \
<>yYxXcCvVbBnNmM,;.:-_        \
                              ")
   (cons "standard" quail-keyboard-layout-standard))
  "Alist of keyboard names and corresponding layout strings.
See the documentation of `quail-keyboard-layout' for the format of
 the layout string.")

;;;###autoload
(defun quail-set-keyboard-layout (kbd-type)
  "Set the current keyboard layout to the same as keyboard KBD-TYPE.

Since some Quail packages depends on a physical layout of keys (not
characters generated by them), those are created by assuming the
standard layout defined in `quail-keyboard-layout-standard'.  This
function tells Quail system the layout of your keyboard so that what
you type is correctly handled."
  (interactive
   (let* ((completing-ignore-case t)
	  (type (completing-read "Keyboard type: "
				 quail-keyboard-layout-alist)))
     (list type)))
  (let ((layout (assoc kbd-type quail-keyboard-layout-alist)))
    (if (null layout)
	;; Here, we had better ask a user to define his own keyboard
	;; layout interactively.
	(error "Unknown keyboard type `%s'" kbd-type))
    (setq quail-keyboard-layout (cdr layout))))

(defun quail-keyboard-translate (ch)
  "Translate CHAR according to `quail-keyboard-layout' and return the result."
  (if (eq quail-keyboard-layout quail-keyboard-layout-standard)
      ;; All Quail packages are designed based on
      ;; `quail-keyboard-layout-standard'.
      ch
    (let ((i 0))
      (while (and (< i quail-keyboard-layout-len)
		  (/= ch (aref quail-keyboard-layout i)))
	(setq i (1+ i)))
      (if (= i quail-keyboard-layout-len)
	  ;; CH is not in quail-keyboard-layout, which means that a
	  ;; user typed a key which generated a character code to be
	  ;; handled out of Quail.  Just return CH and make
	  ;; quail-execute-non-quail-command handle it correctly.
	  ch
	(let ((char (aref quail-keyboard-layout-standard i)))
	  (if (= char ?\ )
	      ;; A user typed a key at the location not convered by
	      ;; quail-keyboard-layout-standard.  Just return CH as
	      ;; well as above.
	      ch
	    char))))))

;; Quail map

(defsubst quail-map-p (object)
  "Return t if OBJECT is a Quail map.

A Quail map holds information how a particular key should be translated.
Its format is (TRANSLATION . ALIST).
TRANSLATION is either a character, or a cons (INDEX . VECTOR).
In the latter case, each element of VECTOR is a candidate for the translation,
and INDEX points the currently selected translation.

ALIST is normally a list of elements that look like (CHAR . DEFN),
where DEFN is another Quail map for a longer key (CHAR added to the
current key).  It may also be a symbol of a function which returns an
alist of the above format.

Just after a Quail package is read, TRANSLATION may be a string or a
vector.  Then each element of the string or vector is a candidate for
the translation.  These objects are transformed to cons cells in the
format \(INDEX . VECTOR), as described above."
  (and (consp object)
       (let ((translation (car object)))
	 (or (integerp translation) (null translation)
	     (vectorp translation) (stringp translation)
	     (symbolp translation)
	     (and (consp translation) (not (vectorp (cdr translation))))))
       (let ((alist (cdr object)))
	 (or (and (listp alist) (consp (car alist)))
	     (symbolp alist)))))

;;;###autoload
(defmacro quail-define-rules (&rest rules)
  "Define translation rules of the current Quail package.
Each argument is a list of KEY and TRANSLATION.
KEY is a string meaning a sequence of keystrokes to be translated.
TRANSLATION is a character, a string, a vector, a Quail map, or a function.
It it is a character, it is the sole translation of KEY.
If it is a string, each character is a candidate for the translation.
If it is a vector, each element (string or character) is a candidate
  for the translation.
In these cases, a key specific Quail map is generated and assigned to KEY.

If TRANSLATION is a Quail map or a function symbol which returns a Quail map,
 it is used to handle KEY."
  `(quail-install-map
    ',(let ((l rules)
	    (map (list nil)))
	(while l
	  (quail-defrule-internal (car (car l)) (car (cdr (car l))) map)
	  (setq l (cdr l)))
	map)))

;;;###autoload
(defun quail-install-map (map)
  "Install the Quail map MAP in the current Quail package.
The installed map can be referred by the function `quail-map'."
  (if (null quail-current-package)
      (error "No current Quail package"))
  (if (null (quail-map-p map))
      (error "Invalid Quail map `%s'" map))
  (setcar (cdr (cdr quail-current-package)) map))

;;;###autoload
(defun quail-defrule (key translation &optional name)
  "Add one translation rule, KEY to TRANSLATION, in the current Quail package.
KEY is a string meaning a sequence of keystrokes to be translated.
TRANSLATION is a character, a string, a vector, a Quail map,
 a function, or a cons.
It it is a character, it is the sole translation of KEY.
If it is a string, each character is a candidate for the translation.
If it is a vector, each element (string or character) is a candidate
 for the translation.
If it is a cons, the car is one of the above and the cdr is a function
 to call when translating KEY (the return value is assigned to the
 variable `quail-current-data').  If the cdr part is not a function,
 the value itself is assigned to `quail-current-data'.
In these cases, a key specific Quail map is generated and assigned to KEY.

If TRANSLATION is a Quail map or a function symbol which returns a Quail map,
 it is used to handle KEY.
Optional argument NAME, if specified, says which Quail package
to define this translation rule in.  The default is to define it in the
current Quail package."
  (if name
      (let ((package (quail-package name)))
	(if (null package)
	    (error "No Quail package `%s'" name))
	(setq quail-current-package package)))
  (quail-defrule-internal key translation (quail-map)))

;;;###autoload
(defun quail-defrule-internal (key trans map)
  "Define KEY as TRANS in a Quail map MAP."
  (if (null (stringp key))
      "Invalid Quail key `%s'" key)
  (if (not (or (numberp trans) (stringp trans) (vectorp trans)
	       (consp trans)
	       (symbolp trans)
	       (quail-map-p trans)))
      (error "Invalid Quail translation `%s'" trans))
  (if (null (quail-map-p map))
      (error "Invalid Quail map `%s'" map))
  (let ((len (length key))
	(idx 0)
	ch entry)
    ;; Make a map for registering TRANS if necessary.
    (while (< idx len)
      (if (null (consp map))
	  ;; We come here, for example, when we try to define a rule
	  ;; for "ABC" but a rule for "AB" is already defined as a
	  ;; symbol.
	  (error "Quail key %s is too long" key))
      (setq ch (aref key idx)
	    entry (assq ch (cdr map)))
      (if (null entry)
	  (progn
	    (setq entry (cons ch (list nil)))
	    (setcdr map (cons entry (cdr map)))))
      (setq map (cdr entry))
      (setq idx (1+ idx)))
    (if (symbolp trans)
	(if (cdr map)
	    ;; We come here, for example, when we try to define a rule
	    ;; for "AB" as a symbol but a rule for "ABC" is already
	    ;; defined.
	    (error "Quail key %s is too short" key)
	  (setcdr entry trans))
      (if (quail-map-p trans)
	  (if (not (listp (cdr map)))
	      ;; We come here, for example, when we try to define a rule
	      ;; for "AB" as a symbol but a rule for "ABC" is already
	      ;; defined.
	      (error "Quail key %s is too short" key)
	    (if (not (listp (cdr trans)))
		(if (cdr map)
		    ;; We come here, for example, when we try to
		    ;; define a rule for "AB" as a symbol but a rule
		    ;; for "ABC" is already defined.
		    (error "Quail key %s is too short" key)
		  (setcdr entry trans))
	      (setcdr entry (append trans (cdr map)))))
	(setcar map trans)))))

(defun quail-get-translation (def key len)
  "Return the translation specified as DEF for KEY of length LEN.
The translation is either a character or a cons of the form (INDEX . VECTOR),
where VECTOR is a vector of candidates (character or string) for
the translation, and INDEX points into VECTOR to specify the currently
selected translation."
  (if (and def (symbolp def))
      ;; DEF is a symbol of a function which returns valid translation.
      (setq def (funcall def key len)))
  (if (and (consp def) (not (vectorp (cdr def))))
      (setq def (car def)))

  (cond
   ((or (integerp def) (consp def))
    def)

   ((null def)
    ;; No translation.
    nil)

   ((stringp def)
    ;; Each character in DEF is a candidate of translation.  Reform
    ;; it as (INDICES . VECTOR).
    (setq def (string-to-vector def))
    ;; But if the length is 1, we don't need vector but a single
    ;; candidate as the translation.
    (if (= (length def) 1)
	(aref def 0)
      (cons (list 0 0 0 0 nil) def)))

   ((vectorp def)
    ;; Each element (string or character) in DEF is a candidate of
    ;; translation.  Reform it as (INDICES . VECTOR).
    (cons (list 0 0 0 0 nil) def))

   (t
    (error "Invalid object in Quail map: %s" def))))

(defun quail-lookup-key (key &optional len)
  "Lookup KEY of length LEN in the current Quail map and return the definition.
The returned value is a Quail map specific to KEY."
  (or len
      (setq len (length key)))
  (let ((idx 0)
	(map (quail-map))
	(kbd-translate (quail-kbd-translate))
	slot ch translation def)
    (while (and map (< idx len))
      (setq ch (if kbd-translate (quail-keyboard-translate (aref key idx))
		 (aref key idx)))
      (setq idx (1+ idx))
      (if (and (cdr map) (symbolp (cdr map)))
	  (setcdr map (funcall (cdr map) key idx)))
      (setq slot (assq ch (cdr map)))
      (if (and (cdr slot) (symbolp (cdr slot)))
	  (setcdr slot (funcall (cdr slot) key idx)))
      (setq map (cdr slot)))
    (setq def (car map))
    (setq quail-current-translations nil)
    (if (and map (setq translation (quail-get-translation def key len)))
	(progn
	  (if (and (consp def) (not (vectorp (cdr def))))
	      (progn
		(if (not (equal (car def) translation))
		    ;; We must reflect TRANSLATION to car part of DEF.
		    (setcar def translation))
		(setq quail-current-data
		      (if (functionp (cdr def))
			  (funcall (cdr def))
			(cdr def))))
	    (if (not (equal def translation))
		;; We must reflect TRANSLATION to car part of MAP.
		(setcar map translation)))
	  (if (and (consp translation) (vectorp (cdr translation))) 
	      (progn
		(setq quail-current-translations translation)
		(if (quail-forget-last-selection)
		    (setcar (car quail-current-translations) 0))))
	  ;; We may have to reform cdr part of MAP.
	  (if (and (cdr map) (symbolp (cdr map)))
	      (progn
		(setcdr map (funcall (cdr map) key len))))
	  ))
    map))

;; If set to non-nil, exit conversion mode before starting new translation.
(defvar quail-exit-conversion-mode nil)

(defvar quail-prefix-arg nil)

(defun quail-start-translation (arg)
  "Start translating the typed character in Quail mode."
  (interactive "*p")
  (setq prefix-arg arg)
  (setq quail-prefix-arg arg)
  (setq unread-command-events
	(cons last-command-event unread-command-events))
  ;; Check the possibility of translating the last key.
  (if (and (integerp last-command-event)
	   (assq (if (quail-kbd-translate)
		     (quail-keyboard-translate last-command-event)
		   last-command-event)
		 (cdr (quail-map))))
      ;; Ok, we can start translation.
      (if (quail-conversion-keymap)
	  ;; We must start translation in conversion mode.
	  (setq quail-exit-conversion-mode nil
		overriding-terminal-local-map (quail-conversion-keymap))
	(quail-setup-overlays nil)
	(setq quail-current-key "")
	(setq overriding-terminal-local-map (quail-translation-keymap)))
    ;; Since the last event doesn't start any translation, handle it
    ;; out of Quail mode.  We come back to Quail mode later by setting
    ;; function `quail-toggle-mode-temporarily' in
    ;; `post-command-hook'.
    (add-hook 'post-command-hook 'quail-toggle-mode-temporarily nil t)))

(defsubst quail-point-in-conversion-region ()
  "Return non-nil value if the point is in conversion region of Quail mode."
  (let (start pos)
    (and (setq start (overlay-start quail-conv-overlay))
	 (>= (setq pos (point)) start)
	 (<= pos (overlay-end quail-conv-overlay)))))

(defun quail-start-translation-in-conversion-mode ()
  "Start translating the typed character in conversion mode of Quail mode."
  (interactive "*")
  (setq unread-command-events
	(cons last-command-event unread-command-events))
  ;; Check the possibility of translating the last key.
  (if (and (integerp last-command-event)
	   (assq (if (quail-kbd-translate)
		     (quail-keyboard-translate last-command-event)
		   last-command-event)
		 (cdr (quail-map))))
      ;; Ok, we can start translation.
      (progn
	(quail-setup-overlays t)
	(setq quail-current-key "")
	(setq overriding-terminal-local-map (quail-translation-keymap))
	(move-overlay quail-overlay (point) (point)))
    ;; Since the last event doesn't start any translation, handle it
    ;; out of Quail mode.  We come back to Quail mode later by setting
    ;; function `quail-toggle-mode-temporarily' in
    ;; `post-command-hook'.
    (add-hook 'post-command-hook 'quail-toggle-mode-temporarily nil t)))

(defsubst quail-delete-region ()
  "Delete the text in the current translation region of Quail."
  (if (overlay-start quail-overlay)
      (delete-region (overlay-start quail-overlay)
		     (overlay-end quail-overlay))))

(defun quail-terminate-translation ()
  "Terminate the translation of the current key."
  (when (overlayp quail-overlay)
    (let ((start (overlay-start quail-overlay)))
      (if (and start
	       (< start (overlay-end quail-overlay)))
	  ;; Here we simulate self-insert-command.
	  (let ((seq (string-to-sequence
		      (buffer-substring (overlay-start quail-overlay)
					(overlay-end quail-overlay))
		      'list))
		last-command-char)
	    (goto-char start)
	    (quail-delete-region)
	    (setq last-command-char (car seq))
	    (self-insert-command (or quail-prefix-arg 1))
	    (setq quail-prefix-arg nil)
	    (setq seq (cdr seq))
	    (while seq
	      (setq last-command-char (car seq))
	      (self-insert-command 1)
	      (setq seq (cdr seq))))))
    (delete-overlay quail-overlay))
  (if (buffer-live-p quail-guidance-buf)
      (save-excursion
	(set-buffer quail-guidance-buf)
	(erase-buffer)))
  (setq overriding-terminal-local-map
	(quail-conversion-keymap))
  ;; Run this hook only when the current input method doesn't require
  ;; conversion.  When conversion is required, the conversion function
  ;; should run this hook at a proper timing.
  (unless (quail-conversion-keymap)
    (run-hooks 'input-method-after-insert-chunk-hook)))

(defun quail-select-current ()
  "Select the current text shown in Quail translation region."
  (interactive)
  (quail-terminate-translation))

;; Update the current translation status according to CONTROL-FLAG.
;; If CONTROL-FLAG is integer value, it is the number of keys in the
;; head quail-current-key which can be translated.  The remaining keys
;; are put back to unread-command-events to be handled again.
;; If CONTROL-FLAG is t, terminate the translation for the whole keys
;; in quail-current-key.
;; If CONTROL-FLAG is nil, proceed the translation with more keys.

(defun quail-update-translation (control-flag)
  (quail-delete-region)
  (let ((func (quail-update-translation-function)))
    (if func
	(funcall func control-flag)
      (if (numberp control-flag)
	  (let ((len (length quail-current-key)))
	    (while (> len control-flag)
	      (setq len (1- len))
	      (setq unread-command-events
		    (cons (aref quail-current-key len)
			  unread-command-events)))
	    ;; Insert the translated sequence.
	    ;; It is a string containing multibyte characters.
	    ;; If enable-multibyte-characters, just insert it.
	    (if enable-multibyte-characters
		(insert (or quail-current-str
			    (substring quail-current-key 0 len)))
	      ;; Otherwise, in case the user is using a single-byte
	      ;; extended-ASCII character set,
	      ;; try inserting the translated character.
	      (let ((char (or quail-current-str
			      (substring quail-current-key 0 len))))
		(if (stringp char)
		    (setq char (sref char 0)))
		(if (= (length (split-char char)) 2)
		    (insert-char (logand char 255) 1)
		  (error "Three-byte characters require enabling multibyte characters")))))
	(insert (or quail-current-str quail-current-key)))))
  (quail-update-guidance)
  (if control-flag
      (quail-terminate-translation)))

(defun quail-self-insert-command ()
  "Add the typed character to the key for translation."
  (interactive "*")
  (setq quail-current-key
	(concat quail-current-key (char-to-string last-command-event)))
  (unless (catch 'quail-tag
	    (quail-update-translation (quail-translate-key))
	    t)
    ;; If someone throws for `quail-tag' by value nil, we exit from
    ;; translation mode.
    (setq overriding-terminal-local-map nil)))

;; Return the actual definition part of Quail map MAP.
(defun quail-map-definition (map)
  (let ((def (car map)))
    (if (and (consp def) (not (vectorp (cdr def))))
	(setq def (car def)))
    def))

;; Return a string to be shown as the current translation of key
;; sequence of length LEN.  DEF is a definition part of Quail map for
;; the sequence.
(defun quail-get-current-str (len def)
  (or (and (consp def) (aref (cdr def) (car (car def))))
      def
      (and (> len 1)
	   (let ((str (quail-get-current-str
		       (1- len)
		       (quail-map-definition (quail-lookup-key
					      quail-current-key (1- len))))))
	     (if str
		 (concat (if (stringp str) str (char-to-string str))
			 (substring quail-current-key (1- len) len)))))))

(defvar quail-guidance-translations-starting-column 20)

;; Update `quail-current-translations' to make RELATIVE-INDEX the
;; current translation.
(defun quail-update-current-translations (&optional relative-index)
  (let* ((indices (car quail-current-translations))
	 (cur (car indices))
	 (start (nth 1 indices))
	 (end (nth 2 indices)))
    ;; Validate the index number of current translation.
    (if (< cur 0)
	(setcar indices (setq cur 0))
      (if (>= cur (length (cdr quail-current-translations)))
	  (setcar indices
		  (setq cur (1- (length (cdr quail-current-translations)))))))

    (if (or (null end)			; We have not yet calculated END.
	    (< cur start)		; We moved to the previous block.
	    (>= cur end))		; We moved to the next block.
	(let ((len (length (cdr quail-current-translations)))
	      (maxcol (- (window-width quail-guidance-win)
			 quail-guidance-translations-starting-column))
	      (block (nth 3 indices))
	      col idx width trans num-items blocks)
	  (if (< cur start)
	      ;; We must calculate from the head.
	      (setq start 0 block 0)
	    (if end			; i.e. (>= cur end)
		(setq start end)))
	  (setq idx start col 0 end start num-items 0)
	  ;; Loop until we hit the tail, or reach the block of CUR.
	  (while (and (< idx len) (>= cur end))
	    (if (= num-items 0)
		(setq start idx col 0 block (1+ block)))
	    (setq trans (aref (cdr quail-current-translations) idx))
	    (setq width (if (integerp trans) (char-width trans)
			  (string-width trans)))
	    (setq col (+ col width 3) num-items (1+ num-items))
	    (if (and (> num-items 0)
		     (or (>= col maxcol) (> num-items 10)))
		(setq end idx num-items 0)
	      (setq idx (1+ idx))))
	  (setcar (nthcdr 3 indices) block)
	  (if (>= idx len)
	      (progn
		;; We hit the tail before reaching MAXCOL.
		(setq end idx)
		(setcar (nthcdr 4 indices) block)))
	  (setcar (cdr indices) start)
	  (setcar (nthcdr 2 indices) end)))
    (if relative-index
	(if (>= (+ start relative-index) end)
	    (setcar indices end)
	  (setcar indices (+ start relative-index))))
    (setq quail-current-str
	  (aref (cdr quail-current-translations) (car indices)))))

(defun quail-translate-key ()
  "Translate the current key sequence according to the current Quail map.
Return t if we can terminate the translation.
Return nil if the current key sequence may be followed by more keys.
Return number if we can't find any translation for the current key
sequence.  The number is the count of valid keys in the current
sequence counting from the head."
  (let* ((len (length quail-current-key))
	 (map (quail-lookup-key quail-current-key len))
	 def ch)
    (if map
	(let ((def (quail-map-definition map)))
	  (setq quail-current-str (quail-get-current-str len def))
	  ;; Return t only if we can terminate the current translation.
	  (and
	   ;; No alternative translations.
	   (or (null (consp def)) (= (length (cdr def)) 1))
	   ;; No translation for the longer key.
	   (null (cdr map))
	   ;; No shorter breaking point.
	   (or (null (quail-maximum-shortest))
	       (< len 3)
	       (null (quail-lookup-key quail-current-key (1- len)))
	       (null (quail-lookup-key
		      (substring quail-current-key -2 -1) 1)))))

      ;; There's no translation for the current key sequence.  Before
      ;; giving up, we must check two possibilities.
      (cond ((and
	      (quail-maximum-shortest)
	      (>= len 4)
	      (setq def (quail-map-definition
			 (quail-lookup-key quail-current-key (- len 2))))
	      (quail-lookup-key (substring quail-current-key -2) 2))
	     ;; Now the sequence is "...ABCD", which can be split into
	     ;; "...AB" and "CD..." to get valid translation.
	     ;; At first, get translation of "...AB".
	     (setq quail-current-str (quail-get-current-str (- len 2) def))
	     ;; Then, return the length of "...AB".
	     (- len 2))

	    ((and (> len 0)
		  (quail-lookup-key (substring quail-current-key 0 -1))
		  quail-current-translations
		  (not (quail-deterministic))
		  (setq ch (aref quail-current-key (1- len)))
		  (>= ch ?0) (<= ch ?9))
	     ;; A numeric key is entered to select a desirable translation.
	     (setq quail-current-key (substring quail-current-key 0 -1))
	     ;; We treat key 1,2..,9,0 as specifying 0,1,..8,9.
	     (setq ch (if (= ch ?0) 9 (- ch ?1)))
	     (quail-update-current-translations ch)
	     ;; And, we can terminate the current translation.
	     t)

	    (t
	     ;; No way to handle the last character in this context.
	     (1- len))))))

(defun quail-next-translation ()
  "Select next translation in the current batch of candidates."
  (interactive)
  (if quail-current-translations
      (let ((indices (car quail-current-translations)))
	(if (= (1+ (car indices)) (length (cdr quail-current-translations)))
	    ;; We are already at the tail.
	    (beep)
	  (setcar indices (1+ (car indices)))
	  (quail-update-current-translations)
	  (quail-update-translation nil)))
    (quail-execute-non-quail-command)))

(defun quail-prev-translation ()
  "Select previous translation in the current batch of candidates."
  (interactive)
  (if quail-current-translations
      (let ((indices (car quail-current-translations)))
	(if (= (car indices) 0)
	    ;; We are already at the head.
	    (beep)
	  (setcar indices (1- (car indices)))
	  (quail-update-current-translations)
	  (quail-update-translation nil)))
    (quail-execute-non-quail-command)))

(defun quail-next-translation-block ()
  "Select from the next block of translations."
  (interactive)
  (if quail-current-translations
      (let* ((indices (car quail-current-translations))
	     (offset (- (car indices) (nth 1 indices))))
	(if (>= (nth 2 indices) (length (cdr quail-current-translations)))
	    ;; We are already at the last block.
	    (beep)
	  (setcar indices (+ (nth 2 indices) offset))
	  (quail-update-current-translations)
	  (quail-update-translation nil)))
    (quail-execute-non-quail-command)))

(defun quail-prev-translation-block ()
  "Select the previous batch of 10 translation candidates."
  (interactive)
  (if quail-current-translations
      (let* ((indices (car quail-current-translations))
	     (offset (- (car indices) (nth 1 indices))))
	(if (= (nth 1 indices) 0)
	    ;; We are already at the first block.
	    (beep)
	  (setcar indices (1- (nth 1 indices)))
	  (quail-update-current-translations)
	  (if (< (+ (nth 1 indices) offset) (nth 2 indices))
	      (progn
		(setcar indices (+ (nth 1 indices) offset))
		(quail-update-current-translations)))
	  (quail-update-translation nil)))
    (quail-execute-non-quail-command)))

(defun quail-abort-translation ()
  "Abort translation and delete the current Quail key sequence."
  (interactive)
  (quail-delete-region)
  (quail-terminate-translation))

(defun quail-delete-last-char ()
  "Delete the last input character from the current Quail key sequence."
  (interactive)
  (if (= (length quail-current-key) 1)
      (quail-abort-translation)
    (setq quail-current-key (substring quail-current-key 0 -1))
    (quail-update-translation (quail-translate-key))))

;; For conversion mode.

(defun quail-conversion-backward-char ()
  (interactive)
  (if (<= (point) (overlay-start quail-conv-overlay))
      (error "Beginning of conversion region"))
  (forward-char -1))

(defun quail-conversion-forward-char ()
  (interactive)
  (if (>= (point) (overlay-end quail-conv-overlay))
      (error "End of conversion region"))
  (forward-char 1))

(defun quail-conversion-beginning-of-region ()
  (interactive)
  (goto-char (overlay-start quail-conv-overlay)))

(defun quail-conversion-end-of-region ()
  (interactive)
  (goto-char (overlay-end quail-conv-overlay)))

(defun quail-conversion-delete-char ()
  (interactive)
  (if (>= (point) (overlay-end quail-conv-overlay))
      (error "End of conversion region"))
  (delete-char 1)
  (when (= (overlay-start quail-conv-overlay)
	   (overlay-end quail-conv-overlay))
    (quail-delete-overlays)
    (setq overriding-terminal-local-map nil)))

(defun quail-conversion-backward-delete-char ()
  (interactive)
  (if (<= (point) (overlay-start quail-conv-overlay))
      (error "Beginning of conversion region"))
  (delete-char -1)
  (when (= (overlay-start quail-conv-overlay)
	   (overlay-end quail-conv-overlay))
    (quail-delete-overlays)
    (setq overriding-terminal-local-map nil)))

(defun quail-do-conversion (func &rest args)
  "Call FUNC to convert text in the current conversion region of Quail.
Remaining args are for FUNC."
  (delete-overlay quail-overlay)
  (apply func args))

(defun quail-no-conversion ()
  "Do no conversion of the current conversion region of Quail."
  (interactive)
  (quail-delete-overlays)
  (setq overriding-terminal-local-map nil)
  (run-hooks 'input-method-after-insert-chunk-hook))

;; Guidance, Completion, and Help buffer handlers.

;; Make a new one-line frame for Quail guidance buffer.
(defun quail-make-guidance-frame (buf)
  (let* ((fparam (frame-parameters))
	 (top (cdr (assq 'top fparam)))
	 (border (cdr (assq 'border-width fparam)))
	 (internal-border (cdr (assq 'internal-border-width fparam)))
	 (newtop (- top
		    (frame-char-height) (* internal-border 2) (* border 2))))
    (if (< newtop 0)
	(setq newtop (+ top (frame-pixel-height))))
    (let* ((frame (make-frame (append '((user-position . t) (height . 1)
					(minibuffer) (menu-bar-lines . 0))
				      (cons (cons 'top newtop) fparam))))
	   (win (frame-first-window frame)))
      (set-window-buffer win buf)
      ;;(set-window-dedicated-p win t)
      )))

;; Setup Quail completion buffer.
(defun quail-setup-completion-buf ()
  (unless (buffer-live-p quail-completion-buf)
    (setq quail-completion-buf (get-buffer-create "*Quail Completions*"))
    (save-excursion
      (set-buffer quail-completion-buf)
      (setq quail-overlay (make-overlay 1 1))
      (overlay-put quail-overlay 'face 'highlight))))

;; Return t iff the current Quail package requires showing guidance
;; buffer.
(defun quail-require-guidance-buf ()
  (and input-method-verbose-flag
       (not (and (eq (selected-window) (minibuffer-window))
		 (quail-simple)))))

(defun quail-show-guidance-buf ()
  "Display a guidance buffer for Quail input method in some window.
Create the buffer if it does not exist yet.
The buffer is normally displayed at the echo area,
but if the current buffer is a minibuffer, it is shown in
the bottom-most ordinary window of the same frame,
or in a newly created frame (if the selected frame has no other windows)."
  (when (quail-require-guidance-buf)
    ;; At first, setup a guidance buffer.
    (or (buffer-live-p quail-guidance-buf)
	(setq quail-guidance-buf (generate-new-buffer " *Quail-guidance*")))
    (let ((title (quail-title)))
      (save-excursion
	(set-buffer quail-guidance-buf)
	;; To show the title of Quail package.
	(setq current-input-method t
	      current-input-method-title title)
	(erase-buffer)
	(or (overlayp quail-overlay)
	    (progn
	      (setq quail-overlay (make-overlay 1 1))
	      (overlay-put quail-overlay 'face 'highlight)))
	(delete-overlay quail-overlay)
	(set-buffer-modified-p nil)))
    (bury-buffer quail-guidance-buf)

    ;; Then, display it in an appropriate window.
    (let ((win (minibuffer-window)))
      (if (eq (selected-window) win)
	  ;; Since we are in minibuffer, we can't use it for guidance.
	  (if (eq win (frame-root-window))
	      ;; Create a frame.  It is sure that we are using some
	      ;; window system.
	      (quail-make-guidance-frame quail-guidance-buf)
	    ;; Find the bottom window and split it if necessary.
	    (let (height)
	      (setq win (window-at 0 (- (frame-height) 2)))
	      (setq height (window-height win))
	      ;; If WIN is tall enough, split it vertically and use
	      ;; the lower one.
	      (if (>= height 4)
		  (let ((window-min-height 2))
		    ;; Here, `split-window' returns a lower window
		    ;; which is what we wanted.
		    (setq win (split-window win (- height 2)))))
	      (set-window-buffer win quail-guidance-buf)
	      ;;(set-window-dedicated-p win t)
	      ))
	(set-window-buffer win quail-guidance-buf))
      (setq quail-guidance-win win)))

  ;; And, create a buffer for completion.
  (quail-setup-completion-buf)
  (bury-buffer quail-completion-buf))

(defun quail-hide-guidance-buf ()
  "Hide the Quail guidance buffer."
  (if (buffer-live-p quail-guidance-buf)
      (let ((win-list (get-buffer-window-list quail-guidance-buf t t))
	    win)
	(while win-list
	  (setq win (car win-list) win-list (cdr win-list))
	  (if (eq win (minibuffer-window))
	      ;; We are using echo area for the guidance buffer.
	      ;; Vacate it to the deepest minibuffer.
	      (set-window-buffer win
				 (format " *Minibuf-%d*" (minibuffer-depth)))
	    (if (eq win (frame-root-window (window-frame win)))
		(progn
		  ;; We are using a separate frame for guidance buffer.
		  ;;(set-window-dedicated-p win nil)
		  (delete-frame (window-frame win)))
	      ;;(set-window-dedicated-p win nil)
	      (delete-window win)))))))

(defun quail-update-guidance ()
  "Update the Quail guidance buffer and completion buffer (if displayed now)."
  ;; Update guidance buffer.
  (if (quail-require-guidance-buf)
      (let ((guidance (quail-guidance)))
	(cond ((or (eq guidance t)
		   (listp guidance))
	       ;; Show the current possible translations.
	       (quail-show-translations))
	      ((null guidance)
	       ;; Show the current input keys.
	       (let ((key quail-current-key))
		 (save-excursion
		   (set-buffer quail-guidance-buf)
		   (erase-buffer)
		   (insert key)))))))

  ;; Update completion buffer if displayed now.  We highlight the
  ;; selected candidate string in *Completion* buffer if any.
  (let ((win (get-buffer-window quail-completion-buf))
	key str pos)
    (if win
	(save-excursion
	  (setq str (if (stringp quail-current-str)
			quail-current-str
		      (if (numberp quail-current-str)
			  (char-to-string quail-current-str)))
		key quail-current-key)
	  (set-buffer quail-completion-buf)
	  (goto-char (point-min))
	  (if (null (search-forward (concat " " key ":") nil t))
	      (delete-overlay quail-overlay)
	    (setq pos (point))
	    (if (and str (search-forward (concat "." str) nil t))
		  (move-overlay quail-overlay (1+ (match-beginning 0)) (point))
		(move-overlay quail-overlay (match-beginning 0) (point)))
	    ;; Now POS points end of KEY and (point) points end of STR.
	    (if (pos-visible-in-window-p (point) win)
		;; STR is already visible.
		nil
	      ;; We want to make both KEY and STR visible, but if the
	      ;; window is too short, make at least STR visible.
	      (setq pos (progn (point) (goto-char pos)))
	      (beginning-of-line)
	      (set-window-start win (point))
	      (if (not (pos-visible-in-window-p pos win))
		  (set-window-start win pos))
	      ))))))

(defun quail-show-translations ()
  "Show the current possible translations."
  (let* ((key quail-current-key)
	 (map (quail-lookup-key quail-current-key)))
    (if quail-current-translations
	(quail-update-current-translations))
    (save-excursion
      (set-buffer quail-guidance-buf)
      (erase-buffer)

      ;; Show the current key.
      (let ((guidance (quail-guidance)))
	(if (listp guidance)
	    ;; We must show the specified PROMPTKEY instead of the
	    ;; actual typed keys.
	    (let ((i 0)
		  (len (length key))
		  prompt-key)
	      (while (< i len)
		(setq prompt-key (cdr (assoc (aref key i) guidance)))
		(insert (or prompt-key (aref key i)))
		(setq i (1+ i))))
	  (insert key)))

      ;; Show followable keys.
      (if (cdr map)
	  (let ((l (cdr map)))
	    (insert "[")
	    (while l
	      (insert (car (car l)))
	      (setq l (cdr l)))
	    (insert "]")))

      ;; Show list of translations.
      (if quail-current-translations
	  (let* ((indices (car quail-current-translations))
		 (cur (car indices))
		 (start (nth 1 indices))
		 (end (nth 2 indices))
		 (idx start))
	    (indent-to (- quail-guidance-translations-starting-column 7))
	    (insert (format "(%02d/"(nth 3 indices))
		    (if (nth 4 indices)
			(format "%02d)" (nth 4 indices))
		      "??)"))
	    (while (< idx end)
	      (insert (format " %d." (if (= (- idx start) 9) 0
				       (1+ (- idx start)))))
	      (let ((pos (point)))
		(insert (aref (cdr quail-current-translations) idx))
		(if (= idx cur)
		    (move-overlay quail-overlay pos (point))))
	      (setq idx (1+ idx)))))
      )))

(defun quail-completion ()
  "List all completions for the current key.
All possible translations of the current key and whole possible longer keys
 are shown."
  (interactive)
  (quail-setup-completion-buf)
  (let ((key quail-current-key)
	(map (quail-lookup-key quail-current-key)))
    (save-excursion
      (set-buffer quail-completion-buf)
      (erase-buffer)
      (insert "Possible completion and corresponding translations are:\n")
      (quail-completion-1 key map 1)
      (goto-char (point-min))
      (display-buffer (current-buffer)))
      (quail-update-guidance)))

;; List all completions of KEY in MAP with indentation INDENT.
(defun quail-completion-1 (key map indent)
  (let ((len (length key)))
    (indent-to indent)
    (insert key ":")
    (if (and (symbolp map) (fboundp map))
	(setq map (funcall map key len)))
    (if (car map)
	(quail-completion-list-translations map key (+ indent len 1))
      (insert " -\n"))
    (setq indent (+ indent 2))
    (if (cdr map)
	(let ((l (cdr map))
	      (newkey (make-string (1+ len) 0))
	      (i 0))
	  ;; Set KEY in the first LEN characters of NEWKEY.
	  (while (< i len)
	    (aset newkey i (aref key i))
	    (setq i (1+ i)))
	  (while l			; L = ((CHAR . DEFN) ....) ;
	    (aset newkey len (car (car l)))
	    (quail-completion-1 newkey (cdr (car l)) indent)
	    (setq l (cdr l)))))))

;; List all possible translations of KEY in Quail map MAP with
;; indentation INDENT.
(defun quail-completion-list-translations (map key indent)
  (let ((translations
	 (quail-get-translation (car map) key (length key))))
    (if (integerp translations)
	(insert "(1/1) 1." translations "\n")
      ;; We need only vector part.
      (setq translations (cdr translations))
      ;; Insert every 10 elements with indices in a line.
      (let ((len (length translations))
	    (i 0)
	    num)
	(while (< i len)
	  (when (zerop (% i 10))
	    (when (>= i 10)
	      (newline)
	      (indent-to indent))
	    (insert (format "(%d/%d)" (1+ (/ i 10)) (1+ (/ len 10)))))
	  ;; We show the last digit of FROM while converting
	  ;; 0,1,..,9 to 1,2,..,0.
	  (insert (format " %d." (% (1+ i) 10)))
	  (insert (aref translations i))
	  (setq i (1+ i)))
	(newline)))))

(defun quail-help ()
  "Show brief description of the current Quail package."
  (interactive)
  (let ((package quail-current-package))
    (with-output-to-temp-buffer "*Quail-Help*"
      (save-excursion
	(set-buffer standard-output)
	(let ((quail-current-package package))
	  (insert "Quail input method (name:"
		  (quail-name)
		  ", mode line indicator:["
		  (quail-title)
		  "])\n---- Documentation ----\n"
		  (quail-docstring))
	  (newline)
	  (if (quail-show-layout) (quail-show-kbd-layout))
	  (quail-help-insert-keymap-description
	   quail-mode-map
	   "---- Key bindings (before starting translation) ----
key		binding
---		-------\n")
	  (quail-help-insert-keymap-description
	   (quail-translation-keymap)
	   "--- Key bindings (while translating) ---
key		binding
---		-------\n")
	  (if (quail-conversion-keymap)
	      (quail-help-insert-keymap-description
	       (quail-conversion-keymap)
	       "--- Key bindings (while converting) ---
key		binding
---		-------\n"))
	  (help-mode))))))

(defun quail-help-insert-keymap-description (keymap &optional header)
  (let (from to)
    (if header
	(insert header))
    (save-excursion
      (save-window-excursion
	(let ((overriding-terminal-local-map keymap))
	  (describe-bindings))
	(set-buffer "*Help*")
	(goto-char (point-min))
	(forward-line 4)
	(setq from (point))
	(search-forward "Global Bindings:" nil 'move)
	(beginning-of-line)
	(setq to (point))))
    (insert-buffer-substring "*Help*" from to)))

(defun quail-show-kbd-layout ()
  "Show keyboard layout with key tops of multilingual characters."
  (insert "--- Keyboard layout ---\n")
  (let* ((i 0) ch)
    (while (< i quail-keyboard-layout-len)
      (if (= (% i 30) 0)
	  (progn
	    (newline)
	    (indent-to (/ i 30)))
	(if (= (% i 2) 0)
	    (insert "   ")))
      (setq ch (aref quail-keyboard-layout i))
      (when (and (quail-kbd-translate)
		 (/= ch ?\ ))
	;; This is the case that the current input method simulates
	;; some keyboard layout (which means it requires keyboard
	;; translation) and a key at location `i' exists on users
	;; keyboard.  We must translate that key by
	;; `quail-keyboard-layout-standard'.  But if if there's no
	;; corresponding key in that standard layout, we must simulate
	;; what is inserted if that key is pressed by setting CH a
	;; minus value.
	(setq ch (aref quail-keyboard-layout-standard i))
	(if (= ch ?\ )
	    (setq ch (- (aref quail-keyboard-layout i)))))
      (if (< ch 0)
	  (let ((last-command-event (- ch)))
	    (self-insert-command 1))
	(if (= ch ?\ )
	    (insert ch)
	  (let* ((map (cdr (assq ch (cdr (quail-map)))))
		 (translation (and map (quail-get-translation 
					(car map) (char-to-string ch) 1))))
	    (if (integerp translation)
		(insert translation)
	      (if (consp translation)
		  (insert (aref (cdr translation) (car translation)))
		(let ((last-command-event ch))
		  (self-insert-command 1)))))))
      (setq i (1+ i))))
  (newline))

(defun quail-translation-help ()
  "Show help message while translating in Quail mode."
  (interactive)
  (let ((package quail-current-package)
	(current-key quail-current-key))
    (with-output-to-temp-buffer "*Quail-Help*"
      (save-excursion
	(set-buffer standard-output)
	(let ((quail-current-package package))
	  (princ "You are translating the key sequence ")
	  (prin1 quail-current-key)
	  (princ" in Quail mode.\n")
	  (quail-help-insert-keymap-description
	   (quail-translation-keymap)
	   "-----------------------
key		binding
---		-------\n"))
	(help-mode)))))

(defun quail-conversion-help ()
  "Show help message while converting in Quail mode."
  (interactive)
  (let ((package quail-current-package)
	(str (buffer-substring (overlay-start quail-conv-overlay)
			       (overlay-end quail-conv-overlay))))
    (with-output-to-temp-buffer "*Quail-Help*"
      (save-excursion
	(set-buffer standard-output)
	(let ((quail-current-package package))
	  (princ "You are converting the string ")
	  (prin1 str)
	  (princ " in Quail mode.\n")
	  (quail-help-insert-keymap-description
	   (quail-conversion-keymap)
	   "-----------------------
key		binding
---		-------\n"))
	(help-mode)))))


(defvar quail-directory-name "quail"
  "Name of Quail directory which cotains Quail packages.
This is a sub-directory of LEIM directory.")

;;;###autoload
(defun quail-update-leim-list-file (dirname &rest dirnames)
  "Update entries for Quail packages in `LEIM' list file in directory DIRNAME.
DIRNAME is a directory containing Emacs input methods;
normally, it should specify the `leim' subdirectory
of the Emacs source tree.

It searches for Quail packages under `quail' subdirectory of DIRNAME,
and update the file \"leim-list.el\" in DIRNAME.

When called from a program, the remaining arguments are additional
directory names to search for Quail packages under `quail' subdirectory
of each directory."
  (interactive "FDirectory of LEIM: ")
  (setq dirname (expand-file-name dirname))
  (let ((leim-list (expand-file-name leim-list-file-name dirname))
	quail-dirs list-buf pkg-list pkg-buf pos)
    (if (not (file-writable-p leim-list))
	(error "Can't write to file \"%s\"" leim-list))
    (message "Updating %s ..." leim-list)
    (setq list-buf (find-file-noselect leim-list))

    ;; At first, clean up the file.
    (save-excursion
      (set-buffer list-buf)
      (goto-char 1)

      ;; Insert the correct header.
      (if (looking-at (regexp-quote leim-list-header))
	  (goto-char (match-end 0))
	(insert leim-list-header))
      (setq pos (point))
      (if (not (re-search-forward leim-list-entry-regexp nil t))
	  nil

	;; Remove garbages after the header.
	(goto-char (match-beginning 0))
	(if (< pos (point))
	    (delete-region pos (point)))

	;; Remove all entries for Quail.
	(while (re-search-forward leim-list-entry-regexp nil 'move)
	  (goto-char (match-beginning 0))
	  (setq pos (point))
	  (condition-case nil
	      (let ((form (read list-buf)))
		(when (equal (nth 3 form) ''quail-use-package)
		  (if (eolp) (forward-line 1))
		  (delete-region pos (point))))
	    (error
	     ;; Delete the remaining contents because it seems that
	     ;; this file is broken.
	     (message "Garbages in %s deleted" leim-list)
	     (delete-region pos (point-max)))))))

    ;; Search for `quail' subdirector under each DIRNAMES.
    (setq dirnames (cons dirname dirnames))
    (let ((l dirnames))
      (while l
	(setcar l (expand-file-name (car l)))
	(setq dirname (expand-file-name quail-directory-name (car l)))
	(if (file-readable-p dirname)
	    (setq quail-dirs (cons dirname quail-dirs))
	  (message "%s doesn't has `%s' subdirectory, just ignored"
		   (car l) quail-directory-name)
	  (setq quail-dirs (cons nil quail-dirs)))
	(setq l (cdr l)))
      (setq quail-dirs (nreverse quail-dirs)))

    ;; Insert input method registering forms.
    (while quail-dirs
      (setq dirname (car quail-dirs))
      (when dirname
	(setq pkg-list (directory-files dirname 'full "\\.el$" 'nosort))
	(while pkg-list
	  (message "Checking %s ..." (car pkg-list))
	  (with-temp-buffer
	    (insert-file-contents (car pkg-list))
	    (goto-char (point-min))
	    (while (search-forward "(quail-define-package" nil t)
	      (goto-char (match-beginning 0))
	      (condition-case nil
		  (let ((form (read (current-buffer))))
		    (save-excursion
		      (set-buffer list-buf)
		      (insert
		       (format "(register-input-method
 %S %S '%s
 %S %S
 %S)\n"
			       (nth 1 form) ; PACKAGE-NAME
			       (nth 2 form) ; LANGUAGE
			       'quail-use-package ; ACTIVATE-FUNC
			       (nth 3 form) ; PACKAGE-TITLE
			       (progn	; PACKAGE-DESCRIPTION (one line)
				 (string-match ".*" (nth 5 form))
				 (match-string 0 (nth 5 form)))
			       (file-relative-name ; PACKAGE-FILENAME
				(file-name-sans-extension (car pkg-list))
				(car dirnames))))))
		(error
		 ;; Ignore the remaining contents of this file.
		 (goto-char (point-max))
		 (message "Some part of \"%s\" is broken" dirname)))))
	  (setq pkg-list (cdr pkg-list)))
	(setq quail-dirs (cdr quail-dirs) dirnames (cdr dirnames))))

    ;; At last, write out LEIM list file.
    (save-excursion
      (set-buffer list-buf)
      (setq buffer-file-coding-system 'iso-2022-7bit)
      (save-buffer 0))
    (kill-buffer list-buf)
    (message "Updating %s ... done" leim-list)))
;;
(provide 'quail)

;;; quail.el ends here
