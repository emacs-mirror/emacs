;;; quail.el -- provides simple input method for multilingual text

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

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
  "The current Quail package to input multilingual text in Quail minor mode.
See the documentation of `quail-package-alist' for the format.")
(make-variable-buffer-local 'quail-current-package)
(put 'quail-current-package 'permanent-local t)

;; Quail uses the following two buffers to assist users.
;; A buffer to show available key sequence or translation list.
(defvar quail-guidance-buf nil)
;; A buffer to show completion list of the current key sequence.
(defvar quail-completion-buf nil)

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
  "Cons of indices and vector of possible translations of the current key.")

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
  CONVERSION-KEYS.

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

Installation of LEIM for Quail is very simple, just copy Quail
packages (byte-compiled Emacs Lisp files) to somewhere in your
`load-path'.

LEIM is available from the same ftp directory as Emacs."))
	  (error ""))
      (setq libraries (cdr libraries))))
  (quail-select-package package-name)
  (setq current-input-method-title (quail-title))
  (quail-mode 1))

(defun quail-inactivate ()
  "Turn off Quail input method."
  (interactive)
  (throw 'quail-tag t))

(or (assq 'quail-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(quail-mode " Quail") minor-mode-alist)))

(defvar quail-mode-map 
  (let ((map (make-keymap))
	(i ? ))
    (while (< i 127)
      (define-key map (char-to-string i) 'quail-start-translation)
      (setq i (1+ i)))
    map)
  "Keymap for Quail mode.")

(or (assq 'quail-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'quail-mode quail-mode-map) minor-mode-map-alist)))

(defvar quail-translation-keymap
  (let ((map (make-keymap))
	(i 0))
    (while (< i ?\ )
      (define-key map (char-to-string i) 'quail-execute-non-quail-command)
      (setq i (1+ i)))
    (while (< i 127)
      (define-key map (char-to-string i) 'quail-self-insert-command)
      (setq i (1+ i)))
    (define-key map "\177" 'quail-delete-last-char)
    (define-key map "\C-\\" 'quail-inactivate)
    (define-key map "\C-f" 'quail-next-translation)
    (define-key map "\C-b" 'quail-prev-translation)
    (define-key map "\C-n" 'quail-next-translation-block)
    (define-key map "\C-p" 'quail-prev-translation-block)
    (define-key map "\C-i" 'quail-completion)
    (define-key map "\C-@" 'quail-select-current)
    (define-key map "\C-c" 'quail-abort-translation)
    (define-key map "\C-h" 'quail-translation-help)
    (define-key map [tab] 'quail-completion)
    (define-key map [delete] 'quail-delete-last-char)
    (define-key map [backspace] 'quail-delete-last-char)
    ;; At last, define default key binding.
    (append map '((t . quail-execute-non-quail-command))))
  "Keymap used processing translation in Quail mode.
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
    (define-key map "\C-b" 'quail-conversion-backward-char)
    (define-key map "\C-f" 'quail-conversion-forward-char)
    (define-key map "\C-a" 'quail-conversion-beginning-of-region)
    (define-key map "\C-e" 'quail-conversion-end-of-region)
    (define-key map "\C-d" 'quail-conversion-delete-char)
    (define-key map "\C-h" 'quail-conversion-help)
    (define-key map "\C-\\" 'quail-inactivate)
    (define-key map "\177" 'quail-conversion-backward-delete-char)
    (define-key map [delete] 'quail-conversion-backward-delete-char)
    (define-key map [backspace] 'quail-conversion-backward-delete-char)
    ;; At last, define default key binding.
    (append map '((t . quail-execute-non-quail-command))))
  "Keymap used for processing conversion in Quail mode.
This map is activated while convesion region is active but translation
region is not active.")

(defun quail-define-package (name language title
				  &optional guidance docstring translation-keys
				  forget-last-selection deterministic
				  kbd-translate show-layout create-decode-map
				  maximum-shortest overlay-plist
				  update-translation-function
				  conversion-keys)
  "Define NAME as a new Quail package for input LANGUAGE.
TITLE is a string to be displayed at mode-line to indicate this package.
Optional arguments are GUIDANCE, DOCSTRING, TRANLSATION-KEYS,
 FORGET-LAST-SELECTION, DETERMINISTIC, KBD-TRANSLATE, SHOW-LAYOUT,
 CREATE-DECODE-MAP, MAXIMUM-SHORTEST, OVERLAY-PLIST,
 UPDATE-TRANSLATION-FUNCTION, and CONVERSION-KEYS.

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
vs. corresponding command to be called."
  (let (translation-keymap conversion-keymap)
    (if deterministic (setq forget-last-selection t))
    (if translation-keys
      (progn
	(setq translation-keymap (copy-keymap quail-translation-keymap))
	(while translation-keys
	  (define-key translation-keymap
	    (car (car translation-keys)) (cdr (car translation-keys)))
	  (setq translation-keys (cdr translation-keys))))
      (setq translation-keymap quail-translation-keymap))
    (if conversion-keys
      (progn
	(setq conversion-keymap (copy-keymap quail-conversion-keymap))
	(while conversion-keys
	  (define-key conversion-keymap
	    (car (car conversion-keys)) (cdr (car conversion-keys)))
	  (setq conversion-keys (cdr conversion-keys)))))
    (quail-add-package
     (list name title (list nil) guidance (or docstring "")
	   translation-keymap
	   forget-last-selection deterministic kbd-translate show-layout
	   (if create-decode-map (list 'decode-map) nil)
	   maximum-shortest overlay-plist update-translation-function
	   conversion-keymap)))
  (register-input-method language (list name 'quail-use-package))
  (quail-select-package name))

;; Quail minor mode handlers.

;; Setup overlays used in Quail mode.
(defun quail-setup-overlays ()
  (let ((pos (point)))
    (if (overlayp quail-overlay)
	(move-overlay quail-overlay pos pos)
      (setq quail-overlay (make-overlay pos pos nil nil t))
      (overlay-put quail-overlay 'face 'underline)
      (let ((l (quail-overlay-plist)))
	(while l
	  (overlay-put quail-overlay (car l) (car (cdr l)))
	  (setq l (cdr (cdr l))))))
    (if (overlayp quail-conv-overlay)
	(move-overlay quail-conv-overlay pos pos)
      (setq quail-conv-overlay (make-overlay pos pos nil nil t))
      (overlay-put quail-conv-overlay 'face 'underline)
      ;;(overlay-put quail-conv-overlay 'modification-hooks
      ;;'(quail-conv-overlay-modification-hook))
      )))

;; Delete overlays used in Quail mode.
(defun quail-delete-overlays ()
  (if (overlayp quail-overlay)
      (delete-overlay quail-overlay))
  (if (overlayp quail-conv-overlay)
      (delete-overlay quail-conv-overlay)))

;; While translating and converting, we enter the recursive edit and
;; exit it frequently, which results in frequent and annoying change
;; of and annoying in mode line.   To avoid it, we use a modified
;; mode-line-format.
(defvar quail-mode-line-format nil)

;; Return a modified mode-line-format which doesn't show the recursive
;; editing level.  But, we only pay attention to the top level
;; elements of the current mode-line-format.
(defun quail-generate-mode-line-format ()
  (if (listp mode-line-format)
      (let ((new (copy-sequence mode-line-format))
	    l elt idx)
	(setq l new)
	(while l
	  (setq elt (car l))
	  (if (and (stringp elt)
		   (or (setq idx (string-match "%\\[" elt))
		       (setq idx (string-match "%\\]" elt))))
	      (setcar l (concat (substring elt 0 idx)
				(substring elt (+ idx 2)))))
	  (setq l (cdr l)))
	new)
    mode-line-format))

(defun quail-mode (&optional arg)
  "Toggle Quail minor mode.
With arg, turn Quail mode on if and only if arg is positive.
Try \\[describe-bindings] in Quail mode to see the available key binding.
The command \\[describe-input-method] describes the current Quail package."
  (interactive "P")
  (setq quail-mode
	(if (null arg) (null quail-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (null quail-mode)
      ;; Let's turn off Quail mode.
      (progn
	(quail-hide-guidance-buf)
	(quail-delete-overlays)
	(setq describe-current-input-method-function nil)
	(setq current-input-method nil)
	(run-hooks 'quail-mode-exit-hook)
	(run-hooks 'input-method-inactivate-hook))
    ;; Let's turn on Quail mode.
    (if (null quail-current-package)
	;; Quail package is not yet selected.  Select one now.
	(let (name)
	  (if quail-package-alist
	      (setq name (car (car quail-package-alist)))
	    (setq quail-mode nil)
	    (error "No Quail package loaded"))
	  (quail-select-package name)))
    (setq inactivate-current-input-method-function 'quail-mode)
    (setq describe-current-input-method-function 'quail-help)
    (setq quail-mode-line-format (quail-generate-mode-line-format))
    (quail-delete-overlays)
    (quail-show-guidance-buf)
    ;; If we are in minibuffer, turn off Quail mode before exiting.
    (if (eq (selected-window) (minibuffer-window))
	(add-hook 'minibuffer-exit-hook 'quail-exit-from-minibuffer))
    (make-local-hook 'post-command-hook)
    (run-hooks 'quail-mode-hook)
    (run-hooks 'input-method-activate-hook))
  (force-mode-line-update))

(defun quail-exit-from-minibuffer ()
  (if quail-mode (quail-mode -1))
  (if (<= (minibuffer-depth) 1)
      (remove-hook 'minibuffer-exit-hook 'quail-exit-from-minibuffer)))

(defvar quail-saved-overriding-local-map nil)
(defvar quail-saved-current-buffer nil)

;; Toggle `quail-mode'.  This function is added to `post-command-hook'
;; in Quail mode, to turn Quail mode temporarily off, or back on
;; after one non-Quail command.
(defun quail-toggle-mode-temporarily ()
  (if quail-mode
      ;; We are going to handle following events out of Quail mode.
      (setq quail-mode nil
	    quail-saved-overriding-local-map overriding-local-map
	    quail-saved-current-buffer (current-buffer)
	    overriding-local-map nil)
    ;; We have just executed one non-Quail command.  We don't need
    ;; this hook any more.
    (remove-hook 'post-command-hook 'quail-toggle-mode-temporarily t)
    ;; If the command changed the current buffer, we should not go
    ;; back to Quail mode.
    (if (not (eq (current-buffer) quail-saved-current-buffer))
	(throw 'quail-tag nil)
      ;; Let's go back to Quail mode.
      (setq quail-mode t)
      (setq overriding-local-map quail-saved-overriding-local-map)
      ;; If whole text in conversion area was deleted, exit from the
      ;; recursive edit.
      (let ((start (overlay-start quail-conv-overlay)))
	(if (and start (= start (overlay-end quail-conv-overlay)))
	    (throw 'quail-tag nil)))
      )))

(defun quail-execute-non-quail-command ()
  "Execute one non-Quail command in Quail mode.
The current translation and conversion are terminated."
  (interactive)
  (setq unread-command-events (cons last-input-event unread-command-events))
  (quail-delete-overlays)
  (if (buffer-live-p quail-guidance-buf)
      (save-excursion
	(set-buffer quail-guidance-buf)
	(erase-buffer)))
  (throw 'quail-tag nil))

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
  1!2@3#4$5%6^7&8*9(0)-_=+`~  \
  qQwWeErRtTyYuUiIoOpP[{]}    \
  aAsSdDfFgGhHjJkKlL;:'\"\\|    \
  zZxXcCvVbBnNmM,<.>/?        "
  "Standard keyboard layout of printable characters Quail assumes.
See the documentation of `quail-keyboard-layout' for this format.
This layout is almost the same as that of VT100,
 but the location of key \\ (backslash) is just right of key ' (single-quote),
 not right of RETURN key.")

(defvar quail-keyboard-layout quail-keyboard-layout-standard
  "A string which represents physical key layout of a particular keyboard.
We assume there are four rows and each row has 15 keys (columns),
	the first column of the first row is left of key '1',
	the first column of the second row is left of key `q',
	the first column of the third row is left of key `a',
	the first column of the fourth row is left of key `z'.
Nth (N is even) and (N+1)th characters in the string are non-shifted
 and shifted characters respectively at the same location.
The location of Nth character is row (N / 30) and column ((N mod 30) / 2).")

(defconst quail-keyboard-layout-len 120)

;; Here we provide several examples of famous keyboard layouts.

(defvar quail-keyboard-layout-alist
  (list
   '("sun-type3" . "\
  1!2@3#4$5%6^7&8*9(0)-_=+\\|`~\
  qQwWeErRtTyYuUiIoOpP[{]}    \
  aAsSdDfFgGhHjJkKlL;:'\"      \
  zZxXcCvVbBnNmM,<.>/?        ")
   (cons "standard" quail-keyboard-layout-standard))
  "Alist of keyboard names and corresponding layout strings.
See the documentation of `quail-keyboard-layout' for the format of
 the layout string.")

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
      ch
    (let ((i 0))
      (while (and (< i quail-keyboard-layout-len)
		  (/= ch (aref quail-keyboard-layout i)))
	(setq i (1+ i)))
      (if (= i quail-keyboard-layout-len)
	  (error "Character `%c' not found in your keyboard layout" ch))
      (aref quail-keyboard-layout-standard i))))

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
	 (or (integerp translation) (consp translation) (null translation)
	     (vectorp translation) (stringp translation)
	     (symbolp translation)))
       (let ((alist (cdr object)))
	 (or (listp alist) (symbolp alist)))))

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

(defun quail-install-map (map)
  "Install the Quail map MAP in the current Quail package.
The installed map can be referred by the function `quail-map'."
  (if (null quail-current-package)
      (error "No current Quail package"))
  (if (null (quail-map-p map))
      (error "Invalid Quail map `%s'" map))
  (setcar (cdr (cdr quail-current-package)) map))

(defun quail-defrule (key translation &optional name)
  "Add one translation rule, KEY to TRANSLATION, in the current Quail package.
KEY is a string meaning a sequence of keystrokes to be translated.
TRANSLATION is a character, a string, a vector, a Quail map, or a function.
It it is a character, it is the sole translation of KEY.
If it is a string, each character is a candidate for the translation.
If it is a vector, each element (string or character) is a candidate
  for the translation.
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

;; Define KEY as TRANS in a Quail map MAP.
(defun quail-defrule-internal (key trans map)
  (if (null (stringp key))
      "Invalid Quail key `%s'" key)
  (if (not (or (numberp trans) (stringp trans) (vectorp trans)
	       (symbolp trans)
	       (quail-map-p trans)))
      (error "Invalid Quail translation `%s'" trans))
  (if (null (quail-map-p map))
      (error "Invalid Quail map `%s'" map))
  (let ((len (length key))
	(idx 0)
	ch entry)
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

(defun quail-get-translation (map key len)
  "Return the translation specified in Quail map MAP for KEY of length LEN.
The translation is either a character or a cons of the form (INDEX . VECTOR),
where VECTOR is a vector of candidates (character or string) for
the translation, and INDEX points into VECTOR to specify the currently
selected translation."
  (let ((def (car map)))
    (if (and def (symbolp def))
	;; DEF is a symbol of a function which returns valid translation.
	(setq def (funcall def key len)))
    (cond
     ((or (integerp def) (consp def))
      def)

     ((null def)
      ;; No translation.
      nil)

     ((stringp def)
      ;; Each character in DEF is a candidate of translation.  Reform
      ;; it as (INDEX . VECTOR).
      (setq def (string-to-vector def))
      ;; But if the length is 1, we don't need vector but a single
      ;; character as the translation.
      (if (= (length def) 1)
	  (aref def 0)
	(cons 0 def)))

     ((vectorp def)
      ;; Each element (string or character) in DEF is a candidate of
      ;; translation.  Reform it as (INDEX . VECTOR).
      (cons 0 def))

     (t
      (error "Invalid object in Quail map: %s" def)))))

(defun quail-lookup-key (key len)
  "Lookup KEY of length LEN in the current Quail map and return the definition.
The returned value is a Quail map specific to KEY."
  (let ((idx 0)
	(map (quail-map))
	(kbd-translate (quail-kbd-translate))
	slot ch translation)
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
    (if (and map (setq translation (quail-get-translation map key len)))
	(progn
	  ;; We may have to reform car part of MAP.
	  (if (not (equal (car map) translation))
	      (setcar map translation))
	  (if (consp translation) 
	      (progn
		(setq quail-current-translations translation)
		(if (quail-forget-last-selection)
		    (setcar quail-current-translations 0))))
	  ;; We may have to reform cdr part of MAP.
	  (if (and (cdr map) (symbolp (cdr map)))
	      (progn
		(setcdr map (funcall (cdr map) key len))))
	  ))
    map))

(defun quail-conv-overlay-modification-hook (overlay after &rest ignore)
  (if (and after
	   (= (overlay-start overlay) (overlay-end overlay)))
      ;; Whole text in conversion area was deleted.  Let's exit from
      ;; the recursive edit.
      (throw 'exit nil)))

(defvar quail-suppress-conversion nil
  "If non-nil, suppress converting facility of the current Quail package.")

;; If set to non-nil, exit conversion mode before starting new translation.
(defvar quail-exit-conversion-mode nil)

(defun quail-start-translation ()
  "Start translating the typed character in Quail mode."
  (interactive "*")
  (setq unread-command-events
	(cons last-command-event unread-command-events))
  ;; Check the possibility of translating the last key.
  (if (assq last-command-event (cdr (quail-map)))
      ;; Ok, we can start translation.
      (let ((mode-line-format quail-mode-line-format))
	(quail-setup-overlays)
	(if (catch 'quail-tag
	      (if (and (not quail-suppress-conversion)
		       (quail-conversion-keymap))
		  ;; We must start translation in conversion mode.
		  (let ((overriding-local-map (quail-conversion-keymap)))
		    (setq quail-exit-conversion-mode nil)
		    (recursive-edit)
		    (if (and auto-fill-function
			     (> (current-column) (current-fill-column)))
			(run-hooks 'auto-fill-function)))
		(let ((overriding-local-map (quail-translation-keymap)))
		  (setq quail-current-key "")
		  (recursive-edit)))
	      (if (prog1 (< (overlay-start quail-conv-overlay)
			    (overlay-end quail-conv-overlay))
		    (delete-overlay quail-conv-overlay))
		  (run-hooks 'input-method-after-insert-chunk-hook))
	      nil)
	    ;; Someone has thrown a tag with value t, which means
	    ;; we should turn Quail mode off.
	    (quail-mode -1)))
    ;; Since the typed character doesn't start any translation, handle
    ;; it out of Quail mode.  We come back to Quail mode later because
    ;; function `quail-toggle-mode-temporarily' is in
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
  (if (or quail-exit-conversion-mode
	  (not (quail-point-in-conversion-region)))
      (progn
	;; We must start translation with new conversion region.
	(setq quail-exit-conversion-mode nil)
	(throw 'exit nil)))
  ;; Check the possibility of translating the last key.
  (if (assq last-command-event (cdr (quail-map)))
      ;; Ok, we can start translation.
      (let ((overriding-local-map (quail-translation-keymap)))
	(setq quail-current-key "")
	(move-overlay quail-overlay (point) (point))
	(recursive-edit))
    ;; Since the typed character doesn't start any translation, handle
    ;; it out of Quail mode.  We come back to Quail mode later because
    ;; function `quail-toggle-mode-temporarily' is in
    ;; `post-command-hook'.
    (add-hook 'post-command-hook 'quail-toggle-mode-temporarily nil t)))

(defun quail-terminate-translation ()
  "Terminate the translation of the current key."
  (let ((start (overlay-start quail-overlay)))
    (if (and start
	     (< start (overlay-end quail-overlay)))
	;; Here we simulate self-insert-command.
	(let (last-command-char)
	  (goto-char start)
	  ;; The first one might want to expand an abbrev.
	  (setq last-command-char (following-char))
	  (delete-char 1)
	  (self-insert-command 1)
	  (if (< (point) (overlay-end quail-overlay))
	      (if overwrite-mode
		  (while (< (point) (overlay-end quail-overlay))
		    (setq last-command-char (following-char))
		    (delete-char 1)
		    (self-insert-command 1))
		;; The last one might still want to auto-fill.
		(goto-char (overlay-end quail-overlay))
		(let ((last-command-char (preceding-char)))
		  (delete-char -1)
		  (self-insert-command 1)))))))
  (delete-overlay quail-overlay)
  (if (buffer-live-p quail-guidance-buf)
      (save-excursion
	(set-buffer quail-guidance-buf)
	(erase-buffer)))
  (throw 'exit nil))

(defsubst quail-delete-region ()
  "Delete the text in the current translation region of Quail."
  (delete-region (overlay-start quail-overlay) (overlay-end quail-overlay)))

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
	    (insert (or quail-current-str
			(substring quail-current-key 0 len))))
	(insert (or quail-current-str quail-current-key)))))
  (quail-update-guidance)
  (if control-flag
      (quail-terminate-translation)))

(defun quail-self-insert-command ()
  "Add the typed character to the key for translation."
  (interactive "*")
  (setq quail-current-key
	(concat quail-current-key (char-to-string last-command-event)))
  (quail-update-translation (quail-translate-key)))

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
	(let ((def (car map)))
	  (setq quail-current-str
		(if (consp def) (aref (cdr def) (car def)) def))
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
	      (setq def (car (quail-lookup-key quail-current-key (- len 2))))
	      (quail-lookup-key (substring quail-current-key -2) 2))
	     ;; Now the sequence is "...ABCD", which can be split into
	     ;; "...AB" and "CD..." to get valid translation.
	     ;; At first, get translation of "...AB".
	     (setq quail-current-str
		   (if (consp def) (aref (cdr def) (car def)) def))
	     ;; Then, return the length of "...AB".
	     (- len 2))

	    ((and quail-current-translations
		  (not (quail-deterministic))
		  (setq ch (aref quail-current-key (1- len)))
		  (>= ch ?0) (<= ch ?9))
	     ;; A numeric key is entered to select a desirable translation.
	     (setq quail-current-key (substring quail-current-key 0 -1))
	     (quail-select-translation
	      (+ (* (/ (car quail-current-translations) 10) 10)
		 ;; We treat key 1,2..,9,0 as specifying 0,1,..8,9.
		 (if (= ch ?0) 9 (- ch ?1))))
	     ;; And, we can terminate the current translation.
	     t)

	    (t
	     ;; No way to handle the last character in this context.
	     (1- len))))))

(defun quail-next-translation ()
  "Select next translation in the current batch of candidates."
  (interactive)
  (if quail-current-translations
      (progn
	(quail-select-translation (1+ (car quail-current-translations)))
	(quail-update-translation nil))
    (beep)))

(defun quail-prev-translation ()
  "Select previous translation in the current batch of candidates."
  (interactive)
  (if quail-current-translations
      (progn
	(quail-select-translation (1- (car quail-current-translations)))
	(quail-update-translation nil))
    (beep)))

(defun quail-next-translation-block ()
  "Select the next batch of 10 translation candidates."
  (interactive)
  (if quail-current-translations
      (let ((limit (1- (length (cdr quail-current-translations))))
	    (n (car quail-current-translations)))
	(if (< (/ n 10) (/ limit 10))
	    (progn
	      (quail-select-translation (min (+ n 10) limit))
	      (quail-update-translation nil))
	  ;; We are already at the last block.
	  (beep)))
    (beep)))

(defun quail-prev-translation-block ()
  "Select the previous batch of 10 translation candidates."
  (interactive)
  (if (and quail-current-translations
	   (>= (car quail-current-translations) 10))
      (progn
	(quail-select-translation (- (car quail-current-translations) 10))
	(quail-update-translation nil))
    (beep)))

(defun quail-select-translation (n)
  "Select Nth translation in the current batch of translation candidates."
  (if (or (< n 0) (>= n (length (cdr quail-current-translations))))
      (beep)
    (setcar quail-current-translations n)
    (setq quail-current-str (aref (cdr quail-current-translations) n))))

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
  (if (= (overlay-start quail-conv-overlay)
	 (overlay-end quail-conv-overlay))
      (throw 'quail-tag nil)))

(defun quail-conversion-backward-delete-char ()
  (interactive)
  (if (<= (point) (overlay-start quail-conv-overlay))
      (error "Beginning of conversion region"))
  (delete-char -1)
  (if (= (overlay-start quail-conv-overlay)
	 (overlay-end quail-conv-overlay))
      (throw 'quail-tag nil)))

(defun quail-do-conversion (func &rest args)
  "Call FUNC to convert text in the current conversion region of Quail.
Remaining args are for FUNC."
  (delete-overlay quail-overlay)
  (apply func args))

(defun quail-no-conversion ()
  "Do no conversion of the current conversion region of Quail."
  (interactive)
  (throw 'exit nil))

;; Guidance, Completion, and Help buffer handlers.

(defun quail-show-guidance-buf ()
  "Display a Quail guidance buffer in some window.
Create the buffer if it does not exist yet.
The window is normally shown in a minibuffer,
but if the selected window is a minibuffer, it is shown in
the bottommost ordinary window."

  (if (or (null input-method-tersely-flag)
	  (not (eq (selected-window) (minibuffer-window))))
      (progn
	;; At first, setup a guidance buffer.
	(or (buffer-live-p quail-guidance-buf)
	    (setq quail-guidance-buf
		  (get-buffer-create " *Quail-guidance*")))
	(save-excursion
	  (let ((title (quail-title)))
	    (set-buffer quail-guidance-buf)
	    ;; Show the title of Quail package in the left of mode-line.
	    (setq current-input-method nil)
	    (setq current-input-method-title title)
	    (setq mode-line-format (cons '("[" current-input-method-title "]")
					 default-mode-line-format))
	    (erase-buffer)
	    (or (overlayp quail-overlay)
		(progn
		  (setq quail-overlay (make-overlay 1 1))
		  (overlay-put quail-overlay 'face 'highlight)))
	    (delete-overlay quail-overlay)
	    (set-buffer-modified-p nil)))
	(bury-buffer quail-guidance-buf)

	;; Then, display it in an appropriate window.
	(if (not (get-buffer-window quail-guidance-buf))
	    ;; Guidance buffer is not yet shown in any window.
	    (let ((win (minibuffer-window)))
	      (if (eq (selected-window) win)
		  ;; Since we are in minibuffer, we can't use it for guidance.
		  ;; Let's find the bottom window.
		  (let (height)
		    (setq win (window-at 0 (- (frame-height) 2)))
		    (setq height (window-height win))
		    ;; If WIN is too tall, split it vertically and use
		    ;; the lower one.
		    (if (>= height 4)
			(let ((window-min-height 2))
			  ;; Here, `split-window' returns a lower window
			  ;; which is what we wanted.
			  (setq win (split-window win (- height 2)))))
		    (set-window-buffer win quail-guidance-buf)
		    (set-window-dedicated-p win t))
		(set-window-buffer win quail-guidance-buf))))))

  ;; And, create a buffer for completion.
  (or (buffer-live-p quail-completion-buf)
      (progn
	(setq quail-completion-buf (get-buffer-create "*Quail Completions*"))
	(save-excursion
	  (set-buffer quail-completion-buf)
	  (setq quail-overlay (make-overlay 1 1))
	  (overlay-put quail-overlay 'face 'highlight))))
  (bury-buffer quail-completion-buf))

(defun quail-hide-guidance-buf ()
  "Hide the Quail guidance buffer."
  (let* ((win (minibuffer-window))
	 (buf (window-buffer win)))
    (if (eq buf quail-guidance-buf)
	;; Quail guidance buffer is at echo area.  Vacate it to the
	;; deepest minibuffer.
	(set-window-buffer win (format " *Minibuf-%d*" (minibuffer-depth)))
      ;; Delete the window for guidance buffer.
      (if (or (null input-method-tersely-flag)
	      (not (eq (selected-window) (minibuffer-window))))
	  (progn
	    (setq win (get-buffer-window quail-guidance-buf))
	    (set-window-dedicated-p win nil)
	    (delete-window win))))))

(defun quail-update-guidance ()
  "Update the Quail guidance buffer and completion buffer (if displayed now)."
  ;; Update guidance buffer.
  (if (or (null input-method-tersely-flag)
	  (not (eq (selected-window) (minibuffer-window))))
      (let ((guidance (quail-guidance)))
	(cond ((eq guidance t)
	       ;; Show the current possible translations.
	       (quail-show-translations))
	      ((null guidance)
	       ;; Show the current input keys.
	       (let ((key quail-current-key))
		 (save-excursion
		   (set-buffer quail-guidance-buf)
		   (erase-buffer)
		   (insert key))))
	      ((listp guidance)
	       ;; Show alternative characters specified in this alist.
	       (let* ((key quail-current-key)
		      (len (length key))
		      (i 0)
		      ch alternative)
		 (save-excursion
		   (set-buffer quail-guidance-buf)
		   (erase-buffer)
		   (while (< i len)
		     (setq ch (aref key i))
		     (setq alternative (cdr (assoc ch guidance)))
		     (insert (or alternative ch))
		     (setq i (1+ i)))))))))

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
  (let ((key quail-current-key)
	(map (quail-lookup-key quail-current-key (length quail-current-key))))
    (save-excursion
      (set-buffer quail-guidance-buf)
      (erase-buffer)

      ;; Show the current key.
      (insert key)

      ;; Show possible following keys.
      (if (cdr map)
	  (let ((l (cdr map)))
	    (insert "[")
	    (while l
	      (insert (car (car l)))
	      (setq l (cdr l)))
	    (insert "]")))

      ;; Show list of translations.
      (if (consp (car map))
	  (let* ((idx (car (car map)))
		 (translations (cdr (car map)))
		 (from (* (/ idx 10) 10))
		 (to (min (+ from 10) (length translations))))
	    (indent-to 10)
	    (insert (format "(%d/%d)"
			    (1+ (/ from 10))
			    (1+ (/ (length translations) 10))))
	    (while (< from to)
	      ;; We show the last digit of FROM, but by changing
	      ;; 0,1,..,9 to 1,2,..,0.
	      (insert (format " %d."
			      (if (= (% from 10) 9) 0 (1+ (% from 10)))))
	      (let ((pos (point)))
		(insert (aref translations from))
		(if (= idx from)
		    (move-overlay quail-overlay pos (point))))
	      (setq from (1+ from)))))
      )))

(defun quail-completion ()
  "List all completions for the current key.
All possible translations of the current key and whole possible longer keys
 are shown."
  (interactive)
  (let ((key quail-current-key)
	(map (quail-lookup-key quail-current-key (length quail-current-key))))
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
;; indentation INDENT."
(defun quail-completion-list-translations (map key indent)
  (let ((translations
	 (quail-get-translation map key (length key))))
    (if (integerp translations)
	(insert "(1/1) 1." translations "\n")
      ;; We need only vector part.
      (setq translations (cdr translations))
      ;; Insert every 10 elements with indices in a line.
      (let ((len (length translations))
	    (i 0)
	    (first t)
	    num)
	(while (< i len)
	  (if first
	      (progn
		(insert "(1/1)")
		(setq first nil))
	    (if (= (% i 10) 0)
		(progn
		  (newline)
		  (indent-to indent)
		  (insert (format "(%d/%d)" (1+ (/ i 10)) (1+ (/ len 10)))))))
	  ;; We show the last digit of FROM while converting
	  ;; 0,1,..,9 to 1,2,..,0.
	  (insert (format " %d." (if (= (% i 10) 9) 0 (1+ (% i 10)))))
	  (insert (aref translations i))
	  (setq i (1+ i)))
	(newline)))))

(defun quail-help ()
  "Show brief description of the current Quail package."
  (interactive)
  (let ((package quail-current-package)
	(buf (get-buffer-create "*Quail-help*")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (setq quail-current-package package)
      (insert "Quail input method (name:"
	      (quail-name)
	      ", mode line indicator:["
	      (quail-title)
	      "])\n---- Documentation ----\n"
	      (quail-docstring))
      (newline)
      (if (quail-show-layout) (quail-show-kbd-layout))
      (insert )
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
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (help-mode))
    (display-buffer buf)))

(defun quail-help-insert-keymap-description (keymap &optional header)
  (let (from to)
    (if header
	(insert header))
    (save-excursion
      (save-window-excursion
	(let ((overriding-local-map keymap))
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
      (if (= ch ?\ )
	  (insert ch)
	(let ((map (cdr (assq ch (cdr (quail-map))))))
	  (if map
	      (let ((translation
		     (quail-get-translation map (char-to-string ch) 1)))
		(if (integerp translation)
		    (insert translation)
		  (insert (aref (cdr translation) (car translation)))))
	    (insert ch))))
      (setq i (1+ i))))
  (newline))

(defun quail-translation-help ()
  "Show help message while translating in Quail mode."
  (interactive)
  (let ((package quail-current-package)
	(current-key quail-current-key)
	(buf (get-buffer-create "*Quail-Help*")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (setq quail-current-package package)
      (insert
       (format "You are translating the key sequence \"%s\" in Quail mode.\n"
	       quail-current-key))
      (quail-help-insert-keymap-description
       (quail-translation-keymap)
       "-----------------------
key		binding
---		-------\n")
      (goto-char (point-min))
      (set-buffer-modified-p nil))
    (display-buffer buf)))
  
(defun quail-conversion-help ()
  "Show help message while converting in Quail mode."
  (interactive)
  (let ((package quail-current-package)
	(str (buffer-substring (overlay-start quail-conv-overlay)
				 (overlay-end quail-conv-overlay)))
	(buf (get-buffer-create "*Quail-Help*")))
    (save-excursion
      (set-buffer buf)
      (erase-buffer)
      (setq quail-current-package package)
      (insert
       (format "You are converting the string \"%s\" in Quail mode.\n" str))
      (quail-help-insert-keymap-description
       (quail-conversion-keymap)
       "-----------------------
key		binding
---		-------\n")
      (goto-char (point-min))
      (set-buffer-modified-p nil))
    (display-buffer buf)))

;;
(provide 'quail)

;;; quail.el ends here
