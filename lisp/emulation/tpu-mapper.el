;;; tpu-mapper.el  ---  Create a TPU-edt keymap file for x-windows emacs.

;; Copyright (C) 1993, 1994, 1995 Free Software Foundation, Inc.

;; Author: Rob Riepel <riepel@networking.stanford.edu>
;; Maintainer: Rob Riepel <riepel@networking.stanford.edu>
;; Keywords: emulations

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

;;  This emacs lisp program can be used to create an emacs lisp file that
;;  defines the TPU-edt keypad for emacs running on x-windows.  Please read
;;  the "Usage" AND "Known Problems" sections before attempting to run this
;;  program.

;;; Usage:

;;  Simply load this file into the X-windows version of emacs (version 19)
;;  using the following command.

;;    emacs -q -l tpu-mapper.el

;;  The "-q" option prevents loading of your .emacs file (commands therein
;;  might confuse this program).

;;  An instruction screen showing the TPU-edt keypad will be displayed, and
;;  you will be prompted to press the TPU-edt editing keys.  Tpu-mapper uses
;;  the keys you press to create an Emacs Lisp file that will define a
;;  TPU-edt keypad for your X server.  You can even re-arrange the standard
;;  EDT keypad to suit your tastes (or to cope with those silly Sun and PC
;;  keypads).

;;  Finally, you will be prompted for the name of the file to store the key
;;  definitions.  If you chose the default, TPU-edt will find it and load it
;;  automatically.  If you specify a different file name, you will need to
;;  set the variable "tpu-xkeys-file" before loading TPU-edt.  Here's how
;;  you might go about doing that in your .emacs file.

;;    (setq tpu-xkeys-file (expand-file-name "~/.my-emacs-x-keys"))
;;    (load "tpu-edt")

;;; Known Problems:

;;  Sometimes, tpu-mapper will ignore a key you press, and just continue to
;;  prompt for the same key.  This can happen when your window manager sucks
;;  up the key and doesn't pass it on to Emacs, or it could be an Emacs bug.
;;  Either way, there's nothing that tpu-mapper can do about it.  You must
;;  press RETURN, to skip the current key and continue.  Later, you and/or
;;  your local X guru can try to figure out why the key is being ignored.

;;; Code:

;;;
;;;  Make sure we're running X-windows and Emacs version 19
;;;
(cond
 ((not (and window-system (not (string-lessp emacs-version "19"))))
  (error "tpu-mapper requires running in Emacs 19, with an X display")))


;;;
;;;  Decide whether we're running Lucid Emacs or Emacs itself.
;;;
(defconst tpu-lucid-emacs19-p (string-match "Lucid" emacs-version)
  "Non-NIL if we are running Lucid Emacs version 19.")


;;;
;;;  Key variables
;;;
(defvar tpu-kp4 nil)
(defvar tpu-kp5 nil)
(defvar tpu-key nil)
(defvar tpu-enter nil)
(defvar tpu-return nil)
(defvar tpu-key-seq nil)
(defvar tpu-enter-seq nil)
(defvar tpu-return-seq nil)


;;;
;;;  Make sure the window is big enough to display the instructions
;;;
(if tpu-lucid-emacs19-p (set-screen-size (selected-screen) 80 36)
  (set-frame-size (selected-frame) 80 36))


;;;
;;;  Create buffers - Directions, Keys, Gold-Keys
;;;
(if (not (get-buffer "Directions")) (generate-new-buffer "Directions"))
(if (not (get-buffer "Keys")) (generate-new-buffer "Keys"))
(if (not (get-buffer "Gold-Keys")) (generate-new-buffer "Gold-Keys"))


;;;
;;;  Put headers in the Keys buffer
;;;
(set-buffer "Keys")
(insert "\
;;  Key definitions for TPU-edt
;;
")


;;;
;;;   Display directions
;;;
(switch-to-buffer "Directions")
(insert "
    This program prompts you to press keys to create a custom keymap file
    for use with the x-windows version of Emacs and TPU-edt.

    Start by pressing the RETURN key, and continue by pressing the keys
    specified in the mini-buffer.  You can re-arrange the TPU-edt keypad
    by pressing any key you want at any prompt.  If you want to entirely
    omit a key, just press RETURN at the prompt.

    Here's a picture of the standard TPU/edt keypad for reference:

          _______________________    _______________________________
         | HELP  |      Do       |  |       |       |       |       |
         |KeyDefs|               |  |       |       |       |       |
         |_______|_______________|  |_______|_______|_______|_______|
          _______________________    _______________________________
         | Find  |Insert |Remove |  | Gold  | HELP  |FndNxt | Del L |
         |       |       |Sto Tex|  |  key  |E-Help | Find  |Undel L|
         |_______|_______|_______|  |_______|_______|_______|_______|
         |Select |Pre Scr|Nex Scr|  | Page  | Sect  |Append | Del W |
         | Reset |Pre Win|Nex Win|  |  Do   | Fill  |Replace|Undel W|
         |_______|_______|_______|  |_______|_______|_______|_______|
                 |Move up|          |Forward|Reverse|Remove | Del C |
                 |  Top  |          |Bottom |  Top  |Insert |Undel C|
          _______|_______|_______   |_______|_______|_______|_______|
         |Mov Lef|Mov Dow|Mov Rig|  | Word  |  EOL  | Char  |       |
         |StaOfLi|Bottom |EndOfLi|  |ChngCas|Del EOL|SpecIns| Enter |
         |_______|_______|_______|  |_______|_______|_______|       |
                                    |     Line      |Select | Subs  |
                                    |   Open Line   | Reset |       |
                                    |_______________|_______|_______|


")
(delete-other-windows)
(goto-char (point-min))

;;;
;;;  Save <CR> for future reference
;;;
(cond
 (tpu-lucid-emacs19-p
  (setq tpu-return-seq (read-key-sequence "Hit carriage-return <CR> to continue "))
  (setq tpu-return (concat "[" (format "%s" (event-key (aref tpu-return-seq 0))) "]")))
 (t
  (message "Hit carriage-return <CR> to continue ")
  (setq tpu-return-seq (read-event))
  (setq tpu-return (concat "[" (format "%s" tpu-return-seq) "]")))) 


;;;
;;;  Key mapping functions
;;;
(defun tpu-lucid-map-key (ident descrip func gold-func)
  (interactive)
  (setq tpu-key-seq (read-key-sequence (format "Press %s%s: " ident descrip)))
  (setq tpu-key (concat "[" (format "%s" (event-key (aref tpu-key-seq 0))) "]"))
  (cond ((not (equal tpu-key tpu-return))
	 (set-buffer "Keys")
	 (insert (format"(global-set-key %s %s)\n" tpu-key func))
	 (set-buffer "Gold-Keys")
	 (insert (format "(define-key GOLD-map %s %s)\n" tpu-key gold-func))
	 (set-buffer "Directions"))
	;; bogosity to get next prompt to come up, if the user hits <CR>!
	;; check periodically to see if this is still needed...
	(t
	 (format "%s" tpu-key)))
  tpu-key)

(defun tpu-emacs-map-key (ident descrip func gold-func)
  (interactive)
  (message "Press %s%s: " ident descrip)
  (setq tpu-key-seq (read-event))
  (setq tpu-key (concat "[" (format "%s" tpu-key-seq) "]"))
  (cond ((not (equal tpu-key tpu-return))
	 (set-buffer "Keys")
	 (insert (format"(global-set-key %s %s)\n" tpu-key func))
	 (set-buffer "Gold-Keys")
	 (insert (format "(define-key GOLD-map %s %s)\n" tpu-key gold-func))
	 (set-buffer "Directions"))
	;; bogosity to get next prompt to come up, if the user hits <CR>!
	;; check periodically to see if this is still needed...
	(t
	 (format "%s" tpu-key)))
  tpu-key)

(fset 'tpu-map-key (if tpu-lucid-emacs19-p 'tpu-lucid-map-key 'tpu-emacs-map-key))


(set-buffer "Keys")
(insert "
;;  Arrows
;;
")
(set-buffer "Gold-Keys")
(insert "
;;  GOLD Arrows
;;
")
(set-buffer "Directions")

(tpu-map-key "Up-Arrow"     ""  "'tpu-previous-line"  "'tpu-move-to-beginning")
(tpu-map-key "Down-arrow"   ""  "'tpu-next-line"      "'tpu-move-to-end")
(tpu-map-key "Right-arrow"  ""  "'tpu-forward-char"   "'end-of-line")
(tpu-map-key "Left-arrow"   ""  "'tpu-backward-char"  "'beginning-of-line")


(set-buffer "Keys")
(insert "
;;  PF keys
;;
")
(set-buffer "Gold-Keys")
(insert "
;;  GOLD PF keys
;;
")
(set-buffer "Directions")

(tpu-map-key "PF1"  " - The GOLD key"               "GOLD-map"                 "'keyboard-quit")
(tpu-map-key "PF2"  " - The Keypad Help key"        "'tpu-help"                 "'help-for-help")
(tpu-map-key "PF3"  " - The Find/Find-Next key"     "'tpu-search-again"         "'tpu-search")
(tpu-map-key "PF4"  " - The Del/Undelete Line key"  "'tpu-delete-current-line"  "'tpu-undelete-lines")

(set-buffer "Keys")
(insert "
;;  KP0-9 KP- KP, KP. and KPenter
;;
")
(set-buffer "Gold-Keys")
(insert "
;;  GOLD KP0-9 KP- KP, and KPenter
;;
")
(set-buffer "Directions")

(tpu-map-key "KP-0"      " - The Line/Open-Line key"               "'tpu-line"                 "'open-line")
(tpu-map-key "KP-1"      " - The Word/Change-Case key"             "'tpu-word"                 "'tpu-change-case")
(tpu-map-key "KP-2"      " - The EOL/Delete-EOL key"               "'tpu-end-of-line"          "'tpu-delete-to-eol")
(tpu-map-key "KP-3"      " - The Character/Special-Insert key"     "'tpu-char"                 "'tpu-special-insert")
(setq tpu-kp4 (tpu-map-key "KP-4"      " - The Forward/Bottom key"               "'tpu-advance-direction"    "'tpu-move-to-end"))
(setq tpu-kp5 (tpu-map-key "KP-5"      " - The Reverse/Top key"                  "'tpu-backup-direction"     "'tpu-move-to-beginning"))
(tpu-map-key "KP-6"      " - The Remove/Insert key"                "'tpu-cut"                  "'tpu-paste")
(tpu-map-key "KP-7"      " - The Page/Do key"                      "'tpu-page"                 "'execute-extended-command")
(tpu-map-key "KP-8"      " - The Section/Fill key"                 "'tpu-scroll-window"        "'tpu-fill")
(tpu-map-key "KP-9"      " - The Append/Replace key"               "'tpu-append-region"        "'tpu-replace")
(tpu-map-key "KP--"      " - The Delete/Undelete Word key"         "'tpu-delete-current-word"  "'tpu-undelete-words")
(tpu-map-key "KP-,"      " - The Delete/Undelete Character key"    "'tpu-delete-current-char"  "'tpu-undelete-char")
(tpu-map-key "KP-."      " - The Select/Reset key"                 "'tpu-select"               "'tpu-unselect")
(tpu-map-key "KP-Enter"  " - The Enter key on the numeric keypad"  "'newline"                  "'tpu-substitute")
;; Save the enter key
(setq tpu-enter tpu-key)
(setq tpu-enter-seq tpu-key-seq)

(set-buffer "Keys")
(insert "
;;  Editing keypad (find, insert, remove)
;;                 (select, prev, next)
;;
")
(set-buffer "Gold-Keys")
(insert "
;;  GOLD Editing keypad (find, insert, remove)
;;                      (select, prev, next)
;;
")
(set-buffer "Directions")

(tpu-map-key "Find"      " - The Find key on the editing keypad"       "'tpu-search"              "'nil")
(tpu-map-key "Insert"    " - The Insert key on the editing keypad"     "'tpu-paste"               "'nil")
(tpu-map-key "Remove"    " - The Remove key on the editing keypad"     "'tpu-cut"                 "'tpu-store-text")
(tpu-map-key "Select"    " - The Select key on the editing keypad"     "'tpu-select"              "'tpu-unselect")
(tpu-map-key "Prev Scr"  " - The Prev Scr key on the editing keypad"   "'tpu-scroll-window-down"  "'tpu-previous-window")
(tpu-map-key "Next Scr"  " - The Next Scr key on the editing keypad"   "'tpu-scroll-window-up"    "'tpu-next-window")

(set-buffer "Keys")
(insert "
;;  F10-14 Help Do F17
;;
")
(set-buffer "Gold-Keys")
(insert "
;;  GOLD F10-14 Help Do F17
;;
")
(set-buffer "Directions")

(tpu-map-key "F10"       " - Invokes the Exit function on VT200+ terminals"  "'tpu-exit"                    "'nil")
(tpu-map-key "F11"       " - Inserts an Escape character into the text"      "'tpu-insert-escape"           "'nil")
(tpu-map-key "Backspace" " - Not Delete nor ^H!  Sometimes on the F12 key"   "'tpu-next-beginning-of-line" "'nil")
(tpu-map-key "F13"       " - Invokes the delete previous word function"      "'tpu-delete-previous-word"   "'nil")
(tpu-map-key "F14"       " - Toggles insert/overstrike modes"                "'tpu-toggle-overwrite-mode"  "'nil")
(tpu-map-key "Help"      " - Brings up the help screen, same as PF2"         "'tpu-help"                   "'describe-bindings")
(tpu-map-key "Do"        " - Invokes the COMMAND function"                   "'execute-extended-command"   "'nil")
(tpu-map-key "F17"       ""                                                  "'tpu-goto-breadcrumb"        "'tpu-drop-breadcrumb")

(set-buffer "Gold-Keys")
(cond
 ((not (equal tpu-enter tpu-return))
  (insert "
;;  Minibuffer map additions to make KP_enter = RET
;;
")

  (insert (format "(define-key minibuffer-local-map %s 'exit-minibuffer)\n" tpu-enter))
  (insert (format "(define-key minibuffer-local-ns-map %s 'exit-minibuffer)\n" tpu-enter))
  (insert (format "(define-key minibuffer-local-completion-map %s 'exit-minibuffer)\n" tpu-enter))
  (insert (format "(define-key minibuffer-local-must-match-map %s 'minibuffer-complete-and-exit)\n" tpu-enter))))

(cond
 ((not (or (equal tpu-kp4 tpu-return) (equal tpu-kp5 tpu-return)))
  (insert "
;;  Minibuffer map additions to allow KP-4/5 termination of search strings.
;;
")

  (insert (format "(define-key minibuffer-local-map %s 'tpu-search-forward-exit)\n" tpu-kp4))
  (insert (format "(define-key minibuffer-local-map %s 'tpu-search-backward-exit)\n" tpu-kp5))))

(insert "
;;  Define the tpu-help-enter/return symbols
;;
")

(cond (tpu-lucid-emacs19-p
       (insert (format "(setq tpu-help-enter \"%s\")\n" tpu-enter-seq))
       (insert (format "(setq tpu-help-return \"%s\")\n" tpu-return-seq))
       (insert "(setq tpu-help-N \"[#<keypress-event N>]\")\n")
       (insert "(setq tpu-help-n \"[#<keypress-event n>]\")\n")
       (insert "(setq tpu-help-P \"[#<keypress-event P>]\")\n")
       (insert "(setq tpu-help-p \"[#<keypress-event p>]\")\n"))
      (t
       (insert (format "(setq tpu-help-enter \"%s\")\n" tpu-enter))))

(append-to-buffer "Keys" 1 (point))
(set-buffer "Keys")

;;;
;;;  Save the key mapping program
;;;
(let ((file (if tpu-lucid-emacs19-p "~/.tpu-lucid-keys" "~/.tpu-keys")))
  (set-visited-file-name
   (read-file-name (format "Save key mapping to file (default %s): " file) "" file)))
(save-buffer)

;;;
;;;  Load the newly defined keys and clean up
;;;
(eval-current-buffer)
(kill-buffer (current-buffer))
(kill-buffer "*scratch*")
(kill-buffer "Gold-Keys")

;;;
;;;  Let them know it worked.
;;;
(switch-to-buffer "Directions")
(erase-buffer)
(insert "
    A custom TPU-edt keymap file has been created.

    Press GOLD-k to remove this buffer and continue editing.
")
(goto-char (point-min))

;;; tpu-mapper.el ends here
