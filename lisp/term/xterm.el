;;; xterm.el --- define function key sequences and standard colors for xterm  -*- lexical-binding: t -*-

;; Copyright (C) 1995, 2001-2017 Free Software Foundation, Inc.

;; Author: FSF
;; Keywords: terminals

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defgroup xterm nil
  "XTerm support."
  :version "24.1"
  :group 'terminals)

(defconst xterm--extra-capabilities-type
  ;; NOTE: If you add entries here, make sure to update
  ;; `terminal-init-xterm' as well.
  '(set (const :tag "modifyOtherKeys support" modifyOtherKeys)
        (const :tag "report background" reportBackground)
        (const :tag "get X selection" getSelection)
        (const :tag "set X selection" setSelection)))

(defcustom xterm-extra-capabilities 'check
  "Whether Xterm supports some additional, more modern, features.
If nil, just assume that it does not.
If `check', try to check if it does.
If a list, assume that the listed features are supported, without checking.

The relevant features are:
  modifyOtherKeys  -- if supported, more key bindings work (e.g., \"\\C-,\")
  reportBackground -- if supported, Xterm reports its background color
  getSelection     -- if supported, Xterm yanks text from the X selection
  setSelection     -- if supported, Xterm saves killed text to the X selection"
  :version "24.1"
  :type `(choice (const :tag "Check" check)
                 ,xterm--extra-capabilities-type))

(defcustom xterm-max-cut-length 100000
  "Maximum number of bytes to cut into xterm using the OSC 52 sequence.

The OSC 52 sequence requires a terminator byte.  Some terminals will ignore or
mistreat a terminated sequence that is longer than a certain size, usually to
protect users from runaway sequences.

This variable allows you to tweak the maximum number of bytes that will be sent
using the OSC 52 sequence.

If you select a region larger than this size, it won't be copied to your system
clipboard.  Since clipboard data is base 64 encoded, the actual number of
string bytes that can be copied is 3/4 of this value."
  :version "25.1"
  :type 'integer)

(defcustom xterm-set-window-title nil
  "Whether Emacs should set window titles to an Emacs frame in an XTerm."
  :version "27.1"
  :type 'boolean)

(defconst xterm-paste-ending-sequence "\e[201~"
  "Characters send by the terminal to end a bracketed paste.")

(defun xterm--pasted-text ()
  "Handle the rest of a terminal paste operation.
Return the pasted text as a string."
  (let ((end-marker-length (length xterm-paste-ending-sequence)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (while (not (search-backward xterm-paste-ending-sequence
                                   (- (point) end-marker-length) t))
	(let ((event (read-event nil nil
                                 ;; Use finite timeout to avoid glomming the
                                 ;; event onto this-command-keys.
                                 most-positive-fixnum)))
	  (when (eql event ?\r)
	    (setf event ?\n))
	  (insert event)))
      (let ((last-coding-system-used))
	(decode-coding-region (point-min) (point) (keyboard-coding-system)
                              t)))))

(defun xterm-paste ()
  "Handle the start of a terminal paste operation."
  (interactive)
  (let* ((pasted-text (xterm--pasted-text))
         (interprogram-paste-function (lambda () pasted-text)))
    (yank)))

(define-key global-map [xterm-paste] #'xterm-paste)

(defvar xterm-rxvt-function-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\e[2~" [insert])
    (define-key map "\e[3~" [delete])
    (define-key map "\e[4~" [select])
    (define-key map "\e[5~" [prior])
    (define-key map "\e[6~" [next])

    (define-key map "\e[15~" [f5])
    (define-key map "\e[17~" [f6])
    (define-key map "\e[18~" [f7])
    (define-key map "\e[19~" [f8])
    (define-key map "\e[20~" [f9])
    (define-key map "\e[21~" [f10])

    (define-key map "\e[2;2~" [S-insert])

    ;; Other versions of xterm might emit these.
    (define-key map "\e[A" [up])
    (define-key map "\e[B" [down])
    (define-key map "\e[C" [right])
    (define-key map "\e[D" [left])

    (define-key map "\e[11~" [f1])
    (define-key map "\e[12~" [f2])
    (define-key map "\e[13~" [f3])
    (define-key map "\e[14~" [f4])

    ;; Recognize the start of a bracketed paste sequence.  The handler
    ;; internally recognizes the end.
    (define-key map "\e[200~" [xterm-paste])

    map)
  "Keymap of escape sequences, shared between xterm and rxvt support.")

(defvar xterm-function-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map xterm-rxvt-function-map)

    ;; xterm from X.org 6.8.2 uses these key definitions.
    (define-key map "\eOP" [f1])
    (define-key map "\eOQ" [f2])
    (define-key map "\eOR" [f3])
    (define-key map "\eOS" [f4])
    (define-key map "\e[23~" [f11])
    (define-key map "\e[24~" [f12])

    (define-key map "\eO2P" [S-f1])
    (define-key map "\eO2Q" [S-f2])
    (define-key map "\eO2R" [S-f3])
    (define-key map "\eO2S" [S-f4])
    (define-key map "\e[1;2P" [S-f1])
    (define-key map "\e[1;2Q" [S-f2])
    (define-key map "\e[1;2R" [S-f3])
    (define-key map "\e[1;2S" [S-f4])
    (define-key map "\e[15;2~" [S-f5])
    (define-key map "\e[17;2~" [S-f6])
    (define-key map "\e[18;2~" [S-f7])
    (define-key map "\e[19;2~" [S-f8])
    (define-key map "\e[20;2~" [S-f9])
    (define-key map "\e[21;2~" [S-f10])
    (define-key map "\e[23;2~" [S-f11])
    (define-key map "\e[24;2~" [S-f12])

    (define-key map "\eO5P" [C-f1])
    (define-key map "\eO5Q" [C-f2])
    (define-key map "\eO5R" [C-f3])
    (define-key map "\eO5S" [C-f4])
    (define-key map "\e[15;5~" [C-f5])
    (define-key map "\e[17;5~" [C-f6])
    (define-key map "\e[18;5~" [C-f7])
    (define-key map "\e[19;5~" [C-f8])
    (define-key map "\e[20;5~" [C-f9])
    (define-key map "\e[21;5~" [C-f10])
    (define-key map "\e[23;5~" [C-f11])
    (define-key map "\e[24;5~" [C-f12])

    (define-key map "\eO6P" [C-S-f1])
    (define-key map "\eO6Q" [C-S-f2])
    (define-key map "\eO6R" [C-S-f3])
    (define-key map "\eO6S" [C-S-f4])
    (define-key map "\e[15;6~" [C-S-f5])
    (define-key map "\e[17;6~" [C-S-f6])
    (define-key map "\e[18;6~" [C-S-f7])
    (define-key map "\e[19;6~" [C-S-f8])
    (define-key map "\e[20;6~" [C-S-f9])
    (define-key map "\e[21;6~" [C-S-f10])
    (define-key map "\e[23;6~" [C-S-f11])
    (define-key map "\e[24;6~" [C-S-f12])

    (define-key map "\eO3P" [M-f1])
    (define-key map "\eO3Q" [M-f2])
    (define-key map "\eO3R" [M-f3])
    (define-key map "\eO3S" [M-f4])
    (define-key map "\e[15;3~" [M-f5])
    (define-key map "\e[17;3~" [M-f6])
    (define-key map "\e[18;3~" [M-f7])
    (define-key map "\e[19;3~" [M-f8])
    (define-key map "\e[20;3~" [M-f9])
    (define-key map "\e[21;3~" [M-f10])
    (define-key map "\e[23;3~" [M-f11])
    (define-key map "\e[24;3~" [M-f12])

    (define-key map "\eO4P" [M-S-f1])
    (define-key map "\eO4Q" [M-S-f2])
    (define-key map "\eO4R" [M-S-f3])
    (define-key map "\eO4S" [M-S-f4])
    (define-key map "\e[15;4~" [M-S-f5])
    (define-key map "\e[17;4~" [M-S-f6])
    (define-key map "\e[18;4~" [M-S-f7])
    (define-key map "\e[19;4~" [M-S-f8])
    (define-key map "\e[20;4~" [M-S-f9])
    (define-key map "\e[21;4~" [M-S-f10])
    (define-key map "\e[23;4~" [M-S-f11])
    (define-key map "\e[24;4~" [M-S-f12])

    (define-key map "\eOA" [up])
    (define-key map "\eOB" [down])
    (define-key map "\eOC" [right])
    (define-key map "\eOD" [left])
    (define-key map "\eOF" [end])
    (define-key map "\eOH" [home])

    (define-key map "\e[1;2A" [S-up])
    (define-key map "\e[1;2B" [S-down])
    (define-key map "\e[1;2C" [S-right])
    (define-key map "\e[1;2D" [S-left])
    (define-key map "\e[1;2F" [S-end])
    (define-key map "\e[1;2H" [S-home])

    (define-key map "\e[1;4A" [M-S-up])
    (define-key map "\e[1;4B" [M-S-down])
    (define-key map "\e[1;4C" [M-S-right])
    (define-key map "\e[1;4D" [M-S-left])
    (define-key map "\e[1;4F" [M-S-end])
    (define-key map "\e[1;4H" [M-S-home])

    (define-key map "\e[1;5A" [C-up])
    (define-key map "\e[1;5B" [C-down])
    (define-key map "\e[1;5C" [C-right])
    (define-key map "\e[1;5D" [C-left])
    (define-key map "\e[1;5F" [C-end])
    (define-key map "\e[1;5H" [C-home])

    (define-key map "\e[1;6A" [C-S-up])
    (define-key map "\e[1;6B" [C-S-down])
    (define-key map "\e[1;6C" [C-S-right])
    (define-key map "\e[1;6D" [C-S-left])
    (define-key map "\e[1;6F" [C-S-end])
    (define-key map "\e[1;6H" [C-S-home])

    (define-key map "\e[1;7A" [C-M-up])
    (define-key map "\e[1;7B" [C-M-down])
    (define-key map "\e[1;7C" [C-M-right])
    (define-key map "\e[1;7D" [C-M-left])
    (define-key map "\e[1;7F" [C-M-end])
    (define-key map "\e[1;7H" [C-M-home])

    (define-key map "\e[1;8A" [C-M-S-up])
    (define-key map "\e[1;8B" [C-M-S-down])
    (define-key map "\e[1;8C" [C-M-S-right])
    (define-key map "\e[1;8D" [C-M-S-left])
    (define-key map "\e[1;8F" [C-M-S-end])
    (define-key map "\e[1;8H" [C-M-S-home])

    (define-key map "\e[1;3A" [M-up])
    (define-key map "\e[1;3B" [M-down])
    (define-key map "\e[1;3C" [M-right])
    (define-key map "\e[1;3D" [M-left])
    (define-key map "\e[1;3F" [M-end])
    (define-key map "\e[1;3H" [M-home])

    (define-key map "\e[3;2~" [S-delete])
    (define-key map "\e[5;2~" [S-prior])
    (define-key map "\e[6;2~" [S-next])

    (define-key map "\e[2;4~" [M-S-insert])
    (define-key map "\e[3;4~" [M-S-delete])
    (define-key map "\e[5;4~" [M-S-prior])
    (define-key map "\e[6;4~" [M-S-next])

    (define-key map "\e[2;5~" [C-insert])
    (define-key map "\e[3;5~" [C-delete])
    (define-key map "\e[5;5~" [C-prior])
    (define-key map "\e[6;5~" [C-next])

    (define-key map "\e[2;6~" [C-S-insert])
    (define-key map "\e[3;6~" [C-S-delete])
    (define-key map "\e[5;6~" [C-S-prior])
    (define-key map "\e[6;6~" [C-S-next])

    (define-key map "\e[2;7~" [C-M-insert])
    (define-key map "\e[3;7~" [C-M-delete])
    (define-key map "\e[5;7~" [C-M-prior])
    (define-key map "\e[6;7~" [C-M-next])

    (define-key map "\e[2;8~" [C-M-S-insert])
    (define-key map "\e[3;8~" [C-M-S-delete])
    (define-key map "\e[5;8~" [C-M-S-prior])
    (define-key map "\e[6;8~" [C-M-S-next])

    (define-key map "\e[2;3~" [M-insert])
    (define-key map "\e[3;3~" [M-delete])
    (define-key map "\e[5;3~" [M-prior])
    (define-key map "\e[6;3~" [M-next])

    (define-key map "\e[29~" [print])

    (define-key map "\eOj" [kp-multiply])
    (define-key map "\eOk" [kp-add])
    (define-key map "\eOl" [kp-separator])
    (define-key map "\eOm" [kp-subtract])
    (define-key map "\eOo" [kp-divide])
    (define-key map "\eOp" [kp-0])
    (define-key map "\eOq" [kp-1])
    (define-key map "\eOr" [kp-2])
    (define-key map "\eOs" [kp-3])
    (define-key map "\eOt" [kp-4])
    (define-key map "\eOu" [kp-5])
    (define-key map "\eOv" [kp-6])
    (define-key map "\eOw" [kp-7])
    (define-key map "\eOx" [kp-8])
    (define-key map "\eOy" [kp-9])

    (define-key map "\eO2j" [S-kp-multiply])
    (define-key map "\eO2k" [S-kp-add])
    (define-key map "\eO2l" [S-kp-separator])
    (define-key map "\eO2m" [S-kp-subtract])
    (define-key map "\eO2o" [S-kp-divide])
    (define-key map "\eO2p" [S-kp-0])
    (define-key map "\eO2q" [S-kp-1])
    (define-key map "\eO2r" [S-kp-2])
    (define-key map "\eO2s" [S-kp-3])
    (define-key map "\eO2t" [S-kp-4])
    (define-key map "\eO2u" [S-kp-5])
    (define-key map "\eO2v" [S-kp-6])
    (define-key map "\eO2w" [S-kp-7])
    (define-key map "\eO2x" [S-kp-8])
    (define-key map "\eO2y" [S-kp-9])

    (define-key map "\eO4j" [M-S-kp-multiply])
    (define-key map "\eO4k" [M-S-kp-add])
    (define-key map "\eO4l" [M-S-kp-separator])
    (define-key map "\eO4m" [M-S-kp-subtract])
    (define-key map "\eO4o" [M-S-kp-divide])
    (define-key map "\eO4p" [M-S-kp-0])
    (define-key map "\eO4q" [M-S-kp-1])
    (define-key map "\eO4r" [M-S-kp-2])
    (define-key map "\eO4s" [M-S-kp-3])
    (define-key map "\eO4t" [M-S-kp-4])
    (define-key map "\eO4u" [M-S-kp-5])
    (define-key map "\eO4v" [M-S-kp-6])
    (define-key map "\eO4w" [M-S-kp-7])
    (define-key map "\eO4x" [M-S-kp-8])
    (define-key map "\eO4y" [M-S-kp-9])

    (define-key map "\eO6j" [C-S-kp-multiply])
    (define-key map "\eO6k" [C-S-kp-add])
    (define-key map "\eO6l" [C-S-kp-separator])
    (define-key map "\eO6m" [C-S-kp-subtract])
    (define-key map "\eO6o" [C-S-kp-divide])
    (define-key map "\eO6p" [C-S-kp-0])
    (define-key map "\eO6q" [C-S-kp-1])
    (define-key map "\eO6r" [C-S-kp-2])
    (define-key map "\eO6s" [C-S-kp-3])
    (define-key map "\eO6t" [C-S-kp-4])
    (define-key map "\eO6u" [C-S-kp-5])
    (define-key map "\eO6v" [C-S-kp-6])
    (define-key map "\eO6w" [C-S-kp-7])
    (define-key map "\eO6x" [C-S-kp-8])
    (define-key map "\eO6y" [C-S-kp-9])

    (define-key map "\eO8j" [C-M-S-kp-multiply])
    (define-key map "\eO8k" [C-M-S-kp-add])
    (define-key map "\eO8l" [C-M-S-kp-separator])
    (define-key map "\eO8m" [C-M-S-kp-subtract])
    (define-key map "\eO8o" [C-M-S-kp-divide])
    (define-key map "\eO8p" [C-M-S-kp-0])
    (define-key map "\eO8q" [C-M-S-kp-1])
    (define-key map "\eO8r" [C-M-S-kp-2])
    (define-key map "\eO8s" [C-M-S-kp-3])
    (define-key map "\eO8t" [C-M-S-kp-4])
    (define-key map "\eO8u" [C-M-S-kp-5])
    (define-key map "\eO8v" [C-M-S-kp-6])
    (define-key map "\eO8w" [C-M-S-kp-7])
    (define-key map "\eO8x" [C-M-S-kp-8])
    (define-key map "\eO8y" [C-M-S-kp-9])

    ;; These keys are available in xterm starting from version 216
    ;; if the modifyOtherKeys resource is set to 1.
    (dolist (bind '((5 9   [C-tab])
                    (5 13  [C-return])
                    (5 39  [?\C-\'])
                    (5 44  [?\C-,])
                    (5 45  [?\C--])
                    (5 46  [?\C-.])
                    (5 47  [?\C-/])
                    (5 48  [?\C-0])
                    (5 49  [?\C-1])
                    ;; Not all C-DIGIT keys have a distinct binding.
                    (5 57  [?\C-9])
                    (5 59  [?\C-\;])
                    (5 61  [?\C-=])
                    (5 92  [?\C-\\])

                    (6 33  [?\C-!])
                    (6 34  [?\C-\"])
                    (6 35  [?\C-#])
                    (6 36  [?\C-$])
                    (6 37  [?\C-%])
                    (6 38  [?\C-&])
                    (6 40  [?\C-\(])
                    (6 41  [?\C-\)])
                    (6 42  [?\C-*])
                    (6 43  [?\C-+])
                    (6 58  [?\C-:])
                    (6 60  [?\C-<])
                    (6 62  [?\C->])
                    (6 63  [(control ??)])

                    ;; These are the strings emitted for various C-M-
                    ;; combinations for keyboards whose Meta and Alt
                    ;; modifiers are on the same key (usually labeled "Alt").
                    (13 9  [C-M-tab])
                    (13 13 [C-M-return])

                    (13 39 [?\C-\M-\'])
                    (13 44 [?\C-\M-,])
                    (13 45 [?\C-\M--])
                    (13 46 [?\C-\M-.])
                    (13 47 [?\C-\M-/])
                    (13 48 [?\C-\M-0])
                    (13 49 [?\C-\M-1])
                    (13 50 [?\C-\M-2])
                    (13 51 [?\C-\M-3])
                    (13 52 [?\C-\M-4])
                    (13 53 [?\C-\M-5])
                    (13 54 [?\C-\M-6])
                    (13 55 [?\C-\M-7])
                    (13 56 [?\C-\M-8])
                    (13 57 [?\C-\M-9])
                    (13 59 [?\C-\M-\;])
                    (13 61 [?\C-\M-=])
                    (13 92 [?\C-\M-\\])

                    (14 33  [?\C-\M-!])
                    (14 34  [?\C-\M-\"])
                    (14 35  [?\C-\M-#])
                    (14 36  [?\C-\M-$])
                    (14 37  [?\C-\M-%])
                    (14 38  [?\C-\M-&])
                    (14 40  [?\C-\M-\(])
                    (14 41  [?\C-\M-\)])
                    (14 42  [?\C-\M-*])
                    (14 43  [?\C-\M-+])
                    (14 58  [?\C-\M-:])
                    (14 60  [?\C-\M-<])
                    (14 62  [?\C-\M->])
                    (14 63  [(control meta ??)])

                    (7 9  [C-M-tab])
                    (7 13 [C-M-return])

                    (7 32 [?\C-\M-\s])
                    (7 39 [?\C-\M-\'])
                    (7 44 [?\C-\M-,])
                    (7 45 [?\C-\M--])
                    (7 46 [?\C-\M-.])
                    (7 47 [?\C-\M-/])
                    (7 48 [?\C-\M-0])
                    (7 49 [?\C-\M-1])
                    (7 50 [?\C-\M-2])
                    (7 51 [?\C-\M-3])
                    (7 52 [?\C-\M-4])
                    (7 53 [?\C-\M-5])
                    (7 54 [?\C-\M-6])
                    (7 55 [?\C-\M-7])
                    (7 56 [?\C-\M-8])
                    (7 57 [?\C-\M-9])
                    (7 59 [?\C-\M-\;])
                    (7 61 [?\C-\M-=])
                    (7 92 [?\C-\M-\\])

                    (8 33  [?\C-\M-!])
                    (8 34  [?\C-\M-\"])
                    (8 35  [?\C-\M-#])
                    (8 36  [?\C-\M-$])
                    (8 37  [?\C-\M-%])
                    (8 38  [?\C-\M-&])
                    (8 40  [?\C-\M-\(])
                    (8 41  [?\C-\M-\)])
                    (8 42  [?\C-\M-*])
                    (8 43  [?\C-\M-+])
                    (8 58  [?\C-\M-:])
                    (8 60  [?\C-\M-<])
                    (8 62  [?\C-\M->])
                    (8 63  [(control meta ??)])

                    (2 9   [S-tab])
                    (2 13  [S-return])

                    (6 9   [C-S-tab])
                    (6 13  [C-S-return])))
      (define-key map
        (format "\e[27;%d;%d~" (nth 0 bind) (nth 1 bind)) (nth 2 bind))
      ;; For formatOtherKeys=1, the sequence is a bit shorter (bug#13839).
      (define-key map
        (format "\e[%d;%du" (nth 1 bind) (nth 0 bind)) (nth 2 bind)))

    ;; Other versions of xterm might emit these.
    (define-key map "\e[1~" [home])

    (define-key map "\eO2A" [S-up])
    (define-key map "\eO2B" [S-down])
    (define-key map "\eO2C" [S-right])
    (define-key map "\eO2D" [S-left])
    (define-key map "\eO2F" [S-end])
    (define-key map "\eO2H" [S-home])

    (define-key map "\eO5A" [C-up])
    (define-key map "\eO5B" [C-down])
    (define-key map "\eO5C" [C-right])
    (define-key map "\eO5D" [C-left])
    (define-key map "\eO5F" [C-end])
    (define-key map "\eO5H" [C-home])

    map)
  "Function key map overrides for xterm.")

(defvar xterm-alternatives-map
  (let ((map (make-sparse-keymap)))
    ;; The terminal initialization C code file might have initialized
    ;; function keys F13->F60 from the termcap/terminfo information.
    ;; On a PC-style keyboard these keys correspond to
    ;; MODIFIER-FUNCTION_KEY, where modifier is S-, C, A-, C-S-.  The code
    ;; here substitutes the corresponding definitions in function-key-map.
    ;; The mapping from escape sequences to Fn is done in input-decode-map
    ;; whereas this here mapping is done in local-function-key-map so that
    ;; bindings to f45 still work, in case your keyboard really has an f45
    ;; key rather than C-S-f9.
    (define-key map [f13] [S-f1])
    (define-key map [f14] [S-f2])
    (define-key map [f15] [S-f3])
    (define-key map [f16] [S-f4])
    (define-key map [f17] [S-f5])
    (define-key map [f18] [S-f6])
    (define-key map [f19] [S-f7])
    (define-key map [f20] [S-f8])
    (define-key map [f21] [S-f9])
    (define-key map [f22] [S-f10])
    (define-key map [f23] [S-f11])
    (define-key map [f24] [S-f12])

    (define-key map [f25] [C-f1])
    (define-key map [f26] [C-f2])
    (define-key map [f27] [C-f3])
    (define-key map [f28] [C-f4])
    (define-key map [f29] [C-f5])
    (define-key map [f30] [C-f6])
    (define-key map [f31] [C-f7])
    (define-key map [f32] [C-f8])
    (define-key map [f33] [C-f9])
    (define-key map [f34] [C-f10])
    (define-key map [f35] [C-f11])
    (define-key map [f36] [C-f12])

    (define-key map [f37] [C-S-f1])
    (define-key map [f38] [C-S-f2])
    (define-key map [f39] [C-S-f3])
    (define-key map [f40] [C-S-f4])
    (define-key map [f41] [C-S-f5])
    (define-key map [f42] [C-S-f6])
    (define-key map [f43] [C-S-f7])
    (define-key map [f44] [C-S-f8])
    (define-key map [f45] [C-S-f9])
    (define-key map [f46] [C-S-f10])
    (define-key map [f47] [C-S-f11])
    (define-key map [f48] [C-S-f12])

    (define-key map [f49] [M-f1])
    (define-key map [f50] [M-f2])
    (define-key map [f51] [M-f3])
    (define-key map [f52] [M-f4])
    (define-key map [f53] [M-f5])
    (define-key map [f54] [M-f6])
    (define-key map [f55] [M-f7])
    (define-key map [f56] [M-f8])
    (define-key map [f57] [M-f9])
    (define-key map [f58] [M-f10])
    (define-key map [f59] [M-f11])
    (define-key map [f60] [M-f12])

    (define-key map [f61] [M-S-f1])
    (define-key map [f62] [M-S-f2])
    (define-key map [f63] [M-S-f3])
    (define-key map [f64] [M-S-f4])
    (define-key map [f65] [M-S-f5])
    (define-key map [f66] [M-S-f6])
    (define-key map [f67] [M-S-f7])
    (define-key map [f68] [M-S-f8])
    (define-key map [f69] [M-S-f9])
    (define-key map [f70] [M-S-f10])
    (define-key map [f71] [M-S-f11])
    (define-key map [f72] [M-S-f12])

    map)
  "Keymap of possible alternative meanings for some keys.")

;; Set up colors, for those versions of xterm that support it.
(defvar xterm-standard-colors
  ;; The names in the comments taken from XTerm-col.ad in the xterm
  ;; distribution, see https://invisible-island.net/xterm/.  RGB values are
  ;; from rgb.txt.
  '(("black"          0 (  0   0   0))	; black
    ("red"            1 (205   0   0))	; red3
    ("green"          2 (  0 205   0))	; green3
    ("yellow"         3 (205 205   0))	; yellow3
    ("blue"           4 (  0   0 238))	; blue2
    ("magenta"        5 (205   0 205))	; magenta3
    ("cyan"           6 (  0 205 205))	; cyan3
    ("white"          7 (229 229 229))	; gray90
    ("brightblack"    8 (127 127 127))	; gray50
    ("brightred"      9 (255   0   0))	; red
    ("brightgreen"   10 (  0 255   0))	; green
    ("brightyellow"  11 (255 255   0))	; yellow
    ("brightblue"    12 (92   92 255))	; rgb:5c/5c/ff
    ("brightmagenta" 13 (255   0 255))	; magenta
    ("brightcyan"    14 (  0 255 255))	; cyan
    ("brightwhite"   15 (255 255 255)))	; white
  "Names of 16 standard xterm/aixterm colors, their numbers, and RGB values.")

(defun xterm--report-background-handler ()
  (let ((str "")
        chr)
    ;; The reply should be: \e ] 11 ; rgb: NUMBER1 / NUMBER2 / NUMBER3 \e \\
    (while (and (setq chr (read-event nil nil 2)) (not (equal chr ?\\)))
      (setq str (concat str (string chr))))
    (when (string-match
           "rgb:\\([a-f0-9]+\\)/\\([a-f0-9]+\\)/\\([a-f0-9]+\\)" str)
      (let ((recompute-faces
             (xterm-maybe-set-dark-background-mode
              (string-to-number (match-string 1 str) 16)
              (string-to-number (match-string 2 str) 16)
              (string-to-number (match-string 3 str) 16))))

        ;; Recompute faces here in case the background mode was
        ;; set to dark.  We used to call
        ;; `tty-set-up-initial-frame-faces' only once, but that
        ;; caused the light background faces to be computed
        ;; incorrectly.  See:
        ;; http://permalink.gmane.org/gmane.emacs.devel/119627
        (when recompute-faces
          (tty-set-up-initial-frame-faces))))))

(defun xterm--version-handler ()
  (let ((str "")
        chr)
    ;; The reply should be: \e [ > NUMBER1 ; NUMBER2 ; NUMBER3 c
    ;; If the timeout is completely removed for read-event, this
    ;; might hang for terminals that pretend to be xterm, but don't
    ;; respond to this escape sequence.  RMS' opinion was to remove
    ;; it completely.  That might be right, but let's first try to
    ;; see if by using a longer timeout we get rid of most issues.
    (while (and (setq chr (read-event nil nil 2)) (not (equal chr ?c)))
      (setq str (concat str (string chr))))
    ;; Since xterm-280, the terminal type (NUMBER1) is now 41 instead of 0.
    (when (string-match "\\([0-9]+\\);\\([0-9]+\\);0" str)
      (let ((version (string-to-number (match-string 2 str))))
        (when (and (> version 2000) (equal (match-string 1 str) "1"))
          ;; Hack attack!  bug#16988: gnome-terminal reports "1;NNNN;0"
          ;; with a large NNNN but is based on a rather old xterm code.
          ;; Gnome terminal 3.6.1 reports 1;3406;0
          ;; Gnome terminal 2.32.1 reports 1;2802;0
          (setq version 200))
        (when (equal (match-string 1 str) "83")
          ;; `screen' (which returns 83;40003;0) seems to also lack support for
          ;; some of these (bug#17607, bug#20356).
          ;; Note: this code path should normally not be used any more
          ;; since term/screen.el now binds xterm-extra-capabilities
          ;; to a fixed value, rather than using the dynamic checking.
          (setq version 200))
        ;; If version is 242 or higher, assume the xterm supports
        ;; reporting the background color (TODO: maybe earlier
        ;; versions do too...)
        (when (>= version 242)
          (xterm--query "\e]11;?\e\\"
                        '(("\e]11;" .  xterm--report-background-handler))))

        ;; If version is 216 (the version when modifyOtherKeys was
        ;; introduced) or higher, initialize the
        ;; modifyOtherKeys support.
        (when (>= version 216)
          (xterm--init-modify-other-keys))
        ;; In version 203 support for accessing the X selection was
        ;; added.  Hterm reports itself as version 256 and supports it
        ;; as well.  gnome-terminal doesn't and is excluded by this
        ;; test.
        (when (>= version 203)
          ;; Most xterms seem to have it disabled by default, and if it's
          ;; disabled, C-y will incur a timeout, so we only use it if the user
          ;; explicitly requests it.
          ;;(xterm--init-activate-get-selection)
          (xterm--init-activate-set-selection))))))

(defvar xterm-query-timeout 2
  "Seconds to wait for an answer from the terminal.
Can be nil to mean \"no timeout\".")

(defun xterm--query (query handlers &optional no-async)
  "Send QUERY string to the terminal and watch for a response.
HANDLERS is an alist with elements of the form (STRING . FUNCTION).
We run the first FUNCTION whose STRING matches the input events."
  ;; We used to query synchronously, but the need to use `discard-input' is
  ;; rather annoying (bug#6758).  Maybe we could always use the asynchronous
  ;; approach, but it's less tested.
  ;; FIXME: Merge the two branches.
  (let ((register
         (lambda (handlers)
           (dolist (handler handlers)
             (define-key input-decode-map (car handler)
               (lambda (&optional _prompt)
                 ;; Unregister the handler, since we don't expect
                 ;; further answers.
                 (dolist (handler handlers)
                   (define-key input-decode-map (car handler) nil))
                 (funcall (cdr handler))
                 []))))))
    (if (and (or (null xterm-query-timeout) (input-pending-p))
             (not no-async))
        (progn
          (funcall register handlers)
          (send-string-to-terminal query))
      ;; Pending input can be mistakenly returned by the calls to
      ;; read-event below: discard it.
      (discard-input)
      (send-string-to-terminal query)
      (while handlers
        (let ((handler (pop handlers))
              (i 0))
          (while (and (< i (length (car handler)))
                      (let ((evt (read-event nil nil xterm-query-timeout)))
                        (if (and (null evt) (= i 0) (not no-async))
                            ;; Timeout on the first event: fallback on async.
                            (progn
                              (funcall register (cons handler handlers))
                              (setq handlers nil)
                              nil)
                          (or (eq evt (aref (car handler) i))
                              (progn (if evt (push evt unread-command-events))
                                     nil)))))
            (setq i (1+ i)))
          (if (= i (length (car handler)))
              (progn (setq handlers nil)
                     (funcall (cdr handler)))
            (while (> i 0)
              (push (aref (car handler) (setq i (1- i)))
                    unread-command-events))))))))

(defun xterm--push-map (map basemap)
  ;; Use inheritance to let the main keymaps override those defaults.
  ;; This way we don't override terminfo-derived settings or settings
  ;; made in the init file.
  (set-keymap-parent
   basemap
   (make-composed-keymap map (keymap-parent basemap))))

(defun terminal-init-xterm ()
  "Terminal initialization function for xterm."
  ;; rxvt terminals sometimes set the TERM variable to "xterm", but
  ;; rxvt's keybindings are incompatible with xterm's. It is
  ;; better in that case to use rxvt's initialization function.
  (if (and (getenv "COLORTERM" (selected-frame))
	   (string-match "\\`rxvt" (getenv "COLORTERM" (selected-frame))))
      (tty-run-terminal-initialization (selected-frame) "rxvt")

      (xterm--push-map xterm-alternatives-map local-function-key-map)
      (xterm--push-map xterm-function-map     input-decode-map))

  (xterm-register-default-colors xterm-standard-colors)
  (tty-set-up-initial-frame-faces)

  (if (eq xterm-extra-capabilities 'check)
      ;; Try to find out the type of terminal by sending a "Secondary
      ;; Device Attributes (DA)" query.
      (xterm--query "\e[>0c"
                    ;; Some terminals (like macOS's Terminal.app) respond to
                    ;; this query as if it were a "Primary Device Attributes"
                    ;; query instead, so we should handle that too.
                    '(("\e[?" . xterm--version-handler)
                      ("\e[>" . xterm--version-handler)))

    (when (memq 'reportBackground xterm-extra-capabilities)
      (xterm--query "\e]11;?\e\\"
                    '(("\e]11;" .  xterm--report-background-handler))))

    (when (memq 'modifyOtherKeys xterm-extra-capabilities)
      (xterm--init-modify-other-keys))

    (when (memq 'getSelection xterm-extra-capabilities)
      (xterm--init-activate-get-selection))
    (when (memq 'setSelection xterm-extra-capabilities)
      (xterm--init-activate-set-selection)))

  (when xterm-set-window-title
    (xterm--init-frame-title))
  ;; Unconditionally enable bracketed paste mode: terminals that don't
  ;; support it just ignore the sequence.
  (xterm--init-bracketed-paste-mode)

  (run-hooks 'terminal-init-xterm-hook))

(defun xterm--init-modify-other-keys ()
  "Terminal initialization for xterm's modifyOtherKeys support."
  (send-string-to-terminal "\e[>4;1m")
  (push "\e[>4m" (terminal-parameter nil 'tty-mode-reset-strings))
  (push "\e[>4;1m" (terminal-parameter nil 'tty-mode-set-strings)))

(defun xterm--init-bracketed-paste-mode ()
  "Terminal initialization for bracketed paste mode."
  (send-string-to-terminal "\e[?2004h")
  (push "\e[?2004l" (terminal-parameter nil 'tty-mode-reset-strings))
  (push "\e[?2004h" (terminal-parameter nil 'tty-mode-set-strings)))

(defun xterm--init-activate-get-selection ()
  "Terminal initialization for `gui-get-selection'."
  (set-terminal-parameter nil 'xterm--get-selection t))

(defun xterm--init-activate-set-selection ()
  "Terminal initialization for `gui-set-selection'."
  (set-terminal-parameter nil 'xterm--set-selection t))

(defun xterm--init-frame-title ()
  "Terminal initialization for XTerm frame titles."
  (xterm-set-window-title)
  (add-hook 'after-make-frame-functions 'xterm-set-window-title-flag)
  (add-hook 'window-configuration-change-hook 'xterm-unset-window-title-flag)
  (add-hook 'post-command-hook 'xterm-set-window-title)
  (add-hook 'minibuffer-exit-hook 'xterm-set-window-title))

(defvar xterm-window-title-flag nil
  "Whether a new frame has been created, calling for a title update.")

(defun xterm-set-window-title-flag (_frame)
  "Set `xterm-window-title-flag'.
See `xterm--init-frame-title'"
  (setq xterm-window-title-flag t))

(defun xterm-unset-window-title-flag ()
  (when xterm-window-title-flag
    (setq xterm-window-title-flag nil)
    (xterm-set-window-title)))

(defun xterm-set-window-title (&optional terminal)
  "Set the window title of the Xterm TERMINAL.
The title is constructed from `frame-title-format'."
  (send-string-to-terminal
   (format "\e]2;%s\a" (format-mode-line frame-title-format))
   terminal))

(defun xterm--selection-char (type)
  (pcase type
    ('PRIMARY "p")
    ('CLIPBOARD "c")
    (_ (error "Invalid selection type: %S" type))))

(cl-defmethod gui-backend-get-selection
    (type data-type
     &context (window-system nil)
              ;; Only applies to terminals which have it enabled.
              ((terminal-parameter nil 'xterm--get-selection) (eql t)))
  (unless (eq data-type 'STRING)
    (error "Unsupported data type %S" data-type))
  (let* ((screen (eq (terminal-parameter nil 'terminal-initted)
                     'terminal-init-screen))
         (query (concat "\e]52;" (xterm--selection-char type) ";")))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (xterm--query
       (concat (when screen "\eP") query "?\a" (when screen "\e\\"))
       (list (cons query (lambda ()
                           (while (let ((char (read-char)))
                                    (unless (eq char ?\a)
                                      (insert char)
                                      t))))))
       'no-async)
      (base64-decode-region (point-min) (point-max))
      (decode-coding-region (point-min) (point-max) 'utf-8-unix t))))

(cl-defmethod gui-backend-set-selection
    (type data
     &context (window-system nil)
              ;; Only applies to terminals which have it enabled.
              ((terminal-parameter nil 'xterm--set-selection) (eql t)))
  "Copy DATA to the X selection using the OSC 52 escape sequence.

TYPE specifies which selection to set; it must be either
`PRIMARY' or `CLIPBOARD'.  DATA must be a string.

This can be used as a `gui-set-selection' method for
xterm-compatible terminal emulators.  Then your system clipboard
will be updated whenever you copy a region of text in Emacs.

If the resulting OSC 52 sequence would be longer than
`xterm-max-cut-length', then the TEXT is not sent to the system
clipboard.

This function either sends a raw OSC 52 sequence or wraps the OSC
52 in a Device Control String sequence.  This way, it will work
on a bare terminal emulators as well as inside the screen
program.  When inside the screen program, this function also
chops long DCS sequences into multiple smaller ones to avoid
hitting screen's max DCS length."
  (let* ((screen (eq (terminal-parameter nil 'terminal-initted)
                     'terminal-init-screen))
         (bytes (encode-coding-string data 'utf-8-unix))
         (base-64 (if screen
                      (replace-regexp-in-string
                       "\n" "\e\\\eP"
                       (base64-encode-string bytes)
                       :fixedcase :literal)
                    (base64-encode-string bytes :no-line-break)))
         (length (length base-64)))
    (if (> length xterm-max-cut-length)
        (progn
          (warn "Selection too long to send to terminal: %d bytes" length)
          (sit-for 2))
      (send-string-to-terminal
       (concat
        (when screen "\eP")
        "\e]52;" (xterm--selection-char type) ";" base-64 "\a"
        (when screen "\e\\"))))))

(defun xterm-rgb-convert-to-16bit (prim)
  "Convert an 8-bit primary color value PRIM to a corresponding 16-bit value."
  (logior prim (lsh prim 8)))

(defun xterm-register-default-colors (colors)
  "Register the default set of colors for xterm or compatible emulator.

This function registers the number of colors returned by `display-color-cells'
for the currently selected frame.  The first (16) colors are taken from
COLORS, which see, while the rest are computed assuming
either the 88- or 256-color standard color scheme supported by latest
versions of xterm."
  (let* ((ncolors (display-color-cells))
	 (color (car colors)))
    (if (> ncolors 0)
	;; Clear the 8 default tty colors registered by startup.el
	(tty-color-clear))
    ;; Only register as many colors as are supported by the display.
    (while (and (> ncolors 0) colors)
      (tty-color-define (car color) (cadr color)
			(mapcar #'xterm-rgb-convert-to-16bit
				(car (cddr color))))
      (setq colors (cdr colors)
	    color (car colors)
	    ncolors (1- ncolors)))
    ;; We've exhausted the colors from `colors'.  If there
    ;; are more colors to support, compute them now.
    (when (> ncolors 0)
      (cond
       ((= ncolors 16777200) ; 24-bit xterm
	;; all named tty colors
	(let ((idx (length xterm-standard-colors)))
	  (mapc (lambda (color)
		  (unless (assoc (car color) xterm-standard-colors)
		    (tty-color-define (car color) idx (cdr color))
		    (setq idx (1+ idx))))
		color-name-rgb-alist)))
       ((= ncolors 240)	; 256-color xterm
	;; 216 non-gray colors first
	(let ((r 0) (g 0) (b 0))
	  (while (> ncolors 24)
	    ;; This and other formulas taken from 256colres.pl and
	    ;; 88colres.pl in the xterm distribution.
	    (tty-color-define (format "color-%d" (- 256 ncolors))
			      (- 256 ncolors)
			      (mapcar #'xterm-rgb-convert-to-16bit
				      (list (if (zerop r) 0 (+ (* r 40) 55))
					    (if (zerop g) 0 (+ (* g 40) 55))
					    (if (zerop b) 0 (+ (* b 40) 55)))))

	    (setq b (1+ b))
	    (if (> b 5)
		(setq g (1+ g)
		      b 0))
	    (if (> g 5)
		(setq r (1+ r)
		      g 0))
	    (setq ncolors (1- ncolors))))
	;; Now the 24 gray colors
	(while (> ncolors 0)
	  (setq color (xterm-rgb-convert-to-16bit (+ 8 (* (- 24 ncolors) 10))))
	  (tty-color-define (format "color-%d" (- 256 ncolors))
			    (- 256 ncolors)
			    (list color color color))
	  (setq ncolors (1- ncolors))))
       ((= ncolors 72)  ; 88-color xterm
	;; 64 non-gray colors
	(let ((levels '(0 139 205 255))
	      (r 0) (g 0) (b 0))
	  (while (> ncolors 8)
	    (tty-color-define (format "color-%d" (- 88 ncolors))
			      (- 88 ncolors)
			      (mapcar #'xterm-rgb-convert-to-16bit
				      (list (nth r levels)
					    (nth g levels)
					    (nth b levels))))
	    (setq b (1+ b))
	    (if (> b 3)
		(setq g (1+ g)
		      b 0))
	    (if (> g 3)
		(setq r (1+ r)
		      g 0))
	    (setq ncolors (1- ncolors))))
	;; Now the 8 gray colors
	(while (> ncolors 0)
	  (setq color (xterm-rgb-convert-to-16bit
		       (floor
			(if (= ncolors 8)
			    46.36363636
			  (+ (* (- 8 ncolors) 23.18181818) 69.54545454)))))
	  (tty-color-define (format "color-%d" (- 88 ncolors))
			    (- 88 ncolors)
			    (list color color color))
	  (setq ncolors (1- ncolors))))
       (t (error "Unsupported number of xterm colors (%d)" (+ 16 ncolors)))))
    ;; Modifying color mappings means realized faces don't use the
    ;; right colors, so clear them.
    (clear-face-cache)))

(defun xterm-maybe-set-dark-background-mode (redc greenc bluec)
  ;; Use the heuristic in `frame-set-background-mode' to decide if a
  ;; frame is dark.
  (when (< (+ redc greenc bluec) (* .6 (+ 65535 65535 65535)))
    (set-terminal-parameter nil 'background-mode 'dark)
    t))

(provide 'xterm)                        ;Backward compatibility.
(provide 'term/xterm)
;;; xterm.el ends here
