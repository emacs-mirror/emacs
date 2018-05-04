;;; x-win.el --- parse relevant switches and set up for X  -*- lexical-binding:t -*-

;; Copyright (C) 1993-1994, 2001-2018 Free Software Foundation, Inc.

;; Author: FSF
;; Keywords: terminals, i18n

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

;; X-win.el: this file defines functions to initialize the X window
;; system and process X-specific command line parameters before
;; creating the first X frame.

;; Beginning in Emacs 23, the act of loading this file should not have
;; the side effect of initializing the window system or processing
;; command line arguments (this file is now loaded in loadup.el).  See
;; `handle-args-function' and `window-system-initialization' for more details.

;; startup.el will then examine startup files, and eventually call the hooks
;; which create the first window(s).

;;; Code:

;; These are the standard X switches from the Xt Initialize.c file of
;; Release 4.

;; Command line		Resource Manager string

;; +rv			*reverseVideo
;; +synchronous		*synchronous
;; -background		*background
;; -bd			*borderColor
;; -bg			*background
;; -bordercolor		*borderColor
;; -borderwidth		.borderWidth
;; -bw			.borderWidth
;; -display		.display
;; -fg			*foreground
;; -fn			*font
;; -font		*font
;; -foreground		*foreground
;; -geometry		.geometry
;; -iconic		.iconic
;; -name		.name
;; -reverse		*reverseVideo
;; -rv			*reverseVideo
;; -selectionTimeout    .selectionTimeout
;; -synchronous		*synchronous
;; -xrm

;; An alist of X options and the function which handles them.  See
;; ../startup.el.

(eval-when-compile (require 'cl-lib))

(if (not (fboundp 'x-create-frame))
    (error "%s: Loading x-win.el but not compiled for X" invocation-name))

(require 'term/common-win)
(require 'frame)
(require 'mouse)
(require 'scroll-bar)
(require 'faces)
(require 'select)
(require 'menu-bar)
(require 'fontset)
(require 'x-dnd)

(defvar x-invocation-args)
(defvar x-keysym-table)
(defvar x-selection-timeout)
(defvar x-session-id)
(defvar x-session-previous-id)

(defun x-handle-no-bitmap-icon (_switch)
  (setq default-frame-alist (cons '(icon-type) default-frame-alist)))

;; Handle the --parent-id option.
(defun x-handle-parent-id (switch)
  (or (consp x-invocation-args)
      (error "%s: missing argument to `%s' option" invocation-name switch))
  (setq initial-frame-alist (cons
                             (cons 'parent-id
                                   (string-to-number (car x-invocation-args)))
                             initial-frame-alist)
        x-invocation-args (cdr x-invocation-args)))

;; Handle the --smid switch.  This is used by the session manager
;; to give us back our session id we had on the previous run.
(defun x-handle-smid (switch)
  (or (consp x-invocation-args)
      (error "%s: missing argument to `%s' option" invocation-name switch))
  (setq x-session-previous-id (car x-invocation-args)
	x-invocation-args (cdr x-invocation-args)))

(defvar emacs-save-session-functions nil
  "Special hook run when a save-session event occurs.
The functions do not get any argument.
Functions can return non-nil to inform the session manager that the
window system shutdown should be aborted.

See also `emacs-session-save'.")

(defun emacs-session-filename (session-id)
  "Construct a filename to save the session in based on SESSION-ID.
Return a filename in `user-emacs-directory', unless the session file
already exists in the home directory."
  (let ((basename (concat "session." session-id)))
    (locate-user-emacs-file basename
                            (concat ".emacs-" basename))))

(defun emacs-session-save ()
  "This function is called when the window system is shutting down.
If this function returns non-nil, the window system shutdown is canceled.

When a session manager tells Emacs that the window system is shutting
down, this function is called.  It calls the functions in the hook
`emacs-save-session-functions'.  Functions are called with the current
buffer set to a temporary buffer.  Functions should use `insert' to insert
lisp code to save the session state.  The buffer is saved in a file in the
home directory of the user running Emacs.  The file is evaluated when
Emacs is restarted by the session manager.

If any of the functions returns non-nil, no more functions are called
and this function returns non-nil.  This will inform the session manager
that it should abort the window system shutdown."
  (let ((filename (emacs-session-filename x-session-id))
	(buf (get-buffer-create (concat " *SES " x-session-id))))
    (when (file-exists-p filename)
      (delete-file filename))
    (with-current-buffer buf
      (let ((cancel-shutdown (condition-case nil
				 ;; A return of t means cancel the shutdown.
				 (run-hook-with-args-until-success
				  'emacs-save-session-functions)
			       (error t))))
	(unless cancel-shutdown
	  (write-file filename))
	(kill-buffer buf)
	cancel-shutdown))))

(defun emacs-session-restore (previous-session-id)
  "Restore the Emacs session if started by a session manager.
The file saved by `emacs-session-save' is evaluated and deleted if it
exists."
  (let ((filename (emacs-session-filename previous-session-id)))
    (when (file-exists-p filename)
      (load-file filename)
      (delete-file filename)
      (message "Restored session data"))))




;;
;; Standard X cursor shapes, courtesy of Mr. Fox, who wanted ALL of them.
;;

(defconst x-pointer-X-cursor 0)
(defconst x-pointer-arrow 2)
(defconst x-pointer-based-arrow-down 4)
(defconst x-pointer-based-arrow-up 6)
(defconst x-pointer-boat 8)
(defconst x-pointer-bogosity 10)
(defconst x-pointer-bottom-left-corner 12)
(defconst x-pointer-bottom-right-corner 14)
(defconst x-pointer-bottom-side 16)
(defconst x-pointer-bottom-tee 18)
(defconst x-pointer-box-spiral 20)
(defconst x-pointer-center-ptr 22)
(defconst x-pointer-circle 24)
(defconst x-pointer-clock 26)
(defconst x-pointer-coffee-mug 28)
(defconst x-pointer-cross 30)
(defconst x-pointer-cross-reverse 32)
(defconst x-pointer-crosshair 34)
(defconst x-pointer-diamond-cross 36)
(defconst x-pointer-dot 38)
(defconst x-pointer-dotbox 40)
(defconst x-pointer-double-arrow 42)
(defconst x-pointer-draft-large 44)
(defconst x-pointer-draft-small 46)
(defconst x-pointer-draped-box 48)
(defconst x-pointer-exchange 50)
(defconst x-pointer-fleur 52)
(defconst x-pointer-gobbler 54)
(defconst x-pointer-gumby 56)
(defconst x-pointer-hand1 58)
(defconst x-pointer-hand2 60)
(defconst x-pointer-heart 62)
(defconst x-pointer-icon 64)
(defconst x-pointer-iron-cross 66)
(defconst x-pointer-left-ptr 68)
(defconst x-pointer-left-side 70)
(defconst x-pointer-left-tee 72)
(defconst x-pointer-leftbutton 74)
(defconst x-pointer-ll-angle 76)
(defconst x-pointer-lr-angle 78)
(defconst x-pointer-man 80)
(defconst x-pointer-middlebutton 82)
(defconst x-pointer-mouse 84)
(defconst x-pointer-pencil 86)
(defconst x-pointer-pirate 88)
(defconst x-pointer-plus 90)
(defconst x-pointer-question-arrow 92)
(defconst x-pointer-right-ptr 94)
(defconst x-pointer-right-side 96)
(defconst x-pointer-right-tee 98)
(defconst x-pointer-rightbutton 100)
(defconst x-pointer-rtl-logo 102)
(defconst x-pointer-sailboat 104)
(defconst x-pointer-sb-down-arrow 106)
(defconst x-pointer-sb-h-double-arrow 108)
(defconst x-pointer-sb-left-arrow 110)
(defconst x-pointer-sb-right-arrow 112)
(defconst x-pointer-sb-up-arrow 114)
(defconst x-pointer-sb-v-double-arrow 116)
(defconst x-pointer-shuttle 118)
(defconst x-pointer-sizing 120)
(defconst x-pointer-spider 122)
(defconst x-pointer-spraycan 124)
(defconst x-pointer-star 126)
(defconst x-pointer-target 128)
(defconst x-pointer-tcross 130)
(defconst x-pointer-top-left-arrow 132)
(defconst x-pointer-top-left-corner 134)
(defconst x-pointer-top-right-corner 136)
(defconst x-pointer-top-side 138)
(defconst x-pointer-top-tee 140)
(defconst x-pointer-trek 142)
(defconst x-pointer-ul-angle 144)
(defconst x-pointer-umbrella 146)
(defconst x-pointer-ur-angle 148)
(defconst x-pointer-watch 150)
(defconst x-pointer-xterm 152)
(defconst x-pointer-invisible 255)


;;;; Keysyms

(defun vendor-specific-keysyms (vendor)
  "Return the appropriate value of `system-key-alist' for VENDOR.
VENDOR is a string containing the name of the X Server's vendor,
as returned by `x-server-vendor'."
  (cond ((or (string-equal vendor "Hewlett-Packard Incorporated")
	     (string-equal vendor "Hewlett-Packard Company"))
	 '((  168 . mute-acute)
	   (  169 . mute-grave)
	   (  170 . mute-asciicircum)
	   (  171 . mute-diaeresis)
	   (  172 . mute-asciitilde)
	   (  175 . lira)
	   (  190 . guilder)
	   (  252 . block)
	   (  256 . longminus)
	   (65388 . reset)
	   (65389 . system)
	   (65390 . user)
	   (65391 . clearline)
	   (65392 . insertline)
	   (65393 . deleteline)
	   (65394 . insertchar)
	   (65395 . deletechar)
	   (65396 . backtab)
	   (65397 . kp-backtab)))
	;; Fixme: What about non-X11/NeWS sun server?
	((or (string-equal vendor "X11/NeWS - Sun Microsystems Inc.")
	     (string-equal vendor "X Consortium"))
	 '((392976 . f36)
	   (392977 . f37)
	   (393056 . req)
	   ;; These are for Sun under X11R6
	   (393072 . props)
	   (393073 . front)
	   (393074 . copy)
	   (393075 . open)
	   (393076 . paste)
	   (393077 . cut)))
	(t
	 ;; This is used by DEC's X server.
	 '((65280 . remove)))))

;; Latin-1
(let ((i 160))
  (while (< i 256)
    (puthash i i x-keysym-table)
    (setq i (1+ i))))

;; Table from Kuhn's proposed additions to the `KEYSYM Encoding'
;; appendix to the X protocol definition.
(dolist
     (pair
      '(
	;; Latin-2
	(#x1a1 . ?Ą)
	(#x1a2 . ?˘)
	(#x1a3 . ?Ł)
	(#x1a5 . ?Ľ)
	(#x1a6 . ?Ś)
	(#x1a9 . ?Š)
	(#x1aa . ?Ş)
	(#x1ab . ?Ť)
	(#x1ac . ?Ź)
	(#x1ae . ?Ž)
	(#x1af . ?Ż)
	(#x1b1 . ?ą)
	(#x1b2 . ?˛)
	(#x1b3 . ?ł)
	(#x1b5 . ?ľ)
	(#x1b6 . ?ś)
	(#x1b7 . ?ˇ)
	(#x1b9 . ?š)
	(#x1ba . ?ş)
	(#x1bb . ?ť)
	(#x1bc . ?ź)
	(#x1bd . ?˝)
	(#x1be . ?ž)
	(#x1bf . ?ż)
	(#x1c0 . ?Ŕ)
	(#x1c3 . ?Ă)
	(#x1c5 . ?Ĺ)
	(#x1c6 . ?Ć)
	(#x1c8 . ?Č)
	(#x1ca . ?Ę)
	(#x1cc . ?Ě)
	(#x1cf . ?Ď)
	(#x1d0 . ?Đ)
	(#x1d1 . ?Ń)
	(#x1d2 . ?Ň)
	(#x1d5 . ?Ő)
	(#x1d8 . ?Ř)
	(#x1d9 . ?Ů)
	(#x1db . ?Ű)
	(#x1de . ?Ţ)
	(#x1e0 . ?ŕ)
	(#x1e3 . ?ă)
	(#x1e5 . ?ĺ)
	(#x1e6 . ?ć)
	(#x1e8 . ?č)
	(#x1ea . ?ę)
	(#x1ec . ?ě)
	(#x1ef . ?ď)
	(#x1f0 . ?đ)
	(#x1f1 . ?ń)
	(#x1f2 . ?ň)
	(#x1f5 . ?ő)
	(#x1f8 . ?ř)
	(#x1f9 . ?ů)
	(#x1fb . ?ű)
	(#x1fe . ?ţ)
	(#x1ff . ?˙)
	;; Latin-3
	(#x2a1 . ?Ħ)
	(#x2a6 . ?Ĥ)
	(#x2a9 . ?İ)
	(#x2ab . ?Ğ)
	(#x2ac . ?Ĵ)
	(#x2b1 . ?ħ)
	(#x2b6 . ?ĥ)
	(#x2b9 . ?ı)
	(#x2bb . ?ğ)
	(#x2bc . ?ĵ)
	(#x2c5 . ?Ċ)
	(#x2c6 . ?Ĉ)
	(#x2d5 . ?Ġ)
	(#x2d8 . ?Ĝ)
	(#x2dd . ?Ŭ)
	(#x2de . ?Ŝ)
	(#x2e5 . ?ċ)
	(#x2e6 . ?ĉ)
	(#x2f5 . ?ġ)
	(#x2f8 . ?ĝ)
	(#x2fd . ?ŭ)
	(#x2fe . ?ŝ)
	;; Latin-4
	(#x3a2 . ?ĸ)
	(#x3a3 . ?Ŗ)
	(#x3a5 . ?Ĩ)
	(#x3a6 . ?Ļ)
	(#x3aa . ?Ē)
	(#x3ab . ?Ģ)
	(#x3ac . ?Ŧ)
	(#x3b3 . ?ŗ)
	(#x3b5 . ?ĩ)
	(#x3b6 . ?ļ)
	(#x3ba . ?ē)
	(#x3bb . ?ģ)
	(#x3bc . ?ŧ)
	(#x3bd . ?Ŋ)
	(#x3bf . ?ŋ)
	(#x3c0 . ?Ā)
	(#x3c7 . ?Į)
	(#x3cc . ?Ė)
	(#x3cf . ?Ī)
	(#x3d1 . ?Ņ)
	(#x3d2 . ?Ō)
	(#x3d3 . ?Ķ)
	(#x3d9 . ?Ų)
	(#x3dd . ?Ũ)
	(#x3de . ?Ū)
	(#x3e0 . ?ā)
	(#x3e7 . ?į)
	(#x3ec . ?ė)
	(#x3ef . ?ī)
	(#x3f1 . ?ņ)
	(#x3f2 . ?ō)
	(#x3f3 . ?ķ)
	(#x3f9 . ?ų)
	(#x3fd . ?ũ)
	(#x3fe . ?ū)
	(#x47e . ?‾)
	(#x4a1 . ?。)
	(#x4a2 . ?\「)
	(#x4a3 . ?\」)
	(#x4a4 . ?、)
	(#x4a5 . ?・)
	(#x4a6 . ?ヲ)
	(#x4a7 . ?ァ)
	(#x4a8 . ?ィ)
	(#x4a9 . ?ゥ)
	(#x4aa . ?ェ)
	(#x4ab . ?ォ)
	(#x4ac . ?ャ)
	(#x4ad . ?ュ)
	(#x4ae . ?ョ)
	(#x4af . ?ッ)
	(#x4b0 . ?ー)
	(#x4b1 . ?ア)
	(#x4b2 . ?イ)
	(#x4b3 . ?ウ)
	(#x4b4 . ?エ)
	(#x4b5 . ?オ)
	(#x4b6 . ?カ)
	(#x4b7 . ?キ)
	(#x4b8 . ?ク)
	(#x4b9 . ?ケ)
	(#x4ba . ?コ)
	(#x4bb . ?サ)
	(#x4bc . ?シ)
	(#x4bd . ?ス)
	(#x4be . ?セ)
	(#x4bf . ?ソ)
	(#x4c0 . ?タ)
	(#x4c1 . ?チ)
	(#x4c2 . ?ツ)
	(#x4c3 . ?テ)
	(#x4c4 . ?ト)
	(#x4c5 . ?ナ)
	(#x4c6 . ?ニ)
	(#x4c7 . ?ヌ)
	(#x4c8 . ?ネ)
	(#x4c9 . ?ノ)
	(#x4ca . ?ハ)
	(#x4cb . ?ヒ)
	(#x4cc . ?フ)
	(#x4cd . ?ヘ)
	(#x4ce . ?ホ)
	(#x4cf . ?マ)
	(#x4d0 . ?ミ)
	(#x4d1 . ?ム)
	(#x4d2 . ?メ)
	(#x4d3 . ?モ)
	(#x4d4 . ?ヤ)
	(#x4d5 . ?ユ)
	(#x4d6 . ?ヨ)
	(#x4d7 . ?ラ)
	(#x4d8 . ?リ)
	(#x4d9 . ?ル)
	(#x4da . ?レ)
	(#x4db . ?ロ)
	(#x4dc . ?ワ)
	(#x4dd . ?ン)
	(#x4de . ?゛)
	(#x4df . ?゜)
	;; Arabic
	(#x5ac . ?،)
	(#x5bb . ?؛)
	(#x5bf . ?؟)
	(#x5c1 . ?ء)
	(#x5c2 . ?آ)
	(#x5c3 . ?أ)
	(#x5c4 . ?ؤ)
	(#x5c5 . ?إ)
	(#x5c6 . ?ئ)
	(#x5c7 . ?ا)
	(#x5c8 . ?ب)
	(#x5c9 . ?ة)
	(#x5ca . ?ت)
	(#x5cb . ?ث)
	(#x5cc . ?ج)
	(#x5cd . ?ح)
	(#x5ce . ?خ)
	(#x5cf . ?د)
	(#x5d0 . ?ذ)
	(#x5d1 . ?ر)
	(#x5d2 . ?ز)
	(#x5d3 . ?س)
	(#x5d4 . ?ش)
	(#x5d5 . ?ص)
	(#x5d6 . ?ض)
	(#x5d7 . ?ط)
	(#x5d8 . ?ظ)
	(#x5d9 . ?ع)
	(#x5da . ?غ)
	(#x5e0 . ?ـ)
	(#x5e1 . ?ف)
	(#x5e2 . ?ق)
	(#x5e3 . ?ك)
	(#x5e4 . ?ل)
	(#x5e5 . ?م)
	(#x5e6 . ?ن)
	(#x5e7 . ?ه)
	(#x5e8 . ?و)
	(#x5e9 . ?ى)
	(#x5ea . ?ي)
	(#x5eb . ?ً)
	(#x5ec . ?ٌ)
	(#x5ed . ?ٍ)
	(#x5ee . ?َ)
	(#x5ef . ?ُ)
	(#x5f0 . ?ِ)
	(#x5f1 . ?ّ)
	(#x5f2 . ?ْ)
	;; Cyrillic
	(#x680 . ?Ғ)
	(#x681 . ?Җ)
	(#x682 . ?Қ)
	(#x683 . ?Ҝ)
	(#x684 . ?Ң)
	(#x685 . ?Ү)
	(#x686 . ?Ұ)
	(#x687 . ?Ҳ)
	(#x688 . ?Ҷ)
	(#x689 . ?Ҹ)
	(#x68a . ?Һ)
	(#x68c . ?Ә)
	(#x68d . ?Ӣ)
	(#x68e . ?Ө)
	(#x68f . ?Ӯ)
	(#x690 . ?ғ)
	(#x691 . ?җ)
	(#x692 . ?қ)
	(#x693 . ?ҝ)
	(#x694 . ?ң)
	(#x695 . ?ү)
	(#x696 . ?ұ)
	(#x697 . ?ҳ)
	(#x698 . ?ҷ)
	(#x699 . ?ҹ)
	(#x69a . ?һ)
	(#x69c . ?ә)
	(#x69d . ?ӣ)
	(#x69e . ?ө)
	(#x69f . ?ӯ)
	(#x6a1 . ?ђ)
	(#x6a2 . ?ѓ)
	(#x6a3 . ?ё)
	(#x6a4 . ?є)
	(#x6a5 . ?ѕ)
	(#x6a6 . ?і)
	(#x6a7 . ?ї)
	(#x6a8 . ?ј)
	(#x6a9 . ?љ)
	(#x6aa . ?њ)
	(#x6ab . ?ћ)
	(#x6ac . ?ќ)
	(#x6ae . ?ў)
	(#x6af . ?џ)
	(#x6b0 . ?№)
	(#x6b1 . ?Ђ)
	(#x6b2 . ?Ѓ)
	(#x6b3 . ?Ё)
	(#x6b4 . ?Є)
	(#x6b5 . ?Ѕ)
	(#x6b6 . ?І)
	(#x6b7 . ?Ї)
	(#x6b8 . ?Ј)
	(#x6b9 . ?Љ)
	(#x6ba . ?Њ)
	(#x6bb . ?Ћ)
	(#x6bc . ?Ќ)
	(#x6be . ?Ў)
	(#x6bf . ?Џ)
	(#x6c0 . ?ю)
	(#x6c1 . ?а)
	(#x6c2 . ?б)
	(#x6c3 . ?ц)
	(#x6c4 . ?д)
	(#x6c5 . ?е)
	(#x6c6 . ?ф)
	(#x6c7 . ?г)
	(#x6c8 . ?х)
	(#x6c9 . ?и)
	(#x6ca . ?й)
	(#x6cb . ?к)
	(#x6cc . ?л)
	(#x6cd . ?м)
	(#x6ce . ?н)
	(#x6cf . ?о)
	(#x6d0 . ?п)
	(#x6d1 . ?я)
	(#x6d2 . ?р)
	(#x6d3 . ?с)
	(#x6d4 . ?т)
	(#x6d5 . ?у)
	(#x6d6 . ?ж)
	(#x6d7 . ?в)
	(#x6d8 . ?ь)
	(#x6d9 . ?ы)
	(#x6da . ?з)
	(#x6db . ?ш)
	(#x6dc . ?э)
	(#x6dd . ?щ)
	(#x6de . ?ч)
	(#x6df . ?ъ)
	(#x6e0 . ?Ю)
	(#x6e1 . ?А)
	(#x6e2 . ?Б)
	(#x6e3 . ?Ц)
	(#x6e4 . ?Д)
	(#x6e5 . ?Е)
	(#x6e6 . ?Ф)
	(#x6e7 . ?Г)
	(#x6e8 . ?Х)
	(#x6e9 . ?И)
	(#x6ea . ?Й)
	(#x6eb . ?К)
	(#x6ec . ?Л)
	(#x6ed . ?М)
	(#x6ee . ?Н)
	(#x6ef . ?О)
	(#x6f0 . ?П)
	(#x6f1 . ?Я)
	(#x6f2 . ?Р)
	(#x6f3 . ?С)
	(#x6f4 . ?Т)
	(#x6f5 . ?У)
	(#x6f6 . ?Ж)
	(#x6f7 . ?В)
	(#x6f8 . ?Ь)
	(#x6f9 . ?Ы)
	(#x6fa . ?З)
	(#x6fb . ?Ш)
	(#x6fc . ?Э)
	(#x6fd . ?Щ)
	(#x6fe . ?Ч)
	(#x6ff . ?Ъ)
	;; Greek
	(#x7a1 . ?Ά)
	(#x7a2 . ?Έ)
	(#x7a3 . ?Ή)
	(#x7a4 . ?Ί)
	(#x7a5 . ?Ϊ)
	(#x7a7 . ?Ό)
	(#x7a8 . ?Ύ)
	(#x7a9 . ?Ϋ)
	(#x7ab . ?Ώ)
	(#x7ae . ?΅)
	(#x7af . ?―)
	(#x7b1 . ?ά)
	(#x7b2 . ?έ)
	(#x7b3 . ?ή)
	(#x7b4 . ?ί)
	(#x7b5 . ?ϊ)
	(#x7b6 . ?ΐ)
	(#x7b7 . ?ό)
	(#x7b8 . ?ύ)
	(#x7b9 . ?ϋ)
	(#x7ba . ?ΰ)
	(#x7bb . ?ώ)
	(#x7c1 . ?Α)
	(#x7c2 . ?Β)
	(#x7c3 . ?Γ)
	(#x7c4 . ?Δ)
	(#x7c5 . ?Ε)
	(#x7c6 . ?Ζ)
	(#x7c7 . ?Η)
	(#x7c8 . ?Θ)
	(#x7c9 . ?Ι)
	(#x7ca . ?Κ)
	(#x7cb . ?Λ)
	(#x7cc . ?Μ)
	(#x7cd . ?Ν)
	(#x7ce . ?Ξ)
	(#x7cf . ?Ο)
	(#x7d0 . ?Π)
	(#x7d1 . ?Ρ)
	(#x7d2 . ?Σ)
	(#x7d4 . ?Τ)
	(#x7d5 . ?Υ)
	(#x7d6 . ?Φ)
	(#x7d7 . ?Χ)
	(#x7d8 . ?Ψ)
	(#x7d9 . ?Ω)
	(#x7e1 . ?α)
	(#x7e2 . ?β)
	(#x7e3 . ?γ)
	(#x7e4 . ?δ)
	(#x7e5 . ?ε)
	(#x7e6 . ?ζ)
	(#x7e7 . ?η)
	(#x7e8 . ?θ)
	(#x7e9 . ?ι)
	(#x7ea . ?κ)
	(#x7eb . ?λ)
	(#x7ec . ?μ)
	(#x7ed . ?ν)
	(#x7ee . ?ξ)
	(#x7ef . ?ο)
	(#x7f0 . ?π)
	(#x7f1 . ?ρ)
	(#x7f2 . ?σ)
	(#x7f3 . ?ς)
	(#x7f4 . ?τ)
	(#x7f5 . ?υ)
	(#x7f6 . ?φ)
	(#x7f7 . ?χ)
	(#x7f8 . ?ψ)
	(#x7f9 . ?ω)
	 ;; Technical
	(#x8a1 . ?⎷)
	(#x8a2 . ?┌)
	(#x8a3 . ?─)
	(#x8a4 . ?⌠)
	(#x8a5 . ?⌡)
	(#x8a6 . ?│)
	(#x8a7 . ?⎡)
	(#x8a8 . ?⎣)
	(#x8a9 . ?⎤)
	(#x8aa . ?⎦)
	(#x8ab . ?⎛)
	(#x8ac . ?⎝)
	(#x8ad . ?⎞)
	(#x8ae . ?⎠)
	(#x8af . ?⎨)
	(#x8b0 . ?⎬)
	(#x8bc . ?≤)
	(#x8bd . ?≠)
	(#x8be . ?≥)
	(#x8bf . ?∫)
	(#x8c0 . ?∴)
	(#x8c1 . ?∝)
	(#x8c2 . ?∞)
	(#x8c5 . ?∇)
	(#x8c8 . ?∼)
	(#x8c9 . ?≃)
	(#x8cd . ?⇔)
	(#x8ce . ?⇒)
	(#x8cf . ?≡)
	(#x8d6 . ?√)
	(#x8da . ?⊂)
	(#x8db . ?⊃)
	(#x8dc . ?∩)
	(#x8dd . ?∪)
	(#x8de . ?∧)
	(#x8df . ?∨)
	(#x8ef . ?∂)
	(#x8f6 . ?ƒ)
	(#x8fb . ?←)
	(#x8fc . ?↑)
	(#x8fd . ?→)
	(#x8fe . ?↓)
	;; Special
	(#x9e0 . ?◆)
	(#x9e1 . ?▒)
	(#x9e2 . ?␉)
	(#x9e3 . ?␌)
	(#x9e4 . ?␍)
	(#x9e5 . ?␊)
	(#x9e8 . ?␤)
	(#x9e9 . ?␋)
	(#x9ea . ?┘)
	(#x9eb . ?┐)
	(#x9ec . ?┌)
	(#x9ed . ?└)
	(#x9ee . ?┼)
	(#x9ef . ?⎺)
	(#x9f0 . ?⎻)
	(#x9f1 . ?─)
	(#x9f2 . ?⎼)
	(#x9f3 . ?⎽)
	(#x9f4 . ?├)
	(#x9f5 . ?┤)
	(#x9f6 . ?┴)
	(#x9f7 . ?┬)
	(#x9f8 . ?│)
	;; Publishing
	(#xaa1 . ? )
	(#xaa2 . ? )
	(#xaa3 . ? )
	(#xaa4 . ? )
	(#xaa5 . ? )
	(#xaa6 . ? )
	(#xaa7 . ? )
	(#xaa8 . ? )
	(#xaa9 . ?—)
	(#xaaa . ?–)
	(#xaae . ?…)
	(#xaaf . ?‥)
	(#xab0 . ?⅓)
	(#xab1 . ?⅔)
	(#xab2 . ?⅕)
	(#xab3 . ?⅖)
	(#xab4 . ?⅗)
	(#xab5 . ?⅘)
	(#xab6 . ?⅙)
	(#xab7 . ?⅚)
	(#xab8 . ?℅)
	(#xabb . ?‒)
	(#xabc . ?〈)
	(#xabe . ?〉)
	(#xac3 . ?⅛)
	(#xac4 . ?⅜)
	(#xac5 . ?⅝)
	(#xac6 . ?⅞)
	(#xac9 . ?™)
	(#xaca . ?☓)
	(#xacc . ?◁)
	(#xacd . ?▷)
	(#xace . ?○)
	(#xacf . ?▯)
	(#xad0 . ?‘)
	(#xad1 . ?’)
	(#xad2 . ?“)
	(#xad3 . ?”)
	(#xad4 . ?℞)
	(#xad6 . ?′)
	(#xad7 . ?″)
	(#xad9 . ?✝)
	(#xadb . ?▬)
	(#xadc . ?◀)
	(#xadd . ?▶)
	(#xade . ?●)
	(#xadf . ?▮)
	(#xae0 . ?◦)
	(#xae1 . ?▫)
	(#xae2 . ?▭)
	(#xae3 . ?△)
	(#xae4 . ?▽)
	(#xae5 . ?☆)
	(#xae6 . ?•)
	(#xae7 . ?▪)
	(#xae8 . ?▲)
	(#xae9 . ?▼)
	(#xaea . ?☜)
	(#xaeb . ?☞)
	(#xaec . ?♣)
	(#xaed . ?♦)
	(#xaee . ?♥)
	(#xaf0 . ?✠)
	(#xaf1 . ?†)
	(#xaf2 . ?‡)
	(#xaf3 . ?✓)
	(#xaf4 . ?✗)
	(#xaf5 . ?♯)
	(#xaf6 . ?♭)
	(#xaf7 . ?♂)
	(#xaf8 . ?♀)
	(#xaf9 . ?☎)
	(#xafa . ?⌕)
	(#xafb . ?℗)
	(#xafc . ?‸)
	(#xafd . ?‚)
	(#xafe . ?„)
	;; APL
	(#xba3 . ?<)
	(#xba6 . ?>)
	(#xba8 . ?∨)
	(#xba9 . ?∧)
	(#xbc0 . ?¯)
	(#xbc2 . ?⊥)
	(#xbc3 . ?∩)
	(#xbc4 . ?⌊)
	(#xbc6 . ?_)
	(#xbca . ?∘)
	(#xbcc . ?⎕)
	(#xbce . ?⊤)
	(#xbcf . ?○)
	(#xbd3 . ?⌈)
	(#xbd6 . ?∪)
	(#xbd8 . ?⊃)
	(#xbda . ?⊂)
	(#xbdc . ?⊢)
	(#xbfc . ?⊣)
	;; Hebrew
	(#xcdf . ?‗)
	(#xce0 . ?א)
	(#xce1 . ?ב)
	(#xce2 . ?ג)
	(#xce3 . ?ד)
	(#xce4 . ?ה)
	(#xce5 . ?ו)
	(#xce6 . ?ז)
	(#xce7 . ?ח)
	(#xce8 . ?ט)
	(#xce9 . ?י)
	(#xcea . ?ך)
	(#xceb . ?כ)
	(#xcec . ?ל)
	(#xced . ?ם)
	(#xcee . ?מ)
	(#xcef . ?ן)
	(#xcf0 . ?נ)
	(#xcf1 . ?ס)
	(#xcf2 . ?ע)
	(#xcf3 . ?ף)
	(#xcf4 . ?פ)
	(#xcf5 . ?ץ)
	(#xcf6 . ?צ)
	(#xcf7 . ?ק)
	(#xcf8 . ?ר)
	(#xcf9 . ?ש)
	(#xcfa . ?ת)
	;; Thai
	(#xda1 . ?ก)
	(#xda2 . ?ข)
	(#xda3 . ?ฃ)
	(#xda4 . ?ค)
	(#xda5 . ?ฅ)
	(#xda6 . ?ฆ)
	(#xda7 . ?ง)
	(#xda8 . ?จ)
	(#xda9 . ?ฉ)
	(#xdaa . ?ช)
	(#xdab . ?ซ)
	(#xdac . ?ฌ)
	(#xdad . ?ญ)
	(#xdae . ?ฎ)
	(#xdaf . ?ฏ)
	(#xdb0 . ?ฐ)
	(#xdb1 . ?ฑ)
	(#xdb2 . ?ฒ)
	(#xdb3 . ?ณ)
	(#xdb4 . ?ด)
	(#xdb5 . ?ต)
	(#xdb6 . ?ถ)
	(#xdb7 . ?ท)
	(#xdb8 . ?ธ)
	(#xdb9 . ?น)
	(#xdba . ?บ)
	(#xdbb . ?ป)
	(#xdbc . ?ผ)
	(#xdbd . ?ฝ)
	(#xdbe . ?พ)
	(#xdbf . ?ฟ)
	(#xdc0 . ?ภ)
	(#xdc1 . ?ม)
	(#xdc2 . ?ย)
	(#xdc3 . ?ร)
	(#xdc4 . ?ฤ)
	(#xdc5 . ?ล)
	(#xdc6 . ?ฦ)
	(#xdc7 . ?ว)
	(#xdc8 . ?ศ)
	(#xdc9 . ?ษ)
	(#xdca . ?ส)
	(#xdcb . ?ห)
	(#xdcc . ?ฬ)
	(#xdcd . ?อ)
	(#xdce . ?ฮ)
	(#xdcf . ?ฯ)
	(#xdd0 . ?ะ)
	(#xdd1 . ?ั)
	(#xdd2 . ?า)
	(#xdd3 . ?ำ)
	(#xdd4 . ?ิ)
	(#xdd5 . ?ี)
	(#xdd6 . ?ึ)
	(#xdd7 . ?ื)
	(#xdd8 . ?ุ)
	(#xdd9 . ?ู)
	(#xdda . ?ฺ)
	(#xddf . ?฿)
	(#xde0 . ?เ)
	(#xde1 . ?แ)
	(#xde2 . ?โ)
	(#xde3 . ?ใ)
	(#xde4 . ?ไ)
	(#xde5 . ?ๅ)
	(#xde6 . ?ๆ)
	(#xde7 . ?็)
	(#xde8 . ?่)
	(#xde9 . ?้)
	(#xdea . ?๊)
	(#xdeb . ?๋)
	(#xdec . ?์)
	(#xded . ?ํ)
	(#xdf0 . ?๐)
	(#xdf1 . ?๑)
	(#xdf2 . ?๒)
	(#xdf3 . ?๓)
	(#xdf4 . ?๔)
	(#xdf5 . ?๕)
	(#xdf6 . ?๖)
	(#xdf7 . ?๗)
	(#xdf8 . ?๘)
	(#xdf9 . ?๙)
	;; Korean
	(#xea1 . ?ㄱ)
	(#xea2 . ?ㄲ)
	(#xea3 . ?ㄳ)
	(#xea4 . ?ㄴ)
	(#xea5 . ?ㄵ)
	(#xea6 . ?ㄶ)
	(#xea7 . ?ㄷ)
	(#xea8 . ?ㄸ)
	(#xea9 . ?ㄹ)
	(#xeaa . ?ㄺ)
	(#xeab . ?ㄻ)
	(#xeac . ?ㄼ)
	(#xead . ?ㄽ)
	(#xeae . ?ㄾ)
	(#xeaf . ?ㄿ)
	(#xeb0 . ?ㅀ)
	(#xeb1 . ?ㅁ)
	(#xeb2 . ?ㅂ)
	(#xeb3 . ?ㅃ)
	(#xeb4 . ?ㅄ)
	(#xeb5 . ?ㅅ)
	(#xeb6 . ?ㅆ)
	(#xeb7 . ?ㅇ)
	(#xeb8 . ?ㅈ)
	(#xeb9 . ?ㅉ)
	(#xeba . ?ㅊ)
	(#xebb . ?ㅋ)
	(#xebc . ?ㅌ)
	(#xebd . ?ㅍ)
	(#xebe . ?ㅎ)
	(#xebf . ?ㅏ)
	(#xec0 . ?ㅐ)
	(#xec1 . ?ㅑ)
	(#xec2 . ?ㅒ)
	(#xec3 . ?ㅓ)
	(#xec4 . ?ㅔ)
	(#xec5 . ?ㅕ)
	(#xec6 . ?ㅖ)
	(#xec7 . ?ㅗ)
	(#xec8 . ?ㅘ)
	(#xec9 . ?ㅙ)
	(#xeca . ?ㅚ)
	(#xecb . ?ㅛ)
	(#xecc . ?ㅜ)
	(#xecd . ?ㅝ)
	(#xece . ?ㅞ)
	(#xecf . ?ㅟ)
	(#xed0 . ?ㅠ)
	(#xed1 . ?ㅡ)
	(#xed2 . ?ㅢ)
	(#xed3 . ?ㅣ)
	(#xed4 . ?ᆨ)
	(#xed5 . ?ᆩ)
	(#xed6 . ?ᆪ)
	(#xed7 . ?ᆫ)
	(#xed8 . ?ᆬ)
	(#xed9 . ?ᆭ)
	(#xeda . ?ᆮ)
	(#xedb . ?ᆯ)
	(#xedc . ?ᆰ)
	(#xedd . ?ᆱ)
	(#xede . ?ᆲ)
	(#xedf . ?ᆳ)
	(#xee0 . ?ᆴ)
	(#xee1 . ?ᆵ)
	(#xee2 . ?ᆶ)
	(#xee3 . ?ᆷ)
	(#xee4 . ?ᆸ)
	(#xee5 . ?ᆹ)
	(#xee6 . ?ᆺ)
	(#xee7 . ?ᆻ)
	(#xee8 . ?ᆼ)
	(#xee9 . ?ᆽ)
	(#xeea . ?ᆾ)
	(#xeeb . ?ᆿ)
	(#xeec . ?ᇀ)
	(#xeed . ?ᇁ)
	(#xeee . ?ᇂ)
	(#xeef . ?ㅭ)
	(#xef0 . ?ㅱ)
	(#xef1 . ?ㅸ)
	(#xef2 . ?ㅿ)
	(#xef3 . ?ㆁ)
	(#xef4 . ?ㆄ)
	(#xef5 . ?ㆆ)
	(#xef6 . ?ㆍ)
	(#xef7 . ?ㆎ)
	(#xef8 . ?ᇫ)
	(#xef9 . ?ᇰ)
	(#xefa . ?ᇹ)
	(#xeff . ?₩)
	;; Latin-5
	;; Latin-6
	;; Latin-7
	;; Latin-8
	;; Latin-9
	(#x13bc . ?Œ)
	(#x13bd . ?œ)
	(#x13be . ?Ÿ)
	;; Currency
	(#x20a0 . ?₠)
	(#x20a1 . ?₡)
	(#x20a2 . ?₢)
	(#x20a3 . ?₣)
	(#x20a4 . ?₤)
	(#x20a5 . ?₥)
	(#x20a6 . ?₦)
	(#x20a7 . ?₧)
	(#x20a8 . ?₨)
	(#x20aa . ?₪)
	(#x20ab . ?₫)
	(#x20ac . ?€)))
  (puthash (car pair) (cdr pair) x-keysym-table))

;; The following keysym codes for graphics are listed in the document
;; as not having unicodes available:

;; #x08b1	TOP LEFT SUMMATION	Technical
;; #x08b2	BOTTOM LEFT SUMMATION	Technical
;; #x08b3	TOP VERTICAL SUMMATION CONNECTOR	Technical
;; #x08b4	BOTTOM VERTICAL SUMMATION CONNECTOR	Technical
;; #x08b5	TOP RIGHT SUMMATION	Technical
;; #x08b6	BOTTOM RIGHT SUMMATION	Technical
;; #x08b7	RIGHT MIDDLE SUMMATION	Technical
;; #x0aac	SIGNIFICANT BLANK SYMBOL	Publish
;; #x0abd	DECIMAL POINT	Publish
;; #x0abf	MARKER	Publish
;; #x0acb	TRADEMARK SIGN IN CIRCLE	Publish
;; #x0ada	HEXAGRAM	Publish
;; #x0aff	CURSOR	Publish
;; #x0dde	THAI MAIHANAKAT	Thai


;;;; Selections

(define-obsolete-function-alias 'x-cut-buffer-or-selection-value
  'x-selection-value "24.1")

;; Arrange for the kill and yank functions to set and check the clipboard.

(defun x-clipboard-yank ()
  "Insert the clipboard contents, or the last stretch of killed text."
  (declare (obsolete clipboard-yank "25.1"))
  (interactive "*")
  (let ((clipboard-text (gui--selection-value-internal 'CLIPBOARD))
	(select-enable-clipboard t))
    (if (and clipboard-text (> (length clipboard-text) 0))
	(kill-new clipboard-text))
    (yank)))

(declare-function accelerate-menu "xmenu.c" (&optional frame) t)

(defun x-menu-bar-open (&optional frame)
  "Open the menu bar if it is shown.
`popup-menu' is used if it is off."
  (interactive "i")
  (cond
   ((and (not (zerop (or (frame-parameter nil 'menu-bar-lines) 0)))
	 (fboundp 'accelerate-menu))
    (accelerate-menu frame))
   (t
    (popup-menu (mouse-menu-bar-map) last-nonmenu-event))))


;;; Window system initialization.

(defun x-win-suspend-error ()
  "Report an error when a suspend is attempted.
This returns an error if any Emacs frames are X frames."
  ;; Don't allow suspending if any of the frames are X frames.
  (if (memq 'x (mapcar #'window-system (frame-list)))
      (error "Cannot suspend Emacs while an X GUI frame exists")))

(defvar x-initialized nil
  "Non-nil if the X window system has been initialized.")

(declare-function x-open-connection "xfns.c"
		  (display &optional xrm-string must-succeed))
(declare-function x-server-max-request-size "xfns.c" (&optional terminal))
(declare-function x-get-resource "frame.c"
		  (attribute class &optional component subclass))
(declare-function x-parse-geometry "frame.c" (string))
(defvar x-resource-name)
(defvar x-display-name)
(defvar x-command-line-resources)

(cl-defmethod window-system-initialization (&context (window-system x)
                                            &optional display)
  "Initialize Emacs for X frames and open the first connection to an X server."
  (cl-assert (not x-initialized))

  ;; Make sure we have a valid resource name.
  (or (stringp x-resource-name)
      (let (i)
	(setq x-resource-name (copy-sequence invocation-name))

	;; Change any . or * characters in x-resource-name to hyphens,
	;; so as not to choke when we use it in X resource queries.
	(while (setq i (string-match "[.*]" x-resource-name))
	  (aset x-resource-name i ?-))))

  (x-open-connection (or display
			 (setq x-display-name (or (getenv "DISPLAY" (selected-frame))
						  (getenv "DISPLAY"))))
		     x-command-line-resources
		     ;; Exit Emacs with fatal error if this fails and we
		     ;; are the initial display.
		     (eq initial-window-system 'x))

  ;; Create the default fontset.
  (create-default-fontset)

  ;; Create the standard fontset.
  (condition-case err
	(create-fontset-from-fontset-spec standard-fontset-spec t)
    (error (display-warning
	    'initialization
	    (format "Creation of the standard fontset failed: %s" err)
	    :error)))

  ;; Create fontset specified in X resources "Fontset-N" (N is 0, 1, ...).
  (create-fontset-from-x-resource)

  ;; Set scroll bar mode to right if set by X resources. Default is left.
  (if (equal (x-get-resource "verticalScrollBars" "ScrollBars") "right")
      (customize-set-variable 'scroll-bar-mode 'right))

  ;; Apply a geometry resource to the initial frame.  Put it at the end
  ;; of the alist, so that anything specified on the command line takes
  ;; precedence.
  (let* ((res-geometry (x-get-resource "geometry" "Geometry"))
	 parsed)
    (if res-geometry
	(progn
	  (setq parsed (x-parse-geometry res-geometry))
	  ;; If the resource specifies a position,
	  ;; call the position and size "user-specified".
	  (if (or (assq 'top parsed) (assq 'left parsed))
	      (setq parsed (cons '(user-position . t)
				 (cons '(user-size . t) parsed))))
	  ;; All geometry parms apply to the initial frame.
	  (setq initial-frame-alist (append initial-frame-alist parsed))
	  ;; The size parms apply to all frames.  Don't set it if there are
	  ;; sizes there already (from command line).
	  (if (and (assq 'height parsed)
		   (not (assq 'height default-frame-alist)))
	      (setq default-frame-alist
		    (cons (cons 'height (cdr (assq 'height parsed)))
			  default-frame-alist)))
	  (if (and (assq 'width parsed)
		   (not (assq 'width default-frame-alist)))
	      (setq default-frame-alist
		    (cons (cons 'width (cdr (assq 'width parsed)))
			  default-frame-alist))))))

  ;; Check the reverseVideo resource.
  (let ((case-fold-search t))
    (let ((rv (x-get-resource "reverseVideo" "ReverseVideo")))
      (if (and rv
	       (string-match "^\\(true\\|yes\\|on\\)$" rv))
	  (setq default-frame-alist
		(cons '(reverse . t) default-frame-alist)))))

  ;; Set x-selection-timeout, measured in milliseconds.
  (let ((res-selection-timeout (x-get-resource "selectionTimeout"
					       "SelectionTimeout")))
    (setq x-selection-timeout
	  (if res-selection-timeout
	      (string-to-number res-selection-timeout)
	    5000)))

  ;; Don't let Emacs suspend under X.
  (add-hook 'suspend-hook 'x-win-suspend-error)

  ;; During initialization, we defer sending size hints to the window
  ;; manager, because that can induce a race condition:
  ;; https://lists.gnu.org/r/emacs-devel/2008-10/msg00033.html
  ;; Send the size hints once initialization is done.
  (add-hook 'after-init-hook 'x-wm-set-size-hint)

  ;; Turn off window-splitting optimization; X is usually fast enough
  ;; that this is only annoying.
  (setq split-window-keep-point t)

  ;; Motif direct handling of f10 wasn't working right,
  ;; So temporarily we've turned it off in lwlib-Xm.c
  ;; and turned the Emacs f10 back on.
  ;; ;; Motif normally handles f10 itself, so don't try to handle it a second time.
  ;; (if (featurep 'motif)
  ;;     (global-set-key [f10] 'ignore))

  ;; Enable CLIPBOARD copy/paste through menu bar commands.
  (menu-bar-enable-clipboard)

  ;; ;; Override Paste so it looks at CLIPBOARD first.
  ;; (define-key menu-bar-edit-menu [paste]
  ;;   (append '(menu-item "Paste" x-clipboard-yank
  ;; 			:enable (not buffer-read-only)
  ;; 			:help "Paste (yank) text most recently cut/copied")
  ;; 	    nil))

  (x-apply-session-resources)
  (setq x-initialized t))

(declare-function x-own-selection-internal "xselect.c"
		  (selection value &optional frame))
(declare-function x-disown-selection-internal "xselect.c"
		  (selection &optional time-object terminal))
(declare-function x-selection-owner-p "xselect.c"
		  (&optional selection terminal))
(declare-function x-selection-exists-p "xselect.c"
		  (&optional selection terminal))
(declare-function x-get-selection-internal "xselect.c"
		  (selection-symbol target-type &optional time-stamp terminal))

(add-to-list 'display-format-alist '("\\`[^:]*:[0-9]+\\(\\.[0-9]+\\)?\\'" . x))
(cl-defmethod handle-args-function (args &context (window-system x))
  (x-handle-args args))

(cl-defmethod frame-creation-function (params &context (window-system x))
  (x-create-frame-with-faces params))

(cl-defmethod gui-backend-set-selection (selection value
                                         &context (window-system x))
  (if value (x-own-selection-internal selection value)
    (x-disown-selection-internal selection)))

(cl-defmethod gui-backend-selection-owner-p (selection
                                             &context (window-system x))
  (x-selection-owner-p selection))

(cl-defmethod gui-backend-selection-exists-p (selection
                                              &context (window-system x))
  (x-selection-exists-p selection))

(cl-defmethod gui-backend-get-selection (selection-symbol target-type
                                         &context (window-system x)
                                         &optional time-stamp terminal)
  (x-get-selection-internal selection-symbol target-type time-stamp terminal))

;; Initiate drag and drop
(add-hook 'after-make-frame-functions 'x-dnd-init-frame)
(define-key special-event-map [drag-n-drop] 'x-dnd-handle-drag-n-drop-event)

(defcustom x-gtk-stock-map
  (mapcar (lambda (arg)
	    (cons (purecopy (car arg)) (purecopy (cdr arg))))
  '(
    ("etc/images/new" . ("document-new" "gtk-new"))
    ("etc/images/open" . ("document-open" "gtk-open"))
    ("etc/images/diropen" . "n:system-file-manager")
    ("etc/images/close" . ("window-close" "gtk-close"))
    ("etc/images/save" . ("document-save" "gtk-save"))
    ("etc/images/saveas" . ("document-save-as" "gtk-save-as"))
    ("etc/images/undo" . ("edit-undo" "gtk-undo"))
    ("etc/images/cut" . ("edit-cut" "gtk-cut"))
    ("etc/images/copy" . ("edit-copy" "gtk-copy"))
    ("etc/images/paste" . ("edit-paste" "gtk-paste"))
    ("etc/images/search" . ("edit-find" "gtk-find"))
    ("etc/images/print" . ("document-print" "gtk-print"))
    ("etc/images/preferences" . ("preferences-system" "gtk-preferences"))
    ("etc/images/help" . ("help-browser" "gtk-help"))
    ("etc/images/left-arrow" . ("go-previous" "gtk-go-back"))
    ("etc/images/right-arrow" . ("go-next" "gtk-go-forward"))
    ("etc/images/home" . ("go-home" "gtk-home"))
    ("etc/images/jump-to" . ("go-jump" "gtk-jump-to"))
    ("etc/images/index" . "gtk-index")
    ("etc/images/exit" . ("application-exit" "gtk-quit"))
    ("etc/images/cancel" . "gtk-cancel")
    ("etc/images/info" . ("dialog-information" "gtk-info"))
    ("etc/images/bookmark_add" . "n:bookmark_add")
    ;; Used in Gnus and/or MH-E:
    ("etc/images/attach" . "gtk-attach")
    ("etc/images/connect" . "gtk-connect")
    ("etc/images/contact" . "gtk-contact")
    ("etc/images/delete" . ("edit-delete" "gtk-delete"))
    ("etc/images/describe" . ("document-properties" "gtk-properties"))
    ("etc/images/disconnect" . "gtk-disconnect")
    ;; ("etc/images/exit" . "gtk-exit")
    ("etc/images/lock-broken" . "gtk-lock_broken")
    ("etc/images/lock-ok" . "gtk-lock_ok")
    ("etc/images/lock" . "gtk-lock")
    ("etc/images/next-page" . "gtk-next-page")
    ("etc/images/refresh" . ("view-refresh" "gtk-refresh"))
    ("etc/images/sort-ascending" . ("view-sort-ascending" "gtk-sort-ascending"))
    ("etc/images/sort-column-ascending" . "gtk-sort-column-ascending")
    ("etc/images/sort-criteria" . "gtk-sort-criteria")
    ("etc/images/sort-descending" . ("view-sort-descending"
				     "gtk-sort-descending"))
    ("etc/images/sort-row-ascending" . "gtk-sort-row-ascending")
    ("images/gnus/toggle-subscription" . "gtk-task-recurring")
    ("images/mail/compose" . "gtk-mail-compose")
    ("images/mail/copy" . "gtk-mail-copy")
    ("images/mail/forward" . "gtk-mail-forward")
    ("images/mail/inbox" . "gtk-inbox")
    ("images/mail/move" . "gtk-mail-move")
    ("images/mail/not-spam" . "gtk-not-spam")
    ("images/mail/outbox" . "gtk-outbox")
    ("images/mail/reply-all" . "gtk-mail-reply-to-all")
    ("images/mail/reply" . "gtk-mail-reply")
    ("images/mail/save-draft" . "gtk-mail-handling")
    ("images/mail/send" . "gtk-mail-send")
    ("images/mail/spam" . "gtk-spam")
    ;; Used for GDB Graphical Interface
    ("images/gud/break" . "gtk-no")
    ("images/gud/recstart" . ("media-record" "gtk-media-record"))
    ("images/gud/recstop" . ("media-playback-stop" "gtk-media-stop"))
    ;; No themed versions available:
    ;; mail/preview (combining stock_mail and stock_zoom)
    ;; mail/save    (combining stock_mail, stock_save and stock_convert)
    ))
  "How icons for tool bars are mapped to Gtk+ stock items.
Emacs must be compiled with the Gtk+ toolkit for this to have any effect.
A value that begins with n: denotes a named icon instead of a stock icon."
  :version "22.2"
  :type '(choice (repeat
		  (choice symbol
			  (cons (string :tag "Emacs icon")
				(choice (group (string :tag "Named")
					       (string :tag "Stock"))
					(string :tag "Stock/named"))))))
  :group 'x)

(defcustom icon-map-list '(x-gtk-stock-map)
  "A list of alists that map icon file names to stock/named icons.
The alists are searched in the order they appear.  The first match is used.
The keys in the alists are file names without extension and with two directory
components.  For example, to map /usr/share/emacs/22.1.1/etc/images/open.xpm
to stock item gtk-open, use:

  (\"etc/images/open\" . \"gtk-open\")

Themes also have named icons.  To map to one of those, use n: before the name:

  (\"etc/images/diropen\" . \"n:system-file-manager\")

The list elements are either the symbol name for the alist or the
alist itself.

If you don't want stock icons, set the variable to nil."
  :version "22.2"
  :type '(choice (const :tag "Don't use stock icons" nil)
		 (repeat (choice symbol
				 (cons (string :tag "Emacs icon")
				       (string :tag "Stock/named")))))
  :group 'x)

(defconst x-gtk-stock-cache (make-hash-table :weakness t :test 'equal))

(defun x-gtk-map-stock (file)
  "Map icon with file name FILE to a Gtk+ stock name.
This uses `icon-map-list' to map icon file names to stock icon names."
  (when (stringp file)
    (or (gethash file x-gtk-stock-cache)
	(puthash
	 file
	 (save-match-data
	   (let* ((file-sans (file-name-sans-extension file))
		  (key (and (string-match "/\\([^/]+/[^/]+/[^/]+$\\)"
					  file-sans)
			    (match-string 1 file-sans)))
		  (icon-map icon-map-list)
		  elem value)
	     (while (and (null value) icon-map)
	       (setq elem (car icon-map)
		     value (assoc-string (or key file-sans)
					 (if (symbolp elem)
					     (symbol-value elem)
					   elem))
		     icon-map (cdr icon-map)))
	     (and value (cdr value))))
	 x-gtk-stock-cache))))

(global-set-key [XF86WakeUp] 'ignore)

(provide 'x-win)
(provide 'term/x-win)

;;; x-win.el ends here
