;;; keypad.el --- simplified keypad bindings

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: Kim F. Storm <storm@cua.dk>
;; Keywords: keyboard convenience

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

;; The keypad package allows easy binding of the keypad keys to
;; various commonly used sets of commands.
;;
;; With the following setup, the keypad can be used for numeric data
;; entry, or to give numeric prefix arguments to emacs commands.
;;
;;  (keypad-setup 'numeric)
;;  (keypad-setup 'prefix t)
;;
;;    +--------+--------+--------+
;;    |  M-7   |  M-8   |  M-9   |
;;    |   7    |   8    |   9    |
;;    +--------+--------+--------+
;;    |  M-4   |  M-5   |  M-6   |
;;    |   4    |   5    |   6    |
;;    +--------+--------+--------+
;;    |  M-1   |  M-2   |  M-3   |
;;    |   1    |   2    |   3    |
;;    +--------+--------+--------+
;;    |       M-0       |  M--   |
;;    |        0        |   .    |
;;    +-----------------+--------+

;; The following keypad setup is used for navigation:
;;
;;  (keypad-setup 'cursor)
;;  (keypad-setup 'S-cursor t)
;;
;;    +--------+--------+--------+
;;    | S-home | S-up   | S-PgUp |
;;    |  Home  |  up    |  PgUp  |
;;    +--------+--------+--------+
;;    | S-left |S-space |S-right |
;;    |  left  | space  | right  |
;;    +--------+--------+--------+
;;    | S-end  | S-down | S-PgDn |
;;    |  end   |  down  |  PgDn  |
;;    +--------+--------+--------+
;;    |    S-insert     |S-delete|
;;    |     insert      | delete |
;;    +-----------------+--------+


;;; Code:

(provide 'keypad)

;;; Customization

;;;###autoload
(defcustom keypad-setup nil
  "Specifies the keypad setup for unshifted keypad keys.
The options are:
 'prefix   Numeric prefix argument, i.e.  M-0 .. M-9 and M--
 'cursor   Cursor movement keys.
 'S-cursor Shifted cursor movement keys.
 'numeric  Plain numeric, i.e. 0 .. 9 and .  (or DECIMAL arg)
 'none     Removes all bindings for keypad keys in function-key-map.
 nil       Keep existing bindings for the keypad keys."
  :set (lambda (symbol value)
	 (if value
	     (keypad-setup value nil keypad-decimal-key)))
  :initialize 'custom-initialize-default
  :set-after '(keypad-decimal-key)
  :require 'keypad
  :link '(emacs-commentary-link "keypad.el")
  :version "21.4"
  :type '(choice (const :tag "Numeric prefix arguments" prefix) 
		 (const :tag "Cursor keys" cursor)
		 (const :tag "Shifted cursor keys" S-cursor)
		 (const :tag "Plain Numeric Keypad" numeric)
		 (const :tag "Remove bindings" none)
		 (other :tag "Keep existing bindings" :value nil))
  :group 'keyboard)

(defcustom keypad-decimal-key ?.
  "Character produced by the unshifted decimal key on the keypad."
  :type 'character
  :group 'keyboard)

;;;###autoload
(defcustom keypad-shifted-setup nil
  "Specifies the keypad setup for shifted keypad keys.
See `keypad-setup' for available options."
  :set (lambda (symbol value)
	 (if value
	     (keypad-setup value t keypad-shifted-decimal-key)))
  :initialize 'custom-initialize-default
  :set-after '(keypad-shifted-decimal-key)
  :require 'keypad
  :link '(emacs-commentary-link "keypad.el")
  :version "21.4"
  :type '(choice (const :tag "Numeric prefix arguments" prefix) 
		 (const :tag "Cursor keys" cursor)
		 (const :tag "Shifted cursor keys" S-cursor)
		 (const :tag "Plain Numeric Keypad" numeric)
		 (const :tag "Remove bindings" none)
		 (other :tag "Keep existing bindings" :value nil))
  :group 'keyboard)

(defcustom keypad-shifted-decimal-key ?.
  "Character produced by the unshifted decimal key on the keypad."
  :type 'character
  :group 'keyboard)

;;;###autoload
(defun keypad-setup (setup &optional numlock decimal)
  "Set keypad bindings in function-key-map according to SETUP.
If optional second argument NUMLOCK is non-nil, the NumLock On bindings
are changed. Otherwise, the NumLock Off bindings are changed.

 Setup      Binding
 -------------------------------------------------------------
 'prefix   Command prefix argument, i.e.  M-0 .. M-9 and M--
 'S-cursor Bind shifted keypad keys to the shifted cursor movement keys.
 'cursor   Bind keypad keys to the cursor movement keys.
 'numeric  Plain numeric, i.e. 0 .. 9 and .  (or DECIMAL arg)
 'none     Removes all bindings for keypad keys in function-key-map.

If SETUP is 'numeric and the optional third argument DECIMAL is non-nil,
the decimal key on the keypad is mapped to DECIMAL instead of `.'"
  (let ((i 0)
	(kp
	 (cond
	  (numlock
	   [kp-decimal kp-0 kp-1 kp-2 kp-3 kp-4
		       kp-5 kp-6 kp-7 kp-8 kp-9])
	  (t
	   [kp-delete kp-insert kp-end kp-down kp-next kp-left
		      kp-space kp-right kp-home kp-up kp-prior])))
	(bind
	 (cond
	  ((eq setup 'numeric)
	   (vector (or decimal ?.) ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
	  ((eq setup 'prefix)
	   [?\M-- ?\M-0 ?\M-1 ?\M-2 ?\M-3 ?\M-4
		  ?\M-5 ?\M-6 ?\M-7 ?\M-8 ?\M-9])
	  ((eq setup 'cursor)
	   [delete insert end down next left
		  space right home up prior])
	  ((eq setup 'S-cursor)
	   [S-delete S-insert S-end S-down S-next S-left 
		     S-space S-right S-home S-up S-prior])
	  ((eq setup 'none)
	   nil)
	  (t
	   (signal 'error (list "Unknown keypad setup: " setup))))))

    ;; Bind the keys in KP list to BIND list in function-key-map.
    ;; If BIND is nil, all bindings for the keys are removed.
    (if (not (boundp 'function-key-map))
	(setq function-key-map (make-sparse-keymap)))

    (while (< i 11)
      (define-key function-key-map (vector (aref kp i))
	(if bind (vector (aref bind i))))
      (setq i (1+ i)))))

;;; keypad.el ends here
