;;; vt-control.el --- Common VTxxx control functions

;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Rob Riepel <riepel@networking.stanford.edu>
;; Maintainer: Rob Riepel <riepel@networking.stanford.edu>
;; Keywords: vt100

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;;

;;; Revision: $Id: vt-control.el,v 2.2 1993/08/01 21:47:43 riepel Exp $

;;; Commentary:

;;  The functions contained in this file send various VT control codes
;;  to the terminal where emacs is running.  The following functions are
;;  available.

;;    Function           Action

;;    vt-wide            set wide screen (132 characters)
;;    vt-narrow          set narrow screen (80 characters)
;;    vt-toggle-screen   toggle wide/narrow screen
;;    vt-keypad-on       set applications keypad on
;;    vt-keypad-off      set applications keypad off
;;    vt-numlock         toggle applications keypad on/off

;;; Usage:

;;  To use enable these functions, simply load this file.

;;  Note: vt-control makes no effort to determine how the terminal is
;;        initially set.  It assumes the terminal starts with a width
;;        of 80 characters and the applications keypad enabled.  Nor
;;        does vt-control try to restore the terminal when emacs is
;;        killed or suspended.

;;; Code:


;;;  Revision Information

(defconst vt-revision "$Revision: 2.2 $"
  "Revision number of vt-control.")


;;;  Global variables

(defvar vt-applications-keypad-p t
  "If non-nil, keypad is in applications mode.")

(defvar vt-wide-p nil
  "If non-nil, the screen is 132 characters wide.")


;;;  Screen width functions.

(defun vt-wide nil
  "Set the screen 132 characters wide."
  (interactive)
  (send-string-to-terminal "\e[?3h")
  (set-screen-width 132)
  (setq vt-wide-p t))

(defun vt-narrow nil
  "Set the screen 80 characters wide."
  (interactive)
  (send-string-to-terminal "\e[?3l")
  (set-screen-width 80)
  (setq vt-wide-p nil))

(defun vt-toggle-screen nil
  "Toggle between 80 and 132 character screen width."
  (interactive)
  (if vt-wide-p (vt-narrow) (vt-wide)))


;;;  Applications keypad functions.

(defun vt-keypad-on (&optional tell)
  "Turn on the VT applications keypad."
  (interactive)
  (send-string-to-terminal "\e[\e=")
  (setq vt-applications-keypad-p t)
  (if (or tell (interactive-p)) (message "Applications keypad enabled.")))

(defun vt-keypad-off (&optional tell)
  "Turn off the VT applications keypad."
  (interactive "p")
  (send-string-to-terminal "\e[\e>")
  (setq vt-applications-keypad-p nil)
  (if (or tell (interactive-p)) (message "Applications keypad disabled.")))

(defun vt-numlock nil
  "Toggle VT application keypad on and off."
  (interactive)
  (if vt-applications-keypad-p (vt-keypad-off (interactive-p))
    (vt-keypad-on (interactive-p))))

;;; vt-control.el ends here
