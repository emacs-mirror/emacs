;;; esh-toggle --- toggle to and from the *eshell* buffer

;; Copyright (C) 1997, 1998 Mikael Sjödin (mic@docs.uu.se)

;; Author: Mikael Sjödin <mic@docs.uu.se>
;;         John Wiegley <johnw@gnu.org>
;; Created: 19 Nov 1998
;; Version: 2.0
;; Keywords: processes
;; X-URL: http://www.emacs.org/~johnw/eshell.html

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provides the command eshell-toggle which toggles between the
;; *eshell* buffer and whatever buffer you are editing.
;;
;; This is done in an "intelligent" way.  Features are:
;;
;;  - Starts a eshell if non is existing.
;;
;;  - Minimum distortion of your window configuration.
;;
;;  - When done in the eshell-buffer you are returned to the same
;;    window configuration you had before you toggled to the eshell.
;;
;;  - If you desire, you automagically get a "cd" command in the
;;    eshell to the directory where your current buffers file exists;
;;    just call eshell-toggle-cd instead of eshell-toggle.
;;
;;  - You can convinently choose if you want to have the eshell in
;;    another window or in the whole frame.  Just invoke eshell-toggle
;;    again to get the eshell in the whole frame.
;;
;; This file has been tested under Emacs 20.2.
;;
;; To use, call the functions `eshell-toggle' or `eshell-toggle-cd'.
;; It's most helpful to bind these to a key.

;;; Thanks to:

;; Christian Stern <Christian.Stern@physik.uni-regensburg.de> for
;; helpful sugestions.

;;; User Variables:

(defvar eshell-toggle-goto-eob t
  "*If non-nil `eshell-toggle' moves point to end of Eshell buffer.
When `eshell-toggle-cd' is called the point is always moved to the
end of the eshell-buffer")

(defvar eshell-toggle-automatic-cd t
  "*If non-nil `eshell-toggle-cd' will send a \"cd\" to Eshell.
If nil `eshell-toggle-cd' will only insert the \"cd\" command in the
eshell-buffer.  Leaving it to the user to press RET to send the
command to the eshell.")

;;; User Functions:

;;;###autoload
(defun eshell-toggle-cd ()
  "Calls `eshell-toggle' with a prefix argument.
See the command `eshell-toggle'"
  (interactive)
  (eshell-toggle t))

;;;###autoload
(defun eshell-toggle (make-cd)
  "Toggles between the *eshell* buffer and the current buffer.
With a prefix ARG also insert a \"cd DIR\" command into the eshell,
where DIR is the directory of the current buffer.

Call twice in a row to get a full screen window for the *eshell*
buffer.

When called in the *eshell* buffer returns you to the buffer you were
editing before caling the first time.

Options: `eshell-toggle-goto-eob'"
  (interactive "P")
  ;; Try to descide on one of three possibilities:
  ;; 1. If not in eshell-buffer, switch to it.
  ;; 2. If in eshell-buffer and called twice in a row, delete other
  ;;    windows
  ;; 3. If in eshell-buffer and not called twice in a row, return to
  ;;    state before going to the eshell-buffer
  (if (eq major-mode 'eshell-mode)
      (if (and (or (eq last-command 'eshell-toggle)
		   (eq last-command 'eshell-toggle-cd))
	       (not (eq (count-windows) 1)))
	  (delete-other-windows)
	(eshell-toggle-buffer-return-from-eshell))
    (eshell-toggle-buffer-goto-eshell make-cd)))

;;; Internal Functions:

(defvar eshell-toggle-pre-eshell-win-conf nil
  "Contains window config before the *eshell* buffer was selected")

(defun eshell-toggle-buffer-return-from-eshell ()
  "Restores window config used before switching the *eshell* buffer.
If no configuration has been stored, just bury the *eshell* buffer."
  (if (window-configuration-p eshell-toggle-pre-eshell-win-conf)
      (progn
	(set-window-configuration eshell-toggle-pre-eshell-win-conf)
	(setq eshell-toggle-pre-eshell-win-conf nil)
	(bury-buffer (get-buffer "*eshell*")))
    (bury-buffer)))

(defun eshell-toggle-buffer-goto-eshell (make-cd)
  "Switches other window to the *eshell* buffer.
If no *eshell* buffer exists start a new eshell and switch to it in
other window.  If argument MAKE-CD is non-nil, insert a \"cd DIR\"
command into the eshell, where DIR is the directory of the current
buffer.
Stores the window cofiguration before creating and/or switching window."
  (setq eshell-toggle-pre-eshell-win-conf (current-window-configuration))
  (let ((eshell-buffer (get-buffer "*eshell*"))
	(cd-command
	 ;; Find out which directory we are in (the method differs for
	 ;; different buffers)
	 (or (and make-cd
		  (buffer-file-name)
		  (file-name-directory (buffer-file-name))
		  (concat "cd " (file-name-directory (buffer-file-name))))
	     (and make-cd
		  list-buffers-directory
		  (concat "cd " list-buffers-directory)))))
    ;; Switch to an existin eshell if one exists, otherwise switch to
    ;; another window and start a new eshell
    (if eshell-buffer
	(switch-to-buffer-other-window eshell-buffer)
      (eshell-toggle-buffer-switch-to-other-window)
      ;; Sometimes an error is generated when I call `eshell' (it has
      ;; to do with my eshell-mode-hook which inserts text into the
      ;; newly created eshell-buffer and thats not allways a good
      ;; idea).
      (condition-case the-error
	  (eshell)
	(error (switch-to-buffer "*eshell*"))))
    (if (or cd-command eshell-toggle-goto-eob)
	(goto-char (point-max)))
    (if cd-command
	(progn
	  (insert cd-command)
	  (if eshell-toggle-automatic-cd
	      (eshell-send-input))))))

(defun eshell-toggle-buffer-switch-to-other-window ()
  "Switches to other window.
If the current window is the only window in the current frame, create
a new window and switch to it.  (This is less intrusive to the current
window configuration then `switch-buffer-other-window')"
  (let ((this-window (selected-window)))
    (other-window 1)
    ;; If we did not switch window then we only have one window and
    ;; need to create a new one.
    (if (eq this-window (selected-window))
	(progn
	  (split-window-vertically)
	  (other-window 1)))))

(provide 'esh-toggle)

;;; esh-toggle.el ends here
