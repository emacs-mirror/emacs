;;; wallpaper.el --- Set wallpaper using external command  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>
;; Keywords: images

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

;; This library provides the command `wallpaper-set', which uses an
;; external command to set the desktop background.  This is obviously
;; a bit tricky to get right, as there is no lack of platforms, window
;; managers, desktop environments and tools.
;;
;; If this doesn't work in your environment, customize the user option
;; `wallpaper-commands'.

;;; Code:

(eval-when-compile (require 'subr-x))

(defcustom wallpaper-commands
  '(
    ;; Sway (Wayland)
    ("swaybg" "-o" "*" "-i" "%f" "-m" "fill")
    ;; Gnome
    ("gsettings" "set" "org.gnome.desktop.background" "picture-uri" "file://%f")
    ;; Other / General X
    ("gm" "display" "-size" "%wx%h" "-window" "root" "%f")
    ("display" "-resize" "%wx%h" "-window" "root" "%f")
    ("feh" "--bg-max" "%f")
    ("xloadimage" "-onroot" "-fullscreen" "%f")
    ("xsetbg" " %f")
    )
  "List of executables and arguments for setting the wallpaper.
This is used by `wallpaper-set', which will test the commands
in the order they appear.

Every item in the list has the following form:

  (COMMAND ARG1 .. ARGN)

COMMAND is the name of the executable (a string) and ARG1 .. ARGN
is its command line arguments (also strings).

In each of the command line arguments, \"%f\" will be replaced
with the full file name, \"%h\" with the height of the selected
frame's display (as returned by `display-pixel-height'), and
\"%w\" with the width of the selected frame's display (as
returned by `display-pixel-width').

Note: If you find that you need to use a command that is not in
this list to set the wallpaper in your environment, we would love
to hear about it!  Please send an email to bug-gnu-emacs@gnu.org
and tell us the command (and all options) that worked for you.
You can also use \\[report-emacs-bug]."
  :type '(repeat (repeat string))
  :group 'image
  :version "29.1")

(defvar wallpaper-debug nil
  "If non-nil, display debug messages.")

(defun wallpaper-debug (&rest args)
  (when wallpaper-debug
    (apply #'message
           (concat "wallpaper-debug: " (car args))
           (cdr args))))

(cl-defmethod wallpaper--check-command ((_type (eql 'gsettings)))
  (equal (getenv "XDG_CURRENT_DESKTOP") "GNOME"))

(cl-defmethod wallpaper--check-command ((_type (eql 'swaybg)))
  (and (getenv "WAYLAND_DISPLAY")
       (getenv "SWAYSOCK")))

(cl-defmethod wallpaper--check-command (_type)
  t)

(defun wallpaper--find-command ()
  "Return a valid command for this system."
  (catch 'found
    (dolist (cmd wallpaper-commands)
      (if (and (wallpaper--check-command (intern (car cmd)))
               (executable-find (car cmd)))
          (throw 'found cmd)))))

(defun wallpaper-set (file)
  "Set the desktop background to FILE in a graphical environment."
  (interactive (list (and
                      (display-graphic-p)
                      (read-file-name "Set desktop background to: "
                                      default-directory nil t nil
                                      (lambda (fn)
                                        (or (file-directory-p fn)
                                            (string-match (image-file-name-regexp) fn)))))))
  (when (file-directory-p file)
    (error "Can't set wallpaper to a directory: %s" file))
  (unless (file-exists-p file)
    (error "No such file: %s" file))
  (unless (file-readable-p file)
    (error "File is not readable: %s" file))
  (when (display-graphic-p)
    (let* ((command (wallpaper--find-command))
           (fmt-spec `((?f . ,(expand-file-name file))
                       (?h . ,(display-pixel-height))
                       (?w . ,(display-pixel-width))))
           (bufname (format " *wallpaper-%s*" (random)))
           (process
            (and command
                 (apply #'start-process "set-wallpaper" bufname
                        (car command)
                        (mapcar (lambda (arg) (format-spec arg fmt-spec))
                                (cdr command))))))
      (unless command
        (error "Can't find a suitable command for setting the wallpaper"))
      (wallpaper-debug "Using command %s" (car command))
      (setf (process-sentinel process)
            (lambda (process status)
              (unwind-protect
                  (unless (and (eq (process-status process) 'exit)
                               (zerop (process-exit-status process)))
                    (message "command %S %s: %S" (string-join (process-command process) " ")
                             (string-replace "\n" "" status)
                             (with-current-buffer (process-buffer process)
                               (string-clean-whitespace (buffer-string)))))
                (ignore-errors
                  (kill-buffer (process-buffer process))))))
      process)))

(provide 'wallpaper)

;;; wallpaper.el ends here
