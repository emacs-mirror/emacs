;;; wallpaper.el --- Change desktop background from Emacs  -*- lexical-binding: t; -*-

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

;; This library provides the command `wallpaper-set', which sets the
;; desktop background.
;;
;; On GNU/Linux and other Unix-like systems, it uses an external
;; command to set the desktop background.
;;
;; On Haiku, it uses the `haiku-set-wallpaper' function, which does
;; not rely on any external commands.
;;
;; Finding an external command to use is obviously a bit tricky to get
;; right, as there is no lack of platforms, window managers, desktop
;; environments and tools.  However, it should be detected
;; automatically in most cases.  If it doesn't work in your
;; environment, customize the user options `wallpaper-command' and
;; `wallpaper-command-args'.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'xdg)


;;; Finding the wallpaper command

(defvar wallpaper--default-commands
  ;; When updating this, also update the custom :type for `wallpaper-command'.
  '(
    ;; Sway (Wayland)
    ("swaybg" "-o" "*" "-i" "%f" "-m" "fill")
    ;; Wayland General
    ("wbg" "%f")
    ;; Gnome
    ("gsettings" "set" "org.gnome.desktop.background" "picture-uri" "file://%f")
    ;; KDE Plasma
    ("plasma-apply-wallpaperimage" "%f")
    ;; Other / General X
    ("gm" "display" "-size" "%wx%h" "-window" "root" "%f")
    ("display" "-resize" "%wx%h" "-window" "root" "%f")
    ("feh" "--bg-max" "%f")
    ("fbsetbg" "-a" "%f")
    ("xwallpaper" "--zoom" "%f")
    ("hsetroot" "-full" "%f")
    ("xloadimage" "-onroot" "-fullscreen" "%f")
    ("xsetbg" " %f")
    )
  "List of executables and options used for setting the wallpaper.
This is used by `wallpaper--find-command' to automatically set
`wallpaper-command', and by `wallpaper--find-command-args' to set
`wallpaper-command-args'.  The commands will be tested in the
order in which they appear.

Every item in the list has the following form:

  (COMMAND ARG1 .. ARGN)

COMMAND is the name of the executable (a string) and ARG1 .. ARGN
is its command line arguments (also strings).

In each of the command line arguments, \"%f\", \"%h\" and \"%w\"
will be replaced as described in `wallpaper-command-args'.")

(cl-defmethod wallpaper--check-command ((_type (eql 'gsettings)))
  (member "GNOME" (xdg-current-desktop)))

(cl-defmethod wallpaper--check-command ((_type (eql 'plasma-apply-wallpaperimage)))
  (member "KDE" (xdg-current-desktop)))

(cl-defmethod wallpaper--check-command ((_type (eql 'swaybg)))
  (and (getenv "WAYLAND_DISPLAY")
       (getenv "SWAYSOCK")))

(cl-defmethod wallpaper--check-command ((_type (eql 'wbg)))
  (getenv "WAYLAND_DISPLAY"))

(cl-defmethod wallpaper--check-command (_type)
  t)

(defun wallpaper--find-command ()
  "Return a valid command to set the wallpaper in this environment."
  (catch 'found
    (dolist (cmd wallpaper--default-commands)
      (if (and (wallpaper--check-command (intern (car cmd)))
               (executable-find (car cmd)))
          (throw 'found (car cmd))))))

(defvar wallpaper-command) ; silence byte-compiler
(defun wallpaper--find-command-arguments ()
  "Return command line arguments matching `wallpaper-command'."
  (cdr (assoc wallpaper-command wallpaper--default-commands)))


;;; Customizable variables

(defvar wallpaper-command-args) ; silence byte-compiler
(defun wallpaper--set-wallpaper-command (sym val)
  "Set `wallpaper-command', and update `wallpaper-command-args'.
Used to set `wallpaper-command'."
  ;; Note: `wallpaper-command' is used by `wallpaper--find-command-arguments'.
  (prog1 (set-default sym val)
    (set-default 'wallpaper-command-args
                 (wallpaper--find-command-arguments))))

(defcustom wallpaper-command (wallpaper--find-command)
  "Executable used for setting the wallpaper.
A suitable command for your environment should be detected
automatically, so there is usually no need to customize this.

If you set this to any supported command using customize or
`setopt', the user option `wallpaper-command-args' is
automatically updated to match.  If you need to change this to an
unsupported command, you will want to manually customize
`wallpaper-command-args' to match.

Note: If you find that you need to use a command in your
environment that is not automatically detected, we would love to
hear about it!  Please send an email to bug-gnu-emacs@gnu.org and
tell us the command (and all options) that worked for you.  You
can also use \\[report-emacs-bug].

The value of this variable is ignored on Haiku systems, where a
native API will be used instead (see `haiku-set-wallpaper')."
  :type
  '(choice
    (radio
     (const :tag "gsettings                   (GNOME)"            "gsettings")
     (const :tag "plasma-apply-wallpaperimage (KDE Plasma)"       "plasma-apply-wallpaperimage")
     (const :tag "swaybg                      (Wayland/Sway)"     "swaybg")
     (const :tag "wbg                         (Wayland)"          "wbg")
     (const :tag "gm                          (X Window System)"  "gm")
     (const :tag "display                     (X Window System)"  "display")
     (const :tag "feh                         (X Window System)"  "feh")
     (const :tag "fbsetbg                     (X Window System)"  "fbsetbg")
     (const :tag "xwallpaper                  (X Window System)"  "xwallpaper")
     (const :tag "hsetroot                    (X Window System)"  "hsetroot")
     (const :tag "xloadimage                  (X Window System)"  "xloadimage")
     (const :tag "xsetbg                      (X Window System)"  "xsetbg"))
    (const :tag "Other (specify)"         string))
  :set #'wallpaper--set-wallpaper-command
  :group 'image
  :version "29.1")

(defcustom wallpaper-command-args (wallpaper--find-command-arguments)
  "Command line arguments for `wallpaper-command'.
A suitable command for your environment should be detected
automatically, so there is usually no need to customize this.
However, if you do need to change this, you might also want to
customize `wallpaper-command' to match.

In each of the command line arguments, \"%f\" will be replaced
with the full file name, \"%h\" with the height of the selected
frame's display (as returned by `display-pixel-height'), and
\"%w\" with the width of the selected frame's display (as
returned by `display-pixel-width').

If `wallpaper-set' is run from a TTY frame, it will prompt for a
height and width for \"%h\" and \"%w\" instead.

The value of this variable is ignored on Haiku systems, where a
native API will be used instead (see `haiku-set-wallpaper')."
  :type '(repeat string)
  :group 'image
  :version "29.1")


;;; Utility functions

(defvar wallpaper-debug nil
  "If non-nil, display debug messages.")

(defun wallpaper-debug (&rest args)
  (when wallpaper-debug
    (apply #'message
           (concat "wallpaper-debug: " (car args))
           (cdr args))))


;;; wallpaper-set

(defvar wallpaper-default-width 1080
  "Default width used by `wallpaper-set'.
This is only used when it can't be detected automatically.
See also `wallpaper-default-height'.")

(defvar wallpaper-default-height 1920
  "Default height used by `wallpaper-set'.
This is only used when it can't be detected automatically.
See also `wallpaper-default-width'.")

(defun wallpaper--get-height-or-width (desc fun default)
  (if (display-graphic-p)
      (funcall fun)
    (read-number (format "Wallpaper %s in pixels: " desc) default)))

(declare-function haiku-set-wallpaper "term/haiku-win.el")

(defun wallpaper-set (file)
  "Set the desktop background to FILE in a graphical environment.

On GNU/Linux and other Unix-like systems, this relies on an
external command.  Which command to use is automatically detected
in most cases, but can be manually customized with the user
options `wallpaper-command' and `wallpaper-command-args'.

On Haiku, no external command is needed, so the value of
`wallpaper-commands' is ignored."
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
  (cond ((featurep 'haiku)
         (haiku-set-wallpaper file))
        (t
         (let* ((fmt-spec `((?f . ,(expand-file-name file))
                            (?h . ,(wallpaper--get-height-or-width
                                    "height"
                                    #'display-pixel-height
                                    wallpaper-default-height))
                            (?w . ,(wallpaper--get-height-or-width
                                    "width"
                                    #'display-pixel-width
                                    wallpaper-default-width))))
                (bufname (format " *wallpaper-%s*" (random)))
                (process
                 (and wallpaper-command
                      (apply #'start-process "set-wallpaper" bufname
                             wallpaper-command
                             (mapcar (lambda (arg) (format-spec arg fmt-spec))
                                     wallpaper-command-args)))))
           (unless wallpaper-command
             (error "Couldn't find a suitable command for setting the wallpaper"))
           (wallpaper-debug "Using command %S %S" wallpaper-command
                            wallpaper-command-args)
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
           process))))

(provide 'wallpaper)

;;; wallpaper.el ends here
