;;; wallpaper.el --- Change the desktop background  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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
;; command to set the desktop background.  This should work seamlessly
;; on both X and Wayland.
;;
;; Finding an external command to use is obviously a bit tricky to get
;; right, as there is no lack of platforms, window managers, desktop
;; environments and tools.  However, it should be detected
;; automatically in most cases.  If it doesn't work in your
;; environment, customize the user options `wallpaper-command' and
;; `wallpaper-command-args'.
;;
;; On MS-Windows, it uses the `w32-set-wallpaper' function, and on
;; Haiku the `haiku-set-wallpaper' function, neither of which relies
;; on any external commands.  The value of `wallpaper-command' and
;; `wallpaper-command-args' are ignored on such systems.
;;
;; On macOS, the "osascript" command is used.  You might need to
;; disable the option "Change picture" in the "Desktop & Screensaver"
;; preferences for this to work (this was seen with macOS 10.13).
;; You might also have to tweak some permissions.
;;
;; Note: If you find that you need to use a command in your
;; environment that was not automatically detected, we would love to
;; hear about it!  Please send an email to bug-gnu-emacs@gnu.org and
;; tell us the command (and all options) that worked for you.  You can
;; also use `M-x report-emacs-bug'.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'xdg)
(eval-when-compile (require 'cl-lib))

(defvar wallpaper-debug nil
  "If non-nil, display debug messages.")

(defun wallpaper-debug (&rest args)
  (when wallpaper-debug
    (apply #'message
           (concat "wallpaper-debug: " (car args))
           (cdr args))))

(defvar wallpaper-set-function
  (cond ((fboundp 'w32-set-wallpaper)
         #'w32-set-wallpaper)
        ((and (fboundp 'haiku-set-wallpaper)
              (featurep 'haiku))
         'haiku-set-wallpaper)
        (#'wallpaper-default-set-function))
  "Function used by `wallpaper-set' to set the wallpaper.
The function takes one argument, FILE, which is the file name of
the image file to set the wallpaper to.")

(defun wallpaper--use-default-set-function-p ()
  (eq wallpaper-set-function #'wallpaper-default-set-function))


;;; Finding the wallpaper command

(cl-defstruct (wallpaper-setter
               ;; Get rid of the default constructor (`make-wallpaper-cmd').
               (:constructor nil)
               (:constructor
                wallpaper-setter-create
                ( name command args-raw
                  &rest rest-plist
                  &aux
                  (args (if (or (listp args-raw) (symbolp args-raw))
                            args-raw
                          (string-split args-raw)))
                  (predicate (plist-get rest-plist :predicate))
                  (init-action (plist-get rest-plist :init-action))
                  (detach (plist-get rest-plist :detach))))
               (:copier wallpaper-setter-copy))
  "Structure containing a method to set the wallpaper.

NAME is a description of the setter (e.g. the name of the Desktop
Environment).

COMMAND is the executable to run to set the wallpaper.

ARGS is the default list of command line arguments for COMMAND.

PREDICATE is a function that will be called without any arguments
and should return non-nil if this setter should be used.

INIT-ACTION is a function that will be called without any
arguments before trying to set the wallpaper.

DETACH, if non-nil, means that the wallpaper process should
continue running even after exiting Emacs."
  name
  command
  args
  (predicate #'always)
  init-action
  detach)

;;;###autoload
(put 'wallpaper-setter-create 'lisp-indent-function 1)

(defun wallpaper--init-action-kill (process-name)
  "Return kill function for `init-action' of a `wallpaper-setter' structure.
The returned function kills any process named PROCESS-NAME owned
by the current effective user id."
  (lambda ()
    (when-let* ((procs
                 (seq-filter (lambda (p) (let-alist p
                                           (and (= .euid (user-uid))
                                                (equal .comm process-name))))
                             (mapcar (lambda (pid)
                                       (cons (cons 'pid pid)
                                             (process-attributes pid)))
                                     (list-system-processes)))))
      (dolist (proc procs)
        (let-alist proc
          (when (y-or-n-p (format "Kill \"%s\" process with PID %d?" .comm .pid))
            (signal-process .pid 'TERM)))))))

(defmacro wallpaper--default-methods-create (&rest items)
  "Helper macro for defining `wallpaper--default-setters'."
  (cons 'list
        (mapcar
         (lambda (item)
           `(wallpaper-setter-create ,@item))
         items)))

(defvar wallpaper--default-setters
  (wallpaper--default-methods-create

   ;; macOS.
   ;; NB. Should come first to override everything else.
   ("macOS"
    "osascript"
    '("-e" "tell application \"Finder\" to set desktop picture to POSIX file \"%f\"")
    :predicate (lambda ()
                 (eq system-type 'darwin)))

   ;; Desktop environments.
   ("Gnome"
    "gsettings"
    "set org.gnome.desktop.background picture-uri file://%F"
    :predicate (lambda ()
                 (or (and (getenv "DESKTOP_SESSION")
                          (member (downcase (getenv "DESKTOP_SESSION"))
                                  '("gnome" "gnome" "gnome-wayland" "gnome-xorg"
                                    "unity" "ubuntu" "pantheon" "budgie-desktop"
                                    "pop")))
                     (member "GNOME" (xdg-current-desktop))
                     (member "Budgie" (xdg-current-desktop))
                     (member "GNOME-Classic" (xdg-current-desktop)))))

   ("KDE Plasma"
    "plasma-apply-wallpaperimage" "%f"
    :predicate (lambda ()
                 (member "KDE" (xdg-current-desktop))))

   ("XFCE"
    "xfconf-query" #'wallpaper-xfce-command-args
    :predicate (lambda ()
                 (or (and (getenv "DESKTOP_SESSION")
                          (member (downcase (getenv "DESKTOP_SESSION"))
                                  '("xubuntu" "ubuntustudio")))
                     (member "XFCE" (xdg-current-desktop)))))

   ("LXDE"
    "pcmanfm" "--set-wallpaper=%f"
    :predicate (lambda ()
                 (member "LXDE" (xdg-current-desktop))))

   ("LXQt"
    "pcmanfm-qt" "--set-wallpaper=%f" ; "--wallpaper-mode=MODE"
    :predicate (lambda ()
                 (or (member (and (getenv "DESKTOP_SESSION")
                                  (downcase (getenv "DESKTOP_SESSION")))
                             '("lubuntu" "lxqt"))
                     (member "LXQt" (xdg-current-desktop)))))

   ("Mate"
    "gsettings" "set org.mate.background picture-filename %f"
    :predicate (lambda ()
                 (or (and (getenv "DESKTOP_SESSION")
                          (equal "mate" (downcase (getenv "DESKTOP_SESSION"))))
                     (member "MATE" (xdg-current-desktop)))))

   ("Cinnamon"
    "gsettings" "set org.cinnamon.desktop.background picture-uri file://%F"
    :predicate (lambda ()
                 (or (equal "cinnamon" (and (getenv "DESKTOP_SESSION")
                                            (downcase (getenv "DESKTOP_SESSION"))))
                     (member "X-Cinnamon" (xdg-current-desktop)))))

   ("Deepin"
    "gsettings" "set com.deepin.wrap.gnome.desktop.background picture-uri file://%F"
    :predicate (lambda ()
                 (member "Deepin" (xdg-current-desktop))))

   ;; Wayland general.
   ("Sway (Wayland)"
    "swaybg" "-o * -i %f -m fill"
    :predicate (lambda ()
                 (and (getenv "WAYLAND_DISPLAY")
                      (getenv "SWAYSOCK")))
    :init-action (wallpaper--init-action-kill "swaybg")
    :detach t)

   ("wbg"
    "wbg" "%f"
    :predicate (lambda ()
                 (getenv "WAYLAND_DISPLAY"))
    :init-action (wallpaper--init-action-kill "wbg")
    :detach t)

   ;; X general.
   ("GraphicsMagick"
    "gm" "display -size %wx%h -window root %f")

   ("ImageMagick"
    "display" "-resize %wx%h -window root %f")

   ("feh"
    "feh" "--bg-max %f")

   ("fbsetbg"
    "fbsetbg" "-a %f")

   ("xwallpaper"
    "xwallpaper" "--zoom %f")

   ("hsetroot"
    "hsetroot" "-full %f")

   ("xloadimage"
    "xloadimage" "-onroot -fullscreen %f")

   ("xsetbg"
    "xsetbg" "%f")
   )
  "List of setters used for setting the wallpaper.
Every item in the list is a structure of type
`wallpaper-setter' (which see).

This is used by `wallpaper--find-command' to automatically set
`wallpaper-command', and by `wallpaper--find-command-args' to set
`wallpaper-command-args'.  The setters will be tested in the
order in which they appear.")

(defun wallpaper-xfce-command-args ()
  (let ((info
         (with-temp-buffer
           (call-process "xfconf-query" nil t nil
                         "-c" "xfce4-desktop"
                         "-p" "/backdrop/single-workspace-mode")
           (buffer-string))))
    (list "-c" "xfce4-desktop"
          "-p" (format "/backdrop/screen%%S/monitor%%M/workspace%s/last-image"
                       (if (equal info "true")
                           "0"
                         "%W"))
          "-s" "%f")))

(defvar wallpaper--current-setter nil)

(defun wallpaper--find-setter ()
  (when (wallpaper--use-default-set-function-p)
    (or (and (wallpaper-setter-p wallpaper--current-setter)
             wallpaper--current-setter)
        (setq wallpaper--current-setter
              (catch 'found
                (dolist (setter wallpaper--default-setters)
                  (wallpaper-debug "Testing setter %s" (wallpaper-setter-name setter))
                  (when (and (executable-find (wallpaper-setter-command setter))
                             (if-let* ((pred (wallpaper-setter-predicate setter)))
                                 (funcall pred)
                               t))
                    (wallpaper-debug "Found setter %s" (wallpaper-setter-name setter))
                    (throw 'found setter))))))))

(defun wallpaper--find-command ()
  "Return the appropriate command to set the wallpaper."
  (when-let* ((setter (wallpaper--find-setter)))
    (wallpaper-setter-command setter)))

(defun wallpaper--find-command-args ()
  "Return command line arguments matching `wallpaper-command'."
  (when-let* ((setter (wallpaper--find-setter)))
    (wallpaper-setter-args setter)))


;;; Customizable variables

(defvar wallpaper-command-args) ; silence byte-compiler
(defun wallpaper--set-wallpaper-command (sym val)
  "Set `wallpaper-command', and update `wallpaper-command-args'.
Used to set `wallpaper-command'."
  ;; Note: `wallpaper-command' is used by `wallpaper--find-command-args'.
  (prog1 (set-default sym val)
    (set-default 'wallpaper-command-args
                 (wallpaper--find-command-args))))

(defcustom wallpaper-command (wallpaper--find-command)
  "Executable used by `wallpaper-set' for setting the wallpaper.
A suitable command for your environment should be detected
automatically, so there is usually no need to customize this.

If you set this to any supported command using customize or
`setopt', the user option `wallpaper-command-args' is
automatically updated to match.  If you need to change this to an
unsupported command, you will want to manually customize
`wallpaper-command-args' to match.

The value of this variable is ignored on MS-Windows and Haiku
systems, where a native API is used instead."
  :type
  '(choice
    (radio
     (const :tag "gsettings                   (GNOME)"            "gsettings")
     (const :tag "plasma-apply-wallpaperimage (KDE Plasma)"       "plasma-apply-wallpaperimage")
     (const :tag "xfconf-query                (XFCE)"             "xfconf-query")
     (const :tag "pcmanf                      (LXDE)"             "pcmanf")
     (const :tag "pcmanf-qt                   (LXQt)"             "pcmanf-qt")
     (const :tag "swaybg                      (Wayland/Sway)"     "swaybg")
     (const :tag "wbg                         (Wayland)"          "wbg")
     (const :tag "gm                          (X Window System)"  "gm")
     (const :tag "display                     (X Window System)"  "display")
     (const :tag "feh                         (X Window System)"  "feh")
     (const :tag "fbsetbg                     (X Window System)"  "fbsetbg")
     (const :tag "xwallpaper                  (X Window System)"  "xwallpaper")
     (const :tag "hsetroot                    (X Window System)"  "hsetroot")
     (const :tag "xloadimage                  (X Window System)"  "xloadimage")
     (const :tag "xsetbg                      (X Window System)"  "xsetbg")
     (const :tag "osascript                   (macOS)"            "osascript"))
    (const :tag "Other (specify)"         string)
    (const :tag "None" nil))
  :set #'wallpaper--set-wallpaper-command
  :group 'image
  :version "29.1")

(defcustom wallpaper-command-args (wallpaper--find-command-args)
  "Command line arguments for `wallpaper-command'.
A suitable command for your environment should be detected
automatically, so there is usually no need to customize this.
However, if you do need to change this, you might also want to
customize `wallpaper-command' to match.

The value is a list of command list arguments to use, or a
function that returns a list of command line arguments.

In each command line argument, these specifiers will be replaced:

  %f   full file name
  %h   height of the selected frame's display (as returned
         by `display-pixel-height')
  %w   the width of the selected frame's display (as returned
         by `display-pixel-width').
  %F   full file name URI-encoded
  %S   current X screen (e.g. \"0\")
  %W   current workspace (e.g., \"0\")
  %M   name of the monitor (e.g., \"0\" or \"LVDS\")

If `wallpaper-set' is run from a TTY frame, instead prompt for a
height and width to use for %h and %w.

The value of this variable is ignored on MS-Windows and Haiku
systems, where a native API is used instead."
  :type '(choice (repeat string)
                 function)
  :group 'image
  :version "29.1")


;;; Utility functions

(defvar wallpaper-default-width 1080
  "Default width used by `wallpaper-set'.
This is only used when it can't be detected automatically.
See also `wallpaper-default-height'.")

(defvar wallpaper-default-height 1920
  "Default height used by `wallpaper-set'.
This is only used when it can't be detected automatically.
See also `wallpaper-default-width'.")

(defun wallpaper--get-height-or-width (desc fun default)
  (cond ((display-graphic-p) (funcall fun))
        (noninteractive default)
        ((read-number (format "Wallpaper %s in pixels: " desc) default))))

(autoload 'ffap-file-at-point "ffap")

(defvar wallpaper-image-file-extensions
  '("bmp" "gif" "heif" "jpeg" "jpg" "png" "tif" "tiff" "webp")
  "List of file extensions that `wallpaper-set' will consider for completion.")

(defun wallpaper--image-file-regexp ()
  (rx-to-string '(: "." (eval `(or ,@wallpaper-image-file-extensions)) eos) t))

(defun wallpaper--get-default-file ()
  (catch 'found
    (dolist (file (list buffer-file-name (ffap-file-at-point)))
      (when (and file (string-match (wallpaper--image-file-regexp) file))
        (throw 'found (abbreviate-file-name
                       (expand-file-name file)))))))


;;; wallpaper-set

(declare-function x-open-connection "xfns.c")

(defun wallpaper--x-monitor-name ()
  "Get the monitor name for `wallpaper-set'.
On a graphical display, try using the same monitor as the current
frame.
On a non-graphical display, try to get the name by connecting to
the display server directly, or run \"xrandr\" if that doesn't
work.  Prompt for the monitor name if neither method works.

This function is meaningful only on X and is used only there."
  (if (or (display-graphic-p)
          noninteractive)
      (let-alist (car (display-monitor-attributes-list))
        (if (and .name (member .source '("XRandr" "XRandR 1.5" "Gdk")))
            .name
          "0"))
    (if-let* ((name
               (and (getenv "DISPLAY")
                    (or
                     (cdr (assq 'name
                                (progn
                                  (x-open-connection (getenv "DISPLAY"))
                                  (car (display-monitor-attributes-list
                                        (car (last (terminal-list))))))))
                     (and (executable-find "xrandr")
                          (with-temp-buffer
                            (call-process "xrandr" nil t nil)
                            (goto-char (point-min))
                            (re-search-forward (rx bol
                                                   (group (+ (not (in " \n"))))
                                                   " connected")
                                               nil t)
                            (match-string 1)))))))
        ;; Prefer "0" to "default" as that works in XFCE.
        (if (equal name "default") "0" name)
      (read-string (format-prompt "Monitor name" nil)))))

(defun wallpaper--format-arg (format file)
  "Format a `wallpaper-command-args' argument ARG using FORMAT.
FILE is the image file name."
  (format-spec
   format
   `((?f . ,(expand-file-name file))
     (?F . ,(lambda ()
              (mapconcat #'url-hexify-string
                         (file-name-split file)
                         "/")))
     (?h . ,(lambda ()
              (wallpaper--get-height-or-width
               "height"
               #'display-pixel-height
               wallpaper-default-height)))
     (?w . ,(lambda ()
              (wallpaper--get-height-or-width
               "width"
               #'display-pixel-width
               wallpaper-default-width)))
     ;; screen number
     (?S . ,(lambda ()
              (let ((display (frame-parameter (selected-frame) 'display)))
                (if (and display
                         (string-match (rx ":" (+ (in "0-9")) "."
                                           (group (+ (in "0-9"))) eos)
                                       display))
                    (match-string 1 display)
                  "0"))))
     ;; monitor name
     (?M . ,#'wallpaper--x-monitor-name)
     ;; workspace
     (?W . ,(lambda ()
              (or (and (fboundp 'x-window-property)
                       (display-graphic-p)
                       (number-to-string
                        (or (x-window-property "_NET_CURRENT_DESKTOP" nil "CARDINAL" 0 nil t)
                            (x-window-property "WIN_WORKSPACE" nil "CARDINAL" 0 nil t)
                            0)))
                  "0"))))))

(defun wallpaper-default-set-function (file)
  "Set the wallpaper to FILE using a command.
This is the default function for `wallpaper-set-function'."
  (unless wallpaper-command
    (error "Couldn't find a command to set the wallpaper with"))
  (let* ((args (if (functionp wallpaper-command-args)
                   (funcall wallpaper-command-args)
                 wallpaper-command-args))
         (real-args (mapcar (lambda (arg) (wallpaper--format-arg arg file))
                            args))
         (bufname (format " *wallpaper-%s*" (random)))
         (setter (and (wallpaper-setter-p wallpaper--current-setter)
                      (equal (wallpaper-setter-command wallpaper--current-setter)
                             wallpaper-command)
                      wallpaper--current-setter))
         (init-action (and setter (wallpaper-setter-init-action setter)))
         (detach (and setter (wallpaper-setter-detach setter)))
         process)
    (when init-action
      (funcall init-action))
    (wallpaper-debug "Using command: \"%s %s\""
                     wallpaper-command (string-join real-args " "))
    (if detach
        (apply #'call-process wallpaper-command nil 0 nil real-args)
      (setq process
            (apply #'start-process "set-wallpaper" bufname
                   wallpaper-command real-args))
      (setf (process-sentinel process)
            (lambda (process status)
              (unwind-protect
                  (if (and (eq (process-status process) 'exit)
                           (zerop (process-exit-status process)))
                      (message "Desktop wallpaper changed to %s"
                               (abbreviate-file-name file))
                    (message "command \"%s %s\": %S"
                             (string-join (process-command process) " ")
                             (string-replace "\n" "" status)
                             (with-current-buffer (process-buffer process)
                               (string-clean-whitespace (buffer-string)))))
                (ignore-errors
                  (kill-buffer (process-buffer process)))))))
    process))

;;;###autoload
(defun wallpaper-set (file)
  "Set the desktop background to FILE in a graphical environment.

On GNU/Linux and other Unix-like systems, this relies on an
external command.  Which command to use is automatically detected
in most cases, but can be manually customized with the user
options `wallpaper-command' and `wallpaper-command-args'.

On MS-Windows and Haiku systems, no external command is needed,
so the value of `wallpaper-commands' is ignored."
  (interactive
   (let ((default (wallpaper--get-default-file)))
     (list (read-file-name (format-prompt "Set desktop background to" default)
                           default-directory default
                           t nil
                           (let ((re (wallpaper--image-file-regexp)))
                             (lambda (file-name)
                               (or (file-directory-p file-name)
                                   (string-match re file-name))))))))
  (when (file-directory-p file)
    (error "Can't set wallpaper to a directory: %s" file))
  (unless (file-exists-p file)
    (error "No such file: %s" file))
  (unless (file-readable-p file)
    (error "File is not readable: %s" file))
  (wallpaper-debug "Using image %S:" file)
  (funcall wallpaper-set-function file))

(provide 'wallpaper)

;;; wallpaper.el ends here
