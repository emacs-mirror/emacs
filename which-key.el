;;; which-key.el --- Display available keybindings in popup

;; Copyright (C) 2015 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/justbur/emacs-which-key
;; Version: 0.2.1
;; Keywords:
;; Package-Requires: ((emacs "24.3") (s "1.9.0") (dash "2.11.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  This is a rewrite of guide-key https://github.com/kai2nenobu/guide-key. See
;;  https://github.com/justbur/emacs-which-key for more information.

;;; Code:

(require 'cl-lib)
(require 's)
(require 'dash)

(defgroup which-key nil
  "Customization options for which-key-mode"
  :group 'help
  :prefix "which-key-")

(defcustom which-key-idle-delay 1.0
  "Delay (in seconds) for which-key buffer to popup."
  :group 'which-key
  :type 'float)

(defcustom which-key-echo-keystrokes
  (min echo-keystrokes (/ (float which-key-idle-delay) 4))
  "Value to use for `echo-keystrokes'.
This only applies when `which-key-popup-type' is minibuffer.  It
needs to be less than `which-key-idle-delay' or else the echo
will erase the which-key popup."
  :group 'which-key
  :type 'float)

(defcustom which-key-max-description-length 27
  "Truncate the description of keys to this length.
Also adds \"..\"."
  :group 'which-key
  :type 'integer)

(defcustom which-key-separator "→"
  "Separator to use between key and description."
  :group 'which-key
  :type 'string)

(defcustom which-key-unicode-correction 3
  "Correction for wide unicode characters.
Since we measure width in terms of the number of characters,
Unicode characters that are wider than ASCII characters throw off
the calculation for available width in the which-key buffer.  This
variable allows you to adjust for the wide unicode characters by
artificially reducing the available width in the buffer.

The default of 3 means allow for the total extra width
contributed by any wide unicode characters to be up to one
additional ASCII character in the which-key buffer.  Increase this
number if you are seeing charaters get cutoff on the right side
of the which-key popup."
  :group 'which-key
  :type 'integer)

(defcustom which-key-key-replacement-alist
  '(("<\\(\\(C-\\|M-\\)*.+\\)>" . "\\1") ("left" . "←") ("right" . "→"))
  "The strings in the car of each cons are replaced with the
strings in the cdr for each key.  Elisp regexp can be used as
in the first example."
  :group 'which-key
  :type '(alist :key-type regexp :value-type string))

(defcustom which-key-description-replacement-alist
  '(("Prefix Command" . "prefix"))
  "See `which-key-key-replacement-alist'.
This is a list of lists for replacing descriptions."
  :group 'which-key
  :type '(alist :key-type regexp :value-type string))

(defcustom which-key-key-based-description-replacement-alist '()
  "Each item in the list is a cons cell.
The car of each cons cell is either a string like \"C-c\", in
which case it's interpreted as a key sequence or a value of
`major-mode'.  Here are two examples:

(\"SPC f f\" . \"find files\")
(emacs-lisp-mode . ((\"SPC m d\" . \"debug\")))

In the first case the description of the key sequence \"SPC f f\"
is overwritten with \"find files\". The second case works the
same way using the alist matched when `major-mode' is
emacs-lisp-mode."
:group 'which-key)

(defcustom which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL")
  "These keys will automatically be truncated to one character
and have `which-key-special-key-face' applied to them."
  :group 'which-key
  :type '(repeat string))

(defcustom which-key-buffer-name " *which-key*"
  "Name of which-key buffer."
  :group 'which-key
  :type 'string)

(defcustom which-key-show-prefix 'left
  "Whether to and where to display the current prefix sequence.
Possible choices are left (the default), top and nil.  Nil turns
the feature off."
  :group 'which-key
  :type '(radio (const :tag "Left of keys" left)
                (const :tag "In first line" top)
                (const :tag "Hide" nil)))

(defcustom which-key-popup-type 'minibuffer
  "Supported types are minibuffer, side-window, frame, and custom."
  :group 'which-key
  :type '(radio (const :tag "Show in minibuffer" minibuffer)
                (const :tag "Show in side window" side-window)
                (const :tag "Show in popup frame" frame)
                (const :tag "Use your custom display functions" custom)))

(defcustom which-key-side-window-location 'right
  "Location of which-key popup when `which-key-popup-type' is side-window.
Should be one of top, bottom, left or right."
  :group 'which-key
  :type '(radio (const right)
                (const bottom)
                (const left)
                (const top)))

(defcustom which-key-side-window-max-width 0.333
  "Maximum width of which-key popup when type is side-window and
location is left or right.
This variable can also be a number between 0 and 1. In that case, it denotes
a percentage out of the frame's width."
  :group 'which-key
  :type 'float)

(defcustom which-key-side-window-max-height 0.25
  "Maximum height of which-key popup when type is side-window and
location is top or bottom.
This variable can also be a number between 0 and 1. In that case, it denotes
a percentage out of the frame's height."
  :group 'which-key
  :type 'float)

(defcustom which-key-frame-max-width 60
  "Maximum width of which-key popup when type is frame."
  :group 'which-key
  :type 'integer)

(defcustom which-key-frame-max-height 20
  "Maximum height of which-key popup when type is frame."
  :group 'which-key
  :type 'integer)

(defcustom which-key-show-remaining-keys t
  "Show remaining keys in last slot, when keys are hidden."
  :group 'which-key
  :type '(radio (const :tag "Yes" t)
                (const :tag "No" nil)))

(defcustom which-key-sort nil
  "Sort output by `key-description' if non-nil."
  :group 'which-key
  :type 'boolean)

;; Faces
(defface which-key-key-face
  '((t . (:inherit font-lock-constant-face)))
  "Face for which-key keys"
  :group 'which-key)

(defface which-key-separator-face
  '((t . (:inherit font-lock-comment-face)))
  "Face for the separator (default separator is an arrow)"
  :group 'which-key)

(defface which-key-command-description-face
  '((t . (:inherit font-lock-function-name-face)))
  "Face for the key description when it is a command"
  :group 'which-key)

(defface which-key-group-description-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face for the key description when it is a group or prefix"
  :group 'which-key)

(defface which-key-special-key-face
  '((t . (:inherit which-key-key-face :inverse-video t :weight bold)))
  "Face for special keys (SPC, TAB, RET)"
  :group 'which-key)

;; Custom popup
(defcustom which-key-custom-popup-max-dimensions-function nil
  "Variable to hold a custom max-dimensions function.
Will be passed the width of the active window and is expected to
return the maximum height in lines and width in characters of the
which-key popup in the form a cons cell (height . width)."
  :group 'which-key
  :type 'function)

(defcustom which-key-custom-hide-popup-function nil
  "Variable to hold a custom hide-popup function.
It takes no arguments and the return value is ignored."
  :group 'which-key
  :type 'function)

(defcustom which-key-custom-show-popup-function nil
  "Variable to hold a custom show-popup function.
Will be passed the required dimensions in the form (height .
width) in lines and characters respectively.  The return value is
ignored."
  :group 'which-key
  :type 'function)

(defvar which-key-inhibit nil
  "Prevent which-key from popping up momentarily by setting this
to a non-nil value for the execution of a command. Like this

\(let \(\(which-key-inhibit t\)\)
...\)")

;; Internal Vars
(defvar which-key--buffer nil
  "Internal: Holds reference to which-key buffer.")
(defvar which-key--window nil
  "Internal: Holds reference to which-key window.")
(defvar which-key--open-timer nil
  "Internal: Holds reference to open window timer.")
(defvar which-key--is-setup nil
  "Internal: Non-nil if which-key buffer has been setup.")
(defvar which-key--frame nil
  "Internal: Holds reference to which-key frame.
Used when `which-key-popup-type' is frame.")
(defvar which-key--echo-keystrokes-backup nil
  "Internal: Backup the initial value of `echo-keystrokes'.")
(defvar which-key--pages-plist nil
  "Internal: Holds page objects")
(defvar which-key--lighter-backup nil
  "Internal: Holds lighter backup")

;;;###autoload
(define-minor-mode which-key-mode
  "Toggle which-key-mode."
  :global t
  :lighter " WK"
  (if which-key-mode
      (progn
        (unless which-key--is-setup (which-key--setup))
        (add-hook 'pre-command-hook #'which-key--hide-popup)
        (add-hook 'pre-command-hook #'which-key--lighter-restore)
        (add-hook 'focus-out-hook #'which-key--stop-open-timer)
        (add-hook 'focus-in-hook #'which-key--start-open-timer)
        (which-key--start-open-timer))
    ;; make sure echo-keystrokes returns to original value
    (setq echo-keystrokes which-key--echo-keystrokes-backup)
    (remove-hook 'pre-command-hook #'which-key--hide-popup)
    (remove-hook 'pre-command-hook #'which-key--lighter-restore)
    (remove-hook 'focus-out-hook #'which-key--stop-open-timer)
    (remove-hook 'focus-in-hook #'which-key--start-open-timer)
    (which-key--stop-open-timer)))

(defun which-key--setup ()
  "Initial setup for which-key.
Reduce `echo-keystrokes' if necessary (it will interfer if it's
set too high) and setup which-key buffer."
  (when (eq which-key-popup-type 'minibuffer)
    (which-key--setup-echo-keystrokes))
  (setq which-key--buffer (get-buffer-create which-key-buffer-name))
  (with-current-buffer which-key--buffer
    ;; suppress confusing minibuffer message
    (let (message-log-max)
      (toggle-truncate-lines 1)
      (message ""))
    (setq-local cursor-type nil)
    (setq-local cursor-in-non-selected-windows nil)
    (setq-local mode-line-format nil))
  (setq which-key--is-setup t))

(defun which-key--setup-echo-keystrokes ()
  "Reduce `echo-keystrokes' if necessary (it will interfer if
it's set too high)."
  (when (> (abs (- echo-keystrokes which-key-echo-keystrokes)) 0.000001)
    (setq which-key--echo-keystrokes-backup echo-keystrokes)
    (if (> which-key-idle-delay which-key-echo-keystrokes)
        (setq echo-keystrokes which-key-echo-keystrokes)
      (setq which-key-echo-keystrokes
            (min echo-keystrokes (/ (float which-key-idle-delay) 4))
            echo-keystrokes which-key-echo-keystrokes))
    (message "which-key: echo-keystrokes changed from %s to %s"
             which-key--echo-keystrokes-backup echo-keystrokes)))

;; Default configuration functions for use by users. Should be the "best"
;; configurations

;;;###autoload
(defun which-key-setup-side-window-right ()
  "Apply suggested settings for side-window that opens on right."
  (interactive)
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'right
        which-key-show-prefix 'top))

;;;###autoload
(defun which-key-setup-side-window-bottom ()
  "Apply suggested settings for side-window that opens on
bottom."
  (interactive)
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-show-prefix nil))

;;;###autoload
(defun which-key-setup-minibuffer ()
  "Apply suggested settings for minibuffer."
  (interactive)
  (which-key--setup-echo-keystrokes)
  (setq which-key-popup-type 'minibuffer
        which-key-show-prefix 'left))


;; Helper functions to modify replacement lists.

(defun which-key--add-key-based-replacements (alist key repl)
  "Internal function to add (KEY . REPL) to ALIST."
  (when (or (not (stringp key)) (not (stringp repl)))
    (error "KEY and REPL should be strings"))
  (cond ((null alist) (list (cons key repl)))
        ((assoc-string key alist)
         (message "which-key: the key %s already exists in %s. This addition \
will override that replacement."
                  key alist)
         (setcdr (assoc-string key alist) repl)
         alist)
        (t (cons (cons key repl) alist))))

;;;###autoload
(defun which-key-add-key-based-replacements (key-sequence replacement &rest more)
  "Replace the description of KEY-SEQUENCE with REPLACEMENT.
Both KEY-SEQUENCE and REPLACEMENT should be strings.  For Example,

\(which-key-add-key-based-replacements \"C-x 1\" \"maximize\"\)

MORE allows you to specifcy additional KEY REPL pairs.  All
replacements are added to
`which-key-key-based-description-replacement-alist'."
  ;; TODO: Make interactive
  (while key-sequence
    (setq which-key-key-based-description-replacement-alist
          (which-key--add-key-based-replacements
           which-key-key-based-description-replacement-alist
           key-sequence replacement))
    (setq key-sequence (pop more) replacement (pop more))))

;;;###autoload
(defun which-key-add-major-mode-key-based-replacements (mode key-sequence replacement &rest more)
  "Functions like `which-key-add-key-based-replacements'.
The difference is that MODE specifies the `major-mode' that must
be active for KEY-SEQUENCE and REPLACEMENT (MORE contains
addition KEY-SEQUENCE REPLACEMENT pairs) to apply."
  ;; TODO: Make interactive
  (when (not (symbolp mode))
    (error "MODE should be a symbol corresponding to a value of major-mode"))
  (let ((mode-alist (cdr (assq mode which-key-key-based-description-replacement-alist))))
    (while key-sequence
      (setq mode-alist (which-key--add-key-based-replacements mode-alist key-sequence replacement))
      (setq key-sequence (pop more) replacement (pop more)))
    (if (assq mode which-key-key-based-description-replacement-alist)
        (setcdr (assq mode which-key-key-based-description-replacement-alist) mode-alist)
      (push (cons mode mode-alist) which-key-key-based-description-replacement-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for computing window sizes

(defun which-key--text-width-to-total (text-width)
  "Convert window text-width to window total-width.
TEXT-WIDTH is the desired text width of the window.  The function
calculates what total width is required for a window in the
selected to have a text-width of TEXT-WIDTH columns.  The
calculation considers possible fringes and scroll bars.  This
function assumes that the desired window has the same character
width as the frame."
  (let ((char-width (frame-char-width)))
    (+ text-width
       (/ (frame-fringe-width) char-width)
       (/ (frame-scroll-bar-width) char-width)
       (if (which-key--char-enlarged-p) 1 0)
       ;; add padding to account for possible wide (unicode) characters
       3)))

(defun which-key--total-width-to-text (total-width)
  "Convert window total-width to window text-width.
TOTAL-WIDTH is the desired total width of the window.  The function calculates
what text width fits such a window.  The calculation considers possible fringes
and scroll bars.  This function assumes that the desired window has the same
character width as the frame."
  (let ((char-width (frame-char-width)))
    (- total-width
       (/ (frame-fringe-width) char-width)
       (/ (frame-scroll-bar-width) char-width)
       (if (which-key--char-enlarged-p) 1 0)
       ;; add padding to account for possible wide (unicode) characters
       3)))

(defun which-key--char-enlarged-p (&optional frame)
  (> (frame-char-width)
     (/ (float (frame-pixel-width)) (window-total-width (frame-root-window)))))

(defun which-key--char-reduced-p (&optional frame)
  (< (frame-char-width)
     (/ (float (frame-pixel-width)) (window-total-width (frame-root-window)))))

(defun which-key--char-exact-p (&optional frame)
  (= (frame-char-width)
     (/ (float (frame-pixel-width)) (window-total-width (frame-root-window)))))

(defun which-key--width-or-percentage-to-width (width-or-percentage)
  "Return window total width.
If WIDTH-OR-PERCENTAGE is a whole number, return it unchanged.  Otherwise, it
should be a percentage (a number between 0 and 1) out of the frame's width.
More precisely, it should be a percentage out of the frame's root window's
total width."
  (if (wholenump width-or-percentage)
      width-or-percentage
    (round (* width-or-percentage (window-total-width (frame-root-window))))))

(defun which-key--height-or-percentage-to-height (height-or-percentage)
  "Return window total height.
If HEIGHT-OR-PERCENTAGE is a whole number, return it unchanged.  Otherwise, it
should be a percentage (a number between 0 and 1) out of the frame's height.
More precisely, it should be a percentage out of the frame's root window's
total height."
  (if (wholenump height-or-percentage)
      height-or-percentage
    (round (* height-or-percentage (window-total-height (frame-root-window))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show/hide which-key buffer

(defun which-key--hide-popup ()
  "This function is called to hide the which-key buffer."
  (cl-case which-key-popup-type
    (minibuffer (which-key--hide-buffer-minibuffer))
    (side-window (which-key--hide-buffer-side-window))
    (frame (which-key--hide-buffer-frame))
    (custom (funcall which-key-custom-hide-popup-function))))

(defun which-key--hide-buffer-minibuffer ()
  "Does nothing.
Stub for consistency with other hide-buffer functions."
  nil)

(defun which-key--hide-buffer-side-window ()
  "Hide which-key buffer when side-window popup is used."
  (when (buffer-live-p which-key--buffer)
    ;; in case which-key buffer was shown in an existing window, `quit-window'
    ;; will re-show the previous buffer, instead of closing the window
    (quit-windows-on which-key--buffer)))

(defun which-key--hide-buffer-frame ()
  "Hide which-key buffer when frame popup is used."
  (when (frame-live-p which-key--frame)
    (delete-frame which-key--frame)))

(defun which-key--show-popup (act-popup-dim)
  "Show the which-key buffer.
ACT-POPUP-DIM includes the dimensions, (height . width) of the
buffer text to be displayed in the popup.  Return nil if no window
is shown, or if there is no need to start the closing timer."
  (when (and (> (car act-popup-dim) 0) (> (cdr act-popup-dim) 0))
    (cl-case which-key-popup-type
      (minibuffer (which-key--show-buffer-minibuffer act-popup-dim))
      (side-window (which-key--show-buffer-side-window act-popup-dim))
      (frame (which-key--show-buffer-frame act-popup-dim))
      (custom (funcall which-key-custom-show-popup-function act-popup-dim)))))

(defun which-key--show-buffer-minibuffer (act-popup-dim)
  "Does nothing.
Stub for consistency with other show-buffer functions."
  nil)

(defun which-key--fit-buffer-to-window-horizontally (&optional window &rest params)
  "Slightly modified version of `fit-buffer-to-window'.
Use &rest params because `fit-buffer-to-window' has a different
call signature in different emacs versions"
  (let ((fit-window-to-buffer-horizontally t))
    (apply #'fit-window-to-buffer window params)))

(defun which-key--show-buffer-side-window (_act-popup-dim)
  "Show which-key buffer when popup type is side-window."
  (let* ((side which-key-side-window-location)
         (alist '((window-width . which-key--fit-buffer-to-window-horizontally)
                  (window-height . fit-window-to-buffer))))
    ;; Note: `display-buffer-in-side-window' and `display-buffer-in-major-side-window'
    ;; were added in Emacs 24.3

    ;; If two side windows exist in the same side, `display-buffer-in-side-window'
    ;; will use on of them, which isn't desirable. `display-buffer-in-major-side-window'
    ;; will pop a new window, so we use that.
    ;; +-------------------------+         +-------------------------+
    ;; |     regular window      |         |     regular window      |
    ;; |                         |         +------------+------------+
    ;; +------------+------------+   -->   | side-win 1 | side-win 2 |
    ;; | side-win 1 | side-win 2 |         |------------+------------|
    ;; |            |            |         |     which-key window    |
    ;; +------------+------------+         +------------+------------+
    ;; (display-buffer which-key--buffer (cons 'display-buffer-in-side-window alist))
    ;; side defaults to bottom
    (if (get-buffer-window which-key--buffer)
        (display-buffer-reuse-window which-key--buffer alist)
      (display-buffer-in-major-side-window which-key--buffer side 0 alist))))

(defun which-key--show-buffer-frame (act-popup-dim)
  "Show which-key buffer when popup type is frame."
  (let* ((orig-window (selected-window))
         (frame-height (+ (car act-popup-dim)
                          (if (with-current-buffer which-key--buffer
                                mode-line-format)
                              1
                            0)))
         ;; without adding 2, frame sometimes isn't wide enough for the buffer.
         ;; this is probably because of the fringes. however, setting fringes
         ;; sizes to 0 (instead of adding 2) didn't always make the frame wide
         ;; enough. don't know why it is so.
         (frame-width (+ (cdr act-popup-dim) 2))
         (new-window (if (and (frame-live-p which-key--frame)
                              (eq which-key--buffer
                                  (window-buffer (frame-root-window which-key--frame))))
                         (which-key--show-buffer-reuse-frame frame-height frame-width)
                       (which-key--show-buffer-new-frame frame-height frame-width))))
    (when new-window
      ;; display successful
      (setq which-key--frame (window-frame new-window))
      new-window)))

(defun which-key--show-buffer-new-frame (frame-height frame-width)
  "Helper for `which-key--show-buffer-frame'."
  (let* ((frame-params `((height . ,frame-height)
                         (width . ,frame-width)
                         ;; tell the window manager to respect the given sizes
                         (user-size . t)
                         ;; which-key frame doesn't need a minibuffer
                         (minibuffer . nil)
                         (name . "which-key")
                         ;; no need for scroll bars in which-key frame
                         (vertical-scroll-bars . nil)
                         ;; (left-fringe . 0)
                         ;; (right-fringe . 0)
                         ;; (right-divider-width . 0)
                         ;; make sure frame is visible
                         (visibility . t)))
         (alist `((pop-up-frame-parameters . ,frame-params)))
         (orig-frame (selected-frame))
         (new-window (display-buffer-pop-up-frame which-key--buffer alist)))
    (when new-window
      ;; display successful
      (redirect-frame-focus (window-frame new-window) orig-frame)
      new-window)))

(defun which-key--show-buffer-reuse-frame (frame-height frame-width)
  "Helper for `which-key--show-buffer-frame'."
  (let ((window
         (display-buffer-reuse-window which-key--buffer
                                      `((reusable-frames . ,which-key--frame)))))
    (when window
      ;; display successful
      (set-frame-size (window-frame window) frame-width frame-height)
      window)))

;; Keep for popwin maybe (Used to work)
;; (defun which-key-show-buffer-popwin (height width)
;;   "Using popwin popup buffer with dimensions HEIGHT and WIDTH."
;;   (popwin:popup-buffer which-key-buffer-name
;;                        :height height
;;                        :width width
;;                        :noselect t
;;                        :position which-key-side-window-location))

;; (defun which-key-hide-buffer-popwin ()
;;   "Hide popwin buffer."
;;   (when (eq popwin:popup-buffer (get-buffer which-key--buffer))
;;     (popwin:close-popup-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Max dimension of available window functions

(defun which-key--popup-max-dimensions (selected-window-width)
  "Dimesion functions should return the maximum possible (height
. width) of the intended popup. SELECTED-WINDOW-WIDTH is the
width of currently active window, not the which-key buffer
window."
  (cl-case which-key-popup-type
    (minibuffer (which-key--minibuffer-max-dimensions))
    (side-window (which-key--side-window-max-dimensions))
    (frame (which-key--frame-max-dimensions))
    (custom (funcall which-key-custom-popup-max-dimensions-function selected-window-width))))

(defun which-key--minibuffer-max-dimensions ()
  "Return max-dimensions of minibuffer (height . width).
Measured in lines and characters respectively."
  (cons
   ;; height
   (if (floatp max-mini-window-height)
       (floor (* (frame-text-lines)
                 max-mini-window-height))
     max-mini-window-height)
   ;; width
   (frame-text-cols)))

(defun which-key--side-window-max-dimensions ()
  "Return max-dimensions of the side-window popup (height .
width) in lines and characters respectively."
  (cons
   ;; height
   (if (member which-key-side-window-location '(left right))
       ;; 1 is a kludge to make sure there is no overlap
       (- (frame-height) (window-text-height (minibuffer-window)) 1)
     ;; (window-mode-line-height which-key--window))
     ;; FIXME: change to something like (min which-*-height (calculate-max-height))
     (which-key--height-or-percentage-to-height which-key-side-window-max-height))
   ;; width
   (if (member which-key-side-window-location '(left right))
       (which-key--total-width-to-text (which-key--width-or-percentage-to-width
                                        which-key-side-window-max-width))
     (frame-width))))

(defun which-key--frame-max-dimensions ()
  "Return max-dimensions of the frame popup (height .
width) in lines and characters respectively."
  (cons which-key-frame-max-height which-key-frame-max-width))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for retrieving and formatting keys

(defun which-key--maybe-replace (string repl-alist &optional literal)
  "Perform replacements on STRING.
REPL-ALIST is an alist where the car of each element is the text
to replace and the cdr is the replacement text.  Unless LITERAL is
non-nil regexp is used in the replacements.  Whether or not a
replacement occurs return the new STRING."
  (save-match-data
    (let ((new-string string))
      (dolist (repl repl-alist)
        (when (string-match (car repl) new-string)
          (setq new-string
                (replace-match (cdr repl) t literal new-string))))
      new-string)))

(defun which-key--maybe-replace-key-based (string keys)
  "KEYS is a key sequence like \"C-c C-c\" and STRING is the
description that is possibly replaced using the
`which-key-key-based-description-replacement-alist'. Whether or
not a replacement occurs return the new STRING."
  (let* ((alist which-key-key-based-description-replacement-alist)
         (str-res (assoc-string keys alist))
         (mode-alist (assq major-mode alist))
         (mode-res (when mode-alist (assoc-string keys mode-alist))))
    (cond (mode-res (cdr mode-res))
          (str-res (cdr str-res))
          (t string))))

(defun which-key--propertize-key (key)
  "Add a face to KEY.
If KEY contains any \"special keys\" defined in
`which-key-special-keys' then truncate and add the corresponding
`which-key-special-key-face'."
  (let ((key-w-face (propertize key 'face 'which-key-key-face))
        (regexp (concat "\\("
                        (mapconcat 'identity which-key-special-keys
                                   "\\|") "\\)")))
    (save-match-data
      (if (string-match regexp key)
          (let ((beg (match-beginning 0)) (end (match-end 0)))
            (concat (substring key-w-face 0 beg)
                    (propertize (substring key-w-face beg (1+ beg))
                                'face 'which-key-special-key-face)
                    (substring key-w-face end (string-width key-w-face))))
        key-w-face))))

(defsubst which-key--truncate-description (desc)
  "Truncate DESC description to `which-key-max-description-length'."
  (if (> (string-width desc) which-key-max-description-length)
      (concat (substring desc 0 which-key-max-description-length) "..")
    desc))

(defsubst which-key--group-p (description)
  (or (string-match-p "^\\(group:\\|Prefix\\)" description)
      (keymapp (intern description))))

(defun which-key--propertize-description (description group)
  "Add face to DESCRIPTION where the face chosen depends on
whether the description represents a group or a command. Also
make some minor adjustments to the description string, like
removing a \"group:\" prefix."
  (let* ((desc description)
         (desc (if (string-match-p "^group:" desc)
                   (substring desc 6) desc))
         (desc (if group (concat "+" desc) desc))
         (desc (which-key--truncate-description desc)))
    (propertize desc 'face
                (if group
                    'which-key-group-description-face
                  'which-key-command-description-face))))

(defun which-key--format-and-replace (unformatted prefix-keys)
  "Take a list of (key . desc) cons cells in UNFORMATTED, add
faces and perform replacements according to the three replacement
alists. Returns a list (key separator description)."
  (let ((sep-w-face
         (propertize which-key-separator 'face 'which-key-separator-face)))
    (mapcar
     (lambda (key-desc-cons)
       (let* ((key (car key-desc-cons))
              (desc (cdr key-desc-cons))
              (group (which-key--group-p desc))
              (keys (concat prefix-keys " " key))
              (key (which-key--maybe-replace
                    key which-key-key-replacement-alist))
              (desc (which-key--maybe-replace
                     desc which-key-description-replacement-alist))
              (desc (which-key--maybe-replace-key-based desc keys))
              (key-w-face (which-key--propertize-key key))
              (desc-w-face (which-key--propertize-description desc group)))
         (list key-w-face sep-w-face desc-w-face)))
     unformatted)))

(defun which-key--key-description< (a b)
  "Order key descriptions A and B.
Order is lexicographic within a \"class\", where the classes and
the ordering of classes are listed below.

special (SPC,TAB,...) < single char < mod (C-,M-,...) < other."
  (let* ((aem? (string-equal a ""))
         (bem? (string-equal b ""))
         (a1? (= 1 (length a)))
         (b1? (= 1 (length b)))
         (srgxp "^\\(RET\\|SPC\\|TAB\\|DEL\\|LFD\\|ESC\\|NUL\\)")
         (asp? (string-match-p srgxp a))
         (bsp? (string-match-p srgxp b))
         (prrgxp "^\\(M\\|C\\|S\\|A\\|H\\|s\\)-")
         (apr? (string-match-p prrgxp a))
         (bpr? (string-match-p prrgxp b)))
    (cond ((or aem? bem?) (and aem? (not bem?)))
          ((and asp? bsp?)
           (if (string-equal (substring a 0 3) (substring b 0 3))
               (which-key--key-description< (substring a 3) (substring b 3))
             (string-lessp a b)))
          ((or asp? bsp?) asp?)
          ((and a1? b1?) (string-lessp a b))
          ((or a1? b1?) a1?)
          ((and apr? bpr?)
           (if (string-equal (substring a 0 2) (substring b 0 2))
               (which-key--key-description< (substring a 2) (substring b 2))
             (string-lessp a b)))
          ((or apr? bpr?) apr?)
          (t (string-lessp a b)))))

(defun which-key--get-formatted-key-bindings (buffer key-seq)
  "Uses `describe-buffer-bindings' to collect the key bindings in
BUFFER that follow the key sequence KEY-SEQ."
  (let ((key-str-qt (regexp-quote (key-description key-seq)))
        key-match desc-match unformatted format-res
        formatted column-width)
    (with-temp-buffer
      (describe-buffer-bindings buffer key-seq)
      (goto-char (point-max)) ; want to put last keys in first
      (while (re-search-backward
              (format "^%s \\([^ \t]+\\)[ \t]+\\(\\(?:[^ \t\n]+ ?\\)+\\)$"
                      key-str-qt)
              nil t)
        (setq key-match (match-string 1)
              desc-match (match-string 2))
        (cl-pushnew (cons key-match desc-match) unformatted
                    :test (lambda (x y) (string-equal (car x) (car y))))))
    (when which-key-sort
      (setq unformatted
            (sort unformatted
                  (lambda (a b) (which-key--key-description< (car a) (car b))))))
    (which-key--format-and-replace unformatted (key-description key-seq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for laying out which-key buffer pages

(defsubst which-key--join-columns (columns)
  "Transpose columns into rows, concat rows into lines and rows into page."
  (let* ((padded (apply (apply-partially #'-pad "") (reverse columns)))
         (rows (apply #'cl-mapcar #'list padded)))
    (mapconcat (lambda (row) (mapconcat #'identity row " ")) rows "\n")))

(defsubst which-key--max-len (keys index)
  "Internal function for finding the max length of the INDEX
element in each list element of KEYS."
  (cl-reduce
   (lambda (x y) (max x (string-width (nth index y)))) keys :initial-value 0))

(defun which-key--pad-column (col-keys)
  (let* ((col-key-width  (which-key--max-len col-keys 0))
         (col-sep-width  (which-key--max-len col-keys 1))
         (col-desc-width (which-key--max-len col-keys 2))
         (col-width      (+ 3 col-key-width col-sep-width col-desc-width)))
    (cons col-width
          (mapcar (lambda (k)
                    (concat
                     (s-repeat (- col-key-width (string-width (nth 0 k))) " ")
                     (nth 0 k) " " (nth 1 k) " " (nth 2 k)
                     (s-repeat (- col-desc-width (string-width (nth 2 k))) " ")))
                  col-keys))))

(defun which-key--partition-columns (keys avl-lines avl-width)
  (let ((cols-w-widths (mapcar #'which-key--pad-column
                               (-partition-all avl-lines keys)))
        (page-width 0) (n-pages 0)
        page-cols pages keys/page page-widths)
    (dolist (col cols-w-widths)
      (if (<= (+ (car col) page-width) avl-width)
          (progn (push (cdr col) page-cols)
                 (setq page-width (+ page-width (car col))))
        (when (> (length page-cols) 0)
          (push (which-key--join-columns page-cols) pages)
          (push (* (length page-cols) avl-lines) keys/page)
          (push page-width page-widths)
          (setq n-pages (1+ n-pages) page-cols '() page-width 0))))
    (when (> (length page-cols) 0)
      (push (which-key--join-columns page-cols) pages)
      (push (* (length page-cols) avl-lines) keys/page)
      (push page-width page-widths)
      (setq n-pages (1+ n-pages)))
    (list :pages (reverse pages) :page-height avl-lines
          :page-widths (reverse page-widths)
          :keys/page (reverse keys/page) :n-pages n-pages
          :tot-keys (cl-reduce '+ keys/page :initial-value 0))))

(defun which-key--create-pages (prefix-keys keys sel-win-width)
  (let* ((max-dims (which-key--popup-max-dimensions sel-win-width))
         (max-lines (car max-dims))
         (max-width (cdr max-dims))
         (prefix-w-face (which-key--propertize-key prefix-keys))
         (prefix-left (when (eq which-key-show-prefix 'left)
                        (+ 2 (string-width prefix-w-face))))
         (prefix-top (eq which-key-show-prefix 'top))
         (avl-lines (if prefix-top (- max-lines 1) max-lines))
         (avl-width (if prefix-left (- max-width prefix-left) max-width))
         (vertical (and (eq which-key-popup-type 'side-window)
                        (member which-key-side-window-location '(left right))))
         (result (which-key--partition-columns keys avl-lines avl-width))
         pages keys/page n-pages found prev-result)
    (cond ((or vertical (> (plist-get result :n-pages) 1) (= 1 avl-lines))
           result)
          ;; do a simple search for the smallest number of lines
          (t (while (and (> avl-lines 1) (not found))
               (setq avl-lines (- avl-lines 1)
                     prev-result result
                     result (which-key--partition-columns
                             keys avl-lines avl-width)
                     found (> (plist-get result :n-pages) 1)))
             (if (and (> avl-lines 1) found) prev-result result)))))

(defun which-key--lighter-status (n-shown n-tot)
  (when which-key-show-remaining-keys
    (setq which-key--lighter-backup (cdr (assq 'which-key-mode minor-mode-alist)))
    (setcar (cdr (assq 'which-key-mode minor-mode-alist))
            (format " WK: %s/%s keys" n-shown n-tot))))
(defun which-key--lighter-restore ()
  (when which-key-show-remaining-keys
    (setcar (cdr (assq 'which-key-mode minor-mode-alist)) which-key--lighter-backup)))

(defun which-key--show-page (n &optional prefix-keys)
  "Show page N, starting from 0.
PREFIX-KEYS holds the description of the prefix keys."
  (let ((n-pages (plist-get which-key--pages-plist :n-pages)))
    (if (= 0 n-pages)
        (if prefix-keys
            (message "%s-  which-key can't show keys: Settings and/or frame size are too restrictive."
                     prefix-keys)
          (message "which-key can't show keys: Settings and/or frame size are too restrictive."))
      (let* ((i (mod n n-pages))
             (page (nth i (plist-get which-key--pages-plist :pages)))
             (height (plist-get which-key--pages-plist :page-height))
             (width (nth i (plist-get which-key--pages-plist :page-widths)))
             (n-shown (nth i (plist-get which-key--pages-plist :keys/page)))
             (n-tot (plist-get which-key--pages-plist :tot-keys))
             (prefix-w-face (which-key--propertize-key prefix-keys))
             (prefix-width (string-width prefix-w-face))
             spaces)
        (cond ((eq which-key-show-prefix 'left)
               (setq spaces (s-repeat prefix-width " ")
                     page (concat
                           prefix-w-face "  "
                           (s-replace "\n" (concat "\n  " spaces) page))))
              ((eq which-key-show-prefix 'top)
               (setq page (concat prefix-w-face "-\n" page))))
        (which-key--lighter-status n-shown n-tot)
        (if (eq which-key-popup-type 'minibuffer)
            (let (message-log-max) (message "%s" page))
          (with-current-buffer which-key--buffer
            (erase-buffer)
            (insert page)
            (goto-char (point-max))))
        (which-key--show-popup (cons height width))))))

;; (setq map (make-sparse-keymap))
;; (define-key map (kbd "C-M-1") (lambda () (interactive) (which-key--show-page 0)))
;; (define-key map (kbd "C-M-2") (lambda () (interactive) (which-key--show-page 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update

(defun which-key--update ()
  "Fill `which-key--buffer' with key descriptions and reformat.
Finally, show the buffer."
  (let ((prefix-keys (this-single-command-keys)))
    ;; (when (> (length prefix-keys) 0)
    ;;  (message "key: %s" (key-description prefix-keys)))
    ;; (when (> (length prefix-keys) 0)
    ;;  (message "key binding: %s" (key-binding prefix-keys)))
    (when (and (> (length prefix-keys) 0)
               (or
                (keymapp (key-binding prefix-keys))
                ;; Some keymaps are stored here like iso-transl-ctl-x-8-map
                (keymapp (lookup-key key-translation-map prefix-keys))
                ;; just in case someone uses one of these
                (keymapp (lookup-key function-key-map prefix-keys)))
               (not which-key-inhibit))
      (let ((formatted-keys (which-key--get-formatted-key-bindings
                             (current-buffer) prefix-keys))
            (prefix-keys-desc (key-description prefix-keys)))
        (setq which-key--pages-plist (which-key--create-pages
                                      prefix-keys-desc formatted-keys
                                      (window-width)))
        (which-key--show-page 0 prefix-keys-desc)))))

;; Timers

(defun which-key--start-open-timer ()
  "Activate idle timer to trigger `which-key--update'."
  (which-key--stop-open-timer) ; start over
  (setq which-key--open-timer
        (run-with-idle-timer which-key-idle-delay t 'which-key--update)))

(defun which-key--stop-open-timer ()
  "Deactivate idle timer for `which-key--update'."
  (when which-key--open-timer (cancel-timer which-key--open-timer)))


;; TODO
;; fix status key

(provide 'which-key)
;;; which-key.el ends here
