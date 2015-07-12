;;; which-key.el --- Display available keybindings in popup

;; Copyright (C) 2015 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/justbur/which-key/
;; Version: 0.1
;; Keywords:
;; Package-Requires: ((emacs "24.3") (s "1.9.0") (dash "2.11.0"))

;;; Commentary:
;;
;;  This is a rewrite of guide-key https://github.com/kai2nenobu/guide-key
;;  with the following goals:
;;
;;    1. Remove polling function for performance reasons
;;    2. Try to simplify code as much as possible
;;    3. Switch away from using popwin (planned)
;;    4. Add replacement strings to create "aliases" for functions.
;;

;;; Code:

(require 'cl-lib)
(require 's)
(require 'dash)

(defgroup which-key nil "Customization options for which-key-mode")
(defcustom which-key-idle-delay 1.0
  "Delay (in seconds) for which-key buffer to popup."
  :group 'which-key
  :type 'float)
(defcustom which-key-echo-keystrokes
  (min echo-keystrokes (/ (float which-key-idle-delay) 4))
  "Value to use for echo-keystrokes. This only applies when
`which-key-popup-type' is minibuffer. It needs to be less than
`which-key-idle-delay' or else the echo will erase the which-key
popup."
  :group 'which-key
  :type 'float)
(defcustom which-key-max-description-length 27
  "Truncate the description of keys to this length.  Also adds
\"..\"."
  :group 'which-key
  :type 'integer)
(defcustom which-key-separator "→"
  "Separator to use between key and description."
  :group 'which-key
  :type 'string)
(defcustom which-key-unicode-correction 0
  "Correction for wide unicode characters. Set to a positive
number to adjust width of columns in case which-key is cutting
off text on the right side of the window."
  :group 'which-key
  :type 'integer)
(defcustom which-key-key-replacement-alist
  '(("<\\(\\(C-\\|M-\\)*.+\\)>" . "\\1") ("left" . "←") ("right" . "→"))
  "The strings in the car of each cons are replaced with the
strings in the cdr for each key. Elisp regexp can be used as
in the first example."
  :group 'which-key
  :type '(alist :key-type regexp :value-type string))
(defcustom which-key-description-replacement-alist
  '(("Prefix Command" . "prefix") (".+/\\(.+\\)" . "\\1")
    ("select-window-\\([1-9]\\)" . "Window \\1"))
  "See `which-key-key-replacement-alist'. This is a list of lists
for replacing descriptions. The second one removes \"namespace/\"
from \"namespace/function\". This is a convention for naming
functions but not a rule, so remove this replacement if it
becomes problematic."
  :group 'which-key
  :type '(alist :key-type regexp :value-type string))
(defcustom which-key-key-based-description-replacement-alist '()
  "Each item in the list is a cons cell. The car of each cons
cell is either a string like \"C-c\", in which case it's
interpreted as a key sequence or a value of `major-mode'. Here
are two examples:

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
(defcustom which-key-buffer-name "*which-key*"
  "Name of which-key buffer."
  :group 'which-key
  :type 'string)
(defcustom which-key-show-prefix 'left
  "Whether to and where to display the current prefix sequence.
Possible choices are left (the default), top and nil. Nil turns
the feature off."
  :group 'which-key
  :type '(radio (const :tag "Left of keys" left)
                (const :tag "In first line" top)
                (const  :tag "Hide" nil)))
(defcustom which-key-popup-type 'minibuffer
  "Supported types are minibuffer, side-window and frame."
  :group 'which-key
  :type '(radio (const :tag "Show in minibuffer" minibuffer)
                (const :tag "Show in side window" side-window)
                (const :tag "Show in popup frame" frame)))
(defcustom which-key-side-window-location 'right
  "Location of which-key popup when `which-key-popup-type' is
side-window.  Should be one of top, bottom, left or right."
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
(defcustom which-key-show-page-number t
  "Show page number and remaining keys in last slot, when keys
are hidden?"
  :group 'which-key
  :type '(radio (const :tag "Yes" t)
                (const :tag "No" nil)))

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

;; Internal Vars
;; (defvar popwin:popup-buffer nil)
(defvar which-key--buffer nil
  "Internal: Holds reference to which-key buffer.")
(defvar which-key--window nil
  "Internal: Holds reference to which-key window.")
(defvar which-key--open-timer nil
  "Internal: Holds reference to open window timer.")
(defvar which-key--setup-p nil
  "Internal: Non-nil if which-key buffer has been setup.")
(defvar which-key--frame nil
  "Internal: Holds reference to which-key frame.
Used when `which-key-popup-type' is frame.")
(defvar which-key--echo-keystrokes-backup echo-keystrokes
  "Internal: Backup the initial value of echo-keystrokes.")

;;;###autoload
(define-minor-mode which-key-mode
  "Toggle which-key-mode."
  :global t
  :lighter " WK"
  (if which-key-mode
      (progn
        (unless which-key--setup-p (which-key/setup))
        ;; reduce echo-keystrokes for minibuffer popup
        ;; (it can interfer if it's too slow)
        (when (and (> echo-keystrokes 0)
                   (eq which-key-popup-type 'minibuffer))
          (setq echo-keystrokes which-key-echo-keystrokes)
          (message "Which-key-mode enabled (note echo-keystrokes changed from %s to %s)"
                   which-key--echo-keystrokes-backup echo-keystrokes))
        (add-hook 'pre-command-hook #'which-key/hide-popup)
        (add-hook 'focus-out-hook #'which-key/stop-open-timer)
        (add-hook 'focus-in-hook #'which-key/start-open-timer)
        (which-key/start-open-timer))
    ;; make sure echo-keystrokes returns to original value
    (setq echo-keystrokes which-key--echo-keystrokes-backup)
    (remove-hook 'pre-command-hook #'which-key/hide-popup)
    (remove-hook 'focus-out-hook #'which-key/stop-open-timer)
    (remove-hook 'focus-in-hook #'which-key/start-open-timer)
    (which-key/stop-open-timer)))

(defun which-key/setup ()
  "Create buffer for which-key."
  (setq which-key--buffer (get-buffer-create which-key-buffer-name))
  (with-current-buffer which-key--buffer
    (toggle-truncate-lines 1)
    (setq-local cursor-type nil)
    (setq-local cursor-in-non-selected-windows nil)
    (setq-local mode-line-format nil))
  (setq which-key--setup-p t))

;; Default configuration functions for use by users. Should be the "best"
;; configurations

;;;###autoload
(defun which-key/setup-side-window-right ()
  "Apply suggested settings for side-window that opens on right."
  (interactive)
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'right
        which-key-show-prefix 'top))

;;;###autoload
(defun which-key/setup-side-window-bottom ()
  "Apply suggested settings for side-window that opens on
bottom."
  (interactive)
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-show-prefix nil))

;;;###autoload
(defun which-key/setup-minibuffer ()
  "Apply suggested settings for minibuffer."
  (interactive)
  (setq which-key-popup-type 'minibuffer
        which-key-show-prefix 'left))


;; Helper functions to modify replacement lists.

(defun which-key//add-key-based-replacements (alist key repl)
  "Internal function to add (KEY . REPL) to ALIST."
  (when (or (not (stringp key)) (not (stringp repl)))
    (error "KEY and REPL should be strings"))
  (when (assoc-string key alist)
    (message "which-key note: The key %s already exists in %s. This addition will override that replacement."))
  (setq alist (push (cons key repl) alist))
  alist)

(defun which-key/add-key-based-replacements (key repl &rest more)
  "Replace the description of a key sequence KEY (e.g., \"C-c
C-c\") with REPL. Both KEY and REPL should be strings. MORE
allows you to specifcy additional KEY REPL pairs. All
replacements are added to
`which-key-key-based-description-replacement-alist'."
  ;; TODO: Make interactive
  (while key
    (setq which-key-key-based-description-replacement-alist
          (which-key//add-key-based-replacements
           which-key-key-based-description-replacement-alist key repl))
    (setq key (pop more) repl (pop more))))

(defun which-key/add-major-mode-key-based-replacements (mode key repl &rest more)
  "Functions like `which-key/add-key-based-replacements' with the
exception that KEY and REPL (MORE contains addition KEY REPL
pairs) will only apply when the major-mode MODE is active."
  ;; TODO: Make interactive
  (when (not (symbolp mode))
    (error "MODE should be a symbol corresponding to a value of major-mode"))
  (let ((mode-alist (cdr (assq mode which-key-key-based-description-replacement-alist))))
    (while key
      (setq mode-alist (which-key//add-key-based-replacements
                        mode-alist key repl))
      (setq key (pop more) repl (pop more)))
    (setq which-key-key-based-description-replacement-alist
          (assq-delete-all mode which-key-key-based-description-replacement-alist)
          which-key-key-based-description-replacement-alist
          (push (cons mode mode-alist)
                which-key-key-based-description-replacement-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for computing window sizes

(defun which-key/text-width-to-total (text-width)
  "Convert window text-width to window total-width.
TEXT-WIDTH is the desired text width of the window. The function calculates what
total width is required for a window in the selected to have a text-width of
TEXT-WIDTH columns. The calculation considers possible fringes and scroll bars.
This function assumes that the desired window has the same character width as
the frame."
  (let ((char-width (frame-char-width)))
    (+ text-width
       (/ (frame-fringe-width) char-width)
       (/ (frame-scroll-bar-width) char-width)
       (if (which-key/char-enlarged-p) 1 0)
       ;; add padding to account for possible wide (unicode) characters
       3)))

(defun which-key/total-width-to-text (total-width)
  "Convert window total-width to window text-width.
TOTAL-WIDTH is the desired total width of the window. The function calculates
what text width fits such a window. The calculation considers possible fringes
and scroll bars. This function assumes that the desired window has the same
character width as the frame."
  (let ((char-width (frame-char-width)))
    (- total-width
       (/ (frame-fringe-width) char-width)
       (/ (frame-scroll-bar-width) char-width)
       (if (which-key/char-enlarged-p) 1 0)
       ;; add padding to account for possible wide (unicode) characters
       3)))

(defun which-key/char-enlarged-p (&optional frame)
  (> (frame-char-width) (/ (float (frame-pixel-width)) (window-total-width (frame-root-window)))))

(defun which-key/char-reduced-p (&optional frame)
  (< (frame-char-width) (/ (float (frame-pixel-width)) (window-total-width (frame-root-window)))))

(defun which-key/char-exact-p (&optional frame)
  (= (frame-char-width) (/ (float (frame-pixel-width)) (window-total-width (frame-root-window)))))

(defun which-key/width-or-percentage-to-width (width-or-percentage)
  "Return window total width.
If WIDTH-OR-PERCENTAGE is a whole number, return it unchanged. Otherwise, it
should be a percentage (a number between 0 and 1) out of the frame's width.
More precisely, it should be a percentage out of the frame's root window's
total width."
  (if (wholenump width-or-percentage)
      width-or-percentage
    (round (* width-or-percentage (window-total-width (frame-root-window))))))

(defun which-key/height-or-percentage-to-height (height-or-percentage)
  "Return window total height.
If HEIGHT-OR-PERCENTAGE is a whole number, return it unchanged. Otherwise, it
should be a percentage (a number between 0 and 1) out of the frame's height.
More precisely, it should be a percentage out of the frame's root window's
total height."
  (if (wholenump height-or-percentage)
      height-or-percentage
    (round (* height-or-percentage (window-total-height (frame-root-window))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show/hide guide buffer

(defun which-key/hide-popup ()
  (cl-case which-key-popup-type
    (minibuffer (which-key/hide-buffer-minibuffer))
    (side-window (which-key/hide-buffer-side-window))
    (frame (which-key/hide-buffer-frame))))

(defun which-key/hide-buffer-minibuffer ()
  nil)

(defun which-key/hide-buffer-side-window ()
  (when (buffer-live-p which-key--buffer)
    ;; in case which-key buffer was shown in an existing window, `quit-window'
    ;; will re-show the previous buffer, instead of closing the window
    (quit-windows-on which-key--buffer)))

(defun which-key/hide-buffer-frame ()
  (when (frame-live-p which-key--frame)
    (delete-frame which-key--frame)))

(defun which-key/show-popup (act-popup-dim)
  "Show guide window. ACT-POPUP-DIM includes the
dimensions, (height . width) of the buffer text to be displayed
in the popup.  Return nil if no window is shown, or if there is no
need to start the closing timer."
  (when (and (> (car act-popup-dim) 0) (> (cdr act-popup-dim) 0))
    (cl-case which-key-popup-type
      (minibuffer (which-key/show-buffer-minibuffer act-popup-dim))
      (side-window (which-key/show-buffer-side-window act-popup-dim))
      (frame (which-key/show-buffer-frame act-popup-dim)))))

(defun which-key/show-buffer-minibuffer (act-popup-dim)
  nil)

;; &rest params because `fit-buffer-to-window' has a different call signature
;; in different emacs versions
(defun which-key/fit-buffer-to-window-horizontally (&optional window &rest params)
  (let ((fit-window-to-buffer-horizontally t))
    (apply #'fit-window-to-buffer window params)))

(defun which-key/show-buffer-side-window (_act-popup-dim)
  (let* ((side which-key-side-window-location)
         (alist '((window-width . which-key/fit-buffer-to-window-horizontally)
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

(defun which-key/show-buffer-frame (act-popup-dim)
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
                         (which-key/show-buffer-reuse-frame frame-height frame-width)
                       (which-key/show-buffer-new-frame frame-height frame-width))))
    (when new-window
      ;; display successful
      (setq which-key--frame (window-frame new-window))
      new-window)))

(defun which-key/show-buffer-new-frame (frame-height frame-width)
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

(defun which-key/show-buffer-reuse-frame (frame-height frame-width)
  (let ((window
         (display-buffer-reuse-window which-key--buffer
                                      `((reusable-frames . ,which-key--frame)))))
    (when window
      ;; display successful
      (set-frame-size (window-frame window) frame-width frame-height)
      window)))

;; Keep for popwin maybe (Used to work)
;; (defun which-key/show-buffer-popwin (height width)
;;   "Using popwin popup buffer with dimensions HEIGHT and WIDTH."
;;   (popwin:popup-buffer which-key-buffer-name
;;                        :height height
;;                        :width width
;;                        :noselect t
;;                        :position which-key-side-window-location))

;; (defun which-key/hide-buffer-popwin ()
;;   "Hide popwin buffer."
;;   (when (eq popwin:popup-buffer (get-buffer which-key--buffer))
;;     (popwin:close-popup-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Max dimension of available window functions

(defun which-key/popup-max-dimensions (selected-window-width)
  "Dimesion functions should return the maximum possible (height . width)
of the intended popup."
  (cl-case which-key-popup-type
    (minibuffer (which-key/minibuffer-max-dimensions))
    (side-window (which-key/side-window-max-dimensions))
    (frame (which-key/frame-max-dimensions))))

(defun which-key/minibuffer-max-dimensions ()
  (cons
   ;; height
   (if (floatp max-mini-window-height)
       (floor (* (frame-text-lines)
                 max-mini-window-height))
     max-mini-window-height)
   ;; width
   (frame-text-cols)))

(defun which-key/side-window-max-dimensions ()
  (cons
   ;; height
   (if (member which-key-side-window-location '(left right))
       (- (frame-height) (window-text-height (minibuffer-window)) 1) ;; 1 is a kludge to make sure there is no overlap
     ;; (window-mode-line-height which-key--window))
     ;; FIXME: change to something like (min which-*-height (calculate-max-height))
     (which-key/height-or-percentage-to-height which-key-side-window-max-height))
   ;; width
   (if (member which-key-side-window-location '(left right))
       (which-key/total-width-to-text (which-key/width-or-percentage-to-width
                                       which-key-side-window-max-width))
     (frame-width))))

(defun which-key/frame-max-dimensions ()
  (cons which-key-frame-max-height which-key-frame-max-width))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for retrieving and formatting keys

(defun which-key/maybe-replace (string repl-alist &optional literal)
  "Perform replacements on STRING.
REPL-ALIST is an alist where the car of each element is the text
to replace and the cdr is the replacement text. Unless LITERAL is
non-nil regexp is used in the replacements."
  (save-match-data
    (let ((new-string string))
      (dolist (repl repl-alist)
        (when (string-match (car repl) new-string)
          (setq new-string
                (replace-match (cdr repl) t literal new-string))))
      new-string)))

(defun which-key/maybe-replace-key-based (string keys)
  (let* ((alist which-key-key-based-description-replacement-alist)
         (str-res (assoc-string keys alist))
         (mode-alist (assq major-mode alist))
         (mode-res (when mode-alist (assoc-string keys mode-alist))))
    (cond (mode-res (cdr mode-res))
          (str-res (cdr str-res))
          (t string))))

(defun which-key/propertize-key (key)
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
                    (substring key-w-face end (length key-w-face))))
        key-w-face))))

(defsubst which-key/truncate-description (desc)
  "Truncate DESC description to `which-key-max-description-length'."
  (if (> (length desc) which-key-max-description-length)
      (concat (substring desc 0 which-key-max-description-length) "..")
    desc))

(defun which-key/format-and-replace (unformatted prefix-keys)
  "Turn each key-desc-cons in UNFORMATTED into formatted
strings (including text properties), and pad with spaces so that
all are a uniform length. Replacements are performed using the
key and description replacement alists."
  (let ((max-key-width 0)
        (sep-w-face (propertize which-key-separator 'face 'which-key-separator-face))) ;(max-desc-width 0)
    ;; first replace and apply faces
    (mapcar
     (lambda (key-desc-cons)
       (let* ((key (car key-desc-cons))
              (desc (cdr key-desc-cons))
              (keys (concat prefix-keys " " key))
              (key (which-key/maybe-replace
                    key which-key-key-replacement-alist))
              (desc (which-key/maybe-replace
                     desc which-key-description-replacement-alist))
              (desc (which-key/maybe-replace-key-based desc keys))
              (group (string-match-p "^group:" desc))
              (desc (if group (substring desc 6) desc))
              (prefix (string-match-p "^Prefix" desc))
              (desc (if (or prefix group) (concat "+" desc) desc))
              (desc-face (if (or prefix group)
                             'which-key-group-description-face
                           'which-key-command-description-face))
              (desc (which-key/truncate-description desc))
              (key-w-face (which-key/propertize-key key))
              (desc-w-face (propertize desc 'face desc-face))
              (key-width (length (substring-no-properties key-w-face))))
         ;; (desc-width (length (substring-no-properties desc-w-face))))
         (setq max-key-width (max key-width max-key-width))
         ;; (setq max-desc-width (max desc-width max-desc-width))
         (list key-w-face sep-w-face desc-w-face)))
     unformatted)))
;; pad to max key-width and max desc-width

(defun which-key/get-formatted-key-bindings (buffer key)
  (let ((key-str-qt (regexp-quote (key-description key)))
        key-match desc-match unformatted format-res
        formatted column-width)
    (with-temp-buffer
      (describe-buffer-bindings buffer key)
      (goto-char (point-max)) ; want to put last keys in first
      (while (re-search-backward
              (format "^%s \\([^ \t]+\\)[ \t]+\\(\\(?:[^ \t\n]+ ?\\)+\\)$"
                      key-str-qt)
              nil t)
        (setq key-match (match-string 1)
              desc-match (match-string 2))
        (cl-pushnew (cons key-match desc-match) unformatted
                    :test (lambda (x y) (string-equal (car x) (car y))))))
    (which-key/format-and-replace unformatted (key-description key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for laying out which-key buffer pages

(defsubst which-key//max-len (keys index)
  (cl-reduce
   (lambda (x y) (max x (if (eq (car y) 'status)
                            0 (length (substring-no-properties (nth index y))))))
   keys :initial-value 0))

(defun which-key/create-page-vertical (keys max-lines max-width prefix-width)
  "Format KEYS into string representing a single page of text.
N-COLUMNS is the number of text columns to use and MAX-LINES is
the maximum number of lines availabel in the target buffer."
  (let* ((n-keys (length keys))
         (avl-lines max-lines)
         (avl-width (- (+ 1 max-width) prefix-width)); we get 1 back for not putting a space after the last column
         (rem-keys keys)
         (n-col-lines (min avl-lines n-keys))
         (act-n-lines n-col-lines) ; n-col-lines in first column
         (all-columns (list
                       (mapcar (lambda (i)
                                 (if (> i 1) (s-repeat prefix-width " ") ""))
                               (number-sequence 1 n-col-lines))))
         (act-width prefix-width)
         (max-iter 100)
         (iter-n 0)
         col-keys col-key-width col-desc-width col-width col-split done
         n-columns new-column page col-sep-width prev-rem-keys)
    (while (and (<= iter-n max-iter) (not done))
      (setq iter-n         (1+ iter-n)
            col-split      (-split-at n-col-lines rem-keys)
            col-keys       (car col-split)
            prev-rem-keys  rem-keys
            rem-keys       (cadr col-split)
            n-col-lines    (min avl-lines (length rem-keys))
            col-key-width  (which-key//max-len col-keys 0)
            col-sep-width  (which-key//max-len col-keys 1)
            col-desc-width (which-key//max-len col-keys 2)
            col-width      (+ 3 col-key-width col-sep-width col-desc-width)
            new-column     (mapcar
                            (lambda (k)
                              (if (eq (car k) 'status)
                                  (concat (s-repeat (+ col-key-width col-sep-width) " ") "  " (cdr k))
                                (concat (s-repeat (- col-key-width
                                                     (length (substring-no-properties (nth 0 k)))) " ")
                                        (nth 0 k) " " (nth 1 k) " " (nth 2 k)
                                        (s-repeat (- col-desc-width
                                                     (length (substring-no-properties (nth 2 k)))) " "))))
                            col-keys))
      (if (<= col-width avl-width)
          (setq all-columns (push new-column all-columns)
                act-width   (+ act-width col-width)
                avl-width   (- avl-width col-width)) 
        (setq done t
              rem-keys prev-rem-keys))
      (when (<= (length rem-keys) 0) (setq done t)))
    (setq all-columns (reverse all-columns)
          n-columns (length all-columns))
    (dotimes (i act-n-lines)
      (dotimes (j n-columns)
        (setq page (concat page (nth i (nth j all-columns))
                           (if (not (= j (- n-columns 1))) " "
                             (when (not (= i (- act-n-lines 1))) "\n"))))))
    (list page act-n-lines act-width rem-keys (- n-keys (length rem-keys)))))

(defun which-key/create-page (keys max-lines max-width prefix-width vertical use-status-key page-n)
  (let* ((n-keys (length keys))
         (first-try (which-key/create-page-vertical keys max-lines max-width prefix-width))
         (n-rem-keys (length (nth 3 first-try)))
         (status-key-i (- n-keys n-rem-keys 1))
         (next-try-lines max-lines)
         (iter-n 0)
         (max-iter (+ 1 max-lines))
         prev-try prev-n-rem-keys next-try found status-key)
    (cond ((and (> n-rem-keys 0) use-status-key)
           (setq status-key
                 (cons 'status (propertize
                                (format "%s keys not shown" (1+ n-rem-keys))
                                'face 'font-lock-comment-face)))
           (which-key/create-page-vertical (-insert-at status-key-i status-key keys)
                                           max-lines max-width prefix-width))
          ((or vertical (> n-rem-keys 0) (= 1 max-lines))
           first-try)
          ;; do a simple search for the smallest number of lines (TODO: Implement binary search)
          (t (while (and (<= iter-n max-iter) (not found))
               (setq iter-n (1+ iter-n)
                     prev-try next-try
                     next-try-lines (- next-try-lines 1)
                     next-try (which-key/create-page-vertical
                               keys next-try-lines max-width prefix-width)
                     n-rem-keys (length (nth 3 next-try))
                     found (or (= next-try-lines 0) (> n-rem-keys 0))))
             prev-try))))

(defun which-key/populate-buffer (prefix-keys formatted-keys sel-win-width)
  "Insert FORMATTED-STRINGS into which-key buffer, breaking after BUFFER-WIDTH."
  (let* ((vertical (and (eq which-key-popup-type 'side-window)
                        (member which-key-side-window-location '(left right))))
         (prefix-w-face (which-key/propertize-key prefix-keys))
         (prefix-len (+ 2 (length (substring-no-properties prefix-w-face))))
         (prefix-string (when which-key-show-prefix
                          (if (eq which-key-show-prefix 'left)
                              (concat prefix-w-face "  ")
                            (concat prefix-w-face "-\n"))))
         (max-dims (which-key/popup-max-dimensions sel-win-width))
         (max-lines (when (car max-dims) (car max-dims)))
         (prefix-width (if (eq which-key-show-prefix 'left) prefix-len 0))
         (avl-width (when (cdr max-dims) (- (cdr max-dims)
                                            prefix-width which-key-unicode-correction)))
         (keys-rem formatted-keys)
         (max-pages (+ 1 (length formatted-keys)))
         (page-n 0)
         keys-per-page pages first-page first-page-str page-res no-room
         max-pages-reached)
    (while (and keys-rem (not max-pages-reached) (not no-room))
      (setq page-n (1+ page-n)
            page-res (which-key/create-page keys-rem
                                            max-lines avl-width prefix-width
                                            vertical which-key-show-page-number page-n)
            pages (push page-res pages)
            keys-per-page (push (if (nth 4 page-res) (nth 4 page-res) 0) keys-per-page)
            keys-rem (nth 3 page-res)
            no-room (<= (car keys-per-page) 0)
            max-pages-reached (>= page-n max-pages)))
    ;; not doing anything with other pages for now
    (setq keys-per-page (reverse keys-per-page)
          pages (reverse pages)
          first-page (car pages)
          first-page-str (concat prefix-string (car first-page)))
    (cond (no-room
           (message "%s-  which-key can't show keys: Settings and/or frame size are too restrictive." prefix-keys)
           (cons 0 0))
          (max-pages-reached
           (error "error: which-key reached the maximum number of pages")
           (cons 0 0))
          ((<= (length formatted-keys) 0)
           (message "%s-  which-key: no keys to display" prefix-keys)
           (cons 0 0))
          (t
           (if (eq which-key-popup-type 'minibuffer)
               (let (message-log-max) (message "%s" first-page-str))
             (with-current-buffer which-key--buffer
               (erase-buffer)
               (insert first-page-str)
               (goto-char (point-min))))
           (cons (nth 1 first-page) (nth 2 first-page))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update

(defun which-key/update ()
  "Fill which-key--buffer with key descriptions and reformat.
Finally, show the buffer."
  (let ((prefix-keys (this-single-command-keys)))
    ;; (when (> (length prefix-keys) 0)
    ;;  (message "key: %s" (key-description prefix-keys)))
    ;; (when (> (length prefix-keys) 0)
    ;;  (message "key binding: %s" (key-binding prefix-keys)))
    (when (and (> (length prefix-keys) 0)
               (keymapp (key-binding prefix-keys)))
      (let* ((buf (current-buffer))
             ;; get formatted key bindings
             (formatted-keys (which-key/get-formatted-key-bindings
                              buf prefix-keys))
             ;; populate target buffer
             (popup-act-dim (which-key/populate-buffer
                             (key-description prefix-keys)
                             formatted-keys (window-width))))
        ;; show buffer
        (which-key/show-popup popup-act-dim)))))

;; Timers

(defun which-key/start-open-timer ()
  "Activate idle timer."
  (which-key/stop-open-timer) ; start over
  (setq which-key--open-timer
        (run-with-idle-timer which-key-idle-delay t 'which-key/update)))

(defun which-key/stop-open-timer ()
  "Deactivate idle timer."
  (when which-key--open-timer (cancel-timer which-key--open-timer)))
(provide 'which-key)

;;; which-key.el ends here
