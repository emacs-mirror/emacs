;;; which-key.el --- Display available keybindings in popup

;; Copyright (C) 2015 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/justbur/which-key/
;; Version: 0.1
;; Keywords:
;; Package-Requires: ((emacs "24.3") (s "1.9.0"))

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

(require 'cl-macs)
(require 'cl-extra)
(require 's)

(defvar which-key-idle-delay 1
  "Delay (in seconds) for which-key buffer to popup.")
(defvar which-key-echo-keystrokes
  (min echo-keystrokes (/ (float which-key-idle-delay) 4))
  "Value to use for echo-keystrokes. This only applies when
`which-key-popup-type' is minibuffer. It needs to be less than
`which-key-idle-delay' or else the echo will erase the which-key
popup.")
(defvar which-key-max-description-length 27
  "Truncate the description of keys to this length.  Also adds
\"..\".")
(defvar which-key-separator "→"
  "Separator to use between key and description.")
(defvar which-key-key-replacement-alist
  '(("<\\(\\(C-\\|M-\\)*.+\\)>" . "\\1") ("\\(left\\)" ."←")
    ("\\(right\\)" . "→"))
    "The strings in the car of each cons cell are replaced with the
strings in the cdr for each key.")
(defvar which-key-description-replacement-alist
  '(("Prefix Command" . "prefix"))
  "See `which-key-key-replacement-alist'.  This is a list of cons
cells for replacing descriptions.")
(defvar which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL")
  "These keys will automatically be truncated to one character
and have `which-key-special-key-face' applied to them.")
(defvar which-key-buffer-name "*which-key*"
  "Name of which-key buffer.")
(defvar which-key-popup-type 'minibuffer
  "Supported types are minibuffer, side-window and frame.")
(defvar which-key-side-window-location 'right
  "Location of which-key popup when `which-key-popup-type' is
side-window.  Should be one of top, bottom, left or right.")
(defvar which-key-side-window-max-width 60
  "Maximum width of which-key popup when type is side-window and
location is left or right.")
(defvar which-key-side-window-max-height 20
  "Maximum height of which-key popup when type is side-window and
location is top or bottom.")
(defvar which-key-frame-max-width 60
  "Maximum width of which-key popup when type is frame.")
(defvar which-key-frame-max-height 20
  "Maximum height of which-key popup when type is frame.")

;; Faces
(defface which-key-key-face
  '((t . (:inherit font-lock-constant-face)))
  "Face for which-key keys")
(defface which-key-separator-face
  '((t . (:inherit font-lock-comment-face)))
  "Face for the separator (default separator is an arrow)")
(defface which-key-command-description-face
  '((t . (:inherit font-lock-function-name-face)))
  "Face for the key description when it is a command")
(defface which-key-group-description-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face for the key description when it is a group or prefix")
(defface which-key-special-key-face
  '((t . (:inherit which-key-key-face :inverse-video t :weight bold)))
  "Face for special keys (SPC, TAB, RET)")

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
    (setq-local cursor-type nil)
    (setq-local cursor-in-non-selected-windows nil))
  (setq which-key--setup-p t))

;; Timers

(defun which-key/start-open-timer ()
  "Activate idle timer."
  (which-key/stop-open-timer)           ; start over
  (setq which-key--open-timer
        (run-with-idle-timer which-key-idle-delay t 'which-key/update)))

(defun which-key/stop-open-timer ()
  "Deactivate idle timer."
  (when which-key--open-timer (cancel-timer which-key--open-timer)))

;; Update

(defun which-key/update ()
  "Fill which-key--buffer with key descriptions and reformat.
Finally, show the buffer."
  (let ((prefix-keys (this-single-command-keys)))
    (if (> (length prefix-keys) 0)
        (progn
          (let* ((buf (current-buffer))
                 ;; get formatted key bindings
                 (fmt-width-cons (which-key/get-formatted-key-bindings buf prefix-keys))
                 (formatted-keys (car fmt-width-cons))
                 (column-width (cdr fmt-width-cons))
                 ;; populate target buffer
                 (popup-act-dim
                  (which-key/populate-buffer (key-description prefix-keys)
                                             formatted-keys column-width (window-width))))
            ;; show buffer
            (which-key/show-popup popup-act-dim)))
      ;; command finished maybe close the window
      (which-key/hide-popup))))

;; Show/hide guide buffer

;; Should this be used instead?
;; (defun which-key/hide-buffer-display-buffer ()
;;   (when (window-live-p which-key--window)
;;     (delete-window which-key--window)))

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
  (cl-case which-key-popup-type
    (minibuffer (which-key/show-buffer-minibuffer act-popup-dim))
    (side-window (which-key/show-buffer-side-window act-popup-dim))
    (frame (which-key/show-buffer-frame act-popup-dim))))

(defun which-key/show-buffer-minibuffer (act-popup-dim)
  nil)

(defun which-key/show-buffer-side-window (act-popup-dim)
  (let* ((height (car act-popup-dim))
         (width (cdr act-popup-dim))
         (side which-key-side-window-location)
         (alist (delq nil (list (when height (cons 'window-height height))
                                (when width (cons 'window-width width))))))
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

;; Size functions

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
       (frame-height)
     ;; FIXME: change to something like (min which-*-height (calculate-max-height))
     which-key-side-window-max-height)
   ;; width
   (if (member which-key-side-window-location '(left right))
       which-key-side-window-max-width
     (frame-width))))

(defun which-key/frame-max-dimensions ()
  (cons which-key-frame-max-height which-key-frame-max-width))

;; Buffer contents functions

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
                    :test (lambda (x y) (string-equal (car x) (car y)))))
      (setq format-res (which-key/format-matches unformatted)
            formatted (car format-res)
            column-width (cdr format-res)))
    (cons formatted column-width)))

(defun which-key/create-page (prefix-len max-lines n-columns keys)
  "Format KEYS into string representing a single page of text.
N-COLUMNS is the number of text columns to use and MAX-LINES is
the maximum number of lines availabel in the target buffer."
  (let* ((n-keys (length keys))
         (n-lines (min (ceiling (/ (float n-keys) n-columns)) max-lines))
         (line-padding (s-repeat prefix-len " "))
         lines)
    (dotimes (i n-lines)
      (setq lines
            (push
             (cl-subseq keys (* i n-columns) (min n-keys (* (1+ i) n-columns)))
             lines)))
    (mapconcat (lambda (x) (apply 'concat x)) (reverse lines) (concat "\n" line-padding))))

(defun which-key/populate-buffer (prefix-keys formatted-keys column-width sel-win-width)
  "Insert FORMATTED-STRINGS into which-key buffer, breaking after BUFFER-WIDTH."
  (let* ((prefix-w-face (which-key/propertize-key prefix-keys))
         (prefix-len (+ 2 (length (substring-no-properties prefix-w-face))))
         (n-keys (length formatted-keys))
         (max-dims (which-key/popup-max-dimensions sel-win-width))
         (max-height (when (car max-dims) (car max-dims)))
         (max-width (when (cdr max-dims) (cdr max-dims)))
         ;; the 3 leaves room for the ... possibly on the first page
         (n-columns (/ (- max-width prefix-len 3) column-width)) ;; integer division
         (act-width (* n-columns column-width))
         ;; (avl-lines/page (which-key/available-lines))
         (max-keys/page (when max-height (* n-columns max-height)))
         (n-pages (if max-keys/page
                      (ceiling (/ (float n-keys) max-keys/page)) 1))
         pages act-height first-page)
    (when (and (> n-keys 0) (> n-columns 0))
      (dotimes (p n-pages)
        (setq pages
              (push (which-key/create-page prefix-len max-height n-columns
                                           (cl-subseq formatted-keys (* p max-keys/page)
                                                      (min (* (1+ p) max-keys/page) n-keys))) pages)))
      ;; not doing anything with other pages for now
      (setq pages (reverse pages)
            act-height (1+  (s-count-matches "\n" (car pages))))
      (setq first-page (concat prefix-w-face "  " (car pages)))
      (when (> (length pages) 1) (setq first-page (concat first-page "...")))
      (if (eq which-key-popup-type 'minibuffer)
          (let (message-log-max) (message "%s" first-page))
        (with-current-buffer which-key--buffer
          (erase-buffer)
          (insert first-page)
          (goto-char (point-min)))))
    (cons act-height act-width)))

(defun which-key/maybe-replace (string repl-alist &optional literal)
  "Perform replacements on STRING.
REPL-ALIST is an alist where the car of each element is the text
to replace and the cdr is the replacement text. Unless LITERAL is
non-nil regexp is used in the replacements."
  (let ((new-string string))
    (dolist (repl repl-alist)
      (setq new-string
            (if (string-match (car repl) new-string)
                (replace-match (cdr repl) t literal new-string)
              new-string)))
    new-string))

(defun which-key/propertize-key (key)
  (let ((key-w-face (propertize key 'face 'which-key-key-face)))
    (dolist (special-key which-key-special-keys)
      (when (string-match special-key key)
        (let ((beg (match-beginning 0)) (end (match-end 0)))
          (setq key-w-face
                (concat (substring key-w-face 0 beg)
                        (propertize (substring key-w-face beg (1+ beg))
                         'face 'which-key-special-key-face)
                        (when (< end (length key-w-face))
                          (substring key-w-face end (length key-w-face))))))))
    key-w-face))

(defsubst which-key/truncate-description (desc)
  "Truncate DESC description to `which-key-max-description-length'."
  (if (> (length desc) which-key-max-description-length)
      (concat (substring desc 0 which-key-max-description-length) "..")
    desc))

(defun which-key/format-matches (unformatted)
  "Turn each key-desc-cons in UNFORMATTED into formatted
strings (including text properties), and pad with spaces so that
all are a uniform length. MAX-LEN-KEY and MAX-LEN-DESC are the
longest key and description in the buffer, respectively.
Replacements are performed using the key and description
replacement alists."
  (let ((max-key-width 0)
        (max-desc-width 0)
        (sep-w-face (propertize which-key-separator 'face 'which-key-separator-face))
        (sep-width (length which-key-separator))
        after-replacements)
    ;; first replace and apply faces
    (setq after-replacements
          (mapcar
           (lambda (key-desc-cons)
             (let* ((key (which-key/maybe-replace
                          (car key-desc-cons) which-key-key-replacement-alist))
                    (desc (which-key/maybe-replace
                           (cdr key-desc-cons) which-key-description-replacement-alist))
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
                    (key-width (length (substring-no-properties key-w-face)))
                    (desc-width (length (substring-no-properties desc-w-face))))
               (setq max-key-width (max key-width max-key-width))
               (setq max-desc-width (max desc-width max-desc-width))
               (cons key-w-face desc-w-face)))
           unformatted))
    ;; pad to max key-width and max desc-width
    (cons
     (mapcar (lambda (x)
               (concat (s-pad-left max-key-width " " (car x))
                       " " sep-w-face " "
                       (s-pad-right max-desc-width " " (cdr x))
                       " "))
             after-replacements)
     (+ 3 max-key-width sep-width max-desc-width ))))

(provide 'which-key)

;;; which-key.el ends here
