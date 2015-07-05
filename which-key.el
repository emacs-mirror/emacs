;;; which-key.el --- Display available keybindings in popup

;; Copyright (C) 2015 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/justbur/which-key/
;; Version: 0.1
;; Keywords:
;; Package-Requires: ((s "1.9.0") (popwin "1.0.0"))

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

(defvar which-key-idle-delay 0.6
  "Delay (in seconds) for which-key buffer to popup.")
(defvar which-key-close-buffer-idle-delay 4
  "Delay (in seconds) after which buffer is forced closed.")
(defvar which-key-max-description-length 27
  "Truncate the description of keys to this length.  Also adds
\"..\".")
(defvar which-key-key-replacement-alist
  '((">". "") ("<" . "") ("left" ."←") ("right" . "→"))
  "The strings in the car of each cons cell are replaced with the
strings in the cdr for each key.")
(defvar which-key-general-replacement-alist
  '(("Prefix Command" . "prefix"))
  "See `which-key-key-replacement-alist'.  This is a list of cons
cells for replacing any text, keys and descriptions.")
(defvar which-key-buffer-name "*which-key*"
  "Name of which-key buffer.")
(defvar which-key-buffer-position 'bottom
  "Position of which-key buffer.")
(defvar which-key-vertical-buffer-width 60
  "Width of which-key buffer.")
(defvar which-key-horizontal-buffer-height 20
  "Height of which-key buffer.")
(defvar which-key-display-method 'minibuffer
  "Controls the method used to display the keys. The default is
minibuffer, but other possibilities are 'popwin and
'display-buffer. You will also be able write your own display
function (not implemented yet).")

(defconst which-key-buffer-display-function
  'display-buffer-in-side-window
  "Controls where the buffer is displayed.  The current default is
also controlled by `which-key-buffer-position'.  Other options are
currently disabled.")

;; Internal Vars
(defvar popwin:popup-buffer nil)
(defvar which-key--buffer nil
  "Internal: Holds reference to which-key buffer.")
(defvar which-key--window nil
  "Internal: Holds reference to which-key window.")
(defvar which-key--open-timer nil
  "Internal: Holds reference to open window timer.")
(defvar which-key--close-timer nil
  "Internal: Holds reference to close window timer.")
(defvar which-key--setup-p nil
  "Internal: Non-nil if which-key buffer has been setup.")

;;;###autoload
(define-minor-mode which-key-mode
  "Toggle which-key-mode."
  :global t
  :lighter " WK"
  (if which-key-mode
      (progn
        (unless which-key--setup-p (which-key/setup))
        (add-hook 'focus-out-hook 'which-key/stop-open-timer)
        (add-hook 'focus-in-hook 'which-key/start-open-timer)
        (which-key/start-open-timer))
    (remove-hook 'focus-out-hook 'which-key/stop-open-timer)
    (remove-hook 'focus-in-hook 'which-key/start-open-timer)
    (which-key/stop-open-timer)))

(defun which-key/setup ()
  "Create buffer for which-key."
  (require 's)
  (require 'popwin)
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

(defun which-key/start-close-timer ()
  "Activate idle timer."
  (which-key/stop-close-timer)          ; start over
  (setq which-key--close-timer
        (run-at-time which-key-close-buffer-idle-delay
                     nil 'which-key/hide-buffer)))

(defun which-key/stop-close-timer ()
  "Deactivate idle timer."
  (when which-key--close-timer (cancel-timer which-key--close-timer)))

;; Update

(defun which-key/update ()
  "Fill which-key--buffer with key descriptions and reformat.
Finally, show the buffer."
  (let ((key (this-single-command-keys)))
    (if (> (length key) 0)
        (progn
          (which-key/stop-close-timer)
          (which-key/hide-buffer)
          (let* ((buf (current-buffer))
                 ;; (bottom-or-top (member which-key-buffer-position '(top bottom)))
                 ;; get formatted key bindings
                 (fmt-width-cons (which-key/get-formatted-key-bindings buf key))
                 (formatted-keys (car fmt-width-cons))
                 (column-width (cdr fmt-width-cons))
                 (buffer-width (which-key/buffer-width column-width (window-width)))
                 ;; populate target buffer
                 (n-lines (which-key/populate-buffer formatted-keys column-width buffer-width)))
            ;; show buffer
            (when (which-key/show-buffer n-lines buffer-width)
              (which-key/start-close-timer))))
      ;; command finished maybe close the window
      (which-key/hide-buffer))))

;; Show/hide guide buffer

(defun which-key/hide-buffer ()
  (when (buffer-live-p which-key--buffer)
    (delete-windows-on which-key--buffer)))

(defun which-key/show-buffer (height width)
  "Show guide window.
Return nil if no window is shown, or if there is no need to start the
closing timer."
  (cl-case which-key-display-method
    (minibuffer (which-key/show-buffer-minibuf height width))
    (side-window (which-key/show-buffer-db height width))))

(defun which-key/show-buffer-minibuf (height width)
  nil)

(defun which-key/show-buffer-db (height width)
  (let* ((side which-key-buffer-position)
         (alist (delq nil (list (when side (cons 'side side))
                                (when height (cons 'window-height height))
                                (when width (cons 'window-width width))))))
    (display-buffer which-key--buffer (cons 'display-buffer-in-side-window alist))))

;; Size functions

(defun which-key/buffer-width (column-width sel-window-width)
  (cl-case which-key-display-method
    (minibuffer (which-key/buffer-width-minibuf column-width sel-window-width))
    (side-window (which-key/buffer-width-db column-width sel-window-width))))

(defun which-key/buffer-width-minibuf (column-width sel-window-width)
  (frame-text-cols))

(defun which-key/buffer-width-db (column-width sel-window-width)
  (if (member which-key-buffer-position '(left right))
      (min which-key-vertical-buffer-width column-width)
    (frame-width)))

(defun which-key/available-lines ()
  (cl-case which-key-display-method
    (minibuffer (which-key/available-lines-minibuf))
    (side-window (which-key/available-lines-db))))

(defun which-key/available-lines-minibuf ()
  "Only works for minibuffer right now."
  (if (floatp max-mini-window-height)
      (floor (* (frame-text-lines)
                max-mini-window-height))
    max-mini-window-height))

(defun which-key/available-lines-db ()
  (if (member which-key-buffer-position '(left right))
      (frame-height)
    ;; FIXME: change to something like (min which-*-height (calculate-max-height))
    which-key-horizontal-buffer-height))

;; Buffer contents functions

(defun which-key/get-formatted-key-bindings (buffer key)
  (let ((max-len-key 0) (max-len-desc 0)
        (key-str-qt (regexp-quote (key-description key)))
        key-match desc-match unformatted formatted)
    (with-temp-buffer
      (describe-buffer-bindings buffer key)
      (which-key/replace-strings-from-alist which-key-general-replacement-alist)
      (goto-char (point-max)) ; want to put last keys in first
      (while (re-search-backward
              (format "^%s \\([^ \t]+\\)[ \t]+\\(\\(?:[^ \t\n]+ ?\\)+\\)$"
                      key-str-qt)
              nil t)
        (setq key-match (s-replace-all
                         which-key-key-replacement-alist (match-string 1))
              desc-match (match-string 2)
              max-len-key (max max-len-key (length key-match))
              max-len-desc (max max-len-desc (length desc-match)))
        (cl-pushnew (cons key-match desc-match) unformatted
                    :test (lambda (x y) (string-equal (car x) (car y)))))
      (setq max-len-desc (if (> max-len-desc which-key-max-description-length)
                             (+ 2 which-key-max-description-length) ; for the ..
                           max-len-desc)
            formatted (which-key/format-matches
                       unformatted max-len-key max-len-desc)))
    (cons formatted (+ 4 max-len-key max-len-desc))))

(defun which-key/populate-buffer (formatted-keys column-width buffer-width)
  "Insert FORMATTED-STRINGS into buffer, breaking after BUFFER-WIDTH."
  (let* ((char-count 0) (line-breaks 0) (this-column 1)
         (width (if buffer-width buffer-width (frame-text-width)))
         (n-keys (length formatted-keys))
         (n-columns (/ width column-width)) ;; integer division
         (n-lines (which-key/available-lines))
         (max-lines (ceiling (/ (float n-keys) n-columns)))
         (n-lines (if n-lines (min n-lines max-lines) max-lines))
         lines str-to-insert start end)
    (when (> n-columns 0)
      (dotimes (i n-lines)
        (setq lines
              (push (subseq formatted-keys (* i n-columns) (min n-keys (* (1+ i) n-columns)))
                    lines)))
      (setq str-to-insert (mapconcat (lambda (x) (apply 'concat x)) (reverse lines) "\n"))
      (if (eq which-key-display-method 'minibuffer)
          (let (message-log-max) (message "%s" str-to-insert))
        (with-current-buffer which-key--buffer
          (insert str-to-insert))))
    n-lines))

(defun which-key/replace-strings-from-alist (replacements)
  "Find and replace text in buffer according to REPLACEMENTS,
which is an alist where the car of each element is the text to
replace and the cdr is the replacement text."
  (dolist (rep replacements)
    (save-excursion
      (goto-char (point-min))
      (while (or (search-forward (car rep) nil t))
        (replace-match (cdr rep) t t)))))

(defun which-key/format-matches (unformatted max-len-key max-len-desc)
  "Turn each key-desc-cons in UNFORMATTED into formatted
strings (including text properties), and pad with spaces so that
all are a uniform length.  MAX-LEN-KEY and MAX-LEN-DESC are the
longest key and description in the buffer, respectively."
  (mapcar
   (lambda (key-desc-cons)
     (let* ((key (car key-desc-cons))
            (desc (cdr key-desc-cons))
            (group (string-match-p "^group:" desc))
            (desc (if group (substring desc 6) desc))
            (prefix (string-match-p "^Prefix" desc))
            (desc (if (or prefix group) (concat "+" desc) desc))
            (desc-face (if (or prefix group)
                           'font-lock-keyword-face 'font-lock-function-name-face))
            ;; (sign (if (or prefix group) "▶" "→"))
            (sign "→")
            (desc (which-key/truncate-description desc))
            ;; pad keys to max-len-key
            (padded-key (s-pad-left max-len-key " " key))
            (padded-desc (s-pad-right max-len-desc " " desc)))
       (format (concat (propertize "%s" 'face 'font-lock-constant-face) " "
                       (propertize sign 'face 'font-lock-comment-face) " "
                       (propertize "%s" 'face desc-face) " ")
               padded-key padded-desc)))
   unformatted))

(defsubst which-key/truncate-description (desc)
  "Truncate DESC description to `which-key-max-description-length'."
  (if (> (length desc) which-key-max-description-length)
      (concat (substring desc 0 which-key-max-description-length) "..")
    desc))
