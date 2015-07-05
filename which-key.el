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
  "Width of which-key buffer .")
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
       (which-key/make-display-method-aliases which-key-display-method)
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

;; Helper functions

(defsubst which-key/truncate-description (desc)
  "Truncate DESC description to `which-key-max-description-length'."
  (if (> (length desc) which-key-max-description-length)
      (concat (substring desc 0 which-key-max-description-length) "..")
    desc))

(defun which-key/available-lines-per-page ()
  "Only works for minibuffer right now."
  (when (eq which-key-display-method 'minibuffer)
    (if (floatp max-mini-window-height)
        (floor (* (frame-text-lines)
                  max-mini-window-height))
      max-mini-window-height)))

(defun which-key/replace-strings-from-alist (replacements)
  "Find and replace text in buffer according to REPLACEMENTS,
which is an alist where the car of each element is the text to
replace and the cdr is the replacement text."
  (dolist (rep replacements)
      (save-excursion
        (goto-char (point-min))
        (while (or (search-forward (car rep) nil t))
          (replace-match (cdr rep) t t)))))

;; in case I decide to add padding
;; (defsubst which-key/buffer-height (line-breaks) line-breaks)

(defun which-key/buffer-width (column-width sel-window-width)
  (cond ((eq which-key-display-method 'minibuffer)
         (frame-text-cols))
        ((and (eq which-key-buffer-display-function 'display-buffer-in-side-window)
              (member which-key-buffer-position '(left right)))
         (min which-key-vertical-buffer-width column-width))
        ((eq which-key-buffer-display-function 'display-buffer-in-side-window)
         (frame-text-width))
        ;; ((eq which-key-buffer-display-function 'display-buffer-below-selected)
        ;;  sel-window-width)
        (t nil)))

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

;; "Core" functions

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

(defun which-key/create-page (avl-lines n-columns keys)
  (let (lines
        (n-keys (length keys))
        (n-lines (min (ceiling (/ (float n-keys) n-columns)) avl-lines)))
    (dotimes (i n-lines)
      (setq lines
            (push
             (subseq keys (* i n-columns) (min n-keys (* (1+ i) n-columns)))
             lines)))
    (mapconcat (lambda (x) (apply 'concat x)) (reverse lines) "\n")))

(defun which-key/populate-buffer (formatted-keys column-width buffer-width)
  "Insert FORMATTED-STRINGS into which-key buffer, breaking after BUFFER-WIDTH."
  (let* ((width (if buffer-width buffer-width (frame-text-width)))
         (n-keys (length formatted-keys))
         (n-columns (/ width column-width)) ;; integer division
         (avl-lines/page (which-key/available-lines-per-page))
         (n-keys/page (when avl-lines/page (* n-columns avl-lines/page)))
         (n-pages (if n-keys/page
                      (ceiling (/ (float n-keys) n-keys/page)) 1))
         lines pages n-lines )
    (when (> n-columns 0)
      (dotimes (p n-pages)
        (setq pages
              (push (which-key/create-page avl-lines/page n-columns
                     (subseq formatted-keys (* p n-keys/page)
                             (min (* (1+ p) n-keys/page) n-keys))) pages)))
      (setq pages (reverse pages))
      (if (eq which-key-display-method 'minibuffer)
          (let (message-log-max) (message "%s" (car pages)))
        (insert (car pages))))
    n-lines))

(defun which-key/update ()
  "Fill which-key--buffer with key descriptions and reformat.
Finally, show the buffer."
  (let ((key (this-single-command-keys)))
    (if (> (length key) 0)
        (progn
          (when which-key--close-timer (cancel-timer which-key--close-timer))
          (which-key/hide-buffer)
          (let* ((buf (current-buffer))
                 (bottom-or-top (member which-key-buffer-position '(top bottom)))
                 ;; get formatted key bindings
                 (fmt-width-cons (which-key/get-formatted-key-bindings buf key))
                 (formatted-keys (car fmt-width-cons))
                 (column-width (cdr fmt-width-cons))
                 (buffer-width (which-key/buffer-width column-width (window-width)))
                 n-lines)
            ;; populate target buffer
            (setq n-lines (which-key/populate-buffer
                           formatted-keys column-width buffer-width))
            ;; show buffer
            (unless (eq which-key-display-method 'minibuffer)
              (setq which-key--window (which-key/show-buffer n-lines buffer-width)
                    which-key--close-timer (run-at-time
                                            which-key-close-buffer-idle-delay
                                            nil 'which-key/hide-buffer)))))
      ;; command finished maybe close the window
      (which-key/hide-buffer))))

;; Timers

(defun which-key/start-open-timer ()
  "Activate idle timer."
  (when which-key--open-timer (cancel-timer which-key--open-timer)); start over
  (setq which-key--open-timer
        (run-with-idle-timer which-key-idle-delay t 'which-key/update)))

(defun which-key/stop-open-timer ()
  "Deactivate idle timer."
  (cancel-timer which-key--open-timer))

;; placeholder for page flipping 
;; (defun which-key/start-next-page-timer ())

;; Display functions

(defun which-key/show-buffer-display-buffer (height width)
  (let ((side which-key-buffer-position) alist)
    (setq alist (list (when side   (cons 'side side))
                      (when height (cons 'window-height  height))
                      (when width  (cons 'window-width  width))))
    (display-buffer "*which-key*" (cons which-key-buffer-display-function alist))))

(defun which-key/hide-buffer-display-buffer ()
  (when (window-live-p which-key--window)
    (delete-window which-key--window)))

(defun which-key/show-buffer-popwin (height width)
  "Using popwin popup buffer with dimensions HEIGHT and WIDTH."
  (popwin:popup-buffer which-key-buffer-name
                       :height height
                       :width width
                       :noselect t
                       :position which-key-buffer-position))

(defun which-key/hide-buffer-popwin ()
  "Hide popwin buffer."
  (when (eq popwin:popup-buffer (get-buffer which-key--buffer))
    (popwin:close-popup-window)))

(defun which-key/make-display-method-aliases (method)
  (cond
   ((eq method 'minibuffer)
    (defun which-key/hide-buffer ()))
   ((member method '(popwin display-buffer))
         (defalias 'which-key/show-buffer
           (intern (concat "which-key/show-buffer-" (symbol-name method))))
         (defalias 'which-key/hide-buffer
           (intern (concat "which-key/hide-buffer-" (symbol-name method)))))
        (t (error "error: Invalid choice for which-key-display-method"))))

(provide 'which-key)

;;; which-key.el ends here
