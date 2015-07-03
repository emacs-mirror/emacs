;;; which-key.el

;; Copyright (C) 2015 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/justbur/which-key/
;; Version: 0.1
;; Keywords:
;; Package-Requires: ((s "1.9.0" popwin "1.0.0"))

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
(defvar which-key-max-description-length 30
  "Truncate the description of keys to this length (adds
  \"..\")")
(defvar which-key-key-replacement-alist
  '((">". "") ("<" . "") ("left" ."←") ("right" . "→"))
  "The strings in the car of each cons cell are replaced with the
  strings in the cdr for each key.")
(defvar which-key-general-replacement-alist nil
  "See `which-key-key-replacement-alist'. This is a list of cons
  cells for replacing any text, keys and descriptions. You can
  also use elisp regexp in the car of the cells.")
(defvar which-key-buffer-name "*which-key*"
  "Name of which-key buffer.")
(defvar which-key-buffer-position 'bottom
  "Position of which-key buffer")
(defvar which-key-vertical-buffer-width 60
  "Width of which-key buffer .")

(defconst which-key-buffer-display-function
  'display-buffer-in-side-window
  "Controls where the buffer is displayed.
  The current default is also controlled by
  `which-key-buffer-position'. Other options are currently
  disabled.")

;; Internal Vars
(defvar which-key--buffer nil
  "Internal: Holds reference to which-key buffer.")
(defvar which-key--window nil
  "Internal: Holds reference to which-key window.")
(defvar which-key--timer nil
  "Internal: Holds reference to timer.")
(defvar which-key--close-timer nil
  "Internal: Holds reference to close window timer.")
(defvar which-key--setup-p nil
  "Internal: Non-nil if which-key buffer has been setup")

(define-minor-mode which-key-mode
  "Toggle which-key-mode."
  :global t
  :lighter " WK"
  (funcall (if which-key-mode
               (progn
                 (unless which-key--setup-p (which-key/setup))
                 'which-key/turn-on-timer)
             'which-key/turn-off-timer)))

(defsubst which-key/truncate-description (desc)
  "Truncate key description to `which-key-max-description-length'."
  (if (> (length desc) which-key-max-description-length)
      (concat (substring desc 0 which-key-max-description-length) "..")
    desc))

(defun which-key/format-matches (unformatted max-len-key max-len-desc)
  "Turn `key-desc-cons' into formatted strings (including text
properties), and pad with spaces so that all are a uniform
length."
  (mapcar
   (lambda (key-desc-cons)
     (let* ((key (car key-desc-cons))
            (desc (cdr key-desc-cons))
            (group (string-match-p "^group:" desc))
            (prefix (string-match-p "^Prefix" desc))
            (desc-face (if (or prefix group)
                           'font-lock-keyword-face 'font-lock-function-name-face))
            (sign (if (or prefix group) "▶" "→"))
            (tmp-desc (which-key/truncate-description (if group (substring desc 6) desc)))
            (key-padding (s-repeat (- max-len-key (length key)) " "))
            (padded-desc (s-pad-right max-len-desc " " tmp-desc)))
       (format (concat (propertize "%s%s" 'face 'font-lock-constant-face) " "
                       (propertize sign 'face 'font-lock-comment-face)
                       (propertize " %s" 'face desc-face))
               key-padding key padded-desc)))
   unformatted))

(defun which-key/replace-strings-from-alist (replacements)
  "Find and replace text in buffer according to REPLACEMENTS,
which is an alist where the car of each element is the text to
replace and the cdr is the replacement text. "
  (dolist (rep replacements)
    (let ((trunc-car (which-key/truncate-description (car rep)))
          old-face)
      (save-excursion
        (while (or (search-forward (car rep) nil t)
                   (search-forward trunc-car nil t))
          (setq old-face (get-text-property (match-beginning 0) 'face))
          (replace-match (propertize (cdr rep) 'face old-face) nil t))))))

(defun which-key/buffer-width (max-len-key max-len-desc sel-window-width)
  (cond ((and (eq which-key-buffer-display-function 'display-buffer-in-side-window)
              (member which-key-buffer-position '(left right)))
         (min which-key-vertical-buffer-width (+ 3 max-len-desc max-len-key)))
        ((eq which-key-buffer-display-function 'display-buffer-in-side-window)
         (frame-width))
        ;; ((eq which-key-buffer-display-function 'display-buffer-below-selected)
        ;;  sel-window-width)
        (t nil)))

(defsubst which-key/buffer-height (line-breaks) (+ 2 line-breaks))

(defun which-key/insert-keys (formatted-strings buffer-width)
  "Insert strings into buffer breaking after `which-key-buffer-width'."
  (let ((char-count 0)
        (line-breaks 0)
        (width (if buffer-width buffer-width (frame-width))))
    (insert (mapconcat
             (lambda (str)
               (let* ((str-len (length (substring-no-properties str)))
                      (new-count (+ char-count str-len)))
                 (if (> new-count width)
                     (progn (setq char-count str-len)
                            (cl-incf line-breaks)
                            (concat "\n" str))
                   (setq char-count new-count)
                   str))) formatted-strings ""))
    line-breaks))

(defun which-key/update-buffer-and-show ()
  "Fill which-key--buffer with key descriptions and reformat.
Finally, show the buffer."
  (let ((key (this-single-command-keys)))
    (if (> (length key) 0)
        (progn
          (when which-key--close-timer (cancel-timer which-key--close-timer))
          (which-key/hide-buffer)
          (let ((buf (current-buffer)) (win-width (window-width))
                (key-str-qt (regexp-quote (key-description key)))
                (bottom-or-top (member which-key-buffer-position '(top bottom)))
                (max-len-key 0) (max-len-desc 0)
                key-match desc-match unformatted formatted buffer-width
                line-breaks)
            ;; get keybindings
            (with-temp-buffer
              (describe-buffer-bindings buf key)
              (goto-char (point-max))
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
                    max-len-desc (1+ max-len-desc) ; pad with one character
                    formatted (which-key/format-matches
                               unformatted max-len-key max-len-desc)))
            (with-current-buffer (get-buffer which-key--buffer)
              (erase-buffer)
              (setq buffer-width (which-key/buffer-width
                                  max-len-key max-len-desc win-width)
                    line-breaks  (which-key/insert-keys
                                  formatted buffer-width))
              (goto-char (point-min))
              (which-key/replace-strings-from-alist
               which-key-general-replacement-alist)
              (goto-char (point-max)))
            (setq which-key--window (which-key/show-buffer
                                     (which-key/buffer-height line-breaks)
                                     buffer-width))
            (setq which-key--close-timer (run-at-time
                                          which-key-close-buffer-idle-delay
                                          nil 'which-key/hide-buffer))))
      ;; close the window
      (which-key/hide-buffer))))

(defun which-key/setup ()
  "Create buffer for which-key."
  (require 's)
  (require 'popwin)
  (setq which-key--buffer (get-buffer-create which-key-buffer-name))
  (setq which-key--setup-p t))

;; (defun which-key/show-buffer (height width)
;;   (let ((side which-key-buffer-position) alist)
;;     (setq alist (list (when side   (cons 'side side))
;;                       (when height (cons 'window-height  height))
;;                       (when width  (cons 'window-width  width))))
;;     (display-buffer "*which-key*" (cons which-key-buffer-display-function alist))))

;; (defun which-key/hide-buffer ()
;;   "Like it says :\)"
;;   (when (window-live-p which-key--window)
;;     (delete-window which-key--window)))

(defun which-key/show-buffer (height width)
  (popwin:popup-buffer which-key-buffer-name
                       :width width
                       :height height
                       :noselect t
                       :position which-key-buffer-position))

(defun which-key/hide-buffer ()
  "Like it says :\)"
  (when (eq popwin:popup-buffer (get-buffer which-key--buffer))
    (popwin:close-popup-window)))

(defun which-key/turn-on-timer ()
  "Activate idle timer."
  (setq which-key--timer
        (run-with-idle-timer which-key-idle-delay t 'which-key/update-buffer-and-show)))

(defun which-key/turn-off-timer ()
  "Deactivate idle timer."
  (cancel-timer which-key--timer))

(provide 'which-key)

;;; which-key.el ends here
