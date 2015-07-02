;;; which-key.el

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

;; Internal Vars
(defvar which-key--buffer nil
  "Internal: Holds reference to which-key buffer.")
(defvar which-key--timer nil
  "Internal: Holds reference to timer.")
(defvar which-key--setup-p nil
  "Internal: Non-nil if which-key buffer has been setup")

(define-minor-mode which-key-mode
  "Toggle which-key-mode."
  :global t
  :lighter " WK"
  :require 'popwin
  :require 's
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

(defun which-key/format-matches (key-desc-cons max-len-key max-len-desc)
  "Turn `key-desc-cons' into formatted strings (including text
properties), and pad with spaces so that all are a uniform
length."
  (let* ((key (car key-desc-cons))
         (desc (cdr key-desc-cons))
         (group (string-match-p "^group:" desc))
         (prefix (string-match-p "^Prefix" desc))
         (desc-face (if (or prefix group)
                        'font-lock-keyword-face 'font-lock-function-name-face))
         (tmp-desc (which-key/truncate-description (if group (substring desc 6) desc)))
         (key-padding (s-repeat (- max-len-key (length key)) " "))
         (padded-desc (s-pad-right max-len-desc " " tmp-desc)))
    (format (concat (propertize "[" 'face 'font-lock-comment-face) "%s"
                    (propertize "]" 'face 'font-lock-comment-face) "%s"
                    (propertize " %s" 'face desc-face))
            key key-padding padded-desc)))

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

(defun which-key/get-vertical-buffer-width (max-len-key max-len-desc)
  (min which-key-vertical-buffer-width (+ 3 max-len-desc max-len-key)))

(defun which-key/insert-keys (formatted-strings vertical-buffer-width)
  "Insert strings into buffer breaking after `which-key-buffer-width'."
  (let ((char-count 0)
        (line-breaks 0)
        (width (if vertical-buffer-width
                   vertical-buffer-width
                   (frame-width))))
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
  "Fill which-key--buffer with key descriptions and reformat. Finally, show the buffer."
  (let ((key (this-single-command-keys)))
    (when (> (length key) 0)
      (let ((buf (current-buffer))
            (key-str-qt (regexp-quote (key-description key)))
            (bottom-or-top (member which-key-buffer-position '(top bottom)))
            (max-len-key 0) (max-len-desc 0) key-match desc-match
            unformatted formatted buffer-height buffer-width vertical-buffer-width)
        ;; get keybindings
        (with-temp-buffer
          (describe-buffer-bindings buf key)
          (goto-char (point-max))
          (while (re-search-backward
                  (format "^%s \\([^ \t]+\\)[ \t]+\\(\\(?:[^ \t\n]+ ?\\)+\\)$" key-str-qt)
                  nil t)
            (setq key-match (s-replace-all which-key-key-replacement-alist (match-string 1))
                  desc-match (match-string 2)
                  max-len-key (max max-len-key (length key-match))
                  max-len-desc (max max-len-desc (length desc-match)))
            (cl-pushnew (cons key-match desc-match) unformatted
                        :test (lambda (x y) (string-equal (car x) (car y)))))
          (setq max-len-desc (if (> max-len-desc which-key-max-description-length)
                                 (+ 2 which-key-max-description-length) ; for the ..
                               max-len-desc))
          (setq formatted (mapcar (lambda (str)
                                    (which-key/format-matches str max-len-key max-len-desc))
                                  unformatted)))
        (with-current-buffer (get-buffer which-key--buffer)
          (erase-buffer)
          (setq vertical-buffer-width
                (which-key/get-vertical-buffer-width max-len-desc max-len-key)
                buffer-line-breaks
                (which-key/insert-keys formatted (unless bottom-or-top vertical-buffer-width)))
          (goto-char (point-min))
          (which-key/replace-strings-from-alist which-key-general-replacement-alist)
          (if bottom-or-top
              (setq buffer-height (+ 2 buffer-line-breaks))
            (setq buffer-width vertical-buffer-width)))
        (which-key/show-buffer buffer-height buffer-width)
        (run-at-time which-key-close-buffer-idle-delay nil 'which-key/hide-buffer)))))

(defun which-key/setup ()
  "Create buffer for which-key."
  (setq which-key--buffer (get-buffer-create which-key-buffer-name))
  (setq which-key--setup-p t))

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
