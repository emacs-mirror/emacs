;;; completion-preview.el --- Preview completion with inline overlay  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Free Software Foundation, Inc.

;; Author: Eshel Yaron <me@eshelyaron.com>
;; Maintainer: Eshel Yaron <me@eshelyaron.com>
;; Keywords: abbrev convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides the Completion Preview mode.  This minor mode
;; displays a completion suggestion for the symbol at point in an
;; overlay after point.  Check out the customization group
;; `completion-preview' for user options that you may want to tweak.
;;
;; To enable Completion Preview mode, use `completion-preview-mode'.
;; To accept the completion suggestion, press TAB.  If you want to
;; ignore a completion suggestion, just go on editing or moving around
;; the buffer.  Completion Preview mode continues to update the
;; suggestion as you type according to the text around point.
;;
;; The commands `completion-preview-next-candidate' and
;; `completion-preview-prev-candidate' allow you to cycle the
;; completion candidate that the preview suggests.  These commands
;; don't have a default keybinding, but you can bind them, for
;; example, to M-n and M-p in `completion-preview-active-mode-map' to
;; have them handy whenever the preview is visible.
;;
;; If you set the user option `completion-preview-exact-match-only' to
;; non-nil, Completion Preview mode only suggests a completion
;; candidate when its the only possible completion for the (partial)
;; symbol at point.  The user option `completion-preview-commands'
;; says which commands should trigger the completion preview.  The
;; user option `completion-preview-minimum-symbol-length' specifies a
;; minimum number of consecutive characters with word or symbol syntax
;; that should appear around point for Emacs to suggest a completion.
;; By default, this option is set to 3, so Emacs suggests a completion
;; if you type "foo", but typing just "fo" doesn't show the preview.

;;; Code:

(require 'mwheel)

(defgroup completion-preview nil
  "In-buffer completion preview."
  :group 'completion)

(defcustom completion-preview-exact-match-only nil
  "Whether to show completion preview only when there is an exact match.

If this option is non-nil, Completion Preview mode only shows the
preview when there is exactly one completion candidate that
matches the symbol at point.  Otherwise, if this option is nil,
when there are multiple matching candidates the preview shows the
first candidate, and you can cycle between the candidates with
\\[completion-preview-next-candidate] and
\\[completion-preview-prev-candidate]."
  :type 'boolean
  :version "30.1")

(defcustom completion-preview-commands '(self-insert-command
                                         insert-char
                                         delete-backward-char
                                         backward-delete-char-untabify
                                         analyze-text-conversion)
  "List of commands that should trigger completion preview."
  :type '(repeat (function :tag "Command" :value self-insert-command))
  :version "30.1")

(defcustom completion-preview-minimum-symbol-length 3
  "Minimum length of the symbol at point for showing completion preview."
  :type 'natnum
  :version "30.1")

(defcustom completion-preview-message-format
  "Completion suggestion %i out of %n"
  "Message to show after cycling the completion preview suggestion.

If the value is a string, `completion-preview-next-candidate' and
`completion-preview-prev-candidate' display this string in the
echo area, after substituting \"%i\" with the 1-based index of
the completion suggestion that the preview is showing, and \"%n\"
with the total number of available completion suggestions for the
text around point.

If this option is nil, these commands do not display any message."
  :type '(choice (string :tag "Message format")
                 (const :tag "No message" nil))
  :version "30.1")

(defvar completion-preview-sort-function #'minibuffer--sort-by-length-alpha
  "Sort function to use for choosing a completion candidate to preview.")

(defface completion-preview
  '((t :inherit shadow))
  "Face for completion preview overlay."
  :version "30.1")

(defface completion-preview-exact
  '((((supports :underline t))
     :underline t :inherit completion-preview)
    (((supports :weight bold))
     :weight bold :inherit completion-preview)
    (t :background "gray"))
  "Face for exact completion preview overlay."
  :version "30.1")

(defface completion-preview-highlight
  '((t :inherit highlight))
  "Face for highlighting the completion preview when the mouse is over it."
  :version "30.1")

(defvar-keymap completion-preview-active-mode-map
  :doc "Keymap for Completion Preview Active mode."
  "C-i" #'completion-preview-insert
  ;; "M-n" #'completion-preview-next-candidate
  ;; "M-p" #'completion-preview-prev-candidate
  )

(defvar-keymap completion-preview--mouse-map
  :doc "Keymap for mouse clicks on the completion preview."
  "<down-mouse-1>" #'completion-preview-insert
  "C-<down-mouse-1>" #'completion-at-point
  "<down-mouse-2>" #'completion-at-point
  "<wheel-up>"     #'completion-preview-prev-candidate
  "<wheel-down>"   #'completion-preview-next-candidate
  (key-description (vector mouse-wheel-up-event))
  #'completion-preview-prev-candidate
  (key-description (vector mouse-wheel-down-event))
  #'completion-preview-next-candidate)

(defvar-local completion-preview--overlay nil)

(defvar completion-preview--internal-commands
  '(completion-preview-next-candidate
    completion-preview-prev-candidate
    ;; Don't dismiss or update the preview when the user scrolls.
    mwheel-scroll)
  "List of commands that manipulate the completion preview.

Completion Preview mode avoids updating the preview after these commands.")

(defsubst completion-preview--internal-command-p ()
  "Return non-nil if `this-command' manipulates the completion preview."
  (memq this-command completion-preview--internal-commands))

(defsubst completion-preview-require-certain-commands ()
  "Check if `this-command' is one of `completion-preview-commands'."
  (or (completion-preview--internal-command-p)
      (memq this-command completion-preview-commands)))

(defun completion-preview-require-minimum-symbol-length ()
  "Check if the length of symbol at point is at least above a certain threshold.
`completion-preview-minimum-symbol-length' determines that threshold."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (and bounds (<= completion-preview-minimum-symbol-length
                    (- (cdr bounds) (car bounds))))))

(defun completion-preview-hide ()
  "Hide the completion preview."
  (when completion-preview--overlay
    (delete-overlay completion-preview--overlay)
    (setq completion-preview--overlay nil)))

(defun completion-preview--make-overlay (pos string)
  "Make preview overlay showing STRING at POS, or move existing preview there."
  (if completion-preview--overlay
      (move-overlay completion-preview--overlay pos pos)
    (setq completion-preview--overlay (make-overlay pos pos))
    (overlay-put completion-preview--overlay 'window (selected-window)))
  (let ((previous (overlay-get completion-preview--overlay 'after-string)))
    (unless (and previous (string= previous string)
                 (eq (get-text-property 0 'face previous)
                     (get-text-property 0 'face string)))
      (add-text-properties 0 1 '(cursor 1) string)
      (overlay-put completion-preview--overlay 'after-string string))
    completion-preview--overlay))

(defsubst completion-preview--get (prop)
  "Return property PROP of the completion preview overlay."
  (overlay-get completion-preview--overlay prop))

(defun completion-preview--window-selection-change (window)
  "Hide completion preview in WINDOW after switching to another window.
Completion Preview mode adds this function to
`window-selection-change-functions', which see."
  (unless (or (eq window (selected-window))
              (eq window (minibuffer-selected-window)))
    (with-current-buffer (window-buffer window)
      (completion-preview-active-mode -1))))

(define-minor-mode completion-preview-active-mode
  "Mode for when the completion preview is shown."
  :interactive nil
  (if completion-preview-active-mode
      (add-hook 'window-selection-change-functions
                #'completion-preview--window-selection-change nil t)
    (remove-hook 'window-selection-change-functions
                 #'completion-preview--window-selection-change t)
    (completion-preview-hide)))

(defun completion-preview--try-table (table beg end props)
  "Check TABLE for a completion matching the text between BEG and END.

PROPS is a property list with additional information about TABLE.
See `completion-at-point-functions' for more details.

If TABLE contains a matching completion, return a list
\(PREVIEW BEG END ALL BASE EXIT-FN) where PREVIEW is the text to
show in the completion preview, ALL is the list of all matching
completion candidates, BASE is a common prefix that TABLE elided
from the start of each candidate, and EXIT-FN is either a
function to call after inserting PREVIEW or nil.  If TABLE does
not contain matching completions, or if there are multiple
matching completions and `completion-preview-exact-match-only' is
non-nil, return nil instead."
  (let* ((pred (plist-get props :predicate))
         (exit-fn (plist-get props :exit-function))
         (string (buffer-substring beg end))
         (md (completion-metadata string table pred))
         (sort-fn (or (completion-metadata-get md 'cycle-sort-function)
                      (completion-metadata-get md 'display-sort-function)
                      completion-preview-sort-function))
         (all (let ((completion-lazy-hilit t))
                (completion-all-completions string table pred
                                            (- (point) beg) md)))
         (last (last all))
         (base (or (cdr last) 0))
         (prefix (substring string base)))
    (when last
      (setcdr last nil)
      (when-let ((sorted (funcall sort-fn
                                  (delete prefix (all-completions prefix all)))))
        (unless (and (cdr sorted) completion-preview-exact-match-only)
          (list (propertize (substring (car sorted) (length prefix))
                            'face (if (cdr sorted)
                                      'completion-preview
                                    'completion-preview-exact)
                            'mouse-face 'completion-preview-highlight
                            'keymap completion-preview--mouse-map)
                (+ beg base) end sorted
                (substring string 0 base) exit-fn))))))

(defun completion-preview--capf-wrapper (capf)
  "Translate return value of CAPF to properties for completion preview overlay."
  (let ((res (ignore-errors (funcall capf))))
    (and (consp res)
         (not (functionp res))
         (seq-let (beg end table &rest plist) res
           (or (completion-preview--try-table table beg end plist)
               (unless (eq 'no (plist-get plist :exclusive))
                 ;; Return non-nil to exclude other capfs.
                 '(nil)))))))

(defun completion-preview--update ()
  "Update completion preview."
  (seq-let (preview beg end all base exit-fn)
      (run-hook-wrapped
       'completion-at-point-functions
       #'completion-preview--capf-wrapper)
    (when preview
      (let ((ov (completion-preview--make-overlay end preview)))
        (overlay-put ov 'completion-preview-beg beg)
        (overlay-put ov 'completion-preview-end end)
        (overlay-put ov 'completion-preview-index 0)
        (overlay-put ov 'completion-preview-cands all)
        (overlay-put ov 'completion-preview-base base)
        (overlay-put ov 'completion-preview-exit-fn exit-fn)
        (completion-preview-active-mode)))))

(defun completion-preview--show ()
  "Show a new completion preview.

Call `completion-at-point-functions' in order to obtain and
display a completion candidate for the text around point.

If the preview is already shown, first check whether the
suggested candidate remains a valid completion for the text at
point.  If so, update the preview according the new text at
point, otherwise hide it."
  (when completion-preview-active-mode
    ;; We were already showing a preview before this command, so we
    ;; check if the text before point is still a prefix of the
    ;; candidate that the preview suggested, and if so we first update
    ;; existing preview according to the changes made by this command,
    ;; and only then try to get a new candidate.  This ensures that we
    ;; never display a stale preview and that the preview doesn't
    ;; flicker, even with slow completion backends.
    (let* ((beg (completion-preview--get 'completion-preview-beg))
           (cands (completion-preview--get 'completion-preview-cands))
           (index (completion-preview--get 'completion-preview-index))
           (cand (nth index cands))
           (len (length cand))
           (end (+ beg len))
           (cur (point))
           (face (get-text-property 0 'face (completion-preview--get 'after-string))))
      (if (and (< beg cur end) (string-prefix-p (buffer-substring beg cur) cand))
          ;; The previous preview is still applicable, update it.
          (overlay-put (completion-preview--make-overlay
                        cur (propertize (substring cand (- cur beg))
                                        'face face
                                        'mouse-face 'completion-preview-highlight
                                        'keymap completion-preview--mouse-map))
                       'completion-preview-end cur)
        ;; The previous preview is no longer applicable, hide it.
        (completion-preview-active-mode -1))))
  ;; Run `completion-at-point-functions' to get a new candidate.
  (while-no-input (completion-preview--update)))

(defun completion-preview--post-command ()
  "Create, update or delete completion preview post last command."
  (if (and (completion-preview-require-certain-commands)
           (completion-preview-require-minimum-symbol-length))
      ;; We should show the preview.
      (or
       ;; If we're called after a command that itself updates the
       ;; preview, don't do anything.
       (completion-preview--internal-command-p)
       ;; Otherwise, show the preview.
       (completion-preview--show))
    (completion-preview-active-mode -1)))

(defun completion-preview-insert ()
  "Insert the completion candidate that the preview is showing."
  (interactive)
  (if completion-preview-active-mode
      (let* ((pre (completion-preview--get 'completion-preview-base))
             (end (completion-preview--get 'completion-preview-end))
             (ind (completion-preview--get 'completion-preview-index))
             (all (completion-preview--get 'completion-preview-cands))
             (efn (completion-preview--get 'completion-preview-exit-fn))
             (aft (completion-preview--get 'after-string))
             (str (concat pre (nth ind all))))
        (completion-preview-active-mode -1)
        (goto-char end)
        (insert (substring-no-properties aft))
        (when (functionp efn) (funcall efn str 'finished)))
    (user-error "No current completion preview")))

(defun completion-preview-prev-candidate ()
  "Cycle the candidate that the preview is showing to the previous suggestion."
  (interactive)
  (completion-preview-next-candidate -1))

(defun completion-preview-next-candidate (direction)
  "Cycle the candidate that the preview is showing in direction DIRECTION.

DIRECTION should be either 1 which means cycle forward, or -1
which means cycle backward.  Interactively, DIRECTION is the
prefix argument and defaults to 1."
  (interactive "p")
  (when completion-preview-active-mode
    (let* ((beg (completion-preview--get 'completion-preview-beg))
           (all (completion-preview--get 'completion-preview-cands))
           (cur (completion-preview--get 'completion-preview-index))
           (len (length all))
           (new (mod (+ cur direction) len))
           (str (nth new all))
           (pos (point)))
      (while (or (<= (+ beg (length str)) pos)
                 (not (string-prefix-p (buffer-substring beg pos) str)))
        (setq new (mod (+ new direction) len) str (nth new all)))
      (let ((aft (propertize (substring str (- pos beg))
                             'face (if (< 1 len)
                                       'completion-preview
                                     'completion-preview-exact)
                             'mouse-face 'completion-preview-highlight
                             'keymap completion-preview--mouse-map)))
        (add-text-properties 0 1 '(cursor 1) aft)
        (overlay-put completion-preview--overlay 'completion-preview-index new)
        (overlay-put completion-preview--overlay 'after-string aft))
      (when completion-preview-message-format
        (message (format-spec completion-preview-message-format
                              `((?i . ,(1+ new)) (?n . ,len))))))))

(defun completion-preview--active-p (_symbol buffer)
  "Check if the completion preview is currently shown in BUFFER."
  (buffer-local-value 'completion-preview-active-mode buffer))

(dolist (cmd '(completion-preview-insert
               completion-preview-prev-candidate
               completion-preview-next-candidate))
  (put cmd 'completion-predicate #'completion-preview--active-p))

;;;###autoload
(define-minor-mode completion-preview-mode
  "Show in-buffer completion suggestions in a preview as you type.

This mode automatically shows and updates the completion preview
according to the text around point.
\\<completion-preview-active-mode-map>\
When the preview is visible, \\[completion-preview-insert]
accepts the completion suggestion,
\\[completion-preview-next-candidate] cycles forward to the next
completion suggestion, and \\[completion-preview-prev-candidate]
cycles backward."
  :lighter " CP"
  (if completion-preview-mode
      (add-hook 'post-command-hook #'completion-preview--post-command nil t)
    (remove-hook 'post-command-hook #'completion-preview--post-command t)
    (completion-preview-active-mode -1)))

(provide 'completion-preview)
;;; completion-preview.el ends here
