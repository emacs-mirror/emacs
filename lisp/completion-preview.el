;;; completion-preview.el --- Preview completion with inline overlay  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

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
;; displays the top completion candidate for the symbol at point in an
;; overlay after point.  Check out the customization group
;; `completion-preview' for user options that you may want to tweak.
;;
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
;;
;; The user option `completion-preview-insert-on-completion' controls
;; what happens when you invoke `completion-at-point' while the
;; completion preview is visible.  By default this option is nil,
;; which tells `completion-at-point' to ignore the completion preview
;; and show the list of completion candidates as usual.  If you set
;; `completion-preview-insert-on-completion' to non-nil, then
;; `completion-at-point' inserts the preview directly without looking
;; for more candidates.

;;; Code:

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
                                         backward-delete-char-untabify)
  "List of commands that should trigger completion preview."
  :type '(repeat (function :tag "Command" :value self-insert-command))
  :version "30.1")

(defcustom completion-preview-minimum-symbol-length 3
  "Minimum length of the symbol at point for showing completion preview."
  :type 'natnum
  :version "30.1")

(defcustom completion-preview-insert-on-completion nil
  "Whether \\[completion-at-point] inserts the previewed suggestion."
  :type 'boolean
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

(defvar-keymap completion-preview-active-mode-map
  :doc "Keymap for Completion Preview Active mode."
  "C-i" #'completion-preview-insert
  ;; "M-n" #'completion-preview-next-candidate
  ;; "M-p" #'completion-preview-prev-candidate
  )

(defvar-local completion-preview--overlay nil)

(defvar completion-preview--internal-commands
  '(completion-preview-next-candidate completion-preview-prev-candidate)
  "List of commands that manipulate the completion preview.")

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
  "Make a new completion preview overlay at POS showing STRING."
  (if completion-preview--overlay
      (move-overlay completion-preview--overlay pos pos)
    (setq completion-preview--overlay (make-overlay pos pos))
    (overlay-put completion-preview--overlay 'window (selected-window)))
  (let ((previous (overlay-get completion-preview--overlay 'after-string)))
    (unless (and previous (string= previous string))
      (add-text-properties 0 1 '(cursor 1) string)
      (overlay-put completion-preview--overlay 'after-string string))
    completion-preview--overlay))

(defun completion-preview--get (prop)
  "Return property PROP of the completion preview overlay."
  (overlay-get completion-preview--overlay prop))

(define-minor-mode completion-preview-active-mode
  "Mode for when the completion preview is shown."
  :interactive nil
  (if completion-preview-active-mode
      (add-hook 'completion-at-point-functions #'completion-preview--insert -1 t)
    (remove-hook 'completion-at-point-functions #'completion-preview--insert t)
    (completion-preview-hide)))

(defun completion-preview--exit-function (func)
  "Return an exit function that hides the completion preview and calls FUNC."
  (lambda (&rest args)
    (completion-preview-active-mode -1)
    (when (functionp func) (apply func args))))

(defun completion-preview--update ()
  "Update completion preview."
  (seq-let (beg end table &rest plist)
      (let ((completion-preview-insert-on-completion nil))
        (run-hook-with-args-until-success 'completion-at-point-functions))
    (when (and beg end table)
      (let* ((pred (plist-get plist :predicate))
             (exit-fn (completion-preview--exit-function
                       (plist-get plist :exit-function)))
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
             (bbeg (+ beg base))
             (prefix (substring string base)))
        (when last
          (setcdr last nil)
          (let* ((filtered (remove prefix (all-completions prefix all)))
                 (sorted (funcall sort-fn filtered))
                 (multi (cadr sorted))  ; multiple candidates
                 (cand (car sorted)))
            (when (and cand
                       (not (and multi
                                 completion-preview-exact-match-only)))
              (let* ((face (if multi
                               'completion-preview
                             'completion-preview-exact))
                     (after (propertize (substring cand (length prefix))
                                        'face face))
                     (ov (completion-preview--make-overlay end after)))
                (overlay-put ov 'completion-preview-beg bbeg)
                (overlay-put ov 'completion-preview-end end)
                (overlay-put ov 'completion-preview-index 0)
                (overlay-put ov 'completion-preview-cands sorted)
                (overlay-put ov 'completion-preview-exit-fn exit-fn)
                (completion-preview-active-mode)))))))))

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
                                        'face face))
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

(defun completion-preview--insert ()
  "Completion at point function for inserting the current preview.

When `completion-preview-insert-on-completion' is nil, this
function returns nil.  Completion Preview mode adds this function
to `completion-at-point-functions' when the preview is shown,
such that `completion-at-point' inserts the preview candidate if
and only if `completion-preview-insert-on-completion' is non-nil."
  (when (and completion-preview-active-mode
             completion-preview-insert-on-completion)
    (list (completion-preview--get 'completion-preview-beg)
          (completion-preview--get 'completion-preview-end)
          (list (nth (completion-preview--get 'completion-preview-index)
                     (completion-preview--get 'completion-preview-cands)))
          :exit-function (completion-preview--get 'completion-preview-exit-fn))))

(defun completion-preview-insert ()
  "Insert the completion candidate that the preview shows."
  (interactive)
  (let ((completion-preview-insert-on-completion t))
    (completion-at-point)))

(defun completion-preview-prev-candidate ()
  "Cycle the candidate that the preview shows to the previous suggestion."
  (interactive)
  (completion-preview-next-candidate -1))

(defun completion-preview-next-candidate (direction)
  "Cycle the candidate that the preview shows in direction DIRECTION.

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
                                     'completion-preview-exact))))
        (add-text-properties 0 1 '(cursor 1) aft)
        (overlay-put completion-preview--overlay 'completion-preview-index new)
        (overlay-put completion-preview--overlay 'after-string aft)))))

;;;###autoload
(define-minor-mode completion-preview-mode
  "Show in-buffer completion preview as you type."
  :lighter " CP"
  (if completion-preview-mode
      (add-hook 'post-command-hook #'completion-preview--post-command nil t)
    (remove-hook 'post-command-hook #'completion-preview--post-command t)
    (completion-preview-active-mode -1)))

(provide 'completion-preview)
;;; completion-preview.el ends here
