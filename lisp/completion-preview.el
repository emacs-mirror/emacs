;;; completion-preview.el --- Preview completion with inline overlay  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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
;; Completion Preview mode uses `completion-at-point-functions' to find
;; relevant completion suggestions, similarly to `completion-at-point'.
;; You can use `completion-at-point' with your favorite in-buffer
;; completion interface together with Completion Preview mode, just
;; invoke `completion-at-point' as usual when you want to see all
;; currently available completions.  Another reason to invoke
;; `completion-at-point' is when you want non-prefix completion, since
;; Completion Preview mode only shows one prefix completion.
;;
;; The commands `completion-preview-next-candidate' and
;; `completion-preview-prev-candidate' allow you to cycle the
;; completion candidate that the preview suggests.  These commands
;; don't have a default keybinding, but you can bind them, for
;; example, to M-n and M-p in `completion-preview-active-mode-map' to
;; have them handy whenever the preview is visible.
;;
;; When the completion candidate that the preview is showing shares a
;; common prefix with all other candidates, Completion Preview mode
;; underlines that common prefix.  If you want to insert the common
;; prefix but with a different suffix than the one the preview is
;; showing, use the command `completion-preview-complete'.  This command
;; inserts just the common prefix and lets you go on typing as usual.
;; If you invoke `completion-preview-complete' when there is no common
;; prefix (so nothing is underlined in the preview), it displays a list
;; of all matching completion candidates.
;;
;; You can also insert only the first word of the completion candidate
;; with the command `completion-preview-insert-word'.  With a numeric
;; prefix argument, it inserts that many words instead of just the one.
;; This command is not bound by default, but you may want to bind it to
;; M-f (or remap `forward-word') in `completion-preview-active-mode-map'
;; since it's very much like a `forward-word' that also moves "into" the
;; completion preview.  To define your own command that inserts part of
;; a completion candidate by moving "into" the completion preview, use
;; the function `completion-preview-partial-insert'.  For example, you
;; can define a command that completes exactly one symbol as follows:
;;
;;   (defun my-completion-preview-insert-symbol ()
;;     (interactive)
;;     (completion-preview-partial-insert #'forward-symbol 1))
;;
;; Similarly to `completion-preview-insert-word', the command
;; `completion-preview-insert-sexp' lets you complete by one or more
;; balanced expressions.  The definition of this command is very similar
;; to the simple example above, expect it uses `forward-sexp' rather
;; than `forward-symbol'.  This command can be useful when you're using
;; Completion Preview mode with long, complex completion candidates,
;; such as entire shell commands from the shell history.
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
;; If you want the preview to appear also after non-symbol characters,
;; such as punctuation, set `completion-preview-minimum-symbol-length'
;; to nil.  If you do so, you may want to customize the user option
;; `completion-preview-idle-delay' to have the preview appear only
;; when you pause typing for a short duration rather than after every
;; key.  Try setting it to 0.2 seconds and see how that works for you.
;;
;; By default, Completion Preview mode automatically adapts the
;; background color of the preview overlay to match the background color
;; of the buffer text it's completing.  If you prefer a distinct
;; background color for the preview, disable this feature by customizing
;; `completion-preview-adapt-background-color' to nil.
;;
;; Sometimes you may want to use Completion Preview mode alongside other
;; Emacs features that place an overlay after point, in a way that could
;; "compete" with the preview overlay.  In such cases, you should give
;; the completion preview overlay a higher priority, so it properly
;; appears immediately after point, before other overlays.  To do that,
;; set the variable `completion-preview-overlay-priority'.  You can set
;; it buffer-locally if you only use competing overlays in some buffers.
;; In particular, an important use case for this variable is enabling
;; Completion Preview mode for `M-:' and other minibuffers that support
;; `completion-at-point'.  In the minibuffer, some message are displayed
;; using an overlay that may, by default, conflict with the completion
;; preview overlay.  Use `completion-preview-overlay-priority' to
;; resolve this conflict by giving the completion preview overlay a
;; higher priority:
;;
;;   (add-hook 'eval-expression-minibuffer-setup-hook
;;             (lambda ()
;;               (setq-local completion-preview-overlay-priority 1200)
;;               (completion-preview-mode)))

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
                                         backward-delete-char-untabify
                                         analyze-text-conversion
                                         completion-preview-complete
                                         completion-preview-insert-word
                                         completion-preview-insert-sexp)
  "List of commands that should trigger completion preview."
  :type '(repeat (function :tag "Command" :value self-insert-command))
  :version "30.1")

(defcustom completion-preview-minimum-symbol-length 3
  "Minimum length of the symbol at point for showing completion preview.

If this is nil rather than a number of characters, show the preview also
after non-symbol characters, such as punctuation or whitespace."
  :type '(choice (natnum :tag "Minimum number of symbol characters")
                 (const :tag "Disable minimum symbol length requirement" nil))
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

(defcustom completion-preview-idle-delay nil
  "If non-nil, wait this many idle seconds before displaying completion preview.

If this is nil, display the completion preview without delay."
  :type '(choice (number :tag "Delay duration in seconds")
                 (const :tag "No delay" nil))
  :version "30.1")

(defcustom completion-preview-ignore-case nil
  "Whether Completion Preview mode ignores case differences.

By default this option is nil, which says that case is significant, so a
completion candidate \"FooBar\" matches prefix \"Foo\", but not \"foo\".
If you set it to non-nil, then Completion Preview mode also suggests
completions that differ in case from the prefix that you type; for
example, it may suggest completing \"foo\" with the suffix \"Bar\" when
there's an available completion candidate \"FooBar\".  Note that in this
case, when you insert the completion (with `completion-preview-insert'),
Completion Preview mode does not update the completed prefix according
to the capitalization of the completion candidate, instead it simply
ignores such case differences, so the resulting string is \"fooBar\".

See also `completion-ignore-case'."
  :type 'boolean
  :version "31.1")

(defcustom completion-preview-adapt-background-color 'completion-preview
  "Control automatic adaptation of completion preview background color.

This is either a face name or a (possibly empty) list of face names,
which Completion Preview mode automatically remaps when showing the
preview, such that the background color of the face(s) matches the
background color at point.

By default, this option specifies the `completion-preview' face (which
also affects its descendent faces `completion-preview-common' and
`completion-preview-exact') so the completion preview uses the
background color at point.

This is especially useful when there are other overlays at point that
affect the background color, for example with `hl-line-mode'."
  :type '(choice face
                 (repeat :tag "List of faces" face)
                 (const :tag "Disable" nil))
  :version "31.1")

(defcustom completion-preview-sort-function #'minibuffer--sort-by-length-alpha
  "Sort function to use for choosing a completion candidate to preview.

Completion Preview mode calls the function that this option specifies to
sort completion candidates.  The function takes one argument, the list
of candidates, and returns the list sorted.

The default sort function sorts first by length, then alphabetically.
To disable sorting, set this option to `identity'.

If the completion table that produces the candidates already specifies a
sort function, it takes precedence over this option."
  :type '(choice
          (function-item :tag "Sort alphabetically"
                         minibuffer-sort-alphabetically)
          (function-item :tag "First by length, then alphabetically"
                         minibuffer--sort-by-length-alpha)
          (function-item :tag "Disable sorting" identity)
          (function :tag "Custom sort function"))
  :version "31.1")

(defface completion-preview
  '((t :inherit shadow))
  "Face for completion candidates in the completion preview overlay."
  :version "30.1")

(defface completion-preview-common
  '((((supports :underline t))
     :underline t :inherit completion-preview)
    (((supports :weight bold))
     :weight bold :inherit completion-preview)
    (t :background "gray"))
  "Face for the longest common prefix in the completion preview."
  :version "30.1")

(defface completion-preview-exact
  ;; An exact match is also the longest common prefix of all matches.
  '((t :underline "#00aa00" :inherit completion-preview-common))
  "Face for matches in the completion preview overlay."
  :version "30.1")

(defface completion-preview-highlight
  '((t :inherit highlight))
  "Face for highlighting the completion preview when the mouse is over it."
  :version "30.1")

(defvar-keymap completion-preview-active-mode-map
  :doc "Keymap for Completion Preview Active mode."
  "C-i" #'completion-preview-insert
  ;; FIXME: Should this have another/better binding by default?
  "M-i" #'completion-preview-complete
  ;; "M-n" #'completion-preview-next-candidate
  ;; "M-p" #'completion-preview-prev-candidate
  ;; "<remap> <forward-word>" #'completion-preview-insert-word
  ;; "<remap> <forward-sexp>" #'completion-preview-insert-sexp
  )

(defun completion-preview--ignore ()
  "Do nothing, including updating the completion preview.

This is the same as `ignore', except that Completion Preview mode skips
hiding or updating the completion preview after this command runs."
  (interactive)
  nil)

(put 'completion-preview--ignore 'completion-predicate #'ignore)

(defvar-keymap completion-preview--mouse-map
  :doc "Keymap for mouse clicks on the completion preview."
  "<mouse-1>"        #'completion-preview-insert
  ;; Ignore the corresponding button-down event.
  "<down-mouse-1>"   #'completion-preview--ignore
  "C-<mouse-1>"      #'completion-preview-complete
  "C-<down-mouse-1>" #'completion-preview--ignore
  "<mouse-2>"        #'completion-preview-complete
  "<down-mouse-2>"   #'completion-preview--ignore
  "<wheel-up>"       #'completion-preview-prev-candidate
  "<wheel-down>"     #'completion-preview-next-candidate)

(defvar-local completion-preview--overlay nil)

(defvar completion-preview--internal-commands
  '(completion-preview-next-candidate
    completion-preview-prev-candidate
    completion-preview--ignore
    ;; Don't dismiss or update the preview when the user scrolls.
    mwheel-scroll)
  "List of commands that manipulate the completion preview.

Completion Preview mode avoids updating the preview after these commands.")

(defvar-local completion-preview--inhibit-update-p nil
  "Whether to inhibit updating the completion preview following this command.")

(defsubst completion-preview--inhibit-update ()
  "Inhibit updating the completion preview following this command."
  (setq completion-preview--inhibit-update-p t))

(defsubst completion-preview-require-certain-commands ()
  "Check if `this-command' is one of `completion-preview-commands'."
  (memq this-command completion-preview-commands))

(defun completion-preview-require-minimum-symbol-length ()
  "Check if the length of symbol at point is at least above a certain threshold.
`completion-preview-minimum-symbol-length' determines that threshold."
  (or (null completion-preview-minimum-symbol-length)
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
        (and bounds (<= completion-preview-minimum-symbol-length
                        (- (cdr bounds) (car bounds)))))))

(defun completion-preview-hide ()
  "Hide the completion preview."
  (when completion-preview--overlay
    (delete-overlay completion-preview--overlay)
    (setq completion-preview--overlay nil
          completion-preview--inhibit-update-p nil)))

(defvar completion-preview-overlay-priority nil
  "Value of the `priority' property for the completion preview overlay.")

(defun completion-preview--bg-color (pos)
  "Return background color at POS."
  ;; This takes into account face remappings and multiple overlays that
  ;; specify the `face' property, unlike `background-color-at-point'.
  (catch 'found
    (named-let rec ((spec (seq-keep (lambda (ov) (overlay-get ov 'face))
                                    (overlays-at pos t)))
                    (trace nil))
      (dolist (face (if (face-list-p spec) spec (list spec)))
        (let (cur)
          (if (and (setq cur (alist-get face face-remapping-alist))
                   (not (memq face trace)))
              (rec cur (cons face trace))
            (cond ((and face (symbolp face))
                   (let ((value (face-attribute face :background nil t)))
                     (unless (member value '(nil "unspecified-bg" unspecified))
                       (throw 'found value))))
                  ((consp face)
                   (when-let* ((value (or (cdr (memq 'background-color face))
                                          (cadr (memq :background face)))))
                     (throw 'found value)))))))
      (unless trace
        (save-excursion
          (goto-char pos)
          (font-lock-ensure (pos-bol) (pos-eol)))
        (rec (or (and font-lock-mode
                      (get-text-property pos 'font-lock-face))
                 (get-text-property pos 'face))
             '(nil))
        (rec 'default '(nil))))))

(defvar completion-preview--face-remap-cookie-jar nil)

(declare-function face-remap-remove-relative "face-remap" (cookie))

(defun completion-preview--make-overlay (pos string)
  "Make preview overlay showing STRING at POS, or move existing preview there."
  (if completion-preview--overlay
      (move-overlay completion-preview--overlay pos pos)
    (setq completion-preview--overlay (make-overlay pos pos))
    (overlay-put completion-preview--overlay 'priority
                 completion-preview-overlay-priority)
    (overlay-put completion-preview--overlay 'window (selected-window)))
  (add-text-properties 0 1 '(cursor 1) string)
  (overlay-put completion-preview--overlay 'after-string string)
  (mapc #'face-remap-remove-relative completion-preview--face-remap-cookie-jar)
  (setq completion-preview--face-remap-cookie-jar
        (when (and completion-preview-adapt-background-color (< (point-min) pos))
          (mapcar (lambda (face)
                    (face-remap-add-relative
                     face `(:background ,(completion-preview--bg-color (1- pos)))))
                  (ensure-list completion-preview-adapt-background-color))))
  completion-preview--overlay)

(defsubst completion-preview--propertize-for-mouse (str)
  "`propertize' STR, a completion suggestion, with mouse-related properties."
  (propertize str
              'mouse-face 'completion-preview-highlight
              'help-echo "click to accept, scroll to cycle"
              'keymap completion-preview--mouse-map))

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
      (progn
        (add-hook 'window-selection-change-functions
                  #'completion-preview--window-selection-change nil t)
        ;; Give keymap precedence over other minor mode maps.
        ;; TODO: Use explicit minor mode precedence instead when
        ;; implemented (bug#74492).
        (setf (alist-get 'completion-preview-active-mode
                         minor-mode-overriding-map-alist)
              completion-preview-active-mode-map))
    (remove-hook 'window-selection-change-functions
                 #'completion-preview--window-selection-change t)
    (completion-preview-hide)))

(defvar completion-preview-completion-styles '(basic)
  "List of completion styles that Completion Preview mode uses.

Since Completion Preview mode shows prefix completion candidates, this
list should normally only include completion styles that perform prefix
completion, but other candidates are filtered out and cause no harm.

See also `completion-styles'.")

(defun completion-preview--try-table (table beg end props)
  "Check TABLE for a completion matching the text between BEG and END.

PROPS is a property list with additional information about TABLE.
See `completion-at-point-functions' for more details.

If TABLE contains a matching candidate, return a list
\(BASE COMMON SUFFIXES) where BASE is a prefix of the text
between BEG and END that TABLE elided from the start of each candidate,
COMMON is the longest common prefix of all matching candidates,
SUFFIXES is a list of different suffixes that together with COMMON yield
the matching candidates.  If TABLE does not contain matching
candidates or if there are multiple matching completions and
`completion-preview-exact-match-only' is non-nil, return nil instead."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                                  ;;
  ;;   | buffer text |  preview  |                                    ;;
  ;;   |             |           |                                    ;;
  ;;  beg           end          |                                    ;;
  ;;   |------+------|--+--------|    Each of base, common and suffix ;;
  ;;   | base |  common | suffix | <- may be empty, except common and ;;
  ;;                                  suffix cannot both be empty.    ;;
  ;;                                                                  ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (let* ((pred (plist-get props :predicate))
         (string (buffer-substring beg end))
         (completion-ignore-case completion-preview-ignore-case)
         (completion-extra-properties props)
         (md (completion-metadata string table pred))
         (sort-fn (or (completion-metadata-get md 'cycle-sort-function)
                      (completion-metadata-get md 'display-sort-function)
                      completion-preview-sort-function))
         (all (let ((completion-lazy-hilit t)
                    ;; FIXME: This does not override styles prescribed
                    ;; by the completion category via
                    ;; e.g. `completion-category-defaults'.
                    (completion-styles completion-preview-completion-styles))
                (completion-all-completions string table pred
                                            (- (point) beg) md)))
         (last (last all))
         (base (or (cdr last) 0))
         (prefix (substring string base)))
    (when last
      (setcdr last nil)
      (when-let* ((sorted (funcall sort-fn
                                   (delete prefix (all-completions prefix all))))
                  (common (try-completion prefix sorted))
                  (lencom (length common))
                  (suffixes sorted))
        (unless (and (cdr suffixes) completion-preview-exact-match-only)
          ;; Remove the common prefix from each candidate.
          (while sorted
            (setcar sorted (substring (car sorted) lencom))
            (setq sorted (cdr sorted)))
          (list (substring string 0 base) common suffixes))))))

(defun completion-preview--capf-wrapper (capf)
  "Translate return value of CAPF to properties for completion preview overlay."
  (let ((res (ignore-errors (funcall capf))))
    (and (consp res)
         (not (functionp res))
         (seq-let (beg end table &rest plist) res
           (or (when-let* ((data (completion-preview--try-table
                                  table beg end plist)))
                 `(,(+ beg (length (car data))) ,end ,plist ,@data))
               (unless (eq 'no (plist-get plist :exclusive))
                 ;; Return non-nil to exclude other capfs.
                 '(nil)))))))

(defun completion-preview--update ()
  "Update completion preview."
  (seq-let (beg end props base common suffixes)
      (run-hook-wrapped
       'completion-at-point-functions
       #'completion-preview--capf-wrapper)
    (when-let* ((suffix (car suffixes))
                (inhibit-quit t))
      ;; Critical section, do not quit upon receiving input here.
      (set-text-properties 0 (length suffix)
                           (list 'face (if (cdr suffixes)
                                           'completion-preview
                                         'completion-preview-exact))
                           suffix)
      (set-text-properties 0 (length common)
                           (list 'face (if (cdr suffixes)
                                           'completion-preview-common
                                         'completion-preview-exact))
                           common)
      (let ((ov (completion-preview--make-overlay
                 end (completion-preview--propertize-for-mouse
                      (concat (substring common (- end beg)) suffix)))))
        (overlay-put ov 'completion-preview-beg beg)
        (overlay-put ov 'completion-preview-end end)
        (overlay-put ov 'completion-preview-index 0)
        (overlay-put ov 'completion-preview-suffixes suffixes)
        (overlay-put ov 'completion-preview-common common)
        (overlay-put ov 'completion-preview-base base)
        (overlay-put ov 'completion-preview-props props)
        (completion-preview-active-mode)))))

(defun completion-preview--try-update ()
  "Try to update completion preview, but give up as soon as input arrives."
  (while-no-input (completion-preview--update)))

(defun completion-preview--update-from-timer (window buffer)
  "Update completion preview if WINDOW and BUFFER are current."
  (when (and (eq (selected-window) window) (eq (current-buffer) buffer))
    (completion-preview--try-update)))

(defvar-local completion-preview--timer nil
  "Idle timer for updating the completion preview.")

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
           (end (max (point) (overlay-start completion-preview--overlay)))
           (sufs (completion-preview--get 'completion-preview-suffixes))
           (index (completion-preview--get 'completion-preview-index))
           (common (completion-preview--get 'completion-preview-common))
           (suffix (nth index sufs))
           (cand nil))
      (set-text-properties 0 (length suffix)
                           (list 'face (if (cdr sufs)
                                           'completion-preview
                                         'completion-preview-exact))
                           suffix)
      (setq cand (concat common (nth index sufs)))
      (if (and (<= beg (point) end (1- (+ beg (length cand))))
               (string-prefix-p (buffer-substring beg end) cand))
          ;; The previous preview is still applicable, update it.
          (overlay-put (completion-preview--make-overlay
                        end (completion-preview--propertize-for-mouse
                             (substring cand (- end beg))))
                       'completion-preview-end end)
        ;; The previous preview is no longer applicable, hide it.
        (completion-preview-active-mode -1))))
  ;; Run `completion-at-point-functions' to get a new candidate.
  (if completion-preview-idle-delay
      (setq completion-preview--timer
            (run-with-idle-timer completion-preview-idle-delay
                                 nil #'completion-preview--update-from-timer
                                 (selected-window) (current-buffer)))
    (completion-preview--try-update)))

(defun completion-preview--post-command ()
  "Create, update or delete completion preview post last command."
  (let ((internal-p (or completion-preview--inhibit-update-p
                        (memq this-command
                              completion-preview--internal-commands))))
    (setq completion-preview--inhibit-update-p nil)

    (when (timerp completion-preview--timer)
      (cancel-timer completion-preview--timer)
      (setq completion-preview--timer nil))

    (cond
     (internal-p
      ;; `this-command' took care of updating the preview.  Do nothing.
      )
     ((and (completion-preview-require-certain-commands)
           (completion-preview-require-minimum-symbol-length)
           (not buffer-read-only))
      ;; All conditions met.  Show or update the preview.
      (completion-preview--show))
     (completion-preview-active-mode
      ;; The preview is shown, but it shouldn't be.  Hide it.
      (completion-preview-active-mode -1)))))

(defun completion-preview--barf-if-no-preview ()
  "Signal a `user-error' if completion preview is not active."
  (unless completion-preview-active-mode
    (user-error "No current completion preview")))

(defun completion-preview-insert ()
  "Insert the completion candidate that the preview is showing."
  (interactive nil completion-preview-active-mode)
  (completion-preview--barf-if-no-preview)
  (let* ((pre (completion-preview--get 'completion-preview-base))
         (end (completion-preview--get 'completion-preview-end))
         (ind (completion-preview--get 'completion-preview-index))
         (all (completion-preview--get 'completion-preview-suffixes))
         (com (completion-preview--get 'completion-preview-common))
         (efn (plist-get (completion-preview--get 'completion-preview-props)
                         :exit-function))
         (aft (completion-preview--get 'after-string))
         (str (concat pre com (nth ind all))))
    (completion-preview-active-mode -1)
    (goto-char end)
    (insert-and-inherit (substring-no-properties aft))
    (when (functionp efn) (funcall efn str 'finished))))

(defun completion-preview-partial-insert (fun &rest args)
  "Insert part of the current completion preview candidate.

This function calls FUN with arguments ARGS, after temporarily inserting
the entire current completion preview candidate.  FUN should move point:
if it moves point forward into the completion text, this function
inserts the prefix of the completion candidate up to that point.
Beyond moving point, FUN should not modify the current buffer."
  (completion-preview--barf-if-no-preview)
  (let* ((end (completion-preview--get 'completion-preview-end))
         (aft (completion-preview--get 'after-string))
         (eoc (+ end (length aft))))
    ;; Keep region active, if it is already.  This lets commands that
    ;; call this function interact correctly with `shift-select-mode'.
    (let ((deactivate-mark nil))
      ;; Partially insert current completion candidate.
      (catch 'abort-atomic-change
        (atomic-change-group
          (let ((change-group (prepare-change-group)))
            (save-excursion
              (goto-char end)
              ;; Temporarily insert the full completion candidate.
              (insert-and-inherit (substring-no-properties aft)))
            ;; Set point to the end of the prefix that we want to keep.
            (apply fun args)
            (unless (< end (point))
              ;; Point didn't advance into the completion, so abort change
              ;; to avoid littering `buffer-undo-list' with a nop entry.
              (throw 'abort-atomic-change nil))
            ;; Delete the rest.
            (delete-region (min (point) eoc) eoc)
            ;; Combine into one change group.
            (undo-amalgamate-change-group change-group)))))
    ;; Cleanup.
    (cond
     ;; If we kept the entire completion candidate, call :exit-function.
     ((<= eoc (point))
      (let* ((pre (completion-preview--get 'completion-preview-base))
             (ind (completion-preview--get 'completion-preview-index))
             (all (completion-preview--get 'completion-preview-suffixes))
             (com (completion-preview--get 'completion-preview-common))
             (efn (plist-get
                   (completion-preview--get 'completion-preview-props)
                   :exit-function)))
        (completion-preview-active-mode -1)
        (when (functionp efn) (funcall efn (concat pre com (nth ind all))
                                       'finished))))
     ;; If we kept anything, update preview overlay accordingly.
     ((< end (point))
      (completion-preview--inhibit-update)
      (overlay-put (completion-preview--make-overlay
                    (point)
                    (completion-preview--propertize-for-mouse
                     (substring aft (- (point) end))))
                   'completion-preview-end (point)))
     ;; If we kept nothing, do nothing.
     )))

(defun completion-preview-insert-word (&optional n)
  "Insert the first N words of the current completion preview candidate.

Interactively, N is the numeric prefix argument, and it defaults to 1."
  (interactive "^p" completion-preview-active-mode)
  (completion-preview-partial-insert #'forward-word n))

(defun completion-preview-insert-sexp (&optional n)
  "Insert the first N s-expressions of the current completion preview candidate.

Interactively, N is the numeric prefix argument, and it defaults to 1."
  (interactive "^p" completion-preview-active-mode)
  (completion-preview-partial-insert #'forward-sexp n 'interactive))

(defun completion-preview-complete ()
  "Complete up to the longest common prefix of all completion candidates.

If you call this command twice in a row, or otherwise if there is no
common prefix to insert, it displays the list of matching completion
candidates unless `completion-auto-help' is nil.  If you repeat this
command again when the completions list is visible, it scrolls the
completions list."
  (interactive nil completion-preview-active-mode)
  (completion-preview--barf-if-no-preview)
  (let* ((beg (completion-preview--get 'completion-preview-beg))
         (end (completion-preview--get 'completion-preview-end))
         (com (completion-preview--get 'completion-preview-common))
         (cur (completion-preview--get 'completion-preview-index))
         (all (completion-preview--get 'completion-preview-suffixes))
         (base (completion-preview--get 'completion-preview-base))
         (props (completion-preview--get 'completion-preview-props))
         (efn (plist-get props :exit-function))
         (ins (substring-no-properties com (- end beg))))
    (goto-char end)
    (if (string-empty-p ins)
        ;; If there's nothing to insert, call `completion-at-point' to
        ;; show the completions list (or just display a message when
        ;; `completion-auto-help' is nil).
        (let* ((completion-styles completion-preview-completion-styles)
               (sub (substring-no-properties com))
               (col (mapcar (lambda (suf)
                              (concat sub (substring-no-properties suf)))
                            (append (nthcdr cur all) (take cur all))))
               ;; The candidates are already in order.
               (props (plist-put props :display-sort-function #'identity))
               ;; The :exit-function might be slow, e.g. when the
               ;; backend is Eglot, so we ensure that the preview is
               ;; hidden before any original :exit-function is called.
               (props (plist-put props :exit-function
                                 (when (functionp efn)
                                   (lambda (string status)
                                     (completion-preview-active-mode -1)
                                     (funcall efn string status)))))
               ;; The predicate is meant for the original completion
               ;; candidates, which may be symbols or cons cells, but
               ;; now we only have strings, so it might not be applicable.
               (props (plist-put props :predicate nil))
               (completion-at-point-functions
                (list (lambda () `(,beg ,end ,col ,@props)))))
          (completion-preview--inhibit-update)
          (completion-at-point))
      ;; Otherwise, insert the common prefix and update the preview.
      (insert-and-inherit ins)
      (let ((suf (nth cur all))
            (pos (point)))
        (if (or (string-empty-p suf) (null suf))
            ;; If we've inserted a full candidate, let the post-command
            ;; hook update the completion preview in case the candidate
            ;; can be completed further.
            (when (functionp efn)
              ;; Remove stale preview since `efn' can make arbitrary
              ;; text and point modifications that might interfere with
              ;; a subsequent preview update.  See bug#76606.
              (completion-preview-active-mode -1)
              (funcall efn (concat base com) (if (cdr all) 'exact 'finished)))
          ;; Otherwise, remove the common prefix from the preview.
          (completion-preview--inhibit-update)
          (overlay-put (completion-preview--make-overlay
                        pos (completion-preview--propertize-for-mouse suf))
                       'completion-preview-end pos))))))

(defun completion-preview-prev-candidate (n)
  "Cycle the candidate the preview is showing N candidates backward.

If N is negative, cycle -N candidates forward.  Interactively, N is the
prefix argument and defaults to 1."
  (interactive "p" completion-preview-active-mode)
  (completion-preview-next-candidate (- n)))

(defun completion-preview-next-candidate (n)
  "Cycle the candidate the preview is showing N candidates forward.

If N is negative, cycle -N candidates backward.  Interactively, N is the
prefix argument and defaults to 1."
  (interactive "p" completion-preview-active-mode)
  (when completion-preview-active-mode
    (let* ((beg (completion-preview--get 'completion-preview-beg))
           (end (completion-preview--get 'completion-preview-end))
           (all (completion-preview--get 'completion-preview-suffixes))
           (com (completion-preview--get 'completion-preview-common))
           (cur (completion-preview--get 'completion-preview-index))
           (len (length all))
           (new (mod (+ cur n) len))
           (suf (nth new all))
           (lencom (length com)))
      ;; Skip suffixes that are no longer applicable.  This may happen
      ;; when the user continues typing and immediately runs this
      ;; command, before the completion backend returns an updated set
      ;; of completions for the new (longer) prefix, so we still have
      ;; the previous (larger) set of candidates at hand.
      (while (or (<= (+ beg lencom (length suf)) end)
                 (not (string-prefix-p (buffer-substring beg end)
                                       (concat com suf))))
        (setq new (mod (+ new n) len)
              suf (nth new all)))
      (set-text-properties 0 (length suf)
                           (list 'face (if (cdr all)
                                           'completion-preview
                                         'completion-preview-exact))
                           suf)
      (let ((aft (completion-preview--propertize-for-mouse
                  (substring (concat com suf) (- end beg)))))
        (add-text-properties 0 1 '(cursor 1) aft)
        (overlay-put completion-preview--overlay 'completion-preview-index new)
        (overlay-put completion-preview--overlay 'after-string aft))
      (when completion-preview-message-format
        (message (format-spec completion-preview-message-format
                              `((?i . ,(1+ new)) (?n . ,len))))))))

(defun completion-preview-active-p (_symbol buffer)
  "Check if the completion preview is currently shown in BUFFER.

The first argument, SYMBOL, is ignored.  You can use this function as
the `completion-predicate' property of commands that you define that
should only be available when the completion preview is active."
  (declare
   (obsolete "check for `completion-preview-active-mode' instead." "31.1"))
  (buffer-local-value 'completion-preview-active-mode buffer))

;;;###autoload
(define-minor-mode completion-preview-mode
  "Show in-buffer completion suggestions in a preview as you type.

This mode automatically shows and updates the completion preview
according to the text around point.
\\<completion-preview-active-mode-map>\
When the preview is visible, \\[completion-preview-insert] accepts the
completion suggestion, \\[completion-preview-complete] completes up to
the longest common prefix of all completion candidates,
\\[completion-preview-next-candidate] cycles forward to the next
completion suggestion, and \\[completion-preview-prev-candidate] cycles
backward."
  :lighter " CP"
  (if completion-preview-mode
      (add-hook 'post-command-hook #'completion-preview--post-command nil t)
    (remove-hook 'post-command-hook #'completion-preview--post-command t)
    (when completion-preview-active-mode (completion-preview-active-mode -1))
    (when (timerp completion-preview--timer)
      (cancel-timer completion-preview--timer)
      (setq completion-preview--timer nil))))

;;;###autoload
(define-globalized-minor-mode global-completion-preview-mode
  completion-preview-mode completion-preview-mode
  :predicate '((not archive-mode
                    calc-mode
                    compilation-mode
                    diff-mode
                    dired-mode
                    image-mode
                    minibuffer-mode
                    minibuffer-inactive-mode
                    org-agenda-mode
                    special-mode
                    wdired-mode)
               t))

(provide 'completion-preview)
;;; completion-preview.el ends here
