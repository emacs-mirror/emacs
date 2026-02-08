;;; erc-status-sidebar.el --- HexChat-like activity overview for ERC  -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2020-2026 Free Software Foundation, Inc.

;; Author: Andrew Barbarello
;; Maintainer: Amin Bandali <bandali@gnu.org>, F. Jason Park <jp@neverwas.me>
;; URL: https://github.com/drewbarbs/erc-status-sidebar

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a HexChat-like sidebar for joined channels in
;; ERC.  It relies on the `erc-track' module, and displays all of the
;; same information that `erc-track' does in the mode line, but in an
;; alternative format in form of a sidebar.

;; Shout out to sidebar.el <https://github.com/sebastiencs/sidebar.el>
;; and outline-toc.el <https://github.com/abingham/outline-toc.el> for
;; the sidebar window management ideas.

;; Usage:

;; Use M-x erc-status-sidebar-open RET to open the ERC status sidebar
;; in the current frame.  Make sure that the `erc-track' module is
;; active (this is the default).

;; Use M-x erc-status-sidebar-close RET to close the sidebar on the
;; current frame.  With a prefix argument, it closes the sidebar on
;; all frames.

;; Use M-x erc-status-sidebar-kill RET to kill the sidebar buffer and
;; close the sidebar on all frames.

;; In addition to the commands above, you can also try the all-in-one
;; entry point `erc-bufbar-mode'.  See its doc string for usage.

;; If you want the status sidebar enabled whenever you use ERC, add
;; `bufbar' to `erc-modules'.  Note that this library also has a major
;; mode, `erc-status-sidebar-mode', which is for internal use.

;;; Code:

(require 'erc)
(require 'erc-track)
(require 'fringe)
(require 'seq)

(defgroup erc-status-sidebar nil
  "A responsive side window listing all connected ERC buffers.
More commonly known as a window list or \"buflist\", this side
panel displays clickable buffer names for switching to with the
mouse.  By default, ERC highlights the name corresponding to the
selected window's buffer, if any.  In this context, \"connected\"
just means associated with the same IRC session, even one that
has ceased communicating with its server.  For information on how
the window itself works, see Info node `(elisp) Side Windows'."
  :group 'erc)

(defcustom erc-status-sidebar-buffer-name "*ERC Status*"
  "Name of the sidebar buffer."
  :type 'string)

(defcustom erc-status-sidebar-mode-line-format "ERC Status"
  "Mode line format for the status sidebar."
  :type 'string)

(defcustom erc-status-sidebar-header-line-format nil
  "Header line format for the status sidebar."
  :type '(choice (const :tag "No header line" nil)
                 string))

(defcustom erc-status-sidebar-width 15
  "Default width of the sidebar (in columns)."
  :type 'number)

(defcustom erc-status-sidebar-channel-sort
  'erc-status-sidebar-default-chansort
  "Sorting function used to determine order of channels in the sidebar."
  :type 'function)

(defcustom erc-status-sidebar-channel-format
  'erc-status-sidebar-default-chan-format
  "Function used to format channel names for display in the sidebar.
Only consulted for certain values of `erc-status-sidebar-style'."
  :type 'function)

(defcustom erc-status-sidebar-highlight-active-buffer t
  "Whether to highlight the selected window's buffer in the sidebar.
ERC uses the same instance across all frames.  May not be
compatible with all values of `erc-status-sidebar-style'."
  :package-version '(ERC . "5.6")
  :type 'boolean)

(defcustom erc-status-sidebar-style 'all-queries-first
  "Preset style for rendering the sidebar.

When set to `channels-only', ERC limits the items in the
status bar to uniquified channels.  It uses the options
and functions

  `erc-channel-list',
  `erc-status-sidebar-channel-sort',
  `erc-status-sidebar-get-channame',
  `erc-status-sidebar-channel-format'
  `erc-status-sidebar-default-insert'

for selecting, formatting, naming, and inserting entries.  When
set to one of the various `all-*' values, such as `all-mixed',
ERC shows channels and queries under their respective server
buffers, using the functions

  `erc-status-sidebar-all-target-buffers',
  `erc-status-sidebar-default-allsort',
  `erc-status-sidebar-prefer-target-as-name',
  `erc-status-sidebar-default-chan-format',
  `erc-status-sidebar-pad-hierarchy'

for the above-mentioned purposes.  ERC also accepts a list of
functions to perform these roles a la carte.  Since the members
of the above sets aren't really interoperable, we don't offer
them here as customization choices, but you can still specify
them manually.  See doc strings for a description of their
expected arguments and return values."
  :package-version '(ERC . "5.6")
  :type '(choice (const channels-only)
                 (const all-mixed)
                 (const all-queries-first)
                 (const all-channels-first)
                 (list (function :tag "Buffer lister")
                       (function :tag "Buffer sorter")
                       (function :tag "Name extractor")
                       (function :tag "Name formatter")
                       (function :tag "Name inserter"))))

(defcustom erc-status-sidebar-click-display-action t
  "How to display a buffer when clicked.
Values can be anything recognized by `display-buffer' for its
ACTION parameter."
  :package-version '(ERC . "5.6")
  :type '(choice (const :tag "Always use/create other window" t)
                 (const :tag "Let `display-buffer' decide" nil)
                 (const :tag "Same window" (display-buffer-same-window
                                            (inhibit-same-window . nil)))
                 (cons :tag "Action"
                       (choice function (repeat function))
                       (alist :tag "Action arguments"
                              :key-type symbol
                              :value-type (sexp :tag "Value")))))

(defvar erc-status-sidebar--singular-p t
  "Whether to restrict the sidebar to a single frame.
This variable only affects `erc-bufbar-mode'.  Disabling it does
not arrange for automatically showing the sidebar in all frames.
Rather, disabling it allows for displaying the sidebar in the
selected frame even if it's already showing in some other frame.")

(defvar hl-line-mode)
(declare-function hl-line-highlight "hl-line" nil)

(defun erc-status-sidebar-display-window ()
  "Display the status buffer in a side window.  Return the new window."
  (display-buffer
   (erc-status-sidebar-get-buffer)
   `(display-buffer-in-side-window . ((side . left)
                                      (window-width . ,erc-status-sidebar-width)))))

(defun erc-status-sidebar-get-window (&optional no-creation)
  "Return the created/existing window displaying the status buffer.

If NO-CREATION is non-nil, the window is not created."
  (let ((sidebar-window (get-buffer-window erc-status-sidebar-buffer-name
                                           erc-status-sidebar--singular-p)))
    (unless (or sidebar-window no-creation)
      (with-current-buffer (erc-status-sidebar-get-buffer)
        (setq vertical-scroll-bar nil
              cursor-type nil))
      (setq sidebar-window (erc-status-sidebar-display-window))
      (set-window-dedicated-p sidebar-window t)
      (set-window-parameter sidebar-window 'no-delete-other-windows t)
      ;; Don't cycle to this window with `other-window'.
      (set-window-parameter sidebar-window 'no-other-window t)
      (set-window-fringes sidebar-window 0 0)
      ;; Set a custom display table so the window doesn't show a
      ;; truncation symbol when a channel name is too big.
      (let ((dt (make-display-table)))
        (set-window-display-table sidebar-window dt)
        (set-display-table-slot dt 'truncation ?\ )))
    sidebar-window))

(defun erc-status-sidebar-buffer-exists-p ()
  "Check if the sidebar buffer exists."
  (get-buffer erc-status-sidebar-buffer-name))

(defun erc-status-sidebar-get-buffer ()
  "Return the sidebar buffer, creating it if it doesn't exist."
  (get-buffer-create erc-status-sidebar-buffer-name))

(defun erc-status-sidebar-close (&optional all-frames)
  "Close the sidebar.

If called with prefix argument (ALL-FRAMES non-nil), the sidebar
will be closed on all frames.

The erc-status-sidebar buffer is left alone, but the window
containing it on the current frame is closed.  See
`erc-status-sidebar-kill'."
  (interactive "P")
  (mapcar #'delete-window ; FIXME use `mapc'.
          (get-buffer-window-list (erc-status-sidebar-get-buffer)
                                  nil (if all-frames t))))

(defmacro erc-status-sidebar-writable (&rest body)
  "Make the status buffer writable while executing BODY."
  `(let ((buffer-read-only nil))
     ,@body))

(defun erc-status-sidebar--open ()
  "Maybe open the sidebar, respecting `erc-status-sidebar--singular-p'."
  (save-excursion
    (if (erc-status-sidebar-buffer-exists-p)
        (erc-status-sidebar-get-window)
      (with-current-buffer (erc-status-sidebar-get-buffer)
        (erc-status-sidebar-mode)
        (erc-status-sidebar-refresh)))))

;;;###autoload(autoload 'erc-bufbar-mode "erc-status-sidebar" nil t)
(define-erc-module bufbar nil
  "Show `erc-track'-like activity in a side window.
When enabling, show the sidebar immediately in the current frame
if called from a connected ERC buffer.  Otherwise, arrange for
doing so on connect or whenever next displaying a new ERC buffer.
When disabling, hide the status window in all frames.  With a
negative prefix arg, also shutdown the session.  Normally, this
module only allows one sidebar window in an Emacs session.  To
override this, use `erc-status-sidebar-open' to force creation
and `erc-status-sidebar-close' to hide a single instance on the
current frame only."
  ((unless erc-track-mode
     (unless (memq 'track erc-modules)
       (erc--warn-once-before-connect 'erc-bufbar-mode
         "Module `bufbar' needs global module `track'. Enabling now."
         " This will affect \C-]all\C-] ERC sessions."
         " Add `track' to `erc-modules' to silence this message."))
     (erc-track-mode +1))
   (add-hook 'erc--setup-buffer-hook #'erc-status-sidebar--open)
   ;; Preserve side-window dimensions after `custom-buffer-done'.
   (when-let* (((not erc--updating-modules-p))
               (buf (or (and (derived-mode-p 'erc-mode) (current-buffer))
                        (car (erc-buffer-filter
                              (lambda () erc-server-connected))))))
     (with-current-buffer buf
       (erc-status-sidebar--open))))
  ((remove-hook 'erc--setup-buffer-hook #'erc-status-sidebar--open)
   (erc-status-sidebar-close 'all-frames)
   (when-let* ((arg erc--module-toggle-prefix-arg)
               ((numberp arg))
               ((< arg 0)))
     (erc-status-sidebar-kill))))

;;;###autoload
(defun erc-status-sidebar-open ()
  "Open or create a sidebar window in the current frame.
When `erc-bufbar-mode' is active, do this even if one already
exists in another frame."
  (interactive)
  (let ((erc-status-sidebar--singular-p (not erc-bufbar-mode)))
    (erc-status-sidebar--open)))

;;;###autoload
(defun erc-status-sidebar-toggle ()
  "Toggle the sidebar open/closed on the current frame.
When opening, and `erc-bufbar-mode' is active, create a sidebar
even if one already exists in another frame."
  (interactive)
  (if (get-buffer-window erc-status-sidebar-buffer-name nil)
      (erc-status-sidebar-close)
    (erc-status-sidebar-open)))

(defun erc-status-sidebar-get-channame (buffer)
  "Return name of BUFFER with all leading \"#\" characters removed."
  (let ((s (buffer-name buffer)))
    (if (string-match "^#\\{1,2\\}" s)
        (setq s (replace-match "" t t s)))
    (downcase s)))

(defun erc-status-sidebar-default-chansort (chanlist)
  "Sort CHANLIST case-insensitively for display in the sidebar."
  (sort chanlist (lambda (x y)
                   (string< (erc-status-sidebar-get-channame x)
                            (erc-status-sidebar-get-channame y)))))

(defvar erc-status-sidebar--trimpat nil)
(defvar erc-status-sidebar--prechan nil)

(defun erc-status-sidebar-prefer-target-as-name (buffer)
  "Return some name to represent buffer in the sidebar."
  (if-let* ((target (buffer-local-value 'erc--target buffer)))
      (cond ((and erc-status-sidebar--trimpat (erc--target-channel-p target))
             (string-trim-left (erc--target-string target)
                               erc-status-sidebar--trimpat))
            ((and erc-status-sidebar--prechan (erc--target-channel-p target))
             (concat erc-status-sidebar--prechan
                     (erc--target-string target)))
            (t (erc--target-string target)))
    (buffer-name buffer)))

;; This could be converted into an option if people want.
(defvar erc-status-sidebar--show-disconnected t)

(defun erc-status-sidebar-all-target-buffers (process)
  (erc-buffer-filter (lambda ()
                       (and erc--target
                            (or erc-status-sidebar--show-disconnected
                                (erc-server-process-alive))))
                     process))

;; FIXME profile this.  Rebuilding the graph every time track updates
;; seems wasteful for occasions where server messages are processed
;; unthrottled, such as during history playback.  If it's a problem,
;; we should look into rewriting this using `ewoc' or some other
;; solution that maintains a persistent model.
(defun erc-status-sidebar-default-allsort (target-buffers)
  "Return a list of servers interspersed with their targets."
  (mapcan (pcase-lambda (`(,proc . ,chans))
            (cons (process-buffer proc)
                  (let ((erc-status-sidebar--trimpat
                         (and (eq erc-status-sidebar-style 'all-mixed)
                              (with-current-buffer (process-buffer proc)
                                (when-let* ((ch-pfxs (erc--get-isupport-entry
                                                      'CHANTYPES 'single)))
                                  (regexp-quote ch-pfxs)))))
                        (erc-status-sidebar--prechan
                         (and (eq erc-status-sidebar-style
                                  'all-queries-first)
                              "\C-?")))
                    (sort chans
                          (lambda (x y)
                            (string<
                             (erc-status-sidebar-prefer-target-as-name x)
                             (erc-status-sidebar-prefer-target-as-name y)))))))
          (sort (seq-group-by (lambda (b)
                                (buffer-local-value 'erc-server-process b))
                              target-buffers)
                (lambda (a b)
                  (string< (buffer-name (process-buffer (car a)))
                           (buffer-name (process-buffer (car b))))))))

(defvar-local erc-status-sidebar--active-marker nil
  "Marker indicating currently active buffer.")

(defun erc-status-sidebar--set-active-line (erc-buffer)
  (when (and erc-status-sidebar-highlight-active-buffer
             (eq (window-buffer (and (minibuffer-window-active-p
                                      (selected-window))
                                     (minibuffer-selected-window)))
                 erc-buffer))
    (set-marker erc-status-sidebar--active-marker (point))))

(defun erc-status-sidebar-default-insert (channame chanbuf _chanlist)
  "Insert CHANNAME followed by a newline.
Maybe arrange to highlight line if CHANBUF is showing in the
focused window."
  (erc-status-sidebar--set-active-line chanbuf)
  (insert channame "\n"))

(defun erc-status-sidebar-pad-hierarchy (bufname buffer buflist)
  "Prefix BUFNAME to emphasize BUFFER's role in BUFLIST."
  (if (and (buffer-live-p buffer) (buffer-local-value 'erc--target buffer))
      (insert " ")
    (unless (eq buffer (car buflist))
      (insert "\n"))) ;  ^L
  (when bufname
    (erc-status-sidebar--set-active-line buffer))
  (insert (or bufname
              (and-let* (((not (buffer-live-p buffer)))
                         (next (cadr (member buffer buflist)))
                         ((buffer-live-p next))
                         (proc (buffer-local-value 'erc-server-process next))
                         (id (process-get proc 'erc-networks--id))
                         ((erc-networks--id-string id))))
              "???")
          "\n"))

(defun erc-status-sidebar-default-chan-format (channame
                                               &optional num-messages erc-face)
  "Format CHANNAME for display in the sidebar.

If NUM-MESSAGES is non-nil, append it to the channel name.  If
ERC-FACE is non-nil, apply it to channel name.  If it is equal to
`erc-default-face', also apply bold property to make the channel
name stand out."
  (when num-messages
    (setq channame (format "%s [%d]" channame num-messages)))
  (when erc-face
    (put-text-property 0 (length channame) 'face erc-face channame)
    (when (eq erc-face 'erc-default-face)
      (add-face-text-property 0 (length channame) 'bold t channame)))
  channame)

(defun erc-status-sidebar-refresh ()
  "Update the content of the sidebar."
  (interactive)
  (pcase-let* ((`(,list-fn ,sort-fn ,name-fn ,fmt-fn ,insert-fn)
                (pcase erc-status-sidebar-style
                  ('channels-only (list #'erc-channel-list
                                        erc-status-sidebar-channel-sort
                                        #'erc-status-sidebar-get-channame
                                        erc-status-sidebar-channel-format
                                        #'erc-status-sidebar-default-insert))
                  ((or 'all-mixed 'all-queries-first 'all-channels-first)
                   '(erc-status-sidebar-all-target-buffers
                     erc-status-sidebar-default-allsort
                     erc-status-sidebar-prefer-target-as-name
                     erc-status-sidebar-default-chan-format
                     erc-status-sidebar-pad-hierarchy))
                  (v v)))
               (chanlist (apply sort-fn (funcall list-fn nil) nil))
               (windows nil))
    (with-current-buffer (erc-status-sidebar-get-buffer)
      (dolist (window (get-buffer-window-list nil nil t))
        (push (cons window (window-start window)) windows))
      (erc-status-sidebar-writable
       (delete-region (point-min) (point-max))
       (goto-char (point-min))
       (if erc-status-sidebar--active-marker
           (set-marker erc-status-sidebar--active-marker nil)
         (setq erc-status-sidebar--active-marker (make-marker)))
       (dolist (chanbuf chanlist)
         (let* ((tup (seq-find (lambda (tup) (eq (car tup) chanbuf))
                               erc-modified-channels-alist))
                (count (if tup (cadr tup)))
                (face (if tup (cddr tup)))
                (face (if (or (not (buffer-live-p chanbuf))
                              (not (erc-server-process-alive chanbuf)))
                          `(shadow ,face)
                        face))
                (channame (apply fmt-fn
                                 (copy-sequence (funcall name-fn chanbuf))
                                 count face nil))
                (cnlen (length channame)))
           (put-text-property 0 cnlen 'erc-buf chanbuf channame)
           (put-text-property 0 cnlen 'mouse-face 'highlight channame)
           (put-text-property
            0 cnlen 'help-echo
            "mouse-1: switch to buffer in other window" channame)
           (funcall insert-fn channame chanbuf chanlist)))
       (when windows
         (map-apply #'set-window-start windows))
       (when (and erc-status-sidebar-highlight-active-buffer
                  (marker-buffer erc-status-sidebar--active-marker))
         (goto-char erc-status-sidebar--active-marker)
         (require 'hl-line)
         (unless hl-line-mode (hl-line-mode +1))
         (hl-line-highlight))))))

(defun erc-status-sidebar-kill ()
  "Close the ERC status sidebar and its buffer."
  (interactive)
  (when (and erc-bufbar-mode (not erc--module-toggle-prefix-arg))
    (erc-bufbar-mode -1))
  (ignore-errors (kill-buffer erc-status-sidebar-buffer-name)))

(defun erc-status-sidebar-click (event)
  "Handle click EVENT in `erc-status-sidebar-mode-map'."
  (interactive "e")
  (save-excursion
    (let ((window (posn-window (event-start event)))
          (pos (posn-point (event-end event))))
      ;; Current buffer is "ERC Status" and its window is selected
      (cl-assert (eq major-mode 'erc-status-sidebar-mode))
      (cl-assert (eq (selected-window) window))
      (cl-assert (eq (window-buffer window) (current-buffer)))
      (when-let* ((buf (get-text-property pos 'erc-buf)))
        ;; Option operates relative to last selected window
        (select-window (get-mru-window nil nil 'not-selected))
        (pop-to-buffer buf erc-status-sidebar-click-display-action)))))

(defun erc-status-sidebar-scroll-up (lines)
  "Scroll sidebar buffer's content LINES linse upward.
If LINES is nil, scroll up a full screen's worth."
  (interactive "P")
  (let ((other-window-scroll-buffer (erc-status-sidebar-get-buffer)))
    (scroll-other-window lines)))

(defun erc-status-sidebar-scroll-down (lines)
  "Scroll sidebar buffer's content LINES lines downward.
If LINES is nil, scroll down a full screen's worth."
  (interactive "P")
  (let ((other-window-scroll-buffer (erc-status-sidebar-get-buffer)))
    (scroll-other-window-down lines)))

(defun erc-status-sidebar-recenter (arg)
  "Recenter the status sidebar.
Expect `erc-status-sidebar-highlight-active-buffer' to be non-nil
and to be invoked in a buffer matching the line currently
highlighted."
  (interactive "P")
  (let* ((buf (erc-status-sidebar-get-buffer))
         (win (get-buffer-window buf)))
    (with-current-buffer buf
      (when (and erc-status-sidebar--active-marker
                 (marker-position erc-status-sidebar--active-marker))
        (with-selected-window win
          (goto-char erc-status-sidebar--active-marker)
          (recenter arg t))))))

(defvar erc-status-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map [mouse-1] #'erc-status-sidebar-click)
    map))

(defvar erc-status-sidebar-refresh-triggers
  '(erc-track-list-changed-hook
    erc-join-hook
    erc-part-hook
    erc-kill-buffer-hook
    erc-kill-channel-hook
    erc-kill-server-hook
    erc-kick-hook
    erc-disconnected-hook
    erc-quit-hook)
  "Hooks to refresh the sidebar on.
This may be set locally in the status-sidebar buffer under
various conditions, like when the option
`erc-status-sidebar-highlight-active-buffer' is non-nil.")

(defvar erc-status-sidebar--highlight-refresh-triggers
  '(window-selection-change-functions)
  "Triggers enabled with `erc-status-sidebar-highlight-active-buffer'.")

(defun erc-status-sidebar--refresh-unless-input ()
  "Run `erc-status-sidebar-refresh' unless there are unread commands.
Also abstain when the user is interacting with the minibuffer."
  (unless (or (input-pending-p) (minibuffer-window-active-p (selected-window)))
    (erc-status-sidebar-refresh)))

(defun erc-status-sidebar--post-refresh (&rest _ignore)
  "Schedule sidebar refresh for execution after command stack is cleared.

Ignore arguments in IGNORE, allowing this function to be added to
hooks that invoke it with arguments."
  (run-at-time 0 nil #'erc-status-sidebar--refresh-unless-input))

(defun erc-status-sidebar-mode--unhook ()
  "Remove hooks installed by `erc-status-sidebar-mode'."
  (dolist (hk erc-status-sidebar-refresh-triggers)
    (remove-hook hk #'erc-status-sidebar--post-refresh))
  (remove-hook 'window-configuration-change-hook
               #'erc-status-sidebar-set-window-preserve-size))

(defun erc-status-sidebar-set-window-preserve-size ()
  "Tell Emacs to preserve the current height/width of the ERC sidebar window.

Note that preserve status needs to be reset when the window is
manually resized, so `erc-status-sidebar-mode' adds this function
to the `window-configuration-change-hook'."
  (when (and (eq (selected-window) (let (erc-status-sidebar--singular-p)
                                     (erc-status-sidebar-get-window)))
             (fboundp 'window-preserve-size))
    (unless (eq (window-total-width) (window-min-size nil t))
      (apply #'window-preserve-size (selected-window) t t nil))))

(define-derived-mode erc-status-sidebar-mode special-mode "ERC Sidebar"
  "Major mode for ERC status sidebar."
  ;; Users invoking M-x erc-status-sidebar-mode most likely expect to
  ;; summon the module's minor-mode, `erc-bufbar-mode'.
  :interactive nil
  ;; Don't scroll the buffer horizontally, if a channel name is
  ;; obscured then the window can be resized.
  (setq-local auto-hscroll-mode nil)
  (setq cursor-type nil
        buffer-read-only t
        mode-line-format erc-status-sidebar-mode-line-format
        header-line-format erc-status-sidebar-header-line-format)
  (erc-status-sidebar-set-window-preserve-size)

  (add-hook 'window-configuration-change-hook
            #'erc-status-sidebar-set-window-preserve-size nil t)
  (when erc-status-sidebar-highlight-active-buffer
    (setq-local erc-status-sidebar-refresh-triggers
                `(,@erc-status-sidebar--highlight-refresh-triggers
                  ,@erc-status-sidebar-refresh-triggers)))
  (dolist (hk erc-status-sidebar-refresh-triggers)
    (add-hook hk #'erc-status-sidebar--post-refresh))

  ;; `change-major-mode-hook' is run *before* the
  ;; erc-status-sidebar-mode initialization code, so it won't undo the
  ;; add-hook's we did in the previous expressions.
  (add-hook 'change-major-mode-hook #'erc-status-sidebar-mode--unhook nil t)
  (add-hook 'kill-buffer-hook #'erc-status-sidebar-mode--unhook nil t))

(provide 'erc-status-sidebar)
;;; erc-status-sidebar.el ends here

;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
