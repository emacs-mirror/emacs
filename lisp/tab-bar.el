;;; tab-bar.el --- frame-local tabs with named persistent window configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Juri Linkov <juri@linkov.net>
;; Keywords: frames tabs
;; Maintainer: emacs-devel@gnu.org

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

;; Provides `tab-bar-mode' to control display of the tab bar and
;; bindings for the global tab bar.

;; The normal global binding for [tab-bar] (below) uses the value of
;; `tab-bar-map' as the actual keymap to define the tab bar.  Modes
;; may either bind items under the [tab-bar] prefix key of the local
;; map to add to the global bar or may set `tab-bar-map'
;; buffer-locally to override it.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'seq))


(defgroup tab-bar nil
  "Frame-local tabs."
  :group 'convenience
  :version "27.1")

(defgroup tab-bar-faces '((tab-bar custom-face)) ; tab-bar is defined in faces.el
  "Faces used in the tab bar."
  :group 'tab-bar
  :group 'faces
  :version "27.1")

(defface tab-bar-tab
  '((default
      :inherit tab-bar)
    (((class color) (min-colors 88))
     :box (:line-width 1 :style released-button))
    (t
     :inverse-video nil))
  "Tab bar face for selected tab."
  :version "27.1"
  :group 'tab-bar-faces)

(defface tab-bar-tab-inactive
  '((default
      :inherit tab-bar-tab)
    (((class color) (min-colors 88))
     :background "grey75")
    (t
     :inverse-video t))
  "Tab bar face for non-selected tab."
  :version "27.1"
  :group 'tab-bar-faces)


(defcustom tab-bar-select-tab-modifiers '()
  "List of modifier keys for selecting a tab by its index digit.
Possible modifier keys are `control', `meta', `shift', `hyper', `super' and
`alt'.  To help you to select a tab by its number, you can customize
`tab-bar-tab-hints' that will show tab numbers alongside the tab name."
  :type '(set :tag "Tab selection modifier keys"
              (const control)
              (const meta)
              (const shift)
              (const hyper)
              (const super)
              (const alt))
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         ;; Reenable the tab-bar with new keybindings
         (tab-bar-mode -1)
         (tab-bar-mode 1))
  :group 'tab-bar
  :version "27.1")


(define-minor-mode tab-bar-mode
  "Toggle the tab bar in all graphical frames (Tab Bar mode)."
  :global t
  ;; It's defined in C/cus-start, this stops the d-m-m macro defining it again.
  :variable tab-bar-mode
  (let ((val (if tab-bar-mode 1 0)))
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'tab-bar-lines val))
    ;; If the user has given `default-frame-alist' a `tab-bar-lines'
    ;; parameter, replace it.
    (if (assq 'tab-bar-lines default-frame-alist)
        (setq default-frame-alist
              (cons (cons 'tab-bar-lines val)
                    (assq-delete-all 'tab-bar-lines
                                     default-frame-alist)))))

  (when (and tab-bar-mode tab-bar-new-button
             (not (get-text-property 0 'display tab-bar-new-button)))
    ;; This file is pre-loaded so only here we can use the right data-directory:
    (add-text-properties 0 (length tab-bar-new-button)
                         `(display (image :type xpm
                                          :file "tabs/new.xpm"
                                          :margin (2 . 0)
                                          :ascent center))
                         tab-bar-new-button))

  (when (and tab-bar-mode tab-bar-close-button
             (not (get-text-property 0 'display tab-bar-close-button)))
    ;; This file is pre-loaded so only here we can use the right data-directory:
    (add-text-properties 0 (length tab-bar-close-button)
                         `(display (image :type xpm
                                          :file "tabs/close.xpm"
                                          :margin (2 . 0)
                                          :ascent center))
                         tab-bar-close-button))

  (if tab-bar-mode
      (progn
        (when tab-bar-select-tab-modifiers
          (global-set-key (vector (append tab-bar-select-tab-modifiers (list ?0)))
                          'tab-bar-switch-to-recent-tab)
          (dotimes (i 9)
            (global-set-key (vector (append tab-bar-select-tab-modifiers
                                            (list (+ i 1 ?0))))
                            'tab-bar-select-tab)))
        ;; Don't override user customized key bindings
        (unless (global-key-binding [(control tab)])
          (global-set-key [(control tab)] 'tab-next))
        (unless (global-key-binding [(control shift tab)])
          (global-set-key [(control shift tab)] 'tab-previous))
        (unless (global-key-binding [(control shift iso-lefttab)])
          (global-set-key [(control shift iso-lefttab)] 'tab-previous)))
    ;; Unset only keys bound by tab-bar
    (when (eq (global-key-binding [(control tab)]) 'tab-next)
      (global-unset-key [(control tab)]))
    (when (eq (global-key-binding [(control shift tab)]) 'tab-previous)
      (global-unset-key [(control shift tab)]))
    (when (eq (global-key-binding [(control shift iso-lefttab)]) 'tab-previous)
      (global-unset-key [(control shift iso-lefttab)]))))

(defun tab-bar-handle-mouse (event)
  "Text-mode emulation of switching tabs on the tab bar.
This command is used when you click the mouse in the tab bar
on a console which has no window system but does have a mouse."
  (interactive "e")
  (let* ((x-position (car (posn-x-y (event-start event))))
         (keymap (lookup-key (cons 'keymap (nreverse (current-active-maps))) [tab-bar]))
         (column 0))
    (when x-position
      (unless (catch 'done
                (map-keymap
                 (lambda (key binding)
                   (when (eq (car-safe binding) 'menu-item)
                     (when (> (+ column (length (nth 1 binding))) x-position)
                       (if (get-text-property (- x-position column) 'close-tab (nth 1 binding))
                           (let* ((close-key (vector (intern (format "C-%s" key))))
                                  (close-def (lookup-key keymap close-key)))
                             (when close-def
                               (call-interactively close-def)))
                         (call-interactively (nth 2 binding)))
                       (throw 'done t))
                     (setq column (+ column (length (nth 1 binding))))))
                 keymap))
        ;; Clicking anywhere outside existing tabs will add a new tab
        (tab-bar-new-tab)))))

;; Used in the Show/Hide menu, to have the toggle reflect the current frame.
(defun toggle-tab-bar-mode-from-frame (&optional arg)
  "Toggle tab bar on or off, based on the status of the current frame.
See `tab-bar-mode' for more information."
  (interactive (list (or current-prefix-arg 'toggle)))
  (if (eq arg 'toggle)
      (tab-bar-mode (if (> (frame-parameter nil 'tab-bar-lines) 0) 0 1))
    (tab-bar-mode arg)))

(defvar tab-bar-map (make-sparse-keymap)
  "Keymap for the tab bar.
Define this locally to override the global tab bar.")

(global-set-key [tab-bar]
                `(menu-item ,(purecopy "tab bar") ignore
                            :filter tab-bar-make-keymap))

(defconst tab-bar-keymap-cache (make-hash-table :weakness t :test 'equal))

(defun tab-bar-make-keymap (&optional _ignore)
  "Generate an actual keymap from `tab-bar-map'.
Its main job is to show tabs in the tab bar."
  (if (= 1 (length tab-bar-map))
      (tab-bar-make-keymap-1)
    (let ((key (cons (frame-terminal) tab-bar-map)))
      (or (gethash key tab-bar-keymap-cache)
          (puthash key tab-bar-map tab-bar-keymap-cache)))))


(defcustom tab-bar-show t
  "Defines when to show the tab bar.
If t, enable `tab-bar-mode' automatically on using the commands that
create new window configurations (e.g. `tab-new').
If the value is `1', then hide the tab bar when it has only one tab,
and show it again once more tabs are created.
If nil, always keep the tab bar hidden.  In this case it's still
possible to use persistent named window configurations by relying on
keyboard commands `tab-new', `tab-close', `tab-next', `tab-switcher', etc."
  :type '(choice (const :tag "Always" t)
                 (const :tag "When more than one tab" 1)
                 (const :tag "Never" nil))
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (tab-bar-mode
          (if (or (eq val t)
                  (and (natnump val)
                       (> (length (funcall tab-bar-tabs-function)) val)))
              1 -1)))
  :group 'tab-bar
  :version "27.1")

(defcustom tab-bar-new-tab-choice t
  "Defines what to show in a new tab.
If t, start a new tab with the current buffer, i.e. the buffer
that was current before calling the command that adds a new tab
(this is the same what `make-frame' does by default).
If the value is a string, use it as a buffer name to switch to
if such buffer exists, or switch to a buffer visiting the file or
directory that the string specifies.  If the value is a function,
call it with no arguments and switch to the buffer that it returns.
If nil, duplicate the contents of the tab that was active
before calling the command that adds a new tab."
  :type '(choice (const     :tag "Current buffer" t)
                 (string    :tag "Buffer" "*scratch*")
                 (directory :tag "Directory" :value "~/")
                 (file      :tag "File" :value "~/.emacs")
                 (function  :tag "Function")
                 (const     :tag "Duplicate tab" nil))
  :group 'tab-bar
  :version "27.1")

(defcustom tab-bar-new-button-show t
  "If non-nil, show the \"New tab\" button in the tab bar.
When this is nil, you can create new tabs with \\[tab-new]."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (force-mode-line-update))
  :group 'tab-bar
  :version "27.1")

(defvar tab-bar-new-button " + "
  "Button for creating a new tab.")

(defcustom tab-bar-close-button-show t
  "Defines where to show the close tab button.
If t, show the close tab button on all tabs.
If `selected', show it only on the selected tab.
If `non-selected', show it only on non-selected tab.
If nil, don't show it at all."
  :type '(choice (const :tag "On all tabs" t)
                 (const :tag "On selected tab" selected)
                 (const :tag "On non-selected tabs" non-selected)
                 (const :tag "None" nil))
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (force-mode-line-update))
  :group 'tab-bar
  :version "27.1")

(defvar tab-bar-close-button
  (propertize " x"
              'close-tab t
              :help "Click to close tab")
  "Button for closing the clicked tab.")

(defvar tab-bar-back-button " < "
  "Button for going back in tab history.")

(defvar tab-bar-forward-button " > "
  "Button for going forward in tab history.")

(defcustom tab-bar-tab-hints nil
  "Show absolute numbers on tabs in the tab bar before the tab name.
This helps to select the tab by its number using `tab-bar-select-tab'
and `tab-bar-select-tab-modifiers'."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (force-mode-line-update))
  :group 'tab-bar
  :version "27.1")

(defvar tab-bar-separator nil
  "String that delimits tabs.")


(defcustom tab-bar-tab-name-function #'tab-bar-tab-name-current
  "Function to get a tab name.
Function gets no arguments.
The choice is between displaying only the name of the current buffer
in the tab name (default), or displaying the names of all buffers
from all windows in the window configuration."
  :type '(choice (const :tag "Selected window buffer"
                        tab-bar-tab-name-current)
                 (const :tag "Selected window buffer with window count"
                        tab-bar-tab-name-current-with-count)
                 (const :tag "Truncated buffer name"
                        tab-bar-tab-name-truncated)
                 (const :tag "All window buffers"
                        tab-bar-tab-name-all)
                 (function  :tag "Function"))
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (force-mode-line-update))
  :group 'tab-bar
  :version "27.1")

(defun tab-bar-tab-name-current ()
  "Generate tab name from the buffer of the selected window."
  (buffer-name (window-buffer (minibuffer-selected-window))))

(defun tab-bar-tab-name-current-with-count ()
  "Generate tab name from the buffer of the selected window.
Also add the number of windows in the window configuration."
  (let ((count (length (window-list-1 nil 'nomini)))
        (name (window-buffer (minibuffer-selected-window))))
    (if (> count 1)
        (format "%s (%d)" name count)
      (format "%s" name))))

(defun tab-bar-tab-name-all ()
  "Generate tab name from buffers of all windows."
  (mapconcat #'buffer-name
             (delete-dups (mapcar #'window-buffer
                                  (window-list-1 (frame-first-window)
                                                 'nomini)))
             ", "))

(defcustom tab-bar-tab-name-truncated-max 20
  "Maximum length of the tab name from the current buffer.
Effective when `tab-bar-tab-name-function' is customized
to `tab-bar-tab-name-truncated'."
  :type 'integer
  :group 'tab-bar
  :version "27.1")

(defvar tab-bar-tab-name-ellipsis nil)

(defun tab-bar-tab-name-truncated ()
  "Generate tab name from the buffer of the selected window.
Truncate it to the length specified by `tab-bar-tab-name-truncated-max'.
Append ellipsis `tab-bar-tab-name-ellipsis' in this case."
  (let ((tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
        (ellipsis (cond
                   (tab-bar-tab-name-ellipsis)
                   ((char-displayable-p ?…) "…")
                   ("..."))))
    (if (< (length tab-name) tab-bar-tab-name-truncated-max)
        tab-name
      (propertize (truncate-string-to-width
                   tab-name tab-bar-tab-name-truncated-max nil nil
                   ellipsis)
                  'help-echo tab-name))))


(defvar tab-bar-tabs-function #'tab-bar-tabs
  "Function to get a list of tabs to display in the tab bar.
This function should have one optional argument FRAME,
defaulting to the selected frame when nil.
It should return a list of alists with parameters
that include at least the element (name . TAB-NAME).
For example, \\='((tab (name . \"Tab 1\")) (current-tab (name . \"Tab 2\")))
By default, use function `tab-bar-tabs'.")

(defun tab-bar-tabs (&optional frame)
  "Return a list of tabs belonging to the selected frame.
Ensure the frame parameter `tabs' is pre-populated.
Update the current tab name when it exists.
Return its existing value or a new value."
  (let ((tabs (frame-parameter frame 'tabs)))
    (if tabs
        (let* ((current-tab (assq 'current-tab tabs))
               (current-tab-name (assq 'name current-tab))
               (current-tab-explicit-name (assq 'explicit-name current-tab)))
          (when (and current-tab-name
                     current-tab-explicit-name
                     (not (cdr current-tab-explicit-name)))
            (setf (cdr current-tab-name)
                  (funcall tab-bar-tab-name-function))))
      ;; Create default tabs
      (setq tabs (list (tab-bar--current-tab)))
      (set-frame-parameter frame 'tabs tabs))
    tabs))


(defun tab-bar-make-keymap-1 ()
  "Generate an actual keymap from `tab-bar-map', without caching."
  (let* ((separator (or tab-bar-separator (if window-system " " "|")))
         (i 0)
         (tabs (funcall tab-bar-tabs-function)))
    (append
     '(keymap (mouse-1 . tab-bar-handle-mouse))
     (when tab-bar-history-mode
       `((sep-history-back menu-item ,separator ignore)
         (history-back
          menu-item ,tab-bar-back-button tab-bar-history-back
          :help "Click to go back in tab history")
         (sep-history-forward menu-item ,separator ignore)
         (history-forward
          menu-item ,tab-bar-forward-button tab-bar-history-forward
          :help "Click to go forward in tab history")))
     (mapcan
      (lambda (tab)
        (setq i (1+ i))
        (append
         `((,(intern (format "sep-%i" i)) menu-item ,separator ignore))
         (cond
          ((eq (car tab) 'current-tab)
           `((current-tab
              menu-item
              ,(propertize (concat (if tab-bar-tab-hints (format "%d " i) "")
                                   (alist-get 'name tab)
                                   (or (and tab-bar-close-button-show
                                            (not (eq tab-bar-close-button-show
                                                     'non-selected))
                                            tab-bar-close-button) ""))
                           'face 'tab-bar-tab)
              ignore
              :help "Current tab")))
          (t
           `((,(intern (format "tab-%i" i))
              menu-item
              ,(propertize (concat (if tab-bar-tab-hints (format "%d " i) "")
                                   (alist-get 'name tab)
                                   (or (and tab-bar-close-button-show
                                            (not (eq tab-bar-close-button-show
                                                     'selected))
                                            tab-bar-close-button) ""))
                           'face 'tab-bar-tab-inactive)
              ,(or
                (alist-get 'binding tab)
                `(lambda ()
                   (interactive)
                   (tab-bar-select-tab ,i)))
              :help "Click to visit tab"))))
         `((,(if (eq (car tab) 'current-tab) 'C-current-tab (intern (format "C-tab-%i" i)))
            menu-item ""
            ,(or
              (alist-get 'close-binding tab)
              `(lambda ()
                 (interactive)
                 (tab-bar-close-tab ,i)))))))
      tabs)
     `((sep-add-tab menu-item ,separator ignore))
     (when (and tab-bar-new-button-show tab-bar-new-button)
       `((add-tab menu-item ,tab-bar-new-button tab-bar-new-tab
                  :help "New tab"))))))


;; Some window-configuration parameters don't need to be persistent.
;; Don't save to the desktop file such tab parameters that are saved
;; as "Unprintable entity" so can't be used after restoring the desktop.
;; Actually tab-bar-select-tab already can handle unprintable entities,
;; but it's better not to waste the desktop file with useless data.
(defun frameset-filter-tabs (current _filtered _parameters saving)
  (if saving
      (mapcar (lambda (current)
                (if (consp current)
                    (seq-reduce (lambda (current param)
                                  (assq-delete-all param current))
                                '(wc wc-point wc-bl wc-bbl wc-history-back wc-history-forward)
                                (copy-sequence current))
                  current))
              current)
    current))

(push '(tabs . frameset-filter-tabs) frameset-filter-alist)

(defun tab-bar--tab (&optional frame)
  (let* ((tab (assq 'current-tab (frame-parameter frame 'tabs)))
         (tab-explicit-name (alist-get 'explicit-name tab))
         (bl  (seq-filter #'buffer-live-p (frame-parameter frame 'buffer-list)))
         (bbl (seq-filter #'buffer-live-p (frame-parameter frame 'buried-buffer-list))))
    `(tab
      (name . ,(if tab-explicit-name
                   (alist-get 'name tab)
                 (funcall tab-bar-tab-name-function)))
      (explicit-name . ,tab-explicit-name)
      (time . ,(float-time))
      (ws . ,(window-state-get
              (frame-root-window (or frame (selected-frame))) 'writable))
      (wc . ,(current-window-configuration))
      (wc-point . ,(point-marker))
      (wc-bl . ,bl)
      (wc-bbl . ,bbl)
      (wc-history-back . ,(gethash (or frame (selected-frame)) tab-bar-history-back))
      (wc-history-forward . ,(gethash (or frame (selected-frame)) tab-bar-history-forward)))))

(defun tab-bar--current-tab (&optional tab frame)
  ;; `tab` here is an argument meaning 'use tab as template'. This is
  ;; necessary when switching tabs, otherwise the destination tab
  ;; inherit the current tab's `explicit-name` parameter.
  (let* ((tab (or tab (assq 'current-tab (frame-parameter frame 'tabs))))
         (tab-explicit-name (alist-get 'explicit-name tab)))
    `(current-tab
      (name . ,(if tab-explicit-name
                   (alist-get 'name tab)
                 (funcall tab-bar-tab-name-function)))
      (explicit-name . ,tab-explicit-name))))

(defun tab-bar--current-tab-index (&optional tabs frame)
  (seq-position (or tabs (funcall tab-bar-tabs-function frame))
                'current-tab (lambda (a b) (eq (car a) b))))

(defun tab-bar--tab-index (tab &optional tabs frame)
  (seq-position (or tabs (funcall tab-bar-tabs-function frame))
                tab #'eq))

(defun tab-bar--tab-index-by-name (name &optional tabs frame)
  (seq-position (or tabs (funcall tab-bar-tabs-function frame))
                name (lambda (a b) (equal (alist-get 'name a) b))))

(defun tab-bar--tab-index-recent (nth &optional tabs frame)
  (let* ((tabs (or tabs (funcall tab-bar-tabs-function frame)))
         (sorted-tabs (tab-bar--tabs-recent tabs frame))
         (tab (nth (1- nth) sorted-tabs)))
    (tab-bar--tab-index tab tabs)))

(defun tab-bar--tabs-recent (&optional tabs frame)
  (let* ((tabs (or tabs (funcall tab-bar-tabs-function frame))))
    (seq-sort-by (lambda (tab) (alist-get 'time tab)) #'>
                 (seq-remove (lambda (tab)
                               (eq (car tab) 'current-tab))
                             tabs))))


(defun tab-bar-select-tab (&optional arg)
  "Switch to the tab by its absolute position ARG in the tab bar.
When this command is bound to a numeric key (with a prefix or modifier key
using `tab-bar-select-tab-modifiers'), calling it without an argument
will translate its bound numeric key to the numeric argument.
ARG counts from 1."
  (interactive "P")
  (unless (integerp arg)
    (let ((key (event-basic-type last-command-event)))
      (setq arg (if (and (characterp key) (>= key ?1) (<= key ?9))
                    (- key ?0)
                  1))))

  (let* ((tabs (funcall tab-bar-tabs-function))
         (from-index (tab-bar--current-tab-index tabs))
         (to-index (1- (max 1 (min arg (length tabs))))))
    (unless (eq from-index to-index)
      (let* ((from-tab (tab-bar--tab))
             (to-tab (nth to-index tabs))
             (wc (alist-get 'wc to-tab))
             (ws (alist-get 'ws to-tab)))

        ;; During the same session, use window-configuration to switch
        ;; tabs, because window-configurations are more reliable
        ;; (they keep references to live buffers) than window-states.
        ;; But after restoring tabs from a previously saved session,
        ;; its value of window-configuration is unreadable,
        ;; so restore its saved window-state.
        (cond
         ((window-configuration-p wc)
          (let ((wc-point (alist-get 'wc-point to-tab))
                (wc-bl  (seq-filter #'buffer-live-p (alist-get 'wc-bl to-tab)))
                (wc-bbl (seq-filter #'buffer-live-p (alist-get 'wc-bbl to-tab)))
                (wc-history-back (alist-get 'wc-history-back to-tab))
                (wc-history-forward (alist-get 'wc-history-forward to-tab)))

            (set-window-configuration wc)

            ;; set-window-configuration does not restore the value of
            ;; point in the current buffer, so restore it separately.
            (when (and (markerp wc-point)
                       (marker-buffer wc-point)
                       ;; FIXME: After dired-revert, marker relocates to 1.
                       ;; window-configuration restores point to global point
                       ;; in this dired buffer, not to its window point,
                       ;; but this is slightly better than 1.
                       ;; Maybe better to save dired-filename in each window?
                       (not (eq 1 (marker-position wc-point))))
              (goto-char wc-point))

            (when wc-bl  (set-frame-parameter nil 'buffer-list wc-bl))
            (when wc-bbl (set-frame-parameter nil 'buried-buffer-list wc-bbl))

            (puthash (selected-frame)
                     (and (window-configuration-p (alist-get 'wc (car wc-history-back)))
                          wc-history-back)
                     tab-bar-history-back)
            (puthash (selected-frame)
                     (and (window-configuration-p (alist-get 'wc (car wc-history-forward)))
                          wc-history-forward)
                     tab-bar-history-forward)))

         (ws
          (window-state-put ws (frame-root-window (selected-frame)) 'safe)))

        (setq tab-bar-history-omit t)

        (when from-index
          (setf (nth from-index tabs) from-tab))
        (setf (nth to-index tabs) (tab-bar--current-tab (nth to-index tabs)))

        (unless tab-bar-mode
          (message "Selected tab '%s'" (alist-get 'name to-tab))))

      (force-mode-line-update))))

(defun tab-bar-switch-to-next-tab (&optional arg)
  "Switch to ARGth next tab."
  (interactive "p")
  (unless (integerp arg)
    (setq arg 1))
  (let* ((tabs (funcall tab-bar-tabs-function))
         (from-index (or (tab-bar--current-tab-index tabs) 0))
         (to-index (mod (+ from-index arg) (length tabs))))
    (tab-bar-select-tab (1+ to-index))))

(defun tab-bar-switch-to-prev-tab (&optional arg)
  "Switch to ARGth previous tab."
  (interactive "p")
  (unless (integerp arg)
    (setq arg 1))
  (tab-bar-switch-to-next-tab (- arg)))

(defun tab-bar-switch-to-recent-tab (&optional arg)
  "Switch to ARGth most recently visited tab."
  (interactive "p")
  (unless (integerp arg)
    (setq arg 1))
  (let ((tab-index (tab-bar--tab-index-recent arg)))
    (if tab-index
        (tab-bar-select-tab (1+ tab-index))
      (message "No more recent tabs"))))

(defun tab-bar-switch-to-tab (name)
  "Switch to the tab by NAME.
Default values are tab names sorted by recency, so you can use \
\\<minibuffer-local-map>\\[next-history-element]
to get the name of the last visited tab, the second last, and so on."
  (interactive
   (let* ((recent-tabs (mapcar (lambda (tab)
                                 (alist-get 'name tab))
                               (tab-bar--tabs-recent))))
     (list (completing-read "Switch to tab by name (default recent): "
                            recent-tabs nil nil nil nil recent-tabs))))
  (tab-bar-select-tab (1+ (or (tab-bar--tab-index-by-name name) 0))))

(defalias 'tab-bar-select-tab-by-name 'tab-bar-switch-to-tab)


(defun tab-bar-move-tab-to (to-index &optional from-index)
  "Move tab from FROM-INDEX position to new position at TO-INDEX.
FROM-INDEX defaults to the current tab index.
FROM-INDEX and TO-INDEX count from 1."
  (interactive "P")
  (let* ((tabs (funcall tab-bar-tabs-function))
         (from-index (or from-index (1+ (tab-bar--current-tab-index tabs))))
         (from-tab (nth (1- from-index) tabs))
         (to-index (max 0 (min (1- (or to-index 1)) (1- (length tabs))))))
    (setq tabs (delq from-tab tabs))
    (cl-pushnew from-tab (nthcdr to-index tabs))
    (set-frame-parameter nil 'tabs tabs)
    (force-mode-line-update)))

(defun tab-bar-move-tab (&optional arg)
  "Move the current tab ARG positions to the right.
If a negative ARG, move the current tab ARG positions to the left."
  (interactive "p")
  (let* ((tabs (funcall tab-bar-tabs-function))
         (from-index (or (tab-bar--current-tab-index tabs) 0))
         (to-index (mod (+ from-index arg) (length tabs))))
    (tab-bar-move-tab-to (1+ to-index) (1+ from-index))))

(defun tab-bar-move-tab-to-frame (arg &optional from-frame from-index to-frame to-index)
  "Move tab from FROM-INDEX position to new position at TO-INDEX.
FROM-INDEX defaults to the current tab index.
FROM-INDEX and TO-INDEX count from 1.
FROM-FRAME specifies the source frame and defaults to the selected frame.
TO-FRAME specifies the target frame and defaults the next frame.
Interactively, ARG selects the ARGth different frame to move to."
  (interactive "P")
  (unless from-frame
    (setq from-frame (selected-frame)))
  (unless to-frame
    (dotimes (_ (prefix-numeric-value arg))
      (setq to-frame (next-frame to-frame))))
  (unless (eq from-frame to-frame)
    (let* ((from-tabs (funcall tab-bar-tabs-function from-frame))
           (from-index (or from-index (1+ (tab-bar--current-tab-index from-tabs))))
           (from-tab (nth (1- from-index) from-tabs))
           (to-tabs (funcall tab-bar-tabs-function to-frame))
           (to-index (max 0 (min (1- (or to-index 1)) (1- (length to-tabs))))))
      (cl-pushnew (assq-delete-all
                   'wc (if (eq (car from-tab) 'current-tab)
                           (tab-bar--tab from-frame)
                         from-tab))
                  (nthcdr to-index to-tabs))
      (with-selected-frame from-frame
        (let ((inhibit-message t) ; avoid message about deleted tab
              tab-bar-closed-tabs)
          (tab-bar-close-tab from-index)))
      (set-frame-parameter to-frame 'tabs to-tabs)
      (force-mode-line-update t))))


(defcustom tab-bar-new-tab-to 'right
  "Defines where to create a new tab.
If `leftmost', create as the first tab.
If `left', create to the left from the current tab.
If `right', create to the right from the current tab.
If `rightmost', create as the last tab.
If the value is a function, it should return a number as a position
on the tab bar specifying where to insert a new tab."
  :type '(choice (const :tag "First tab" leftmost)
                 (const :tag "To the left" left)
                 (const :tag "To the right" right)
                 (const :tag "Last tab" rightmost)
                 (function :tag "Function"))
  :group 'tab-bar
  :version "27.1")

(defcustom tab-bar-tab-post-open-functions nil
  "List of functions to call after creating a new tab.
The current tab is supplied as an argument. Any modifications
made to the tab argument will be applied after all functions are
called."
  :type '(repeat function)
  :group 'tab-bar
  :version "27.1")

(defun tab-bar-new-tab-to (&optional to-index)
  "Add a new tab at the absolute position TO-INDEX.
TO-INDEX counts from 1.  If no TO-INDEX is specified, then add
a new tab at the position specified by `tab-bar-new-tab-to'.

After the tab is created, the hooks in
`tab-bar-tab-post-open-functions' are run."
  (interactive "P")
  (let* ((tabs (funcall tab-bar-tabs-function))
         (from-index (tab-bar--current-tab-index tabs))
         (from-tab (tab-bar--tab)))

    (when tab-bar-new-tab-choice
      (delete-other-windows)
      ;; Create a new window to get rid of old window parameters
      ;; (e.g. prev/next buffers) of old window.
      (split-window) (delete-window)
      (let ((buffer
             (if (functionp tab-bar-new-tab-choice)
                 (funcall tab-bar-new-tab-choice)
               (if (stringp tab-bar-new-tab-choice)
                   (or (get-buffer tab-bar-new-tab-choice)
                       (find-file-noselect tab-bar-new-tab-choice))))))
        (when (buffer-live-p buffer)
          (switch-to-buffer buffer))))

    (when from-index
      (setf (nth from-index tabs) from-tab))
    (let ((to-tab (tab-bar--current-tab))
          (to-index (or (if to-index (1- to-index))
                        (pcase tab-bar-new-tab-to
                          ('leftmost 0)
                          ('rightmost (length tabs))
                          ('left (or from-index 1))
                          ('right (1+ (or from-index 0)))
                          ((pred functionp)
                           (funcall tab-bar-new-tab-to))))))
      (setq to-index (max 0 (min (or to-index 0) (length tabs))))
      (cl-pushnew to-tab (nthcdr to-index tabs))

      (when (eq to-index 0)
        ;; pushnew handles the head of tabs but not frame-parameter
        (set-frame-parameter nil 'tabs tabs))

      (run-hook-with-args 'tab-bar-tab-post-open-functions
                          (nth to-index tabs)))

    (when (and (not tab-bar-mode)
               (or (eq tab-bar-show t)
                   (and (natnump tab-bar-show)
                        (> (length tabs) tab-bar-show))))
      (tab-bar-mode 1))

    (force-mode-line-update)
    (unless tab-bar-mode
      (message "Added new tab at %s" tab-bar-new-tab-to))))

(defun tab-bar-new-tab (&optional arg)
  "Create a new tab ARG positions to the right.
If a negative ARG, create a new tab ARG positions to the left.
If ARG is zero, create a new tab in place of the current tab."
  (interactive "P")
  (if arg
      (let* ((tabs (funcall tab-bar-tabs-function))
             (from-index (or (tab-bar--current-tab-index tabs) 0))
             (to-index (+ from-index (prefix-numeric-value arg))))
        (tab-bar-new-tab-to (1+ to-index)))
    (tab-bar-new-tab-to)))


(defvar tab-bar-closed-tabs nil
  "A list of closed tabs to be able to undo their closing.")

(defcustom tab-bar-close-tab-select 'recent
  "Defines what tab to select after closing the specified tab.
If `left', select the adjacent left tab.
If `right', select the adjacent right tab.
If `recent', select the most recently visited tab."
  :type '(choice (const :tag "Select left tab" left)
                 (const :tag "Select right tab" right)
                 (const :tag "Select recent tab" recent))
  :group 'tab-bar
  :version "27.1")

(defcustom tab-bar-close-last-tab-choice nil
  "Defines what to do when the last tab is closed.
If nil, do nothing and show a message, like closing the last window or frame.
If `delete-frame', delete the containing frame, as a web browser would do.
If `tab-bar-mode-disable', disable tab-bar-mode so that tabs no longer show in the frame.
If the value is a function, call that function with the tab to be closed as an argument."
  :type '(choice (const    :tag "Do nothing and show message" nil)
                 (const    :tag "Close the containing frame" delete-frame)
                 (const    :tag "Disable tab-bar-mode" tab-bar-mode-disable)
                 (function :tag "Function"))
  :group 'tab-bar
  :version "27.1")

(defcustom tab-bar-tab-prevent-close-functions nil
  "List of functions to call to determine whether to close a tab.
The tab to be closed and a boolean indicating whether or not it
is the only tab in the frame are supplied as arguments. If any
function returns a non-nil value, the tab will not be closed."
  :type '(repeat function)
  :group 'tab-bar
  :version "27.1")

(defcustom tab-bar-tab-pre-close-functions nil
  "List of functions to call before closing a tab.
The tab to be closed and a boolean indicating whether or not it
is the only tab in the frame are supplied as arguments,
respectively."
  :type '(repeat function)
  :group 'tab-bar
  :version "27.1")

(defun tab-bar-close-tab (&optional arg to-index)
  "Close the tab specified by its absolute position ARG.
If no ARG is specified, then close the current tab and switch
to the tab specified by `tab-bar-close-tab-select'.
ARG counts from 1.
Optional TO-INDEX could be specified to override the value of
`tab-bar-close-tab-select' programmatically with a position
of an existing tab to select after closing the current tab.
TO-INDEX counts from 1.

The functions in `tab-bar-tab-prevent-close-functions' will be
run to determine whether or not to close the tab.
Just before the tab is closed, the functions in
`tab-bar-tab-pre-close-functions' will be run.  The base behavior
for the last tab on a frame is determined by
`tab-bar-close-last-tab-choice'."
  (interactive "P")
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-index (tab-bar--current-tab-index tabs))
         (close-index (if (integerp arg) (1- arg) current-index))
         (last-tab-p (= 1 (length tabs)))
         (prevent-close (run-hook-with-args-until-success
                         'tab-bar-tab-prevent-close-functions
                         (nth close-index tabs)
                         last-tab-p)))

    (unless prevent-close
      (run-hook-with-args 'tab-bar-tab-pre-close-functions
                          (nth close-index tabs)
                          last-tab-p)

      (if last-tab-p
          (pcase tab-bar-close-last-tab-choice
            ('nil
             (user-error "Attempt to delete the sole tab in a frame"))
            ('delete-frame
             (delete-frame))
            ('tab-bar-mode-disable
             (tab-bar-mode -1))
            ((pred functionp)
             ;; Give the handler function the full extent of the tab's
             ;; data, not just it's name and explicit-name flag.
             (funcall tab-bar-close-last-tab-choice (tab-bar--tab))))

        ;; More than one tab still open
        (when (eq current-index close-index)
          ;; Select another tab before deleting the current tab
          (let ((to-index (or (if to-index (1- to-index))
                              (pcase tab-bar-close-tab-select
                                ('left (1- (if (< current-index 1) 2 current-index)))
                                ('right (if (> (length tabs) (1+ current-index))
                                            (1+ current-index)
                                          (1- current-index)))
                                ('recent (tab-bar--tab-index-recent 1 tabs))))))
            (setq to-index (max 0 (min (or to-index 0) (1- (length tabs)))))
            (let ((inhibit-message t)) ; avoid message about selected tab
              (tab-bar-select-tab (1+ to-index)))
            ;; Re-read tabs after selecting another tab
            (setq tabs (funcall tab-bar-tabs-function))))

        (let ((close-tab (nth close-index tabs)))
          (push `((frame . ,(selected-frame))
                  (index . ,close-index)
                  (tab . ,(if (eq (car close-tab) 'current-tab)
                              (tab-bar--tab)
                            close-tab)))
                tab-bar-closed-tabs)
          (set-frame-parameter nil 'tabs (delq close-tab tabs)))

        (when (and tab-bar-mode
                   (and (natnump tab-bar-show)
                        (<= (length tabs) tab-bar-show)))
          (tab-bar-mode -1))

        (force-mode-line-update)
        (unless tab-bar-mode
          (message "Deleted tab and switched to %s" tab-bar-close-tab-select))))))

(defun tab-bar-close-tab-by-name (name)
  "Close the tab by NAME."
  (interactive
   (list (completing-read "Close tab by name: "
                          (mapcar (lambda (tab)
                                    (alist-get 'name tab))
                                  (funcall tab-bar-tabs-function)))))
  (tab-bar-close-tab (1+ (tab-bar--tab-index-by-name name))))

(defun tab-bar-close-other-tabs ()
  "Close all tabs on the selected frame, except the selected one."
  (interactive)
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-index (tab-bar--current-tab-index tabs)))
    (when current-index
      (dotimes (index (length tabs))
        (unless (or (eq index current-index)
                    (run-hook-with-args-until-success
                     'tab-bar-tab-prevent-close-functions
                     (nth index tabs)
                     ; last-tab-p logically can't ever be true if we
                     ; make it this far
                     nil))
          (push `((frame . ,(selected-frame))
                  (index . ,index)
                  (tab . ,(nth index tabs)))
                tab-bar-closed-tabs)
          (run-hook-with-args 'tab-bar-tab-pre-close-functions (nth index tabs) nil)))
      (set-frame-parameter nil 'tabs (list (nth current-index tabs)))

      (when (and tab-bar-mode
                 (and (natnump tab-bar-show)
                      (<= 1 tab-bar-show)))
        (tab-bar-mode -1))

      (force-mode-line-update)
      (unless tab-bar-mode
        (message "Deleted all other tabs")))))

(defun tab-bar-undo-close-tab ()
  "Restore the last closed tab."
  (interactive)
  ;; Pop out closed tabs that were on already deleted frames
  (while (and tab-bar-closed-tabs
              (not (frame-live-p (alist-get 'frame (car tab-bar-closed-tabs)))))
    (pop tab-bar-closed-tabs))

  (if tab-bar-closed-tabs
      (let* ((closed (pop tab-bar-closed-tabs))
             (frame (alist-get 'frame closed))
             (index (alist-get 'index closed))
             (tab (alist-get 'tab closed)))
        (unless (eq frame (selected-frame))
          (select-frame-set-input-focus frame))

        (let ((tabs (funcall tab-bar-tabs-function)))
          (setq index (max 0 (min index (length tabs))))
          (cl-pushnew tab (nthcdr index tabs))
          (when (eq index 0)
            ;; pushnew handles the head of tabs but not frame-parameter
            (set-frame-parameter nil 'tabs tabs))
          (tab-bar-select-tab (1+ index))))

    (message "No more closed tabs to undo")))


(defun tab-bar-rename-tab (name &optional arg)
  "Rename the tab specified by its absolute position ARG.
If no ARG is specified, then rename the current tab.
ARG counts from 1.
If NAME is the empty string, then use the automatic name
function `tab-bar-tab-name-function'."
  (interactive
   (let* ((tabs (funcall tab-bar-tabs-function))
          (tab-index (or current-prefix-arg (1+ (tab-bar--current-tab-index tabs))))
          (tab-name (alist-get 'name (nth (1- tab-index) tabs))))
     (list (read-from-minibuffer
            "New name for tab (leave blank for automatic naming): "
            nil nil nil nil tab-name)
           current-prefix-arg)))
  (let* ((tabs (funcall tab-bar-tabs-function))
         (tab-index (if arg
                        (1- (max 0 (min arg (length tabs))))
                      (tab-bar--current-tab-index tabs)))
         (tab-to-rename (nth tab-index tabs))
         (tab-explicit-name (> (length name) 0))
         (tab-new-name (if tab-explicit-name
                           name
                         (funcall tab-bar-tab-name-function))))
    (setf (alist-get 'name tab-to-rename) tab-new-name
          (alist-get 'explicit-name tab-to-rename) tab-explicit-name)

    (force-mode-line-update)
    (unless tab-bar-mode
      (message "Renamed tab to '%s'" tab-new-name))))

(defun tab-bar-rename-tab-by-name (tab-name new-name)
  "Rename the tab named TAB-NAME.
If NEW-NAME is the empty string, then use the automatic name
function `tab-bar-tab-name-function'."
  (interactive
   (let ((tab-name (completing-read "Rename tab by name: "
                                    (mapcar (lambda (tab)
                                              (alist-get 'name tab))
                                            (funcall tab-bar-tabs-function)))))
     (list tab-name (read-from-minibuffer
                     "New name for tab (leave blank for automatic naming): "
                     nil nil nil nil tab-name))))
  (tab-bar-rename-tab new-name (1+ (tab-bar--tab-index-by-name tab-name))))


;;; Tab history mode

(defvar tab-bar-history-limit 10
  "The number of history elements to keep.")

(defvar tab-bar-history-omit nil
  "When non-nil, omit window-configuration changes from the current command.")

(defvar tab-bar-history-back (make-hash-table)
  "History of back changes in every tab per frame.")

(defvar tab-bar-history-forward (make-hash-table)
  "History of forward changes in every tab per frame.")

(defvar tab-bar-history-old nil
  "Window configuration before the current command.")

(defvar tab-bar-history-old-minibuffer-depth 0
  "Minibuffer depth before the current command.")

(defun tab-bar-history--pre-change ()
  (setq tab-bar-history-old-minibuffer-depth (minibuffer-depth))
  ;; Store wc before possibly entering the minibuffer
  (when (zerop tab-bar-history-old-minibuffer-depth)
    (setq tab-bar-history-old
          `((wc . ,(current-window-configuration))
            (wc-point . ,(point-marker))))))

(defun tab-bar--history-change ()
  (when (and (not tab-bar-history-omit)
             tab-bar-history-old
             ;; Store wc before possibly entering the minibuffer
             (zerop tab-bar-history-old-minibuffer-depth))
    (puthash (selected-frame)
             (seq-take (cons tab-bar-history-old
                             (gethash (selected-frame) tab-bar-history-back))
                       tab-bar-history-limit)
             tab-bar-history-back))
  (when tab-bar-history-omit
    (setq tab-bar-history-omit nil)))

(defun tab-bar-history-back ()
  "Restore a previous window configuration used in the current tab.
This navigates back in the history of window configurations."
  (interactive)
  (setq tab-bar-history-omit t)
  (let* ((history (pop (gethash (selected-frame) tab-bar-history-back)))
         (wc (alist-get 'wc history))
         (wc-point (alist-get 'wc-point history)))
    (if (window-configuration-p wc)
        (progn
          (puthash (selected-frame)
                   (cons tab-bar-history-old
                         (gethash (selected-frame) tab-bar-history-forward))
                   tab-bar-history-forward)
          (set-window-configuration wc)
          (when (and (markerp wc-point) (marker-buffer wc-point))
            (goto-char wc-point)))
      (message "No more tab back history"))))

(defun tab-bar-history-forward ()
  "Cancel restoration of the previous window configuration.
This navigates forward in the history of window configurations."
  (interactive)
  (setq tab-bar-history-omit t)
  (let* ((history (pop (gethash (selected-frame) tab-bar-history-forward)))
         (wc (alist-get 'wc history))
         (wc-point (alist-get 'wc-point history)))
    (if (window-configuration-p wc)
        (progn
          (puthash (selected-frame)
                   (cons tab-bar-history-old
                         (gethash (selected-frame) tab-bar-history-back))
                   tab-bar-history-back)
          (set-window-configuration wc)
          (when (and (markerp wc-point) (marker-buffer wc-point))
            (goto-char wc-point)))
      (message "No more tab forward history"))))

(define-minor-mode tab-bar-history-mode
  "Toggle tab history mode for the tab bar.
Tab history mode remembers window configurations used in every tab,
and can restore them."
  :global t :group 'tab-bar
  (if tab-bar-history-mode
      (progn
        (when (and tab-bar-mode (not (get-text-property 0 'display tab-bar-back-button)))
          ;; This file is pre-loaded so only here we can use the right data-directory:
          (add-text-properties 0 (length tab-bar-back-button)
                               `(display (image :type xpm
                                                :file "tabs/left-arrow.xpm"
                                                :margin (2 . 0)
                                                :ascent center))
                               tab-bar-back-button))
        (when (and tab-bar-mode (not (get-text-property 0 'display tab-bar-forward-button)))
          ;; This file is pre-loaded so only here we can use the right data-directory:
          (add-text-properties 0 (length tab-bar-forward-button)
                               `(display (image :type xpm
                                                :file "tabs/right-arrow.xpm"
                                                :margin (2 . 0)
                                                :ascent center))
                               tab-bar-forward-button))

        (add-hook 'pre-command-hook 'tab-bar-history--pre-change)
        (add-hook 'window-configuration-change-hook 'tab-bar--history-change))
    (remove-hook 'pre-command-hook 'tab-bar-history--pre-change)
    (remove-hook 'window-configuration-change-hook 'tab-bar--history-change)))


;;; Short aliases

(defalias 'tab-new         'tab-bar-new-tab)
(defalias 'tab-new-to      'tab-bar-new-tab-to)
(defalias 'tab-close       'tab-bar-close-tab)
(defalias 'tab-close-other 'tab-bar-close-other-tabs)
(defalias 'tab-undo        'tab-bar-undo-close-tab)
(defalias 'tab-select      'tab-bar-select-tab)
(defalias 'tab-next        'tab-bar-switch-to-next-tab)
(defalias 'tab-previous    'tab-bar-switch-to-prev-tab)
(defalias 'tab-recent      'tab-bar-switch-to-recent-tab)
(defalias 'tab-move        'tab-bar-move-tab)
(defalias 'tab-move-to     'tab-bar-move-tab-to)
(defalias 'tab-rename      'tab-bar-rename-tab)
(defalias 'tab-list        'tab-switcher)


;;; Non-graphical access to frame-local tabs (named window configurations)

(defun tab-switcher ()
  "Display a list of named window configurations.
The list is displayed in the buffer `*Tabs*'.
It's placed in the center of the frame to resemble a window list
displayed by a window switcher in some window managers on Alt+Tab.

In this list of window configurations you can delete or select them.
Type ? after invocation to get help on commands available.
Type q to remove the list of window configurations from the display.

The first column shows `D' for a window configuration you have
marked for deletion."
  (interactive)
  (let ((dir default-directory)
        (minibuf (minibuffer-selected-window)))
    (let ((tab-bar-show nil)) ; don't enable tab-bar-mode if it's disabled
      (tab-bar-new-tab))
    ;; Handle the case when it's called in the active minibuffer.
    (when minibuf (select-window (minibuffer-selected-window)))
    (delete-other-windows)
    ;; Create a new window to replace the existing one, to not break the
    ;; window parameters (e.g. prev/next buffers) of the window just saved
    ;; to the window configuration.  So when a saved window is restored,
    ;; its parameters left intact.
    (split-window) (delete-window)
    (let ((switch-to-buffer-preserve-window-point nil))
      (switch-to-buffer (tab-switcher-noselect)))
    (setq default-directory dir))
  (message "Commands: d, x; RET; q to quit; ? for help."))

(defun tab-switcher-noselect ()
  "Create and return a buffer with a list of window configurations.
The list is displayed in a buffer named `*Tabs*'.

For more information, see the function `tab-switcher'."
  (let* ((tabs (seq-remove (lambda (tab)
                             (eq (car tab) 'current-tab))
                           (funcall tab-bar-tabs-function)))
         ;; Sort by recency
         (tabs (sort tabs (lambda (a b) (< (alist-get 'time b)
                                           (alist-get 'time a))))))
    (with-current-buffer (get-buffer-create
                          (format " *Tabs*<%s>" (or (frame-parameter nil 'window-id)
                                                    (frame-parameter nil 'name))))
      (setq buffer-read-only nil)
      (erase-buffer)
      (tab-switcher-mode)
      ;; Vertical alignment to the center of the frame
      (insert-char ?\n (/ (- (frame-height) (length tabs) 1) 2))
      ;; Horizontal alignment to the center of the frame
      (setq tab-switcher-column (- (/ (frame-width) 2) 15))
      (dolist (tab tabs)
        (insert (propertize
                 (format "%s %s\n"
                         (make-string tab-switcher-column ?\040)
                         (propertize
                          (alist-get 'name tab)
                          'mouse-face 'highlight
                          'help-echo "mouse-2: select this window configuration"))
                 'tab tab)))
      (goto-char (point-min))
      (goto-char (or (next-single-property-change (point) 'tab) (point-min)))
      (when (> (length tabs) 1)
        (tab-switcher-next-line))
      (move-to-column tab-switcher-column)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (current-buffer))))

(defvar tab-switcher-column 3)
(make-variable-buffer-local 'tab-switcher-column)

(defvar tab-switcher-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "q"    'quit-window)
    (define-key map "\C-m" 'tab-switcher-select)
    (define-key map "d"    'tab-switcher-delete)
    (define-key map "k"    'tab-switcher-delete)
    (define-key map "\C-d" 'tab-switcher-delete-backwards)
    (define-key map "\C-k" 'tab-switcher-delete)
    (define-key map "x"    'tab-switcher-execute)
    (define-key map " "    'tab-switcher-next-line)
    (define-key map "n"    'tab-switcher-next-line)
    (define-key map "p"    'tab-switcher-prev-line)
    (define-key map "\177" 'tab-switcher-backup-unmark)
    (define-key map "?"    'describe-mode)
    (define-key map "u"    'tab-switcher-unmark)
    (define-key map [mouse-2] 'tab-switcher-mouse-select)
    (define-key map [follow-link] 'mouse-face)
    map)
  "Local keymap for `tab-switcher-mode' buffers.")

(define-derived-mode tab-switcher-mode nil "Window Configurations"
  "Major mode for selecting a window configuration.
Each line describes one window configuration in Emacs.
Letters do not insert themselves; instead, they are commands.
\\<tab-switcher-mode-map>
\\[tab-switcher-mouse-select] -- select window configuration you click on.
\\[tab-switcher-select] -- select current line's window configuration.
\\[tab-switcher-delete] -- mark that window configuration to be deleted, and move down.
\\[tab-switcher-delete-backwards] -- mark that window configuration to be deleted, and move up.
\\[tab-switcher-execute] -- delete marked window configurations.
\\[tab-switcher-unmark] -- remove all kinds of marks from current line.
  With prefix argument, also move up one line.
\\[tab-switcher-backup-unmark] -- back up a line and remove marks."
  (setq truncate-lines t))

(defun tab-switcher-current-tab (error-if-non-existent-p)
  "Return window configuration described by this line of the list."
  (let* ((where (save-excursion
                  (beginning-of-line)
                  (+ 2 (point) tab-switcher-column)))
         (tab (and (not (eobp)) (get-text-property where 'tab))))
    (or tab
        (if error-if-non-existent-p
            (user-error "No window configuration on this line")
          nil))))

(defun tab-switcher-next-line (&optional arg)
  (interactive "p")
  (forward-line arg)
  (beginning-of-line)
  (move-to-column tab-switcher-column))

(defun tab-switcher-prev-line (&optional arg)
  (interactive "p")
  (forward-line (- arg))
  (beginning-of-line)
  (move-to-column tab-switcher-column))

(defun tab-switcher-unmark (&optional backup)
  "Cancel all requested operations on window configuration on this line and move down.
Optional prefix arg means move up."
  (interactive "P")
  (beginning-of-line)
  (move-to-column tab-switcher-column)
  (let* ((buffer-read-only nil))
    (delete-char 1)
    (insert " "))
  (forward-line (if backup -1 1))
  (move-to-column tab-switcher-column))

(defun tab-switcher-backup-unmark ()
  "Move up and cancel all requested operations on window configuration on line above."
  (interactive)
  (forward-line -1)
  (tab-switcher-unmark)
  (forward-line -1)
  (move-to-column tab-switcher-column))

(defun tab-switcher-delete (&optional arg)
  "Mark window configuration on this line to be deleted by \\<tab-switcher-mode-map>\\[tab-switcher-execute] command.
Prefix arg is how many window configurations to delete.
Negative arg means delete backwards."
  (interactive "p")
  (let ((buffer-read-only nil))
    (if (or (null arg) (= arg 0))
        (setq arg 1))
    (while (> arg 0)
      (delete-char 1)
      (insert ?D)
      (forward-line 1)
      (setq arg (1- arg)))
    (while (< arg 0)
      (delete-char 1)
      (insert ?D)
      (forward-line -1)
      (setq arg (1+ arg)))
    (move-to-column tab-switcher-column)))

(defun tab-switcher-delete-backwards (&optional arg)
  "Mark window configuration on this line to be deleted by \\<tab-switcher-mode-map>\\[tab-switcher-execute] command.
Then move up one line.  Prefix arg means move that many lines."
  (interactive "p")
  (tab-switcher-delete (- (or arg 1))))

(defun tab-switcher-delete-from-list (tab)
  "Delete the window configuration from both lists."
  (push `((frame . ,(selected-frame))
          (index . ,(tab-bar--tab-index tab))
          (tab . ,tab))
        tab-bar-closed-tabs)
  (set-frame-parameter nil 'tabs (delq tab (funcall tab-bar-tabs-function))))

(defun tab-switcher-execute ()
  "Delete window configurations marked with \\<tab-switcher-mode-map>\\[tab-switcher-delete] commands."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((buffer-read-only nil))
      (while (re-search-forward
              (format "^%sD" (make-string tab-switcher-column ?\040))
              nil t)
        (forward-char -1)
        (let ((tab (tab-switcher-current-tab nil)))
          (when tab
            (tab-switcher-delete-from-list tab)
            (beginning-of-line)
            (delete-region (point) (progn (forward-line 1) (point))))))))
  (beginning-of-line)
  (move-to-column tab-switcher-column)
  (force-mode-line-update))

(defun tab-switcher-select ()
  "Select this line's window configuration.
This command deletes and replaces all the previously existing windows
in the selected frame."
  (interactive)
  (let* ((to-tab (tab-switcher-current-tab t)))
    (kill-buffer (current-buffer))
    ;; Delete the current window configuration of tab list
    ;; without storing it in the undo list of closed tabs
    (let ((inhibit-message t) ; avoid message about deleted tab
          tab-bar-closed-tabs)
      (tab-bar-close-tab nil (1+ (tab-bar--tab-index to-tab))))))

(defun tab-switcher-mouse-select (event)
  "Select the window configuration whose line you click on."
  (interactive "e")
  (set-buffer (window-buffer (posn-window (event-end event))))
  (goto-char (posn-point (event-end event)))
  (tab-switcher-select))


(defun tab-bar--reusable-frames (all-frames)
  (cond
   ((eq all-frames t) (frame-list))
   ((eq all-frames 'visible) (visible-frame-list))
   ((framep all-frames) (list all-frames))
   (t (list (selected-frame)))))

(defun tab-bar-get-buffer-tab (buffer-or-name &optional all-frames ignore-current-tab)
  "Return a tab owning a window whose buffer is BUFFER-OR-NAME.
BUFFER-OR-NAME may be a buffer or a buffer name and defaults to
the current buffer.

The optional argument ALL-FRAMES specifies the frames to consider:

- t means consider all tabs on all existing frames.

- `visible' means consider all tabs on all visible frames.

- A frame means consider all tabs on that frame only.

Any other value of ALL-FRAMES means consider all tabs on the
selected frame and no others.

When the optional argument IGNORE-CURRENT-TAB is non-nil,
don't take into account the buffers in the currently selected tab."
  (let ((buffer (if buffer-or-name
                    (get-buffer buffer-or-name)
                  (current-buffer))))
    (when (bufferp buffer)
      (seq-some
       (lambda (frame)
         (seq-some
          (lambda (tab)
            (when (if (eq (car tab) 'current-tab)
                      (unless ignore-current-tab
                        (get-buffer-window buffer frame))
                    (let* ((state (alist-get 'ws tab))
                           (buffers (when state
                                      (window-state-buffers state))))
                      (or
                       ;; non-writable window-state
                       (memq buffer buffers)
                       ;; writable window-state
                       (member (buffer-name buffer) buffers))))
              (append tab `((index . ,(tab-bar--tab-index tab nil frame))
                            (frame . ,frame)))))
          (funcall tab-bar-tabs-function frame)))
       (tab-bar--reusable-frames all-frames)))))

(defun display-buffer-in-tab (buffer alist)
  "Display BUFFER in a tab.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

If ALIST contains a `tab-name' entry, it creates a new tab with that name and
displays BUFFER in a new tab.  If a tab with this name already exists, it
switches to that tab before displaying BUFFER.  The `tab-name' entry can be
a function, then it is called with two arguments: BUFFER and ALIST, and
should return the tab name.  When a `tab-name' entry is omitted, create
a new tab without an explicit name.

If ALIST contains a `reusable-frames' entry, its value determines
which frames to search for a reusable tab:
  nil -- the selected frame (actually the last non-minibuffer frame)
  A frame   -- just that frame
  `visible' -- all visible frames
  0   -- all frames on the current terminal
  t   -- all frames.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let* ((tab-name (alist-get 'tab-name alist))
         (reusable-frames (alist-get 'reusable-frames alist))
         (reusable-tab (when reusable-frames
                         (tab-bar-get-buffer-tab buffer reusable-frames))))
    (if reusable-tab
        (let* ((frame (alist-get 'frame reusable-tab))
               (index (alist-get 'index reusable-tab)))
          (when frame
            (select-frame-set-input-focus frame))
          (when index
            (tab-bar-select-tab (1+ index)))
          (when (get-buffer-window buffer frame)
            (select-window (get-buffer-window buffer frame))))
      (when (functionp tab-name)
        (setq tab-name (funcall tab-name buffer alist)))
      (if tab-name
          (let ((tab-index (tab-bar--tab-index-by-name tab-name)))
            (if tab-index
                (tab-bar-select-tab (1+ tab-index))
              (let ((tab-bar-new-tab-choice t))
                (tab-bar-new-tab)
                (tab-bar-rename-tab tab-name))))
        (let ((tab-bar-new-tab-choice t))
          (tab-bar-new-tab))))))

(defun switch-to-buffer-other-tab (buffer-or-name &optional norecord)
  "Switch to buffer BUFFER-OR-NAME in another tab.
Like \\[switch-to-buffer-other-frame] (which see), but creates a new tab."
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other tab: ")))
  (display-buffer (window-normalize-buffer-to-switch-to buffer-or-name)
                  '((display-buffer-in-tab
                     display-buffer-same-window)
                    (inhibit-same-window . nil))
                  norecord))

(defun find-file-other-tab (filename &optional wildcards)
  "Edit file FILENAME, in another tab.
Like \\[find-file-other-frame] (which see), but creates a new tab."
  (interactive
   (find-file-read-args "Find file in other tab: "
                        (confirm-nonexistent-file-or-buffer)))
  (let ((value (find-file-noselect filename nil nil wildcards)))
    (if (listp value)
        (progn
          (setq value (nreverse value))
          (switch-to-buffer-other-tab (car value))
          (mapc 'switch-to-buffer (cdr value))
          value)
      (switch-to-buffer-other-tab value))))

(define-key tab-prefix-map "2" 'tab-new)
(define-key tab-prefix-map "1" 'tab-close-other)
(define-key tab-prefix-map "0" 'tab-close)
(define-key tab-prefix-map "o" 'tab-next)
(define-key tab-prefix-map "m" 'tab-move)
(define-key tab-prefix-map "r" 'tab-rename)
(define-key tab-prefix-map "\r" 'tab-bar-select-tab-by-name)
(define-key tab-prefix-map "b" 'switch-to-buffer-other-tab)
(define-key tab-prefix-map "f" 'find-file-other-tab)
(define-key tab-prefix-map "\C-f" 'find-file-other-tab)


(provide 'tab-bar)

;;; tab-bar.el ends here
