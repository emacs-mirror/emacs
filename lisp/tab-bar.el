;;; tab-bar.el --- frame-local tabs with named persistent window configurations -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022 Free Software Foundation, Inc.

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
;; `tab-bar-map' as the actual keymap to define the tab bar.

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

(defface tab-bar-tab-group-current
  '((t :inherit tab-bar-tab :box nil :weight bold))
  "Tab bar face for current group tab."
  :version "28.1"
  :group 'tab-bar-faces)

(defface tab-bar-tab-group-inactive
  '((t :inherit (shadow tab-bar-tab-inactive)))
  "Tab bar face for inactive group tab."
  :version "28.1"
  :group 'tab-bar-faces)

(defface tab-bar-tab-ungrouped
  '((t :inherit (shadow tab-bar-tab-inactive)))
  "Tab bar face for ungrouped tab when tab groups are used."
  :version "28.1"
  :group 'tab-bar-faces)


(defcustom tab-bar-select-tab-modifiers '()
  "List of modifier keys for selecting tab-bar tabs by their numbers.
Possible modifier keys are `control', `meta', `shift', `hyper', `super' and
`alt'.  Pressing one of the modifiers in the list and a digit selects the
tab whose number equals the digit (see `tab-bar-select-tab').
The digit 9 selects the last (rightmost) tab (see `tab-last').
The digit 0 selects the most recently visited tab (see `tab-recent').
For easier selection of tabs by their numbers, consider customizing
`tab-bar-tab-hints', which will show tab numbers alongside the tab name."
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
         (when tab-bar-mode
           (tab-bar--undefine-keys)
           (tab-bar--define-keys)))
  :group 'tab-bar
  :version "27.1")

(defun tab-bar--define-keys ()
  "Install key bindings for switching between tabs if the user has configured them."
  (when tab-bar-select-tab-modifiers
    (global-set-key (vector (append tab-bar-select-tab-modifiers (list ?0)))
                    'tab-recent)
    (dotimes (i 8)
      (global-set-key (vector (append tab-bar-select-tab-modifiers
                                      (list (+ i 1 ?0))))
                      'tab-bar-select-tab))
    (global-set-key (vector (append tab-bar-select-tab-modifiers (list ?9)))
                    'tab-last))
  ;; Don't override user customized key bindings
  (unless (global-key-binding [(control tab)])
    (global-set-key [(control tab)] 'tab-next))
  (unless (global-key-binding [(control shift tab)])
    (global-set-key [(control shift tab)] 'tab-previous))
  (unless (global-key-binding [(control shift iso-lefttab)])
    (global-set-key [(control shift iso-lefttab)] 'tab-previous))

  ;; Replace default value with a condition that supports displaying
  ;; global-mode-string in the tab bar instead of the mode line.
  (when (and (memq 'tab-bar-format-global tab-bar-format)
             (member '(global-mode-string ("" global-mode-string))
                     mode-line-misc-info))
    (setf (alist-get 'global-mode-string mode-line-misc-info)
          '(("" (:eval (if (and tab-bar-mode
                                (memq 'tab-bar-format-global
                                      tab-bar-format))
                           "" global-mode-string)))))))

(defun tab-bar--undefine-keys ()
  "Uninstall key bindings previously bound by `tab-bar--define-keys'."
  (when (eq (global-key-binding [(control tab)]) 'tab-next)
    (global-unset-key [(control tab)]))
  (when (eq (global-key-binding [(control shift tab)]) 'tab-previous)
    (global-unset-key [(control shift tab)]))
  (when (eq (global-key-binding [(control shift iso-lefttab)]) 'tab-previous)
    (global-unset-key [(control shift iso-lefttab)])))

(defun tab-bar--load-buttons ()
  "Load the icons for the tab buttons."
  (when (and tab-bar-new-button
             (not (get-text-property 0 'display tab-bar-new-button)))
    ;; This file is pre-loaded so only here we can use the right data-directory:
    (add-text-properties 0 (length tab-bar-new-button)
                         `(display (image :type xpm
                                          :file "tabs/new.xpm"
                                          :margin ,tab-bar-button-margin
                                          :ascent center))
                         tab-bar-new-button))

  (when (and tab-bar-close-button
             (not (get-text-property 0 'display tab-bar-close-button)))
    ;; This file is pre-loaded so only here we can use the right data-directory:
    (add-text-properties 0 (length tab-bar-close-button)
                         `(display (image :type xpm
                                          :file "tabs/close.xpm"
                                          :margin ,tab-bar-button-margin
                                          :ascent center))
                         tab-bar-close-button)))

(defun tab-bar--tab-bar-lines-for-frame (frame)
  "Determine and return the value of `tab-bar-lines' for FRAME.
Return 0 if `tab-bar-mode' is not enabled.  Otherwise return
either 1 or 0 depending on the value of the customizable variable
`tab-bar-show', which see."
  (cond
   ((not tab-bar-mode) 0)
   ((not tab-bar-show) 0)
   ((eq tab-bar-show t) 1)
   ((natnump tab-bar-show)
    (if (> (length (funcall tab-bar-tabs-function frame)) tab-bar-show) 1 0))))

(defun tab-bar--update-tab-bar-lines (&optional frames)
  "Update the `tab-bar-lines' frame parameter in FRAMES.
If the optional parameter FRAMES is omitted, update only
the currently selected frame.  If it is t, update all frames
as well as the default for new frames.  Otherwise FRAMES should be
a list of frames to update."
  (let ((frame-lst (cond ((null frames)
                          (list (selected-frame)))
                         ((eq frames t)
                          (frame-list))
                         (t frames))))
    ;; Loop over all frames and update `tab-bar-lines'
    (dolist (frame frame-lst)
      (unless (frame-parameter frame 'tab-bar-lines-keep-state)
        (set-frame-parameter frame 'tab-bar-lines
                             (tab-bar--tab-bar-lines-for-frame frame)))))
  ;; Update `default-frame-alist'
  (when (eq frames t)
    (setq default-frame-alist
          (cons (cons 'tab-bar-lines (if (and tab-bar-mode (eq tab-bar-show t)) 1 0))
                (assq-delete-all 'tab-bar-lines default-frame-alist)))))

(define-minor-mode tab-bar-mode
  "Toggle the tab bar in all graphical frames (Tab Bar mode)."
  :global t
  ;; It's defined in C/cus-start, this stops the d-m-m macro defining it again.
  :variable tab-bar-mode

  ;; Recalculate `tab-bar-lines' for all frames
  (tab-bar--update-tab-bar-lines t)

  (when tab-bar-mode
    (tab-bar--load-buttons))
  (if tab-bar-mode
      (tab-bar--define-keys)
    (tab-bar--undefine-keys)))


;;; Key bindings

(defun tab-bar--key-to-number (key)
  "Return the tab number represented by KEY.
If KEY is a symbol 'tab-N', where N is a tab number, the value is N.
If KEY is \\='current-tab, the value is nil.
For any other value of KEY, the value is t."
  (cond
   ((null key) t)
   ((eq key 'current-tab) nil)
   ((let ((key-name (format "%S" key)))
      (when (string-prefix-p "tab-" key-name)
        (string-to-number (string-replace "tab-" "" key-name)))))
   (t t)))

(defvar tab-bar--dragging-in-progress)

(defun tab-bar--event-to-item (posn)
  "This function extracts extra info from the mouse event at position POSN.
It returns a list of the form (KEY KEY-BINDING CLOSE-P), where:
 KEY is a symbol representing a tab, such as \\='tab-1 or \\='current-tab;
 KEY-BINDING is the binding of KEY;
 CLOSE-P is non-nil if the mouse event was a click on the close button \"x\",
   nil otherwise."
  (setq tab-bar--dragging-in-progress nil)
  (if (posn-window posn)
      (let ((caption (car (posn-string posn))))
        (when caption
          (get-text-property 0 'menu-item caption)))
    ;; Text-mode emulation of switching tabs on the tab bar.
    ;; This code is used when you click the mouse in the tab bar
    ;; on a console which has no window system but does have a mouse.
    (let* ((x-position (car (posn-x-y posn)))
           (keymap (lookup-key (cons 'keymap (nreverse (current-active-maps))) [tab-bar]))
           (column 0))
      (when x-position
        (catch 'done
          (map-keymap
           (lambda (key binding)
             (when (eq (car-safe binding) 'menu-item)
               (when (> (+ column (length (nth 1 binding))) x-position)
                 (throw 'done (list key (nth 2 binding)
                                    (get-text-property
                                     (- x-position column)
                                     'close-tab (nth 1 binding)))))
               (setq column (+ column (length (nth 1 binding))))))
           keymap))))))

(defun tab-bar-mouse-down-1 (event)
  "Select the tab at mouse click, or add a new tab on the tab bar.
Whether this command adds a new tab or selects an existing tab
depends on whether the click is on the \"+\" button or on an
existing tab."
  (interactive "e")
  (let* ((item (tab-bar--event-to-item (event-start event)))
         (tab-number (tab-bar--key-to-number (nth 0 item))))
    (setq tab-bar--dragging-in-progress t)
    ;; Don't close the tab when clicked on the close button.  Also
    ;; don't add new tab on down-mouse.  Let `tab-bar-mouse-1' do this.
    (unless (or (memq (car item) '(add-tab history-back history-forward))
                (nth 2 item))
      (if (functionp (nth 1 item))
          (call-interactively (nth 1 item))
        (unless (eq tab-number t)
          (tab-bar-select-tab tab-number))))))

(defun tab-bar-mouse-1 (event)
  "Close the tab whose \"x\" close button you click.
See also `tab-bar-mouse-close-tab', which closes the tab
regardless of where you click on it.  Also add a new tab."
  (interactive "e")
  (let* ((item (tab-bar--event-to-item (event-start event)))
         (tab-number (tab-bar--key-to-number (nth 0 item))))
    (cond
     ((and (memq (car item) '(add-tab history-back history-forward))
           (functionp (nth 1 item)))
      (call-interactively (nth 1 item)))
     ((and (nth 2 item) (not (eq tab-number t)))
      (tab-bar-close-tab tab-number)))))

(defun tab-bar-mouse-close-tab (event)
  "Close the tab you click on.
This is in contrast with `tab-bar-mouse-1' that closes a tab
only when you click on its \"x\" close button."
  (interactive "e")
  (let* ((item (tab-bar--event-to-item (event-start event)))
         (tab-number (tab-bar--key-to-number (nth 0 item))))
    (unless (eq tab-number t)
      (tab-bar-close-tab tab-number))))

(defun tab-bar-mouse-context-menu (event)
  "Pop up the context menu for the tab on which you click."
  (interactive "e")
  (let* ((item (tab-bar--event-to-item (event-start event)))
         (tab-number (tab-bar--key-to-number (nth 0 item)))
         (menu (make-sparse-keymap (propertize "Context Menu" 'hide t))))

    (cond
     ((eq tab-number t)
      (define-key-after menu [new-tab]
        '(menu-item "New tab" tab-bar-new-tab
                    :help "Create a new tab"))
      (when tab-bar-closed-tabs
        (define-key-after menu [undo-close]
          '(menu-item "Reopen closed tab" tab-bar-undo-close-tab
                      :help "Undo closing the tab"))))

     (t
      (define-key-after menu [duplicate-tab]
        `(menu-item "Duplicate" (lambda () (interactive)
                                  (tab-bar-duplicate-tab
                                   nil ,tab-number))
                    :help "Clone the tab"))
      (define-key-after menu [detach-tab]
        `(menu-item "Detach" (lambda () (interactive)
                               (tab-bar-detach-tab
                                ,tab-number))
                    :help "Move the tab to new frame"))
      (define-key-after menu [close]
        `(menu-item "Close" (lambda () (interactive)
                              (tab-bar-close-tab ,tab-number))
                    :help "Close the tab"))
      (define-key-after menu [close-other]
        `(menu-item "Close other tabs"
                    (lambda () (interactive)
                      (tab-bar-close-other-tabs ,tab-number))
                    :help "Close all other tabs"))))

    (popup-menu menu event)))

(defun tab-bar-mouse-move-tab (event)
  "Move a tab to a different position on the tab bar.
This command should be bound to a drag event.  It moves the tab
at the mouse-down event to the position at mouse-up event."
  (interactive "e")
  (setq tab-bar--dragging-in-progress nil)
  (let ((from (tab-bar--key-to-number
               (nth 0 (tab-bar--event-to-item
                       (event-start event)))))
        (to (tab-bar--key-to-number
             (nth 0 (tab-bar--event-to-item
                     (event-end event))))))
    (unless (or (eq from to) (eq from t) (eq to t))
      (tab-bar-move-tab-to
       (if (null to) (1+ (tab-bar--current-tab-index)) to) from))))

(defvar tab-bar-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] 'tab-bar-mouse-down-1)
    (define-key map [drag-mouse-1] 'tab-bar-mouse-move-tab)
    (define-key map [mouse-1]      'tab-bar-mouse-1)
    (define-key map [down-mouse-2] 'tab-bar-mouse-close-tab)
    (define-key map [mouse-2]      'ignore)
    (define-key map [down-mouse-3] 'tab-bar-mouse-context-menu)

    (define-key map [mouse-4]     'tab-previous)
    (define-key map [mouse-5]     'tab-next)
    (define-key map [wheel-up]    'tab-previous)
    (define-key map [wheel-down]  'tab-next)
    (define-key map [wheel-left]  'tab-previous)
    (define-key map [wheel-right] 'tab-next)

    (define-key map [S-mouse-4]     'tab-bar-move-tab-backward)
    (define-key map [S-mouse-5]     'tab-bar-move-tab)
    (define-key map [S-wheel-up]    'tab-bar-move-tab-backward)
    (define-key map [S-wheel-down]  'tab-bar-move-tab)
    (define-key map [S-wheel-left]  'tab-bar-move-tab-backward)
    (define-key map [S-wheel-right] 'tab-bar-move-tab)

    map)
  "Keymap for the commands used on the tab bar.")

(global-set-key [tab-bar]
                `(menu-item ,(purecopy "tab bar") ignore
                            :filter tab-bar-make-keymap))

(defun tab-bar-make-keymap (&optional _ignore)
  "Generate an actual keymap from `tab-bar-map'.
Its main job is to show tabs in the tab bar
and to bind mouse events to the commands."
  (tab-bar-make-keymap-1))


(defun toggle-tab-bar-mode-from-frame (&optional arg)
  "Toggle tab bar on or off, based on the status of the current frame.
Used in the Show/Hide menu, to have the toggle reflect the current frame.
See `tab-bar-mode' for more information."
  (interactive (list (or current-prefix-arg 'toggle)))
  (if (eq arg 'toggle)
      (tab-bar-mode (if (> (frame-parameter nil 'tab-bar-lines) 0) 0 1))
    (tab-bar-mode arg)))

(defun toggle-frame-tab-bar (&optional frame)
  "Toggle tab bar of the selected frame.
When calling from Lisp, use the optional argument FRAME to toggle
the tab bar on that frame.
This is useful if you want to enable the tab bar individually
on each new frame when the global `tab-bar-mode' is disabled,
or if you want to disable the tab bar individually on each
new frame when the global `tab-bar-mode' is enabled, by using

  (add-hook 'after-make-frame-functions 'toggle-frame-tab-bar)"
  (interactive)
  (set-frame-parameter frame 'tab-bar-lines
                       (if (> (frame-parameter frame 'tab-bar-lines) 0) 0 1))
  (set-frame-parameter frame 'tab-bar-lines-keep-state
                       (not (frame-parameter frame 'tab-bar-lines-keep-state))))


(defcustom tab-bar-show t
  "Defines when to show the tab bar.
If t, the default, enable `tab-bar-mode' automatically upon using
the commands that create new window configurations (e.g., `tab-new').
If a non-negative integer, show the tab bar only if the number of
the tabs exceeds the value of this variable.  In particular,
if the value is 1, hide the tab bar when it has only one tab, and
show it again once more tabs are created.  A value that is a
non-negative integer also makes the tab bar appearance be different
on different frames: the tab bar can be shown on some frames and
hidden on others, depending on how many tab-bar tabs are on that
frame, and whether that number is greater than the numerical value
of this variable.
If nil, always keep the tab bar hidden.  In this case it's still
possible to use persistent named window configurations by relying on
keyboard commands `tab-new', `tab-close', `tab-next', `tab-switcher', etc.

Setting this variable directly does not take effect; please customize
it (see the info node `Easy Customization'), then it will automatically
update the tab bar on all frames according to the new value.

To enable or disable the tab bar individually on each frame,
you can use the command `toggle-frame-tab-bar'."
  :type '(choice (const :tag "Always" t)
                 (const :tag "When more than one tab" 1)
                 (const :tag "Never" nil))
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (tab-bar-mode 1)
           (tab-bar--update-tab-bar-lines t)))
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

(defcustom tab-bar-new-tab-group t
  "Defines what group to assign to a new tab.
If nil, don't set a default group automatically.
If t, inherit the group name from the previous tab.
If the value is a string, use it as the group name of a new tab.
If the value is a function, call it with no arguments
to get the group name."
  :type '(choice (const    :tag "No automatic group" nil)
                 (const    :tag "Inherit group from previous tab" t)
                 (string   :tag "Fixed group name")
                 (function :tag "Function that returns group name"))
  :group 'tab-bar
  :version "28.1")

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
(make-obsolete-variable 'tab-bar-new-button-show 'tab-bar-format "28.1")

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

(defun tab-bar-separator ()
  "Separator between tabs."
  (or tab-bar-separator (if window-system " " "|")))


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

(defvar tab-bar-tab-name-ellipsis t)

(defun tab-bar-tab-name-truncated ()
  "Generate tab name from the buffer of the selected window.
Truncate it to the length specified by `tab-bar-tab-name-truncated-max'.
Append ellipsis `tab-bar-tab-name-ellipsis' in this case."
  (let ((tab-name (buffer-name (window-buffer (minibuffer-selected-window)))))
    (if (< (length tab-name) tab-bar-tab-name-truncated-max)
        tab-name
      (propertize (truncate-string-to-width
                   tab-name tab-bar-tab-name-truncated-max nil nil
                   tab-bar-tab-name-ellipsis)
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
  "Return a list of tabs belonging to the FRAME.
Ensure the frame parameter `tabs' is pre-populated.
Update the current tab name when it exists.
Return its existing value or a new value."
  (let ((tabs (frame-parameter frame 'tabs)))
    (if tabs
        (let* ((current-tab (tab-bar--current-tab-find tabs))
               (current-tab-name (assq 'name current-tab))
               (current-tab-explicit-name (assq 'explicit-name current-tab)))
          (when (and current-tab-name
                     current-tab-explicit-name
                     (not (cdr current-tab-explicit-name)))
            (setf (cdr current-tab-name)
                  (funcall tab-bar-tab-name-function))))
      ;; Create default tabs
      (setq tabs (list (tab-bar--current-tab-make)))
      (tab-bar-tabs-set tabs frame))
    tabs))

(defun tab-bar-tabs-set (tabs &optional frame)
  "Set a list of TABS on the FRAME."
  (set-frame-parameter frame 'tabs tabs))


(defcustom tab-bar-tab-face-function #'tab-bar-tab-face-default
  "Function to define a tab face.
Function gets one argument: a tab."
  :type 'function
  :group 'tab-bar
  :version "28.1")

(defun tab-bar-tab-face-default (tab)
  (if (eq (car tab) 'current-tab) 'tab-bar-tab 'tab-bar-tab-inactive))

(defcustom tab-bar-tab-name-format-function #'tab-bar-tab-name-format-default
  "Function to format a tab name.
Function gets two arguments, the tab and its number, and should return
the formatted tab name to display in the tab bar."
  :type 'function
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (force-mode-line-update))
  :group 'tab-bar
  :version "28.1")

(defun tab-bar-tab-name-format-default (tab i)
  (let ((current-p (eq (car tab) 'current-tab)))
    (propertize
     (concat (if tab-bar-tab-hints (format "%d " i) "")
             (alist-get 'name tab)
             (or (and tab-bar-close-button-show
                      (not (eq tab-bar-close-button-show
                               (if current-p 'non-selected 'selected)))
                      tab-bar-close-button)
                 ""))
     'face (funcall tab-bar-tab-face-function tab))))

(defcustom tab-bar-format '(tab-bar-format-history
                            tab-bar-format-tabs
                            tab-bar-separator
                            tab-bar-format-add-tab)
  "Template for displaying tab bar items.
Every item in the list is a function that returns
a string, or a list of menu-item elements, or nil.
Adding a function to the list causes the tab bar to show
that string, or display a tab button which, when clicked,
will invoke the command that is the binding of the menu item.
The menu-item binding of nil will produce a tab clicking
on which will select that tab.  The menu-item's title is
displayed as the label of the tab.
If a function returns nil, it doesn't directly affect the
tab bar appearance, but can do that by some side-effect.
If the list ends with `tab-bar-format-align-right' and
`tab-bar-format-global', then after enabling `display-time-mode'
(or any other mode that uses `global-mode-string'),
it will display time aligned to the right on the tab bar instead
of the mode line.  Replacing `tab-bar-format-tabs' with
`tab-bar-format-tabs-groups' will group tabs on the tab bar."
  :type 'hook
  :options '(tab-bar-format-menu-bar
             tab-bar-format-history
             tab-bar-format-tabs
             tab-bar-format-tabs-groups
             tab-bar-separator
             tab-bar-format-add-tab
             tab-bar-format-align-right
             tab-bar-format-global)
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (force-mode-line-update))
  :group 'tab-bar
  :version "28.1")

(defun tab-bar-menu-bar (event)
  "Pop up the same menu as displayed by the menu bar.
Used by `tab-bar-format-menu-bar'."
  (interactive "e")
  (let ((menu (make-sparse-keymap (propertize "Menu Bar" 'hide t))))
    (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
    (map-keymap (lambda (key binding)
                  (when (consp binding)
                    (define-key-after menu (vector key)
                      (copy-sequence binding))))
                (menu-bar-keymap))
    (popup-menu menu event)))

(defun tab-bar-format-menu-bar ()
  "Produce the Menu button for the tab bar that shows the menu bar."
  `((menu-bar menu-item (propertize "Menu" 'face 'tab-bar-tab-inactive)
     tab-bar-menu-bar :help "Menu Bar")))

(defun tab-bar-format-history ()
  "Produce back and forward buttons for the tab bar.
These buttons will be shown when `tab-bar-history-mode' is enabled.
You can hide these buttons by customizing `tab-bar-format' and removing
`tab-bar-format-history' from it."
  (when tab-bar-history-mode
    `((sep-history-back menu-item ,(tab-bar-separator) ignore)
      (history-back
       menu-item ,tab-bar-back-button tab-bar-history-back
       :help "Click to go back in tab history")
      (sep-history-forward menu-item ,(tab-bar-separator) ignore)
      (history-forward
       menu-item ,tab-bar-forward-button tab-bar-history-forward
       :help "Click to go forward in tab history"))))

(defun tab-bar--format-tab (tab i)
  "Format TAB using its index I and return the result as a keymap."
  (append
   `((,(intern (format "sep-%i" i)) menu-item ,(tab-bar-separator) ignore))
   (cond
    ((eq (car tab) 'current-tab)
     `((current-tab
        menu-item
        ,(funcall tab-bar-tab-name-format-function tab i)
        ignore
        :help "Current tab")))
    (t
     `((,(intern (format "tab-%i" i))
        menu-item
        ,(funcall tab-bar-tab-name-format-function tab i)
        ,(alist-get 'binding tab)
        :help "Click to visit tab"))))
   (when (alist-get 'close-binding tab)
     `((,(if (eq (car tab) 'current-tab) 'C-current-tab (intern (format "C-tab-%i" i)))
        menu-item ""
        ,(alist-get 'close-binding tab))))))

(defun tab-bar-format-tabs ()
  "Produce all the tabs for the tab bar."
  (let ((i 0))
    (mapcan
     (lambda (tab)
       (setq i (1+ i))
       (tab-bar--format-tab tab i))
     (funcall tab-bar-tabs-function))))

(defcustom tab-bar-tab-group-function #'tab-bar-tab-group-default
  "Function to get a tab group name.
Function gets one argument: a tab."
  :type 'function
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (force-mode-line-update))
  :group 'tab-bar
  :version "28.1")

(defun tab-bar-tab-group-default (tab)
  (alist-get 'group tab))

(defcustom tab-bar-tab-group-format-function #'tab-bar-tab-group-format-default
  "Function to format a tab group name.
Function gets two arguments, a tab with a group name and its number,
and should return the formatted tab group name to display in the tab bar."
  :type 'function
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (force-mode-line-update))
  :group 'tab-bar
  :version "28.1")

(defun tab-bar-tab-group-format-default (tab i)
  (propertize
   (concat (if tab-bar-tab-hints (format "%d " i) "")
           (funcall tab-bar-tab-group-function tab))
   'face 'tab-bar-tab-group-inactive))

(defcustom tab-bar-tab-group-face-function #'tab-bar-tab-group-face-default
  "Function to define a tab group face.
Function gets one argument: a tab."
  :type 'function
  :group 'tab-bar
  :version "28.1")

(defun tab-bar-tab-group-face-default (tab)
  (if (not (or (eq (car tab) 'current-tab)
               (funcall tab-bar-tab-group-function tab)))
      'tab-bar-tab-ungrouped
    (tab-bar-tab-face-default tab)))

(defun tab-bar--format-tab-group (tab i &optional current-p)
  "Format TAB as a tab that represents a group of tabs.
The argument I is the tab index, and CURRENT-P is non-nil
when the tab is current.  Return the result as a keymap."
  (append
   `((,(intern (format "sep-%i" i)) menu-item ,(tab-bar-separator) ignore))
   `((,(intern (format "group-%i" i))
      menu-item
      ,(if current-p
           (propertize (funcall tab-bar-tab-group-function tab)
                       'face 'tab-bar-tab-group-current)
         (funcall tab-bar-tab-group-format-function tab i))
      ,(if current-p 'ignore
         (or
          (alist-get 'binding tab)
          `(lambda ()
             (interactive)
             (tab-bar-select-tab ,i))))
      :help "Click to visit group"))))

(defun tab-bar-format-tabs-groups ()
  "Produce tabs for the tab bar grouped according to their groups."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-group (funcall tab-bar-tab-group-function
                                 (tab-bar--current-tab-find tabs)))
         (previous-group nil)
         (i 0))
    (mapcan
     (lambda (tab)
       (let ((tab-group (funcall tab-bar-tab-group-function tab)))
         (setq i (1+ i))
         (prog1 (cond
                 ;; Show current group tabs and ungrouped tabs
                 ((or (equal tab-group current-group) (not tab-group))
                  (append
                   ;; Prepend current group name before first tab
                   (when (and (not (equal previous-group tab-group)) tab-group)
                     (tab-bar--format-tab-group tab i t))
                   ;; Override default tab faces to use group faces
                   (let ((tab-bar-tab-face-function tab-bar-tab-group-face-function))
                     (tab-bar--format-tab tab i))))
                 ;; Show first tab of other groups with a group name
                 ((not (equal previous-group tab-group))
                  (tab-bar--format-tab-group tab i))
                 ;; Hide other group tabs
                 (t nil))
           (setq previous-group tab-group))))
     tabs)))

(defun tab-bar-format-add-tab ()
  "Button to add a new tab."
  (when (and tab-bar-new-button-show tab-bar-new-button)
    `((add-tab menu-item ,tab-bar-new-button tab-bar-new-tab
               :help "New tab"))))

(defun tab-bar-format-align-right ()
  "Align the rest of tab bar items to the right."
  (let* ((rest (cdr (memq 'tab-bar-format-align-right tab-bar-format)))
         (rest (tab-bar-format-list rest))
         (rest (mapconcat (lambda (item) (nth 2 item)) rest ""))
         (hpos (length rest))
         (str (propertize " " 'display `(space :align-to (- right ,hpos)))))
    `((align-right menu-item ,str ignore))))

(defun tab-bar-format-global ()
  "Produce display of `global-mode-string' in the tab bar.
When `tab-bar-format-global' is added to `tab-bar-format'
(possibly appended after `tab-bar-format-align-right'),
then modes that display information on the mode line
using `global-mode-string' will display the same text
on the tab bar instead."
  `((global menu-item ,(string-trim-right (format-mode-line global-mode-string)) ignore)))

(defun tab-bar-format-list (format-list)
  (let ((i 0))
    (apply #'append
           (mapcar
            (lambda (format)
              (setq i (1+ i))
              (cond
               ((functionp format)
                (let ((ret (funcall format)))
                  (when (stringp ret)
                    (setq ret `((,(intern (format "str-%i" i))
                                 menu-item ,ret ignore))))
                  ret))))
            format-list))))

(defun tab-bar-make-keymap-1 ()
  "Generate an actual keymap from `tab-bar-map', without caching."
  (append tab-bar-map (tab-bar-format-list tab-bar-format)))


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
                                '(wc wc-point wc-bl wc-bbl
                                  wc-history-back wc-history-forward)
                                (copy-sequence current))
                  current))
              current)
    current))

(push '(tabs . frameset-filter-tabs) frameset-filter-alist)

(defun tab-bar--tab (&optional frame)
  "Make a new tab data structure that can be added to tabs on the FRAME."
  (let* ((tab (tab-bar--current-tab-find nil frame))
         (tab-explicit-name (alist-get 'explicit-name tab))
         (tab-group (alist-get 'group tab))
         (bl  (seq-filter #'buffer-live-p (frame-parameter
                                           frame 'buffer-list)))
         (bbl (seq-filter #'buffer-live-p (frame-parameter
                                           frame 'buried-buffer-list))))
    `(tab
      (name . ,(if tab-explicit-name
                   (alist-get 'name tab)
                 (funcall tab-bar-tab-name-function)))
      (explicit-name . ,tab-explicit-name)
      ,@(if tab-group `((group . ,tab-group)))
      (time . ,(float-time))
      (ws . ,(window-state-get
              (frame-root-window (or frame (selected-frame))) 'writable))
      (wc . ,(current-window-configuration))
      (wc-point . ,(point-marker))
      (wc-bl . ,bl)
      (wc-bbl . ,bbl)
      (wc-history-back . ,(gethash (or frame (selected-frame))
                                   tab-bar-history-back))
      (wc-history-forward . ,(gethash (or frame (selected-frame))
                                      tab-bar-history-forward))
      ;; Copy other possible parameters
      ,@(mapcan (lambda (param)
                  (unless (memq (car param)
                                '(name explicit-name group time
                                  ws wc wc-point wc-bl wc-bbl
                                  wc-history-back wc-history-forward))
                    (list param)))
                (cdr tab)))))

(defun tab-bar--current-tab (&optional tab frame)
  "Make the current tab data structure from TAB on FRAME."
  (tab-bar--current-tab-make (or tab (tab-bar--current-tab-find nil frame))))

(defun tab-bar--current-tab-make (&optional tab)
  "Make the current tab data structure from TAB.
TAB here is an argument meaning \"use tab as template\",
i.e. the tab is created using data from TAB.  This is
necessary when switching tabs, otherwise the destination tab
inherits the current tab's `explicit-name' parameter."
  (let* ((tab-explicit-name (alist-get 'explicit-name tab))
         (tab-group (if tab
                        (alist-get 'group tab)
                      (pcase tab-bar-new-tab-group
                        ((pred stringp) tab-bar-new-tab-group)
                        ((pred functionp) (funcall tab-bar-new-tab-group))))))
    `(current-tab
      (name . ,(if tab-explicit-name
                   (alist-get 'name tab)
                 (funcall tab-bar-tab-name-function)))
      (explicit-name . ,tab-explicit-name)
      ,@(if tab-group `((group . ,tab-group)))
      ;; Copy other possible parameters
      ,@(mapcan (lambda (param)
                  (unless (memq (car param)
                                '(name explicit-name group time
                                  ws wc wc-point wc-bl wc-bbl
                                  wc-history-back wc-history-forward))
                    (list param)))
                (cdr tab)))))

(defun tab-bar--current-tab-find (&optional tabs frame)
  ;; Find the current tab as a pointer to its data structure.
  (assq 'current-tab (or tabs (funcall tab-bar-tabs-function frame))))

(defun tab-bar--current-tab-index (&optional tabs frame)
  ;; Return the index of the current tab.
  (seq-position (or tabs (funcall tab-bar-tabs-function frame))
                'current-tab (lambda (a b) (eq (car a) b))))

(defun tab-bar--tab-index (tab &optional tabs frame)
  ;; Return the index of TAB.
  (seq-position (or tabs (funcall tab-bar-tabs-function frame))
                tab #'eq))

(defun tab-bar--tab-index-by-name (name &optional tabs frame)
  ;; Return the index of TAB by the its NAME.
  (seq-position (or tabs (funcall tab-bar-tabs-function frame))
                name (lambda (a b) (equal (alist-get 'name a) b))))

(defun tab-bar--tab-index-recent (nth &optional tabs frame)
  ;; Return the index of NTH recent tab.
  (let* ((tabs (or tabs (funcall tab-bar-tabs-function frame)))
         (sorted-tabs (tab-bar--tabs-recent tabs frame))
         (tab (nth (1- nth) sorted-tabs)))
    (tab-bar--tab-index tab tabs)))

(defun tab-bar--tabs-recent (&optional tabs frame)
  ;; Return the list of tabs sorted by recency.
  (let* ((tabs (or tabs (funcall tab-bar-tabs-function frame))))
    (seq-sort-by (lambda (tab) (alist-get 'time tab)) #'>
                 (seq-remove (lambda (tab)
                               (eq (car tab) 'current-tab))
                             tabs))))


(defun tab-bar-select-tab (&optional tab-number)
  "Switch to the tab by its absolute position TAB-NUMBER in the tab bar.
When this command is bound to a numeric key (with a key prefix or modifier key
using `tab-bar-select-tab-modifiers'), calling it without an argument
will translate its bound numeric key to the numeric argument.
Also the prefix argument TAB-NUMBER can be used to override
the numeric key, so it takes precedence over the bound digit key.
For example, `<MODIFIER>-2' will select the second tab, but `C-u 15
<MODIFIER>-2' will select the 15th tab.  TAB-NUMBER counts from 1.
Negative TAB-NUMBER counts tabs from the end of the tab bar."
  (interactive "P")
  (unless (integerp tab-number)
    (let ((key (event-basic-type last-command-event)))
      (setq tab-number (if (and (characterp key) (>= key ?1) (<= key ?9))
                           (- key ?0)
                         0))))

  (let* ((tabs (funcall tab-bar-tabs-function))
         (from-index (tab-bar--current-tab-index tabs))
         (to-number (cond ((< tab-number 0) (+ (length tabs) (1+ tab-number)))
                          ((zerop tab-number) (1+ from-index))
                          (t tab-number)))
         (to-index (1- (max 1 (min to-number (length tabs))))))

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
         ((and (window-configuration-p wc)
               ;; Check for such cases as cloning a frame with tabs.
               ;; When tabs were cloned to another frame, then fall back
               ;; to using `window-state-put' below.
               (eq (window-configuration-frame wc) (selected-frame)))
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
          (window-state-put ws nil 'safe)))

        (setq tab-bar-history-omit t)

        (when from-index
          (setf (nth from-index tabs) from-tab))
        (setf (nth to-index tabs) (tab-bar--current-tab-make (nth to-index tabs)))

        (unless tab-bar-mode
          (message "Selected tab '%s'" (alist-get 'name to-tab))))

      (force-mode-line-update))))

(defun tab-bar-switch-to-next-tab (&optional arg)
  "Switch to ARGth next tab.
Interactively, ARG is the prefix numeric argument and defaults to 1."
  (interactive "p")
  (unless (integerp arg)
    (setq arg 1))
  (let* ((tabs (funcall tab-bar-tabs-function))
         (from-index (or (tab-bar--current-tab-index tabs) 0))
         (to-index (mod (+ from-index arg) (length tabs))))
    (tab-bar-select-tab (1+ to-index))))

(defun tab-bar-switch-to-prev-tab (&optional arg)
  "Switch to ARGth previous tab.
Interactively, ARG is the prefix numeric argument and defaults to 1."
  (interactive "p")
  (unless (integerp arg)
    (setq arg 1))
  (tab-bar-switch-to-next-tab (- arg)))

(defun tab-bar-switch-to-last-tab (&optional arg)
  "Switch to the last tab or ARGth tab from the end of the tab bar.
Interactively, ARG is the prefix numeric argument; it defaults to 1,
which means the last tab on the tab bar.  For example, `C-u 2
<MODIFIER>-9' selects the tab before the last tab."
  (interactive "p")
  (tab-bar-select-tab (- (length (funcall tab-bar-tabs-function))
                         (1- (abs (or arg 1))))))

(defun tab-bar-switch-to-recent-tab (&optional arg)
  "Switch to ARGth most recently visited tab.
Interactively, ARG is the prefix numeric argument and defaults to 1."
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
to get the name of the most recently visited tab, the second
most recent, and so on."
  (interactive
   (let* ((recent-tabs (mapcar (lambda (tab)
                                 (alist-get 'name tab))
                               (tab-bar--tabs-recent))))
     (list (completing-read (format-prompt "Switch to tab by name"
                                           (car recent-tabs))
                            recent-tabs nil nil nil nil recent-tabs))))
  (tab-bar-select-tab (1+ (or (tab-bar--tab-index-by-name name) 0))))

(defalias 'tab-bar-select-tab-by-name 'tab-bar-switch-to-tab)


(defun tab-bar-move-tab-to (to-number &optional from-number)
  "Move tab from FROM-NUMBER position to new position at TO-NUMBER.
FROM-NUMBER defaults to the current tab number.
FROM-NUMBER and TO-NUMBER count from 1.
Negative TO-NUMBER counts tabs from the end of the tab bar.
Argument addressing is absolute in contrast to `tab-bar-move-tab'
where argument addressing is relative."
  (interactive "P")
  (let* ((tabs (funcall tab-bar-tabs-function))
         (from-number (or from-number (1+ (tab-bar--current-tab-index tabs))))
         (from-tab (nth (1- from-number) tabs))
         (to-number (if to-number (prefix-numeric-value to-number) 1))
         (to-number (if (< to-number 0) (+ (length tabs) (1+ to-number)) to-number))
         (to-index (max 0 (min (1- to-number) (1- (length tabs))))))
    (setq tabs (delq from-tab tabs))
    (cl-pushnew from-tab (nthcdr to-index tabs))
    (tab-bar-tabs-set tabs)
    (force-mode-line-update)))

(defun tab-bar-move-tab (&optional arg)
  "Move the current tab ARG positions to the right.
Interactively, ARG is the prefix numeric argument and defaults to 1.
If ARG is negative, move the current tab ARG positions to the left.
Argument addressing is relative in contrast to `tab-bar-move-tab-to',
where argument addressing is absolute."
  (interactive "p")
  (let* ((tabs (funcall tab-bar-tabs-function))
         (from-index (or (tab-bar--current-tab-index tabs) 0))
         (to-index (mod (+ from-index arg) (length tabs))))
    (tab-bar-move-tab-to (1+ to-index) (1+ from-index))))

(defun tab-bar-move-tab-backward (&optional arg)
  "Move the current tab ARG positions to the left.
Interactively, ARG is the prefix numeric argument and defaults to 1.
Like `tab-bar-move-tab', but moves in the opposite direction."
  (interactive "p")
  (tab-bar-move-tab (- (or arg 1))))

(defun tab-bar-move-tab-to-frame (arg &optional from-frame from-number to-frame to-number)
  "Move tab from FROM-NUMBER position to new position at TO-NUMBER.
FROM-NUMBER defaults to the current tab number.
FROM-NUMBER and TO-NUMBER count from 1.
FROM-FRAME specifies the source frame and defaults to the selected frame.
TO-FRAME specifies the target frame and defaults the next frame.
Interactively, ARG selects the ARGth next frame on the same terminal,
to which to move the tab; ARG defaults to 1."
  (interactive "P")
  (unless from-frame
    (setq from-frame (selected-frame)))
  (unless to-frame
    (dotimes (_ (prefix-numeric-value arg))
      (setq to-frame (next-frame to-frame))))
  (unless (eq from-frame to-frame)
    (let* ((from-tabs (funcall tab-bar-tabs-function from-frame))
           (from-number (or from-number (1+ (tab-bar--current-tab-index from-tabs))))
           (from-tab (nth (1- from-number) from-tabs))
           (to-tabs (funcall tab-bar-tabs-function to-frame))
           (to-index (max 0 (min (1- (or to-number 1)) (1- (length to-tabs))))))
      (cl-pushnew (assq-delete-all
                   'wc (if (eq (car from-tab) 'current-tab)
                           (tab-bar--tab from-frame)
                         from-tab))
                  (nthcdr to-index to-tabs))
      (with-selected-frame from-frame
        (let ((inhibit-message t) ; avoid message about deleted tab
              (tab-bar-close-last-tab-choice 'delete-frame)
              tab-bar-closed-tabs)
          (tab-bar-close-tab from-number)))
      (tab-bar-tabs-set to-tabs to-frame)
      (force-mode-line-update t))))

(defun tab-bar-detach-tab (&optional from-number)
  "Move tab number FROM-NUMBER to a new frame.
FROM-NUMBER defaults to the current tab (which happens interactively)."
  (interactive (list (1+ (tab-bar--current-tab-index))))
  (let* ((tabs (funcall tab-bar-tabs-function))
         (tab-index (1- (or from-number (1+ (tab-bar--current-tab-index tabs)))))
         (tab-name (alist-get 'name (nth tab-index tabs)))
         ;; On some window managers, `make-frame' selects the new frame,
         ;; so previously selected frame is saved to `from-frame'.
         (from-frame (selected-frame))
         (new-frame (make-frame `((name . ,tab-name)))))
    (tab-bar-move-tab-to-frame nil from-frame from-number new-frame nil)
    (with-selected-frame new-frame
      (tab-bar-close-tab))))

(defun tab-bar-move-window-to-tab ()
  "Move the selected window to a new tab.
This command removes the selected window from the configuration stored
on the current tab, and makes a new tab with that window in its
configuration."
  (interactive)
  (let ((tab-bar-new-tab-choice 'window))
    (tab-bar-new-tab))
  (tab-bar-switch-to-recent-tab)
  (delete-window)
  (tab-bar-switch-to-recent-tab))


(defcustom tab-bar-new-tab-to 'right
  "Where to create a new tab.
If `leftmost', create as the first tab.
If `left', create to the left of the current tab.
If `right', create to the right of the current tab.
If `rightmost', create as the last tab.
If the value is a function, it should return a number as a position
on the tab bar specifying where to add a new tab."
  :type '(choice (const :tag "Add as First" leftmost)
                 (const :tag "Add to Left" left)
                 (const :tag "Add to Right" right)
                 (const :tag "Add as Last" rightmost)
                 (function :tag "Function"))
  :group 'tab-bar
  :version "27.1")

(defcustom tab-bar-tab-post-open-functions nil
  "List of functions to call after creating a new tab.
The current tab is supplied as an argument.  Any modifications made
to the tab argument will be applied after all functions are called."
  :type '(repeat function)
  :group 'tab-bar
  :version "27.1")

(defun tab-bar-new-tab-to (&optional tab-number)
  "Add a new tab at the absolute position TAB-NUMBER.
TAB-NUMBER counts from 1.  If no TAB-NUMBER is specified, then add
a new tab at the position specified by `tab-bar-new-tab-to'.
Negative TAB-NUMBER counts tabs from the end of the tab bar,
and -1 means the new tab will become the last one.
Argument addressing is absolute in contrast to `tab-bar-new-tab',
where argument addressing is relative.
After the tab is created, the hooks in
`tab-bar-tab-post-open-functions' are run."
  (interactive "P")
  (let* ((tabs (funcall tab-bar-tabs-function))
         (from-index (tab-bar--current-tab-index tabs))
         (from-tab (tab-bar--tab)))

    (when tab-bar-new-tab-choice
      ;; Handle the case when it's called in the active minibuffer.
      (when (minibuffer-selected-window)
        (select-window (minibuffer-selected-window)))
      (let ((ignore-window-parameters t))
        (delete-other-windows))
      (unless (eq tab-bar-new-tab-choice 'window)
        ;; Create a new window to get rid of old window parameters
        ;; (e.g. prev/next buffers) of old window.
        (split-window) (delete-window))
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

    (let* ((to-tab (tab-bar--current-tab-make
                    (when (eq tab-bar-new-tab-group t)
                      `((group . ,(alist-get 'group from-tab))))))
           (to-number (and tab-number (prefix-numeric-value tab-number)))
           (to-index (or (if to-number
                             (if (< to-number 0)
                                 (+ (length tabs) (1+ to-number))
                               (1- to-number)))
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
        ;; `pushnew' handles the head of tabs but not frame-parameter
        (tab-bar-tabs-set tabs))

      (run-hook-with-args 'tab-bar-tab-post-open-functions
                          (nth to-index tabs)))

    (when tab-bar-show
      (if (not tab-bar-mode)
          ;; Turn on `tab-bar-mode' since a tab was created.
          ;; Note: this also updates `tab-bar-lines'.
          (tab-bar-mode 1)
        (tab-bar--update-tab-bar-lines)))

    (force-mode-line-update)
    (unless tab-bar-mode
      (message "Added new tab at %s" tab-bar-new-tab-to))))

(defun tab-bar-new-tab (&optional arg from-number)
  "Create a new tab ARG positions to the right.
If a negative ARG, create a new tab ARG positions to the left.
If ARG is zero, create a new tab in place of the current tab.
If no ARG is specified, then add a new tab at the position
specified by `tab-bar-new-tab-to'.
Argument addressing is relative in contrast to `tab-bar-new-tab-to',
where argument addressing is absolute.
If FROM-NUMBER is a tab number, a new tab is created from that tab."
  (interactive "P")
  (when from-number
    (let ((inhibit-message t))
      (tab-bar-select-tab from-number)))
  (if arg
      (let* ((tabs (funcall tab-bar-tabs-function))
             (from-index (or (tab-bar--current-tab-index tabs) 0))
             (to-index (+ from-index (prefix-numeric-value arg))))
        (tab-bar-new-tab-to (1+ to-index)))
    (tab-bar-new-tab-to)))

(defun tab-bar-duplicate-tab (&optional arg from-number)
  "Clone the current tab to ARG positions to the right.
ARG and FROM-NUMBER have the same meaning as in `tab-bar-new-tab'."
  (interactive "P")
  (let ((tab-bar-new-tab-choice nil)
        (tab-bar-new-tab-group t))
    (tab-bar-new-tab arg from-number)))


(defvar tab-bar-closed-tabs nil
  "A list of closed tabs to be able to undo their closing.")

(defcustom tab-bar-close-tab-select 'recent
  "Which tab to make current after closing the specified tab.
If `left', select the adjacent left tab.
If `right', select the adjacent right tab.
If `recent', select the most recently visited tab."
  :type '(choice (const :tag "Select left tab" left)
                 (const :tag "Select right tab" right)
                 (const :tag "Select recent tab" recent))
  :group 'tab-bar
  :version "27.1")

(defcustom tab-bar-close-last-tab-choice nil
  "What to do when the last tab is closed.
If nil, do nothing and show a message, like closing the last window or frame.
If `delete-frame', delete the containing frame, as a web browser would do.
If `tab-bar-mode-disable', disable `tab-bar-mode' so that tabs no longer show
in the frame.
If the value is a function, call that function with the tab to be closed
as an argument."
  :type '(choice (const    :tag "Do nothing and show message" nil)
                 (const    :tag "Close the containing frame" delete-frame)
                 (const    :tag "Disable tab-bar-mode" tab-bar-mode-disable)
                 (function :tag "Function"))
  :group 'tab-bar
  :version "27.1")

(defcustom tab-bar-tab-prevent-close-functions nil
  "List of functions to call to determine whether to close a tab.
The tab to be closed and a boolean indicating whether or not it
is the only tab in the frame are supplied as arguments.  If any
function returns a non-nil value, the tab will not be closed."
  :type '(repeat function)
  :group 'tab-bar
  :version "27.1")

(defcustom tab-bar-tab-pre-close-functions nil
  "List of functions to call before closing a tab.
Each function is called with two arguments: the tab to be closed
and a boolean indicating whether or not it is the only tab on its frame."
  :type '(repeat function)
  :group 'tab-bar
  :version "27.1")

(defun tab-bar-close-tab (&optional tab-number to-number)
  "Close the tab specified by its absolute position TAB-NUMBER.
If no TAB-NUMBER is specified, then close the current tab and switch
to the tab specified by `tab-bar-close-tab-select'.
Interactively, TAB-NUMBER is the prefix numeric argument, and defaults to 1.
TAB-NUMBER counts from 1.
Optional TO-NUMBER could be specified to override the value of
`tab-bar-close-tab-select' programmatically with a position
of an existing tab to select after closing the current tab.
TO-NUMBER counts from 1.

The functions in `tab-bar-tab-prevent-close-functions' will be
run to determine whether or not to close the tab.
Just before the tab is closed, the functions in
`tab-bar-tab-pre-close-functions' will be run.  The base behavior
for the last tab on a frame is determined by
`tab-bar-close-last-tab-choice'."
  (interactive "P")
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-index (tab-bar--current-tab-index tabs))
         (close-index (if (integerp tab-number) (1- tab-number) current-index))
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
          (let ((to-index (or (if to-number (1- to-number))
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
          (tab-bar-tabs-set (delq close-tab tabs)))

        ;; Recalculate `tab-bar-lines' and update frames
        (tab-bar--update-tab-bar-lines)

        (force-mode-line-update)
        (unless tab-bar-mode
          (message "Deleted tab and switched to %s" tab-bar-close-tab-select))))))

(defun tab-bar-close-tab-by-name (name)
  "Close the tab given its NAME.
Interactively, prompt for NAME."
  (interactive
   (list (completing-read "Close tab by name: "
                          (mapcar (lambda (tab)
                                    (alist-get 'name tab))
                                  (funcall tab-bar-tabs-function)))))
  (tab-bar-close-tab (1+ (tab-bar--tab-index-by-name name))))

(defun tab-bar-close-other-tabs (&optional tab-number)
  "Close all tabs on the selected frame, except the tab TAB-NUMBER.
TAB-NUMBER counts from 1 and defaults to the current tab (which
happens interactively)."
  (interactive)
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-index (tab-bar--current-tab-index tabs))
         (keep-index (if (integerp tab-number)
                         (1- (max 1 (min tab-number (length tabs))))
                       current-index))
         (index 0))

    (when (nth keep-index tabs)
      (unless (eq keep-index current-index)
        (tab-bar-select-tab (1+ keep-index))
        (setq tabs (funcall tab-bar-tabs-function)))

      (dolist (tab tabs)
        (unless (or (eq index keep-index)
                    (run-hook-with-args-until-success
                     'tab-bar-tab-prevent-close-functions tab
                     ;; `last-tab-p' logically can't ever be true
                     ;; if we make it this far
                     nil))
          (push `((frame . ,(selected-frame))
                  (index . ,index)
                  (tab . ,tab))
                tab-bar-closed-tabs)
          (run-hook-with-args 'tab-bar-tab-pre-close-functions tab nil)
          (setq tabs (delq tab tabs)))
        (setq index (1+ index)))
      (tab-bar-tabs-set tabs)

      ;; Recalculate tab-bar-lines and update frames
      (tab-bar--update-tab-bar-lines)

      (force-mode-line-update)
      (unless tab-bar-mode
        (message "Deleted all other tabs")))))

(defun tab-bar-undo-close-tab ()
  "Restore the most recently closed tab."
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
            (tab-bar-tabs-set tabs))
          (tab-bar-select-tab (1+ index))))

    (message "No more closed tabs to undo")))


(defun tab-bar-rename-tab (name &optional tab-number)
  "Give the tab specified by its absolute position TAB-NUMBER a new NAME.
If no TAB-NUMBER is specified, then rename the current tab.
Interactively, TAB-NUMBER is the prefix numeric argument, and defaults
to the current tab.
TAB-NUMBER counts from 1.
Interactively, prompt for the new NAME.
If NAME is the empty string, then use the automatic name
function `tab-bar-tab-name-function'."
  (interactive
   (let* ((tabs (funcall tab-bar-tabs-function))
          (tab-number (or current-prefix-arg (1+ (tab-bar--current-tab-index tabs))))
          (tab-name (alist-get 'name (nth (1- tab-number) tabs))))
     (list (read-from-minibuffer
            "New name for tab (leave blank for automatic naming): "
            nil nil nil nil tab-name)
           current-prefix-arg)))
  (let* ((tabs (funcall tab-bar-tabs-function))
         (tab-index (if (integerp tab-number)
                        (1- (max 0 (min tab-number (length tabs))))
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
  "Rename the tab named TAB-NAME to NEW-NAME.
Interactively, prompt for TAB-NAME and NEW-NAME.
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


;;; Tab groups

(defun tab-bar-move-tab-to-group (&optional tab)
  "Relocate TAB (by default, the current tab) closer to its group."
  (interactive)
  (let* ((tabs (funcall tab-bar-tabs-function))
         (tab (or tab (tab-bar--current-tab-find tabs)))
         (tab-index (tab-bar--tab-index tab))
         (group (alist-get 'group tab))
         ;; Beginning position of the same group
         (beg (seq-position tabs group
                            (lambda (tb gr)
                              (and (not (eq tb tab))
                                   (equal (alist-get 'group tb) gr)))))
         ;; Size of the same group
         (len (when beg
                (seq-position (nthcdr beg tabs) group
                              (lambda (tb gr)
                                (not (equal (alist-get 'group tb) gr))))))
         (pos (when beg
                (cond
                 ;; Don't move tab when it's already inside group bounds
                 ((and len (>= tab-index beg) (<= tab-index (+ beg len))) nil)
                 ;; Move tab from the right to the group end
                 ((and len (> tab-index (+ beg len))) (+ beg len 1))
                 ;; Move tab from the left to the group beginning
                 ((< tab-index beg) beg)))))
    (when pos
      (tab-bar-move-tab-to pos (1+ tab-index)))))

(defcustom tab-bar-tab-post-change-group-functions nil
  "List of functions to call after changing a tab group.
The current tab is supplied as an argument."
  :type 'hook
  :options '(tab-bar-move-tab-to-group)
  :group 'tab-bar
  :version "28.1")

(defun tab-bar-change-tab-group (group-name &optional tab-number)
  "Add the tab specified by its absolute position TAB-NUMBER to GROUP-NAME.
If no TAB-NUMBER is specified, then set the GROUP-NAME for the current tab.
Interactively, TAB-NUMBER is the prefix numeric argument, and the command
prompts for GROUP-NAME.
TAB-NUMBER counts from 1.
If GROUP-NAME is the empty string, then remove the tab from any group.
While using this command, you might also want to replace
`tab-bar-format-tabs' with `tab-bar-format-tabs-groups' in
`tab-bar-format' to group tabs on the tab bar."
  (interactive
   (let* ((tabs (funcall tab-bar-tabs-function))
          (tab-number (or current-prefix-arg
                         (1+ (tab-bar--current-tab-index tabs))))
          (group-name (funcall tab-bar-tab-group-function
                               (nth (1- tab-number) tabs))))
     (list (completing-read
            "Group name for tab (leave blank to remove group): "
            (delete-dups
             (delq nil (cons group-name
                             (mapcar (lambda (tab)
                                       (funcall tab-bar-tab-group-function tab))
                                     (funcall tab-bar-tabs-function))))))
           current-prefix-arg)))
  (let* ((tabs (funcall tab-bar-tabs-function))
         (tab-index (if tab-number
                        (1- (max 0 (min tab-number (length tabs))))
                      (tab-bar--current-tab-index tabs)))
         (tab (nth tab-index tabs))
         (group (assq 'group tab))
         (group-new-name (and (> (length group-name) 0) group-name)))
    (if group
        (setcdr group group-new-name)
      (nconc tab `((group . ,group-new-name))))

    (run-hook-with-args 'tab-bar-tab-post-change-group-functions tab)

    (force-mode-line-update)
    (unless tab-bar-mode
      (message "Set tab group to '%s'" group-new-name))))

(defun tab-bar-close-group-tabs (group-name)
  "Close all tabs that belong to GROUP-NAME on the selected frame.
Interactively, prompt for GROUP-NAME."
  (interactive
   (let ((group-name (funcall tab-bar-tab-group-function
                              (tab-bar--current-tab-find))))
     (list (completing-read
            "Close all tabs with group name: "
            (delete-dups
             (delq nil (cons group-name
                             (mapcar (lambda (tab)
                                       (funcall tab-bar-tab-group-function tab))
                                     (funcall tab-bar-tabs-function)))))))))
  (let* ((close-group (and (> (length group-name) 0) group-name))
         (tab-bar-tab-prevent-close-functions
          (cons (lambda (tab _last-tab-p)
                  (not (equal (funcall tab-bar-tab-group-function tab)
                              close-group)))
                tab-bar-tab-prevent-close-functions)))
    (tab-bar-close-other-tabs)

    (when (equal (funcall tab-bar-tab-group-function
                          (tab-bar--current-tab-find))
                 close-group)
      (tab-bar-close-tab))))


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

(defun tab-bar--history-pre-change ()
  (setq tab-bar-history-old-minibuffer-depth (minibuffer-depth))
  ;; Store window-configuration before possibly entering the minibuffer.
  (when (zerop tab-bar-history-old-minibuffer-depth)
    (setq tab-bar-history-old
          `((wc . ,(current-window-configuration))
            (wc-point . ,(point-marker))))))

(defun tab-bar--history-change ()
  (when (and (not tab-bar-history-omit)
             tab-bar-history-old
             ;; Store window-configuration before possibly entering
             ;; the minibuffer.
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
                                                :margin ,tab-bar-button-margin
                                                :ascent center))
                               tab-bar-back-button))
        (when (and tab-bar-mode (not (get-text-property 0 'display tab-bar-forward-button)))
          ;; This file is pre-loaded so only here we can use the right data-directory:
          (add-text-properties 0 (length tab-bar-forward-button)
                               `(display (image :type xpm
                                                :file "tabs/right-arrow.xpm"
                                                :margin ,tab-bar-button-margin
                                                :ascent center))
                               tab-bar-forward-button))

        (add-hook 'pre-command-hook 'tab-bar--history-pre-change)
        (add-hook 'window-configuration-change-hook 'tab-bar--history-change))
    (remove-hook 'pre-command-hook 'tab-bar--history-pre-change)
    (remove-hook 'window-configuration-change-hook 'tab-bar--history-change)))


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
  (let ((dir default-directory))
    (let ((tab-bar-new-tab-choice t)
          ;; Don't enable tab-bar-mode if it's disabled
          (tab-bar-show nil))
      (tab-bar-new-tab))
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

(defvar-local tab-switcher-column 3)

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
  "Move to ARGth next line in the list of tabs.
Interactively, ARG is the prefix numeric argument and defaults to 1."
  (interactive "p")
  (forward-line arg)
  (beginning-of-line)
  (move-to-column tab-switcher-column))

(defun tab-switcher-prev-line (&optional arg)
  "Move to ARGth previous line in the list of tabs.
Interactively, ARG is the prefix numeric argument and defaults to 1."
  (interactive "p")
  (forward-line (- arg))
  (beginning-of-line)
  (move-to-column tab-switcher-column))

(defun tab-switcher-unmark (&optional backup)
  "Cancel requested operations on window configuration on this line and move down.
With prefix arg, move up instead."
  (interactive "P")
  (beginning-of-line)
  (move-to-column tab-switcher-column)
  (let* ((buffer-read-only nil))
    (delete-char 1)
    (insert " "))
  (forward-line (if backup -1 1))
  (move-to-column tab-switcher-column))

(defun tab-switcher-backup-unmark ()
  "Move up one line and cancel requested operations on window configuration there."
  (interactive)
  (forward-line -1)
  (tab-switcher-unmark)
  (forward-line -1)
  (move-to-column tab-switcher-column))

(defun tab-switcher-delete (&optional arg)
  "Mark window configuration on this line to be deleted by \\<tab-switcher-mode-map>\\[tab-switcher-execute] command.
Prefix arg says how many window configurations to delete.
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
  "Delete the window configuration from the list of tabs."
  (push `((frame . ,(selected-frame))
          (index . ,(tab-bar--tab-index tab))
          (tab . ,tab))
        tab-bar-closed-tabs)
  (tab-bar-tabs-set (delq tab (funcall tab-bar-tabs-function))))

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
This command replaces all the existing windows in the selected frame
with those specified by the selected window configuration."
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
  "Return the tab that owns the window whose buffer is BUFFER-OR-NAME.
BUFFER-OR-NAME may be a buffer or a buffer name, and defaults to
the current buffer.

The optional argument ALL-FRAMES specifies the frames to consider:

- t means consider all tabs on all existing frames.

- `visible' means consider all tabs on all visible frames.

- A frame means consider all tabs on that frame only.

- Any other value of ALL-FRAMES means consider all tabs on the
selected frame and no others.

When the optional argument IGNORE-CURRENT-TAB is non-nil,
don't take into account the buffers in the currently selected tab.
Otherwise, prefer buffers of the current tab."
  (let ((buffer (if buffer-or-name
                    (get-buffer buffer-or-name)
                  (current-buffer))))
    (when (bufferp buffer)
      (seq-some
       (lambda (frame)
         (seq-some
          (lambda (tab)
            (when (if (eq (car tab) 'current-tab)
                      (get-buffer-window buffer frame)
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
          (let* ((tabs (funcall tab-bar-tabs-function frame))
                 (current-tab (tab-bar--current-tab-find tabs)))
            (setq tabs (remq current-tab tabs))
            (if ignore-current-tab
                ;; Use tabs without current-tab.
                tabs
              ;; Make sure current-tab is at the beginning of tabs.
              (cons current-tab tabs)))))
       (tab-bar--reusable-frames all-frames)))))

(defun display-buffer-in-tab (buffer alist)
  "Display BUFFER in a tab using display actions in ALIST.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

If ALIST contains a `tab-name' entry, it creates a new tab with that name and
displays BUFFER in a new tab.  If a tab with this name already exists, it
switches to that tab before displaying BUFFER.  The `tab-name' entry can be
a function, in which case it is called with two arguments: BUFFER and ALIST,
and should return the tab name.  When a `tab-name' entry is omitted, create
a new tab without an explicit name.

The ALIST entry `tab-group' (string or function) defines the tab group.

If ALIST contains a `reusable-frames' entry, its value determines
which frames to search for a reusable tab:
  nil -- do not reuse any frames;
  a frame  -- just that frame;
  `visible' -- all visible frames;
  0 -- all frames on the current terminal;
  t -- all frames;
  other non-nil values -- use the selected frame.

If ALIST contains a non-nil `ignore-current-tab' entry, then the buffers
of the current tab are skipped when searching for a reusable tab.
Otherwise, prefer buffers of the current tab.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let* ((reusable-frames (alist-get 'reusable-frames alist))
         (ignore-current-tab (alist-get 'ignore-current-tab alist))
         (reusable-tab (when reusable-frames
                         (tab-bar-get-buffer-tab buffer reusable-frames
                                                 ignore-current-tab))))
    (if reusable-tab
        (let* ((frame (alist-get 'frame reusable-tab))
               (index (alist-get 'index reusable-tab)))
          (when frame
            (select-frame-set-input-focus frame))
          (when index
            (tab-bar-select-tab (1+ index)))
          (when (get-buffer-window buffer frame)
            (select-window (get-buffer-window buffer frame))))
      (let ((tab-name (alist-get 'tab-name alist)))
        (when (functionp tab-name)
          (setq tab-name (funcall tab-name buffer alist)))
        (if tab-name
            (let ((tab-index (tab-bar--tab-index-by-name tab-name)))
              (if tab-index
                  (progn
                    (tab-bar-select-tab (1+ tab-index))
                    (when (get-buffer-window buffer)
                      (select-window (get-buffer-window buffer))))
                (display-buffer-in-new-tab buffer alist)))
          (display-buffer-in-new-tab buffer alist))))))

(defun display-buffer-in-new-tab (buffer alist)
  "Display BUFFER in a new tab using display actions in ALIST.
ALIST is an association list of action symbols and values.  See
Info node `(elisp) Buffer Display Action Alists' for details of
such alists.

Like `display-buffer-in-tab', but always creates a new tab unconditionally,
without checking if a suitable tab already exists.

If ALIST contains a `tab-name' entry, it creates a new tab with that name
and displays BUFFER in a new tab.  The `tab-name' entry can be a function,
in which case it is called with two arguments: BUFFER and ALIST, and should
return the tab name.  When a `tab-name' entry is omitted, create a new tab
without an explicit name.

The ALIST entry `tab-group' (string or function) defines the tab group.

This is an action function for buffer display, see Info
node `(elisp) Buffer Display Action Functions'.  It should be
called only by `display-buffer' or a function directly or
indirectly called by the latter."
  (let ((tab-bar-new-tab-choice t))
    (tab-bar-new-tab)
    (let ((tab-name (alist-get 'tab-name alist)))
      (when (functionp tab-name)
        (setq tab-name (funcall tab-name buffer alist)))
      (when tab-name
        (tab-bar-rename-tab tab-name)))
    (let ((tab-group (alist-get 'tab-group alist)))
      (when (functionp tab-group)
        (setq tab-group (funcall tab-group buffer alist)))
      (when tab-group
        (tab-bar-change-tab-group tab-group)))
    (window--display-buffer buffer (selected-window) 'tab alist)))

(defun switch-to-buffer-other-tab (buffer-or-name &optional _norecord)
  "Switch to buffer BUFFER-OR-NAME in another tab.
Like \\[switch-to-buffer-other-frame] (which see), but creates a new tab.
Interactively, prompt for the buffer to switch to."
  (declare (advertised-calling-convention (buffer-or-name) "28.1"))
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other tab: ")))
  (display-buffer (window-normalize-buffer-to-switch-to buffer-or-name)
                  '((display-buffer-in-tab)
                    (inhibit-same-window . nil))))

(defun find-file-other-tab (filename &optional wildcards)
  "Edit file FILENAME, in another tab.
Like \\[find-file-other-frame] (which see), but creates a new tab.
Interactively, prompt for FILENAME.
If WILDCARDS is non-nil, FILENAME can include widcards, and all matching
files will be visited."
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

(defun find-file-read-only-other-tab (filename &optional wildcards)
  "Edit file FILENAME, in another tab, but don't allow changes.
Like \\[find-file-other-frame] (which see), but creates a new tab.
Like \\[find-file-other-tab], but marks buffer as read-only.
Use \\[read-only-mode] to permit editing.
Interactively, prompt for FILENAME.
If WILDCARDS is non-nil, FILENAME can include widcards, and all matching
files will be visited."
  (interactive
   (find-file-read-args "Find file read-only in other tab: "
                        (confirm-nonexistent-file-or-buffer)))
  (find-file--read-only (lambda (filename wildcards)
                          (window-buffer
                           (find-file-other-tab filename wildcards)))
                        filename wildcards))

(defun other-tab-prefix ()
  "Display the buffer of the next command in a new tab.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new tab before displaying the buffer, or switches to the tab
that already contains that buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
  (interactive)
  (display-buffer-override-next-command
   (lambda (buffer alist)
     (cons (progn
             (display-buffer-in-tab
              buffer (append alist '((inhibit-same-window . nil))))
             (selected-window))
           'tab))
   nil "[other-tab]")
  (message "Display next command buffer in a new tab..."))


;;; Short aliases and keybindings

(defalias 'tab-new             'tab-bar-new-tab)
(defalias 'tab-new-to          'tab-bar-new-tab-to)
(defalias 'tab-duplicate       'tab-bar-duplicate-tab)
(defalias 'tab-detach          'tab-bar-detach-tab)
(defalias 'tab-window-detach   'tab-bar-move-window-to-tab)
(defalias 'tab-close           'tab-bar-close-tab)
(defalias 'tab-close-other     'tab-bar-close-other-tabs)
(defalias 'tab-close-group     'tab-bar-close-group-tabs)
(defalias 'tab-undo            'tab-bar-undo-close-tab)
(defalias 'tab-select          'tab-bar-select-tab)
(defalias 'tab-switch          'tab-bar-switch-to-tab)
(defalias 'tab-next            'tab-bar-switch-to-next-tab)
(defalias 'tab-previous        'tab-bar-switch-to-prev-tab)
(defalias 'tab-last            'tab-bar-switch-to-last-tab)
(defalias 'tab-recent          'tab-bar-switch-to-recent-tab)
(defalias 'tab-move            'tab-bar-move-tab)
(defalias 'tab-move-to         'tab-bar-move-tab-to)
(defalias 'tab-rename          'tab-bar-rename-tab)
(defalias 'tab-group           'tab-bar-change-tab-group)
(defalias 'tab-list            'tab-switcher)

(define-key tab-prefix-map "n" 'tab-duplicate)
(define-key tab-prefix-map "N" 'tab-new-to)
(define-key tab-prefix-map "2" 'tab-new)
(define-key tab-prefix-map "1" 'tab-close-other)
(define-key tab-prefix-map "0" 'tab-close)
(define-key tab-prefix-map "u" 'tab-undo)
(define-key tab-prefix-map "o" 'tab-next)
(define-key tab-prefix-map "O" 'tab-previous)
(define-key tab-prefix-map "m" 'tab-move)
(define-key tab-prefix-map "M" 'tab-move-to)
(define-key tab-prefix-map "G" 'tab-group)
(define-key tab-prefix-map "r" 'tab-rename)
(define-key tab-prefix-map "\r" 'tab-switch)
(define-key tab-prefix-map "b" 'switch-to-buffer-other-tab)
(define-key tab-prefix-map "f" 'find-file-other-tab)
(define-key tab-prefix-map "\C-f" 'find-file-other-tab)
(define-key tab-prefix-map "\C-r" 'find-file-read-only-other-tab)
(define-key tab-prefix-map "t" 'other-tab-prefix)

(defvar tab-bar-switch-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "o" 'tab-next)
    (define-key map "O" 'tab-previous)
    map)
  "Keymap to repeat tab switch key sequences `C-x t o o O'.
Used in `repeat-mode'.")
(put 'tab-next 'repeat-map 'tab-bar-switch-repeat-map)
(put 'tab-previous 'repeat-map 'tab-bar-switch-repeat-map)

(defvar tab-bar-move-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" 'tab-move)
    (define-key map "M" 'tab-bar-move-tab-backward)
    map)
  "Keymap to repeat tab move key sequences `C-x t m m M'.
Used in `repeat-mode'.")
(put 'tab-move 'repeat-map 'tab-bar-move-repeat-map)
(put 'tab-bar-move-tab-backward 'repeat-map 'tab-bar-move-repeat-map)


(provide 'tab-bar)

;;; tab-bar.el ends here
