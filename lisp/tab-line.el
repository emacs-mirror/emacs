;;; tab-line.el --- window-local tabs with window buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2026 Free Software Foundation, Inc.

;; Author: Juri Linkov <juri@linkov.net>
;; Keywords: windows tabs
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

;; To enable this mode, run `M-x global-tab-line-mode'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'icons)


(defgroup tab-line nil
  "Window-local tabs."
  :group 'convenience
  :version "27.1")

(defcustom tab-line-tab-face-functions
  '(tab-line-tab-face-modified tab-line-tab-face-special)
  "Functions called to modify tab faces.
Each function is called with five arguments: the tab, a list of
all tabs, the face returned by the previously called modifier,
whether the tab is a buffer (when nil, the buffer is extracted from
the association list using the key `buffer'), and whether the tab
is selected."
  :type '(repeat
          (choice (function-item tab-line-tab-face-special)
                  (function-item tab-line-tab-face-modified)
                  (function-item tab-line-tab-face-inactive-alternating)
                  (function-item tab-line-tab-face-group)
                  (function :tag "Custom function")))
  :group 'tab-line
  :version "28.1")

(defgroup tab-line-faces '((tab-line custom-face)) ; tab-line is defined in faces.el
  "Faces used in the tab line."
  :group 'tab-line
  :group 'faces
  :version "27.1")

(defface tab-line-tab
  '((default :inherit tab-line)
    (((class color) (min-colors 88) (background light))
     :box (:line-width 1 :style released-button))
    (((class color) (min-colors 88) (background dark))
     :box (:line-width 1 :style released-button)
     :background "grey40"
     :foreground "white")
    (t :inverse-video nil))
  "Tab line face for selected tab."
  :version "31.1"
  :group 'tab-line-faces)

(defface tab-line-tab-inactive
  '((default :inherit tab-line-tab)
    (((class color) (min-colors 88) (background light))
     :background "grey75")
    (((class color) (min-colors 88) (background dark))
     :background "grey20")
    (t :inverse-video t))
  "Tab line face for non-selected tab."
  :version "31.1"
  :group 'tab-line-faces)

(defface tab-line-tab-inactive-alternate
  '((t :inherit tab-line-tab-inactive :background "grey65"))
  "Alternate face for inactive tab-line tabs.
Applied to alternating tabs when option
`tab-line-tab-face-functions' includes function
`tab-line-tab-face-inactive-alternating'."
  :version "28.1"
  :group 'tab-line-faces)

(defface tab-line-tab-special
  '((default :weight bold)
    (((supports :slant italic))
     :slant italic :weight normal))
  "Face for special (i.e. non-file-backed) tabs.
Applied when option `tab-line-tab-face-functions' includes
function `tab-line-tab-face-special'."
  :version "28.1"
  :group 'tab-line-faces)

(defface tab-line-tab-modified
  '((t :inherit font-lock-doc-face))
  "Face for modified tabs.
Applied when option `tab-line-tab-face-functions' includes
function `tab-line-tab-face-modified'."
  :version "28.1"
  :group 'tab-line-faces)

(defface tab-line-tab-group
  '((t :inherit tab-line :box nil))
  "Face for group tabs.
Applied when option `tab-line-tab-face-functions' includes
function `tab-line-tab-face-group'."
  :version "28.1"
  :group 'tab-line-faces)

(defface tab-line-tab-current
  '((default :inherit tab-line-tab)
    (((class color) (min-colors 88) (background light))
     :background "grey85")
    (((class color) (min-colors 88) (background dark))
     :background "grey40"))
  "Tab line face for tab with current buffer in selected window."
  :version "31.1"
  :group 'tab-line-faces)

(defface tab-line-highlight
  '((((class color) (min-colors 88) (background light))
     :box (:line-width 1 :style released-button)
     :background "grey85"
     :foreground "black")
    (((class color) (min-colors 88) (background dark))
     :box (:line-width 1 :style released-button)
     :background "grey40"
     :foreground "white")
    (t :inverse-video nil))
  "Tab line face for highlighting."
  :version "31.1"
  :group 'tab-line-faces)

(defface tab-line-close-highlight
  '((t :foreground "red"))
  "Tab line face for highlighting of the close button."
  :version "27.1"
  :group 'tab-line-faces)


(defvar-keymap tab-line-tab-map
  :doc "Local keymap for `tab-line-mode' window tabs."
  "<tab-line> <down-mouse-1>"      #'tab-line-select-tab
  "<tab-line> <mouse-2>"           #'tab-line-close-tab
  "<tab-line> <down-mouse-3>"      #'tab-line-tab-context-menu
  "<tab-line> <touchscreen-begin>" #'tab-line-select-tab
  "RET" #'tab-line-select-tab)

(defvar-keymap tab-line-add-map
  :doc "Local keymap to add `tab-line-mode' window tabs."
  "<tab-line> <down-mouse-1>"      #'tab-line-new-tab
  "<tab-line> <down-mouse-2>"      #'tab-line-new-tab
  "<tab-line> <touchscreen-begin>" #'tab-line-new-tab
  "RET" #'tab-line-new-tab)

(defvar-keymap tab-line-tab-close-map
  :doc "Local keymap to close `tab-line-mode' window tabs."
  "<tab-line> <mouse-1>"           #'tab-line-close-tab
  "<tab-line> <mouse-2>"           #'tab-line-close-tab
  "<tab-line> <touchscreen-begin>" #'tab-line-close-tab)

(defvar-keymap tab-line-left-map
  :doc "Local keymap to scroll `tab-line-mode' window tabs to the left."
  "<tab-line> <down-mouse-1>"      #'tab-line-hscroll-left
  "<tab-line> <down-mouse-2>"      #'tab-line-hscroll-left
  "<tab-line> <touchscreen-begin>" #'tab-line-hscroll-left
  "RET"                            #'tab-line-new-tab)

(defvar-keymap tab-line-right-map
  :doc "Local keymap to scroll `tab-line-mode' window tabs to the right."
  "<tab-line> <down-mouse-1>"      #'tab-line-hscroll-right
  "<tab-line> <down-mouse-2>"      #'tab-line-hscroll-right
  "<tab-line> <touchscreen-begin>" #'tab-line-hscroll-right
  "RET"                            #'tab-line-new-tab)


(defcustom tab-line-new-tab-choice t
  "Defines what to show in a new tab.
If t, display a selection menu with all available buffers.
If the value is a function, call it with no arguments."
  :type '(choice (const     :tag "Buffer menu" t)
                 (function  :tag "Function"))
  :group 'tab-line
  :version "27.1")

(defcustom tab-line-new-button-show t
  "If non-nil, show the \"New tab\" button in the tab line."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (force-mode-line-update t))
  :group 'tab-line
  :version "27.1")

(define-icon tab-line-new nil
  `((image "symbols/plus_16.svg" "tabs/new.xpm"
           :face shadow
           :height (1 . em)
           :margin (2 . 0)
           :ascent center)
    (text " + "))
  "Icon for creating a new tab."
  :version "30.1"
  :help-echo "New tab")

(defvar tab-line-new-button
  (propertize (icon-string 'tab-line-new)
              'rear-nonsticky nil
              'keymap tab-line-add-map
              'mouse-face 'tab-line-highlight
              'help-echo "Click to add tab")
  "Button for creating a new tab.")

(defvar tab-line-new-button-functions
  '(tab-line-tabs-window-buffers
    tab-line-tabs-fixed-window-buffers)
  "Functions of `tab-line-tabs-function' for which to show a new button.")

(defcustom tab-line-close-button-show t
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
         (force-mode-line-update t))
  :group 'tab-line
  :version "27.1")

(define-icon tab-line-close nil
  `((image "symbols/cross_16.svg" "tabs/close.xpm"
           :face shadow
           :height (1 . em)
           :margin (2 . 0)
           :ascent center)
    (text " x"))
  "Icon for closing the clicked tab."
  :version "30.1"
  :help-echo "Click to close tab")

(defvar tab-line-close-button
  (propertize (icon-string 'tab-line-close)
              'rear-nonsticky nil ;; important to not break auto-scroll
              'keymap tab-line-tab-close-map
              'mouse-face 'tab-line-close-highlight
              'help-echo "Click to close tab")
  "Button for closing the clicked tab.")

(defcustom tab-line-close-modified-button-show t
  "If non-nil, the close button appearance will change when its buffer is modified."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (force-mode-line-update t))
  :group 'tab-line
  :version "31.1")

(define-icon tab-line-close-modified nil
  `((image "symbols/dot_medium_16.svg" "tabs/close-modified.xpm"
           :face shadow
           :height (1 . em)
           :margin (2 . 0)
           :ascent center)
    (symbol ,(concat " " [#x2022])) ; bullet
    (text " *"))
  "Icon for closing the clicked tab when tab is modified."
  :version "31.1"
  :help-echo "Click to close tab")

(defvar tab-line-close-modified-button
  (propertize (icon-string 'tab-line-close-modified)
              'rear-nonsticky nil
              'keymap tab-line-tab-close-map
              'mouse-face 'tab-line-close-highlight
              'help-echo "Click to close tab")
  "Button for closing the clicked tab when tab is modified.")

(define-icon tab-line-left nil
  `((image "symbols/chevron_left_16.svg" "tabs/left-arrow.xpm"
           :face shadow
           :height (1 . em)
           :margin (2 . 0)
           :ascent center)
    (text " <"))
  "Icon for scrolling horizontally to the left."
  :version "30.1")

(defvar tab-line-left-button
  (propertize (icon-string 'tab-line-left)
              'rear-nonsticky nil
              'keymap tab-line-left-map
              'mouse-face 'tab-line-highlight
              'help-echo "Click to scroll left")
  "Button for scrolling horizontally to the left.")

(define-icon tab-line-right nil
  `((image "symbols/chevron_right_16.svg" "tabs/right-arrow.xpm"
           :face shadow
           :height (1 . em)
           :margin (2 . 0)
           :ascent center)
    (text "> "))
  "Icon for scrolling horizontally to the right."
  :version "30.1")

(defvar tab-line-right-button
  (propertize (icon-string 'tab-line-right)
              'rear-nonsticky nil
              'keymap tab-line-right-map
              'mouse-face 'tab-line-highlight
              'help-echo "Click to scroll right")
  "Button for scrolling horizontally to the right.")

(defvar tab-line-separator nil
  "String that delimits tabs.")


(defcustom tab-line-tab-name-function #'tab-line-tab-name-buffer
  "Function to get a tab name.
The function is called with one or two arguments: the buffer or
another object whose tab's name is requested, and, optionally,
the list of all tabs.  The result of this function is cached
using `tab-line-cache-key-function'."
  :type '(choice (const :tag "Buffer name"
                        tab-line-tab-name-buffer)
                 (const :tag "Truncated buffer name"
                        tab-line-tab-name-truncated-buffer)
                 (function :tag "Function"))
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (tab-line-force-update t))
  :group 'tab-line
  :version "27.1")

(defun tab-line-tab-name-buffer (buffer &optional _buffers)
  "Generate tab name from BUFFER.
Reduce tab width proportionally to space taken by other tabs.
This function can be overridden by changing the default value of the
variable `tab-line-tab-name-function'."
  (buffer-name buffer))

(defcustom tab-line-tab-name-truncated-max 20
  "Maximum length of the tab name from the current buffer.
Effective when `tab-line-tab-name-function' is customized
to `tab-line-tab-name-truncated-buffer'."
  :type 'natnum
  :group 'tab-line
  :version "27.1")

(defvar tab-line-tab-name-ellipsis t)

(defun tab-line-tab-name-truncated-buffer (buffer &optional _buffers)
  "Generate tab name from BUFFER, truncating it as needed.
Truncate it to the length specified by `tab-line-tab-name-truncated-max'.
If truncated, append ellipsis per `tab-line-tab-name-ellipsis'."
  (let ((tab-name (buffer-name buffer)))
    (if (< (length tab-name) tab-line-tab-name-truncated-max)
        tab-name
      (propertize (truncate-string-to-width
                   tab-name tab-line-tab-name-truncated-max nil nil
                   tab-line-tab-name-ellipsis)
                  'help-echo tab-name))))


(defcustom tab-line-tabs-function #'tab-line-tabs-fixed-window-buffers
  "Function to get a list of tabs to display in the tab line.
This function should return either a list of buffers whose names will
be displayed, or just a list of strings to display in the tab line.
By default, use function `tab-line-tabs-fixed-window-buffers' that
returns a list of buffers associated with the selected window where
buffers always keep the original order after switching buffers.
When `tab-line-tabs-mode-buffers', return a list of buffers
with the same major mode as the current buffer.
When `tab-line-tabs-buffer-groups', return a list of buffers
grouped by `tab-line-tabs-buffer-group-function'.
The result of this function is cached using
`tab-line-cache-key-function'."
  :type '(choice (const :tag "Window buffers"
                        tab-line-tabs-window-buffers)
                 (const :tag "Window buffers with fixed order"
                        tab-line-tabs-fixed-window-buffers)
                 (const :tag "Same mode buffers"
                        tab-line-tabs-mode-buffers)
                 (const :tag "Grouped buffers"
                        tab-line-tabs-buffer-groups)
                 (function :tag "Function"))
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (tab-line-force-update t))
  :group 'tab-line
  :version "27.1")

(defvar tab-line-tabs-buffer-list-function #'tab-line-tabs-buffer-list
  "Function to return a global list of buffers.
Used only for `tab-line-tabs-mode-buffers' and `tab-line-tabs-buffer-groups'.")

(defun tab-line-tabs-buffer-list ()
  (seq-filter (lambda (b) (and (buffer-live-p b)
                               (/= (aref (buffer-name b) 0) ?\s)))
              (seq-uniq (append (list (current-buffer))
                                (mapcar #'car (window-prev-buffers))
                                (buffer-list)))))

(defun tab-line-tabs-mode-buffers ()
  "Return a list of buffers with the same major mode as the current buffer."
  (let ((mode major-mode))
    (seq-sort-by #'buffer-name #'string<
                 (seq-filter (lambda (b) (with-current-buffer b
                                           (derived-mode-p mode)))
                             (funcall tab-line-tabs-buffer-list-function)))))

(defun tab-line-tab-modified-p (tab buffer-p)
  "Return t if TAB's buffer is modified.
BUFFER-P specifies whether the tab is a buffer; if nil, the buffer
is extracted from the association list TAB using the key `buffer'."
  (let ((buffer (if buffer-p tab (cdr (assq 'buffer tab)))))
    (when (and buffer (buffer-file-name buffer) (buffer-modified-p buffer))
      t)))

(defcustom tab-line-tabs-buffer-group-function
  #'tab-line-tabs-buffer-group-by-mode
  "Function to add a buffer to the appropriate group of tabs.
Takes a buffer as argument and should return a group name as a string.
If the return value is nil, the buffer has no group, so \"No group\"
is displayed instead of a group name and the buffer is not grouped
together with other buffers.
If the value is `tab-line-tabs-buffer-group-by-mode',
use mode-to-group mappings in `tab-line-tabs-buffer-groups'
to group by major mode.  If the value is
`tab-line-tabs-buffer-group-by-project' use the project name
as a group name."
  :type '(choice (const :tag "Group by mode"
                        tab-line-tabs-buffer-group-by-mode)
                 (const :tag "Group by project name"
                        tab-line-tabs-buffer-group-by-project)
                 (function :tag "Custom function"))
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (tab-line-force-update t))
  :group 'tab-line
  :version "30.1")

(defcustom tab-line-tabs-buffer-group-sort-function
  #'tab-line-tabs-buffer-group-sort-by-name
  "Function to sort buffers in a group."
  :type '(choice (const :tag "Don't sort" nil)
                 (const :tag "Sort by name alphabetically"
                        tab-line-tabs-buffer-group-sort-by-name)
                 (function :tag "Custom function"))
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (tab-line-force-update t))
  :group 'tab-line
  :version "30.1")

(defun tab-line-tabs-buffer-group-sort-by-name (a b)
  (string< (buffer-name a) (buffer-name b)))

(defcustom tab-line-tabs-buffer-groups-sort-function #'string<
  "Function to sort group names."
  :type '(choice (const :tag "Don't sort" nil)
                 (const :tag "Sort alphabetically" string<)
                 (function :tag "Custom function"))
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (tab-line-force-update t))
  :group 'tab-line
  :version "30.1")

(defvar tab-line-tabs-buffer-groups mouse-buffer-menu-mode-groups
  "How to group various major modes together in the tab line.
Each element has the form (REGEXP . GROUPNAME).
If the major mode's name matches REGEXP, it belongs to GROUPNAME.
The default is for each major mode to have a separate group
named the same as the mode.")

(defun tab-line-tabs-buffer-group-by-mode (&optional buffer)
  "Group tab buffers by major mode."
  (let ((mode (if buffer (with-current-buffer buffer
                           (format-mode-line mode-name))
                (format-mode-line mode-name))))
    (or (cdr (seq-find (lambda (group)
                         (string-match-p (car group) mode))
                       tab-line-tabs-buffer-groups))
        mode)))

(declare-function project-name "project" (project))
(defun tab-line-tabs-buffer-group-by-project (&optional buffer)
  "Group tab buffers by project name."
  (with-current-buffer buffer
    (if-let* ((project (project-current)))
        (project-name project)
      "No project")))

(defun tab-line-tabs-buffer-group-name (&optional buffer)
  (if (functionp tab-line-tabs-buffer-group-function)
      (funcall tab-line-tabs-buffer-group-function buffer)
    (tab-line-tabs-buffer-group-by-mode buffer)))

(defun tab-line-tabs-buffer-groups ()
  "Return a list of tabs that should be displayed in the tab line.
By default return a list of buffers grouped by major mode,
according to `tab-line-tabs-buffer-groups'.
If non-nil, `tab-line-tabs-buffer-group-function' is used to
generate the group name."
  (if (window-parameter nil 'tab-line-groups)
      (let* ((buffers (funcall tab-line-tabs-buffer-list-function))
             (groups (delq nil
                           (mapcar #'car
                                   (seq-group-by #'tab-line-tabs-buffer-group-name
                                                 buffers))))
             (sorted-groups (if (functionp tab-line-tabs-buffer-groups-sort-function)
                                (seq-sort tab-line-tabs-buffer-groups-sort-function
                                          groups)
                              groups))
             (selected-group (window-parameter nil 'tab-line-group))
             (tabs
              (mapcar (lambda (group)
                        `(tab
                          (name . ,group)
                          (selected . ,(equal group selected-group))
                          (select . ,(lambda ()
                                       (set-window-parameter nil 'tab-line-groups nil)
                                       (set-window-parameter nil 'tab-line-group group)
                                       (set-window-parameter nil 'tab-line-hscroll nil)))))
                      sorted-groups)))
        tabs)
    (let* ((window-parameter (window-parameter nil 'tab-line-group))
           (group-name (tab-line-tabs-buffer-group-name (current-buffer)))
           (group (prog1 (or window-parameter group-name "No group")
                    (when (equal window-parameter group-name)
                      (set-window-parameter nil 'tab-line-group nil))))
           (group-tab `(tab
                        (name . ,group)
                        (group-tab . t)
                        (select . ,(lambda ()
                                     (set-window-parameter nil 'tab-line-groups t)
                                     (set-window-parameter nil 'tab-line-group group)
                                     (set-window-parameter nil 'tab-line-hscroll nil)))))
           (buffers (seq-filter (lambda (b)
                                  (equal (tab-line-tabs-buffer-group-name b) group))
                                (funcall tab-line-tabs-buffer-list-function)))
           (sorted-buffers (if (functionp tab-line-tabs-buffer-group-sort-function)
                               (seq-sort tab-line-tabs-buffer-group-sort-function
                                         buffers)
                             buffers))
           (tabs (mapcar (lambda (buffer)
                           `(tab
                             (name . ,(funcall tab-line-tab-name-function buffer))
                             (selected . ,(eq buffer (current-buffer)))
                             (buffer . ,buffer)
                             (close . ,(lambda (&optional b)
                                         ;; kill-buffer because bury-buffer
                                         ;; won't remove the buffer from tab-line
                                         (kill-buffer (or b buffer))))))
                         sorted-buffers)))
      (cons group-tab tabs))))

(defcustom tab-line-tabs-window-buffers-filter-function
  #'identity
  "Filter which buffers should be displayed in the tab line."
  :type '(choice function
                 (const :tag "Show all buffers" identity)
                 (const :tag "Omit excluded buffers" tab-line-tabs-non-excluded))
  :group 'tab-line
  :version "31.1")

(defvar tab-line-exclude-buffers)
(defvar tab-line-exclude-modes)

(defun tab-line-tabs-non-excluded (buffers)
  "Filter BUFFERS to remove excluded buffers from the list.
Intended to be used in `tab-line-tabs-window-buffers-filter-function'."
  (seq-remove
   (lambda (b)
     (or (memq (buffer-local-value 'major-mode b)
               tab-line-exclude-modes)
         (buffer-match-p tab-line-exclude-buffers b)
         (get (buffer-local-value 'major-mode b) 'tab-line-exclude)
         (buffer-local-value 'tab-line-exclude b)))
   buffers))

(defun tab-line-tabs-window-buffers ()
  "Return a list of tabs that should be displayed in the tab line.
By default returns a list of window buffers, i.e. buffers previously
shown in the same window where the tab line is displayed.
This list can be overridden by changing the default value of the
variable `tab-line-tabs-function'."
  (let* ((window (selected-window))
         (buffer (window-buffer window))
         (next-buffers (seq-remove (lambda (b) (eq b buffer))
                                   (window-next-buffers window)))
         (next-buffers (seq-filter #'buffer-live-p next-buffers))
         (prev-buffers (seq-remove (lambda (b) (eq b buffer))
                                   (mapcar #'car (window-prev-buffers window))))
         (prev-buffers (seq-filter #'buffer-live-p prev-buffers))
         ;; Remove next-buffers from prev-buffers
         (prev-buffers (seq-difference prev-buffers next-buffers)))
    (funcall
     tab-line-tabs-window-buffers-filter-function
     (append (reverse prev-buffers)
             (list buffer)
             next-buffers))))

(defun tab-line-tabs-fixed-window-buffers ()
  "Like `tab-line-tabs-window-buffers' but keep stable sorting order.
This means that switching to a buffer previously shown in the same
window will keep the same order of tabs that was before switching.
And newly displayed buffers are added to the end of the tab line."
  (let* ((old-buffers (window-parameter nil 'tab-line-buffers))
         (buffer-positions (let ((index-table (make-hash-table
                                               :size (length old-buffers)
                                               :test #'eq)))
                             (seq-do-indexed
                              (lambda (buf idx) (puthash buf idx index-table))
                              old-buffers)
                             index-table))
         (new-buffers (sort (tab-line-tabs-window-buffers)
                            :in-place t
                            :key (lambda (buffer)
                                   (gethash buffer buffer-positions
                                            most-positive-fixnum)))))
    (set-window-parameter nil 'tab-line-buffers new-buffers)
    new-buffers))

(add-to-list 'window-persistent-parameters '(tab-line-buffers . t))


(defcustom tab-line-tab-name-format-function #'tab-line-tab-name-format-default
  "Function to format a tab name.
The function will be called two arguments: the tab whose name to format,
and the list of all the tabs; it should return the formatted tab name
to display in the tab line.
The first argument could also be a different object, for example the buffer
which the tab will represent.  The result of this function is cached
using `tab-line-cache-key-function'."
  :type 'function
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (tab-line-force-update t))
  :group 'tab-line
  :version "28.1")

(defun tab-line-tab-name-format-default (tab tabs)
  "Default function to use as `tab-line-tab-name-format-function', which see."
  (let* ((buffer-p (bufferp tab))
         (selected-p (if buffer-p
                         (eq tab (window-buffer))
                       (cdr (assq 'selected tab))))
         (name (if buffer-p
                   (funcall tab-line-tab-name-function tab tabs)
                 (cdr (assq 'name tab))))
         (face (if selected-p
                   (if (mode-line-window-selected-p)
                       'tab-line-tab-current
                     'tab-line-tab)
                 'tab-line-tab-inactive)))
    (dolist (fn tab-line-tab-face-functions)
      (setf face (funcall fn tab tabs face buffer-p selected-p)))
    (apply 'propertize
           (concat (propertize (string-replace "%" "%%" name) ;; (bug#57848)
                               'face face
                               'keymap tab-line-tab-map
                               'help-echo (if selected-p "Current tab"
                                            "Click to select tab")
                               ;; Don't turn mouse-1 into mouse-2 (bug#49247)
                               'follow-link 'ignore)
                   (let ((close (or (and (or buffer-p (assq 'buffer tab)
                                             (assq 'close tab))
                                         tab-line-close-button-show
                                         (not (eq tab-line-close-button-show
                                                  (if selected-p 'non-selected
                                                    'selected)))
                                         (if (and tab-line-close-modified-button-show
                                                  (tab-line-tab-modified-p tab buffer-p))
                                             tab-line-close-modified-button
                                           tab-line-close-button))
                                    "")))
                     (setq close (copy-sequence close))
                     ;; Don't overwrite the icon face
                     (add-face-text-property 0 (length close) face t close)
                     close))
           `(
             tab ,tab
             ,@(if selected-p '(selected t))
             mouse-face tab-line-highlight))))

(defun tab-line-format-template (tabs)
  "Template of the format for displaying tab line for selected window.
This is used by `tab-line-format'."
  (let* ((separator (or tab-line-separator (if (window-system) " " "|")))
         (hscroll (window-parameter nil 'tab-line-hscroll))
         (strings
          (mapcar
           (lambda (tab)
             (concat separator
                     (funcall tab-line-tab-name-format-function tab tabs)))
           tabs))
         (hscroll-data (tab-line-auto-hscroll strings hscroll)))
    (setq hscroll (nth 1 hscroll-data))
    (append
     (if (null (nth 0 hscroll-data))
         (when hscroll
           (setq hscroll nil)
           (set-window-parameter nil 'tab-line-hscroll hscroll))
       (list separator
             (when (and (numberp hscroll) (not (zerop hscroll)))
               tab-line-left-button)
             (when (if (numberp hscroll)
                       (< (truncate hscroll) (1- (length strings)))
                     (> (length strings) 1))
               tab-line-right-button)))
     (if hscroll (nthcdr (truncate hscroll) strings) strings)
     (list separator)
     (when (and (memq tab-line-tabs-function tab-line-new-button-functions)
                tab-line-new-button-show
                tab-line-new-button)
       (list tab-line-new-button)))))

(defun tab-line-tab-face-inactive-alternating (tab tabs face _buffer-p selected-p)
  "Return FACE for TAB in TABS with alternation.
SELECTED-P nil means TAB is not the selected tab.
When TAB is not selected and is even-numbered, make FACE
inherit from `tab-line-tab-inactive-alternate'.  For use in
`tab-line-tab-face-functions'."
  (when (and (not selected-p) (evenp (cl-position tab tabs)))
    (setf face `(:inherit (tab-line-tab-inactive-alternate ,face))))
  face)

(defun tab-line-tab-face-special (tab _tabs face buffer-p _selected-p)
  "Return FACE for TAB according to whether its buffer is special.
TAB is either a buffer (if BUFFER-P is non-nil), or an association
list with the buffer given by the key `buffer'.
When TAB specifies a non-file-visiting buffer, make FACE inherit
from `tab-line-tab-special'.
For use in `tab-line-tab-face-functions'."
  (let ((buffer (if buffer-p tab (cdr (assq 'buffer tab)))))
    (when (and buffer (not (buffer-file-name buffer)))
      (setf face `(:inherit (tab-line-tab-special ,face)))))
  face)

(defun tab-line-tab-face-modified (tab _tabs face buffer-p _selected-p)
  "Return FACE for TAB according to whether its buffer is modified.
TAB is either a buffer (if BUFFER-P is non-nil), or an association
list with the buffer given by the key `buffer'.
When TAB's buffer is a modified, file-backed buffer, make FACE inherit
from `tab-line-tab-modified'.
For use in `tab-line-tab-face-functions'."
  (when (tab-line-tab-modified-p tab buffer-p)
    (setf face `(:inherit (tab-line-tab-modified ,face))))
  face)

(defun tab-line-tab-face-group (tab _tabs face _buffer-p _selected-p)
  "Return FACE for TAB according to whether it's a group tab.
TAB is either a buffer (if BUFFER-P is non-nil), or an association
list with the buffer given by the key `buffer'.
For use in `tab-line-tab-face-functions'."
  (when (alist-get 'group-tab tab)
    (setf face `(:inherit (tab-line-tab-group ,face))))
  face)

(defvar tab-line-auto-hscroll)

(defun tab-line-force-update (all)
  "Force redisplay of the current buffer’s tab line.
This function also clears the tab-line cache.  With optional non-nil ALL,
it clears the tab-line cache of all tab lines and forces their redisplay."
  (if all
      (walk-windows
       (lambda (window)
         (set-window-parameter window 'tab-line-cache nil))
       'no-mini t)
    (set-window-parameter nil 'tab-line-cache nil))
  (force-mode-line-update all))

(defun tab-line-cache-key-default (tabs)
  "Return default list of cache keys."
  (list
   tabs
   ;; handle buffer renames
   (buffer-name (window-buffer))
   ;; handle tab-line scrolling
   (window-parameter nil 'tab-line-hscroll)
   ;; for setting face 'tab-line-tab-current'
   (mode-line-window-selected-p)
   ;; for `tab-line-tab-face-modified'
   (and (or tab-line-close-modified-button-show
            (memq 'tab-line-tab-face-modified
                  tab-line-tab-face-functions))
        (buffer-file-name)
        (buffer-modified-p))))

(defvar tab-line-cache-key-function #'tab-line-cache-key-default
  "Function that adds more cache keys.
It is called with one argument, a list of tabs, and should return a list
of cache keys.  You can use `add-function' to add more cache keys.
Also there is the function `tab-line-force-update' that clears the cache.")

(defun tab-line-format ()
  "Format for displaying the tab line of the selected window."
  (let* ((tabs (funcall tab-line-tabs-function))
         (cache-key (funcall tab-line-cache-key-function tabs))
         (cache (window-parameter nil 'tab-line-cache)))
    ;; Enable auto-hscroll again after it was disabled on manual scrolling.
    ;; The moment to enable it is when the window-buffer was updated.
    (when (and tab-line-auto-hscroll        ; if auto-hscroll was enabled
               (integerp (nth 2 cache-key)) ; integer on manual scroll
               cache                        ; window-buffer was updated
               (not (equal (nth 1 (car cache)) (nth 1 cache-key))))
      (set-window-parameter nil 'tab-line-hscroll (float (nth 2 cache-key))))
    (or (and cache (equal (car cache) cache-key) (cdr cache))
        (cdr (set-window-parameter
              nil 'tab-line-cache
              (cons cache-key (tab-line-format-template tabs)))))))


(defcustom tab-line-auto-hscroll t
  "Allow or disallow automatic horizontal scrolling of the tab line.
Non-nil means the tab lines are automatically scrolled horizontally to make
the selected tab visible."
  :type 'boolean
  :group 'tab-line
  :version "27.1")

(defvar tab-line-auto-hscroll-buffer (generate-new-buffer " *tab-line-hscroll*"))

(defun tab-line--get-tab-property (prop string)
  (or (get-pos-property 1 prop string) ;; for most cases of 1-char separator
      (get-pos-property 0 prop string) ;; for empty separator
      (let ((pos (next-single-property-change 0 prop string))) ;; long separator
        (and pos (get-pos-property pos prop string)))))

(defun tab-line-auto-hscroll (strings hscroll)
  (with-current-buffer tab-line-auto-hscroll-buffer
    (let ((truncate-partial-width-windows nil)
          (inhibit-modification-hooks t)
          show-arrows)
      (setq truncate-lines nil
            word-wrap nil)
      (erase-buffer)
      (apply 'insert strings)
      (goto-char (point-min))
      (add-face-text-property (point-min) (point-max) 'tab-line t)
      ;; Continuation means tab-line doesn't fit completely,
      ;; thus scroll arrows are needed for scrolling.
      (setq show-arrows (> (vertical-motion 1) 0))
      ;; Try to auto-hscroll only when scrolling is needed,
      ;; but no manual scrolling was performed before.
      (when (and tab-line-auto-hscroll
                 show-arrows
                 ;; Do nothing when scrolled manually
                 (not (integerp hscroll)))
        (let ((selected (seq-position strings 'selected
                                      (lambda (str prop)
                                        (tab-line--get-tab-property prop str)))))
          (cond
           ((null selected)
            ;; Do nothing if no tab is selected
            )
           ((or (not (numberp hscroll)) (< selected (truncate hscroll)))
            ;; Selected is scrolled to the left, or no scrolling yet
            (erase-buffer)
            (apply 'insert (reverse (seq-subseq strings 0 (1+ selected))))
            (goto-char (point-min))
            (add-face-text-property (point-min) (point-max) 'tab-line)
            (if (> (vertical-motion 1) 0)
                (let* ((point (previous-single-property-change (point) 'tab))
                       (tab-prop (when point
                                   (or (get-pos-property point 'tab)
                                       (and (setq point (previous-single-property-change point 'tab))
                                            (get-pos-property point 'tab)))))
                       (new-hscroll (when tab-prop
                                      (seq-position strings tab-prop
                                                    (lambda (str tab)
                                                      (eq (tab-line--get-tab-property 'tab str) tab))))))
                  (when new-hscroll
                    (setq hscroll (float new-hscroll))
                    (set-window-parameter nil 'tab-line-hscroll hscroll)))
              (setq hscroll nil)
              (set-window-parameter nil 'tab-line-hscroll hscroll)))
           (t
            ;; Check if the selected tab is already visible
            (erase-buffer)
            (apply 'insert (seq-subseq strings (truncate hscroll) (1+ selected)))
            (goto-char (point-min))
            (add-face-text-property (point-min) (point-max) 'tab-line)
            (when (> (vertical-motion 1) 0)
              ;; Not visible already
              (erase-buffer)
              (apply 'insert (reverse (seq-subseq strings 0 (1+ selected))))
              (goto-char (point-min))
              (add-face-text-property (point-min) (point-max) 'tab-line)
              (when (> (vertical-motion 1) 0)
                (let* ((point (previous-single-property-change (point) 'tab))
                       (tab-prop (when point
                                   (or (get-pos-property point 'tab)
                                       (and (setq point (previous-single-property-change point 'tab))
                                            (get-pos-property point 'tab)))))
                       (new-hscroll (when tab-prop
                                      (seq-position strings tab-prop
                                                    (lambda (str tab)
                                                      (eq (tab-line--get-tab-property 'tab str) tab))))))
                  (when new-hscroll
                    (setq hscroll (float new-hscroll))
                    (set-window-parameter nil 'tab-line-hscroll hscroll)))))))))
      (list show-arrows hscroll))))


(defun tab-line-hscroll (&optional arg window)
  (let* ((hscroll (window-parameter window 'tab-line-hscroll))
         (tabs (if window
                   (with-selected-window window (funcall tab-line-tabs-function))
                 (funcall tab-line-tabs-function))))
    (set-window-parameter
     window 'tab-line-hscroll
     (max 0 (min (+ (if (numberp hscroll) (truncate hscroll) 0) (or arg 1))
                 (1- (length tabs)))))
    (when window
      (force-mode-line-update t))))

(defun tab-line-hscroll-right (&optional arg event)
  "Scroll the tab line ARG positions to the right.
Interactively, ARG is the prefix numeric argument and defaults to 1."
  (interactive (list current-prefix-arg last-nonmenu-event))
  (when (tab-line-track-tap event)
    (let ((window (posn-window (tab-line-event-start event))))
      (tab-line-hscroll arg window)
      (force-mode-line-update window))))

(defun tab-line-hscroll-left (&optional arg event)
  "Scroll the tab line ARG positions to the left.
Interactively, ARG is the prefix numeric argument and defaults to 1."
  (interactive (list current-prefix-arg last-nonmenu-event))
  (when (tab-line-track-tap event)
    (let ((window (posn-window (tab-line-event-start event))))
      (tab-line-hscroll (- (or arg 1)) window)
      (force-mode-line-update window))))


(defun tab-line-new-tab (&optional event)
  "Add a new tab to the selected-window's tab line.
This command is usually invoked by clicking on the plus-shaped button
on the tab line.  Switching to another buffer also adds a new tab
corresponding to the new buffer shown in the window."
  (interactive (list last-nonmenu-event))
  (when (tab-line-track-tap event)
    (if (functionp tab-line-new-tab-choice)
        (funcall tab-line-new-tab-choice)
      (let ((tab-line-tabs-buffer-groups mouse-buffer-menu-mode-groups))
        (if (and (consp event)
                 (display-popup-menus-p)
                 (not tty-menu-open-use-tmm))
            (mouse-buffer-menu event) ; like (buffer-menu-open)
          ;; tty menu doesn't support mouse clicks, so use tmm
          (tmm-prompt (mouse-buffer-menu-keymap)))))))

(defun tab-line-select-tab (&optional event)
  "Switch to the buffer specified by the tab on which you click.
This command maintains the original order of prev/next buffers.
So, for example, switching to a previous tab is equivalent to
using the `previous-buffer' command."
  (interactive "e")
  (when (tab-line-track-tap event #'tab-line-tab-context-menu)
    (let* ((posnp (tab-line-event-start event))
           (tab (tab-line--get-tab-property 'tab (car (posn-string posnp))))
           (buffer (if (bufferp tab) tab (cdr (assq 'buffer tab)))))
      (if buffer
          (tab-line-select-tab-buffer buffer (posn-window posnp))
        (let ((select (cdr (assq 'select tab))))
          (when (functionp select)
            (with-selected-window (posn-window posnp)
              (funcall select)
              (force-mode-line-update))))))))

(defun tab-line-select-tab-buffer (buffer &optional window)
  (if (eq tab-line-tabs-function #'tab-line-tabs-window-buffers)
      (let* ((window-buffer (window-buffer window))
             (next-buffers (seq-remove (lambda (b) (eq b window-buffer))
                                       (window-next-buffers window)))
             (prev-buffers (seq-remove (lambda (b) (eq b window-buffer))
                                       (mapcar #'car (window-prev-buffers window))))
             ;; Remove next-buffers from prev-buffers
             (prev-buffers (seq-difference prev-buffers next-buffers)))
        (cond
         ((memq buffer next-buffers)
          (dotimes (_ (1+ (seq-position next-buffers buffer)))
            (switch-to-next-buffer window)))
         ((memq buffer prev-buffers)
          (dotimes (_ (1+ (seq-position prev-buffers buffer)))
            (switch-to-prev-buffer window)))))
    (with-selected-window window
      (let ((switch-to-buffer-obey-display-actions nil))
        (switch-to-buffer buffer)))))

(defcustom tab-line-switch-cycling t
  "Wrap tabs on tab switch while cycling.
If non-nil, `tab-line-switch-to-prev-tab' in the first tab
switches to the last tab and `tab-line-switch-to-next-tab' in the
last tab switches to the first tab.  This variable is not consulted
when `tab-line-tabs-function' is `tab-line-tabs-window-buffers'."
  :type 'boolean
  :group 'tab-line
  :version "28.1")

(defun tab-line-switch-to-prev-tab (&optional event arg)
  "Switch to the ARGth previous tab's buffer.
When `tab-line-tabs-function' is `tab-line-tabs-window-buffers',
its effect is the same as using the `previous-buffer' command
\(\\[previous-buffer]).
For other values of `tab-line-tabs-function' this command
switches to the previous buffer in the sequence defined by
`tab-line-tabs-function'.  To wrap buffer cycling in this case
is possible when `tab-line-switch-cycling' is non-nil."
  (interactive (list last-nonmenu-event
                     (prefix-numeric-value current-prefix-arg)))
  (with-selected-window (posn-window (tab-line-event-start event))
    (if (eq tab-line-tabs-function #'tab-line-tabs-window-buffers)
        (previous-buffer arg t)
      (let* ((buffers (seq-keep
                       (lambda (tab) (or (and (bufferp tab) tab)
                                         (alist-get 'buffer tab)))
                       (funcall tab-line-tabs-function)))
             (old-pos (seq-position buffers (current-buffer)))
             (new-pos (when old-pos (- old-pos (or arg 1))))
             (new-pos (when new-pos
                        (if tab-line-switch-cycling
                            (mod new-pos (length buffers))
                          (max new-pos 0))))
             (buffer (when new-pos (nth new-pos buffers))))
        (when (bufferp buffer)
          (let ((switch-to-buffer-obey-display-actions nil))
            (switch-to-buffer buffer)))))))

(defun tab-line-switch-to-next-tab (&optional event arg)
  "Switch to the next ARGth tab's buffer.
When `tab-line-tabs-function' is `tab-line-tabs-window-buffers',
its effect is the same as using the `next-buffer' command
\(\\[next-buffer]).
For other values of `tab-line-tabs-function' this command
switches to the next buffer in the sequence defined by
`tab-line-tabs-function'.  To wrap buffer cycling in this case
is possible when `tab-line-switch-cycling' is non-nil."
  (interactive (list last-nonmenu-event
                     (prefix-numeric-value current-prefix-arg)))
  (with-selected-window (posn-window (tab-line-event-start event))
    (if (eq tab-line-tabs-function #'tab-line-tabs-window-buffers)
        (next-buffer arg t)
      (let* ((buffers (seq-keep
                       (lambda (tab) (or (and (bufferp tab) tab)
                                         (alist-get 'buffer tab)))
                       (funcall tab-line-tabs-function)))
             (old-pos (seq-position buffers (current-buffer)))
             (new-pos (when old-pos (+ old-pos (or arg 1))))
             (new-pos (when new-pos
                        (if tab-line-switch-cycling
                            (mod new-pos (length buffers))
                          (min new-pos (1- (length buffers))))))
             (buffer (when new-pos (nth new-pos buffers))))
        (when (bufferp buffer)
          (let ((switch-to-buffer-obey-display-actions nil))
            (switch-to-buffer buffer)))))))

(defun tab-line-mouse-move-tab (event)
  "Move a tab to a different position on the tab line using mouse.
This command should be bound to a drag event.  It moves the tab
at the mouse-down event to the position at mouse-up event.
It can be used only when `tab-line-tabs-function' is
customized to `tab-line-tabs-fixed-window-buffers'."
  (interactive "e")
  (when (eq tab-line-tabs-function #'tab-line-tabs-fixed-window-buffers)
    (let* ((posnp1 (tab-line-event-start event))
           (posnp2 (event-end event))
           (string1 (car (posn-string posnp1)))
           (string2 (car (posn-string posnp2)))
           (buffer1 (when string1 (tab-line--get-tab-property 'tab string1)))
           (buffer2 (when string2 (tab-line--get-tab-property 'tab string2)))
           (window1 (posn-window posnp1))
           (window2 (posn-window posnp2))
           (buffers (window-parameter window1 'tab-line-buffers))
           (pos2 (when buffer2 (seq-position buffers buffer2))))
      (when (and (eq window1 window2) buffer1 pos2)
        (setq buffers (delq buffer1 buffers))
        (cl-pushnew buffer1 (nthcdr pos2 buffers))
        (set-window-parameter window1 'tab-line-buffers buffers)
        (set-window-parameter window1 'tab-line-cache nil)
        (with-selected-window window1 (force-mode-line-update))))))

(defun tab-line-move-tab-forward (&optional arg)
  "Move a tab to a different position on the tab line.
ARG specifies the number of positions to move:
- When positive, move the current tab ARG positions to the right.
- When negative, move the current tab -ARG positions to the left.
- When nil, act as if ARG is 1, moving one position to the right.
It can be used only when `tab-line-tabs-function' is
customized to `tab-line-tabs-fixed-window-buffers'."
  (interactive "p")
  (when (eq tab-line-tabs-function #'tab-line-tabs-fixed-window-buffers)
    (let* ((window (selected-window))
           (buffers (window-parameter window 'tab-line-buffers))
           (buffer (current-buffer))
           (pos (seq-position buffers buffer))
           (len (length buffers))
           (new-pos (+ pos (or arg 1))))
      (when (and pos (> len 1))
        (setq new-pos (if tab-line-switch-cycling
                          (mod new-pos len)
                        (max 0 (min new-pos (1- len)))))
        (setq buffers (delq buffer buffers))
        (setq buffers (append
                       (seq-take buffers new-pos)
                       (list buffer)
                       (seq-drop buffers new-pos)))
        (set-window-parameter window 'tab-line-buffers buffers)
        (set-window-parameter window 'tab-line-cache nil)
        (force-mode-line-update)))))

(defun tab-line-move-tab-backward (&optional arg)
  "Move a tab to a different position on the tab line.
ARG specifies the number of positions to move:
- When positive, move the current tab ARG positions to the left.
- When negative, move the current tab -ARG positions to the right.
- When nil, act as if ARG is 1, moving one position to the left.
It can be used only when `tab-line-tabs-function' is
customized to `tab-line-tabs-fixed-window-buffers'."
  (interactive "p")
  (tab-line-move-tab-forward (- (or arg 1))))


(defcustom tab-line-close-tab-function 'bury-buffer
  "What to do upon closing a tab on the tab line.
If `bury-buffer', put the tab's buffer at the end of the list of all
buffers, which effectively hides the buffer's tab from the tab line.
If `kill-buffer', kills the tab's buffer.
When a function, it is called with the tab as its argument.
This option is useful when `tab-line-tabs-function' has the value
`tab-line-tabs-window-buffers' or `tab-line-tabs-fixed-window-buffers'."
  :type '(choice (const :tag "Bury buffer" bury-buffer)
                 (const :tag "Kill buffer" kill-buffer)
                 (function :tag "Function"))
  :group 'tab-line
  :version "27.1")

(defun tab-line--current-tab ()
  "Return the current tab in the tab line."
  (seq-find (lambda (tab)
              (eq (if (bufferp tab) tab (alist-get 'buffer tab))
                  (current-buffer)))
            (funcall tab-line-tabs-function)))

(defun tab-line-close-tab (&optional event)
  "Close the selected tab.
This command is usually invoked by clicking on the close button on the
right side of the tab.  This command buries the buffer, so it goes out of
sight of the tab line."
  (interactive (list last-nonmenu-event))
  (when (tab-line-track-tap event)
    (let* ((posnp (tab-line-event-start event))
           (tab (if (consp event)
                    (tab-line--get-tab-property 'tab (car (posn-string posnp)))
                  (tab-line--current-tab)))
           (buffer (if (bufferp tab) tab (cdr (assq 'buffer tab))))
           (close-function (unless (bufferp tab) (cdr (assq 'close tab)))))
      (with-selected-window (posn-window posnp)
        (cond
         ((functionp close-function)
          (funcall close-function))
         ((eq tab-line-close-tab-function 'kill-buffer)
          (kill-buffer buffer))
         ((eq tab-line-close-tab-function 'bury-buffer)
          (if (eq buffer (current-buffer))
              (bury-buffer)
            (set-window-prev-buffers nil (assq-delete-all buffer (window-prev-buffers)))
            (set-window-next-buffers nil (delq buffer (window-next-buffers)))))
         ((functionp tab-line-close-tab-function)
          (funcall tab-line-close-tab-function tab)))
        (force-mode-line-update)))))

(defun tab-line-close-other-tabs (&optional event)
  "Close all tabs on the selected window, except the tab on EVENT.
It preforms the same actions on the closed tabs as in `tab-line-close-tab'."
  (interactive (list last-nonmenu-event))
  (when (tab-line-track-tap event)
    (let* ((posnp (tab-line-event-start event))
           (keep-tab (if (consp event)
                         (tab-line--get-tab-property 'tab (car (posn-string posnp)))
                       (tab-line--current-tab))))
      (with-selected-window (posn-window posnp)
        (dolist (tab (delete keep-tab (funcall tab-line-tabs-function)))
          (let ((buffer (if (bufferp tab) tab (cdr (assq 'buffer tab))))
                (close-function (unless (bufferp tab) (cdr (assq 'close tab)))))
            (cond
             ((functionp close-function)
              (funcall close-function))
             ((eq tab-line-close-tab-function 'kill-buffer)
              (kill-buffer buffer))
             ((eq tab-line-close-tab-function 'bury-buffer)
              (if (eq buffer (current-buffer))
                  (bury-buffer)
                (set-window-prev-buffers nil (assq-delete-all buffer (window-prev-buffers)))
                (set-window-next-buffers nil (delq buffer (window-next-buffers)))))
             ((functionp tab-line-close-tab-function)
              (funcall tab-line-close-tab-function tab)))))
        (force-mode-line-update)))))

(defun tab-line-tab-context-menu (&optional event)
  "Pop up the context menu for a tab-line tab."
  (interactive "e")
  (let ((menu (make-sparse-keymap (propertize "Context Menu" 'hide t))))
    (define-key-after menu [close]
      '(menu-item "Close" tab-line-close-tab :help "Close the tab"))
    (define-key-after menu [close-other]
      '(menu-item "Close other tabs" tab-line-close-other-tabs
                  :help "Close all other tabs"))
    (popup-menu menu event)))

(defun tab-line-context-menu (&optional event)
  "Pop up the context menu for the tab line."
  (interactive "e")
  (let ((menu (make-sparse-keymap (propertize "Context Menu" 'hide t))))
    (define-key-after menu [close]
      '(menu-item "New tab" tab-line-new-tab :help "Create a new tab"))
    (popup-menu menu event)))


;;; Touch screen support.

(defvar touch-screen-delay)

(defun tab-line-track-tap (event &optional function)
  "Track a tap starting from EVENT.
If EVENT is not a `touchscreen-begin' event, return t.
Otherwise, return t if the tap completes successfully, and nil if
the tap should be ignored.

If FUNCTION is specified and the tap does not complete within
`touch-screen-delay' seconds, display the appropriate context
menu by calling FUNCTION with EVENT, and return nil."
  (if (not (eq (car-safe event) 'touchscreen-begin))
      t
    (let ((result (catch 'context-menu
                    (let (timer)
                      (unwind-protect
                          (progn
                            (when function
                              (setq timer
                                    (run-at-time touch-screen-delay t
                                                 #'throw 'context-menu
                                                 'context-menu)))
                            (touch-screen-track-tap event))
                        (when timer
                          (cancel-timer timer)))))))
      (cond ((eq result 'context-menu)
             (prog1 nil
               (funcall function event)))
            (result t)))))

(defun tab-line-event-start (event)
  "Like `event-start'.
However, return the correct mouse position list if EVENT is a
`touchscreen-begin' event."
  (or (and (eq (car-safe event) 'touchscreen-begin)
           (cdadr event))
      (event-start event)))


(defcustom tab-line-define-keys t
  "Define specific tab-line key bindings.
If t, the default, key mappings for switching and moving tabs
are defined.  If nil, do not define any key mappings."
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (lambda (sym val)
         (tab-line--undefine-keys)
         (set-default sym val)
         ;; Enable the new keybindings
         (tab-line--define-keys))
  :group 'tab-line
  :version "31.1")

(defun tab-line--define-keys ()
  "Install key bindings to switch between tabs if so configured."
  (when tab-line-define-keys
    (when (eq (keymap-lookup ctl-x-map "<left>") 'previous-buffer)
      (keymap-set ctl-x-map "<left>" #'tab-line-switch-to-prev-tab))
    (when (eq (keymap-lookup ctl-x-map "C-<left>") 'previous-buffer)
      (keymap-set ctl-x-map "C-<left>" #'tab-line-switch-to-prev-tab))
    (unless (keymap-lookup ctl-x-map "M-<left>")
      (keymap-set ctl-x-map "M-<left>" #'tab-line-move-tab-backward))
    (when (eq (keymap-lookup ctl-x-map "<right>") 'next-buffer)
      (keymap-set ctl-x-map "<right>" #'tab-line-switch-to-next-tab))
    (when (eq (keymap-lookup ctl-x-map "C-<right>") 'next-buffer)
      (keymap-set ctl-x-map "C-<right>" #'tab-line-switch-to-next-tab))
    (unless (keymap-lookup ctl-x-map "M-<right>")
      (keymap-set ctl-x-map "M-<right>" #'tab-line-move-tab-forward))))

(defun tab-line--undefine-keys ()
  "Uninstall key bindings previously bound by `tab-line--define-keys'."
  (when tab-line-define-keys
    (when (eq (keymap-lookup ctl-x-map "<left>") 'tab-line-switch-to-prev-tab)
      (keymap-set ctl-x-map "<left>" #'previous-buffer))
    (when (eq (keymap-lookup ctl-x-map "C-<left>") 'tab-line-switch-to-prev-tab)
      (keymap-set ctl-x-map "C-<left>" #'previous-buffer))
    (when (eq (keymap-lookup ctl-x-map "M-<left>") 'tab-line-move-tab-backward)
      (keymap-set ctl-x-map "M-<left>" nil))
    (when (eq (keymap-lookup ctl-x-map "<right>") 'tab-line-switch-to-next-tab)
      (keymap-set ctl-x-map "<right>" #'next-buffer))
    (when (eq (keymap-lookup ctl-x-map "C-<right>") 'tab-line-switch-to-next-tab)
      (keymap-set ctl-x-map "C-<right>" #'next-buffer))
    (when (eq (keymap-lookup ctl-x-map "M-<right>") 'tab-line-move-tab-forward)
      (keymap-set ctl-x-map "M-<right>" nil))))

(defvar-keymap tab-line-mode-map
  :doc "Keymap for keys of `tab-line-mode'.")

(defvar-keymap tab-line-switch-repeat-map
  :doc "Keymap to repeat tab/buffer cycling.  Used in `repeat-mode'."
  :repeat t
  "<left>"    #'tab-line-switch-to-prev-tab
  "M-<left>"  #'tab-line-move-tab-backward
  "<right>"   #'tab-line-switch-to-next-tab
  "M-<right>" #'tab-line-move-tab-forward)

;;;###autoload
(define-minor-mode tab-line-mode
  "Toggle display of tab line in the windows displaying the current buffer.

When this mode is enabled, each window displays a tab line on its
top screen line.  The tab line is a row of tabs -- buttons which
you can click to have the window display the buffer whose name is
shown on the button.  Clicking on the \"x\" icon of the button
removes the button (but does not kill the corresponding buffer).
In addition, the tab line shows a \"+\" button which adds a new
button, so you could have one more buffer shown on the tab line."
  :lighter nil
  (let ((default-value '(:eval (tab-line-format))))
    ;; Preserve the existing tab-line set outside of this mode
    (if (or (null tab-line-format)
            (equal tab-line-format default-value))
        (if tab-line-mode
            (setq tab-line-format default-value)
          (setq tab-line-format nil))
      (message "tab-line-format set outside of tab-line-mode, currently `%S'"
               tab-line-format))))

(defcustom tab-line-exclude-modes
  '(completion-list-mode)
  "List of major modes for which the tab-line display is not enabled.
Buffers under any of these major modes will not show the tab line in
their windows, even if `global-tab-line-mode' is enabled.

See also `tab-line-exclude-buffers', for exclude buffers."
  :type '(repeat symbol)
  :group 'tab-line
  :version "27.1")

(defcustom tab-line-exclude-buffers nil
  "Whether tab-line should not be enabled in a buffer.

The value must be a condition which is passed to `buffer-match-p' (which
see).

You can include multiple conditions, for example:

  To exclude multiple modes and buffer names:
  \\='(or \"\\*eshell\\*\"
         (derived-mode completion-list-mode
                       eshell-mode
                       term-mode
                       ...)
         ...)

If the condition yields a non-nil value, tab line will not be enabled in
those buffers.

See also `tab-line-exclude-modes', for only exclude major modes."
  :type '(buffer-predicate :tag "Predicate for `buffer-match-p'")
  :safe #'booleanp
  :group 'tab-line
  :version "31.1")

;;;###autoload
(defvar-local tab-line-exclude nil)

(defun tab-line-mode--turn-on ()
  "Turn on `tab-line-mode' in all pertinent buffers.
Temporary buffers, buffers whose names begin with a space, buffers
under major modes that are either mentioned in `tab-line-exclude-mode'
or have a non-nil `tab-line-exclude' property on their symbol,
and buffers that have a non-nil buffer-local value
of `tab-line-exclude', are exempt from `tab-line-mode'."
  (unless (or (minibufferp)
              (string-match-p "\\` " (buffer-name))
              (memq major-mode tab-line-exclude-modes)
              (buffer-match-p tab-line-exclude-buffers (buffer-name))
              (get major-mode 'tab-line-exclude)
              (buffer-local-value 'tab-line-exclude (current-buffer)))
    (tab-line-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-tab-line-mode
  tab-line-mode tab-line-mode--turn-on
  :group 'tab-line
  :version "27.1"
  (if global-tab-line-mode
      (tab-line--define-keys)
    (tab-line--undefine-keys)))


(global-set-key [tab-line down-mouse-3] 'tab-line-context-menu)
(global-set-key [tab-line drag-mouse-1] 'tab-line-mouse-move-tab)

(global-set-key [tab-line mouse-4]    'tab-line-hscroll-left)
(global-set-key [tab-line mouse-5]    'tab-line-hscroll-right)
(global-set-key [tab-line wheel-up]   'tab-line-hscroll-left)
(global-set-key [tab-line wheel-down] 'tab-line-hscroll-right)
(global-set-key [tab-line wheel-left] 'tab-line-hscroll-left)
(global-set-key [tab-line wheel-right] 'tab-line-hscroll-right)

(global-set-key [tab-line S-mouse-4]    'tab-line-switch-to-prev-tab)
(global-set-key [tab-line S-mouse-5]    'tab-line-switch-to-next-tab)
(global-set-key [tab-line S-wheel-up]   'tab-line-switch-to-prev-tab)
(global-set-key [tab-line S-wheel-down] 'tab-line-switch-to-next-tab)
(global-set-key [tab-line S-wheel-left] 'tab-line-switch-to-prev-tab)
(global-set-key [tab-line S-wheel-right] 'tab-line-switch-to-next-tab)


(provide 'tab-line)
;;; tab-line.el ends here
