;;; tab-line.el --- window-local tabs with window buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022 Free Software Foundation, Inc.

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
(require 'seq) ; tab-line.el is not pre-loaded so it's safe to use it here


(defgroup tab-line nil
  "Window-local tabs."
  :group 'convenience
  :version "27.1")

(defcustom tab-line-tab-face-functions
  '(tab-line-tab-face-modified tab-line-tab-face-special)
  "Functions called to modify tab faces.
Each function is called with five arguments: the tab, a list of
all tabs, the face returned by the previously called modifier,
whether the tab is a buffer, and whether the tab is selected."
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
    (((class color) (min-colors 88))
     :box (:line-width 1 :style released-button))
    (t :inverse-video nil))
  "Tab line face for selected tab."
  :version "27.1"
  :group 'tab-line-faces)

(defface tab-line-tab-inactive
  '((default :inherit tab-line-tab)
    (((class color) (min-colors 88))
     :background "grey75")
    (t :inverse-video t))
  "Tab line face for non-selected tab."
  :version "27.1"
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
    (((class color) (min-colors 88))
     :background "grey85"))
  "Tab line face for tab with current buffer in selected window."
  :version "27.1"
  :group 'tab-line-faces)

(defface tab-line-highlight
  '((((class color) (min-colors 88))
     :box (:line-width 1 :style released-button)
     :background "grey85"
     :foreground "black")
    (t :inverse-video nil))
  "Tab line face for highlighting."
  :version "27.1"
  :group 'tab-line-faces)

(defface tab-line-close-highlight
  '((t :foreground "red"))
  "Tab line face for highlighting of the close button."
  :version "27.1"
  :group 'tab-line-faces)


(defvar tab-line-tab-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab-line down-mouse-1] 'tab-line-select-tab)
    (define-key map [tab-line mouse-2] 'tab-line-close-tab)
    (define-key map [tab-line down-mouse-3] 'tab-line-tab-context-menu)
    (define-key map "\C-m" 'tab-line-select-tab)
    map)
  "Local keymap for `tab-line-mode' window tabs.")

(defvar tab-line-add-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab-line down-mouse-1] 'tab-line-new-tab)
    (define-key map [tab-line down-mouse-2] 'tab-line-new-tab)
    (define-key map "\C-m" 'tab-line-new-tab)
    map)
  "Local keymap to add `tab-line-mode' window tabs.")

(defvar tab-line-tab-close-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab-line mouse-1] 'tab-line-close-tab)
    (define-key map [tab-line mouse-2] 'tab-line-close-tab)
    map)
  "Local keymap to close `tab-line-mode' window tabs.")

(defvar tab-line-left-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab-line down-mouse-1] 'tab-line-hscroll-left)
    (define-key map [tab-line down-mouse-2] 'tab-line-hscroll-left)
    (define-key map "\C-m" 'tab-line-new-tab)
    map)
  "Local keymap to scroll `tab-line-mode' window tabs to the left.")

(defvar tab-line-right-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab-line down-mouse-1] 'tab-line-hscroll-right)
    (define-key map [tab-line down-mouse-2] 'tab-line-hscroll-right)
    (define-key map "\C-m" 'tab-line-new-tab)
    map)
  "Local keymap to scroll `tab-line-mode' window tabs to the right.")


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
         (force-mode-line-update))
  :group 'tab-line
  :version "27.1")

(defvar tab-line-new-button
  (propertize " + "
              'display '(image :type xpm
                               :file "tabs/new.xpm"
                               :margin (2 . 0)
                               :ascent center)
              'keymap tab-line-add-map
              'mouse-face 'tab-line-highlight
              'help-echo "Click to add tab")
  "Button for creating a new tab.")

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
         (force-mode-line-update))
  :group 'tab-line
  :version "27.1")

(defvar tab-line-close-button
  (propertize " x"
              'display '(image :type xpm
                               :file "tabs/close.xpm"
                               :margin (2 . 0)
                               :ascent center)
              'keymap tab-line-tab-close-map
              'mouse-face 'tab-line-close-highlight
              'help-echo "Click to close tab")
  "Button for closing the clicked tab.")

(defvar tab-line-left-button
  (propertize " <"
              'display '(image :type xpm
                               :file "tabs/left-arrow.xpm"
                               :margin (2 . 0)
                               :ascent center)
              'keymap tab-line-left-map
              'mouse-face 'tab-line-highlight
              'help-echo "Click to scroll left")
  "Button for scrolling horizontally to the left.")

(defvar tab-line-right-button
  (propertize "> "
              'display '(image :type xpm
                               :file "tabs/right-arrow.xpm"
                               :margin (2 . 0)
                               :ascent center)
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
the list of all tabs."
  :type '(choice (const :tag "Buffer name"
                        tab-line-tab-name-buffer)
                 (const :tag "Truncated buffer name"
                        tab-line-tab-name-truncated-buffer)
                 (function :tag "Function"))
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (force-mode-line-update))
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
  :type 'integer
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


(defcustom tab-line-tabs-function #'tab-line-tabs-window-buffers
  "Function to get a list of tabs to display in the tab line.
This function should return either a list of buffers whose names will
be displayed, or just a list of strings to display in the tab line.
By default, use function `tab-line-tabs-window-buffers' that
returns a list of buffers associated with the selected window.
When `tab-line-tabs-mode-buffers', return a list of buffers
with the same major mode as the current buffer.
When `tab-line-tabs-buffer-groups', return a list of buffers
grouped either by `tab-line-tabs-buffer-group-function', when set,
or by `tab-line-tabs-buffer-groups'."
  :type '(choice (const :tag "Window buffers"
                        tab-line-tabs-window-buffers)
                 (const :tag "Same mode buffers"
                        tab-line-tabs-mode-buffers)
                 (const :tag "Grouped buffers"
                        tab-line-tabs-buffer-groups)
                 (function :tag "Function"))
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (force-mode-line-update))
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

(defvar tab-line-tabs-buffer-group-function nil
  "Function to add a buffer to the appropriate group of tabs.
Takes a buffer as arg and should return a group name as a string.
If the return value is nil, the buffer should be filtered out.")

(defvar tab-line-tabs-buffer-group-sort-function nil
  "Function to sort buffers in a group.")

(defvar tab-line-tabs-buffer-groups-sort-function #'string<
  "Function to sort group names.")

(defvar tab-line-tabs-buffer-groups mouse-buffer-menu-mode-groups
  "How to group various major modes together in the tab line.
Each element has the form (REGEXP . GROUPNAME).
If the major mode's name matches REGEXP, it belongs to GROUPNAME.
The default is for each major mode to have a separate group
named the same as the mode.")

(defun tab-line-tabs-buffer-group-name (&optional buffer)
  (if (functionp tab-line-tabs-buffer-group-function)
      (funcall tab-line-tabs-buffer-group-function buffer)
    (let ((mode (if buffer (with-current-buffer buffer
                             (format-mode-line mode-name))
                  (format-mode-line mode-name))))
      (or (cdr (seq-find (lambda (group)
                           (string-match-p (car group) mode))
                         tab-line-tabs-buffer-groups))
          mode))))

(defun tab-line-tabs-buffer-groups ()
  "Return a list of tabs that should be displayed in the tab line.
By default return a list of buffers grouped by major mode,
according to `tab-line-tabs-buffer-groups'.
If non-nil, `tab-line-tabs-buffer-group-function' is used to
generate the group name."
  (if (window-parameter nil 'tab-line-groups)
      (let* ((buffers (funcall tab-line-tabs-buffer-list-function))
             (groups
              (seq-sort tab-line-tabs-buffer-groups-sort-function
                        (delq nil (mapcar #'car (seq-group-by
                                                 (lambda (buffer)
                                                   (tab-line-tabs-buffer-group-name
                                                    buffer))
                                                 buffers)))))
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
                      groups)))
        tabs)

    (let* ((window-parameter (window-parameter nil 'tab-line-group))
           (group-name (tab-line-tabs-buffer-group-name (current-buffer)))
           (group (prog1 (or window-parameter group-name "All")
                    (when (equal window-parameter group-name)
                      (set-window-parameter nil 'tab-line-group nil))))
           (group-tab `(tab
                        (name . ,group)
                        (group-tab . t)
                        (select . ,(lambda ()
                                     (set-window-parameter nil 'tab-line-groups t)
                                     (set-window-parameter nil 'tab-line-group group)
                                     (set-window-parameter nil 'tab-line-hscroll nil)))))
           (buffers
            (seq-filter (lambda (b)
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
    (append (reverse prev-buffers)
            (list buffer)
            next-buffers)))


(defcustom tab-line-tab-name-format-function #'tab-line-tab-name-format-default
  "Function to format a tab name.
The function will be called two arguments: the tab whose name to format,
and the list of all the tabs; it should return the formatted tab name
to display in the tab line.
The first argument could also be a different object, for example the buffer
which the tab will represent."
  :type 'function
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (set-default sym val)
         (force-mode-line-update))
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
                   (if (eq (selected-window) (old-selected-window))
                       'tab-line-tab-current
                     'tab-line-tab)
                 'tab-line-tab-inactive)))
    (dolist (fn tab-line-tab-face-functions)
      (setf face (funcall fn tab tabs face buffer-p selected-p)))
    (apply 'propertize
           (concat (propertize name
                               'keymap tab-line-tab-map
                               ;; Don't turn mouse-1 into mouse-2 (bug#49247)
                               'follow-link 'ignore)
                   (or (and (or buffer-p (assq 'buffer tab) (assq 'close tab))
                            tab-line-close-button-show
                            (not (eq tab-line-close-button-show
                                     (if selected-p 'non-selected 'selected)))
                            tab-line-close-button)
                       ""))
           `(
             tab ,tab
             ,@(if selected-p '(selected t))
             face ,face
             mouse-face tab-line-highlight))))

(defun tab-line-format-template (tabs)
  "Template of the format for displaying tab line for selected window.
This is used by `tab-line-format'."
  (let* ((separator (or tab-line-separator (if window-system " " "|")))
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
     (when (and (eq tab-line-tabs-function #'tab-line-tabs-window-buffers)
                tab-line-new-button-show
                tab-line-new-button)
       (list tab-line-new-button)))))

(defun tab-line-tab-face-inactive-alternating (tab tabs face _buffer-p selected-p)
  "Return FACE for TAB in TABS with alternation.
SELECTED-P nil means TAB is not the selected tab.
When TAB is not selected and is even-numbered, make FACE
inherit from `tab-line-tab-inactive-alternate'.  For use in
`tab-line-tab-face-functions'."
  (when (and (not selected-p) (cl-evenp (cl-position tab tabs)))
    (setf face `(:inherit (tab-line-tab-inactive-alternate ,face))))
  face)

(defun tab-line-tab-face-special (tab _tabs face buffer-p _selected-p)
  "Return FACE for TAB according to whether its buffer is special.
When TAB is a non-file-visiting buffer, make FACE inherit from
`tab-line-tab-special'.  For use in
`tab-line-tab-face-functions'."
  (when (and buffer-p (not (buffer-file-name tab)))
    (setf face `(:inherit (tab-line-tab-special ,face))))
  face)

(defun tab-line-tab-face-modified (tab _tabs face buffer-p _selected-p)
  "Return FACE for TAB according to whether its buffer is modified.
When TAB is a modified, file-backed buffer, make FACE inherit
from `tab-line-tab-modified'.  For use in
`tab-line-tab-face-functions'."
  (when (and buffer-p (buffer-file-name tab) (buffer-modified-p tab))
    (setf face `(:inherit (tab-line-tab-modified ,face))))
  face)

(defun tab-line-tab-face-group (tab _tabs face _buffer-p _selected-p)
  "Return FACE for TAB according to whether it's a group tab.
For use in `tab-line-tab-face-functions'."
  (when (alist-get 'group-tab tab)
    (setf face `(:inherit (tab-line-tab-group ,face))))
  face)

(defvar tab-line-auto-hscroll)

(defun tab-line-format ()
  "Format for displaying the tab line of the selected window."
  (let* ((tabs (funcall tab-line-tabs-function))
         (cache-key (list tabs
                          ;; handle buffer renames
                          (buffer-name (window-buffer))
                          ;; handle tab-line scrolling
                          (window-parameter nil 'tab-line-hscroll)
                          ;; for setting face 'tab-line-tab-current'
                          (eq (selected-window) (old-selected-window))
                          (and (memq 'tab-line-tab-face-modified
                                     tab-line-tab-face-functions)
                               (buffer-file-name) (buffer-modified-p))))
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
      (setq truncate-lines nil)
      (erase-buffer)
      (apply 'insert strings)
      (goto-char (point-min))
      (add-face-text-property (point-min) (point-max) 'tab-line)
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
  (let ((window (and (listp event) (posn-window (event-start event)))))
    (tab-line-hscroll arg window)
    (force-mode-line-update window)))

(defun tab-line-hscroll-left (&optional arg event)
  "Scroll the tab line ARG positions to the left.
Interactively, ARG is the prefix numeric argument and defaults to 1."
  (interactive (list current-prefix-arg last-nonmenu-event))
  (let ((window (and (listp event) (posn-window (event-start event)))))
    (tab-line-hscroll (- (or arg 1)) window)
    (force-mode-line-update window)))


(defun tab-line-new-tab (&optional event)
  "Add a new tab to the selected-window's tab line.
This command is usually invoked by clicking on the plus-shaped button
on the tab line.  Switching to another buffer also adds a new tab
corresponding to the new buffer shown in the window."
  (interactive (list last-nonmenu-event))
  (if (functionp tab-line-new-tab-choice)
      (funcall tab-line-new-tab-choice)
    (let ((tab-line-tabs-buffer-groups mouse-buffer-menu-mode-groups))
      (if (and (listp event)
               (display-popup-menus-p)
               (not tty-menu-open-use-tmm))
          (mouse-buffer-menu event) ; like (buffer-menu-open)
        ;; tty menu doesn't support mouse clicks, so use tmm
        (tmm-prompt (mouse-buffer-menu-keymap))))))

(defun tab-line-select-tab (&optional event)
  "Switch to the buffer specified by the tab on which you click.
This command maintains the original order of prev/next buffers.
So, for example, switching to a previous tab is equivalent to
using the `previous-buffer' command."
  (interactive "e")
  (let* ((posnp (event-start event))
         (tab (tab-line--get-tab-property 'tab (car (posn-string posnp))))
         (buffer (if (bufferp tab) tab (cdr (assq 'buffer tab)))))
    (if buffer
        (tab-line-select-tab-buffer buffer (posn-window posnp))
      (let ((select (cdr (assq 'select tab))))
        (when (functionp select)
          (with-selected-window (posn-window posnp)
            (funcall select)
            (force-mode-line-update)))))))

(defun tab-line-select-tab-buffer (buffer &optional window)
  (let* ((window-buffer (window-buffer window))
         (next-buffers (seq-remove (lambda (b) (eq b window-buffer))
                                   (window-next-buffers window)))
         (prev-buffers (seq-remove (lambda (b) (eq b window-buffer))
                                   (mapcar #'car (window-prev-buffers window))))
         ;; Remove next-buffers from prev-buffers
         (prev-buffers (seq-difference prev-buffers next-buffers)))
    (cond
     ((and (eq tab-line-tabs-function #'tab-line-tabs-window-buffers)
           (memq buffer next-buffers))
      (dotimes (_ (1+ (seq-position next-buffers buffer)))
        (switch-to-next-buffer window)))
     ((and (eq tab-line-tabs-function #'tab-line-tabs-window-buffers)
           (memq buffer prev-buffers))
      (dotimes (_ (1+ (seq-position prev-buffers buffer)))
        (switch-to-prev-buffer window)))
     (t
      (with-selected-window window
        (switch-to-buffer buffer))))))

(defcustom tab-line-switch-cycling nil
  "Enable cycling tab switch.
If non-nil, `tab-line-switch-to-prev-tab' in the first tab
switches to the last tab and `tab-line-switch-to-next-tab' in the
last tab switches to the first tab.  This variable is not consulted
when `tab-line-tabs-function' is `tab-line-tabs-window-buffers'."
  :type 'boolean
  :group 'tab-line
  :version "28.1")

(defun tab-line-switch-to-prev-tab (&optional event)
  "Switch to the previous tab's buffer.
Its effect is the same as using the `previous-buffer' command
(\\[previous-buffer])."
  (interactive (list last-nonmenu-event))
  (let ((window (and (listp event) (posn-window (event-start event)))))
    (if (eq tab-line-tabs-function #'tab-line-tabs-window-buffers)
        (switch-to-prev-buffer window)
      (with-selected-window (or window (selected-window))
        (let* ((tabs (funcall tab-line-tabs-function))
               (pos (seq-position
                     tabs (current-buffer)
                     (lambda (tab buffer)
                       (if (bufferp tab)
                           (eq buffer tab)
                         (eq buffer (cdr (assq 'buffer tab)))))))
               (tab (if pos
                        (if (and tab-line-switch-cycling (<= pos 0))
                            (nth (1- (length tabs)) tabs)
                          (nth (1- pos) tabs))))
               (buffer (if (bufferp tab) tab (cdr (assq 'buffer tab)))))
          (when (bufferp buffer)
            (switch-to-buffer buffer)))))))

(defun tab-line-switch-to-next-tab (&optional event)
  "Switch to the next tab's buffer.
Its effect is the same as using the `next-buffer' command
(\\[next-buffer])."
  (interactive (list last-nonmenu-event))
  (let ((window (and (listp event) (posn-window (event-start event)))))
    (if (eq tab-line-tabs-function #'tab-line-tabs-window-buffers)
        (switch-to-next-buffer window)
      (with-selected-window (or window (selected-window))
        (let* ((tabs (funcall tab-line-tabs-function))
               (pos (seq-position
                     tabs (current-buffer)
                     (lambda (tab buffer)
                       (if (bufferp tab)
                           (eq buffer tab)
                         (eq buffer (cdr (assq 'buffer tab)))))))
               (tab (if pos
                        (if (and tab-line-switch-cycling (<= (length tabs) (1+ pos)))
                            (car tabs)
                          (nth (1+ pos) tabs))))
               (buffer (if (bufferp tab) tab (cdr (assq 'buffer tab)))))
          (when (bufferp buffer)
            (switch-to-buffer buffer)))))))


(defcustom tab-line-close-tab-function 'bury-buffer
  "What to do upon closing a tab on the tab line.
If `bury-buffer', put the tab's buffer at the end of the list of all
buffers, which effectively hides the buffer's tab from the tab line.
If `kill-buffer', kills the tab's buffer.
When a function, it is called with the tab as its argument.
This option is useful when `tab-line-tabs-function' has the value
`tab-line-tabs-window-buffers'."
  :type '(choice (const :tag "Bury buffer" bury-buffer)
                 (const :tag "Kill buffer" kill-buffer)
                 (function :tag "Function"))
  :group 'tab-line
  :version "27.1")

(defun tab-line-close-tab (&optional event)
  "Close the selected tab.
This command is usually invoked by clicking on the close button on the
right side of the tab.  This command buries the buffer, so it goes out of
sight of the tab line."
  (interactive (list last-nonmenu-event))
  (let* ((posnp (and (listp event) (event-start event)))
         (window (and posnp (posn-window posnp)))
         (tab (tab-line--get-tab-property 'tab (car (posn-string posnp))))
         (buffer (if (bufferp tab) tab (cdr (assq 'buffer tab))))
         (close-function (unless (bufferp tab) (cdr (assq 'close tab)))))
    (with-selected-window (or window (selected-window))
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
      (force-mode-line-update))))

(defun tab-line-tab-context-menu (&optional event)
  "Pop up the context menu for a tab-line tab."
  (interactive "e")
  (let ((menu (make-sparse-keymap (propertize "Context Menu" 'hide t))))
    (define-key-after menu [close]
      '(menu-item "Close" tab-line-close-tab :help "Close the tab"))
    (popup-menu menu event)))

(defun tab-line-context-menu (&optional event)
  "Pop up the context menu for the tab line."
  (interactive "e")
  (let ((menu (make-sparse-keymap (propertize "Context Menu" 'hide t))))
    (define-key-after menu [close]
      '(menu-item "New tab" tab-line-new-tab :help "Create a new tab"))
    (popup-menu menu event)))


;;;###autoload
(define-minor-mode tab-line-mode
  "Toggle display of tab line in the windows displaying the current buffer."
  :lighter nil
  (setq tab-line-format (when tab-line-mode '(:eval (tab-line-format)))))

(defcustom tab-line-exclude-modes
  '(completion-list-mode)
  "List of major modes for which the tab-line display is not enabled.
Buffers under any of these major modes will not show the tab line in
their windows, even if `global-tab-line-mode' is enabled."
  :type '(repeat symbol)
  :group 'tab-line
  :version "27.1")

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
              (get major-mode 'tab-line-exclude)
              (buffer-local-value 'tab-line-exclude (current-buffer)))
    (tab-line-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-tab-line-mode
  tab-line-mode tab-line-mode--turn-on
  :group 'tab-line
  :version "27.1")


(global-set-key [tab-line down-mouse-3] 'tab-line-context-menu)

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
