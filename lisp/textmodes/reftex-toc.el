;;; reftex-toc.el --- RefTeX's table of contents mode

;; Copyright (C) 1997-2000, 2003-2017 Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@science.uva.nl>
;; Maintainer: auctex-devel@gnu.org

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

;;; Code:

(provide 'reftex-toc)
(require 'reftex)
;;;

(define-obsolete-variable-alias 'reftex-toc-map 'reftex-toc-mode-map "24.1")
(defvar reftex-toc-mode-map
  (let ((map (make-sparse-keymap)))

    (define-key map (if (featurep 'xemacs) [(button2)] [(mouse-2)])
      'reftex-toc-mouse-goto-line-and-hide)
    (define-key map [follow-link] 'mouse-face)

    (substitute-key-definition
     'next-line 'reftex-toc-next map global-map)
    (substitute-key-definition
     'previous-line 'reftex-toc-previous map global-map)

    (define-key map "n" 'reftex-toc-next)
    (define-key map "p" 'reftex-toc-previous)
    (define-key map "?" 'reftex-toc-show-help)
    (define-key map " " 'reftex-toc-view-line)
    (define-key map "\C-m" 'reftex-toc-goto-line-and-hide)
    (define-key map "\C-i" 'reftex-toc-goto-line)
    (define-key map "\C-c>" 'reftex-toc-display-index)
    (define-key map "r" 'reftex-toc-rescan)
    (define-key map "R" 'reftex-toc-Rescan)
    (define-key map "q" 'reftex-toc-quit) ;
    (define-key map "k" 'reftex-toc-quit-and-kill)
    (define-key map "f" 'reftex-toc-toggle-follow) ;
    (define-key map "a" 'reftex-toggle-auto-toc-recenter)
    (define-key map "d" 'reftex-toc-toggle-dedicated-frame)
    (define-key map "F" 'reftex-toc-toggle-file-boundary)
    (define-key map "i" 'reftex-toc-toggle-index)
    (define-key map "l" 'reftex-toc-toggle-labels)
    (define-key map "t" 'reftex-toc-max-level)
    (define-key map "c" 'reftex-toc-toggle-context)
    ;; (define-key map "%" 'reftex-toc-toggle-commented)
    (define-key map "\M-%" 'reftex-toc-rename-label)
    (define-key map "x" 'reftex-toc-external)
    (define-key map "z" 'reftex-toc-jump)
    (define-key map "." 'reftex-toc-show-calling-point)
    (define-key map "\C-c\C-n" 'reftex-toc-next-heading)
    (define-key map "\C-c\C-p" 'reftex-toc-previous-heading)
    (define-key map ">" 'reftex-toc-demote)
    (define-key map "<" 'reftex-toc-promote)

    (easy-menu-define
      reftex-toc-menu map
      "Menu for Table of Contents buffer"
      '("TOC"
        ["Show Location" reftex-toc-view-line t]
        ["Go To Location" reftex-toc-goto-line t]
        ["Exit & Go To Location" reftex-toc-goto-line-and-hide t]
        ["Show Calling Point" reftex-toc-show-calling-point t]
        ["Quit" reftex-toc-quit t]
        "--"
        ("Edit"
         ["Promote" reftex-toc-promote t]
         ["Demote" reftex-toc-demote t]
         ["Rename Label" reftex-toc-rename-label t])
        "--"
        ["Index" reftex-toc-display-index t]
        ["External Document TOC  " reftex-toc-external t]
        "--"
        ("Update"
         ["Rebuilt *toc* Buffer" revert-buffer t]
         ["Rescan One File" reftex-toc-rescan reftex-enable-partial-scans]
         ["Rescan Entire Document" reftex-toc-Rescan t])
        ("Options"
         "TOC Items"
         ["File Boundaries" reftex-toc-toggle-file-boundary :style toggle
          :selected reftex-toc-include-file-boundaries]
         ["Labels" reftex-toc-toggle-labels :style toggle
          :selected reftex-toc-include-labels]
         ["Index Entries" reftex-toc-toggle-index :style toggle
          :selected reftex-toc-include-index-entries]
         ["Context" reftex-toc-toggle-context :style toggle
          :selected reftex-toc-include-context]
         "--"
         ["Follow Mode" reftex-toc-toggle-follow :style toggle
          :selected reftex-toc-follow-mode]
         ["Auto Recenter" reftex-toggle-auto-toc-recenter :style toggle
          :selected reftex-toc-auto-recenter-timer]
         ["Dedicated Frame" reftex-toc-toggle-dedicated-frame t])
        "--"
        ["Help" reftex-toc-show-help t]))

    map)
  "Keymap used for *toc* buffer.")

(defvar reftex-toc-menu)
(defvar reftex-last-window-height nil)
(defvar reftex-last-window-width nil)
(defvar reftex-toc-include-labels-indicator nil)
(defvar reftex-toc-include-index-indicator nil)
(defvar reftex-toc-max-level-indicator nil)

(define-derived-mode reftex-toc-mode special-mode "TOC"
  "Major mode for managing Table of Contents for LaTeX files.
This buffer was created with RefTeX.
Press `?' for a summary of important key bindings.

Here are all local bindings.

\\{reftex-toc-mode-map}"
  (set (make-local-variable 'transient-mark-mode) t)
  (when (featurep 'xemacs)
    (set (make-local-variable 'zmacs-regions) t))
  (set (make-local-variable 'revert-buffer-function) 'reftex-toc-revert)
  (set (make-local-variable 'reftex-toc-include-labels-indicator) "")
  (set (make-local-variable 'reftex-toc-max-level-indicator)
       (if (= reftex-toc-max-level 100)
           "ALL"
         (int-to-string reftex-toc-max-level)))
  (setq mode-line-format
        (list "----  " 'mode-line-buffer-identification
              "  " 'global-mode-string "   (" mode-name ")"
              "  L<" 'reftex-toc-include-labels-indicator ">"
              "  I<" 'reftex-toc-include-index-indicator ">"
              "  T<" 'reftex-toc-max-level-indicator ">"
              " -%-"))
  (setq truncate-lines t)
  (when (featurep 'xemacs)
    ;; XEmacs needs the call to make-local-hook
    (make-local-hook 'post-command-hook)
    (make-local-hook 'pre-command-hook))
  (make-local-variable 'reftex-last-follow-point)
  (add-hook 'post-command-hook 'reftex-toc-post-command-hook nil t)
  (add-hook 'pre-command-hook  'reftex-toc-pre-command-hook nil t)
  (easy-menu-add reftex-toc-menu reftex-toc-mode-map))

(defvar reftex-last-toc-file nil
  "Stores the file name from which `reftex-toc' was called.  For redo command.")


(defvar reftex-toc-return-marker (make-marker)
  "Marker which makes it possible to return from TOC to old position.")

(defconst reftex-toc-help
"                      AVAILABLE KEYS IN TOC BUFFER
                      ============================
n / p      next-line / previous-line
SPC        Show the corresponding location of the LaTeX document.
TAB        Goto the location and keep the TOC window.
RET        Goto the location and hide the TOC window (also on mouse-2).
< / >      Promote / Demote section, or all sections in region.
C-c >      Display Index. With prefix arg, restrict index to current section.
q / k      Hide/Kill *toc* buffer, return to position of reftex-toc command.
l i c F    Toggle display of  [l]abels,  [i]ndex,  [c]ontext,  [F]ile borders.
t          Change maximum toc depth (e.g. `3 t' hides levels greater than 3).
f / g      Toggle follow mode / Refresh *toc* buffer.
a / d      Toggle auto recenter / Toggle dedicated frame
r / C-u r  Reparse the LaTeX document     / Reparse entire LaTeX document.
.          In other window, show position from where `reftex-toc' was called.
M-%        Global search and replace to rename label at point.
x          Switch to TOC of external document (with LaTeX package `xr').
z          Jump to a specific section (e.g. '3 z' goes to section 3).")

(defvar reftex--rebuilding-toc nil)

;;;###autoload
(defun reftex-toc (&optional _rebuild reuse)
  ;; FIXME: Get rid of the `rebuild' argument.
  "Show the table of contents for the current document.
When called with a raw C-u prefix, rescan the document first."

;; The REUSE argument means, search all visible frames for a window
;; displaying the toc window.  If yes, reuse this window.

  (interactive)

  (if (or (not (string= reftex-last-toc-master (reftex-TeX-master-file)))
          ;; FIXME: use (interactive "P") to receive current-prefix-arg as
          ;; an argument instead of using the var here, which forces us to set
          ;; current-prefix-arg in the callers.
          current-prefix-arg)
      (reftex-erase-buffer "*toc*"))

  (setq reftex-last-toc-file   (buffer-file-name))
  (setq reftex-last-toc-master (reftex-TeX-master-file))

  (set-marker reftex-toc-return-marker (point))

  ;; If follow mode is active, arrange to delay it one command
  (if reftex-toc-follow-mode
      (setq reftex-toc-follow-mode 1))

  (and reftex-toc-include-index-entries
       (reftex-ensure-index-support))
  (or reftex-support-index
      (setq reftex-toc-include-index-entries nil))

  ;; Ensure access to scanning info and rescan buffer if prefix arg is '(4)
  (reftex-access-scan-info current-prefix-arg)

  (let* ((this-buf (current-buffer))
         (docstruct-symbol reftex-docstruct-symbol)
         (xr-data (assq 'xr (symbol-value reftex-docstruct-symbol)))
         (xr-alist (cons (cons "" (buffer-file-name)) (nth 1 xr-data)))
         (here-I-am (if reftex--rebuilding-toc
                        (get 'reftex-toc :reftex-data)
                      (car (reftex-where-am-I))))
         (unsplittable (if (fboundp 'frame-property)
                           (frame-property (selected-frame) 'unsplittable)
                         (frame-parameter nil 'unsplittable)))
         offset toc-window)

    (if (setq toc-window (get-buffer-window
                          "*toc*"
                          (if reuse 'visible)))
        (select-window toc-window)
      (when (or (not reftex-toc-keep-other-windows)
                (< (window-height) (* 2 window-min-height)))
        (delete-other-windows))

      (setq reftex-last-window-width (window-total-width)
            reftex-last-window-height (window-height))  ; remember

      (unless unsplittable
        (if reftex-toc-split-windows-horizontally
            (split-window-right
             (floor (* (window-total-width)
                       reftex-toc-split-windows-fraction)))
          (split-window-below
           (floor (* (window-height)
                     reftex-toc-split-windows-fraction)))))

      (switch-to-buffer "*toc*"))

    (or (eq major-mode 'reftex-toc-mode) (reftex-toc-mode))
    (set (make-local-variable 'reftex-docstruct-symbol) docstruct-symbol)
    (setq reftex-toc-include-labels-indicator
          (if (eq reftex-toc-include-labels t)
              "ALL"
            reftex-toc-include-labels))
    (setq reftex-toc-include-index-indicator
          (if (eq reftex-toc-include-index-entries t)
              "ALL"
            reftex-toc-include-index-entries))

    (cond
     ((= (buffer-size) 0)
      ;; buffer is empty - fill it with the table of contents
      (message "Building *toc* buffer...")

      (setq buffer-read-only nil)
      (insert (format
"TABLE-OF-CONTENTS on %s
SPC=view TAB=goto RET=goto+hide [q]uit [r]escan [l]abels [f]ollow [x]r [?]Help
------------------------------------------------------------------------------
" (abbreviate-file-name reftex-last-toc-master)))

      (if (reftex-use-fonts)
          (put-text-property (point-min) (point) 'font-lock-face reftex-toc-header-face))
      (if (fboundp 'cursor-intangible-mode)
          (cursor-intangible-mode 1)
        ;; If `cursor-intangible' is not available, fallback on the old
        ;; intrusive `intangible' property.
        (put-text-property (point-min) (point) 'intangible t))
      (add-text-properties (point-min) (point)
                           '(cursor-intangible t
                             front-sticky (cursor-intangible)
                             rear-nonsticky (cursor-intangible)))
      (put-text-property (point-min) (1+ (point-min)) 'xr-alist xr-alist)

      (setq offset
            (reftex-insert-docstruct
             this-buf
             t ; include TOC
             reftex-toc-include-labels
             reftex-toc-include-index-entries
             reftex-toc-include-file-boundaries
             reftex-toc-include-context
             nil ; counter
             nil ; commented
             here-I-am
             ""     ; xr-prefix
             t      ; a TOC buffer
             ))

      (run-hooks 'reftex-display-copied-context-hook)
      (message "Building *toc* buffer...done.")
      (setq buffer-read-only t))
     (t
      ;; Only compute the offset
      (setq offset
            (or (reftex-get-offset this-buf here-I-am
                                   (if reftex-toc-include-labels " " nil)
                                   t
                                   reftex-toc-include-index-entries
                                   reftex-toc-include-file-boundaries)
                (reftex-last-assoc-before-elt
                 'toc here-I-am
                 (symbol-value reftex-docstruct-symbol))))
      (put 'reftex-toc :reftex-line 3)
      (goto-char (point-min))
      (forward-line 2)))

    ;; Find the correct starting point
    (reftex-find-start-point (point) offset (get 'reftex-toc :reftex-line))
    (setq reftex-last-follow-point (point))))

;;;###autoload
(defun reftex-toc-recenter (&optional arg)
  "Display the TOC window and highlight line corresponding to current position."
  (interactive "P")
  (let ((buf (current-buffer))
        (frame (selected-frame)))
    (reftex-toc arg t)
    (if (= (count-lines 1 (point)) 2)
        (let ((current-prefix-arg nil))
          (select-window (get-buffer-window buf frame))
          (reftex-toc nil t)))
    (and (> (point) 1) ;FIXME: Is this point-min or do we care about narrowing?
         (not (get-text-property (point) 'cursor-intangible))
         (memq reftex-highlight-selection '(cursor both))
         (reftex-highlight 2
                           (or (previous-single-property-change
                                (min (point-max) (1+ (point))) :data)
                               (point-min))
                           (or (next-single-property-change (point) :data)
                               (point-max))))
    (select-window (get-buffer-window buf frame))))

(defun reftex-toc-pre-command-hook ()
  ;; used as pre command hook in *toc* buffer
  (reftex-unhighlight 0)
  )

(defun reftex-toc-post-command-hook ()
  ;; used in the post-command-hook for the *toc* buffer
  ;; FIXME: Lots of redundancy with reftex-index-post-command-hook!
  (when (get-text-property (point) :data)
    (put 'reftex-toc :reftex-data (get-text-property (point) :data))
    (and (> (point) 1) ;FIXME: Is this point-min or do we care about narrowing?
         (not (get-text-property (point) 'cursor-intangible))
         (memq reftex-highlight-selection '(cursor both))
         (reftex-highlight 2
           (or (previous-single-property-change (1+ (point)) :data)
               (point-min))
           (or (next-single-property-change (point) :data)
               (point-max)))))
  (if (integerp reftex-toc-follow-mode)
      ;; remove delayed action
      (setq reftex-toc-follow-mode t)
    (and (not (reftex-toc-dframe-p))
         reftex-toc-follow-mode
         (not (equal reftex-last-follow-point (point)))
         ;; show context in other window
         (setq reftex-last-follow-point (point))
         (condition-case nil
             (reftex-toc-visit-location nil (not reftex-revisit-to-follow))
           (error t)))))

(defun reftex-re-enlarge ()
  "Enlarge window to a remembered size."
  ;; FIXME: reftex-last-window-width might be the width of another window on
  ;; another frame, so the enlarge-window call might make no sense.
  ;; We should just use `quit-window' instead nowadays.
  (let ((count (if reftex-toc-split-windows-horizontally
		   (- (or reftex-last-window-width (window-total-width))
		      (window-total-width))
		 (- (or reftex-last-window-height (window-height))
		    (window-height)))))
    (when (> count 0)
      (with-demoted-errors           ;E.g. the window might be the root window!
        (enlarge-window count reftex-toc-split-windows-horizontally)))))

(defun reftex-toc-dframe-p (&optional frame error)
  ;; Check if FRAME is the dedicated TOC frame.
  ;; If yes, and ERROR is non-nil, throw an error.
  (setq frame (or frame (selected-frame)))
  (let ((res (equal
              (if (fboundp 'frame-property)
                  (frame-property frame 'name)
                (frame-parameter  frame 'name))
              "RefTeX TOC Frame")))
    (if (and res error)
        (error "This frame is view-only.  Use `C-c =' to create TOC window for commands"))
    res))

(defun reftex-toc-show-help ()
  "Show a summary of special key bindings."
  (interactive)
  (reftex-toc-dframe-p nil 'error)
  (with-output-to-temp-buffer "*RefTeX Help*"
    (princ reftex-toc-help))
  (reftex-enlarge-to-fit "*RefTeX Help*" t)
  ;; If follow mode is active, arrange to delay it one command
  (if reftex-toc-follow-mode
      (setq reftex-toc-follow-mode 1)))

(defun reftex-toc-next (&optional _arg)
  "Move to next selectable item."
  (interactive)
  (when (featurep 'xemacs) (setq zmacs-region-stays t))
  (setq reftex-callback-fwd t)
  (or (eobp) (forward-char 1))
  (goto-char (or (next-single-property-change (point) :data)
                 (point))))
(defun reftex-toc-previous (&optional _arg)
  "Move to previous selectable item."
  (interactive)
  (when (featurep 'xemacs) (setq zmacs-region-stays t))
  (setq reftex-callback-fwd nil)
  (goto-char (or (previous-single-property-change (point) :data)
                 (point))))
(defun reftex-toc-next-heading (&optional arg)
  "Move to next table of contents line."
  (interactive "p")
  (when (featurep 'xemacs) (setq zmacs-region-stays t))
  (end-of-line)
  (re-search-forward "^ " nil t arg)
  (beginning-of-line))
(defun reftex-toc-previous-heading (&optional arg)
  "Move to previous table of contents line."
  (interactive "p")
  (when (featurep 'xemacs) (setq zmacs-region-stays t))
  (re-search-backward "^ " nil t arg))
(defun reftex-toc-toggle-follow ()
  "Toggle follow (other window follows with context)."
  (interactive)
  (setq reftex-last-follow-point -1)
  (setq reftex-toc-follow-mode (not reftex-toc-follow-mode)))
(defun reftex-toc-toggle-file-boundary ()
  "Toggle inclusion of file boundaries in *toc* buffer."
  (interactive)
  (setq reftex-toc-include-file-boundaries
        (not reftex-toc-include-file-boundaries))
  (reftex-toc-revert))
(defun reftex-toc-toggle-labels (arg)
  "Toggle inclusion of labels in *toc* buffer.
With prefix ARG, prompt for a label type and include only labels of
that specific type."
  (interactive "P")
  (setq reftex-toc-include-labels
        (if arg (reftex-query-label-type)
          (not reftex-toc-include-labels)))
  (reftex-toc-revert))
(defun reftex-toc-toggle-index (arg)
  "Toggle inclusion of index in *toc* buffer.
With prefix arg, prompt for an index tag and include only entries of that
specific index."
  (interactive "P")
  (setq reftex-toc-include-index-entries
        (if arg (reftex-index-select-tag)
          (not reftex-toc-include-index-entries)))
  (reftex-toc-revert))
(defun reftex-toc-toggle-context ()
  "Toggle inclusion of label context in *toc* buffer.
Label context is only displayed when the labels are there as well."
  (interactive)
  (setq reftex-toc-include-context (not reftex-toc-include-context))
  (reftex-toc-revert))
(defun reftex-toc-max-level (arg)
  "Set the maximum level of TOC lines in this buffer to value of prefix ARG.
When no prefix is given, set the max level to a large number, so that all
levels are shown.  For example, to set the level to 3, type `3 m'."
  (interactive "P")
  (setq reftex-toc-max-level (if arg
                                 (prefix-numeric-value arg)
                               100))
  (setq reftex-toc-max-level-indicator
        (if arg (int-to-string reftex-toc-max-level) "ALL"))
  (reftex-toc-revert))
(defun reftex-toc-view-line ()
  "View document location in other window."
  (interactive)
  (reftex-toc-dframe-p nil 'error)
  (reftex-toc-visit-location))
(defun reftex-toc-goto-line-and-hide ()
  "Go to document location in other window.  Hide the TOC window."
  (interactive)
  (reftex-toc-dframe-p nil 'error)
  (reftex-toc-visit-location 'hide))
(defun reftex-toc-goto-line ()
  "Go to document location in other window.  TOC window stays."
  (interactive)
  (reftex-toc-dframe-p nil 'error)
  (reftex-toc-visit-location t))
(defun reftex-toc-mouse-goto-line-and-hide (ev)
  "Go to document location in other window.  Hide the TOC window."
  (interactive "e")
  (mouse-set-point ev)
  (reftex-toc-dframe-p nil 'error)
  (reftex-toc-visit-location 'hide))
(defun reftex-toc-show-calling-point ()
  "Show point where `reftex-toc' was called from."
  (interactive)
  (reftex-toc-dframe-p nil 'error)
  (let ((this-window (selected-window)))
    (unwind-protect
        (progn
          (switch-to-buffer-other-window
           (marker-buffer reftex-toc-return-marker))
          (goto-char (marker-position reftex-toc-return-marker))
          (recenter '(4)))
      (select-window this-window))))
(defun reftex-toc-quit ()
  "Hide the TOC window and do not move point.
If the TOC window is the only window on the dedicated TOC frame, the frame
is destroyed."
  (interactive)
  (if (and (one-window-p)
           (reftex-toc-dframe-p)
           (> (length (frame-list)) 1))
      (delete-frame)
    (or (one-window-p) (delete-window))
    (switch-to-buffer (marker-buffer reftex-toc-return-marker))
    (reftex-re-enlarge)
    (goto-char (or (marker-position reftex-toc-return-marker) (point)))))
(defun reftex-toc-quit-and-kill ()
  "Kill the *toc* buffer."
  (interactive)
  (kill-buffer "*toc*")
  (or (one-window-p) (delete-window))
  (switch-to-buffer (marker-buffer reftex-toc-return-marker))
  (reftex-re-enlarge)
  (goto-char (marker-position reftex-toc-return-marker)))
(defun reftex-toc-display-index (&optional arg)
  "Display the index buffer for the current document.
This works just like `reftex-display-index' from a LaTeX buffer.
With prefix arg 1, restrict index to the section at point."
  (interactive "P")
  (reftex-toc-dframe-p nil 'error)
  (let ((data (get-text-property (point) :data))
        (docstruct (symbol-value reftex-docstruct-symbol))
        bor eor restr)
    (when (equal arg 2)
      (setq bor (reftex-last-assoc-before-elt 'toc data docstruct)
            eor (assoc 'toc (cdr (memq bor docstruct)))
            restr (list (nth 6 bor) bor eor)))
    (reftex-toc-goto-line)
    (reftex-display-index (if restr nil arg) restr)))

;; Rescanning the document and rebuilding the TOC buffer.
(defun reftex-toc-rescan (&rest _)
  "Regenerate the *toc* buffer by reparsing file of section at point."
  (interactive)
  (if (and reftex-enable-partial-scans
           (null current-prefix-arg))
      (let* ((data (get-text-property (point) :data))
             (what (car data))
             (file (cond ((eq what 'toc) (nth 3 data))
                         ((memq what '(eof bof file-error)) (nth 1 data))
                         ((stringp what) (nth 3 data))
                         ((eq what 'index) (nth 3 data))))
             (line (+ (count-lines (point-min) (point)) (if (bolp) 1 0))))
        (if (not file)
            (error "Don't know which file to rescan.  Try `C-u r'")
          (put 'reftex-toc :reftex-line line)
          (switch-to-buffer-other-window
           (reftex-get-file-buffer-force file))
          (setq current-prefix-arg '(4))
          (let ((reftex--rebuilding-toc t))
            (reftex-toc))))
    (reftex-toc-Rescan))
  (reftex-kill-temporary-buffers))

(defun reftex-toc-Rescan (&rest _)
  "Regenerate the *toc* buffer by reparsing the entire document."
  (interactive)
  (let* ((line (+ (count-lines (point-min) (point)) (if (bolp) 1 0))))
    (put 'reftex-toc :reftex-line line))
  (switch-to-buffer-other-window
   (reftex-get-file-buffer-force reftex-last-toc-file))
  (setq current-prefix-arg '(16))
  (let ((reftex--rebuilding-toc t))
    (reftex-toc)))

(defun reftex-toc-revert (&rest _)
  "Regenerate the TOC from the internal lists."
  (interactive)
  (let ((unsplittable
         (if (fboundp 'frame-property)
             (frame-property (selected-frame) 'unsplittable)
           (frame-parameter nil 'unsplittable)))
        (reftex--rebuilding-toc t))
    (if unsplittable
        (switch-to-buffer
         (reftex-get-file-buffer-force reftex-last-toc-file))
      (switch-to-buffer-other-window
       (reftex-get-file-buffer-force reftex-last-toc-file))))
  (reftex-erase-buffer "*toc*")
  (setq current-prefix-arg nil)
  (reftex-toc t t))

(defun reftex-toc-external (&rest _)
  "Switch to table of contents of an external document."
  (interactive)
  (reftex-toc-dframe-p nil 'error)
  (let* ((old-buf (current-buffer))
         (xr-alist (get-text-property 1 'xr-alist))
         (xr-index (reftex-select-external-document
                   xr-alist 0)))
    (switch-to-buffer-other-window (or (reftex-get-file-buffer-force
                                        (cdr (nth xr-index xr-alist)))
                                       (error "Cannot switch document")))
    (reftex-toc)
    (if (equal old-buf (current-buffer))
        (message "")
      (message "Switched document"))))

(defun reftex-toc-jump (arg)
  "Jump to a specific section.  E.g. '3 z' jumps to section 3.
Useful for large TOCs."
  (interactive "P")
  (goto-char (point-min))
  (re-search-forward
   (concat "^ *" (number-to-string (if (numberp arg) arg 1)) " ")
   nil t)
  (beginning-of-line))

;; Promotion/Demotion stuff

(defvar reftex--pro-or-de)
(defvar reftex--start-line)
(defvar reftex--mark-line)

(defun reftex-toc-demote (&optional _arg)
  "Demote section at point.  If region is active, apply to all in region."
  (interactive)
  (reftex-toc-do-promote 1))
(defun reftex-toc-promote (&optional _arg)
  "Promote section at point.  If region is active, apply to all in region."
  (interactive)
  (reftex-toc-do-promote -1))
(defun reftex-toc-do-promote (delta)
  "Workhorse for `reftex-toc-promote' and `reftex-toc-demote'.
Changes the level of sections in the current region, or just the section at
point."
  ;; We should not do anything unless we are sure this is going to work for
  ;; each section in the region.  Therefore we first collect information and
  ;; test.
  (let* ((reftex--start-line (+ (count-lines (point-min) (point))
                                (if (bolp) 1 0)))
	 (reftex--mark-line
          (if (reftex-region-active-p)
              (save-excursion (goto-char (mark))
                              (+ (count-lines (point-min) (point))
                                 (if (bolp) 1 0)))))
         (start-pos (point))
         (reftex--pro-or-de (if (> delta 0) "de" "pro"))
         beg end entries data sections nsec msg)
    (setq msg
          (catch 'exit
            (if (reftex-region-active-p)
                ;; A region is dangerous, check if we have a brand new scan,
                ;; to make sure we are not missing any section statements.
                (if (not (reftex-toc-check-docstruct))
                    (reftex-toc-load-all-files-for-promotion)   ;; exits
                  (setq beg (min (point) (mark))
                        end (max (point) (mark))))
              (setq beg (point) end (point)))
            (goto-char beg)
            (while (and (setq data (get-text-property (point) :data))
                        (<= (point) end))
              (if (eq (car data) 'toc) (push (cons data (point)) entries))
              (goto-char (or (next-single-property-change (point) :data)
                             (point-max))))
            (setq entries (nreverse entries))
            ;; Get the relevant section numbers, for an informative message
            (goto-char start-pos)
            (setq sections (reftex-toc-extract-section-number (car entries)))
            (if (> (setq nsec (length entries)) 1)
                (setq sections
                      (concat sections "-"
                              (reftex-toc-extract-section-number
                               (nth (1- nsec) entries)))))
            ;; Run through the list and prepare the changes.
            (setq entries (mapcar
                           (lambda (e) (reftex-toc-promote-prepare e delta))
                           entries))
            ;; Ask for permission
            (if (or (not reftex-toc-confirm-promotion)           ; never confirm
                    (and (integerp reftex-toc-confirm-promotion) ; confirm if many
                         (< nsec reftex-toc-confirm-promotion))
                    (yes-or-no-p                                 ; ask
                     (format "%s %d toc-entr%s (section%s %s)? "
                             (if (< delta 0) "Promote" "Demote")
                             nsec
                             (if (= 1 nsec) "y" "ies")
                             (if (= 1 nsec) "" "s")
                             sections)))
                nil              ; we have permission, do nothing
              (error "Abort"))   ; abort, we don't have permission
            ;; Do the changes
            (mapc 'reftex-toc-promote-action entries)
            ;; Rescan the document and rebuilt the toc buffer
            (save-window-excursion
              (reftex-toc-Rescan))
            (reftex-toc-restore-region reftex--start-line reftex--mark-line)
            (message "%d section%s %smoted"
                     nsec (if (= 1 nsec) "" "s") reftex--pro-or-de)
            nil))
    (if msg (progn (ding) (message "%s" msg)))))


(defun reftex-toc-restore-region (point-line &optional mark-line)
  (let ((mpos
         (when mark-line
           (goto-char (point-min))
           (forward-line (1- mark-line))
           (point))))
    (when point-line
      (goto-char (point-min))
      (forward-line (1- point-line)))
    (when mpos
      (set-mark mpos)
      (if (featurep 'xemacs)
          (zmacs-activate-region)
        (setq mark-active t
              deactivate-mark nil)))))

(defun reftex-toc-promote-prepare (x delta)
  "Look at a TOC entry and see if we could pro/demote it.
This function prepares everything for the change, but does not do it.
The return value is a list with information needed when doing the
promotion/demotion later.  DELTA is the level change."
  (let* ((data (car x))
         (toc-point (cdr x))
         (marker (nth 4 data))
         (literal (nth 7 data))
         (load nil)
         (name nil)
         ;; Here follows some paranoid code to make very sure we are not
         ;; going to break anything
         (_
          (if (and (markerp marker) (marker-buffer marker))
              ;; Buffer is still live and we have the marker.
              (progn
                (with-current-buffer (marker-buffer marker)
                  ;; Goto the buffer and check of section is unchanged
                  (goto-char (marker-position marker))
                  (if (looking-at (regexp-quote literal))
                      ;; OK, get the makro name
                      (progn
                        (beginning-of-line 1)
                        (if (looking-at reftex-section-regexp)
                            (setq name (reftex-match-string 2))
                          (error "Something is wrong!  Contact maintainer!")))
                    ;; Section has changed, request scan and loading
                    ;; We use a variable to delay until after the safe-exc.
                    ;; because otherwise we lose the region.
                    (setq load t)))
                ;; Scan document and load all files, this exits command
                (if load (reftex-toc-load-all-files-for-promotion))) ; exits
            ;; We don't have a live marker: scan and load files.
            (reftex-toc-load-all-files-for-promotion)))
         (level (cdr (assoc name reftex-section-levels-all)))
         (_ (if (not (integerp level))
                (progn
                  (goto-char toc-point)
                  (error "Cannot %smote special sections" reftex--pro-or-de))))
         (newlevel (if (>= level 0) (+ delta level) (- level delta)))
         (_ (if (or (and (>= level 0) (= newlevel -1))
                    (and (< level 0)  (= newlevel 0)))
                (error "Cannot %smote \\%s" reftex--pro-or-de name)))
         (newname (reftex-toc-newhead-from-alist newlevel name
                                                 reftex-section-levels-all)))
    (if (and name newname)
        (list data name newname toc-point)
      (goto-char toc-point)
      (error "Cannot %smote \\%s" reftex--pro-or-de name))))

(defun reftex-toc-promote-action (x)
  "Change the level of a TOC entry.
`reftex--pro-or-de' is assumed to be dynamically scoped into this function."
  (let* ((data (car x))
         (name (nth 1 x))
         (newname (nth 2 x))
         (marker (nth 4 data)))
    (with-current-buffer (marker-buffer marker)
      (goto-char (marker-position marker))
      (if (looking-at (concat "\\([ \t]*" reftex-section-pre-regexp "\\)" (regexp-quote name)))
          (replace-match (concat "\\1" newname))
        (error "Fatal error during %smotion" reftex--pro-or-de)))))

(defun reftex-toc-extract-section-number (entry)
  "Get the numbering of a TOC entry, for message purposes."
  (if (string-match "\\s-*\\(\\S-+\\)" (nth 2 (car entry)))
      (match-string 1 (nth 2 (car entry)))
    "?"))

(defun reftex-toc-newhead-from-alist (nlevel head alist)
  "Get new heading with level NLEVEL from ALIST.
If there are no such entries, return nil.
If there are several different entries with same new level, choose the
one with the smallest distance to the association of HEAD in the alist.
This makes it possible for promotion to work several sets of headings,
if these sets are sorted blocks in the alist."
  (let* ((al alist)
         (ass (assoc head al))
         (pos (length (memq ass al)))
         dist (mindist 1000) newhead)
    (while al
      (if (equal (cdar al) nlevel)
          (progn
            (setq dist (abs (- (length al) pos)))
            (if (< dist mindist)
                (setq newhead (car (car al))
                      mindist dist))))
      (setq al (cdr al)))
    newhead))

(defun reftex-toc-check-docstruct ()
  "Check if the current docstruct is fully up to date and all files visited."
  ;; We do this by checking if all sections are on the right position
  (let ((docstruct (symbol-value reftex-docstruct-symbol))
        entry marker empoint)
    (catch 'exit
      (while (setq entry (pop docstruct))
        (if (eq (car entry) 'toc)
            (progn
              (setq marker (nth 4 entry)
                    empoint (nth 8 entry))
              (if (not (and (markerp marker)
                            (marker-buffer marker)
                            (= (marker-position marker) empoint)))
                  (throw 'exit nil)))))
      t)))

(defun reftex-toc-load-all-files-for-promotion ()
  "Make sure all files of the document are being visited by buffers,
and that the scanning info is absolutely up to date.
We do this by rescanning with `reftex-keep-temporary-buffers' bound to t.
The variable `reftex--pro-or-de' is assumed to be dynamically scoped into this function.
When finished, we exit with an error message."
  (let ((reftex-keep-temporary-buffers t))
    (reftex-toc-Rescan)
    (reftex-toc-restore-region reftex--start-line reftex--mark-line)
    (throw 'exit
           "TOC had to be updated first.  Please check selection and repeat the command.")))

(defun reftex-toc-rename-label ()
  "Rename the currently selected label in the *toc* buffer.
This launches a global search and replace in order to rename a label.
Renaming a label is hardly ever necessary - the only exception is after
promotion/demotion in connection with a package like fancyref, where the
label prefix determines the wording of a reference."
  (interactive)
  (let* ((toc (get-text-property (point) :data))
         (label (car toc)) newlabel)
    (if (not (stringp label))
        (error "This is not a label entry"))
    (setq newlabel (read-string (format "Rename label \"%s\" to:" label)))
    (if (assoc newlabel (symbol-value reftex-docstruct-symbol))
        (if (not (y-or-n-p
                  (format-message "Label `%s' exists.  Use anyway? " label)))
            (error "Abort")))
    (save-excursion
      (save-window-excursion
        (reftex-toc-visit-location t)
        (condition-case nil
            (reftex-query-replace-document
             (concat "{" (regexp-quote label) "}")
             (format "{%s}" newlabel))
          (error t))))
    (reftex-toc-rescan)))


(defun reftex-toc-visit-location (&optional final no-revisit)
  ;; Visit the tex file corresponding to the TOC entry on the current line.
  ;; If FINAL is t, stay there
  ;; If FINAL is 'hide, hide the TOC window.
  ;; Otherwise, move cursor back into TOC window.
  ;; NO-REVISIT means don't visit files, just use live buffers.
  ;; This function is pretty clever about finding back a section heading,
  ;; even if the buffer is not live, or things like outline, x-symbol etc.
  ;; have been active.

  (let* ((toc (get-text-property (point) :data))
         (toc-window (selected-window))
         match)

    (unless toc (error "Don't know which TOC line to visit"))

    (cond

     ((eq (car toc) 'toc)
      ;; a toc entry
      (setq match (reftex-toc-find-section toc no-revisit)))

     ((eq (car toc) 'index)
      ;; an index entry
      (setq match (reftex-index-show-entry toc no-revisit)))

     ((memq (car toc) '(bof eof))
      ;; A file entry
      (setq match
            (let ((where (car toc))
                  (file (nth 1 toc)))
              (if (or (not no-revisit) (reftex-get-buffer-visiting file))
                  (progn
                    (switch-to-buffer-other-window
                     (reftex-get-file-buffer-force file nil))
                    (goto-char (if (eq where 'bof) (point-min) (point-max))))
                (message "%s" reftex-no-follow-message) nil))))

     ((stringp (car toc))
      ;; a label
      (setq match (reftex-show-label-location toc reftex-callback-fwd
                                                no-revisit t))))

    (unless match
      (select-window toc-window)
      (error "Cannot find location"))

    ;; Use the `final' parameter to decide what to do next.
    (cond
     ((eq final t)
      (with-selected-window toc-window
        (reftex-unhighlight 0)))
     ((eq final 'hide)
      (let ((window (selected-window))
            (buffer (window-buffer)))
        (unless (eq window toc-window) ;FIXME: Can this happen?
          (with-selected-window toc-window
            (reftex-unhighlight 0)
            (or (one-window-p) (delete-window))))
        ;; If window is still live, buffer is already visible
        ;; so let's not make it visible in yet-another-window.
        (unless (window-live-p window)
          ;; FIXME: How could window not be live?
          (pop-to-buffer-same-window buffer))
        (reftex-re-enlarge)))
     (t
      (unless (eq (selected-frame) (window-frame toc-window))
        ;; Make sure `toc-window' is not just selected but has focus.
        (select-frame-set-input-focus (window-frame toc-window)))
      (select-window toc-window)))))

(defun reftex-toc-find-section (toc &optional no-revisit)
  (let* ((file (nth 3 toc))
         (marker (nth 4 toc))
         (level (nth 5 toc))
         (literal (nth 7 toc))
         (emergency-point (nth 8 toc))
         (match
          (cond
           ((and (markerp marker) (marker-buffer marker))
            ;; Buffer is still live and we have the marker.  Should be easy.
            (switch-to-buffer-other-window (marker-buffer marker))
            (push-mark nil)
            (goto-char (marker-position marker))
            (or (looking-at (regexp-quote literal))
                (looking-at (reftex-make-regexp-allow-for-ctrl-m literal))
                (looking-at (reftex-make-desperate-section-regexp literal))
                (looking-at (concat "\\\\"
                                    (regexp-quote
                                     (car
                                      (rassq level
                                             reftex-section-levels-all)))
                                    "[[{]?"))))
           ((or (not no-revisit)
                (reftex-get-buffer-visiting file))
            ;; Marker is lost.  Use the backup method.
            (switch-to-buffer-other-window
             (reftex-get-file-buffer-force file nil))
            (goto-char (or emergency-point (point-min)))
            (or (looking-at (regexp-quote literal))
                (let ((len (length literal)))
                  (or (reftex-nearest-match (regexp-quote literal) len)
                      (reftex-nearest-match
                       (reftex-make-regexp-allow-for-ctrl-m literal) len)
                      (reftex-nearest-match
                       (reftex-make-desperate-section-regexp literal) len)))))
           (t (message "%s" reftex-no-follow-message) nil))))
    (when match
      (goto-char (match-beginning 0))
      (if (not (= (point) (point-max))) (recenter 1))
      (reftex-highlight 0 (match-beginning 0) (match-end 0) (current-buffer)))
    match))

(defun reftex-make-desperate-section-regexp (old)
  ;; Return a regexp which will still match a section statement even if
  ;; x-symbol or isotex or the like have been at work in the mean time.
  (let* ((n (1+ (string-match "[[{]" old)))
         (new (regexp-quote (substring old 0 (1+ (string-match "[[{]" old)))))
         (old (substring old n)))
    (while (string-match
            "\\([\r\n]\\)\\|\\(\\`\\|[ \t\n\r]\\)\\([a-zA-Z0-9]+\\)\\([ \t\n\r]\\|}\\'\\)"
            old)
      (if (match-beginning 1)
          (setq new (concat new "[^\n\r]*[\n\r]"))
        (setq new (concat new "[^\n\r]*" (match-string 3 old))))
      (setq old (substring old (match-end 0))))
    new))

;; Auto recentering of TOC window

(defun reftex-recenter-toc-when-idle ()
  (and (> (buffer-size) 5)
       reftex-mode
       (not (active-minibuffer-window))
       (fboundp 'reftex-toc-mode)
       (get-buffer-window "*toc*" 'visible)
       (string= reftex-last-toc-master (reftex-TeX-master-file))
       (let (current-prefix-arg)
         (reftex-toc-recenter))))

;;;###autoload
(defun reftex-toggle-auto-toc-recenter ()
  "Toggle the automatic recentering of the TOC window.
When active, leaving point idle will make the TOC window jump to the correct
section."
  (interactive)
  (if reftex-toc-auto-recenter-timer
      (progn
        (if (featurep 'xemacs)
            (delete-itimer reftex-toc-auto-recenter-timer)
          (cancel-timer reftex-toc-auto-recenter-timer))
        (setq reftex-toc-auto-recenter-timer nil)
        (message "Automatic recentering of TOC window was turned off"))
    (setq reftex-toc-auto-recenter-timer
          (if (featurep 'xemacs)
              (start-itimer "RefTeX Idle Timer for recenter"
                            'reftex-recenter-toc-when-idle
                            reftex-idle-time reftex-idle-time t)
            (run-with-idle-timer
             reftex-idle-time t 'reftex-recenter-toc-when-idle)))
    (message "Automatic recentering of TOC window was turned on")))

(defun reftex-toc-toggle-dedicated-frame ()
  "Toggle the display of a separate frame for the TOC.
This frame is not used by the `reftex-toc' commands, but it is useful to
always show the current section in connection with the option
`reftex-auto-recenter-toc'."
  (interactive)
  (catch 'exit
    (let* ((frames (frame-list)) frame
           (get-frame-prop-func (if (fboundp 'frame-property)
                                    'frame-property
                                  'frame-parameter)))
      (while (setq frame (pop frames))
        (if (equal (funcall get-frame-prop-func frame 'name)
                   "RefTeX TOC Frame")
            (progn
              (delete-frame frame)
              (throw 'exit nil))))
      (reftex-make-separate-toc-frame))))

(defun reftex-make-separate-toc-frame ()
  ;; Create a new fame showing only the TOC buffer.
  (let ((current-frame (selected-frame))
        (current-window (selected-window))
        (current-toc-window (get-buffer-window "*toc*" 'visible))
        current-toc-frame)
    (if (and current-toc-window
             (not (equal (selected-frame) (window-frame current-toc-window))))
        nil
      (setq current-toc-frame
            (make-frame
             '((name . "RefTeX TOC Frame")
               (height . 20) (width . 60)
               (unsplittable . t)
               (minibuffer . nil)
               (default-toolbar-visible-p . nil)
               (menubar-visible-p . nil)
               (horizontal-scrollbar-visible-p . nil))))
      (select-frame current-toc-frame)
      (switch-to-buffer "*toc*")
      (select-frame current-frame)
      (cond ((fboundp 'x-focus-frame)
             (x-focus-frame current-frame))
            ((and (featurep 'xemacs) ; `focus-frame' is a nop in Emacs.
                  (fboundp 'focus-frame))
             (focus-frame current-frame)))
      (select-window current-window)
      (when (eq reftex-auto-recenter-toc 'frame)
        (unless reftex-toc-auto-recenter-timer
          (reftex-toggle-auto-toc-recenter))
        (add-hook 'delete-frame-hook 'reftex-toc-delete-frame-hook)))))

(defun reftex-toc-delete-frame-hook (frame)
  (if (and reftex-toc-auto-recenter-timer
           (reftex-toc-dframe-p frame))
      (progn
      (reftex-toggle-auto-toc-recenter))))

;;; reftex-toc.el ends here

;; Local Variables:
;; generated-autoload-file: "reftex-loaddefs.el"
;; End:
