;;; mouse.el --- window system-independent mouse support  -*- lexical-binding: t -*-

;; Copyright (C) 1993-1995, 1999-2020 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: hardware, mouse
;; Package: emacs

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

;; This package provides various useful commands (including help
;; system access) through the mouse.  All this code assumes that mouse
;; interpretation has been abstracted into Emacs input events.

;;; Code:

(eval-when-compile (require 'rect))

;;; Utility functions.

;; Indent track-mouse like progn.
(put 'track-mouse 'lisp-indent-function 0)

(defgroup mouse nil
  "Input from the mouse."  ;; "Mouse support."
  :group 'environment
  :group 'editing)

(defcustom mouse-yank-at-point nil
  "If non-nil, mouse yank commands yank at point instead of at click."
  :type 'boolean)

(defcustom mouse-drag-copy-region nil
  "If non-nil, copy to kill-ring upon mouse adjustments of the region.

This affects `mouse-save-then-kill' (\\[mouse-save-then-kill]) in
addition to mouse drags."
  :type 'boolean
  :version "24.1")

(defcustom mouse-1-click-follows-link 450
  "Non-nil means that clicking Mouse-1 on a link follows the link.

With the default setting, an ordinary Mouse-1 click on a link
performs the same action as Mouse-2 on that link, while a longer
Mouse-1 click (hold down the Mouse-1 button for more than 450
milliseconds) performs the original Mouse-1 binding (which
typically sets point where you click the mouse).

If value is an integer, the time elapsed between pressing and
releasing the mouse button determines whether to follow the link
or perform the normal Mouse-1 action (typically set point).
The absolute numeric value specifies the maximum duration of a
\"short click\" in milliseconds.  A positive value means that a
short click follows the link, and a longer click performs the
normal action.  A negative value gives the opposite behavior.

If value is `double', a double click follows the link.

Otherwise, a single Mouse-1 click unconditionally follows the link.

Note that dragging the mouse never follows the link.

This feature only works in modes that specifically identify
clickable text as links, so it may not work with some external
packages.  See `mouse-on-link-p' for details."
  :version "22.1"
  :type '(choice (const :tag "Disabled" nil)
		 (const :tag "Double click" double)
                 (number :tag "Single click time limit" :value 450)
                 (other :tag "Single click" t)))

(defcustom mouse-1-click-in-non-selected-windows t
  "If non-nil, a Mouse-1 click also follows links in non-selected windows.

If nil, a Mouse-1 click on a link in a non-selected window performs
the normal mouse-1 binding, typically selects the window and sets
point at the click position."
  :type 'boolean
  :version "22.1")

(defvar mouse--last-down nil)

(defun mouse--down-1-maybe-follows-link (&optional _prompt)
  (when mouse-1-click-follows-link
    (setq mouse--last-down (cons (car-safe last-input-event) (current-time))))
  nil)

(defun mouse--click-1-maybe-follows-link (&optional _prompt)
  "Turn `mouse-1' events into `mouse-2' events if follows-link.
Expects to be bound to `(double-)mouse-1' in `key-translation-map'."
  (and mouse--last-down
       (pcase mouse-1-click-follows-link
         ('nil nil)
         ('double (eq 'double-mouse-1 (car-safe last-input-event)))
         (_ (and (eq 'mouse-1 (car-safe last-input-event))
                 (or (not (numberp mouse-1-click-follows-link))
		     (funcall (if (< mouse-1-click-follows-link 0)
				  (lambda (a b) (time-less-p b a))
				#'time-less-p)
			      (time-since (cdr mouse--last-down))
                              (/ (abs mouse-1-click-follows-link) 1000.0))))))
       (eq (car mouse--last-down)
           (event-convert-list (list 'down (car-safe last-input-event))))
       (let* ((action (mouse-on-link-p (event-start last-input-event))))
         (when (and action
                    (or mouse-1-click-in-non-selected-windows
                        (eq (selected-window)
                            (posn-window (event-start last-input-event)))))
           ;; Turn the mouse-1 into a mouse-2 to follow links,
           ;; but only if ‘mouse-on-link-p’ hasn’t returned a
           ;; string or vector (see its docstring).
           (if (arrayp action)
               (vector (aref action 0))
             (let ((newup (if (eq mouse-1-click-follows-link 'double)
                              'double-mouse-2 'mouse-2)))
               ;; If mouse-2 has never been done by the user, it
               ;; doesn't have the necessary property to be
               ;; interpreted correctly.
               (unless (get newup 'event-kind)
                 (put newup 'event-kind
                      (get (car last-input-event) 'event-kind)))
               ;; Modify the event in-place, otherwise we can get a prefix
               ;; added again, so a click on the header-line turns
               ;; into a [header-line header-line mouse-2] :-(.
               ;; See fake_prefixed_keys in src/keyboard.c's.
               (setf (car last-input-event) newup)
               (vector last-input-event)))))))

(define-key key-translation-map [down-mouse-1]
  #'mouse--down-1-maybe-follows-link)
(define-key key-translation-map [double-down-mouse-1]
  #'mouse--down-1-maybe-follows-link)
(define-key key-translation-map [mouse-1]
  #'mouse--click-1-maybe-follows-link)
(define-key key-translation-map [double-mouse-1]
  #'mouse--click-1-maybe-follows-link)


;; Provide a mode-specific menu on a mouse button.

(defun minor-mode-menu-from-indicator (indicator)
  "Show menu for minor mode specified by INDICATOR.
Interactively, INDICATOR is read using completion.
If there is no menu defined for the minor mode, then create one with
items `Turn Off' and `Help'."
  (interactive
   (list (completing-read
	  "Minor mode indicator: "
	  (describe-minor-mode-completion-table-for-indicator))))
  (let* ((minor-mode (lookup-minor-mode-from-indicator indicator))
         (mm-fun (or (get minor-mode :minor-mode-function) minor-mode)))
    (unless minor-mode (error "Cannot find minor mode for `%s'" indicator))
    (let* ((map (cdr-safe (assq minor-mode minor-mode-map-alist)))
           (menu (and (keymapp map) (lookup-key map [menu-bar]))))
      (setq menu
            (if menu
                (mouse-menu-non-singleton menu)
              (if (fboundp mm-fun)      ; bug#20201
                  `(keymap
                    ,(format "%s - %s" indicator
			     (capitalize
			      (replace-regexp-in-string
			       "-" " " (format "%S" minor-mode))))
                    (turn-off menu-item "Turn off minor mode" ,mm-fun)
                    (help menu-item "Help for minor mode"
                          (lambda () (interactive)
                            (describe-function ',mm-fun)))))))
      (if menu
          (popup-menu menu)
        (message "No menu available")))))

(defun mouse-minor-mode-menu (event)
  "Show minor-mode menu for EVENT on minor modes area of the mode line."
  (interactive "@e")
  (let ((indicator (car (nth 4 (car (cdr event))))))
    (minor-mode-menu-from-indicator indicator)))

(defun mouse-menu-major-mode-map ()
  (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
  (let* (;; Keymap from which to inherit; may be null.
	 (ancestor (mouse-menu-non-singleton
		    (and (current-local-map)
			 (local-key-binding [menu-bar]))))
	 ;; Make a keymap in which our last command leads to a menu or
	 ;; default to the edit menu.
	 (newmap (if ancestor
		     (make-sparse-keymap (concat (format-mode-line mode-name)
                                                 " Mode"))
		   menu-bar-edit-menu)))
    (if ancestor
	(set-keymap-parent newmap ancestor))
    newmap))

(defun mouse-menu-non-singleton (menubar)
  "Return menu keybar MENUBAR, or a lone submenu inside it.
If MENUBAR defines exactly one submenu, return just that submenu.
Otherwise, return MENUBAR."
  (if menubar
      (let (submap)
        (map-keymap
         (lambda (k v) (setq submap (if submap t (cons k v))))
         (keymap-canonicalize menubar))
        (if (eq submap t)
            menubar
          (lookup-key menubar (vector (car submap)))))))

(defun mouse-menu-bar-map ()
  "Return a keymap equivalent to the menu bar.
The contents are the items that would be in the menu bar whether or
not it is actually displayed."
  (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
  (let* ((local-menu (and (current-local-map)
			  (lookup-key (current-local-map) [menu-bar])))
	 (global-menu (lookup-key global-map [menu-bar]))
	 ;; If a keymap doesn't have a prompt string (a lazy
	 ;; programmer didn't bother to provide one), create it and
	 ;; insert it into the keymap; each keymap gets its own
	 ;; prompt.  This is required for non-toolkit versions to
	 ;; display non-empty menu pane names.
	 (minor-mode-menus
	  (mapcar
           (lambda (menu)
             (let* ((minor-mode (car menu))
                    (menu (cdr menu))
                    (title-or-map (cadr menu)))
               (or (stringp title-or-map)
                   (setq menu
                         (cons 'keymap
                               (cons (concat
                                      (capitalize (subst-char-in-string
                                                   ?- ?\s (symbol-name
                                                           minor-mode)))
                                      " Menu")
                                     (cdr menu)))))
               menu))
	   (minor-mode-key-binding [menu-bar])))
	 (local-title-or-map (and local-menu (cadr local-menu)))
	 (global-title-or-map (cadr global-menu)))
    (or (null local-menu)
	(stringp local-title-or-map)
	(setq local-menu (cons 'keymap
			       (cons (concat (format-mode-line mode-name)
                                             " Mode Menu")
				     (cdr local-menu)))))
    (or (stringp global-title-or-map)
	(setq global-menu (cons 'keymap
			        (cons "Global Menu"
				      (cdr global-menu)))))
    ;; Supplying the list is faster than making a new map.
    ;; FIXME: We have a problem here: we have to use the global/local/minor
    ;; so they're displayed in the expected order, but later on in the command
    ;; loop, they're actually looked up in the opposite order.
    (apply 'append
           global-menu
           local-menu
           minor-mode-menus)))

(defun mouse-major-mode-menu (event &optional prefix)
  "Pop up a mode-specific menu of mouse commands.
Default to the Edit menu if the major mode doesn't define a menu."
  (declare (obsolete mouse-menu-major-mode-map "23.1"))
  (interactive "@e\nP")
  (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
  (popup-menu (mouse-menu-major-mode-map) event prefix))

(defun mouse-popup-menubar (event prefix)
  "Pop up a menu equivalent to the menu bar for keyboard EVENT with PREFIX.
The contents are the items that would be in the menu bar whether or
not it is actually displayed."
  (declare (obsolete mouse-menu-bar-map "23.1"))
  (interactive "@e \nP")
  (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
  (popup-menu (mouse-menu-bar-map) (unless (integerp event) event) prefix))

(defun mouse-popup-menubar-stuff (event prefix)
  "Popup a menu like either `mouse-major-mode-menu' or `mouse-popup-menubar'.
Use the former if the menu bar is showing, otherwise the latter."
  (declare (obsolete nil "23.1"))
  (interactive "@e\nP")
  (run-hooks 'activate-menubar-hook 'menu-bar-update-hook)
  (popup-menu
   (if (zerop (or (frame-parameter nil 'menu-bar-lines) 0))
       (mouse-menu-bar-map)
     (mouse-menu-major-mode-map))
   event prefix))

;; Commands that operate on windows.

(defun mouse-minibuffer-check (event)
  (let ((w (posn-window (event-start event))))
    (and (window-minibuffer-p w)
	 (not (minibuffer-window-active-p w))
	 (user-error "Minibuffer window is not active")))
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook))

(defun mouse-delete-window (click)
  "Delete the window you click on.
Do nothing if the frame has just one window.
This command must be bound to a mouse click."
  (interactive "e")
  (unless (one-window-p t)
    (mouse-minibuffer-check click)
    (delete-window (posn-window (event-start click)))))

(defun mouse-select-window (click)
  "Select the window clicked on; don't move point."
  (interactive "e")
  (mouse-minibuffer-check click)
  (let ((oframe (selected-frame))
	(frame (window-frame (posn-window (event-start click)))))
    (select-window (posn-window (event-start click)))
    (raise-frame frame)
    (select-frame frame)
    (or (eq frame oframe)
	(set-mouse-position (selected-frame) (1- (frame-width)) 0))))

(define-obsolete-function-alias 'mouse-tear-off-window 'tear-off-window "24.4")
(defun tear-off-window (click)
  "Delete the selected window, and create a new frame displaying its buffer."
  (interactive (list last-nonmenu-event))
  (mouse-minibuffer-check click)
  (let* ((window (posn-window (event-start click)))
	 (buf (window-buffer window))
	 (frame (make-frame)))          ;FIXME: Use pop-to-buffer.
    (select-frame frame)
    (switch-to-buffer buf)
    (delete-window window)))

(defun mouse-delete-other-windows ()
  "Delete all windows except the one you click on."
  (interactive "@")
  (delete-other-windows))

(defun mouse-split-window-vertically (click)
  "Select Emacs window mouse is on, then split it vertically in half.
The window is split at the line clicked on.
This command must be bound to a mouse click."
  (interactive "@e")
  (mouse-minibuffer-check click)
  (let ((start (event-start click)))
    (select-window (posn-window start))
    (let ((new-height (1+ (cdr (posn-col-row (event-end click)))))
	  (first-line window-min-height)
	  (last-line (- (window-height) window-min-height)))
      (if (< last-line first-line)
	  (user-error "Window too short to split")
        ;; Bind `window-combination-resize' to nil so we are sure to get
        ;; the split right at the line clicked on.
        (let (window-combination-resize)
          (split-window-vertically
           (min (max new-height first-line) last-line)))))))

(defun mouse-split-window-horizontally (click)
  "Select Emacs window mouse is on, then split it horizontally in half.
The window is split at the column clicked on.
This command must be bound to a mouse click."
  (interactive "@e")
  (mouse-minibuffer-check click)
  (let ((start (event-start click)))
    (select-window (posn-window start))
    (let ((new-width (1+ (car (posn-col-row (event-end click)))))
	  (first-col window-min-width)
	  (last-col (- (window-width) window-min-width)))
      (if (< last-col first-col)
	  (user-error "Window too narrow to split")
        ;; Bind `window-combination-resize' to nil so we are sure to get
        ;; the split right at the column clicked on.
	(let (window-combination-resize)
          (split-window-horizontally
           (min (max new-width first-col) last-col)))))))

(defun mouse-drag-line (start-event line)
  "Drag a mode line, header line, or vertical line with the mouse.
START-EVENT is the starting mouse event of the drag action.  LINE
must be one of the symbols `header', `mode', or `vertical'."
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (let* ((echo-keystrokes 0)
	 (start (event-start start-event))
	 (window (posn-window start))
	 (frame (window-frame window))
	 ;; `position' records the x- or y-coordinate of the last
	 ;; sampled position.
	 (position (if (eq line 'vertical)
		       (+ (window-pixel-left window)
			  (car (posn-x-y start)))
		     (+ (window-pixel-top window)
			(cdr (posn-x-y start)))))
	 ;; `last-position' records the x- or y-coordinate of the
	 ;; previously sampled position.  The difference of `position'
	 ;; and `last-position' determines the size change of WINDOW.
	 (last-position position)
	 (draggable t)
	 posn-window growth dragged)
    ;; Decide on whether we are allowed to track at all and whose
    ;; window's edge we drag.
    (cond
     ((eq line 'header)
      ;; Drag bottom edge of window above the header line.
      (setq window (window-in-direction 'above window t)))
     ((eq line 'mode))
     ((eq line 'vertical)
      (let ((divider-width (frame-right-divider-width frame)))
        (when (and (or (not (numberp divider-width))
                       (zerop divider-width))
                   (eq (frame-parameter frame 'vertical-scroll-bars) 'left))
          (setq window (window-in-direction 'left window t))))))
    (let* ((exitfun nil)
           (move
	    (lambda (event) (interactive "e")
	      (cond
	       ((not (consp event))
		nil)
	       ((eq line 'vertical)
		;; Drag right edge of `window'.
		(setq start (event-start event))
		(setq position (car (posn-x-y start)))
		;; Set `posn-window' to the window where `event' was recorded.
		;; This can be `window' or the window on the left or right of
		;; `window'.
		(when (window-live-p (setq posn-window (posn-window start)))
		  ;; Add left edge of `posn-window' to `position'.
		  (setq position (+ (window-pixel-left posn-window) position))
		  (unless (nth 1 start)
		    ;; Add width of objects on the left of the text area to
		    ;; `position'.
		    (when (eq (window-current-scroll-bars posn-window) 'left)
		      (setq position (+ (window-scroll-bar-width posn-window)
					position)))
		    (setq position (+ (car (window-fringes posn-window))
				      (or (car (window-margins posn-window)) 0)
				      position))))
		;; When the cursor overshoots after shrinking a window to its
		;; minimum size and the dragging direction changes, have the
		;; cursor first catch up with the window edge.
		(unless (or (zerop (setq growth (- position last-position)))
			    (and (> growth 0)
				 (< position (+ (window-pixel-left window)
						(window-pixel-width window))))
			    (and (< growth 0)
				 (> position (+ (window-pixel-left window)
						(window-pixel-width window)))))
		  (setq dragged t)
		  (adjust-window-trailing-edge window growth t t))
		(setq last-position position))
	       (draggable
		;; Drag bottom edge of `window'.
		(setq start (event-start event))
		;; Set `posn-window' to the window where `event' was recorded.
		;; This can be either `window' or the window above or below of
		;; `window'.
		(setq posn-window (posn-window start))
		(setq position (cdr (posn-x-y start)))
		(when (window-live-p posn-window)
		  ;; Add top edge of `posn-window' to `position'.
		  (setq position (+ (window-pixel-top posn-window) position))
		  ;; If necessary, add height of header line to `position'
		  (when (memq (posn-area start)
			      '(nil left-fringe right-fringe left-margin right-margin))
		    (setq position (+ (window-header-line-height posn-window) position))))
		;; When the cursor overshoots after shrinking a window to its
		;; minimum size and the dragging direction changes, have the
		;; cursor first catch up with the window edge.
		(unless (or (zerop (setq growth (- position last-position)))
			    (and (> growth 0)
				 (< position (+ (window-pixel-top window)
						(window-pixel-height window))))
			    (and (< growth 0)
				 (> position (+ (window-pixel-top window)
						(window-pixel-height window)))))
		  (setq dragged t)
		  (adjust-window-trailing-edge window growth nil t))
		(setq last-position position)))))
           (old-track-mouse track-mouse))
      ;; Start tracking.  The special value 'dragging' signals the
      ;; display engine to freeze the mouse pointer shape for as long
      ;; as we drag.
      (setq track-mouse 'dragging)
      ;; Loop reading events and sampling the position of the mouse.
      (setq exitfun
	    (set-transient-map
	     (let ((map (make-sparse-keymap)))
	       (define-key map [switch-frame] #'ignore)
	       (define-key map [select-window] #'ignore)
	       (define-key map [scroll-bar-movement] #'ignore)
	       (define-key map [mouse-movement] move)
	       ;; Swallow drag-mouse-1 events to avoid selecting some other window.
	       (define-key map [drag-mouse-1]
		 (lambda () (interactive) (funcall exitfun)))
	       ;; For vertical line dragging swallow also a mouse-1
	       ;; event (but only if we dragged at least once to allow mouse-1
	       ;; clicks to get through).
	       (when (eq line 'vertical)
		 (define-key map [mouse-1]
		   `(menu-item "" ,(lambda () (interactive) (funcall exitfun))
			       :filter ,(lambda (cmd) (if dragged cmd)))))
	       ;; Some of the events will of course end up looked up
	       ;; with a mode-line, header-line or vertical-line prefix ...
	       (define-key map [mode-line] map)
	       (define-key map [header-line] map)
	       (define-key map [vertical-line] map)
	       ;; ... and some maybe even with a right- or bottom-divider
	       ;; prefix.
	       (define-key map [right-divider] map)
	       (define-key map [bottom-divider] map)
	       map)
	     t (lambda () (setq track-mouse old-track-mouse)))))))

(defun mouse-drag-mode-line (start-event)
  "Change the height of a window by dragging on its mode line.
START-EVENT is the starting mouse event of the drag action.

If the drag happens in a mode line on the bottom of a frame and
that frame's `drag-with-mode-line' parameter is non-nil, drag the
frame instead."
  (interactive "e")
  (let* ((start (event-start start-event))
	 (window (posn-window start))
         (frame (window-frame window)))
    (cond
     ((not (window-live-p window)))
     ((or (not (window-at-side-p window 'bottom))
          ;; Allow resizing the minibuffer window if it's on the
          ;; same frame as and immediately below `window', and it's
          ;; either active or `resize-mini-windows' is nil.
          (let ((minibuffer-window (minibuffer-window frame)))
            (and (eq (window-frame minibuffer-window) frame)
                 (or (not resize-mini-windows)
                     (eq minibuffer-window
                         (active-minibuffer-window))))))
      (mouse-drag-line start-event 'mode))
     ((and (frame-parameter frame 'drag-with-mode-line)
           (window-at-side-p window 'bottom)
           (let ((minibuffer-window (minibuffer-window frame)))
             (not (eq (window-frame minibuffer-window) frame))))
      ;; Drag frame when the window is on the bottom of its frame and
      ;; there is no minibuffer window below.
      (mouse-drag-frame start-event 'move)))))

(defun mouse-drag-header-line (start-event)
  "Change the height of a window by dragging on its header line.
START-EVENT is the starting mouse event of the drag action.

If the drag happens in a header line on the top of a frame and
that frame's `drag-with-header-line' parameter is non-nil, drag
the frame instead."
  (interactive "e")
  (let* ((start (event-start start-event))
	 (window (posn-window start)))
    (if (and (window-live-p window)
             (not (window-at-side-p window 'top)))
        (mouse-drag-line start-event 'header)
      (let ((frame (window-frame window)))
        (when (frame-parameter frame 'drag-with-header-line)
          (mouse-drag-frame start-event 'move))))))

(defun mouse-drag-vertical-line (start-event)
  "Change the width of a window by dragging on a vertical line.
START-EVENT is the starting mouse event of the drag action."
  (interactive "e")
  (mouse-drag-line start-event 'vertical))

(defun mouse-resize-frame (frame x-diff y-diff &optional x-move y-move)
  "Helper function for `mouse-drag-frame'."
  (let* ((frame-x-y (frame-position frame))
         (frame-x (car frame-x-y))
         (frame-y (cdr frame-x-y))
         alist)
    (if (> x-diff 0)
        (when x-move
          (setq x-diff (min x-diff frame-x))
          (setq x-move (- frame-x x-diff)))
      (let* ((min-width (frame-windows-min-size frame t nil t))
             (min-diff (max 0 (- (frame-inner-width frame) min-width))))
        (setq x-diff (max x-diff (- min-diff)))
        (when x-move
          (setq x-move (+ frame-x (- x-diff))))))

    (if (> y-diff 0)
        (when y-move
          (setq y-diff (min y-diff frame-y))
          (setq y-move (- frame-y y-diff)))
      (let* ((min-height (frame-windows-min-size frame nil nil t))
             (min-diff (max 0 (- (frame-inner-height frame) min-height))))
        (setq y-diff (max y-diff (- min-diff)))
        (when y-move
          (setq y-move (+ frame-y (- y-diff))))))

    (unless (zerop x-diff)
      (when x-move
        (push `(left . ,x-move) alist))
      (push `(width . (text-pixels . ,(+ (frame-text-width frame) x-diff)))
            alist))
    (unless (zerop y-diff)
      (when y-move
        (push `(top . ,y-move) alist))
      (push `(height . (text-pixels . ,(+ (frame-text-height frame) y-diff)))
            alist))
    (when alist
      (modify-frame-parameters frame alist))))

(defun mouse-drag-frame (start-event part)
  "Drag a frame or one of its edges with the mouse.
START-EVENT is the starting mouse event of the drag action.  Its
position window denotes the frame that will be dragged.

PART specifies the part that has been dragged and must be one of
the symbols `left', `top', `right', `bottom', `top-left',
`top-right', `bottom-left', `bottom-right' to drag an internal
border or edge.  If PART equals `move', this means to move the
frame with the mouse."
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (let* ((echo-keystrokes 0)
	 (start (event-start start-event))
         (window (posn-window start))
         ;; FRAME is the frame to drag.
         (frame (if (window-live-p window)
                    (window-frame window)
                  window))
         (width (frame-native-width frame))
         (height (frame-native-height frame))
         ;; PARENT is the parent frame of FRAME or, if FRAME is a
         ;; top-level frame, FRAME's workarea.
         (parent (frame-parent frame))
         (parent-edges
          (if parent
              `(0 0 ,(frame-native-width parent) ,(frame-native-height parent))
            (let* ((attributes
                    (car (display-monitor-attributes-list)))
                   (workarea (assq 'workarea attributes)))
              (and workarea
                   `(,(nth 1 workarea) ,(nth 2 workarea)
                     ,(+ (nth 1 workarea) (nth 3 workarea))
                     ,(+ (nth 2 workarea) (nth 4 workarea)))))))
         (parent-left (and parent-edges (nth 0 parent-edges)))
         (parent-top (and parent-edges (nth 1 parent-edges)))
         (parent-right (and parent-edges (nth 2 parent-edges)))
         (parent-bottom (and parent-edges (nth 3 parent-edges)))
         ;; `pos-x' and `pos-y' record the x- and y-coordinates of the
	 ;; last sampled mouse position.  Note that we sample absolute
	 ;; mouse positions to avoid that moving the mouse from one
	 ;; frame into another gets into our way.  `last-x' and `last-y'
	 ;; records the x- and y-coordinates of the previously sampled
	 ;; position.  The differences between `last-x' and `pos-x' as
	 ;; well as `last-y' and `pos-y' determine the amount the mouse
	 ;; has been dragged between the last two samples.
         pos-x-y pos-x pos-y
         (last-x-y (mouse-absolute-pixel-position))
         (last-x (car last-x-y))
         (last-y (cdr last-x-y))
         ;; `snap-x' and `snap-y' record the x- and y-coordinates of the
         ;; mouse position when FRAME snapped.  As soon as the
         ;; difference between `pos-x' and `snap-x' (or `pos-y' and
         ;; `snap-y') exceeds the value of FRAME's `snap-width'
         ;; parameter, unsnap FRAME (at the respective side).  `snap-x'
         ;; and `snap-y' nil mean FRAME is currently not snapped.
         snap-x snap-y
         (exitfun nil)
         (move
          (lambda (event)
            (interactive "e")
            (when (consp event)
              (setq pos-x-y (mouse-absolute-pixel-position))
              (setq pos-x (car pos-x-y))
              (setq pos-y (cdr pos-x-y))
              (cond
               ((eq part 'left)
                (mouse-resize-frame frame (- last-x pos-x) 0 t))
               ((eq part 'top)
                (mouse-resize-frame frame 0 (- last-y pos-y) nil t))
               ((eq part 'right)
                (mouse-resize-frame frame (- pos-x last-x) 0))
               ((eq part 'bottom)
                (mouse-resize-frame frame 0 (- pos-y last-y)))
               ((eq part 'top-left)
                (mouse-resize-frame
                 frame (- last-x pos-x) (- last-y pos-y) t t))
               ((eq part 'top-right)
                (mouse-resize-frame
                 frame (- pos-x last-x) (- last-y pos-y) nil t))
               ((eq part 'bottom-left)
                (mouse-resize-frame
                 frame (- last-x pos-x) (- pos-y last-y) t))
               ((eq part 'bottom-right)
                (mouse-resize-frame
                 frame (- pos-x last-x) (- pos-y last-y)))
               ((eq part 'move)
                (let* ((old-position (frame-position frame))
                       (old-left (car old-position))
                       (old-top (cdr old-position))
                       (left (+ old-left (- pos-x last-x)))
                       (top (+ old-top (- pos-y last-y)))
                       right bottom
                       ;; `snap-width' (maybe also a yet to be provided
                       ;; `snap-height') could become floats to handle
                       ;; proportionality wrt PARENT.  We don't do any
                       ;; checks on this parameter so far.
                       (snap-width (frame-parameter frame 'snap-width)))
                  ;; Docking and constraining.
                  (when (and (numberp snap-width) parent-edges)
                    (cond
                     ;; Docking at the left parent edge.
                     ((< pos-x last-x)
                      (cond
                       ((and (> left parent-left)
                             (<= (- left parent-left) snap-width))
                        ;; Snap when the mouse moved leftward and
                        ;; FRAME's left edge would end up within
                        ;; `snap-width' pixels from PARENT's left edge.
                        (setq snap-x pos-x)
                        (setq left parent-left))
                       ((and (<= left parent-left)
                             (<= (- parent-left left) snap-width)
                             snap-x (<= (- snap-x pos-x) snap-width))
                        ;; Stay snapped when the mouse moved leftward
                        ;; but not more than `snap-width' pixels from
                        ;; the time FRAME snapped.
                        (setq left parent-left))
                       (t
                        ;; Unsnap when the mouse moved more than
                        ;; `snap-width' pixels leftward from the time
                        ;; FRAME snapped.
                        (setq snap-x nil))))
                     ((> pos-x last-x)
                      (setq right (+ left width))
                      (cond
                       ((and (< right parent-right)
                             (<= (- parent-right right) snap-width))
                        ;; Snap when the mouse moved rightward and
                        ;; FRAME's right edge would end up within
                        ;; `snap-width' pixels from PARENT's right edge.
                        (setq snap-x pos-x)
                        (setq left (- parent-right width)))
                       ((and (>= right parent-right)
                             (<= (- right parent-right) snap-width)
                             snap-x (<= (- pos-x snap-x) snap-width))
                        ;; Stay snapped when the mouse moved rightward
                        ;; but not more more than `snap-width' pixels
                        ;; from the time FRAME snapped.
                        (setq left (- parent-right width)))
                       (t
                        ;; Unsnap when the mouse moved rightward more
                        ;; than `snap-width' pixels from the time FRAME
                        ;; snapped.
                        (setq snap-x nil)))))

                    (cond
                     ((< pos-y last-y)
                      (cond
                       ((and (> top parent-top)
                             (<= (- top parent-top) snap-width))
                        ;; Snap when the mouse moved upward and FRAME's
                        ;; top edge would end up within `snap-width'
                        ;; pixels from PARENT's top edge.
                        (setq snap-y pos-y)
                        (setq top parent-top))
                       ((and (<= top parent-top)
                             (<= (- parent-top top) snap-width)
                             snap-y (<= (- snap-y pos-y) snap-width))
                        ;; Stay snapped when the mouse moved upward but
                        ;; not more more than `snap-width' pixels from
                        ;; the time FRAME snapped.
                        (setq top parent-top))
                       (t
                        ;; Unsnap when the mouse moved upward more than
                        ;; `snap-width' pixels from the time FRAME
                        ;; snapped.
                        (setq snap-y nil))))
                     ((> pos-y last-y)
                      (setq bottom (+ top height))
                      (cond
                       ((and (< bottom parent-bottom)
                             (<= (- parent-bottom bottom) snap-width))
                        ;; Snap when the mouse moved downward and
                        ;; FRAME's bottom edge would end up within
                        ;; `snap-width' pixels from PARENT's bottom
                        ;; edge.
                        (setq snap-y pos-y)
                        (setq top (- parent-bottom height)))
                       ((and (>= bottom parent-bottom)
                             (<= (- bottom parent-bottom) snap-width)
                             snap-y (<= (- pos-y snap-y) snap-width))
                        ;; Stay snapped when the mouse moved downward
                        ;; but not more more than `snap-width' pixels
                        ;; from the time FRAME snapped.
                        (setq top (- parent-bottom height)))
                       (t
                        ;; Unsnap when the mouse moved downward more
                        ;; than `snap-width' pixels from the time FRAME
                        ;; snapped.
                        (setq snap-y nil))))))

                  ;; If requested, constrain FRAME's draggable areas to
                  ;; PARENT's edges.  The `top-visible' parameter should
                  ;; be set when FRAME has a draggable header-line.  If
                  ;; set to a number, it ascertains that the top of
                  ;; FRAME is always constrained to the top of PARENT
                  ;; and that at least as many pixels of FRAME as
                  ;; specified by that number are visible on each of the
                  ;; three remaining sides of PARENT.
                  ;;
                  ;; The `bottom-visible' parameter should be set when
                  ;; FRAME has a draggable mode-line.  If set to a
                  ;; number, it ascertains that the bottom of FRAME is
                  ;; always constrained to the bottom of PARENT and that
                  ;; at least as many pixels of FRAME as specified by
                  ;; that number are visible on each of the three
                  ;; remaining sides of PARENT.
                  (let ((par (frame-parameter frame 'top-visible))
                        bottom-visible)
                    (unless par
                      (setq par (frame-parameter frame 'bottom-visible))
                      (setq bottom-visible t))
                    (when (and (numberp par) parent-edges)
                      (setq left
                            (max (min (- parent-right par) left)
                                 (+ (- parent-left width) par)))
                      (setq top
                            (if bottom-visible
                                (min (max top (- parent-top (- height par)))
                                     (- parent-bottom height))
                              (min (max top parent-top)
                                   (- parent-bottom par))))))

                  ;; Use `modify-frame-parameters' since `left' and
                  ;; `top' may want to move FRAME out of its PARENT.
                  (modify-frame-parameters
                   frame
                   `((left . (+ ,left)) (top . (+ ,top)))))))
              (setq last-x pos-x)
              (setq last-y pos-y))))
         (old-track-mouse track-mouse))
    ;; Start tracking.  The special value 'dragging' signals the
    ;; display engine to freeze the mouse pointer shape for as long
    ;; as we drag.
    (setq track-mouse 'dragging)
    ;; Loop reading events and sampling the position of the mouse.
    (setq exitfun
          (set-transient-map
           (let ((map (make-sparse-keymap)))
             (define-key map [switch-frame] #'ignore)
             (define-key map [select-window] #'ignore)
             (define-key map [scroll-bar-movement] #'ignore)
             (define-key map [mouse-movement] move)
             ;; Swallow drag-mouse-1 events to avoid selecting some other window.
             (define-key map [drag-mouse-1]
               (lambda () (interactive) (funcall exitfun)))
             ;; Some of the events will of course end up looked up
             ;; with a mode-line, header-line or vertical-line prefix ...
             (define-key map [mode-line] map)
             (define-key map [header-line] map)
             (define-key map [vertical-line] map)
             ;; ... and some maybe even with a right- or bottom-divider
             ;; prefix.
             (define-key map [right-divider] map)
             (define-key map [bottom-divider] map)
             map)
           t (lambda () (setq track-mouse old-track-mouse))))))

(defun mouse-drag-left-edge (start-event)
  "Drag left edge of a frame with the mouse.
START-EVENT is the starting mouse event of the drag action."
  (interactive "e")
  (mouse-drag-frame start-event 'left))

(defun mouse-drag-top-left-corner (start-event)
  "Drag top left corner of a frame with the mouse.
START-EVENT is the starting mouse event of the drag action."
  (interactive "e")
  (mouse-drag-frame start-event 'top-left))

(defun mouse-drag-top-edge (start-event)
  "Drag top edge of a frame with the mouse.
START-EVENT is the starting mouse event of the drag action."
  (interactive "e")
  (mouse-drag-frame start-event 'top))

(defun mouse-drag-top-right-corner (start-event)
  "Drag top right corner of a frame with the mouse.
START-EVENT is the starting mouse event of the drag action."
  (interactive "e")
  (mouse-drag-frame start-event 'top-right))

(defun mouse-drag-right-edge (start-event)
  "Drag right edge of a frame with the mouse.
START-EVENT is the starting mouse event of the drag action."
  (interactive "e")
  (mouse-drag-frame start-event 'right))

(defun mouse-drag-bottom-right-corner (start-event)
  "Drag bottom right corner of a frame with the mouse.
START-EVENT is the starting mouse event of the drag action."
  (interactive "e")
  (mouse-drag-frame start-event 'bottom-right))

(defun mouse-drag-bottom-edge (start-event)
  "Drag bottom edge of a frame with the mouse.
START-EVENT is the starting mouse event of the drag action."
  (interactive "e")
  (mouse-drag-frame start-event 'bottom))

(defun mouse-drag-bottom-left-corner (start-event)
  "Drag bottom left corner of a frame with the mouse.
START-EVENT is the starting mouse event of the drag action."
  (interactive "e")
  (mouse-drag-frame start-event 'bottom-left))

(defcustom mouse-select-region-move-to-beginning nil
  "Effect of selecting a region extending backward from double click.
Nil means keep point at the position clicked (region end);
non-nil means move point to beginning of region."
  :type '(choice (const :tag "Don't move point" nil)
		 (const :tag "Move point to beginning of region" t))
  :version "26.1")

(defun mouse-set-point (event &optional promote-to-region)
  "Move point to the position clicked on with the mouse.
This should be bound to a mouse click event type.
If PROMOTE-TO-REGION is non-nil and event is a multiple-click, select
the corresponding element around point, with the resulting position of
point determined by `mouse-select-region-move-to-beginning'."
  (interactive "e\np")
  (mouse-minibuffer-check event)
  (if (and promote-to-region (> (event-click-count event) 1))
      (progn
        (mouse-set-region event)
        (when mouse-select-region-move-to-beginning
          (when (> (posn-point (event-start event)) (region-beginning))
            (exchange-point-and-mark))))
    ;; Use event-end in case called from mouse-drag-region.
    ;; If EVENT is a click, event-end and event-start give same value.
    (posn-set-point (event-end event))))

(defvar mouse-last-region-beg nil)
(defvar mouse-last-region-end nil)
(defvar mouse-last-region-tick nil)

(defun mouse-region-match ()
  "Return non-nil if there's an active region that was set with the mouse."
  (and (mark t) mark-active
       (eq mouse-last-region-beg (region-beginning))
       (eq mouse-last-region-end (region-end))
       (eq mouse-last-region-tick (buffer-modified-tick))))

(defvar mouse--drag-start-event nil)

(defun mouse-set-region (click)
  "Set the region to the text dragged over, and copy to kill ring.
This should be bound to a mouse drag event.
See the `mouse-drag-copy-region' variable to control whether this
command alters the kill ring or not."
  (interactive "e")
  (mouse-minibuffer-check click)
  (select-window (posn-window (event-start click)))
  (let ((beg (posn-point (event-start click)))
        (end
         (if (eq (posn-window (event-end click)) (selected-window))
             (posn-point (event-end click))
           ;; If the mouse ends up in any other window or on the menu
           ;; bar, use `window-point' of selected window (Bug#23707).
           (window-point)))
        (click-count (event-click-count click)))
    (let ((drag-start (terminal-parameter nil 'mouse-drag-start)))
      (when drag-start
        ;; Drag events don't come with a click count, sadly, so we hack
        ;; our way around this problem by remembering the start-event in
        ;; `mouse-drag-start' and fetching the click-count from there.
        (when (and (<= click-count 1)
                   (equal beg (posn-point (event-start drag-start))))
          (setq click-count (event-click-count drag-start)))
        ;; Occasionally we get spurious drag events where the user hasn't
        ;; dragged his mouse, but instead Emacs has dragged the text under the
        ;; user's mouse.  Try to recover those cases (bug#17562).
        (when (and (equal (posn-x-y (event-start click))
                          (posn-x-y (event-end click)))
                   (not (eq (car drag-start) 'mouse-movement)))
          (setq end beg))
        (setf (terminal-parameter nil 'mouse-drag-start) nil)))
    (when (and (integerp beg) (integerp end))
      (let ((range (mouse-start-end beg end (1- click-count))))
        (if (< end beg)
            (setq end (nth 0 range) beg (nth 1 range))
          (setq beg (nth 0 range) end (nth 1 range)))))
    (and mouse-drag-copy-region (integerp beg) (integerp end)
	 ;; Don't set this-command to `kill-region', so a following
	 ;; C-w won't double the text in the kill ring.  Ignore
	 ;; `last-command' so we don't append to a preceding kill.
	 (let (this-command last-command deactivate-mark)
	   (copy-region-as-kill beg end)))
    (if (numberp beg) (goto-char beg))
    ;; On a text terminal, bounce the cursor.
    (or transient-mark-mode
	(window-system)
	(sit-for 1))
    (push-mark)
    (set-mark (point))
    (if (numberp end) (goto-char end))
    (mouse-set-region-1)))

(defun mouse-set-region-1 ()
  ;; Set transient-mark-mode for a little while.
  (unless (eq (car-safe transient-mark-mode) 'only)
    (setq-local transient-mark-mode
                (cons 'only
                      (unless (eq transient-mark-mode 'lambda)
                        transient-mark-mode))))
  (setq mouse-last-region-beg (region-beginning))
  (setq mouse-last-region-end (region-end))
  (setq mouse-last-region-tick (buffer-modified-tick)))

(defcustom mouse-scroll-delay 0.25
  "The pause between scroll steps caused by mouse drags, in seconds.
If you drag the mouse beyond the edge of a window, Emacs scrolls the
window to bring the text beyond that edge into view, with a delay of
this many seconds between scroll steps.  Scrolling stops when you move
the mouse back into the window, or release the button.
This variable's value may be non-integral.
Setting this to zero causes Emacs to scroll as fast as it can."
  :type 'number)

(defcustom mouse-scroll-min-lines 1
  "The minimum number of lines scrolled by dragging mouse out of window.
Moving the mouse out the top or bottom edge of the window begins
scrolling repeatedly.  The number of lines scrolled per repetition
is normally equal to the number of lines beyond the window edge that
the mouse has moved.  However, it always scrolls at least the number
of lines specified by this variable."
  :type 'integer)

(defun mouse-scroll-subr (window jump &optional overlay start adjust)
  "Scroll the window WINDOW, JUMP lines at a time, until new input arrives.
If OVERLAY is an overlay, let it stretch from START to the far edge of
the newly visible text.
ADJUST, if non-nil, is a function, without arguments, to call after
setting point.
Upon exit, point is at the far edge of the newly visible text."
  (cond
   ((and (> jump 0) (< jump mouse-scroll-min-lines))
    (setq jump mouse-scroll-min-lines))
   ((and (< jump 0) (< (- jump) mouse-scroll-min-lines))
    (setq jump (- mouse-scroll-min-lines))))
  (let ((opoint (point)))
    (while (progn
	     (goto-char (window-start window))
	     (if (not (zerop (vertical-motion jump window)))
		 (progn
		   (set-window-start window (point))
		   (if (natnump jump)
		       (if (window-end window)
			   (progn
			     (goto-char (window-end window))
			     ;; window-end doesn't reflect the window's new
			     ;; start position until the next redisplay.
			     (vertical-motion (1- jump) window))
			 (vertical-motion (- (window-height window) 2)))
		     (goto-char (window-start window)))
		   (if overlay
		       (move-overlay overlay start (point)))
		   ;; Now that we have scrolled WINDOW properly,
		   ;; put point back where it was for the redisplay
		   ;; so that we don't mess up the selected window.
		   (or (eq window (selected-window))
		       (goto-char opoint))
                   (when adjust
                     (funcall adjust))
		   (sit-for mouse-scroll-delay)))))
    (or (eq window (selected-window))
	(goto-char opoint))))

(defvar mouse-selection-click-count 0)

(defvar mouse-selection-click-count-buffer nil)

(defun mouse-drag-region (start-event)
  "Set the region to the text that the mouse is dragged over.
Highlight the drag area as you move the mouse.
This must be bound to a button-down mouse event.
In Transient Mark mode, the highlighting remains as long as the mark
remains active.  Otherwise, it remains until the next input event.

When the region already exists and `mouse-drag-and-drop-region'
is non-nil, this moves the entire region of text to where mouse
is dragged over to."
  (interactive "e")
  (if (and mouse-drag-and-drop-region
           (not (member 'triple (event-modifiers start-event)))
           (equal (mouse-posn-property (event-start start-event) 'face) 'region))
      (mouse-drag-and-drop-region start-event)
    ;; Give temporary modes such as isearch a chance to turn off.
    (run-hooks 'mouse-leave-buffer-hook)
    (mouse-drag-track start-event)))

;; Inhibit the region-confinement when undoing mouse-drag-region
;; immediately after the command.  Otherwise, the selection left
;; active around the dragged text would prevent an undo of the whole
;; operation.
(put 'mouse-drag-region 'undo-inhibit-region t)

(defun mouse-posn-property (pos property)
  "Look for a property at click position.
POS may be either a buffer position or a click position like
those returned from `event-start'.  If the click position is on
a string, the text property PROPERTY is examined.
If this is nil or the click is not on a string, then
the corresponding buffer position is searched for PROPERTY.
If PROPERTY is encountered in one of those places,
its value is returned."
  (if (consp pos)
      (let ((w (posn-window pos)) (pt (posn-point pos))
	    (str (posn-string pos)))
        ;; FIXME: When STR has a `category' property and there's another
        ;; `category' property at PT, we should probably disregard the
        ;; `category' property at PT while doing the (get-char-property
        ;; pt property w)!
	(or (and str
		 (get-text-property (cdr str) property (car str)))
            ;; Mouse clicks in the fringe come with a position in
            ;; (nth 5).  This is useful but is not exactly where we clicked, so
            ;; don't look up that position's properties!
	    (and pt (not (memq (posn-area pos) '(left-fringe right-fringe
                                                 left-margin right-margin)))
		 (get-char-property pt property w))))
    (get-char-property pos property)))

(defun mouse-on-link-p (pos)
  "Return non-nil if POS is on a link in the current buffer.
POS must specify a buffer position in the current buffer, as a list
of the form returned by the `event-start' and `event-end' functions,
or a mouse event location in the selected window (see `event-start').
However, if `mouse-1-click-in-non-selected-windows' is non-nil,
POS may be a mouse event location in any window.

A clickable link is identified by one of the following methods:

- If the character at POS has a non-nil `follow-link' text or
overlay property, the value of that property determines what to do.

- If there is a local key-binding or a keybinding at position POS
for the `follow-link' event, the binding of that event determines
what to do.

The resulting value determine whether POS is inside a link:

- If the value is `mouse-face', POS is inside a link if there
is a non-nil `mouse-face' property at POS.  Return t in this case.

- If the value is a function, FUNC, POS is inside a link if
the call (FUNC POS) returns non-nil.  Return the return value
from that call.  Arg is (posn-point POS) if POS is a mouse event.

- Otherwise, return the value itself.

The return value is interpreted as follows:

- If it is an array, the mouse-1 event is translated into the
first element of that array, i.e. the action of the mouse-1
click is the local or global binding of that event.

- Otherwise, the mouse-1 event is translated into a mouse-2 event
at the same position."
  (let ((action
	 (and (or (not (consp pos))
		  mouse-1-click-in-non-selected-windows
		  (eq (selected-window) (posn-window pos)))
	      (or (mouse-posn-property pos 'follow-link)
                  (let ((area (posn-area pos)))
                    (when area
                      (key-binding (vector area 'follow-link) nil t pos)))
		  (key-binding [follow-link] nil t pos)))))
    (cond
     ((eq action 'mouse-face)
      (and (mouse-posn-property pos 'mouse-face) t))
     ((functionp action)
      ;; FIXME: This seems questionable if the click is not in a buffer.
      ;; Should we instead decide that `action' takes a `posn'?
      (if (consp pos)
	  (with-current-buffer (window-buffer (posn-window pos))
	    (funcall action (posn-point pos)))
	(funcall action pos)))
     (t action))))

(defun mouse-fixup-help-message (msg)
  "Fix help message MSG for `mouse-1-click-follows-link'."
  (let (mp pos)
    (if (and mouse-1-click-follows-link
	     (stringp msg)
	     (string-match-p "\\`mouse-2" msg)
	     (setq mp (mouse-pixel-position))
	     (consp (setq pos (cdr mp)))
	     (car pos) (>= (car pos) 0)
	     (cdr pos) (>= (cdr pos) 0)
	     (setq pos (posn-at-x-y (car pos) (cdr pos) (car mp)))
	     (windowp (posn-window pos)))
	(with-current-buffer (window-buffer (posn-window pos))
	  (if (mouse-on-link-p pos)
	      (setq msg (concat
		    (cond
		     ((eq mouse-1-click-follows-link 'double) "double-")
		     ((and (integerp mouse-1-click-follows-link)
			   (< mouse-1-click-follows-link 0)) "Long ")
		     (t ""))
		    "mouse-1" (substring msg 7)))))))
  msg)

(defun mouse-drag-track (start-event)
    "Track mouse drags by highlighting area between point and cursor.
The region will be defined with mark and point."
  (mouse-minibuffer-check start-event)
  (setq mouse-selection-click-count-buffer (current-buffer))
  (deactivate-mark)
  (let* ((scroll-margin 0) ; Avoid margin scrolling (Bug#9541).
	 (start-posn (event-start start-event))
	 (start-point (posn-point start-posn))
	 (start-window (posn-window start-posn))
	 (_ (with-current-buffer (window-buffer start-window)
	      (setq deactivate-mark nil)))
         ;; We've recorded what we needed from the current buffer and
         ;; window, now let's jump to the place of the event, where things
         ;; are happening.
         (_ (mouse-set-point start-event))
         (echo-keystrokes 0)
	 (bounds (window-edges start-window))
	 (make-cursor-line-fully-visible nil)
	 (top (nth 1 bounds))
	 (bottom (if (or (window-minibuffer-p start-window)
                         ;; Do not account for the mode line if there
                         ;; is no mode line, which is common for child
                         ;; frames.
                         (not mode-line-format))
		     (nth 3 bounds)
		   ;; Don't count the mode line.
		   (1- (nth 3 bounds))))
	 (click-count (1- (event-click-count start-event)))
	 ;; Suppress automatic hscrolling, because that is a nuisance
	 ;; when setting point near the right fringe (but see below).
	 (auto-hscroll-mode-saved auto-hscroll-mode)
         (old-track-mouse track-mouse))

    (setq mouse-selection-click-count click-count)
    ;; In case the down click is in the middle of some intangible text,
    ;; use the end of that text, and put it in START-POINT.
    (if (< (point) start-point)
	(goto-char start-point))
    (setq start-point (point))

    ;; Activate the region, using `mouse-start-end' to determine where
    ;; to put point and mark (e.g., double-click will select a word).
    (setq-local transient-mark-mode
                (if (eq transient-mark-mode 'lambda)
                    '(only)
                  (cons 'only transient-mark-mode)))
    (let ((range (mouse-start-end start-point start-point click-count)))
      (push-mark (nth 0 range) t t)
      (goto-char (nth 1 range)))

    (setf (terminal-parameter nil 'mouse-drag-start) start-event)
    (setq track-mouse t)
    (setq auto-hscroll-mode nil)

    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map [switch-frame] #'ignore)
       (define-key map [select-window] #'ignore)
       (define-key map [mouse-movement]
         (lambda (event) (interactive "e")
           (let* ((end (event-end event))
                  (end-point (posn-point end)))
             (unless (eq end-point start-point)
               ;; As soon as the user moves, we can re-enable auto-hscroll.
               (setq auto-hscroll-mode auto-hscroll-mode-saved)
               ;; And remember that we have moved, so mouse-set-region can know
               ;; its event is really a drag event.
               (setcar start-event 'mouse-movement))
             (if (and (eq (posn-window end) start-window)
                      (integer-or-marker-p end-point))
                 (mouse--drag-set-mark-and-point start-point
                                                 end-point click-count)
               (let ((mouse-row (cdr (cdr (mouse-position)))))
                 (cond
                  ((null mouse-row))
                  ((< mouse-row top)
                   (mouse-scroll-subr start-window (- mouse-row top)
                                      nil start-point))
                  ((>= mouse-row bottom)
                   (mouse-scroll-subr start-window (1+ (- mouse-row bottom))
                                      nil start-point))))))))
       map)
     t (lambda ()
         (setq track-mouse old-track-mouse)
         (setq auto-hscroll-mode auto-hscroll-mode-saved)
         (deactivate-mark)
         (pop-mark)))))

(defun mouse--drag-set-mark-and-point (start click click-count)
  (let* ((range (mouse-start-end start click click-count))
	 (beg (nth 0 range))
	 (end (nth 1 range)))
    (cond ((eq (mark) beg)
	   (goto-char end))
	  ((eq (mark) end)
	   (goto-char beg))
	  ((< click (mark))
	   (set-mark end)
	   (goto-char beg))
	  (t
	   (set-mark beg)
	   (goto-char end)))))

;; Commands to handle xterm-style multiple clicks.
(defun mouse-skip-word (dir)
  "Skip over word, over whitespace, or over identical punctuation.
If DIR is positive skip forward; if negative, skip backward."
  (let* ((char (following-char))
	 (syntax (char-to-string (char-syntax char))))
    (cond ((string= syntax "w")
	   ;; Here, we can't use skip-syntax-forward/backward because
	   ;; they don't pay attention to word-separating-categories,
	   ;; and thus they will skip over a true word boundary.  So,
	   ;; we simulate the original behavior by using forward-word.
	   (if (< dir 0)
	       (if (not (looking-at "\\<"))
		   (forward-word -1))
	     (if (or (looking-at "\\<") (not (looking-at "\\>")))
		 (forward-word 1))))
	  ((string= syntax " ")
	   (if (< dir 0)
	       (skip-syntax-backward syntax)
	     (skip-syntax-forward syntax)))
	  ((string= syntax "_")
	   (if (< dir 0)
	       (skip-syntax-backward "w_")
	     (skip-syntax-forward "w_")))
	  ((< dir 0)
	   (while (and (not (bobp)) (= (preceding-char) char))
	     (forward-char -1)))
	  (t
	   (while (and (not (eobp)) (= (following-char) char))
	     (forward-char 1))))))

(defun mouse-start-end (start end mode)
  "Return a list of region bounds based on START and END according to MODE.
If MODE is 0 then set point to (min START END), mark to (max START END).
If MODE is 1 then set point to start of word at (min START END),
mark to end of word at (max START END).
If MODE is 2 then do the same for lines."
  (if (> start end)
      (let ((temp start))
        (setq start end
              end temp)))
  (setq mode (mod mode 3))
  (cond ((= mode 0)
	 (list start end))
        ((and (= mode 1)
              (= start end)
	      (char-after start)
              (= (char-syntax (char-after start)) ?\())
         (if (/= (syntax-class (syntax-after start)) 4) ; raw syntax code for ?\(
             ;; This happens in CC Mode when unbalanced parens in CPP
             ;; constructs are given punctuation syntax with
             ;; syntax-table text properties.  (2016-02-21).
             (signal 'scan-error (list "Containing expression ends prematurely"
                                       start start))
           (list start
                 (save-excursion
                   (goto-char start)
                   (forward-sexp 1)
                   (point)))))
        ((and (= mode 1)
              (= start end)
	      (char-after start)
              (= (char-syntax (char-after start)) ?\)))
         (if (/= (syntax-class (syntax-after start)) 5) ; raw syntax code for ?\)
             ;; See above comment about CC Mode.
             (signal 'scan-error (list "Unbalanced parentheses" start start))
           (list (save-excursion
                   (goto-char (1+ start))
                   (backward-sexp 1)
                   (point))
                 (1+ start))))
	((and (= mode 1)
              (= start end)
	      (char-after start)
              (= (char-syntax (char-after start)) ?\"))
	 (let ((open (or (eq start (point-min))
			 (save-excursion
			   (goto-char (- start 1))
			   (looking-at "\\s(\\|\\s \\|\\s>")))))
	   (if open
	       (list start
		     (save-excursion
		       (condition-case nil
			   (progn
			     (goto-char start)
			     (forward-sexp 1)
			     (point))
			 (error end))))
	     (list (save-excursion
		     (condition-case nil
			 (progn
			   (goto-char (1+ start))
			   (backward-sexp 1)
			   (point))
		       (error end)))
		   (1+ start)))))
        ((= mode 1)
	 (list (save-excursion
		 (goto-char start)
		 (mouse-skip-word -1)
		 (point))
	       (save-excursion
		 (goto-char end)
		 (mouse-skip-word 1)
		 (point))))
        ((= mode 2)
	 (list (save-excursion
		 (goto-char start)
		 (line-beginning-position 1))
	       (save-excursion
		 (goto-char end)
		 (forward-line 1)
		 (point))))))

;; Subroutine: set the mark where CLICK happened,
;; but don't do anything else.
(defun mouse-set-mark-fast (click)
  (mouse-minibuffer-check click)
  (let ((posn (event-start click)))
    (select-window (posn-window posn))
    (if (numberp (posn-point posn))
	(push-mark (posn-point posn) t t))))

(defun mouse-undouble-last-event (events)
  (let* ((index (1- (length events)))
	 (last (nthcdr index events))
	 (event (car last))
	 (basic (event-basic-type event))
	 (old-modifiers (event-modifiers event))
	 (modifiers (delq 'double (delq 'triple (copy-sequence old-modifiers))))
	 (new
	  (if (consp event)
	      ;; Use reverse, not nreverse, since event-modifiers
	      ;; does not copy the list it returns.
	      (cons (event-convert-list (reverse (cons basic modifiers)))
		    (cdr event))
	    event)))
    (setcar last new)
    (if (and (not (equal modifiers old-modifiers))
	     (key-binding (apply 'vector events)))
	t
      (setcar last event)
      nil)))

;; Momentarily show where the mark is, if highlighting doesn't show it.

(defun mouse-set-mark (click)
  "Set mark at the position clicked on with the mouse.
Display cursor at that position for a second.
This must be bound to a mouse click."
  (interactive "e")
  (mouse-minibuffer-check click)
  (select-window (posn-window (event-start click)))
  ;; FIXME: Use save-excursion
  (let ((point-save (point)))
    (unwind-protect
	(progn (mouse-set-point click)
	       (push-mark nil t t)
	       (or transient-mark-mode
		   (sit-for 1)))
      (goto-char point-save))))

(defun mouse-kill (click)
  "Kill the region between point and the mouse click.
The text is saved in the kill ring, as with \\[kill-region]."
  (interactive "e")
  (mouse-minibuffer-check click)
  (let* ((posn (event-start click))
	 (click-posn (posn-point posn)))
    (select-window (posn-window posn))
    (if (numberp click-posn)
	(kill-region (min (point) click-posn)
		     (max (point) click-posn)))))

(defun mouse-yank-at-click (click arg)
  "Insert the last stretch of killed text at the position clicked on.
Also move point to one end of the text thus inserted (normally the end),
and set mark at the beginning.
Prefix arguments are interpreted as with \\[yank].
If `mouse-yank-at-point' is non-nil, insert at point
regardless of where you click."
  (interactive "e\nP")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (when select-active-regions
    ;; Without this, confusing things happen upon e.g. inserting into
    ;; the middle of an active region.
    (deactivate-mark))
  (or mouse-yank-at-point (mouse-set-point click))
  (setq this-command 'yank)
  (setq mouse-selection-click-count 0)
  (yank arg))

(defun mouse-yank-primary (click)
  "Insert the primary selection at the position clicked on.
Move point to the end of the inserted text, and set mark at
beginning.  If `mouse-yank-at-point' is non-nil, insert at point
regardless of where you click."
  (interactive "e")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  ;; Without this, confusing things happen upon e.g. inserting into
  ;; the middle of an active region.
  (when select-active-regions
    (let (select-active-regions)
      (deactivate-mark)))
  (or mouse-yank-at-point (mouse-set-point click))
  (let ((primary (gui-get-primary-selection)))
    (push-mark)
    (insert-for-yank primary)))

(defun mouse-kill-ring-save (click)
  "Copy the region between point and the mouse click in the kill ring.
This does not delete the region; it acts like \\[kill-ring-save]."
  (interactive "e")
  (mouse-set-mark-fast click)
  (let (this-command last-command)
    (kill-ring-save (point) (mark t))))

;; This function used to delete the text between point and the mouse
;; whenever it was equal to the front of the kill ring, but some
;; people found that confusing.

;; The position of the last invocation of `mouse-save-then-kill'.
(defvar mouse-save-then-kill-posn nil)

(defun mouse-save-then-kill-delete-region (beg end)
  ;; We must make our own undo boundaries
  ;; because they happen automatically only for the current buffer.
  (undo-boundary)
  (if (or (= beg end) (eq buffer-undo-list t))
      ;; If we have no undo list in this buffer,
      ;; just delete.
      (delete-region beg end)
    ;; Delete, but make the undo-list entry share with the kill ring.
    ;; First, delete just one char, so in case buffer is being modified
    ;; for the first time, the undo list records that fact.
    (let ((inhibit-modification-hooks t))
      (delete-region beg
		     (+ beg (if (> end beg) 1 -1))))
    (let ((buffer-undo-list buffer-undo-list))
      ;; Undo that deletion--but don't change the undo list!
      (let ((inhibit-modification-hooks t))
	(primitive-undo 1 buffer-undo-list))
      ;; Now delete the rest of the specified region,
      ;; but don't record it.
      (setq buffer-undo-list t)
      (if (/= (length (car kill-ring)) (- (max end beg) (min end beg)))
	  (error "Lossage in mouse-save-then-kill-delete-region"))
      (delete-region beg end))
    (let ((tail buffer-undo-list))
      ;; Search back in buffer-undo-list for the string
      ;; that came from deleting one character.
      (while (and tail (not (stringp (car (car tail)))))
	(setq tail (cdr tail)))
      ;; Replace it with an entry for the entire deleted text.
      (and tail
	   (setcar tail (cons (car kill-ring) (min beg end))))))
  (undo-boundary))

(defun mouse-save-then-kill (click)
  "Set the region according to CLICK; the second time, kill it.
CLICK should be a mouse click event.

If the region is inactive, activate it temporarily.  Set mark at
the original point, and move point to the position of CLICK.

If the region is already active, adjust it.  Normally, do this by
moving point or mark, whichever is closer, to CLICK.  But if you
have selected whole words or lines, move point or mark to the
word or line boundary closest to CLICK instead.

If `mouse-drag-copy-region' is non-nil, this command also saves the
new region to the kill ring (replacing the previous kill if the
previous region was just saved to the kill ring).

If this command is called a second consecutive time with the same
CLICK position, kill the region (or delete it
if `mouse-drag-copy-region' is non-nil)."
  (interactive "e")
  (mouse-minibuffer-check click)
  (let* ((posn     (event-start click))
	 (click-pt (posn-point posn))
	 (window   (posn-window posn))
	 (buf      (window-buffer window))
	 ;; Don't let a subsequent kill command append to this one.
	 (this-command this-command)
	 ;; Check if the user has multi-clicked to select words/lines.
	 (click-count
	  (if (and (eq mouse-selection-click-count-buffer buf)
		   (with-current-buffer buf (mark t)))
	      mouse-selection-click-count
	    0)))
    (cond
     ((not (numberp click-pt)) nil)
     ;; If the user clicked without moving point, kill the region.
     ;; This also resets `mouse-selection-click-count'.
     ((and (eq last-command 'mouse-save-then-kill)
	   (eq click-pt mouse-save-then-kill-posn)
	   (eq window (selected-window)))
      (if mouse-drag-copy-region
          ;; Region already saved in the previous click;
          ;; don't make a duplicate entry, just delete.
          (funcall region-extract-function 'delete-only)
        (kill-region (mark t) (point) 'region))
      (setq mouse-selection-click-count 0)
      (setq mouse-save-then-kill-posn nil))

     ;; Otherwise, if there is a suitable region, adjust it by moving
     ;; one end (whichever is closer) to CLICK-PT.
     ((or (with-current-buffer buf (region-active-p))
	  (and (eq window (selected-window))
	       (mark t)
	       (or (and (eq last-command 'mouse-save-then-kill)
			mouse-save-then-kill-posn)
		   (and (memq last-command '(mouse-drag-region
					     mouse-set-region))
			(or mark-even-if-inactive
			    (not transient-mark-mode))))))
      (select-window window)
      (let* ((range (mouse-start-end click-pt click-pt click-count)))
	(if (< (abs (- click-pt (mark t)))
	       (abs (- click-pt (point))))
	    (set-mark (car range))
	  (goto-char (nth 1 range)))
	(setq deactivate-mark nil)
	(mouse-set-region-1)
        (when mouse-drag-copy-region
          ;; Region already copied to kill-ring once, so replace.
          (kill-new (funcall region-extract-function nil) t))
	;; Arrange for a repeated mouse-3 to kill the region.
	(setq mouse-save-then-kill-posn click-pt)))

     ;; Otherwise, set the mark where point is and move to CLICK-PT.
     (t
      (select-window window)
      (mouse-set-mark-fast click)
      (let ((before-scroll (with-current-buffer buf point-before-scroll)))
	(if before-scroll (goto-char before-scroll)))
      (exchange-point-and-mark)
      (mouse-set-region-1)
      (when mouse-drag-copy-region
        (kill-new (filter-buffer-substring (mark t) (point))))
      (setq mouse-save-then-kill-posn click-pt)))))


(global-set-key [M-mouse-1] 'mouse-start-secondary)
(global-set-key [M-drag-mouse-1] 'mouse-set-secondary)
(global-set-key [M-down-mouse-1] 'mouse-drag-secondary)
(global-set-key [M-mouse-3] 'mouse-secondary-save-then-kill)
(global-set-key [M-mouse-2] 'mouse-yank-secondary)

(defconst mouse-secondary-overlay
  (let ((ol (make-overlay (point-min) (point-min))))
    (delete-overlay ol)
    (overlay-put ol 'face 'secondary-selection)
    ol)
  "An overlay which records the current secondary selection.
It is deleted when there is no secondary selection.")

(defvar mouse-secondary-click-count 0)

;; A marker which records the specified first end for a secondary selection.
;; May be nil.
(defvar mouse-secondary-start nil)

(defun mouse-start-secondary (click)
  "Set one end of the secondary selection to the position clicked on.
Use \\[mouse-secondary-save-then-kill] to set the other end
and complete the secondary selection."
  (interactive "e")
  (mouse-minibuffer-check click)
  (let ((posn (event-start click)))
    (with-current-buffer (window-buffer (posn-window posn))
      ;; Cancel any preexisting secondary selection.
      (delete-overlay mouse-secondary-overlay)
      (if (numberp (posn-point posn))
	  (progn
	    (or mouse-secondary-start
		(setq mouse-secondary-start (make-marker)))
	    (move-marker mouse-secondary-start (posn-point posn)))))))

(defun mouse-set-secondary (click)
  "Set the secondary selection to the text that the mouse is dragged over.
This must be bound to a mouse drag event."
  (interactive "e")
  (mouse-minibuffer-check click)
  (let ((posn (event-start click))
	beg
	(end (event-end click)))
    (with-current-buffer (window-buffer (posn-window posn))
      (if (numberp (posn-point posn))
	  (setq beg (posn-point posn)))
      (move-overlay mouse-secondary-overlay beg (posn-point end))
      (gui-set-selection
       'SECONDARY
       (buffer-substring (overlay-start mouse-secondary-overlay)
			 (overlay-end mouse-secondary-overlay))))))

(defun mouse-drag-secondary (start-event)
  "Set the secondary selection to the text that the mouse is dragged over.
Highlight the drag area as you move the mouse.
This must be bound to a button-down mouse event.
The function returns a non-nil value if it creates a secondary selection."
  (interactive "e")
  (mouse-minibuffer-check start-event)
  (let* ((echo-keystrokes 0)
	 (start-posn (event-start start-event))
	 (start-point (posn-point start-posn))
	 (start-window (posn-window start-posn))
	 (bounds (window-edges start-window))
	 (top (nth 1 bounds))
	 (bottom (if (window-minibuffer-p start-window)
		     (nth 3 bounds)
		   ;; Don't count the mode line.
		   (1- (nth 3 bounds))))
	 (click-count (1- (event-click-count start-event))))
    (with-current-buffer (window-buffer start-window)
      (setq mouse-secondary-click-count click-count)
      (if (> (mod click-count 3) 0)
	  ;; Double or triple press: make an initial selection
	  ;; of one word or line.
	  (let ((range (mouse-start-end start-point start-point click-count)))
	    (set-marker mouse-secondary-start nil)
	    (move-overlay mouse-secondary-overlay (car range) (nth 1 range)
			  (window-buffer start-window)))
	;; Single-press: cancel any preexisting secondary selection.
	(or mouse-secondary-start
	    (setq mouse-secondary-start (make-marker)))
	(set-marker mouse-secondary-start start-point)
	(delete-overlay mouse-secondary-overlay))
      ;; FIXME: Use mouse-drag-track!
      (let (event end end-point)
	(track-mouse
	  (while (progn
		   (setq event (read-event))
		   (or (mouse-movement-p event)
		       (memq (car-safe event) '(switch-frame select-window))))

	    (if (memq (car-safe event) '(switch-frame select-window))
		nil
	      (setq end (event-end event)
		    end-point (posn-point end))
	      (cond
	       ;; Are we moving within the original window?
	       ((and (eq (posn-window end) start-window)
		     (integer-or-marker-p end-point))
		(let ((range (mouse-start-end start-point end-point
					      click-count)))
		  (if (or (/= start-point end-point)
			  (null (marker-position mouse-secondary-start)))
		      (progn
			(set-marker mouse-secondary-start nil)
			(move-overlay mouse-secondary-overlay
				      (car range) (nth 1 range))))))
               (t
                (let ((mouse-row (cdr (cdr (mouse-position)))))
                  (cond
                   ((null mouse-row))
                   ((< mouse-row top)
                    (mouse-scroll-subr start-window (- mouse-row top)
				       mouse-secondary-overlay start-point))
                   ((>= mouse-row bottom)
                    (mouse-scroll-subr start-window (1+ (- mouse-row bottom))
                                       mouse-secondary-overlay start-point)))))))))

	(if (consp event)
	    (if (marker-position mouse-secondary-start)
		(save-window-excursion
		  (delete-overlay mouse-secondary-overlay)
		  (gui-set-selection 'SECONDARY nil)
		  (select-window start-window)
		  (save-excursion
		    (goto-char mouse-secondary-start)
		    (sit-for 1)
		    nil))
	      (gui-set-selection
	       'SECONDARY
	       (buffer-substring (overlay-start mouse-secondary-overlay)
				 (overlay-end mouse-secondary-overlay)))))))))

(defun mouse-yank-secondary (click)
  "Insert the secondary selection at the position clicked on.
Move point to the end of the inserted text.
If `mouse-yank-at-point' is non-nil, insert at point
regardless of where you click."
  (interactive "e")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (or mouse-yank-at-point (mouse-set-point click))
  (let ((secondary (gui-get-selection 'SECONDARY)))
    (if secondary
        (insert-for-yank secondary)
      (error "No secondary selection"))))

(defun mouse-kill-secondary ()
  "Kill the text in the secondary selection.
This is intended more as a keyboard command than as a mouse command
but it can work as either one.

The current buffer (in case of keyboard use), or the buffer clicked on,
must be the one that the secondary selection is in.  This requirement
is to prevent accidents."
  (interactive)
  (let* ((keys (this-command-keys))
	 (click (elt keys (1- (length keys)))))
    (or (eq (overlay-buffer mouse-secondary-overlay)
	    (if (listp click)
		(window-buffer (posn-window (event-start click)))
	      (current-buffer)))
	(error "Select or click on the buffer where the secondary selection is")))
  (let (this-command)
    (with-current-buffer (overlay-buffer mouse-secondary-overlay)
      (kill-region (overlay-start mouse-secondary-overlay)
		   (overlay-end mouse-secondary-overlay))))
  (delete-overlay mouse-secondary-overlay))

(defun mouse-secondary-save-then-kill (click)
  "Set the secondary selection and save it to the kill ring.
The second time, kill it.  CLICK should be a mouse click event.

If you have not called `mouse-start-secondary' in the clicked
buffer, activate the secondary selection and set it between point
and the click position CLICK.

Otherwise, adjust the bounds of the secondary selection.
Normally, do this by moving its beginning or end, whichever is
closer, to CLICK.  But if you have selected whole words or lines,
adjust to the word or line boundary closest to CLICK instead.

If this command is called a second consecutive time with the same
CLICK position, kill the secondary selection."
  (interactive "e")
  (mouse-minibuffer-check click)
  (let* ((posn     (event-start click))
	 (click-pt (posn-point posn))
	 (window   (posn-window posn))
	 (buf      (window-buffer window))
	 ;; Don't let a subsequent kill command append to this one.
	 (this-command this-command)
	 ;; Check if the user has multi-clicked to select words/lines.
	 (click-count
	  (if (eq (overlay-buffer mouse-secondary-overlay) buf)
	      mouse-secondary-click-count
	    0))
	 (beg (overlay-start mouse-secondary-overlay))
	 (end (overlay-end mouse-secondary-overlay)))

    (cond
     ((not (numberp click-pt)) nil)

     ;; If the secondary selection is not active in BUF, activate it.
     ((not (eq buf (or (overlay-buffer mouse-secondary-overlay)
		       (if mouse-secondary-start
			   (marker-buffer mouse-secondary-start)))))
      (select-window window)
      (setq mouse-secondary-start (make-marker))
      (move-marker mouse-secondary-start (point))
      (move-overlay mouse-secondary-overlay (point) click-pt buf)
      (kill-ring-save (point) click-pt))

     ;; If the user clicked without moving point, delete the secondary
     ;; selection.  This also resets `mouse-secondary-click-count'.
     ((and (eq last-command 'mouse-secondary-save-then-kill)
	   (eq click-pt mouse-save-then-kill-posn)
	   (eq window (selected-window)))
      (mouse-save-then-kill-delete-region beg end)
      (delete-overlay mouse-secondary-overlay)
      (setq mouse-secondary-click-count 0)
      (setq mouse-save-then-kill-posn nil))

     ;; Otherwise, if there is a suitable secondary selection overlay,
     ;; adjust it by moving one end (whichever is closer) to CLICK-PT.
     ((and beg (eq buf (overlay-buffer mouse-secondary-overlay)))
      (let* ((range (mouse-start-end click-pt click-pt click-count)))
	(if (< (abs (- click-pt beg))
	       (abs (- click-pt end)))
	    (move-overlay mouse-secondary-overlay (car range) end)
	  (move-overlay mouse-secondary-overlay beg (nth 1 range))))
      (setq deactivate-mark nil)
      (if (eq last-command 'mouse-secondary-save-then-kill)
	  ;; If the front of the kill ring comes from an immediately
	  ;; previous use of this command, replace the entry.
	  (kill-new
	   (buffer-substring (overlay-start mouse-secondary-overlay)
			     (overlay-end mouse-secondary-overlay))
	   t)
	(let (deactivate-mark)
	  (copy-region-as-kill (overlay-start mouse-secondary-overlay)
			       (overlay-end mouse-secondary-overlay))))
      (setq mouse-save-then-kill-posn click-pt))

     ;; Otherwise, set the secondary selection overlay.
     (t
      (select-window window)
      (if mouse-secondary-start
	  ;; All we have is one end of a selection, so put the other
	  ;; end here.
	  (let ((start (+ 0 mouse-secondary-start)))
	    (kill-ring-save start click-pt)
	    (move-overlay mouse-secondary-overlay start click-pt)))
      (setq mouse-save-then-kill-posn click-pt))))

  ;; Finally, set the window system's secondary selection.
  (let (str)
    (and (overlay-buffer mouse-secondary-overlay)
	 (setq str (buffer-substring (overlay-start mouse-secondary-overlay)
				     (overlay-end mouse-secondary-overlay)))
	 (> (length str) 0)
	 (gui-set-selection 'SECONDARY str))))

(defun secondary-selection-exist-p ()
  "Return non-nil if the secondary selection exists in the current buffer."
  (memq mouse-secondary-overlay (overlays-in (point-min) (point-max))))

(defun secondary-selection-to-region ()
  "Set beginning and end of the region to those of the secondary selection.
This puts mark and point at the beginning and the end of the
secondary selection, respectively.  This works when the secondary
selection exists and the region does not exist in current buffer;
the secondary selection will be deleted afterward.
If the region is active, or the secondary selection doesn't exist,
this function does nothing."
  (when (and (not (region-active-p))
             (secondary-selection-exist-p))
    (let ((beg (overlay-start mouse-secondary-overlay))
          (end (overlay-end mouse-secondary-overlay)))
      (push-mark beg t t)
      (goto-char end))
    ;; Delete the secondary selection on current buffer.
    (delete-overlay mouse-secondary-overlay)))

(defun secondary-selection-from-region ()
  "Set beginning and end of the secondary selection to those of the region.
When there is no region, this function does nothing."
  (when (region-active-p) ; Create the secondary selection from the region.
    (delete-overlay mouse-secondary-overlay) ; Delete the secondary selection even on a different buffer.
    (move-overlay mouse-secondary-overlay (region-beginning) (region-end))))


(declare-function rectangle--col-pos "rect" (col kind))
(declare-function rectangle--reset-point-crutches "rect" ())

(defconst mouse--rectangle-track-cursor t
  "Whether the mouse tracks the cursor when selecting a rectangle.
If nil, the mouse tracks the rectangle corner instead.")

(defun mouse-drag-region-rectangle (start-event)
  "Set the region to the rectangle that the mouse is dragged over.
This must be bound to a button-down mouse event."
  (interactive "e")
  (let* ((scroll-margin 0)
         (start-pos (event-start start-event))
         (start-posn (event-start start-event))
         (start-point (posn-point start-posn))
         (start-window (posn-window start-posn))
         (start-hscroll (window-hscroll start-window))
         (start-col (+ (car (posn-col-row start-pos)) start-hscroll))
         (bounds (window-edges start-window))
         (top (nth 1 bounds))
         (bottom (if (window-minibuffer-p start-window)
                     (nth 3 bounds)
                   (1- (nth 3 bounds))))
         (extra-margin (round (line-number-display-width 'columns)))
         (dragged nil)
         (old-track-mouse track-mouse)
         (old-mouse-fine-grained-tracking mouse-fine-grained-tracking)
         ;; For right-to-left text, columns are counted from the right margin;
         ;; translate from mouse events, which always count from the left.
         (adjusted-col (lambda (col)
                         (if (eq (current-bidi-paragraph-direction)
                                 'right-to-left)
                             (- (window-width) col extra-margin
                                (if mouse--rectangle-track-cursor 1 -1))
                           (- col extra-margin))))
         (map (make-sparse-keymap)))
    (define-key map [switch-frame] #'ignore)
    (define-key map [select-window] #'ignore)
    (define-key map [mouse-movement]
      (lambda (event)
        (interactive "e")
        (unless dragged
          ;; This is actually a drag.
          (setq dragged t)
          (mouse-minibuffer-check start-event)
          (deactivate-mark)
          (setq-local transient-mark-mode
                      (if (eq transient-mark-mode 'lambda)
                          '(only)
                        (cons 'only transient-mark-mode)))
          (posn-set-point start-pos)
          (rectangle-mark-mode)
          ;; Only tell rectangle about the exact column if we are possibly
          ;; beyond end-of-line or in a tab, since the column we got from
          ;; the mouse position isn't necessarily accurate for use in
          ;; specifying a rectangle (which uses the `move-to-column'
          ;; measure).
          (when (or (eolp) (eq (following-char) ?\t))
            (let ((col (funcall adjusted-col start-col)))
              (rectangle--col-pos col 'mark)
              (rectangle--col-pos col 'point))))

        (let* ((posn (event-end event))
               (window (posn-window posn))
               (hscroll (if (window-live-p window)
                            (window-hscroll window)
                          0))
               (mouse-row (cddr (mouse-position)))
               (mouse-col (+ (car (posn-col-row posn)) hscroll
                             (if mouse--rectangle-track-cursor 0 1)))
               (set-col (lambda ()
                          (if (or (eolp) (eq (following-char) ?\t))
                              (rectangle--col-pos
                               (funcall adjusted-col mouse-col) 'point)
                            (unless mouse--rectangle-track-cursor
                              (forward-char))
                            (rectangle--reset-point-crutches))))
               (scroll-adjust (lambda ()
                                (move-to-column
                                 (funcall adjusted-col mouse-col))
                                (funcall set-col))))
          (if (and (eq window start-window)
                   mouse-row
                   (<= top mouse-row (1- bottom)))
              ;; Drag inside the same window.
              (progn
                (posn-set-point posn)
                (funcall set-col))
            ;; Drag outside the window: scroll.
            (cond
             ((null mouse-row))
             ((< mouse-row top)
              (mouse-scroll-subr
               start-window (- mouse-row top) nil start-point
               scroll-adjust))
             ((>= mouse-row bottom)
              (mouse-scroll-subr
               start-window (1+ (- mouse-row bottom)) nil start-point
               scroll-adjust)))))))
    (condition-case err
        (progn
          (setq track-mouse t)
          (setq mouse-fine-grained-tracking t)
          (set-transient-map
           map t
           (lambda ()
             (setq track-mouse old-track-mouse)
             (setq mouse-fine-grained-tracking old-mouse-fine-grained-tracking)
             (when (or (not dragged)
                       (not (mark))
                       (equal (rectangle-dimensions (mark) (point)) '(0 . 1)))
               ;; No nontrivial region selected; deactivate rectangle mode.
               (deactivate-mark)))))
      ;; Clean up in case something went wrong.
      (error (setq track-mouse old-track-mouse)
             (setq mouse-fine-grained-tracking old-mouse-fine-grained-tracking)
             (signal (car err) (cdr err))))))

;; The drag event must be bound to something but does not need any effect,
;; as everything takes place in `mouse-drag-region-rectangle'.
;; The click event can be anything; `mouse-set-point' is just a convenience.
(global-set-key [C-M-down-mouse-1] #'mouse-drag-region-rectangle)
(global-set-key [C-M-drag-mouse-1] #'ignore)
(global-set-key [C-M-mouse-1]      #'mouse-set-point)


(defcustom mouse-buffer-menu-maxlen 20
  "Number of buffers in one pane (submenu) of the buffer menu.
If we have lots of buffers, divide them into groups of
`mouse-buffer-menu-maxlen' and make a pane (or submenu) for each one."
  :type 'integer)

(defcustom mouse-buffer-menu-mode-mult 4
  "Group the buffers by the major mode groups on \\[mouse-buffer-menu]?
This number which determines (in a hairy way) whether \\[mouse-buffer-menu]
will split the buffer menu by the major modes (see
`mouse-buffer-menu-mode-groups') or just by menu length.
Set to 1 (or even 0!) if you want to group by major mode always, and to
a large number if you prefer a mixed multitude.  The default is 4."
  :type 'integer
  :version "20.3")

(defvar mouse-buffer-menu-mode-groups
  (mapcar (lambda (arg) (cons  (purecopy (car arg)) (purecopy (cdr arg))))
  '(("Info\\|Help\\|Apropos\\|Man" . "Help")
    ("\\bVM\\b\\|\\bMH\\b\\|Message\\b\\|Mail\\|Group\\|Score\\|Summary\\|Article"
     . "Mail/News")
    ("\\<C\\>" . "C")
    ("ObjC" . "C")
    ("Text" . "Text")
    ("Outline" . "Text")
    ("\\(HT\\|SG\\|X\\|XHT\\)ML" . "SGML")
    ("log\\|diff\\|vc\\|cvs\\|Git\\|Annotate" . "Version Control")
    ("Threads\\|Memory\\|Disassembly\\|Breakpoints\\|Frames\\|Locals\\|Registers\\|Inferior I/O\\|Debugger"
     . "GDB")
    ("Lisp" . "Lisp")))
  "How to group various major modes together in \\[mouse-buffer-menu].
Each element has the form (REGEXP . GROUPNAME).
If the major mode's name string matches REGEXP, use GROUPNAME instead.")

(defun mouse-buffer-menu (event)
  "Pop up a menu of buffers for selection with the mouse.
This switches buffers in the window that you clicked on,
and selects that window."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((buf (x-popup-menu event (mouse-buffer-menu-map)))
        (window (posn-window (event-start event))))
    (when buf
      (select-window
       (if (framep window) (frame-selected-window window)
         window))
      (switch-to-buffer buf))))

(defun mouse-buffer-menu-map ()
  ;; Make an alist of elements that look like (MENU-ITEM . BUFFER).
  (let ((buffers (buffer-list)) split-by-major-mode sum-of-squares)
    (dolist (buf buffers)
      ;; Divide all buffers into buckets for various major modes.
      ;; Each bucket looks like (MODE NAMESTRING BUFFERS...).
      (with-current-buffer buf
        (let* ((adjusted-major-mode major-mode) elt)
          (dolist (group mouse-buffer-menu-mode-groups)
            (when (string-match (car group) (format-mode-line mode-name))
              (setq adjusted-major-mode (cdr group))))
          (setq elt (assoc adjusted-major-mode split-by-major-mode))
          (unless elt
            (setq elt (list adjusted-major-mode
                            (if (stringp adjusted-major-mode)
                                adjusted-major-mode
                              (format-mode-line mode-name nil nil buf)))
                  split-by-major-mode (cons elt split-by-major-mode)))
          (or (memq buf (cdr (cdr elt)))
              (setcdr (cdr elt) (cons buf (cdr (cdr elt))))))))
    ;; Compute the sum of squares of sizes of the major-mode buckets.
    (let ((tail split-by-major-mode))
      (setq sum-of-squares 0)
      (while tail
	(setq sum-of-squares
	      (+ sum-of-squares
		 (let ((len (length (cdr (cdr (car tail)))))) (* len len))))
	(setq tail (cdr tail))))
    (if (< (* sum-of-squares mouse-buffer-menu-mode-mult)
	   (* (length buffers) (length buffers)))
	;; Subdividing by major modes really helps, so let's do it.
	(let (subdivided-menus (buffers-left (length buffers)))
	  ;; Sort the list to put the most popular major modes first.
	  (setq split-by-major-mode
		(sort split-by-major-mode
		      (function (lambda (elt1 elt2)
				  (> (length elt1) (length elt2))))))
	  ;; Make a separate submenu for each major mode
	  ;; that has more than one buffer,
	  ;; unless all the remaining buffers are less than 1/10 of them.
	  (while (and split-by-major-mode
		      (and (> (length (car split-by-major-mode)) 3)
			   (> (* buffers-left 10) (length buffers))))
	    (let ((this-mode-list (mouse-buffer-menu-alist
				   (cdr (cdr (car split-by-major-mode))))))
	      (and this-mode-list
		   (setq subdivided-menus
			 (cons (cons
				(nth 1 (car split-by-major-mode))
				this-mode-list)
			       subdivided-menus))))
	    (setq buffers-left
		  (- buffers-left (length (cdr (car split-by-major-mode)))))
	    (setq split-by-major-mode (cdr split-by-major-mode)))
	  ;; If any major modes are left over,
	  ;; make a single submenu for them.
	  (if split-by-major-mode
	      (let ((others-list
		     (mouse-buffer-menu-alist
		      ;; we don't need split-by-major-mode any more,
		      ;; so we can ditch it with nconc (mapcan).
		      (mapcan 'cddr split-by-major-mode))))
		(and others-list
		     (setq subdivided-menus
			   (cons (cons "Others" others-list)
				 subdivided-menus)))))
          (cons "Buffer Menu" (nreverse subdivided-menus)))
      (cons "Buffer Menu"
            (mouse-buffer-menu-split "Select Buffer"
                                     (mouse-buffer-menu-alist buffers))))))

(defun mouse-buffer-menu-alist (buffers)
  (let (tail
	(maxlen 0)
	head)
    (setq buffers
	  (sort buffers
		(function (lambda (elt1 elt2)
			    (string< (buffer-name elt1) (buffer-name elt2))))))
    (setq tail buffers)
    (while tail
      (or (eq ?\s (aref (buffer-name (car tail)) 0))
	  (setq maxlen
		(max maxlen
		     (length (buffer-name (car tail))))))
      (setq tail (cdr tail)))
    (setq tail buffers)
    (while tail
      (let ((elt (car tail)))
	(if (/= (aref (buffer-name elt) 0) ?\s)
	    (setq head
		  (cons
		   (cons
		    (format
		     (format "%%-%ds  %%s%%s  %%s" maxlen)
		     (buffer-name elt)
		     (if (buffer-modified-p elt) "*" " ")
		     (with-current-buffer elt
		       (if buffer-read-only "%" " "))
		     (or (buffer-file-name elt)
			 (with-current-buffer elt
			   (if list-buffers-directory
			       (expand-file-name
				list-buffers-directory)))
			 ""))
		    elt)
		   head))))
      (setq tail (cdr tail)))
    ;; Compensate for the reversal that the above loop does.
    (nreverse head)))

(defun mouse-buffer-menu-split (title alist)
  ;; If we have lots of buffers, divide them into groups of 20
  ;; and make a pane (or submenu) for each one.
  (if (> (length alist) (/ (* mouse-buffer-menu-maxlen 3) 2))
      (let ((alist alist) sublists next
	    (i 1))
	(while alist
	  ;; Pull off the next mouse-buffer-menu-maxlen buffers
	  ;; and make them the next element of sublist.
	  (setq next (nthcdr mouse-buffer-menu-maxlen alist))
	  (if next
	      (setcdr (nthcdr (1- mouse-buffer-menu-maxlen) alist)
		      nil))
	  (setq sublists (cons (cons (format "Buffers %d" i) alist)
			       sublists))
	  (setq i (1+ i))
	  (setq alist next))
	(nreverse sublists))
    ;; Few buffers--put them all in one pane.
    (list (cons title alist))))

(define-obsolete-function-alias
  'mouse-choose-completion 'choose-completion "23.2")

;; Font selection.

(defun font-menu-add-default ()
  (let* ((default (frame-parameter nil 'font))
	 (font-alist x-fixed-font-alist)
	 (elt (or (assoc "Misc" font-alist) (nth 1 font-alist))))
    (if (assoc "Default" elt)
	(delete (assoc "Default" elt) elt))
    (setcdr elt
	    (cons (list "Default" default)
		  (cdr elt)))))

(defvar x-fixed-font-alist
  (list
   (purecopy "Font Menu")
   (cons
    (purecopy "Misc")
    (mapcar
     (lambda (arg) (cons  (purecopy (car arg)) (purecopy (cdr arg))))
     ;; For these, we specify the pixel height and width.
    '(("fixed" "fixed")
     ("6x10" "-misc-fixed-medium-r-normal--10-*-*-*-c-60-iso8859-1" "6x10")
     ("6x12"
      "-misc-fixed-medium-r-semicondensed--12-*-*-*-c-60-iso8859-1" "6x12")
     ("6x13"
      "-misc-fixed-medium-r-semicondensed--13-*-*-*-c-60-iso8859-1" "6x13")
     ("7x13" "-misc-fixed-medium-r-normal--13-*-*-*-c-70-iso8859-1" "7x13")
     ("7x14" "-misc-fixed-medium-r-normal--14-*-*-*-c-70-iso8859-1" "7x14")
     ("8x13" "-misc-fixed-medium-r-normal--13-*-*-*-c-80-iso8859-1" "8x13")
     ("9x15" "-misc-fixed-medium-r-normal--15-*-*-*-c-90-iso8859-1" "9x15")
     ("10x20" "-misc-fixed-medium-r-normal--20-*-*-*-c-100-iso8859-1" "10x20")
     ("11x18" "-misc-fixed-medium-r-normal--18-*-*-*-c-110-iso8859-1" "11x18")
     ("12x24" "-misc-fixed-medium-r-normal--24-*-*-*-c-120-iso8859-1" "12x24")
     ("")
     ("clean 5x8"
      "-schumacher-clean-medium-r-normal--8-*-*-*-c-50-iso8859-1")
     ("clean 6x8"
      "-schumacher-clean-medium-r-normal--8-*-*-*-c-60-iso8859-1")
     ("clean 8x8"
      "-schumacher-clean-medium-r-normal--8-*-*-*-c-80-iso8859-1")
     ("clean 8x10"
      "-schumacher-clean-medium-r-normal--10-*-*-*-c-80-iso8859-1")
     ("clean 8x14"
      "-schumacher-clean-medium-r-normal--14-*-*-*-c-80-iso8859-1")
     ("clean 8x16"
      "-schumacher-clean-medium-r-normal--16-*-*-*-c-80-iso8859-1")
     ("")
     ("sony 8x16" "-sony-fixed-medium-r-normal--16-*-*-*-c-80-iso8859-1")
     ;; We don't seem to have these; who knows what they are.
     ;; ("fg-18" "fg-18")
     ;; ("fg-25" "fg-25")
     ("lucidasanstypewriter-12" "-b&h-lucidatypewriter-medium-r-normal-sans-*-120-*-*-*-*-iso8859-1")
     ("lucidasanstypewriter-bold-14" "-b&h-lucidatypewriter-bold-r-normal-sans-*-140-*-*-*-*-iso8859-1")
     ("lucidasanstypewriter-bold-24"
      "-b&h-lucidatypewriter-bold-r-normal-sans-*-240-*-*-*-*-iso8859-1")
     ;; ("lucidatypewriter-bold-r-24" "-b&h-lucidatypewriter-bold-r-normal-sans-24-240-75-75-m-140-iso8859-1")
     ;; ("fixed-medium-20" "-misc-fixed-medium-*-*-*-20-*-*-*-*-*-*-*")
     )))

   (cons
    (purecopy "Courier")
    (mapcar
     (lambda (arg) (cons  (purecopy (car arg)) (purecopy (cdr arg))))
     ;; For these, we specify the point height.
     '(("8" "-adobe-courier-medium-r-normal--*-80-*-*-m-*-iso8859-1")
     ("10" "-adobe-courier-medium-r-normal--*-100-*-*-m-*-iso8859-1")
     ("12" "-adobe-courier-medium-r-normal--*-120-*-*-m-*-iso8859-1")
     ("14" "-adobe-courier-medium-r-normal--*-140-*-*-m-*-iso8859-1")
     ("18" "-adobe-courier-medium-r-normal--*-180-*-*-m-*-iso8859-1")
     ("24" "-adobe-courier-medium-r-normal--*-240-*-*-m-*-iso8859-1")
     ("8 bold" "-adobe-courier-bold-r-normal--*-80-*-*-m-*-iso8859-1")
     ("10 bold" "-adobe-courier-bold-r-normal--*-100-*-*-m-*-iso8859-1")
     ("12 bold" "-adobe-courier-bold-r-normal--*-120-*-*-m-*-iso8859-1")
     ("14 bold" "-adobe-courier-bold-r-normal--*-140-*-*-m-*-iso8859-1")
     ("18 bold" "-adobe-courier-bold-r-normal--*-180-*-*-m-*-iso8859-1")
     ("24 bold" "-adobe-courier-bold-r-normal--*-240-*-*-m-*-iso8859-1")
     ("8 slant" "-adobe-courier-medium-o-normal--*-80-*-*-m-*-iso8859-1")
     ("10 slant" "-adobe-courier-medium-o-normal--*-100-*-*-m-*-iso8859-1")
     ("12 slant" "-adobe-courier-medium-o-normal--*-120-*-*-m-*-iso8859-1")
     ("14 slant" "-adobe-courier-medium-o-normal--*-140-*-*-m-*-iso8859-1")
     ("18 slant" "-adobe-courier-medium-o-normal--*-180-*-*-m-*-iso8859-1")
     ("24 slant" "-adobe-courier-medium-o-normal--*-240-*-*-m-*-iso8859-1")
     ("8 bold slant" "-adobe-courier-bold-o-normal--*-80-*-*-m-*-iso8859-1")
     ("10 bold slant" "-adobe-courier-bold-o-normal--*-100-*-*-m-*-iso8859-1")
     ("12 bold slant" "-adobe-courier-bold-o-normal--*-120-*-*-m-*-iso8859-1")
     ("14 bold slant" "-adobe-courier-bold-o-normal--*-140-*-*-m-*-iso8859-1")
     ("18 bold slant" "-adobe-courier-bold-o-normal--*-180-*-*-m-*-iso8859-1")
     ("24 bold slant" "-adobe-courier-bold-o-normal--*-240-*-*-m-*-iso8859-1")
    ))))
  "X fonts suitable for use in Emacs.")

(declare-function generate-fontset-menu "fontset" ())

(defun mouse-select-font ()
  "Prompt for a font name, using `x-popup-menu', and return it."
  (interactive)
  (unless (display-multi-font-p)
    (error "Cannot change fonts on this display"))
  (car
   (x-popup-menu
    (if (listp last-nonmenu-event)
	last-nonmenu-event
      (list '(0 0) (selected-window)))
    (append x-fixed-font-alist
	    (list (generate-fontset-menu))))))

(declare-function text-scale-mode "face-remap")

(defun mouse-set-font (&rest fonts)
  "Set the default font for the selected frame.
The argument FONTS is a list of font names; the first valid font
in this list is used.

When called interactively, pop up a menu and allow the user to
choose a font."
  (interactive
   (progn (unless (display-multi-font-p)
	    (error "Cannot change fonts on this display"))
	  (x-popup-menu
	   (if (listp last-nonmenu-event)
	       last-nonmenu-event
	     (list '(0 0) (selected-window)))
	   ;; Append list of fontsets currently defined.
	   (append x-fixed-font-alist (list (generate-fontset-menu))))))
  (if fonts
      (let (font)
	(while fonts
	  (condition-case nil
	      (progn
		(set-frame-font (car fonts))
		(setq font (car fonts))
		(setq fonts nil))
	    (error
	     (setq fonts (cdr fonts)))))
	(if (null font)
	    (error "Font not found")))))

(defvar mouse-appearance-menu-map nil)
(declare-function x-select-font "xfns.c" (&optional frame ignored)) ; USE_GTK
(declare-function buffer-face-mode-invoke "face-remap"
                  (face arg &optional interactive))
(declare-function font-face-attributes "font.c" (font &optional frame))
(defvar w32-use-w32-font-dialog)
(defvar w32-fixed-font-alist)

(defun mouse-appearance-menu (event)
  "Show a menu for changing the default face in the current buffer."
  (interactive "@e")
  (require 'face-remap)
  (when (display-multi-font-p)
    (with-selected-window (car (event-start event))
      (if mouse-appearance-menu-map
	  nil ; regenerate new fonts
	;; Initialize mouse-appearance-menu-map
	(setq mouse-appearance-menu-map
	      (make-sparse-keymap "Change Default Buffer Face"))
	(define-key mouse-appearance-menu-map [face-remap-reset-base]
	  '(menu-item "Reset to Default" face-remap-reset-base))
	(define-key mouse-appearance-menu-map [text-scale-decrease]
	  '(menu-item "Decrease Buffer Text Size" text-scale-decrease))
	(define-key mouse-appearance-menu-map [text-scale-increase]
	  '(menu-item "Increase Buffer Text Size" text-scale-increase))
	;; Font selector
	(if (and (functionp 'x-select-font)
		 (or (not (boundp 'w32-use-w32-font-dialog))
		     w32-use-w32-font-dialog))
	    (define-key mouse-appearance-menu-map [x-select-font]
	      '(menu-item "Change Buffer Font..." x-select-font))
	  ;; If the select-font is unavailable, construct a menu.
	  (let ((font-submenu (make-sparse-keymap "Change Text Font"))
		(font-alist (cdr (append
				  (if (eq system-type 'windows-nt)
				      w32-fixed-font-alist
				    x-fixed-font-alist)
				  (list (generate-fontset-menu))))))
	    (dolist (family font-alist)
	      (let* ((submenu-name (car family))
		     (submenu-map (make-sparse-keymap submenu-name)))
		(dolist (font (cdr family))
		  (let ((font-name (car font))
			font-symbol)
		    (if (string= font-name "")
			(define-key submenu-map [space]
			  '("--"))
		      (setq font-symbol (intern (cadr font)))
		      (define-key submenu-map (vector font-symbol)
			(list 'menu-item (car font) font-symbol)))))
		(define-key font-submenu (vector (intern submenu-name))
		  (list 'menu-item submenu-name submenu-map))))
	    (define-key mouse-appearance-menu-map [font-submenu]
	      (list 'menu-item "Change Text Font" font-submenu)))))
      (let ((choice (x-popup-menu event mouse-appearance-menu-map)))
	(setq choice (nth (1- (length choice)) choice))
	(cond ((eq choice 'text-scale-increase)
	       (text-scale-increase 1))
	      ((eq choice 'text-scale-decrease)
	       (text-scale-increase -1))
	      ((eq choice 'face-remap-reset-base)
	       (text-scale-mode 0)
	       (buffer-face-mode 0))
	      (choice
	       ;; Either choice == 'x-select-font, or choice is a
	       ;; symbol whose name is a font.
	       (let ((font (if (eq choice 'x-select-font)
			       (x-select-font)
			     (symbol-name choice))))
		 (buffer-face-mode-invoke
		  (if (fontp font 'font-spec)
		      (list :font font)
		    (font-face-attributes font))
		  t (called-interactively-p 'interactive)))))))))


;; Drag and drop support.
(defcustom mouse-drag-and-drop-region nil
  "If non-nil, dragging the mouse drags the region, if it exists.
If the value is a modifier, such as `control' or `shift' or
`meta', then if that modifier key is pressed when dropping the
region, text is copied instead of being cut."
  :type `(choice
          (const :tag "Disable dragging the region" nil)
          ,@(mapcar
             (lambda (modifier)
               `(const :tag ,(format "Enable, but copy with the %s modifier"
                                     modifier)
                       modifier))
             '(alt super hyper shift control meta))
          (other :tag "Enable dragging the region" t))
  :version "26.1")

(defcustom mouse-drag-and-drop-region-cut-when-buffers-differ nil
  "If non-nil, cut text also when source and destination buffers differ.
If this option is nil, `mouse-drag-and-drop-region' will leave
the text in the source buffer alone when dropping it in a
different buffer.  If this is non-nil, it will cut the text just
as it does when dropping text in the source buffer."
  :type 'boolean
  :version "26.1")

(defcustom mouse-drag-and-drop-region-show-tooltip 256
  "If non-nil, text is shown by a tooltip in a graphic display.
If this option is nil, `mouse-drag-and-drop-region' does not show
tooltips.  If this is t, it shows the entire text dragged in a
tooltip.  If this is an integer (as with the default value of
256), it will show that many characters of the dragged text in
a tooltip."
  :type 'integer
  :version "26.1")

(defcustom mouse-drag-and-drop-region-show-cursor t
  "If non-nil, move point with mouse cursor during dragging.
If this is nil, `mouse-drag-and-drop-region' leaves point alone.
Otherwise, it will move point together with the mouse cursor and,
in addition, temporarily highlight the original region with the
`mouse-drag-and-drop-region' face."
  :type 'boolean
  :version "26.1")

(defface mouse-drag-and-drop-region '((t :inherit region))
  "Face to highlight original text during dragging.
This face is used by `mouse-drag-and-drop-region' to temporarily
highlight the original region when
`mouse-drag-and-drop-region-show-cursor' is non-nil."
  :version "26.1")

(declare-function rectangle-dimensions "rect" (start end))
(declare-function rectangle-position-as-coordinates "rect" (position))
(declare-function rectangle-intersect-p "rect" (pos1 size1 pos2 size2))

(defun mouse-drag-and-drop-region (event)
  "Move text in the region to point where mouse is dragged to.
The transportation of text is also referred as `drag and drop'.
When text is dragged over to a different buffer, or if a
modifier key was pressed when dropping, and the value of the
variable `mouse-drag-and-drop-region' is that modifier, the text
is copied instead of being cut."
  (interactive "e")
  (let* ((mouse-button (event-basic-type last-input-event))
         (mouse-drag-and-drop-region-show-tooltip
          (when (and mouse-drag-and-drop-region-show-tooltip
                     (display-multi-frame-p)
                     (require 'tooltip))
            mouse-drag-and-drop-region-show-tooltip))
         (start (region-beginning))
         (end (region-end))
         (point (point))
         (buffer (current-buffer))
         (window (selected-window))
         (text-from-read-only buffer-read-only)
         ;; Use multiple overlays to cover cases where the region has more
         ;; than one boundary.
         (mouse-drag-and-drop-overlays (mapcar (lambda (bounds)
                                                 (make-overlay (car bounds)
                                                               (cdr bounds)))
                                               (region-bounds)))
         (region-noncontiguous (region-noncontiguous-p))
         point-to-paste
         point-to-paste-read-only
         window-to-paste
         buffer-to-paste
         cursor-in-text-area
         no-modifier-on-drop
         drag-but-negligible
         clicked
         value-selection    ; This remains nil when event was "click".
         text-tooltip
         states
         window-exempt)

    ;; STATES stores for each window on this frame its start and point
    ;; positions so we can restore them on all windows but for the one
    ;; where the drop occurs.  For inter-frame drags we'll have to do
    ;; this for all windows on all visible frames.  In addition we save
    ;; also the cursor type for the window's buffer so we can restore it
    ;; in case we modified it.
    ;; https://lists.gnu.org/archive/html/emacs-devel/2017-12/msg00090.html
    (walk-window-tree
     (lambda (window)
       (setq states
             (cons
              (list
               window
               (copy-marker (window-start window))
               (copy-marker (window-point window))
               (with-current-buffer (window-buffer window)
                 cursor-type))
              states))))

    (ignore-errors
      (track-mouse
        (setq track-mouse 'dropping)
        ;; When event was "click" instead of "drag", skip loop.
        (while (progn
                 (setq event (read-key))      ; read-event or read-key
                 (or (mouse-movement-p event)
                     ;; Handle `mouse-autoselect-window'.
                     (memq (car event) '(select-window switch-frame))))
          ;; Obtain the dragged text in region.  When the loop was
          ;; skipped, value-selection remains nil.
          (unless value-selection
            (setq value-selection (funcall region-extract-function nil))
            (when mouse-drag-and-drop-region-show-tooltip
              (let ((text-size mouse-drag-and-drop-region-show-tooltip))
                (setq text-tooltip
                      (if (and (integerp text-size)
                               (> (length value-selection) text-size))
                          (concat
                           (substring value-selection 0 (/ text-size 2))
                           "\n...\n"
                           (substring value-selection (- (/ text-size 2)) -1))
                        value-selection))))

            ;; Check if selected text is read-only.
            (setq text-from-read-only
                  (or text-from-read-only
                      (catch 'loop
                        (dolist (bound (region-bounds))
                          (when (text-property-not-all
                                 (car bound) (cdr bound) 'read-only nil)
                            (throw 'loop t)))))))

          (setq window-to-paste (posn-window (event-end event)))
          (setq point-to-paste (posn-point (event-end event)))
          ;; Set nil when target buffer is minibuffer.
          (setq buffer-to-paste (let (buf)
                                  (when (windowp window-to-paste)
                                    (setq buf (window-buffer window-to-paste))
                                    (when (not (minibufferp buf))
                                      buf))))
          (setq cursor-in-text-area (and window-to-paste
                                         point-to-paste
                                         buffer-to-paste))

          (when cursor-in-text-area
            ;; Check if point under mouse is read-only.
            (save-window-excursion
              (select-window window-to-paste)
              (setq point-to-paste-read-only
                    (or buffer-read-only
                        (get-text-property point-to-paste 'read-only))))

            ;; Check if "drag but negligible".  Operation "drag but
            ;; negligible" is defined as drag-and-drop the text to
            ;; the original region.  When modifier is pressed, the
            ;; text will be inserted to inside of the original
            ;; region.
            ;;
            ;; If the region is rectangular, check if the newly inserted
            ;; rectangular text would intersect the already selected
            ;; region. If it would, then set "drag-but-negligible" to t.
            ;; As a special case, allow dragging the region freely anywhere
            ;; to the left, as this will never trigger its contents to be
            ;; inserted into the overlays tracking it.
            (setq drag-but-negligible
                  (and (eq (overlay-buffer (car mouse-drag-and-drop-overlays))
                           buffer-to-paste)
                       (if region-noncontiguous
                           (let ((dimensions (rectangle-dimensions start end))
                                 (start-coordinates
                                  (rectangle-position-as-coordinates start))
                                 (point-to-paste-coordinates
                                  (rectangle-position-as-coordinates
                                   point-to-paste)))
                             (and (rectangle-intersect-p
                                   start-coordinates dimensions
                                   point-to-paste-coordinates dimensions)
                                  (not (< (car point-to-paste-coordinates)
                                           (car start-coordinates)))))
                         (and (<= (overlay-start
                                   (car mouse-drag-and-drop-overlays))
                                  point-to-paste)
                              (<= point-to-paste
                                  (overlay-end
                                   (car mouse-drag-and-drop-overlays))))))))

          ;; Show a tooltip.
          (if mouse-drag-and-drop-region-show-tooltip
              (tooltip-show text-tooltip)
            (tooltip-hide))

          ;; Show cursor and highlight the original region.
          (when mouse-drag-and-drop-region-show-cursor
            ;; Modify cursor even when point is out of frame.
            (setq cursor-type (cond
                               ((not cursor-in-text-area)
                                nil)
                               ((or point-to-paste-read-only
                                    drag-but-negligible)
                                'hollow)
                               (t
                                'bar)))
            (when cursor-in-text-area
              (dolist (overlay mouse-drag-and-drop-overlays)
                (overlay-put overlay
                           'face 'mouse-drag-and-drop-region))
              (deactivate-mark)     ; Maintain region in other window.
              (mouse-set-point event)))))

      ;; Hide a tooltip.
      (when mouse-drag-and-drop-region-show-tooltip (tooltip-hide))

      ;; Check if modifier was pressed on drop.
      (setq no-modifier-on-drop
            (not (member mouse-drag-and-drop-region (event-modifiers event))))

      ;; Check if event was "click".
      (setq clicked (not value-selection))

      ;; Restore status on drag to outside of text-area or non-mouse input.
      (when (or (not cursor-in-text-area)
                (not (equal (event-basic-type event) mouse-button)))
        (setq drag-but-negligible t
              no-modifier-on-drop t))

      ;; Do not modify any buffers when event is "click",
      ;; "drag but negligible", or "drag to read-only".
      (let* ((mouse-drag-and-drop-region-cut-when-buffers-differ
              (if no-modifier-on-drop
                  mouse-drag-and-drop-region-cut-when-buffers-differ
                (not mouse-drag-and-drop-region-cut-when-buffers-differ)))
             (wanna-paste-to-same-buffer (equal buffer-to-paste buffer))
             (wanna-cut-on-same-buffer (and wanna-paste-to-same-buffer
                                            no-modifier-on-drop))
             (wanna-cut-on-other-buffer
              (and (not wanna-paste-to-same-buffer)
                   mouse-drag-and-drop-region-cut-when-buffers-differ))
             (cannot-paste (or point-to-paste-read-only
                               (when (or wanna-cut-on-same-buffer
                                         wanna-cut-on-other-buffer)
                                 text-from-read-only))))

        (cond
         ;; Move point within region.
         (clicked
          (deactivate-mark)
          (mouse-set-point event))
         ;; Undo operation. Set back the original text as region.
         ((or (and drag-but-negligible
                   no-modifier-on-drop)
              cannot-paste)
          ;; Inform user either source or destination buffer cannot be modified.
          (when (and (not drag-but-negligible)
                     cannot-paste)
            (message "Buffer is read-only"))

          ;; Select source window back and restore region.
          ;; (set-window-point window point)
          (select-window window)
          (goto-char point)
          (setq deactivate-mark nil)
          (activate-mark)
          (when region-noncontiguous
            (rectangle-mark-mode)))
         ;; Modify buffers.
         (t
          ;; * DESTINATION BUFFER::
          ;; Insert the text to destination buffer under mouse.
          (select-window window-to-paste)
          (setq window-exempt window-to-paste)
          (goto-char point-to-paste)
          (push-mark)
          (insert-for-yank value-selection)

          ;; On success, set the text as region on destination buffer.
          (when (not (equal (mark) (point)))
            (setq deactivate-mark nil)
            (activate-mark)
            (when region-noncontiguous
              (rectangle-mark-mode)))

          ;; * SOURCE BUFFER::
          ;; Set back the original text as region or delete the original
          ;; text, on source buffer.
          (if wanna-paste-to-same-buffer
              ;; When source buffer and destination buffer are the same,
              ;; remove the original text.
              (when no-modifier-on-drop
                (let (deactivate-mark)
                  (dolist (overlay mouse-drag-and-drop-overlays)
                    (delete-region (overlay-start overlay)
                                   (overlay-end overlay)))))
            ;; When source buffer and destination buffer are different,
            ;; keep (set back the original text as region) or remove the
            ;; original text.
            (select-window window) ; Select window with source buffer.
            (goto-char point) ; Move point to the original text on source buffer.

            (if mouse-drag-and-drop-region-cut-when-buffers-differ
                ;; Remove the dragged text from source buffer like
                ;; operation `cut'.
                (dolist (overlay mouse-drag-and-drop-overlays)
                    (delete-region (overlay-start overlay)
                                   (overlay-end overlay)))
              ;; Set back the dragged text as region on source buffer
              ;; like operation `copy'.
              (activate-mark))
            (select-window window-to-paste))))))

    ;; Clean up.
    (dolist (overlay mouse-drag-and-drop-overlays)
      (delete-overlay overlay))

    ;; Restore old states but for the window where the drop
    ;; occurred. Restore cursor types for all windows.
    (dolist (state states)
      (let ((window (car state)))
        (when (and window-exempt
                   (not (eq window window-exempt)))
          (set-window-start window (nth 1 state) 'noforce)
          (set-marker (nth 1 state) nil)
          ;; If window is selected, the following automatically sets
          ;; point for that window's buffer.
          (set-window-point window (nth 2 state))
          (set-marker (nth 2 state) nil))
        (with-current-buffer (window-buffer window)
          (setq cursor-type (nth 3 state)))))))


;;; Bindings for mouse commands.

(global-set-key [down-mouse-1]	'mouse-drag-region)
(global-set-key [mouse-1]	'mouse-set-point)
(global-set-key [drag-mouse-1]	'mouse-set-region)

(defun mouse--strip-first-event (_prompt)
  (substring (this-single-command-raw-keys) 1))

(define-key function-key-map [left-fringe mouse-1] 'mouse--strip-first-event)
(define-key function-key-map [right-fringe mouse-1] 'mouse--strip-first-event)

(global-set-key [mouse-2]	'mouse-yank-primary)
;; Allow yanking also when the corresponding cursor is "in the fringe".
(define-key function-key-map [right-fringe mouse-2] 'mouse--strip-first-event)
(define-key function-key-map [left-fringe mouse-2] 'mouse--strip-first-event)
(global-set-key [mouse-3]	'mouse-save-then-kill)
(define-key function-key-map [right-fringe mouse-3] 'mouse--strip-first-event)
(define-key function-key-map [left-fringe mouse-3] 'mouse--strip-first-event)

;; By binding these to down-going events, we let the user use the up-going
;; event to make the selection, saving a click.
(global-set-key [C-down-mouse-1] 'mouse-buffer-menu)
(if (not (eq system-type 'ms-dos))
    (global-set-key [S-down-mouse-1] 'mouse-appearance-menu))
;; C-down-mouse-2 is bound in facemenu.el.
(global-set-key [C-down-mouse-3]
  `(menu-item ,(purecopy "Menu Bar") ignore
    :filter (lambda (_)
              (if (zerop (or (frame-parameter nil 'menu-bar-lines) 0))
                  (mouse-menu-bar-map)
                (mouse-menu-major-mode-map)))))

;; Binding mouse-1 to mouse-select-window when on mode-, header-, or
;; vertical-line prevents Emacs from signaling an error when the mouse
;; button is released after dragging these lines, on non-toolkit
;; versions.
(global-set-key [header-line down-mouse-1] 'mouse-drag-header-line)
(global-set-key [header-line mouse-1] 'mouse-select-window)
(global-set-key [tab-line mouse-1] 'mouse-select-window)
;; (global-set-key [mode-line drag-mouse-1] 'mouse-select-window)
(global-set-key [mode-line down-mouse-1] 'mouse-drag-mode-line)
(global-set-key [mode-line mouse-1] 'mouse-select-window)
(global-set-key [mode-line mouse-2] 'mouse-delete-other-windows)
(global-set-key [mode-line mouse-3] 'mouse-delete-window)
(global-set-key [mode-line C-mouse-2] 'mouse-split-window-horizontally)
(global-set-key [vertical-scroll-bar C-mouse-2] 'mouse-split-window-vertically)
(global-set-key [horizontal-scroll-bar C-mouse-2] 'mouse-split-window-horizontally)
(global-set-key [vertical-line down-mouse-1] 'mouse-drag-vertical-line)
(global-set-key [vertical-line mouse-1] 'mouse-select-window)
(global-set-key [vertical-line C-mouse-2] 'mouse-split-window-vertically)
(global-set-key [right-divider down-mouse-1] 'mouse-drag-vertical-line)
(global-set-key [right-divider mouse-1] 'ignore)
(global-set-key [right-divider C-mouse-2] 'mouse-split-window-vertically)
(global-set-key [bottom-divider down-mouse-1] 'mouse-drag-mode-line)
(global-set-key [bottom-divider mouse-1] 'ignore)
(global-set-key [bottom-divider C-mouse-2] 'mouse-split-window-horizontally)
(global-set-key [left-edge down-mouse-1] 'mouse-drag-left-edge)
(global-set-key [left-edge mouse-1] 'ignore)
(global-set-key [top-left-corner down-mouse-1] 'mouse-drag-top-left-corner)
(global-set-key [top-left-corner mouse-1] 'ignore)
(global-set-key [top-edge down-mouse-1] 'mouse-drag-top-edge)
(global-set-key [top-edge mouse-1] 'ignore)
(global-set-key [top-right-corner down-mouse-1] 'mouse-drag-top-right-corner)
(global-set-key [top-right-corner mouse-1] 'ignore)
(global-set-key [right-edge down-mouse-1] 'mouse-drag-right-edge)
(global-set-key [right-edge mouse-1] 'ignore)
(global-set-key [bottom-right-corner down-mouse-1] 'mouse-drag-bottom-right-corner)
(global-set-key [bottom-right-corner mouse-1] 'ignore)
(global-set-key [bottom-edge down-mouse-1] 'mouse-drag-bottom-edge)
(global-set-key [bottom-edge mouse-1] 'ignore)
(global-set-key [bottom-left-corner down-mouse-1] 'mouse-drag-bottom-left-corner)
(global-set-key [bottom-left-corner mouse-1] 'ignore)

(provide 'mouse)

;;; mouse.el ends here
