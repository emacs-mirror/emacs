;;; frameset.el --- save and restore frame and window setup -*- lexical-binding: t -*-

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Author: Juanma Barranquero <lekktu@gmail.com>
;; Keywords: convenience

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides a set of operations to save a frameset (the state
;; of all or a subset of the existing frames and windows), both
;; in-session and persistently, and restore it at some point in the
;; future.
;;
;; It should be noted that restoring the frames' windows depends on
;; the buffers they are displaying, but this package does not provide
;; any way to save and restore sets of buffers (see desktop.el for
;; that).  So, it's up to the user of frameset.el to make sure that
;; any relevant buffer is loaded before trying to restore a frameset.
;; When a window is restored and a buffer is missing, the window will
;; be deleted unless it is the last one in the frame, in which case
;; some previous buffer will be shown instead.

;;; Code:

(require 'cl-lib)


;; Framesets have two fields:
;; - properties: a property list to store both frameset-specific and
;;   user-defined serializable data.  Currently defined properties
;;   include:
;;     :version ID  - Identifies the version of the frameset struct;
;;                    this is the only property always present and
;;                    must not be modified.
;;     :app APPINFO - Freeform.  Can be used by applications and
;;                    packages to indicate the intended (but by no
;;                    means exclusive) use of the frameset.  For
;;                    example, currently desktop.el sets :app to
;;                    `(desktop . ,desktop-file-version).
;;     :name NAME   - The name of the frameset instance; a string.
;;     :desc TEXT   - A description for user consumption (to choose
;; 		      among framesets, etc.); a string.
;; - states: an alist of items (FRAME-PARAMETERS . WINDOW-STATE) in
;;   no particular order.  Each item represents a frame to be
;;   restored.

(cl-defstruct (frameset (:type list) :named
			(:copier nil)
			(:predicate nil))
  properties   ;; property list
  states)      ;; list of conses (frame-state . window-state)

(defun copy-frameset (frameset)
  "Return a copy of FRAMESET.
This is a deep copy done with `copy-tree'."
  (copy-tree frameset t))

;;;###autoload
(defun frameset-p (frameset)
  "If FRAMESET is a frameset, return its :version.
Else return nil."
  (and (eq (car-safe frameset) 'frameset)
       (plist-get (cl-second frameset) :version)))

;; A setf'able accessor to the frameset's properties
(defun frameset-prop (frameset prop)
  "Return the value of the PROP property of FRAMESET.

Properties other than :version can be set with

  (setf (frameset-prop FRAMESET PROP) NEW-VALUE)"
  (plist-get (frameset-properties frameset) prop))

(gv-define-setter frameset-prop (val fs prop)
  (macroexp-let2 nil v val
    `(progn
       (cl-assert (not (eq ,prop :version)) t ":version can not be set")
       (setf (frameset-properties ,fs)
	     (plist-put (frameset-properties ,fs) ,prop ,v))
       ,v)))


;; Filtering

(defvar frameset-filter-alist
  '((background-color	 . frameset-filter-sanitize-color)
    (buffer-list	 . t)
    (buffer-predicate	 . t)
    (buried-buffer-list	 . t)
    (font		 . frameset-filter-save-parm)
    (foreground-color	 . frameset-filter-sanitize-color)
    (fullscreen		 . frameset-filter-save-parm)
    (GUI:font		 . frameset-filter-restore-parm)
    (GUI:fullscreen	 . frameset-filter-restore-parm)
    (GUI:height		 . frameset-filter-restore-parm)
    (GUI:width		 . frameset-filter-restore-parm)
    (height		 . frameset-filter-save-parm)
    (left		 . frameset-filter-iconified)
    (minibuffer		 . frameset-filter-minibuffer)
    (top		 . frameset-filter-iconified)
    (width		 . frameset-filter-save-parm))
  "Alist of frame parameters and filtering functions.

Each element is a cons (PARAM . ACTION), where PARAM is a parameter
name (a symbol identifying a frame parameter), and ACTION can be:

 t         The parameter is always removed from the parameter list.
 :save     The parameter is removed when saving the frame.
 :restore  The parameter is removed when restoring the frame.
 FILTER    A filter function.

FILTER can be a symbol FILTER-FUN, or a list (FILTER-FUN ARGS...).
It will be called with four arguments CURRENT, FILTERED, PARAMETERS
and SAVING, plus any additional ARGS:

 CURRENT     A cons (PARAM . VALUE), where PARAM is the one being
             filtered and VALUE is its current value.
 FILTERED    The alist of parameters filtered so far.
 PARAMETERS  The complete alist of parameters being filtered,
 SAVING      Non-nil if filtering before saving state, nil otherwise.

The FILTER-FUN function must return:
 nil                  CURRENT is removed from the list.
 t                    CURRENT is left as is.
 (PARAM' . VALUE')    Replace CURRENT with this.

Frame parameters not on this list are passed intact.")

(defvar frameset--target-display nil
  ;; Either (minibuffer . VALUE) or nil.
  ;; This refers to the current frame config being processed inside
  ;; `frame--restore-frames' and its auxiliary functions (like filtering).
  ;; If nil, there is no need to change the display.
  ;; If non-nil, display parameter to use when creating the frame.
  "Internal use only.")

(defun frameset-switch-to-gui-p (parameters)
  "True when switching to a graphic display.
Return t if PARAMETERS describes a text-only terminal and
the target is a graphic display; otherwise return nil.
Only meaningful when called from a filtering function in
`frameset-filter-alist'."
  (and frameset--target-display			    ; we're switching
       (null (cdr (assq 'display parameters)))	    ; from a tty
       (cdr frameset--target-display)))		    ; to a GUI display

(defun frameset-switch-to-tty-p (parameters)
  "True when switching to a text-only terminal.
Return t if PARAMETERS describes a graphic display and
the target is a text-only terminal; otherwise return nil.
Only meaningful when called from a filtering function in
`frameset-filter-alist'."
  (and frameset--target-display			  ; we're switching
       (cdr (assq 'display parameters))	          ; from a GUI display
       (null (cdr frameset--target-display))))	  ; to a tty

(defun frameset-filter-sanitize-color (current _filtered parameters saving)
  "When switching to a GUI frame, remove \"unspecified\" colors.
Useful as a filter function for tty-specific parameters."
  (or saving
      (not (frameset-switch-to-gui-p parameters))
      (not (stringp (cdr current)))
      (not (string-match-p "^unspecified-[fb]g$" (cdr current)))))

(defun frameset-filter-minibuffer (current _filtered _parameters saving)
  "When saving, convert (minibuffer . #<window>) parameter to (minibuffer . t)."
  (or (not saving)
      (if (windowp (cdr current))
	  '(minibuffer . t)
	t)))

(defun frameset-filter-save-parm (current _filtered parameters saving
					  &optional prefix)
  "When switching to a tty frame, save parameter P as PREFIX:P.
The parameter can be later restored with `frameset-filter-restore-parm'.
PREFIX defaults to `GUI'."
  (unless prefix (setq prefix 'GUI))
  (cond (saving t)
	((frameset-switch-to-tty-p parameters)
	 (let ((prefix:p (intern (format "%s:%s" prefix (car current)))))
	   (if (assq prefix:p parameters)
	       nil
	     (cons prefix:p (cdr current)))))
	((frameset-switch-to-gui-p parameters)
	 (not (assq (intern (format "%s:%s" prefix (car current))) parameters)))
	(t t)))

(defun frameset-filter-restore-parm (current filtered parameters saving)
  "When switching to a GUI frame, restore PREFIX:P parameter as P.
CURRENT must be of the form (PREFIX:P . value)."
  (or saving
      (not (frameset-switch-to-gui-p parameters))
      (let* ((prefix:p (symbol-name (car current)))
	     (p (intern (substring prefix:p
				   (1+ (string-match-p ":" prefix:p)))))
	     (val (cdr current))
	     (found (assq p filtered)))
	(if (not found)
	    (cons p val)
	  (setcdr found val)
	  nil))))

(defun frameset-filter-iconified (_current _filtered parameters saving)
  "Remove CURRENT when saving an iconified frame.
This is used for positions parameters `left' and `top', which are
meaningless in an iconified frame, so the frame is restored in a
default position."
  (not (and saving (eq (cdr (assq 'visibility parameters)) 'icon))))

(defun frameset-keep-original-display-p (force-display)
  "True if saved frames' displays should be honored."
  (cond ((daemonp) t)
	((eq system-type 'windows-nt) nil)
	(t (null force-display))))

(defun frameset-filter-params (parameters filter-alist saving)
  "Filter parameter list PARAMETERS and return a filtered list.
FILTER-ALIST is an alist of parameter filters, in the format of
`frameset-filter-alist' (which see).
SAVING is non-nil while filtering parameters to save a frameset,
nil while the filtering is done to restore it."
  (let ((filtered nil))
    (dolist (current parameters)
      (pcase (cdr (assq (car current) filter-alist))
	(`nil
	 (push current filtered))
	(`t
	 nil)
	(:save
	 (unless saving (push current filtered)))
	(:restore
	 (when saving (push current filtered)))
	((or `(,fun . ,args) (and fun (pred fboundp)))
	 (let ((this (apply fun current filtered parameters saving args)))
	   (when this
	     (push (if (eq this t) current this) filtered))))
	(other
	 (delay-warning 'frameset (format "Unknown filter %S" other) :error))))
    ;; Set the display parameter after filtering, so that filter functions
    ;; have access to its original value.
    (when frameset--target-display
      (let ((display (assq 'display filtered)))
	(if display
	    (setcdr display (cdr frameset--target-display))
	  (push frameset--target-display filtered))))
    filtered))


;; Saving framesets

(defun frameset--set-id (frame)
  "Set FRAME's `frameset-id' if not yet set.
Internal use only."
  (unless (frame-parameter frame 'frameset-id)
    (set-frame-parameter frame
			 'frameset-id
			 (mapconcat (lambda (n) (format "%04X" n))
				    (cl-loop repeat 4 collect (random 65536))
				    "-"))))

(defun frameset--process-minibuffer-frames (frame-list)
  "Process FRAME-LIST and record minibuffer relationships.
FRAME-LIST is a list of frames."
  ;; Record frames with their own minibuffer
  (dolist (frame (minibuffer-frame-list))
    (when (memq frame frame-list)
      (frameset--set-id frame)
      ;; For minibuffer-owning frames, frameset--mini is a cons
      ;; (t . DEFAULT?), where DEFAULT? is a boolean indicating whether
      ;; the frame is the one pointed out by `default-minibuffer-frame'.
      (set-frame-parameter frame
			   'frameset--mini
			   (cons t (eq frame default-minibuffer-frame)))))
  ;; Now link minibufferless frames with their minibuffer frames
  (dolist (frame frame-list)
    (unless (frame-parameter frame 'frameset--mini)
      (frameset--set-id frame)
      (let* ((mb-frame (window-frame (minibuffer-window frame)))
	     (id (and mb-frame (frame-parameter mb-frame 'frameset-id))))
	(if (null id)
	    (error "Minibuffer frame %S for %S is excluded" mb-frame frame)
	  ;; For minibufferless frames, frameset--mini is a cons
	  ;; (nil . FRAME-ID), where FRAME-ID is the frameset-id of
	  ;; the frame containing its minibuffer window.
	  (set-frame-parameter frame
			       'frameset--mini
			       (cons nil id)))))))

;;;###autoload
(cl-defun frameset-save (frame-list &key filters predicate properties)
  "Return the frameset of FRAME-LIST, a list of frames.
If nil, FRAME-LIST defaults to all live frames.
FILTERS is an alist of parameter filters; defaults to `frameset-filter-alist'.
PREDICATE is a predicate function, which must return non-nil for frames that
should be saved; it defaults to saving all frames from FRAME-LIST.
PROPERTIES is a user-defined property list to add to the frameset."
  (let ((frames (cl-delete-if-not #'frame-live-p
				  (cl-delete-if-not (or predicate #'framep)
						    (or (copy-sequence frame-list)
							(frame-list))))))
    (frameset--process-minibuffer-frames frames)
    (make-frameset :properties (append '(:version 1) properties)
		   :states (mapcar
			    (lambda (frame)
			      (cons
			       (frameset-filter-params (frame-parameters frame)
						       (or filters
							   frameset-filter-alist)
						       t)
			       (window-state-get (frame-root-window frame) t)))
			    frames))))


;; Restoring framesets

(defvar frameset--reuse-list nil
  "Internal use only.")

(defun frameset--compute-pos (value left/top right/bottom)
  (pcase value
    (`(+ ,val) (+ left/top val))
    (`(- ,val) (+ right/bottom val))
    (val val)))

(defun frameset--move-onscreen (frame force-onscreen)
  "If FRAME is offscreen, move it back onscreen and, if necessary, resize it.
For the description of FORCE-ONSCREEN, see `frameset-restore'.
When forced onscreen, frames wider than the monitor's workarea are converted
to fullwidth, and frames taller than the workarea are converted to fullheight.
NOTE: This only works for non-iconified frames.  Internal use only."
  (pcase-let* ((`(,left ,top ,width ,height) (cl-cdadr (frame-monitor-attributes frame)))
  	       (right (+ left width -1))
  	       (bottom (+ top height -1))
  	       (fr-left (frameset--compute-pos (frame-parameter frame 'left) left right))
  	       (fr-top (frameset--compute-pos (frame-parameter frame 'top) top bottom))
	       (ch-width (frame-char-width frame))
	       (ch-height (frame-char-height frame))
  	       (fr-width (max (frame-pixel-width frame) (* ch-width (frame-width frame))))
  	       (fr-height (max (frame-pixel-height frame) (* ch-height (frame-height frame))))
  	       (fr-right (+ fr-left fr-width -1))
  	       (fr-bottom (+ fr-top fr-height -1)))
    (when (pcase force-onscreen
	    ;; Any corner is outside the screen.
	    (`all (or (< fr-bottom top)  (> fr-bottom bottom)
		      (< fr-left   left) (> fr-left   right)
		      (< fr-right  left) (> fr-right  right)
		      (< fr-top    top)  (> fr-top    bottom)))
	    ;; Displaced to the left, right, above or below the screen.
	    (`t   (or (> fr-left   right)
		      (< fr-right  left)
		      (> fr-top    bottom)
		      (< fr-bottom top)))
	    ;; Fully inside, no need to do anything.
	    (_ nil))
      (let ((fullwidth (> fr-width width))
	    (fullheight (> fr-height height))
	    (params nil))
	;; Position frame horizontally.
	(cond (fullwidth
	       (push `(left . ,left) params))
	      ((> fr-right right)
	       (push `(left . ,(+ left (- width fr-width))) params))
	      ((< fr-left left)
	       (push `(left . ,left) params)))
	;; Position frame vertically.
	(cond (fullheight
	       (push `(top . ,top) params))
	      ((> fr-bottom bottom)
	       (push `(top . ,(+ top (- height fr-height))) params))
	      ((< fr-top top)
	       (push `(top . ,top) params)))
	;; Compute fullscreen state, if required.
	(when (or fullwidth fullheight)
	  (push (cons 'fullscreen
		      (cond ((not fullwidth) 'fullheight)
			    ((not fullheight) 'fullwidth)
			    (t 'maximized)))
		params))
	;; Finally, move the frame back onscreen.
	(when params
	  (modify-frame-parameters frame params))))))

(defun frameset--find-frame (predicate display &rest args)
  "Find a frame in `frameset--reuse-list' satisfying PREDICATE.
Look through available frames whose display property matches DISPLAY
and return the first one for which (PREDICATE frame ARGS) returns t.
If PREDICATE is nil, it is always satisfied.  Internal use only."
  (cl-find-if (lambda (frame)
		(and (equal (frame-parameter frame 'display) display)
		     (or (null predicate)
			 (apply predicate frame args))))
	      frameset--reuse-list))

(defun frameset--reuse-frame (display frame-cfg)
  "Look for an existing frame to reuse.
DISPLAY is the display where the frame will be shown, and FRAME-CFG
is the parameter list of the frame being restored.  Internal use only."
  (let ((frame nil)
	mini)
    ;; There are no fancy heuristics there.  We could implement some
    ;; based on frame size and/or position, etc., but it is not clear
    ;; that any "gain" (in the sense of reduced flickering, etc.) is
    ;; worth the added complexity.  In fact, the code below mainly
    ;; tries to work nicely when M-x desktop-read is used after a
    ;; desktop session has already been loaded.  The other main use
    ;; case, which is the initial desktop-read upon starting Emacs,
    ;; will usually have only one frame, and should already work.
    (cond ((null display)
	   ;; When the target is tty, every existing frame is reusable.
	   (setq frame (frameset--find-frame nil display)))
	  ((car (setq mini (cdr (assq 'frameset--mini frame-cfg))))
	   ;; If the frame has its own minibuffer, let's see whether
	   ;; that frame has already been loaded (which can happen after
	   ;; M-x desktop-read).
	   (setq frame (frameset--find-frame
			(lambda (f id)
			  (string= (frame-parameter f 'frameset-id) id))
			display (cdr mini)))
	   ;; If it has not been loaded, and it is not a minibuffer-only frame,
	   ;; let's look for an existing non-minibuffer-only frame to reuse.
	   (unless (or frame (eq (cdr (assq 'minibuffer frame-cfg)) 'only))
	     (setq frame (frameset--find-frame
			  (lambda (f)
			    (let ((w (frame-parameter f 'minibuffer)))
			      (and (window-live-p w)
				   (window-minibuffer-p w)
				   (eq (window-frame w) f))))
			  display))))
	  (mini
	   ;; For minibufferless frames, check whether they already exist,
	   ;; and that they are linked to the right minibuffer frame.
	   (setq frame (frameset--find-frame
	   		(lambda (f id mini-id)
	   		  (and (string= (frame-parameter f 'frameset-id) id)
	   		       (string= (frame-parameter (window-frame (minibuffer-window f))
	   						 'frameset-id)
	   				mini-id)))
	   		display (cdr (assq 'frameset-id frame-cfg)) (cdr mini))))
	  (t
	   ;; Default to just finding a frame in the same display.
	   (setq frame (frameset--find-frame nil display))))
    ;; If found, remove from the list.
    (when frame
      (setq frameset--reuse-list (delq frame frameset--reuse-list)))
    frame))

(defun frameset--get-frame (frame-cfg window-cfg filters force-onscreen)
  "Set up and return a frame according to its saved state.
That means either reusing an existing frame or creating one anew.
FRAME-CFG is the frame's parameter list; WINDOW-CFG is its window state.
For the meaning of FORCE-ONSCREEN, see `frameset-restore'."
  (let* ((fullscreen (cdr (assq 'fullscreen frame-cfg)))
	 (lines (assq 'tool-bar-lines frame-cfg))
	 (filtered-cfg (frameset-filter-params frame-cfg filters nil))
	 (display (cdr (assq 'display filtered-cfg))) ;; post-filtering
	 alt-cfg frame)

    ;; This works around bug#14795 (or feature#14795, if not a bug :-)
    (setq filtered-cfg (assq-delete-all 'tool-bar-lines filtered-cfg))
    (push '(tool-bar-lines . 0) filtered-cfg)

    (when fullscreen
      ;; Currently Emacs has the limitation that it does not record the size
      ;; and position of a frame before maximizing it, so we cannot save &
      ;; restore that info.  Instead, when restoring, we resort to creating
      ;; invisible "fullscreen" frames of default size and then maximizing them
      ;; (and making them visible) which at least is somewhat user-friendly
      ;; when these frames are later de-maximized.
      (let ((width (and (eq fullscreen 'fullheight) (cdr (assq 'width filtered-cfg))))
	    (height (and (eq fullscreen 'fullwidth) (cdr (assq 'height filtered-cfg))))
	    (visible (assq 'visibility filtered-cfg)))
	(setq filtered-cfg (cl-delete-if (lambda (p)
					   (memq p '(visibility fullscreen width height)))
					 filtered-cfg :key #'car))
	(when width
	  (setq filtered-cfg (append `((user-size . t) (width . ,width))
				     filtered-cfg)))
	(when height
	  (setq filtered-cfg (append `((user-size . t) (height . ,height))
				     filtered-cfg)))
	;; These are parameters to apply after creating/setting the frame.
	(push visible alt-cfg)
	(push (cons 'fullscreen fullscreen) alt-cfg)))

    ;; Time to find or create a frame an apply the big bunch of parameters.
    ;; If a frame needs to be created and it falls partially or fully offscreen,
    ;; sometimes it gets "pushed back" onscreen; however, moving it afterwards is
    ;; allowed.  So we create the frame as invisible and then reapply the full
    ;; parameter list (including position and size parameters).
    (setq frame (or (and frameset--reuse-list
			 (frameset--reuse-frame display filtered-cfg))
		    (make-frame-on-display display
					   (cons '(visibility)
						 (cl-loop
						  for param in '(left top width height minibuffer)
						  collect (assq param filtered-cfg))))))
    (modify-frame-parameters frame
			     (if (eq (frame-parameter frame 'fullscreen) fullscreen)
				 ;; Workaround for bug#14949
				 (assq-delete-all 'fullscreen filtered-cfg)
			       filtered-cfg))

    ;; If requested, force frames to be onscreen.
    (when (and force-onscreen
	       ;; FIXME: iconified frames should be checked too,
	       ;; but it is impossible without deiconifying them.
	       (not (eq (frame-parameter frame 'visibility) 'icon)))
      (frameset--move-onscreen frame force-onscreen))

    ;; Let's give the finishing touches (visibility, tool-bar, maximization).
    (when lines (push lines alt-cfg))
    (when alt-cfg (modify-frame-parameters frame alt-cfg))
    ;; Now restore window state.
    (window-state-put window-cfg (frame-root-window frame) 'safe)
    frame))

(defun frameset--sort-states (state1 state2)
  "Predicate to sort frame states in a suitable order to be created.
It sorts minibuffer-owning frames before minibufferless ones."
  (pcase-let ((`(,hasmini1 ,id-def1) (assq 'frameset--mini (car state1)))
	      (`(,hasmini2 ,id-def2) (assq 'frameset--mini (car state2))))
    (cond ((eq id-def1 t) t)
	  ((eq id-def2 t) nil)
	  ((not (eq hasmini1 hasmini2)) (eq hasmini1 t))
	  ((eq hasmini1 nil) (string< id-def1 id-def2))
	  (t t))))

(defun frameset-sort-frames-for-deletion (frame1 _frame2)
  "Predicate to sort live frames for deletion.
Minibufferless frames must go first to avoid errors when attempting
to delete a frame whose minibuffer window is used by another frame."
  (not (frame-parameter frame1 'minibuffer)))

;;;###autoload
(cl-defun frameset-restore (frameset &key filters reuse-frames force-display force-onscreen)
  "Restore a FRAMESET into the current display(s).

FILTERS is an alist of parameter filters; defaults to `frameset-filter-alist'.

REUSE-FRAMES describes how to reuse existing frames while restoring a frameset:
  t       Reuse any existing frame if possible; delete leftover frames.
  nil     Restore frameset in new frames and delete existing frames.
  keep    Restore frameset in new frames and keep the existing ones.
  LIST    A list of frames to reuse; only these will be reused, if possible,
            and any leftover one will be deleted; other frames not on this
            list will be kept.

FORCE-DISPLAY can be:
  t       Frames will be restored in the current display.
  nil     Frames will be restored, if possible, in their original displays.
  delete  Frames in other displays will be deleted instead of restored.

FORCE-ONSCREEN can be:
  all     Force onscreen any frame fully or partially offscreen.
  t	  Force onscreen only those frames that are fully offscreen.
  nil	  Do not force any frame back onscreen.

All keywords default to nil."

  (cl-assert (frameset-p frameset))

  (let* ((delete-saved (eq force-display 'delete))
	 (forcing (not (frameset-keep-original-display-p force-display)))
	 (target (and forcing (cons 'display (frame-parameter nil 'display))))
	 other-frames)

    ;; frameset--reuse-list is a list of frames potentially reusable.  Later we
    ;; will decide which ones can be reused, and how to deal with any leftover.
    (pcase reuse-frames
      ((or `nil `keep)
       (setq frameset--reuse-list nil
	     other-frames (frame-list)))
      ((pred consp)
       (setq frameset--reuse-list (copy-sequence reuse-frames)
	     other-frames (cl-delete-if (lambda (frame)
					   (memq frame frameset--reuse-list))
					 (frame-list))))
      (_
       (setq frameset--reuse-list (frame-list)
	     other-frames nil)))

    ;; Sort saved states to guarantee that minibufferless frames will be created
    ;; after the frames that contain their minibuffer windows.
    (dolist (state (sort (copy-sequence (frameset-states frameset))
			 #'frameset--sort-states))
      (condition-case-unless-debug err
	  (pcase-let* ((`(,frame-cfg . ,window-cfg) state)
		       ((and d-mini `(,hasmini . ,mb-id))
			(cdr (assq 'frameset--mini frame-cfg)))
		       (default (and (booleanp mb-id) mb-id))
		       (frame nil) (to-tty nil))
	    ;; Only set target if forcing displays and the target display is different.
	    (if (or (not forcing)
		    (equal target (or (assq 'display frame-cfg) '(display . nil))))
		(setq frameset--target-display nil)
	      (setq frameset--target-display target
		    to-tty (null (cdr target))))
	    ;; If keeping non-reusable frames, and the frame-id of one of them
	    ;; matches the frame-id of a frame being restored (because, for example,
	    ;; the frameset has already been read in the same session), remove the
	    ;; frame-id from the non-reusable frame, which is not useful anymore.
	    (when (and other-frames
		       (or (eq reuse-frames 'keep) (consp reuse-frames)))
	      (let ((dup (cl-find (cdr (assq 'frameset-frame-id frame-cfg))
				  other-frames
				  :key (lambda (frame)
					 (frame-parameter frame 'frameset-frame-id))
				  :test #'string=)))
		(when dup
		  (set-frame-parameter dup 'frameset-frame-id nil))))
	    ;; Time to restore frames and set up their minibuffers as they were.
	    ;; We only skip a frame (thus deleting it) if either:
	    ;; - we're switching displays, and the user chose the option to delete, or
	    ;; - we're switching to tty, and the frame to restore is minibuffer-only.
	    (unless (and frameset--target-display
			 (or delete-saved
			     (and to-tty
				  (eq (cdr (assq 'minibuffer frame-cfg)) 'only))))

	      ;; Restore minibuffers.  Some of this stuff could be done in a filter
	      ;; function, but it would be messy because restoring minibuffers affects
	      ;; global state; it's best to do it here than add a bunch of global
	      ;; variables to pass info back-and-forth to/from the filter function.
	      (cond
	       ((null d-mini)) ;; No frameset--mini.  Process as normal frame.
	       (to-tty) ;; Ignore minibuffer stuff and process as normal frame.
	       (hasmini ;; Frame has minibuffer (or it is minibuffer-only).
		(when (eq (cdr (assq 'minibuffer frame-cfg)) 'only)
		  (setq frame-cfg (append '((tool-bar-lines . 0) (menu-bar-lines . 0))
					  frame-cfg))))
	       (t ;; Frame depends on other frame's minibuffer window.
		(let* ((mb-frame (or (cl-find-if
				      (lambda (f)
					(string= (frame-parameter f 'frameset-id)
						 mb-id))
				      (frame-list))
				     (error "Minibuffer frame %S not found" mb-id)))
		       (mb-param (assq 'minibuffer frame-cfg))
		       (mb-window (minibuffer-window mb-frame)))
		  (unless (and (window-live-p mb-window)
			       (window-minibuffer-p mb-window))
		    (error "Not a minibuffer window %s" mb-window))
		  (if mb-param
		      (setcdr mb-param mb-window)
		    (push (cons 'minibuffer mb-window) frame-cfg))))))
	    ;; OK, we're ready at last to create (or reuse) a frame and
	    ;; restore the window config.
	    (setq frame (frameset--get-frame frame-cfg window-cfg
					     (or filters frameset-filter-alist)
					     force-onscreen))
	    ;; Set default-minibuffer if required.
	    (when default (setq default-minibuffer-frame frame)))
	(error
	 (delay-warning 'frameset (error-message-string err) :error))))

    ;; In case we try to delete the initial frame, we want to make sure that
    ;; other frames are already visible (discussed in thread for bug#14841).
    (sit-for 0 t)

    ;; Delete remaining frames, but do not fail if some resist being deleted.
    (unless (eq reuse-frames 'keep)
      (dolist (frame (sort (nconc (if (listp reuse-frames) nil other-frames)
				  frameset--reuse-list)
			   #'frameset-sort-frames-for-deletion))
	(condition-case err
	    (delete-frame frame)
	  (error
	   (delay-warning 'frameset (error-message-string err))))))
    (setq frameset--reuse-list nil)

    ;; Make sure there's at least one visible frame.
    (unless (or (daemonp) (visible-frame-list))
      (make-frame-visible (car (frame-list))))))

(provide 'frameset)

;;; frameset.el ends here
