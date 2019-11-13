;;;

;;; Code:
(eval-when-compile (require 'cl-lib))
(or (featurep 'pgtk)
    (error "%s: Loading pgtk-win.el but not compiled for pure Gtk+-3."
           (invocation-name)))

;; Documentation-purposes only: actually loaded in loadup.el.
(require 'term/common-win)
(require 'frame)
(require 'mouse)
(require 'scroll-bar)
(require 'faces)
(require 'menu-bar)
(require 'fontset)
(require 'dnd)

(defgroup pgtk nil
  "Pure-GTK specific features."
  :group 'environment)

;;;; Command line argument handling.

(defvar x-invocation-args)
;; Set in term/common-win.el; currently unused by Gtk's x-open-connection.
(defvar x-command-line-resources)

;; pgtkterm.c.
(defvar pgtk-input-file)

(defun pgtk-handle-nxopen (_switch &optional temp)
  (setq unread-command-events (append unread-command-events
                                      (if temp '(pgtk-open-temp-file)
                                        '(pgtk-open-file)))
        pgtk-input-file (append pgtk-input-file (list (pop x-invocation-args)))))

(defun pgtk-handle-nxopentemp (switch)
  (pgtk-handle-nxopen switch t))

(defun pgtk-ignore-1-arg (_switch)
  (setq x-invocation-args (cdr x-invocation-args)))

;;;; File handling.

(defcustom pgtk-pop-up-frames 'fresh
  "Non-nil means open files upon request from the Workspace in a new frame.
If t, always do so.  Any other non-nil value means open a new frame
unless the current buffer is a scratch buffer."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Always" t)
                 (other :tag "Except for scratch buffer" fresh))
  :version "23.1"
  :group 'pgtk)

(declare-function pgtk-hide-emacs "pgtkfns.c" (on))


(defun pgtk-drag-n-drop (event &optional new-frame force-text)
  "Edit the files listed in the drag-n-drop EVENT.
Switch to a buffer editing the last file dropped."
  (interactive "e")
  (let* ((window (posn-window (event-start event)))
         (arg (car (cdr (cdr event))))
         (type (car arg))
         (data (car (cdr arg)))
         (url-or-string (cond ((eq type 'file)
                               (concat "file:" data))
                              (t data))))
    (set-frame-selected-window nil window)
    (when new-frame
      (select-frame (make-frame)))
    (raise-frame)
    (setq window (selected-window))
    (if force-text
        (dnd-insert-text window 'private data)
      (dnd-handle-one-url window 'private url-or-string))))


(defun pgtk-drag-n-drop-other-frame (event)
  "Edit the files listed in the drag-n-drop EVENT, in other frames.
May create new frames, or reuse existing ones.  The frame editing
the last file dropped is selected."
  (interactive "e")
  (pgtk-drag-n-drop event t))

(defun pgtk-drag-n-drop-as-text (event)
  "Drop the data in EVENT as text."
  (interactive "e")
  (pgtk-drag-n-drop event nil t))

(defun pgtk-drag-n-drop-as-text-other-frame (event)
  "Drop the data in EVENT as text in a new frame."
  (interactive "e")
  (pgtk-drag-n-drop event t t))

(global-set-key [drag-n-drop] 'pgtk-drag-n-drop)
(global-set-key [C-drag-n-drop] 'pgtk-drag-n-drop-other-frame)
(global-set-key [M-drag-n-drop] 'pgtk-drag-n-drop-as-text)
(global-set-key [C-M-drag-n-drop] 'pgtk-drag-n-drop-as-text-other-frame)

;;;; Frame-related functions.

;; pgtkterm.c
(defvar pgtk-alternate-modifier)
(defvar pgtk-right-alternate-modifier)
(defvar pgtk-right-command-modifier)
(defvar pgtk-right-control-modifier)

;; You say tomAYto, I say tomAHto..
(defvaralias 'pgtk-option-modifier 'pgtk-alternate-modifier)
(defvaralias 'pgtk-right-option-modifier 'pgtk-right-alternate-modifier)

(defun pgtk-do-hide-emacs ()
  (interactive)
  (pgtk-hide-emacs t))

(declare-function pgtk-hide-others "pgtkfns.c" ())

(defun pgtk-do-hide-others ()
  (interactive)
  (pgtk-hide-others))

(declare-function pgtk-emacs-info-panel "pgtkfns.c" ())

(defun pgtk-do-emacs-info-panel ()
  (interactive)
  (pgtk-emacs-info-panel))

(defun pgtk-next-frame ()
  "Switch to next visible frame."
  (interactive)
  (other-frame 1))

(defun pgtk-prev-frame ()
  "Switch to previous visible frame."
  (interactive)
  (other-frame -1))

;; Frame will be focused anyway, so select it
;; (if this is not done, mode line is dimmed until first interaction)
;; FIXME: Sounds like we're working around a bug in the underlying code.
(add-hook 'after-make-frame-functions 'select-frame)

(defvar tool-bar-mode)
(declare-function tool-bar-mode "tool-bar" (&optional arg))

;; Based on a function by David Reitter <dreitter@inf.ed.ac.uk> ;
;; see https://lists.gnu.org/archive/html/emacs-devel/2005-09/msg00681.html .
(defun pgtk-toggle-toolbar (&optional frame)
  "Switches the tool bar on and off in frame FRAME.
 If FRAME is nil, the change applies to the selected frame."
  (interactive)
  (modify-frame-parameters
   frame (list (cons 'tool-bar-lines
		       (if (> (or (frame-parameter frame 'tool-bar-lines) 0) 0)
				   0 1)) ))
  (if (not tool-bar-mode) (tool-bar-mode t)))


;;;; Dialog-related functions.

;; Ask user for confirm before printing.  Due to Kevin Rodgers.
(defun pgtk-print-buffer ()
  "Interactive front-end to `print-buffer': asks for user confirmation first."
  (interactive)
  (if (and (called-interactively-p 'interactive)
           (or (listp last-nonmenu-event)
               (and (char-or-string-p (event-basic-type last-command-event))
                    (memq 'super (event-modifiers last-command-event)))))
      (let ((last-nonmenu-event (if (listp last-nonmenu-event)
                                    last-nonmenu-event
                                  ;; Fake it:
                                  `(mouse-1 POSITION 1))))
        (if (y-or-n-p (format "Print buffer %s? " (buffer-name)))
            (print-buffer)
	  (error "Canceled")))
    (print-buffer)))

;;;; Font support.

;; Needed for font listing functions under both backend and normal
(setq scalable-fonts-allowed t)

;; Set to use font panel instead
(declare-function pgtk-popup-font-panel "pgtkfns.c" (&optional frame))
(defalias 'x-select-font 'pgtk-popup-font-panel "Pop up the font panel.
This function has been overloaded in Nextstep.")
(defalias 'mouse-set-font 'pgtk-popup-font-panel "Pop up the font panel.
This function has been overloaded in Nextstep.")

;; pgtkterm.c
(defvar pgtk-input-font)
(defvar pgtk-input-fontsize)

(defun pgtk-respond-to-change-font ()
  "Respond to changeFont: event, expecting `pgtk-input-font' and\n\
`pgtk-input-fontsize' of new font."
  (interactive)
  (modify-frame-parameters (selected-frame)
                           (list (cons 'fontsize pgtk-input-fontsize)))
  (modify-frame-parameters (selected-frame)
                           (list (cons 'font pgtk-input-font)))
  (set-frame-font pgtk-input-font))


;; Default fontset.  This is mainly here to show how a fontset
;; can be set up manually.  Ordinarily, fontsets are auto-created whenever
;; a font is chosen by
(defvar pgtk-standard-fontset-spec
  ;; Only some code supports this so far, so use uglier XLFD version
  ;; "-pgtk-*-*-*-*-*-10-*-*-*-*-*-fontset-standard,latin:Courier,han:Kai"
  (mapconcat 'identity
             '("-*-Monospace-*-*-*-*-10-*-*-*-*-*-fontset-standard"
               "latin:-*-Courier-*-*-*-*-10-*-*-*-*-*-iso10646-1"
               "han:-*-Kai-*-*-*-*-10-*-*-*-*-*-iso10646-1"
               "cyrillic:-*-Trebuchet$MS-*-*-*-*-10-*-*-*-*-*-iso10646-1")
             ",")
  "String of fontset spec of the standard fontset.
This defines a fontset consisting of the Courier and other fonts.
See the documentation of `create-fontset-from-fontset-spec' for the format.")


;;;; Pasteboard support.

(define-obsolete-function-alias 'pgtk-store-cut-buffer-internal
  'gui-set-selection "24.1")


(defun pgtk-copy-including-secondary ()
  (interactive)
  (call-interactively 'kill-ring-save)
  (gui-set-selection 'SECONDARY (buffer-substring (point) (mark t))))

(defun pgtk-paste-secondary ()
  (interactive)
  (insert (gui-get-selection 'SECONDARY)))


;;;; Color support.

;; Functions for color panel + drag
(defun pgtk-face-at-pos (pos)
  (let* ((frame (car pos))
         (frame-pos (cons (cadr pos) (cddr pos)))
         (window (window-at (car frame-pos) (cdr frame-pos) frame))
         (window-pos (coordinates-in-window-p frame-pos window))
         (buffer (window-buffer window))
         (edges (window-edges window)))
    (cond
     ((not window-pos)
      nil)
     ((eq window-pos 'mode-line)
      'mode-line)
     ((eq window-pos 'vertical-line)
      'default)
     ((consp window-pos)
      (with-current-buffer buffer
        (let ((p (car (compute-motion (window-start window)
                                      (cons (nth 0 edges) (nth 1 edges))
                                      (window-end window)
                                      frame-pos
                                      (- (window-width window) 1)
                                      nil
                                      window))))
          (cond
           ((eq p (window-point window))
            'cursor)
           ((and mark-active (< (region-beginning) p) (< p (region-end)))
            'region)
           (t
	    (let ((faces (get-char-property p 'face window)))
	      (if (consp faces) (car faces) faces)))))))
     (t
      nil))))

(defun pgtk-suspend-error ()
  ;; Don't allow suspending if any of the frames are PGTK frames.
  (if (memq 'pgtk (mapcar 'window-system (frame-list)))
      (error "Cannot suspend Emacs while a PGTK GUI frame exists")))


;; Set some options to be as Nextstep-like as possible.
(setq frame-title-format t
      icon-title-format t)


(defvar pgtk-initialized nil
  "Non-nil if pure-GTK windowing has been initialized.")

(declare-function x-handle-args "common-win" (args))
(declare-function x-open-connection "pgtkfns.c"
                  (display &optional xrm-string must-succeed))
(declare-function pgtk-set-resource "pgtkfns.c" (owner name value))

;; Do the actual pure-GTK Windows setup here; the above code just
;; defines functions and variables that we use now.
(cl-defmethod window-system-initialization (&context (window-system pgtk)
                                            &optional display)
  "Initialize Emacs for pure-GTK windowing."
  (cl-assert (not pgtk-initialized))

  ;; PENDING: not needed?
  (setq command-line-args (x-handle-args command-line-args))

  ;; Make sure we have a valid resource name.
  (or (stringp x-resource-name)
      (let (i)
	(setq x-resource-name (invocation-name))

	;; Change any . or * characters in x-resource-name to hyphens,
	;; so as not to choke when we use it in X resource queries.
	(while (setq i (string-match "[.*]" x-resource-name))
	  (aset x-resource-name i ?-))))

  ;; Setup the default fontset.
  (create-default-fontset)
  ;; Create the standard fontset.
  (condition-case err
      (create-fontset-from-fontset-spec pgtk-standard-fontset-spec t)
    (error (display-warning
            'initialization
            (format "Creation of the standard fontset failed: %s" err)
            :error)))

  (x-open-connection (or display
                         x-display-name)
		     x-command-line-resources
		     ;; Exit Emacs with fatal error if this fails and we
		     ;; are the initial display.
                     (= (length (frame-list)) 0))

  (x-apply-session-resources)

  ;; Don't let Emacs suspend under PGTK.
  (add-hook 'suspend-hook 'pgtk-suspend-error)

  (setq pgtk-initialized t))

;; Any display name is OK.
(add-to-list 'display-format-alist '(".*" . pgtk))
(cl-defmethod handle-args-function (args &context (window-system pgtk))
  (x-handle-args args))

(cl-defmethod frame-creation-function (params &context (window-system pgtk))
  (x-create-frame-with-faces params))

(declare-function pgtk-own-selection-internal "pgtkselect.c" (selection value &optional frame))
(declare-function pgtk-disown-selection-internal "pgtkselect.c" (selection &optional time_object terminal))
(declare-function pgtk-selection-owner-p "pgtkselect.c" (&optional selection terminal))
(declare-function pgtk-selection-exists-p "pgtkselect.c" (&optional selection terminal))
(declare-function pgtk-get-selection-internal "pgtkselect.c" (selection-symbol target-type &optional time_stamp terminal))

(cl-defmethod gui-backend-set-selection (selection value
                                         &context (window-system pgtk))
  (if value (pgtk-own-selection-internal selection value)
    (pgtk-disown-selection-internal selection)))

(cl-defmethod gui-backend-selection-owner-p (selection
                                             &context (window-system pgtk))
  (pgtk-selection-owner-p selection))

(cl-defmethod gui-backend-selection-exists-p (selection
                                              &context (window-system pgtk))
  (pgtk-selection-exists-p selection))

(cl-defmethod gui-backend-get-selection (selection-symbol target-type
                                         &context (window-system pgtk))
  (pgtk-get-selection-internal selection-symbol target-type))


(defvar pgtk-preedit-overlay nil)

(defun pgtk-preedit-text (e)
  (interactive "e")
  (when pgtk-preedit-overlay
    (delete-overlay pgtk-preedit-overlay))
  (setq pgtk-preedit-overlay nil)

  (let ((ovstr "")
        (idx 0)
        atts ov str color face-name)
    (dolist (part (nth 1 e))
      (setq str (car part))
      (setq face-name (intern (format "pgtk-im-%d" idx)))
      (eval
       `(defface ,face-name nil "face of input method preedit"))
      (setq atts nil)
      (when (setq color (cdr-safe (assq 'fg (cdr part))))
        (setq atts (append atts `(:foreground ,color))))
      (when (setq color (cdr-safe (assq 'bg (cdr part))))
        (setq atts (append atts `(:background ,color))))
      (when (setq color (cdr-safe (assq 'ul (cdr part))))
        (setq atts (append atts `(:underline ,color))))
      (face-spec-set face-name `((t . ,atts)))
      (add-text-properties 0 (length str) `(face ,face-name) str)
      (setq ovstr (concat ovstr str))
      (setq idx (1+ idx)))

    (setq ov (make-overlay (point) (point)))
    (overlay-put ov 'before-string ovstr)
    (setq pgtk-preedit-overlay ov)))

(provide 'pgtk-win)
(provide 'term/pgtk-win)

;;; pgtk-win.el ends here
