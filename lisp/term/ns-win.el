;;; ns-win.el --- lisp side of interface with NeXT/Open/GNUstep/macOS window system  -*- lexical-binding: t -*-

;; Copyright (C) 1993-1994, 2005-2018 Free Software Foundation, Inc.

;; Authors: Carl Edman
;;	Christian Limpach
;;	Scott Bender
;;	Christophe de Dinechin
;;	Adrian Robert
;; Keywords: terminals

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

;; ns-win.el: this file is loaded from ../lisp/startup.el when it
;; recognizes that Nextstep windows are to be used.  Command line
;; switches are parsed and those pertaining to Nextstep are processed
;; and removed from the command line.  The Nextstep display is opened
;; and hooks are set for popping up the initial window.

;; startup.el will then examine startup files, and eventually call the hooks
;; which create the first window (s).

;; A number of other Nextstep convenience functions are defined in
;; this file, which works in close coordination with src/nsfns.m.

;;; Code:
(eval-when-compile (require 'cl-lib))
(or (featurep 'ns)
    (error "%s: Loading ns-win.el but not compiled for GNUstep/macOS"
           invocation-name))

;; Documentation-purposes only: actually loaded in loadup.el.
(require 'frame)
(require 'mouse)
(require 'faces)
(require 'menu-bar)
(require 'fontset)
(require 'dnd)
(require 'ucs-normalize)

(defgroup ns nil
  "GNUstep/macOS specific features."
  :group 'environment)

;;;; Command line argument handling.

(defvar x-invocation-args)
;; Set in term/common-win.el; currently unused by Nextstep's x-open-connection.
(defvar x-command-line-resources)

;; nsterm.m.
(defvar ns-input-file)

(defun ns-handle-nxopen (_switch &optional temp)
  (setq unread-command-events (append unread-command-events
                                      (if temp '(ns-open-temp-file)
                                        '(ns-open-file)))
        ns-input-file (append ns-input-file (list (pop x-invocation-args)))))

(defun ns-handle-nxopentemp (switch)
  (ns-handle-nxopen switch t))

(defun ns-ignore-1-arg (_switch)
  (setq x-invocation-args (cdr x-invocation-args)))

(defun ns-parse-geometry (geom)
  "Parse a Nextstep-style geometry string GEOM.
Returns an alist of the form ((top . TOP), (left . LEFT) ... ).
The properties returned may include `top', `left', `height', and `width'."
  (when (string-match "\\([0-9]+\\)\\( \\([0-9]+\\)\\( \\([0-9]+\\)\
\\( \\([0-9]+\\) ?\\)?\\)?\\)?"
		      geom)
    (apply
     'append
     (list
      (list (cons 'top (string-to-number (match-string 1 geom))))
      (if (match-string 3 geom)
	  (list (cons 'left (string-to-number (match-string 3 geom)))))
      (if (match-string 5 geom)
	  (list (cons 'height (string-to-number (match-string 5 geom)))))
      (if (match-string 7 geom)
	  (list (cons 'width (string-to-number (match-string 7 geom)))))))))

;;;; Keyboard mapping.

(define-obsolete-variable-alias 'ns-alternatives-map 'x-alternatives-map "24.1")

;; Here are some Nextstep-like bindings for command key sequences.
(define-key global-map [?\s-,] 'customize)
(define-key global-map [?\s-'] 'next-multiframe-window)
(define-key global-map [?\s-`] 'other-frame)
(define-key global-map [?\s-~] 'ns-prev-frame)
(define-key global-map [?\s--] 'center-line)
(define-key global-map [?\s-:] 'ispell)
(define-key global-map [?\s-?] 'info)
(define-key global-map [?\s-^] 'kill-some-buffers)
(define-key global-map [?\s-&] 'kill-current-buffer)
(define-key global-map [?\s-C] 'ns-popup-color-panel)
(define-key global-map [?\s-D] 'dired)
(define-key global-map [?\s-E] 'edit-abbrevs)
(define-key global-map [?\s-L] 'shell-command)
(define-key global-map [?\s-M] 'manual-entry)
(define-key global-map [?\s-S] 'ns-write-file-using-panel)
(define-key global-map [?\s-a] 'mark-whole-buffer)
(define-key global-map [?\s-c] 'ns-copy-including-secondary)
(define-key global-map [?\s-d] 'isearch-repeat-backward)
(define-key global-map [?\s-e] 'isearch-yank-kill)
(define-key global-map [?\s-f] 'isearch-forward)
(define-key global-map [?\s-g] 'isearch-repeat-forward)
(define-key global-map [?\s-h] 'ns-do-hide-emacs)
(define-key global-map [?\s-H] 'ns-do-hide-others)
(define-key global-map [?\M-\s-h] 'ns-do-hide-others)
(define-key global-map [?\s-j] 'exchange-point-and-mark)
(define-key global-map [?\s-k] 'kill-current-buffer)
(define-key global-map [?\s-l] 'goto-line)
(define-key global-map [?\s-m] 'iconify-frame)
(define-key global-map [?\s-n] 'make-frame)
(define-key global-map [?\s-o] 'ns-open-file-using-panel)
(define-key global-map [?\s-p] 'ns-print-buffer)
(define-key global-map [?\s-q] 'save-buffers-kill-emacs)
(define-key global-map [?\s-s] 'save-buffer)
(define-key global-map [?\s-t] 'ns-popup-font-panel)
(define-key global-map [?\s-u] 'revert-buffer)
(define-key global-map [?\s-v] 'yank)
(define-key global-map [?\s-w] 'delete-frame)
(define-key global-map [?\s-x] 'kill-region)
(define-key global-map [?\s-y] 'ns-paste-secondary)
(define-key global-map [?\s-z] 'undo)
(define-key global-map [?\s-|] 'shell-command-on-region)
(define-key global-map [s-kp-bar] 'shell-command-on-region)
(define-key global-map [?\C-\s- ] 'ns-do-show-character-palette)
;; (as in Terminal.app)
(define-key global-map [s-right] 'ns-next-frame)
(define-key global-map [s-left] 'ns-prev-frame)

(define-key global-map [home] 'beginning-of-buffer)
(define-key global-map [end] 'end-of-buffer)
(define-key global-map [kp-home] 'beginning-of-buffer)
(define-key global-map [kp-end] 'end-of-buffer)
(define-key global-map [kp-prior] 'scroll-down-command)
(define-key global-map [kp-next] 'scroll-up-command)

;; Allow shift-clicks to work similarly to under Nextstep.
(define-key global-map [S-mouse-1] 'mouse-save-then-kill)
(global-unset-key [S-down-mouse-1])

;; Special Nextstep-generated events are converted to function keys.  Here
;; are the bindings for them.  Note, these keys are actually declared in
;; x-setup-function-keys in common-win.
(define-key global-map [ns-power-off] 'save-buffers-kill-emacs)
(define-key global-map [ns-open-file] 'ns-find-file)
(define-key global-map [ns-open-temp-file] [ns-open-file])
(define-key global-map [ns-change-font] 'ns-respond-to-change-font)
(define-key global-map [ns-open-file-line] 'ns-open-file-select-line)
(define-key global-map [ns-spi-service-call] 'ns-spi-service-call)
(define-key global-map [ns-new-frame] 'make-frame)
(define-key global-map [ns-toggle-toolbar] 'ns-toggle-toolbar)
(define-key global-map [ns-show-prefs] 'customize)


;; Set up a number of aliases and other layers to pretend we're using
;; the Choi/Mitsuharu Carbon port.

(defvaralias 'mac-allow-anti-aliasing 'ns-antialias-text)
(defvaralias 'mac-command-modifier 'ns-command-modifier)
(defvaralias 'mac-right-command-modifier 'ns-right-command-modifier)
(defvaralias 'mac-control-modifier 'ns-control-modifier)
(defvaralias 'mac-right-control-modifier 'ns-right-control-modifier)
(defvaralias 'mac-option-modifier 'ns-option-modifier)
(defvaralias 'mac-right-option-modifier 'ns-right-option-modifier)
(defvaralias 'mac-function-modifier 'ns-function-modifier)
(declare-function ns-do-applescript "nsfns.m" (script))
(defalias 'do-applescript 'ns-do-applescript)

;;;; Services
(declare-function ns-perform-service "nsfns.m" (service send))

(defun ns-define-service (path)
  (let ((mapping [menu-bar services])
	(service (mapconcat 'identity path "/"))
	(name (intern
               (subst-char-in-string
                ?\s ?-
                (mapconcat 'identity (cons "ns-service" path) "-")))))
    ;; This defines the function.
    (defalias name
      (lambda (arg)
        (interactive "p")
        (let* ((in-string
                (cond ((stringp arg) arg)
                      (mark-active
                       (buffer-substring (region-beginning) (region-end)))))
               (out-string (ns-perform-service service in-string)))
          (cond
           ((stringp arg) out-string)
           ((and out-string (or (not in-string)
                                (not (string= in-string out-string))))
            (if mark-active (delete-region (region-beginning) (region-end)))
            (insert out-string)
            (setq deactivate-mark nil))))))
    (cond
     ((lookup-key global-map mapping)
      (while (cdr path)
	(setq mapping (vconcat mapping (list (intern (car path)))))
	(if (not (keymapp (lookup-key global-map mapping)))
	    (define-key global-map mapping
	      (cons (car path) (make-sparse-keymap (car path)))))
	(setq path (cdr path)))
      (setq mapping (vconcat mapping (list (intern (car path)))))
      (define-key global-map mapping (cons (car path) name))))
    name))

;; nsterm.m
(defvar ns-input-spi-name)
(defvar ns-input-spi-arg)

(declare-function dnd-open-file "dnd" (uri action))

;; Handles multiline strings that are passed to the "open-file" service.
(defun ns-open-file-service (filenames)
  "Open multiple files when selecting a multiline string FILENAMES."
  (let ((filelist (split-string filenames "[\n\r]+" t "[ \u00A0\t]+")))
    ;; The path strings are trimmed for spaces, nbsp and tabs.
    (dolist (filestring filelist)
      (dnd-open-file filestring nil))))


(defun ns-spi-service-call ()
  "Respond to a service request."
  (interactive)
  (cond ((string-equal ns-input-spi-name "open-selection")
	 (switch-to-buffer (generate-new-buffer "*untitled*"))
	 (insert ns-input-spi-arg))
	((string-equal ns-input-spi-name "open-file")
	 (ns-open-file-service ns-input-spi-arg))
	((string-equal ns-input-spi-name "mail-selection")
	 (compose-mail)
	 (rfc822-goto-eoh)
	 (forward-line 1)
	 (insert ns-input-spi-arg))
	((string-equal ns-input-spi-name "mail-to")
	 (compose-mail ns-input-spi-arg))
	(t (error "Service %s not recognized" ns-input-spi-name))))


;; Composed key sequence handling for Nextstep system input methods.
;; (On Nextstep systems, input methods are provided for CJK
;; characters, etc. which require multiple keystrokes, and during
;; entry a partial ("working") result is typically shown in the
;; editing window.)

(defface ns-working-text-face
  '((t :underline t))
  "Face used to highlight working text during compose sequence insert."
  :group 'ns)

(defvar ns-working-overlay nil
  "Overlay used to highlight working text during compose sequence insert.
When text is in th echo area, this just stores the length of the working text.")

(defvar ns-working-text)		; nsterm.m

;; Test if in echo area, based on mac-win.el 2007/08/26 unicode-2.
;; This will fail if called from a NONASCII_KEYSTROKE event on the global map.
(defun ns-in-echo-area ()
  "Whether, for purposes of inserting working composition text, the minibuffer
is currently being used."
  (or isearch-mode
      (and cursor-in-echo-area (current-message))
      ;; Overlay strings are not shown in some cases.
      (get-char-property (point) 'invisible)
      (and (not (bobp))
	   (or (and (get-char-property (point) 'display)
		    (eq (get-char-property (1- (point)) 'display)
			(get-char-property (point) 'display)))
	       (and (get-char-property (point) 'composition)
		    (eq (get-char-property (1- (point)) 'composition)
			(get-char-property (point) 'composition)))))))

;; The 'interactive' here stays for subinvocations, so the ns-in-echo-area
;; always returns nil for some reason.  If this WASN'T the case, we could
;; map this to [ns-insert-working-text] and eliminate Fevals in nsterm.m.
;; These functions test whether in echo area and delegate accordingly.
(defun ns-put-working-text ()
  (interactive)
  (if (ns-in-echo-area) (ns-echo-working-text) (ns-insert-working-text)))
(defun ns-unput-working-text ()
  (interactive)
  (ns-delete-working-text))

(defun ns-insert-working-text ()
  "Insert contents of `ns-working-text' as UTF-8 string and mark with
`ns-working-overlay'.  Any previously existing working text is cleared first.
The overlay is assigned the face `ns-working-text-face'."
  ;; FIXME: if buffer is read-only, don't try to insert anything, and
  ;; if text is bound to a command, execute that instead (Bug#1453).
  (interactive)
  (ns-delete-working-text)
  (let ((start (point)))
    (insert ns-working-text)
    (overlay-put (setq ns-working-overlay (make-overlay start (point)
							(current-buffer) nil t))
		 'face 'ns-working-text-face)))

(defun ns-echo-working-text ()
  "Echo contents of `ns-working-text' in message display area.
See `ns-insert-working-text'."
  (ns-delete-working-text)
  (let* ((msg (current-message))
	 (msglen (length msg))
	 message-log-max)
    (setq ns-working-overlay (length ns-working-text))
    (setq msg (concat msg ns-working-text))
    (put-text-property msglen (+ msglen ns-working-overlay)
		       'face 'ns-working-text-face msg)
    (message "%s" msg)))

(defun ns-delete-working-text()
  "Delete working text and clear `ns-working-overlay'."
  (interactive)
  (cond
   ((and (overlayp ns-working-overlay)
         ;; Still alive?
         (overlay-buffer ns-working-overlay))
    (with-current-buffer (overlay-buffer ns-working-overlay)
      (delete-region (overlay-start ns-working-overlay)
                     (overlay-end ns-working-overlay))
      (delete-overlay ns-working-overlay)))
   ((integerp ns-working-overlay)
    (let ((msg (current-message))
          message-log-max)
      (setq msg (substring msg 0 (- (length msg) ns-working-overlay)))
      (message "%s" msg))))
  (setq ns-working-overlay nil))


;; macOS file system Unicode UTF-8 NFD (decomposed form) support.
(when (eq system-type 'darwin)
  ;; Used prior to Emacs 25.
  (define-coding-system-alias 'utf-8-nfd 'utf-8-hfs)

  (set-file-name-coding-system 'utf-8-hfs-unix))

;;;; Inter-app communications support.

(defun ns-insert-file ()
  "Insert contents of file `ns-input-file' like insert-file but with less
prompting.  If file is a directory perform a `find-file' on it."
  (interactive)
  (let ((f (pop ns-input-file)))
    (if (file-directory-p f)
        (find-file f)
      (push-mark (+ (point) (cadr (insert-file-contents f)))))))

(defvar ns-select-overlay nil
  "Overlay used to highlight areas in files requested by Nextstep apps.")
(make-variable-buffer-local 'ns-select-overlay)

(defvar ns-input-line) 			; nsterm.m

(defun ns-open-file-select-line ()
  "Open a buffer containing the file `ns-input-file'.
Lines are highlighted according to `ns-input-line'."
  (interactive)
  (ns-find-file)
  (cond
   ((and ns-input-line (buffer-modified-p))
    (if ns-select-overlay
        (setq ns-select-overlay (delete-overlay ns-select-overlay)))
    (deactivate-mark)
    (goto-char (point-min))
    (forward-line (1- (if (consp ns-input-line)
                          (min (car ns-input-line) (cdr ns-input-line))
                        ns-input-line))))
   (ns-input-line
    (if (not ns-select-overlay)
        (overlay-put (setq ns-select-overlay (make-overlay (point-min)
                                                           (point-min)))
                     'face 'highlight))
    (let ((beg (save-excursion
                 (goto-char (point-min))
                 (line-beginning-position
                  (if (consp ns-input-line)
                      (min (car ns-input-line) (cdr ns-input-line))
                    ns-input-line))))
          (end (save-excursion
                 (goto-char (point-min))
                 (line-beginning-position
                  (1+ (if (consp ns-input-line)
                          (max (car ns-input-line) (cdr ns-input-line))
                        ns-input-line))))))
      (move-overlay ns-select-overlay beg end)
      (deactivate-mark)
      (goto-char beg)))
   (t
    (if ns-select-overlay
        (setq ns-select-overlay (delete-overlay ns-select-overlay))))))

(defun ns-unselect-line ()
  "Removes any Nextstep highlight a buffer may contain."
  (if ns-select-overlay
      (setq ns-select-overlay (delete-overlay ns-select-overlay))))

(add-hook 'first-change-hook 'ns-unselect-line)

;;;; Preferences handling.
(declare-function ns-get-resource "nsfns.m" (owner name))

(defun get-lisp-resource (arg1 arg2)
  (let ((res (ns-get-resource arg1 arg2)))
    (cond
     ((not res) 'unbound)
     ((string-equal (upcase res) "YES") t)
     ((string-equal (upcase res) "NO")  nil)
     (t (read res)))))

;; nsterm.m

(declare-function ns-read-file-name "nsfns.m"
		  (prompt &optional dir mustmatch init dir_only_p))

;;;; File handling.

(defun x-file-dialog (prompt dir default_filename mustmatch only_dir_p)
  "SKIP: real doc in xfns.c."
  (ns-read-file-name prompt dir mustmatch default_filename only_dir_p))

(defun ns-open-file-using-panel ()
  "Pop up open-file panel, and load the result in a buffer."
  (interactive)
  ;; Prompt dir defaultName isLoad initial.
  (setq ns-input-file (ns-read-file-name "Select File to Load" nil t nil))
  (if ns-input-file
      (and (setq ns-input-file (list ns-input-file)) (ns-find-file))))

(defun ns-write-file-using-panel ()
  "Pop up save-file panel, and save buffer in resulting name."
  (interactive)
  (let (ns-output-file)
    ;; Prompt dir defaultName isLoad initial.
    (setq ns-output-file (ns-read-file-name "Save As" nil nil nil))
    (message ns-output-file)
    (if ns-output-file (write-file ns-output-file))))

(defcustom ns-pop-up-frames 'fresh
  "Non-nil means open files upon request from the Workspace in a new frame.
If t, always do so.  Any other non-nil value means open a new frame
unless the current buffer is a scratch buffer."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Always" t)
                 (other :tag "Except for scratch buffer" fresh))
  :version "23.1"
  :group 'ns)

(declare-function ns-hide-emacs "nsfns.m" (on))

(defun ns-find-file ()
  "Do a `find-file' with the `ns-input-file' as argument."
  (interactive)
  (let* ((f (file-truename
	     (expand-file-name (pop ns-input-file)
			       command-line-default-directory)))
         (file (find-file-noselect f))
         (bufwin1 (get-buffer-window file 'visible))
         (bufwin2 (get-buffer-window "*scratch*" 'visible)))
    (cond
     (bufwin1
      (select-frame (window-frame bufwin1))
      (raise-frame (window-frame bufwin1))
      (select-window bufwin1))
     ((and (eq ns-pop-up-frames 'fresh) bufwin2)
      (ns-hide-emacs 'activate)
      (select-frame (window-frame bufwin2))
      (raise-frame (window-frame bufwin2))
      (select-window bufwin2)
      (find-file f))
     (ns-pop-up-frames
      (ns-hide-emacs 'activate)
      (let ((pop-up-frames t)) (pop-to-buffer file nil)))
     (t
      (ns-hide-emacs 'activate)
      (find-file f)))))


(defun ns-drag-n-drop (event &optional new-frame force-text)
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


(defun ns-drag-n-drop-other-frame (event)
  "Edit the files listed in the drag-n-drop EVENT, in other frames.
May create new frames, or reuse existing ones.  The frame editing
the last file dropped is selected."
  (interactive "e")
  (ns-drag-n-drop event t))

(defun ns-drag-n-drop-as-text (event)
  "Drop the data in EVENT as text."
  (interactive "e")
  (ns-drag-n-drop event nil t))

(defun ns-drag-n-drop-as-text-other-frame (event)
  "Drop the data in EVENT as text in a new frame."
  (interactive "e")
  (ns-drag-n-drop event t t))

(global-set-key [drag-n-drop] 'ns-drag-n-drop)
(global-set-key [C-drag-n-drop] 'ns-drag-n-drop-other-frame)
(global-set-key [M-drag-n-drop] 'ns-drag-n-drop-as-text)
(global-set-key [C-M-drag-n-drop] 'ns-drag-n-drop-as-text-other-frame)

;;;; Frame-related functions.

;; nsterm.m
(defvar ns-alternate-modifier)
(defvar ns-right-alternate-modifier)
(defvar ns-right-command-modifier)
(defvar ns-right-control-modifier)

;; You say tomAYto, I say tomAHto..
(with-no-warnings
  (defvaralias 'ns-option-modifier 'ns-alternate-modifier)
  (defvaralias 'ns-right-option-modifier 'ns-right-alternate-modifier))

(defun ns-do-hide-emacs ()
  (interactive)
  (ns-hide-emacs t))

(declare-function ns-hide-others "nsfns.m" ())

(defun ns-do-hide-others ()
  (interactive)
  (ns-hide-others))

(declare-function ns-emacs-info-panel "nsfns.m" ())

(defun ns-do-emacs-info-panel ()
  (interactive)
  (ns-emacs-info-panel))

(declare-function ns-show-character-palette "nsfns.m" ())

(defun ns-do-show-character-palette ()
  (interactive)
  (ns-show-character-palette))

(defun ns-next-frame ()
  "Switch to next visible frame."
  (interactive)
  (other-frame 1))

(defun ns-prev-frame ()
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
;; see https://lists.gnu.org/r/emacs-devel/2005-09/msg00681.html .
(defun ns-toggle-toolbar (&optional frame)
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
(defun ns-print-buffer ()
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
(declare-function ns-popup-font-panel "nsfns.m" (&optional frame))
(defalias 'x-select-font 'ns-popup-font-panel "Pop up the font panel.
This function has been overloaded in Nextstep.")
(defalias 'mouse-set-font 'ns-popup-font-panel "Pop up the font panel.
This function has been overloaded in Nextstep.")

;; nsterm.m
(defvar ns-input-font)
(defvar ns-input-fontsize)

(defun ns-respond-to-change-font ()
  "Respond to changeFont: event, expecting `ns-input-font' and\n\
`ns-input-fontsize' of new font."
  (interactive)
  (modify-frame-parameters (selected-frame)
                           (list (cons 'fontsize ns-input-fontsize)))
  (modify-frame-parameters (selected-frame)
                           (list (cons 'font ns-input-font)))
  (set-frame-font ns-input-font))


;; Default fontset for macOS.  This is mainly here to show how a fontset
;; can be set up manually.  Ordinarily, fontsets are auto-created whenever
;; a font is chosen by
(defvar ns-standard-fontset-spec
  ;; Only some code supports this so far, so use uglier XLFD version
  ;; "-ns-*-*-*-*-*-10-*-*-*-*-*-fontset-standard,latin:Courier,han:Kai"
  (mapconcat 'identity
             '("-ns-*-*-*-*-*-10-*-*-*-*-*-fontset-standard"
               "latin:-*-Courier-*-*-*-*-10-*-*-*-*-*-iso10646-1"
               "han:-*-Kai-*-*-*-*-10-*-*-*-*-*-iso10646-1"
               "cyrillic:-*-Trebuchet$MS-*-*-*-*-10-*-*-*-*-*-iso10646-1")
             ",")
  "String of fontset spec of the standard fontset.
This defines a fontset consisting of the Courier and other fonts that
come with macOS.
See the documentation of `create-fontset-from-fontset-spec' for the format.")

(defvar ns-reg-to-script)               ; nsfont.m

;; This maps font registries (not exposed by NS APIs for font selection) to
;; Unicode scripts (which can be mapped to Unicode character ranges which are).
;; See ../international/fontset.el
(setq ns-reg-to-script
      '(("iso8859-1" . latin)
	("iso8859-2" . latin)
	("iso8859-3" . latin)
	("iso8859-4" . latin)
	("iso8859-5" . cyrillic)
	("microsoft-cp1251" . cyrillic)
	("koi8-r" . cyrillic)
	("iso8859-6" . arabic)
	("iso8859-7" . greek)
	("iso8859-8" . hebrew)
	("iso8859-9" . latin)
	("iso8859-10" . latin)
	("iso8859-11" . thai)
	("tis620" . thai)
	("iso8859-13" . latin)
	("iso8859-14" . latin)
	("iso8859-15" . latin)
	("iso8859-16" . latin)
	("viscii1.1-1" . latin)
	("jisx0201" . kana)
	("jisx0208" . han)
	("jisx0212" . han)
	("jisx0213" . han)
	("gb2312.1980" . han)
	("gb18030" . han)
	("gbk-0" . han)
	("big5" . han)
	("cns11643" . han)
	("sisheng_cwnn" . bopomofo)
	("ksc5601.1987" . hangul)
	("ethiopic-unicode" . ethiopic)
	("is13194-devanagari" . indian-is13194)
	("iso10646.indian-1" . devanagari)))


;;;; Pasteboard support.

(define-obsolete-function-alias 'ns-store-cut-buffer-internal
  'gui-set-selection "24.1")


(defun ns-copy-including-secondary ()
  (interactive)
  (call-interactively 'kill-ring-save)
  (gui-set-selection 'SECONDARY (buffer-substring (point) (mark t))))

(defun ns-paste-secondary ()
  (interactive)
  (insert (gui-get-selection 'SECONDARY)))


;;;; Scrollbar handling.

(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-toolkit-scroll)
(global-set-key [horizontal-scroll-bar down-mouse-1] 'scroll-bar-toolkit-horizontal-scroll)
(global-unset-key [vertical-scroll-bar mouse-1])
(global-unset-key [vertical-scroll-bar drag-mouse-1])
(global-unset-key [horizontal-scroll-bar mouse-1])
(global-unset-key [horizontal-scroll-bar drag-mouse-1])


;;;; macOS-like defaults for trackpad and mouse wheel scrolling on
;;;; macOS 10.7+.

(defvar ns-version-string)
(defvar mouse-wheel-scroll-amount)
(defvar mouse-wheel-progressive-speed)

;; FIXME: This doesn't look right.  Is there a better way to do this
;; that keeps customize happy?
(when (featurep 'cocoa)
  (let ((appkit-version
         (progn (string-match "^appkit-\\([^\s-]*\\)" ns-version-string)
                (string-to-number (match-string 1 ns-version-string)))))
    ;; Appkit 1138 ~= macOS 10.7.
    (when (>= appkit-version 1138)
      (setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
      (put 'mouse-wheel-scroll-amount 'customized-value
           (list (custom-quote (symbol-value 'mouse-wheel-scroll-amount))))

      (setq mouse-wheel-progressive-speed nil)
      (put 'mouse-wheel-progressive-speed 'customized-value
           (list (custom-quote
                  (symbol-value 'mouse-wheel-progressive-speed)))))))


;;;; Color support.

;; Functions for color panel + drag
(defun ns-face-at-pos (pos)
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

(defun ns-suspend-error ()
  ;; Don't allow suspending if any of the frames are NS frames.
  (if (memq 'ns (mapcar 'window-system (frame-list)))
      (error "Cannot suspend Emacs while an NS GUI frame exists")))


;; Set some options to be as Nextstep-like as possible.
(setq frame-title-format "%b"
      icon-title-format "%b")


(defvar ns-initialized nil
  "Non-nil if Nextstep windowing has been initialized.")

(declare-function x-handle-args "common-win" (args))
(declare-function ns-list-services "nsfns.m" ())
(declare-function x-open-connection "nsfns.m"
                  (display &optional xrm-string must-succeed))
(declare-function ns-set-resource "nsfns.m" (owner name value))

;; Do the actual Nextstep Windows setup here; the above code just
;; defines functions and variables that we use now.
(cl-defmethod window-system-initialization (&context (window-system ns)
                                            &optional _display)
  "Initialize Emacs for Nextstep (Cocoa / GNUstep) windowing."
  (cl-assert (not ns-initialized))

  ;; PENDING: not needed?
  (setq command-line-args (x-handle-args command-line-args))

  ;; Setup the default fontset.
  (create-default-fontset)
  ;; Create the standard fontset.
  (condition-case err
      (create-fontset-from-fontset-spec ns-standard-fontset-spec t)
    (error (display-warning
            'initialization
            (format "Creation of the standard fontset failed: %s" err)
            :error)))

  (x-open-connection (or (system-name) "") x-command-line-resources t)

  ;; Add GNUstep menu items Services, Hide and Quit.  Rename Help to Info
  ;; and put it first (i.e. omit from menu-bar-final-items.
  (if (featurep 'gnustep)
      (progn
	(setq menu-bar-final-items '(buffer services hide-app quit))

	;; If running under GNUstep, "Help" is moved and renamed "Info".
	(bindings--define-key global-map [menu-bar help-menu]
	  (cons "Info" menu-bar-help-menu))
	(bindings--define-key global-map [menu-bar quit]
	  '(menu-item "Quit" save-buffers-kill-emacs
		      :help "Save unsaved buffers, then exit"))
	(bindings--define-key global-map [menu-bar hide-app]
	  '(menu-item "Hide" ns-do-hide-emacs
		      :help "Hide Emacs"))
	(bindings--define-key global-map [menu-bar services]
	  (cons "Services" (make-sparse-keymap "Services")))))


  (dolist (service (ns-list-services))
      (if (eq (car service) 'undefined)
	  (ns-define-service (cdr service))
	(define-key global-map (vector (car service))
	  (ns-define-service (cdr service)))))

  (if (and (eq (get-lisp-resource nil "NXAutoLaunch") t)
	   (eq (get-lisp-resource nil "HideOnAutoLaunch") t))
      (add-hook 'after-init-hook 'ns-do-hide-emacs))

  ;; FIXME: This will surely lead to "MODIFIED OUTSIDE CUSTOM" warnings.
  (menu-bar-mode (if (get-lisp-resource nil "Menus") 1 -1))

  ;; For Darwin nothing except UTF-8 makes sense.
  (when (eq system-type 'darwin)
      (add-hook 'before-init-hook
                #'(lambda ()
                    (setq locale-coding-system 'utf-8-unix)
                    (setq default-process-coding-system
                          '(utf-8-unix . utf-8-unix)))))

  ;; Mac OS X Lion introduces PressAndHold, which is unsupported by this port.
  ;; See this thread for more details:
  ;; https://lists.gnu.org/r/emacs-devel/2011-06/msg00505.html
  (ns-set-resource nil "ApplePressAndHoldEnabled" "NO")

  (x-apply-session-resources)

  ;; Don't let Emacs suspend under NS.
  (add-hook 'suspend-hook 'ns-suspend-error)

  (setq ns-initialized t))

;; Any display name is OK.
(add-to-list 'display-format-alist '(".*" . ns))
(cl-defmethod handle-args-function (args &context (window-system ns))
  (x-handle-args args))

(cl-defmethod frame-creation-function (params &context (window-system ns))
  (x-create-frame-with-faces params))

(declare-function ns-own-selection-internal "nsselect.m" (selection value))
(declare-function ns-disown-selection-internal "nsselect.m" (selection))
(declare-function ns-selection-owner-p "nsselect.m" (&optional selection))
(declare-function ns-selection-exists-p "nsselect.m" (&optional selection))
(declare-function ns-get-selection "nsselect.m" (selection-symbol target-type))

(cl-defmethod gui-backend-set-selection (selection value
                                         &context (window-system ns))
  (if value (ns-own-selection-internal selection value)
    (ns-disown-selection-internal selection)))

(cl-defmethod gui-backend-selection-owner-p (selection
                                             &context (window-system ns))
  (ns-selection-owner-p selection))

(cl-defmethod gui-backend-selection-exists-p (selection
                                              &context (window-system ns))
  (ns-selection-exists-p selection))

(cl-defmethod gui-backend-get-selection (selection-symbol target-type
                                         &context (window-system ns))
  (ns-get-selection selection-symbol target-type))

(provide 'ns-win)
(provide 'term/ns-win)

;;; ns-win.el ends here
