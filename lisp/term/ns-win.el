;;; ns-win.el --- lisp side of interface with NeXT/Open/GNUstep/MacOS X window system

;; Copyright (C) 1993, 1994, 2005, 2006, 2007, 2008
;;   Free Software Foundation, Inc.

;; Authors: Carl Edman, Christian Limpach, Scott Bender,
;;          Christophe de Dinechin, Adrian Robert
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

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


(if (not (featurep 'ns))
    (error "%s: Loading ns-win.el but not compiled for GNUStep/MacOS"
	   (invocation-name)))

(eval-when-compile (require 'cl))

;; Documentation-purposes only: actually loaded in loadup.el
(require 'frame)
(require 'mouse)
(require 'faces)
(require 'easymenu)
(require 'menu-bar)
(require 'fontset)

;; Not needed?
;;(require 'ispell)

;; nsterm.m
(defvar ns-version-string)
(defvar ns-expand-space)
(defvar ns-cursor-blink-rate)
(defvar ns-alternate-modifier)

;;;; Command line argument handling.

(defvar ns-invocation-args nil)
(defvar ns-command-line-resources nil)

;; Handler for switches of the form "-switch value" or "-switch".
(defun ns-handle-switch (switch &optional numeric)
  (let ((aelt (assoc switch command-line-ns-option-alist)))
    (if aelt
	(setq default-frame-alist
	      (cons (cons (nth 3 aelt)
			  (if numeric
			      (string-to-number (pop ns-invocation-args))
			    (or (nth 4 aelt) (pop ns-invocation-args))))
		    default-frame-alist)))))

;; Handler for switches of the form "-switch n"
(defun ns-handle-numeric-switch (switch)
  (ns-handle-switch switch t))

;; Make -iconic apply only to the initial frame!
(defun ns-handle-iconic (switch)
  (setq initial-frame-alist
        (cons '(visibility . icon) initial-frame-alist)))

;; Handle the -name option, set the name of the initial frame.
(defun ns-handle-name-switch (switch)
  (or (consp ns-invocation-args)
      (error "%s: missing argument to `%s' option" (invocation-name) switch))
  (setq initial-frame-alist (cons (cons 'name (pop ns-invocation-args))
                                  initial-frame-alist)))

;; Set (but not used?) in frame.el.
(defvar x-display-name nil
  "The name of the Nextstep display on which Emacs was started.")

;; nsterm.m.
(defvar ns-input-file)

(defun ns-handle-nxopen (switch)
  (setq unread-command-events (append unread-command-events '(ns-open-file))
        ns-input-file (append ns-input-file (list (pop ns-invocation-args)))))

(defun ns-handle-nxopentemp (switch)
  (setq unread-command-events (append unread-command-events
				      '(ns-open-temp-file))
        ns-input-file (append ns-input-file (list (pop ns-invocation-args)))))

(defun ns-ignore-0-arg (switch))
(defun ns-ignore-1-arg (switch)
  (setq ns-invocation-args (cdr ns-invocation-args)))
(defun ns-ignore-2-arg (switch)
  (setq ns-invocation-args (cddr ns-invocation-args)))

(defun ns-handle-args (args)
  "Process Nextstep-related command line options.
This is run before the user's startup file is loaded.
The options in ARGS are copied to `ns-invocation-args'.
The Nextstep-related settings are then applied using the handlers
defined in `command-line-ns-option-alist'.
The return value is ARGS minus the number of arguments processed."
  ;; We use ARGS to accumulate the args that we don't handle here, to return.
  (setq ns-invocation-args args
        args nil)
  (while ns-invocation-args
    (let* ((this-switch (pop ns-invocation-args))
	   (orig-this-switch this-switch)
	   completion argval aelt handler)
      ;; Check for long options with attached arguments
      ;; and separate out the attached option argument into argval.
      (if (string-match "^--[^=]*=" this-switch)
	  (setq argval (substring this-switch (match-end 0))
		this-switch (substring this-switch 0 (1- (match-end 0)))))
      ;; Complete names of long options.
      (if (string-match "^--" this-switch)
	  (progn
	    (setq completion (try-completion this-switch
                                             command-line-ns-option-alist))
	    (if (eq completion t)
		;; Exact match for long option.
		nil
	      (if (stringp completion)
		  (let ((elt (assoc completion command-line-ns-option-alist)))
		    ;; Check for abbreviated long option.
		    (or elt
			(error "Option `%s' is ambiguous" this-switch))
		    (setq this-switch completion))))))
      (setq aelt (assoc this-switch command-line-ns-option-alist))
      (if aelt (setq handler (nth 2 aelt)))
      (if handler
	  (if argval
	      (let ((ns-invocation-args
		     (cons argval ns-invocation-args)))
		(funcall handler this-switch))
	    (funcall handler this-switch))
	(setq args (cons orig-this-switch args)))))
  (nreverse args))

(defun x-parse-geometry (geom)
  "Parse a Nextstep-style geometry string STRING.
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

;; These tell read-char how to convert
;; these special chars to ASCII.
(put 'backspace 'ascii-character 127)
(put 'delete 'ascii-character 127)
(put 'tab 'ascii-character ?\t)
(put 'S-tab 'ascii-character (logior 16 ?\t))
(put 'linefeed 'ascii-character ?\n)
(put 'clear 'ascii-character 12)
(put 'return 'ascii-character 13)
(put 'escape 'ascii-character ?\e)

;; Map certain keypad keys into ASCII characters
;; that people usually expect.
(define-key function-key-map [backspace] [127])
(define-key function-key-map [delete] [127])
(define-key function-key-map [tab] [?\t])
(define-key function-key-map [S-tab] [25])
(define-key function-key-map [linefeed] [?\n])
(define-key function-key-map [clear] [11])
(define-key function-key-map [return] [13])
(define-key function-key-map [escape] [?\e])
(define-key function-key-map [M-backspace] [?\M-\d])
(define-key function-key-map [M-delete] [?\M-\d])
(define-key function-key-map [M-tab] [?\M-\t])
(define-key function-key-map [M-linefeed] [?\M-\n])
(define-key function-key-map [M-clear] [?\M-\013])
(define-key function-key-map [M-return] [?\M-\015])
(define-key function-key-map [M-escape] [?\M-\e])


;; Here are some Nextstep-like bindings for command key sequences.
(define-key global-map [?\s-,] 'ns-popup-prefs-panel)
(define-key global-map [?\s-'] 'next-multiframe-window)
(define-key global-map [?\s-`] 'other-frame)
(define-key global-map [?\s--] 'center-line)
(define-key global-map [?\s-:] 'ispell)
(define-key global-map [?\s-\;] 'ispell-next)
(define-key global-map [?\s-?] 'info)
(define-key global-map [?\s-^] 'kill-some-buffers)
(define-key global-map [?\s-&] 'kill-this-buffer)
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
(define-key global-map [?\s-j] 'exchange-point-and-mark)
(define-key global-map [?\s-k] 'kill-this-buffer)
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
;; (as in Terminal.app)
(define-key global-map [s-right] 'ns-next-frame)
(define-key global-map [s-left] 'ns-prev-frame)

(define-key global-map [home] 'beginning-of-buffer)
(define-key global-map [end] 'end-of-buffer)
(define-key global-map [kp-home] 'beginning-of-buffer)
(define-key global-map [kp-end] 'end-of-buffer)
(define-key global-map [kp-prior] 'scroll-down)
(define-key global-map [kp-next] 'scroll-up)


;; Special Nextstep-generated events are converted to function keys.  Here
;; are the bindings for them.
(define-key global-map [ns-power-off]
  (lambda () (interactive) (save-buffers-kill-emacs t)))
(define-key global-map [ns-open-file] 'ns-find-file)
(define-key global-map [ns-open-temp-file] [ns-open-file])
(define-key global-map [ns-drag-file] 'ns-insert-file)
(define-key global-map [ns-drag-color] 'ns-set-foreground-at-mouse)
(define-key global-map [S-ns-drag-color] 'ns-set-background-at-mouse)
(define-key global-map [ns-drag-text] 'ns-insert-text)
(define-key global-map [ns-change-font] 'ns-respond-to-change-font)
(define-key global-map [ns-open-file-line] 'ns-open-file-select-line)
(define-key global-map [ns-insert-working-text] 'ns-insert-working-text)
(define-key global-map [ns-delete-working-text] 'ns-delete-working-text)
(define-key global-map [ns-spi-service-call] 'ns-spi-service-call)
(define-key global-map [ns-new-frame] 'make-frame)



;; Functions to set environment variables by running a subshell.
;;; Idea based on Nextstep 4.2 distribution, this version of code
;;; based on mac-read-environment-vars-from-shell () by David Reitter.
;;; Mostly used only under ns-extended-platform-support-mode.

(defun ns-make-command-string (cmdlist)
  (mapconcat 'identity cmdlist " ; "))

;;;###autoload
(defun ns-grabenv (&optional shell-path startup)
  "Set the Emacs environment using the output of a shell command.
This runs a shell subprocess, and interpret its output as a
series of environment variables to insert into the emacs
environment.
SHELL-PATH gives the path to the shell; if nil, this defaults to
the current setting of `shell-file-name'.
STARTUP is a list of commands for the shell to execute; if nil,
this defaults to \"printenv\"."
  (interactive)
  (with-temp-buffer
    (let ((shell-file-name (if shell-path shell-path shell-file-name))
	  (cmd (ns-make-command-string (if startup startup '("printenv")))))
      (shell-command cmd t)
      (while (search-forward-regexp "^\\([A-Za-z_0-9]+\\)=\\(.*\\)$" nil t)
	(setenv (match-string 1)
		(if (equal (match-string 1) "PATH")
		    (concat (getenv "PATH") ":" (match-string 2))
		  (match-string 2)))))))

;; Set up a number of aliases and other layers to pretend we're using
;; the Choi/Mitsuharu Carbon port.

(defvaralias 'mac-allow-anti-aliasing 'ns-antialias-text)
(defvaralias 'mac-command-modifier 'ns-command-modifier)
(defvaralias 'mac-control-modifier 'ns-control-modifier)
(defvaralias 'mac-option-modifier 'ns-option-modifier)
(defvaralias 'mac-function-modifier 'ns-function-modifier)
(defalias 'do-applescript 'ns-do-applescript)


(defvar menu-bar-ns-file-menu)		; below

;; Toggle some additional Nextstep-like features that may interfere
;; with users' expectations coming from emacs on other platforms.
(define-minor-mode ns-extended-platform-support-mode
  "Toggle Nextstep extended platform support features.
   When this mode is active (no modeline indicator):
   - File menu is altered slightly in keeping with conventions.
   - Screen position is preserved in scrolling.
   - Transient mark mode is activated"
  :init-value nil
  :global t
  :group 'ns
  (if ns-extended-platform-support-mode
      (progn
	(defun ns-show-manual () "Show Emacs.app manual" (interactive) (info "ns-emacs"))
	(setq where-is-preferred-modifier 'super)
        (setq scroll-preserve-screen-position t)
        (transient-mark-mode 1)

        ;; Change file menu to simplify and add a couple of
        ;; Nextstep-specific items
        (easy-menu-remove-item global-map '("menu-bar") 'file)
        (easy-menu-add-item global-map '(menu-bar)
                            (cons "File" menu-bar-ns-file-menu) 'edit)
	(define-key menu-bar-help-menu [ns-manual]
	  '(menu-item "Emacs.app Manual" ns-show-manual)))
    (progn
      ;; Undo everything above.
      (fmakunbound 'ns-show-manual)
      (setq where-is-preferred-modifier 'nil)
      (setq scroll-preserve-screen-position nil)
      (transient-mark-mode 0)
      (easy-menu-remove-item global-map '("menu-bar") 'file)
      (easy-menu-add-item global-map '(menu-bar)
                          (cons "File" menu-bar-file-menu) 'edit)
      (easy-menu-remove-item global-map '("menu-bar" "help-menu") 'ns-manual)
)))


(defun x-setup-function-keys (frame)
  "Set up function Keys for Nextstep for frame FRAME."
  (unless (terminal-parameter frame 'x-setup-function-keys)
    (with-selected-frame frame
      (setq interprogram-cut-function 'x-select-text
	    interprogram-paste-function 'x-cut-buffer-or-selection-value)
      ;; (let ((map (copy-keymap x-alternatives-map)))
      ;;   (set-keymap-parent map (keymap-parent local-function-key-map))
      ;;   (set-keymap-parent local-function-key-map map))
      (setq system-key-alist
            (list
             (cons (logior (lsh 0 16)   1) 'ns-power-off)
             (cons (logior (lsh 0 16)   2) 'ns-open-file)
             (cons (logior (lsh 0 16)   3) 'ns-open-temp-file)
             (cons (logior (lsh 0 16)   4) 'ns-drag-file)
             (cons (logior (lsh 0 16)   5) 'ns-drag-color)
             (cons (logior (lsh 0 16)   6) 'ns-drag-text)
             (cons (logior (lsh 0 16)   7) 'ns-change-font)
             (cons (logior (lsh 0 16)   8) 'ns-open-file-line)
             (cons (logior (lsh 0 16)   9) 'ns-insert-working-text)
             (cons (logior (lsh 0 16)  10) 'ns-delete-working-text)
             (cons (logior (lsh 0 16)  11) 'ns-spi-service-call)
             (cons (logior (lsh 0 16)  12) 'ns-new-frame)
             (cons (logior (lsh 1 16)  32) 'f1)
             (cons (logior (lsh 1 16)  33) 'f2)
             (cons (logior (lsh 1 16)  34) 'f3)
             (cons (logior (lsh 1 16)  35) 'f4)
             (cons (logior (lsh 1 16)  36) 'f5)
             (cons (logior (lsh 1 16)  37) 'f6)
             (cons (logior (lsh 1 16)  38) 'f7)
             (cons (logior (lsh 1 16)  39) 'f8)
             (cons (logior (lsh 1 16)  40) 'f9)
             (cons (logior (lsh 1 16)  41) 'f10)
             (cons (logior (lsh 1 16)  42) 'f11)
             (cons (logior (lsh 1 16)  43) 'f12)
             (cons (logior (lsh 1 16)  44) 'kp-insert)
             (cons (logior (lsh 1 16)  45) 'kp-delete)
             (cons (logior (lsh 1 16)  46) 'kp-home)
             (cons (logior (lsh 1 16)  47) 'kp-end)
             (cons (logior (lsh 1 16)  48) 'kp-prior)
             (cons (logior (lsh 1 16)  49) 'kp-next)
             (cons (logior (lsh 1 16)  50) 'print-screen)
             (cons (logior (lsh 1 16)  51) 'scroll-lock)
             (cons (logior (lsh 1 16)  52) 'pause)
             (cons (logior (lsh 1 16)  53) 'system)
             (cons (logior (lsh 1 16)  54) 'break)
             (cons (logior (lsh 1 16)  56) 'please-tell-carl-what-this-key-is-called-56)
             (cons (logior (lsh 1 16)  61) 'please-tell-carl-what-this-key-is-called-61)
             (cons (logior (lsh 1 16)  62) 'please-tell-carl-what-this-key-is-called-62)
             (cons (logior (lsh 1 16)  63) 'please-tell-carl-what-this-key-is-called-63)
             (cons (logior (lsh 1 16)  64) 'please-tell-carl-what-this-key-is-called-64)
             (cons (logior (lsh 1 16)  69) 'please-tell-carl-what-this-key-is-called-69)
             (cons (logior (lsh 1 16)  70) 'please-tell-carl-what-this-key-is-called-70)
             (cons (logior (lsh 1 16)  71) 'please-tell-carl-what-this-key-is-called-71)
             (cons (logior (lsh 1 16)  72) 'please-tell-carl-what-this-key-is-called-72)
             (cons (logior (lsh 1 16)  73) 'please-tell-carl-what-this-key-is-called-73)
             (cons (logior (lsh 2 16)   3) 'kp-enter)
             (cons (logior (lsh 2 16)   9) 'kp-tab)
             (cons (logior (lsh 2 16)  28) 'kp-quit)
             (cons (logior (lsh 2 16)  35) 'kp-hash)
             (cons (logior (lsh 2 16)  42) 'kp-multiply)
             (cons (logior (lsh 2 16)  43) 'kp-add)
             (cons (logior (lsh 2 16)  44) 'kp-separator)
             (cons (logior (lsh 2 16)  45) 'kp-subtract)
             (cons (logior (lsh 2 16)  46) 'kp-decimal)
             (cons (logior (lsh 2 16)  47) 'kp-divide)
             (cons (logior (lsh 2 16)  48) 'kp-0)
             (cons (logior (lsh 2 16)  49) 'kp-1)
             (cons (logior (lsh 2 16)  50) 'kp-2)
             (cons (logior (lsh 2 16)  51) 'kp-3)
             (cons (logior (lsh 2 16)  52) 'kp-4)
             (cons (logior (lsh 2 16)  53) 'kp-5)
             (cons (logior (lsh 2 16)  54) 'kp-6)
             (cons (logior (lsh 2 16)  55) 'kp-7)
             (cons (logior (lsh 2 16)  56) 'kp-8)
             (cons (logior (lsh 2 16)  57) 'kp-9)
             (cons (logior (lsh 2 16)  60) 'kp-less)
             (cons (logior (lsh 2 16)  61) 'kp-equal)
             (cons (logior (lsh 2 16)  62) 'kp-more)
             (cons (logior (lsh 2 16)  64) 'kp-at)
             (cons (logior (lsh 2 16)  92) 'kp-backslash)
             (cons (logior (lsh 2 16)  96) 'kp-backtick)
             (cons (logior (lsh 2 16) 124) 'kp-bar)
             (cons (logior (lsh 2 16) 126) 'kp-tilde)
             (cons (logior (lsh 2 16) 157) 'kp-mu)
             (cons (logior (lsh 2 16) 165) 'kp-yen)
             (cons (logior (lsh 2 16) 167) 'kp-paragraph)
             (cons (logior (lsh 2 16) 172) 'left)
             (cons (logior (lsh 2 16) 173) 'up)
             (cons (logior (lsh 2 16) 174) 'right)
             (cons (logior (lsh 2 16) 175) 'down)
             (cons (logior (lsh 2 16) 176) 'kp-ring)
             (cons (logior (lsh 2 16) 201) 'kp-square)
             (cons (logior (lsh 2 16) 204) 'kp-cube)
             (cons (logior (lsh 3 16)   8) 'backspace)
             (cons (logior (lsh 3 16)   9) 'tab)
             (cons (logior (lsh 3 16)  10) 'linefeed)
             (cons (logior (lsh 3 16)  11) 'clear)
             (cons (logior (lsh 3 16)  13) 'return)
             (cons (logior (lsh 3 16)  18) 'pause)
             (cons (logior (lsh 3 16)  25) 'S-tab)
             (cons (logior (lsh 3 16)  27) 'escape)
             (cons (logior (lsh 3 16) 127) 'delete)
             ))
      (set-terminal-parameter frame 'x-setup-function-keys t))))



;;;; Miscellaneous mouse bindings.

;;; Allow shift-clicks to work just like under Nextstep
(defun mouse-extend-region (event)
  "Move point or mark so as to extend region.
This should be bound to a mouse click event type."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((posn (event-end event)))
    (if (not (windowp (posn-window posn)))
        (error "Cursor not in text area of window"))
    (select-window (posn-window posn))
    (cond
     ((not (numberp (posn-point posn))))
     ((or (not mark-active) (> (abs (- (posn-point posn) (point)))
                               (abs (- (posn-point posn) (mark)))))
      (let ((point-save (point)))
        (unwind-protect
            (progn
              (goto-char (posn-point posn))
              (push-mark nil t t)
              (or transient-mark-mode
                  (sit-for 1)))
          (goto-char point-save))))
     (t
      (goto-char (posn-point posn))))))

(define-key global-map [S-mouse-1] 'mouse-extend-region)
(global-unset-key [S-down-mouse-1])



;; Must come after keybindings.

(fmakunbound 'clipboard-yank)
(fmakunbound 'clipboard-kill-ring-save)
(fmakunbound 'clipboard-kill-region)
(fmakunbound 'menu-bar-enable-clipboard)

;; Add a couple of menus and rearrange some others; easiest just to redo toplvl
;; Note keymap defns must be given last-to-first
(define-key global-map [menu-bar] (make-sparse-keymap "menu-bar"))

(setq menu-bar-final-items
      (cond ((eq system-type 'darwin)
             '(buffer windows services help-menu))
            ;; Otherwise, GNUstep.
            (t
             '(buffer windows services hide-app quit))))

;; Add standard top-level items to GNUstep menu.
(unless (eq system-type 'darwin)
  (define-key global-map [menu-bar quit] '("Quit" . save-buffers-kill-emacs))
  (define-key global-map [menu-bar hide-app] '("Hide" . ns-do-hide-emacs)))

(define-key global-map [menu-bar services]
  (cons "Services" (make-sparse-keymap "Services")))
(define-key global-map [menu-bar windows] (make-sparse-keymap "Windows"))
(define-key global-map [menu-bar buffer]
  (cons "Buffers" global-buffers-menu-map))
;;  (cons "Buffers" (make-sparse-keymap "Buffers")))
(define-key global-map [menu-bar tools] (cons "Tools" menu-bar-tools-menu))
(define-key global-map [menu-bar options] (cons "Options" menu-bar-options-menu))
(define-key global-map [menu-bar edit] (cons "Edit" menu-bar-edit-menu))
(define-key global-map [menu-bar file] (cons "File" menu-bar-file-menu))

;; If running under GNUstep, rename "Help" to "Info"
(cond ((eq system-type 'darwin)
       (define-key global-map [menu-bar help-menu]
	 (cons "Help" menu-bar-help-menu)))
      (t
       (let ((contents (reverse (cdr menu-bar-help-menu))))
	 (setq menu-bar-help-menu
	       (append (list 'keymap) (cdr contents) (list "Info"))))
       (define-key global-map [menu-bar help-menu]
	 (cons "Info" menu-bar-help-menu))))

(if (not (eq system-type 'darwin))
    ;; in OS X it's in the app menu already
    (define-key menu-bar-help-menu [info-panel]
      '("About Emacs..." . ns-do-emacs-info-panel)))


;;;; File menu, replaces standard under ns-extended-platform-support
(defvar menu-bar-ns-file-menu (make-sparse-keymap "File"))
(define-key menu-bar-ns-file-menu [one-window]
  '("Remove Splits" . delete-other-windows))
(define-key menu-bar-ns-file-menu [split-window]
  '("Split Window" . split-window-vertically))

(define-key menu-bar-ns-file-menu [separator-print] '("--"))

(defvar ns-ps-print-menu-map (make-sparse-keymap "Postscript Print"))
(define-key ns-ps-print-menu-map [ps-print-region]
  '("Region (B+W)" . ps-print-region))
(define-key ns-ps-print-menu-map [ps-print-buffer]
  '("Buffer (B+W)" . ps-print-buffer))
(define-key ns-ps-print-menu-map [ps-print-region-faces]
  '("Region" . ps-print-region-with-faces))
(define-key ns-ps-print-menu-map [ps-print-buffer-faces]
  '("Buffer" . ps-print-buffer-with-faces))
(define-key menu-bar-ns-file-menu [postscript-print]
  (cons "Postscript Print" ns-ps-print-menu-map))

(define-key menu-bar-ns-file-menu [print-region]
  '("Print Region" . print-region))
(define-key menu-bar-ns-file-menu [print-buffer]
  '("Print Buffer" . ns-print-buffer))

(define-key menu-bar-ns-file-menu [separator-save] '("--"))

(define-key menu-bar-ns-file-menu [recover-session]
  '("Recover Crashed Session" . recover-session))
(define-key menu-bar-ns-file-menu [revert-buffer]
  '("Revert Buffer" . revert-buffer))
(define-key menu-bar-ns-file-menu [write-file]
  '("Save Buffer As..." . ns-write-file-using-panel))
(define-key menu-bar-ns-file-menu [save-buffer] '("Save Buffer" . save-buffer))

(define-key menu-bar-ns-file-menu [kill-buffer]
  '("Kill Current Buffer" . kill-this-buffer))
(define-key menu-bar-ns-file-menu [delete-this-frame]
  '("Close Frame" . delete-frame))

(define-key menu-bar-ns-file-menu [separator-open] '("--"))

(define-key menu-bar-ns-file-menu [insert-file]
  '("Insert File..." . insert-file))
(define-key menu-bar-ns-file-menu [dired]
  '("Open Directory..." . ns-open-file-using-panel))
(define-key menu-bar-ns-file-menu [open-file]
  '("Open File..." . ns-open-file-using-panel))
(define-key menu-bar-ns-file-menu [make-frame]
  '("New Frame" . make-frame))


;;;; Edit menu: Modify slightly

;; Substitute a Copy function that works better under X (for GNUstep).
(easy-menu-remove-item global-map '("menu-bar" "edit") 'copy)
(define-key-after menu-bar-edit-menu [copy]
  '(menu-item "Copy" ns-copy-including-secondary
    :enable mark-active
    :help "Copy text in region between mark and current position")
  'cut)

;; Change to same precondition as select-and-paste, as we don't have
;; `x-selection-exists-p'.
(easy-menu-remove-item global-map '("menu-bar" "edit") 'paste)
(define-key-after menu-bar-edit-menu [paste]
  '(menu-item "Paste" yank
    :enable (and (cdr yank-menu) (not buffer-read-only))
    :help "Paste (yank) text most recently cut/copied")
  'copy)

;; Change text to be more consistent with surrounding menu items `paste', etc.
(easy-menu-remove-item global-map '("menu-bar" "edit") 'paste-from-menu)
(define-key-after menu-bar-edit-menu [select-paste]
  '(menu-item "Select and Paste" yank-menu
    :enable (and (cdr yank-menu) (not buffer-read-only))
    :help "Choose a string from the kill ring and paste it")
  'paste)

;; Separate undo from cut/paste section, add spell for platform consistency.
(define-key-after menu-bar-edit-menu [separator-undo] '("--") 'undo)
(define-key-after menu-bar-edit-menu [spell] '("Spell" . ispell-menu-map) 'fill)


;;;; Windows menu
(defun menu-bar-select-frame (&optional frame)
  (interactive)
  (make-frame-visible last-command-event)
  (raise-frame last-command-event)
  (select-frame last-command-event))

(defun menu-bar-update-frames ()
  ;; If user discards the Windows item, play along.
  (when (lookup-key (current-global-map) [menu-bar windows])
    (let ((frames (frame-list))
          (frames-menu (make-sparse-keymap "Select Frame")))
      (setcdr frames-menu
              (nconc
               (mapcar (lambda (frame)
                         (list* frame
                                (cdr (assq 'name (frame-parameters frame)))
                                'menu-bar-select-frame))
                       frames)
               (cdr frames-menu)))
      (define-key frames-menu [separator-frames] '("--"))
      (define-key frames-menu [popup-color-panel]
        '("Colors..." . ns-popup-color-panel))
      (define-key frames-menu [popup-font-panel]
        '("Font Panel..." . ns-popup-font-panel))
      (define-key frames-menu [separator-arrange] '("--"))
      (define-key frames-menu [arrange-all-frames]
        '("Arrange All Frames" . ns-arrange-all-frames))
      (define-key frames-menu [arrange-visible-frames]
        '("Arrange Visible Frames" . ns-arrange-visible-frames))
      ;; Don't use delete-frame as event name
      ;; because that is a special event.
      (define-key (current-global-map) [menu-bar windows]
        (cons "Windows" frames-menu)))))

(defun force-menu-bar-update-buffers ()
  ;; This is a hack to get around fact that we already checked
  ;; frame-or-buffer-changed-p and reset it, so menu-bar-update-buffers
  ;; does not pick up any change.
  (menu-bar-update-buffers t))

(add-hook 'menu-bar-update-fab-hook 'menu-bar-update-frames)
(add-hook 'menu-bar-update-fab-hook 'force-menu-bar-update-buffers)

(defun menu-bar-update-frames-and-buffers ()
  (if (frame-or-buffer-changed-p)
      (run-hooks 'menu-bar-update-fab-hook)))

(setq menu-bar-update-hook
      (delq 'menu-bar-update-buffers menu-bar-update-hook))
(add-hook 'menu-bar-update-hook 'menu-bar-update-frames-and-buffers)

(menu-bar-update-frames-and-buffers)


;; ns-arrange functions contributed
;; by Eberhard Mandler <mandler@dbag.ulm.DaimlerBenz.COM>
(defun ns-arrange-all-frames ()
  "Arranges all frames according to topline"
  (interactive)
  (ns-arrange-frames t))

(defun ns-arrange-visible-frames ()
  "Arranges all visible frames according to topline"
  (interactive)
  (ns-arrange-frames nil))

(defun ns-arrange-frames ( vis)
  (let ((frame (next-frame))
	(end-frame (selected-frame))
	(inc-x 20)                      ;relative position of frames
	(inc-y 22)
	(x-pos 100)                     ;start position
	(y-pos 40)
	(done nil))
    (while (not done)                   ;cycle through all frames
      (if (not (or vis (eq (frame-visible-p frame) t)))
          (setq x-pos x-pos); do nothing; true case
	(set-frame-position frame x-pos y-pos)
	(setq x-pos (+ x-pos inc-x))
	(setq y-pos (+ y-pos inc-y))
	(raise-frame frame))
      (select-frame frame)
      (setq frame (next-frame))
      (setq done (equal frame end-frame)))
    (set-frame-position end-frame x-pos y-pos)
    (raise-frame frame)
    (select-frame frame)))


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
      (lexical-let ((service service))
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
              (setq deactivate-mark nil)))))))
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

(precompute-menubar-bindings)

;; nsterm.m
(defvar ns-input-spi-name)
(defvar ns-input-spi-arg)

(defun ns-spi-service-call ()
  "Respond to a service request."
  (interactive)
  (cond ((string-equal ns-input-spi-name "open-selection")
	 (switch-to-buffer (generate-new-buffer "*untitled*"))
	 (insert ns-input-spi-arg))
	((string-equal ns-input-spi-name "open-file")
	 (dnd-open-file ns-input-spi-arg nil))
	((string-equal ns-input-spi-name "mail-selection")
	 (compose-mail)
	 (rfc822-goto-eoh)
	 (forward-line 1)
	 (insert ns-input-spi-arg))
	((string-equal ns-input-spi-name "mail-to")
	 (compose-mail ns-input-spi-arg))
	(t (error (concat "Service " ns-input-spi-name " not recognized")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; Composed key sequence handling for Nextstep system input methods.
;;;; (On Nextstep systems, input methods are provided for CJK
;;;; characters, etc. which require multiple keystrokes, and during
;;;; entry a partial ("working") result is typically shown in the
;;;; editing window.)

(defface ns-working-text-face
  '((t :underline t))
  "Face used to highlight working text during compose sequence insert."
  :group 'ns)

(defvar ns-working-overlay nil
  "Overlay used to highlight working text during compose sequence insert.")
(make-variable-buffer-local 'ns-working-overlay)
(defvar ns-working-overlay-len 0
  "Length of working text during compose sequence insert.")
(make-variable-buffer-local 'ns-working-overlay-len)

;; Based on mac-win.el 2007/08/26 unicode-2.  This will fail if called
;; from an "interactive" function.
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

;; Currently not used, doesn't work because the 'interactive' here stays
;; for subinvocations.
(defun ns-insert-working-text ()
  (interactive)
  (if (ns-in-echo-area) (ns-echo-working-text) (ns-put-working-text)))

(defvar ns-working-text)		; nsterm.m

(defun ns-put-working-text ()
  "Insert contents of ns-working-text as UTF8 string and mark with
ns-working-overlay.  Any previously existing working text is cleared first.
The overlay is assigned the face ns-working-text-face."
  (interactive)
  (if ns-working-overlay (ns-delete-working-text))
  (let ((start (point)))
    (insert ns-working-text)
    (overlay-put (setq ns-working-overlay (make-overlay start (point)
							(current-buffer) nil t))
		 'face 'ns-working-text-face)
    (setq ns-working-overlay-len (+ ns-working-overlay-len (- (point) start)))))

(defun ns-echo-working-text ()
  "Echo contents of ns-working-text in message display area.
See ns-insert-working-text."
  (if ns-working-overlay (ns-unecho-working-text))
  (let* ((msg (current-message))
	 (msglen (length msg))
	 message-log-max)
    (setq ns-working-overlay-len (length ns-working-text))
    (setq msg (concat msg ns-working-text))
    (put-text-property msglen (+ msglen ns-working-overlay-len) 'face 'ns-working-text-face msg)
    (message "%s" msg)
    (setq ns-working-overlay t)))

(defun ns-delete-working-text()
  "Delete working text and clear ns-working-overlay."
  (interactive)
  (delete-backward-char ns-working-overlay-len)
  (setq ns-working-overlay-len 0)
  (delete-overlay ns-working-overlay))

(defun ns-unecho-working-text()
  "Delete working text from echo area and clear ns-working-overlay."
  (let ((msg (current-message))
	message-log-max)
    (setq msg (substring msg 0 (- (length msg) ns-working-overlay-len)))
    (setq ns-working-overlay-len 0)
    (setq ns-working-overlay nil)))


(declare-function ns-convert-utf8-nfd-to-nfc "nsfns.m" (str))

;;;; OS X file system Unicode UTF-8 NFD (decomposed form) support
;; Lisp code based on utf-8m.el, by Seiji Zenitani, Eiji Honjoh, and
;; Carsten Bormann.
(if (eq system-type 'darwin)
    (progn

      (defun ns-utf8-nfd-post-read-conversion (length)
	"Calls ns-convert-utf8-nfd-to-nfc to compose char sequences."
	(save-excursion
	  (save-restriction
	    (narrow-to-region (point) (+ (point) length))
	    (let ((str (buffer-string)))
	      (delete-region (point-min) (point-max))
	      (insert (ns-convert-utf8-nfd-to-nfc str))
	      (- (point-max) (point-min))
	      ))))

      (define-coding-system 'utf-8-nfd
	"UTF-8 NFD (decomposed) encoding."
	:coding-type 'utf-8
	:mnemonic ?U
	:charset-list '(unicode)
	:post-read-conversion 'ns-utf8-nfd-post-read-conversion)
      (set-file-name-coding-system 'utf-8-nfd)))

;; PENDING: disable composition-based display for Indic scripts as it
;;        is not working well under Nextstep for some reason
(set-char-table-range composition-function-table
                      '(#x0900 . #x0DFF) nil)


;;;; Inter-app communications support.

(defvar ns-input-text)			; nsterm.m

(defun ns-insert-text ()
  "Insert contents of ns-input-text at point."
  (interactive)
  (insert ns-input-text)
  (setq ns-input-text nil))

(defun ns-insert-file ()
  "Insert contents of file ns-input-file like insert-file but with less
prompting.  If file is a directory perform a find-file on it."
  (interactive)
  (let ((f))
    (setq f (car ns-input-file))
    (setq ns-input-file (cdr ns-input-file))
    (if (file-directory-p f)
        (find-file f)
      (push-mark (+ (point) (car (cdr (insert-file-contents f))))))))

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
    (goto-line (if (consp ns-input-line)
                   (min (car ns-input-line) (cdr ns-input-line))
                 ns-input-line)))
   (ns-input-line
    (if (not ns-select-overlay)
        (overlay-put (setq ns-select-overlay (make-overlay (point-min) (point-min)))
                     'face 'highlight))
    (let ((beg (save-excursion
                 (goto-line (if (consp ns-input-line)
                                (min (car ns-input-line) (cdr ns-input-line))
                              ns-input-line))
                 (point)))
          (end (save-excursion
                 (goto-line (+ 1 (if (consp ns-input-line)
                                     (max (car ns-input-line) (cdr ns-input-line))
                                   ns-input-line)))
                 (point))))
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
(defvar ns-command-modifier)
(defvar ns-control-modifier)
(defvar ns-function-modifier)
(defvar ns-antialias-text)
(defvar ns-use-qd-smoothing)
(defvar ns-use-system-highlight-color)

(declare-function ns-set-resource "nsfns.m" (owner name value))
(declare-function ns-font-name "nsfns.m" (name))
(declare-function ns-read-file-name "nsfns.m"
		  (prompt &optional dir isLoad init))

(defun ns-save-preferences ()
  "Set all the defaults."
  (interactive)
  ;; Global preferences
  (ns-set-resource nil "AlternateModifier" (symbol-name ns-alternate-modifier))
  (ns-set-resource nil "CommandModifier" (symbol-name ns-command-modifier))
  (ns-set-resource nil "ControlModifier" (symbol-name ns-control-modifier))
  (ns-set-resource nil "FunctionModifier" (symbol-name ns-function-modifier))
  (ns-set-resource nil "CursorBlinkRate"
                   (if ns-cursor-blink-rate
                       (number-to-string ns-cursor-blink-rate)
                     "NO"))
  (ns-set-resource nil "ExpandSpace"
                   (if ns-expand-space
                       (number-to-string ns-expand-space)
                     "NO"))
  (ns-set-resource nil "GSFontAntiAlias" (if ns-antialias-text "YES" "NO"))
  (ns-set-resource nil "UseQuickdrawSmoothing"
		   (if ns-use-qd-smoothing "YES" "NO"))
  (ns-set-resource nil "UseSystemHighlightColor"
		   (if ns-use-system-highlight-color "YES" "NO"))
  ;; Default frame parameters
  (let ((p (frame-parameters))
	v)
    (if (setq v (assq 'font p))
	(ns-set-resource nil "Font" (ns-font-name (cdr v))))
    (if (setq v (assq 'fontsize p))
	(ns-set-resource nil "FontSize" (number-to-string (cdr v))))
    (if (setq v (assq 'foreground-color p))
	(ns-set-resource nil "Foreground" (cdr v)))
    (if (setq v (assq 'background-color p))
	(ns-set-resource nil "Background" (cdr v)))
    (if (setq v (assq 'cursor-color p))
	(ns-set-resource nil "CursorColor" (cdr v)))
    (if (setq v (assq 'cursor-type p))
	(ns-set-resource nil "CursorType" (if (symbolp (cdr v))
					      (symbol-name (cdr v))
					    (cdr v))))
    (if (setq v (assq 'underline p))
	(ns-set-resource nil "Underline"
			 (case (cdr v)
			       ((t) "YES")
			       ((nil) "NO")
			       (t (cdr v)))))
    (if (setq v (assq 'internal-border-width p))
	(ns-set-resource nil "InternalBorderWidth"
			 (number-to-string (cdr v))))
    (if (setq v (assq 'vertical-scroll-bars p))
	(ns-set-resource nil "VerticalScrollBars"
			 (case (cdr v)
			       ((t) "YES")
			       ((nil) "NO")
			       ((left) "left")
			       ((right) "right")
			       (t nil))))
    (if (setq v (assq 'height p))
	(ns-set-resource nil "Height" (number-to-string (cdr v))))
    (if (setq v (assq 'width p))
	(ns-set-resource nil "Width" (number-to-string (cdr v))))
    (if (setq v (assq 'top p))
	(ns-set-resource nil "Top" (number-to-string (cdr v))))
    (if (setq v (assq 'left p))
	(ns-set-resource nil "Left" (number-to-string (cdr v))))
    ;; These not fully supported
    (if (setq v (assq 'auto-raise p))
	(ns-set-resource nil "AutoRaise" (if (cdr v) "YES" "NO")))
    (if (setq v (assq 'auto-lower p))
	(ns-set-resource nil "AutoLower" (if (cdr v) "YES" "NO")))
    (if (setq v (assq 'menu-bar-lines p))
	(ns-set-resource nil "Menus" (if (cdr v) "YES" "NO")))
    )
  (let ((fl (face-list)))
    (while (consp fl)
      (or (eq 'default (car fl))
          ;; dont save Default* since it causes all created faces to
          ;; inherit its values.  The properties of the default face
          ;; have already been saved from the frame-parameters anyway.
          (let* ((name (symbol-name (car fl)))
                 (font (face-font (car fl)))
                 ;; (fontsize (face-fontsize (car fl)))
                 (foreground (face-foreground (car fl)))
                 (background (face-background (car fl)))
                 (underline (face-underline-p (car fl)))
                 (italic (face-italic-p (car fl)))
                 (bold (face-bold-p (car fl)))
                 (stipple (face-stipple (car fl))))
            ;; (ns-set-resource nil (concat name ".attributeFont")
            ;;                  (if font font nil))
            ;; (ns-set-resource nil (concat name ".attributeFontSize")
            ;;                  (if fontsize (number-to-string fontsize) nil))
            (ns-set-resource nil (concat name ".attributeForeground")
                             (if foreground foreground nil))
            (ns-set-resource nil (concat name ".attributeBackground")
                             (if background background nil))
            (ns-set-resource nil (concat name ".attributeUnderline")
                             (if underline "YES" nil))
            (ns-set-resource nil (concat name ".attributeItalic")
                             (if italic "YES" nil))
            (ns-set-resource nil (concat name ".attributeBold")
                             (if bold "YES" nil))
            (and stipple
                 (or (stringp stipple)
                     (setq stipple (prin1-to-string stipple))))
            (ns-set-resource nil (concat name ".attributeStipple")
                             (if stipple stipple nil))))
      (setq fl (cdr fl)))))

(declare-function menu-bar-options-save-orig "ns-win" () t)

;; call ns-save-preferences when menu-bar-options-save is called
(fset 'menu-bar-options-save-orig (symbol-function 'menu-bar-options-save))
(defun ns-save-options ()
  (interactive)
  (menu-bar-options-save-orig)
  (ns-save-preferences))
(fset 'menu-bar-options-save (symbol-function 'ns-save-options))


;;;; File handling.

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

(defvar ns-pop-up-frames 'fresh
  "*Non-nil means open files upon request from the Workspace in a new frame.
If t, always do so.  Any other non-nil value means open a new frame
unless the current buffer is a scratch buffer.")

(declare-function ns-hide-emacs "nsfns.m" (on))

(defun ns-find-file ()
  "Do a find-file with the ns-input-file as argument."
  (interactive)
  (let ((f) (file) (bufwin1) (bufwin2))
    (setq f (file-truename (car ns-input-file)))
    (setq ns-input-file (cdr ns-input-file))
    (setq file (find-file-noselect f))
    (setq bufwin1 (get-buffer-window file 'visible))
    (setq bufwin2 (get-buffer-window "*scratch*" 'visibile))
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



;;;; Frame-related functions.

;; Don't show the frame name; that's redundant with Nextstep.
(setq-default mode-line-frame-identification '("  "))

;; You say tomAYto, I say tomAHto..
(defvaralias 'ns-option-modifier 'ns-alternate-modifier)

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

(defun ns-next-frame ()
  "Switch to next visible frame."
  (interactive)
  (other-frame 1))
(defun ns-prev-frame ()
  "Switch to previous visible frame."
  (interactive)
  (other-frame -1))

;; If no position specified, make new frame offset by 25 from current.
(defvar parameters)		     ; dynamically bound in make-frame

(add-hook 'before-make-frame-hook
          (lambda ()
            (let ((left (cdr (assq 'left (frame-parameters))))
                  (top (cdr (assq 'top (frame-parameters)))))
              (if (consp left) (setq left (cadr left)))
              (if (consp top) (setq top (cadr top)))
              (cond
               ((or (assq 'top parameters) (assq 'left parameters)))
               ((or (not left) (not top)))
               (t
                (setq parameters (cons (cons 'left (+ left 25))
                                       (cons (cons 'top (+ top 25))
                                             parameters))))))))

;; frame will be focused anyway, so select it
(add-hook 'after-make-frame-functions 'select-frame)

;; (defun ns-win-suspend-error ()
;;   (error "Suspending an emacs running under *Step/OS X makes no sense"))
;; (add-hook 'suspend-hook 'ns-win-suspend-error)
;; (substitute-key-definition 'suspend-emacs 'iconify-or-deiconify-frame
;; 			   global-map)

;; Based on a function by David Reitter <dreitter@inf.ed.ac.uk> ;
;; see http://lists.gnu.org/archive/html/emacs-devel/2005-09/msg00681.html .
(defun ns-toggle-toolbar (&optional frame)
  "Switches the tool bar on and off in frame FRAME.
 If FRAME is nil, the change applies to the selected frame."
  (interactive)
  (modify-frame-parameters
   frame (list (cons 'tool-bar-lines
		       (if (> (or (frame-parameter frame 'tool-bar-lines) 0) 0)
				   0 1)) ))
  (if (not tool-bar-mode) (tool-bar-mode t)))

(defvar ns-cursor-blink-mode) 		; nsterm.m

;; Redefine from frame.el.
(define-minor-mode blink-cursor-mode
  "Toggle blinking cursor mode.
With a numeric argument, turn blinking cursor mode on if ARG is positive,
otherwise turn it off.  When blinking cursor mode is enabled, the
cursor of the selected window blinks.

Note that this command is effective only when Emacs
displays through a window system, because then Emacs does its own
cursor display.  On a text-only terminal, this is not implemented."
  :init-value (not (or noninteractive
		       no-blinking-cursor
		       (eq ns-cursor-blink-rate nil)))
  :initialize 'custom-initialize-safe-default
  :group 'cursor
  :global t
  (if blink-cursor-mode
      (setq ns-cursor-blink-mode t)
      (setq ns-cursor-blink-mode nil)))



;;;; Dialog-related functions.

;; Ask user for confirm before printing.  Due to Kevin Rodgers.
(defun ns-print-buffer ()
  "Interactive front-end to `print-buffer': asks for user confirmation first."
  (interactive)
  (if (and (interactive-p)
           (or (listp last-nonmenu-event)
               (and (char-or-string-p (event-basic-type last-command-event))
                    (memq 'super (event-modifiers last-command-event)))))
      (let ((last-nonmenu-event (if (listp last-nonmenu-event)
                                    last-nonmenu-event
                                  ;; Fake it:
                                  `(mouse-1 POSITION 1))))
        (if (y-or-n-p (format "Print buffer %s? " (buffer-name)))
            (print-buffer)
	  (error "Cancelled")))
    (print-buffer)))

(defun ns-yes-or-no-p (prompt)
  "Ask user a \"yes or no\" question using a Nextstep graphical panel.
PROMPT is the prompt string."
  (interactive)
  (setq last-nonmenu-event nil)
  (yes-or-no-p prompt))


;;;; Font support.

;; Needed for font listing functions under both backend and normal
(setq scalable-fonts-allowed t)

;; Set to use font panel instead
(defalias 'generate-fontset-menu 'ns-popup-font-panel)
(defalias 'mouse-set-font 'ns-popup-font-panel)

;; nsterm.m
(defvar ns-input-font)
(defvar ns-input-fontsize)

(defun ns-respond-to-change-font ()
  "Respond to changeFont: event, expecting ns-input-font and\n\
ns-input-fontsize of new font."
  (interactive)
  (modify-frame-parameters (selected-frame)
                           (list (cons 'font ns-input-font)
                                 (cons 'fontsize ns-input-fontsize)))
  (set-frame-font ns-input-font))


;; Default fontset for Mac OS X.  This is mainly here to show how a fontset
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
come with OS X\".
See the documentation of `create-fontset-from-fontset-spec for the format.")

;; Conditional on new-fontset so bootstrapping works on non-GUI compiles.
(if (fboundp 'new-fontset)
    (progn
      ;; Setup the default fontset.
      (setup-default-fontset)
      ;; Create the standard fontset.
      (create-fontset-from-fontset-spec ns-standard-fontset-spec t)))

;;(push (cons 'font "-ns-*-*-*-*-*-10-*-*-*-*-*-fontset-standard")
;;      default-frame-alist)

;; Add some additional scripts to var we use for fontset generation.
(setq script-representative-chars
      (cons '(kana #xff8a)
	    (cons '(symbol #x2295 #x2287 #x25a1)
                  script-representative-chars)))


;;;; Pasteboard support.

(declare-function ns-get-cut-buffer-internal "nsselect.m" (buffer))

(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard."
  (ns-get-cut-buffer-internal 'PRIMARY))

(declare-function ns-store-cut-buffer-internal "nsselect.m" (buffer string))

(defun ns-set-pasteboard (string)
  "Store STRING into the pasteboard of the Nextstep display server."
  ;; Check the data type of STRING.
  (if (not (stringp string)) (error "Nonstring given to pasteboard"))
  (ns-store-cut-buffer-internal 'PRIMARY string))

;; We keep track of the last text selected here, so we can check the
;; current selection against it, and avoid passing back our own text
;; from x-cut-buffer-or-selection-value.
(defvar ns-last-selected-text nil)

(defun x-select-text (text &optional push)
  "Put TEXT, a string, on the pasteboard."
  ;; Don't send the pasteboard too much text.
  ;; It becomes slow, and if really big it causes errors.
  (ns-set-pasteboard text)
  (setq ns-last-selected-text text))

;; Return the value of the current Nextstep selection.  For
;; compatibility with older Nextstep applications, this checks cut
;; buffer 0 before retrieving the value of the primary selection.
(defun x-cut-buffer-or-selection-value ()
  (let (text)

    ;; Consult the selection, then the cut buffer.  Treat empty strings
    ;; as if they were unset.
    (or text (setq text (ns-get-pasteboard)))
    (if (string= text "") (setq text nil))

    (cond
     ((not text) nil)
     ((eq text ns-last-selected-text) nil)
     ((string= text ns-last-selected-text)
      ;; Record the newer string, so subsequent calls can use the `eq' test.
      (setq ns-last-selected-text text)
      nil)
     (t
      (setq ns-last-selected-text text)))))

(defun ns-copy-including-secondary ()
  (interactive)
  (call-interactively 'kill-ring-save)
  (ns-store-cut-buffer-internal 'SECONDARY
				(buffer-substring (point) (mark t))))
(defun ns-paste-secondary ()
  (interactive)
  (insert (ns-get-cut-buffer-internal 'SECONDARY)))

;; PENDING: not sure what to do here.. for now interprog- are set in
;; init-fn-keys, and unsure whether these x- settings have an effect.
;;(setq interprogram-cut-function 'x-select-text
;;      interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; These only needed if above not working.

(set-face-background 'region "ns_selection_color")



;;;; Scrollbar handling.

(global-set-key [vertical-scroll-bar down-mouse-1] 'ns-handle-scroll-bar-event)
(global-unset-key [vertical-scroll-bar mouse-1])
(global-unset-key [vertical-scroll-bar drag-mouse-1])

(defun ns-scroll-bar-move (event)
  "Scroll the frame according to an Nextstep scroller event."
  (interactive "e")
  (let* ((pos (event-end event))
         (window (nth 0 pos))
         (scale (nth 2 pos)))
    (save-excursion
      (set-buffer (window-buffer window))
      (cond
       ((eq (car scale) (cdr scale))
	(goto-char (point-max)))
       ((= (car scale) 0)
	(goto-char (point-min)))
       (t
	(goto-char (+ (point-min) 1
		      (scroll-bar-scale scale (- (point-max) (point-min)))))))
      (beginning-of-line)
      (set-window-start window (point))
      (vertical-motion (/ (window-height window) 2) window))))

(defun ns-handle-scroll-bar-event (event)
  "Handle scroll bar EVENT to emulate Mac Toolbox style scrolling."
  (interactive "e")
  (let* ((position (event-start event))
	 (bar-part (nth 4 position))
	 (window (nth 0 position))
	 (old-window (selected-window)))
    (cond
     ((eq bar-part 'ratio)
      (ns-scroll-bar-move event))
     ((eq bar-part 'handle)
      (if (eq window (selected-window))
	  (track-mouse (ns-scroll-bar-move event))
        ;; track-mouse faster for selected window, slower for unselected.
	(ns-scroll-bar-move event)))
     (t
      (select-window window)
      (cond
       ((eq bar-part 'up)
	(goto-char (window-start window))
	(scroll-down 1))
       ((eq bar-part 'above-handle)
	(scroll-down))
       ((eq bar-part 'below-handle)
	(scroll-up))
       ((eq bar-part 'down)
	(goto-char (window-start window))
	(scroll-up 1)))
      (select-window old-window)))))


;;;; Color support.

(declare-function ns-list-colors "nsfns.m" (&optional frame))

(defvar x-colors (ns-list-colors)
  "The list of colors defined in non-PANTONE color files.")
(defvar colors x-colors
  "The list of colors defined in non-PANTONE color files.")

(defun xw-defined-colors (&optional frame)
  "Return a list of colors supported for a particular frame.
The argument FRAME specifies which frame to try.
The value may be different for frames on different Nextstep displays."
  (or frame (setq frame (selected-frame)))
  (let ((all-colors x-colors)
	(this-color nil)
	(defined-colors nil))
    (while all-colors
      (setq this-color (car all-colors)
	    all-colors (cdr all-colors))
      ;; (and (face-color-supported-p frame this-color t)
      (setq defined-colors (cons this-color defined-colors))) ;;)
    defined-colors))

(declare-function ns-set-alpha "nsfns.m" (color alpha))

;; Convenience and work-around for fact that set color fns now require named.
(defun ns-set-background-alpha (alpha)
  "Sets alpha (opacity) of background.
Set from 0.0 (fully transparent) to 1.0 (fully opaque; default).
Note, tranparency works better on Tiger (10.4) and higher."
  (interactive "nSet background alpha to: ")
  (let ((bgcolor (cdr (assq 'background-color (frame-parameters)))))
    (set-frame-parameter (selected-frame)
			 'background-color (ns-set-alpha bgcolor alpha))))

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
      'modeline)
     ((eq window-pos 'vertical-line)
      'default)
     ((consp window-pos)
      (save-excursion
        (set-buffer buffer)
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

(defvar ns-input-color)			; nsterm.m

(defun ns-set-foreground-at-mouse ()
  "Set the foreground color at the mouse location to ns-input-color."
  (interactive)
  (let* ((pos (mouse-position))
         (frame (car pos))
         (face (ns-face-at-pos pos)))
    (cond
     ((eq face 'cursor)
      (modify-frame-parameters frame (list (cons 'cursor-color
                                                 ns-input-color))))
     ((not face)
      (modify-frame-parameters frame (list (cons 'foreground-color
                                                 ns-input-color))))
     (t
      (set-face-foreground face ns-input-color frame)))))

(defun ns-set-background-at-mouse ()
  "Set the background color at the mouse location to ns-input-color."
  (interactive)
  (let* ((pos (mouse-position))
         (frame (car pos))
         (face (ns-face-at-pos pos)))
    (cond
     ((eq face 'cursor)
      (modify-frame-parameters frame (list (cons 'cursor-color
                                                 ns-input-color))))
     ((not face)
      (modify-frame-parameters frame (list (cons 'background-color
                                                 ns-input-color))))
     (t
      (set-face-background face ns-input-color frame)))))

;; Set some options to be as Nextstep-like as possible.
(setq frame-title-format t
      icon-title-format t)

;; Set up browser connectivity.
(defvar browse-url-generic-program)

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program
      (cond ((eq system-type 'darwin) "open")
            ;; Otherwise, GNUstep.
            (t "gopen")))


(defvar ns-initialized nil
  "Non-nil if Nextstep windowing has been initialized.")

(declare-function ns-list-services "nsfns.m" ())

;; Do the actual Nextstep Windows setup here; the above code just
;; defines functions and variables that we use now.
(defun ns-initialize-window-system ()
  "Initialize Emacs for Nextstep (Cocoa / GNUstep) windowing."

  ;; PENDING: not needed?
  (setq command-line-args (ns-handle-args command-line-args))

  (x-open-connection (system-name) nil t)

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
  (mouse-wheel-mode 1)

  (setq ns-initialized t))

(add-to-list 'handle-args-function-alist '(ns . ns-handle-args))
(add-to-list 'frame-creation-function-alist '(ns . x-create-frame-with-faces))
(add-to-list 'window-system-initialization-alist '(ns . ns-initialize-window-system))


(provide 'ns-win)

;; arch-tag: eb138a45-4e2e-4d68-b1c9-a39665731644
;;; ns-win.el ends here
