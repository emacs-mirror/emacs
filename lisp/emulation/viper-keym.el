;;; viper-keym.el -- Main Viper keymaps

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


(require 'viper-util)

;;; Variables

;;; Keymaps

;; Keymaps for vital things like \e and C-z.
;; Not for users
(defvar vip-vi-intercept-map (make-sparse-keymap))
(defvar vip-insert-intercept-map (make-sparse-keymap))
(defvar vip-emacs-intercept-map (make-sparse-keymap))
  
(vip-deflocalvar vip-vi-local-user-map (make-sparse-keymap)
  "Keymap for user-defined local bindings.
Useful for changing bindings such as ZZ in certain major modes.
For instance, in letter-mode, one may want to bind ZZ to
mh-send-letter. In a newsreader such as gnus, tin, or rn, ZZ could be bound
to save-buffers-kill-emacs then post article, etc.")
(put 'vip-vi-local-user-map 'permanent-local t)	

(defvar vip-vi-global-user-map (make-sparse-keymap)
  "Keymap for user-defined global bindings.
These bindings are seen in all Viper buffers.")

(defvar vip-vi-basic-map (make-keymap)
  "This is the main keymap in effect in Viper's Vi state.
This map is global, shared by all buffers.")

(defvar  vip-vi-kbd-map (make-sparse-keymap)
  "This keymap keeps keyboard macros defined via the :map command.")

(defvar vip-vi-diehard-map (make-sparse-keymap)
  "This keymap is in use when the user asks Viper to simulate Vi very closely.
This happens when vip-expert-level is 1 or 2. See vip-set-expert-level.")
  

(vip-deflocalvar vip-insert-local-user-map (make-sparse-keymap)
  "Auxiliary map for per-buffer user-defined keybindings in Insert state.")
(put 'vip-insert-local-user-map 'permanent-local t)	

(defvar vip-insert-global-user-map (make-sparse-keymap)
  "Auxiliary map for global user-defined bindings in Insert state.")

(defvar vip-insert-basic-map (make-sparse-keymap)
  "The basic insert-mode keymap.")

(defvar vip-insert-diehard-map (make-keymap)
  "Map used when user wants vi-style keys in insert mode.
Most of the Emacs keys are suppressed. This map overshadows
vip-insert-basic-map. Not recommended, except for novice users.")

(defvar  vip-insert-kbd-map  (make-sparse-keymap)
  "This keymap keeps VI-style kbd macros for insert mode.")

(defvar vip-replace-map (make-sparse-keymap)
  "Map used in Viper's replace state.")
  
(defvar vip-emacs-global-user-map (make-sparse-keymap)
  "Auxiliary map for global user-defined bindings in Emacs state.")

(defvar  vip-emacs-kbd-map  (make-sparse-keymap)
  "This keymap keeps Vi-style kbd macros for emacs mode.")
  
(vip-deflocalvar vip-emacs-local-user-map  (make-sparse-keymap)
  "Auxiliary map for local user-defined bindings in Emacs state.")
(put 'vip-emacs-local-user-map 'permanent-local t)  

;; This keymap should stay empty
(defvar vip-empty-keymap (make-sparse-keymap))


;;; Variables used by minor modes

;; Association list of the form 
;; ((major-mode . keymap) (major-mode . keymap) ...)
;; Viper uses these keymaps to make user-requested adjustments
;; to its Vi state in various major modes.")
(defvar vip-vi-state-modifier-alist nil)

;; Association list of the form 
;; ((major-mode . keymap) (major-mode . keymap) ...)
;; Viper uses these keymaps to make user-requested adjustments
;; to its Insert state in various major modes.")
(defvar vip-insert-state-modifier-alist nil)

;; Association list of the form 
;; ((major-mode . keymap) (major-mode . keymap) ...)
;; Viper uses these keymaps to make user-requested adjustments
;; to its Emacs state in various major modes.
(defvar vip-emacs-state-modifier-alist nil)

;; Tells vip-add-local-keys to create a new vip-vi-local-user-map for new
;; buffers. Not a user option.
(vip-deflocalvar vip-need-new-vi-local-map t "")
(put 'vip-need-new-vi-local-map  'permanent-local t)

;; Tells vip-add-local-keys to create a new vip-insert-local-user-map for new
;; buffers. Not a user option.
(vip-deflocalvar vip-need-new-insert-local-map t "")
(put 'vip-need-new-insert-local-map  'permanent-local t)

;; Tells vip-add-local-keys to create a new vip-emacs-local-user-map for new
;; buffers. Not a user option.
(vip-deflocalvar vip-need-new-emacs-local-map t "")
(put 'vip-need-new-emacs-local-map  'permanent-local t)



;; Insert mode keymap

;; for novice users, pretend you are the real vi.
(define-key vip-insert-diehard-map "\t"   'vip-insert-tab)
(define-key vip-insert-diehard-map "\C-a" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-b" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-c" 'vip-change-state-to-vi)
(define-key vip-insert-diehard-map "\C-e" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-f" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-g" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-i" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-k" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-l" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-n" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-o" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-p" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-q" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-r" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-s" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-u" 'vip-erase-line)
(define-key vip-insert-diehard-map "\C-x" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-y" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-z" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-]" 'self-insert-command)
(define-key vip-insert-diehard-map "\C-_" 'self-insert-command)

(let ((i ?\ ))
  (while (<= i ?~)
    (define-key vip-insert-diehard-map (make-string 1 i) 'self-insert-command)
    (setq i (1+ i))))

;; Insert mode map when user wants emacs style
(define-key vip-insert-basic-map "\C-d" 'vip-backward-indent)
(define-key vip-insert-basic-map "\C-w" 'vip-delete-backward-word)
(define-key vip-insert-basic-map "\C-t" 'vip-forward-indent)
(define-key vip-insert-basic-map 
  (if vip-xemacs-p [(shift tab)] [S-tab]) 'vip-insert-tab)
(define-key vip-insert-basic-map "\C-v" 'quoted-insert)
(define-key vip-insert-basic-map "\C-?" 'vip-del-backward-char-in-insert)
(define-key vip-insert-basic-map "\C-c\M-p"
  'vip-insert-prev-from-insertion-ring)
(define-key vip-insert-basic-map "\C-c\M-n"
  'vip-insert-next-from-insertion-ring)


;; Replace keymap
(define-key vip-replace-map "\C-t" 'vip-forward-indent)
(define-key vip-replace-map "\C-j" 'vip-replace-state-exit-cmd)
(define-key vip-replace-map "\C-m" 'vip-replace-state-exit-cmd)
(define-key vip-replace-map "\C-?" 'vip-del-backward-char-in-replace)



;; Vi keymaps

(define-key vip-vi-basic-map "\C-^" 
  (function (lambda () (interactive) (vip-ex "e#"))))
(define-key vip-vi-basic-map "\C-b" 'vip-scroll-back)
(define-key vip-vi-basic-map "\C-d" 'vip-scroll-up)
(define-key vip-vi-basic-map "\C-e" 'vip-scroll-up-one)
(define-key vip-vi-basic-map "\C-f" 'vip-scroll)
(define-key vip-vi-basic-map "\C-m" 'vip-next-line-at-bol)
(define-key vip-vi-basic-map "\C-u" 'vip-scroll-down)
(define-key vip-vi-basic-map "\C-y" 'vip-scroll-down-one)
(define-key vip-vi-basic-map "\C-s" 'vip-isearch-forward)
(define-key vip-vi-basic-map "\C-r" 'vip-isearch-backward)
;(define-key vip-vi-basic-map "\C-\\" 'universal-argument)
(define-key vip-vi-basic-map "\C-c/" 'vip-toggle-search-style)
(define-key vip-vi-basic-map "\C-cg" 'vip-info-on-file)

(define-key vip-vi-basic-map "\C-c\M-p" 'vip-prev-destructive-command)
(define-key vip-vi-basic-map "\C-c\M-n" 'vip-next-destructive-command)


(define-key vip-vi-basic-map " " 'vip-forward-char)
(define-key vip-vi-basic-map "!" 'vip-command-argument)
(define-key vip-vi-basic-map "\"" 'vip-command-argument)
(define-key vip-vi-basic-map "#" 'vip-command-argument)
(define-key vip-vi-basic-map "$" 'vip-goto-eol)
(define-key vip-vi-basic-map "%" 'vip-paren-match)
(define-key vip-vi-basic-map "&"
  (function (lambda () (interactive) (vip-ex "&"))))
(define-key vip-vi-basic-map "'" 'vip-goto-mark-and-skip-white)
(define-key vip-vi-basic-map "(" 'vip-backward-sentence)
(define-key vip-vi-basic-map ")" 'vip-forward-sentence)
(define-key vip-vi-basic-map "*" 'call-last-kbd-macro)
(define-key vip-vi-basic-map "+" 'vip-next-line-at-bol)
(define-key vip-vi-basic-map "," 'vip-repeat-find-opposite)
(define-key vip-vi-basic-map "-" 'vip-previous-line-at-bol)
(define-key vip-vi-basic-map "." 'vip-repeat)
(define-key vip-vi-basic-map "/" 'vip-search-forward)

(define-key vip-vi-basic-map "0" 'vip-beginning-of-line)
(define-key vip-vi-basic-map "1" 'vip-digit-argument)
(define-key vip-vi-basic-map "2" 'vip-digit-argument)
(define-key vip-vi-basic-map "3" 'vip-digit-argument)
(define-key vip-vi-basic-map "4" 'vip-digit-argument)
(define-key vip-vi-basic-map "5" 'vip-digit-argument)
(define-key vip-vi-basic-map "6" 'vip-digit-argument)
(define-key vip-vi-basic-map "7" 'vip-digit-argument)
(define-key vip-vi-basic-map "8" 'vip-digit-argument)
(define-key vip-vi-basic-map "9" 'vip-digit-argument)

(define-key vip-vi-basic-map ":" 'vip-ex)
(define-key vip-vi-basic-map ";" 'vip-repeat-find)
(define-key vip-vi-basic-map "<" 'vip-command-argument)
(define-key vip-vi-basic-map "=" 'vip-command-argument)
(define-key vip-vi-basic-map ">" 'vip-command-argument)
(define-key vip-vi-basic-map "?" 'vip-search-backward)
(define-key vip-vi-basic-map "@" 'vip-register-macro)

(define-key vip-vi-basic-map "A" 'vip-Append)
(define-key vip-vi-basic-map "B" 'vip-backward-Word)
(define-key vip-vi-basic-map "C" 'vip-change-to-eol)
(define-key vip-vi-basic-map "D" 'vip-kill-line)
(define-key vip-vi-basic-map "E" 'vip-end-of-Word)
(define-key vip-vi-basic-map "F" 'vip-find-char-backward)
(define-key vip-vi-basic-map "G" 'vip-goto-line)
(define-key vip-vi-basic-map "H" 'vip-window-top)
(define-key vip-vi-basic-map "I" 'vip-Insert)
(define-key vip-vi-basic-map "J" 'vip-join-lines)
(define-key vip-vi-basic-map "K" 'vip-nil)
(define-key vip-vi-basic-map "L" 'vip-window-bottom)
(define-key vip-vi-basic-map "M" 'vip-window-middle)
(define-key vip-vi-basic-map "N" 'vip-search-Next)
(define-key vip-vi-basic-map "O" 'vip-Open-line)
(define-key vip-vi-basic-map "P" 'vip-Put-back)
(define-key vip-vi-basic-map "Q" 'vip-query-replace)
(define-key vip-vi-basic-map "R" 'vip-overwrite)
(define-key vip-vi-basic-map "S" 'vip-substitute-line)
(define-key vip-vi-basic-map "T" 'vip-goto-char-backward)
(define-key vip-vi-basic-map "U" 'vip-undo)
(define-key vip-vi-basic-map "V" 'find-file-other-window)
(define-key vip-vi-basic-map "W" 'vip-forward-Word)
(define-key vip-vi-basic-map "X" 'vip-delete-backward-char)
(define-key vip-vi-basic-map "Y" 'vip-yank-line)
(define-key vip-vi-basic-map "ZZ" 'vip-save-kill-buffer)

(define-key vip-vi-basic-map "\\" 'vip-escape-to-emacs)
(define-key vip-vi-basic-map "[" 'vip-brac-function)
(define-key vip-vi-basic-map "]" 'vip-ket-function)
(define-key vip-vi-basic-map "_" 'vip-alternate-ESC)
(define-key vip-vi-basic-map "^" 'vip-bol-and-skip-white)
(define-key vip-vi-basic-map "`" 'vip-goto-mark)

(define-key vip-vi-basic-map "a" 'vip-append)
(define-key vip-vi-basic-map "b" 'vip-backward-word)
(define-key vip-vi-basic-map "c" 'vip-command-argument)
(define-key vip-vi-basic-map "d" 'vip-command-argument)
(define-key vip-vi-basic-map "e" 'vip-end-of-word)
(define-key vip-vi-basic-map "f" 'vip-find-char-forward)
(define-key vip-vi-basic-map "g" 'vip-nil)
(define-key vip-vi-basic-map "h" 'vip-backward-char)
(define-key vip-vi-basic-map "i" 'vip-insert)
(define-key vip-vi-basic-map "j" 'vip-next-line)
(define-key vip-vi-basic-map "k" 'vip-previous-line)
(define-key vip-vi-basic-map "l" 'vip-forward-char)
(define-key vip-vi-basic-map "m" 'vip-mark-point)
(define-key vip-vi-basic-map "n" 'vip-search-next)
(define-key vip-vi-basic-map "o" 'vip-open-line)
(define-key vip-vi-basic-map "p" 'vip-put-back)
(define-key vip-vi-basic-map "q" 'vip-nil)
(define-key vip-vi-basic-map "r" 'vip-replace-char)
(define-key vip-vi-basic-map "s" 'vip-substitute)
(define-key vip-vi-basic-map "t" 'vip-goto-char-forward)
(define-key vip-vi-basic-map "u" 'vip-undo)
(define-key vip-vi-basic-map "v" 'find-file)
(define-key vip-vi-basic-map "\C-v" 'vip-find-file-other-frame)
(define-key vip-vi-basic-map "w" 'vip-forward-word)
(define-key vip-vi-basic-map "x" 'vip-delete-char)
(define-key vip-vi-basic-map "y" 'vip-command-argument)
(define-key vip-vi-basic-map "zH" 'vip-line-to-top)
(define-key vip-vi-basic-map "zM" 'vip-line-to-middle)
(define-key vip-vi-basic-map "zL" 'vip-line-to-bottom)
(define-key vip-vi-basic-map "z\C-m" 'vip-line-to-top)
(define-key vip-vi-basic-map "z." 'vip-line-to-middle)
(define-key vip-vi-basic-map "z-" 'vip-line-to-bottom)

(define-key vip-vi-basic-map "{" 'vip-backward-paragraph)
(define-key vip-vi-basic-map "|" 'vip-goto-col)
(define-key vip-vi-basic-map "}" 'vip-forward-paragraph)
(define-key vip-vi-basic-map "~" 'vip-toggle-case)
(define-key vip-vi-basic-map "\C-?" 'vip-backward-char)
  
;;; Escape from Emacs to Vi for one command
(global-set-key "\M-\C-z" 'vip-escape-to-vi)  ;; in emacs-state

;;; This is vip-vi-diehard-map. Used when vip-vi-diehard-minor-mode is on.

(define-key vip-vi-diehard-map "\C-a" 'vip-nil)
(define-key vip-vi-diehard-map "\C-c" 'vip-nil)
(define-key vip-vi-diehard-map "\C-g" 'vip-info-on-file)
(define-key vip-vi-diehard-map "\C-i" 'vip-nil)
(define-key vip-vi-diehard-map "\C-k" 'vip-nil)
(define-key vip-vi-diehard-map "\C-l" 'redraw-display)
(define-key vip-vi-diehard-map "\C-n" 'vip-next-line)
(define-key vip-vi-diehard-map "\C-o" 'vip-nil)
(define-key vip-vi-diehard-map "\C-p" 'vip-previous-line)
(define-key vip-vi-diehard-map "\C-q" 'vip-nil)
(define-key vip-vi-diehard-map "\C-r" 'redraw-display)
(define-key vip-vi-diehard-map "\C-s" 'vip-nil)
(define-key vip-vi-diehard-map "\C-t" 'vip-nil)
(define-key vip-vi-diehard-map "\C-v" 'vip-nil)
(define-key vip-vi-diehard-map "\C-w" 'vip-nil)
(define-key vip-vi-diehard-map "@" 'vip-nil)
(define-key vip-vi-diehard-map "*" 'vip-nil)
(define-key vip-vi-diehard-map "#" 'vip-nil)
(define-key vip-vi-diehard-map "\C-_" 'vip-nil)
(define-key vip-vi-diehard-map "\C-]" 'vip-nil);; This is actually tags.


;;; Minibuffer keymap
  

(defvar vip-minibuffer-map (make-sparse-keymap)
  "Keymap used to modify keys when Minibuffer is in Insert state.")
  
(define-key vip-minibuffer-map "\C-m" 'vip-exit-minibuffer)
(define-key vip-minibuffer-map "\C-j" 'vip-exit-minibuffer)

;; Map used to read Ex-style commands.
(defvar vip-ex-cmd-map (make-sparse-keymap))
(define-key vip-ex-cmd-map " "  'ex-cmd-read-exit)
(define-key vip-ex-cmd-map "\t" 'ex-cmd-complete)

;; Keymap for reading file names in Ex-style commands.
(defvar ex-read-filename-map (make-sparse-keymap))
(define-key ex-read-filename-map " " 'vip-complete-filename-or-exit)


	  

;;; Code

(defun vip-add-local-keys (state alist)
  "Override some vi-state or insert-state bindings in the current buffer.
The effect is seen in the current buffer only.
Useful for customizing  mailer buffers, gnus, etc.
STATE is 'vi-state, 'insert-state, or 'emacs-state
ALIST is of the form ((key . func) (key . func) ...)
Normally, this would be called from a hook to a major mode or
on a per buffer basis.
Usage:
      (vip-add-local-keys state '((key-str . func) (key-str . func)...))   "
      
  (let (map)
    (cond ((eq state 'vi-state)
	   (if vip-need-new-vi-local-map
	       (setq vip-vi-local-user-map (make-sparse-keymap)))
	   (setq vip-need-new-vi-local-map nil
		 map vip-vi-local-user-map))
	  ((eq state 'insert-state)
	   (if vip-need-new-insert-local-map
	       (setq vip-insert-local-user-map (make-sparse-keymap)))
	   (setq vip-need-new-insert-local-map nil
		 map vip-insert-local-user-map))
	  ((eq state 'emacs-state)
	   (if vip-need-new-emacs-local-map
	       (setq vip-emacs-local-user-map (make-sparse-keymap)))
	   (setq vip-need-new-emacs-local-map nil
		 map vip-emacs-local-user-map))
	  (t 
	   (error
	    "Invalid state in vip-add-local-keys: %S. Valid states: vi-state, insert-state or emacs-state" state)))

    (vip-modify-keymap map alist)
    (vip-normalize-minor-mode-map-alist)
    (vip-set-mode-vars-for vip-current-state)))
    

(defun vip-modify-major-mode (mode state keymap)
  "Modify key bindings in a major-mode in a Viper state using a keymap.

If the default for a major mode is emacs-state, then modifications to this
major mode may not take effect until the buffer switches state to Vi,
Insert or Emacs. If this happens, add vip-change-state-to-emacs to this
major mode's hook. If no such hook exists, you may have to put an advice on
the function that invokes the major mode. See vip-set-hooks for hints.

The above needs not to be done for major modes that come up in Vi or Insert
state by default.

Arguments: (major-mode vip-state keymap)"
  (let ((alist
	 (cond ((eq state 'vi-state) 'vip-vi-state-modifier-alist)
	       ((eq state 'insert-state) 'vip-insert-state-modifier-alist)
	       ((eq state 'emacs-state) 'vip-emacs-state-modifier-alist)))
	elt)
    (if (setq elt (assoc mode (eval alist)))
	(set alist (delq elt (eval alist))))
    (set alist (cons (cons mode keymap) (eval alist)))
    
    ;; Normalization usually doesn't help here, since one needs to
    ;; normalize in the actual buffer where changes to the keymap are
    ;; to take place. However, it doesn't hurt, and it helps whenever this
    ;; function is actually called from within the right buffer.
    (vip-normalize-minor-mode-map-alist)
    
    (vip-set-mode-vars-for vip-current-state)))

    
(defun vip-debug-keymaps ()
  "Displays variables that control Viper's keymaps."
  (interactive)
  (with-output-to-temp-buffer " *vip-debug*"
    (princ (format "Buffer name:  %s\n\n" (buffer-name)))
    (princ "Variables:  \n")
    (princ (format "major-mode:  %S\n" major-mode))
    (princ (format "vip-current-state:  %S\n" vip-current-state))
    (princ (format "vip-mode-string:  %S\n\n" vip-mode-string))
    (princ (format "vip-vi-intercept-minor-mode:  %S\n"
		   vip-vi-intercept-minor-mode))
    (princ (format "vip-insert-intercept-minor-mode:  %S\n"
		   vip-insert-intercept-minor-mode))
    (princ (format "vip-emacs-intercept-minor-mode:  %S\n"
		   vip-emacs-intercept-minor-mode))
    (princ (format "vip-vi-minibuffer-minor-mode:  %S\n"
		   vip-vi-minibuffer-minor-mode))
    (princ (format "vip-insert-minibuffer-minor-mode:  %S\n\n"
		   vip-insert-minibuffer-minor-mode))
    (princ (format "vip-vi-local-user-minor-mode:  %S\n"
		   vip-vi-local-user-minor-mode))
    (princ (format "vip-vi-global-user-minor-mode:  %S\n"
		   vip-vi-global-user-minor-mode))
    (princ (format "vip-vi-kbd-minor-mode:  %S\n" vip-vi-kbd-minor-mode))
    (princ (format "vip-vi-state-modifier-minor-mode:  %S\n"
		   vip-vi-state-modifier-minor-mode))
    (princ (format "vip-vi-diehard-minor-mode:  %S\n"
		   vip-vi-diehard-minor-mode))
    (princ (format "vip-vi-basic-minor-mode:  %S\n" vip-vi-basic-minor-mode))
    (princ (format "vip-replace-minor-mode:  %S\n" vip-replace-minor-mode))
    (princ (format "vip-insert-local-user-minor-mode:  %S\n"
		   vip-insert-local-user-minor-mode))
    (princ (format "vip-insert-global-user-minor-mode:  %S\n"
		   vip-insert-global-user-minor-mode))
    (princ (format "vip-insert-kbd-minor-mode:  %S\n"
		   vip-insert-kbd-minor-mode)) 
    (princ (format "vip-insert-state-modifier-minor-mode:  %S\n"
		   vip-insert-state-modifier-minor-mode))
    (princ (format "vip-insert-diehard-minor-mode:  %S\n"
		   vip-insert-diehard-minor-mode))
    (princ (format "vip-insert-basic-minor-mode:  %S\n"
		   vip-insert-basic-minor-mode))
    (princ (format "vip-emacs-local-user-minor-mode:  %S\n"
		   vip-emacs-local-user-minor-mode))
    (princ (format "vip-emacs-kbd-minor-mode:  %S\n"
		   vip-emacs-kbd-minor-mode))
    (princ (format "vip-emacs-global-user-minor-mode:  %S\n"
		   vip-emacs-global-user-minor-mode))
    (princ (format "vip-emacs-state-modifier-minor-mode:  %S\n"
		   vip-emacs-state-modifier-minor-mode))
    
    (princ (format "\nvip-expert-level  %S\n" vip-expert-level))
    (princ (format "vip-no-multiple-ESC  %S\n" vip-no-multiple-ESC))
    (princ (format "vip-always  %S\n" vip-always))
    (princ (format "vip-ex-style-motion  %S\n"
		   vip-ex-style-motion))
    (princ (format "vip-ex-style-editing-in-insert  %S\n"
		   vip-ex-style-editing-in-insert))
    (princ (format "vip-want-emacs-keys-in-vi  %S\n"
		   vip-want-emacs-keys-in-vi)) 
    (princ (format "vip-want-emacs-keys-in-insert  %S\n"
		   vip-want-emacs-keys-in-insert)) 
    (princ (format "vip-want-ctl-h-help  %S\n" vip-want-ctl-h-help))
    
    (princ "\n\n\n")
    (princ (format "Default value for minor-mode-map-alist:  \n%S\n\n"
		   (default-value 'minor-mode-map-alist)))
    (princ (format "Actual value for minor-mode-map-alist:  \n%S\n"
		   minor-mode-map-alist))
    ))
   

;;; Keymap utils
	     
(defun vip-add-keymap (mapsrc mapdst) 
  "Add contents of mapsrc to mapdst. It is assumed that mapsrc is sparse."
  (if vip-xemacs-p
      (map-keymap (function (lambda (key binding)
			      (define-key mapdst key binding)))
		  mapsrc)
    (mapcar 
     (function (lambda (p) 
		 (define-key mapdst (vector (car p)) (cdr p))
		 ))
     (cdr mapsrc))))
  
(defun vip-modify-keymap (map alist)
   "Modifies MAP with bindings specified in the ALIST. The alist has the
form ((key . function) (key . function) ... )."
   (mapcar (function (lambda (p)
		       (define-key map (eval (car p)) (cdr p)))) 
	   alist))


(provide 'viper-keym)

;;;  viper-keym.el ends here
