;;; viper.el --- A full-featured Vi emulator for GNU Emacs 19 and XEmacs 19,
;;		 a VI Plan for Emacs Rescue,
;;		 and a venomous VI PERil.
;;		 Viper Is also a Package for Emacs Rebels.
;;
;;  Keywords: emulations
;;  Author: Michael Kifer <kifer@cs.sunysb.edu>

;; Copyright (C) 1994, 1995, 1996, 1997 Free Software Foundation, Inc.

(defconst viper-version "2.95 of July 9, 1997"
  "The current version of Viper")

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Viper is a full-featured Vi emulator for Emacs 19.  It emulates and
;; improves upon the standard features of Vi and, at the same time, allows
;; full access to all Emacs facilities.  Viper supports multiple undo,
;; file name completion, command, file, and search history and it extends
;; Vi in many other ways. Viper is highly customizable through the various
;; hooks, user variables, and keymaps.  It is implemented as a collection
;; of minor modes and it is designed to provide full access to all Emacs
;; major and minor modes.
;;
;;; History
;;
;; Viper is a new name for a package formerly known as VIP-19,
;; which was a successor of VIP version 3.5 by Masahiko Sato
;; <ms@sail.stanford.edu> and VIP version 4.2 by Aamod Sane
;; <sane@cs.uiuc.edu>. Some ideas from vip 4.4.2 by Aamod Sane
;; were also shamelessly plagiarized.
;;
;; Viper maintains some degree of compatibility with these older
;; packages. See the documentation for customization.
;;
;; The main difference between Viper and these older packages are:
;;
;; 1. Viper emulates Vi at several levels, from almost complete conformity
;;    to a rather loose Vi-compliance.
;;
;; 2. Viper provides full access to all major and minor modes of Emacs
;;    without the need to type extra keys.
;;    The older versions of VIP (and other Vi emulators) do not work with
;;    some major and minor modes.
;;
;; 3. Viper supports vi-style undo.
;;
;; 4. Viper fully emulates (and improves upon) vi's replacement mode.
;;
;; 5. Viper has a better interface to ex, including command, variable, and
;;    file name completion.
;;
;; 6. Viper uses native Emacs history and completion features; it doesn't
;;    rely on other packages (such as gmhist.el and completer.el) to provide
;;    these features.
;;
;; 7. Viper supports Vi-style editing in the minibuffer, by allowing the
;;    user to switch from Insert state to Vi state to Replace state, etc.
;;
;; 8. Viper keeps history of recently inserted pieces of text and recently
;;    executed Vi-style destructive commands, such as `i', `d', etc.
;;    These pieces of text can be inserted in later insertion commands;
;;    the previous destructive commands can be re-executed.
;;
;; 9. Viper has Vi-style keyboard macros, which enhances the similar
;;    facility in the original Vi.
;;    First, one can execute any Emacs command while defining a
;;    macro, not just the Vi commands. Second, macros are defined in a
;;    WYSYWYG mode, using an interface to Emacs' WYSIWYG style of defining
;;    macros. Third, in Viper, one can define macros that are specific to
;;    a given buffer, a given major mode, or macros defined for all buffers.
;;    The same macro name can have several different definitions:
;;    one global, several definitions for various major modes, and
;;    definitions for specific buffers.
;;    Buffer-specific definitions override mode-specific
;;    definitions, which, in turn, override global definitions.
;;
;;
;;; Installation:
;;  -------------
;;
;;  (require 'viper)
;;

;;; Acknowledgements:
;;  -----------------
;;  Bug reports and ideas contributed by many users have helped
;;  improve Viper and the various versions of VIP.
;;  See the on-line manual for a complete list of contributors. 
;;
;;
;;; Notes:
;;
;; 1. Major modes.
;; In most cases, Viper handles major modes correctly, i.e., they come up
;; in the right state (either  vi-state or emacs-state). For instance, text
;; files come up in vi-state, while, say, Dired appears in emacs-state by
;; default. 
;; However, some modes do not appear in the right mode in the beginning,
;; usually because they neglect to follow Emacs conventions (e.g., they don't
;; use kill-all-local-variables when they start). Some major modes
;; may fail to come up in emacs-state if they call hooks, such as
;; text-hook, for no good reason. 
;; 
;; As an immediate solution, you can hit C-z to bring about the right mode.
;; An interim solution is to add an appropriate hook to the mode like this:
;; 
;;     (add-hook 'your-favorite-mode 'viper-mode)
;; or    
;;     (add-hook 'your-favorite-mode 'vip-change-state-to-emacs)
;; 
;; whichever applies. The right thing to do, however, is to complain to the
;; author of the respective package. (Sometimes they also neglect to equip
;; their  modes with hooks, which is one more reason for complaining.)
;; 
;; 2. Keymap handling
;;    Because Emacs 19 has an elegant mechanism for turning minor mode keymaps
;;    on and off, implementation of Viper has been greatly simplified. Viper
;;    has several minor modes.
;;
;; Viper's  Vi state consists of seven minor modes:
;;
;;  vip-vi-intercept-minor-mode
;;  vip-vi-local-user-minor-mode
;;  vip-vi-global-user-minor-mode
;;  vip-vi-kbd-minor-mode
;;  vip-vi-state-modifier-minor-mode
;;  vip-vi-diehard-minor-mode
;;  vip-vi-basic-minor-mode
;;
;;  Bindings done to the keymap of the first mode overshadow those done to
;;  the second, which, in turn, overshadows those done to the third, etc.
;;
;;  The last vip-vi-basic-minor-mode contains most of the usual Vi bindings
;;  in its edit mode. This mode provides access to all Emacs facilities.
;;  Novice users, however, may want to set their viper-expert-level to 1
;;  in their .vip file. This will enable vip-vi-diehard-minor-mode. This
;;  minor mode's bindings make Viper simulate the usual Vi very closely.
;;  For instance,  C-c will not have its standard Emacs binding
;;  and so many of the goodies of Emacs are not available.
;;
;;  A skilled user should set viper-expert-level to at least 3. This will
;;  enable `C-c' and many Emacs facilities will become available.
;;  In this case, vip-vi-diehard-minor-mode is inactive.
;;
;;  Viper gurus should have at least
;;      (setq viper-expert-level 4)
;;  in their ~/.viper files. This will unsuppress all Emacs keys that are not
;;  essential for VI-style editing.
;;  Pick-and-choose users may want to put
;;      (setq viper-expert-level 5)
;;  in ~/.viper. Viper will then leave it up to the user to set the variables
;;  vip-want-*  See viper-set-expert-level for details.
;;
;;  The very first minor mode, vip-vi-intercept-minor-mode, is of no
;;  concern for the user. It is needed to bind Viper's vital keys, such as
;;  ESC and C-z.
;;
;;  The second mode,  vip-vi-local-user-minor-mode, usually has an
;;  empty keymap. However, the user can set bindings in this keymap, which
;;  will overshadow the corresponding bindings in the other two minor
;;  modes. This is useful, for example, for setting up ZZ in gnus,
;;  rmail, mh-e, etc., to send  message instead of saving it in a file.
;;  Likewise, in Dired mode, you may want to bind ZN and ZP to commands
;;  that would visit the next or the previous file in the Dired buffer.
;;  Setting local keys is tricky, so don't do it directly. Instead, use
;;  vip-add-local-keys function (see its doc).
;;
;;  The third minor mode, vip-vi-global-user-minor-mode, is also intended
;;  for the users but, unlike vip-vi-local-user-minor-mode, its key
;;  bindings are seen in all Viper buffers. This mode keys can be done
;;  with define-key command.
;;
;;  The fourth minor mode, vip-vi-kbd-minor-mode, is used by keyboard
;;  macros. Users are NOT supposed to modify this keymap directly.
;;
;;  The fifth mode, vip-vi-state-modifier-minor-mode, can be used to set
;;  key bindings that are visible in some major modes but not in others.
;;
;;  Users are allowed to modify keymaps that belong to
;;  vip-vi-local-user-minor-mode, vip-vi-global-user-minor-mode,
;;  and vip-vi-state-modifier-minor-mode only.
;;
;;  Viper's Insert state also has seven minor modes:
;;
;;      vip-insert-intercept-minor-mode
;;  	vip-insert-local-user-minor-mode
;;  	vip-insert-global-user-minor-mode
;;  	vip-insert-kbd-minor-mode
;;      vip-insert-state-modifier-minor-mode
;;  	vip-insert-diehard-minor-mode
;;  	vip-insert-basic-minor-mode
;;
;;  As with VI's editing modes, the first mode, vip-insert-intercept-minor-mode
;;  is used to bind vital keys that are not to be changed by the user.
;;
;;  The next mode, vip-insert-local-user-minor-mode, is used to customize
;;  bindings in the insert state of Viper. The third mode,
;;  vip-insert-global-user-minor-mode is like
;;  vip-insert-local-user-minor-mode, except that its bindings are seen in 
;;  all Viper buffers. As with vip-vi-local-user-minor-mode, its bindings
;;  should be done via the function vip-add-local-keys. Bindings for
;;  vip-insert-global-user-minor-mode can be set with the define-key command.
;;
;;  The next minor mode, vip-insert-kbd-minor-mode,
;;  is used for keyboard VI-style macros defined with :map!. 
;;
;;  The fifth minor mode, vip-insert-state-modifier-minor-mode, is like
;;  vip-vi-state-modifier-minor-mode, except that it is used in the Insert
;;  state; it can be used to modify keys in a mode-specific fashion. 
;;
;;  The minor mode vip-insert-diehard-minor-mode is in effect when
;;  the user wants a high degree of Vi compatibility (a bad idea, really!).
;;  The last minor mode, vip-insert-basic-minor-mode, is always in effect
;;  when Viper is in insert state. It binds a small number of keys needed for
;;  Viper's operation. 
;;
;;  Finally, Viper provides minor modes for overriding bindings set by Emacs
;;  modes when Viper is in Emacs state:
;;
;; 	vip-emacs-local-user-minor-mode
;;  	vip-emacs-global-user-minor-mode
;;      vip-emacs-kbd-minor-mode
;;      vip-emacs-state-modifier-minor-mode
;;
;;  These minor modes are in effect when Viper is in Emacs state. The keymap
;;  associated with vip-emacs-global-user-minor-mode,
;;  vip-emacs-global-user-map, overrides the global and local keymaps as
;;  well as the minor mode keymaps set by other modes. The keymap of
;;  vip-emacs-local-user-minor-mode, vip-emacs-local-user-map, overrides
;;  everything, but it is used on a per buffer basis.
;;  The keymap associated with vip-emacs-state-modifier-minor-mode
;;  overrides keys on a per-major-mode basis. The mode
;;  vip-emacs-kbd-minor-mode is used to define Vi-style macros in Emacs
;;  state.
;;
;;  3. There is also one minor mode that is used when Viper is in its
;;     replace-state (used for commands like cw, C, etc.). This mode is
;;     called
;;
;;       vip-replace-minor-mode
;;
;;     and its keymap is vip-replace-map. Replace minor mode is always
;;     used in conjunction with the minor modes for insert-state, and its
;;     keymap overshadows the keymaps for insert minor modes.
;;
;;  4. Defining buffer-local bindings in Vi and Insert modes. 
;;  As mentioned before, sometimes, it is convenient to have
;;  buffer-specific of mode-specific key bindings in Vi and insert modes.
;;  Viper provides a special function, vip-add-local-keys, to do precisely
;;  this. For instance, is you need to add couple of mode-specific bindings
;;  to Insert mode, you can put 
;;
;;       (vip-add-local-keys 'insert-state '((key1 . func1) (key2 .func2))) 
;;
;;  somewhere in a hook of this major mode. If you put something like this
;;  in your own elisp function, this will define bindings specific to the
;;  buffer that was current at the time of the call to vip-add-local-keys.
;;  The only thing to make sure here is that the major mode of this buffer
;;  is written according to Emacs conventions, which includes a call to
;;  (kill-all-local-variables). See vip-add-local-keys for more details.
;;
;;
;;  TO DO (volunteers?):
;;
;; 1. Some of the code that is inherited from VIP-3.5 is rather
;;    convoluted. Instead of vip-command-argument, keymaps should bind the
;;    actual commands. E.g., "dw" should be bound to a generic command
;;    vip-delete that will delete things based on the value of
;;    last-command-char. This would greatly simplify the logic and the code.
;;
;; 2. Somebody should venture to write a customization package a la
;;    options.el that would allow the user to change values of variables
;;    that meet certain specs (e.g., match a regexp) and whose doc string
;;    starts with a '*'. Then, the user should be offered to save
;;    variables that were changed. This will make user's customization job
;;    much easier.
;;

;; Code

(require 'advice)
(require 'cl)
(require 'ring)

;; compiler pacifier
(defvar mark-even-if-inactive)
(defvar viper-expert-level)
(defvar vip-expert-level)

;; loading happens only in non-interactive compilation
;; in order to spare non-viperized emacs from being viperized
(if noninteractive
    (eval-when-compile
      (let ((load-path (cons (expand-file-name ".") load-path)))
	(or (featurep 'viper-init)
	    (load "viper-init.el" nil nil 'nosuffix))
	(or (featurep 'viper-cmd)
	    (load "viper-cmd.el" nil nil 'nosuffix))
	)))
;; end pacifier

(require 'viper-init)

;; better be defined before Viper custom group.
(defvar vip-custom-file-name (vip-convert-standard-file-name "~/.viper")
  "Viper customisation file.
If set by the user, this must be done _before_ Viper is loaded in `~/.emacs'.")

(defgroup viper nil
  "Vi emulation within Emacs.
NOTE: Viper customization should be saved in `vip-custom-file-name', which
defaults to `~/.viper'."
  :prefix "vip-"
  :group 'emulations)

(require 'viper-cmd)

(defvar vip-always t
  "See `viper-always'. This variable is for compatibility with older Vipers.")
(defcustom viper-always vip-always
  "Non-nil means, arrange for vi-state to be a default when appropriate.
This is different from `viper-mode' variable in that `viper-mode' determines
whether to use Viper in the first place, while `viper-always', if nil, lets
user decide when to invoke Viper in a major mode."
  :type 'boolean
  :tag "Always Invoke Viper"
  :group 'viper)

;; Non-viper variables that need to be saved in case the user decides to
;; de-viperize emacs.
(defvar viper-saved-non-viper-variables nil)
;; Contains user settings for vars affected by viper-set-expert-level function.
;; Not a user option.
(defvar viper-saved-user-settings nil)
	       
(defcustom viper-mode (cond (noninteractive nil)
			    (t 'ask))
  "To Viperize or not to Viperize.
If t, viperize emacs. If nil -- don't. If `ask', ask the user.
This variable is used primatily when Viper is being loaded.

Must be set in `~/.emacs' before Viper is loaded.
DO NOT set this variable interactively."
  :type '(choice (const nil) (const t) (const ask))
  :tag "Set Viper Mode on Loading"
  :group 'viper)


;; The following are provided for compatibility with older VIP's

;;;###autoload
(defalias 'vip-mode 'viper-mode)

(defalias 'vip-change-mode-to-vi 'vip-change-state-to-vi)
(defalias 'vip-change-mode-to-insert 'vip-change-state-to-insert)
(defalias 'vip-change-mode-to-emacs 'vip-change-state-to-emacs)




;;;###autoload
(defun viper-mode ()
  "Turn on Viper emulation of Vi."
  (interactive)
  (if (not noninteractive)
      (progn
	;; if the user requested viper-mode explicitly
	(if viper-mode
	    ()
	  (setq viper-mode 1)
	  (load-library "viper"))

	(if vip-first-time ; This check is important. Without it, startup and 
	    (progn	   ; expert-level msgs mix up when viper-mode recurses
	      (setq vip-first-time nil)
	      (if (not vip-inhibit-startup-message)
		  (save-window-excursion
		    (setq vip-inhibit-startup-message t)
		    (delete-other-windows)
		    (switch-to-buffer "Viper Startup Message")
		    (erase-buffer)
		    (insert
		     (substitute-command-keys
		      "Viper Is a Package for Emacs Rebels,
a VI Plan for Emacs Rescue, and a venomous VI PERil.

Incidentally, Viper emulates Vi under GNU Emacs 20 and XEmacs 20.
It supports all of what is good in Vi and Ex, while extending
and improving upon much of it.

   1. Viper supports Vi at several levels. Level 1 is the closest to Vi,
      level 5 provides the most flexibility to depart from many Vi conventions.
      
      You will be asked to specify your user level in a following screen.
   
      If you select user level 1 then the keys ^X, ^C, ^Z, and ^G will behave
      as in VI, to smooth transition to Viper for the beginners. However, to
      use Emacs productively, you are advised to reach user level 3 or higher. 
      
      At user level 2 or higher, ^X and ^C have Emacs, not Vi, bindings;
      ^Z toggles Vi/Emacs states; ^G is Emacs' keyboard-quit (like ^C in Vi).
   
   2. Vi exit functions (e.g., :wq, ZZ) work on INDIVIDUAL files -- they
      do not cause Emacs to quit, except at user level 1 (for a novice).
   3. ^X^C EXITS EMACS.
   4. Viper supports multiple undo: `u' will undo. Typing `.' will repeat
      undo. Another `u' changes direction.
   
   6. Emacs Meta key is `C-\\' (in all modes) or `\\ ESC' (in Vi command mode).
      On a window system, the best way is to use the Meta-key on your keyboard.
   7. Try \\[keyboard-quit] and \\[abort-recursive-edit] repeatedly,if
      something funny happens. This would abort the current editing command. 
      
For more information on Viper:

   a. Type `:help' in Vi command mode
   b. Print Viper manual, found in ./etc/viper.dvi
   c. Print the Quick Reference, found in ./etc/viperCard.dvi

To submit a bug report or to contact the author, type :submitReport in Vi
command mode. To shoo Viper away and return to pure Emacs (horror!), type:

   M-x viper-go-away
    
This startup message appears whenever you load Viper, unless you type `y' now."
		      ))
		    (goto-char (point-min))
		    (if (y-or-n-p "Inhibit Viper startup message? ")
			(vip-save-setting
			 'vip-inhibit-startup-message
			 "Viper startup message inhibited"
			 vip-custom-file-name t))
		    ;;(kill-buffer (current-buffer))
		    (message
		     "The last message is in buffer `Viper Startup Message'")
		    (sit-for 4)
		    ))
	      (viper-set-expert-level 'dont-change-unless)))
	(vip-change-state-to-vi))))
   

;; This hook designed to enable Vi-style editing in comint-based modes."
(defun vip-comint-mode-hook ()
  (setq require-final-newline nil
	vip-ex-style-editing-in-insert nil
	vip-ex-style-motion nil)
  (vip-change-state-to-insert))


;; remove viper hooks from SYMBOL
(defun viper-remove-hooks (symbol)
  (cond ((not (boundp symbol)) nil)
	((not (listp (eval symbol))) nil)
	((string-match "-hook" (symbol-name symbol))
	 (remove-hook symbol 'viper-mode)
	 (remove-hook symbol 'vip-change-state-to-emacs)
	 (remove-hook symbol 'vip-change-state-to-insert)
	 (remove-hook symbol 'vip-change-state-to-vi)
	 )))

;; Remove local value in all existing buffers
;; This doesn't delocalize vars (which would have been desirable)
(defun viper-delocalize-var (symbol)
  (mapcar (function (lambda (buf)
		      (save-excursion
			(set-buffer buf)
			(kill-local-variable symbol))))
	  (buffer-list)))


(defun viper-go-away ()
  "De-Viperize Emacs.
This function tries to do as good a job as possible. However, it may undo some
user customization, unrelated to Viper. For instance, if the user advised
`read-file-name', `describe-key', and some others, then this advice will be
undone.
It also doesn't undo some Viper settings. For instance, `minor-mode-map-alist'
remains buffer-local."
  (interactive)

  ;; restore non-viper vars
  (setq-default
   default-major-mode
   (viper-standard-value 'default-major-mode viper-saved-non-viper-variables) 
   next-line-add-newlines
   (viper-standard-value 
    'next-line-add-newlines viper-saved-non-viper-variables) 
   require-final-newline
   (viper-standard-value 
    'require-final-newline viper-saved-non-viper-variables) 
   scroll-step
   (viper-standard-value 'scroll-step viper-saved-non-viper-variables) 
   mode-line-buffer-identification
   (viper-standard-value
    'mode-line-buffer-identification viper-saved-non-viper-variables)
   global-mode-string
   (viper-standard-value 'global-mode-string viper-saved-non-viper-variables))

  (if vip-emacs-p
      (setq-default
       mark-even-if-inactive
       (viper-standard-value
	'mark-even-if-inactive viper-saved-non-viper-variables)))

  ;; Ideally, we would like to be able to de-localize local variables 
  (viper-delocalize-var 'minor-mode-map-alist)
  (viper-delocalize-var 'require-final-newline)

  
  ;; deactivate all advices done by Viper.
  (ad-deactivate-regexp "vip-")

  (setq viper-mode nil)

  (viper-delocalize-var 'vip-vi-minibuffer-minor-mode)
  (viper-delocalize-var 'vip-insert-minibuffer-minor-mode)
  (viper-delocalize-var 'vip-vi-intercept-minor-mode)
  (viper-delocalize-var 'vip-insert-intercept-minor-mode)
  
  (viper-delocalize-var 'vip-vi-local-user-minor-mode)
  (viper-delocalize-var 'vip-vi-kbd-minor-mode)
  (viper-delocalize-var 'vip-vi-global-user-minor-mode)
  (viper-delocalize-var 'vip-vi-state-modifier-minor-mode)
  (viper-delocalize-var 'vip-vi-diehard-minor-mode)
  (viper-delocalize-var 'vip-vi-basic-minor-mode)
	
  (viper-delocalize-var 'vip-replace-minor-mode)
	
  (viper-delocalize-var 'vip-insert-local-user-minor-mode)
  (viper-delocalize-var 'vip-insert-kbd-minor-mode)
  (viper-delocalize-var 'vip-insert-global-user-minor-mode)
  (viper-delocalize-var 'vip-insert-state-modifier-minor-mode)
  (viper-delocalize-var 'vip-insert-diehard-minor-mode)
  (viper-delocalize-var 'vip-insert-basic-minor-mode)
  
  (viper-delocalize-var 'vip-emacs-intercept-minor-mode)
  (viper-delocalize-var 'vip-emacs-local-user-minor-mode)
  (viper-delocalize-var 'vip-emacs-kbd-minor-mode)
  (viper-delocalize-var 'vip-emacs-global-user-minor-mode)
  (viper-delocalize-var 'vip-emacs-state-modifier-minor-mode)

  (setq-default vip-vi-minibuffer-minor-mode	   nil
		vip-insert-minibuffer-minor-mode   nil
		vip-vi-intercept-minor-mode	   nil
		vip-insert-intercept-minor-mode	   nil
		
		vip-vi-local-user-minor-mode       nil
		vip-vi-kbd-minor-mode        	   nil
		vip-vi-global-user-minor-mode      nil
		vip-vi-state-modifier-minor-mode   nil
		vip-vi-diehard-minor-mode     	   nil
		vip-vi-basic-minor-mode       	   nil
		
		vip-replace-minor-mode 	      	   nil
		
		vip-insert-local-user-minor-mode     nil
		vip-insert-kbd-minor-mode     	     nil
		vip-insert-global-user-minor-mode    nil
		vip-insert-state-modifier-minor-mode nil
		vip-insert-diehard-minor-mode 	     nil
		vip-insert-basic-minor-mode   	     nil

		vip-emacs-intercept-minor-mode       nil
		vip-emacs-local-user-minor-mode      nil
		vip-emacs-kbd-minor-mode             nil
		vip-emacs-global-user-minor-mode     nil
		vip-emacs-state-modifier-minor-mode  nil
		)

  ;; remove all hooks set by viper
  (mapatoms 'viper-remove-hooks)
  (remove-hook 'comint-mode-hook 'vip-comint-mode-hook)
  (remove-hook 'minibuffer-setup-hook 'vip-minibuffer-setup-sentinel)
  )




;; This sets major mode hooks to make them come up in vi-state.
(defun vip-set-hooks ()
  
  ;; It is of course a misnomer to call viper-mode a `major mode'.
  ;; However, this has the effect that if the user didn't specify the
  ;; default mode, new buffers that fall back on the default will come up
  ;; in Fundamental Mode and Vi state.
  (setq default-major-mode 'viper-mode)
  
  ;; The following major modes should come up in vi-state
  (defadvice fundamental-mode (after vip-fundamental-mode-ad activate)
    "Run `vip-change-state-to-vi' on entry."
    (vip-change-state-to-vi))

  (defvar makefile-mode-hook)
  (add-hook 'makefile-mode-hook 'viper-mode)

  (defvar help-mode-hook)
  (add-hook 'help-mode-hook 'viper-mode)
  (vip-modify-major-mode 'help-mode 'vi-state vip-help-modifier-map)

  (defvar awk-mode-hook)
  (add-hook 'awk-mode-hook 'viper-mode)
  
  (defvar html-mode-hook)
  (add-hook 'html-mode-hook 'viper-mode)
  (defvar html-helper-mode-hook)
  (add-hook 'html-helper-mode-hook 'viper-mode)

  (defvar java-mode-hook)
  (add-hook 'java-mode-hook 'viper-mode)
  
  (defvar javascript-mode-hook)
  (add-hook 'javascript-mode-hook 'viper-mode)
  
  (defvar emacs-lisp-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'viper-mode)
  (defvar lisp-mode-hook)
  (add-hook 'lisp-mode-hook 'viper-mode)
  (defvar lisp-interaction-mode-hook)
  (add-hook 'lisp-interaction-mode-hook 'viper-mode)
  
  (defvar bibtex-mode-hook)
  (add-hook 'bibtex-mode-hook 'viper-mode) 	  
      
  (defvar cc-mode-hook)
  (add-hook 'cc-mode-hook 'viper-mode)
      
  (defvar c-mode-hook)
  (add-hook 'c-mode-hook 'viper-mode)
      
  (defvar c++-mode-hook)
  (add-hook 'c++-mode-hook 'viper-mode)

  (defvar fortran-mode-hook)
  (add-hook 'fortran-mode-hook 'viper-mode)
  (defvar f90-mode-hook)
  (add-hook 'f90-mode-hook 'viper-mode)

  (defvar basic-mode-hook)
  (add-hook 'basic-mode-hook 'viper-mode)
  (defvar bat-mode-hook)
  (add-hook 'bat-mode-hook 'viper-mode)
  
  (defvar asm-mode-hook)
  (add-hook 'asm-mode-hook 'viper-mode)

  (defvar prolog-mode-hook)
  (add-hook 'prolog-mode-hook 'viper-mode)
      
  (defvar text-mode-hook)
  (add-hook 'text-mode-hook 'viper-mode)
      
  (add-hook 'completion-list-mode-hook 'viper-mode)  
  (add-hook 'compilation-mode-hook     'viper-mode)  

  (defvar perl-mode-hook)
  (add-hook 'perl-mode-hook 'viper-mode)  

  (defvar tcl-mode-hook)
  (add-hook 'tcl-mode-hook 'viper-mode)  
  
  (defvar python-mode-hook)
  (add-hook 'python-mode-hook 'viper-mode)  
  
  (defvar emerge-startup-hook)
  (add-hook 'emerge-startup-hook 'vip-change-state-to-emacs)

  ;; Tell vc-diff to put *vc* in Vi mode
  (if (featurep 'vc)
      (defadvice vc-diff (after vip-vc-ad activate)
	"Force Vi state in VC diff buffer."
	(vip-change-state-to-vi))
    (vip-eval-after-load
     "vc"
     '(defadvice vc-diff (after vip-vc-ad activate)
	"Force Vi state in VC diff buffer."
	(vip-change-state-to-vi))))
    
  (vip-eval-after-load
   "emerge"
   '(defadvice emerge-quit (after vip-emerge-advice activate)
      "Run `vip-change-state-to-vi' after quitting emerge."
      (vip-change-state-to-vi)))
  ;; In case Emerge was loaded before Viper.
  (defadvice emerge-quit (after vip-emerge-advice activate)
    "Run `vip-change-state-to-vi' after quitting emerge."
    (vip-change-state-to-vi))
  
  ;; passwd.el sets up its own buffer, which turns up in Vi mode,
  ;; thus overriding the local map. We don't need Vi mode here.
  (vip-eval-after-load
   "passwd"
   '(defadvice read-passwd-1 (before vip-passwd-ad activate)
      "Switch to emacs state while reading password."
      (vip-change-state-to-emacs)))
  
  ;; Emacs shell, ange-ftp, and comint-based modes
  (defvar comint-mode-hook)
  (vip-modify-major-mode 
   'comint-mode 'insert-state vip-comint-mode-modifier-map)
  (vip-modify-major-mode 
   'comint-mode 'vi-state vip-comint-mode-modifier-map)
  (vip-modify-major-mode 
   'shell-mode 'insert-state vip-comint-mode-modifier-map)
  (vip-modify-major-mode 
   'shell-mode 'vi-state vip-comint-mode-modifier-map)
  ;; ange-ftp in XEmacs
  (vip-modify-major-mode 
   'ange-ftp-shell-mode 'insert-state vip-comint-mode-modifier-map)
  (vip-modify-major-mode 
   'ange-ftp-shell-mode 'vi-state vip-comint-mode-modifier-map)
  ;; ange-ftp in Emacs
  (vip-modify-major-mode 
   'internal-ange-ftp-mode 'insert-state vip-comint-mode-modifier-map)
  (vip-modify-major-mode 
   'internal-ange-ftp-mode 'vi-state vip-comint-mode-modifier-map)
  ;; set hook
  (add-hook 'comint-mode-hook 'vip-comint-mode-hook)
  
  ;; Shell scripts
  (defvar sh-mode-hook)
  (add-hook 'sh-mode-hook 'viper-mode)
  (defvar ksh-mode-hook)
  (add-hook 'ksh-mode-hook 'viper-mode)
  
  ;; Dired
  (vip-modify-major-mode 'dired-mode 'emacs-state vip-dired-modifier-map)
  (vip-set-emacs-state-searchstyle-macros nil 'dired-mode)
  (add-hook 'dired-mode-hook 'vip-change-state-to-emacs)

  ;; Tar
  (vip-modify-major-mode 'tar-mode 'emacs-state vip-slash-and-colon-map)
  (vip-set-emacs-state-searchstyle-macros nil 'tar-mode)

  ;; MH-E
  (vip-modify-major-mode 'mh-folder-mode 'emacs-state vip-slash-and-colon-map)
  (vip-set-emacs-state-searchstyle-macros nil 'mh-folder-mode)
  ;; changing state to emacs is needed so the preceding will take hold
  (add-hook 'mh-folder-mode-hook 'vip-change-state-to-emacs)
  (add-hook 'mh-show-mode-hook 'viper-mode)

  ;; Gnus
  (vip-modify-major-mode 'gnus-group-mode 'emacs-state vip-slash-and-colon-map)
  (vip-set-emacs-state-searchstyle-macros nil 'gnus-group-mode)
  (vip-modify-major-mode 
   'gnus-summary-mode 'emacs-state vip-slash-and-colon-map)
  (vip-set-emacs-state-searchstyle-macros nil 'gnus-summary-mode)
  ;; changing state to emacs is needed so the preceding will take hold
  (add-hook 'gnus-group-mode-hook 'vip-change-state-to-emacs)
  (add-hook 'gnus-summary-mode-hook 'vip-change-state-to-emacs)
  (add-hook 'gnus-article-mode-hook 'viper-mode)

  ;; Info
  (vip-modify-major-mode 'Info-mode 'emacs-state vip-slash-and-colon-map)
  (vip-set-emacs-state-searchstyle-macros nil 'Info-mode)
  ;; Switching to emacs is needed  so the above will take hold
  (defadvice Info-mode (after vip-Info-ad activate)
    "Switch to emacs mode."
    (vip-change-state-to-emacs))

  ;; Buffer menu
  (vip-modify-major-mode 
   'Buffer-menu-mode 'emacs-state vip-slash-and-colon-map)
  (vip-set-emacs-state-searchstyle-macros nil 'Buffer-menu-mode)
  ;; Switching to emacs is needed  so the above will take hold
  (defadvice Buffer-menu-mode (after vip-Buffer-menu-ad activate)
    "Switch to emacs mode."
    (vip-change-state-to-emacs))

  ;; View mode
  (defvar view-mode-hook)
  (defvar view-hook)
  (add-hook 'view-hook 'vip-change-state-to-emacs)
  (add-hook 'view-mode-hook 'vip-change-state-to-emacs)
  
  ;; For VM users.
  ;; Put summary and other VM buffers in Emacs state.
  (defvar vm-mode-hooks)
  (defvar vm-summary-mode-hooks)
  (add-hook 'vm-mode-hooks   'vip-change-state-to-emacs)
  (add-hook 'vm-summary-mode-hooks   'vip-change-state-to-emacs)
  
  ;; For RMAIL users.
  ;; Put buf in Emacs state after edit.
  (vip-eval-after-load
   "rmailedit"
   '(defadvice rmail-cease-edit (after vip-rmail-advice activate)
      "Switch to emacs state when done editing message."
      (vip-change-state-to-emacs)))
  ;; In case RMAIL was loaded before Viper.
  (defadvice rmail-cease-edit (after vip-rmail-advice activate)
    "Switch to emacs state when done editing message."
    (vip-change-state-to-emacs))
  ) ; vip-set-hooks


;; these are primarily advices and Vi-ish variable settings
(defun vip-non-hook-settings ()

  ;; This var is not local in Emacs, so we make it local.
  ;; It must be local because although the stack of minor modes can be the same
  ;; for all buffers, the associated *keymaps* can be different. In Viper,
  ;; vip-vi-local-user-map, vip-insert-local-user-map, and others can have
  ;; different keymaps for different buffers.
  ;; Also, the keymaps associated with vip-vi/insert-state-modifier-minor-mode
  ;; can be different.
  (make-variable-buffer-local 'minor-mode-map-alist)
  
  ;; Viper changes the default mode-line-buffer-identification
  (setq-default mode-line-buffer-identification '(" %b"))
  
  ;; setup emacs-supported vi-style feel
  (setq next-line-add-newlines nil
	require-final-newline t)
  
  (make-variable-buffer-local 'require-final-newline)
  
  ;; don't bark when mark is inactive
  (if vip-emacs-p
      (setq mark-even-if-inactive t))
  
  (setq scroll-step 1)
  
  ;; Variable displaying the current Viper state in the mode line.
  (vip-deflocalvar vip-mode-string vip-emacs-state-id)
  (or (memq 'vip-mode-string global-mode-string)
      (setq global-mode-string
	    (append '("" vip-mode-string) (cdr global-mode-string))))

  (defadvice read-key-sequence (around vip-read-keyseq-ad activate)
    "Harness to work for Viper. This advice is harmless---don't worry!"
    (let (inhibit-quit event keyseq)
      (setq keyseq ad-do-it)
      (setq event (if vip-xemacs-p
		      (elt keyseq 0) ; XEmacs returns vector of events
		    (elt (listify-key-sequence keyseq) 0)))
      (if (vip-ESC-event-p event)
	  (let (unread-command-events)
	    (vip-set-unread-command-events keyseq)
	    (if (vip-fast-keysequence-p)
		(let ((vip-vi-global-user-minor-mode  nil)
		      (vip-vi-local-user-minor-mode  nil)
		      (vip-replace-minor-mode nil) ; actually unnecessary
		      (vip-insert-global-user-minor-mode  nil)
		      (vip-insert-local-user-minor-mode  nil))
		  (setq keyseq ad-do-it)) 
	      (setq keyseq ad-do-it))))
      keyseq))
  
  (defadvice describe-key (before vip-read-keyseq-ad protect activate)
    "Force to read key via `read-key-sequence'."
    (interactive (list (vip-events-to-keys
			(read-key-sequence "Describe key: ")))))
  
  (defadvice describe-key-briefly (before vip-read-keyseq-ad protect activate)
    "Force to read key via `read-key-sequence'."
    (interactive (list (vip-events-to-keys
			(read-key-sequence "Describe key briefly: ")))))
  
  
  ;; Advice for use in find-file and read-file-name commands.
  (defadvice exit-minibuffer (before vip-exit-minibuffer-advice activate)
    "Run `vip-minibuffer-exit-hook' just before exiting the minibuffer."
    (run-hooks 'vip-minibuffer-exit-hook))
  
  (defadvice find-file (before vip-add-suffix-advice activate)
    "Use `read-file-name' for reading arguments."
    (interactive (cons (read-file-name "Find file: " nil default-directory)
		       ;; if Mule and prefix argument, ask for coding system
		       (cond ((and vip-emacs-p 
				   (boundp 'MULE))   ; Emacs 20 with MULE
			      nil)
			     ((and vip-xemacs-p
				   (featurep 'mule)) ; XEmacs 20 with MULE
			      (list
			       (and current-prefix-arg
				    (read-coding-system 
				     "Coding-system: "))))
			     )
		       )))
  
  (defadvice find-file-other-window (before vip-add-suffix-advice activate)
    "Use `read-file-name' for reading arguments."
    (interactive (cons (read-file-name "Find file in other window: "
				       nil default-directory)
		       ;; if Mule and prefix argument, ask for coding system
		       (cond ((and vip-emacs-p 
				   (boundp 'MULE)) ; Emacs 20 with MULE
			      nil)
			     ((and vip-xemacs-p
				   (featurep 'mule))   ; XEmacs 20 with MULE
			      (list
			       (and current-prefix-arg
				    (read-coding-system 
				     "Coding-system: "))))
			     )
		       )))
  

  (defadvice find-file-other-frame (before vip-add-suffix-advice activate)
    "Use `read-file-name' for reading arguments."
    (interactive (cons (read-file-name "Find file in other frame: "
				       nil default-directory)
		       ;; if Mule and prefix argument, ask for coding system
		       (cond ((and vip-emacs-p 
				   (boundp 'MULE))   ; Emacs 20 with MULE
			      nil)
			     ((and vip-xemacs-p
				   (featurep 'mule)) ; XEmacs 20 with MULE
			      (list
			       (and current-prefix-arg
				    (read-coding-system 
				     "Coding-system: "))))
			     )
		       )))

  
  (defadvice read-file-name (around vip-suffix-advice activate)
    "Tell `exit-minibuffer' to run `vip-file-add-suffix' as a hook."
    (let ((vip-minibuffer-exit-hook 'vip-file-add-suffix))
      ad-do-it))
  
  (defadvice start-kbd-macro (after vip-kbd-advice activate)
    "Remove Viper's intercepting bindings for C-x ).
  This may be needed if the previous `:map' command terminated abnormally."
    (define-key vip-vi-intercept-map "\C-x)" nil)
    (define-key vip-insert-intercept-map "\C-x)" nil)
    (define-key vip-emacs-intercept-map "\C-x)" nil))
  
  (cond ((vip-window-display-p)
	 (let* ((search-key (if vip-xemacs-p
				[(meta shift button1up)] [M-S-mouse-1]))
		(search-key-catch
		 (if vip-xemacs-p [(meta shift button1)] [M-S-down-mouse-1]))
		(insert-key (if vip-xemacs-p
				[(meta shift button2up)] [M-S-mouse-2]))
		(insert-key-catch
		 (if vip-xemacs-p [(meta shift button2)] [M-S-down-mouse-2]))
		(search-key-unbound (and (not (key-binding search-key))
					 (not (key-binding search-key-catch))))
		(insert-key-unbound (and (not (key-binding insert-key))
					 (not (key-binding insert-key-catch))))
		)
	   
	   (if search-key-unbound
	       (global-set-key search-key 'vip-mouse-click-search-word))
	   (if insert-key-unbound
	       (global-set-key insert-key 'vip-mouse-click-insert-word))
	   
	   ;; The following would be needed if you want to use the above two
	   ;; while clicking in another frame. If you only want to use them
	   ;; by clicking in another window, not frame, the bindings below
	   ;; aren't necessary.
	   
	   ;; These must be bound to mouse-down event for the same mouse
	   ;; buttons as 'vip-mouse-click-search-word and
	   ;; 'vip-mouse-click-insert-word
	   (if search-key-unbound
	       (global-set-key search-key-catch 'vip-mouse-catch-frame-switch))
	   (if insert-key-unbound
	       (global-set-key insert-key-catch 'vip-mouse-catch-frame-switch))
	   
	   (if vip-xemacs-p
	       (add-hook 'mouse-leave-frame-hook
			 'vip-remember-current-frame)
	     (defadvice handle-switch-frame (before vip-frame-advice activate)
	       "Remember the selected frame before the switch-frame event." 
	       (vip-remember-current-frame (selected-frame))))
	   )))
  ) ; end vip-non-hook-settings


(if (eq viper-mode 'ask)
    (progn
      (save-window-excursion
	(with-output-to-temp-buffer " *vip-info*"
	  (princ "
You have loaded Viper, and are about to Viperize your emacs!

Viper is a Package for Emacs Rebels
and a venomous VI PERil,

It's time to decide: to Viperize or not to Viperize...

If you wish to Viperize AND make this your way of life, please put 

	(setq viper-mode t)
	(require 'viper)

in your .emacs file (preferably, close to the top).
These two lines must come in the order given.

Also, the startup file name has been changed from .vip to .viper"))
	(if (y-or-n-p "Viperize? ")
	    (setq viper-mode t)
	  (setq viper-mode nil))
	(message "")
	(kill-buffer " *vip-info*"))))




;; Get viper standard value of SYMBOL. If symbol is customized, get its
;; standard value. Otherwise, get the value saved in the alist STORAGE.  If
;; STORAGE is nil, use viper-saved-user-settings. 
(defun viper-standard-value (symbol &optional storage)
  (or (eval (car (get symbol 'customized-value)))
      (eval (car (get symbol 'saved-value)))
      (nth 1 (assoc symbol (or storage viper-saved-user-settings)))))



;; save non-viper vars that Viper might change
(if (null viper-saved-non-viper-variables)
    (setq viper-saved-non-viper-variables
	  (list
	   (cons 'default-major-mode (list default-major-mode))
	   (cons 'next-line-add-newlines (list next-line-add-newlines))
	   (cons 'require-final-newline (list require-final-newline))
	   (cons 'scroll-step (list scroll-step))
	   (cons 'mode-line-buffer-identification
		 (list (default-value 'mode-line-buffer-identification)))
	   (cons 'global-mode-string (list global-mode-string))
	   (if vip-emacs-p
	       (cons 'mark-even-if-inactive (list mark-even-if-inactive)))
	   )))
       
      
;; Set some useful macros, advices
;; These must be BEFORE ~/.viper is loaded, 
;; so the user can unrecord them in ~/.viper.
(if viper-mode
    (progn
      ;; set advices and some variables that give emacs Vi look.
      (vip-non-hook-settings)

      ;; repeat the 2nd previous command without rotating the command history
      (vip-record-kbd-macro
       (vector vip-repeat-from-history-key '\1) 'vi-state
       [(meta x) v i p - r e p e a t - f r o m - h i s t o r y return] 't)
      ;; repeat the 3d previous command without rotating the command history
      (vip-record-kbd-macro
       (vector vip-repeat-from-history-key '\2) 'vi-state
       [(meta x) v i p - r e p e a t - f r o m - h i s t o r y return] 't)
      
      ;; set macros for toggling case sensitivity and regexp search 
      (vip-set-searchstyle-toggling-macros nil)
      ;; Make %%% toggle parsing comments for matching parentheses
      (vip-set-parsing-style-toggling-macro nil)
      ))
 

;; ~/.viper is loaded if it exists
(if (and (file-exists-p vip-custom-file-name)
	 viper-mode
	 (not noninteractive))
    (load vip-custom-file-name))

;; VIP compatibility: merge whatever the user has in vip-mode-map into
;; Viper's basic map.
(vip-add-keymap vip-mode-map vip-vi-global-user-map)

(if (boundp 'vip-expert-level)
    (setq viper-expert-level vip-expert-level))



;; Applying Viper customization -- runs after (load .vip)

;; Save user settings or Viper defaults for vars controled by
;; viper-expert-level 
(if (null viper-saved-user-settings)
    (setq viper-saved-user-settings 
	  (list (cons 'vip-want-ctl-h-help (list vip-want-ctl-h-help))
		(cons 'viper-always (list viper-always))
		(cons 'vip-no-multiple-ESC (list vip-no-multiple-ESC))
		(cons 'vip-ex-style-motion (list vip-ex-style-motion))
		(cons 'vip-ex-style-editing-in-insert
		      (list vip-ex-style-editing-in-insert))
		(cons 'vip-want-emacs-keys-in-vi 
		      (list vip-want-emacs-keys-in-vi))
		(cons 'vip-electric-mode (list vip-electric-mode))
		(cons 'vip-want-emacs-keys-in-insert
		      (list vip-want-emacs-keys-in-insert))
		(cons 'vip-re-search (list vip-re-search)))))
	      

(if viper-mode
    (progn
      (vip-set-minibuffer-style)
      (if vip-buffer-search-char
	  (vip-buffer-search-enable))
      (vip-update-alphanumeric-class)
      ))
   

;;; Familiarize Viper with some minor modes that have their own keymaps
(if viper-mode
    (progn
      (vip-harness-minor-mode "compile")
      (vip-harness-minor-mode "outline")
      (vip-harness-minor-mode "allout")
      (vip-harness-minor-mode "xref")
      (vip-harness-minor-mode "lmenu")
      (vip-harness-minor-mode "vc")
      (vip-harness-minor-mode "ltx-math") ; LaTeX-math-mode in AUC-TeX
      (vip-harness-minor-mode "latex")    ; which is in one of these two files
      (vip-harness-minor-mode "cyrillic")
      (vip-harness-minor-mode "russian")
      (vip-harness-minor-mode "view-less")
      (vip-harness-minor-mode "view")
      ))


;; Intercept maps could go in viper-keym.el
;; We keep them here in case someone redefines them in ~/.viper

(define-key vip-vi-intercept-map vip-ESC-key 'vip-intercept-ESC-key)
(define-key vip-insert-intercept-map vip-ESC-key 'vip-intercept-ESC-key)

;; This is taken care of by vip-insert-global-user-map.
;;(define-key vip-replace-map vip-ESC-key 'vip-intercept-ESC-key)


;; The default vip-toggle-key is \C-z; for the novice, it suspends or
;; iconifies Emacs
(define-key vip-vi-intercept-map vip-toggle-key 'vip-toggle-key-action)
(define-key vip-emacs-intercept-map vip-toggle-key 'vip-change-state-to-vi)


(if (and viper-mode
	 (or viper-always 
	     (and (< viper-expert-level 5) (> viper-expert-level 0))))
    (vip-set-hooks))
    
;; Let all minor modes take effect after loading
;; this may not be enough, so we also set default minor-mode-alist.
;; Without setting the default, new buffers that come up in emacs mode have
;; minor-mode-map-alist = nil, unless we call vip-change-state-*
(if (eq vip-current-state 'emacs-state)
    (progn
      (vip-change-state-to-emacs)
      (setq-default minor-mode-map-alist minor-mode-map-alist)
      ))
    


(run-hooks 'vip-load-hook) ; the last chance to change something

(provide 'vip)
(provide 'viper)


;;; Local Variables:
;;; eval: (put 'vip-deflocalvar 'lisp-indent-hook 'defun)
;;; End:

;;;  viper.el ends here
