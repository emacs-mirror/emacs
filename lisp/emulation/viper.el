;;; viper.el --- A full-featured Vi emulator for GNU Emacs 19 and XEmacs 19,
;;		 a VI Plan for Emacs Rescue,
;;		 and a venomous VI PERil.
;;		 Viper Is also a Package for Emacs Rebels.

;; Copyright (C) 1994, 1995 Free Software Foundation, Inc.

;;  Version:  2.72
;;  Keywords: emulations
;;  Author: Michael Kifer <kifer@cs.sunysb.edu>

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

;; LCD Archive Entry:
;; viper|Michael Kifer|kifer@cs.sunysb.edu|
;; A full-featured  Vi emulator for Emacs 19 and XEmacs 19|
;; 19-February-95|2.72|~/modes/viper.tar.Z|

(defconst viper-version "2.72 of February 19, 1995"
  "The current version of Viper")

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
;;    Bffer-specific definitions override mode-specific
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
;;  Bug reports and ideas contributed by the following users
;;  have helped improve Viper and the various versions of VIP: 
;;
;;  jjm@hplb.hpl.hp.com (Jean-Jacques Moreau), jl@cse.ogi.edu (John
;;  Launchbury), rxga@ulysses.att.com, jamesm@bga.com (D.J. Miller II),
;;  ascott@fws214.intel.com (Andy Scott), toma@convex.convex.com,
;;  gvr@cs.brown.edu, dave@hellgate.utah.edu, cook@biostat.wisc.edu
;;  (Tom Cook), lindstro@biostat.wisc.edu (Mary Lindstrom),
;;  edmonds@edmonds.home.cs.ubc.ca (Brian Edmonds), mveiga@dit.upm.es
;;  (Marcelino Veiga Tuimil), dwight@toolucky.llnl.gov (Dwight Shih),
;;  phil_brooks@MENTORG.COM (Phil Brooks), kin@isi.com (Kin Cho),
;;  ahg@panix.com (Al Gelders), dwallach@cs.princeton.edu (Dan Wallach),
;;  hpz@ibmhpz.aug.ipp-garching.mpg.de (Hans-Peter Zehrfeld),
;;  simonb@prl.philips.co.uk (Simon Blanchard), Mark.Bordas@East.Sun.COM
;;  (Mark Bordas), gviswana@cs.wisc.edu (Guhan Viswanathan)
;;
;; Special thanks to Marcelino Veiga Tuimil <mveiga@dit.upm.es> for
;; suggesting a way of intercepting ESC sequences on dumb terminals. Due to
;; this, Viper can now handle arrow keys, F-keys, etc., in Xterm windows
;; and on dumb terminals. This also made it possible to implement Vi-style
;; timeout macros.
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
;; use (kill-all-local-variables) when they start. Some major modes
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
;;  Novice users, however, may want to set their vip-expert-level to 1
;;  in their .vip file. This will enable vip-vi-diehard-minor-mode. This
;;  minor mode's bindings make Viper simulate the usual Vi very closely.
;;  For instance,  C-c will not have its standard Emacs binding
;;  and so many of the goodies of Emacs are not available.
;;
;;  An skilled user, should set vip-expert-level to at least 3. This will
;;  enable ;;  C-c and many Emacs facilities will become available.
;;  In this case, vip-vi-diehard-minor-mode is inactive.
;;
;;  Viper gurus should have at least
;;      (setq vip-expert-level 4)
;;  in their ~/.vip files. This will unsuppress all Emacs keys that are not
;;  essential for VI-style editing.
;;  Pick-and-choose users may want to put
;;      (setq vip-expert-level 5)
;;  in ~/.vip. Viper will then leave it up to the user to set the variables
;;  vip-want-*  See vip-set-expert-level for details.
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


(require 'advice)
(require 'cl)
(require 'ring)

(require 'viper-util)


;;; Variables
	      
;; Is t until viper-mode executes for the very first time. 
;; Prevents recursive descend into startup messages.
(defvar vip-first-time t)

(defvar vip-expert-level 0
  "User's expert level.
The minor mode vip-vi-diehard-minor-mode is in effect when
vip-expert-level is 1 or 2 or when vip-want-emacs-keys-in-vi is t.
The minor mode vip-insert-diehard-minor-mode is in effect when
vip-expert-level is 1 or 2 or if vip-want-emacs-keys-in-insert is t.
Use `M-x vip-set-expert-level' to change this.")

;; Max expert level supported by Viper. This is NOT a user option.
;; It is here to make it hard for the user from resetting it.
(defconst vip-max-expert-level 5)

;; Contains user settings for vars affected by vip-set-expert-level function.
;; Not a user option.
(defvar vip-saved-user-settings nil)
	       

;;; Viper minor modes

;; for some reason, this is not local in Emacs, so I made it so.
(make-variable-buffer-local 'minor-mode-map-alist)

;; Ideally, minor-mode-map-alist should be permanent-local. But Emacs has a
;; bug that precludes that. So, there is a workaround in
;; vip-harness-minor-mode. 
;;(put 'minor-mode-map-alist 'permanent-local t)

;; Mode for vital things like \e, C-z.
(vip-deflocalvar vip-vi-intercept-minor-mode nil)

(vip-deflocalvar vip-vi-basic-minor-mode nil
  "Viper's minor mode for Vi bindings.")
  
(vip-deflocalvar vip-vi-local-user-minor-mode nil
  "Auxiliary minor mode for user-defined local bindings in Vi state.")

(vip-deflocalvar vip-vi-global-user-minor-mode nil
  "Auxiliary minor mode for user-defined global bindings in Vi state.")

(vip-deflocalvar vip-vi-state-modifier-minor-mode nil
  "Minor mode used to make major-mode-specific modification to Vi state.")

(vip-deflocalvar vip-vi-diehard-minor-mode nil
  "This minor mode is in effect when the user wants Viper to be Vi.")

(vip-deflocalvar vip-vi-kbd-minor-mode nil
  "Minor mode for Ex command macros Vi state.
The corresponding keymap stores key bindings of Vi macros defined with
the Ex command :map.")

;; Mode for vital things like \e, C-z.
(vip-deflocalvar vip-insert-intercept-minor-mode nil)

(vip-deflocalvar vip-insert-basic-minor-mode nil
  "Viper's minor mode for bindings in Insert mode.")

(vip-deflocalvar vip-insert-local-user-minor-mode nil
  "Auxiliary minor mode for buffer-local user-defined bindings in Insert state.
This is a way to overshadow normal Insert mode bindings locally to certain
designated buffers.")

(vip-deflocalvar vip-insert-global-user-minor-mode nil
  "Auxiliary minor mode for global user-defined bindings in Insert state.")

(vip-deflocalvar vip-insert-state-modifier-minor-mode nil
  "Minor mode used to make major-mode-specific modification to Insert state.")

(vip-deflocalvar vip-insert-diehard-minor-mode nil
  "Minor mode that simulates Vi very closely.
Not recommened, except for the novice user.")

(vip-deflocalvar vip-insert-kbd-minor-mode nil
"Minor mode for Ex command macros Insert state.
The corresponding keymap stores key bindings of Vi macros defined with
the Ex command :map!.")

(vip-deflocalvar vip-replace-minor-mode nil
  "Minor mode in effect in replace state (cw, C, and the like commands).")

;; Mode for vital things like \C-z and \C-x)
;; This is t, by default. So, any new buffer will have C-z defined as
;; switch to Vi, unless we switched states in this buffer
(vip-deflocalvar vip-emacs-intercept-minor-mode t)
  
(vip-deflocalvar vip-emacs-local-user-minor-mode t
  "Minor mode for local user bindings effective in Emacs state.
Users can use it to override Emacs bindings when Viper is in its Emacs
state.")  
  
(vip-deflocalvar vip-emacs-global-user-minor-mode t
  "Minor mode for global user bindings in effect in Emacs state.
Users can use it to override Emacs bindings when Viper is in its Emacs
state.")  

(vip-deflocalvar vip-emacs-kbd-minor-mode t
  "Minor mode for Vi style macros in Emacs state.
The corresponding keymap stores key bindings of Vi macros defined with
`vip-record-kbd-macro' command. There is no Ex-level command to do this
interactively.")

(vip-deflocalvar vip-emacs-state-modifier-minor-mode t
  "Minor mode used to make major-mode-specific modification to Emacs state.
For instance, a Vi purist may want to bind `dd' in Dired mode to a function
that deletes a file.")



;;; ISO characters
  
(defvar vip-automatic-iso-accents nil
  "*If non-nil, ISO accents will be turned on in insert/replace emacs states and turned off in vi-state. 
For some users, this behavior may be too primitive. In this case, use
insert/emacs/vi state hooks.")
  
  
;;; Emacs keys in other states.  

(defvar vip-want-emacs-keys-in-insert t
  "*Set to nil if you want complete Vi compatibility in insert mode.
Complete compatibility with Vi is not recommended for power use of Viper.")

(defvar vip-want-emacs-keys-in-vi t
  "*Set to nil if you want complete Vi compatibility in Vi mode.
Full Vi compatibility is not recommended for power use of Viper.")



;; VI-style Undo

;; Used to 'undo' complex commands, such as replace and insert commands.
(vip-deflocalvar vip-undo-needs-adjustment nil)
(put 'vip-undo-needs-adjustment 'permanent-local t)

;; A mark that Viper puts on buffer-undo-list.  Marks the beginning of a
;; complex command that must be undone atomically. If inserted, it is
;; erased by vip-change-state-to-vi and vip-repeat.
(defconst vip-buffer-undo-list-mark 'viper)

(defvar vip-keep-point-on-undo nil
  "*Non-nil means not to move point while undoing commands.
This style is different from Emacs and Vi. Try it to see if
it better fits your working style.")  

;; Replace mode and changing text

;; Viper's own after/before change functions, which get add-hook'ed to Emacs'
(vip-deflocalvar vip-after-change-functions nil "")
(vip-deflocalvar vip-before-change-functions nil "")
(vip-deflocalvar vip-post-command-hooks nil "")
(vip-deflocalvar vip-pre-command-hooks nil "")

;; Can be used to pass global states around for short period of time
(vip-deflocalvar vip-intermediate-command nil "")

;; Indicates that the current destructive command has started in replace mode.
(vip-deflocalvar vip-began-as-replace nil "")

(defvar vip-replace-overlay-cursor-color "Red"
  "*Color to use in Replace state")

  
(vip-deflocalvar vip-replace-overlay nil "")
(put 'vip-replace-overlay 'permanent-local t)

(if window-system
    (progn
      (make-face 'vip-replace-overlay-face)
      (or (face-differs-from-default-p 'vip-replace-overlay-face)
	  (progn
	    (if (vip-can-use-colors "darkseagreen2" "Black")
		(progn
		  (set-face-background
		   'vip-replace-overlay-face "darkseagreen2")
		  (set-face-foreground 'vip-replace-overlay-face "Black")))
	    (set-face-underline-p 'vip-replace-overlay-face t))
	  )))
	    
(defvar vip-replace-overlay-face 'vip-replace-overlay-face
  "*Face for highlighting replace regions on a window display.")
  
(defvar vip-replace-region-end-symbol 
  (if (and window-system (vip-display-color-p)) ""  "$")
  "*Symbol to mark the end of a replacement region. A string.
At present, only the first character of a non-empty string is used to
actually mark the region.")
(defvar vip-replace-region-start-symbol ""
  "*Symbol to mark the beginning of a replacement region. A string.
Not yet implemented.")
  
;; These are local marker that must be initialized to nil and moved with
;; `vip-move-marker-locally'
;;
;; Remember the last position inside the replace region.
(vip-deflocalvar vip-last-posn-in-replace-region nil)
;; Remember the last position while inserting
(vip-deflocalvar vip-last-posn-while-in-insert-state nil)
(put 'vip-last-posn-in-replace-region 'permanent-local t)
(put 'vip-last-posn-while-in-insert-state 'permanent-local t)

(vip-deflocalvar vip-sitting-in-replace nil "")
(put 'vip-sitting-in-replace 'permanent-local t)
  
;; Remember the number of characters that have to be deleted in replace
;; mode to compensate for the inserted characters.
(vip-deflocalvar vip-replace-chars-to-delete 0 "")
(vip-deflocalvar vip-replace-chars-deleted 0 "")

;; Insertion ring and command ring
(defvar vip-insertion-ring-size 14
  "The size of the insertion ring.")
;; The insertion ring.
(defvar vip-insertion-ring nil)
;; This is temp insertion ring. Used to do rotation for display purposes.
;; When rotation just started, it is initialized to vip-insertion-ring.
(defvar vip-temp-insertion-ring nil)
(defvar vip-last-inserted-string-from-insertion-ring "")

(defvar vip-command-ring-size 14
  "The size of the command ring.")
;; The command ring.
(defvar vip-command-ring nil)
;; This is temp command ring. Used to do rotation for display purposes.
;; When rotation just started, it is initialized to vip-command-ring.
(defvar vip-temp-command-ring nil)

;; Modes and related variables

;; Current mode.  One of: `emacs-state', `vi-state', `insert-state'
(vip-deflocalvar vip-current-state 'emacs-state)


(defvar vip-toggle-key "\C-z"
  "The key used to change states from emacs to Vi and back.
In insert mode, this key also functions as Meta. 
Must be set in .vip file or prior to loading Viper.
This setting cannot be changed interactively.")

(defvar vip-ESC-key "\e" 
  "Key used to ESC. 
Must be set in .vip file or prior to loading Viper.
This setting cannot be changed interactively.")

(defvar vip-no-multiple-ESC  t
  "*If true, multiple ESC in Vi mode will cause bell to ring.
\_ is then mapped to Meta.
This is set to t on a windowing terminal and to 'twice on a dumb
terminal (unless the user level is 1, 2, or 5). On a dumb terminal, this
enables cursor keys and is generally more convenient, as terminals usually
don't have a convenient Meta key.
Setting vip-no-multiple-ESC to nil will allow as many multiple ESC,
as is allowed by the major mode in effect.") 


(defvar vip-want-ctl-h-help nil
  "*If t then C-h is bound to help-command in insert mode, if nil then it is
bound to delete-backward-char.")

;; Autoindent in insert

;; Variable that keeps track of whether C-t has been pressed.
(vip-deflocalvar vip-cted nil "")

;; Preserve the indent value, used by C-d in insert mode.
(vip-deflocalvar vip-current-indent 0)

;; Whether to preserve the indent, used by C-d in insert mode.
(vip-deflocalvar vip-preserve-indent nil)

(defconst vip-auto-indent nil
  "*Autoindent if t.")

(defconst vip-shift-width 8
  "*The shiftwidth variable.")

;; Variables for repeating destructive commands

(defconst vip-keep-point-on-repeat t
  "*If t, don't move point when repeating previous command.
This is useful for doing repeated changes with the '.' key.
The user can change this to nil, if she likes when the cursor moves
to a new place after repeating previous Vi command.") 

;; Remember insert point as a marker.  This is a local marker that must be
;; initialized to nil and moved with `vip-move-marker-locally'.
(vip-deflocalvar vip-insert-point nil)
(put 'vip-insert-point 'permanent-local t)

;; This remembers the point before dabbrev-expand was called.
;; If vip-insert-point turns out to be bigger than that, it is reset
;; back to vip-pre-command-point.
;; The reason this is needed is because dabbrev-expand (and possibly
;; others) may jump to before the insertion point, delete something and
;; then reinsert a bigger piece. For instance:  bla^blo
;; If dabbrev-expand is called after `blo' and ^ undicates vip-insert-point,
;; then point jumps to the beginning of `blo'. If expansion is found, `blablo'
;; is deleted, and we have |^, where | denotes point. Next, dabbrev-expand
;; will insert the expansion, and we get: blablo^
;; Whatever we insert next goes before the ^, i.e., before the
;; vip-insert-point marker. So, Viper will think that nothing was
;; inserted. Remembering the orig position of the marker circumvents the
;; problem.
;; We don't know of any command, except dabbrev-expand, that has the same
;; problem. However, the same trick can be used if such a command is
;; discovered later.
;;
(vip-deflocalvar vip-pre-command-point nil)
(put 'vip-pre-command-point 'permanent-local t) ; this is probably an overkill

;; This is used for saving inserted text.
(defvar vip-last-insertion  nil)
  
;; Remembers the last replaced region.
(defvar vip-last-replace-region "")
  
;; Remember com point as a marker.
;; This is a local marker. Should be moved with `vip-move-marker-locally'
(vip-deflocalvar vip-com-point nil)

;; If non-nil, the value is a list (M-COM VAL COM REG inserted-text cmd-keys)
;; It is used to re-execute last destructive command.
;; M-COM is a Lisp symbol representing the function to be executed.
;; VAL is the prefix argument that was used with that command.
;; COM is an internal descriptor, such as ?r, ?c, ?C, which contains
;; additional information on how the function in M-COM is to be handled.
;; REG is the register used by command
;; INSERTED-TEXT is text inserted by that command (in case of o, c, C, i, r
;; commands).
;; COMMAND-KEYS are the keys that were typed to invoke the command.
(defvar vip-d-com nil)

;; The character remembered by the Vi `r' command.
(defvar vip-d-char nil)

;; Name of register to store deleted or yanked strings
(defvar vip-use-register nil)



;; Variables for Moves and Searches

;; For use by `;' command.
(defvar vip-f-char nil)

;; For use by `.' command.
(defvar vip-F-char nil)

;; For use by `;' command.
(defvar vip-f-forward nil)

;; For use by `;' command.
(defvar vip-f-offset nil)

;; Last search string
(defvar vip-s-string "")

(defvar vip-quote-string "> "
  "String inserted at the beginning of quoted region.")

;; If t, search is forward.
(defvar vip-s-forward nil)

(defconst vip-case-fold-search nil
  "*If t, search ignores cases.")

(defconst vip-re-search t
  "*If t, search is reg-exp search, otherwise vanilla search.")

(defconst vip-re-query-replace t
  "*If t then do regexp replace, if nil then do string replace.")

(defconst vip-re-replace t
  "*If t, do regexp replace. nil means do string replace.")

(vip-deflocalvar vip-ex-style-motion t
  "*Ex-style: the commands l,h do not cross lines, etc.")

(vip-deflocalvar vip-ex-style-editing-in-insert t
  "*The keys ^H, ^? don't jump lines in insert, ESC moves cursor back, etc.
Note: this doesn't preclude ^H and ^? from deleting characters by moving
past the insertion point. This is a feature, not a bug. ")

(vip-deflocalvar vip-delete-backwards-in-replace nil
  "*If t, DEL key will delete characters while moving the cursor backwards.
If nil, the cursor will move backwards without deleting anything.")

(defconst vip-buffer-search-char nil
  "*Key bound for buffer-searching.")

(defconst vip-search-wrap-around-t t
  "*If t, search wraps around.")
  
(vip-deflocalvar vip-related-files-and-buffers-ring nil
  "*Ring of file and buffer names that are considered to be related to the
current buffer.
These buffers can be cycled through via :R and :P commands.")
(put 'vip-related-files-and-buffers-ring 'permanent-local t)

;; Used to find out if we are done with searching the current buffer.
(vip-deflocalvar vip-local-search-start-marker nil)
;; As above, but global
(defvar vip-search-start-marker (make-marker))

;; the search overlay
(vip-deflocalvar vip-search-overlay nil)


(defvar vip-heading-start 
  (concat "^\\s-*(\\s-*defun\\s-\\|"			        ; lisp
	  "^{\\s-*$\\|^[_a-zA-Z][^()]*[()].*{\\s-*$\\|"	        ; C/C++
	  "^\\s-*class.*{\\|^\\s-*struct.*{\\|^\\s-*enum.*{\\|"
	  "^\\\\[sb][a-z]*{.*}\\s-*$\\|"	    		; latex
	  "^@node\\|@table\\|^@m?enu\\|^@itemize\\|^@if\\|"	; texinfo
	  "^.+:-")			                        ; prolog
  "*Regexps for Headings. Used by \[\[ and \]\].")

(defvar vip-heading-end 
  (concat "^}\\|"						; C/C++
	  "^\\\\end{\\|"					; latex
	  "^@end \\|"						; texinfo
	  ")\n\n[ \t\n]*\\|"					; lisp
	  "\\.\\s-*$")						; prolog
      "*Regexps to end Headings/Sections. Used by \[\].")


;; These two vars control the interaction of jumps performed by ' and `.
;; In this new version, '' doesn't erase the marks set by ``, so one can
;; use both kinds of jumps interchangeably and without loosing positions
;; inside the lines.

;; Remembers position of the last jump done using ``'.
(vip-deflocalvar vip-last-jump  nil)
;; Remembers position of the last jump done using `''.
(vip-deflocalvar vip-last-jump-ignore 0)

;; Some common error messages

(defconst vip-SpuriousText "Spurious text after command"  "")
(defconst vip-BadExCommand "Not an editor command"   "")
(defconst vip-InvalidCommandArgument "Invalid command argument"   "")
(defconst vip-NoPrevSearch "No previous search string"   "")
(defconst vip-EmptyRegister "`%c': Nothing in this register"   "")
(defconst vip-InvalidRegister "`%c': Invalid register"   "")
(defconst vip-EmptyTextmarker "`%c': Text marker doesn't point anywhere"   "")
(defconst vip-InvalidTextmarker "`%c': Invalid text marker"   "")
(defconst vip-InvalidViCommand "Invalid command"   "")
(defconst vip-BadAddress "Ill-formed address"   "")
(defconst vip-FirstAddrExceedsSecond "First address exceeds second"   "")
(defconst vip-NoFileSpecified "No file specified"   "")


;; History variables

(defvar vip-history nil)
;; History of search strings.
(defvar vip-search-history  (list ""))
;; History of query-replace strings used as a source.
(defvar vip-replace1-history nil)
;; History of query-replace strings used as replacement.
(defvar vip-replace2-history nil)
;; History of region quoting strings.
(defvar vip-quote-region-history (list vip-quote-string))
;; History of Ex-style commands.
(defvar vip-ex-history nil)
;; History of shell commands.
(defvar vip-shell-history nil)


;; Last shell command. There are two of these, one for Ex (in viper-ex)
;; and one for Vi.

;; Last shell command executed with ! command.
(defvar vip-last-shell-com nil)



;;; Miscellaneous

;; setup emacs-supported vi-style feel
(setq mark-even-if-inactive t
      next-line-add-newlines nil
      require-final-newline t)

(defvar vip-inhibit-startup-message nil
  "Whether Viper startup message should be inhibited.")

(defvar vip-always t
  "t means, arrange that vi-state will be a default.")

(defvar vip-custom-file-name "~/.vip"
  "Viper customisation file.
This variable must be set _before_ loading Viper.")

(defvar vip-info-file-name "viper"
  "The name prefix for Viper Info files.")

(defvar vip-spell-function 'ispell-region
  "Spell function used by #s<move> command to spell.")

(defvar vip-tags-file-name "TAGS")

;; Minibuffer

(defvar vip-vi-style-in-minibuffer t
  "If t, use vi-style editing in minibuffer.
Should be set in `~/.vip' file.")
  
;; overlay used in the minibuffer to indicate which state it is in
(vip-deflocalvar vip-minibuffer-overlay nil)

;; Hook, specific to Viper, which is run just *before* exiting the minibuffer.
;; Beginning with Emacs 19.26, the standard `minibuffer-exit-hook' is run
;; *after* exiting the minibuffer
(defvar vip-minibuffer-exit-hook nil)

(vip-deflocalvar vip-vi-minibuffer-minor-mode nil
   "Minor mode that forces Vi-style when the Minibuffer is in Vi state.")
(vip-deflocalvar vip-insert-minibuffer-minor-mode nil
   "Minor mode that forces Vi-style when the Minibuffer is in Insert state.")
  
(vip-deflocalvar vip-add-newline-at-eob t
  "If t, always add a newline at the end of buffer.
Usually, Viper adds a newline character at the end of the last
line in a buffer, if it's missing. In some major modes, however, like
shell-mode, this is undesirable and must be set to nil. See vip-set-hooks.")
       

;; Mode line
(defconst vip-vi-state-id  	"<V> "
  "Mode line tag identifying the Vi mode of Viper.")
(defconst vip-emacs-state-id	"<E> "
  "Mode line tag identifying the Emacs mode of Viper.")
(defconst vip-insert-state-id	"<I> "
  "Mode line tag identifying the Insert mode of Viper.")
(defconst vip-replace-state-id	"<R> "
  "Mode line tag identifying the Replace mode of Viper.")

;; Viper changes the default mode-line-buffer-identification
(setq-default mode-line-buffer-identification '(" %b"))

;; Variable displaying the current Viper state in the mode line.
(vip-deflocalvar vip-mode-string vip-emacs-state-id)
(or (memq 'vip-mode-string global-mode-string)
    (setq global-mode-string
	  (append '("" vip-mode-string) (cdr global-mode-string))))


(defvar vip-vi-state-hooks nil
  "*Hooks run just before the switch to Vi mode is completed.")
(defvar vip-insert-state-hooks nil
  "*Hooks run just before the switch to Insert mode is completed.")
(defvar vip-replace-state-hooks nil
  "*Hooks run just before the switch to Replace mode is completed.")
(defvar vip-emacs-state-hooks nil
  "*Hooks run just before the switch to Emacs mode is completed.")
  
(defvar vip-load-hooks nil
  "Hooks run just after loading Viper.")
  

;; Generic predicates

;; These test functions are shamelessly lifted from vip 4.4.2 by Aamod Sane

;; generate test functions
;; given symbol foo, foo-p is the test function, foos is the set of
;; Viper command keys
;; (macroexpand '(vip-test-com-defun foo))
;; (defun foo-p (com) (consp (memq (if (< com 0) (- com) com) foos)))

(defmacro vip-test-com-defun (name)
  (let* (;;(snm (make-symbol "s1"))
	 (snm (symbol-name name))
	 ;;(nm-p (make-symbol "s2"))
	 (nm-p (intern (concat snm "-p")))
	 ;;(nms (make-symbol "s3"))
	 (nms (intern (concat snm "s"))))
    (` (defun (, nm-p) (com) 
	 (consp (memq (if (< com 0) (- com) com) (, nms)))))))
  
;; Variables for defining VI commands

(defconst vip-prefix-commands '(?c ?d ?y ?! ?= ?# ?< ?> ?\")
  "Modifying commands that can be prefixes to movement commands")
(vip-test-com-defun vip-prefix-command)
  
(defconst vip-charpair-commands '(?c ?d ?y ?! ?= ?< ?> ?r ?R)
  "Commands that are pairs eg. dd. r and R here are a hack")
(vip-test-com-defun vip-charpair-command)

(defconst vip-movement-commands '(?b ?B ?e ?E ?f ?F ?G ?h ?H ?j ?k ?l
				     ?H ?M ?n ?t ?T ?w ?W ?$ ?%
				     ?^ ?( ?) ?- ?+ ?| ?{ ?} ?[ ?] ?' ?`
				     ?; ?, ?0 ?? ?/
				     )
				     "Movement commands")
(vip-test-com-defun vip-movement-command)

(defconst vip-dotable-commands '(?c ?d ?C ?D ?> ?<)
  "Commands that can be repeated by .(dotted)")
(vip-test-com-defun vip-dotable-command)

(defconst vip-hash-cmds '(?c ?C ?g ?q ?S)
  "Commands that can follow a #")
(vip-test-com-defun vip-hash-cmd)

(defconst vip-regsuffix-commands '(?d ?y ?Y ?D ?p ?P ?x ?X)
  "Commands that may have registers as prefix")
(vip-test-com-defun vip-regsuffix-command)



;;; Arrange the keymaps
(require 'viper-keym)


;;;; CODE

;; changing mode

;; Change state to NEW-STATE---either emacs-state, vi-state, or insert-state.
(defun vip-change-state (new-state)
  ;; keep them always fresh
  (add-hook 'post-command-hook 'vip-post-command-sentinel t)
  (add-hook 'pre-command-hook 'vip-pre-command-sentinel t)
  ;; These hooks will be added back if switching to insert/replace mode
  (remove-hook 'vip-post-command-hooks
	       'vip-insert-state-post-command-sentinel)
  (remove-hook 'vip-pre-command-hooks
	       'vip-insert-state-pre-command-sentinel)
  (cond ((eq new-state 'vi-state)
	 (cond ((member vip-current-state '(insert-state replace-state))
		    
		;; move vip-last-posn-while-in-insert-state
		;; This is a normal hook that is executed in insert/replace
		;; states after each command. In Vi/Emacs state, it does
		;; nothing. We need to execute it here to make sure that
		;; the last posn was recorded when we hit ESC.
		;; It may be left unrecorded if the last thing done in
		;; insert/repl state was dabbrev-expansion or abbrev
		;; expansion caused by hitting ESC
		(vip-insert-state-post-command-sentinel)
		
		(condition-case conds
		    (progn
		      (vip-save-last-insertion
		       vip-insert-point 
		       vip-last-posn-while-in-insert-state)
		      (if vip-began-as-replace
			  (setq vip-began-as-replace nil)
			;; repeat insert commands if numerical arg > 1
			(save-excursion
			  (vip-repeat-insert-command))))
		  (error
		   (vip-message-conditions conds)))
		     
		(if (> (length vip-last-insertion) 0)
		    (vip-push-onto-ring vip-last-insertion
					'vip-insertion-ring))
		
		(if vip-ex-style-editing-in-insert
		    (or (bolp) (backward-char 1))))
	       ))
	 
	;; insert or replace
	((memq new-state '(insert-state replace-state))
	 (if (memq vip-current-state '(emacs-state vi-state))
	     (vip-move-marker-locally 'vip-insert-point (point)))
	 (vip-move-marker-locally 'vip-last-posn-while-in-insert-state (point))
	 (add-hook 'vip-post-command-hooks
		   'vip-insert-state-post-command-sentinel t)
	 (add-hook 'vip-pre-command-hooks
		   'vip-insert-state-pre-command-sentinel t)
	 )
	) ; outermost cond
  
  ;; Nothing needs to be done to switch to emacs mode! Just set some
  ;; variables, which is done in vip-change-state-to-emacs!

  (setq vip-current-state new-state)
  (vip-normalize-minor-mode-map-alist)
  (vip-adjust-keys-for new-state)
  (vip-set-mode-vars-for new-state)
  (vip-refresh-mode-line)
  )


    
(defun vip-adjust-keys-for (state)
  "Make necessary adjustments to keymaps before entering STATE."
  (cond ((memq state '(insert-state replace-state))
	 (if vip-auto-indent
	     (progn
	       (define-key vip-insert-basic-map "\C-m" 'vip-autoindent)
	       (if vip-want-emacs-keys-in-insert
		   ;; expert
		   (define-key vip-insert-basic-map "\C-j" nil)
		 ;; novice
		 (define-key vip-insert-basic-map "\C-j" 'vip-autoindent))))
		    
	 (setq vip-insert-diehard-minor-mode
	       (not vip-want-emacs-keys-in-insert))
		   
	 (if vip-want-ctl-h-help
	     (progn 
	       (define-key vip-insert-basic-map "\C-h" 'help-command)
	       (define-key vip-replace-map "\C-h" 'help-command))
	   (define-key vip-insert-basic-map 
	     "\C-h" 'vip-del-backward-char-in-insert)
	   (define-key vip-replace-map
	     "\C-h" 'vip-del-backward-char-in-replace)))
		     
	(t
	 (setq vip-vi-diehard-minor-mode (not vip-want-emacs-keys-in-vi))
	 (if vip-want-ctl-h-help
	     (define-key vip-vi-basic-map "\C-h" 'help-command)
	   (define-key vip-vi-basic-map "\C-h" 'vip-backward-char)))
	))
	     
    
(defun  vip-normalize-minor-mode-map-alist ()
  "Normalizes minor-mode-map-alist by putting Viper keymaps first.
This ensures that Viper bindings are in effect, regardless of which minor
modes were turned on by the user or by other packages."
  (setq minor-mode-map-alist 
	(vip-append-filter-alist
	 (list
	       (cons 'vip-vi-intercept-minor-mode vip-vi-intercept-map)
	       (cons 'vip-vi-minibuffer-minor-mode vip-minibuffer-map) 
	       (cons 'vip-vi-local-user-minor-mode vip-vi-local-user-map)
	       (cons 'vip-vi-kbd-minor-mode vip-vi-kbd-map)
	       (cons 'vip-vi-global-user-minor-mode vip-vi-global-user-map)
	       (cons 'vip-vi-state-modifier-minor-mode
		     (if (keymapp
			  (cdr (assoc major-mode vip-vi-state-modifier-alist)))
			 (cdr (assoc major-mode vip-vi-state-modifier-alist))
		       vip-empty-keymap))
	       (cons 'vip-vi-diehard-minor-mode  vip-vi-diehard-map)
	       (cons 'vip-vi-basic-minor-mode     vip-vi-basic-map)
	       (cons 'vip-insert-intercept-minor-mode vip-insert-intercept-map)
	       (cons 'vip-replace-minor-mode  vip-replace-map)
	       ;; vip-insert-minibuffer-minor-mode must come after
	       ;; vip-replace-minor-mode 
	       (cons 'vip-insert-minibuffer-minor-mode
		     vip-minibuffer-map) 
	       (cons 'vip-insert-local-user-minor-mode
		     vip-insert-local-user-map)
	       (cons 'vip-insert-kbd-minor-mode vip-insert-kbd-map)
	       (cons 'vip-insert-global-user-minor-mode
		     vip-insert-global-user-map)
	       (cons 'vip-insert-state-modifier-minor-mode
		     (if (keymapp
			  (cdr
			   (assoc major-mode vip-insert-state-modifier-alist)))
			 (cdr
			  (assoc major-mode vip-insert-state-modifier-alist))
		       vip-empty-keymap))
	       (cons 'vip-insert-diehard-minor-mode vip-insert-diehard-map)
	       (cons 'vip-insert-basic-minor-mode vip-insert-basic-map)
	       (cons 'vip-emacs-intercept-minor-mode
		     vip-emacs-intercept-map)
	       (cons 'vip-emacs-local-user-minor-mode
		     vip-emacs-local-user-map)
	       (cons 'vip-emacs-kbd-minor-mode vip-emacs-kbd-map)
	       (cons 'vip-emacs-global-user-minor-mode
		     vip-emacs-global-user-map)
	       (cons 'vip-emacs-state-modifier-minor-mode
		     (if (keymapp
			  (cdr
			   (assoc major-mode vip-emacs-state-modifier-alist)))
			 (cdr
			  (assoc major-mode vip-emacs-state-modifier-alist))
		       vip-empty-keymap))
	       )
	 minor-mode-map-alist)))
	 
 



;; Viper mode-changing commands and utilities

(defun vip-refresh-mode-line ()
  "Modifies mode-line-buffer-identification."
  (setq vip-mode-string	
	(cond ((eq vip-current-state 'emacs-state) vip-emacs-state-id)
	      ((eq vip-current-state 'vi-state) vip-vi-state-id)
	      ((eq vip-current-state 'replace-state) vip-replace-state-id)
	      ((eq vip-current-state 'insert-state) vip-insert-state-id)))
    
  ;; Sets Viper mode string in global-mode-string
  (force-mode-line-update))
  
;;;###autoload
(defun viper-mode ()
  "Turn on Viper emulation of Vi."
  (interactive)
  (if (not noninteractive)
      (progn
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
		      "Viper Is a Package for Emacs Rebels.
It is also a VI Plan for Emacs Rescue and a venomous VI PERil.

Technically speaking, Viper is a Vi emulation package for Emacs 19 and
XEmacs 19.  It supports virtually all of Vi and Ex functionality, extending
and improving upon much of it.

   1. Viper supports Vi at several levels. Level 1 is the closest to
      Vi, level 5 provides the most flexibility to depart from many Vi
      conventions.
      
      You will be asked to specify your user level in a following screen.
   
      If you select user level 1 then the keys ^X, ^C, ^Z, and ^G will
      behave as in VI, to smooth transition to Viper for the beginners. 
      However, to use Emacs productively, you are advised to reach user
      level 3 or higher. 
      
      If your user level is 2 or higher, ^X and ^C will invoke Emacs
      functions,as usual in Emacs; ^Z will toggle vi/emacs modes, and
      ^G will be the usual Emacs's keyboard-quit (something like ^C in VI).
   
   2. Vi exit functions (e.g., :wq, ZZ) work on INDIVIDUAL files -- they
      do not cause Emacs to quit, except at user level 1 (a novice).
   3. ^X^C EXITS EMACS.
   4. Viper supports multiple undo: `u' will undo. Typing `.' will repeat
      undo. Another `u' changes direction.
   
   6. Emacs Meta functions are invoked by typing `_' or `\\ ESC'.
      On a window system, the best way is to use the Meta-key.
   7. Try \\[keyboard-quit] and \\[abort-recursive-edit] repeatedly,
      if something funny happens. This would abort the current editing
      command. 
      
You can get more information on Viper by:

   a. Typing `:help' in Vi state
   b. Printing Viper manual, found in ./etc/viper.dvi
   c. Printing ViperCard, the Quick Reference, found in ./etc/viperCard.dvi
    
This startup message appears whenever you load Viper, unless you type `y' now."
		      ))
		    (goto-char (point-min))
		    (if (y-or-n-p "Inhibit Viper startup message? ")
			(vip-save-setting
			 'vip-inhibit-startup-message
			 "Viper startup message inhibited"
			 vip-custom-file-name t))
		    (kill-buffer (current-buffer))))
	      (message " ")
	      (vip-set-expert-level 'dont-change-unless)))
	(vip-change-state-to-vi))))
	
;;;###autoload
(defalias 'vip-mode 'viper-mode)


(defun vip-exit-insert-state ()
  "Switch from Insert state to Vi state."
  (interactive)
  (vip-change-state-to-vi))

(defun vip-set-mode-vars-for (state)
  "Sets Viper minor mode variables to put Viper's state STATE in effect."
  
  ;; Emacs state
  (setq vip-vi-minibuffer-minor-mode	   nil
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
	vip-emacs-intercept-minor-mode       t
	vip-emacs-local-user-minor-mode      t
	vip-emacs-kbd-minor-mode             (not (vip-is-in-minibuffer))
	vip-emacs-global-user-minor-mode     t
	vip-emacs-state-modifier-minor-mode  t
	)
  
  ;; Vi state
  (if (eq state 'vi-state) ; adjust for vi-state
      (setq 
       vip-vi-intercept-minor-mode	   t 
       vip-vi-minibuffer-minor-mode	   (vip-is-in-minibuffer)
       vip-vi-local-user-minor-mode	   t
       vip-vi-kbd-minor-mode        	   (not (vip-is-in-minibuffer))
       vip-vi-global-user-minor-mode	   t
       vip-vi-state-modifier-minor-mode    t
       ;; don't let the diehard keymap block command completion 
       ;; and other things in the minibuffer
       vip-vi-diehard-minor-mode    	   (not
					    (or vip-want-emacs-keys-in-vi
						(vip-is-in-minibuffer)))
       vip-vi-basic-minor-mode      	    t 
       vip-emacs-intercept-minor-mode       nil
       vip-emacs-local-user-minor-mode      nil
       vip-emacs-kbd-minor-mode     	    nil
       vip-emacs-global-user-minor-mode     nil
       vip-emacs-state-modifier-minor-mode  nil
       ))
  
  ;; Insert and Replace states
  (if (member state '(insert-state replace-state))
      (setq 
       vip-insert-intercept-minor-mode	    t 
       vip-replace-minor-mode	     	    (eq state 'replace-state)
       vip-insert-minibuffer-minor-mode	    (vip-is-in-minibuffer)
       vip-insert-local-user-minor-mode     t
       vip-insert-kbd-minor-mode     	    (not (vip-is-in-minibuffer))
       vip-insert-global-user-minor-mode    t
       vip-insert-state-modifier-minor-mode  t
       ;; don't let the diehard keymap block command completion 
       ;; and other things in the minibuffer
       vip-insert-diehard-minor-mode 	    (not
					     (or vip-want-emacs-keys-in-insert
						 (vip-is-in-minibuffer)))
       vip-insert-basic-minor-mode   	    t
       vip-emacs-intercept-minor-mode       nil
       vip-emacs-local-user-minor-mode      nil
       vip-emacs-kbd-minor-mode     	    nil
       vip-emacs-global-user-minor-mode     nil
       vip-emacs-state-modifier-minor-mode  nil
       ))
       
  ;; minibuffer faces
  (if window-system
      (setq vip-minibuffer-current-face
	    (cond ((eq state 'emacs-state) vip-minibuffer-emacs-face)
		  ((eq state 'vi-state) vip-minibuffer-vi-face)
		  ((memq state '(insert-state replace-state))
		   vip-minibuffer-insert-face))))
  
  (if (vip-is-in-minibuffer)
      (vip-set-minibuffer-overlay))
  )

;; This also takes care of the annoying incomplete lines in files.
;; Also, this fixed 'undo' to work vi-style for complex commands.
(defun vip-change-state-to-vi ()
  "Change Viper state to Vi."
  (interactive)
  (if (and vip-first-time (not (vip-is-in-minibuffer)))
      (viper-mode)
    (if overwrite-mode (overwrite-mode nil))
    (if abbrev-mode (expand-abbrev))
    (if (and auto-fill-function (> (current-column) fill-column))
	(funcall auto-fill-function))
    (vip-add-newline-at-eob-if-necessary)
    (if vip-undo-needs-adjustment  (vip-adjust-undo))
    (vip-change-state 'vi-state)
    (if (and vip-automatic-iso-accents (fboundp 'iso-accents-mode))
	(iso-accents-mode -1)) ; turn off iso accents
    
    ;; Protection against user errors in hooks
    (condition-case conds
	(run-hooks 'vip-vi-state-hooks)
      (error
       (vip-message-conditions conds)))))

(defun vip-change-state-to-insert ()
  "Change Viper state to Insert."
  (interactive)
  (vip-change-state 'insert-state)
  (if (and vip-automatic-iso-accents (fboundp 'iso-accents-mode))
      (iso-accents-mode 1)) ; turn iso accents on
  
  ;; Protection against user errors in hooks
  (condition-case conds
      (run-hooks 'vip-insert-state-hooks)
    (error
     (vip-message-conditions conds))))
     
(defsubst vip-downgrade-to-insert ()
 (setq vip-current-state 'insert-state
       vip-replace-minor-mode nil)
 )

    
  
;; Change to replace state. When the end of replacement region is reached,
;; replace state changes to insert state.
(defun vip-change-state-to-replace (&optional non-R-cmd)
  (vip-change-state 'replace-state)
  (if (and vip-automatic-iso-accents (fboundp 'iso-accents-mode))
      (iso-accents-mode 1)) ; turn iso accents on
  ;; Run insert-state-hook
  (condition-case conds
      (run-hooks 'vip-insert-state-hooks 'vip-replace-state-hooks)
    (error
     (vip-message-conditions conds)))
  
  (if non-R-cmd
      (vip-start-replace)
    ;; 'R' is implemented using Emacs's overwrite-mode
    (vip-start-R-mode))
  )

    
(defun vip-change-state-to-emacs ()
  "Change Viper state to Emacs."
  (interactive)
  (vip-change-state 'emacs-state)
  (if (and vip-automatic-iso-accents (fboundp 'iso-accents-mode))
      (iso-accents-mode 1)) ; turn iso accents on
  
  ;; Protection agains user errors in hooks
  (condition-case conds
      (run-hooks 'vip-emacs-state-hooks)
    (error
     (vip-message-conditions conds))))
  
;; escape to emacs mode termporarily
(defun vip-escape-to-emacs (arg &optional events)
  "Escape to Emacs state from Vi state for one Emacs command.
ARG is used as the prefix value for the executed command.  If
EVENTS is a list of events, which become the beginning of the command."
  (interactive "P")
  (vip-escape-to-state arg events 'emacs-state))
  
;; escape to Vi mode termporarily
(defun vip-escape-to-vi ()
  "Escape from Emacs state to Vi state for one Vi 1-character command.
This doesn't work with prefix arguments or most complex commands like
cw, dw,  etc. But it does work with some 2-character commands,
like dd or dr."
  (interactive)
  (vip-escape-to-state nil nil 'vi-state))  
  
;; Escape to STATE mode for one Emacs command.
(defun vip-escape-to-state (arg events state)
  (let (com key prefix-arg)
    ;; this temporarily turns off Viper's minor mode keymaps
    (vip-set-mode-vars-for state)
    (vip-normalize-minor-mode-map-alist)
    (if events (vip-set-unread-command-events events))
    
    ;; protect against keyboard quit and other errors
    (condition-case nil
	(progn
	  (unwind-protect
	      (progn
		(setq com (key-binding (setq key 
					     (if vip-xemacs-p
						 (read-key-sequence nil)
					       (read-key-sequence nil t)))))
		;; In case of indirection--chase definitions.
		;; Have to do it here because we execute this command under
		;; different keymaps, so command-execute may not do the
		;; right thing there
		(while (vectorp com) (setq com (key-binding com))))
	    nil)
	  ;; exec command in the right Viper state
	  ;; otherwise, if we switch buffers in the escaped command,
	  ;; Viper's mode vars will remain those of `state'. When we return
	  ;; to the orig buffer, the bindings will be screwed up.
	  (vip-set-mode-vars-for vip-current-state)
	  
	  ;; this-command, last-command-char, last-command-event
	  (setq this-command com)
	  (if vip-xemacs-p ; XEmacs represents key sequences as vectors
	      (setq last-command-event (vip-seq-last-elt key)
		    last-command-char (event-to-character last-command-event))
	    ;; Emacs represents them as sequences (str or vec)
	    (setq last-command-event (vip-seq-last-elt key)
		  last-command-char last-command-event))
	    
	  (if (commandp com)
	      (progn
		(setq prefix-arg arg)
		(command-execute com)))
	  )
      (quit (ding))
      (error (beep 1))))
  (vip-set-mode-vars-for vip-current-state)) ; set state in new buffer
      
(defun vip-exec-form-in-emacs  (form)
  "Execute FORM in Emacs, temporarily disabling Viper's minor modes.
Similar to vip-escape-to-emacs, but accepts forms rather than keystrokes."
  (let ((buff (current-buffer))
	result)
    (vip-set-mode-vars-for 'emacs-state)
    (setq result (eval form))
    (if (not (equal buff (current-buffer))) ; cmd switched buffer
	(save-excursion
	  (set-buffer buff)
	  (vip-set-mode-vars-for vip-current-state)))
    (vip-set-mode-vars-for vip-current-state)
    result))


  
;; This is needed because minor modes sometimes override essential Viper
;; bindings. By letting Viper know which files these modes are in, it will
;; arrange to reorganize minor-mode-map-alist so that things will work right.
(defun vip-harness-minor-mode (load-file)
  "Familiarize Viper with a minor mode defined in LOAD_FILE.
Minor modes that have their own keymaps may overshadow Viper keymaps.
This function is designed to make Viper aware of the packages that define
such minor modes.
Usage:
    (vip-harness-minor-mode load-file)

LOAD-FILE is a name of the file where the specific minor mode is defined.
Suffixes such as .el or .elc should be stripped."

  (interactive "sEnter name of the load file: ")
  
  (vip-eval-after-load load-file '(vip-normalize-minor-mode-map-alist))
  
  ;; This is a work-around the emacs bug that doesn't let us make
  ;; minor-mode-map-alist permanent-local.
  ;; This workaround changes the default for minor-mode-map-alist
  ;; each time a harnessed minor mode adds its own keymap to the a-list.
  (vip-eval-after-load load-file '(setq-default minor-mode-map-alist
					    minor-mode-map-alist))
  )
  
;; This doesn't work, i.e., doesn't replace vip-harness-minor-mode
;; function, since autoloaded files don't seem to be loaded with lisp's
;; `load' function.
;;(defadvice load (after vip-load-advice activate)
;;  "Rearrange `minor-mode-map-alist' after loading a file or a library."
;;  (vip-normalize-minor-mode-map-alist)
;;  (setq-default minor-mode-map-alist minor-mode-map-alist))



(defun vip-ESC (arg)
  "Emulate ESC key in Emacs.
Prevents multiple escape keystrokes if vip-no-multiple-ESC is true. In that
case \@ will be bound to ESC. If vip-no-multiple-ESC is 'twice double ESC
would dings in vi-state. Other ESC sequences are emulated via the current
Emacs's major mode keymap. This is more convenient on dumb terminals and in
Emacs -nw, since this won't block functional keys such as up,down,
etc. Meta key also will work. When vip-no-multiple-ESC is nil, ESC key
behaves as in Emacs, any number of multiple escapes is allowed."
  (interactive "P")
  (let (char)
    (cond ((and (not vip-no-multiple-ESC) (eq vip-current-state 'vi-state))
	   (setq char (vip-read-char-exclusive))
	   (vip-escape-to-emacs arg (list ?\e char) ))
	  ((and (eq vip-no-multiple-ESC 'twice) 
		(eq vip-current-state 'vi-state))
	   (setq char (vip-read-char-exclusive))
	   (if (= char (string-to-char vip-ESC-key))
	       (ding)
	     (vip-escape-to-emacs arg (list ?\e char) )))
	  (t (ding)))
    ))

(defun vip-alternate-ESC (arg)
  "ESC key without checking for multiple keystrokes."
  (interactive "P")
  (vip-escape-to-emacs arg '(?\e)))
  

;; Intercept ESC sequences on dumb terminals.
;; Based on the idea contributed by Marcelino Veiga Tuimil <mveiga@dit.upm.es>

;; Check if last key was ESC and if so try to reread it as a function key.
;; But only if there are characters to read during a very short time.
;; Returns the last event, if any.
(defun vip-envelop-ESC-key ()
  (let ((event last-input-event)
	(keyseq [nil])
	inhibit-quit)
    (if (vip-ESC-event-p event)
	(progn 
	  (if (vip-fast-keysequence-p)
	      (progn
		(let ((vip-vi-intercept-minor-mode nil)
		      (vip-insert-intercept-minor-mode nil)
		      (vip-emacs-intercept-minor-mode nil)
		      (vip-vi-state-modifier-minor-mode  nil)
		      (vip-vi-global-user-minor-mode  nil)
		      (vip-vi-local-user-minor-mode  nil)
		      (vip-replace-minor-mode nil) ; actually unnecessary
		      (vip-insert-state-modifier-minor-mode  nil)
		      (vip-insert-global-user-minor-mode  nil)
		      (vip-insert-local-user-minor-mode  nil)
		      (vip-emacs-state-modifier-minor-mode  nil)
		      (vip-emacs-global-user-minor-mode  nil)
		      (vip-emacs-local-user-minor-mode  nil)
		      )
		  ;; The treatment of XEmacs, below, is temporary, since we
		  ;; don't know how XEmacs will implement dumb terminals.
		  ;; Note: the treatment of fast keysequences here is
		  ;; needed only on dumb terminals in order to be able to
		  ;; handle function keys correctly.
		  (if vip-xemacs-p
		      (setq keyseq (vector event))
		    (vip-set-unread-command-events event)
		    (setq keyseq
			  (funcall
			   (ad-get-orig-definition 'read-key-sequence) nil))
		    ))
		;; If keyseq translates into something that still has ESC
		;; in the beginning, separate ESC from the rest of the seq.
		;;
		;; This is needed for the following reason:
		;; If ESC is the first symbol, we interpret it as if the
		;; user typed ESC and then quickly some other symbols.
		;; If ESC is not the first one, then the key sequence
		;; entered was apparently translated into a function key or
		;; something (e.g., one may have
		;; (define-key function-key-map "\e[192z" [f11])
		;; which would translate the escape-sequence generated by
		;; f11 in an xterm window into the symbolic key f11.
		(if (vip-ESC-event-p (elt keyseq 0))
		    (progn
		      ;; put keys following ESC on the unread list
		      ;; and return ESC as the key-sequence
		      (vip-set-unread-command-events (subseq keyseq 1))
		      (setq last-input-event event
			    keyseq "\e")))
		) ; end progn
		
	    ;; this is escape event with nothing after it
	    ;; put in unread-command-event and then re-read
	    (vip-set-unread-command-events event)
	    (setq keyseq
		  (funcall (ad-get-orig-definition 'read-key-sequence) nil))
	    ))
      ;; not an escape event
      (setq keyseq (vector event)))
    keyseq))

    
    
(defadvice read-key-sequence (around vip-read-key-sequence-ad activate)
  (let (inhibit-quit event keyseq)
    (setq keyseq ad-do-it)
    (setq event (if vip-xemacs-p
		    (elt keyseq 0) ; XEmacs returns vector of events
		  (elt (listify-key-sequence keyseq) 0)))
    (if (vip-ESC-event-p event)
	(let (unread-command-events unread-command-event)
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
    
(defadvice describe-key (before vip-read-key-sequence-ad protect activate)
  "Force `describe-key' to read key via `read-key-sequence'."
  (interactive (list (vip-events-to-keys
		      (read-key-sequence "Describe key: ")))))

(defadvice describe-key-briefly 
  (before vip-read-key-sequence-ad protect activate)
  "Force `describe-key-briefly' to read key via `read-key-sequence'."
  (interactive (list (vip-events-to-keys
		      (read-key-sequence "Describe key briefly: ")))))

(defun vip-intercept-ESC-key ()
  "Listen to ESC key.
If a sequence of keys starting with ESC is issued with very short delays,
interpret these keys in Emacs mode, so ESC won't be interpreted as a Vi key."
  (interactive)
  (let ((cmd (or (key-binding (vip-envelop-ESC-key)) 
		 '(lambda () (interactive) (error "")))))
    
    ;; call the actual function to execute ESC (if no other symbols followed)
    ;; or the key bound to the ESC sequence (if the sequence was issued
    ;; with very short delay between characters.
    (if (eq cmd 'vip-intercept-ESC-key)
	(setq cmd
	      (cond ((eq vip-current-state 'vi-state)
		     'vip-ESC)
		    ((eq vip-current-state 'insert-state)
		     'vip-exit-insert-state) 
		    ((eq vip-current-state 'replace-state)
		     'vip-replace-state-exit-cmd)
		    (t 'vip-change-state-to-vi)
		    )))
    (call-interactively cmd)))
	   


;; prefix argument for Vi mode

;; In Vi mode, prefix argument is a dotted pair (NUM . COM) where NUM
;; represents the numeric value of the prefix argument and COM represents
;; command prefix such as "c", "d", "m" and "y".

(defun vip-prefix-arg-value (event com)
  "Compute numeric prefix arg value. 
Invoked by CHAR. COM is the command part obtained so far." 
  (let (value)
    ;; read while number
    (while (and (numberp event) (>= event ?0) (<= event ?9))
      (setq value (+ (* (if (numberp value) value 0) 10) (- event ?0)))
      (setq event (vip-read-event-convert-to-char)))
    
    (setq prefix-arg value)
    (if com (setq prefix-arg (cons prefix-arg com)))
    (while (eq event ?U)
      (vip-describe-arg prefix-arg)
      (setq event (vip-read-event-convert-to-char)))
    (vip-set-unread-command-events event)))

(defun vip-prefix-arg-com (char value com)
  "Vi operator as prefix argument."
  (let ((cont t))
    (while (and cont
		(memq char
		      (list ?c ?d ?y ?! ?< ?> ?= ?# ?r ?R ?\"
			    vip-buffer-search-char)))
      (if com
	  ;; this means that we already have a command character, so we
	  ;; construct a com list and exit while.  however, if char is "
	  ;; it is an error.
	  (progn
	    ;; new com is (CHAR . OLDCOM)
	    (if (memq char '(?# ?\")) (error ""))
	    (setq com (cons char com))
	    (setq cont nil))
	;; If com is nil we set com as char, and read more.  Again, if char
	;; is ", we read the name of register and store it in vip-use-register.
	;; if char is !, =, or #, a complete com is formed so we exit the
	;; while loop.
	(cond ((memq char '(?! ?=))
	       (setq com char)
	       (setq char (read-char))
	       (setq cont nil))
	      ((= char ?#)
	       ;; read a char and encode it as com
	       (setq com (+ 128 (read-char)))
	       (setq char (read-char)))
	      ((= char ?\")
	       (let ((reg (read-char)))
		 (if (vip-valid-register reg)
		     (setq vip-use-register reg)
		   (error ""))
		 (setq char (read-char))))
	      (t
	       (setq com char)
	       (setq char (vip-read-char-exclusive)))))))
  (if (atom com)
      ;; com is a single char, so we construct prefix-arg 
      ;; and if char is ?, describe prefix arg, otherwise exit by
      ;; pushing the char back
      (progn
	(setq prefix-arg (cons value com))
	(while (= char ?U)
	  (vip-describe-arg prefix-arg)
	  (setq char (read-char)))
	(vip-set-unread-command-events char)
	)
    ;; as com is non-nil, this means that we have a command to execute
    (if (memq (car com) '(?r ?R))
	;; execute apropriate region command.
	(let ((char (car com)) (com (cdr com)))
	  (setq prefix-arg (cons value com))
	  (if (= char ?r) (vip-region prefix-arg)
	    (vip-Region prefix-arg))
	  ;; reset prefix-arg
	  (setq prefix-arg nil))
      ;; otherwise, reset prefix arg and call appropriate command
      (setq value (if (null value) 1 value))
      (setq prefix-arg nil)
      (cond ((equal com '(?c . ?c)) (vip-line (cons value ?C)))
	    ((equal com '(?d . ?d)) (vip-line (cons value ?D)))
	    ((equal com '(?d . ?y)) (vip-yank-defun))
	    ((equal com '(?y . ?y)) (vip-line (cons value ?Y)))
	    ((equal com '(?< . ?<)) (vip-line (cons value ?<)))
	    ((equal com '(?> . ?>)) (vip-line (cons value ?>)))
	    ((equal com '(?! . ?!)) (vip-line (cons value ?!)))
	    ((equal com '(?= . ?=)) (vip-line (cons value ?=)))
	    (t (error ""))))))

(defun vip-describe-arg (arg)
  (let (val com)
    (setq val (vip-P-val arg)
	  com (vip-getcom arg))
    (if (null val)
	(if (null com)
	    (message "Value is nil, and command is nil")
	  (message "Value is nil, and command is `%c'" com))
      (if (null com)
	  (message "Value is `%d', and command is nil" val)
	(message "Value is `%d', and command is `%c'" val com)))))

(defun vip-digit-argument (arg)
  "Begin numeric argument for the next command."
  (interactive "P")
  (vip-prefix-arg-value last-command-char
			(if (consp arg) (cdr arg) nil)))

(defun vip-command-argument (arg)
  "Accept a motion command as an argument."
  (interactive "P")
  (condition-case nil
      (vip-prefix-arg-com
       last-command-char   
       (cond ((null arg) nil)
	     ((consp arg) (car arg))
	     ((numberp arg) arg)
	     (t (error vip-InvalidCommandArgument)))
       (cond ((null arg) nil)
	     ((consp arg) (cdr arg))
	     ((numberp arg) nil)
	     (t (error vip-InvalidCommandArgument))))
    (quit (setq vip-use-register nil)
	  (signal 'quit nil)))
  (vip-deactivate-mark))

;; Get value part of prefix-argument ARG.
(defsubst vip-p-val (arg)
  (cond ((null arg) 1)
	((consp arg) (if (null (car arg)) 1 (car arg)))
	(t arg)))

;; Get raw value part of prefix-argument ARG.
(defsubst vip-P-val (arg)
  (cond ((consp arg) (car arg))
	(t arg)))

;; Get com part of prefix-argument ARG.
(defsubst vip-getcom (arg)
  (cond ((null arg) nil)
	((consp arg) (cdr arg))
	(t nil)))

;; Get com part of prefix-argument ARG and modify it.
(defun vip-getCom (arg)
  (let ((com (vip-getcom arg)))
    (cond ((equal com ?c) ?C)
	  ((equal com ?d) ?D)
	  ((equal com ?y) ?Y)
	  (t com))))


;; repeat last destructive command

;; Append region to text in register REG.
;; START and END are buffer positions indicating what to append.
(defsubst vip-append-to-register (reg start end)
  (set-register reg (concat (or (get-register reg) "")
			    (buffer-substring start end))))
			    
;; define functions to be executed

;; invoked by C command
(defun vip-exec-change (m-com com) 
  ;; handle C cmd at the eol and at eob.
  (if (or (and (eolp) (= vip-com-point (point)))
	  (= vip-com-point (point-max)))
      (progn
	(insert " ")(backward-char 1)))
  (if (= vip-com-point (point))
      (vip-forward-char-carefully))
  (if (= com ?c)
      (vip-change vip-com-point (point))
    (vip-change-subr vip-com-point (point))))

;; this is invoked by vip-substitute-line
(defun vip-exec-Change (m-com com)
  (save-excursion
    (set-mark vip-com-point)
    (vip-enlarge-region (mark t) (point))
    (if vip-use-register
	(progn
	  (cond ((vip-valid-register vip-use-register '(letter digit))
		 ;;(vip-valid-register vip-use-register '(letter)
		 (copy-to-register
		  vip-use-register (mark t) (point) nil))
		((vip-valid-register vip-use-register '(Letter))
		 (vip-append-to-register
		  (downcase vip-use-register) (mark t) (point)))
		(t (setq vip-use-register nil)
		   (error vip-InvalidRegister vip-use-register)))
	  (setq vip-use-register nil)))
    (delete-region (mark t) (point)))
  (open-line 1)
  (if (= com ?C) (vip-change-mode-to-insert) (vip-yank-last-insertion)))

(defun vip-exec-delete (m-com com)
  (if vip-use-register
      (progn
	(cond ((vip-valid-register vip-use-register '(letter digit))
	       ;;(vip-valid-register vip-use-register '(letter))
	       (copy-to-register
		vip-use-register vip-com-point (point) nil))
	      ((vip-valid-register vip-use-register '(Letter))
	       (vip-append-to-register
		(downcase vip-use-register) vip-com-point (point)))
	      (t (setq vip-use-register nil)
		 (error vip-InvalidRegister vip-use-register)))
	(setq vip-use-register nil)))
  (setq last-command
	(if (eq last-command 'd-command) 'kill-region nil))
  (kill-region vip-com-point (point))
  (setq this-command 'd-command)
  (if vip-ex-style-motion
      (if (and (eolp) (not (bolp))) (backward-char 1))))

(defun vip-exec-Delete (m-com com)
  (save-excursion
    (set-mark vip-com-point)
    (vip-enlarge-region (mark t) (point))
    (if vip-use-register
	(progn
	  (cond ((vip-valid-register vip-use-register '(letter digit))
		 ;;(vip-valid-register vip-use-register '(letter))
		 (copy-to-register
		  vip-use-register (mark t) (point) nil))
		((vip-valid-register vip-use-register '(Letter))
		 (vip-append-to-register
		  (downcase vip-use-register) (mark t) (point)))
		(t (setq vip-use-register nil)
		   (error vip-InvalidRegister vip-use-register)))
	  (setq vip-use-register nil)))
    (setq last-command
	  (if (eq last-command 'D-command) 'kill-region nil))
    (kill-region (mark t) (point))
    (if (eq m-com 'vip-line) (setq this-command 'D-command)))
  (back-to-indentation))

(defun vip-exec-yank (m-com com)
  (if vip-use-register
      (progn
	(cond ((vip-valid-register vip-use-register '(letter digit))
	       ;; (vip-valid-register vip-use-register '(letter))
	       (copy-to-register
		vip-use-register vip-com-point (point) nil))
	      ((vip-valid-register vip-use-register '(Letter))
	       (vip-append-to-register
		(downcase vip-use-register) vip-com-point (point)))
	      (t (setq vip-use-register nil)
		 (error vip-InvalidRegister vip-use-register)))
	(setq vip-use-register nil)))
  (setq last-command nil)
  (copy-region-as-kill vip-com-point (point))
  (goto-char vip-com-point))

(defun vip-exec-Yank (m-com com)
  (save-excursion
    (set-mark vip-com-point)
    (vip-enlarge-region (mark t) (point))
    (if vip-use-register
	(progn
	  (cond ((vip-valid-register vip-use-register '(letter digit))
		 ;;(vip-valid-register vip-use-register '(letter))
		 (copy-to-register
		  vip-use-register (mark t) (point) nil))
		((vip-valid-register vip-use-register '(Letter))
		 (vip-append-to-register
		  (downcase vip-use-register) (mark t) (point)))
		(t (setq vip-use-register nil)
		   (error vip-InvalidRegister  vip-use-register)))
	  (setq vip-use-register nil)))
    (setq last-command nil)
    (copy-region-as-kill (mark t) (point)))
  (goto-char vip-com-point))

(defun vip-exec-bang (m-com com)
  (save-excursion
    (set-mark vip-com-point)
    (vip-enlarge-region (mark t) (point))
    (shell-command-on-region
     (mark t) (point)
     (if (= com ?!)
	 (setq vip-last-shell-com
	       (vip-read-string-with-history 
		"!"
		nil
		'vip-shell-history
		(car vip-shell-history)
		))
       vip-last-shell-com)
     t)))

(defun vip-exec-equals (m-com com)
  (save-excursion
    (set-mark vip-com-point)
    (vip-enlarge-region (mark t) (point))
    (if (> (mark t) (point)) (exchange-point-and-mark))
    (indent-region (mark t) (point) nil)))

(defun vip-exec-shift (m-com com)
  (save-excursion
    (set-mark vip-com-point)
    (vip-enlarge-region (mark t) (point))
    (if (> (mark t) (point)) (exchange-point-and-mark))
    (indent-rigidly (mark t) (point) 
		    (if (= com ?>)
			vip-shift-width
		      (- vip-shift-width)))))

;; this is needed because some commands fake com by setting it to ?r, which
;; denotes repeated insert command.
(defsubst vip-exec-dummy (m-com com)
  nil)

(defun vip-exec-buffer-search (m-com com)
  (setq vip-s-string (buffer-substring (point) vip-com-point))
  (setq vip-s-forward t)
  (setq vip-search-history (cons vip-s-string vip-search-history))
  (vip-search vip-s-string vip-s-forward 1))

(defvar vip-exec-array (make-vector 128 nil))

;; Using a dispatch array allows adding functions like buffer search
;; without affecting other functions. Buffer search can now be bound
;; to any character.

(aset vip-exec-array ?c 'vip-exec-change)
(aset vip-exec-array ?C 'vip-exec-Change)
(aset vip-exec-array ?d 'vip-exec-delete)
(aset vip-exec-array ?D 'vip-exec-Delete)
(aset vip-exec-array ?y 'vip-exec-yank)
(aset vip-exec-array ?Y 'vip-exec-Yank)
(aset vip-exec-array ?r 'vip-exec-dummy)
(aset vip-exec-array ?! 'vip-exec-bang)
(aset vip-exec-array ?< 'vip-exec-shift)
(aset vip-exec-array ?> 'vip-exec-shift)
(aset vip-exec-array ?= 'vip-exec-equals)



;; This function is called by various movement commands to execute a
;; destructive command on the region specified by the movement command. For
;; instance, if the user types cw, then the command vip-forward-word will
;; call vip-execute-com to execute vip-exec-change, which eventually will
;; call vip-change to invoke the replace mode on the region.
;;
;; The list (M-COM VAL COM REG INSETED-TEXT COMMAND-KEYS) is set to
;; vip-d-com for later use by vip-repeat.
(defun vip-execute-com (m-com val com)
  (let ((reg vip-use-register))
    ;; this is the special command `#'
    (if (> com 128)
	(vip-special-prefix-com (- com 128))
      (let ((fn (aref vip-exec-array (if (< com 0) (- com) com))))
	(if (null fn)
	    (error "%c: %s" com vip-InvalidViCommand)
	  (funcall fn m-com com))))
    (if (vip-dotable-command-p com)
	(vip-set-destructive-command
	 (list m-com val
	       (if (memq com (list ?c ?C ?!)) (- com) com)
	       reg nil nil)))
    ))


(defun vip-repeat (arg)
  "Re-execute last destructive command.
Use the info in vip-d-com, which has the form
\(com val ch reg inserted-text command-keys\),
where `com' is the command to be re-executed, `val' is the
argument to `com', `ch' is a flag for repeat, and `reg' is optional;
if it exists, it is the name of the register for `com'.
If the prefix argument, ARG, is non-nil, it is used instead of `val'."
  (interactive "P")
  (let ((save-point (point)) ; save point before repeating prev cmd
	;; Pass along that we are repeating a destructive command
	;; This tells vip-set-destructive-command not to update
	;; vip-command-ring
	(vip-intermediate-command 'vip-repeat))
    (if (eq last-command 'vip-undo)
	;; if the last command was vip-undo, then undo-more
	(vip-undo-more)
      ;; otherwise execute the command stored in vip-d-com.  if arg is non-nil
      ;; its prefix value is used as new prefix value for the command.
      (let ((m-com (car vip-d-com))
	    (val (vip-P-val arg))
	    (com (nth 2 vip-d-com))
	    (reg (nth 3 vip-d-com)))
        (if (null val) (setq val (nth 1 vip-d-com)))
        (if (null m-com) (error "No previous command to repeat."))
        (setq vip-use-register reg)
	(if (nth 4 vip-d-com) ; text inserted by command
	    (setq vip-last-insertion (nth 4 vip-d-com)
		  vip-d-char (nth 4 vip-d-com)))
        (funcall m-com (cons val com))
        (if (and vip-keep-point-on-repeat (< save-point (point)))
	    (goto-char save-point)) ; go back to before repeat.
	(if (and (eolp) (not (bolp)))
	    (backward-char 1))
     ))
  (if vip-undo-needs-adjustment (vip-adjust-undo)) ; take care of undo
  ;; If the prev cmd was rotating the command ring, this means that `.' has
  ;; just executed a command from that ring. So, push it on the ring again.
  ;; If we are just executing previous command , then don't push vip-d-com
  ;; because vip-d-com is not fully constructed in this case (its keys and
  ;; the inserted text may be nil). Besides, in this case, the command
  ;; executed by `.' is already on the ring.
  (if (eq last-command 'vip-display-current-destructive-command)
      (vip-push-onto-ring vip-d-com 'vip-command-ring))
  (vip-deactivate-mark)
  ))
  
(defun vip-repeat-from-history ()
  "Repeat a destructive command from history.
Doesn't change vip-command-ring in any way, so `.' will work as before
executing this command.
This command is supposed to be bound to a two-character Vi macro where
the second character is a digit 0 to 9. The digit indicates which
history command to execute. `<char>0' is equivalent to `.', `<char>1'
invokes the command before that, etc."
  (interactive)
  (let* ((vip-intermediate-command 'repeating-display-destructive-command)
	 (idx (cond (vip-this-kbd-macro
		      (string-to-number
		       (symbol-name (elt vip-this-kbd-macro 1))))
		    (t 0)))
	 (num idx)
	 (vip-d-com vip-d-com))

    (or (and (numberp num) (<= 0 num) (<= num 9))
	(setq idx 0
	      num 0)
	(message
	 "`vip-repeat-from-history' must be invoked as a Vi macro bound to `<key><digit>'"))
    (while (< 0 num)
      (setq vip-d-com (vip-special-ring-rotate1 vip-command-ring -1))
      (setq num (1- num)))
    (vip-repeat nil)
    (while (> idx num)
      (vip-special-ring-rotate1 vip-command-ring 1)
      (setq num (1+ num)))
    ))
      

(defun vip-special-prefix-com (char)
  "This command is invoked interactively by the key sequence #<char>."
  (cond ((= char ?c)
	 (downcase-region (min vip-com-point (point))
			  (max vip-com-point (point))))
	((= char ?C)
	 (upcase-region (min vip-com-point (point))
			(max vip-com-point (point))))
	((= char ?g)
	 (push-mark vip-com-point t)
	 (vip-global-execute))
	((= char ?q)
	 (push-mark vip-com-point t)
	 (vip-quote-region))
	((= char ?s) (funcall vip-spell-function vip-com-point (point)))
	(t (error "#%c: %s" char vip-InvalidViCommand))))


;; undoing

(defun vip-undo ()
  "Undo previous change."
  (interactive)
  (message "undo!")
  (let ((modified (buffer-modified-p))
        (before-undo-pt (point-marker))
	(after-change-functions after-change-functions)
	undo-beg-posn undo-end-posn)
	
    ;; no need to remove this hook, since this var has scope inside a let.
    (add-hook 'after-change-functions
	      '(lambda (beg end len)
		 (setq undo-beg-posn beg
		       undo-end-posn (or end beg))))
  
    (undo-start)
    (undo-more 2)
    (setq undo-beg-posn (or undo-beg-posn before-undo-pt)
	  undo-end-posn (or undo-end-posn undo-beg-posn))
    
    (goto-char undo-beg-posn)
    (sit-for 0)
    (if (and vip-keep-point-on-undo
	     (pos-visible-in-window-p before-undo-pt))
	(progn
	  (push-mark (point-marker) t) 
	  (vip-sit-for-short 300)
	  (goto-char undo-end-posn)
	  (vip-sit-for-short 300)
	  (if (and (> (abs (- undo-beg-posn before-undo-pt)) 1)
		  (> (abs (- undo-end-posn before-undo-pt)) 1))
	      (goto-char before-undo-pt)
	    (goto-char undo-beg-posn)))
      (push-mark before-undo-pt t))
    (if (and (eolp) (not (bolp))) (backward-char 1))
    (if (not modified) (set-buffer-modified-p t)))
  (setq this-command 'vip-undo))

(defun vip-undo-more ()
  "Continue undoing previous changes."
  (message "undo more!")
  (condition-case nil
      (undo-more 1)
    (error (beep)
	   (message "No further undo information in this buffer")))
  (if (and (eolp) (not (bolp))) (backward-char 1))
  (setq this-command 'vip-undo))

;; The following two functions are used to set up undo properly.
;; In VI, unlike Emacs, if you open a line, say, and add a bunch of lines,
;; they are undone all at once.  
(defun vip-adjust-undo ()
  (let ((inhibit-quit t)
	tmp tmp2)
    (setq vip-undo-needs-adjustment nil)
    (if (listp buffer-undo-list)
	(if (setq tmp (memq vip-buffer-undo-list-mark buffer-undo-list))
	    (progn
	      (setq tmp2 (cdr tmp)) ; the part after mark
	      
	      ;; cut tail from buffer-undo-list temporarily by direct
	      ;; manipulation with pointers in buffer-undo-list
	      (setcdr tmp nil)
	      
	      (setq buffer-undo-list (delq nil buffer-undo-list))
	      (setq buffer-undo-list
		    (delq vip-buffer-undo-list-mark buffer-undo-list))
	      ;; restore tail of buffer-undo-list
	      (setq buffer-undo-list (nconc buffer-undo-list tmp2)))
	  (setq buffer-undo-list (delq nil buffer-undo-list))))))
  

(defun vip-set-complex-command-for-undo ()  
  (if (listp buffer-undo-list)
      (if (not vip-undo-needs-adjustment)
	  (let ((inhibit-quit t))
	    (setq buffer-undo-list 
		  (cons vip-buffer-undo-list-mark buffer-undo-list))
	    (setq vip-undo-needs-adjustment t)))))



      
(defun vip-display-current-destructive-command ()
  (let ((text (nth 4 vip-d-com))
	(keys (nth 5 vip-d-com))
	(max-text-len 30))
    
    (setq this-command 'vip-display-current-destructive-command)
	
    (message " `.' runs  %s%s"
	     (concat "`" (vip-array-to-string keys) "'")
	     (vip-abbreviate-string text max-text-len
				    "  inserting  `" "'" "    ......."))
    ))
    
    
;; don't change vip-d-com if it was vip-repeat command invoked with `.'
;; or in some other way (non-interactively).
(defun vip-set-destructive-command (list)
  (or (eq vip-intermediate-command 'vip-repeat)
      (progn
	(setq vip-d-com list)
	(setcar (nthcdr 5 vip-d-com)
		(vip-array-to-string (this-command-keys)))
	(vip-push-onto-ring vip-d-com 'vip-command-ring))))
    
(defun vip-prev-destructive-command (next)
  "Find previous destructive command in the history of destructive commands.
With prefix argument, find next destructive command."
  (interactive "P")
  (let (cmd vip-intermediate-command)
    (if (eq last-command 'vip-display-current-destructive-command)
	;; repeated search through command history
	(setq vip-intermediate-command 'repeating-display-destructive-command)
      ;; first search through command history--set temp ring
      (setq vip-temp-command-ring (copy-list vip-command-ring))) 
    (setq cmd (if next
		  (vip-special-ring-rotate1 vip-temp-command-ring 1)
		(vip-special-ring-rotate1 vip-temp-command-ring -1)))
    (if (null cmd)
	()
      (setq vip-d-com cmd))
    (vip-display-current-destructive-command)))
      
(defun vip-next-destructive-command ()
  "Find next destructive command in the history of destructive commands."
  (interactive)
  (vip-prev-destructive-command 'next))
  
(defun vip-insert-prev-from-insertion-ring (arg)
  "Cycles through insertion ring in the direction of older insertions.
Undoes previous insertion and inserts new.
With prefix argument, cycles in the direction of newer elements.
In minibuffer, this command executes whatever the invocation key is bound
to in the global map, instead of cycling through the insertion ring."
  (interactive "P")
  (let (vip-intermediate-command)
    (if (eq last-command 'vip-insert-from-insertion-ring)
	(progn  ; repeated search through insertion history
	  (setq vip-intermediate-command 'repeating-insertion-from-ring)
	  (if (eq vip-current-state 'replace-state)
	      (undo 1)
	    (if vip-last-inserted-string-from-insertion-ring
		(backward-delete-char
		 (length vip-last-inserted-string-from-insertion-ring))))
	  )
      ;;first search through insertion history
      (setq vip-temp-insertion-ring (copy-list vip-insertion-ring)))
    (setq this-command 'vip-insert-from-insertion-ring)
    ;; so that things will be undone properly
    (setq buffer-undo-list (cons nil buffer-undo-list))
    (setq vip-last-inserted-string-from-insertion-ring
	  (vip-special-ring-rotate1 vip-temp-insertion-ring (if arg 1 -1)))
    
    ;; this change of vip-intermediate-command must come after
    ;; vip-special-ring-rotate1, so that the ring will rotate, but before the
    ;; insertion.
    (setq vip-intermediate-command nil)
    (if vip-last-inserted-string-from-insertion-ring
	(insert vip-last-inserted-string-from-insertion-ring))
    ))

(defun vip-insert-next-from-insertion-ring ()
  "Cycles through insertion ring in the direction of older insertions. Undoes previous insertion and inserts new."
  (interactive)
  (vip-insert-prev-from-insertion-ring 'next))
    

;; some region utilities

(defun vip-add-newline-at-eob-if-necessary ()
  "If at the last line of buffer, add \\n before eob, if newline is missing."
  (save-excursion
      (end-of-line)
      ;; make sure all lines end with newline, unless in the minibuffer or
      ;; when requested otherwise (vip-add-newline-at-eob is nil)
      (if (and
	   (eobp)
	   (not (bolp))
	   vip-add-newline-at-eob
	   (not (vip-is-in-minibuffer)))
	  (insert "\n"))))

(defun vip-yank-defun ()
  (mark-defun)
  (copy-region-as-kill (point) (mark t)))

(defun vip-enlarge-region (beg end)
  "Enlarge region between BEG and END."
  (or beg (setq beg end)) ; if beg is nil, set to end
  (or end (setq end beg)) ; if end is nil, set to beg
  
  (if (< beg end)
      (progn (goto-char beg) (set-mark end))
    (goto-char end)
    (set-mark beg))
  (beginning-of-line)
  (exchange-point-and-mark)
  (if (or (not (eobp)) (not (bolp))) (forward-line 1))
  (if (not (eobp)) (beginning-of-line))
  (if (> beg end) (exchange-point-and-mark)))


(defun vip-quote-region ()
  "Quote region by each line with a user supplied string."
  (setq vip-quote-string
	(vip-read-string-with-history
	 "Quote string: "
	 nil
	 'vip-quote-region-history
	 vip-quote-string))
  (vip-enlarge-region (point) (mark t))
  (if (> (point) (mark t)) (exchange-point-and-mark))
  (insert vip-quote-string)
  (beginning-of-line)
  (forward-line 1)
  (while (and (< (point) (mark t)) (bolp))
    (insert vip-quote-string)
    (beginning-of-line)
    (forward-line 1)))
    

;;  Tells whether BEG is on the same line as END.
;;  If one of the args is nil, it'll return nil.
(defun vip-same-line (beg end)
   (let ((selective-display nil))
     (cond ((and beg end)
	    ;; This 'if' is needed because Emacs treats the next empty line
	    ;; as part of the previous line.
	    (if (or (> beg (point-max)) (> end (point-max))) ; out of range
		()
	      (if (and (> end beg) (= (vip-line-pos 'start) end))
		  (setq end (min (1+ end) (point-max))))
	      (if (and (> beg end) (= (vip-line-pos 'start) beg))
		  (setq beg (min (1+ beg) (point-max))))
	      (<= (count-lines beg end) 1) ))
	   
	   (t nil))
	 ))
	 
	 
;; Check if the string ends with a newline.
(defun vip-end-with-a-newline-p (string)
  (or (string= string "")
      (= (vip-seq-last-elt string) ?\n)))

(defun vip-tmp-insert-at-eob (msg)
  (let ((savemax (point-max)))
      (goto-char savemax)
      (insert msg)
      (sit-for 2)
      (goto-char savemax) (delete-region (point) (point-max))
      ))  
      


;;; Minibuffer business
	    
(defsubst vip-set-minibuffer-style ()
  (add-hook 'minibuffer-setup-hook 'vip-minibuffer-setup-sentinel))
  
  
(defun vip-minibuffer-setup-sentinel ()
  (let ((hook (if vip-vi-style-in-minibuffer
		  'vip-change-state-to-insert
		'vip-change-state-to-emacs)))
    (funcall hook)

    ;; Make sure the minibufer overlay is kept up-to-date. In XEmacs also
    ;; guards against the possibility of detaching this overlay.
    (add-hook 'vip-post-command-hooks 'vip-move-minibuffer-overlay)
    ))
  
;; Interpret last event in the local map
(defun vip-exit-minibuffer ()
  (interactive)
  (let (command)
    (setq command (local-key-binding (char-to-string last-command-char)))
    (if command
	(command-execute command)
      (exit-minibuffer))))
  

(defun vip-set-search-face ()
  (if (not window-system)
      ()
    (defvar vip-search-face
      (progn
	(make-face 'vip-search-face)
	(or (face-differs-from-default-p 'vip-search-face)
	    ;; face wasn't set in .vip or .Xdefaults
	    (if (vip-can-use-colors "Black" "khaki")
		(progn
		  (set-face-background 'vip-search-face "khaki")
		  (set-face-foreground 'vip-search-face "Black"))
	      (copy-face 'italic 'vip-search-face)
	      (set-face-underline-p 'vip-search-face t)))
	'vip-search-face)
        "*Face used to flash out the search pattern.")
    ))
  
  
(defun vip-set-minibuffer-faces ()
  (if (not window-system)
      ()
    (defvar vip-minibuffer-emacs-face
      (progn
	(make-face 'vip-minibuffer-emacs-face)
	(or (face-differs-from-default-p 'vip-minibuffer-emacs-face)
	    ;; face wasn't set in .vip or .Xdefaults
	    (if vip-vi-style-in-minibuffer
		;; emacs state is an exception in the minibuffer
		(if (vip-can-use-colors "darkseagreen2" "Black")
		    (progn
		      (set-face-background
		       'vip-minibuffer-emacs-face "darkseagreen2")
		      (set-face-foreground
		       'vip-minibuffer-emacs-face "Black"))
		  (copy-face 'highlight 'vip-minibuffer-emacs-face))
	      ;; emacs state is the main state in the minibuffer
	      (if (vip-can-use-colors "Black" "pink")
		  (progn
		    (set-face-background 'vip-minibuffer-emacs-face "pink") 
		    (set-face-foreground
		     'vip-minibuffer-emacs-face "Black"))
		(copy-face 'italic 'vip-minibuffer-emacs-face))
	      ))
	'vip-minibuffer-emacs-face)
      "Face used in the Minibuffer when it is in Emacs state.")
    
    (defvar vip-minibuffer-insert-face
      (progn
	(make-face 'vip-minibuffer-insert-face)
	(or (face-differs-from-default-p 'vip-minibuffer-insert-face)
	    (if vip-vi-style-in-minibuffer
		(if (vip-can-use-colors "Black" "pink")
		    (progn
		      (set-face-background 'vip-minibuffer-insert-face "pink") 
		      (set-face-foreground
		       'vip-minibuffer-insert-face "Black"))
		  (copy-face 'italic 'vip-minibuffer-insert-face))
	      ;; If Insert state is an exception
	      (if (vip-can-use-colors "darkseagreen2" "Black")
		  (progn
		    (set-face-background
		     'vip-minibuffer-insert-face "darkseagreen2")
		    (set-face-foreground
		     'vip-minibuffer-insert-face "Black"))
		(copy-face 'highlight 'vip-minibuffer-insert-face))
	      (vip-italicize-face 'vip-minibuffer-insert-face)))
	'vip-minibuffer-insert-face)
      "Face used in the Minibuffer when it is in Insert state.")
    
    (defvar vip-minibuffer-vi-face
      (progn
	(make-face 'vip-minibuffer-vi-face)
	(or (face-differs-from-default-p 'vip-minibuffer-vi-face)
	    (if vip-vi-style-in-minibuffer
		(if (vip-can-use-colors "Black" "grey")
		    (progn
		      (set-face-background 'vip-minibuffer-vi-face "grey")
		      (set-face-foreground 'vip-minibuffer-vi-face "Black"))
		  (copy-face 'bold 'vip-minibuffer-vi-face))
	      (copy-face 'bold 'vip-minibuffer-vi-face)
	      (invert-face 'vip-minibuffer-vi-face)))
	'vip-minibuffer-vi-face)
      "Face used in the Minibuffer when it is in Vi state.")
    
    ;; the current face used in the minibuffer
    (vip-deflocalvar vip-minibuffer-current-face vip-minibuffer-emacs-face "")
    ))
    
  

;;; Reading string with history  
    
(defun vip-read-string-with-history (prompt &optional initial 
					    history-var default keymap)
  ;; Reads string, prompting with PROMPT and inserting the INITIAL
  ;; value. Uses HISTORY-VAR. DEFAULT is the default value to accept if the
  ;; input is an empty string. Uses KEYMAP, if given, or the
  ;; minibuffer-local-map.
  ;; Default value is displayed until the user types something in the
  ;; minibuffer. 
  (let ((minibuffer-setup-hook 
	 '(lambda ()
	    (if (stringp initial)
		(progn
		  (sit-for 840)
		  (erase-buffer)
		  (insert initial)))
	    (vip-minibuffer-setup-sentinel)))
	(val "")
	(padding "")
	temp-msg)
    
    (setq keymap (or keymap minibuffer-local-map)
	  initial (or initial "")
	  temp-msg (if default
		       (format "(default: %s) " default)
		     ""))
		   
    (setq vip-incomplete-ex-cmd nil)
    (setq val (read-from-minibuffer prompt 
				    (concat temp-msg initial val padding)
				    keymap nil history-var))
    (setq minibuffer-setup-hook nil
	  padding (vip-array-to-string (this-command-keys))
	  temp-msg "")
    ;; the following overcomes a glaring bug in history handling
    ;; in XEmacs 19.11
    (if (not (string= val (car (eval history-var))))
	(set history-var (cons val (eval history-var))))
    (if (or (string= (nth 0 (eval history-var)) (nth 1 (eval history-var)))
	    (string= (nth 0 (eval history-var)) ""))
	(set history-var (cdr (eval history-var))))
    (if (string= val "")
	(or default "")
      val)))
  


;; insertion commands

;; Called when state changes from Insert Vi command mode.
;; Repeats the insertion command if Insert state was entered with prefix
;; argument > 1.
(defun vip-repeat-insert-command ()
  (let ((i-com (car vip-d-com))
	(val   (nth 1 vip-d-com))
	(char  (nth 2 vip-d-com)))
    (if (and val (> val 1)) ; first check that val is non-nil
	(progn        
	  (setq vip-d-com (list i-com (1- val) ?r nil nil nil))
	  (vip-repeat nil)
	  (setq vip-d-com (list i-com val char nil nil nil))
	  ))))

(defun vip-insert (arg)
  "Insert before point."
  (interactive "P")
  (vip-set-complex-command-for-undo)
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (vip-set-destructive-command (list 'vip-insert val ?r nil nil nil))
    (if com
	(vip-loop val (vip-yank-last-insertion))
      (vip-change-state-to-insert))))

(defun vip-append (arg)
  "Append after point."
  (interactive "P")
  (vip-set-complex-command-for-undo)
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (vip-set-destructive-command (list 'vip-append val ?r nil nil nil))
    (if (not (eolp)) (forward-char))
    (if (equal com ?r)
	(vip-loop val (vip-yank-last-insertion))
      (vip-change-state-to-insert))))

(defun vip-Append (arg)
  "Append at end of line."
  (interactive "P")
  (vip-set-complex-command-for-undo)
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (vip-set-destructive-command (list 'vip-Append val ?r nil nil nil))
    (end-of-line)
    (if (equal com ?r)
	(vip-loop val (vip-yank-last-insertion))
      (vip-change-state-to-insert))))

(defun vip-Insert (arg)
  "Insert before first non-white."
  (interactive "P")
  (vip-set-complex-command-for-undo)
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (vip-set-destructive-command (list 'vip-Insert val ?r nil nil nil))
    (back-to-indentation)
    (if (equal com ?r)
	(vip-loop val (vip-yank-last-insertion))
      (vip-change-state-to-insert))))

(defun vip-open-line (arg)
  "Open line below."
  (interactive "P")
  (vip-set-complex-command-for-undo)
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (vip-set-destructive-command (list 'vip-open-line val ?r nil nil nil))
    (let ((col (current-indentation)))
      (if (equal com ?r)
	  (vip-loop val
		    (progn
		      (end-of-line)
		      (newline 1)
		      (if vip-auto-indent 
			  (progn (setq vip-cted t) (indent-to col)))
		      (vip-yank-last-insertion)))
	(end-of-line)
	(newline 1)
	(if vip-auto-indent (progn (setq vip-cted t) (indent-to col)))
	(vip-change-state-to-insert)
	))))

(defun vip-Open-line (arg)
  "Open line above."
  (interactive "P")
  (vip-set-complex-command-for-undo)
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (vip-set-destructive-command (list 'vip-Open-line val ?r nil nil nil))
    (let ((col (current-indentation)))
      (if (equal com ?r)
	  (vip-loop val
		    (progn
		      (beginning-of-line)
		      (open-line 1)
		      (if vip-auto-indent 
			  (progn (setq vip-cted t) (indent-to col)))
		      (vip-yank-last-insertion)))
	(beginning-of-line)
	(open-line 1)
	(if vip-auto-indent (progn (setq vip-cted t) (indent-to col)))
	(vip-change-state-to-insert)))))

(defun vip-open-line-at-point (arg)
  "Open line at point."
  (interactive "P")
  (vip-set-complex-command-for-undo)
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (vip-set-destructive-command
     (list 'vip-open-line-at-point val ?r nil nil nil))
    (if (equal com ?r)
	(vip-loop val
		  (progn
		    (open-line 1)
		    (vip-yank-last-insertion)))
      (open-line 1)
      (vip-change-state-to-insert))))

(defun vip-substitute (arg)
  "Substitute characters."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (push-mark nil t)
    (forward-char val)
    (if (equal com ?r)
	(vip-change-subr (mark t) (point))
      (vip-change (mark t) (point)))
    (vip-set-destructive-command (list 'vip-substitute val ?r nil nil nil))
    ))

(defun vip-substitute-line (arg)
  "Substitute lines."
  (interactive "p")
  (vip-set-complex-command-for-undo)
  (vip-line (cons arg ?C)))

;; Prepare for replace
(defun vip-start-replace ()
  (setq vip-began-as-replace t
	vip-sitting-in-replace t
	vip-replace-chars-to-delete 0
	vip-replace-chars-deleted 0)
  (add-hook 'vip-after-change-functions 'vip-replace-mode-spy-after t)
  (add-hook 'vip-before-change-functions 'vip-replace-mode-spy-before t)
  ;; this will get added repeatedly, but no harm
  (add-hook 'after-change-functions 'vip-after-change-sentinel t)
  (add-hook 'before-change-functions 'vip-before-change-sentinel t)
  (vip-move-marker-locally 'vip-last-posn-in-replace-region
			   (vip-replace-start))
  (add-hook 'vip-post-command-hooks 'vip-replace-state-post-command-sentinel t)
  (add-hook 'vip-pre-command-hooks 'vip-replace-state-pre-command-sentinel t)
  )
  
;; Runs vip-after-change-functions inside after-change-functions
(defun vip-after-change-sentinel (beg end len)
  (let ((list vip-after-change-functions))
    (while list
      (funcall (car list) beg end len)
      (setq list (cdr list)))))
      
;; Runs vip-before-change-functions inside before-change-functions
(defun vip-before-change-sentinel (beg end)
  (let ((list vip-before-change-functions))
    (while list
      (funcall (car list) beg end)
      (setq list (cdr list)))))

(defun vip-post-command-sentinel ()
  (run-hooks 'vip-post-command-hooks))
  
(defun vip-pre-command-sentinel ()
  (run-hooks 'vip-pre-command-hooks))
  
;; Needed so that Viper will be able to figure the last inserted
;; chunk of text with reasonable accuracy.
(defun vip-insert-state-post-command-sentinel ()
  (if (and (memq vip-current-state '(insert-state replace-state))
	   vip-insert-point
	   (>= (point) vip-insert-point))
      (setq vip-last-posn-while-in-insert-state (point-marker)))
  (if (and (eq this-command 'dabbrev-expand)
	   (integerp vip-pre-command-point)
	   (> vip-insert-point vip-pre-command-point))
      (move-marker vip-insert-point vip-pre-command-point))
  )
  
(defun vip-insert-state-pre-command-sentinel ()
  (if (and (eq this-command 'dabbrev-expand)
	   (markerp vip-insert-point)
	   (marker-position vip-insert-point))
      (setq vip-pre-command-point (marker-position vip-insert-point))))
	
(defun vip-R-state-post-command-sentinel ()
  ;; This is needed despite vip-replace-state-pre-command-sentinel
  ;; When you jump to another buffer in another frame, the pre-command
  ;; hook won't change cursor color to default in that other frame.
  ;; So, if the second frame cursor was red and we set the point
  ;; outside the replacement region, then the cursor color woll remain
  ;; red. Restoring the default, below, prevents this.
  (vip-restore-cursor-color)
  (if (and (<= (vip-replace-start) (point))
	       (<=  (point) (vip-replace-end)))
      (vip-change-cursor-color vip-replace-overlay-cursor-color)))

(defun vip-replace-state-pre-command-sentinel ()
  (vip-restore-cursor-color))
  
(defun vip-replace-state-post-command-sentinel ()
  ;; This is needed despite vip-replace-state-pre-command-sentinel
  ;; When you jump to another buffer in another frame, the pre-command
  ;; hook won't change cursor color to default in that other frame.
  ;; So, if the second frame cursor was red and we set the point
  ;; outside the replacement region, then the cursor color woll remain
  ;; red. Restoring the default, below, prevents this.
  (vip-restore-cursor-color)
  (cond 
   ((eq vip-current-state 'replace-state)
    ;; delete characters to compensate for inserted chars.
    (let ((replace-boundary 
	   ;; distinguish empty repl-reg-end-symbol from non-empty
	   (- (vip-replace-end)
	      (if (eq (length vip-replace-region-end-symbol) 0)
		  0 1)))
	  )
	  
      (save-excursion
	(goto-char vip-last-posn-in-replace-region)
	(delete-char vip-replace-chars-to-delete)
	(setq vip-replace-chars-to-delete 0
	      vip-replace-chars-deleted 0)
	;; terminate replace mode if reached replace limit
	(if (= vip-last-posn-in-replace-region 
	       (vip-replace-end))
	    (vip-finish-change vip-last-posn-in-replace-region)))
      
      (if (and (<= (vip-replace-start) (point))
	       (<=  (point) replace-boundary))
	  (progn
	    ;; the state may have changed in vip-finish-change above
	    (if (eq vip-current-state 'replace-state)
		(vip-change-cursor-color vip-replace-overlay-cursor-color))
	    (setq vip-last-posn-in-replace-region (point-marker))))
      ))
   
   (t ;; terminate replace mode if changed Viper states.
    (vip-finish-change vip-last-posn-in-replace-region)))
  )


;; checks how many chars were deleted by the last change
(defun vip-replace-mode-spy-before (beg end)
  (setq vip-replace-chars-deleted (- end beg
				     (max 0 (- end (vip-replace-end)))
				     (max 0 (- (vip-replace-start) beg))
				     ))
  )

;; Invoked as an after-change-function to set up parameters of the last change
(defun vip-replace-mode-spy-after (beg end length)
  (if (memq vip-intermediate-command '(repeating-insertion-from-ring))
      (progn
	(setq vip-replace-chars-to-delete 0)
	(vip-move-marker-locally 
	 'vip-last-posn-in-replace-region (point)))
    
    (let (beg-col end-col real-end chars-to-delete)
      (setq real-end (min end (vip-replace-end)))
      (save-excursion
	(goto-char beg)
	(setq beg-col (current-column))
	(goto-char real-end)
	(setq end-col (current-column)))
      
      ;; If beg of change is outside the replacement region, then don't
      ;; delete anything in the repl region (set chars-to-delete to 0).
      ;;
      ;; This works fine except that we have to take special care of
      ;; dabbrev-expand.  The problem stems from new-dabbrev.el, which
      ;; sometimes simply shifts the repl region rightwards, without
      ;; deleting an equal amount of characters.
      ;;
      ;; The reason why new-dabbrev.el causes this are this:
      ;; if one dinamically completes a partial word that starts before the
      ;; replacement region (but ends inside)then new-dabbrev.el first
      ;; moves cursor backwards, to the beginning of the word to be
      ;; completed (say, pt A). Then it inserts the 
      ;; completed word and then deletes the old, incomplete part.
      ;; Since the complete word is inserted at position before the repl
      ;; region, the next If-statement would have set chars-to-delete to 0
      ;; unless we check for the current command, which must be
      ;; dabbrev-expand.
      ;;
      ;; We should be able deal with these problems in a better way
      ;; when emacs will have overlays with sticky back ends.
      ;; In fact, it would be also useful to add overlays for insert
      ;; regions as well, since this will let us capture the situation when
      ;; dabbrev-expand goes back past the insertion point to find the
      ;; beginning of the word to be expanded.
      (if (or (and (<= (vip-replace-start) beg)
		   (<= beg (vip-replace-end)))
	      (and (= length 0) (eq this-command 'dabbrev-expand)))
	  (setq chars-to-delete
		(max (- end-col beg-col) (- real-end beg) 0))
	(setq chars-to-delete 0))
      
      ;; if beg = last change position, it means that we are within the
      ;; same command that does multiple changes. Moreover, it means
      ;; that we have two subsequent changes (insert/delete) that
      ;; complement each other.
      (if (= beg (marker-position vip-last-posn-in-replace-region))
	  (setq vip-replace-chars-to-delete 
		(- (+ chars-to-delete vip-replace-chars-to-delete)
		   vip-replace-chars-deleted)) 
	(setq vip-replace-chars-to-delete chars-to-delete))
      
      (vip-move-marker-locally 
       'vip-last-posn-in-replace-region
       (max (if (> end (vip-replace-end)) (vip-replace-start) end)
	    (or (marker-position vip-last-posn-in-replace-region)
		(vip-replace-start)) 
	    ))
      
      (setq vip-replace-chars-to-delete
	    (max 0 (min vip-replace-chars-to-delete
			(- (vip-replace-end)
			   vip-last-posn-in-replace-region))))
      )))


;; Delete stuff between posn and the end of vip-replace-overlay-marker, if
;; posn is within the overlay.
(defun vip-finish-change (posn)
  (remove-hook 'vip-after-change-functions 'vip-replace-mode-spy-after)
  (remove-hook 'vip-before-change-functions 'vip-replace-mode-spy-before)
  (remove-hook 'vip-post-command-hooks
	       'vip-replace-state-post-command-sentinel) 
  (remove-hook 'vip-pre-command-hooks 'vip-replace-state-pre-command-sentinel) 
  (vip-restore-cursor-color)
  (setq vip-sitting-in-replace nil) ; just in case we'll need to know it
  (save-excursion
    (if (and 
	 vip-replace-overlay
	 (>= posn (vip-replace-start))
	 (<  posn (vip-replace-end)))
	   (delete-region posn (vip-replace-end)))
    )
  
  (if (eq vip-current-state 'replace-state)
      (vip-downgrade-to-insert))
  ;; replace mode ended => nullify vip-last-posn-in-replace-region
  (vip-move-marker-locally 'vip-last-posn-in-replace-region nil)
  (vip-hide-replace-overlay)
  (vip-refresh-mode-line)
  (vip-put-string-on-kill-ring vip-last-replace-region)
  )

(defun vip-put-string-on-kill-ring (string)
  "Make STRING be the first element of the kill ring."
  (setq kill-ring (cons string kill-ring))
  (if (> (length kill-ring) kill-ring-max)
      (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil))
  (setq kill-ring-yank-pointer kill-ring))

(defun vip-finish-R-mode ()
  (remove-hook 'vip-post-command-hooks 'vip-R-state-post-command-sentinel)
  (remove-hook 'vip-pre-command-hooks 'vip-replace-state-pre-command-sentinel)
  (vip-downgrade-to-insert))
  
(defun vip-start-R-mode ()
  ;; Leave arg as 1, not t: XEmacs insists that it must be a pos number
  (overwrite-mode 1)
  (add-hook 'vip-post-command-hooks 'vip-R-state-post-command-sentinel t)
  (add-hook 'vip-pre-command-hooks 'vip-replace-state-pre-command-sentinel t)
  )


  
(defun vip-replace-state-exit-cmd ()
  "Binding for keys that cause Replace state to switch to Vi or to Insert.
These keys are ESC, RET, and LineFeed"
  (interactive)
  (if overwrite-mode  ;; If you are in replace mode invoked via 'R'
      (vip-finish-R-mode)
    (vip-finish-change vip-last-posn-in-replace-region))
  (let (com)
    (if (eq this-command 'vip-intercept-ESC-key)
	(setq com 'vip-exit-insert-state)
      (vip-set-unread-command-events last-input-char)
      (setq com (key-binding (read-key-sequence nil))))
      
    (condition-case conds
	(command-execute com)
      (error
       (vip-message-conditions conds)))
    )
  (vip-hide-replace-overlay))

  
(defun vip-overwrite (arg) 
"This is the function bound to 'R'---unlimited replace.
Similar to Emacs's own overwrite-mode."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)) (len))
    (vip-set-destructive-command (list 'vip-overwrite val ?r nil nil nil))
    (if com
	(progn 
	  ;; Viper saves inserted text in vip-last-insertion
	  (setq len (length vip-last-insertion))
	  (delete-char len)	
	  (vip-loop val (vip-yank-last-insertion)))
      (setq last-command 'vip-overwrite)
      (vip-set-complex-command-for-undo)
      (vip-set-replace-overlay (point) (vip-line-pos 'end))
      (vip-change-state-to-replace)
      )))


;; line commands

(defun vip-line (arg)
  (let ((val (car arg))
	(com (cdr arg)))
    (vip-move-marker-locally 'vip-com-point (point))
    (if (not (eobp))
	(next-line (1- val)))
    ;; this ensures that dd, cc, D, yy will do the right thing on the last
    ;; line of buffer when this line has no \n.
    (vip-add-newline-at-eob-if-necessary)
    (vip-execute-com 'vip-line val com))
  (if (and (eobp) (not (bobp))) (forward-line -1))
  )

(defun vip-yank-line (arg)
  "Yank ARG lines (in Vi's sense)."
  (interactive "P")
  (let ((val (vip-p-val arg)))
    (vip-line (cons val ?Y))))


;; region commands

(defun vip-region (arg)
  (interactive "P")
  (let ((val (vip-P-val arg))
	(com (vip-getcom arg)))
    (vip-move-marker-locally 'vip-com-point (point))
    (exchange-point-and-mark)
    (vip-execute-com 'vip-region val com)))

(defun vip-Region (arg)
  (interactive "P")
  (let ((val (vip-P-val arg))
	(com (vip-getCom arg)))
    (vip-move-marker-locally 'vip-com-point (point))
    (exchange-point-and-mark)
    (vip-execute-com 'vip-Region val com)))

(defun vip-replace-char (arg)
  "Replace the following ARG chars by the character read."
  (interactive "P")
  (if (and (eolp) (bolp)) (error "I see no character to replace here"))
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (vip-replace-char-subr (if (equal com ?r) vip-d-char (read-char)) val)
    (if (and (eolp) (not (bolp))) (forward-char 1))
    (vip-set-destructive-command
     (list 'vip-replace-char val ?r nil vip-d-char nil))
  ))

(defun vip-replace-char-subr (char arg)
  (delete-char arg t)
  (setq vip-d-char char)
  (vip-loop (if (> arg 0) arg (- arg)) 
	    (if (eq char ?\C-m) (insert "\n") (insert char)))
  (backward-char arg))

(defun vip-replace-string ()
  "The old replace string function. 
If you supply null string as the string to be replaced,
the query replace mode will toggle between string replace and regexp replace.
This function comes from VIP 3.5 and is not used in Viper. A nostalgic user
can bind it to a key, if necessary."
  (interactive)
  (let (str)
    (setq str (vip-read-string-with-history
	       (if vip-re-replace "Replace regexp: " "Replace string: ")
	       nil  ; no initial
	       'vip-replace1-history
	       (car vip-replace1-history) ; default
	       ))
    (if (string= str "")
	(progn
	  (setq vip-re-replace (not vip-re-replace))
	  (message (format "Replace mode changed to %s"
			   (if vip-re-replace "regexp replace"
			     "string replace"))))
      (if vip-re-replace
	  (replace-regexp
	   str
	   (vip-read-string-with-history
	    (format "Replace regexp `%s' with: " str)
	    nil  ; no initial
	    'vip-replace2-history
	    (car vip-replace2-history) ; default
	    ))
	(replace-string
	 str
	 (vip-read-string-with-history
	  (format "Replace `%s' with: " str)
	  nil  ; no initial
	  'vip-replace2-history
	  (car vip-replace2-history) ; default
	  )))
      )))


;; basic cursor movement.  j, k, l, h commands.

(defun vip-forward-char (arg)
  "Move point right ARG characters (left if ARG negative).
On reaching end of line, stop and signal error."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (if vip-ex-style-motion
	(progn
	  ;; the boundary condition check gets weird here because
	  ;; forward-char may be the parameter of a delete, and 'dl' works
	  ;; just like 'x' for the last char on a line, so we have to allow
	  ;; the forward motion before the 'vip-execute-com', but, of
	  ;; course, 'dl' doesn't work on an empty line, so we have to
	  ;; catch that condition before 'vip-execute-com'
	  (if (and (eolp) (bolp)) (error "") (forward-char val))
	  (if com (vip-execute-com 'vip-forward-char val com))
	  (if (eolp) (progn (backward-char 1) (error ""))))
      (forward-char val)
      (if com (vip-execute-com 'vip-forward-char val com)))))

(defun vip-backward-char (arg)
  "Move point left ARG characters (right if ARG negative). 
On reaching beginning of line, stop and signal error."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (if vip-ex-style-motion
	(progn
	  (if (bolp) (error "") (backward-char val))
	  (if com (vip-execute-com 'vip-backward-char val com)))
      (backward-char val)
      (if com (vip-execute-com 'vip-backward-char val com)))))
      
(defun vip-forward-char-carefully (&optional arg)      
  "Like forward-char, but doesn't move at end of buffer."
  (setq arg (or arg 1))
  (if (>= (point-max) (+ (point) arg))
      (forward-char arg)
    (goto-char (point-max))))
      
(defun vip-backward-char-carefully (&optional arg)      
  "Like backward-char, but doesn't move at end of buffer."
  (setq arg (or arg 1))
  (if (<= (point-min) (- (point) arg))
      (backward-char arg)
    (goto-char (point-min))))



;;; Word command

;; Words are formed from alpha's and nonalphas - <sp>,\t\n are separators
;; for word movement. When executed with a destructive command, \n is
;; usually left untouched for the last word.

;; skip only one \n
(defun vip-skip-separators (forward)
  (if forward
      (progn
	(skip-chars-forward " \t")
	(if (looking-at "\n")
	    (progn
	      (forward-char)
	      (skip-chars-forward " \t"))))
    (skip-chars-backward " \t")
    (backward-char)
    (if (looking-at "\n")
	(skip-chars-backward " \t")
      (forward-char))))
      
(defconst vip-ALPHA            "a-zA-Z0-9_")
(defconst vip-ALPHA-B          (concat "[" vip-ALPHA "]"))
(defconst vip-NONALPHA         (concat "^" vip-ALPHA))
(defconst vip-NONALPHA-B       (concat "[" vip-NONALPHA "]"))
(defconst vip-SEP               " \t\n")
(defconst vip-SEP-B            (concat "[" vip-SEP "]"))
(defconst vip-NONSEP           (concat "^" vip-SEP))
(defconst vip-NONSEP-B         (concat "[" vip-NONSEP "]"))
(defconst vip-ALPHASEP         (concat vip-ALPHA vip-SEP))
(defconst vip-ALPHASEP-B       (concat "[" vip-ALPHASEP "]"))
(defconst vip-NONALPHASEP      (concat "^" vip-ALPHASEP ))
(defconst vip-NONALPHASEP-B    (concat "[" vip-NONALPHASEP "]"))


(defun vip-forward-word-kernel (val)
  (while (> val 0)
    (cond ((looking-at vip-ALPHA-B)
	   (skip-chars-forward vip-ALPHA)
	   (vip-skip-separators t))
	  ((looking-at vip-SEP-B)
	   (vip-skip-separators t))
	  ((looking-at vip-NONALPHASEP-B)
	   (skip-chars-forward vip-NONALPHASEP)
	   (vip-skip-separators t)))
    (setq val (1- val))))

(defun vip-fwd-skip (pat aux-pat lim)
  (if (and (save-excursion 
	     (re-search-backward pat lim t))
	   (= (point) (match-end 0)))
      (goto-char (match-beginning 0)))
  (skip-chars-backward aux-pat lim)
  (if (= (point) lim)
      (vip-forward-char-carefully))
  )

	  
(defun vip-forward-word (arg)
  "Forward word."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (vip-forward-word-kernel val)
    (if com (progn
	      (cond ((memq com (list ?c (- ?c) ?y (- ?y)))
		     (vip-fwd-skip "\n[ \t]*" " \t" vip-com-point))
		    ((vip-dotable-command-p com)
		     (vip-fwd-skip "\n[ \t]*" "" vip-com-point)))
	      (vip-execute-com 'vip-forward-word val com)))))
	  

(defun vip-forward-Word (arg)
  "Forward word delimited by white character."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (vip-loop val
	      (progn
		(skip-chars-forward vip-NONSEP)
		(vip-skip-separators t)))
    (if com (progn
	      (cond ((memq com (list ?c (- ?c) ?y (- ?y)))
		     (vip-fwd-skip "\n[ \t]*" " \t" vip-com-point))
		    ((vip-dotable-command-p com)
		     (vip-fwd-skip "\n[ \t]*" "" vip-com-point)))
	      (vip-execute-com 'vip-forward-Word val com)))))


;; this is a bit different from Vi, but Vi's end of word 
;; makes no sense whatsoever
(defun vip-end-of-word-kernel ()
  (if (vip-end-of-word-p) (forward-char))
  (if (looking-at "[ \t\n]")
      (skip-chars-forward vip-SEP))
  
  (cond ((looking-at vip-ALPHA-B) (skip-chars-forward vip-ALPHA))
	((looking-at vip-NONALPHASEP-B)
	 (skip-chars-forward vip-NONALPHASEP)))
  (vip-backward-char-carefully))

(defun vip-end-of-word-p ()
  (if (eobp) t
    (save-excursion
      (cond ((looking-at vip-ALPHA-B)
	     (forward-char)
	     (looking-at vip-NONALPHA-B))
	    ((looking-at vip-NONALPHASEP-B)
	     (forward-char)
	     (looking-at vip-ALPHASEP-B))))))

(defun vip-one-char-word-p ()
  (let ((step 2))
    (save-excursion
      (cond ((looking-at vip-ALPHA-B)
	     (if (bobp) (setq step 1) (backward-char))
	     (if (or (bobp) (looking-at vip-NONALPHA-B))
		 (progn
		   (forward-char step)
		   (looking-at vip-NONALPHA-B))
	       nil))
	    ((looking-at vip-NONALPHASEP-B)
	     (if (bobp) (setq step 1) (backward-char))
	     (if (or (bobp) (looking-at vip-ALPHASEP-B))
		 (progn
		   (forward-char step)
		   (looking-at vip-ALPHASEP-B))
	       nil))))))

(defun vip-one-char-Word-p ()
  (and (looking-at vip-NONSEP-B)
       (save-excursion
	 (if (bobp)
	     t
	   (backward-char)
	   (looking-at vip-SEP-B)))
       (save-excursion
	 (forward-char)
	 (or (eobp)
	     (looking-at vip-SEP-B)))))

(defun vip-end-of-word (arg &optional careful)
  "Move point to end of current word."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (vip-loop val (vip-end-of-word-kernel))
    (if com 
	(progn
	  (forward-char)
	  (vip-execute-com 'vip-end-of-word val com)))))

(defun vip-end-of-Word (arg)
  "Forward to end of word delimited by white character."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (vip-loop val
	(progn
	  (vip-end-of-word-kernel)
	  (if (not (re-search-forward 
		    vip-SEP-B nil t 1))
	      (goto-char (point-max)))
	  (skip-chars-backward vip-SEP)
	  (backward-char)))
    (if com 
	(progn
	  (forward-char)
	  (vip-execute-com 'vip-end-of-Word val com)))))

(defun vip-backward-word-kernel (val)
  (while (> val 0)
    (backward-char)
    (cond ((looking-at vip-ALPHA-B)
	   (skip-chars-backward vip-ALPHA))
	  ((looking-at vip-SEP-B)
	   (forward-char)
	   (vip-skip-separators nil)
	   (backward-char)
	   (cond ((looking-at vip-ALPHA-B)
		  (skip-chars-backward vip-ALPHA))
		 ((looking-at vip-NONALPHASEP-B)
		  (skip-chars-backward vip-NONALPHASEP))
		 (t (forward-char))))
	  ((looking-at vip-NONALPHASEP-B)
	   (skip-chars-backward vip-NONALPHASEP)))
    (setq val (1- val))))

(defun vip-backward-word (arg)
  "Backward word."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com
	(let (i)
	  (if (setq i (save-excursion (backward-char) (looking-at "\n")))
	      (backward-char))
	  (vip-move-marker-locally 'vip-com-point (point))
	  (if i (forward-char))))
    (vip-backward-word-kernel val)
    (if com (vip-execute-com 'vip-backward-word val com))))

(defun vip-backward-Word (arg)
  "Backward word delimited by white character."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com
	(let (i)
	  (if (setq i (save-excursion (backward-char) (looking-at "\n")))
	      (backward-char))
	  (vip-move-marker-locally 'vip-com-point (point))
	  (if i (forward-char))))
    (vip-loop val
	      (progn 
		(vip-skip-separators nil)
		(skip-chars-backward vip-NONSEP)))
    (if com (vip-execute-com 'vip-backward-Word val com))))



;; line commands

(defun vip-beginning-of-line (arg)
  "Go to beginning of line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (beginning-of-line val)
    (if com (vip-execute-com 'vip-beginning-of-line val com))))

(defun vip-bol-and-skip-white (arg)
  "Beginning of line at first non-white character."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (forward-to-indentation (1- val))
    (if com (vip-execute-com 'vip-bol-and-skip-white val com))))

(defun vip-goto-eol (arg)
  "Go to end of line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (end-of-line val)
    (if com (vip-execute-com 'vip-goto-eol val com))
    (if vip-ex-style-motion
	(if (and (eolp) (not (bolp)) 
		 ;; a fix for vip-change-to-eol
		 (not (equal vip-current-state 'insert-state)))
	    (backward-char 1)
    ))))


(defun vip-goto-col (arg)
  "Go to ARG's column."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (save-excursion
      (end-of-line)
      (if (> val (1+ (current-column))) (error "")))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (beginning-of-line)
    (forward-char (1- val))
    (if com (vip-execute-com 'vip-goto-col val com))))
    

(defun vip-next-line (arg)
  "Go to next line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (next-line val)
    (if vip-ex-style-motion
	(if (and (eolp) (not (bolp))) (backward-char 1)))
    (setq this-command 'next-line)
    (if com (vip-execute-com 'vip-next-line val com))))

(defun vip-next-line-at-bol (arg)
  "Next line at beginning of line."
  (interactive "P")
  (save-excursion
    (end-of-line)
    (if (eobp) (error "Last line in buffer")))
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (forward-line val)
    (back-to-indentation)
    (if com (vip-execute-com 'vip-next-line-at-bol val com))))

(defun vip-previous-line (arg)	 
  "Go to previous line."    	
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (previous-line val)
    (if vip-ex-style-motion
	(if (and (eolp) (not (bolp))) (backward-char 1)))
    (setq this-command 'previous-line)
    (if com (vip-execute-com 'vip-previous-line val com))))


(defun vip-previous-line-at-bol (arg)
  "Previous line at beginning of line."
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (if (bobp) (error "First line in buffer")))
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (forward-line (- val))
    (back-to-indentation)
    (if com (vip-execute-com 'vip-previous-line val com))))

(defun vip-change-to-eol (arg)
  "Change to end of line."
  (interactive "P")
  (vip-goto-eol (cons arg ?c)))

(defun vip-kill-line (arg)
  "Delete line."
  (interactive "P")
  (vip-goto-eol (cons arg ?d)))

(defun vip-erase-line (arg)
  "Erase line."
  (interactive "P")
  (vip-beginning-of-line (cons arg ?d)))


;; moving around

(defun vip-goto-line (arg)
  "Go to ARG's line.  Without ARG go to end of buffer."
  (interactive "P")
  (let ((val (vip-P-val arg))
	(com (vip-getCom arg)))
    (vip-move-marker-locally 'vip-com-point (point))
    (vip-deactivate-mark)
    (push-mark nil t)
    (if (null val)
	(goto-char (point-max))
      (goto-char (point-min))
      (forward-line (1- val)))
    (if (and (eobp) (bolp) (not (bobp))) (forward-line -1))
    (back-to-indentation)
    (if com (vip-execute-com 'vip-goto-line val com))))

(defun vip-find-char (arg char forward offset)
  "Find ARG's occurrence of CHAR on the current line. 
If FORWARD then search is forward, otherwise backward.  OFFSET is used to
adjust point after search."
  (or (char-or-string-p char) (error ""))
  (let ((arg (if forward arg (- arg)))
	(cmd (if (eq vip-intermediate-command 'vip-repeat)
		 (nth 5 vip-d-com)
	       (vip-array-to-string (this-command-keys))))
	point)
    (save-excursion
      (save-restriction
	(if (> arg 0)
	    (narrow-to-region
	     ;; forward search begins here
	     (if (eolp) (error "Command `%s':  At end of line" cmd) (point))
	     ;; forward search ends here
	     (progn (end-of-line) (point)))
	  (narrow-to-region
	   ;; backward search begins from here
	   (if (bolp)
	       (error "Command `%s':  At beginning of line" cmd) (point))
	   ;; backward search ends here
	   (progn (beginning-of-line) (point))))
	;; if arg > 0, point is forwarded before search.
	(if (> arg 0) (goto-char (1+ (point-min)))
	  (goto-char (point-max)))
	(if (let ((case-fold-search nil))
	      (search-forward (char-to-string char) nil 0 arg))
	    (setq point (point))
	  (error "Command `%s':  `%c' not found" cmd char))))
    (goto-char (+ point (if (> arg 0) (if offset -2 -1) (if offset 1 0))))))

(defun vip-find-char-forward (arg)
  "Find char on the line. 
If called interactively read the char to find from the terminal, and if
called from vip-repeat, the char last used is used.  This behaviour is
controlled by the sign of prefix numeric value."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq vip-f-char (read-char)
	      vip-f-forward t
	      vip-f-offset nil)
      ;; vip-repeat --- set vip-F-char from command-keys
      (setq vip-F-char (if (stringp (nth 5 vip-d-com))
			   (vip-seq-last-elt (nth 5 vip-d-com))
			 vip-F-char)
	    vip-f-char vip-F-char)
      (setq val (- val)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (vip-find-char val (if (> (vip-p-val arg) 0) vip-f-char vip-F-char) t nil)
    (setq val (- val))
    (if com
	(progn
	  (setq vip-F-char vip-f-char) ; set new vip-F-char
	  (forward-char)
	  (vip-execute-com 'vip-find-char-forward val com)))))

(defun vip-goto-char-forward (arg)
  "Go up to char ARG forward on line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq vip-f-char (read-char)
	      vip-f-forward t
	      vip-f-offset t)
      ;; vip-repeat --- set vip-F-char from command-keys
      (setq vip-F-char (if (stringp (nth 5 vip-d-com))
			   (vip-seq-last-elt (nth 5 vip-d-com))
			 vip-F-char)
	    vip-f-char vip-F-char)
      (setq val (- val)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (vip-find-char val (if (> (vip-p-val arg) 0) vip-f-char vip-F-char) t t)
    (setq val (- val))
    (if com
	(progn
	  (setq vip-F-char vip-f-char) ; set new vip-F-char
	  (forward-char)
	  (vip-execute-com 'vip-goto-char-forward val com)))))

(defun vip-find-char-backward (arg)
  "Find char ARG on line backward."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq vip-f-char (read-char)
	      vip-f-forward nil
	      vip-f-offset nil)
      ;; vip-repeat --- set vip-F-char from command-keys
      (setq vip-F-char (if (stringp (nth 5 vip-d-com))
			   (vip-seq-last-elt (nth 5 vip-d-com))
			 vip-F-char)
	    vip-f-char vip-F-char)
      (setq val (- val)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (vip-find-char
     val (if (> (vip-p-val arg) 0) vip-f-char vip-F-char) nil nil)
    (setq val (- val))
    (if com
	(progn
	  (setq vip-F-char vip-f-char) ; set new vip-F-char
	  (vip-execute-com 'vip-find-char-backward val com)))))

(defun vip-goto-char-backward (arg)
  "Go up to char ARG backward on line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if (> val 0)
	;; this means that the function was called interactively
	(setq vip-f-char (read-char)
	      vip-f-forward nil
	      vip-f-offset t)
      ;; vip-repeat --- set vip-F-char from command-keys
      (setq vip-F-char (if (stringp (nth 5 vip-d-com))
			   (vip-seq-last-elt (nth 5 vip-d-com))
			 vip-F-char)
	    vip-f-char vip-F-char)
      (setq val (- val)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (vip-find-char val (if (> (vip-p-val arg) 0) vip-f-char vip-F-char) nil t)
    (setq val (- val))
    (if com
	(progn
	  (setq vip-F-char vip-f-char) ; set new vip-F-char
	  (vip-execute-com 'vip-goto-char-backward val com)))))

(defun vip-repeat-find (arg)
  "Repeat previous find command."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (vip-deactivate-mark)
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (vip-find-char val vip-f-char vip-f-forward vip-f-offset)
    (if com
	(progn
	  (if vip-f-forward (forward-char))
	  (vip-execute-com 'vip-repeat-find val com)))))

(defun vip-repeat-find-opposite (arg)
  "Repeat previous find command in the opposite direction."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (vip-deactivate-mark)
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (vip-find-char val vip-f-char (not vip-f-forward) vip-f-offset)
    (if com
	(progn
	  (if vip-f-forward (forward-char))
	  (vip-execute-com 'vip-repeat-find-opposite val com)))))


;; window scrolling etc.

(defun vip-other-window (arg)
  "Switch to other window."
  (interactive "p")
  (other-window arg)
  (or (not (eq vip-current-state 'emacs-state))
      (string= (buffer-name (current-buffer)) " *Minibuf-1*")
      (vip-change-state-to-vi)))

(defun vip-window-top (arg)
  "Go to home window line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (push-mark nil t) 
    (move-to-window-line (1- val))
    (if (not com) (back-to-indentation))
    (if com (vip-execute-com 'vip-window-top val com))))

(defun vip-window-middle (arg)
  "Go to middle window line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (push-mark nil t) 
    (move-to-window-line (+ (/ (1- (window-height)) 2) (1- val)))
    (if (not com) (back-to-indentation))
    (if com (vip-execute-com 'vip-window-middle val com))))

(defun vip-window-bottom (arg)
  "Go to last window line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (push-mark nil t) 
    (move-to-window-line (- val))
    (if (not com) (back-to-indentation))
    (if com (vip-execute-com 'vip-window-bottom val com))))

(defun vip-line-to-top (arg)
  "Put current line on the home line."
  (interactive "p")
  (recenter (1- arg)))

(defun vip-line-to-middle (arg)
  "Put current line on the middle line."
  (interactive "p")
  (recenter (+ (1- arg) (/ (1- (window-height)) 2))))

(defun vip-line-to-bottom (arg)
  "Put current line on the last line."
  (interactive "p")
  (recenter (- (window-height) (1+ arg))))


;; paren match
;; must correct this to only match ( to ) etc. On the other hand
;; it is good that paren match gets confused, because that way you
;; catch _all_ imbalances. 

(defun vip-paren-match (arg)
  "Go to the matching parenthesis."
  (interactive "P")
  (let ((com (vip-getcom arg)))
    (if (numberp arg)
	(if (or (> arg 99) (< arg 1))
	    (error "Prefix must be between 1 and 99")
	  (goto-char
	   (if (> (point-max) 80000)
	       (* (/ (point-max) 100) arg)
	     (/ (* (point-max) arg) 100)))
	  (back-to-indentation))
      (let (lim)
	(if (and (eolp) (not (bolp))) (forward-char -1))
	(save-excursion
	  (end-of-line)
	  (setq lim (point)))
	(if (re-search-forward "[][(){}]" lim t) 
	    (backward-char) 
	  (error "No matching character on line")))
      (cond ((looking-at "[\(\[{]")
	     (if com (vip-move-marker-locally 'vip-com-point (point)))
	     (forward-sexp 1)
	     (if com
		 (vip-execute-com 'vip-paren-match nil com)
	       (backward-char)))
	    ((looking-at "[])}]")
	     (forward-char)
	     (if com (vip-move-marker-locally 'vip-com-point (point)))
	     (backward-sexp 1)
	     (if com (vip-execute-com 'vip-paren-match nil com)))
	    (t (error ""))))))


;; sentence ,paragraph and heading

(defun vip-forward-sentence (arg)
  "Forward sentence."
  (interactive "P")
  (push-mark nil t) 
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (forward-sentence val)
    (if com (vip-execute-com 'vip-forward-sentence nil com))))

(defun vip-backward-sentence (arg)
  "Backward sentence."
  (interactive "P")
  (push-mark nil t) 
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (backward-sentence val)
    (if com (vip-execute-com 'vip-backward-sentence nil com))))

(defun vip-forward-paragraph (arg)
  "Forward paragraph."
  (interactive "P")
  (push-mark nil t) 
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (forward-paragraph val)
    (if com (vip-execute-com 'vip-forward-paragraph nil com))))

(defun vip-backward-paragraph (arg)
  "Backward paragraph."
  (interactive "P")
  (push-mark nil t) 
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (backward-paragraph val)
    (if com (vip-execute-com 'vip-backward-paragraph nil com))))

;; should be mode-specific etc.

(defun vip-prev-heading (arg)
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (re-search-backward vip-heading-start nil t val)
    (goto-char (match-beginning 0))
    (if com (vip-execute-com 'vip-prev-heading nil com))))

(defun vip-heading-end (arg)
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (re-search-forward vip-heading-end nil t val)
    (goto-char (match-beginning 0))
    (if com (vip-execute-com 'vip-heading-end nil com))))

(defun vip-next-heading (arg)
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getCom arg)))
    (if com (vip-move-marker-locally 'vip-com-point (point)))
    (end-of-line)
    (re-search-forward vip-heading-start nil t val)
    (goto-char (match-beginning 0))
    (if com (vip-execute-com 'vip-next-heading nil com))))


;; scrolling

(setq scroll-step 1)

(defun vip-scroll (arg)
  "Scroll to next screen."
  (interactive "p")
  (if (> arg 0)
      (while (> arg 0)
	(scroll-up)
	(setq arg (1- arg)))
    (while (> 0 arg)
      (scroll-down)
      (setq arg (1+ arg)))))

(defun vip-scroll-back (arg)
  "Scroll to previous screen."
  (interactive "p")
  (vip-scroll (- arg)))

(defun vip-scroll-down (arg)
  "Pull down half screen."
  (interactive "P")
  (condition-case nil
      (if (null arg)
	  (scroll-down (/ (window-height) 2))
	(scroll-down arg))
    (error (beep 1)
	   (message "Beginning of buffer")
	   (goto-char (point-min)))))

(defun vip-scroll-down-one (arg)
  "Scroll up one line."
  (interactive "p")
  (scroll-down arg))

(defun vip-scroll-up (arg)
  "Pull up half screen."
  (interactive "P")
  (condition-case nil
      (if (null arg)
	  (scroll-up (/ (window-height) 2))
	(scroll-up arg))
    (error (beep 1)
	   (message "End of buffer")
	   (goto-char (point-max)))))

(defun vip-scroll-up-one (arg)
  "Scroll down one line."
  (interactive "p")
  (scroll-up arg))


;; searching

(defun vip-if-string (prompt)
  (let ((s (vip-read-string-with-history
	    prompt
	    nil ; no initial
	    'vip-search-history
	    (car vip-search-history))))
    (if (not (string= s ""))
	(setq vip-s-string s))))  
	
    
(defun vip-toggle-search-style (arg) 
  "Toggle the value of vip-case-fold-search/vip-re-search.
Without prefix argument, will ask which search style to toggle. With prefix
arg 1,toggles vip-case-fold-search; with arg 2 toggles vip-re-search.

Although this function is bound to \\[vip-toggle-search-style], the most
convenient way to use it is to bind `//' to the macro
`1 M-x vip-toggle-search-style' and `///' to
`2 M-x vip-toggle-search-style'. In this way, hitting `//' quickly will
toggle case-fold-search and hitting `/' three times witth toggle regexp
search. Macros are more convenient in this case because they don't affect
the Emacs binding of `/'."
  (interactive "P")
  (let (msg)
    (cond ((or (eq arg 1)
	       (and (null arg)
		    (y-or-n-p (format "Search style: '%s'. Want '%s'? "
				      (if vip-case-fold-search
					  "case-insensitive" "case-sensitive")
				      (if vip-case-fold-search
					  "case-sensitive"
					"case-insensitive")))))
	   (setq vip-case-fold-search (null vip-case-fold-search))
	   (if vip-case-fold-search
	       (setq msg "Search becomes case-insensitive")
	     (setq msg "Search becomes case-sensitive")))
	  ((or (eq arg 2)
	       (and (null arg)
		    (y-or-n-p (format "Search style: '%s'. Want '%s'? "
				      (if vip-re-search
					  "regexp-search" "vanilla-search")
				      (if vip-re-search
					  "vanilla-search"
					"regexp-search")))))
	   (setq vip-re-search (null vip-re-search))
	   (if vip-re-search
	       (setq msg "Search becomes regexp-style")
	     (setq msg "Search becomes vanilla-style")))
	  (t
	   (setq msg "Search style remains unchanged")))
    (prin1 msg t)))


(defun vip-search-forward (arg)
  "Search a string forward. 
ARG is used to find the ARG's occurrence of the string.
Null string will repeat previous search."
  (interactive "P")
  (let ((val (vip-P-val arg))
	(com (vip-getcom arg))
	(old-str vip-s-string))
    (setq vip-s-forward t)
    (vip-if-string "/")
    ;; this is not used at present, but may be used later
    (if (or (not (equal old-str vip-s-string))
	    (not (markerp vip-local-search-start-marker))
	    (not (marker-buffer vip-local-search-start-marker)))
	(setq vip-local-search-start-marker (point-marker)))
    (vip-search vip-s-string t val)
    (if com
	(progn
	  (vip-move-marker-locally 'vip-com-point (mark t))
	  (vip-execute-com 'vip-search-next val com)))))

(defun vip-search-backward (arg)
  "Search a string backward. 
ARG is used to find the ARG's occurrence of the string.
Null string will repeat previous search."
  (interactive "P")
  (let ((val (vip-P-val arg))
	(com (vip-getcom arg))
	(old-str vip-s-string))
    (setq vip-s-forward nil)
    (vip-if-string "?")
    ;; this is not used at present, but may be used later
    (if (or (not (equal old-str vip-s-string))
	    (not (markerp vip-local-search-start-marker))
	    (not (marker-buffer vip-local-search-start-marker)))
	(setq vip-local-search-start-marker (point-marker)))
    (vip-search vip-s-string nil val)
    (if com
	(progn
	  (vip-move-marker-locally 'vip-com-point (mark t))
	  (vip-execute-com 'vip-search-next val com)))))
	  

(defun vip-search (string forward arg &optional no-offset init-point)
  "Search for COUNT's occurrence of STRING.
Search is forward if FORWARD is non-nil, otherwise backward.
INIT-POINT is the position where search is to start.
Arguments: (STRING FORWARD COUNT &optional NO-OFFSET INIT-POINT LIMIT)."
  (if (not (equal string ""))
    (let ((val (vip-p-val arg))
	  (com (vip-getcom arg))
	  (null-arg (null (vip-P-val arg))) (offset (not no-offset))
	  (case-fold-search vip-case-fold-search)
	  (start-point (or init-point (point))))
      (vip-deactivate-mark)
      (if forward
	  (condition-case nil
	      (progn
	        (if offset (vip-forward-char-carefully))
	        (if vip-re-search
		    (progn
		      (re-search-forward string nil nil val)
		      (re-search-backward string))
		  (search-forward string nil nil val)
		  (search-backward string))
		(vip-flash-search-pattern)
		(if (not (equal start-point (point)))
		    (push-mark start-point t))) 
	    (search-failed
	     (if (and null-arg vip-search-wrap-around-t)
	         (progn
		   (message "Search wrapped around end of buffer")
		   (goto-char (point-min))
		   (vip-search string forward (cons 1 com) t start-point)
		   ;; delete the wrapped around message
		   (sit-for 2)(message "")
		   )
	       (goto-char start-point)
	       (error "`%s': %s not found"
		      string
		      (if vip-re-search "Pattern" "String"))
	       )))
	;; backward
        (condition-case nil
	    (progn
	      (if vip-re-search
		  (re-search-backward string nil nil val)
	        (search-backward string nil nil val))
	      (vip-flash-search-pattern)
	      (if (not (equal start-point (point)))
		  (push-mark start-point t))) 
	  (search-failed
	   (if (and null-arg vip-search-wrap-around-t)
	       (progn
		 (message "Search wrapped around beginning of buffer")
	         (goto-char (point-max))
	         (vip-search string forward (cons 1 com) t start-point)
		 ;; delete the wrapped around message
		 (sit-for 2)(message "")
		 )
	     (goto-char start-point)
	     (error "`%s': %s not found"
		    string
		    (if vip-re-search "Pattern" "String"))
	     )))))))

(defun vip-search-next (arg)
  "Repeat previous search."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if (null vip-s-string) (error vip-NoPrevSearch))
    (vip-search vip-s-string vip-s-forward arg)
    (if com
	(progn
	  (vip-move-marker-locally 'vip-com-point (mark t))
	  (vip-execute-com 'vip-search-next val com)))))

(defun vip-search-Next (arg)
  "Repeat previous search in the reverse direction."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(com (vip-getcom arg)))
    (if (null vip-s-string) (error vip-NoPrevSearch))
    (vip-search vip-s-string (not vip-s-forward) arg)
    (if com
	(progn
	  (vip-move-marker-locally 'vip-com-point (mark t))
	  (vip-execute-com 'vip-search-Next val com)))))


;; Search contents of buffer defined by one of Viper's motion commands.
;; Repeatable via `n' and `N'.
(defun vip-buffer-search-enable (&optional c)
  (cond (c (setq vip-buffer-search-char c))
	((null vip-buffer-search-char)
	 (setq vip-buffer-search-char ?g)))
  (define-key vip-vi-basic-map
    (char-to-string vip-buffer-search-char) 'vip-command-argument)
  (aset vip-exec-array vip-buffer-search-char 'vip-exec-buffer-search)
  (setq vip-prefix-commands (cons vip-buffer-search-char vip-prefix-commands)))

(defun vip-isearch-forward (arg)
  "This is a Viper wrap-around for isearch-forward."
  (interactive "P")
  ;; emacs bug workaround
  (if (listp arg) (setq arg (car arg)))
  (vip-exec-form-in-emacs (list 'isearch-forward arg)))

(defun vip-isearch-backward (arg)
  "This is a Viper wrap-around for isearch-backward."
  (interactive "P")
  ;; emacs bug workaround
  (if (listp arg) (setq arg (car arg)))
  (vip-exec-form-in-emacs (list 'isearch-backward arg)))


;; visiting and killing files, buffers

(defun vip-switch-to-buffer ()
  "Switch to buffer in the current window."
  (interactive)
  (let (buffer)
    (setq buffer
	  (read-buffer
	   (format "Switch to buffer in this window \(%s\): "
		   (buffer-name (other-buffer (current-buffer))))))
    (switch-to-buffer buffer)
    ))

(defun vip-switch-to-buffer-other-window ()
  "Switch to buffer in another window."
  (interactive)
  (let (buffer)
    (setq buffer
	  (read-buffer
	   (format "Switch to buffer in another window \(%s\): "
		   (buffer-name (other-buffer (current-buffer))))))
    (switch-to-buffer-other-window buffer)
    ))

(defun vip-kill-buffer ()
  "Kill a buffer."
  (interactive)
  (let (buffer buffer-name)
    (setq buffer-name
	  (read-buffer
	   (format "Kill buffer \(%s\): "
		   (buffer-name (current-buffer)))))
    (setq buffer
	  (if (null buffer-name)
	      (current-buffer)
	    (get-buffer buffer-name)))
    (if (null buffer) (error "`%s': No such buffer" buffer-name))
    (if (or (not (buffer-modified-p buffer))
	    (y-or-n-p 
	     (format
	      "Buffer `%s' is modified, are you sure you want to kill it? "
	      buffer-name)))
	(kill-buffer buffer)
      (error "Buffer not killed"))))


(defvar vip-smart-suffix-list '("" "tex" "c" "cc" "el" "p")
  "*List of suffixes that Viper automatically tries to append to filenames ending with a `.'.
This is useful when you the current directory contains files with the same
prefix and many different suffixes. Usually, only one of the suffixes
represents an editable file. However, file completion will stop at the `.'
The smart suffix feature lets you hit RET in such a case, and Viper will
select the appropriate suffix.

Suffixes are tried in the order given and the first suffix for which a
corresponding file exists is selected. If no file exists for any of the
suffixes, the user is asked to confirm.

To turn this feature off, set this variable to nil.")
    
;; Try to add suffix to files ending with a `.'
;; Useful when the user hits RET on a non-completed file name.
(defun vip-file-add-suffix ()
  (let ((count 0)
	(len (length vip-smart-suffix-list))
	(file (buffer-string))
	found key cmd suff)
    (goto-char (point-max))
    (if (and vip-smart-suffix-list (string-match "\\.$" file))
	(progn
	  (while (and (not found) (< count len))
	    (setq suff (nth count vip-smart-suffix-list)
		  count (1+ count))
	    (if (file-exists-p (format "%s%s" file suff))
		(progn
		  (setq found t)
		  (insert suff))))
      
	  (if found
	      ()
	    (vip-tmp-insert-at-eob " [Please complete file name]")
	    (unwind-protect 
		(while (not (memq cmd '(exit-minibuffer vip-exit-minibuffer)))
		  (setq cmd
			(key-binding (setq key (read-key-sequence nil))))
		  (cond ((eq cmd 'self-insert-command)
			 (if vip-xemacs-p
			     (insert (events-to-keys key))
			   (insert key)))
			((memq cmd '(exit-minibuffer vip-exit-minibuffer))
			 nil)
			(t (command-execute cmd)))
		  )))
	      ))
    ))


;; Advice for use in find-file and read-file-name commands.
(defadvice exit-minibuffer (before vip-exit-minibuffer-advice activate)
  "Runs vip-minibuffer-exit-hook just before exiting the minibuffer.
Beginning with Emacs 19.26, the standard `minibuffer-exit-hook' is run
*after* exiting the minibuffer."
  (run-hooks 'vip-minibuffer-exit-hook))

(defadvice find-file (before vip-add-suffix-advice activate)
  "Uses read-file-name to read arguments."
  (interactive (list (read-file-name "Find file: "
				     nil default-directory))))
    
(defadvice find-file-other-window (before vip-add-suffix-advice activate)
  "Uses read-file-name to read arguments."
  (interactive (list (read-file-name "Find file in other window: "
				     nil default-directory))))
    
;; find-file-other-screen doesn't need advice because it apparently uses
;; read-file-name to read its argument.
(defadvice find-file-other-frame (before vip-add-suffix-advice activate)
  "Uses read-file-name to read arguments."
  (interactive (list (read-file-name "Find file in other frame: "
				     nil default-directory))))

(defadvice read-file-name (around vip-suffix-advice activate)
  "Makes exit-minibuffer run `vip-file-add-suffix' as a hook."
  (let ((vip-minibuffer-exit-hook 'vip-file-add-suffix))
    ad-do-it))

;; must be after we did advice or else the advice won't take hold
(if vip-xemacs-p
    (fset 'vip-find-file-other-frame
	  (symbol-function 'find-file-other-screen))
  (fset 'vip-find-file-other-frame
	(symbol-function 'find-file-other-frame)))

     

;; yank and pop

(defsubst vip-yank (text)
  "Yank TEXT silently. This works correctly with Emacs's yank-pop command."
    (insert text)
    (setq this-command 'yank))

(defun vip-put-back (arg)
  "Put back after point/below line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(text (if vip-use-register
		  (cond ((vip-valid-register vip-use-register '(digit))
			 (current-kill (- vip-use-register ?1) 'do-not-rotate))
			((vip-valid-register vip-use-register)
			 (get-register (downcase vip-use-register)))
			(t (error vip-InvalidRegister vip-use-register)))
		(current-kill 0))))
    (if (null text)
	(if vip-use-register
	    (let ((reg vip-use-register))
	      (setq vip-use-register nil)
	      (error vip-EmptyRegister reg))
	  (error "")))
    (setq vip-use-register nil)
    (if (vip-end-with-a-newline-p text)
	(progn
	  (if (eobp)
	      (insert "\n")
	    (forward-line 1))
	  (beginning-of-line))
      (if (not (eolp)) (vip-forward-char-carefully)))
    (set-marker (vip-mark-marker) (point) (current-buffer))
    (vip-set-destructive-command
     (list 'vip-put-back val nil vip-use-register nil nil))
    (vip-loop val (vip-yank text)))
  (exchange-point-and-mark)
  (vip-deactivate-mark))

(defun vip-Put-back (arg)
  "Put back at point/above line."
  (interactive "P")
  (let ((val (vip-p-val arg))
	(text (if vip-use-register
		  (cond ((vip-valid-register vip-use-register '(digit))
			 (current-kill (- vip-use-register ?1) 'do-not-rotate))
			((vip-valid-register vip-use-register)
			 (get-register (downcase vip-use-register)))
			(t (error vip-InvalidRegister vip-use-register)))
		(current-kill 0))))
    (if (null text)
	(if vip-use-register
	    (let ((reg vip-use-register))
	      (setq vip-use-register nil)
	      (error vip-EmptyRegister reg))
	  (error "")))
    (setq vip-use-register nil)
    (if (vip-end-with-a-newline-p text) (beginning-of-line))
    (vip-set-destructive-command
     (list 'vip-Put-back val nil vip-use-register nil nil))
    (set-marker (vip-mark-marker) (point) (current-buffer))
    (vip-loop val (vip-yank text)))
  (exchange-point-and-mark)
  (vip-deactivate-mark))
    

(defun vip-copy-region-as-kill (beg end)
  "Copy region to kill-ring.
If BEG and END do not belong to the same buffer, copy empty region."
  (condition-case nil
      (copy-region-as-kill beg end)
    (error (copy-region-as-kill beg beg))))
    
(defun vip-save-last-insertion (beg end)
  "Saves last inserted text for possible use by vip-repeat command."
  (setq vip-last-insertion (buffer-substring beg end))
  (or (< (length vip-d-com) 5)
      (setcar (nthcdr 4 vip-d-com) vip-last-insertion))
  (or (null vip-command-ring)
      (ring-empty-p vip-command-ring)
      (progn
	(setcar (nthcdr 4 (vip-current-ring-item vip-command-ring))
		vip-last-insertion)
	;; del most recent elt, if identical to the second most-recent
	(vip-cleanup-ring vip-command-ring)))
  )
    
(defsubst vip-yank-last-insertion ()
  "Inserts the text saved by the previous vip-save-last-insertion command."
  (condition-case nil
      (insert vip-last-insertion)
    (error nil)))
  

(defun vip-delete-char (arg)
  "Delete character."
  (interactive "P")
  (let ((val (vip-p-val arg)))
    (vip-set-destructive-command (list 'vip-delete-char val nil nil nil nil))
    (if (> val 1)
	(save-excursion
	  (let ((here (point)))
	    (end-of-line)
	    (if (> val (- (point) here))
		(setq val (- (point) here))))))
    (if (and (eq val 0) (not vip-ex-style-motion)) (setq val 1))
    (if (and vip-ex-style-motion (eolp))
	(if (bolp) (error "") (setq val 0))) ; not bol---simply back 1 ch
    (if vip-use-register
	(progn
	  (cond ((vip-valid-register vip-use-register '((Letter)))
		 (vip-append-to-register
		  (downcase vip-use-register) (point) (- (point) val)))
		((vip-valid-register vip-use-register)
		 (copy-to-register
		  vip-use-register (point) (- (point) val) nil))
		(t (error vip-InvalidRegister vip-use-register)))
	  (setq vip-use-register nil)))
    (if vip-ex-style-motion
	(progn
	  (delete-char val t)
	  (if (and (eolp) (not (bolp))) (backward-char 1)))
      (if (eolp)
          (delete-backward-char val t)
        (delete-char val t)))))

(defun vip-delete-backward-char (arg)
  "Delete previous character. On reaching beginning of line, stop and beep."
  (interactive "P")
  (let ((val (vip-p-val arg)))
    (vip-set-destructive-command
     (list 'vip-delete-backward-char val nil nil nil nil))
    (if (> val 1)
	(save-excursion
	  (let ((here (point)))
	    (beginning-of-line)
	    (if (> val (- here (point)))
		(setq val (- here (point)))))))
    (if vip-use-register
	(progn
	  (cond ((vip-valid-register vip-use-register '(Letter))
		 (vip-append-to-register
		  (downcase vip-use-register) (point) (+ (point) val)))
		((vip-valid-register vip-use-register)
		 (copy-to-register
		  vip-use-register (point) (+ (point) val) nil))
		(t (error vip-InvalidRegister vip-use-register)))
	  (setq vip-use-register nil)))
    (if (bolp) (ding)
      (delete-backward-char val t))))
      
(defun vip-del-backward-char-in-insert ()
  "Delete 1 char backwards while in insert mode."
  (interactive)      
  (if (and vip-ex-style-editing-in-insert (bolp))
      (beep 1)
    (delete-backward-char 1 t)))
      
(defun vip-del-backward-char-in-replace ()
  "Delete one character in replace mode.
If `vip-delete-backwards-in-replace' is t, then DEL key actually deletes
charecters. If it is nil, then the cursor just moves backwards, similarly
to Vi. The variable `vip-ex-style-editing-in-insert', if t, doesn't let the
cursor move past the beginning of the replacement region."
  (interactive)
  (cond (vip-delete-backwards-in-replace
	 (cond ((not (bolp))
		(delete-backward-char 1 t))
	       (vip-ex-style-editing-in-insert
		(beep 1))
	       ((bobp)
		(beep 1))
	       (t
		(delete-backward-char 1 t))))
	(vip-ex-style-editing-in-insert
	 (if (bolp)
	     (beep 1)
	   (backward-char 1)))
	(t 
	 (backward-char 1))))



;; join lines.

(defun vip-join-lines (arg)
  "Join this line to next, if ARG is nil.  Otherwise, join ARG lines."
  (interactive "*P")
  (let ((val (vip-P-val arg)))
    (vip-set-destructive-command (list 'vip-join-lines val nil nil nil nil))
    (vip-loop (if (null val) 1 (1- val))
	      (progn
		(end-of-line)
		(if (not (eobp))
		    (progn
		      (forward-line 1)
		      (delete-region (point) (1- (point)))
		      (fixup-whitespace)))))))


;; Replace state

(defun vip-change (beg end)
  (if (markerp beg) (setq beg (marker-position beg)))
  (if (markerp end) (setq end (marker-position end)))
  ;; beg is sometimes (mark t), which may be nil
  (or beg (setq beg end))
  
  (vip-set-complex-command-for-undo)
  (if vip-use-register
      (progn
	(copy-to-register vip-use-register beg end nil)
	(setq vip-use-register nil)))
  (vip-set-replace-overlay beg end)
  (setq last-command nil) ; separate repl text from prev kills
  
  (if (= (vip-replace-start) (point-max))
      (error "End of buffer"))
      
  (setq vip-last-replace-region
	(buffer-substring (vip-replace-start)
			  (vip-replace-end)))
  
  ;; protect against error while inserting "@" and other disasters
  ;; (e.g., read-only buff)
  (condition-case conds
      (if (vip-same-line (vip-replace-start)
			 (vip-replace-end))
	  (let ((delim-end (if (= (length vip-replace-region-end-symbol) 0)
			       ""
			     (substring vip-replace-region-end-symbol 0 1))))
	    
	    ;; tabs cause problems in replace, so untabify
	    (goto-char (vip-replace-end))
	    (insert-before-markers "@") ; put placeholder after the TAB
	   
	    (untabify (vip-replace-start) (point))
	    ;; del @ and the char under the '$'; don't put on kill ring 
	    (delete-backward-char (1+ (length delim-end)))
	    (insert delim-end)
	    ;; this move takes care of the last posn in the overlay, which
	    ;; has to be shifted because of insert. We can't simply insert
	    ;; "$" before-markers because then overlay-start will shift the
	    ;; beginning of the overlay in case we are replacing a single
	    ;; character. This fixes the bug with `s' and `cl' commands.
	    (vip-move-replace-overlay (vip-replace-start) (point))
	    (goto-char (vip-replace-start))
	    (vip-change-state-to-replace t))
	(kill-region (vip-replace-start)
		     (vip-replace-end))
	(vip-change-state-to-insert))
    (error ;; make sure that the overlay doesn't stay.
           ;; go back to the original point
     (goto-char (vip-replace-start))
     (vip-hide-replace-overlay)
     (vip-message-conditions conds))))


(defun vip-change-subr (beg end)
  ;; beg is sometimes (mark t), which may be nil
  (or beg (setq beg end))
  
  (if vip-use-register
      (progn
	(copy-to-register vip-use-register beg end nil)
	(setq vip-use-register nil)))
  (kill-region beg end)
  (setq this-command 'vip-change)
  (vip-yank-last-insertion))

(defun vip-toggle-case (arg)
  "Toggle character case."
  (interactive "P")
  (let ((val (vip-p-val arg)) (c))
    (vip-set-destructive-command (list 'vip-toggle-case val nil nil nil nil))
    (while (> val 0)
      (setq c (following-char))
      (delete-char 1 nil)
      (if (eq c (upcase c))
	  (insert-char (downcase c) 1)
	(insert-char (upcase c) 1))
      (setq val (1- val)))))


;; query replace

(defun vip-query-replace ()
  "Query replace. 
If a null string is suplied as the string to be replaced,
the query replace mode will toggle between string replace
and regexp replace."
  (interactive)
  (let (str)
    (setq str (vip-read-string-with-history
	       (if vip-re-query-replace "Query replace regexp: "
		 "Query replace: ")
	       nil  ; no initial
	       'vip-replace1-history
	       (car vip-replace1-history) ; default
	       ))
    (if (string= str "")
	(progn
	  (setq vip-re-query-replace (not vip-re-query-replace))
	  (message "Query replace mode changed to %s"
		   (if vip-re-query-replace "regexp replace"
		     "string replace")))
      (if vip-re-query-replace
	  (query-replace-regexp
	   str
	   (vip-read-string-with-history
	    (format "Query replace regexp `%s' with: " str)
	    nil  ; no initial
	    'vip-replace1-history
	    (car vip-replace1-history) ; default
	    ))
	(query-replace
	 str
	 (vip-read-string-with-history
	  (format "Query replace `%s' with: " str)
	  nil  ; no initial
	  'vip-replace1-history
	  (car vip-replace1-history) ; default
	  ))))))


;; marking

(defun vip-mark-beginning-of-buffer ()
  (interactive)
  (push-mark (point))
  (goto-char (point-min))
  (exchange-point-and-mark)
  (message "Mark set at the beginning of buffer"))

(defun vip-mark-end-of-buffer ()
  (interactive)
  (push-mark (point))
  (goto-char (point-max))
  (exchange-point-and-mark)
  (message "Mark set at the end of buffer"))

(defun vip-mark-point ()
  (interactive)
  (let ((char (vip-read-char-exclusive)))
  (cond ((and (<= ?a char) (<= char ?z))
	 (point-to-register (1+ (- char ?a))))
	((= char ?<) (vip-mark-beginning-of-buffer))
	((= char ?>) (vip-mark-end-of-buffer))
	((= char ?.) (vip-set-mark-if-necessary))
	((= char ?,) (vip-cycle-through-mark-ring))
	((= char ?D) (mark-defun))
	(t (error ""))
	)))
	
;; Algorithm: If first invocation of this command save mark on ring, goto
;; mark, M0, and pop the most recent elt from the mark ring into mark,
;; making it into the new mark, M1.
;; Push this mark back and set mark to the original point position, p1.
;; So, if you hit '' or `` then you can return to p1.
;;
;; If repeated command, pop top elt from the ring into mark and
;; jump there. This forgets the position, p1, and puts M1 back into mark.
;; Then we save the current pos, which is M0, jump to M1 and pop M2 from
;; the ring into mark.  Push M2 back on the ring and set mark to M0.
;; etc.
(defun vip-cycle-through-mark-ring ()
  "Visit previous locations on the mark ring.
One can use `` and '' to temporarily jump 1 step back."
  (let* ((sv-pt (point)))
       ;; if repeated `m,' command, pop the previously saved mark.
       ;; Prev saved mark is actually prev saved point. It is used if the
       ;; user types `` or '' and is discarded 
       ;; from the mark ring by the next `m,' command. 
       ;; In any case, go to the previous or previously saved mark.
       ;; Then push the current mark (popped off the ring) and set current
       ;; point to be the mark. Current pt as mark is discarded by the next
       ;; m, command.
       (if (eq last-command 'vip-cycle-through-mark-ring)
	   ()
	 ;; save current mark if the first iteration
	 (setq mark-ring (delete (vip-mark-marker) mark-ring))
	 (if (mark t)
	     (push-mark (mark t) t)) )
       (pop-mark)
       (set-mark-command 1)
       ;; don't duplicate mark on the ring
       (setq mark-ring (delete (vip-mark-marker) mark-ring))
       (push-mark sv-pt t)
       (vip-deactivate-mark)
       (setq this-command 'vip-cycle-through-mark-ring)
       ))
       

(defun vip-goto-mark (arg)
  "Go to mark."
  (interactive "P")
  (let ((char (read-char))
	(com (vip-getcom arg)))
    (vip-goto-mark-subr char com nil)))

(defun vip-goto-mark-and-skip-white (arg)
  "Go to mark and skip to first non-white character on line."
  (interactive "P")
  (let ((char (read-char))
	(com (vip-getCom arg)))
    (vip-goto-mark-subr char com t)))

(defun vip-goto-mark-subr (char com skip-white)
  (if (eobp) 
      (if (bobp)
	  (error "Empty buffer")
	(backward-char 1)))
  (cond ((vip-valid-register char '(letter))
	 (let* ((buff (current-buffer))
	        (reg (1+ (- char ?a)))
	        (text-marker (get-register reg)))
	   (if com (vip-move-marker-locally 'vip-com-point (point)))
	   (if (not (vip-valid-marker text-marker))
	      (error (format vip-EmptyTextmarker char)))
	   (if (and (vip-same-line (point) vip-last-jump)
		    (= (point) vip-last-jump-ignore))
	       (push-mark vip-last-jump t) 
	     (push-mark nil t)) ; no msg
	   (vip-register-to-point reg)
	   (setq vip-last-jump (point-marker))
	   (cond (skip-white 
		  (back-to-indentation)
		  (setq vip-last-jump-ignore (point))))
	   (if com
	       (if (equal buff (current-buffer))
		   (vip-execute-com (if skip-white
					'vip-goto-mark-and-skip-white
				      'vip-goto-mark)
				    nil com)
		 (switch-to-buffer buff)
		 (goto-char vip-com-point)
		 (vip-change-state-to-vi)
		 (error "")))))
	((and (not skip-white) (= char ?`))
	 (if com (vip-move-marker-locally 'vip-com-point (point)))
	 (if (and (vip-same-line (point) vip-last-jump)
		  (= (point) vip-last-jump-ignore))
	     (goto-char vip-last-jump))
	 (if (= (point) (mark t)) (pop-mark))
	 (exchange-point-and-mark)
	 (setq vip-last-jump (point-marker)
	       vip-last-jump-ignore 0)
	 (if com (vip-execute-com 'vip-goto-mark nil com)))
	((and skip-white (= char ?'))
	 (if com (vip-move-marker-locally 'vip-com-point (point)))
	 (if (and (vip-same-line (point) vip-last-jump)
		  (= (point) vip-last-jump-ignore))
	     (goto-char vip-last-jump))
	 (if (= (point) (mark t)) (pop-mark))
	 (exchange-point-and-mark)
	 (setq vip-last-jump (point))
	 (back-to-indentation)
	 (setq vip-last-jump-ignore (point))
	 (if com (vip-execute-com 'vip-goto-mark-and-skip-white nil com)))
	(t (error vip-InvalidTextmarker char))))
	
(defun vip-insert-tab ()
  (interactive)
  (insert-tab))

(defun vip-exchange-point-and-mark ()
  (interactive)
  (exchange-point-and-mark)
  (back-to-indentation))

;; Input Mode Indentation

(defun vip-forward-indent ()
  "Indent forward -- `C-t' in Vi."
  (interactive)
  (setq vip-cted t)
  (indent-to (+ (current-column) vip-shift-width)))

(defun vip-backward-indent ()
  "Backtab, C-d in VI"
  (interactive)
  (if vip-cted
      (let ((p (point)) (c (current-column)) bol (indent t))
	(if (vip-looking-back "[0^]")
	    (progn
	      (if (= ?^ (preceding-char)) (setq vip-preserve-indent t))
	      (delete-backward-char 1)
	      (setq p (point))
	      (setq indent nil)))
	(save-excursion
	  (beginning-of-line)
	  (setq bol (point)))
	(if (re-search-backward "[^ \t]" bol 1) (forward-char))
	(delete-region (point) p)
	(if indent
	    (indent-to (- c vip-shift-width)))
	(if (or (bolp) (vip-looking-back "[^ \t]"))
	    (setq vip-cted nil)))))

(defun vip-autoindent ()
  "Auto Indentation, Vi-style."
  (interactive)
  (let ((col (current-indentation)))
    (if (not vip-preserve-indent)
	(setq vip-current-indent col)
      (setq vip-preserve-indent nil))
    (newline 1)
    (if vip-auto-indent
	(progn
	  (setq vip-cted t)
	  (indent-to vip-current-indent)))))

	   
;; Viewing registers

(defun vip-ket-function (arg)
  "Function called by \], the ket. View registers and call \]\]."
  (interactive "P")
  (let ((reg (read-char)))
    (cond ((vip-valid-register reg '(letter Letter))
	   (view-register (downcase reg)))
	  ((vip-valid-register reg '(digit))
	   (let ((text (current-kill (- reg ?1) 'do-not-rotate)))
	     (save-excursion 
	       (set-buffer (get-buffer-create "*Output*"))
	       (delete-region (point-min) (point-max))
	       (insert (format "Register %c contains the string:\n" reg))
	       (insert text)
	       (goto-char (point-min)))
	     (display-buffer "*Output*")))
	  ((= ?\] reg)
	   (vip-next-heading arg))
	  (t (error
	      vip-InvalidRegister reg)))))

(defun vip-brac-function (arg)
  "Function called by \[, the brac. View textmarkers and call \[\["
  (interactive "P")
  (let ((reg (read-char)))
    (cond ((= ?\[ reg)
	   (vip-prev-heading arg))
	  ((= ?\] reg)
	   (vip-heading-end arg))
	  ((vip-valid-register reg '(letter))
	   (let* ((val (get-register (1+ (- reg ?a))))
		  (buf (if (not val) 
			   (error 
			    (format vip-EmptyTextmarker reg))
			 (marker-buffer val)))
		  (pos (marker-position val))
		  line-no text (s pos) (e pos))
	     (save-excursion 
	       (set-buffer (get-buffer-create "*Output*"))
	       (delete-region (point-min) (point-max))
	       (if (and buf pos)
		   (progn
		     (save-excursion 
		       (set-buffer buf)
		       (setq line-no (1+ (count-lines (point-min) val)))
		       (goto-char pos)
		       (beginning-of-line)
		       (if (re-search-backward "[^ \t]" nil t)
			   (progn
			     (beginning-of-line)
			     (setq s (point))))
		       (goto-char pos)
		       (forward-line 1)
		       (if (re-search-forward "[^ \t]" nil t)
			   (progn
			     (end-of-line)
			     (setq e (point))))
		       (setq text (buffer-substring s e))
		       (setq text (format "%s<%c>%s" 
					  (substring text 0 (- pos s)) 
					  reg (substring text (- pos s)))))
		     (insert
		      (format
		       "Textmarker `%c' is in buffer `%s' at line %d.\n"
				     reg (buffer-name buf) line-no))
		     (insert (format "Here is some text around %c:\n\n %s" 
				     reg text)))
		 (insert (format vip-EmptyTextmarker reg)))
	       (goto-char (point-min)))
	     (display-buffer "*Output*")))
	  (t (error vip-InvalidTextmarker reg)))))
  


;; commands in insertion mode

(defun vip-delete-backward-word (arg)
  "Delete previous word."
  (interactive "p")
  (save-excursion
    (push-mark nil t)
    (backward-word arg)
    (delete-region (point) (mark t))
    (pop-mark)))


(defun vip-set-expert-level (&optional dont-change-unless)
  "Sets the expert level for a Viper user.
Can be called interactively to change (temporarily or permanently) the
current expert level.

The optional argument DONT-CHANGE-UNLESS if not nil, says that
the level should not be changed, unless its current value is
meaningless (i.e., not one of 1,2,3,4,5).

User level determines the setting of Viper variables that are most
sensitive for VI-style look-and-feel."
  
  (interactive)
  
  (if (not (numberp vip-expert-level)) (setq vip-expert-level 0))
  
  (save-window-excursion
    (delete-other-windows)
    ;; if 0 < vip-expert-level < vip-max-expert-level
    ;;    & dont-change-unless = t -- use it; else ask
    (vip-ask-level dont-change-unless))
  
  (setq vip-always          	    	t
	vip-ex-style-motion 	    	t
	vip-ex-style-editing-in-insert  t
	vip-want-ctl-h-help nil)

  (cond
	;; a novice or a beginner
	((eq vip-expert-level 1)
	 (global-set-key vip-toggle-key   ;; in emacs-state
			 (if window-system
			     'vip-iconify
			   'suspend-emacs))
	 (setq vip-no-multiple-ESC	     t
	       vip-re-search	    	     t
	       vip-vi-style-in-minibuffer    t
	       vip-search-wrap-around-t	     t
	       vip-want-emacs-keys-in-vi     nil
	       vip-want-emacs-keys-in-insert nil))
	
	;; an intermediate to guru
	((and (> vip-expert-level 1) (< vip-expert-level 5))
	 (setq vip-no-multiple-ESC	     (if window-system t 'twice)
	       vip-want-emacs-keys-in-vi     t
	       vip-want-emacs-keys-in-insert (> vip-expert-level 2))
	       
	 (if (eq vip-expert-level 4) ; respect user's ex-style motions
	     	    	    	     ; and vip-no-multiple-ESC
	     (progn
	       (setq-default vip-ex-style-editing-in-insert
			     (cdr (assoc 'vip-ex-style-editing-in-insert
					 vip-saved-user-settings))
			     vip-ex-style-motion
			     (cdr (assoc 'vip-ex-style-motion
					 vip-saved-user-settings)))
	       (setq vip-ex-style-motion 
		     (cdr (assoc 'vip-ex-style-motion vip-saved-user-settings))
		     vip-ex-style-editing-in-insert
		     (cdr (assoc 'vip-ex-style-editing-in-insert
				 vip-saved-user-settings))
		     vip-re-search
		     (cdr (assoc 'vip-re-search vip-saved-user-settings))
		     vip-no-multiple-ESC 
		     (cdr (assoc 'vip-no-multiple-ESC
				 vip-saved-user-settings))))))
	       
	;; A wizard
	;; Ideally, if 5 is selected, a buffer should pop up to let the
	;; user toggle variable values.
	(t (setq-default vip-ex-style-editing-in-insert
			 (cdr (assoc 'vip-ex-style-editing-in-insert
				     vip-saved-user-settings))
			 vip-ex-style-motion
			 (cdr (assoc 'vip-ex-style-motion
				     vip-saved-user-settings)))
	   (setq  vip-want-ctl-h-help 
		  (cdr (assoc 'vip-want-ctl-h-help vip-saved-user-settings))
		  vip-always
		  (cdr (assoc 'vip-always vip-saved-user-settings))
		  vip-no-multiple-ESC 
		  (cdr (assoc 'vip-no-multiple-ESC vip-saved-user-settings))
		  vip-ex-style-motion 
		  (cdr (assoc 'vip-ex-style-motion vip-saved-user-settings))
		  vip-ex-style-editing-in-insert
		  (cdr (assoc 'vip-ex-style-editing-in-insert
			      vip-saved-user-settings))
		  vip-re-search
		  (cdr (assoc 'vip-re-search vip-saved-user-settings))
		  vip-want-emacs-keys-in-vi 
		  (cdr (assoc 'vip-want-emacs-keys-in-vi
			      vip-saved-user-settings))
		  vip-want-emacs-keys-in-insert
		  (cdr (assoc 'vip-want-emacs-keys-in-insert
			      vip-saved-user-settings)))))
  (vip-set-mode-vars-for vip-current-state)
  (if (or vip-always
	  (and (> vip-expert-level 0) (> 5 vip-expert-level)))
      (vip-set-hooks)))

(defun vip-ask-level (dont-change-unless)
  "Ask user expert level."
  (let ((ask-buffer " *vip-ask-level*")
	level-changed repeated)
    (save-window-excursion
      (switch-to-buffer ask-buffer)
	      
      (or (eq this-command 'vip-set-expert-level)
	  (and
	   (<= vip-expert-level vip-max-expert-level)
	   (>= vip-expert-level 1))
	  (progn
	    (insert "
	      
	      *** Important Notice for VIP users***
	      
			  This is VIPER
	      
@joke
Viper Is a Package for Emacs Rebels,
a VI Plan for Emacs Rescue,
and a venomous VI PERil.
@end joke

Technically speaking, Viper is a new Vi emulator that replaces
the old VIP package.

Viper emulates Vi much better than VIP.  It also significantly
extends and improves upon Vi in many useful ways.

Although many VIP settings in your ~/.vip are compatible with Viper,
you may have to change some of them. Please refer to the documentation,
which can be obtained by executing

:help

when Viper is in Vi state.

If you will be so lucky as to find a bug, report it via the command

:submitReport

Type any key to continue... ")
	    
	    (read-char)
	    (erase-buffer)))
	      
      (while (or (> vip-expert-level vip-max-expert-level)
		 (< vip-expert-level 1)
		 (null dont-change-unless))
	(erase-buffer)
	(if repeated
	    (progn
	      (message "Invalid user level")
	      (beep 1))
	  (setq repeated t))
	(setq dont-change-unless t
	      level-changed t)
	(insert "
Please specify your level of familiarity with the venomous VI PERil
(and the VI Plan for Emacs Rescue).
You can change it at any time by typing `M-x vip-set-expert-level RET'
	
 1 -- BEGINNER: Almost all Emacs features are suppressed.
          Feels almost like straight Vi. File name completion and
          command history in the minibuffer are thrown in as a bonus. 
          To use Emacs productively, you must reach level 3 or higher.
 2 -- MASTER: C-c now has its standard Emacs meaning in Vi command state,
	  so most Emacs commands can be used when Viper is in Vi state.
	  Good progress---you are well on the way to level 3!
 3 -- GRAND MASTER: Like 3, but most Emacs commands are available also
          in Viper's insert state.
 4 -- GURU: Like 3, but user settings are respected for vip-no-multiple-ESC,
	  vip-re-search, vip-ex-style-motion, & vip-ex-style-editing-in-insert
	  variables. Adjust these settings to your taste.
 5 -- WIZARD: Like 4, but user settings are also respected for vip-always,
	  vip-want-ctl-h-help, vip-want-emacs-keys-in-vi, and 
	  vip-want-emacs-keys-in-insert. Adjust these to your taste.
      
Please, specify your level now: ")
	  
	(setq vip-expert-level (- (vip-read-char-exclusive) ?0))
	) ; end while
      
      ;; tell the user if level was changed
      (and level-changed
	   (progn
	     (insert
	      (format "\n\n\n\n\n\t\tYou have selected user level %d"
		      vip-expert-level))
	     (if (y-or-n-p "Do you wish to make this change permanent? ")
		 ;; save the setting for vip-expert-level
		 (vip-save-setting
		  'vip-expert-level
		  (format "Saving user level %d ..." vip-expert-level)
		  vip-custom-file-name))
	     ))
      (bury-buffer) ; remove ask-buffer from screen
      (message "")
      )))


(defun viper-version ()
  (interactive)
  (message "Viper version is %s" viper-version)) 
  
(defalias 'vip-version 'viper-version)

(defun vip-nil ()
  (interactive)
  (beep 1))
  

;; Returns t, if the string before point matches the regexp STR.
(defsubst vip-looking-back (str)
  (and (save-excursion (re-search-backward str nil t))
       (= (point) (match-end 0))))

    
    
;; if ENFORCE-BUFFER is not nil, error if CHAR is a marker in another buffer
(defun vip-register-to-point (char &optional enforce-buffer)
  "Like jump-to-register, but switches to another buffer in another window."
  (interactive "cViper register to point: ")
  (let ((val (get-register char)))
    (cond
     ((and (fboundp 'frame-configuration-p)
	   (frame-configuration-p val))
      (set-frame-configuration val))
     ((window-configuration-p val)
      (set-window-configuration val))
     ((vip-valid-marker val)
      (if (and enforce-buffer
	       (not (equal (current-buffer) (marker-buffer val))))
	  (error (concat vip-EmptyTextmarker " in this buffer")
		 (1- (+ char ?a))))
      (pop-to-buffer  (marker-buffer val))
      (goto-char val))
     ((and (consp val) (eq (car val) 'file))
      (find-file (cdr val)))
     (t
      (error vip-EmptyTextmarker (1- (+ char ?a)))))))


(defun vip-save-kill-buffer ()
  "Save then kill current buffer. "
  (interactive)
  (if (< vip-expert-level 2)
      (save-buffers-kill-emacs)
    (save-buffer)
    (kill-buffer (current-buffer))))



;;; Bug Report

(defun vip-submit-report ()
  "Submit bug report on Viper."
  (interactive)
  (let ((reporter-prompt-for-summary-p t)
	color-display-p frame-parameters
	minibuffer-emacs-face minibuffer-vi-face minibuffer-insert-face
	varlist salutation window-config)
    
    ;; If mode info is needed, add variable to `let' and then set it below,
    ;; like we did with color-display-p.
    (setq color-display-p (if window-system 
			      (vip-display-color-p)
			    'non-x)
	  minibuffer-vi-face (if window-system
				 (vip-get-face vip-minibuffer-vi-face)
			       'non-x)
	  minibuffer-insert-face (if window-system
				     (vip-get-face vip-minibuffer-insert-face)
				   'non-x)
	  minibuffer-emacs-face (if window-system
				    (vip-get-face vip-minibuffer-emacs-face)
				  'non-x)
	  frame-parameters (if (fboundp 'vip-frame-parameters)
			       (vip-frame-parameters (vip-selected-frame))))
    
    (setq varlist (list 'vip-vi-minibuffer-minor-mode
		        'vip-insert-minibuffer-minor-mode
		        'vip-vi-intercept-minor-mode
		        'vip-vi-local-user-minor-mode     
		        'vip-vi-kbd-minor-mode        	
		        'vip-vi-global-user-minor-mode
		        'vip-vi-state-modifier-minor-mode
		        'vip-vi-diehard-minor-mode   
		        'vip-vi-basic-minor-mode    
		        'vip-replace-minor-mode 	  
		        'vip-insert-intercept-minor-mode
		        'vip-insert-local-user-minor-mode 
		        'vip-insert-kbd-minor-mode     	
		        'vip-insert-global-user-minor-mode
		        'vip-insert-state-modifier-minor-mode
		        'vip-insert-diehard-minor-mode 	
		        'vip-insert-basic-minor-mode   
		        'vip-emacs-intercept-minor-mode 
		        'vip-emacs-local-user-minor-mode 
		        'vip-emacs-kbd-minor-mode 
		        'vip-emacs-global-user-minor-mode
		        'vip-emacs-state-modifier-minor-mode
		        'vip-automatic-iso-accents
		        'vip-want-emacs-keys-in-insert
		        'vip-want-emacs-keys-in-vi
		        'vip-keep-point-on-undo
		        'vip-no-multiple-ESC
		        'vip-ESC-key
		        'vip-want-ctl-h-help
		        'vip-ex-style-editing-in-insert
		        'vip-delete-backwards-in-replace
		        'vip-vi-style-in-minibuffer
		        'vip-vi-state-hooks
		        'vip-insert-state-hooks
		        'vip-replace-state-hooks
		        'vip-emacs-state-hooks
		        'ex-cycle-other-window
		        'ex-cycle-through-non-files
		        'vip-expert-level
		        'major-mode
		        'window-system
			'color-display-p
			'frame-parameters
			'minibuffer-vi-face
			'minibuffer-insert-face
			'minibuffer-emacs-face
			))
	  (setq salutation "
Congratulations! You may have unearthed a bug in Viper!
Please mail a concise, accurate summary of the problem to the address above.

-------------------------------------------------------------------")
	  (setq window-config (current-window-configuration))
	  (with-output-to-temp-buffer " *vip-info*"
	    (switch-to-buffer " *vip-info*")
	    (delete-other-windows)
	    (princ "
PLEASE FOLLOW THESE PROCEDURES
------------------------------

Before reporting a bug, please verify that it is related to Viper, and is
not cause by other packages you are using.

Don't report compilation warnings, unless you are certain that there is a
problem. These warnings are normal and unavoidable.

Please note that users should not modify variables and keymaps other than
those advertised in the manual. Such `customization' is likely to crash
Viper, as it would any other improperly customized Emacs package.

If you are reporting an error message received while executing one of the
Viper commands, type:

    M-x set-variable <Return> debug-on-error <Return> t <Return>
	
Then reproduce the error. The above command will cause Emacs to produce a
back trace of the execution that leads to the error. Please include this
trace in your bug report.

If you believe that one of Viper's commands goes into an infinite loop
\(e.g., Emacs freezes\), type:

    M-x set-variable <Return> debug-on-quit <Return> t <Return>
	
Then reproduce the problem. Wait for a few seconds, then type C-g to abort
the current command. Include the resulting back trace in the bug report.

Mail anyway (y or n)? ")
	    (if (y-or-n-p "Mail anyway? ")
		()
	      (set-window-configuration window-config)
	      (error "Bug report aborted")))

	  (require 'reporter)
	  (set-window-configuration window-config)
    
	  (reporter-submit-bug-report "kifer@cs.sunysb.edu"
				      (vip-version)
				      varlist
				      nil 'delete-other-windows
				      salutation)
	  ))
		    

    
		
;; Needed to smooth out the difference between Emacs' unread-command-events
;; and XEmacs unread-command-event. Arg is a character, an event, a list of
;; events or a sequence of keys.
;; The semantics of placing an event on unread-command-event in XEmacs is
;; not the same as placing (setq unread-command-event event)
;; on the event queue using enqueue-eval-event. For instance, an event
;; sitting in unread-command-event will be available to (next-event).
;; In contrast, evals placed on event queue are not evaluated until all
;; previous commands have been executed. This makes a difference when one
;; of the events placed on the event queue is bound to a function that
;; pauses for input, because these evals won't make input immediately
;; available
;;
;; Due to a bug in unread-command-events, a non-event symbol in
;; unread-command-evets list may cause Emacs to label this symbol to be an
;; event. Below, we delete nil from event lists, since nil is the most
;; common problem here. Hopefully, unread-command-evets will be fixed in
;; the next release.
(defun vip-set-unread-command-events (arg)
  (if vip-emacs-p
      (setq unread-command-events
	    (let ((new-events
		   (cond ((eventp arg) (list arg))
			 ((listp arg) arg)
			 ((sequencep arg)
			  (listify-key-sequence arg))
			 (t (error
			     "vip-set-unread-command-events: Invalid arg, %S"
			     arg)))))
	      (if (not (eventp nil))
		  (setq new-events (delq nil new-events)))
	      (append new-events unread-command-events)))
    ;; XEmacs
    (cond ((numberp arg)
	   (setq unread-command-event (character-to-event arg)))
	  ((eventp arg)
	   (setq unread-command-event arg))
	  ((sequencep arg)
	   (let ((length (length arg))
		 (count 0))
	     (while (< count length)
	       (enqueue-eval-event
		'vip-fudge-event-list-in-xemacs
		(if (stringp arg) 
		    (character-to-event (elt arg count))
		  (elt arg count)))
	       (setq count (1+ count))
	       ) ; while
	     (if (> length 0)
		 (or arg unread-command-event))))
	  (t (error "vip-set-unread-command-events: Invalid argument")))))
  
(defun vip-fudge-event-list-in-xemacs (arg)
  (setq unread-command-event arg))
  

;;; Bring in the rest of the files
(require 'viper-mous)
(require 'viper-macs)
(require 'viper-ex)



;; The following is provided for compatibility with older VIP's

(defalias 'vip-change-mode-to-vi 'vip-change-state-to-vi)
(defalias 'vip-change-mode-to-insert 'vip-change-state-to-insert)
(defalias 'vip-change-mode-to-emacs 'vip-change-state-to-emacs)

;; This was the main Vi mode in old versions of VIP which may have been
;; extensively used by VIP users. We declare it as a global var
;; and, after .vip is loaded, we add this keymap to vip-vi-basic-map.
(defvar vip-mode-map (make-sparse-keymap)
  "This was the main Vi-mode keymap in the old versions of VIP.
Viper provides this variable for compatibility. Whatever the user defines
for this map, is merged into Viper's vip-vi-basic-map after loading .vip")
   


;; Load .vip and setup hooks
(defun vip-shell-mode-hook ()
  "Hook specifically designed to enable Vi-style editing in shell mode."
  (setq vip-add-newline-at-eob nil)
  ;; this is nicer in shell mode
  (setq vip-ex-style-editing-in-insert nil
	vip-ex-style-motion nil)
  (vip-add-local-keys 'vi-state
		      '(("\C-m" . comint-send-input) ; return
			("\C-d" . comint-delchar-or-maybe-eof))) ; \C-d
  (vip-add-local-keys 'insert-state
		      '(("\C-m" . comint-send-input) ; return
			("\C-d" . comint-delchar-or-maybe-eof))) ; \C-d
  )
  

;; This sets major mode hooks to make them come up in vip-state.
(defun vip-set-hooks ()
  
  ;; It is of course a misnomer to call viper-mode a `major mode'.
  ;; However, this has the effect that if the user didn't specify the
  ;; default mode, new buffers that fall back on the default will come up
  ;; in Fundamental Mode and Vi state.
  (setq default-major-mode 'viper-mode)
  
  (defadvice fundamental-mode (after vip-fundamental-mode-ad activate)
    (vip-change-state-to-vi))
  
  ;; The following major modes should come up in vi-state
  (defvar emacs-lisp-mode-hook nil)
  (add-hook 'emacs-lisp-mode-hook 'viper-mode)
  
  (defvar lisp-mode-hook nil)
  (add-hook 'lisp-mode-hook 'viper-mode)
  
  (defvar bibtex-mode-hook nil) 	    		
  (add-hook 'bibtex-mode-hook 'viper-mode) 	  
      
  (defvar cc-mode-hook nil)
  (add-hook 'cc-mode-hook 'viper-mode)
      
  (defvar c-mode-hook nil)
  (add-hook 'c-mode-hook 'viper-mode)
      
  (defvar c++-mode-hook nil)
  (add-hook 'c++-mode-hook 'viper-mode)
  
  (defvar lisp-interaction-mode-hook nil)
  (add-hook 'lisp-interaction-mode-hook 'viper-mode)
      
  (defvar text-mode-hook nil)
  (add-hook 'text-mode-hook 'viper-mode)
      
  (add-hook 'completion-list-mode-hook 'viper-mode)  
  (add-hook 'compilation-mode-hook     'viper-mode)  
  
  (defvar emerge-startup-hook nil)
  (add-hook 'emerge-startup-hook 'vip-change-state-to-emacs)
  ;; Run vip-change-state-to-vi after quitting emerge.
  (vip-eval-after-load "emerge"
		   '(defadvice emerge-quit (after vip-emerge-advice activate)
		     "Run vip-change-state-to-vi after quitting emerge."
		     (vip-change-state-to-vi)))
  ;; In case Emerge was loaded before Viper.
  (defadvice emerge-quit (after vip-emerge-advice activate)
		     "Run vip-change-state-to-vi after quitting emerge."
		     (vip-change-state-to-vi))
  
  (vip-eval-after-load "asm-mode"
		       '(defadvice asm-mode (after vip-asm-mode-ad activate)
		       "Run vip-change-state-to-vi on entry."
		       (vip-change-state-to-vi)))
  
  ;; passwd.el sets up its own buffer, which turns up in Vi mode,
  ;; overriding the local map. Noone needs Vi mode here.
  (vip-eval-after-load
   "passwd"
   '(defadvice read-passwd-1 (before vip-passwd-ad activate)
      "Vi-ism is prohibited when reading passwords, so switch to Emacs."
      (vip-change-state-to-emacs)))
  
  ;; Emacs shell
  (defvar shell-mode-hook nil)
  (add-hook 'shell-mode-hook 'vip-change-state-to-insert)
  (add-hook 'shell-mode-hook 'vip-shell-mode-hook)
  
  ;; Shell scripts
  (defvar sh-mode-hook nil)
  (add-hook 'sh-mode-hook 'viper-mode)
  
  ;; Dired
  ;; This is only necessary when the user uses vip-modify-major-mode
  (add-hook 'dired-mode-hook 'vip-change-state-to-emacs)

  (defvar view-hook nil
    "View hook. Run after view mode.")
  (add-hook 'view-hook 'vip-change-state-to-emacs)
  
  ;; For VM users.
  ;; Put summary and other VM buffers in Emacs state.
  (defvar vm-mode-hooks nil 
    "This hook is run after vm is started.")
  (defvar vm-summary-mode-hooks nil 
    "This hook is run after vm switches to summary mode.")
  (add-hook 'vm-mode-hooks   'vip-change-state-to-emacs)
  (add-hook 'vm-summary-mode-hooks   'vip-change-state-to-emacs)
  
  ;; For RMAIL users.
  ;; Put buf in Emacs state after edit.
  (vip-eval-after-load
   "rmailedit"
   '(defadvice rmail-cease-edit (after vip-rmail-advice activate)
      "Switch buffer to emacs-state  after finishing with editing a message."
      (vip-change-state-to-emacs)))
  ;; In case RMAIL was loaded before Viper.
  (defadvice rmail-cease-edit (after vip-rmail-advice activate)
      "Switch buffer to emacs-state after finishing with editing a message."
      (vip-change-state-to-emacs))
  ) ; vip-set-hooks
      

;; ~/.vip is loaded if it exists
(if (and (file-exists-p vip-custom-file-name)
	 (not noninteractive))
    (load vip-custom-file-name))

;; VIP compatibility: merge whatever the user has in vip-mode-map into
;; Viper's basic map.
(vip-add-keymap vip-mode-map vip-vi-global-user-map)


;; Applying Viper customization -- runs after (load .vip)

;; Save user settings or Viper defaults for vars controled by vip-expert-level
(setq vip-saved-user-settings 
      (list (cons 'vip-want-ctl-h-help vip-want-ctl-h-help)
	    (cons 'vip-always vip-always)
	    (cons 'vip-no-multiple-ESC vip-no-multiple-ESC)
	    (cons 'vip-ex-style-motion vip-ex-style-motion)
	    (cons 'vip-ex-style-editing-in-insert
		    	    	    	    vip-ex-style-editing-in-insert) 
	    (cons 'vip-want-emacs-keys-in-vi vip-want-emacs-keys-in-vi)
	    (cons 'vip-want-emacs-keys-in-insert vip-want-emacs-keys-in-insert)
	    (cons 'vip-re-search vip-re-search)))
	      

(vip-set-minibuffer-style)
(vip-set-minibuffer-faces)
(vip-set-search-face)
   
;;; Familiarize Viper with some minor modes that have their own keymaps
(vip-harness-minor-mode "compile")
(vip-harness-minor-mode "outline")
(vip-harness-minor-mode "allout")
(vip-harness-minor-mode "xref")
(vip-harness-minor-mode "lmenu")
(vip-harness-minor-mode "vc")
(vip-harness-minor-mode "ltx-math") ; LaTeX-math-mode in AUC-TeX
(vip-harness-minor-mode "latex")    ; latex they moved math mode here


;; Intercept maps could go in viper-keym.el
;; We keep them here in case someone redefines them in ~/.vip

(define-key vip-vi-intercept-map vip-ESC-key 'vip-intercept-ESC-key)
(define-key vip-insert-intercept-map vip-ESC-key 'vip-intercept-ESC-key)

;; This is taken care of by vip-insert-global-user-map.
;;(define-key vip-replace-map vip-ESC-key 'vip-intercept-ESC-key)

(define-key vip-insert-intercept-map vip-toggle-key 'vip-alternate-ESC)
;; The default vip-toggle-key is \C-z; for the novice, it suspends or
;; iconifies Emacs
(define-key vip-vi-intercept-map vip-toggle-key
  '(lambda () (interactive)
     (if (and (< vip-expert-level 2) (equal vip-toggle-key "\C-z"))
	 (if window-system (vip-iconify) (suspend-emacs))
       (vip-change-state-to-emacs))))

(define-key vip-emacs-intercept-map vip-toggle-key 'vip-change-state-to-vi)


(if (or vip-always 
	(and (< vip-expert-level 5) (> vip-expert-level 0)))
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
    
;; set some useful macros

;; repeat the 2nd previous command without rotating the command history
(vip-record-kbd-macro
 (vector vip-repeat-from-history-key '\1) 'vi-state
 [(meta x) v i p - r e p e a t - f r o m - h i s t o r y return] 't)
;; repeat the 3d previous command without rotating the command history
(vip-record-kbd-macro
 (vector vip-repeat-from-history-key '\2) 'vi-state
 [(meta x) v i p - r e p e a t - f r o m - h i s t o r y return] 't)
 
;; toggle case sensitivity in search
(vip-record-kbd-macro
 "//" 'vi-state
 [1 (meta x) v i p - t o g g l e - s e a r c h - s t y l e return] 't)
;; toggle regexp/vanila search
(vip-record-kbd-macro
 "///" 'vi-state
 [2 (meta x) v i p - t o g g l e - s e a r c h - s t y l e return] 't)
 

(run-hooks 'vip-load-hooks) ; the last chance to change anything

(provide 'viper)
(provide 'vip19)
(provide 'vip)

;;;  viper.el ends here

