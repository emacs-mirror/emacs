;;;_* Allout - An extensive outline-mode for Emacs.
;;; Note - the lines beginning with ';;;_' are outline topic headers.
;;;        Load this file (or 'eval-current-buffer') and revisit the
;;;        file to give it a whirl.

;;;_ + Provide
(provide 'outline)

;;;_ + Package Identification Stuff

;;;_  - Author: Ken Manheimer <klm@nist.gov>
;;;_  - Maintainer: Ken Manheimer <klm@nist.gov>
;;;_  - Created: Dec 1991 - first release to usenet
;;;_  - Version: $Id: allout.el,v 1.4 1993/12/23 04:55:44 rms Exp rms $||
;;;_  - Keywords: outline mode

;;;_  - LCD Archive Entry

;; LCD Archive Entry:
;; allout|Ken Manheimer|klm@nist.gov
;; |A more thorough outline-mode
;; |27-May-1993|$Id: allout.el,v 1.4 1993/12/23 04:55:44 rms Exp rms $||

;;;_  - Description
;; A full-fledged outline mode, based on the original rudimentary
;; GNU emacs outline functionality.
;;
;; Ken Manheimer		 	Nat'l Inst of Standards and Technology
;; klm@nist.gov (301)975-3539	        (Formerly Nat'l Bureau of Standards)
;;    NIST Shared File Service Manager and Developer

;;;_  - Copyright
;; Copyright (C) 1992, 1993 Free Software Foundation, Inc.

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

;;;_ + User Customization variables

;;;_  - Topic Header configuration

;;;_   = outline-header-prefix
(defvar outline-header-prefix "."
  "*   Leading string for greater than level 0 topic headers.")
(make-variable-buffer-local 'outline-header-prefix)

;;;_   = outline-header-subtraction
(defvar outline-header-subtraction (1- (length outline-header-prefix))
  "*   Leading string for greater than level 0 topic headers.")
(make-variable-buffer-local 'outline-header-subtraction)

;;;_   = outline-primary-bullet
(defvar outline-primary-bullet "*") ;; Changing this var disables any
                                    ;; backwards compatibility with
                                    ;; the original outline mode.
(make-variable-buffer-local 'outline-primary-bullet)

;;;_   = outline-plain-bullets-string
(defvar outline-plain-bullets-string ""
  "*   The bullets normally used in outline topic prefixes.  See
   'outline-distinctive-bullets-string' for the other kind of
   bullets.

   DO NOT include the close-square-bracket, ']', among any bullets.

   You must run 'set-outline-regexp' in order for changes to the
   value of this var to effect outline-mode operation.")
(setq outline-plain-bullets-string (concat outline-primary-bullet
                                           "+-:.;,"))
(make-variable-buffer-local 'outline-plain-bullets-string)

;;;_   = outline-distinctive-bullets-string
(defvar outline-distinctive-bullets-string ""
  "*   The bullets used for distinguishing outline topics.  These
   bullets are not offered among the regular rotation, and are not
   changed when automatically rebulleting, as when shifting the
   level of a topic.  See 'outline-plain-bullets-string' for the
   other kind of bullets.

   DO NOT include the close-square-bracket, ']', among any bullets.

   You must run 'set-outline-regexp' in order for changes
   to the value of this var to effect outline-mode operation.")
(setq outline-distinctive-bullets-string "=>([{}&!?#%\"X@$~")
(make-variable-buffer-local 'outline-distinctive-bullets-string)

;;;_   > outline-numbered-bullet ()
(defvar outline-numbered-bullet ()
  "*   Bullet signifying outline prefixes which are to be numbered.
   Leave it nil if you don't want any numbering, or set it to a
   string with the bullet you want to be used.")
(setq outline-numbered-bullet "#")
(make-variable-buffer-local 'outline-numbered-bullet)

;;;_   = outline-file-xref-bullet
(defvar outline-file-xref-bullet "@"
  "*  Set this var to the bullet you want to use for file cross-references.
   Set it 'nil' if you want to inhibit this capability.")

;;;_  - Miscellaneous customization

;;;_   = outline-stylish-prefixes
(defvar outline-stylish-prefixes t
  "*A true value for this var makes the topic-prefix creation and modification
   functions vary the prefix bullet char according to level.  Otherwise, only
   asterisks ('*') and distinctive bullets are used.

   This is how an outline can look with stylish prefixes:

   * Top level
   .* A topic
   . + One level 3 subtopic
   .  . One level 4 subtopic
   . + Another level 3 subtopic
   .  . A level 4 subtopic
   .  #2 A distinguished, numbered level 4 subtopic
   .  ! A distinguished ('!') level 4 subtopic
   .  #4 Another numbered level 4 subtopic
   
   This would be an outline with stylish prefixes inhibited:

   * Top level
   .* A topic
   .! A distinctive (but measly) subtopic
   . * A sub-subtopic - no bullets from outline-plain-bullets-string but '*'

   Stylish and constant prefixes (as well as old-style prefixes) are
   always respected by the topic maneuvering functions, regardless of
   this variable setting.

   The setting of this var is not relevant when outline-old-style-prefixes
   is t.")
(make-variable-buffer-local 'outline-stylish-prefixes)

;;;_   = outline-old-style-prefixes
(defvar outline-old-style-prefixes nil
  "*Setting this var causes the topic-prefix creation and modification
   functions to make only asterix-padded prefixes, so they look exactly
   like the old style prefixes.

   Both old and new style prefixes are always respected by the topic
   maneuvering functions.")
(make-variable-buffer-local 'outline-old-style-prefixes)

;;;_   = outline-enwrap-isearch-mode
                                        ; Spiffy dynamic-exposure
                                        ; during searches requires
                                        ; Dan LaLiberte's isearch-mode:
(defvar outline-enwrap-isearch-mode "isearch-mode.el"
  "*  Set this var to the name of the (non-compiled) elisp code for
   isearch-mode, if you have Dan LaLiberte's 'isearch-mode'
   stuff and want isearches to reveal hidden stuff encountered in the
   course of a search, and reconceal it if you go past.  Set it nil if
   you don't have the package, or don't want to use this feature.")

;;;_   = outline-use-hanging-indents
(defvar outline-use-hanging-indents t
  "*  Set this var non-nil if you have Kyle E Jones' filladapt stuff,
  and you want outline to fill topics as hanging indents to the
  bullets.")
(make-variable-buffer-local 'outline-use-hanging-indents)

;;;_   = outline-reindent-bodies
(defvar outline-reindent-bodies t
  "*  Set this var non-nil if you want topic depth adjustments to
  reindent hanging bodies (ie, bodies lines indented to beginning of
  heading text).  The performance hit is small.

  Avoid this strenuously when using outline mode on program code.
  It's great for text, though.")
(make-variable-buffer-local 'outline-reindent-bodies)

;;;_   = outline-mode-keys
;;; You have to restart outline-mode - '(outline-mode t)' - to have
;;; any changes take hold.
(defvar outline-mode-keys ()
  "Assoc list of outline-mode-keybindings, for common reference in setting 
up major and minor-mode keybindings.")
(setq outline-mode-keys
      '(
                                        ; Motion commands:
        ("\C-c\C-n" outline-next-visible-heading)
        ("\C-c\C-p" outline-previous-visible-heading)
        ("\C-c\C-u" outline-up-current-level)
        ("\C-c\C-f" outline-forward-current-level)
        ("\C-c\C-b" outline-backward-current-level)
        ("\C-c\C-a" outline-beginning-of-current-entry)
        ("\C-c\C-e" outline-end-of-current-entry)
                                        ; Exposure commands:
        ("\C-c\C-i" outline-show-current-children)
        ("\C-c\C-s" outline-show-current-subtree)
        ("\C-c\C-h" outline-hide-current-subtree)
        ("\C-c\C-o" outline-show-current-entry)
        ("\C-c!" outline-show-all)
                                        ; Alteration commands:
        ("\C-c " open-sibtopic)
        ("\C-c." open-subtopic)
        ("\C-c," open-supertopic)
        ("\C-c'" outline-shift-in)
        ("\C-c>" outline-shift-in)
        ("\C-c<" outline-shift-out)
        ("\C-c\C-m" outline-rebullet-topic)
        ("\C-cb" outline-rebullet-current-heading)
        ("\C-c#" outline-number-siblings)
        ("\C-k" outline-kill-line)
        ("\C-y" outline-yank)
        ("\M-y" outline-yank-pop)
        ("\C-c\C-k" outline-kill-topic)
                                        ; Miscellaneous commands:
        ("\C-c@" outline-resolve-xref)
        ("\C-cc" outline-copy-exposed)))

;;;_ + Code - no user customizations below.

;;;_  #1 Outline Format and Internal Mode Configuration

;;;_   : Topic header format
;;;_    = outline-regexp
(defvar outline-regexp ""
  "*   Regular expression to match the beginning of a heading line.
   Any line whose beginning matches this regexp is considered a
   heading.  This var is set according to the user configuration vars
   by set-outline-regexp.")
(make-variable-buffer-local 'outline-regexp)
;;;_    = outline-bullets-string
(defvar outline-bullets-string ""
  "   A string dictating the valid set of outline topic bullets.  This
   var should *not* be set by the user - it is set by 'set-outline-regexp',
   and is composed from the elements of 'outline-plain-bullets-string'
   and 'outline-distinctive-bullets-string'.")
(make-variable-buffer-local 'outline-bullets-string)
;;;_    = outline-line-boundary-regexp
(defvar outline-line-boundary-regexp ()
  "   outline-regexp with outline-style beginning of line anchor (ie,
   C-j, *or* C-m, for prefixes of hidden topics).  This is properly
   set when outline-regexp is produced by 'set-outline-regexp', so
   that (match-beginning 2) and (match-end 2) delimit the prefix.")
(make-variable-buffer-local 'outline-line-boundary-regexp)
;;;_    = outline-bob-regexp
(defvar outline-bob-regexp ()
  " Like outline-line-boundary-regexp, this is an outline-regexp for
  outline headers at the beginning of the buffer.  (match-beginning 2)
  and (match-end 2)
   delimit the prefix.")
(make-variable-buffer-local 'outline-line-bob-regexp)
;;;_    > outline-reset-header-lead (header-lead)
(defun outline-reset-header-lead (header-lead)
  "*  Reset the leading string used to identify topic headers."
  (interactive "sNew lead string: ")
  ;;()
  (setq outline-header-prefix header-lead)
  (setq outline-header-subtraction (1- (length outline-header-prefix)))
  (set-outline-regexp)
  )
;;;_    > outline-lead-with-comment-string (header-lead)
(defun outline-lead-with-comment-string (&optional header-lead)
  "* Set the topic-header leading string to specified string.  Useful
  when for encapsulating outline structure in programming language
  comments.  Returns the leading string."

  (interactive "P")
  (if (not (stringp header-lead))
      (setq header-lead (read-string
                         "String prefix for topic headers: ")))
  (setq outline-reindent-bodies nil)
  (outline-reset-header-lead header-lead)
  header-lead)
;;;_    > set-outline-regexp ()
(defun set-outline-regexp ()
  "   Generate proper topic-header regexp form for outline functions, from
   outline-plain-bullets-string and outline-distinctive-bullets-string."

  (interactive)
  ;; Derive outline-bullets-string from user configured components:
  (setq outline-bullets-string "")
  (let ((strings (list 'outline-plain-bullets-string
                       'outline-distinctive-bullets-string))
        cur-string
        cur-len
        cur-char-string
        index
        new-string)
    (while strings
      (setq new-string "") (setq index 0)
      (setq cur-len (length (setq cur-string (symbol-value (car strings)))))
      (while (< index cur-len)
        (setq cur-char (aref cur-string index))
        (setq outline-bullets-string
              (concat outline-bullets-string
                      (cond
                                        ; Single dash would denote a
                                        ; sequence, repeated denotes
                                        ; a dash:
                       ((eq cur-char ?-) "--")
                                        ; literal close-square-bracket
                                        ; doesn't work right in the
                                        ; expr, exclude it:
                       ((eq cur-char ?\]) "")
                       (t (regexp-quote  (char-to-string cur-char))))))
        (setq index (1+ index)))
      (setq strings (cdr strings)))
    )
  ;; Derive next for repeated use in outline-pending-bullet:
  (setq outline-plain-bullets-string-len (length outline-plain-bullets-string))
  (setq outline-header-subtraction (1- (length outline-header-prefix)))
  ;; Produce the new outline-regexp:
  (setq outline-regexp (concat "\\(\\"
                               outline-header-prefix
                               "[ \t]*["
                               outline-bullets-string
                               "]\\)\\|\\"
                               outline-primary-bullet
                               "+\\|\^l"))
  (setq outline-line-boundary-regexp
        (concat "\\([\C-j\C-m]\\)\\(" outline-regexp "\\)"))
  (setq outline-bob-regexp
        (concat "\\(\\`\\)\\(" outline-regexp "\\)"))
  )

;;;_   : Key bindings
;;;_    = Generic minor keybindings control
;;;_     ; Stallman's suggestion
(defvar outline-mode-map nil "")
 
(if outline-mode-map
    nil
  (setq outline-mode-map (nconc (make-sparse-keymap) text-mode-map))
  (define-key outline-mode-map "\C-c\C-n" 'outline-next-visible-heading)
  (define-key outline-mode-map "\C-c\C-p" 'outline-previous-visible-heading)
  (define-key outline-mode-map "\C-c\C-i" 'show-children)
  (define-key outline-mode-map "\C-c\C-s" 'show-subtree)
  (define-key outline-mode-map "\C-c\C-h" 'hide-subtree)
  (define-key outline-mode-map "\C-c\C-u" 'outline-up-heading)
  (define-key outline-mode-map "\C-c\C-f" 'outline-forward-same-level)
  (define-key outline-mode-map "\C-c\C-b" 'outline-backward-same-level))
 
(defvar outline-minor-mode nil
  "Non-nil if using Outline mode as a minor mode of some other mode.")
(make-variable-buffer-local 'outline-minor-mode)
(put 'outline-minor-mode 'permanent-local t)
(setq minor-mode-alist (append minor-mode-alist
                               (list '(outline-minor-mode " Outl"))))
 
(defvar outline-minor-mode-map nil)
(if outline-minor-mode-map
    nil
  (setq outline-minor-mode-map (make-sparse-keymap))
  (define-key outline-minor-mode-map "\C-c"
    (lookup-key outline-mode-map "\C-c")))
 
(or (assq 'outline-minor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'outline-minor-mode outline-minor-mode-map)
                minor-mode-map-alist)))
 
(defun outline-minor-mode (&optional arg)
  "Toggle Outline minor mode.
With arg, turn Outline minor mode on if arg is positive, off otherwise.
See the command `outline-mode' for more information on this mode."
  (interactive "P")
  (setq outline-minor-mode
        (if (null arg) (not outline-minor-mode)
          (> (prefix-numeric-value arg) 0)))
  (if outline-minor-mode
      (progn
        (setq selective-display t)
        (run-hooks 'outline-minor-mode-hook))
    (setq selective-display nil)))
;;;_     ; minor-bind-keys (keys-assoc)
(defun minor-bind-keys (keys-assoc)
  "   Establish BINDINGS assoc list in current buffer, returning a list
   for subsequent use by minor-unbind-keys to resume overloaded local
   bindings."
   (interactive)
   ;; Cycle thru key list, registering prevailing local binding for key, if
   ;; any (for prospective resumption by outline-minor-unbind-keys), then
   ;; overloading it with outline-mode one.
   (let ((local-map (or (current-local-map)
                        (make-sparse-keymap)))
         key new-func unbinding-registry prevailing-func)
     (while keys-assoc
       (setq curr-key (car (car keys-assoc)))
       (setq new-func (car (cdr (car keys-assoc))))
       (setq prevailing-func (local-key-binding curr-key))
       (if (not (symbolp prevailing-func))
           (setq prevailing-func nil))
       ;; Register key being changed, prevailing local binding, & new binding:
       (setq unbinding-registry
             (cons (list curr-key (local-key-binding curr-key) new-func)
                   unbinding-registry))
                                        ; Make the binding:
       
       (define-key local-map curr-key new-func)
                                        ; Increment for next iteration:
       (setq keys-assoc (cdr keys-assoc)))
                                        ; Establish modified map:
     (use-local-map local-map)
                                        ; Return the registry:
     unbinding-registry)
   )

;;;_     ; minor-relinquish-keys (unbinding-registry)
(defun minor-relinquish-keys (unbinding-registry)
  "   Given registry of MODAL-BINDINGS, as produced by minor-bind-keys,
   resume the former local keybindings of those keys that retain the
   local bindings set by minor-bind-keys.  Changed local bindings are
   left alone, so other minor (user or modal) bindings are not disrupted.

   Returns a list of those registrations which were not, because of
   tampering subsequent to the registration by minor-bind-keys, resumed."
  (interactive)
  (let (residue curr-item curr-key curr-resume curr-relinquish)
    (while unbinding-registry
      (setq curr-item (car unbinding-registry))
      (setq curr-key (car curr-item))
      (setq curr-resume (car (cdr curr-item)))
      (setq curr-relinquish (car (cdr (cdr curr-item))))
      (if (equal (local-key-binding curr-key) curr-relinquish)
          (if curr-resume
              ;; Was a local binding to be resumed - do so:
              (local-set-key curr-key curr-resume)
            (local-unset-key curr-key))
        ;; Bindings been tampered with since registration - leave it be, and
        ;; register so on residue list:
        (setq residue (cons residue curr-item)))
      (setq unbinding-registry (cdr unbinding-registry)))
    residue)
  )
;;;_    = outline-minor-prior-keys
(defvar outline-minor-prior-keys ()
  "Former key bindings assoc-list, for resumption from  outline minor-mode.")
(make-variable-buffer-local 'outline-minor-prior-keys)

                                        ; Both major and minor mode
                                        ; bindings are dictated by
                                        ; this list - put your
                                        ; settings here.
;;;_    > outline-minor-bind-keys ()
(defun outline-minor-bind-keys ()
  "   Establish outline-mode keybindings as MINOR modality of current buffer."
  (setq outline-minor-prior-keys
        (minor-bind-keys outline-mode-keys)))
;;;_    > outline-minor-relinquish-keys ()
(defun outline-minor-relinquish-keys ()
  "   Resurrect local keybindings as they were before outline-minor-bind-keys."
  (minor-relinquish-keys outline-minor-prior-keys)
)

;;;_   : Mode-Specific Variables Maintenance
;;;_    = outline-mode-prior-settings
(defvar outline-mode-prior-settings nil
  "For internal use by outline mode, registers settings to be resumed
on mode deactivation.")
(make-variable-buffer-local 'outline-mode-prior-settings)
;;;_    > outline-resumptions (name &optional value)
(defun outline-resumptions (name &optional value)

  " Registers information for later reference, or performs resumption of
  outline-mode specific values.  First arg is NAME of variable affected.
  optional second arg is list containing outline-mode-specific VALUE to
  be impose on named variable, and to be registered.  (It's a list so you
  can specify registrations of null values.)  If no value is specified,
  the registered value is returned (encapsulated in the list, so the
  caller can distinguish nil vs no value), and the registration is popped
  from the list."

  (let ((on-list (assq name outline-mode-prior-settings))
        prior-capsule                   ; By 'capsule' i mean a list
                                        ; containing a value, so we can
                                        ; distinguish nil from no value.
        )

    (if value

        ;; Registering:
        (progn
          (if on-list
              nil 	; Already preserved prior value - don't mess with it.
            ;; Register the old value, or nil if previously unbound:
            (setq outline-mode-prior-settings
                  (cons (list name
                              (if (boundp name) (list (symbol-value name))))
                        outline-mode-prior-settings)))
                                        ; And impose the new value:
          (set name (car value)))

      ;; Relinquishing:
      (if (not on-list)

          ;; Oops, not registered - leave it be:
          nil

        ;; Some registration:
                                        ; reestablish it:
        (setq prior-capsule (car (cdr on-list)))
        (if prior-capsule
            (set name (car prior-capsule)) ; Some prior value - reestablish it.
          (makunbound name))		; Previously unbound - demolish var.
                                        ; Remove registration:
        (let (rebuild)
          (while outline-mode-prior-settings
            (if (not (eq (car outline-mode-prior-settings)
                         on-list))
                (setq rebuild
                      (cons (car outline-mode-prior-settings)
                            rebuild)))
            (setq outline-mode-prior-settings
                  (cdr outline-mode-prior-settings)))
          (setq outline-mode-prior-settings rebuild)))))
  )

;;;_   : Overall
;;;_    = outline-mode
(defvar outline-mode () "Allout outline mode minor-mode flag.")
(make-variable-buffer-local 'outline-mode)
;;;_    > outline-mode (&optional toggle)
(defun outline-mode (&optional toggle)
  "  Set minor mode for editing outlines with selective display.

   Look below the description of the bindings for explanation of the
   terminology use in outline-mode commands.

   (Note - this is not a proper minor mode, because it does affect key
   bindings.  It's not too improper, however, because it does resurrect
   any bindings which have not been tampered with since it changed them.)

Exposure Commands		      Movement Commands
C-c C-h	outline-hide-current-subtree  C-c C-n outline-next-visible-heading
C-c C-i	outline-show-current-children C-c C-p outline-previous-visible-heading
C-c C-s	outline-show-current-subtree  C-c C-u outline-up-current-level
C-c C-o	outline-show-current-entry    C-c C-f outline-forward-current-level
C-c !   outline-show-all              C-c C-b outline-backward-current-level
	outline-hide-current-leaves   C-c C-e outline-end-of-current-entry
                                     C-c C-a outline-beginning-of-current-entry


Topic Header Generation Commands
C-c<SP>	open-sibtopic		Create a new sibling after current topic
C-c .	open-subtopic		... an offspring of current topic
C-c ,	open-supertopic		... a sibling of the current topic's parent

Level and Prefix Adjustment Commands
C-c >	outline-shift-in	Shift current topic and all offspring deeper
C-c <	outline-shift-out	... less deep
C-c<CR>	outline-rebullet-topic	Reconcile bullets of topic and its offspring
                                - distinctive bullets are not changed, all
                                  others set suitable according to depth
C-c b	outline-rebullet-current-heading Prompt for alternate bullet for
					 current topic
C-c #	outline-number-siblings	Number bullets of topic and siblings - the
				offspring are not affected.  With repeat
				count, revoke numbering.

Killing and Yanking - all keep siblings numbering reconciled as appropriate
C-k	outline-kill-line	Regular kill line, but respects numbering ,etc
C-c C-k	outline-kill-topic	Kill current topic, including offspring
C-y	outline-yank		Yank, adjusting depth of yanked topic to
				depth of heading if yanking into bare topic
                                heading (ie, prefix sans text)
M-y	outline-yank-pop	Is to outline-yank as yank-pop is to yank

Misc commands
C-c @   outline-resolve-xref    pop-to-buffer named by xref (cf
                                outline-file-xref-bullet)
C-c c	outline-copy-exposed	Copy outline sans all hidden stuff to
				another buffer whose name is derived
				from the current one - \"XXX exposed\"
M-x outlinify-sticky            Activate outline mode for current buffer
                                and establish -*- outline -*- mode specifier
                                as well as file local vars to automatically
                                set exposure.  Try it.

                             Terminology

Topic: A basic cohesive component of an emacs outline, which can
       be closed (made hidden), opened (revealed), generated,
       traversed, and shifted as units, using outline-mode functions.
       A topic is composed of a HEADER, a BODY, and SUBTOPICs (see below).

Exposure: Hidden (~closed~) topics are represented by ellipses ('...')
          at the end of the visible SUPERTOPIC which contains them,
          rather than by their actual text.  Hidden topics are still
          susceptible to editing and regular movement functions, they
          just are not displayed normally, effectively collapsed into
          the ellipses which represent them.  Outline mode provides
          the means to selectively expose topics based on their
          NESTING.

          SUBTOPICS of a topic can be hidden and subsequently revealed
          based on their DEPTH relative to the supertopic from which
          the exposure is being done.

          The BODIES of a topic do not generally become visible except
          during exposure of entire subtrees (see documentation for
          '-current-subtree'), or when the entry is explicitly exposed
          with the 'outline-show-entry' function, or (if you have a
          special version of isearch installed) when encountered by
          incremental searches.

          The CURRENT topic is the more recent visible one before or
          including the text cursor.

Header: The initial portion of an outline topic.  It is composed of a
        topic header PREFIX at the beginning of the line, followed by
        text to the end of the EFFECTIVE LINE.

Body: Any subsequent lines of text following a topic header and preceding
      the next one.  This is also referred to as the entry for a topic.

Prefix: The text which distinguishes topic headers from normal text
        lines.  There are two forms, both of which start at the beginning
        of the topic header (EFFECTIVE) line.  The length of the prefix
        represents the DEPTH of the topic.  The fundamental sort begins
        either with solely an asterisk ('*') or else dot ('.') followed
        by zero or more spaces and then an outline BULLET.  [Note - you
        can now designate your own, arbitrary HEADER-LEAD string, by
        setting the variable 'outline-header-prefix'.]  The second form
        is for backwards compatibility with the original emacs outline
        mode, and consists solely of asterisks.  Both sorts are
        recognized by all outline commands.  The first sort is generated
        by outline topic production commands if the emacs variable
        outline-old-style-prefixes is nil, otherwise the second style is
        used.

Bullet: An outline prefix bullet is one of the characters on either
        of the outline bullet string vars, 'outline-plain-bullets-string'
        and 'outline-distinctive-bullets-string'.  (See their
        documentation for more details.)  The default choice of bullet
        for any prefix depends on the DEPTH of the topic.

Depth and Nesting:
       The length of a topic header prefix, from the initial
       character to the bullet (inclusive), represents the depth of
       the topic.  A topic is considered to contain the subsequent
       topics of greater depth up to the next topic of the same
       depth, and the contained topics are recursively considered to
       be nested within all containing topics.  Contained topics are
       called subtopics.  Immediate subtopics are called 'children'.
       Containing topics are supertopicsimmediate supertopics are
       'parents'.  Contained topics of the same depth are called
       siblings.

Effective line: The regular ascii text in which form outlines are
                saved are manipulated in outline-mode to engage emacs'
                selective-display faculty.  The upshot is that the
                effective end of an outline line can be terminated by
                either a normal Unix newline char, \n, or the special
                outline-mode eol, ^M.  This only matters at the user
                level when you're doing searches which key on the end of
                line character."

  (interactive "P")

  (let* ((active (and (boundp 'outline-mode) outline-mode))
         (toggle (and toggle
                      (or (and (listp toggle)(car toggle))
                          toggle)))
         (explicit-activation (and toggle
                                   (or (symbolp toggle)
                                       (and (natnump toggle)
                                            (not (zerop toggle)))))))
                                       
    (cond

     ((and (not explicit-activation) (or active toggle))
      ;; Activation not explicitly requested, and either in active
      ;; state or deactivation specifically requested:
      (outline-minor-relinquish-keys)
      (outline-resumptions 'selective-display)
      (outline-resumptions 'indent-tabs-mode)
      (outline-resumptions 'paragraph-start)
      (outline-resumptions 'paragraph-separate)
      (setq outline-mode nil))

     ;; Deactivation *not* indicated.
     ((not active)
      ;; Not already active - activate:
      (outline-minor-bind-keys)
      (outline-resumptions 'selective-display '(t))
      (outline-resumptions 'indent-tabs-mode '(nil))
      (or (assq 'outline-mode minor-mode-alist)
          (setq minor-mode-alist
                (cons '(outline-mode " Outline") minor-mode-alist)))
      (set-outline-regexp)

      (make-local-variable 'paragraph-start)
      (outline-resumptions 'paragraph-start
                           (list (concat paragraph-start "\\|^\\("
                                         outline-regexp "\\)")))
      (make-local-variable 'paragraph-separate)
      (outline-resumptions 'paragraph-separate
                           (list (concat paragraph-separate "\\|^\\("
                                         outline-regexp "\\)")))

      (if outline-enwrap-isearch-mode
          (outline-enwrap-isearch))
      (if (and outline-use-hanging-indents
               (boundp 'filladapt-prefix-table))
          ;; Add outline-prefix recognition to filladapt - not standard:
          (progn (setq filladapt-prefix-table
                       (cons (cons (concat "\\(" outline-regexp "\\) ")
                                   'filladapt-hanging-list)
                             filladapt-prefix-table))
                 (setq filladapt-hanging-list-prefixes
                       (cons outline-regexp
                             filladapt-hanging-list-prefixes))))
      (run-hooks 'outline-mode-hook)
      (setq outline-mode t))
     ) ; cond
    ) ; let*
  ) ; defun
    

;;;_  #2 Internal Position State-Tracking Variables
;;; All basic outline functions which directly do string matches to
;;; evaluate heading prefix location set the variables
;;; outline-recent-prefix-beginning and outline-recent-prefix-end when
;;; successful.  Functions starting with 'outline-recent-' all use
;;; this state, providing the means to avoid redundant searches for
;;; just established data.  This optimization can provide significant
;;; speed improvement, but it must be employed carefully.
;;;_   = outline-recent-prefix-beginning
(defvar outline-recent-prefix-beginning 0
  "   Buffer point of the start of the last topic prefix encountered.")
(make-variable-buffer-local 'outline-recent-prefix-beginning)
;;;_   = outline-recent-prefix-end
(defvar outline-recent-prefix-end 0
  "   Buffer point of the end of the last topic prefix encountered.")
(make-variable-buffer-local 'outline-recent-prefix-end)

;;;_  #3 Exposure Control

;;;_   : Fundamental
;;;_    > outline-flag-region (from to flag)
(defun outline-flag-region (from to flag)
  "   Hides or shows lines from FROM to TO, according to FLAG.
   Uses emacs selective-display, where text is show if FLAG put at
   beginning of line is `\\n' (newline character), while text is
   hidden if FLAG is `\\^M' (control-M).

   returns nil iff no changes were effected."
  (let ((buffer-read-only nil))
    (subst-char-in-region from to
                          (if (= flag ?\n) ?\^M ?\n)
                          flag t)))
;;;_    > outline-flag-current-subtree (flag)
(defun outline-flag-current-subtree (flag)
  (save-excursion
    (outline-back-to-current-heading)
    (outline-flag-region (point)
			  (progn (outline-end-of-current-subtree) (point))
			  flag)))

;;;_   : Topic-specific
;;;_    > outline-hide-current-entry ()
(defun outline-hide-current-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (outline-back-to-current-heading)
  (save-excursion
   (outline-flag-region (point)
                        (progn (outline-end-of-current-entry) (point))
                        ?\^M)))
;;;_    > outline-show-current-entry (&optional arg)
(defun outline-show-current-entry (&optional arg)
  "Show body directly following this heading, or hide it if repeat count."
  (interactive "P")
  (if arg
      (outline-hide-current-entry)
    (save-excursion
      (outline-flag-region (point)
                           (progn (outline-end-of-current-entry) (point))
                           ?\n))))
;;;_    > outline-show-entry ()
; outline-show-entry basically for isearch dynamic exposure, as is...
(defun outline-show-entry ()
  "   Like outline-show-current-entry, but reveals an entry that is nested
   within hidden topics."
  (interactive)
  (save-excursion
    (outline-goto-prefix)
    (outline-flag-region (if (not (bobp)) (1- (point)) (point))
                         (progn (outline-pre-next-preface) (point)) ?\n)))
;;;_    > outline-hide-current-entry-completely ()
; ... outline-hide-current-entry-completely also for isearch dynamic exposure:
(defun outline-hide-current-entry-completely ()
  "Like outline-hide-current-entry, but conceal topic completely."
  (interactive)
  (save-excursion
    (outline-goto-prefix)
    (outline-flag-region (if (not (bobp)) (1- (point)) (point))
                         (progn (outline-pre-next-preface)
                                (if (looking-at "\C-m")
                                    (point)
                                  (1- (point))))
                         ?\C-m)))
;;;_    > outline-show-current-subtree ()
(defun outline-show-current-subtree ()
  "Show everything after this heading at deeper levels."
  (interactive)
  (outline-flag-current-subtree ?\n))
;;;_    > outline-hide-current-subtree (&optional just-close)
(defun outline-hide-current-subtree (&optional just-close)

  "   Hide everything after this heading at deeper levels, or if it's
  already closed, and optional arg JUST-CLOSE is nil, hide the current
  level."

  (interactive)
  (let ((orig-eol (save-excursion
                    (end-of-line)(outline-goto-prefix)(end-of-line)(point))))
    (outline-flag-current-subtree ?\^M)
    (if (and (= orig-eol (save-excursion (goto-char orig-eol)
                                         (end-of-line)
                                         (point)))
             ;; Structure didn't change - try hiding current level:
             (if (not just-close)
                 (outline-up-current-level 1 t)))
        (outline-hide-current-subtree))))
;;;_    > outline-show-current-branches ()
(defun outline-show-current-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (outline-show-current-children 1000))
;;;_    > outline-hide-current-leaves ()
(defun outline-hide-current-leaves ()
  "Hide all body after this heading at deeper levels."
  (interactive)
  (outline-back-to-current-heading)
  (outline-hide-region-body (point) (progn (outline-end-of-current-subtree)
                                           (point))))
;;;_    > outline-show-current-children (&optional level)
(defun outline-show-current-children (&optional level)
  "  Show all direct subheadings of this heading.  Optional LEVEL specifies
   how many levels below the current level should be shown."
  (interactive "p")
  (or level (setq level 1))
  (save-excursion
   (save-restriction
    (beginning-of-line)
    (setq level (+ level (progn (outline-back-to-current-heading)
                                (outline-recent-depth))))
    (narrow-to-region (point)
		      (progn (outline-end-of-current-subtree) (1+ (point))))
    (goto-char (point-min))
    (while (and (not (eobp))
                (outline-next-heading))
      (if (<= (outline-recent-depth) level)
	  (save-excursion
	   (let ((end (1+ (point))))
	     (forward-char -1)
	     (if (memq (preceding-char) '(?\n ?\^M))
		 (forward-char -1))
	     (outline-flag-region (point) end ?\n))))))))

;;;_   : Region and beyond
;;;_    > outline-show-all ()
(defun outline-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (outline-flag-region (point-min) (point-max) ?\n))
;;;_    > outline-hide-bodies ()
(defun outline-hide-bodies ()
  "Hide all of buffer except headings."
  (interactive)
  (outline-hide-region-body (point-min) (point-max)))
;;;_    > outline-hide-region-body (start end)
(defun outline-hide-region-body (start end)
  "Hide all body lines in the region, but not headings."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (not (eobp))
	(outline-flag-region (point)
                             (progn (outline-pre-next-preface) (point)) ?\^M)
	(if (not (eobp))
	    (forward-char
	     (if (looking-at "[\n\^M][\n\^M]")
		 2 1)))))))
;;;_    > outline-expose ()
(defun outline-expose (spec &rest followers)

  "Dictate wholesale exposure scheme for current topic, according to SPEC.

SPEC is either a number or a list of specs.  Optional successive args
dictate exposure for subsequent siblings of current topic.

Numbers, the symbols '*' and '+', and the null list dictate different
exposure depths for the corresponding topic.  Numbers indicate the
depth to open, with negative numbers first forcing a close, and then
opening to their absolute value.  Positive numbers jsut reopen, and 0
just closes.  '*' completely opens the topic, including bodies, and
'+' shows all the sub headers, but not the bodies.

If the spec is a list, the first element must be a number which
dictates the exposure depth of the topic as a whole.  Subsequent
elements of the list are nested SPECs, dictating the specific exposure
for the corresponding offspring of the topic, as the SPEC as a whole
does for the parent topic.

Optional FOLLOWER elements dictate exposure for subsequent siblings
of the parent topic."

  (interactive "xExposure spec: ")
  (save-excursion
    (let ((start-point (progn (outline-goto-prefix)(point)))
          done)
      (cond ((null spec) nil)
            ((symbolp spec)
             (if (eq spec '*) (outline-show-current-subtree))
             (if (eq spec '+) (outline-show-current-branches)))
            ((numberp spec)
             (if (zerop spec)
                 ;; Just hide if zero:
                 (outline-hide-current-subtree t)
               (if (> 0 spec)
                   ;; Close before opening if negative:
                   (progn (outline-hide-current-subtree)
                          (setq spec (* -1 spec))))
               (outline-show-current-children spec)))
            ((listp spec)
             (outline-expose (car spec))
             (if (and (outline-descend-to-depth (+ (outline-current-depth) 1))
                      (not (outline-hidden-p)))
                 (while (and (setq spec (cdr spec))
                             (not done))
                   (outline-expose (car spec))
                   (setq done (not (outline-next-sibling)))))))))
  (while (and followers (outline-next-sibling))
    (outline-expose (car followers))
    (setq followers (cdr followers)))
  )
;;;_    > outline-exposure '()
(defmacro outline-exposure (&rest spec)
  "  Literal frontend for 'outline-expose', passes arguments unevaluated,
  so you needn't quote them."
  (cons 'outline-expose (mapcar '(lambda (x) (list 'quote x)) spec)))

;;;_  #4 Navigation

;;;_   : Position Assessment

;;;_    . Residual state - from most recent outline context operation.
;;;_     > outline-recent-depth ()
(defun outline-recent-depth ()
  "   Return depth of last heading encountered by an outline maneuvering
   function.

   All outline functions which directly do string matches to assess
   headings set the variables outline-recent-prefix-beginning and
   outline-recent-prefix-end if successful.  This function uses those settings
   to return the current depth."

  (max 1
       (- outline-recent-prefix-end
          outline-recent-prefix-beginning
          outline-header-subtraction)))
;;;_     > outline-recent-prefix ()
(defun outline-recent-prefix ()
  "   Like outline-recent-depth, but returns text of last encountered prefix.

   All outline functions which directly do string matches to assess
   headings set the variables outline-recent-prefix-beginning and
   outline-recent-prefix-end if successful.  This function uses those settings
   to return the current depth."
  (buffer-substring outline-recent-prefix-beginning outline-recent-prefix-end))
;;;_     > outline-recent-bullet ()
(defun outline-recent-bullet ()
  "   Like outline-recent-prefix, but returns bullet of last encountered
   prefix.

   All outline functions which directly do string matches to assess
   headings set the variables outline-recent-prefix-beginning and
   outline-recent-prefix-end if successful.  This function uses those settings
   to return the current depth of the most recently matched topic."
  (buffer-substring (1- outline-recent-prefix-end) outline-recent-prefix-end))

;;;_    . Active position evaluation - if you can't use the residual state.
;;;_     > outline-on-current-heading-p ()
(defun outline-on-current-heading-p ()
  "   Return prefix beginning point if point is on same line as current
   visible topic's header line."
  (save-excursion
    (beginning-of-line)
    (and (looking-at outline-regexp)
         (setq outline-recent-prefix-end (match-end 0)
               outline-recent-prefix-beginning (match-beginning 0)))))
;;;_     > outline-hidden-p ()
(defun outline-hidden-p ()
  "True if point is in hidden text."
  (interactive)
  (save-excursion
    (and (re-search-backward "[\C-j\C-m]" (point-min) t)
         (looking-at "\C-m"))))
;;;_     > outline-current-depth ()
(defun outline-current-depth ()
  "   Return the depth to which the current containing visible topic is
   nested in the outline."
  (save-excursion
    (if (outline-back-to-current-heading)
        (max 1
             (- outline-recent-prefix-end
                outline-recent-prefix-beginning
                outline-header-subtraction))
      0)))
;;;_     > outline-depth ()
(defun outline-depth ()
  "   Like outline-current-depth, but respects hidden as well as visible
   topics."
  (save-excursion
    (if (outline-goto-prefix)
        (outline-recent-depth)
      (progn
        (setq outline-recent-prefix-end (point)
              outline-recent-prefix-beginning (point))
        0))))
;;;_     > outline-get-current-prefix ()
(defun outline-get-current-prefix ()
  "   Topic prefix of the current topic."
  (save-excursion
    (if (outline-goto-prefix)
        (outline-recent-prefix))))
;;;_     > outline-get-bullet ()
(defun outline-get-bullet ()
  "   Return bullet of containing topic (visible or not)."
  (save-excursion
    (and (outline-goto-prefix)
         (outline-recent-bullet))))
;;;_     > outline-current-bullet ()
(defun outline-current-bullet ()
  "  Return bullet of current (visible) topic heading, or none if none found."
  (condition-case err
      (save-excursion
        (outline-back-to-current-heading)
        (buffer-substring (- outline-recent-prefix-end 1)
                          outline-recent-prefix-end))
    ;; Quick and dirty provision, ostensibly for missing bullet:
    (args-out-of-range nil))
  )
;;;_     > outline-get-prefix-bullet (prefix)
(defun outline-get-prefix-bullet (prefix)
  "   Return the bullet of the header prefix string PREFIX."
  ;; Doesn't make sense if we're old-style prefixes, but this just
  ;; oughtn't be called then, so forget about it...
  (if (string-match outline-regexp prefix)
      (substring prefix (1- (match-end 0)) (match-end 0))))

;;;_   : Within Topic
;;;_    > outline-goto-prefix ()
(defun outline-goto-prefix ()
  "  Put point at beginning of outline prefix for current topic, visible
   or not.

   Returns a list of char address of the beginning of the prefix and the
   end of it, or nil if none."

  (cond ((and (or (save-excursion (beginning-of-line) (bobp))
                  (memq (preceding-char) '(?\n ?\^M)))
              (looking-at outline-regexp))
         (setq outline-recent-prefix-end (match-end 0)
               outline-recent-prefix-beginning
               (goto-char (match-beginning 0))))
        ((re-search-backward outline-line-boundary-regexp
                             ;; unbounded search,
                             ;; stay at limit and return nil if failed:
                             nil 1)
         (setq outline-recent-prefix-end (match-end 2)
               outline-recent-prefix-beginning
               (goto-char (match-beginning 2))))
        ;; We should be at the beginning of the buffer if the last
        ;; condition failed.  line-boundary-regexp doesn't cover topic
        ;; at bob - Check for it.
        ((looking-at outline-regexp)
         (setq outline-recent-prefix-end (match-end 0)
               outline-recent-prefix-beginning
               (goto-char (match-beginning 0)))))
 )
;;;_    > outline-end-of-prefix ()
(defun outline-end-of-prefix ()
  "   Position cursor at beginning of header text."
  (if (not (outline-goto-prefix))
      nil
    (let ((match-data (match-data)))
      (goto-char (match-end 0))
      (while (looking-at "[0-9]") (forward-char 1))
      (if (and (not (eolp)) (looking-at "\\s-")) (forward-char 1))
      (store-match-data match-data))
    ;; Reestablish where we are:
    (outline-current-depth))
  )
;;;_    > outline-back-to-current-heading ()
(defun outline-back-to-current-heading ()
  "   Move to heading line of current visible topic, or beginning of heading
   if already on visible heading line."
  (beginning-of-line)
  (prog1 (or (outline-on-current-heading-p)
             (and (re-search-backward (concat "^\\(" outline-regexp "\\)")
                                      nil
                                      'move)
                  (setq outline-recent-prefix-end (match-end 1)
                        outline-recent-prefix-beginning (match-beginning 1))))
    (if (interactive-p) (outline-end-of-prefix))
    )
  )
;;;_    > outline-pre-next-preface ()
(defun outline-pre-next-preface ()
  "Skip forward to just before the next heading line.

   Returns that character position."

  (if (re-search-forward outline-line-boundary-regexp nil 'move)
      (progn (goto-char (match-beginning 0))
             (setq outline-recent-prefix-end (match-end 2)
                   outline-recent-prefix-beginning (match-beginning 2))))
  )
;;;_    > outline-end-of-current-subtree ()
(defun outline-end-of-current-subtree ()
  "  Put point at the end of the last leaf in the currently visible topic."
  (interactive)
  (outline-back-to-current-heading)
  (let ((opoint (point))
	(level (outline-recent-depth)))
    (outline-next-heading)
    (while (and (not (eobp))
                (> (outline-recent-depth) level))
      (outline-next-heading))
    (if (not (eobp)) (forward-char -1))
    (if (memq (preceding-char) '(?\n ?\^M)) (forward-char -1))))
;;;_    > outline-beginning-of-current-entry ()
(defun outline-beginning-of-current-entry ()
  "   Position the point at the beginning of the body of the current topic."
  (interactive)
  (outline-end-of-prefix))
;;;_    > outline-beginning-of-current-entry ()
(defun outline-end-of-current-entry ()
  "   Position the point at the end of the current topic's entry."
  (interactive)
  (outline-show-entry)
  (prog1 (outline-pre-next-preface)
    (if (and (not (bobp))(looking-at "^$"))
        (forward-char -1)))
)

;;;_   : Depth-wise
;;;_    > outline-ascend-to-depth (depth)
(defun outline-ascend-to-depth (depth)
  "   Ascend to depth DEPTH, returning depth if successful, nil if not."
  (if (and (> depth 0)(<= depth (outline-depth)))
      (let ((last-good (point)))
        (while (and (< depth (outline-depth))
                    (setq last-good (point))
                    (outline-beginning-of-level)
                    (outline-previous-heading)))
        (if (= (outline-recent-depth) depth)
            (progn (goto-char outline-recent-prefix-beginning)
                   depth)
          (goto-char last-good)
          nil))
    (if (interactive-p) (outline-end-of-prefix))
    )
  )
;;;_    > outline-descend-to-depth (depth)
(defun outline-descend-to-depth (depth)
  "   Descend to depth DEPTH within current topic, returning depth if
   successful, nil if not."
  (let ((start-point (point))
        (start-depth (outline-depth)))
    (while
        (and (> (outline-depth) 0)
             (not (= depth (outline-recent-depth))) ; ... not there yet
             (outline-next-heading)     ; ... go further
             (< start-depth (outline-recent-depth)))) ; ... still in topic
    (if (and (> (outline-depth) 0)
             (= (outline-recent-depth) depth))
        depth
      (goto-char start-point)
      nil))
  )
;;;_    > outline-up-current-level (arg &optional dont-complain)
(defun outline-up-current-level (arg &optional dont-complain)
  "   Move to the heading line of which the present line is a subheading.
   With argument, move up ARG levels.  Don't return an error if
   second, optional argument DONT-COMPLAIN, is non-nil."
  (interactive "p")
  (outline-back-to-current-heading)
  (let ((present-level (outline-recent-depth)))
    ;; Loop for iterating arg:
    (while (and (> (outline-recent-depth) 1)
                (> arg 0)
                (not (bobp)))
      ;; Loop for going back over current or greater depth:
      (while (and (not (< (outline-recent-depth) present-level))
                  (outline-previous-visible-heading 1)))
      (setq present-level (outline-current-depth))
      (setq arg (- arg 1)))
    )
  (prog1 (if (<= arg 0)
             outline-recent-prefix-beginning
           (if (interactive-p) (outline-end-of-prefix))
           (if (not dont-complain)
               (error "Can't ascend past outermost level.")))
    (if (interactive-p) (outline-end-of-prefix)))
  )

;;;_   : Linear
;;;_    > outline-next-visible-heading (arg)
(defun outline-next-visible-heading (arg)
  "   Move to the next visible heading line.

   With argument, repeats, backward if negative."
  (interactive "p")
  (if (< arg 0) (beginning-of-line) (end-of-line))
  (if (re-search-forward (concat "^\\(" outline-regexp "\\)")
                         nil
                         'go
                         arg)
      (progn (outline-end-of-prefix)
             (setq outline-recent-prefix-end (match-end 1)
                   outline-recent-prefix-beginning (match-beginning 1))))
  )
;;;_    > outline-previous-visible-heading (arg)
(defun outline-previous-visible-heading (arg)
  "   Move to the previous heading line.

   With argument, repeats or can move forward if negative.
   A heading line is one that starts with a `*' (or that outline-regexp
   matches)."
  (interactive "p")
  (outline-next-visible-heading (- arg))
  )
;;;_    > outline-next-heading (&optional backward)
(defun outline-next-heading (&optional backward)
  "   Move to the heading for the topic (possibly invisible) before this one.

   Optional arg BACKWARD means search for most recent prior heading.

   Returns the location of the heading, or nil if none found."

  (if (and backward (bobp))
      nil
    (if backward (outline-goto-prefix)
      (if (and (bobp) (not (eobp)))
          (forward-char 1)))

    (if (if backward
            ;; searches are unbounded and return nil if failed:
            (or (re-search-backward outline-line-boundary-regexp
                                    nil
                                    0)
                (looking-at outline-bob-regexp))
          (re-search-forward outline-line-boundary-regexp
                             nil
                             0))
        (progn;; Got some valid location state - set vars:
          (setq outline-recent-prefix-end
                (or (match-end 2) outline-recent-prefix-end))
          (goto-char (setq outline-recent-prefix-beginning
                           (or (match-beginning 2)
                               outline-recent-prefix-beginning))))
      )
    )
  )
;;;_    > outline-previous-heading ()
(defun outline-previous-heading ()
  "   Move to the next (possibly invisible) heading line.

   Optional repeat-count arg means go that number of headings.

   Return the location of the beginning of the heading, or nil if not found."

  (outline-next-heading t)
  )
;;;_    > outline-next-sibling (&optional backward)
(defun outline-next-sibling (&optional backward)
  "   Like outline-forward-current-level, but respects invisible topics.

   Go backward if optional arg BACKWARD is non-nil.

   Return depth if successful, nil otherwise."

  (if (and backward (bobp))
      nil
    (let ((start-depth (outline-depth))
          (start-point (point))
          last-good)
      (while (and (not (if backward (bobp) (eobp)))
                  (if backward (outline-previous-heading)
                    (outline-next-heading))
                  (> (outline-recent-depth) start-depth)))
      (if (and (not (eobp))
               (and (> (outline-depth) 0)
                    (= (outline-recent-depth) start-depth)))
          outline-recent-prefix-beginning
        (goto-char start-point)
        nil)
      )
    )
  )
;;;_    > outline-previous-sibling (&optional arg)
(defun outline-previous-sibling (&optional arg)
  "   Like outline-forward-current-level, but goes backwards and respects
   invisible topics.

   Optional repeat count means go number backward.

   Note that the beginning of a level is (currently) defined by this
   implementation to be the first of previous successor topics of
   equal or greater depth.

   Return depth if successful, nil otherwise."
  (outline-next-sibling t)
  )
;;;_    > outline-beginning-of-level ()
(defun outline-beginning-of-level ()
  "   Go back to the first sibling at this level, visible or not."
  (outline-end-of-level 'backward))
;;;_    > outline-end-of-level (&optional backward)
(defun outline-end-of-level (&optional backward)
  "   Go to the last sibling at this level, visible or not."

  (while (outline-previous-sibling))
  (prog1 (outline-recent-depth)
    (if (interactive-p) (outline-end-of-prefix)))
)
;;;_    > outline-forward-current-level (arg &optional backward)
(defun outline-forward-current-level (arg &optional backward)
  "   Position the point at the next heading of the same level, taking
   optional repeat-count.

   Returns that position, else nil if is not found."
  (interactive "p")
  (outline-back-to-current-heading)
      (let ((amt (if arg (if (< arg 0)
                             ;; Negative arg - invert direction.
                             (progn (setq backward (not backward))
                                    (abs arg))
                           arg);; Positive arg - just use it.
                   1)));; No arg - use 1:
        (while (and (> amt 0)
                    (outline-next-sibling backward))
          (setq amt (1- amt)))
        (if (interactive-p) (outline-end-of-prefix))
        (if (> amt 0)
            (error "This is the %s topic on level %d."
                   (if backward "first" "last")
                   (outline-current-depth))
          t)
        )
  )
;;;_    > outline-backward-current-level (arg)
(defun outline-backward-current-level (arg)
  "   Position the point at the previous heading of the same level, taking
   optional repeat-count.

   Returns that position, else nil if is not found."
  (interactive "p")
  (unwind-protect
      (outline-forward-current-level arg t)
    (outline-end-of-prefix))
)

;;;_   : Search with Dynamic Exposure (requires isearch-mode)
;;;_    = outline-search-reconceal
(defvar outline-search-reconceal nil
  "Used for outline isearch provisions, to track whether current search
match was concealed outside of search.  The value is the location of the
match, if it was concealed, regular if the entire topic was concealed, in
a list if the entry was concealed.")
;;;_    = outline-search-quitting
(defconst outline-search-quitting nil
  "Variable used by isearch-terminate/outline-provisions and
isearch-done/outline-provisions to distinguish between a conclusion
and cancellation of a search.")

;;;_    > outline-enwrap-isearch ()
(defun outline-enwrap-isearch ()
  "   Impose isearch-mode wrappers so isearch progressively exposes and
   reconceals hidden topics when working in outline mode, but works
   elsewhere.

   The function checks to ensure that the rebindings are done only once."

                                        ; Should isearch-mode be employed,
  (if (or (not outline-enwrap-isearch-mode)
                                        ; or are preparations already done?
          (fboundp 'real-isearch-terminate))

      ;; ... no - skip this all:
      nil

    ;; ... yes:

                                        ; Ensure load of isearch-mode:
    (if (or (and (fboundp 'isearch-mode)
                 (fboundp 'isearch-quote-char))
            (condition-case error 
                (load-library outline-enwrap-isearch-mode)
              (file-error (message "Skipping isearch-mode provisions - %s '%s'"
                                   (car (cdr error))
                                   (car (cdr (cdr error))))
                          (sit-for 1)
                          ;; Inhibit subsequent tries and return nil:
                          (setq outline-enwrap-isearch-mode nil))))
        ;; Isearch-mode loaded, encapsulate specific entry points for
        ;; outline dynamic-exposure business:
        (progn 
                
                                        ; stash crucial isearch-mode
                                        ; funcs under known, private
                                        ; names, then register wrapper
                                        ; functions under the old
                                        ; names, in their stead:
                                        ; 'isearch-quit' is pre v 1.2:
          (fset 'real-isearch-terminate
                                        ; 'isearch-quit is pre v 1.2:
                (or (if (fboundp 'isearch-quit)
                        (symbol-function 'isearch-quit))
                    (if (fboundp 'isearch-abort)
                                        ; 'isearch-abort' is v 1.2 and on:
                        (symbol-function 'isearch-abort))))
          (fset 'isearch-quit 'isearch-terminate/outline-provisions)
          (fset 'isearch-abort 'isearch-terminate/outline-provisions)
          (fset 'real-isearch-done (symbol-function 'isearch-done))
          (fset 'isearch-done 'isearch-done/outline-provisions)
          (fset 'real-isearch-update (symbol-function 'isearch-update))
          (fset 'isearch-update 'isearch-update/outline-provisions)
          (make-variable-buffer-local 'outline-search-reconceal))
      )
    )
  )
;;;_    > outline-isearch-arrival-business ()
(defun outline-isearch-arrival-business ()
  "   Do outline business like exposing current point, if necessary,
   registering reconcealment requirements in outline-search-reconceal
   accordingly.

   Set outline-search-reconceal to nil if current point is not
   concealed, to value of point if entire topic is concealed, and a
   list containing point if only the topic body is concealed.

   This will be used to determine whether outline-hide-current-entry
   or outline-hide-current-entry-completely will be necessary to
   restore the prior concealment state."

  (if (and (boundp 'outline-mode) outline-mode)
      (setq outline-search-reconceal
            (if (outline-hidden-p)
                (save-excursion
                  (if (re-search-backward outline-line-boundary-regexp nil 1)
                      ;; Nil value means we got to b-o-b - wouldn't need
                      ;; to advance.
                      (forward-char 1))
                                        ; We'll return point or list
                                        ; containing point, depending
                                        ; on concealment state of
                                        ; topic prefix.
                  (prog1 (if (outline-hidden-p) (point) (list (point)))
                                        ; And reveal the current
                                        ; search target:
                    (outline-show-entry)))))))
;;;_    > outline-isearch-advancing-business ()
(defun outline-isearch-advancing-business ()
  "   Do outline business like deexposing current point, if necessary,
   according to reconceal state registration."
  (if (and (boundp 'outline-mode) outline-mode outline-search-reconceal)
      (save-excursion
        (if (listp outline-search-reconceal)
            ;; Leave the topic visible:
            (progn (goto-char (car outline-search-reconceal))
                   (outline-hide-current-entry))
          ;; Rehide the entire topic:
          (goto-char outline-search-reconceal)
          (outline-hide-current-entry-completely))))
  )
;;;_    > isearch-terminate/outline-provisions ()
(defun isearch-terminate/outline-provisions ()
  (interactive)
    (if (and (boundp 'outline-mode)
             outline-mode
             outline-enwrap-isearch-mode)
        (outline-isearch-advancing-business))
    (let ((outline-search-quitting t)
          (outline-search-reconceal nil))
      (real-isearch-terminate)))
;;;_    > isearch-done/outline-provisions ()
(defun isearch-done/outline-provisions (&optional nopush)
  (interactive)
  (if (and (boundp 'outline-mode)
           outline-mode
           outline-enwrap-isearch-mode)
      (progn (save-excursion
               (if (and outline-search-reconceal
                        (not (listp outline-search-reconceal)))
                   ;; The topic was concealed - reveal it, its siblings,
                   ;; and any ancestors that are still concealed:
                   (progn
                     (message "(exposing destination)")(sit-for 0)
                     ;; Ensure target topic's siblings are exposed:
                     (outline-ascend-to-depth (1- (outline-current-depth)))
                     ;; Ensure that the target topic's ancestors are exposed
                     (while (outline-hidden-p)
                       (outline-show-current-children))
                     (outline-show-current-children)
                     (outline-show-current-entry)))
               (outline-isearch-arrival-business))
             (if (not (and (boundp 'outline-search-quitting)
                           outline-search-quitting))
                 (outline-show-current-children))))
  (if nopush
      ;; isearch-done in newer version of isearch mode takes arg:
      (real-isearch-done nopush)
    (real-isearch-done)))
;;;_    > isearch-update/outline-provisions ()
(defun isearch-update/outline-provisions ()
  "    Wrapper around isearch which exposes and conceals hidden outline
   portions encountered in the course of searching."
  (if (not (and (boundp 'outline-mode)
                outline-mode
                outline-enwrap-isearch-mode))
      ;; Just do the plain business:
      (real-isearch-update)

    ;; Ah - provide for outline conditions:
    (outline-isearch-advancing-business)
    (real-isearch-update)
    (cond (isearch-success (outline-isearch-arrival-business))
          ((not isearch-success) (outline-isearch-advancing-business)))
    )
  )

;;;_  #5 Manipulation

;;;_   : Topic Format Assessment
;;;_    > outline-solicit-alternate-bullet (depth &optional current-bullet)
(defun outline-solicit-alternate-bullet (depth &optional current-bullet)

  "   Prompt for and return a bullet char as an alternative to the
   current one, but offer one suitable for current depth DEPTH
   as default."

  (let* ((default-bullet (or current-bullet
                             (outline-bullet-for-depth depth)))
	 (choice (solicit-char-in-string
                  (format "Select bullet: %s ('%s' default): "
                          outline-bullets-string
                          default-bullet)
                  (string-sans-char outline-bullets-string ?\\)
                  t)))
    (if (string= choice "") default-bullet choice))
  )
;;;_    > outline-sibling-index (&optional depth)
(defun outline-sibling-index (&optional depth)
  "   Item number of this prospective topic among it's siblings.

   If optional arg depth is greater than current depth, then we're
   opening a new level, and return 0.

   If less than this depth, ascend to that depth and count..."

  (save-excursion
    (cond ((and depth (<= depth 0) 0))
          ((or (not depth) (= depth (outline-depth)))
           (let ((index 1))
             (while (outline-previous-sibling) (setq index (1+ index)))
             index))
          ((< depth (outline-recent-depth))
           (outline-ascend-to-depth depth)
           (outline-sibling-index))
          (0))))
;;;_    > outline-distinctive-bullet (bullet)
(defun outline-distinctive-bullet (bullet)
  "   True if bullet is one of those on outline-distinctive-bullets-string."
  (string-match (regexp-quote bullet) outline-distinctive-bullets-string))
;;;_    > outline-numbered-type-prefix (&optional prefix)
(defun outline-numbered-type-prefix (&optional prefix)
  "   True if current header prefix bullet is numbered bullet."
  (and outline-numbered-bullet
        (string= outline-numbered-bullet
                 (if prefix
                     (outline-get-prefix-bullet prefix)
                   (outline-get-bullet)))))
;;;_    > outline-bullet-for-depth (&optional depth)
(defun outline-bullet-for-depth (&optional depth)
  "   Return outline topic bullet suited to DEPTH, or for current depth if none
   specified."
  ;; Find bullet in plain-bullets-string modulo DEPTH.
  (if outline-stylish-prefixes
      (char-to-string (aref outline-plain-bullets-string
                            (% (max 0 (- depth 2))
                               outline-plain-bullets-string-len)))
    outline-primary-bullet)
  )

;;;_   : Topic Production
;;;_    > outline-make-topic-prefix (&optional prior-bullet
(defun outline-make-topic-prefix (&optional prior-bullet
                                            new
                                            depth
                                            solicit
                                            number-control
                                            index)
  ;; Depth null means use current depth, non-null means we're either
  ;; opening a new topic after current topic, lower or higher, or we're
  ;; changing level of current topic.
  ;; Solicit dominates specified bullet-char.
  "   Generate a topic prefix suitable for optional arg DEPTH, or current
   depth if not specified.

   All the arguments are optional.

   PRIOR-BULLET indicates the bullet of the prefix being changed, or
   nil if none.  This bullet may be preserved (other options
   notwithstanding) if it is on the outline-distinctive-bullets-string,
   for instance.

   Second arg NEW indicates that a new topic is being opened after the
   topic at point, if non-nil.  Default bullet for new topics, eg, may
   be set (contingent to other args) to numbered bullets if previous
   sibling is one.  The implication otherwise is that the current topic
   is being adjusted - shifted or rebulleted - and we don't consider
   bullet or previous sibling.

   Third arg DEPTH forces the topic prefix to that depth, regardless of
   the current topics' depth.

   Fourth arg SOLICIT non-nil provokes solicitation from the user of a
   choice among the valid bullets.  (This overrides other all the
   options, including, eg, a distinctive PRIOR-BULLET.)

   Fifth arg, NUMBER-CONTROL, matters only if 'outline-numbered-bullet'
   is non-nil *and* soliciting was not explicitly invoked.  Then
   NUMBER-CONTROL non-nil forces prefix to either numbered or
   denumbered format, depending on the value of the sixth arg, INDEX.

   (Note that NUMBER-CONTROL does *not* apply to level 1 topics.  Sorry...)

   If NUMBER-CONTROL is non-nil and sixth arg INDEX is non-nil then
   the prefix of the topic is forced to be numbered.  Non-nil
   NUMBER-CONTROL and nil INDEX forces non-numbered format on the
   bullet.  Non-nil NUMBER-CONTROL and non-nil, non-number INDEX means
   that the index for the numbered prefix will be derived, by counting
   siblings back to start of level.  If INDEX is a number, then that
   number is used as the index for the numbered prefix (allowing, eg,
   sequential renumbering to not require this function counting back the
   index for each successive sibling)."

  ;; The options are ordered in likely frequence of use, most common
  ;; highest, least lowest.  Ie, more likely to be doing prefix
  ;; adjustments than soliciting, and yet more than numbering.
  ;; Current prefix is least dominant, but most likely to be commonly
  ;; specified...

  (let* (body
         numbering
         denumbering
         (depth (or depth (outline-depth)))
         (header-lead outline-header-prefix)
         (bullet-char

          ;; Getting value for bullet char is practically the whole job:

          (cond
                                        ; Simplest situation - level 1:
           ((<= depth 1) (setq header-lead "") outline-primary-bullet)
                                        ; Simple, too: all asterisks:
           (outline-old-style-prefixes
            ;; Cheat - make body the whole thing, null out header-lead and
            ;; bullet-char:
            (setq body (make-string depth
                                    (string-to-char outline-primary-bullet)))
            (setq header-lead "")
            "")

           ;; (Neither level 1 nor old-style, so we're space padding.
           ;; Sneak it in the condition of the next case, whatever it is.)

           ;; Solicitation overrides numbering and other cases:
           ((progn (setq body (make-string (- depth 2) ?\ ))
                   ;; The actual condition:
                   solicit)
            (let* ((got (outline-solicit-alternate-bullet depth)))
              ;; Gotta check whether we're numbering and got a numbered bullet:
              (setq numbering (and outline-numbered-bullet
                                   (not (and number-control (not index)))
                                   (string= got outline-numbered-bullet)))
              ;; Now return what we got, regardless:
              got))

           ;; Numbering invoked through args:
           ((and outline-numbered-bullet number-control)
            (if (setq numbering (not (setq denumbering (not index))))
                outline-numbered-bullet
              (if (and current-bullet
                       (not (string= outline-numbered-bullet
                                     current-bullet)))
                  current-bullet
                (outline-bullet-for-depth depth))))

          ;;; Neither soliciting nor controlled numbering ;;;
             ;;; (may be controlled denumbering, tho) ;;;

           ;; Check wrt previous sibling:
           ((and new				  ; only check for new prefixes
                 (<= depth (outline-depth))
                 outline-numbered-bullet	      ; ... & numbering enabled
                 (not denumbering)
                 (let ((sibling-bullet
                        (save-excursion
                          ;; Locate correct sibling:
                          (or (>= depth (outline-depth))
                              (outline-ascend-to-depth depth))
                          (outline-get-bullet))))
                   (if (and sibling-bullet
                            (string= outline-numbered-bullet sibling-bullet))
                       (setq numbering sibling-bullet)))))

           ;; Distinctive prior bullet?
           ((and prior-bullet
                 (outline-distinctive-bullet prior-bullet)
                 ;; Either non-numbered:
                 (or (not (and outline-numbered-bullet
                               (string= prior-bullet outline-numbered-bullet)))
                     ;; or numbered, and not denumbering:
                     (setq numbering (not denumbering)))
                 ;; Here 'tis:
                 prior-bullet))

           ;; Else, standard bullet per depth:
           ((outline-bullet-for-depth depth)))))

    (concat header-lead
            body
            bullet-char
            (if numbering
                (format "%d" (cond ((and index (numberp index)) index)
                                   (new (1+ (outline-sibling-index depth)))
                                   ((outline-sibling-index))))))
    )
  )
;;;_    > open-topic (relative-depth &optional before)
(defun open-topic (relative-depth &optional before)
  " Open a new topic at depth DEPTH.  New topic is situated after current
  one, unless optional flag BEFORE is non-nil, or unless current line
  is complete empty (not even whitespace), in which case open is done
  on current line.

  Nuances:

   - Creation of new topics is with respect to the visible topic
     containing the cursor, regardless of intervening concealed ones.

   - New headers are generally created after/before the body of a
     topic.  However, they are created right at cursor location if the
     cursor is on a blank line, even if that breaks the current topic
     body.  This is intentional, to provide a simple means for
     deliberately dividing topic bodies.

   - Double spacing of topic lists is preserved.  Also, the first
     level two topic is created double-spaced (and so would be
     subsequent siblings, if that's left intact).  Otherwise,
     single-spacing is used.

   - Creation of sibling or nested topics is with respect to the topic
     you're starting from, even when creating backwards.  This way you
     can easily create a sibling in front of the current topic without
     having to go to its preceding sibling, and then open forward
     from there."

  (let* ((depth (+ (outline-current-depth) relative-depth))
         (opening-on-blank (if (looking-at "^\$")
                               (not (setq before nil))))
         opening-numbered	; Will get while computing ref-topic, below
         ref-depth		; Will get while computing ref-topic, next
         (ref-topic (save-excursion
                      (cond ((< relative-depth 0)
                             (outline-ascend-to-depth depth))
                            ((>= relative-depth 1) nil)
                            (t (outline-back-to-current-heading)))
                      (setq ref-depth (outline-recent-depth))
                      (setq opening-numbered
                            (save-excursion
                              (and outline-numbered-bullet
                                   (or (<= relative-depth 0)
                                       (outline-descend-to-depth depth))
                                   (if (outline-numbered-type-prefix)
                                       outline-numbered-bullet))))
                      (point)))
         dbl-space
         doing-beginning
         )

    (if (not opening-on-blank)
                                        ; Positioning and vertical
                                        ; padding - only if not
                                        ; opening-on-blank:
        (progn 
          (goto-char ref-topic)
          (setq dbl-space               ; Determine double space action:
                (or (and (not (> relative-depth 0))
                         ;; not descending,
                         (save-excursion
                           ;; preceded by a blank line?
                           (forward-line -1)
                           (looking-at "^\\s-*$")))
                    (and (= ref-depth 1)
                         (or before
                             (= depth 1)
                             (save-excursion
                               ;; Don't already have following
                               ;; vertical padding:
                               (not (outline-pre-next-preface)))))))

                                        ; Position to prior heading,
                                        ; if inserting backwards:
          (if before (progn (outline-back-to-current-heading)
                            (setq doing-beginning (bobp))
                            (if (and (not (outline-previous-sibling))
                                     (not (bobp)))
                                (outline-previous-heading))))

          (if (and (<= depth ref-depth)
                   (= ref-depth (outline-current-depth)))
              ;; Not going inwards, don't snug up:
              (if doing-beginning
                  (open-line (if dbl-space 2 1))
                (outline-end-of-current-subtree))
            ;; Going inwards - double-space if first offspring is,
            ;; otherwise snug up.
            (end-of-line)		; So we skip any concealed progeny.
            (outline-pre-next-preface)
            (if (bolp)
                ;; Blank lines between current header body and next
                ;; header - get to last substantive (non-white-space)
                ;; line in body:
                (re-search-backward "[^ \t\n]" nil t))
            (if (save-excursion
                  (outline-next-heading)
                  (if (> (outline-recent-depth) ref-depth)
                      ;; This is an offspring.
                      (progn (forward-line -1)
                             (looking-at "^\\s-*$"))))
                (progn (forward-line 1)
                       (open-line 1)))
            (end-of-line))
          ;;(if doing-beginning (goto-char doing-beginning))
          (if (not (bobp)) (newline (if dbl-space 2 1)))
          ))
    (insert-string (concat (outline-make-topic-prefix opening-numbered
                                                      t
                                                      depth)
                           " "))

    ;;(if doing-beginning (save-excursion (newline (if dbl-space 2 1))))


    (outline-rebullet-heading nil		;;; solicit
                              depth 		;;; depth
                              nil 		;;; number-control
                              nil		;;; index
                              t)     (end-of-line)
    )
  )
;;;_    > open-subtopic (arg)
(defun open-subtopic (arg)
  "   Open new topic header at deeper level than the current one.

  Negative universal arg means to open deeper, but place the new topic
  prior to the current one."
  (interactive "p")
  (open-topic 1 (> 0 arg)))
;;;_    > open-sibtopic (arg)
(defun open-sibtopic (arg)
  "   Open new topic header at same level as the current one.  Negative
  universal arg means to place the new topic prior to the current
  one."
  (interactive "p")
  (open-topic 0 (> 0 arg)))
;;;_    > open-supertopic (arg)
(defun open-supertopic (arg)
  "   Open new topic header at shallower level than the current one.
  Negative universal arg means to open shallower, but place the new
  topic prior to the current one."

  (interactive "p")
  (open-topic -1 (> 0 arg)))

;;;_   : Outline Alteration
;;;_    . Topic Form Modification
;;;_     > outline-reindent-body (old-depth new-depth)
(defun outline-reindent-body (old-depth new-depth)
  "  Reindent body lines which were indented at old-depth to new-depth.

  Note that refill of indented paragraphs is not done, and tabs are
  not accommodated.  ('untabify' your outline if you want to preserve
  hanging body indents.)"

  (save-excursion
    (save-restriction
      (outline-goto-prefix)
      (forward-char 1)
      (let* ((old-spaces-expr (make-string (1+ old-depth) ?\ ))
             (new-spaces-expr (concat (make-string (1+ new-depth) ?\ )
                                      ;; spaces followed by non-space:
                                      "\\1")))
        (while (and (re-search-forward "[\C-j\C-m]" nil t)
                    (not (looking-at outline-regexp)))
          (if (looking-at old-spaces-expr)
              (replace-match new-spaces-expr)))))))
;;;_     > outline-rebullet-current-heading (arg)
(defun outline-rebullet-current-heading (arg)
  "   Like non-interactive version 'outline-rebullet-heading', but work on
   (only) visible heading containing point.

   With repeat count, solicit for bullet."
  (interactive "P")
  (save-excursion (outline-back-to-current-heading)
                  (outline-end-of-prefix)
                  (outline-rebullet-heading (not arg)	;;; solicit
                                            nil		;;; depth
                                            nil		;;; number-control
                                            nil		;;; index
                                            t)		;;; do-successors
                  )
  )
;;;_     > outline-rebullet-heading (&optional solicit ...)
(defvar current-bullet nil
  "Variable local to outline-rebullet-heading,but referenced by
outline-make-topic-prefix, also.  Should be resolved with explicitly
parameterized communication between the two, if suitable.")
(defun outline-rebullet-heading (&optional solicit
                                           new-depth
                                           number-control
                                           index
                                           do-successors)

  "   Adjust bullet of current topic prefix.

   All args are optional.

   If SOLICIT is non-nil then the choice of bullet is solicited from
   user.  Otherwise the distinctiveness of the bullet or the topic
   depth determines it.

   Second arg DEPTH forces the topic prefix to that depth, regardless
   of the topic's current depth.

   Third arg NUMBER-CONTROL can force the prefix to or away from
   numbered form.  It has effect only if 'outline-numbered-bullet' is
   non-nil and soliciting was not explicitly invoked (via first arg).
   Its effect, numbering or denumbering, then depends on the setting
   of the forth arg, INDEX.

   If NUMBER-CONTROL is non-nil and forth arg INDEX is nil, then the
   prefix of the topic is forced to be non-numbered.  Null index and
   non-nil NUMBER-CONTROL forces denumbering.  Non-nil INDEX (and
   non-nil NUMBER-CONTROL) forces a numbered-prefix form.  If non-nil
   INDEX is a number, then that number is used for the numbered
   prefix.  Non-nil and non-number means that the index for the
   numbered prefix will be derived by outline-make-topic-prefix.

   Fifth arg DO-SUCCESSORS t means re-resolve count on succeeding
   siblings.

   Cf vars 'outline-stylish-prefixes', 'outline-old-style-prefixes',
   and 'outline-numbered-bullet', which all affect the behavior of
   this function."

  (let* ((current-depth (outline-depth))
         (new-depth (or new-depth current-depth))
         (mb outline-recent-prefix-beginning)
         (me outline-recent-prefix-end)
         (current-bullet (buffer-substring (- me 1) me))
         (new-prefix (outline-make-topic-prefix current-bullet
                                                nil
                                                new-depth
                                                solicit
                                                number-control
                                                index)))

    ;; Don't need to reinsert identical one:
    (if (and (= current-depth new-depth)
             (string= current-bullet
                      (substring new-prefix (1- (length new-prefix)))))
        t

      ;; New prefix probably different from old:
      ;; get rid of old one:
      (delete-region mb me)
      (goto-char mb)
      ;; Dispense with number if numbered-bullet prefix:
      (if (and outline-numbered-bullet
               (string= outline-numbered-bullet current-bullet)
               (looking-at "[0-9]+"))
          (delete-region (match-beginning 0)(match-end 0)))

      ;; Put in new prefix:
      (insert-string new-prefix)
      )

    ;; Reindent the body if elected and depth changed:
    (if (and outline-reindent-bodies
             (not (= new-depth current-depth)))
        (outline-reindent-body current-depth new-depth))

    ;; Recursively rectify successive siblings if selected:
    (if do-successors
        (save-excursion
          (while (outline-next-sibling)
            (setq index
                  (cond ((numberp index) (1+ index))
                        ((not number-control)  (outline-sibling-index))))
            (if (outline-numbered-type-prefix)
                (outline-rebullet-heading nil		;;; solicit
                                          new-depth	;;; new-depth
                                          number-control;;; number-control
                                          index		;;; index
                                          nil)))))	;;;(dont!)do-successors
      )
  )
;;;_     > outline-rebullet-topic (arg)
(defun outline-rebullet-topic (arg)
  "   Like outline-rebullet-topic-grunt, but start from topic visible at point.
   Descends into invisible as well as visible topics, however.

   With repeat count, shift topic depth by that amount."
  (interactive "P")
  (let ((start-col (current-column))
        (was-eol (eolp)))
    (save-excursion
      ;; Normalize arg:
      (cond ((null arg) (setq arg 0))
            ((listp arg) (setq arg (car arg))))
      ;; Fill the user in, in case we're shifting a big topic:
      (if (not (zerop arg)) (message "Shifting..."))
      (outline-back-to-current-heading)
      (if (<= (+ (outline-recent-depth) arg) 0)
          (error "Attempt to shift topic below level 1"))
      (outline-rebullet-topic-grunt arg)
      (if (not (zerop arg)) (message "Shifting... done.")))
    (move-to-column (max 0 (+ start-col arg))))
  )
;;;_      > outline-rebullet-topic-grunt (&optional relative-depth ...)
(defun outline-rebullet-topic-grunt (&optional relative-depth
                                               starting-depth
                                               starting-point
                                               index
                                               do-successors)

  "   Rebullet the topic at point, visible or invisible, and all
   contained subtopics.  See outline-rebullet-heading for rebulleting
   behavior.

   All arguments are optional.

   First arg RELATIVE-DEPTH means to shift the depth of the entire
   topic that amount.

   The rest of the args are for internal recursive use by the function
   itself.  The are STARTING-DEPTH, STARTING-POINT, and INDEX."

  (let* ((relative-depth (or relative-depth 0))
         (new-depth (outline-depth))
         (starting-depth (or starting-depth new-depth))
         (on-starting-call  (null starting-point))
         (index (or index
                    ;; Leave index null on starting call, so rebullet-heading
                    ;; calculates it at what might be new depth:
                    (and (or (zerop relative-depth)
                             (not on-starting-call))
                         (outline-sibling-index))))
         (moving-outwards (< 0 relative-depth))
         (starting-point (or starting-point (point))))

    ;; Sanity check for excessive promotion done only on starting call:
    (and on-starting-call
         moving-outwards
         (> 0 (+ starting-depth relative-depth))
         (error "Attempt to shift topic out beyond level 1."))	;;; ====>

    (cond ((= starting-depth new-depth)
           ;; We're at depth to work on this one:
           (outline-rebullet-heading nil		;;; solicit
                                     (+ starting-depth	;;; starting-depth
                                        relative-depth)
                                     nil		;;; number
                                     index		;;; index
                                     ;; Every contained topic will get hit,
                                     ;; and we have to get to outside ones
                                     ;; deliberately:
                                     nil)		;;; do-successors
           ;; ... and work on subsequent ones which are at greater depth:
           (setq index 0)
           (outline-next-heading)
           (while (and (not (eobp))
                       (< starting-depth (outline-recent-depth)))
             (setq index (1+ index))
             (outline-rebullet-topic-grunt relative-depth   ;;; relative-depth
                                           (1+ starting-depth);;;starting-depth
                                           starting-point   ;;; starting-point
                                           index)))	    ;;; index

          ((< starting-depth new-depth)
           ;; Rare case - subtopic more than one level deeper than parent.
           ;; Treat this one at an even deeper level:
           (outline-rebullet-topic-grunt relative-depth   ;;; relative-depth
                                         new-depth	  ;;; starting-depth
                                         starting-point	  ;;; starting-point
                                         index)))	  ;;; index

    (if on-starting-call
        (progn
          ;; Rectify numbering of former siblings of the adjusted topic,
          ;; if topic has changed depth
          (if (or do-successors
                  (and (not (zerop relative-depth))
                       (or (= (outline-recent-depth) starting-depth)
                           (= (outline-recent-depth) (+ starting-depth
                                                        relative-depth)))))
              (outline-rebullet-heading nil nil nil nil t))
          ;; Now rectify numbering of new siblings of the adjusted topic,
          ;; if depth has been changed:
          (progn (goto-char starting-point)
                 (if (not (zerop relative-depth))
                     (outline-rebullet-heading nil nil nil nil t)))))
    )
  )
;;;_     > outline-number-siblings (&optional denumber)
(defun outline-number-siblings (&optional denumber)
  "   Assign numbered topic prefix to this topic and its siblings.

   With universal argument, denumber - assign default bullet to this
   topic and its siblings.

   With repeated universal argument (`^U^U'), solicit bullet for each
   rebulleting each topic at this level."

  (interactive "P")

  (save-excursion
    (outline-back-to-current-heading)
    (outline-beginning-of-level)
    (let ((index (if (not denumber) 1))
          (use-bullet (equal '(16) denumber))
          (more t))
      (while more
        (outline-rebullet-heading use-bullet		;;; solicit
                                  nil			;;; depth
                                  t			;;; number-control
                                  index			;;; index
                                  nil)			;;; do-successors
        (if index (setq index (1+ index)))
        (setq more (outline-next-sibling)))
      )
    )
  )
;;;_     > outline-shift-in (arg)
(defun outline-shift-in (arg)
  "   Decrease prefix depth of current heading and any topics collapsed
   within it."
  (interactive "p")
  (outline-rebullet-topic arg))
;;;_     > outline-shift-out (arg)
(defun outline-shift-out (arg)
  "   Decrease prefix depth of current heading and any topics collapsed
   within it."
  (interactive "p")
  (outline-rebullet-topic (* arg -1)))
;;;_    . Surgery (kill-ring) functions with special provisions for outlines:
;;;_     > outline-kill-line (&optional arg)
(defun outline-kill-line (&optional arg)
  "   Kill line, adjusting subsequent lines suitably for outline mode."

  (interactive "*P")
  (if (not (and
            (boundp 'outline-mode) outline-mode		; active outline mode,
            outline-numbered-bullet		; numbers may need adjustment,
            (bolp)				; may be clipping topic head,
            (looking-at outline-regexp)))	; are clipping topic head.
      ;; Above conditions do not obtain - just do a regular kill:
      (kill-line arg)
    ;; Ah, have to watch out for adjustments:
    (let* ((depth (outline-depth))
           (ascender depth))
      (kill-line arg)
      (sit-for 0)
      (save-excursion
        (if (not (looking-at outline-regexp))
            (outline-next-heading))
        (if (> (outline-depth) depth)
            ;; An intervening parent was removed from after a subtree:
            (setq depth (outline-recent-depth)))
        (while (and (> (outline-depth) 0)
                    (> (outline-recent-depth) ascender)
                    (outline-ascend-to-depth (setq ascender
                                                   (1- ascender)))))
        ;; Have to try going forward until we find another at
        ;; desired depth:
        (if (and outline-numbered-bullet
                 (outline-descend-to-depth depth))
            (outline-rebullet-heading nil		;;; solicit
                                      depth		;;; depth
                                      nil 		;;; number-control
                                      nil		;;; index
                                      t)		;;; do-successors
          )
        )
      )
    )
  )
;;;_     > outline-kill-topic ()
(defun outline-kill-topic ()
  "   Kill topic together with subtopics."

  ;; Some finagling is done to make complex topic kills appear faster
  ;; than they actually are.  A redisplay is performed immediately
  ;; after the region is disposed of, though the renumbering process
  ;; has yet to be performed.  This means that there may appear to be
  ;; a lag *after* the kill has been performed.

  (interactive)
  (let* ((beg (outline-back-to-current-heading))
         (depth (outline-recent-depth)))
    (outline-end-of-current-subtree)
    (if (not (eobp))
        (forward-char 1))
    (kill-region beg (point))
    (sit-for 0)
    (save-excursion
      (if (and outline-numbered-bullet
               (outline-descend-to-depth depth))
          (outline-rebullet-heading nil		;;; solicit
                                    depth	;;; depth
                                    nil		;;; number-control
                                    nil		;;; index
                                    t)		;;; do-successors
        )
      )
    )
  )
;;;_     > outline-yank (&optional arg)
(defun outline-yank (&optional arg)
  "   Like regular yank, except does depth adjustment of yanked topics, when:

   1 the stuff being yanked starts with a valid outline header prefix, and
   2 it is being yanked at the end of a line which consists of only a valid
     topic prefix.

   If these two conditions hold then the depth of the yanked topics
   are all adjusted the amount it takes to make the first one at the
   depth of the header into which it's being yanked.

   The point is left in from of yanked, adjusted topics, rather than
   at the end (and vice-versa with the mark).  Non-adjusted yanks,
   however, (ones that don't qualify for adjustment) are handled
   exactly like normal yanks.

   Outline-yank-pop is used with outline-yank just as normal yank-pop
   is used with normal yank in non-outline buffers."

  (interactive "*P")
  (setq this-command 'yank)
  (if (not (and (boundp 'outline-mode) outline-mode))

      ;; Outline irrelevant - just do regular yank:
      (yank arg)

    ;; Outline *is* relevant:
    (let ((beginning (point))
          topic-yanked
          established-depth)   ; Depth of the prefix into which we're yanking.
      ;; Get current depth and numbering ... Oops, not doing anything
      ;; with the number just yet...
      (if (and (eolp)
               (save-excursion (beginning-of-line)
                               (looking-at outline-regexp)))
          (setq established-depth (- (match-end 0) (match-beginning 0))))
      (yank arg)
      (exchange-dot-and-mark)
      (if (and established-depth        ; the established stuff qualifies.
               ;; The yanked stuff also qualifies - is topic(s):
               (looking-at (concat "\\(" outline-regexp "\\)")))
          ;; Ok, adjust the depth of the yanked stuff.  Note that the
          ;; stuff may have more than a single root, so we have to
          ;; iterate over all the top level ones yanked, and do them in
          ;; such a way that the adjustment of one new one won't affect
          ;; any of the other new ones.  We use the focus of the
          ;; narrowed region to successively exclude processed siblings.
          (let* ((yanked-beg (match-beginning 1))
                 (yanked-end (match-end 1))
                 (yanked-bullet (buffer-substring (1- yanked-end) yanked-end))
                 (yanked-depth (- yanked-end yanked-beg))
                 (depth-diff (- established-depth yanked-depth))
                 done
                 (more t))
            (setq topic-yanked t)
            (save-excursion
              (save-restriction
                (narrow-to-region yanked-beg (mark))
                ;; First trim off excessive blank line at end, if any:
                (goto-char (point-max))
                (if (looking-at "^$") (delete-char -1))
                (goto-char (point-min))
                ;; Work backwards, with each shallowest level,
                ;; successively excluding the last processed topic
                ;; from the narrow region:
                (goto-char (point-max))
                (while more
                  (outline-back-to-current-heading)
                  ;; go as high as we can in each bunch:
                  (while (outline-ascend-to-depth
                          (1- (outline-depth))))
                  (save-excursion
                    (outline-rebullet-topic-grunt depth-diff
                                                  (outline-depth)
                                                  (point)))
                  (if (setq more (not (bobp)))
                      (progn (widen)
                             (forward-char -1)
                             (narrow-to-region yanked-beg (point)))))))
            ;; Preserve new bullet if it's a distinctive one, otherwise
            ;; use old one:
            (if (string-match yanked-bullet outline-distinctive-bullets-string)
                (delete-region (save-excursion
                                 (beginning-of-line)
                                 (point))
                               yanked-beg)
              (delete-region yanked-beg (+ yanked-beg established-depth))
              ;; and extraneous digits and a space:
              (while (looking-at "[0-9]") (delete-char 1))
              (if (looking-at " ") (delete-char 1))
              )
            (goto-char yanked-beg)
            )
        ;; Not established-depth or looking-at...
        (setq topic-yanked (looking-at outline-regexp))
        (exchange-dot-and-mark))
      (if (and topic-yanked outline-numbered-bullet)
          (progn
            ;; Renumber, in case necessary:
            (sit-for 0)
            (save-excursion
              (goto-char beginning)
              (if (outline-goto-prefix)
                  (outline-rebullet-heading nil		;;; solicit
                                            (outline-depth) ;;; depth
                                            nil		;;; number-control
                                            nil		;;; index
                                            t)		;;; do-successors
                )
              )
            )
        )
      )
    )
  )
;;;_     > outline-yank-pop (&optional arg)
(defun outline-yank-pop (&optional arg)
  "   Just like yank-pop, but works like outline-yank when popping
  topics just after fresh outline prefixes.  Adapts level of popped
  stuff to level of fresh prefix."

  (interactive "*p")
  (if (not (eq last-command 'yank))
      (error "Previous command was not a yank"))
  (setq this-command 'yank)
  (delete-region (point) (mark))
  (rotate-yank-pointer arg)
  (outline-yank)
  )

;;;_   : Specialty bullet functions
;;;_    . File Cross references
;;;_     > outline-resolve-xref ()
(defun outline-resolve-xref ()
  "  Pop to file associated with current heading, if it has an xref bullet
  (according to setting of 'outline-file-xref-bullet')."
  (interactive)
  (if (not outline-file-xref-bullet)
      (error
       "outline cross references disabled - no 'outline-file-xref-bullet'")
    (if (not (string= (outline-current-bullet) outline-file-xref-bullet))
        (error "current heading lacks cross-reference bullet '%s'"
               outline-file-xref-bullet)
      (let (file-name)
        (save-excursion
          (let* ((text-start outline-recent-prefix-end)
                 (heading-end (progn (outline-pre-next-preface)
                                     (point))))
            (goto-char text-start)
            (setq file-name
                  (if (re-search-forward "\\s-\\(\\S-*\\)" heading-end t)
                      (buffer-substring (match-beginning 1) (match-end 1))))))
        (setq file-name
              (if (not (= (aref file-name 0) ?:))
                  (expand-file-name file-name)
                                        ; A registry-files ref, strip the ':'
                                        ; and try to follow it:
                (let ((reg-ref (reference-registered-file
                                (substring file-name 1) nil t)))
                  (if reg-ref (car (cdr reg-ref))))))
        (if (or (file-exists-p file-name)
                (if (file-writable-p file-name)
                    (y-or-n-p (format "%s not there, create one? "
                                      file-name))
                  (error "%s not found and can't be created" file-name)))
            (condition-case failure
                (find-file-other-window file-name)
              (error failure))
          (error "%s not found" file-name))
        )
      )
    )
  )
;;;_     > outline-to-entry-end - Unmaintained compatibility - ignore this!
;-------------------------------------------------------------------
; Something added solely for use by a "smart menu" package someone got
; off the net.  I have no idea whether this is appropriate code.

(defvar next-entry-exists nil "Used by outline-to-entry-end, dunno why.")
(defun outline-to-entry-end (&optional include-sub-entries curr-entry-level)
  "   Go to end of whole entry if optional INCLUDE-SUB-ENTRIES is non-nil.
   CURR-ENTRY-LEVEL is an integer representing the length of the current level
   string which matched to 'outline-regexp'.  If INCLUDE-SUB-ENTRIES is nil,
   CURR-ENTRY-LEVEL is not needed."
  (while (and (setq next-entry-exists
		    (re-search-forward outline-regexp nil t))
	      include-sub-entries
	      (save-excursion
		(beginning-of-line)
		(> (outline-depth) curr-entry-level))))
  (if next-entry-exists
      (progn (beginning-of-line) (point))
    (goto-char (point-max))))
;;; Outline topic prefix and level adjustment funcs:

;;;_  #6 miscellaneous
;;;_   > outline-copy-exposed (&optional workbuf)
(defun outline-copy-exposed (&optional workbuf)
  "   Duplicate buffer to other buffer, sans hidden stuff.

   Without repeat count, this simple-minded function just generates
   the new buffer by concatenating the current buffer name with \"
   exposed\", and doing a 'get-buffer' on it."

  (interactive)
  (if (not workbuf) (setq workbuf (concat (buffer-name) " exposed")))
  (let ((buf (current-buffer)))
    (if (not (get-buffer workbuf))
	(generate-new-buffer workbuf))
    (pop-to-buffer workbuf)
    (erase-buffer)
    (insert-buffer buf)
    ;; (replace-regexp "\^M[^\^M\^J]*" "")
    (while (re-search-forward "\^M[^\^M\^J]*" nil t)
      (replace-match "" nil nil))
    (goto-char (point-min))
    )
  )
;;;_   > outlinify-sticky ()
(defun outlinify-sticky (&optional arg)
  "   Activate outline mode and establish file eval to set initial exposure.
  
  Invoke with a string argument to designate a string to prepend to
  topic prefixs, or with a universal argument to be prompted for the
  string to be used.  Suitable defaults are provided for lisp,
  emacs-lisp, c, c++, awk, sh, csh, and perl modes."

 (interactive "P") (outline-mode t)
 (cond (arg
        (if (stringp arg)
            ;; Use arg as the header-prefix:
            (outline-lead-with-comment-string arg)
          ;; Otherwise, let function solicit string:
          (setq arg (outline-lead-with-comment-string))))
       ((member major-mode '(emacs-lisp-mode lisp-mode))
        (setq arg (outline-lead-with-comment-string ";;;_")))
       ((member major-mode '(awk-mode csh-mode sh-mode perl-mode))
        ;; Bare '#' (ie, not '#_') so we don't break the magic number:
        (setq arg (outline-lead-with-comment-string "#")))
       ((eq major-mode 'c++-mode)
        (setq arg (outline-lead-with-comment-string "//_")))
       ((eq major-mode 'c-mode)
        ;; User's will have to know to close off the comments:
        (setq arg (outline-lead-with-comment-string "/*_"))))
  (let* ((lead-prefix (format "%s%s"
                              (concat outline-header-prefix (if arg " " ""))
                              outline-primary-bullet))
         (lead-line (format "%s%s %s\n%s %s\n  %s %s %s"
                            (if arg outline-header-prefix "")
                            outline-primary-bullet
                            "Local emacs vars."
                            "'(This topic sets initial outline exposure"
                            "of the file when loaded by emacs,"
                            "Encapsulate it in comments if"
                            "file is a program"
                            "otherwise ignore it,")))

    (save-excursion
                                        ; Put a topic at the top, if
                                        ; none there already:
      (goto-char (point-min))
      (if (not (looking-at outline-regexp))
          (insert-string
           (if (not arg) outline-primary-bullet
             (format "%s%s\n" outline-header-prefix outline-primary-bullet))))
                               
                                        ; File-vars stuff, at the bottom:
      (goto-char (point-max))
                                        ; Insert preamble:
      (insert-string (format "\n\n%s\n%s %s %s\n%s %s "
                             lead-line
                             lead-prefix
                             "local"
                             "variables:"
                             lead-prefix
                             "eval:"))
                                        ; Insert outline-mode activation:
      (insert-string
       (format "%s\n\t\t%s\n\t\t\t%s\n"
               "(condition-case err"
               "(save-excursion"
               "(outline-mode t)"))
                                        ; Conditionally insert prefix
                                        ; leader customization:
      (if arg (insert-string (format "\t\t\t(%s \"%s\")\n"
                                     "outline-lead-with-comment-string"
                                     arg)))
                                        ; Insert announcement and
                                        ; exposure control:
      (insert-string
       (format "\t\t\t%s %s\n\t\t\t%s %s\n\t\t%s %s"
               "(message \"Adjusting '%s' visibility\""
               "(buffer-name))"
               "(goto-char 0)"
               "(outline-exposure -1 0))"
               "(error (message "
               "\"Failed file var 'allout' provisions\")))"))
                                        ; Insert postamble:
      (insert-string (format "\n%s End: )\n"
                             lead-prefix)))))
;;;_   > solicit-char-in-string (prompt string &optional do-defaulting)
(defun solicit-char-in-string (prompt string &optional do-defaulting)
  "   Solicit (with first arg PROMPT) choice of a character from string STRING.

   Optional arg DO-DEFAULTING indicates to accept empty input (CR)."

  (let ((new-prompt prompt)
        got)

    (while (not got)
      (message "%s" new-prompt)

      ;; We do our own reading here, so we can circumvent, eg, special
      ;; treatment for '?' character.  (Might oughta change minibuffer
      ;; keymap instead, oh well.)
      (setq got
            (char-to-string (let ((cursor-in-echo-area t)) (read-char))))

      (if (null (string-match got string))
          (if (and do-defaulting (string= got "\^M"))
              ;; We're defaulting, return null string to indicate that:
              (setq got "")
            ;; Failed match and not defaulting,
            ;; set the prompt to give feedback,
            (setq new-prompt (concat prompt
                                     got
                                     " ...pick from: "
                                     string
                                     ""))
            ;; and set loop to try again:
            (setq got nil))
        ;; Got a match - give feedback:
        (message "")))
    ;; got something out of loop - return it:
    got)
  )
;;;_   > string-sans-char (string char)
(defun string-sans-char (string char)
  "  Return a copy of STRING that lacks all instances of CHAR."
  (cond ((string= string "") "")
        ((= (aref string 0) char) (string-sans-char (substring string 1) char))
        ((concat (substring string 0 1)
                 (string-sans-char (substring string 1) char)))))

;;;_* Local emacs vars.
'(
Local variables:
eval: (save-excursion
        (if (not (condition-case err (outline-mode t)
                   (wrong-number-of-arguments nil)))
            (progn
              (message
               "Allout outline-mode not loaded, not adjusting buffer exposure")
              (sit-for 1))
          (message "Adjusting '%s' visibility" (buffer-name))
          (outline-lead-with-comment-string ";;;_")
          (goto-char 0)
          (outline-exposure (-1 () () () 1) 0)))
End:
)

