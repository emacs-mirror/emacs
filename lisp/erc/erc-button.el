;;; erc-button.el --- A way of buttonizing certain things in ERC buffers  -*- lexical-binding:t -*-

;; Copyright (C) 1996-2004, 2006-2026 Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Maintainer: Amin Bandali <bandali@gnu.org>, F. Jason Park <jp@neverwas.me>
;; Keywords: comm, irc, button, url, regexp
;; URL: https://www.emacswiki.org/emacs/ErcButton

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

;; Heavily borrowed from gnus-art.el.  Thanks to the original authors.
;; This buttonizes nicks and other stuff to make it all clickable.
;; To enable, add to your init file:
;; (require 'erc-button)
;; (erc-button-mode 1)
;;
;; Todo:
;; * Rewrite all this to do the same, but use button.el.  Why?
;; button.el is much faster, and much more elegant, and solves the
;; problem we get with large buffers and a large erc-button-marker-list.


;;; Code:

(require 'erc)
(require 'wid-edit)
(require 'erc-fill)
(require 'browse-url)

;;; Minor Mode

(defgroup erc-button nil
  "Define how text can be turned into clickable buttons."
  :group 'erc)

;;;###autoload(autoload 'erc-button-mode "erc-button" nil t)
(define-erc-module button nil
  "This mode buttonizes all messages according to `erc-button-alist'."
  ((add-hook 'erc-insert-modify-hook #'erc-button-add-buttons 30)
   (add-hook 'erc-send-modify-hook #'erc-button-add-buttons 30)
   (add-hook 'erc-mode-hook #'erc-button-setup 91)
   (unless erc--updating-modules-p (erc-buffer-do #'erc-button-setup))
   (add-hook 'erc--tab-functions #'erc-button-next)
   (erc--modify-local-map t "<backtab>" #'erc-button-previous))
  ((remove-hook 'erc-insert-modify-hook #'erc-button-add-buttons)
   (remove-hook 'erc-send-modify-hook #'erc-button-add-buttons)
   (remove-hook 'erc-mode-hook #'erc-button-setup)
   (remove-hook 'erc--tab-functions #'erc-button-next)
   (erc--modify-local-map nil "<backtab>" #'erc-button-previous)))

;;; Variables

(defface erc-button '((t :weight bold))
  "ERC button face."
  :group 'erc-faces)

(defface erc-button-nick-default-face '((t :inherit erc-nick-default-face))
  "Default face for a buttonized nickname."
  :package-version '(ERC . "5.6")
  :group 'erc-faces)

(defcustom erc-button-face 'erc-button
  "Face used for highlighting buttons in ERC buffers.

A button is a piece of text that you can activate by pressing
\\`RET' or `mouse-2' above it.  See also `erc-button-keymap'."
  :type 'face
  :group 'erc-faces)

(defcustom erc-button-nickname-face 'erc-button-nick-default-face
  "Face used for ERC nickname buttons."
  :package-version '(ERC . "5.6")
  :type 'face
  :group 'erc-faces)

(defcustom erc-button-mouse-face 'highlight
  "Face used for mouse highlighting in ERC buffers.

Buttons will be displayed in this face when the mouse cursor is
above them."
  :type 'face
  :group 'erc-faces)

(defcustom erc-button-url-regexp browse-url-button-regexp
  "Regular expression that matches URLs."
  :version "27.1"
  :type 'regexp)

(defcustom erc-button-wrap-long-urls nil
  "If non-nil, \"long\" URLs matching `erc-button-url-regexp' will be wrapped.

If this variable is a number, consider URLs longer than its value to
be \"long\".  If t, URLs will be considered \"long\" if they are
longer than `erc-fill-column'."
  :type '(choice integer boolean))

(defcustom erc-button-buttonize-nicks t
  "Flag indicating whether nicks should be buttonized.
Note that beginning in ERC 5.6, some functionality provided by
other modules, such as `fill-wrap', may depend on this option
being non-nil."
  :type 'boolean)

(defcustom erc-button-rfc-url "https://tools.ietf.org/html/rfc%s"
  "URL used to browse RFC references.
%s is replaced by the number."
  :type 'string
  :version "28.1")

(define-obsolete-variable-alias 'erc-button-google-url
  'erc-button-search-url "27.1")

(defcustom erc-button-search-url "https://duckduckgo.com/?q=%s"
  "URL used to search for a term.
%s is replaced by the search string."
  :version "28.1"
  :type 'string)

(defcustom erc-button-alist
  ;; Since the callback is only executed when the user is clicking on
  ;; a button, it makes no sense to optimize performance by
  ;; bytecompiling lambdas in this alist.  On the other hand, it makes
  ;; things hard to maintain.
  '((erc-button-url-regexp 0 t browse-url-button-open-url 0)
    ;; ("<URL: *\\([^<> ]+\\) *>" 0 t browse-url-button-open-url 1)
;;; ("(\\(\\([^~\n \t@][^\n \t@]*\\)@\\([a-zA-Z0-9.:-]+\\)\\)" 1 t finger 2 3)
    ;; emacs internal
    ("[`‘]\\([a-zA-Z][-a-zA-Z_0-9!*<=>+]+\\)['’]"
     1 t erc-button-describe-symbol 1)
    ;; pseudo links
    ("\\(?:\\bInfo: ?\\|(info \\)[\"]\\(([^\"]+\\)[\"])?" 0 t info 1)
    ("\\b\\(Ward\\|Wiki\\|WardsWiki\\|TheWiki\\):\\([A-Z][a-z]+\\([A-Z][a-z]+\\)+\\)"
     0 t (lambda (page)
           (browse-url (concat "http://c2.com/cgi-bin/wiki?" page)))
     2)
    ("EmacsWiki:\\([A-Z][a-z]+\\([A-Z][a-z]+\\)+\\)" 0 t erc-browse-emacswiki 1)
    ("Lisp:\\([a-zA-Z.+-]+\\)" 0 t erc-browse-emacswiki-lisp 1)
    ("\\bGoogle:\\([^ \t\n\r\f]+\\)"
     0 t (lambda (keywords)
           (browse-url (format erc-button-search-url keywords)))
     1)
    ("\\brfc[#: ]?\\([0-9]+\\)"
     0 t (lambda (num)
           (browse-url (format erc-button-rfc-url num)))
     1)
    ;; other
    ("\\s-\\(@\\([0-9][0-9][0-9]\\)\\)" 1 t erc-button-beats-to-time 2))
  "Alist of regexps matching buttons in ERC buffers.
Each entry has the form (REGEXP BUTTON FORM CALLBACK PAR...), where

REGEXP is the string matching text around the button or a symbol
  indicating a variable holding that string, or a list of
  strings, or an alist with the strings in the car.  Note that
  entries in lists or alists are considered to be nicks or other
  complete words.  Therefore they are enclosed in \\< and \\>
  while searching.  Also, use of the special symbol `nicknames'
  for this slot was deprecated in ERC 5.6, but users can still
  use `erc-button-buttonize-nicks' to control whether nicks get
  buttonized.  And because customizing a corresponding CALLBACK
  is no longer possible, an escape hatch has been provided via
  the variable `erc-button-nickname-callback-function'.

BUTTON is the number of the regexp grouping actually matching the
  button.

FORM is either a boolean or a special variable whose value must
  be non-nil for the button to be added.  It can also be a
  function to call in place of `erc-button-add-button' with the
  exact same arguments.  When FORM is also a special variable,
  ERC disregards the variable and calls the function.  Note that
  arbitrary s-expressions were deprecated in ERC 5.6 and may not
  be respected in the future.  If necessary, users can instead
  supply a function that calls `erc-button-add-button' when such
  an expression is non-nil.

CALLBACK is the function to call when the user push this button.
  CALLBACK can also be a symbol.  Its variable value will be used
  as the callback function.

PAR is a number of a regexp grouping whose text will be passed to
  CALLBACK.  There can be several PAR arguments."
  :package-version '(ERC . "5.6")
  :type '(repeat
          (list :tag "Button"
                (choice :tag "Matches"
                        regexp
                        (variable :tag "Variable containing regexp")
                        (repeat :tag "List of words" string)
                        (alist :key-type string :value-type sexp))
                (integer :tag "Number of the regexp section that matches")
                (choice :tag "When to buttonize"
                        (const :tag "Always" t)
                        (function :tag "Alternative buttonizing function")
                        (variable :tag "Var with value treated as boolean"))
                (function :tag "Function to call when button is pressed")
                (repeat :tag "Sections of regexp to send to the function"
                        :inline t
                        (integer :tag "Regexp section number")))))

(defcustom erc-emacswiki-url "https://www.emacswiki.org/emacs/"
  "URL of the EmacsWiki website."
  :type 'string
  :version "28.1")

(defcustom erc-emacswiki-lisp-url "https://www.emacswiki.org/elisp/"
  "URL of the EmacsWiki Elisp area."
  :type 'string)

(defvar erc-button-highlight-nick-once '(QUIT PART JOIN)
  "Messages for which to buttonize only the first nick occurrence.")

(defvar erc-button-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'erc-button-press-button)
    (define-key map (kbd "<mouse-2>") #'erc-button-click-button)
    (define-key map (kbd "TAB") #'erc-button-next)
    (define-key map (kbd "<backtab>") #'erc-button-previous)
    (define-key map [follow-link] 'mouse-face)
    (set-keymap-parent map erc-mode-map)
    map)
  "Local keymap for ERC buttons.")

(defvar erc-button-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\[ "w" table)
    (modify-syntax-entry ?\] "w" table)
    (modify-syntax-entry ?\{ "w" table)
    (modify-syntax-entry ?\} "w" table)
    (modify-syntax-entry ?` "w" table)
    (modify-syntax-entry ?^ "w" table)
    (modify-syntax-entry ?- "w" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?| "w" table)
    (modify-syntax-entry ?\\ "w" table)
    table)
  "Syntax table used when buttonizing messages.
This syntax table should make all the valid nick characters word
constituents.")

(defvar erc-button-keys-added nil
  "Internal variable used to keep track of whether we've added the
global-level ERC button keys yet.")
(make-obsolete-variable 'erc-button-keys-added "no longer relevant" "30.1")

(defvar-local erc-button--has-nickname-entry nil
  "Whether `erc-button-alist' contains a legacy `nicknames' entry.")

(defun erc-button-setup ()
  "Perform major-mode setup for ERC's button module.
Note that prior to ERC 5.6, this function used to modify
`erc-mode-map', but that's now handled by the mode toggles
themselves."
  (setq erc-button-keys-added t)
  (cl-assert (derived-mode-p 'erc-mode))
  ;; It would probably suffice to run this in server buffers alone,
  ;; even though buttonizing happens in all ERC buffers and users have
  ;; been known to set `erc-button-alist' locally.
  (dolist (entry erc-button-alist)
    (pcase entry
      ((or `(nicknames ,_ ,sym . ,_) `('nicknames ,_ ,sym . ,_))
       (setq erc-button--has-nickname-entry t)
       (unless (eq sym 'erc-button-buttonize-nicks)
         (erc--warn-once-before-connect 'erc-button-mode
           "The legacy `nicknames' entry in `erc-button-alist'"
           " is deprecated.  See doc string for details.")))
      ((and `(,_ ,_ ,form . ,_)
            (guard (not (or (and (symbolp form)
                                 (special-variable-p form))
                            (functionp form)))))
       (erc--warn-once-before-connect 'erc-button-mode
         "Arbitrary sexps for the third, FORM slot of `erc-button-alist'"
         " entries are deprecated. Either use a variable or a function"
         " that conditionally calls `erc-button-add-button'.")))))

(defvar erc-button-nickname-callback-function #'erc-button--perform-nick-popup
  "Escape hatch for users needing a non-standard nick-button callback.
Value should be a function accepting a NICK and any number of
trailing arguments that are as yet unspecified.  Runs when
clicking \\`<mouse-1>' or hitting \\`RET' atop a nickname button.")
(make-obsolete-variable 'erc-button-nickname-callback-function
                        "default provides essential functionality" "30.1")

(defun erc-button-add-buttons ()
  "Find external references in the current buffer and make buttons of them.
\"External references\" are things like URLs, as
specified by `erc-button-alist'."
  (interactive)
  (save-excursion
    (with-syntax-table erc-button-syntax-table
      (let ((buffer-read-only nil)
            (inhibit-field-text-motion t)
            (alist erc-button-alist)
            regexp)
        (erc-button-remove-old-buttons)
        (unless (or erc-button--has-nickname-entry
                    (not erc-button-buttonize-nicks)
                    (and (erc--memq-msg-prop 'erc--skip 'button)
                         (not (setq alist nil))))
          (erc-button-add-nickname-buttons
           `(_ _ erc-button--modify-nick-function
               ,erc-button-nickname-callback-function)))
        (dolist (entry alist)
          (if (or (eq (car entry) 'nicknames)
                  ;; Old form retained for backward compatibility.
                  (equal (car entry) (quote 'nicknames)))
              (erc-button-add-nickname-buttons entry)
            (progn
              (setq regexp (or (and (stringp (car entry)) (car entry))
                               (and (boundp (car entry))
                                    (symbol-value (car entry)))))
              (cond ((stringp regexp)
                     (erc-button-add-buttons-1 regexp entry))
                    ((and (listp regexp) (stringp (car regexp)))
                     (dolist (r regexp)
                       (erc-button-add-buttons-1
                        (concat "\\<" (regexp-quote r) "\\>")
                        entry)))
                    ((and (listp regexp) (listp (car regexp))
                          (stringp (caar regexp)))
                     (dolist (elem regexp)
                       (erc-button-add-buttons-1
                        (concat "\\<" (regexp-quote (car elem)) "\\>")
                        entry)))))))))))

(defun erc-button--extract-form (form)
  ;; If a special-variable is also a function, favor the function.
  (cond ((eq t form) t)
        ((functionp form) form)
        ((and (symbolp form) (special-variable-p form))
         (while (let ((val (symbol-value form)))
                  (prog1 (and (not (eq val form))
                              (symbolp val)
                              (special-variable-p val))
                    (setq form val))))
         form)
        (t (eval form t))))

(cl-defstruct erc-button--nick
  ( bounds nil :type cons
    ;; Indicates the nick's position in the current message.  BEG is
    ;; normally also point.
    :documentation "A cons of (BEG . END).")
  ( data nil :type (or null cons)
    ;; When non-nil, the CAR must be a non-casemapped nickname.  For
    ;; compatibility, the CDR should probably be nil, but this may
    ;; have to change eventually.  If non-nil, the entire cons should
    ;; be mutated rather than replaced because it's used as a key in
    ;; hash tables and text-property searches.
    :documentation "A unique cons whose car is a nickname.")
  ( downcased nil :type (or null string)
    :documentation "The case-mapped nickname sans text properties.")
  ( user nil :type (or null erc-server-user)
    ;; Not necessarily present in `erc-server-users'.
    :documentation "A possibly nil or spoofed `erc-server-user'.")
  ( cusr nil :type (or null erc-channel-user)
    ;; The CDR of a value from an `erc-channel-members' table.
    :documentation "A possibly nil `erc-channel-user'.")
  ( nickname-face erc-button-nickname-face :type symbol
    :documentation "Temp `erc-button-nickname-face' while buttonizing.")
  ( mouse-face erc-button-mouse-face :type symbol
    :documentation "Function to return possibly cached face.")
  ( face-cache nil :type (or null function)))

;; This variable is intended to serve as a "core" to be wrapped by
;; (built-in) modules during setup.  It's unclear whether
;; `add-function's practice of removing existing advice before
;; re-adding it is desirable when integrating modules since we're
;; mostly concerned with ensuring one "piece" precedes or follows
;; another (specific piece), which may not yet (or ever) be present.

(defvar erc-button--modify-nick-function #'identity
  "Function to possibly modify aspects of nick being buttonized.
Called with one argument, an `erc-button--nick' object, or nil.
The function should return the same (or similar) object when
buttonizing ought to proceed and nil otherwise.  While running,
all faces defined in `erc-button' are bound temporarily and can
be updated at will.")

(defvar-local erc-button--phantom-cmems nil)

(defvar erc-button--fallback-cmem-function
  #'erc-button--get-user-from-spkr-prop
  "Function to determine channel member if not found in the usual places.
Called with DOWNCASED-NICK, NICK, NICK-BOUNDS, and COUNT when
`erc-button-add-nickname-buttons' cannot find a user object for
DOWNCASED-NICK in `erc-channel-members' or `erc-server-users'.
NICK-BOUNDS is a cons of buffer positions, and COUNT is a number
incremented with each visit, starting at 1.")

(defun erc-button--get-user-from-spkr-prop (_ _ _ count)
  "Attempt to obtain an `erc-channel-user' from current \"msg props\".
But only do so when COUNT is 1, meaning this is the first button
candidate in the just-inserted message."
  (and-let* (((= 1 count))
             (nick (erc--check-msg-prop 'erc--spkr)))
    (gethash nick erc-channel-members)))

;; Historical or fictitious users.  As long as these two structs
;; remain superficial "subclasses" with the same slots and defaults,
;; they can live here instead of in erc-common.el.
(cl-defstruct (erc--phantom-channel-user (:include erc-channel-user)))
(cl-defstruct (erc--phantom-server-user (:include erc-server-user)))

(defun erc-button--add-phantom-speaker (downcased nuh _parsed)
  (pcase-let* ((`(,nick ,login ,host) nuh)
               (cmem (gethash downcased erc-button--phantom-cmems))
               (user (or (car cmem)
                         (make-erc--phantom-server-user
                          :nickname nick
                          :host (and (not (string-empty-p host)) host)
                          :login (and (not (string-empty-p login)) login))))
               (cuser (or (cdr cmem)
                          (make-erc--phantom-channel-user
                           :last-message-time (current-time)))))
    (puthash downcased (cons user cuser) erc-button--phantom-cmems)
    (cons user cuser)))

(defun erc-button--get-phantom-cmem (down _word _bounds _count)
  (gethash down erc-button--phantom-cmems))

(define-minor-mode erc-button--phantom-users-mode
  "Minor mode to recognize unknown speakers.
Expect to be used by module setup code for creating placeholder
users on the fly during history playback.  Treat an unknown
\"PRIVMSG\" speaker, like \"<bob>\", as if they previously
appeared in a prior \"353\" message and are thus a known member
of the channel.  However, don't bother creating an actual
`erc-channel-user' object because their status prefix is unknown.
Instead, just spoof an `erc-server-user' and stash it during
\"PRIVMSG\" handling via `erc--cmem-from-nick-function' and
retrieve it during buttonizing via
`erc-button--fallback-cmem-function'."
  :interactive nil
  (if erc-button--phantom-users-mode
      (progn
        (add-function :after-until (local 'erc--cmem-from-nick-function)
                      #'erc-button--add-phantom-speaker '((depth . 30)))
        (add-function :after-until (local 'erc-button--fallback-cmem-function)
                      #'erc-button--get-phantom-cmem '((depth . 50)))
        (setq erc-button--phantom-cmems (make-hash-table :test #'equal)))
    (remove-function (local 'erc--cmem-from-nick-function)
                     #'erc-button--add-phantom-speaker)
    (remove-function (local 'erc-button--fallback-cmem-function)
                     #'erc-button--get-phantom-cmem)
    (kill-local-variable 'erc-button--phantom-cmems)))

(defun erc-button-add-nickname-buttons (entry)
  "Search through the buffer for nicknames, and add buttons."
  (when-let* ((form (nth 2 entry))
              ;; Spoof `form' slot of default legacy `nicknames' entry
              ;; so `erc-button--extract-form' sees a function value.
              (form (let ((erc-button-buttonize-nicks
                           (and erc-button-buttonize-nicks
                                erc-button--modify-nick-function)))
                      (erc-button--extract-form form)))
              (oncep (if-let* ((erc-button-highlight-nick-once)
                               (c (erc--check-msg-prop 'erc--cmd))
                               ((memq c erc-button-highlight-nick-once)))
                         1 0))
              (seen 0))
    (goto-char (point-min))
    (while-let
        (((or (zerop seen) (zerop oncep)))
         ((erc-forward-word))
         (bounds (or (and (= 1 (cl-incf seen)) (erc--get-speaker-bounds))
                     (erc-bounds-of-word-at-point)))
         (word (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (down (erc-downcase word)))
      (let* ((nick-obj t)
             (cmem (and erc-channel-members
                        (or (gethash down erc-channel-members)
                            (funcall erc-button--fallback-cmem-function
                                     down word bounds seen))))
             (user (or (and cmem (car cmem))
                       (and erc-server-users (gethash down erc-server-users))))
             (data (list word)))
        (when (or (not (functionp form))
                  (and user
                       (setq nick-obj (funcall form (make-erc-button--nick
                                                     :bounds bounds :data data
                                                     :downcased down :user user
                                                     :cusr (cdr cmem)))
                             data (erc-button--nick-data nick-obj)
                             bounds (erc-button--nick-bounds nick-obj))))
          (erc-button-add-button (car bounds) (cdr bounds) (nth 3 entry)
                                 nick-obj data))))))

(defun erc-button-add-buttons-1 (regexp entry)
  "Search through the buffer for matches to ENTRY and add buttons."
  (goto-char (point-min))
  (let (buttonizer)
    (while
        (and (re-search-forward regexp nil t)
             (or buttonizer
                 (setq buttonizer
                       (and-let*
                           ((raw-form (nth 2 entry))
                            (res (or (eq t raw-form)
                                     (erc-button--extract-form raw-form))))
                         (if (functionp res) res #'erc-button-add-button)))))
      (let ((start (match-beginning (nth 1 entry)))
            (end (match-end (nth 1 entry)))
            (fun (nth 3 entry))
            (data (mapcar #'match-string-no-properties (nthcdr 4 entry))))
        (funcall buttonizer start end fun nil data regexp)))))

(defun erc-button-remove-old-buttons ()
  "Remove all existing buttons.
This is called with narrowing in effect, just before the text is
buttonized again.  Removing a button means to remove all the properties
that `erc-button-add-button' adds, except for the face."
  (remove-text-properties
   (point-min) (point-max)
   '(erc-callback nil
                  erc-data nil
                  mouse-face nil
                  keymap nil))
  (erc--restore-important-text-props '(mouse-face)))

(defun erc-button-add-button (from to fun nick-p &optional data regexp)
  "Create a button between FROM and TO with callback FUN and data DATA.
NICK-P specifies if this is a nickname button.
REGEXP is the regular expression which matched for this button."
  ;; Really nasty hack to <URL: > ise urls, and line-wrap them if
  ;; they're going to be wider than `erc-fill-column'.
  ;; This could be a lot cleaner, but it works for me -- lawrence.
  (let (fill-column)
    (when (and erc-button-wrap-long-urls
               (string= regexp erc-button-url-regexp)
               (> (- to from)
                  (setq fill-column (- (if (numberp erc-button-wrap-long-urls)
                                           erc-button-wrap-long-urls
                                         erc-fill-column)
                                       (length erc-fill-prefix)))))
      (setq to (prog1 (point-marker) (insert ">"))
            from (prog2 (goto-char from) (point-marker) (insert "<URL: ")))
      (let ((pos (copy-marker from)))
        (while (> (- to pos) fill-column)
          (goto-char (+ pos fill-column))
          (insert "\n" erc-fill-prefix) ; This ought to figure out
                                        ; what type of filling we're
                                        ; doing, and indent accordingly.
          (move-marker pos (point))))))
  (if nick-p
      (when erc-button-nickname-face
        (erc--merge-prop from to 'font-lock-face
                         (if (erc-button--nick-p nick-p)
                             (erc-button--nick-nickname-face nick-p)
                           erc-button-nickname-face)
                         nil (and (erc-button--nick-p nick-p)
                                  (erc-button--nick-face-cache nick-p))))
    (when erc-button-face
      (erc--merge-prop from to 'font-lock-face erc-button-face)))
  (add-text-properties
   from to
   (nconc (and-let* ((face (or (and (erc-button--nick-p nick-p)
                                    (erc-button--nick-mouse-face nick-p))
                               erc-button-mouse-face)))
            (list 'mouse-face face))
          (list 'erc-callback fun)
          (list 'keymap erc-button-keymap)
          (list 'rear-nonsticky t)
          (and data (list 'erc-data data)))))

(defun erc-button-add-face (from to face)
  "Add FACE to the region between FROM and TO."
  ;; If we just use `add-text-property', then this will overwrite any
  ;; face text property already used for the button.  It will not be
  ;; merged correctly.  If we use overlays, then redisplay will be
  ;; very slow with lots of buttons.  This is why we manually merge
  ;; face text properties.
  (let ((old (erc-list (get-text-property from 'font-lock-face)))
        (pos from)
        (end (next-single-property-change from 'font-lock-face nil to))
        new)
    ;; old is the face at pos, in list form.  It is nil if there is no
    ;; face at pos.  If nil, the new face is FACE.  If not nil, the
    ;; new face is a list containing FACE and the old stuff.  end is
    ;; where this face changes.
    (while (< pos to)
      (setq new (if old (cons face old) face))
      (put-text-property pos end 'font-lock-face new)
      (setq pos end
            old (erc-list (get-text-property pos 'font-lock-face))
            end (next-single-property-change pos 'font-lock-face nil to)))))

;; widget-button-click calls with two args, we ignore the first.
;; Since Emacs runs this directly, rather than with
;; widget-button-click, we need to fake an extra arg in the
;; interactive spec.
(defun erc-button-click-button (_ignore event)
  "Call `erc-button-press-button'."
  (interactive "P\ne")
  (save-excursion
    (mouse-set-point event)
    (erc-button-press-button)))

(defun erc-button-press-button (&rest _ignore)
  "Check text at point for a callback function.
If the text at point has a `erc-callback' property,
call it with the value of the `erc-data' text property."
  (declare (advertised-calling-convention () "28.1"))
  (interactive)
  (let* ((data (get-text-property (point) 'erc-data))
         (fun (get-text-property (point) 'erc-callback)))
    (unless fun
      (message "No button at point"))
    (when (and fun (symbolp fun) (not (fboundp fun)))
      (error "Function %S is not bound" fun))
    (apply fun data)))

(defun erc-button-next-function ()
  "Pseudo completion function that actually jumps to the next button.
For use on `completion-at-point-functions'."
  (declare (obsolete erc-nickserv-identify "30.1"))
  ;; FIXME: This is an abuse of completion-at-point-functions.
  (when (< (point) (erc-beg-of-input-line))
    (let ((start (point)))
      (lambda ()
        (let ((here start))
          ;; FIXME: Use next-single-property-change.
          (while (and (get-text-property here 'erc-callback)
                      (not (= here (point-max))))
            (setq here (1+ here)))
          (while (not (or (get-text-property here 'erc-callback)
                          (= here (point-max))))
            (setq here (1+ here)))
          (if (< here (point-max))
              (goto-char here)
            (error "No next button"))
          t)))))

(defvar erc-button--prev-next-predicate-functions
  '(erc-button--end-of-button-p)
  "Abnormal hook whose members can return non-nil to continue searching.
Otherwise, if all members return nil, point will stay at the
current button.  Called with a single arg, a buffer position
greater than `point-min' with a text property of `erc-callback'.")

(defun erc-button--end-of-button-p (point)
  (get-text-property (1- point) 'erc-callback))

(defun erc--button-next (arg)
  (let* ((nextp (prog1 (>= arg 1) (setq arg (max 1 (abs arg)))))
         (search-fn (if nextp
                        #'next-single-char-property-change
                      #'previous-single-char-property-change))
         (start (point))
         (p start))
    (while (progn
             ;; Break out of current search context.
             (when-let* ((low (max (point-min) (1- (pos-bol))))
                         (high (min (point-max) (1+ (pos-eol))))
                         (prop (get-text-property p 'erc-callback))
                         (q (if nextp
                                (text-property-not-all p high
                                                       'erc-callback prop)
                              (funcall search-fn p 'erc-callback nil low)))
                         ((< low q high)))
               (setq p q))
             ;; Assume that buttons occur frequently enough that
             ;; omitting LIMIT is acceptable.
             (while
                 (and (setq p (funcall search-fn p 'erc-callback))
                      (if nextp (< p erc-insert-marker) (/= p (point-min)))
                      (run-hook-with-args-until-success
                       'erc-button--prev-next-predicate-functions p)))
             (and arg
                  (< (point-min) p erc-insert-marker)
                  (goto-char p)
                  (not (zerop (cl-decf arg))))))
    (when (= (point) start)
      (user-error (if nextp "No next button" "No previous button")))
    t))

(defun erc-button-next (&optional arg)
  "Go to the ARGth next button."
  (declare (advertised-calling-convention (arg) "30.1"))
  (interactive "p")
  (erc--button-next (or arg 1)))

(defun erc-button-previous (&optional arg)
  "Go to ARGth previous button."
  (declare (advertised-calling-convention (arg) "30.1"))
  (interactive "p")
  (erc--button-next (- (or arg 1))))

(defun erc-button-previous-of-nick (arg)
  "Go to ARGth previous button for nick at point."
  (interactive "p")
  (if-let* ((prop (get-text-property (point) 'erc-data))
            (erc-button--prev-next-predicate-functions
             (cons (lambda (p)
                     (not (equal (get-text-property p 'erc-data) prop)))
                   erc-button--prev-next-predicate-functions)))
      (erc--button-next (- arg))
    (user-error "No nick at point")))

(defun erc-browse-emacswiki (thing)
  "Browse to THING in the emacs-wiki."
  (browse-url (concat erc-emacswiki-url thing)))

(defun erc-browse-emacswiki-lisp (thing)
  "Browse to THING in the emacs-wiki elisp area."
  (browse-url (concat erc-emacswiki-lisp-url thing)))

;;; Nickname buttons:

(defcustom erc-nick-popup-alist
  '(("DeOp"  . erc-cmd-DEOP)
    ("Kick"  . erc-button-cmd-KICK)
    ("Msg"   . erc-button-cmd-MSG)
    ("Op"    . erc-cmd-OP)
    ("Query" . erc-cmd-QUERY)
    ("Whois" . erc-cmd-WHOIS)
    ("Lastlog" . erc-cmd-LASTLOG))
  "An alist of possible actions to take on a nickname.
For all entries (ACTION . FUNC), ERC offers ACTION as a possible
completion item and calls the selected entry's FUNC with the
buttonized nickname at point as the only argument.  For
historical reasons, FUNC can also be an arbitrary sexp, in which
case, ERC binds the nick in question to the variable `nick' and
evaluates the expression.

Examples:
 (\"DebianDB\" .
  (shell-command
   (format
    \"ldapsearch -x -P 2 -h db.debian.org -b dc=debian,dc=org ircnick=%s\"
    nick)))"
  :package-version '(ERC . "5.6")
  :type '(repeat (cons (string :tag "Op")
                       (choice function sexp))))

(defun erc-button-cmd-KICK (nick)
  "Prompt for a reason, then kick NICK via `erc-cmd-KICK'.
In server buffers, also prompt for a channel."
  (erc-cmd-KICK
   (or (and erc--target (erc-default-target))
       (let ((targets (mapcar (lambda (b)
                                (cons (erc--target-string
                                       (buffer-local-value 'erc--target b))
                                      b))
                              (erc-channel-list erc-server-process))))
         (completing-read (format "Channel (%s): " (caar targets))
                          targets (pcase-lambda (`(,_ . ,buf))
                                    (with-current-buffer buf
                                      (erc-get-channel-user nick)))
                          t nil t (caar targets))))
   nick
   (read-string "Reason: ")))

(defun erc-button-cmd-MSG (nick)
  "Prompt for a message to NICK, and send it via `erc-cmd-MSG'."
  (let ((msg (read-string (concat "Message to " nick ": "))))
    (erc-cmd-MSG (concat nick " " msg))))

(defvar-local erc-button--nick-popup-alist nil
  "Internally controlled items for `erc-nick-popup-alist'.")

(defun erc-nick-popup (nick)
  (let* ((completion-ignore-case t)
         (alist (append erc-nick-popup-alist erc-button--nick-popup-alist))
         (action (completing-read (format-message
                                   "What action to take on `%s'? " nick)
                                  alist))
         (code (cdr (assoc action alist))))
    (when code
      (erc-set-active-buffer (current-buffer))
      (if (functionp code)
          (funcall code nick)
        (eval code `((nick . ,nick)))))))

(defun erc-button--perform-nick-popup (nick &rest _)
  "Call `erc-nick-popup' with NICK."
  (erc-nick-popup nick))

;;; Callback functions
(defun erc-button-describe-symbol (symbol-name)
  "Describe SYMBOL-NAME.
Use `describe-function' for functions, `describe-variable' for variables,
and `apropos' for other symbols."
  (let ((symbol (intern-soft symbol-name)))
    (cond ((and symbol (fboundp symbol))
           (describe-function symbol))
          ((and symbol (boundp symbol))
           (describe-variable symbol))
          (t (apropos symbol-name)))))

(defun erc-button-beats-to-time (beats)
  "Display BEATS in a readable time format."
  (let* ((seconds (- (* (string-to-number beats) 86.4)
                     3600
                     (- (car (current-time-zone)))))
         (hours (mod (floor seconds 3600) 24))
         (minutes (mod (round seconds 60) 60)))
    (message "@%s is %d:%02d local time"
             beats hours minutes)))

(defun erc-button--display-error-with-buttons
    (from to fun nick-p &optional data regexp)
  "Replace command in region with keys and return new bounds."
  (let* ((o (buffer-substring from to))
         (s (substitute-command-keys o))
         (erc-button-face (and (equal o s) erc-button-face)))
    (delete-region from to)
    (insert s)
    (erc-button-add-button from (point) fun nick-p data regexp)))

;;;###autoload
(defun erc-button--display-error-notice-with-keys (maybe-buffer &rest strings)
  "Add help keys to STRINGS for configuration-related admonishments.
Return inserted result.  Expect MAYBE-BUFFER to be an ERC buffer,
a string, or nil.  When it's a buffer, specify the `buffer'
argument when calling `erc-display-message'.  Otherwise, add it
to STRINGS.  If STRINGS contains any trailing non-nil
non-strings, concatenate leading string members before applying
`format'.  Otherwise, just concatenate everything."
  (let* ((buffer (if (bufferp maybe-buffer)
                     maybe-buffer
                   (when (stringp maybe-buffer)
                     (push maybe-buffer strings))
                   'active))
         (op (if (seq-every-p (lambda (o) (or (not o) (stringp o)))
                              (cdr strings))
                 #'concat
               (let ((head (pop strings)))
                 (while (or (stringp (car strings))
                            (and strings (not (car strings))))
                   (setq head (concat head (pop strings))))
                 (push head strings))
               #'format))
         (string (apply op strings))
         ;; Avoid timestamps unless left-sided.
         (skipp (or (bound-and-true-p erc-stamp--display-margin-mode)
                    (not (fboundp 'erc-timestamp-offset))
                    (zerop (erc-timestamp-offset))))
         (erc--msg-prop-overrides `(,@(and skipp `((erc--skip stamp)))
                                    ,@erc--msg-prop-overrides))
         (erc-insert-post-hook
          (cons (lambda ()
                  (setq string (buffer-substring (point-min)
                                                 (1- (point-max)))))
                erc-insert-post-hook))
         (erc-button-alist
          `((,(rx "\\[" (group (+ (not "]"))) "]") 0
             erc-button--display-error-with-buttons
             erc-button-describe-symbol 1)
            ,@erc-button-alist)))
    (erc-display-message nil '(t notice error) buffer string)
    string))

;;;###autoload
(defun erc-button--display-error-notice-with-keys-and-warn (&rest args)
  "Like `erc-button--display-error-notice-with-keys' but also warn."
  (let ((string (apply #'erc-button--display-error-notice-with-keys args)))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (with-syntax-table lisp-mode-syntax-table
        (skip-syntax-forward "^-"))
      (forward-char)
      (erc--lwarn
       'erc :warning (buffer-substring-no-properties (point) (point-max))))))

(provide 'erc-button)

;;; erc-button.el ends here
;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
