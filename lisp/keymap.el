;;; keymap.el --- Keymap functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: internal
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

;; This library deals with the "new" keymap binding interface: The
;; only key syntax allowed by these functions is the `kbd' one.

;;; Code:



(defun keymap--check (key)
  "Signal an error if KEY doesn't have a valid syntax."
  (unless (key-valid-p key)
    (error "%S is not a valid key definition; see `key-valid-p'" key)))

(defun keymap--compile-check (&rest keys)
  (dolist (key keys)
    (when (or (vectorp key)
              (and (stringp key) (not (key-valid-p key))))
      (byte-compile-warn "Invalid `kbd' syntax: %S" key))))

(defun keymap-set (keymap key definition)
  "Set KEY to DEFINITION in KEYMAP.
KEY is a string that satisfies `key-valid-p'.
If DEFINITION is a string, it must also satisfy `key-valid-p'.

DEFINITION is anything that can be a key's definition:
 nil (means key is undefined in this keymap),
 a command (a Lisp function suitable for interactive calling),
 a string (treated as a keyboard macro or a sequence of input events),
 a keymap (to define a prefix key),
 a symbol (when the key is looked up, the symbol will stand for its
    function definition, which should at that time be one of the above,
    or another symbol whose function definition is used, etc.),
 a cons (STRING . DEFN), meaning that DEFN is the definition
    (DEFN should be a valid definition in its own right) and
    STRING is the menu item name (which is used only if the containing
    keymap has been created with a menu name, see `make-keymap'),
 or a cons (MAP . CHAR), meaning use definition of CHAR in keymap MAP,
 or an extended menu item definition.
 (See info node `(elisp)Extended Menu Items'.)"
  (declare (compiler-macro (lambda (form) (keymap--compile-check key) form)))
  (keymap--check key)
  ;; If we're binding this key to another key, then parse that other
  ;; key, too.
  (when (stringp definition)
    (keymap--check definition)
    (setq definition (key-parse definition)))
  (define-key keymap (key-parse key) definition))

(defun keymap-global-set (key command &optional interactive)
  "Give KEY a global binding as COMMAND.
When called interactively, KEY is a key sequence.  When called from
Lisp, KEY is a string that must satisfy `key-valid-p'.

COMMAND is the command definition to use.  When called interactively,
this function prompts for COMMAND and accepts only names of known
commands, i.e., symbols that satisfy the `commandp' predicate.  When
called from Lisp, COMMAND can be anything that `keymap-set' accepts
as its DEFINITION argument.

If COMMAND is a string (which can only happen when this function is
called from Lisp), it must satisfy `key-valid-p'.

Note that if KEY has a local binding in the current buffer,
that local binding will continue to shadow any global binding
that you make with this function."
  (declare (compiler-macro (lambda (form) (keymap--compile-check key) form))
           (advertised-calling-convention (key command) "29.1"))
  (interactive "KSet key globally: \nCSet key %s globally to command: \np")
  (when interactive
    (setq key (key-description key)))
  (keymap-set (current-global-map) key command))

(defun keymap-local-set (key command &optional interactive)
  "Give KEY a local binding as COMMAND.
When called interactively, KEY is a key sequence.  When called from
Lisp, KEY is a string that must satisfy `key-valid-p'.

COMMAND is the command definition to use.  When called interactively,
this function prompts for COMMAND and accepts only names of known
commands, i.e., symbols that satisfy the `commandp' predicate.  When
called from Lisp, COMMAND can be anything that `keymap-set' accepts
as its DEFINITION argument.

If COMMAND is a string (which can only happen when this function is
called from Lisp), it must satisfy `key-valid-p'.

The binding goes in the current buffer's local keymap, which in most
cases is shared with all other buffers in the same major mode."
  (declare (compiler-macro (lambda (form) (keymap--compile-check key) form))
           (advertised-calling-convention (key command) "29.1"))
  (interactive "KSet key locally: \nCSet key %s locally to command: \np")
  (let ((map (current-local-map)))
    (unless map
      (use-local-map (setq map (make-sparse-keymap))))
    (when interactive
      (setq key (key-description key)))
    (keymap-set map key command)))

(defun keymap-global-unset (key &optional remove)
  "Remove global binding of KEY (if any).
When called interactively, KEY is a key sequence.  When called from
Lisp, KEY is a string that satisfies `key-valid-p'.

If REMOVE is non-nil (interactively, the prefix arg), remove the
binding instead of unsetting it.  See `keymap-unset' for details."
  (declare (compiler-macro (lambda (form) (keymap--compile-check key) form)))
  (interactive
   (list (key-description (read-key-sequence "Unset key globally: "))
         current-prefix-arg))
  (keymap-unset (current-global-map) key remove))

(defun keymap-local-unset (key &optional remove)
  "Remove local binding of KEY (if any).
When called interactively, KEY is a key sequence.  When called from
Lisp, KEY is a string that satisfies `key-valid-p'.

If REMOVE is non-nil (interactively, the prefix arg), remove the
binding instead of unsetting it.  See `keymap-unset' for details."
  (declare (compiler-macro (lambda (form) (keymap--compile-check key) form)))
  (interactive
   (list (key-description (read-key-sequence "Unset key locally: "))
         current-prefix-arg))
  (when (current-local-map)
    (keymap-unset (current-local-map) key remove)))

(defun keymap-unset (keymap key &optional remove)
  "Remove key sequence KEY from KEYMAP.
KEY is a string that satisfies `key-valid-p'.

If REMOVE is non-nil, remove the binding instead of unsetting it.
This only makes a difference when there's a parent keymap.  When
unsetting a key in a child map, it will still shadow the same key
in the parent keymap.  Removing the binding will allow the key in
the parent keymap to be used."
  (declare (compiler-macro (lambda (form) (keymap--compile-check key) form)))
  (keymap--check key)
  (define-key keymap (key-parse key) nil remove))

(defun keymap-substitute (keymap olddef newdef &optional oldmap prefix)
  "Replace OLDDEF with NEWDEF for any keys in KEYMAP now defined as OLDDEF.
In other words, OLDDEF is replaced with NEWDEF wherever it appears.
Alternatively, if optional fourth argument OLDMAP is specified, we redefine
in KEYMAP as NEWDEF those keys that are defined as OLDDEF in OLDMAP.

If you don't specify OLDMAP, you can usually get the same results
in a cleaner way with command remapping, like this:
  (define-key KEYMAP [remap OLDDEF] NEWDEF)
\n(fn KEYMAP OLDDEF NEWDEF &optional OLDMAP)"
  ;; Don't document PREFIX in the doc string because we don't want to
  ;; advertise it.  It's meant for recursive calls only.  Here's its
  ;; meaning

  ;; If optional argument PREFIX is specified, it should be a key
  ;; prefix, a string.  Redefined bindings will then be bound to the
  ;; original key, with PREFIX added at the front.
  (unless prefix
    (setq prefix ""))
  (let* ((scan (or oldmap keymap))
	 (prefix1 (vconcat prefix [nil]))
	 (key-substitution-in-progress
	  (cons scan key-substitution-in-progress)))
    ;; Scan OLDMAP, finding each char or event-symbol that
    ;; has any definition, and act on it with hack-key.
    (map-keymap
     (lambda (char defn)
       (aset prefix1 (length prefix) char)
       (substitute-key-definition-key defn olddef newdef prefix1 keymap))
     scan)))

(defun keymap-set-after (keymap key definition &optional after)
  "Add binding in KEYMAP for KEY => DEFINITION, right after AFTER's binding.
This is like `keymap-set' except that the binding for KEY is placed
just after the binding for the event AFTER, instead of at the beginning
of the map.  Note that AFTER must be an event type (like KEY), NOT a command
\(like DEFINITION).

If AFTER is t or omitted, the new binding goes at the end of the keymap.
AFTER should be a single event type--a symbol or a character, not a sequence.

Bindings are always added before any inherited map.

The order of bindings in a keymap matters only when it is used as
a menu, so this function is not useful for non-menu keymaps."
  (declare (indent defun)
           (compiler-macro (lambda (form) (keymap--compile-check key) form)))
  (keymap--check key)
  (when (eq after t) (setq after nil)) ; nil and t are treated the same
  (when (stringp after)
    (keymap--check after)
    (setq after (key-parse after)))
  ;; If we're binding this key to another key, then parse that other
  ;; key, too.
  (when (stringp definition)
    (keymap--check definition)
    (setq definition (key-parse definition)))
  (define-key-after keymap (key-parse key) definition
    after))

(defun key-parse (keys)
  "Convert KEYS to the internal Emacs key representation.
KEYS should be a string describing a key sequence in the format
returned by \\[describe-key] (`describe-key')."
  (declare (pure t) (side-effect-free t))
  ;; A pure function is expected to preserve the match data.
  (save-match-data
    (let ((case-fold-search nil)
          (len (length keys)) ; We won't alter keys in the loop below.
          (pos 0)
          (res []))
      (while (and (< pos len)
                  (string-match "[^ \t\n\f]+" keys pos))
        (let* ((word-beg (match-beginning 0))
               (word-end (match-end 0))
               (word (substring keys word-beg len))
               (times 1)
               key)
          ;; Try to catch events of the form "<as df>".
          (if (string-match "\\`<[^ <>\t\n\f][^>\t\n\f]*>" word)
              (setq word (match-string 0 word)
                    pos (+ word-beg (match-end 0)))
            (setq word (substring keys word-beg word-end)
                  pos word-end))
          (when (string-match "\\([0-9]+\\)\\*." word)
            (setq times (string-to-number (substring word 0 (match-end 1))))
            (setq word (substring word (1+ (match-end 1)))))
          (cond ((string-match "^<<.+>>$" word)
                 (setq key (vconcat (if (eq (key-binding [?\M-x])
                                            'execute-extended-command)
                                        [?\M-x]
                                      (or (car (where-is-internal
                                                'execute-extended-command))
                                          [?\M-x]))
                                    (substring word 2 -2) "\r")))
                ((and (string-match "^\\(\\([ACHMsS]-\\)*\\)<\\(.+\\)>$" word)
                      (progn
                        (setq word (concat (match-string 1 word)
                                           (match-string 3 word)))
                        (not (string-match
                              "\\<\\(NUL\\|RET\\|LFD\\|TAB\\|ESC\\|SPC\\|DEL\\)$"
                              word))))
                 (setq key (list (intern word))))
                ((or (equal word "REM") (string-match "^;;" word))
                 (setq pos (string-match "$" keys pos)))
                (t
                 (let ((orig-word word) (prefix 0) (bits 0))
                   (while (string-match "^[ACHMsS]-." word)
                     (setq bits (+ bits
                                   (cdr
                                    (assq (aref word 0)
                                          '((?A . ?\A-\0) (?C . ?\C-\0)
                                            (?H . ?\H-\0) (?M . ?\M-\0)
                                            (?s . ?\s-\0) (?S . ?\S-\0))))))
                     (setq prefix (+ prefix 2))
                     (setq word (substring word 2)))
                   (when (string-match "^\\^.$" word)
                     (setq bits (+ bits ?\C-\0))
                     (setq prefix (1+ prefix))
                     (setq word (substring word 1)))
                   (let ((found (assoc word '(("NUL" . "\0") ("RET" . "\r")
                                              ("LFD" . "\n") ("TAB" . "\t")
                                              ("ESC" . "\e") ("SPC" . " ")
                                              ("DEL" . "\177")))))
                     (when found (setq word (cdr found))))
                   (when (string-match "^\\\\[0-7]+$" word)
                     (let ((n 0))
                       (dolist (ch (cdr (string-to-list word)))
                         (setq n (+ (* n 8) ch -48)))
                       (setq word (vector n))))
                   (cond ((= bits 0)
                          (setq key word))
                         ((and (= bits ?\M-\0) (stringp word)
                               (string-match "^-?[0-9]+$" word))
                          (setq key (mapcar (lambda (x) (+ x bits))
                                            (append word nil))))
                         ((/= (length word) 1)
                          (error "%s must prefix a single character, not %s"
                                 (substring orig-word 0 prefix) word))
                         ((and (/= (logand bits ?\C-\0) 0) (stringp word)
                               ;; We used to accept . and ? here,
                               ;; but . is simply wrong,
                               ;; and C-? is not used (we use DEL instead).
                               (string-match "[@-_a-z]" word))
                          (setq key (list (+ bits (- ?\C-\0)
                                             (logand (aref word 0) 31)))))
                         (t
                          (setq key (list (+ bits (aref word 0)))))))))
          (when key
            (dolist (_ (number-sequence 1 times))
              (setq res (vconcat res key))))))
      res)))

(defun key-valid-p (keys)
  "Return non-nil if KEYS, a string, is a valid key sequence.
KEYS should be a string consisting of one or more key strokes,
with a single space character separating one key stroke from another.

Each key stroke is either a single character, or the name of an
event, surrounded by angle brackets <like-this>.  In addition, any
key stroke may be preceded by one or more modifier keys.  Finally,
a limited number of characters have a special shorthand syntax.

Here are some example of valid key sequences.

  \"f\"           (the key `f')
  \"S o m\"       (a three-key sequence of the keys `S', `o' and `m')
  \"C-c o\"       (a two-key sequence: the key `c' with the control modifier
                 followed by the key `o')
  \"H-<left>\"    (the function key named \"left\" with the hyper modifier)
  \"M-RET\"       (the \"return\" key with a meta modifier)
  \"C-M-<space>\" (the \"space\" key with both the control and meta modifiers)

These are the characters that have special shorthand syntax:
NUL, RET, TAB, LFD, ESC, SPC, DEL.

Modifiers have to be specified in this order:

   A-C-H-M-S-s

which is

   Alt-Control-Hyper-Meta-Shift-super"
  (declare (pure t) (side-effect-free error-free))
  (let ((case-fold-search nil))
    (and
     (stringp keys)
     (string-match-p "\\`[^ ]+\\( [^ ]+\\)*\\'" keys)
     (save-match-data
       (catch 'exit
         (let ((prefixes
                "\\(A-\\)?\\(C-\\)?\\(H-\\)?\\(M-\\)?\\(S-\\)?\\(s-\\)?"))
           (dolist (key (split-string keys " "))
             ;; Every key might have these modifiers, and they should be
             ;; in this order.
             (when (string-match (concat "\\`" prefixes) key)
               (setq key (substring key (match-end 0))))
             (unless (or (and (= (length key) 1)
                              ;; Don't accept control characters as keys.
                              (not (< (aref key 0) ?\s))
                              ;; Don't accept Meta'd characters as keys.
                              (or (multibyte-string-p key)
                                  (not (<= 127 (aref key 0) 255))))
                         (and (string-match-p "\\`<[-_A-Za-z0-9]+>\\'" key)
                              ;; Don't allow <M-C-down>.
                              (= (progn
                                   (string-match
                                    (concat "\\`<" prefixes) key)
                                   (match-end 0))
                                 1))
                         (string-match-p
                          "\\`\\(NUL\\|RET\\|TAB\\|LFD\\|ESC\\|SPC\\|DEL\\)\\'"
                          key))
               ;; Invalid.
               (throw 'exit nil)))
           t))))))

(defun key-translate (from to)
  "Translate character FROM to TO on the current terminal.
This function creates a `keyboard-translate-table' if necessary
and then modifies one entry in it.

Both FROM and TO should be specified by strings that satisfy `key-valid-p'."
  (declare (compiler-macro
            (lambda (form) (keymap--compile-check from to) form)))
  (keymap--check from)
  (keymap--check to)
  (or (char-table-p keyboard-translate-table)
      (setq keyboard-translate-table
            (make-char-table 'keyboard-translate-table nil)))
  (aset keyboard-translate-table
        (aref (key-parse from) 0)
        (aref (key-parse to) 0)))

(defun keymap-lookup (keymap key &optional accept-default no-remap position)
  "Return the binding for command KEY in KEYMAP.
KEY is a string that satisfies `key-valid-p'.

If KEYMAP is nil, look up in the current keymaps.  If non-nil, it
should either be a keymap or a list of keymaps, and only these
keymap(s) will be consulted.

The binding is probably a symbol with a function definition.

Normally, `keymap-lookup' ignores bindings for t, which act as
default bindings, used when nothing else in the keymap applies;
this makes it usable as a general function for probing keymaps.
However, if the optional second argument ACCEPT-DEFAULT is
non-nil, `keymap-lookup' does recognize the default bindings,
just as `read-key-sequence' does.

Like the normal command loop, `keymap-lookup' will remap the
command resulting from looking up KEY by looking up the command
in the current keymaps.  However, if the optional third argument
NO-REMAP is non-nil, `keymap-lookup' returns the unmapped
command.

If KEY is a mouse gesture, the keymaps used depend on the clicked
mouse position with regards to the buffer, and local keymaps, if any,
on display and overlay strings.

If the optional argument POSITION is non-nil, it specifies a mouse
position as returned by `event-start' and `event-end', and the lookup
occurs in the keymaps associated with it instead of KEY.  It can also
be a number or marker, in which case the keymap properties at the
specified buffer position are used instead of point."
  (declare (compiler-macro (lambda (form) (keymap--compile-check key) form)))
  (keymap--check key)
  (when (and keymap position)
    (error "Can't pass in both keymap and position"))
  (if keymap
      (let ((value (lookup-key keymap (key-parse key) accept-default)))
        (if (and (not no-remap)
                   (symbolp value))
            (or (command-remapping value) value)
          value))
    (key-binding (key-parse key) accept-default no-remap position)))

(defun keymap-local-lookup (keys &optional accept-default)
  "Return the binding for command KEYS in current local keymap only.
KEY is a string that satisfies `key-valid-p'.

The binding is probably a symbol with a function definition.

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `keymap-lookup' for more details
about this."
  (declare (compiler-macro (lambda (form) (keymap--compile-check keys) form)))
  (when-let ((map (current-local-map)))
    (keymap-lookup map keys accept-default)))

(defun keymap-global-lookup (keys &optional accept-default message)
  "Return the binding for command KEYS in current global keymap only.
KEY is a string that satisfies `key-valid-p'.

The binding is probably a symbol with a function definition.
This function's return values are the same as those of `keymap-lookup'
\(which see).

If optional argument ACCEPT-DEFAULT is non-nil, recognize default
bindings; see the description of `keymap-lookup' for more details
about this.

If MESSAGE (and interactively), message the result."
  (declare (compiler-macro (lambda (form) (keymap--compile-check keys) form)))
  (interactive
   (list (key-description (read-key-sequence "Look up key in global keymap: "))
         nil t))
  (let ((def (keymap-lookup (current-global-map) keys accept-default)))
    (when message
      (message "%s is bound to %s globally" keys def))
    def))


;;; define-keymap and defvar-keymap

(defun define-keymap--compile (form &rest args)
  ;; This compiler macro is only there for compile-time
  ;; error-checking; it does not change the call in any way.
  (while (and args
              (keywordp (car args))
              (not (eq (car args) :menu)))
    (unless (memq (car args) '(:full :keymap :parent :suppress :name :prefix))
      (byte-compile-warn-x (car args) "Invalid keyword: %s" (car args)))
    (setq args (cdr args))
    (when (null args)
      (byte-compile-warn-x form "Uneven number of keywords in %S" form))
    (setq args (cdr args)))
  ;; Bindings.
  (while args
    (let* ((wargs args)
           (key (pop args)))
      (when (and (stringp key) (not (key-valid-p key)))
        (byte-compile-warn-x wargs "Invalid `kbd' syntax: %S" key)))
    (when (null args)
      (byte-compile-warn-x form "Uneven number of key bindings in %S" form))
    (setq args (cdr args)))
  form)

(defun define-keymap (&rest definitions)
  "Create a new keymap and define KEY/DEFINITION pairs as key bindings.
Return the new keymap.

Options can be given as keywords before the KEY/DEFINITION
pairs.  Available keywords are:

:full      If non-nil, create a chartable alist (see `make-keymap').
             If nil (i.e., the default), create a sparse keymap (see
             `make-sparse-keymap').

:suppress  If non-nil, the keymap will be suppressed (see `suppress-keymap').
             If `nodigits', treat digits like other chars.

:parent    If non-nil, this should be a keymap to use as the parent
             (see `set-keymap-parent').

:keymap    If non-nil, instead of creating a new keymap, the given keymap
             will be destructively modified instead.

:name      If non-nil, this should be a string to use as the menu for
             the keymap in case you use it as a menu with `x-popup-menu'.

:prefix    If non-nil, this should be a symbol to be used as a prefix
             command (see `define-prefix-command').  If this is the case,
             this symbol is returned instead of the map itself.

KEY/DEFINITION pairs are as KEY and DEF in `keymap-set'.  KEY can
also be the special symbol `:menu', in which case DEFINITION
should be a MENU form as accepted by `easy-menu-define'.

\(fn &key FULL PARENT SUPPRESS NAME PREFIX KEYMAP &rest [KEY DEFINITION]...)"
  (declare (indent defun)
           (compiler-macro define-keymap--compile))
  (let (full suppress parent name prefix keymap)
    ;; Handle keywords.
    (while (and definitions
                (keywordp (car definitions))
                (not (eq (car definitions) :menu)))
      (let ((keyword (pop definitions)))
        (unless definitions
          (error "Missing keyword value for %s" keyword))
        (let ((value (pop definitions)))
          (pcase keyword
            (:full (setq full value))
            (:keymap (setq keymap value))
            (:parent (setq parent value))
            (:suppress (setq suppress value))
            (:name (setq name value))
            (:prefix (setq prefix value))
            (_ (error "Invalid keyword: %s" keyword))))))

    (when (and prefix
               (or full parent suppress keymap))
      (error "A prefix keymap can't be defined with :full/:parent/:suppress/:keymap keywords"))

    (when (and keymap full)
      (error "Invalid combination: :keymap with :full"))

    (let ((keymap (cond
                   (keymap keymap)
                   (prefix (define-prefix-command prefix nil name))
                   (full (make-keymap name))
                   (t (make-sparse-keymap name))))
          seen-keys)
      (when suppress
        (suppress-keymap keymap (eq suppress 'nodigits)))
      (when parent
        (set-keymap-parent keymap parent))

      ;; Do the bindings.
      (while definitions
        (let ((key (pop definitions)))
          (unless definitions
            (error "Uneven number of key/definition pairs"))
          (let ((def (pop definitions)))
            (if (eq key :menu)
                (easy-menu-define nil keymap "" def)
              (if (member key seen-keys)
                  (error "Duplicate definition for key: %S %s" key keymap)
                (push key seen-keys))
              (keymap-set keymap key def)))))
      keymap)))

(defmacro defvar-keymap (variable-name &rest defs)
  "Define VARIABLE-NAME as a variable with a keymap definition.
See `define-keymap' for an explanation of the keywords and KEY/DEFINITION.

In addition to the keywords accepted by `define-keymap', this
macro also accepts a `:doc' keyword, which (if present) is used
as the variable documentation string.

The `:repeat' keyword can also be specified; it controls the
`repeat-mode' behavior of the bindings in the keymap.  When it is
non-nil, all commands in the map will have the `repeat-map'
symbol property.

More control is available over which commands are repeatable; the
value can also be a property list with properties `:enter' and
`:exit', for example:

     :repeat (:enter (commands ...) :exit (commands ...))

`:enter' specifies the list of additional commands that only
enter `repeat-mode'.  When the list is empty, then only the
commands defined in the map enter `repeat-mode'.  Specifying a
list of commands is useful when there are commands that have the
`repeat-map' symbol property, but don't exist in this specific
map.

`:exit' is a list of commands that exit `repeat-mode'.  When the
list is empty, no commands in the map exit `repeat-mode'.
Specifying a list of commands is useful when those commands exist
in this specific map, but should not have the `repeat-map' symbol
property.

\(fn VARIABLE-NAME &key DOC FULL PARENT SUPPRESS NAME PREFIX KEYMAP REPEAT &rest [KEY DEFINITION]...)"
  (declare (indent 1))
  (let ((opts nil)
        doc repeat props)
    (while (and defs
                (keywordp (car defs))
                (not (eq (car defs) :menu)))
      (let ((keyword (pop defs)))
        (unless defs
          (error "Uneven number of keywords"))
        (cond
         ((eq keyword :doc) (setq doc (pop defs)))
         ((eq keyword :repeat) (setq repeat (pop defs)))
         (t (push keyword opts)
            (push (pop defs) opts)))))
    (unless (zerop (% (length defs) 2))
      (error "Uneven number of key/definition pairs: %s" defs))

    (let ((defs defs)
          key seen-keys)
      (while defs
        (setq key (pop defs))
        (pop defs)
        (when (not (eq key :menu))
          (if (member key seen-keys)
              (error "Duplicate definition for key '%s' in keymap '%s'"
                     key variable-name)
            (push key seen-keys)))))

    (when repeat
      (let ((defs defs)
            def)
        (dolist (def (plist-get repeat :enter))
          (push `(put ',def 'repeat-map ',variable-name) props))
        (while defs
          (pop defs)
          (setq def (pop defs))
          (when (and (memq (car def) '(function quote))
                     (not (memq (cadr def) (plist-get repeat :exit))))
            (push `(put ,def 'repeat-map ',variable-name) props)))))

    (let ((defvar-form
           `(defvar ,variable-name
              (define-keymap ,@(nreverse opts) ,@defs)
              ,@(and doc (list doc)))))
      (if props
          `(progn
             ,defvar-form
             ,@(nreverse props))
        defvar-form))))

(defun make-non-key-event (symbol)
  "Mark SYMBOL as an event that shouldn't be returned from `where-is'."
  (put symbol 'non-key-event t)
  symbol)

(provide 'keymap)

;;; keymap.el ends here
