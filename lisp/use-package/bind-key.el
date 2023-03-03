;;; bind-key.el --- A simple way to manage personal keybindings  -*- lexical-binding: t; -*-

;; Copyright (c) 2012-2023 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@newartisans.com>
;; Maintainer: John Wiegley <johnw@newartisans.com>
;; Created: 16 Jun 2012
;; Version: 2.4.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: keys keybinding config dotemacs extensions
;; URL: https://github.com/jwiegley/use-package

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

;; If you have lots of keybindings set in your init file, it can be
;; hard to know which ones you haven't set yet, and which may now be
;; overriding some new default in a new Emacs version.  This module
;; aims to solve that problem.
;;
;; Bind keys as follows in your init file:
;;
;;   (bind-key "C-c x" 'my-ctrl-c-x-command)
;;
;; If the keybinding argument is a vector, it is passed straight to
;; `define-key', so remapping a key with `[remap COMMAND]' works as
;; expected:
;;
;;   (bind-key [remap original-ctrl-c-x-command] 'my-ctrl-c-x-command)
;;
;; If you want the keybinding to override all minor modes that may also bind
;; the same key, use the `bind-key*' form:
;;
;;   (bind-key* "<C-return>" 'other-window)
;;
;; If you want to rebind a key only in a particular keymap, use:
;;
;;   (bind-key "C-c x" 'my-ctrl-c-x-command some-other-mode-map)
;;
;; To unbind a key within a keymap (for example, to stop your favorite major
;; mode from changing a binding that you don't want to override everywhere),
;; use `unbind-key':
;;
;;   (unbind-key "C-c x" some-other-mode-map)
;;
;; To bind multiple keys at once, or set up a prefix map, a `bind-keys' macro
;; is provided.  It accepts keyword arguments, please see its documentation
;; for a detailed description.
;;
;; To add keys into a specific map, use :map argument
;;
;;    (bind-keys :map dired-mode-map
;;               ("o" . dired-omit-mode)
;;               ("a" . some-custom-dired-function))
;;
;; To set up a prefix map, use `:prefix-map' and `:prefix' arguments (both are
;; required)
;;
;;    (bind-keys :prefix-map my-customize-prefix-map
;;               :prefix "C-c c"
;;               ("f" . customize-face)
;;               ("v" . customize-variable))
;;
;; You can combine all the keywords together.  Additionally,
;; `:prefix-docstring' can be specified to set documentation of created
;; `:prefix-map' variable.
;;
;; To bind multiple keys in a `bind-key*' way (to be sure that your bindings
;; will not be overridden by other modes), you may use `bind-keys*' macro:
;;
;;    (bind-keys*
;;     ("C-o" . other-window)
;;     ("C-M-n" . forward-page)
;;     ("C-M-p" . backward-page))
;;
;; After Emacs loads, you can see a summary of all your personal keybindings
;; currently in effect with this command:
;;
;;   M-x describe-personal-keybindings
;;
;; This display will tell you if you've overridden a default keybinding, and
;; what the default was.  Also, it will tell you if the key was rebound after
;; your binding it with `bind-key', and what it was rebound it to.
;;
;; See the `use-package' info manual for more information.

;;; Code:

(require 'cl-lib)
(require 'easy-mmode)

(defgroup bind-key nil
  "A simple way to manage personal keybindings."
  :group 'keyboard
  :group 'convenience
  :link '(emacs-commentary-link :tag "Commentary" "bind-key.el")
  :version "29.1")

(defcustom bind-key-column-widths '(18 . 40)
  "Width of columns in `describe-personal-keybindings'."
  :type '(cons integer integer)
  :group 'bind-key)

(defcustom bind-key-segregation-regexp
  "\\`\\(\\(C-[chx] \\|M-[gso] \\)\\([CM]-\\)?\\|.+-\\)"
  "Regexp used by \\[describe-personal-keybindings] to divide key sets."
  :type 'regexp
  :group 'bind-key)

(defcustom bind-key-describe-special-forms nil
  "If non-nil, extract docstrings from lambdas, closures and keymaps if possible."
  :type 'boolean
  :group 'bind-key)

;; Create override-global-mode to force key remappings

(defvar override-global-map (make-keymap)
  "Keymap for `override-global-mode'.")

(define-minor-mode override-global-mode
  "A minor mode for allowing keybindings to override other modes.
The main purpose of this mode is to simplify bindings keys in
such a way that they take precedence over other modes.

To achieve this, the keymap `override-global-map' is added to
`emulation-mode-map-alists', which makes it take precedence over
keymaps in `minor-mode-map-alist'.  Thereby, key bindings get an
even higher precedence than global key bindings defined with
`keymap-global-set' (or, in Emacs 28 or older, `global-set-key').

The macro `bind-key*' (which see) provides a convenient way to
add keys to that keymap."
  :init-value t
  :lighter "")

;; the keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists
             `((override-global-mode . ,override-global-map)))

(defvar personal-keybindings nil
  "List of bindings performed by `bind-key'.

Elements have the form ((KEY . [MAP]) CMD ORIGINAL-CMD)")

;;;###autoload
(defmacro bind-key (key-name command &optional keymap predicate)
  "Bind KEY-NAME to COMMAND in KEYMAP (`global-map' if not passed).

KEY-NAME may be a vector, in which case it is passed straight to
`define-key'.  Or it may be a string to be interpreted as
spelled-out keystrokes, e.g., \"C-c C-z\".  See the documentation
of `edmacro-mode' for details.

COMMAND must be an interactive function or lambda form.

KEYMAP, if present, should be a keymap variable or symbol.
For example:

  (bind-key \"M-h\" #\\='some-interactive-function my-mode-map)

  (bind-key \"M-h\" #\\='some-interactive-function \\='my-mode-map)

If PREDICATE is non-nil, it is a form evaluated to determine when
a key should be bound. It must return non-nil in such cases.
Emacs can evaluate this form at any time that it does redisplay
or operates on menu data structures, so you should write it so it
can safely be called at any time."
  (let ((namevar (make-symbol "name"))
        (keyvar (make-symbol "key"))
        (kmapvar (make-symbol "kmap"))
        (kdescvar (make-symbol "kdesc"))
        (bindingvar (make-symbol "binding")))
    `(let* ((,namevar ,key-name)
            (,keyvar ,(if (stringp key-name) (read-kbd-macro key-name)
                        `(if (vectorp ,namevar) ,namevar
                           (read-kbd-macro ,namevar))))
            (,kmapvar (or (if (and ,keymap (symbolp ,keymap))
                              (symbol-value ,keymap) ,keymap)
                          global-map))
            (,kdescvar (cons (if (stringp ,namevar) ,namevar
                               (key-description ,namevar))
                             (if (symbolp ,keymap) ,keymap (quote ,keymap))))
            (,bindingvar (lookup-key ,kmapvar ,keyvar)))
       (let ((entry (assoc ,kdescvar personal-keybindings))
             (details (list ,command
                            (unless (numberp ,bindingvar)
                              ,bindingvar))))
         (if entry
             (setcdr entry details)
           (add-to-list 'personal-keybindings (cons ,kdescvar details))))
       ,(if predicate
            `(define-key ,kmapvar ,keyvar
               '(menu-item "" nil :filter (lambda (&optional _)
                                            (when ,predicate
                                              ,command))))
          `(define-key ,kmapvar ,keyvar ,command)))))

;;;###autoload
(defmacro unbind-key (key-name &optional keymap)
  "Unbind the given KEY-NAME, within the KEYMAP (if specified).
See `bind-key' for more details."
  (let ((namevar (make-symbol "name"))
        (kdescvar (make-symbol "kdesc")))
    `(let* ((,namevar ,key-name)
            (,kdescvar (cons (if (stringp ,namevar) ,namevar
                               (key-description ,namevar))
                             (if (symbolp ,keymap) ,keymap (quote ,keymap)))))
       (bind-key--remove (if (vectorp ,namevar) ,namevar
                           (read-kbd-macro ,namevar))
                         (or (if (and ,keymap (symbolp ,keymap))
                                 (symbol-value ,keymap) ,keymap)
                             global-map))
       (setq personal-keybindings
             (cl-delete-if (lambda (k) (equal (car k) ,kdescvar))
                           personal-keybindings))
       nil)))

(defun bind-key--remove (key keymap)
  "Remove KEY from KEYMAP.

In contrast to `define-key', this function removes the binding from the keymap."
  (define-key keymap key nil)
  ;; Split M-key in ESC key
  (setq key (cl-mapcan (lambda (k)
                         (if (and (integerp k) (/= (logand k ?\M-\0) 0))
                             (list ?\e (logxor k ?\M-\0))
                           (list k)))
                       key))
  ;; Delete single keys directly
  (if (= (length key) 1)
      (delete key keymap)
    ;; Lookup submap and delete key from there
    (let* ((prefix (vconcat (butlast key)))
           (submap (lookup-key keymap prefix)))
      (unless (keymapp submap)
        (error "Not a keymap for %s" key))
      (when (symbolp submap)
        (setq submap (symbol-function submap)))
      (delete (last key) submap)
      ;; Delete submap if it is empty
      (when (= 1 (length submap))
        (bind-key--remove prefix keymap)))))

;;;###autoload
(defmacro bind-key* (key-name command &optional predicate)
  "Similar to `bind-key', but overrides any mode-specific bindings."
  `(bind-key ,key-name ,command override-global-map ,predicate))

(defun bind-keys-form (args keymap)
  "Bind multiple keys at once.

Accepts keyword arguments:
:map MAP               - a keymap into which the keybindings should be
                         added
:prefix KEY            - prefix key for these bindings
:prefix-map MAP        - name of the prefix map that should be created
                         for these bindings
:prefix-docstring STR  - docstring for the prefix-map variable
:menu-name NAME        - optional menu string for prefix map
:repeat-docstring STR  - docstring for the repeat-map variable
:repeat-map MAP        - name of the repeat map that should be created
                         for these bindings. If specified, the
                         `repeat-map' property of each command bound
                         (within the scope of the `:repeat-map' keyword)
                         is set to this map.
:exit BINDINGS         - Within the scope of `:repeat-map' will bind the
                         key in the repeat map, but will not set the
                         `repeat-map' property of the bound command.
:continue BINDINGS     - Within the scope of `:repeat-map' forces the
                         same behavior as if no special keyword had
                         been used (that is, the command is bound, and
                         it's `repeat-map' property set)
:filter FORM           - optional form to determine when bindings apply

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted)."
  (let (map
        prefix-doc
        prefix-map
        prefix
        repeat-map
        repeat-doc
        repeat-type ;; Only used internally
        filter
        menu-name
        pkg)

    ;; Process any initial keyword arguments
    (let ((cont t)
          (arg-change-func 'cddr))
      (while (and cont args)
        (if (cond ((and (eq :map (car args))
                        (not prefix-map))
                   (setq map (cadr args)))
                  ((eq :prefix-docstring (car args))
                   (setq prefix-doc (cadr args)))
                  ((and (eq :prefix-map (car args))
                        (not (memq map '(global-map
                                         override-global-map))))
                   (setq prefix-map (cadr args)))
                  ((eq :repeat-docstring (car args))
                   (setq repeat-doc (cadr args)))
                  ((and (eq :repeat-map (car args))
                        (not (memq map '(global-map
                                         override-global-map))))
                   (setq repeat-map (cadr args))
                   (setq map repeat-map))
                  ((eq :continue (car args))
                   (setq repeat-type :continue
                         arg-change-func 'cdr))
                  ((eq :exit (car args))
                   (setq repeat-type :exit
                         arg-change-func 'cdr))
                  ((eq :prefix (car args))
                   (setq prefix (cadr args)))
                  ((eq :filter (car args))
                   (setq filter (cadr args)) t)
                  ((eq :menu-name (car args))
                   (setq menu-name (cadr args)))
                  ((eq :package (car args))
                   (setq pkg (cadr args))))
            (setq args (funcall arg-change-func args))
          (setq cont nil))))

    (when (or (and prefix-map (not prefix))
              (and prefix (not prefix-map)))
      (error "Both :prefix-map and :prefix must be supplied"))

    (when repeat-type
      (unless repeat-map
        (error ":continue and :exit require specifying :repeat-map")))

    (when (and menu-name (not prefix))
      (error "If :menu-name is supplied, :prefix must be too"))

    (unless map (setq map keymap))

    ;; Process key binding arguments
    (let (first next)
      (while args
        (if (keywordp (car args))
            (progn
              (setq next args)
              (setq args nil))
          (if first
              (nconc first (list (car args)))
            (setq first (list (car args))))
          (setq args (cdr args))))

      (cl-flet
          ((wrap (map bindings)
                 (if (and map pkg (not (memq map '(global-map
                                                   override-global-map))))
                     `((if (boundp ',map)
                           ,(macroexp-progn bindings)
                         (eval-after-load
                             ,(if (symbolp pkg) `',pkg pkg)
                           ',(macroexp-progn bindings))))
                   bindings)))

        (append
         (when prefix-map
           `((defvar ,prefix-map)
             ,@(when prefix-doc `((put ',prefix-map 'variable-documentation ,prefix-doc)))
             ,@(if menu-name
                   `((define-prefix-command ',prefix-map nil ,menu-name))
                 `((define-prefix-command ',prefix-map)))
             ,@(if (and map (not (eq map 'global-map)))
                   (wrap map `((bind-key ,prefix ',prefix-map ,map ,filter)))
                 `((bind-key ,prefix ',prefix-map nil ,filter)))))
         (when repeat-map
           `((defvar ,repeat-map (make-sparse-keymap)
               ,@(when repeat-doc `(,repeat-doc)))))
         (wrap map
               (cl-mapcan
                (lambda (form)
                  (let ((fun (and (cdr form) (list 'function (cdr form)))))
                    (if prefix-map
                        `((bind-key ,(car form) ,fun ,prefix-map ,filter))
                      (if (and map (not (eq map 'global-map)))
                          ;; Only needed in this branch, since when
                          ;; repeat-map is non-nil, map is always
                          ;; non-nil
                          `(,@(when (and repeat-map (not (eq repeat-type :exit)))
                                `((put ,fun 'repeat-map ',repeat-map)))
                            (bind-key ,(car form) ,fun ,map ,filter))
                        `((bind-key ,(car form) ,fun nil ,filter))))))
                first))
         (when next
           (bind-keys-form `(,@(when repeat-map `(:repeat-map ,repeat-map))
                             ,@(if pkg
                                   (cons :package (cons pkg next))
                                 next)) map)))))))

;;;###autoload
(defmacro bind-keys (&rest args)
  "Bind multiple keys at once.

Accepts keyword arguments:
:map MAP               - a keymap into which the keybindings should be
                         added
:prefix KEY            - prefix key for these bindings
:prefix-map MAP        - name of the prefix map that should be created
                         for these bindings
:prefix-docstring STR  - docstring for the prefix-map variable
:menu-name NAME        - optional menu string for prefix map
:repeat-docstring STR  - docstring for the repeat-map variable
:repeat-map MAP        - name of the repeat map that should be created
                         for these bindings. If specified, the
                         `repeat-map' property of each command bound
                         (within the scope of the `:repeat-map' keyword)
                         is set to this map.
:exit BINDINGS         - Within the scope of `:repeat-map' will bind the
                         key in the repeat map, but will not set the
                         `repeat-map' property of the bound command.
:continue BINDINGS     - Within the scope of `:repeat-map' forces the
                         same behavior as if no special keyword had
                         been used (that is, the command is bound, and
                         it's `repeat-map' property set)
:filter FORM           - optional form to determine when bindings apply

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted)."
  (macroexp-progn (bind-keys-form args nil)))

;;;###autoload
(defmacro bind-keys* (&rest args)
  "Bind multiple keys at once, in `override-global-map'.
Accepts the same keyword arguments as `bind-keys' (which see).

This binds keys in such a way that bindings are not overridden by
other modes.  See `override-global-mode'."
  (macroexp-progn (bind-keys-form args 'override-global-map)))

(defun bind-key--get-binding-description (elem)
  (cond
   ((listp elem)
    (cond
     ((memq (car elem) '(lambda function))
      (if (and bind-key-describe-special-forms
               (stringp (nth 2 elem)))
          (nth 2 elem)
        "#<lambda>"))
     ((eq 'closure (car elem))
      (if (and bind-key-describe-special-forms
               (stringp (nth 3 elem)))
          (nth 3 elem)
        "#<closure>"))
     ((eq 'keymap (car elem))
      "#<keymap>")
     (t
      elem)))
   ;; must be a symbol, non-symbol keymap case covered above
   ((and bind-key-describe-special-forms (keymapp elem))
    (let ((doc (get elem 'variable-documentation)))
      (if (stringp doc) doc elem)))
   ((symbolp elem)
    elem)
   (t
    "#<byte-compiled lambda>")))

(defun bind-key--compare-keybindings (l r)
  (let* ((regex bind-key-segregation-regexp)
         (lgroup (and (string-match regex (caar l))
                      (match-string 0 (caar l))))
         (rgroup (and (string-match regex (caar r))
                      (match-string 0 (caar r))))
         (lkeymap (cdar l))
         (rkeymap (cdar r)))
    (cond
     ((and (null lkeymap) rkeymap)
      (cons t t))
     ((and lkeymap (null rkeymap))
      (cons nil t))
     ((and lkeymap rkeymap
           (not (string= (symbol-name lkeymap) (symbol-name rkeymap))))
      (cons (string< (symbol-name lkeymap) (symbol-name rkeymap)) t))
     ((and (null lgroup) rgroup)
      (cons t t))
     ((and lgroup (null rgroup))
      (cons nil t))
     ((and lgroup rgroup)
      (if (string= lgroup rgroup)
          (cons (string< (caar l) (caar r)) nil)
        (cons (string< lgroup rgroup) t)))
     (t
      (cons (string< (caar l) (caar r)) nil)))))

;;;###autoload
(defun describe-personal-keybindings ()
  "Display all the personal keybindings defined by `bind-key'."
  (interactive)
  (with-output-to-temp-buffer "*Personal Keybindings*"
    (princ (format (concat "Key name%s Command%s Comments\n%s %s "
                           "---------------------\n")
                   (make-string (- (car bind-key-column-widths) 9) ? )
                   (make-string (- (cdr bind-key-column-widths) 8) ? )
                   (make-string (1- (car bind-key-column-widths)) ?-)
                   (make-string (1- (cdr bind-key-column-widths)) ?-)))
    (let (last-binding)
      (dolist (binding
               (setq personal-keybindings
                     (sort personal-keybindings
                           (lambda (l r)
                             (car (bind-key--compare-keybindings l r))))))

        (if (not (eq (cdar last-binding) (cdar binding)))
            (princ (format "\n\n%s: %s\n%s\n\n"
                           (cdar binding) (caar binding)
                           (make-string (+ 21 (car bind-key-column-widths)
                                           (cdr bind-key-column-widths)) ?-)))
          (if (and last-binding
                   (cdr (bind-key--compare-keybindings last-binding binding)))
              (princ "\n")))

        (let* ((key-name (caar binding))
               (at-present (lookup-key (or (symbol-value (cdar binding))
                                           (current-global-map))
                                       (read-kbd-macro key-name)))
               (command (nth 1 binding))
               (was-command (nth 2 binding))
               (command-desc (bind-key--get-binding-description command))
               (was-command-desc (and was-command
                                      (bind-key--get-binding-description was-command)))
               (at-present-desc (bind-key--get-binding-description at-present)))
          (let ((line
                 (format
                  (format "%%-%ds%%-%ds%%s\n" (car bind-key-column-widths)
                          (cdr bind-key-column-widths))
                  key-name (format "`%s'" command-desc)
                  (if (string= command-desc at-present-desc)
                      (if (or (null was-command)
                              (string= command-desc was-command-desc))
                          ""
                        (format "was `%s'" was-command-desc))
                    (format "[now: `%s']" at-present)))))
            (princ (if (string-match "[ \t]+\n" line)
                       (replace-match "\n" t t line)
                     line))))

        (setq last-binding binding)))))

(define-obsolete-function-alias 'get-binding-description
  'bind-key--get-binding-description "30.1")
(define-obsolete-function-alias 'compare-keybindings
  'bind-key--compare-keybindings "30.1")

(provide 'bind-key)

;; Local Variables:
;; outline-regexp: ";;;\\(;* [^\s\t\n]\\|###autoload\\)\\|("
;; End:

;;; bind-key.el ends here
