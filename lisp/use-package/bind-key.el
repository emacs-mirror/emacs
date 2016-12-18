;;; bind-key.el --- A simple way to manage personal keybindings

;; Copyright (c) 2012-2015 john wiegley

;; Author: John Wiegley <jwiegley@gmail.com>
;; Maintainer: John Wiegley <jwiegley@gmail.com>
;; Created: 16 Jun 2012
;; Version: 1.0
;; Keywords: keys keybinding config dotemacs
;; URL: https://github.com/jwiegley/use-package

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the gnu general public license as
;; published by the free software foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  see the gnu
;; general public license for more details.

;; You should have received a copy of the gnu general public license
;; along with gnu emacs; see the file copying.  if not, write to the
;; free software foundation, inc., 59 temple place - suite 330,
;; boston, ma 02111-1307, usa.

;;; Commentary:

;; If you have lots of keybindings set in your .emacs file, it can be hard to
;; know which ones you haven't set yet, and which may now be overriding some
;; new default in a new emacs version.  This module aims to solve that
;; problem.
;;
;; Bind keys as follows in your .emacs:
;;
;;   (require 'bind-key)
;;
;;   (bind-key "C-c x" 'my-ctrl-c-x-command)
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
;; This display will tell you if you've overriden a default keybinding, and
;; what the default was.  Also, it will tell you if the key was rebound after
;; your binding it with `bind-key', and what it was rebound it to.

;;; Code:

(require 'cl-lib)
(require 'easy-mmode)

(defgroup bind-key nil
  "A simple way to manage personal keybindings"
  :group 'emacs)

(defcustom bind-key-column-widths '(18 . 40)
  "Width of columns in `describe-personal-keybindings'."
  :type '(cons integer integer)
  :group 'bind-key)

(defcustom bind-key-segregation-regexp
  "\\`\\(\\(C-[chx] \\|M-[gso] \\)\\([CM]-\\)?\\|.+-\\)"
  "Regular expression used to divide key sets in the output from
\\[describe-personal-keybindings]."
  :type 'regexp
  :group 'bind-key)

(defcustom bind-key-describe-special-forms nil
  "If non-nil, extract docstrings from lambdas, closures and keymaps if possible."
  :type 'boolean
  :group 'bind-key)

;; Create override-global-mode to force key remappings

(defvar override-global-map (make-keymap)
  "override-global-mode keymap")

(define-minor-mode override-global-mode
  "A minor mode so that keymap settings override other modes."
  t "")

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
`define-key'. Or it may be a string to be interpreted as
spelled-out keystrokes, e.g., \"C-c C-z\". See documentation of
`edmacro-mode' for details.

If PREDICATE is non-nil, it is a form evaluated to determine when
a key should be bound. It must return non-nil in such cases.
Emacs can evaluate this form at any time that it does redisplay
or operates on menu data structures, so you should write it so it
can safely be called at any time."
  (let ((namevar (make-symbol "name"))
        (keyvar (make-symbol "key"))
        (kdescvar (make-symbol "kdesc"))
        (bindingvar (make-symbol "binding")))
    `(let* ((,namevar ,key-name)
            (,keyvar (if (vectorp ,namevar) ,namevar
                       (read-kbd-macro ,namevar)))
            (,kdescvar (cons (if (stringp ,namevar) ,namevar
                               (key-description ,namevar))
                             (quote ,keymap)))
            (,bindingvar (lookup-key (or ,keymap global-map) ,keyvar)))
       (add-to-list 'personal-keybindings
                    (list ,kdescvar ,command
                          (unless (numberp ,bindingvar) ,bindingvar)))
       ,(if predicate
            `(define-key (or ,keymap global-map) ,keyvar
               '(menu-item "" nil :filter (lambda (&optional _)
                                            (when ,predicate
                                              ,command))))
          `(define-key (or ,keymap global-map) ,keyvar ,command)))))

;;;###autoload
(defmacro unbind-key (key-name &optional keymap)
  "Unbind the given KEY-NAME, within the KEYMAP (if specified).
See `bind-key' for more details."
  `(progn
     (bind-key ,key-name nil ,keymap)
     (setq personal-keybindings
           (cl-delete-if #'(lambda (k)
                             ,(if keymap
                                  `(and (consp (car k))
                                        (string= (caar k) ,key-name)
                                        (eq (cdar k) ',keymap))
                                `(and (stringp (car k))
                                      (string= (car k) ,key-name))))
                         personal-keybindings))))

;;;###autoload
(defmacro bind-key* (key-name command &optional predicate)
  "Similar to `bind-key', but overrides any mode-specific bindings."
  `(bind-key ,key-name ,command override-global-map ,predicate))

(defun bind-keys-form (args)
  "Bind multiple keys at once.

Accepts keyword arguments:
:map MAP               - a keymap into which the keybindings should be
                         added
:prefix KEY            - prefix key for these bindings
:prefix-map MAP        - name of the prefix map that should be created
                         for these bindings
:prefix-docstring STR  - docstring for the prefix-map variable
:menu-name NAME        - optional menu string for prefix map
:filter FORM           - optional form to determine when bindings apply

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted)."
  ;; jww (2016-02-26): This is a hack; this whole function needs to be
  ;; rewritten to normalize arguments the way that use-package.el does.
  (if (and (eq (car args) :package)
           (not (eq (car (cdr (cdr args))) :map)))
      (setq args (cons :map (cons 'global-map args))))
  (let* ((map (plist-get args :map))
         (doc (plist-get args :prefix-docstring))
         (prefix-map (plist-get args :prefix-map))
         (prefix (plist-get args :prefix))
         (filter (plist-get args :filter))
         (menu-name (plist-get args :menu-name))
         (pkg (plist-get args :package))
         (key-bindings (progn
                         (while (keywordp (car args))
                           (pop args)
                           (pop args))
                         args)))
    (when (or (and prefix-map (not prefix))
              (and prefix (not prefix-map)))
      (error "Both :prefix-map and :prefix must be supplied"))
    (when (and menu-name (not prefix))
      (error "If :menu-name is supplied, :prefix must be too"))
    (let ((args key-bindings)
          saw-map first next)
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
                 (if (and map pkg (not (eq map 'global-map)))
                     `((if (boundp ',map)
                           (progn ,@bindings)
                         (eval-after-load
                             ,(if (symbolp pkg) `',pkg pkg)
                           '(progn ,@bindings))))
                   bindings)))
        (append
         (when prefix-map
           `((defvar ,prefix-map)
             ,@(when doc `((put ',prefix-map 'variable-documentation ,doc)))
             ,@(if menu-name
                   `((define-prefix-command ',prefix-map nil ,menu-name))
                 `((define-prefix-command ',prefix-map)))
             ,@(if (and map (not (eq map 'global-map)))
                   (wrap map `((bind-key ,prefix ',prefix-map ,map ,filter)))
                 `((bind-key ,prefix ',prefix-map nil ,filter)))))
         (wrap map
               (cl-mapcan
                (lambda (form)
                  (if prefix-map
                      `((bind-key ,(car form) ',(cdr form) ,prefix-map ,filter))
                    (if (and map (not (eq map 'global-map)))
                        `((bind-key ,(car form) ',(cdr form) ,map ,filter))
                      `((bind-key ,(car form) ',(cdr form) nil ,filter)))))
                first))
         (when next
           (bind-keys-form
            (if pkg
                (cons :package (cons pkg next))
              next))))))))

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
:filter FORM           - optional form to determine when bindings apply

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted)."
  (macroexp-progn (bind-keys-form args)))

;;;###autoload
(defmacro bind-keys* (&rest args)
  (macroexp-progn
   (bind-keys-form `(:map override-global-map ,@args))))

(defun get-binding-description (elem)
  (cond
   ((listp elem)
    (cond
     ((eq 'lambda (car elem))
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

(defun compare-keybindings (l r)
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
                             (car (compare-keybindings l r))))))

        (if (not (eq (cdar last-binding) (cdar binding)))
            (princ (format "\n\n%s\n%s\n\n"
                           (cdar binding)
                           (make-string (+ 21 (car bind-key-column-widths)
                                           (cdr bind-key-column-widths)) ?-)))
          (if (and last-binding
                   (cdr (compare-keybindings last-binding binding)))
              (princ "\n")))

        (let* ((key-name (caar binding))
               (at-present (lookup-key (or (symbol-value (cdar binding))
                                           (current-global-map))
                                       (read-kbd-macro key-name)))
               (command (nth 1 binding))
               (was-command (nth 2 binding))
               (command-desc (get-binding-description command))
               (was-command-desc (and was-command
                                      (get-binding-description was-command)))
               (at-present-desc (get-binding-description at-present))
               )
          (let ((line
                 (format
                  (format "%%-%ds%%-%ds%%s\n" (car bind-key-column-widths)
                          (cdr bind-key-column-widths))
                  key-name (format "`%s\'" command-desc)
                  (if (string= command-desc at-present-desc)
                      (if (or (null was-command)
                              (string= command-desc was-command-desc))
                          ""
                        (format "was `%s\'" was-command-desc))
                    (format "[now: `%s\']" at-present)))))
            (princ (if (string-match "[ \t]+\n" line)
                       (replace-match "\n" t t line)
                     line))))

        (setq last-binding binding)))))

(provide 'bind-key)

;; Local Variables:
;; outline-regexp: ";;;\\(;* [^\s\t\n]\\|###autoload\\)\\|("
;; indent-tabs-mode: nil
;; End:

;;; bind-key.el ends here
