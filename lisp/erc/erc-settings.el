;;; erc-settings.el -- Buffer-local options  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module is based on an ancient proof-of-concept by Mario Lang,
;; with contributions from Diane Murray and possibly others.  It lived
;; in erc-networks.el for over two decades before becoming its own
;; module in 2026.

;; Usage: add `settings' to `erc-modules', and customize the option
;; `erc-settings'.

;;; Code:
(require 'erc)

(defgroup erc-settings nil
  "Buffer-local values for arbitrary user options."
  :group 'erc)

(define-widget 'erc--buffer-match 'lazy
  "Condition for `erc-settings--buffer-match-p'."
  :tag "Valid `erc-settings' match condition"
  :type `(choice (boolean :tag "Always/never")
                 (regexp :tag "Buffer-name regexp")
                 (function :tag "Predicate")
                 (cons :tag "Network" (const network) symbol)
                 (cons :tag "ID" (const id) symbol)
                 (cons :tag "Target" (const target) (choice string
                                                            (const nil)))
                 (cons :tag "Buffer name" (const name) string)
                 (list :tag "Negation"
                       (const not)
                       (erc--buffer-match :tag "Match condition"))
                 (cons :tag "All/any"
                       (choice :tag "Operator" (const or) (const and))
                       (repeat :tag "Match condition" erc--buffer-match))))

(defcustom erc-settings ()
  "A match-condition alist for setting ERC's user options locally.

This \"meta option\" provides a means of specifying buffer-local values
for other user options.  It does this by setting an option's variable
locally in server and target buffers.  If a local binding already exists
for a variable, ERC leaves it alone.  Alist members should be of the
form (MATCHCOND . BINDINGS), where BINDINGS is a list of assignments
like (VAR VAL . FLAGS).

MATCHCOND works like a `buffer-match-p' condition, only tailored
slightly to meet ERC's unique requirements.  The boolean operators
`and', `or', and `not' remain unchanged, as do strings, which ERC treats
as regexps to be matched against a buffer's name.  For servers, this
name is usually a dialed TCP address while connecting and a network (ID)
once connected.  Query and channel buffers normally share their target's
name, possibly suffixed by a disambiguating \"@\" + identifier.  If
MATCHCOND is a function, ERC assumes it's a predicate that takes no
arguments and runs in the candidate ERC buffer.  As for key-value
cons-cell conditions, ERC ignores all traditional ones defined by
`buffer-match-p', like `derived-mode' and `major-mode', instead
preferring to define its own: (network . NETWORK), where NETWORK is a
symbol returned by the function `erc-network'; (id . ID), where ID is a
user-provided symbol as described by the Info node `(erc) Network
Identifier'; (target . TARGET), where TARGET, if non-nil, is the channel
name or query nick; and (name . NAME), which matches a buffer's NAME, as
a string.

In BINDINGS, each member's VAR is the symbol of a user option or
variable to which ERC assigns the sexp VAL locally in the current
buffer.  When FLAGS contains the keyword :eval, ERC evaluates VAL as a
Lisp form and assigns the result to VAR.  Each entry in BINDINGS should
appear in order of decreasing match specificity and increasing
generality because even though ERC applies all matching entries, it
skips subsequent ones for which a local binding already exists.

See Info node `(erc) Settings' for a friendlier description of the DSL
and Info node `(erc) Settings Examples' for example usage."
  :package-version '(ERC . "5.7")
  :type (let* ((flags '(set :tag "Flags"
                            (const :tag "Prefer custom-set (advanced)" :custom)
                            (const :tag "Evaluate form" :eval)))
               (setting `(cons :tag "Binding assignment"
                               (variable :tag "Option/variable")
                               (cons :tag "Value" sexp ,flags))))
          `(alist :key-type erc--buffer-match :value-type (repeat ,setting))))

;;;###autoload(autoload 'erc-settings-mode "erc-settings" nil t)
(define-erc-module settings nil
  "Global module to set ERC options locally per-buffer.
Like many modules, this cannot reasonably be toggled via mode command
mid-session.  To see the result of updates to the option `erc-settings'
immediately, destroy and reopen all affected sessions."
  ((when (and erc--updating-modules-p
              (not (memq #'erc-settings--set-modules
                         (default-value 'erc--set-modules-functions))))
     (erc--warn-once-before-connect 'erc-settings-mode
       "Module `settings' did not load properly. Add it to `erc-modules'"
       " and run `erc-update-modules' before connecting, or use a"
       " Custom-aware configuration method, like `setopt'."))
   ;; Set non-`erc-module' entries in target buffers after all default
   ;; non-connection related local session variables have been set.
   (if erc--updating-modules-p
       (add-hook 'change-major-mode-after-body-hook
                 #'erc-settings--setup -90 t)
     (erc-buffer-do #'erc-settings--setup))
   ;; Set any remaining entries in server buffers once the network is
   ;; known and all local session variables have been populated.
   (add-hook 'erc-after-connect #'erc-settings-bind -30)
   (add-hook 'erc--set-modules-functions #'erc-settings--set-modules 30))
  ((erc-buffer-do #'erc-settings--setup)
   (remove-hook 'erc-after-connect #'erc-settings-bind)
   (remove-hook 'erc--set-modules-functions #'erc-settings--set-modules)))

(defun erc-settings--setup ()
  (if erc-settings-mode
      (when erc-server-connected
        (erc-settings--set (erc-settings--gather-bindings (current-buffer))))
    (remove-hook 'change-major-mode-after-body-hook #'erc-settings--setup t)
    (dolist (entry erc-settings)
      (pcase-dolist (`(,var . ,_) (cdr entry))
        (when (local-variable-p var)
          (kill-local-variable var))))))

(defun erc-settings--buffer-match-p (condition)
  "Return non-nil if CONDITION matches current buffer.
Act almost like `buffer-match-p', except recognize an alternate set of
property-based cons-cell conditions.  Additionally, don't pass any
arguments to predicate-type conditions, and interpret CONDITION in the
current buffer, where CONDITION should be among the following:

- the symbol t, which always matches,
- the symbol nil, which never matches,
- a regular expression matched against current buffer's name,
- a predicate taking no arguments and run in the candidate buffer
- a symbol-keyed cons cell described by one the following:

         `id' `eq' to current buffer's network context ID
    `network' `eq' to current buffer's `erc-network'
     `target' `equal' to current buffer's `erc-target'
       `name' `equal' to current buffer's name
        `and'  a list of matching conditions
         `or'  a list containing at least one matching condition
        `not'  a list of a single condition that matches when negated"
  (pcase condition
    ('t t)
    ((pred stringp) (string-match-p condition (buffer-name)))
    ((pred functionp) (funcall condition))
    (`(id . ,id) (and erc-networks--id
                      (eq (erc-networks--id-symbol erc-networks--id) id)))
    (`(network . ,network) (and erc-network (eq erc-network network)))
    (`(target . ,target) (equal target (erc-target)))
    (`(name . ,name) (equal name (buffer-name)))
    (`(not . ,cond) (not (erc-settings--buffer-match-p cond)))
    (`(or . ,args) (seq-some #'erc-settings--buffer-match-p args))
    (`(and . ,args) (seq-every-p #'erc-settings--buffer-match-p args))))

(defun erc-settings-bind (&rest _)
  "Set all settings in option `erc-settings' from current network's ID."
  (erc-settings--set (erc-settings--gather-bindings (current-buffer))))

;;;###autoload
(defun erc-settings--set-modules (id target target-server-buffer)
  "Set `erc-modules' locally if an entry exists for the current buffer.
In server buffers, bind all matching entries, not just `erc-modules'.
Expect TARGET to be a string or nil and ID to be a symbol.  When TARGET
is non-nil, borrow local values from TARGET-SERVER-BUFFER to temporarily
bind identifying variables sought by match conditions."
  (when (and erc-settings (or erc-settings-mode (memq 'settings erc-modules)))
    ;; Shadow `erc-networks--id' for (id . <id>), `erc--target' for
    ;; (target . <target>), etc.  Although predicates of interest, like
    ;; `erc-query-buffer-p', may call on `buffer-local-value', its
    ;; `buffer' argument will be the current buffer, which doesn't yet
    ;; have any buffer-local bindings.
    (cl-assert (not (local-variable-p 'erc-networks--id)))
    (cl-assert (not (local-variable-p 'erc-network)))
    (cl-assert (not (local-variable-p 'erc--target)))
    (let ((erc-networks--id
           (or (and id (erc-networks--id-create id))
               (and target (buffer-local-value 'erc-networks--id
                                               target-server-buffer)))))
      (if target
          (if-let*
              ((erc-network (buffer-local-value 'erc-network
                                                target-server-buffer))
               (erc--target (erc--target-from-string target))
               (bindings (erc-settings--gather-bindings (current-buffer)))
               (entry (assq 'erc-modules bindings)))
              (erc-settings--set-value 'erc-modules (nth 1 entry) (cddr entry))
            (when-let* ((old-value (with-current-buffer target-server-buffer
                                     (and (local-variable-p 'erc-modules)
                                          erc-modules))))
              (setq-local erc-modules old-value)))
        (erc-settings-bind)))))

(defun erc-settings--gather-bindings (buffer)
  "Return a list of matching bindings for server BUFFER's session."
  (with-current-buffer buffer
    (mapcan (lambda (entry)
              (and (erc-settings--buffer-match-p (car entry))
                   (copy-sequence (cdr entry))))
            erc-settings)))

;; Currently, when a binding contains the experimental flag :custom, ERC
;; tries to use VAR's `custom-set' function, if defined, for setting its
;; value. ERC doesn't do so by default because many such functions use
;; `set-default', which defeats the purpose.
(defun erc-settings--set-value (var value flags)
  "Set VAR to VALUE locally and interpret FLAGS.
Do nothing if VAR already has a local binding.  If VAR has the symbol
property `erc-settings--wrap', assume it's a function that takes the
arguments (SETTER VAR VALUE), and defer to it to perform the actual
setting."
  (unless (local-variable-p var)
    (let ((setter #'set)
          (evalp nil)
          (customp nil))
      (dolist (flag flags)
        (pcase-exhaustive flag
          (:eval (setq evalp t))
          (:custom (setq customp t))))
      (make-local-variable var)
      (when evalp
        (setq value (eval value t)))
      (when customp
        (custom-load-symbol var)
        (setq setter (or (get var 'custom-set) #'set)))
      (if-let* ((xsetter (get var 'erc-settings--wrap)))
          (funcall xsetter setter var value)
        (funcall setter var value)))))

(defun erc-settings--set (bindings)
  "Set all matching bindings locally in current buffer.
Expect BINDINGS to be a list of (KEY VALUE . FLAGS)."
  (pcase-dolist (`(,var ,value . ,flags) bindings)
    (erc-settings--set-value var value flags)))

(defun erc-settings--ensure-prompt-reset (setter var value)
  (cl-assert (eq var 'erc-prompt))
  (when (and erc-input-marker (stringp value))
    (let ((erc-prompt (lambda () value)))
      (with-silent-modifications
        (erc--refresh-prompt))))
  (funcall setter var value))

(put 'erc-prompt 'erc-settings--wrap #'erc-settings--ensure-prompt-reset)

(defun erc-settings--find-condition (predicate settings)
  "Return all conditions in SETTINGS for which PREDICATE returns non-nil."
  (letrec ((found ())
           (find (lambda (condition)
                   (pcase condition
                     (`(and . ,rest) (mapc find rest))
                     (`(or . ,rest) (mapc find rest))
                     (`(not ,cond) (funcall find cond))
                     ((pred (funcall predicate)) (push condition found))))))
    (dolist (setting settings)
      (funcall find (car setting)))
    (nreverse found)))

(defun erc-settings--extract-ids ()
  "Return all arguments to an `id' property condition in `erc-settings'."
  (mapcar #'cdr (erc-settings--find-condition
                 (lambda (condition) (eq (car-safe condition) 'id))
                 erc-settings)))

;;;###autoload
(defun erc-settings-connect-by-id (network-id)
  "Connect via some preconfigured network ID specified in `erc-settings'.
See Info node `(erc) ID-based Settings Example'."
  (interactive (list (completing-read "Network ID: "
                                      (erc-settings--extract-ids))))
  (erc-tls :server nil :port nil :nick nil :user nil
           :password nil :full-name nil :id network-id))

(provide 'erc-settings)

;;; erc-settings.el ends here
;;
;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
