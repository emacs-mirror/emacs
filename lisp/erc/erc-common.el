;;; erc-common.el --- Macros and types for ERC  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.
;;
;; Maintainer: Amin Bandali <bandali@gnu.org>, F. Jason Park <jp@neverwas.me>
;; Keywords: comm, IRC, chat, client, internet
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(eval-when-compile (require 'cl-lib) (require 'subr-x))
(require 'erc-compat)

(defvar erc--casemapping-rfc1459)
(defvar erc--casemapping-rfc1459-strict)
(defvar erc-channel-users)
(defvar erc-dbuf)
(defvar erc-log-p)
(defvar erc-server-users)
(defvar erc-session-server)

(declare-function erc--get-isupport-entry "erc-backend" (key &optional single))
(declare-function erc-get-buffer "erc" (target &optional proc))
(declare-function erc-server-buffer "erc" nil)

(cl-defstruct erc-input
  string insertp sendp)

(cl-defstruct (erc--input-split (:include erc-input))
  lines cmdp)

(cl-defstruct (erc-server-user (:type vector) :named)
  ;; User data
  nickname host login full-name info
  ;; Buffers
  (buffers nil))

(cl-defstruct (erc-channel-user (:type vector) :named)
  voice halfop op admin owner
  ;; Last message time (in the form of the return value of
  ;; (current-time)
  ;;
  ;; This is useful for ordered name completion.
  (last-message-time nil))

(cl-defstruct erc--target
  (string "" :type string :documentation "Received name of target.")
  (symbol nil :type symbol :documentation "Case-mapped name as symbol."))

;; At some point, it may make sense to add a query type with an
;; account field, which may help support reassociation across
;; reconnects and nick changes (likely requires v3 extensions).
;;
;; These channel variants should probably take on a `joined' field to
;; track "joinedness", which `erc-server-JOIN', `erc-server-PART',
;; etc. should toggle.  Functions like `erc--current-buffer-joined-p'
;; may find it useful.

(cl-defstruct (erc--target-channel (:include erc--target)))
(cl-defstruct (erc--target-channel-local (:include erc--target-channel)))

;; Beginning in 5.5/29.1, the `tags' field may take on one of two
;; differing types.  See `erc-tags-format' for details.

(cl-defstruct (erc-response (:conc-name erc-response.))
  (unparsed "" :type string)
  (sender "" :type string)
  (command "" :type string)
  (command-args '() :type list)
  (contents "" :type string)
  (tags '() :type list))

;; TODO move goodies modules here after 29 is released.
(defconst erc--features-to-modules
  '((erc-pcomplete completion pcomplete)
    (erc-capab capab-identify)
    (erc-join autojoin)
    (erc-page page ctcp-page)
    (erc-sound sound ctcp-sound)
    (erc-stamp stamp timestamp)
    (erc-services services nickserv))
  "Migration alist mapping a library feature to module names.
Keys need not be unique: a library may define more than one
module.  Sometimes a module's downcased alias will be its
canonical name.")

(defconst erc--modules-to-features
  (let (pairs)
    (pcase-dolist (`(,feature . ,names) erc--features-to-modules)
      (dolist (name names)
        (push (cons name feature) pairs)))
    (nreverse pairs))
  "Migration alist mapping a module's name to its home library feature.")

(defconst erc--module-name-migrations
  (let (pairs)
    (pcase-dolist (`(,_ ,canonical . ,rest) erc--features-to-modules)
      (dolist (obsolete rest)
        (push (cons obsolete canonical) pairs)))
    pairs)
  "Association list of obsolete module names to canonical names.")

(defun erc--normalize-module-symbol (symbol)
  "Return preferred SYMBOL for `erc-modules'."
  (setq symbol (intern (downcase (symbol-name symbol))))
  (or (cdr (assq symbol erc--module-name-migrations)) symbol))

(defun erc--assemble-toggle (localp name ablsym mode val body)
  (let ((arg (make-symbol "arg")))
    `(defun ,ablsym ,(if localp `(&optional ,arg) '())
       ,(concat
         (if val "Enable" "Disable")
         " ERC " (symbol-name name) " mode."
         (when localp
           (concat "\nWhen called interactively,"
                   " do so in all buffers for the current connection.")))
       (interactive ,@(when localp '("p")))
       ,@(if localp
             `((when (derived-mode-p 'erc-mode)
                 (if ,arg
                     (erc-with-all-buffers-of-server erc-server-process nil
                       (,ablsym))
                   (setq ,mode ,val)
                   ,@body)))
           `(,(if val
                  `(cl-pushnew ',(erc--normalize-module-symbol name)
                               erc-modules)
                `(setq erc-modules (delq ',(erc--normalize-module-symbol name)
                                         erc-modules)))
             (setq ,mode ,val)
             ,@body)))))

(defmacro define-erc-module (name alias doc enable-body disable-body
                                  &optional local-p)
  "Define a new minor mode using ERC conventions.
Symbol NAME is the name of the module.
Symbol ALIAS is the alias to use, or nil.
DOC is the documentation string to use for the minor mode.
ENABLE-BODY is a list of expressions used to enable the mode.
DISABLE-BODY is a list of expressions used to disable the mode.
If LOCAL-P is non-nil, the mode will be created as a buffer-local
mode, rather than a global one.

This will define a minor mode called erc-NAME-mode, possibly
an alias erc-ALIAS-mode, as well as the helper functions
erc-NAME-enable, and erc-NAME-disable.

With LOCAL-P, these helpers take on an optional argument that,
when non-nil, causes them to act on all buffers of a connection.
This feature is mainly intended for interactive use and does not
carry over to their respective minor-mode toggles.  Beware that
for global modules, these helpers and toggles all mutate
`erc-modules'.

Example:

  ;;;###autoload(autoload \\='erc-replace-mode \"erc-replace\")
  (define-erc-module replace nil
    \"This mode replaces incoming text according to `erc-replace-alist'.\"
    ((add-hook \\='erc-insert-modify-hook
               #\\='erc-replace-insert))
    ((remove-hook \\='erc-insert-modify-hook
                  #\\='erc-replace-insert)))"
  (declare (doc-string 3) (indent defun))
  (let* ((sn (symbol-name name))
         (mode (intern (format "erc-%s-mode" (downcase sn))))
         (group (intern (format "erc-%s" (downcase sn))))
         (enable (intern (format "erc-%s-enable" (downcase sn))))
         (disable (intern (format "erc-%s-disable" (downcase sn)))))
    `(progn
       (define-minor-mode
         ,mode
         ,(format "Toggle ERC %S mode.
With a prefix argument ARG, enable %s if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.
%s" name name doc)
         ;; FIXME: We don't know if this group exists, so this `:group' may
         ;; actually just silence a valid warning about the fact that the var
         ;; is not associated with any group.
         :global ,(not local-p) :group (quote ,group)
         (if ,mode
             (,enable)
           (,disable)))
       ,(erc--assemble-toggle local-p name enable mode t enable-body)
       ,(erc--assemble-toggle local-p name disable mode nil disable-body)
       ,@(and-let* ((alias)
                    ((not (eq name alias)))
                    (aname (intern (format "erc-%s-mode"
                                           (downcase (symbol-name alias))))))
           `((defalias ',aname #',mode)
             (put ',aname 'erc-module ',(erc--normalize-module-symbol name))))
       (put ',mode 'erc-module ',(erc--normalize-module-symbol name))
       ;; For find-function and find-variable.
       (put ',mode    'definition-name ',name)
       (put ',enable  'definition-name ',name)
       (put ',disable 'definition-name ',name))))

(defmacro erc-with-buffer (spec &rest body)
  "Execute BODY in the buffer associated with SPEC.

SPEC should have the form

 (TARGET [PROCESS])

If TARGET is a buffer, use it.  Otherwise, use the buffer
matching TARGET in the process specified by PROCESS.

If PROCESS is nil, use the current `erc-server-process'.
See `erc-get-buffer' for details.

See also `with-current-buffer'.

\(fn (TARGET [PROCESS]) BODY...)"
  (declare (indent 1) (debug ((form &optional form) body)))
  (let ((buf (make-symbol "buf"))
        (proc (make-symbol "proc"))
        (target (make-symbol "target"))
        (process (make-symbol "process")))
    `(let* ((,target ,(car spec))
            (,process ,(cadr spec))
            (,buf (if (bufferp ,target)
                      ,target
                    (let ((,proc (or ,process
                                     (and (processp erc-server-process)
                                          erc-server-process))))
                      (if (and ,target ,proc)
                          (erc-get-buffer ,target ,proc))))))
       (when (buffer-live-p ,buf)
         (with-current-buffer ,buf
           ,@body)))))

(defmacro erc-with-server-buffer (&rest body)
  "Execute BODY in the current ERC server buffer.
If no server buffer exists, return nil."
  (declare (indent 0) (debug (body)))
  (let ((buffer (make-symbol "buffer")))
    `(let ((,buffer (erc-server-buffer)))
       (when (buffer-live-p ,buffer)
         (with-current-buffer ,buffer
           ,@body)))))

(defmacro erc-with-all-buffers-of-server (process pred &rest forms)
  "Execute FORMS in all buffers which have same process as this server.
FORMS will be evaluated in all buffers having the process PROCESS and
where PRED matches or in all buffers of the server process if PRED is
nil."
  (declare (indent 2) (debug (form form body)))
  (macroexp-let2 nil pred pred
    `(erc-buffer-filter (lambda ()
                          (when (or (not ,pred) (funcall ,pred))
                            ,@forms))
                        ,process)))

(defun erc-log-aux (string)
  "Do the debug logging of STRING."
  (let ((cb (current-buffer))
        (point 1)
        (was-eob nil)
        (session-buffer (erc-server-buffer)))
    (if session-buffer
        (progn
          (set-buffer session-buffer)
          (if (not (and erc-dbuf (bufferp erc-dbuf) (buffer-live-p erc-dbuf)))
              (progn
                (setq erc-dbuf (get-buffer-create
                                (concat "*ERC-DEBUG: "
                                        erc-session-server "*")))))
          (set-buffer erc-dbuf)
          (setq point (point))
          (setq was-eob (eobp))
          (goto-char (point-max))
          (insert (concat "** " string "\n"))
          (if was-eob (goto-char (point-max))
            (goto-char point))
          (set-buffer cb))
      (message "ERC: ** %s" string))))

(define-inline erc-log (string)
  "Logs STRING if logging is on (see `erc-log-p')."
  (inline-quote
   (when erc-log-p
     (erc-log-aux ,string))))

(defun erc-downcase (string)
  "Return a downcased copy of STRING with properties.
Use the CASEMAPPING ISUPPORT parameter to determine the style."
  (with-case-table (pcase (erc--get-isupport-entry 'CASEMAPPING 'single)
                     ("ascii" ascii-case-table)
                     ("rfc1459-strict" erc--casemapping-rfc1459-strict)
                     (_ erc--casemapping-rfc1459))
    (downcase string)))

(define-inline erc-get-channel-user (nick)
  "Find NICK in the current buffer's `erc-channel-users' hash table."
  (inline-quote (gethash (erc-downcase ,nick) erc-channel-users)))

(define-inline erc-get-server-user (nick)
  "Find NICK in the current server's `erc-server-users' hash table."
  (inline-letevals (nick)
    (inline-quote (erc-with-server-buffer
                    (gethash (erc-downcase ,nick) erc-server-users)))))

(provide 'erc-common)

;;; erc-common.el ends here
