;;; eglot.el --- Client for Language Server Protocol (LSP) servers  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Version: 0.3
;; Author: João Távora <joaotavora@gmail.com>
;; Maintainer: João Távora <joaotavora@gmail.com>
;; URL: https://github.com/joaotavora/eglot
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "26.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simply M-x eglot should be enough to get you started, but here's a
;; little info (see the accompanying README.md or the URL for more).
;;
;; M-x eglot starts a server via a shell-command guessed from
;; `eglot-server-programs', using the current major-mode (for whatever
;; language you're programming in) as a hint.  If it can't guess, it
;; prompts you in the mini-buffer for these things.  Actually, the
;; server needen't be locally started: you can connect to a running
;; server via TCP by entering a <host:port> syntax.
;;
;; Anyway, if the connection is successful, you should see an `eglot'
;; indicator pop up in your mode-line.  More importantly, this means
;; current *and future* file buffers of that major mode *inside your
;; current project* automatically become \"managed\" by the LSP
;; server, i.e.  information about their contents is exchanged
;; periodically to provide enhanced code analysis via
;; `xref-find-definitions', `flymake-mode', `eldoc-mode',
;; `completion-at-point', among others.
;;
;; To "unmanage" these buffers, shutdown the server with M-x
;; eglot-shutdown.
;;
;;; Code:

(require 'json)
(require 'cl-lib)
(require 'project)
(require 'url-parse)
(require 'url-util)
(require 'pcase)
(require 'compile) ; for some faces
(require 'warnings)
(require 'flymake)
(require 'xref)
(require 'subr-x)
(require 'jrpc)
(require 'filenotify)


;;; User tweakable stuff
(defgroup eglot nil
  "Interaction with Language Server Protocol servers"
  :prefix "eglot-"
  :group 'applications)

(defvar eglot-server-programs '((rust-mode . ("rls"))
                                (python-mode . ("pyls"))
                                (js-mode . ("javascript-typescript-stdio"))
                                (sh-mode . ("bash-language-server" "start"))
                                (php-mode . ("php" "vendor/felixfbecker/\
language-server/bin/php-language-server.php")))
  "Alist of (MAJOR-MODE . CONTACT) mapping major modes to server executables.
CONTACT can be anything accepted by that parameter in the
function `eglot', which see.")

(defface eglot-mode-line
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Face for package-name in EGLOT's mode line.")

(defcustom eglot-autoreconnect 3
  "Control ability to reconnect automatically to the LSP server.
If t, always reconnect automatically (not recommended).  If nil,
never reconnect automatically after unexpected server shutdowns,
crashes or network failures.  A positive integer number says to
only autoreconnect if the previous successful connection attempt
lasted more than that many seconds."
  :type '(choice (boolean :tag "Whether to inhibit autoreconnection")
                 (integer :tag "Number of seconds")))


;;; Process management
(defvar eglot--processes-by-project (make-hash-table :test #'equal)
  "Keys are projects.  Values are lists of processes.")

(jrpc-define-process-var eglot--major-mode nil
  "The major-mode this server is managing.")

(jrpc-define-process-var eglot--capabilities :unreported
  "Holds list of capabilities that server reported")

(jrpc-define-process-var eglot--project nil
  "The project the server belongs to.")

(jrpc-define-process-var eglot--spinner `(nil nil t)
  "\"Spinner\" used by some servers.
A list (ID WHAT DONE-P).")

(jrpc-define-process-var eglot--moribund nil
  "Non-nil if server is about to exit")

(jrpc-define-process-var eglot--inhibit-autoreconnect eglot-autoreconnect
  "If non-nil, don't autoreconnect on unexpected quit.")

(jrpc-define-process-var eglot--file-watches (make-hash-table :test #'equal)
  "File system watches for the didChangeWatchedfiles thingy.")

(defun eglot--on-shutdown (proc)
  ;; Turn off `eglot--managed-mode' where appropriate.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eglot--buffer-managed-p proc)
        (eglot--managed-mode -1))))
  ;; Kill any expensive watches
  (maphash (lambda (_id watches)
             (mapcar #'file-notify-rm-watch watches))
           (eglot--file-watches proc))
  ;; Sever the project/process relationship for proc
  (setf (gethash (eglot--project proc) eglot--processes-by-project)
        (delq proc
              (gethash (eglot--project proc) eglot--processes-by-project)))
  (cond ((eglot--moribund proc))
        ((not (eglot--inhibit-autoreconnect proc))
         (eglot--warn "Reconnecting after unexpected server exit.")
         (eglot-reconnect proc))
        ((timerp (eglot--inhibit-autoreconnect proc))
         (eglot--warn "Not auto-reconnecting, last one didn't last long."))))

(defun eglot-shutdown (proc &optional interactive)
  "Politely ask the server PROC to quit.
Forcefully quit it if it doesn't respond.  Don't leave this
function with the server still running.  INTERACTIVE is t if
called interactively."
  (interactive (list (jrpc-current-process-or-lose) t))
  (when interactive (eglot--message "Asking %s politely to terminate" proc))
  (unwind-protect
      (let ((jrpc-request-timeout 3))
        (setf (eglot--moribund proc) t)
        (jrpc-request proc :shutdown nil)
        ;; this one should always fail under normal conditions
        (ignore-errors (jrpc-request proc :exit nil)))
    (when (process-live-p proc)
      (eglot--warn "Brutally deleting existing process %s" proc)
      (delete-process proc))))

(defun eglot--find-current-process ()
  "The current logical EGLOT process."
  (let* ((probe (or (project-current) `(transient . ,default-directory))))
    (cl-find major-mode (gethash probe eglot--processes-by-project)
             :key #'eglot--major-mode)))

(defun eglot--project-short-name (project)
  "Give PROJECT a short name."
  (file-name-base (directory-file-name (car (project-roots project)))))

(defun eglot--all-major-modes ()
  "Return all know major modes."
  (let ((retval))
    (mapatoms (lambda (sym)
                (when (plist-member (symbol-plist sym) 'derived-mode-parent)
                  (push sym retval))))
    retval))

(defun eglot--client-capabilities ()
  "What the EGLOT LSP client supports."
  (jrpc-obj
   :workspace    (jrpc-obj
                  :applyEdit t
                  :workspaceEdit `(:documentChanges :json-false)
                  :didChangeWatchesFiles `(:dynamicRegistration t)
                  :symbol `(:dynamicRegistration :json-false))
   :textDocument (jrpc-obj
                  :synchronization (jrpc-obj
                                    :dynamicRegistration :json-false
                                    :willSave t :willSaveWaitUntil t :didSave t)
                  :completion         `(:dynamicRegistration :json-false)
                  :hover              `(:dynamicRegistration :json-false)
                  :signatureHelp      `(:dynamicRegistration :json-false)
                  :references         `(:dynamicRegistration :json-false)
                  :definition         `(:dynamicRegistration :json-false)
                  :documentSymbol     `(:dynamicRegistration :json-false)
                  :documentHighlight  `(:dynamicRegistration :json-false)
                  :rename             `(:dynamicRegistration :json-false)
                  :publishDiagnostics `(:relatedInformation :json-false))
   :experimental (jrpc-obj)))

(defvar eglot--command-history nil
  "History of CONTACT arguments to `eglot'.")

(defun eglot--interactive ()
  "Helper for `eglot'."
  (let* ((guessed-mode (if buffer-file-name major-mode))
         (managed-mode
          (cond
           ((or (>= (prefix-numeric-value current-prefix-arg) 16)
                (not guessed-mode))
            (intern
             (completing-read
              "[eglot] Start a server to manage buffers of what major mode? "
              (mapcar #'symbol-name (eglot--all-major-modes)) nil t
              (symbol-name guessed-mode) nil (symbol-name guessed-mode) nil)))
           (t guessed-mode)))
         (project (or (project-current) `(transient . ,default-directory)))
         (guessed-command (cdr (assoc managed-mode eglot-server-programs)))
         (base-prompt "[eglot] Enter program to execute (or <host>:<port>): ")
         (prompt
          (cond (current-prefix-arg base-prompt)
                ((null guessed-command)
                 (concat (format "[eglot] Sorry, couldn't guess for `%s'!"
                                 managed-mode)
                         "\n" base-prompt))
                ((and (listp guessed-command)
                      (not (integerp (cadr guessed-command)))
                      (not (executable-find (car guessed-command))))
                 (concat (format "[eglot] I guess you want to run `%s'"
                                 (combine-and-quote-strings guessed-command))
                         (format ", but I can't find `%s' in PATH!"
                                 (car guessed-command))
                         "\n" base-prompt))))
         (contact
          (cond ((not prompt) guessed-command)
                (t
                 (let ((string (read-shell-command
                                prompt
                                (if (listp guessed-command)
                                    (combine-and-quote-strings guessed-command))
                                'eglot-command-history)))
                   (if (and string (string-match
                                    "^\\([^\s\t]+\\):\\([[:digit:]]+\\)$"
                                    (string-trim string)))
                       (list (match-string 1 string) (match-string 2 string))
                     (split-string-and-unquote string)))))))
    (list managed-mode project contact t)))

;;;###autoload
(defun eglot (managed-major-mode project contact &optional interactive)
  "Manage a project with a Language Server Protocol (LSP) server.

The LSP server is started (or contacted) via COMMAND.  If this
operation is successful, current *and future* file buffers of
MANAGED-MAJOR-MODE inside PROJECT automatically become
\"managed\" by the LSP server, meaning information about their
contents is exchanged periodically to provide enhanced
code-analysis via `xref-find-definitions', `flymake-mode',
`eldoc-mode', `completion-at-point', among others.

Interactively, the command attempts to guess MANAGED-MAJOR-MODE
from current buffer, CONTACT from `eglot-server-programs' and
PROJECT from `project-current'.  If it can't guess, the user is
prompted.  With a single \\[universal-argument] prefix arg, it
always prompt for COMMAND.  With two \\[universal-argument]
prefix args, also prompts for MANAGED-MAJOR-MODE.

PROJECT is a project instance as returned by `project-current'.

CONTACT is a list of strings (COMMAND [ARGS...]) specifying how
to start a server subprocess to connect to.  If the second
element in the list is an integer number instead of a string, the
list is interpreted as (HOST PORT [PARAMETERS...]) to connect to
an existing server via TCP, the remaining PARAMETERS being given
as `open-network-stream's optional arguments.  CONTACT can also
be a function of no arguments returning a live connected process
object.

MANAGED-MAJOR-MODE is an Emacs major mode.

INTERACTIVE is t if called interactively."
  (interactive (eglot--interactive))
  (let* ((short-name (eglot--project-short-name project)))
    (let ((current-process (jrpc-current-process)))
      (if (and (process-live-p current-process)
               interactive
               (y-or-n-p "[eglot] Live process found, reconnect instead? "))
          (eglot-reconnect current-process interactive)
        (when (process-live-p current-process)
          (eglot-shutdown current-process))
        (let ((proc (eglot--connect project
                                    managed-major-mode
                                    (format "%s/%s" short-name managed-major-mode)
                                    contact)))
          (eglot--message "Connected! Process `%s' now \
managing `%s' buffers in project `%s'."
                          proc managed-major-mode short-name)
          proc)))))

(defun eglot-reconnect (process &optional interactive)
  "Reconnect to PROCESS.
INTERACTIVE is t if called interactively."
  (interactive (list (jrpc-current-process-or-lose) t))
  (when (process-live-p process)
    (eglot-shutdown process interactive))
  (eglot--connect (eglot--project process)
                  (eglot--major-mode process)
                  (jrpc-name process)
                  (jrpc-contact process))
  (eglot--message "Reconnected!"))

(defalias 'eglot-events-buffer 'jrpc-events-buffer)

(defvar eglot-connect-hook nil "Hook run after connecting in `eglot--connect'.")

(defun eglot--dispatch (proc method id params)
  "Dispatcher passed to `jrpc-connect'.
Builds a function from METHOD, passes it PROC, ID and PARAMS."
  (let* ((handler-sym (intern (concat "eglot--server-" method))))
    (if (functionp handler-sym) ;; FIXME: fails if params is array, not object
        (apply handler-sym proc (append params (if id `(:id ,id))))
      (jrpc-reply proc id
                  :error (jrpc-obj :code -32601 :message "Unimplemented")))))

(defun eglot--connect (project managed-major-mode name contact)
  (let* ((contact (if (functionp contact) (funcall contact) contact))
         (proc (jrpc-connect name contact #'eglot--dispatch #'eglot--on-shutdown))
         success)
    (setf (eglot--project proc) project)
    (setf (eglot--major-mode proc)managed-major-mode)
    (push proc (gethash project eglot--processes-by-project))
    (run-hook-with-args 'eglot-connect-hook proc)
    (unwind-protect
        (cl-destructuring-bind (&key capabilities)
            (jrpc-request
             proc
             :initialize
             (jrpc-obj :processId (unless (eq (process-type proc)
                                              'network)
                                    (emacs-pid))
                       :rootPath  (car (project-roots project))
                       :rootUri  (eglot--path-to-uri
                                  (car (project-roots project)))
                       :initializationOptions  []
                       :capabilities (eglot--client-capabilities)))
          (setf (eglot--capabilities proc) capabilities)
          (setf (jrpc-status proc) nil)
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (eglot--maybe-activate-editing-mode proc)))
          (jrpc-notify proc :initialized (jrpc-obj :__dummy__ t))
          (setf (eglot--inhibit-autoreconnect proc)
                (cond
                 ((booleanp eglot-autoreconnect) (not eglot-autoreconnect))
                 ((cl-plusp eglot-autoreconnect)
                  (run-with-timer eglot-autoreconnect nil
                                  (lambda ()
                                    (setf (eglot--inhibit-autoreconnect proc)
                                          (null eglot-autoreconnect)))))))
          (setq success proc))
      (unless (or success (not (process-live-p proc)) (eglot--moribund proc))
        (eglot-shutdown proc)))))

(defun eglot--server-ready-p (_what _proc)
  "Tell if server of PROC ready for processing deferred WHAT."
  (not (eglot--outstanding-edits-p)))


;;; Helpers
;;;
(defun eglot--error (format &rest args)
  "Error out with FORMAT with ARGS."
  (error "[eglot] %s" (apply #'format format args)))

(defun eglot--message (format &rest args)
  "Message out with FORMAT with ARGS."
  (message "[eglot] %s" (apply #'format format args)))

(defun eglot--warn (format &rest args)
  "Warning message with FORMAT and ARGS."
  (apply #'eglot--message (concat "(warning) " format) args)
  (let ((warning-minimum-level :error))
    (display-warning 'eglot
                     (apply #'format format args)
                     :warning)))

(defun eglot--pos-to-lsp-position (&optional pos)
  "Convert point POS to LSP position."
  (save-excursion
    (jrpc-obj :line
              ;; F!@(#*&#$)CKING OFF-BY-ONE
              (1- (line-number-at-pos pos t))
              :character
              (- (goto-char (or pos (point)))
                 (line-beginning-position)))))

(defun eglot--lsp-position-to-point (pos-plist)
  "Convert LSP position POS-PLIST to Emacs point."
  (save-excursion (goto-char (point-min))
                  (forward-line (plist-get pos-plist :line))
                  (forward-char
                   (min (plist-get pos-plist :character)
                        (- (line-end-position)
                           (line-beginning-position))))
                  (point)))

(defun eglot--path-to-uri (path)
  "URIfy PATH."
  (url-hexify-string
   (concat "file://" (if (eq system-type 'windows-nt) "/") (file-truename path))
   url-path-allowed-chars))

(defun eglot--uri-to-path (uri)
  "Convert URI to a file path."
  (when (keywordp uri) (setq uri (substring (symbol-name uri) 1)))
  (let ((retval (url-filename (url-generic-parse-url (url-unhex-string uri)))))
    (if (eq system-type 'windows-nt) (substring retval 1) retval)))

(defconst eglot--kind-names
  `((1 . "Text") (2 . "Method") (3 . "Function") (4 . "Constructor")
    (5 . "Field") (6 . "Variable") (7 . "Class") (8 . "Interface")
    (9 . "Module") (10 . "Property") (11 . "Unit") (12 . "Value")
    (13 . "Enum") (14 . "Keyword") (15 . "Snippet") (16 . "Color")
    (17 . "File") (18 . "Reference")))

(defun eglot--format-markup (markup)
  "Format MARKUP according to LSP's spec."
  (cond ((stringp markup)
         (with-temp-buffer
           (ignore-errors (funcall (intern "markdown-mode"))) ;escape bytecomp
           (font-lock-ensure)
           (insert markup)
           (string-trim (buffer-string))))
        (t
         (with-temp-buffer
           (ignore-errors (funcall (intern (concat
                                            (plist-get markup :language)
                                            "-mode" ))))
           (insert (plist-get markup :value))
           (font-lock-ensure)
           (buffer-string)))))

(defun eglot--server-capable (feat)
  "Determine if current server is capable of FEAT."
  (plist-get (eglot--capabilities (jrpc-current-process-or-lose)) feat))

(defun eglot--range-region (range)
  "Return region (BEG . END) that represents LSP RANGE."
  (cons (eglot--lsp-position-to-point (plist-get range :start))
        (eglot--lsp-position-to-point (plist-get range :end))))


;;; Minor modes
;;;
(defvar eglot-mode-map (make-sparse-keymap))

(define-minor-mode eglot--managed-mode
  "Mode for source buffers managed by some EGLOT project."
  nil nil eglot-mode-map
  (cond
   (eglot--managed-mode
    (add-hook 'jrpc-find-process-functions 'eglot--find-current-process nil t)
    (add-hook 'jrpc-ready-predicates 'eglot--server-ready-p nil t)
    (add-hook 'after-change-functions 'eglot--after-change nil t)
    (add-hook 'before-change-functions 'eglot--before-change nil t)
    (add-hook 'flymake-diagnostic-functions 'eglot-flymake-backend nil t)
    (add-hook 'kill-buffer-hook 'eglot--signal-textDocument/didClose nil t)
    (add-hook 'before-revert-hook 'eglot--signal-textDocument/didClose nil t)
    (add-hook 'before-save-hook 'eglot--signal-textDocument/willSave nil t)
    (add-hook 'after-save-hook 'eglot--signal-textDocument/didSave nil t)
    (add-hook 'xref-backend-functions 'eglot-xref-backend nil t)
    (add-hook 'completion-at-point-functions #'eglot-completion-at-point nil t)
    (add-function :before-until (local 'eldoc-documentation-function)
                  #'eglot-eldoc-function)
    (add-function :around (local imenu-create-index-function) #'eglot-imenu))
   (t
    (remove-hook 'jrpc-find-process-functions 'eglot--find-current-process t)
    (remove-hook 'jrpc-ready-predicates 'eglot--server-ready-p t)
    (remove-hook 'flymake-diagnostic-functions 'eglot-flymake-backend t)
    (remove-hook 'after-change-functions 'eglot--after-change t)
    (remove-hook 'before-change-functions 'eglot--before-change t)
    (remove-hook 'kill-buffer-hook 'eglot--signal-textDocument/didClose t)
    (remove-hook 'before-revert-hook 'eglot--signal-textDocument/didClose t)
    (remove-hook 'before-save-hook 'eglot--signal-textDocument/willSave t)
    (remove-hook 'after-save-hook 'eglot--signal-textDocument/didSave t)
    (remove-hook 'xref-backend-functions 'eglot-xref-backend t)
    (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
    (remove-function (local 'eldoc-documentation-function)
                     #'eglot-eldoc-function)
    (remove-function (local imenu-create-index-function) #'eglot-imenu)
    (let ((proc (eglot--find-current-process)))
      (when (and (process-live-p proc) (y-or-n-p "[eglot] Kill server too? "))
        (eglot-shutdown proc t))))))

(add-hook 'eglot--managed-mode-hook 'flymake-mode)
(add-hook 'eglot--managed-mode-hook 'eldoc-mode)

(defun eglot--buffer-managed-p (&optional proc)
  "Tell if current buffer can be managed by PROC."
  (and buffer-file-name (let ((cur (eglot--find-current-process)))
                          (or (and (null proc) cur)
                              (and proc (eq proc cur))))))

(defvar-local eglot--current-flymake-report-fn nil
  "Current flymake report function for this buffer")

(defun eglot--maybe-activate-editing-mode (&optional proc)
  "Maybe activate mode function `eglot--managed-mode'.
If PROC is supplied, do it only if BUFFER is managed by it.  In
that case, also signal textDocument/didOpen."
  ;; Called even when revert-buffer-in-progress-p
  (when (eglot--buffer-managed-p proc)
    (eglot--managed-mode 1)
    (eglot--signal-textDocument/didOpen)
    (flymake-start)
    (funcall (or eglot--current-flymake-report-fn #'ignore) nil)))

(add-hook 'find-file-hook 'eglot--maybe-activate-editing-mode)


;;; Mode-line, menu and other sugar
;;;
(defvar eglot--mode-line-format `(:eval (eglot--mode-line-format)))

(put 'eglot--mode-line-format 'risky-local-variable t)

(defun eglot--mode-line-call (what)
  "Make an interactive lambda for calling WHAT from mode-line."
  (lambda (event)
    (interactive "e")
    (with-selected-window (posn-window (event-start event))
      (call-interactively what))))

(defun eglot--mode-line-props (thing face defs &optional prepend)
  "Helper for function `eglot--mode-line-format'.
Uses THING, FACE, DEFS and PREPEND."
  (cl-loop with map = (make-sparse-keymap)
           for (elem . rest) on defs
           for (key def help) = elem
           do (define-key map `[mode-line ,key] (eglot--mode-line-call def))
           concat (format "%s: %s" key help) into blurb
           when rest concat "\n" into blurb
           finally (return `(:propertize ,thing
                                         face ,face
                                         keymap ,map help-echo ,(concat prepend blurb)
                                         mouse-face mode-line-highlight))))

(defun eglot--mode-line-format ()
  "Compose the EGLOT's mode-line."
  (pcase-let* ((proc (jrpc-current-process))
               (name (and (process-live-p proc) (jrpc-name proc)))
               (pending (and proc (length (jrpc-outstanding-request-ids proc))))
               (`(,_id ,doing ,done-p ,detail) (and proc (eglot--spinner proc)))
               (`(,status ,serious-p) (and proc (jrpc-status proc))))
    (append
     `(,(eglot--mode-line-props "eglot" 'eglot-mode-line nil))
     (when name
       `(":" ,(eglot--mode-line-props
               name 'eglot-mode-line
               '((mouse-1 eglot-events-buffer "go to events buffer")
                 (mouse-2 eglot-shutdown      "quit server")
                 (mouse-3 eglot-reconnect     "reconnect to server")))
         ,@(when serious-p
             `("/" ,(eglot--mode-line-props
                     "error" 'compilation-mode-line-fail
                     '((mouse-1 eglot-events-buffer "go to events buffer")
                       (mouse-3 eglot-clear-status  "clear this status"))
                     (format "An error occured: %s\n" status))))
         ,@(when (and doing (not done-p))
             `("/" ,(eglot--mode-line-props
                     (format "%s%s" doing
                             (if detail (format ":%s" detail) ""))
                     'compilation-mode-line-run
                     '((mouse-1 eglot-events-buffer "go to events buffer")))))
         ,@(when (cl-plusp pending)
             `("/" ,(eglot--mode-line-props
                     (format "%d" pending) 'warning
                     '((mouse-1 eglot-events-buffer "go to events buffer")
                       (mouse-3 eglot-clear-status  "clear this status"))
                     (format "%d pending requests\n" pending)))))))))

(add-to-list 'mode-line-misc-info
             `(eglot--managed-mode (" [" eglot--mode-line-format "] ")))


;;; Protocol implementation (Requests, notifications, etc)
;;;
(cl-defun eglot--server-window/showMessage (_process &key type message)
  "Handle notification window/showMessage"
  (eglot--message (propertize "Server reports (type=%s): %s"
                              'face (if (<= type 1) 'error))
                  type message))

(cl-defun eglot--server-window/showMessageRequest
    (process &key id type message actions)
  "Handle server request window/showMessageRequest"
  (let (reply)
    (unwind-protect
        (setq reply
              (completing-read
               (concat
                (format (propertize "[eglot] Server reports (type=%s): %s"
                                    'face (if (<= type 1) 'error))
                        type message)
                "\nChoose an option: ")
               (or (mapcar (lambda (obj) (plist-get obj :title)) actions)
                   '("OK"))
               nil t (plist-get (elt actions 0) :title)))
      (if reply
          (jrpc-reply process id :result (jrpc-obj :title reply))
        (jrpc-reply process id
                    :error (jrpc-obj :code -32800
                                     :message "User cancelled"))))))

(cl-defun eglot--server-window/logMessage (_proc &key _type _message)
  "Handle notification window/logMessage") ;; noop, use events buffer

(cl-defun eglot--server-telemetry/event (_proc &rest _any)
  "Handle notification telemetry/event") ;; noop, use events buffer

(defvar-local eglot--unreported-diagnostics nil
  "Unreported diagnostics for this buffer.")

(cl-defun eglot--server-textDocument/publishDiagnostics
    (_process &key uri diagnostics)
  "Handle notification publishDiagnostics"
  (if-let ((buffer (find-buffer-visiting (eglot--uri-to-path uri))))
      (with-current-buffer buffer
        (cl-loop
         for diag-spec across diagnostics
         collect (cl-destructuring-bind (&key range severity _group
                                              _code source message)
                     diag-spec
                   (pcase-let ((`(,beg . ,end) (eglot--range-region range)))
                     (flymake-make-diagnostic (current-buffer)
                                              beg end
                                              (cond ((<= severity 1) :error)
                                                    ((= severity 2)  :warning)
                                                    (t               :note))
                                              (concat source ": " message))))
         into diags
         finally (cond (eglot--current-flymake-report-fn
                        (funcall eglot--current-flymake-report-fn diags)
                        (setq eglot--unreported-diagnostics nil))
                       (t
                        (setq eglot--unreported-diagnostics diags)))))
    (eglot--warn "Diagnostics received for unvisited %s" uri)))

(cl-defun eglot--register-unregister (proc jsonrpc-id things how)
  "Helper for `eglot--server-client/registerCapability'.
THINGS are either registrations or unregisterations."
  (dolist (thing (cl-coerce things 'list))
    (cl-destructuring-bind (&key id method registerOptions) thing
      (let (retval)
        (unwind-protect
            (setq retval (apply (intern (format "eglot--%s-%s" how method))
                                proc :id id registerOptions))
          (unless (eq t (car retval))
            (cl-return-from eglot--register-unregister
              (jrpc-reply
               proc jsonrpc-id
               :error `(:code -32601 :message ,(or (cadr retval) "sorry")))))))))
  (jrpc-reply proc jsonrpc-id :result (jrpc-obj :message "OK")))

(cl-defun eglot--server-client/registerCapability
    (proc &key id registrations)
  "Handle server request client/registerCapability"
  (eglot--register-unregister proc id registrations 'register))

(cl-defun eglot--server-client/unregisterCapability
    (proc &key id unregisterations) ;; XXX: Yeah, typo and all.. See spec...
  "Handle server request client/unregisterCapability"
  (eglot--register-unregister proc id unregisterations 'unregister))

(cl-defun eglot--server-workspace/applyEdit
    (proc &key id _label edit)
  "Handle server request workspace/applyEdit"
  (condition-case err
      (progn
        (eglot--apply-workspace-edit edit 'confirm)
        (jrpc-reply proc id :result `(:applied )))
    (error
     (jrpc-reply proc id
                 :result `(:applied :json-false)
                 :error
                 (jrpc-obj :code -32001
                           :message (format "%s" err))))))

(defun eglot--TextDocumentIdentifier ()
  "Compute TextDocumentIdentifier object for current buffer."
  (jrpc-obj :uri (eglot--path-to-uri buffer-file-name)))

(defvar-local eglot--versioned-identifier 0)

(defun eglot--VersionedTextDocumentIdentifier ()
  "Compute VersionedTextDocumentIdentifier object for current buffer."
  (append (eglot--TextDocumentIdentifier)
          (jrpc-obj :version eglot--versioned-identifier)))

(defun eglot--TextDocumentItem ()
  "Compute TextDocumentItem object for current buffer."
  (append
   (eglot--VersionedTextDocumentIdentifier)
   (jrpc-obj :languageId
             (if (string-match "\\(.*\\)-mode" (symbol-name major-mode))
                 (match-string 1 (symbol-name major-mode))
               "unknown")
             :text
             (save-restriction
               (widen)
               (buffer-substring-no-properties (point-min) (point-max))))))

(defun eglot--TextDocumentPositionParams ()
  "Compute TextDocumentPositionParams."
  (jrpc-obj :textDocument (eglot--TextDocumentIdentifier)
            :position (eglot--pos-to-lsp-position)))

(defvar-local eglot--recent-changes nil
  "Recent buffer changes as collected by `eglot--before-change'.")

(defun eglot--outstanding-edits-p ()
  "Non-nil if there are outstanding edits."
  (cl-plusp (+ (length (car eglot--recent-changes))
               (length (cdr eglot--recent-changes)))))

(defun eglot--before-change (start end)
  "Hook onto `before-change-functions'.
Records START and END, crucially convert them into
LSP (line/char) positions before that information is
lost (because the after-change thingy doesn't know if newlines
were deleted/added)"
  (setf (car eglot--recent-changes)
        (vconcat (car eglot--recent-changes)
                 `[(,(eglot--pos-to-lsp-position start)
                    ,(eglot--pos-to-lsp-position end))])))

(defun eglot--after-change (start end pre-change-length)
  "Hook onto `after-change-functions'.
Records START, END and PRE-CHANGE-LENGTH locally."
  (cl-incf eglot--versioned-identifier)
  (setf (cdr eglot--recent-changes)
        (vconcat (cdr eglot--recent-changes)
                 `[(,pre-change-length
                    ,(buffer-substring-no-properties start end))])))

;; HACK! Launching a deferred sync request with outstanding changes is a
;; bad idea, since that might lead to the request never having a
;; chance to run, because `jrpc-ready-predicates'.
(advice-add #'jrpc-request :before
            (cl-function (lambda (_proc _method _params &key deferred)
              (when (and eglot--managed-mode deferred)
                (eglot--signal-textDocument/didChange)))))

(defun eglot--signal-textDocument/didChange ()
  "Send textDocument/didChange to server."
  (when (eglot--outstanding-edits-p)
    (let* ((proc (jrpc-current-process-or-lose))
           (sync-kind (eglot--server-capable :textDocumentSync))
           (emacs-messup (/= (length (car eglot--recent-changes))
                             (length (cdr eglot--recent-changes))))
           (full-sync-p (or (eq sync-kind 1) emacs-messup)))
      (when emacs-messup
        (eglot--warn "`eglot--recent-changes' messup: %s" eglot--recent-changes))
      (save-restriction
        (widen)
        (jrpc-notify
         proc :textDocument/didChange
         (jrpc-obj
          :textDocument
          (eglot--VersionedTextDocumentIdentifier)
          :contentChanges
          (if full-sync-p (vector
                           (jrpc-obj
                            :text (buffer-substring-no-properties (point-min)
                                                                  (point-max))))
            (cl-loop for (start-pos end-pos) across (car eglot--recent-changes)
                     for (len after-text) across (cdr eglot--recent-changes)
                     vconcat `[,(jrpc-obj :range (jrpc-obj :start start-pos
                                                           :end end-pos)
                                          :rangeLength len
                                          :text after-text)])))))
      (setq eglot--recent-changes (cons [] []))
      (setf (eglot--spinner proc) (list nil :textDocument/didChange t))
      ;; HACK!
      (jrpc--call-deferred proc))))

(defun eglot--signal-textDocument/didOpen ()
  "Send textDocument/didOpen to server."
  (setq eglot--recent-changes (cons [] []))
  (jrpc-notify
   (jrpc-current-process-or-lose)
   :textDocument/didOpen `(:textDocument ,(eglot--TextDocumentItem))))

(defun eglot--signal-textDocument/didClose ()
  "Send textDocument/didClose to server."
  (jrpc-notify
   (jrpc-current-process-or-lose)
   :textDocument/didClose `(:textDocument ,(eglot--TextDocumentIdentifier))))

(defun eglot--signal-textDocument/willSave ()
  "Send textDocument/willSave to server."
  (let ((proc (jrpc-current-process-or-lose))
        (params `(:reason 1 :textDocument ,(eglot--TextDocumentIdentifier))))
    (jrpc-notify proc :textDocument/willSave params)
    (ignore-errors
      (let ((jrpc-request-timeout 0.5))
        (when (plist-get :willSaveWaitUntil
                         (eglot--server-capable :textDocumentSync))
          (eglot--apply-text-edits
           (jrpc-request proc :textDocument/willSaveWaituntil params)))))))

(defun eglot--signal-textDocument/didSave ()
  "Send textDocument/didSave to server."
  (jrpc-notify
   (jrpc-current-process-or-lose)
   :textDocument/didSave
   (jrpc-obj
    ;; TODO: Handle TextDocumentSaveRegistrationOptions to control this.
    :text (buffer-substring-no-properties (point-min) (point-max))
    :textDocument (eglot--TextDocumentIdentifier))))

(defun eglot-flymake-backend (report-fn &rest _more)
  "An EGLOT Flymake backend.
Calls REPORT-FN maybe if server publishes diagnostics in time."
  (setq eglot--current-flymake-report-fn report-fn)
  ;; Report anything unreported
  (when eglot--unreported-diagnostics
    (funcall report-fn eglot--unreported-diagnostics)
    (setq eglot--unreported-diagnostics nil))
  ;; Signal a didChange that might eventually bring new diagnotics
  (eglot--signal-textDocument/didChange))

(defun eglot-xref-backend ()
  "EGLOT xref backend."
  (when (eglot--server-capable :definitionProvider) 'eglot))

(defvar eglot--xref-known-symbols nil)

(defun eglot--xref-reset-known-symbols (&rest _dummy)
  "Reset `eglot--xref-reset-known-symbols'.
DUMMY is ignored"
  (setq eglot--xref-known-symbols nil))

(advice-add 'xref-find-definitions :after #'eglot--xref-reset-known-symbols)
(advice-add 'xref-find-references :after #'eglot--xref-reset-known-symbols)

(defun eglot--xref-make (name uri position)
  "Like `xref-make' but with LSP's NAME, URI and POSITION."
  (cl-destructuring-bind (&key line character) position
    (xref-make name (xref-make-file-location
                     (eglot--uri-to-path uri)
                     ;; F!@(#*&#$)CKING OFF-BY-ONE again
                     (1+ line) character))))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql eglot)))
  (when (eglot--server-capable :documentSymbolProvider)
    (let ((proc (jrpc-current-process-or-lose))
          (text-id (eglot--TextDocumentIdentifier)))
      (completion-table-with-cache
       (lambda (string)
         (setq eglot--xref-known-symbols
               (mapcar
                (jrpc-lambda (&key name kind location containerName)
                  (propertize name
                              :textDocumentPositionParams
                              (jrpc-obj :textDocument text-id
                                        :position (plist-get
                                                   (plist-get location :range)
                                                   :start))
                              :locations (list location)
                              :kind kind
                              :containerName containerName))
                (jrpc-request proc
                              :textDocument/documentSymbol
                              (jrpc-obj
                               :textDocument text-id))))
         (all-completions string eglot--xref-known-symbols))))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql eglot)))
  (when-let ((symatpt (symbol-at-point)))
    (propertize (symbol-name symatpt)
                :textDocumentPositionParams
                (eglot--TextDocumentPositionParams))))

(cl-defmethod xref-backend-definitions ((_backend (eql eglot)) identifier)
  (let* ((rich-identifier
          (car (member identifier eglot--xref-known-symbols)))
         (location-or-locations
          (if rich-identifier
              (get-text-property 0 :locations rich-identifier)
            (jrpc-request (jrpc-current-process-or-lose)
                          :textDocument/definition
                          (get-text-property
                           0 :textDocumentPositionParams identifier)))))
    (mapcar (jrpc-lambda (&key uri range)
       (eglot--xref-make identifier uri (plist-get range :start)))
     location-or-locations)))

(cl-defmethod xref-backend-references ((_backend (eql eglot)) identifier)
  (unless (eglot--server-capable :referencesProvider)
    (cl-return-from xref-backend-references nil))
  (let ((params
         (or (get-text-property 0 :textDocumentPositionParams identifier)
             (let ((rich (car (member identifier eglot--xref-known-symbols))))
               (and rich (get-text-property 0 :textDocumentPositionParams rich))))))
    (unless params
      (eglot--error "Don' know where %s is in the workspace!" identifier))
    (mapcar
     (jrpc-lambda (&key uri range)
       (eglot--xref-make identifier uri (plist-get range :start)))
     (jrpc-request (jrpc-current-process-or-lose)
                   :textDocument/references
                   (append
                    params
                    (jrpc-obj :context
                              (jrpc-obj :includeDeclaration t)))))))

(cl-defmethod xref-backend-apropos ((_backend (eql eglot)) pattern)
  (when (eglot--server-capable :workspaceSymbolProvider)
    (mapcar
     (jrpc-lambda (&key name location &allow-other-keys)
       (cl-destructuring-bind (&key uri range) location
         (eglot--xref-make name uri (plist-get range :start))))
     (jrpc-request (jrpc-current-process-or-lose)
                   :workspace/symbol
                   (jrpc-obj :query pattern)))))

(defun eglot-completion-at-point ()
  "EGLOT's `completion-at-point' function."
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (proc (jrpc-current-process-or-lose)))
    (when (eglot--server-capable :completionProvider)
      (list
       (or (car bounds) (point))
       (or (cdr bounds) (point))
       (completion-table-with-cache
        (lambda (_ignored)
          (let* ((resp (jrpc-request proc
                                     :textDocument/completion
                                     (eglot--TextDocumentPositionParams)
                                     :deferred :textDocument/completion))
                 (items (if (vectorp resp) resp (plist-get resp :items))))
            (mapcar
             (jrpc-lambda (&rest all &key label &allow-other-keys)
               (add-text-properties 0 1 all label) label)
             items))))
       :annotation-function
       (lambda (obj)
         (propertize (concat " " (or (get-text-property 0 :detail obj)
                                     (cdr (assoc (get-text-property 0 :kind obj)
                                                 eglot--kind-names))))
                     'face 'font-lock-function-name-face))
       :display-sort-function
       (lambda (items)
         (sort items (lambda (a b)
                       (string-lessp
                        (or (get-text-property 0 :sortText a) "")
                        (or (get-text-property 0 :sortText b) "")))))
       :company-doc-buffer
       (lambda (obj)
         (let ((documentation
                (or (get-text-property 0 :documentation obj)
                    (plist-get (jrpc-request proc :completionItem/resolve
                                             (text-properties-at 0 obj))
                               :documentation))))
           (when documentation
             (with-current-buffer (get-buffer-create " *eglot doc*")
               (erase-buffer)
               (ignore-errors (funcall (intern "markdown-mode")))
               (font-lock-ensure)
               (insert documentation)
               (current-buffer)))))
       :exit-function
       (lambda (_string _status) (eglot-eldoc-function))))))

(defvar eglot--highlights nil "Overlays for textDocument/documentHighlight.")

(defun eglot--hover-info (contents &optional range)
  (concat (and range
               (pcase-let ((`(,beg . ,end) (eglot--range-region range)))
                 (concat (buffer-substring beg end)  ": ")))
          (mapconcat #'eglot--format-markup
                     (append
                      (cond ((vectorp contents)
                             contents)
                            (contents
                             (list contents)))) "\n")))

(defun eglot--sig-info (sigs active-sig active-param)
  (cl-loop
   for (sig . moresigs) on (append sigs nil) for i from 0
   concat (cl-destructuring-bind (&key label _documentation parameters) sig
            (let (active-doc)
              (concat
               (propertize (replace-regexp-in-string "(.*$" "(" label)
                           'face 'font-lock-function-name-face)
               (cl-loop
                for (param . moreparams) on (append parameters nil) for j from 0
                concat (cl-destructuring-bind (&key label documentation) param
                         (when (and (eql j active-param) (eql i active-sig))
                           (setq label (propertize
                                        label
                                        'face 'eldoc-highlight-function-argument))
                           (when documentation
                             (setq active-doc (concat label ": " documentation))))
                         label)
                if moreparams concat ", " else concat ")")
               (when active-doc (concat "\n" active-doc)))))
   when moresigs concat "\n"))

(defun eglot-help-at-point ()
  "Request \"hover\" information for the thing at point."
  (interactive)
  (cl-destructuring-bind (&key contents range)
      (jrpc-request (jrpc-current-process-or-lose) :textDocument/hover
                    (eglot--TextDocumentPositionParams))
    (when (seq-empty-p contents) (eglot--error "No hover info here"))
    (with-help-window "*eglot help*"
      (with-current-buffer standard-output
        (insert (eglot--hover-info contents range))))))

(defun eglot-eldoc-function ()
  "EGLOT's `eldoc-documentation-function' function.
If SKIP-SIGNATURE, don't try to send textDocument/signatureHelp."
  (let* ((buffer (current-buffer))
         (proc (jrpc-current-process-or-lose))
         (position-params (eglot--TextDocumentPositionParams))
         sig-showing)
    (cl-macrolet ((when-buffer-window
                   (&body body) `(when (get-buffer-window buffer)
                                   (with-current-buffer buffer ,@body))))
      (when (eglot--server-capable :signatureHelpProvider)
        (jrpc-async-request
         proc :textDocument/signatureHelp position-params
         :success-fn (jrpc-lambda (&key signatures activeSignature
                                        activeParameter)
                       (when-buffer-window
                        (when (cl-plusp (length signatures))
                          (setq sig-showing t)
                          (eldoc-message (eglot--sig-info signatures
                                                          activeSignature
                                                          activeParameter)))))
         :deferred :textDocument/signatureHelp))
      (when (eglot--server-capable :hoverProvider)
        (jrpc-async-request
         proc :textDocument/hover position-params
         :success-fn (jrpc-lambda (&key contents range)
                       (unless sig-showing
                         (when-buffer-window
                          (eldoc-message (eglot--hover-info contents range)))))
         :deferred :textDocument/hover))
      (when (eglot--server-capable :documentHighlightProvider)
        (jrpc-async-request
         proc :textDocument/documentHighlight position-params
         :success-fn (lambda (highlights)
                       (mapc #'delete-overlay eglot--highlights)
                       (setq eglot--highlights
                             (when-buffer-window
                              (mapcar
                               (jrpc-lambda (&key range _kind)
                                 (pcase-let ((`(,beg . ,end)
                                              (eglot--range-region range)))
                                   (let ((ov (make-overlay beg end)))
                                     (overlay-put ov 'face 'highlight)
                                     (overlay-put ov 'evaporate t)
                                     ov)))
                               highlights))))
         :deferred :textDocument/documentHighlight))))
  nil)

(defun eglot-imenu (oldfun)
  "EGLOT's `imenu-create-index-function' overriding OLDFUN."
  (if (eglot--server-capable :documentSymbolProvider)
      (let ((entries
             (mapcar
              (jrpc-lambda (&key name kind location _containerName)
                (cons (propertize name :kind (cdr (assoc kind eglot--kind-names)))
                      (eglot--lsp-position-to-point
                       (plist-get (plist-get location :range) :start))))
              (jrpc-request (jrpc-current-process-or-lose)
                            :textDocument/documentSymbol
                            (jrpc-obj
                             :textDocument (eglot--TextDocumentIdentifier))))))
        (append
         (seq-group-by (lambda (e) (get-text-property 0 :kind (car e)))
                       entries)
         entries))
    (funcall oldfun)))

(defun eglot--apply-text-edits (edits &optional version)
  "Apply EDITS for current buffer if at VERSION, or if it's nil."
  (unless (or (not version) (equal version eglot--versioned-identifier))
    (eglot--error "Edits on `%s' require version %d, you have %d"
                  (current-buffer) version eglot--versioned-identifier))
  (mapc (jrpc-lambda (&key range newText)
          (save-restriction
            (widen)
            (save-excursion
              (pcase-let ((`(,beg . ,end) (eglot--range-region range)))
                (goto-char beg) (delete-region beg end) (insert newText)))))
        edits)
  (eglot--message "%s: Performed %s edits" (current-buffer) (length edits)))

(defun eglot--apply-workspace-edit (wedit &optional confirm)
  "Apply the workspace edit WEDIT.  If CONFIRM, ask user first."
  (let (prepared)
    (cl-destructuring-bind (&key changes documentChanges)
        wedit
      (cl-loop
       for change on documentChanges
       do (push (cl-destructuring-bind (&key textDocument edits) change
                  (cl-destructuring-bind (&key uri version) textDocument
                    (list (eglot--uri-to-path uri) edits version)))
                prepared))
      (cl-loop for (uri edits) on changes by #'cddr
               do (push (list (eglot--uri-to-path uri) edits) prepared)))
    (if (or confirm
            (cl-notevery #'find-buffer-visiting
                         (mapcar #'car prepared)))
        (unless (y-or-n-p
                 (format "[eglot] Server requests to edit %s files.\n  %s\n\
Proceed? "
                         (length prepared)
                         (mapconcat #'identity
                                    (mapcar #'car prepared)
                                    "\n  ")))
          (eglot--error "User cancelled server edit")))
    (unwind-protect
        (let (edit)
          (while (setq edit (car prepared))
            (cl-destructuring-bind (path edits &optional version) edit
              (with-current-buffer (find-file-noselect path)
                (eglot--apply-text-edits edits version))
              (pop prepared))))
      (if prepared
          (eglot--warn "Caution: edits of files %s failed."
                       (mapcar #'car prepared))
        (eglot--message "Edit successful!")))))

(defun eglot-rename (newname)
  "Rename the current symbol to NEWNAME."
  (interactive
   (list (read-from-minibuffer (format "Rename `%s' to: " (symbol-at-point)))))
  (unless (eglot--server-capable :renameProvider)
    (eglot--error "Server can't rename!"))
  (eglot--apply-workspace-edit
   (jrpc-request (jrpc-current-process-or-lose)
                 :textDocument/rename `(,@(eglot--TextDocumentPositionParams)
                                        ,@(jrpc-obj :newName newname)))
   current-prefix-arg))


;;; Dynamic registration
;;;
(cl-defun eglot--register-workspace/didChangeWatchedFiles (proc &key id watchers)
  "Handle dynamic registration of workspace/didChangeWatchedFiles"
  (eglot--unregister-workspace/didChangeWatchedFiles proc :id id)
  (let* (success
         (globs (mapcar (lambda (w) (plist-get w :globPattern)) watchers)))
    (cl-labels
        ((handle-event
          (event)
          (cl-destructuring-bind (desc action file &optional file1) event
            (cond
             ((and (memq action '(created changed deleted))
                   (cl-find file globs
                            :test (lambda (f glob)
                                    (string-match (wildcard-to-regexp
                                                   (expand-file-name glob))
                                                  f))))
              (jrpc-notify
               proc :workspace/didChangeWatchedFiles
               `(:changes ,(vector `(:uri ,(eglot--path-to-uri file)
                                          :type ,(cl-case action
                                                   (created 1)
                                                   (changed 2)
                                                   (deleted 3)))))))
             ((eq action 'renamed)
              (handle-event desc 'deleted file)
              (handle-event desc 'created file1))))))
      (unwind-protect
          (progn (dolist (dir (delete-dups (mapcar #'file-name-directory globs)))
                   (push (file-notify-add-watch dir '(change) #'handle-event)
                         (gethash id (eglot--file-watches proc))))
                 (setq success `(t "OK")))
        (unless success
          (eglot--unregister-workspace/didChangeWatchedFiles proc :id id))))))

(cl-defun eglot--unregister-workspace/didChangeWatchedFiles (proc &key id)
  "Handle dynamic unregistration of workspace/didChangeWatchedFiles"
  (mapc #'file-notify-rm-watch (gethash id (eglot--file-watches proc)))
  (remhash id (eglot--file-watches proc))
  (list t "OK"))


;;; Rust-specific
;;;
(defun eglot--rls-probably-ready-for-p (what proc)
  "Guess if the RLS running in PROC is ready for WHAT."
  (or (eq what :textDocument/completion) ; RLS normally ready for this
                                        ; one, even if building ;
      (pcase-let ((`(,_id ,what ,done ,_detail) (eglot--spinner proc)))
        (and (equal "Indexing" what) done))))

;;;###autoload
(progn
  (add-hook 'rust-mode-hook 'eglot--setup-rls-idiosyncrasies)
  (defun eglot--setup-rls-idiosyncrasies ()
    "Prepare `eglot' to deal with RLS's special treatment."
    (add-hook 'jrpc-ready-predicates 'eglot--rls-probably-ready-for-p t t)))

(cl-defun eglot--server-window/progress
    (process &key id done title message &allow-other-keys)
  "Handle notification window/progress"
  (setf (eglot--spinner process) (list id title done message))
  (when (and (equal "Indexing" title) done)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eglot--buffer-managed-p process)
          (funcall (or eglot--current-flymake-report-fn #'ignore)
                   eglot--unreported-diagnostics))))))

(provide 'eglot)
;;; eglot.el ends here

;; Local Variables:
;; checkdoc-force-docstrings-flag: nil
;; End:
