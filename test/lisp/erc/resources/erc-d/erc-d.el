;;; erc-d.el --- A dumb test server for ERC -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

;; This is a netcat style server for testing ERC.  The "d" in the name
;; stands for "daemon" as well as for "dialog" (as well as for "dumb"
;; because this server isn't very smart).  It either spits out a
;; canned reply when an incoming request matches the expected regexp
;; or signals an error and dies.  The entry point function is
;; `erc-d-run'.
;;
;; Canned scripts, or "dialogs," should be Lisp-Data files containing
;; one or more request/reply forms like this:
;;
;; |  ((mode-chan 1.5 "MODE #chan")          ; request: tag, expr, regex
;; |   (0.1 ":irc.org 324 bob #chan +Cint")  ; reply: delay, content
;; |   (0.0 ":irc.org 329 bob #chan 12345")) ; reply: ...
;;
;; These are referred to as "exchanges."  The first element is a list
;; whose CAR is a descriptive "tag" and whose CDR is an incoming
;; "spec" representing an inbound message from the client.  The rest
;; of the exchange is composed of outgoing specs representing
;; server-to-client messages.  A tag can be any symbol (ideally unique
;; in the dialog), but a leading tilde means the request should be
;; allowed to arrive out of order (within the allotted time).
;;
;; The first element in an incoming spec is a number indicating the
;; maximum number of seconds to wait for a match before raising an
;; error.  The CDR is interpreted as the collective arguments of an
;; `rx' form to be matched against the raw request (stripped of its
;; CRLF line ending).  A "string-start" backslash assertion, "\\`", is
;; prepended to all patterns.
;;
;; Similarly, the leading number in an *outgoing* spec indicates how
;; many seconds to wait before sending the line, which is rendered by
;; concatenating the other members after evaluating each in place.
;; CRLF line endings are appended on the way out and should be absent.
;;
;; Recall that IRC is "asynchronous," meaning some flow intervals
;; don't jibe with lockstep request-reply semantics.  However, for our
;; purposes, grouping things as [input, output1, ..., outputN] makes
;; sense, even though input and output may be completely unrelated.
;;
;; Template interpolation:
;;
;; A rudimentary templating facility is provided for additional
;; flexibility.  However, it's best to keep things simple (even if
;; overly verbose), so others can easily tell what's going on at a
;; glance.  If necessary, consult existing tests for examples (grep
;; for the variables `erc-d-tmpl-vars' and `erc-d-match-handlers').
;;
;; Subprocess or in-process?:
;;
;; Running in-process confers better visibility and easier setup at
;; the cost of additional cleanup and resource wrangling.  With a
;; subprocess, cleanup happens by pulling the plug, but configuration
;; means loading a separate file or passing -eval "(forms...)" during
;; invocation.  In some cases, a subprocess may be the only option,
;; like when trying to avoid `require'ing this file.
;;
;; Dialog objects:
;;
;; For a given exchange, the first argument passed to a request
;; handler is the `erc-d-dialog' object representing the overall
;; conversation with the connecting peer.  It can be used to pass
;; information between handlers during a session.  Some important
;; items are:
;;
;; * name (symbol); name of the current dialog
;;
;; * queue (ring); a backlog of unhandled raw requests, minus CRLF
;; endings.
;;
;; * timers (list of timers); when run, these send messages originally
;; deferred as per the most recently matched exchange's delay info.
;; Normally, all outgoing messages must be sent before another request
;; is considered.  (See `erc-d--send-outgoing' for an escape hatch.)
;;
;; * hunks (iterator of iterators); unconsumed exchanges as read from
;; a Lisp-Data dialog file.  The exchange iterators being dispensed
;; themselves yield portions of member forms as a 2- or 3-part
;; sequence: [tag] spec.  (Here, "hunk" just means "list of raw,
;; unrendered exchange elements")
;;
;; * vars (alist of cons pairs); for sharing state among template
;; functions during the lifetime of an exchange.  Initially populated
;; by `erc-d-tmpl-vars', these KEY/VALUE pairs are expanded in the
;; templates and optionally updated by "exchange handlers" (see
;; `erc-d-match-handlers').  When VALUE is a function, occurrences of
;; KEY in an outgoing spec are replaced with the result of calling
;; VALUE with match data set appropriately.  See
;; `erc-d--render-entries' for details.
;;
;; * exchanges (ring of erc-d-exchange objects); activated hunks
;; allowed to match out of order, plus the current active exchange
;; being yielded from, if any. See `erc-d-exchange'.
;;
;; TODO
;;
;; - Remove un(der)used functionality and simplify API
;; - Maybe migrate d-u and d-i dependencies here

;;; Code:
(eval-and-compile
  (let* ((d (file-name-directory (or (macroexp-file-name) buffer-file-name)))
         (load-path (cons (directory-file-name d) load-path)))
    (require 'erc-d-i)
    (require 'erc-d-u)))

(require 'ring)

(defvar erc-d-server-name "erc-d-server"
  "Default name of a server process and basis for its buffer name.
Only relevant when starting a server with `erc-d-run'.")

(defvar erc-d-server-fqdn "irc.example.org"
  "Usually the same as the server's RPL_MYINFO \"announced name\".
Possibly used by overriding handlers, like the one for PING, and/or
dialog templates for the sender portion of a reply message.")

(defvar erc-d-line-ending "\r\n"
  "Protocol line delimiter for sending and receiving.")

(defvar erc-d-linger-secs nil
  "Seconds to wait before quitting for all dialogs.
For more granular control, use the provided LINGER `rx' variable (alone)
as the incoming template spec of a dialog's last exchange.")

(defvar erc-d-tmpl-vars nil
  "An alist of template bindings available to client dialogs.
Populate it when calling `erc-d-run', and the contents will be made
available to all client dialogs through the `erc-d-dialog' \"vars\"
field and (therefore) to all templates as variables when rendering.  For
example, a key/value pair like (network . \"oftc\") will cause instances
of the (unquoted) symbol `network' to be replaced with \"oftc\" in the
rendered template string.

This list provides default template bindings common to all dialogs.
Each new client-connection process makes a shallow copy on init, but the
usual precautions apply when mutating member items.  Within the span of
a dialog, updates not applicable to all exchanges should die with their
exchange.  See `erc-d--render-entries' for details.  In the unlikely
event that an exchange-specific handler is needed, see
`erc-d-match-handlers'.")

(defvar erc-d-match-handlers nil
  "A plist of exchange-tag symbols mapped to request-handler functions.
This is meant to address edge cases for which `erc-d-tmpl-vars' comes up
short.  These may include (1) needing access to the client process
itself and/or (2) adding or altering outgoing response templates before
rendering.  Note that (2) requires using `erc-d-exchange-rebind' instead
of manipulating exchange bindings directly.

The hook-like function `erc-d-on-match' calls any handler whose key is
`eq' to the tag of the currently matched exchange (passing the client
`erc-d-dialog' as the first argument and the current `erc-d-exchange'
object as the second).  The handler runs just prior to sending the first
response.")

(defvar erc-d-auto-pong t
  "Handle PING requests automatically.")

(defvar erc-d--in-process t
  "Whether the server is running in the same Emacs as ERT.")

(defvar erc-d--slow-mo nil
  "Adjustment for all incoming timeouts.
This is to allow for human interaction or a slow Emacs or CI runner.
The value is the number of seconds to extend all incoming spec timeouts
by on init.  If the value is a negative number, it's negated and
interpreted as a lower bound to raise all incoming timeouts to.  If the
value is a function, it should take an existing timeout in seconds and
return a replacement.")

(defconst erc-d--eof-sentinel "__EOF__")
(defconst erc-d--linger-sentinel "__LINGER__")
(defconst erc-d--drop-sentinel "__DROP__")

(defvar erc-d--clients nil
  "List containing all clients for this server session.")

;; Some :type names may just be made up (not actual CL types)

(cl-defstruct (erc-d-spec) ; see `erc-d--render-entries'
  (head nil :type symbol) ; or number?
  (entry nil :type list)
  (state 0 :type integer))

(cl-defstruct (erc-d-exchange)
  "Object representing a request/response unit from a canned dialog."
  (dialog nil :type erc-d-dialog) ; owning dialog
  (tag nil :type symbol) ;  a.k.a. tag, the caar
  (pattern nil :type string) ; regexp to match requests against
  (inspec nil :type list) ; original unrendered incoming spec
  (hunk nil :type erc-d-u-scan-e) ; active raw exchange hunk being yielded
  (spec nil :type erc-d-spec) ; active spec, see `erc-d--render-entries'
  (timeout nil :type number) ; time allotted for current request
  (timer nil :type timer) ; match timer fires when timeout expires
  (bindings nil :type list) ; `eval'-style env pairs (KEY . VAL) ...
  (rx-bindings nil :type list) ; rx-let bindings
  (deferred nil :type boolean) ; whether sender is paused
  ;; Post-match
  (match-data nil :type match-data) ; from the latest matched request
  (request nil :type string)) ; the original request sans CRLF

(cl-defstruct (erc-d-dialog)
  "Session state for managing a client conversation."
  (process nil :type process) ; client-connection process
  (name nil :type symbol) ; likely the interned stem of the file
  (queue nil :type ring) ; backlog of incoming lines to process
  (hunks nil :type erc-d-u-scan-d) ; nil when done; info on raw exchange hunks
  (timers nil :type list) ; unsent replies
  (vars nil :type list) ; template bindings for rendering
  (exchanges nil :type ring) ; ring of erc-d-exchange objects
  (state nil :type symbol) ; handler's last recorded control state
  (matched nil :type erc-d-exchange) ; currently matched exchange
  (message nil :type erc-d-i-message) ; `erc-d-i-message'
  (match-handlers nil :type list) ; copy of `erc-d-match-handlers'
  (server-fqdn nil :type string) ; copy of `erc-d-server-fqdn'
  (finalizer nil :type function) ; custom teardown, passed dialog and exchange
  ;; Post-match history is a plist whose keys are exchange tags
  ;; (symbols) and whose values are a cons of match-data and request
  ;; values from prior matches.
  (history nil :type list))

(defun erc-d--initialize-client (process)
  "Initialize state variables used by a client PROCESS."
  ;; Discard server-only/owned props
  (process-put process :dialog-dialogs nil)
  (let* ((server (process-get process :server))
         (reader (pop (process-get server :dialog-dialogs)))
         (name (pop reader))
         ;; Copy handlers so they can self-mutate per process
         (mat-h (copy-sequence (process-get process :dialog-match-handlers)))
         (fqdn (copy-sequence (process-get process :dialog-server-fqdn)))
         (vars (copy-sequence (process-get process :dialog-vars)))
         (ending (process-get process :dialog-ending))
         (dialog (make-erc-d-dialog :name name
                                    :process process
                                    :queue (make-ring 10)
                                    :exchanges (make-ring 10)
                                    :match-handlers mat-h
                                    :server-fqdn fqdn)))
    ;; Add items expected by convenience commands like `erc-d-exchange-reload'.
    (setf (alist-get 'EOF vars) `(: ,erc-d--eof-sentinel eot)
          (alist-get 'LINGER vars) `(: ,erc-d--linger-sentinel eot)
          (alist-get 'DROP vars) `(: ,erc-d--drop-sentinel eot)
          (erc-d-dialog-vars dialog) vars
          (erc-d-dialog-hunks dialog) reader)
    ;; Add reverse link, register client, launch
    (process-put process :dialog dialog)
    (process-put process :ending ending)
    (process-put process :ending-regexp (rx-to-string `(+ ,ending)))
    (push process erc-d--clients)
    (erc-d--command-refresh dialog nil)
    (erc-d--on-request process)))

(defun erc-d-load-replacement-dialog (dialog replacement &optional skip)
  "Find REPLACEMENT among backlog and swap out current DIALOG's iterator.
With int SKIP, advance past that many exchanges."
  (let* ((process (erc-d-dialog-process dialog))
         (server (process-get process :server))
         (reader (assoc-default replacement
                                (process-get server :dialog-dialogs)
                                #'eq)))
    (when skip (while (not (zerop skip))
                 (erc-d-u--read-dialog reader)
                 (cl-decf skip)))
    (dolist (timer (erc-d-dialog-timers dialog))
      (cancel-timer timer))
    (dolist (exchange (ring-elements (erc-d-dialog-exchanges dialog)))
      (cancel-timer (erc-d-exchange-timer exchange)))
    (setf (erc-d-dialog-hunks dialog) reader)
    (erc-d--command-refresh dialog nil)))

(defvar erc-d--m-debug (getenv "ERC_D_DEBUG"))

(defun erc-d--m (process format-string &rest args)
  "Output ARGS using FORMAT-STRING to PROCESS's buffer or elsewhere."
  (when erc-d--m-debug
    (setq format-string (concat (format-time-string "%s.%N: ") format-string)))
  (let ((insertp (and process erc-d--in-process))
        (buffer (and process (process-buffer (process-get process :server)))))
    (when (and insertp (buffer-live-p buffer))
      (princ (concat (apply #'format format-string args) "\n") buffer))
    (when (or erc-d--m-debug (not insertp))
      (apply #'message format-string args))))

(defun erc-d--log (process string &optional outbound)
  "Log STRING received from or OUTBOUND to PROCESS peer."
  (let ((id (or (process-get process :log-id)
                (let ((port (erc-d-u--get-remote-port process)))
                  (process-put process :log-id port) port)))
        (name (erc-d-dialog-name (process-get process :dialog))))
    (if outbound
        (erc-d--m process "-> %s:%s %s" name id string)
      (dolist (line (split-string string (process-get process :ending)))
        (erc-d--m process "<- %s:%s %s" name id line)))))

(defun erc-d--log-process-event (server process msg)
  (erc-d--m server "%s: %s" process (string-trim-right msg)))

(defun erc-d--send (process string)
  "Send STRING to PROCESS peer."
  (erc-d--log process string 'outbound)
  (process-send-string process (concat string (process-get process :ending))))

(define-inline erc-d--fuzzy-p (exchange)
  (inline-letevals (exchange)
    (inline-quote
     (let ((tag (symbol-name (erc-d-exchange-tag ,exchange))))
       (eq ?~ (aref tag 0))))))

(define-error 'erc-d-timeout "Timed out awaiting expected request")

(defun erc-d--finalize-dialog (dialog)
  "Delete client-connection and finalize DIALOG.
Return associated server."
  (let ((process (erc-d-dialog-process dialog)))
    (setq erc-d--clients (delq process erc-d--clients))
    (dolist (timer (erc-d-dialog-timers dialog))
      (cancel-timer timer))
    (dolist (exchange (ring-elements (erc-d-dialog-exchanges dialog)))
      (cancel-timer (erc-d-exchange-timer exchange)))
    (prog1 (process-get process :server)
      (delete-process process))))

(defun erc-d--teardown (&optional sig &rest msg)
  "Clean up processes and maybe send signal SIG using MSG."
  (unless erc-d--in-process
    (when sig
      (erc-d--m nil "%s %s" sig (apply #'format-message msg)))
    (kill-emacs (if msg 1 0)))
  (let (process servers)
    (while (setq process (pop erc-d--clients))
      (push (erc-d--finalize-dialog (process-get process :dialog)) servers))
    (dolist (server servers)
      (delete-process server)))
  (dolist (timer timer-list)
    (when (memq (timer--function timer)
                '(erc-d--send erc-d--command-handle-all))
      (erc-d--m nil "Stray timer found: %S" (timer--function timer))
      (cancel-timer timer)))
  (when sig
    (dolist (buf erc-d-u--canned-buffers)
      (kill-buffer buf))
    (setq erc-d-u--canned-buffers nil)
    (signal sig (list (apply #'format-message msg)))))

(defun erc-d--teardown-this-dialog-at-least (dialog)
  "Run `erc-d--teardown' after destroying DIALOG if it's the last one."
  (let ((server (process-get (erc-d-dialog-process dialog) :server))
        (us (erc-d-dialog-process dialog)))
    (erc-d--finalize-dialog dialog)
    (cl-assert (not (memq us erc-d--clients)))
    (unless (or (process-get server :dialog-dialogs)
                (catch 'other
                  (dolist (process erc-d--clients)
                    (when (eq (process-get process :server) server)
                      (throw 'other process)))))
      (push us erc-d--clients)
      (erc-d--teardown))))

(defun erc-d--expire (dialog exchange)
  "Raise timeout error for EXCHANGE.
This will start the teardown for DIALOG."
  (setf (erc-d-exchange-spec exchange) nil)
  (if-let* ((finalizer (erc-d-dialog-finalizer dialog)))
      (funcall finalizer dialog exchange)
    (erc-d--teardown 'erc-d-timeout "Timed out awaiting request: %s"
                     (list :name (erc-d-exchange-tag exchange)
                           :pattern (erc-d-exchange-pattern exchange)
                           :timeout (erc-d-exchange-timeout exchange)
                           :dialog (erc-d-dialog-name dialog)))))

;; Using `run-at-time' here allows test cases to examine replies as
;; they arrive instead of forcing tests to wait until an exchange
;; completes.  The `run-at-time' in `erc-d--command-meter-replies'
;; does the same.  When running as a subprocess, a normal while loop
;; with a `sleep-for' works fine (including with multiple dialogs).
;; FYI, this issue was still present in older versions that called
;; this directly from `erc-d--filter'.

(defun erc-d--on-request (process)
  "Handle one request for client-connection PROCESS."
  (when (process-live-p process)
    (let* ((dialog (process-get process :dialog))
           (queue (erc-d-dialog-queue dialog)))
      (unless (ring-empty-p queue)
        (let* ((parsed (ring-remove queue))
               (cmd (intern (erc-d-i-message.command parsed))))
          (setf (erc-d-dialog-message dialog) parsed)
          (erc-d-command dialog cmd)))
      (run-at-time nil nil #'erc-d--on-request process))))

(defun erc-d--drop-p (exchange)
  (memq 'DROP (erc-d-exchange-inspec exchange)))

(defun erc-d--linger-p (exchange)
  (memq 'LINGER (erc-d-exchange-inspec exchange)))

(defun erc-d--fake-eof (dialog)
  "Simulate receiving a fictitious \"EOF\" message from peer."
  (setf (erc-d-dialog-message dialog) ; use downcase for internal cmds
        (make-erc-d-i-message :command "eof" :unparsed erc-d--eof-sentinel))
  (run-at-time nil nil #'erc-d-command dialog 'eof))

(defun erc-d--forget-process (process)
  "Set sentinel and filter for PROCESS to `ignore'."
  (let ((server (process-get process :server)))
    (set-process-sentinel server #'ignore)
    (set-process-sentinel process #'ignore)
    (set-process-filter server #'ignore)
    (set-process-filter process #'ignore)))

(defun erc-d--process-sentinel (process event)
  "Set up or tear down client-connection PROCESS depending on EVENT."
  (erc-d--log-process-event process process event)
  (if (and (eq 'open (process-status process))
           (process-get process :dialog-dialogs))
      (erc-d--initialize-client process)
    (let* ((dialog (process-get process :dialog))
           (exes (and dialog (erc-d-dialog-exchanges dialog))))
      (if (and exes (not (ring-empty-p exes)))
          (cond ((string-prefix-p "connection broken" event)
                 (erc-d--fake-eof dialog))
                ;; Ignore disconnecting peer when pattern is DROP
                ((and (string-prefix-p "deleted" event)
                      (erc-d--drop-p (ring-ref exes -1))))
                (t (erc-d--forget-process process)
                   (erc-d--teardown)))
        (erc-d--forget-process process)
        (erc-d--teardown)))))

(defun erc-d--filter (process string)
  "Handle input received from peer.
PROCESS represents a client peer connection and STRING is a raw request
including line delimiters."
  (let ((queue (erc-d-dialog-queue (process-get process :dialog)))
        (delim (process-get process :ending-regexp)))
    (setq string (concat (process-get process :stashed-input) string))
    (while (and string (string-match delim string))
      (let ((line (substring string 0 (match-beginning 0))))
        (setq string (unless (= (match-end 0) (length string))
                       (substring string (match-end 0))))
        (erc-d--log process line nil)
        (ring-insert queue (erc-d-i--parse-message line nil))))
    (when string
      (setf (process-get process :stashed-input) string))))

;; Misc process properties:
;;
;; The server property `:dialog-dialogs' is an alist of (symbol
;; . erc-d-u-scan-d) conses, each of which pairs a dialog's name with
;; info on its read progress (described above in the Commentary).
;; This list is populated by `erc-d-run' at the start of each session.
;;
;; Client-connection processes keep a reference to their server via a
;; `:server' property, which can be used to share info with other
;; clients.  There is currently no built-in way to do the same with
;; clients of other servers.  Clients also keep references to their
;; dialogs and raw messages via `:dialog' and `:stashed-input'.
;;
;; The logger stores a unique, human-friendly process name in the
;; client-process property `:log-id'.

(defun erc-d--start (host service name &rest plist)
  "Serve canned replies on HOST at SERVICE.
Return the new server process immediately when `erc-d--in-process' is
non-nil.  Otherwise, serve forever.  PLIST becomes the plist of the
server process and is used to initialize the plists of connection
processes.  NAME is used for the process and the buffer."
  (let* ((buf (get-buffer-create (concat "*" name "*")))
         (proc (make-network-process :server t
                                     :buffer buf
                                     :noquery t
                                     :filter #'erc-d--filter
                                     :log #'erc-d--log-process-event
                                     :sentinel #'erc-d--process-sentinel
                                     :name name
                                     :family (if host 'ipv4 'local)
                                     :coding 'binary
                                     :service (or service t)
                                     :host host
                                     :plist plist)))
    (process-put proc :server proc)
    ;; We don't have a minor mode, so use an arbitrary variable to mark
    ;; buffers owned by us instead
    (with-current-buffer buf (setq erc-d-u--process-buffer t))
    (erc-d--m proc "Starting network process: %S %S"
              proc (erc-d-u--format-bind-address proc))
    (if erc-d--in-process
        proc
      (while (process-live-p proc)
        (accept-process-output nil 0.01)))))

(defun erc-d--wrap-func-val (dialog exchange key func)
  "Return a form invoking FUNC when evaluated.
Arrange for FUNC to be called with the args it expects based on
the description in `erc-d--render-entries'."
  (let (args)
    ;; Ignore &rest or &optional
    (pcase-let ((`(,n . ,_) (func-arity func)))
      (pcase n
        (0)
        (1 (push (apply-partially #'erc-d-exchange-multi dialog exchange key)
                 args))
        (2 (push exchange args)
           (push (apply-partially #'erc-d-exchange-multi dialog exchange key)
                 args))
        (_ (error "Incompatible function: %s" func))))
    (lambda () (apply func args))))

(defun erc-d-exchange-reload (dialog exchange)
  "Rebuild all bindings for EXCHANGE from those in DIALOG."
  (cl-loop for (key . val) in (erc-d-dialog-vars dialog)
           unless (keywordp key)
           do (push (erc-d-u--massage-rx-args key val)
                    (erc-d-exchange-rx-bindings exchange))
           when (functionp val) do
           (setq val (erc-d--wrap-func-val dialog exchange key val))
           do (push (cons key val) (erc-d-exchange-bindings exchange))))

(defun erc-d-exchange-rebind (dialog exchange key val &optional export)
  "Modify a binding between renders.

Bind symbol KEY to VAL, replacing whatever existed before, which may
have been a function.  A third, optional argument, if present and
non-nil, results in the DIALOG's bindings for all EXCHANGEs adopting
this binding.  VAL can either be a function of the type described in
`erc-d--render-entries' or any value acceptable as an argument to the
function `concat'.

DIALOG and EXCHANGE are the current `erc-d-dialog' and `erc-d-exchange'
objects for the request context."
  (when export
    (setf (alist-get key (erc-d-dialog-vars dialog)) val))
  (if (functionp val)
      (setf (alist-get key (erc-d-exchange-bindings exchange))
            (erc-d--wrap-func-val dialog exchange key val))
    (setf (alist-get key (erc-d-exchange-rx-bindings exchange)) (list val)
          (alist-get key (erc-d-exchange-bindings exchange)) val))
  val)

(defun erc-d-exchange-match (exchange match-number &optional tag)
  "Return match portion of current or previous request.
MATCH-NUMBER is the match group number.  TAG, if provided, means the
exchange tag (name) from some previously matched request."
  (if tag
      (pcase-let* ((dialog (erc-d-exchange-dialog exchange))
                   (`(,m-d . ,req) (plist-get (erc-d-dialog-history dialog)
                                              tag)))
        (set-match-data m-d)
        (match-string match-number req))
    (match-string match-number (erc-d-exchange-request exchange))))

(defun erc-d-exchange-multi (dialog exchange key cmd &rest args)
  "Call CMD with ARGS.
This is a utility passed as the first argument to all template
functions.  DIALOG and EXCHANGE are pre-applied.  A few pseudo
commands, like `:request', are provided for convenience so that
the caller's definition doesn't have to include this file.  The
rest are access and mutation utilities, such as `:set', which
assigns KEY a new value, `:get-binding', which looks up KEY in
`erc-d-exchange-bindings', and `:get-var', which looks up KEY in
`erc-d-dialog-vars'."
  (pcase cmd
    (:set (apply #'erc-d-exchange-rebind dialog exchange key args))
    (:reload (apply #'erc-d-exchange-reload dialog exchange args))
    (:rebind (apply #'erc-d-exchange-rebind dialog exchange args))
    (:match (apply #'erc-d-exchange-match exchange args))
    (:request (erc-d-exchange-request exchange))
    (:match-data (erc-d-exchange-match-data exchange))
    (:dialog-name (erc-d-dialog-name dialog))
    (:get-binding (cdr (assq (car args) (erc-d-exchange-bindings exchange))))
    (:get-var (alist-get (car args) (erc-d-dialog-vars dialog)))))

(defun erc-d--render-incoming-entry (exchange spec)
  (let ((rx--local-definitions (rx--extend-local-defs
                                (erc-d-exchange-rx-bindings exchange))))
    (rx-to-string `(: bos ,@(erc-d-spec-entry spec)) 'no-group)))

(defun erc-d--render-outgoing-entry (exchange entry)
  (let (out this)
    (while (setq this (pop entry))
      (set-match-data (erc-d-exchange-match-data exchange))
      (unless (stringp this)
        (cl-assert (symbolp this))
        (setq this (or (alist-get this (erc-d-exchange-bindings exchange))
                       (symbol-value this)))
        ;; Allow reference to overlong var name unbecoming of a template
        (when this
          (when (symbolp this) (setq this (symbol-value this)))
          (when (functionp this) (setq this (save-match-data (funcall this))))
          (unless (stringp this) (error "Unexpected token %S" this))))
      (push this out))
    (apply #'concat (nreverse out))))

(defun erc-d--render-entries (exchange &optional yield-result)
  "Act as an iterator producing rendered strings from EXCHANGE hunks.
When an entry's CAR is an arbitrary symbol, yield that back first, and
consider the entry an \"incoming\" entry.  Then, regardless of the
entry's type (incoming or outgoing), yield back the next element, which
should be a number representing either a timeout (incoming) or a
delay (outgoing).  After that, yield a rendered template (outgoing) or a
regular expression (incoming); both should be treated as immutable.

When evaluating a template, bind the keys in the alist stored in the
dialog's `vars' field to its values, but skip any self-quoters, like
:foo.  When an entry is incoming, replace occurrences of a key with its
value, which can be any valid `rx' form (see Info node `(elisp)
Extending Rx').  Do the same when an entry is outgoing, but expect a
value's form to be (anything that evaluates to) something acceptable by
`concat' or, alternatively, a function that returns a string or nil.

Repeat the last two steps for the remaining entries, all of which are
assumed to be outgoing.  That is, continue yielding a timeout/delay and
a rendered string for each entry, and yield nil when exhausted.

Once again, for an incoming entry, the yielded string is a regexp to be
matched against the raw request.  For outgoing, it's the final response,
ready to be sent out (after adding the appropriate line ending).

To help with testing, bindings are not automatically created from
DIALOG's \"vars\" alist when this function is invoked.  But this can be
forced by sending a non-nil YIELD-RESULT into the generator on the
second \"next\" invocation of a given iteration.  This clobbers any
temporary bindings that don't exist in the DIALOG's `vars' alist, such
as those added via `erc-d-exchange-rebind' (unless \"exported\").

As noted earlier, template symbols can be bound to functions.  When
called during rendering, the match data from the current (matched)
request is accessible by calling the function `match-data'.

A function may ask for up to two required args, which are provided as
needed.  When applicable, the first required arg is a `funcall'-able
helper that accepts various keyword-based commands, like :rebind, and a
variable number of args.  See `erc-d-exchange-multi' for details.  When
specified, the second required arg is the current `erc-d-exchange'
object, which has among its members its owning `erc-d-dialog' object.
This should suffice as a safety valve for any corner-case needs.
Non-required args are ignored."
  (let ((spec (erc-d-exchange-spec exchange))
        (dialog (erc-d-exchange-dialog exchange))
        (entries (erc-d-exchange-hunk exchange)))
    (unless (erc-d-spec-entry spec)
      (setf (erc-d-spec-entry spec) (erc-d-u--read-exchange entries)))
    (catch 'yield
      (while (erc-d-spec-entry spec)
        (pcase (erc-d-spec-state spec)
          (0 (cl-incf (erc-d-spec-state spec))
             (throw 'yield (setf (erc-d-spec-head spec)
                                 (pop (erc-d-spec-entry spec)))))
          (1 (cl-incf (erc-d-spec-state spec))
             (when yield-result
               (erc-d-exchange-reload dialog exchange))
             (unless (numberp (erc-d-spec-head spec))
               (setf (erc-d-exchange-inspec exchange) (erc-d-spec-entry spec))
               (throw 'yield
                      (prog1 (pop (erc-d-spec-entry spec))
                        (setf (erc-d-spec-entry spec)
                              (erc-d--render-incoming-entry exchange spec))))))
          (2 (setf (erc-d-spec-state spec) 0)
             (throw 'yield
                    (let ((entry (erc-d-spec-entry spec)))
                      (setf (erc-d-spec-entry spec) nil)
                      (if (stringp entry)
                          entry
                        (erc-d--render-outgoing-entry exchange entry))))))))))

(defun erc-d--iter (exchange)
  (apply-partially #'erc-d--render-entries exchange))

(defun erc-d-on-match (dialog exchange)
  "Handle matched exchange request.
Allow the first handler in `erc-d-match-handlers' whose key matches TAG
to manipulate replies before they're sent to the DIALOG peer."
  (when-let* ((tag (erc-d-exchange-tag exchange))
              (handler (plist-get (erc-d-dialog-match-handlers dialog) tag)))
    (let ((md (erc-d-exchange-match-data exchange)))
      (set-match-data md)
      (funcall handler dialog exchange))))

(defun erc-d--send-outgoing (dialog exchange)
  "Send outgoing lines for EXCHANGE to DIALOG peer.
Assume the next spec is outgoing.  If its delay value is zero, render
the template and send the resulting message straight away.  Do the same
when DELAY is negative, only arrange for its message to be sent (abs
DELAY) seconds later, and then keep on processing.  If DELAY is
positive, pause processing and yield DELAY."
  (let ((specs (erc-d--iter exchange))
        (process (erc-d-dialog-process dialog))
        (deferred (erc-d-exchange-deferred exchange))
        delay)
    ;; Could stash/pass thunk instead to ensure specs can't be mutated
    ;; between calls (by temporarily replacing dialog member with a fugazi)
    (when deferred
      (erc-d--send process (funcall specs))
      (setf deferred nil (erc-d-exchange-deferred exchange) deferred))
    (while (and (not deferred) (setq delay (funcall specs)))
      (cond ((zerop delay) (erc-d--send process (funcall specs)))
            ((< delay 0) (push (run-at-time (- delay) nil #'erc-d--send
                                            process (funcall specs))
                               (erc-d-dialog-timers dialog)))
            ((setf deferred t (erc-d-exchange-deferred exchange) deferred))))
    delay))

(defun erc-d--add-dialog-linger (dialog exchange)
  "Add finalizer for EXCHANGE in DIALOG."
  (erc-d--m (erc-d-dialog-process dialog)
            "Lingering for %.2f seconds" (erc-d-exchange-timeout exchange))
  (let ((start (current-time)))
    (setf (erc-d-dialog-finalizer dialog)
          (lambda (&rest _)
            (erc-d--m (erc-d-dialog-process dialog)
                      "Lingered for %.2f seconds"
                      (float-time (time-subtract (current-time) start)))
            (erc-d--teardown-this-dialog-at-least dialog)))))

(defun erc-d--add-dialog-drop (dialog exchange)
  "Add finalizer for EXCHANGE in DIALOG."
  (erc-d--m (erc-d-dialog-process dialog)
            "Dropping in %.2f seconds" (erc-d-exchange-timeout exchange))
  (setf (erc-d-dialog-finalizer dialog)
        (lambda (&rest _)
          (erc-d--m (erc-d-dialog-process dialog)
                    "Dropping %S" (erc-d-dialog-name dialog))
          (erc-d--finalize-dialog dialog))))

(defun erc-d--create-exchange (dialog hunk)
  "Initialize next exchange HUNK for DIALOG."
  (let* ((spec (make-erc-d-spec))
         (exchange (make-erc-d-exchange :dialog dialog :hunk hunk :spec spec))
         (specs (erc-d--iter exchange)))
    (setf (erc-d-exchange-tag exchange) (funcall specs)
          (erc-d-exchange-timeout exchange) (funcall specs t)
          (erc-d-exchange-pattern exchange) (funcall specs))
    (cond ((erc-d--linger-p exchange)
           (erc-d--add-dialog-linger dialog exchange))
          ((erc-d--drop-p exchange)
           (erc-d--add-dialog-drop dialog exchange)))
    (setf (erc-d-exchange-timer exchange)
          (run-at-time (erc-d-exchange-timeout exchange)
                       nil #'erc-d--expire dialog exchange))
    exchange))

(defun erc-d--command-consider-prep-fail (dialog line exes)
  (list 'error "Match failed: %S %S" line
        (list :exes (mapcar #'erc-d-exchange-pattern
                            (ring-elements exes))
              :dialog (erc-d-dialog-name dialog))))

(defun erc-d--command-consider-prep-success (dialog line exes matched)
  (setf (erc-d-exchange-request matched) line
        (erc-d-exchange-match-data matched) (match-data)
        ;; Also add current to match history, indexed by exchange tag
        (plist-get (erc-d-dialog-history dialog)
                   (erc-d-exchange-tag matched))
        (cons (match-data) line)) ; do we need to make a copy of this?
  (cancel-timer (erc-d-exchange-timer matched))
  (ring-remove exes (ring-member exes matched)))

(cl-defun erc-d--command-consider (dialog)
  "Maybe return next matched exchange for DIALOG.
Upon encountering a mismatch, return an error of the form (ERROR-SYMBOL
DATA).  But when only fuzzies remain in the exchange pool, return nil."
  (let* ((parsed (erc-d-dialog-message dialog))
         (line (erc-d-i-message.unparsed parsed))
         (exes (erc-d-dialog-exchanges dialog))
         ;;
         matched)
    (let ((elts (ring-elements exes)))
      (while (and (setq matched (pop elts))
                  (not (string-match (erc-d-exchange-pattern matched) line)))
        (if (and (not elts) (erc-d--fuzzy-p matched))
            ;; Nothing to do, so advance
            (cl-return-from erc-d--command-consider nil)
          (cl-assert (or (not elts) (erc-d--fuzzy-p matched))))))
    (if matched
        (erc-d--command-consider-prep-success dialog line exes matched)
      (erc-d--command-consider-prep-fail dialog line exes))))

(defun erc-d--active-ex-p (ring)
  "Return non-nil when RING has a non-fuzzy exchange.
That is, return nil when RING is empty or when it only has exchanges
with leading-tilde tags."
  (let ((i 0)
        (len (ring-length ring))
        ex found)
    (while (and (not found) (< i len))
      (unless (erc-d--fuzzy-p (setq ex (ring-ref ring i)))
        (setq found ex))
      (cl-incf i))
    found))

(defun erc-d--finalize-done (dialog)
  ;; Linger logic for individual dialogs is handled elsewhere
  (if-let* ((finalizer (erc-d-dialog-finalizer dialog)))
      (funcall finalizer dialog)
    (let ((d (process-get (erc-d-dialog-process dialog) :dialog-linger-secs)))
      (push (run-at-time d nil #'erc-d--teardown)
            (erc-d-dialog-timers dialog)))))

(defun erc-d--advance-or-die (dialog)
  "Govern the lifetime of DIALOG.
Replenish exchanges from reader and insert them into the pool of
expected matches, as produced.  Return a symbol indicating session
status: deferring, matching, depleted, or done."
  (let ((exes (erc-d-dialog-exchanges dialog))
        hunk)
    (cond ((erc-d--active-ex-p exes) 'deferring)
          ((setq hunk (erc-d-u--read-dialog (erc-d-dialog-hunks dialog)))
           (let ((exchange (erc-d--create-exchange dialog hunk)))
             (if (erc-d--fuzzy-p exchange)
                 (ring-insert exes exchange)
               (ring-insert-at-beginning exes exchange)))
           'matching)
          ((not (ring-empty-p exes)) 'depleted)
          (t 'done))))

(defun erc-d--command-meter-replies (dialog exchange &optional cmd)
  "Ignore requests until all replies have been sent.
Do this for some previously matched EXCHANGE in DIALOG based on CMD, a
symbol.  As a side effect, maybe schedule the resumption of the main
loop after some delay."
  (let (delay)
    (if (or (not cmd) (eq 'resume cmd))
        (when (setq delay (erc-d--send-outgoing dialog exchange))
          (push (run-at-time delay nil #'erc-d--command-handle-all
                             dialog 'resume)
                (erc-d-dialog-timers dialog))
          (erc-d-dialog-state dialog))
      (setf (erc-d-dialog-state dialog) 'sending))))

(defun erc-d--die-unexpected (dialog)
  (erc-d--teardown 'error "Received unexpected input: %S"
                   (erc-d-i-message.unparsed (erc-d-dialog-message dialog))))

(defun erc-d--command-refresh (dialog matched)
  (let ((state (erc-d--advance-or-die dialog)))
    (when (eq state 'done)
      (erc-d--finalize-done dialog))
    (unless matched
      (when (eq state 'depleted)
        (erc-d--die-unexpected dialog))
      (cl-assert (memq state '(matching depleted)) t))
    (setf (erc-d-dialog-state dialog) state)))

(defun erc-d--command-handle-all (dialog cmd)
  "Create handler to act as control agent and process DIALOG requests.
Have it ingest internal control commands (lowercase symbols) and yield
back others indicating the lifecycle stage of the current dialog."
  (let ((matched (erc-d-dialog-matched dialog)))
    (cond
     (matched
      (or (erc-d--command-meter-replies dialog matched cmd)
          (setf (erc-d-dialog-matched dialog) nil)
          (erc-d--command-refresh dialog t)))
     ((pcase cmd ; FIXME remove command facility or make extensible
        ('resume nil)
        ('eof (erc-d--m (erc-d-dialog-process dialog) "Received an EOF") nil)))
     (t ; matching
      (setq matched nil)
      (catch 'yield
        (while (not matched)
          (when (ring-empty-p (erc-d-dialog-exchanges dialog))
            (erc-d--die-unexpected dialog))
          (when (setq matched (erc-d--command-consider dialog))
            (if (eq (car-safe matched) 'error)
                (apply #'erc-d--teardown matched)
              (erc-d-on-match dialog matched)
              (setf (erc-d-dialog-matched dialog) matched)
              (if-let* ((s (erc-d--command-meter-replies dialog matched nil)))
                  (throw 'yield s)
                (setf (erc-d-dialog-matched dialog) nil))))
          (erc-d--command-refresh dialog matched)))))))

;;;; Handlers for IRC commands

(cl-defgeneric erc-d-command (dialog cmd)
  "Handle new CMD from client for DIALOG.
By default, defer to this dialog's `erc-d--command-handle-all' instance,
which is stored in its `handler' field.")

(cl-defmethod erc-d-command ((dialog erc-d-dialog) cmd)
  (when (eq 'sending (erc-d--command-handle-all dialog cmd))
    (ring-insert-at-beginning (erc-d-dialog-queue dialog)
                              (erc-d-dialog-message dialog))))

;; A similar PONG handler would be useless because we know when to
;; expect them

(cl-defmethod erc-d-command ((dialog erc-d-dialog) (_cmd (eql PING))
                             &context (erc-d-auto-pong (eql t)))
  "Respond to PING request from DIALOG peer when ERC-D-AUTO-PONG is t."
  (let* ((parsed (erc-d-dialog-message dialog))
         (process (erc-d-dialog-process dialog))
         (nonce (car (erc-d-i-message.command-args parsed)))
         (fqdn (erc-d-dialog-server-fqdn dialog)))
    (erc-d--send process (format ":%s PONG %s :%s" fqdn fqdn nonce))))


;;;; Entry points

(defun erc-d-run (host service &optional server-name &rest dialogs)
  "Start serving DIALOGS on HOST at SERVICE.
Pass HOST and SERVICE directly to `make-network-process'.  When present,
use string SERVER-NAME for the server-process name as well as that of
its buffer (w. surrounding asterisks).  When absent, do the same with
`erc-d-server-name'.  When running \"in process,\" return the server
process; otherwise sleep until it dies.

A dialog must be a symbol matching the base name of a dialog file in
`erc-d-u-canned-dialog-dir'.  Global variables `erc-d-server-fqdn',
`erc-d-linger-secs', and `erc-d-tmpl-vars' determine the process's
`erc-d-dialog' fields `:server-fqdn', `:linger-secs', and `:vars',
respectively.  The latter may also be populated via keyword pairs
appearing among DIALOGS."
  (when (and server-name (symbolp server-name))
    (push server-name dialogs)
    (setq server-name nil))
  (let (loaded kwds defaults args)
    (while dialogs
      (if-let* ((dlog (pop dialogs))
                ((keywordp dlog)))
          (progn (push (pop dialogs) kwds) (push dlog kwds))
        (let ((reader (erc-d-u--canned-load-dialog dlog)))
          (when erc-d--slow-mo
            (setq reader (erc-d-u--rewrite-for-slow-mo erc-d--slow-mo reader)))
          (push (cons (erc-d-u--normalize-canned-name dlog) reader) loaded))))
    (setq kwds (erc-d-u--unkeyword kwds)
          defaults `((ending . ,erc-d-line-ending)
                     (server-fqdn . ,erc-d-server-fqdn)
                     (linger-secs . ,erc-d-linger-secs)
                     (vars . ,(or (plist-get kwds 'tmpl-vars) erc-d-tmpl-vars))
                     (dialogs . ,(nreverse loaded)))
          args (list :dialog-match-handlers
                     (erc-d-u--unkeyword (or (plist-get kwds 'match-handlers)
                                             erc-d-match-handlers))))
    (pcase-dolist (`(,var . ,def) defaults)
      (push (or (plist-get kwds var) def) args)
      (push (intern (format ":dialog-%s" var)) args))
    (apply #'erc-d--start host service (or server-name erc-d-server-name)
           args)))

(defun erc-d-serve ()
  "Start serving canned dialogs from the command line.
Although not autoloaded, this function is meant to be summoned via the
Emacs -f flag while starting a batch session.  It prints incoming and
outgoing messages to standard out.

The main options are --host HOST and --port PORT, which default to
localhost and auto, respectively.  The args are the dialogs to run.
Unlike with `erc-d-run', dialogs here *must* be files, meaning Lisp-Data
files adhering to the required format.  (These consist of \"specs\"
detailing timing and template info; see commentary for specifics.)

An optional --add-time N option can also be passed to hike up timeouts
by some number of seconds N.  For example, you might run:

  $ emacs -Q -batch -L . \\
  >   -l erc-d.el \\
  >   -f erc-d-serve \\
  >   --host 192.168.124.1 \\
  >   --port 16667 \\
  >   --add-time 10 \\
  >   ./my-dialog.eld

from a Makefile or manually with \\<global-map>\\[compile]. And then in
another terminal, do:

  $ nc -C 192.168.124.1 16667 ; or telnet if your nc doesn't have -C
  > PASS changeme
  ...

Use `erc-d-run' instead to start the server from within Emacs."
  (unless noninteractive
    (error "Command-line func erc-d-serve not run in -batch session"))
  (setq erc-d--in-process nil)
  (let (port host dialogs erc-d--slow-mo)
    (while command-line-args-left
      (pcase (pop command-line-args-left)
        ("--add-time" (setq erc-d--slow-mo
                            (string-to-number (pop command-line-args-left))))
        ("--linger" (setq erc-d-linger-secs
                          (string-to-number (pop command-line-args-left))))
        ("--host" (setq host (pop command-line-args-left)))
        ("--port" (setq port (string-to-number (pop command-line-args-left))))
        (dialog (push dialog dialogs))))
    (setq dialogs (mapcar #'erc-d-u--massage-canned-name dialogs))
    (when erc-d--slow-mo
      (message "Slow mo is ON"))
    (apply #'erc-d-run (or host "localhost") port nil (nreverse dialogs))))

(provide 'erc-d)

;;; erc-d.el ends here
