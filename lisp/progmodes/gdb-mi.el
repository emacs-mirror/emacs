;;; gdb-mi.el --- User Interface for running GDB  -*- lexical-binding: t -*-

;; Copyright (C) 2007-2022 Free Software Foundation, Inc.

;; Author: Nick Roberts <nickrob@gnu.org>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: unix, tools
;; URL: https://www.emacswiki.org/emacs/GDB-MI

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

;;; Credits:

;; This file was written by Nick Roberts following the general design
;; used in gdb-ui.el for Emacs 22.1 - 23.1.  It was further developed
;; by Dmitry Dzhus <dima@sphinx.net.ru> as part of the Google Summer
;; of Code 2009 Project "Emacs GDB/MI migration".

;;; Commentary:

;; This mode acts as a graphical user interface to GDB.  You can interact with
;; GDB through the GUD buffer in the usual way, but there are also further
;; buffers which control the execution and describe the state of your program.
;; It separates the input/output of your program from that of GDB and displays
;; expressions and their current values in their own buffers.  It also uses
;; features such as the fringe/display margin for breakpoints, and
;; the toolbar (see the GDB Graphical Interface section in the Emacs info
;; manual).

;; M-x gdb will start the debugger.

;; This file uses GDB/MI as the primary interface to GDB.  It runs gdb with
;; GDB/MI (-interp=mi) and access CLI using "-interpreter-exec console
;; cli-command".  This code replaces gdb-ui.el and uses MI tokens instead
;; of queues.  Eventually MI should be asynchronous.

;; Windows Platforms:

;; If you are using Emacs and GDB on Windows you will need to flush the buffer
;; explicitly in your program if you want timely display of I/O in Emacs.
;; Alternatively you can make the output stream unbuffered, for example, by
;; using a macro:

;;           #ifdef UNBUFFERED
;;	     setvbuf (stdout, (char *) NULL, _IONBF, 0);
;;	     #endif

;; and compiling with -DUNBUFFERED while debugging.

;; If you are using Cygwin GDB and find that the source is not being displayed
;; in Emacs when you step through it, possible solutions are to:

;;   1) Use Cygwin X Windows and Cygwin Emacs.
;;        (Since 22.1 Emacs builds under Cygwin.)
;;   2) Use MinGW GDB instead.
;;   3) Use cygwin-mount.el

;;; macOS:

;; GDB in Emacs on macOS works best with FSF GDB as Apple have made
;; some changes to the version that they include as part of macOS.
;; This requires GDB version 7.0 or later as earlier versions do not
;; compile on macOS.

;;; Known Bugs:

;; 1) Stack buffer doesn't parse MI output if you stop in a routine without
;;    line information, e.g., a routine in libc (just a TODO item).

;; TODO:
;; 2) Watch windows to work with threads.
;; 3) Use treebuffer.el instead of the speedbar for watch-expressions?
;; 4) Mark breakpoint locations on scroll-bar of source buffer?

;;; Code:

(require 'gud)
(require 'cl-lib)
(require 'cl-seq)
(eval-when-compile (require 'pcase))

(declare-function speedbar-change-initial-expansion-list
                  "speedbar" (new-default))
(declare-function speedbar-timer-fn "speedbar" ())
(declare-function speedbar-line-text "speedbar" (&optional p))
(declare-function speedbar-change-expand-button-char "speedbar" (char))
(declare-function speedbar-delete-subblock "speedbar" (indent))
(declare-function speedbar-center-buffer-smartly "speedbar" ())

;; FIXME: The declares below are necessary because we don't call `gud-def'
;; at toplevel, so the compiler doesn't know under which circumstances
;; they're defined.
(declare-function gud-until  "gud" (arg))
(declare-function gud-print  "gud" (arg))
(declare-function gud-down   "gud" (arg))
(declare-function gud-up     "gud" (arg))
(declare-function gud-jump   "gud" (arg))
(declare-function gud-finish "gud" (arg))
(declare-function gud-next   "gud" (arg))
(declare-function gud-stepi  "gud" (arg))
(declare-function gud-tbreak "gud" (arg))

(defvar tool-bar-map)
(defvar speedbar-initial-expansion-list-name)
(defvar speedbar-frame)

(defvar-local gdb-memory-address-expression "main"
  "This expression is passed to gdb.
Possible value: main, $rsp, x+3.")
(defvar-local gdb-memory-address nil
  "Address of memory display.")
(defvar-local gdb-memory-last-address nil
  "Last successfully accessed memory address.")
(defvar	gdb-memory-next-page nil
  "Address of next memory page for program memory buffer.")
(defvar	gdb-memory-prev-page nil
  "Address of previous memory page for program memory buffer.")
(defvar-local gdb--memory-display-warning nil
  "Display warning on memory header if t.

When error occurs when retrieving memory, gdb-mi displays the
last successful page.  In that case the expression might not
match the memory displayed.  We want to let the user be aware of
that, so display a warning exclamation mark in the header line.")

(defvar gdb-thread-number nil
  "Main current thread.

Invalidation triggers use this variable to query GDB for
information on the specified thread by wrapping GDB/MI commands
in `gdb-current-context-command'.

This variable may be updated implicitly by GDB via `gdb-stopped'
or explicitly by `gdb-select-thread'.

Only `gdb-setq-thread-number' should be used to change this
value.")

(defvar gdb-frame-number nil
  "Selected frame level for main current thread.

Updated according to the following rules:

When a thread is selected or current thread stops, set to \"0\".

When current thread goes running (and possibly exits eventually),
set to nil.

May be manually changed by user with `gdb-select-frame'.")

(defvar gdb-frame-address nil "Identity of frame for watch expression.")

;; Used to show overlay arrow in source buffer. All set in
;; gdb-get-main-selected-frame. Disassembly buffer should not use
;; these but rely on buffer-local thread information instead.
(defvar gdb-selected-frame nil
  "Name of selected function for main current thread.")
(defvar gdb-selected-file nil
  "Name of selected file for main current thread.")
(defvar gdb-selected-line nil
  "Number of selected line for main current thread.")

(defvar gdb-threads-list nil
  "Associative list of threads provided by \"-thread-info\" MI command.

Keys are thread numbers (in strings) and values are structures as
returned from -thread-info by `gdb-mi--partial-output'.  Updated in
`gdb-thread-list-handler-custom'.")

(defvar gdb-running-threads-count nil
  "Number of currently running threads.

If nil, no information is available.

Updated in `gdb-thread-list-handler-custom'.")

(defvar gdb-stopped-threads-count nil
  "Number of currently stopped threads.

See also `gdb-running-threads-count'.")

(defvar gdb-breakpoints-list nil
  "Associative list of breakpoints provided by \"-break-list\" MI command.

Keys are breakpoint numbers (in string) and values are structures
as returned from \"-break-list\" by `gdb-mi--partial-output'
\(\"body\" field is used). Updated in
`gdb-breakpoints-list-handler-custom'.")

(defvar gdb-current-language nil)
(defvar gdb-var-list nil
  "List of variables in watch window.
Each element has the form
  (VARNUM EXPRESSION NUMCHILD TYPE VALUE STATUS HAS_MORE FP)
where STATUS is nil (`unchanged'), `changed' or `out-of-scope', FP the frame
address for root variables.")
(defvar gdb-main-file nil "Source file from which program execution begins.")

;; Overlay arrow markers
(defvar gdb-stack-position nil)
(defvar gdb-thread-position nil)
(defvar gdb-disassembly-position nil)

(defvar gdb-location-alist nil
  "Alist of breakpoint numbers and full filenames.
Only used for files that Emacs can't find.")
(defvar gdb-active-process nil
  "GUD tooltips display variable values when t, and macro definitions otherwise.")
(defvar gdb-error "Non-nil when GDB is reporting an error.")
(defvar gdb-macro-info nil
  "Non-nil if GDB knows that the inferior includes preprocessor macro info.")
(defvar gdb-register-names nil "List of register names.")
(defvar gdb-changed-registers nil
  "List of changed register numbers (strings).")
(defvar gdb-buffer-fringe-width nil)
(defvar gdb-last-command nil)
(defvar gdb-prompt-name nil)
(defvar gdb-token-number 0)
(defvar gdb-handler-list '()
  "List of gdb-handler keeping track of all pending GDB commands.")
(defvar gdb-source-file-list nil
  "List of source files for the current executable.")
(defvar gdb-first-done-or-error t)
(defvar gdb-source-window-list nil
  "List of windows used for displaying source files.
Sorted in most-recently-visited-first order.")
(defvar gdb-inferior-status nil)
(defvar gdb-continuation nil)
(defvar gdb-supports-non-stop nil)
(defvar gdb-filter-output nil
  "Message to be shown in GUD console.

This variable is updated in `gdb-done-or-error' and returned by
`gud-gdbmi-marker-filter'.")

(defvar gdb-non-stop nil
  "Indicates whether current GDB session is using non-stop mode.

It is initialized to `gdb-non-stop-setting' at the beginning of
every GDB session.")

(defvar-local gdb-buffer-type nil
  "One of the symbols bound in `gdb-buffer-rules'.")

(defvar gdb-output-sink 'nil
  "The disposition of the output of the current gdb command.
Possible values are these symbols:

    `user' -- gdb output should be copied to the GUD buffer
              for the user to see.

    `emacs' -- output should be collected in the partial-output-buffer
	       for subsequent processing by a command.  This is the
	       disposition of output generated by commands that
	       gdb mode sends to gdb on its own behalf.")

(defvar gdb--window-configuration-before nil
  "Stores the window configuration before starting GDB.")

(defcustom gdb-restore-window-configuration-after-quit nil
  "If non-nil, restore window configuration as of before GDB started.

Possible values are:
    t -- Always restore.
    nil -- Don't restore.
    `if-gdb-show-main' -- Restore only if variable `gdb-show-main'
                          is non-nil
    `if-gdb-many-windows' -- Restore only if variable `gdb-many-windows'
                             is non-nil."
  :type '(choice
          (const :tag "Always restore" t)
          (const :tag "Don't restore" nil)
          (const :tag "Depends on `gdb-show-main'" 'if-gdb-show-main)
          (const :tag "Depends on `gdb-many-windows'" 'if-gdb-many-windows))
  :group 'gdb
  :version "28.1")

(defcustom gdb-discard-unordered-replies t
  "Non-nil means discard any out-of-order GDB replies.
This protects against lost GDB replies, assuming that GDB always
replies in the same order as Emacs sends commands.  When receiving a
reply with a given token-number, assume any pending messages with a
lower token-number are out-of-order."
  :type 'boolean
  :group 'gud
  :version "24.4")

(cl-defstruct gdb-handler
  "Data required to handle the reply of a command sent to GDB."
  ;; Prefix of the command sent to GDB.  The GDB reply for this command
  ;; will be prefixed with this same TOKEN-NUMBER
  (token-number nil :read-only t)
  ;; Callback to invoke when the reply is received from GDB
  (function nil :read-only t)
  ;; PENDING-TRIGGER is used to prevent congestion: Emacs won't send
  ;; two requests with the same PENDING-TRIGGER until a reply is received
  ;; for the first one."
  (pending-trigger nil))

(defun gdb-add-handler (token-number handler-function &optional pending-trigger)
  "Insert a new GDB command handler in `gdb-handler-list'.
Handlers are used to keep track of the commands sent to GDB
and to handle the replies received.
Upon reception of a reply prefixed with TOKEN-NUMBER,
invoke the callback HANDLER-FUNCTION.
If PENDING-TRIGGER is specified, no new GDB commands will be
sent with this same PENDING-TRIGGER until a reply is received
for this handler."

  (push (make-gdb-handler :token-number token-number
                          :function handler-function
                          :pending-trigger pending-trigger)
        gdb-handler-list))

(defun gdb-delete-handler (token-number)
  "Remove the handler TOKEN-NUMBER from `gdb-handler-list'.
Additionally, if `gdb-discard-unordered-replies' is non-nil,
discard all handlers having a token number less than TOKEN-NUMBER."
  (if gdb-discard-unordered-replies

      (setq gdb-handler-list
            (cl-delete-if
             (lambda (handler)
               "Discard any HANDLER with a token number `<=' than TOKEN-NUMBER."
               (when (< (gdb-handler-token-number handler) token-number)
                 (message "WARNING! Discarding GDB handler with token #%d\n"
			  (gdb-handler-token-number handler)))
               (<= (gdb-handler-token-number handler) token-number))
             gdb-handler-list))

    (setq gdb-handler-list
          (cl-delete-if
           (lambda (handler)
             "Discard any HANDLER with a token number `eq' to TOKEN-NUMBER."
             (eq (gdb-handler-token-number handler) token-number))
           gdb-handler-list))))

(defun gdb-get-handler-function (token-number)
  "Return the function callback registered with the handler TOKEN-NUMBER."
  (gdb-handler-function
   (cl-find-if (lambda (handler) (eq (gdb-handler-token-number handler)
                                      token-number))
                gdb-handler-list)))


(defun gdb-pending-handler-p (pending-trigger)
  "Return non-nil if a command handler is pending with trigger PENDING-TRIGGER."
  (cl-find-if (lambda (handler) (eq (gdb-handler-pending-trigger handler)
                                     pending-trigger))
               gdb-handler-list))


(defun gdb-handle-reply (token-number)
  "Handle the GDB reply TOKEN-NUMBER.
This invokes the handler registered with this token number
in `gdb-handler-list' and clears all pending handlers invalidated
by the reception of this reply."
  (let ((handler-function (gdb-get-handler-function token-number)))
    (when handler-function
      ;; Protect against errors in handler-function.
      (condition-case err
          (funcall handler-function)
        (error (message (error-message-string err))))
      (gdb-delete-handler token-number))))

(defun gdb-remove-all-pending-triggers ()
  "Remove all pending triggers from gdb-handler-list.
The handlers are left in gdb-handler-list so that replies received
from GDB could still be handled.  However, removing the pending triggers
allows Emacs to send new commands even if replies of previous commands
were not yet received."
  (dolist (handler gdb-handler-list)
    (setf (gdb-handler-pending-trigger handler) nil)))

(defun gdb-wait-for-pending (func)
  "Wait for all pending GDB commands to finish and call FUNC.

This function checks every 0.5 seconds if there are any pending
triggers in `gdb-handler-list'."
  (run-with-timer
   0.5 nil
   (lambda ()
     (if (cl-some #'gdb-handler-pending-trigger gdb-handler-list)
	 (gdb-wait-for-pending func)
       (funcall func)))))

;; Publish-subscribe

(defmacro gdb-add-subscriber (publisher subscriber)
  "Register new PUBLISHER's SUBSCRIBER.

SUBSCRIBER must be a pair, where cdr is a function of one
argument (see `gdb-emit-signal')."
  `(add-to-list ',publisher ,subscriber t))

(defmacro gdb-delete-subscriber (publisher subscriber)
  "Unregister SUBSCRIBER from PUBLISHER."
  `(setq ,publisher (delete ,subscriber
                            ,publisher)))

(defun gdb-get-subscribers (publisher)
  publisher)

(defun gdb-emit-signal (publisher &optional signal)
  "Call cdr for each subscriber of PUBLISHER with SIGNAL as argument."
  (dolist (subscriber (gdb-get-subscribers publisher))
    (funcall (cdr subscriber) signal)))

(defvar gdb-buf-publisher '()
  "Used to invalidate GDB buffers by emitting a signal in `gdb-update'.
Must be a list of pairs with cars being buffers and cdr's being
valid signal handlers.")

(defgroup gdb nil
  "GDB graphical interface."
  :group 'tools
  :link '(info-link "(emacs)GDB Graphical Interface")
  :version "23.2")

(defgroup gdb-non-stop nil
  "GDB non-stop debugging settings."
  :group 'gdb
  :version "23.2")

(defgroup gdb-buffers nil
  "GDB buffers."
  :group 'gdb
  :version "23.2")

(defcustom gdb-debug-log-max 128
  "Maximum size of `gdb-debug-log'.  If nil, size is unlimited."
  :group 'gdb
  :type '(choice (integer :tag "Number of elements")
          (const   :tag "Unlimited" nil))
  :version "22.1")

(defcustom gdb-non-stop-setting (not (eq system-type 'windows-nt))
  "If non-nil, GDB sessions are expected to support the non-stop mode.
When in the non-stop mode, stopped threads can be examined while
other threads continue to execute.

If this is non-nil, GDB will be sent the \"set non-stop 1\" command,
and if that results in an error, the non-stop setting will be
turned off automatically.

On MS-Windows, this is off by default, because MS-Windows targets
don't support the non-stop mode.

GDB session needs to be restarted for this setting to take effect."
  :type 'boolean
  :group 'gdb-non-stop
  :version "26.1")

;; TODO Some commands can't be called with --all (give a notice about
;; it in setting doc)
(defcustom gdb-gud-control-all-threads t
  "When non-nil, GUD execution commands affect all threads when in non-stop mode.
Otherwise, only current thread is affected."
  :type 'boolean
  :group 'gdb-non-stop
  :version "23.2")

(defcustom gdb-switch-reasons t
  "List of stop reasons for which Emacs should switch thread.
When t, switch to stopped thread no matter what the reason was.
When nil, never switch to stopped thread automatically.

This setting is used in non-stop mode only.  In all-stop mode,
Emacs always switches to the thread which caused the stop."
  ;; exited, exited-normally and exited-signaled are not
  ;; thread-specific stop reasons and therefore are not included in
  ;; this list
  :type '(choice
          (const :tag "All reasons" t)
          (set :tag "Selection of reasons..."
               (const :tag "A breakpoint was reached." "breakpoint-hit")
               (const :tag "A watchpoint was triggered." "watchpoint-trigger")
               (const :tag "A read watchpoint was triggered."
                      "read-watchpoint-trigger")
               (const :tag "An access watchpoint was triggered."
                      "access-watchpoint-trigger")
               (const :tag "Function finished execution." "function-finished")
               (const :tag "Location reached." "location-reached")
               (const :tag "Watchpoint has gone out of scope"
                      "watchpoint-scope")
               (const :tag "End of stepping range reached."
                      "end-stepping-range")
               (const :tag "Signal received (like interruption)."
                      "signal-received"))
          (const :tag "None" nil))
  :group 'gdb-non-stop
  :version "23.2"
  :link '(info-link "(gdb)GDB/MI Async Records"))

(defcustom gdb-stopped-functions nil
  "List of functions called whenever GDB stops.

Each function takes one argument, a parsed MI response, which
contains fields of corresponding MI *stopped async record:

    ((stopped-threads . \"all\")
     (thread-id . \"1\")
     (frame (line . \"38\")
            (fullname . \"/home/sphinx/projects/gsoc/server.c\")
            (file . \"server.c\")
            (args ((value . \"0x804b038\")
                   (name . \"arg\")))
            (func . \"hello\")
            (addr . \"0x0804869e\"))
     (reason . \"end-stepping-range\"))

Note that \"reason\" is only present in non-stop debugging mode.

Each function is called after the new current thread was selected
and GDB buffers were updated in `gdb-stopped'."
  :type '(repeat function)
  :group 'gdb
  :version "23.2"
  :link '(info-link "(gdb)GDB/MI Async Records"))

(defcustom gdb-switch-when-another-stopped t
  "When nil, don't switch to stopped thread if some other
stopped thread is already selected."
  :type 'boolean
  :group 'gdb-non-stop
  :version "23.2")

(defcustom gdb-stack-buffer-locations t
  "Show file information or library names in stack buffers."
  :type 'boolean
  :group 'gdb-buffers
  :version "23.2")

(defcustom gdb-stack-buffer-addresses nil
  "Show frame addresses in stack buffers."
  :type 'boolean
  :group 'gdb-buffers
  :version "23.2")

(defcustom gdb-thread-buffer-verbose-names t
  "Show long thread names in threads buffer."
  :type 'boolean
  :group 'gdb-buffers
  :version "23.2")

(defcustom gdb-thread-buffer-arguments t
  "Show function arguments in threads buffer."
  :type 'boolean
  :group 'gdb-buffers
  :version "23.2")

(defcustom gdb-thread-buffer-locations t
  "Show file information or library names in threads buffer."
  :type 'boolean
  :group 'gdb-buffers
  :version "23.2")

(defcustom gdb-thread-buffer-addresses nil
  "Show addresses for thread frames in threads buffer."
  :type 'boolean
  :group 'gdb-buffers
  :version "23.2")

(defcustom gdb-show-threads-by-default nil
  "Show threads list buffer instead of breakpoints list by default."
  :type 'boolean
  :group 'gdb-buffers
  :version "23.2")

(defcustom gdb-registers-enable-filter nil
  "If non-nil, enable register name filter in register buffer.
Use `gdb-registers-filter-pattern-list' to control what register to
filter."
  :type 'boolean
  :group 'gdb-buffers
  :version "28.1")

(defcustom gdb-registers-filter-pattern-list nil
  "Patterns for names that are displayed in register buffer.
Each pattern is a regular expression.  GDB displays registers
whose name matches any pattern in the list.  Refresh the register
buffer for the change to take effect."
  :type '(repeat regexp)
  :group 'gdb-buffers
  :version "28.1")

(defvar gdb-debug-log nil
  "List of commands sent to and replies received from GDB.
Most recent commands are listed first.  This list stores only the last
`gdb-debug-log-max' values.  This variable is used to debug GDB-MI.")

;;;###autoload
(define-minor-mode gdb-enable-debug
  "Toggle logging of transaction between Emacs and Gdb.
The log is stored in `gdb-debug-log' as an alist with elements
whose cons is send, send-item or recv and whose cdr is the string
being transferred.  This list may grow up to a size of
`gdb-debug-log-max' after which the oldest element (at the end of
the list) is deleted every time a new one is added (at the front)."
  :global t
  :group 'gdb
  :version "22.1")

(defcustom gdb-cpp-define-alist-program "gcc -E -dM -"
  "Shell command for generating a list of defined macros in a source file.
This list is used to display the #define directive associated
with an identifier as a tooltip.  It works in a debug session with
GDB, when `gud-tooltip-mode' is t.

Set `gdb-cpp-define-alist-flags' for any include paths or
predefined macros."
  :type 'string
  :group 'gdb
  :version "22.1")

(defcustom gdb-cpp-define-alist-flags ""
  "Preprocessor flags for `gdb-cpp-define-alist-program'."
  :type 'string
  :group 'gdb
  :version "22.1")

(defcustom gdb-create-source-file-list t
  "Non-nil means create a list of files from which the executable was built.
Set this to nil if the GUD buffer displays \"initializing...\" in the mode
line for a long time when starting, possibly because your executable was
built from a large number of files.  This allows quicker initialization
but means that these files are not automatically enabled for debugging,
e.g., you won't be able to click in the fringe to set a breakpoint until
execution has already stopped there."
  :type 'boolean
  :group 'gdb
  :version "23.1")

(defcustom gdb-show-main nil
  "Non-nil means display source file containing the main routine at startup.
Also display the main routine in the disassembly buffer if present."
  :type 'boolean
  :group 'gdb
  :version "22.1")

(defcustom gdb-window-configuration-directory user-emacs-directory
  "Directory where GDB window configuration files are stored.
If nil, use `default-directory'."
  :type 'string
  :group 'gdb
  :version "28.1")

(defcustom gdb-default-window-configuration-file nil
  "If non-nil, load this window configuration (layout) on startup.
This should be the full name of the window configuration file.
If this is not an absolute path, GDB treats it as a relative path
and looks under `gdb-window-configuration-directory'.

Note that this variable only takes effect when variable
`gdb-many-windows' is t."
  :type '(choice (const :tag "None" nil)
                 string)
  :group 'gdb
  :version "28.1")

(defcustom gdb-display-source-buffer-action '(nil . ((inhibit-same-window . t)))
  "`display-buffer' action used when GDB displays a source buffer."
  :type 'sexp
  :group 'gdb
  :version "28.1")

(defcustom gdb-max-source-window-count 1
  "Maximum number of source windows to use.
Until there are such number of source windows on screen, GDB
tries to open a new window when visiting a new source file; after
that GDB starts to reuse existing source windows."
  :type 'number
  :group 'gdb
  :version "28.1")

(defvar gdbmi-debug-mode nil
  "When non-nil, print the messages sent/received from GDB/MI in *Messages*.")

(defun gdb-force-mode-line-update (status)
  (let ((buffer gud-comint-buffer))
    (if (and buffer (buffer-name buffer))
	(with-current-buffer buffer
	  (setq mode-line-process
		(format ":%s [%s]"
			(process-status (get-buffer-process buffer)) status))
	  ;; Force mode line redisplay soon.
	  (force-mode-line-update)))))

;; These two are used for menu and toolbar
(defun gdb-control-all-threads ()
  "Switch to non-stop/A mode."
  (interactive)
  (setq gdb-gud-control-all-threads t)
  ;; Actually forcing the tool-bar to update.
  (force-mode-line-update)
  (message "Now in non-stop/A mode."))

(defun gdb-control-current-thread ()
  "Switch to non-stop/T mode."
  (interactive)
  (setq gdb-gud-control-all-threads nil)
  ;; Actually forcing the tool-bar to update.
  (force-mode-line-update)
  (message "Now in non-stop/T mode."))

(defun gdb-find-watch-expression ()
  (let* ((var (nth (- (line-number-at-pos (point)) 2) gdb-var-list))
	 (varnum (car var)) expr)
    (string-match "\\(var[0-9]+\\)\\.\\(.*\\)" varnum)
    (let ((var1 (assoc (match-string 1 varnum) gdb-var-list)) var2 varnumlet
	  (component-list (split-string (match-string 2 varnum) "\\." t)))
      (setq expr (nth 1 var1))
      (setq varnumlet (car var1))
      (dolist (component component-list)
	(setq var2 (assoc varnumlet gdb-var-list))
	(setq expr (concat expr
			   (if (string-match ".*\\[[0-9]+\\]$" (nth 3 var2))
			       (concat "[" component "]")
			     (concat "." component))))
	(setq varnumlet (concat varnumlet "." component)))
      expr)))

;; noall is used for commands which don't take --all, but only
;; --thread.
(defun gdb-gud-context-command (command &optional noall)
  "When `gdb-non-stop' is t, add --thread option to COMMAND if
`gdb-gud-control-all-threads' is nil and --all option otherwise.
If NOALL is t, always add --thread option no matter what
`gdb-gud-control-all-threads' value is.

When `gdb-non-stop' is nil, return COMMAND unchanged."
  (if gdb-non-stop
      (if (and gdb-gud-control-all-threads
               (not noall)
	       gdb-supports-non-stop)
          (concat command " --all ")
        (gdb-current-context-command command))
    command))

(defmacro gdb-gud-context-call (cmd1 &optional cmd2 noall noarg)
  "`gud-call' wrapper which adds --thread/--all options between
CMD1 and CMD2.  NOALL is the same as in `gdb-gud-context-command'.

NOARG must be t when this macro is used outside `gud-def'."
  `(gud-call
    (concat (gdb-gud-context-command ,cmd1 ,noall) " " ,cmd2)
    ,(when (not noarg) 'arg)))

(defun gdb--check-interpreter (filter proc string)
  (unless (zerop (length string))
    (remove-function (process-filter proc) #'gdb--check-interpreter)
    (unless (memq (aref string 0) '(?^ ?~ ?@ ?& ?* ?=))
      ;; Apparently we're not running with -i=mi (or we're, for
      ;; instance, debugging something inside a Docker instance with
      ;; Emacs on the outside).
      (let ((msg (substitute-command-keys
                  "Error: Either -i=mi wasn't specified on the GDB command line,\
 or the extra socket couldn't be established.  Consider using \\[gud-gdb] instead.")))
        (message msg)
        (setq string (concat (propertize msg 'font-lock-face 'error)
                             "\n" string)))
      ;; Use the old gud-gbd filter, not because it works, but because it
      ;; will properly display GDB's answers rather than hanging waiting for
      ;; answers that aren't coming.
      (setq-local gud-marker-filter #'gud-gdb-marker-filter))
    (funcall filter proc string)))

(defvar gdb-control-level 0)

;;;###autoload
(defun gdb (command-line)
  "Run gdb passing it COMMAND-LINE as arguments.

If COMMAND-LINE names a program FILE to debug, gdb will run in
a buffer named *gud-FILE*, and the directory containing FILE
becomes the initial working directory and source-file directory
for your debugger.
If COMMAND-LINE requests that gdb attaches to a process PID, gdb
will run in *gud-PID*, otherwise it will run in *gud*; in these
cases the initial working directory is the `default-directory' of
the buffer in which this command was invoked.

COMMAND-LINE should include \"-i=mi\" to use gdb's MI text interface.
Note that the old \"--annotate\" option is no longer supported.

If option `gdb-many-windows' is nil (the default value) then gdb just
pops up the GUD buffer unless `gdb-show-main' is t.  In this case
it starts with two windows: one displaying the GUD buffer and the
other with the source file with the main routine of the inferior.

If option `gdb-many-windows' is t, regardless of the value of
`gdb-show-main', the layout below will appear.  Keybindings are
shown in some of the buffers.

Watch expressions appear in the speedbar/slowbar.

The following commands help control operation :

`gdb-many-windows'    - Toggle the number of windows gdb uses.
`gdb-restore-windows' - To restore the window layout.

See Info node `(emacs)GDB Graphical Interface' for a more
detailed description of this mode.


+----------------------------------------------------------------------+
|                               GDB Toolbar                            |
+-----------------------------------+----------------------------------+
| GUD buffer (I/O of GDB)           | Locals buffer                    |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
+-----------------------------------+----------------------------------+
| Source buffer                     | I/O buffer (of debugged program) |
|                                   | (comint-mode)                    |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
|                                   |                                  |
+-----------------------------------+----------------------------------+
| Stack buffer                      | Breakpoints buffer               |
| RET      gdb-select-frame         | SPC    gdb-toggle-breakpoint     |
|                                   | RET    gdb-goto-breakpoint       |
|                                   | D      gdb-delete-breakpoint     |
+-----------------------------------+----------------------------------+"
  ;;
  (interactive (list (gud-query-cmdline 'gdb)))

  (when (and gud-comint-buffer
             (buffer-name gud-comint-buffer)
             (get-buffer-process gud-comint-buffer)
             (with-current-buffer gud-comint-buffer (eq gud-minor-mode 'gdba)))
    (gdb-restore-windows)
    (error
     "Multiple debugging requires restarting in text command mode"))

  ;; Save window configuration before starting gdb so we can restore
  ;; it after gdb quits. Save it regardless of the value of
  ;; `gdb-restore-window-configuration-after-quit'.
  (setq gdb--window-configuration-before (window-state-get))

  ;;
  (gud-common-init command-line nil 'gud-gdbmi-marker-filter)

  ;; Setup a temporary process filter to warn when GDB was not started
  ;; with -i=mi.
  (let ((proc (get-buffer-process gud-comint-buffer)))
    (add-function :around (process-filter proc) #'gdb--check-interpreter))

  (setq-local gud-minor-mode 'gdbmi)
  (setq-local gdb-control-level 0)
  (setq comint-input-sender 'gdb-send)
  (when (ring-empty-p comint-input-ring) ; cf shell-mode
    (let ((hfile (expand-file-name (or (getenv "GDBHISTFILE")
				       (if (eq system-type 'ms-dos)
					   "_gdb_history"
					 ".gdb_history"))))
	  ;; gdb defaults to 256, but we'll default to comint-input-ring-size.
	  (hsize (getenv "HISTSIZE")))
      (dolist (file (append '("~/.gdbinit")
			    (unless (string-equal (expand-file-name ".")
                                                  (expand-file-name "~"))
			      '(".gdbinit"))))
	(if (file-readable-p (setq file (expand-file-name file)))
	    (with-temp-buffer
	      (insert-file-contents file)
	      ;; TODO? check for "set history save\\(  *on\\)?" and do
	      ;; not use history otherwise?
	      (while (re-search-forward
		      "^ *set history \\(filename\\|size\\)  *\\(.*\\)" nil t)
		(cond ((string-equal (match-string 1) "filename")
		       (setq hfile (expand-file-name
				    (match-string 2)
				    (file-name-directory file))))
		      ((string-equal (match-string 1) "size")
		       (setq hsize (match-string 2))))))))
      (and (stringp hsize)
	   (integerp (setq hsize (string-to-number hsize)))
	   (> hsize 0)
           (setq-local comint-input-ring-size hsize))
      (if (stringp hfile)
          (setq-local comint-input-ring-file-name hfile))
      (comint-read-input-ring t)))
  (gud-def gud-tbreak "tbreak %f:%l" "\C-t"
	   "Set temporary breakpoint at current line.")
  (gud-def gud-jump
	   (progn (gud-call "tbreak %f:%l" arg) (gud-call "jump %f:%l"))
	   "\C-j" "Set execution address to current line.")

  (gud-def gud-up     "up %p"     "<" "Up N stack frames (numeric arg).")
  (gud-def gud-down   "down %p"   ">" "Down N stack frames (numeric arg).")
  (gud-def gud-print  "print %e"  "\C-p" "Evaluate C expression at point.")
  (gud-def gud-pstar  "print* %e" nil
	   "Evaluate C dereferenced pointer expression at point.")

  (gud-def gud-step   (gdb-gud-context-call "-exec-step" "%p" t)
           "\C-s"
	   "Step one source line with display.")
  (gud-def gud-stepi  (gdb-gud-context-call "-exec-step-instruction" "%p" t)
           "\C-i"
	   "Step one instruction with display.")
  (gud-def gud-next   (gdb-gud-context-call "-exec-next" "%p" t)
           "\C-n"
	   "Step one line (skip functions).")
  (gud-def gud-nexti  (gdb-gud-context-call "-exec-next-instruction" "%p" t)
           nil
	   "Step one instruction (skip functions).")
  (gud-def gud-cont   (gdb-gud-context-call "-exec-continue")
           "\C-r"
	   "Continue with display.")
  (gud-def gud-finish (gdb-gud-context-call "-exec-finish" nil t)
           "\C-f"
	   "Finish executing current function.")
  (gud-def gud-run    "-exec-run"
           nil
           "Run the program.")

  (gud-def gud-break (if (not (string-match "Disassembly" mode-name))
			 (gud-call "break %f:%l" arg)
		       (save-excursion
			 (beginning-of-line)
			 (forward-char 2)
			 (gud-call "break *%a" arg)))
	   "\C-b" "Set breakpoint at current line or address.")

  (gud-def gud-remove (if (not (string-match "Disassembly" mode-name))
			  (gud-call "clear %f:%l" arg)
			(save-excursion
			  (beginning-of-line)
			  (forward-char 2)
			  (gud-call "clear *%a" arg)))
	   "\C-d" "Remove breakpoint at current line or address.")

  ;; -exec-until doesn't support --all yet
  (gud-def gud-until  (if (not (string-match "Disassembly" mode-name))
			  (gud-call "-exec-until %f:%l" arg)
			(save-excursion
			  (beginning-of-line)
			  (forward-char 2)
			  (gud-call "-exec-until *%a" arg)))
	   "\C-u" "Continue to current line or address.")
  ;; TODO Why arg here?
  (gud-def
   gud-go (gud-call (if gdb-active-process
                        (gdb-gud-context-command "-exec-continue")
                      "-exec-run") arg)
   nil "Start or continue execution.")

  ;; For debugging Emacs only.
  (gud-def gud-pp
	   (gud-call
	    (concat
	     "pp " (if (eq (buffer-local-value
			    'major-mode (window-buffer)) 'speedbar-mode)
		       (gdb-find-watch-expression) "%e")) arg)
	   nil   "Print the Emacs s-expression.")

  (define-key gud-minor-mode-map [left-margin mouse-1]
    'gdb-mouse-set-clear-breakpoint)
  (define-key gud-minor-mode-map [left-fringe mouse-1]
    'gdb-mouse-set-clear-breakpoint)
  (define-key gud-minor-mode-map [left-margin C-mouse-1]
    'gdb-mouse-toggle-breakpoint-margin)
  (define-key gud-minor-mode-map [left-fringe C-mouse-1]
    'gdb-mouse-toggle-breakpoint-fringe)

  (define-key gud-minor-mode-map [left-margin drag-mouse-1]
    'gdb-mouse-until)
  (define-key gud-minor-mode-map [left-fringe drag-mouse-1]
    'gdb-mouse-until)
  (define-key gud-minor-mode-map [left-margin mouse-3]
    'gdb-mouse-until)
  (define-key gud-minor-mode-map [left-fringe mouse-3]
    'gdb-mouse-until)

  (define-key gud-minor-mode-map [left-margin C-drag-mouse-1]
    'gdb-mouse-jump)
  (define-key gud-minor-mode-map [left-fringe C-drag-mouse-1]
    'gdb-mouse-jump)
  (define-key gud-minor-mode-map [left-fringe C-mouse-3]
    'gdb-mouse-jump)
  (define-key gud-minor-mode-map [left-margin C-mouse-3]
    'gdb-mouse-jump)

  (gud-set-repeat-map-property 'gud-gdb-repeat-map)

  (setq-local gud-gdb-completion-function 'gud-gdbmi-completions)

  (add-hook 'completion-at-point-functions #'gud-gdb-completion-at-point
            nil 'local)
  (local-set-key "\C-i" 'completion-at-point)

  (local-set-key [remap comint-delchar-or-maybe-eof] 'gdb-delchar-or-quit)

  (setq gdb-first-prompt t)
  (setq gud-running nil)

  (gdb-update)

  (run-hooks 'gdb-mode-hook))

(defun gdb-init-1 ()
  ;; (Re-)initialize.
  (setq gdb-selected-frame nil
	gdb-frame-number nil
        gdb-thread-number nil
	gdb-var-list nil
	gdb-output-sink 'user
	gdb-location-alist nil
	gdb-source-file-list nil
	gdb-last-command nil
	gdb-token-number 0
	gdb-handler-list '()
	gdb-prompt-name nil
	gdb-first-done-or-error t
	gdb-buffer-fringe-width (car (window-fringes))
	gdb-debug-log nil
	gdb-source-window-list nil
	gdb-inferior-status nil
	gdb-continuation nil
        gdb-buf-publisher '()
        gdb-threads-list '()
        gdb-breakpoints-list '()
        gdb-register-names '()
        gdb-non-stop gdb-non-stop-setting)
  ;;
  (gdbmi-bnf-init)
  ;;
  (setq gdb-buffer-type 'gdbmi)
  ;;
  (gdb-force-mode-line-update
   (propertize "initializing..." 'face font-lock-variable-name-face))

  (gdb-get-buffer-create 'gdb-inferior-io)
  (gdb-clear-inferior-io)
  (gdb-inferior-io--init-proc (get-process "gdb-inferior"))

  (when (eq system-type 'windows-nt)
    ;; Don't create a separate console window for the debuggee.
    (gdb-input "-gdb-set new-console off" 'ignore)
    ;; Force GDB to behave as if its input and output stream were
    ;; connected to a TTY device (since on Windows we use pipes for
    ;; communicating with GDB).
    (gdb-input "-gdb-set interactive-mode on" 'ignore))
  (gdb-input "-gdb-set height 0" 'ignore)

  (when gdb-non-stop
    (gdb-input "-gdb-set non-stop 1" 'gdb-non-stop-handler))

  (gdb-input "-enable-pretty-printing" 'ignore)
  (gdb-input "-enable-frame-filters" 'ignore)

  ;; Find source file and compilation directory here.
  (if gdb-create-source-file-list
      ;; Needs GDB 6.2 onwards.
      (gdb-input "-file-list-exec-source-files" 'gdb-get-source-file-list))
  ;; Needs GDB 6.0 onwards.
  (gdb-input "-file-list-exec-source-file" 'gdb-get-source-file)
  (gdb-input "-gdb-show prompt" 'gdb-get-prompt))

(defun gdb-non-stop-handler ()
  (goto-char (point-min))
  (if (re-search-forward "No symbol" nil t)
      (progn
	(message
         "This version of GDB doesn't support non-stop mode.  Turning it off.")
	(setq gdb-non-stop nil)
	(setq gdb-supports-non-stop nil))
    (setq gdb-supports-non-stop t)
    (gdb-input "-gdb-set target-async 1" 'ignore)
    (gdb-input "-list-target-features" 'gdb-check-target-async)))

(defun gdb-check-target-async ()
  (goto-char (point-min))
  (unless (re-search-forward "async" nil t)
    (message
     "Target doesn't support non-stop mode.  Turning it off.")
    (setq gdb-non-stop nil)
    (gdb-input "-gdb-set non-stop 0" 'ignore)))

(defun gdb-delchar-or-quit (arg)
  "Delete ARG characters or send a quit command to GDB.
Send a quit only if point is at the end of the buffer, there is
no input, and GDB is waiting for input."
  (interactive "p")
  (unless (and (eq (current-buffer) gud-comint-buffer)
	       (eq gud-minor-mode 'gdbmi))
    (error "Not in a GDB-MI buffer"))
  (let ((proc (get-buffer-process gud-comint-buffer)))
    (if (and (eobp)
             (process-live-p proc)
	     (not gud-running)
	     (= (point) (marker-position (process-mark proc))))
	;; Sending an EOF does not work with GDB-MI; submit an
	;; explicit quit command.
	(progn
          (if (> gdb-control-level 0)
              (process-send-eof proc)
            (insert "quit")
            (comint-send-input t t)))
      (delete-char arg))))

(defvar gdb-define-alist nil "Alist of #define directives for GUD tooltips.")

(defun gdb-create-define-alist ()
  "Create an alist of #define directives for GUD tooltips."
  (let* ((file (buffer-file-name))
	 (output
	  (with-output-to-string
	    (with-current-buffer standard-output
 	      (and file
		   (file-exists-p file)
 		   ;; call-process doesn't work with remote file names.
		   (not (file-remote-p default-directory))
 		   (call-process shell-file-name file
				 (list t nil) nil "-c"
				 (concat gdb-cpp-define-alist-program " "
					 gdb-cpp-define-alist-flags))))))
         (define-list (split-string output "\n" t))
         (name))
    (setq gdb-define-alist nil)
    (dolist (define define-list)
      (setq name (nth 1 (split-string define "[( ]")))
      (push (cons name define) gdb-define-alist))))

(declare-function tooltip-show "tooltip" (text &optional use-echo-area))

(defconst gdb--string-regexp (rx "\""
                                 (* (or (seq "\\" nonl)
                                        (not (any "\"\\"))))
                                 "\""))

(defun gdb-tooltip-print (expr)
  (with-current-buffer (gdb-get-buffer 'gdb-partial-output-buffer)
    (goto-char (point-min))
    (cond
     ((re-search-forward (concat ".*value=\\(" gdb--string-regexp
                                 "\\)")
                         nil t)
      (tooltip-show
       (concat expr " = " (gdb-mi--c-string-from-string (match-string 1)))
       (or gud-tooltip-echo-area
	   (not (display-graphic-p)))))
     ((re-search-forward  "msg=\\(\".+\"\\)$" nil t)
      (tooltip-show (gdb-mi--c-string-from-string (match-string 1))
       (or gud-tooltip-echo-area
	   (not (display-graphic-p))))))))

;; If expr is a macro for a function don't print because of possible dangerous
;; side-effects. Also printing a function within a tooltip generates an
;; unexpected starting annotation (phase error).
(defun gdb-tooltip-print-1 (expr)
  (with-current-buffer (gdb-get-buffer 'gdb-partial-output-buffer)
    (goto-char (point-min))
    (if (search-forward "expands to: " nil t)
	(unless (looking-at "\\S-+.*(.*).*")
	  (gdb-input (concat "-data-evaluate-expression \"" expr "\"")
		     (lambda () (gdb-tooltip-print expr)))))))

(defun gdb-init-buffer ()
  (setq-local gud-minor-mode 'gdbmi)
  (setq-local tool-bar-map gud-tool-bar-map)
  (when gud-tooltip-mode
    (make-local-variable 'gdb-define-alist)
    (gdb-create-define-alist)
    (add-hook 'after-save-hook 'gdb-create-define-alist nil t)))

(defmacro gdb--if-arrow (arrow-position start-posn end-posn &rest body)
  (declare (indent 3))
  (let ((buffer (make-symbol "buffer")))
    `(if ,arrow-position
         (let ((,buffer (marker-buffer ,arrow-position)))
           (if (equal ,buffer (window-buffer (posn-window ,end-posn)))
               (with-current-buffer ,buffer
                 (when (or (equal ,start-posn ,end-posn)
                           (equal (posn-point ,start-posn)
                                  (marker-position ,arrow-position)))
                   ,@body)))))))

(defun gdb-mouse-until (event)
  "Continue running until a source line past the current line.
The destination source line can be selected either by clicking
with mouse-3 on the fringe/margin or dragging the arrow
with mouse-1 (default bindings)."
  (interactive "e")
  (let ((start (event-start event))
	(end (event-end event)))
    (gdb--if-arrow gud-overlay-arrow-position start end
      (let ((line (line-number-at-pos (posn-point end))))
        (gud-call (concat "until " (number-to-string line)))))
    (gdb--if-arrow gdb-disassembly-position start end
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- (line-number-at-pos (posn-point end))))
        (forward-char 2)
        (gud-call (concat "until *%a"))))))

(defun gdb-mouse-jump (event)
  "Set execution address/line.
The destination source line can be selected either by clicking with C-mouse-3
on the fringe/margin or dragging the arrow with C-mouse-1 (default bindings).
Unlike `gdb-mouse-until' the destination address can be before the current
line, and no execution takes place."
  (interactive "e")
  (let ((start (event-start event))
	(end (event-end event)))
    (gdb--if-arrow gud-overlay-arrow-position start end
      (let ((line (line-number-at-pos (posn-point end))))
        (gud-call (concat "tbreak " (number-to-string line)))
        (gud-call (concat "jump " (number-to-string line)))))
    (gdb--if-arrow gdb-disassembly-position start end
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- (line-number-at-pos (posn-point end))))
        (forward-char 2)
        (gud-call (concat "tbreak *%a"))
        (gud-call (concat "jump *%a"))))))

(defcustom gdb-show-changed-values t
  "If non-nil change the face of out of scope variables and changed values.
Out of scope variables are suppressed with `shadow' face.
Changed values are highlighted with the face `font-lock-warning-face'.
Used by Speedbar."
  :type 'boolean
  :group 'gdb
  :version "22.1")

(defcustom gdb-max-children 40
  "Maximum number of children before expansion requires confirmation.
Used by Speedbar."
  :type 'integer
  :group 'gdb
  :version "22.1")

(defcustom gdb-delete-out-of-scope t
  "If non-nil delete watch expressions automatically when they go out of scope."
  :type 'boolean
  :group 'gdb
  :version "22.2")

(define-minor-mode gdb-speedbar-auto-raise
  "Minor mode to automatically raise the speedbar for watch expressions."
  :global t
  :group 'gdb
  :version "22.1")

(defcustom gdb-use-colon-colon-notation nil
  "If non-nil use FUN::VAR format to display variables in the speedbar."
  :type 'boolean
  :group 'gdb
  :version "22.1")

(define-key gud-minor-mode-map "\C-c\C-w" 'gud-watch)
(define-key global-map (vconcat gud-key-prefix "\C-w") 'gud-watch)

(declare-function tooltip-identifier-from-point "tooltip" (point))

(defun gud-watch (&optional arg event)
  "Watch expression at point.
With arg, enter name of variable to be watched in the minibuffer."
  (interactive (list current-prefix-arg last-input-event))
  (let ((minor-mode (buffer-local-value 'gud-minor-mode gud-comint-buffer)))
    (if (eq minor-mode 'gdbmi)
	(progn
	  (if event (posn-set-point (event-end event)))
	  (require 'tooltip)
	  (save-selected-window
	    (let ((expr
		   (if arg
		       (completing-read "Name of variable: "
					'gud-gdb-complete-command)
		     (if (and transient-mark-mode mark-active)
			 (buffer-substring (region-beginning) (region-end))
		       (concat (if (derived-mode-p 'gdb-registers-mode) "$")
			       (tooltip-identifier-from-point (point)))))))
	      (set-text-properties 0 (length expr) nil expr)
	      (gdb-input (concat "-var-create - * "  expr "")
			 (lambda () (gdb-var-create-handler expr))))))
      (message "gud-watch is a no-op in this mode."))))

(defsubst gdb-mi--field (value field)
  (cdr (assq field value)))

(defun gdb-var-create-handler (expr)
  (let* ((result (gdb-mi--partial-output)))
    (if (not (gdb-mi--field result 'msg))
        (let ((var
	       (list (gdb-mi--field result 'name)
		     (if (and (string-equal gdb-current-language "c")
			      gdb-use-colon-colon-notation gdb-selected-frame)
			 (setq expr (concat gdb-selected-frame "::" expr))
		       expr)
		     (gdb-mi--field result 'numchild)
		     (gdb-mi--field result 'type)
		     (gdb-mi--field result 'value)
		     nil
		     (gdb-mi--field result 'has_more)
                     gdb-frame-address)))
	  (push var gdb-var-list)
	  (speedbar 1)
	  (unless (string-equal
		   speedbar-initial-expansion-list-name "GUD")
	    (speedbar-change-initial-expansion-list "GUD")))
      (message-box "No symbol \"%s\" in current context." expr))))

(defun gdb-speedbar-update ()
  (when (and (boundp 'speedbar-frame) (frame-live-p speedbar-frame))
    ;; Dummy command to update speedbar even when idle.
    (gdb-input "-environment-pwd"
               'gdb-speedbar-timer-fn
               'gdb-speedbar-update)))

(defun gdb-speedbar-timer-fn ()
  (if gdb-speedbar-auto-raise
      (raise-frame speedbar-frame))
  (speedbar-timer-fn))

                                        ; Uses "-var-list-children --all-values".  Needs GDB 6.1 onwards.
(defun gdb-var-list-children (varnum)
  (gdb-input (concat "-var-update " varnum) 'ignore)
  (gdb-input (concat "-var-list-children --all-values " varnum)
	     (lambda () (gdb-var-list-children-handler varnum))))

(defun gdb-var-list-children-handler (varnum)
  (let* ((var-list nil)
	 (output (gdb-mi--partial-output 'child))
	 (children (gdb-mi--field output 'children)))
    (catch 'child-already-watched
      (dolist (var gdb-var-list)
	(if (string-equal varnum (car var))
	    (progn
	      ;; With dynamic varobjs numchild may have increased.
	      (setcar (nthcdr 2 var) (gdb-mi--field output 'numchild))
	      (push var var-list)
	      (dolist (child children)
		(let ((varchild (list (gdb-mi--field child 'name)
				      (gdb-mi--field child 'exp)
				      (gdb-mi--field child 'numchild)
				      (gdb-mi--field child 'type)
				      (gdb-mi--field child 'value)
				      nil
				      (gdb-mi--field child 'has_more))))
		  (if (assoc (car varchild) gdb-var-list)
		      (throw 'child-already-watched nil))
		  (push varchild var-list))))
	  (push var var-list)))
      (setq gdb-var-list (nreverse var-list))))
  (gdb-speedbar-update))

(defun gdb-var-set-format (format)
  "Set the output format for a variable displayed in the speedbar."
  (let* ((var (nth (- (count-lines (point-min) (point)) 2) gdb-var-list))
	 (varnum (car var)))
    (gdb-input (concat "-var-set-format " varnum " " format) 'ignore)
    (gdb-var-update)))

(defun gdb-var-delete-1 (var varnum)
  (gdb-input (concat "-var-delete " varnum) 'ignore)
  (setq gdb-var-list (delq var gdb-var-list))
  (dolist (varchild gdb-var-list)
    (if (string-match (concat (car var) "\\.") (car varchild))
	(setq gdb-var-list (delq varchild gdb-var-list)))))

(defun gdb-var-delete ()
  "Delete watch expression at point from the speedbar."
  (interactive)
  (let ((text (speedbar-line-text)))
    (string-match "\\(\\S-+\\)" text)
    (let* ((var (nth (- (count-lines (point-min) (point)) 2) gdb-var-list))
           (varnum (car var)))
      (if (string-search "." (car var))
          (message-box "Can only delete a root expression")
        (gdb-var-delete-1 var varnum)))))

(defun gdb-var-delete-children (varnum)
  "Delete children of variable object at point from the speedbar."
  (gdb-input (concat "-var-delete -c " varnum) 'ignore))

(defun gdb-edit-value (_text _token _indent)
  "Assign a value to a variable displayed in the speedbar."
  (let* ((var (nth (- (count-lines (point-min) (point)) 2) gdb-var-list))
	 (varnum (car var))
         (value (read-string "New value: ")))
    (gdb-input (concat "-var-assign " varnum " " value)
	       (lambda () (gdb-edit-value-handler value)))))

(defconst gdb-error-regexp "\\^error,msg=\\(\".+\"\\)")

(defun gdb-edit-value-handler (value)
  (goto-char (point-min))
  (if (re-search-forward gdb-error-regexp nil t)
      (message-box "Invalid number or expression (%s)" value)))

                                        ; Uses "-var-update --all-values".  Needs GDB 6.4 onwards.
(defun gdb-var-update ()
  (gdb-input "-var-update --all-values *"
             'gdb-var-update-handler
             'gdb-var-update))

(defun gdb-var-update-handler ()
  (let ((changelist (gdb-mi--field (gdb-mi--partial-output) 'changelist)))
    (dolist (var gdb-var-list)
      (setcar (nthcdr 5 var) nil))
    (let ((temp-var-list gdb-var-list))
      (dolist (change changelist)
	(let* ((varnum (gdb-mi--field change 'name))
	       (var (assoc varnum gdb-var-list))
	       (new-num (gdb-mi--field change 'new_num_children)))
	  (when var
	    (let ((scope (gdb-mi--field change 'in_scope))
		  (has-more (gdb-mi--field change 'has_more)))
	      (cond ((string-equal scope "false")
		     (if gdb-delete-out-of-scope
			 (gdb-var-delete-1 var varnum)
		       (setcar (nthcdr 5 var) 'out-of-scope)))
		    ((string-equal scope "true")
		     (setcar (nthcdr 6 var) has-more)
		     (when (and (or (not has-more)
				    (string-equal has-more "0"))
				(not new-num)
				(string-equal (nth 2 var) "0"))
		       (setcar (nthcdr 4 var)
			       (gdb-mi--field change 'value))
		       (setcar (nthcdr 5 var) 'changed)))
		    ((string-equal scope "invalid")
		     (gdb-var-delete-1 var varnum)))))
	  (let ((var-list nil) var1
		(children (gdb-mi--field change 'new_children)))
	    (when new-num
              (setq var1 (pop temp-var-list))
              (while var1
                (if (string-equal varnum (car var1))
                    (let ((new (string-to-number new-num))
                          (previous (string-to-number (nth 2 var1))))
                      (setcar (nthcdr 2 var1) new-num)
                      (push var1 var-list)
                      (cond
                       ((> new previous)
                        ;; Add new children to list.
                        (dotimes (_ previous)
                          (push (pop temp-var-list) var-list))
                        (dolist (child children)
                          (let ((varchild
                                 (list (gdb-mi--field child 'name)
                                       (gdb-mi--field child 'exp)
                                       (gdb-mi--field child 'numchild)
                                       (gdb-mi--field child 'type)
                                       (gdb-mi--field child 'value)
                                       'changed
                                       (gdb-mi--field child 'has_more))))
                            (push varchild var-list))))
                       ;; Remove deleted children from list.
                       ((< new previous)
                        (dotimes (_ new)
                          (push (pop temp-var-list) var-list))
                        (dotimes (_ (- previous new))
                          (pop temp-var-list)))))
                  (push var1 var-list))
                (setq var1 (pop temp-var-list)))
              (setq gdb-var-list (nreverse var-list))))))))
  (gdb-speedbar-update))

(defun gdb-speedbar-expand-node (text token indent)
  "Expand the node the user clicked on.
TEXT is the text of the button we clicked on, a + or - item.
TOKEN is data related to this node.
INDENT is the current indentation depth."
  (cond ((string-search "+" text)        ;expand this node
	 (let* ((var (assoc token gdb-var-list))
		(expr (nth 1 var)) (children (nth 2 var)))
	   (if (or (<= (string-to-number children) gdb-max-children)
		   (y-or-n-p
                    (format "%s has %s children.  Continue?" expr children)))
	       (gdb-var-list-children token))))
	((string-search "-" text)	;contract this node
	 (dolist (var gdb-var-list)
	   (if (string-match (concat token "\\.") (car var))
	       (setq gdb-var-list (delq var gdb-var-list))))
	 (gdb-var-delete-children token)
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops...  not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun gdb-get-target-string ()
  (with-current-buffer gud-comint-buffer
    gud-target-name))


;;
;; gdb buffers.
;;
;; Each buffer has a TYPE -- a symbol that identifies the function
;; of that particular buffer.
;;
;; The usual gdb interaction buffer is given the type `gdbmi' and
;; is constructed specially.
;;
;; Others are constructed by gdb-get-buffer-create and
;; named according to the rules set forth in the gdb-buffer-rules

(defvar gdb-buffer-rules '())

(defun gdb-rules-name-maker (rules-entry)
  (cadr rules-entry))
(defun gdb-rules-buffer-mode (rules-entry)
  (nth 2 rules-entry))
(defun gdb-rules-update-trigger (rules-entry)
  (nth 3 rules-entry))

(defun gdb-update-buffer-name ()
  "Rename current buffer according to name-maker associated with
it in `gdb-buffer-rules'."
  (let ((f (gdb-rules-name-maker (assoc gdb-buffer-type
                                        gdb-buffer-rules))))
    (when f (rename-buffer (funcall f)))))

(defun gdb-current-buffer-rules ()
  "Get `gdb-buffer-rules' entry for current buffer type."
  (assoc gdb-buffer-type gdb-buffer-rules))

(defun gdb-current-buffer-thread ()
  "Get thread object of current buffer from `gdb-threads-list'.

When current buffer is not bound to any thread, return main
thread."
  (cdr (assoc gdb-thread-number gdb-threads-list)))

(defun gdb-current-buffer-frame ()
  "Get current stack frame object for thread of current buffer."
  (gdb-mi--field (gdb-current-buffer-thread) 'frame))

(defun gdb-buffer-type (buffer)
  "Get value of `gdb-buffer-type' for BUFFER."
  (with-current-buffer buffer
    gdb-buffer-type))

(defun gdb-buffer-shows-main-thread-p ()
  "Return t if current GDB buffer shows main selected thread and
is not bound to it."
  (current-buffer)
  (not (local-variable-p 'gdb-thread-number)))

(defun gdb-get-buffer (buffer-type &optional thread)
  "Get a specific GDB buffer.

In that buffer, `gdb-buffer-type' must be equal to BUFFER-TYPE
and `gdb-thread-number' (if provided) must be equal to THREAD."
  (catch 'found
    (dolist (buffer (buffer-list) nil)
      (with-current-buffer buffer
        (when (and (eq gdb-buffer-type buffer-type)
                   (or (not thread)
                       (equal gdb-thread-number thread)))
          (throw 'found buffer))))))

(defun gdb-get-buffer-create (buffer-type &optional thread)
  "Create a new GDB buffer of the type specified by BUFFER-TYPE.
The buffer-type should be one of the cars in `gdb-buffer-rules'.

If THREAD is non-nil, it is assigned to `gdb-thread-number'
buffer-local variable of the new buffer.

Buffer mode and name are selected according to buffer type.

If buffer has trigger associated with it in `gdb-buffer-rules',
this trigger is subscribed to `gdb-buf-publisher' and called with
'update argument."
  (or (gdb-get-buffer buffer-type thread)
      (let ((rules (assoc buffer-type gdb-buffer-rules))
            (new (generate-new-buffer "limbo")))
	(with-current-buffer new
	  (let ((mode (gdb-rules-buffer-mode rules))
                (trigger (gdb-rules-update-trigger rules)))
	    (when mode (funcall mode))
	    (setq gdb-buffer-type buffer-type)
            (when thread
              (setq-local gdb-thread-number thread))
            (setq-local gud-minor-mode
                        (buffer-local-value 'gud-minor-mode gud-comint-buffer))
            (setq-local tool-bar-map gud-tool-bar-map)
            (rename-buffer (funcall (gdb-rules-name-maker rules)))
	    (when trigger
              (gdb-add-subscriber gdb-buf-publisher
                                  (cons (current-buffer)
                                        (gdb-bind-function-to-buffer
                                         trigger (current-buffer))))
              (funcall trigger 'start))
            (current-buffer))))))

(defun gdb-bind-function-to-buffer (expr buffer)
  "Return a function which will evaluate EXPR in BUFFER."
  (lambda (&rest args)
    (with-current-buffer buffer
      (apply expr args))))

;; Used to display windows with thread-bound buffers
(defmacro def-gdb-preempt-display-buffer (name buffer &optional doc
					       split-horizontal)
  `(defun ,name (&optional thread)
     ,(when doc doc)
     (message "%s" thread)
     (gdb-preempt-existing-or-display-buffer
      (gdb-get-buffer-create ,buffer thread)
      ,split-horizontal)))

;; This assoc maps buffer type symbols to rules.  Each rule is a list of
;; at least one and possible more functions.  The functions have these
;; roles in defining a buffer type:
;;
;;     NAME - Return a name for this  buffer type.
;;
;; The remaining function(s) are optional:
;;
;;     MODE - called in a new buffer with no arguments, should establish
;;	      the proper mode for the buffer.
;;

(defun gdb-set-buffer-rules (buffer-type &rest rules)
  (let ((binding (assoc buffer-type gdb-buffer-rules)))
    (if binding
	(setcdr binding rules)
      (push (cons buffer-type rules)
	    gdb-buffer-rules))))

(defun gdb-parent-mode ()
  "Generic mode to derive all other GDB buffer modes from."
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  ;; Delete buffer from gdb-buf-publisher when it's killed
  ;; (if it has an associated update trigger)
  (add-hook
   'kill-buffer-hook
   (lambda ()
     (let ((trigger (gdb-rules-update-trigger
                     (gdb-current-buffer-rules))))
       (when trigger
         (gdb-delete-subscriber
          gdb-buf-publisher
          ;; This should match gdb-add-subscriber done in
          ;; gdb-get-buffer-create
          (cons (current-buffer)
                (gdb-bind-function-to-buffer trigger (current-buffer)))))))
   nil t))

;; Partial-output buffer : This accumulates output from a command executed on
;; behalf of emacs (rather than the user).
;;
(gdb-set-buffer-rules 'gdb-partial-output-buffer
		      'gdb-partial-output-name)

(defun gdb-partial-output-name ()
  (concat " *partial-output-"
	  (gdb-get-target-string)
	  "*"))


(gdb-set-buffer-rules 'gdb-inferior-io
		      'gdb-inferior-io-name
		      'gdb-inferior-io-mode)

(defun gdb-inferior-io-name ()
  (concat "*input/output of "
	  (gdb-get-target-string)
	  "*"))

(defun gdb-display-io-buffer ()
  "Display IO of debugged program in a separate window."
  (interactive)
  (gdb-display-buffer (gdb-get-buffer-create 'gdb-inferior-io)))

(defun gdb-inferior-io--init-proc (proc)
  ;; Set up inferior I/O.  Needs GDB 6.4 onwards.
  (set-process-filter proc 'gdb-inferior-filter)
  (set-process-sentinel proc 'gdb-inferior-io-sentinel)
  ;; The process can run on a remote host.
  (let ((tty (or (process-get proc 'remote-tty)
		 (process-tty-name proc))))
    (unless (or (null tty)
		(string= tty ""))
      (gdb-input
       (concat "-inferior-tty-set " tty) 'ignore))))

(defun gdb-inferior-io-sentinel (proc _str)
  (when (eq (process-status proc) 'failed)
    ;; When the debugged process exits, Emacs gets an EIO error on
    ;; read from the pty, and stops listening to it.  If the gdb
    ;; process is still running, remove the pty, make a new one, and
    ;; pass it to gdb.
    (let ((io-buffer (process-buffer proc)))
      (when (and (process-live-p (get-buffer-process gud-comint-buffer))
		 (buffer-live-p io-buffer))
	;; `comint-exec' deletes the original process as a side effect.
	(comint-exec io-buffer "gdb-inferior" nil nil nil)
	(gdb-inferior-io--init-proc (get-buffer-process io-buffer))))))

(defcustom gdb-display-buffer-other-frame-action
  '((display-buffer-reuse-window display-buffer-pop-up-frame)
    (reusable-frames . visible)
    (inhibit-same-window . t)
    (pop-up-frame-parameters (height . 14)
			     (width . 80)
			     (unsplittable . t)
			     (tool-bar-lines . nil)
			     (menu-bar-lines . nil)
			     (minibuffer . nil)))
  "`display-buffer' action for displaying GDB utility frames."
  :group 'gdb
  :type display-buffer--action-custom-type
  :risky t
  :version "24.3")

(defun gdb-frame-io-buffer ()
  "Display IO of debugged program in another frame."
  (interactive)
  (display-buffer (gdb-get-buffer-create 'gdb-inferior-io)
		  gdb-display-buffer-other-frame-action))

(defvar gdb-inferior-io-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'gdb-io-interrupt)
    (define-key map "\C-c\C-z" 'gdb-io-stop)
    (define-key map "\C-c\C-\\" 'gdb-io-quit)
    (define-key map "\C-c\C-d" 'gdb-io-eof)
    (define-key map "\C-d" 'gdb-io-eof)
    map))

;; We want to use comint because it has various nifty and familiar features.
(define-derived-mode gdb-inferior-io-mode comint-mode "Inferior I/O"
  "Major mode for gdb inferior-io."
  :syntax-table nil :abbrev-table nil
  (make-comint-in-buffer "gdb-inferior" (current-buffer) nil))

(defcustom gdb-display-io-nopopup nil
  "When non-nil, and the `gdb-inferior-io' buffer is buried, don't pop it up."
  :type 'boolean
  :group 'gdb
  :version "25.1")

(defun gdb-inferior-filter (proc string)
  (unless (string-equal string "")
    (let (buf)
      (unless (and gdb-display-io-nopopup
                   (setq buf (gdb-get-buffer 'gdb-inferior-io))
                   (null (get-buffer-window buf)))
        (gdb-display-buffer (gdb-get-buffer-create 'gdb-inferior-io)))))
  (with-current-buffer (gdb-get-buffer-create 'gdb-inferior-io)
    (comint-output-filter proc string)))

(defun gdb-io-interrupt ()
  "Interrupt the program being debugged."
  (interactive)
  (interrupt-process
   (get-buffer-process (gdb-get-buffer-create 'gdb-inferior-io)) comint-ptyp))

(defun gdb-io-quit ()
  "Send quit signal to the program being debugged."
  (interactive)
  (quit-process
   (get-buffer-process (gdb-get-buffer-create 'gdb-inferior-io)) comint-ptyp))

(defun gdb-io-stop ()
  "Stop the program being debugged."
  (interactive)
  (stop-process
   (get-buffer-process (gdb-get-buffer-create 'gdb-inferior-io)) comint-ptyp))

(defun gdb-io-eof ()
  "Send end-of-file to the program being debugged."
  (interactive)
  (process-send-eof
   (get-buffer-process (gdb-get-buffer-create 'gdb-inferior-io))))

(defun gdb-clear-inferior-io ()
  (with-current-buffer (gdb-get-buffer-create 'gdb-inferior-io)
    (erase-buffer)))


(defconst breakpoint-xpm-data
  "/* XPM */
static char *magick[] = {
/* columns rows colors chars-per-pixel */
\"10 10 2 1\",
\"  c red\",
\"+ c None\",
/* pixels */
\"+++    +++\",
\"++      ++\",
\"+        +\",
\"          \",
\"          \",
\"          \",
\"          \",
\"+        +\",
\"++      ++\",
\"+++    +++\",
};"
  "XPM data used for breakpoint icon.")

(defconst breakpoint-enabled-pbm-data
  "P1
10 10\",
0 0 0 0 1 1 1 1 0 0 0 0
0 0 0 1 1 1 1 1 1 0 0 0
0 0 1 1 1 1 1 1 1 1 0 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 1 1 1 1 1 1 1 1 1 1 0
0 0 1 1 1 1 1 1 1 1 0 0
0 0 0 1 1 1 1 1 1 0 0 0
0 0 0 0 1 1 1 1 0 0 0 0"
  "PBM data used for enabled breakpoint icon.")

(defconst breakpoint-disabled-pbm-data
  "P1
10 10\",
0 0 1 0 1 0 1 0 0 0
0 1 0 1 0 1 0 1 0 0
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
1 0 1 0 1 0 1 0 1 0
0 1 0 1 0 1 0 1 0 1
0 0 1 0 1 0 1 0 1 0
0 0 0 1 0 1 0 1 0 0"
  "PBM data used for disabled breakpoint icon.")

(defvar breakpoint-enabled-icon nil
  "Icon for enabled breakpoint in display margin.")

(defvar breakpoint-disabled-icon nil
  "Icon for disabled breakpoint in display margin.")

;; Bitmap for breakpoint in fringe
(define-fringe-bitmap 'breakpoint
  "\x3c\x7e\xff\xff\xff\xff\x7e\x3c")
;; Bitmap for gud-overlay-arrow in fringe
(define-fringe-bitmap 'hollow-right-triangle
  "\xe0\x90\x88\x84\x84\x88\x90\xe0")

(defface breakpoint-enabled
  '((t
     :foreground "red1"
     :weight bold))
  "Face for enabled breakpoint icon in fringe."
  :group 'gdb)

(defface breakpoint-disabled
  '((((class color) (min-colors 88)) :foreground "grey70")
    ;; Ensure that on low-color displays that we end up something visible.
    (((class color) (min-colors 8) (background light))
     :foreground "black")
    (((class color) (min-colors 8) (background dark))
     :foreground "white")
    (((type tty) (class mono))
     :inverse-video t)
    (t :background "gray"))
  "Face for disabled breakpoint icon in fringe."
  :group 'gdb)


(defvar gdb-python-guile-commands-regexp
  "python\\|python-interactive\\|pi\\|guile\\|guile-repl\\|gr"
  "Regexp that matches Python and Guile commands supported by GDB.")

(defvar gdb-control-commands-regexp
  (concat
   "^\\("
   "comm\\(a\\(n\\(ds?\\)?\\)?\\)?\\|if\\|while"
   "\\|def\\(i\\(ne?\\)?\\)?\\|doc\\(u\\(m\\(e\\(nt?\\)?\\)?\\)?\\)?\\|"
   gdb-python-guile-commands-regexp
   "\\|while-stepping\\|stepp\\(i\\(ng?\\)?\\)?\\|ws\\|actions"
   "\\|expl\\(o\\(re?\\)?\\)?"
   "\\)\\([[:blank:]]+\\([^[:blank:]]*\\)\\)*$")
  "Regexp matching GDB commands that enter a recursive reading loop.
As long as GDB is in the recursive reading loop, it does not expect
commands to be prefixed by \"-interpreter-exec console\".")

(defun gdb-strip-string-backslash (string)
  (replace-regexp-in-string "\\\\$" "" string))

(defun gdb-send (proc string)
  "A comint send filter for gdb."
  (with-current-buffer gud-comint-buffer
    (let ((inhibit-read-only t))
      (remove-text-properties (point-min) (point-max) '(face nil))))
  ;; mimic <RET> key to repeat previous command in GDB
  (when (= gdb-control-level 0)
    (if (not (string= "" string))
        (if gdb-continuation
            (setq gdb-last-command (concat gdb-continuation
                                           (gdb-strip-string-backslash string)
                                           " "))
          (setq gdb-last-command (gdb-strip-string-backslash string)))
      (if gdb-last-command (setq string gdb-last-command))
      (setq gdb-continuation nil)))
  (if (and (not gdb-continuation)
           (or (string-match "^-" string)
               (> gdb-control-level 0)))
      ;; Either MI command or we are feeding GDB's recursive reading loop.
      (progn
	(setq gdb-first-done-or-error t)
	(process-send-string proc (concat string "\n"))
	(if (and (string-match
                  (concat "^\\("
                          (if (eq system-type 'windows-nt) "\026" "\004")
                          "\\|,q\\|,quit\\|end\\)$")
                  string)
		 (> gdb-control-level 0))
	    (setq gdb-control-level (1- gdb-control-level))))
    ;; CLI command
    (if (string-match "\\\\$" string)
	(setq gdb-continuation
	      (concat gdb-continuation (gdb-strip-string-backslash
					string)
		      " "))
      (setq gdb-first-done-or-error t)
      (let ((to-send (concat "-interpreter-exec console "
                             (gdb-mi-quote (concat gdb-continuation string))
                             "\n")))
        (if gdb-enable-debug
            (push (cons 'mi-send to-send) gdb-debug-log))
        (process-send-string proc to-send))
      (if (and (string-match
                  (concat "^\\("
                          (if (eq system-type 'windows-nt) "\026" "\004")
                          "\\|,q\\|,quit\\|end\\)$")
                  string)
	       (> gdb-control-level 0))
	  (setq gdb-control-level (1- gdb-control-level)))
      (setq gdb-continuation nil)))
  ;; Python and Guile commands that have an argument don't enter the
  ;; recursive reading loop.
  (let* ((control-command-p (string-match gdb-control-commands-regexp string))
         (command-arg (and control-command-p (match-string 3 string)))
         (python-or-guile-p (string-match gdb-python-guile-commands-regexp
                                          string)))
    (if (and control-command-p
             (or (not python-or-guile-p)
                 (null command-arg)
                 (zerop (length command-arg))))
        (setq gdb-control-level (1+ gdb-control-level)))))

(defun gdb-mi-quote (string)
  "Return STRING quoted properly as an MI argument.
The string is enclosed in double quotes.
All embedded quotes, newlines, and backslashes are preceded with a backslash."
  (setq string (replace-regexp-in-string "\\([\"\\]\\)" "\\\\\\&" string))
  (setq string (string-replace "\n" "\\n" string))
  (concat "\"" string "\""))

(defun gdb-input (command handler-function &optional trigger-name)
  "Send COMMAND to GDB via the MI interface.
Run the function HANDLER-FUNCTION, with no arguments, once the command is
complete.  Do not send COMMAND to GDB if TRIGGER-NAME is non-nil and
Emacs is still waiting for a reply from another command previously
sent with the same TRIGGER-NAME."
  (when (or (not trigger-name)
            (not (gdb-pending-handler-p trigger-name)))
    (setq gdb-token-number (1+ gdb-token-number))
    (setq command (concat (number-to-string gdb-token-number) command))

    (if gdb-enable-debug (push (list 'send-item command handler-function)
                               gdb-debug-log))

    (gdb-add-handler gdb-token-number handler-function trigger-name)

    (if gdbmi-debug-mode (message "gdb-input: %s" command))
    (process-send-string (get-buffer-process gud-comint-buffer)
                         (concat command "\n"))))

;; NOFRAME is used for gud execution control commands
(defun gdb-current-context-command (command)
  "Add --thread to gdb COMMAND when needed."
  (if (and gdb-thread-number
	   gdb-supports-non-stop)
      (concat command " --thread " gdb-thread-number)
    command))

(defun gdb-current-context-buffer-name (name)
  "Add thread information and asterisks to string NAME.

If `gdb-thread-number' is nil, just wrap NAME in asterisks."
  (concat "*" name
          (if (local-variable-p 'gdb-thread-number)
              (format " (bound to thread %s)" gdb-thread-number)
            "")
          "*"))

(defun gdb-current-context-mode-name (mode)
  "Add thread information to MODE which is to be used as `mode-name'."
  (concat mode
          (if gdb-thread-number
              (format " [thread %s]" gdb-thread-number)
            "")))


(defcustom gud-gdb-command-name "gdb -i=mi"
  "Default command to execute an executable under the GDB debugger."
  :type 'string
  :group 'gdb)

(defun gdb-resync()
  (setq gud-running nil)
  (setq gdb-output-sink 'user)
  (gdb-remove-all-pending-triggers))

(defun gdb-update (&optional no-proc)
  "Update buffers showing status of debug session.
If NO-PROC is non-nil, do not try to contact the GDB process."
  (when gdb-first-prompt
    (gdb-force-mode-line-update
     (propertize "initializing..." 'face font-lock-variable-name-face))
    (gdb-init-1)
    (setq gdb-first-prompt nil))

  (unless no-proc
    (gdb-get-main-selected-frame))

  ;; We may need to update gdb-threads-list so we can use
  (gdb-get-buffer-create 'gdb-threads-buffer)
  ;; gdb-break-list is maintained in breakpoints handler
  (gdb-get-buffer-create 'gdb-breakpoints-buffer)

  (gdb-get-changed-registers)
  (unless no-proc
    (gdb-emit-signal gdb-buf-publisher 'update))

  (when (and (boundp 'speedbar-frame) (frame-live-p speedbar-frame))
    (dolist (var gdb-var-list)
      (setcar (nthcdr 5 var) nil))
    (gdb-var-update)))

;; gdb-setq-thread-number and gdb-update-gud-running are decoupled
;; because we may need to update current gud-running value without
;; changing current thread (see gdb-running)
(defun gdb-setq-thread-number (number)
  "Set `gdb-thread-number' to NUMBER.
Only this function must be used to change `gdb-thread-number'
value to NUMBER, because `gud-running' and `gdb-frame-number'
need to be updated appropriately when current thread changes."
  ;; GDB 6.8 and earlier always output thread-id="0" when stopping.
  (unless (string-equal number "0") (setq gdb-thread-number number))
  (setq gdb-frame-number "0")
  (gdb-update-gud-running))

(defun gdb-update-gud-running ()
  "Set `gud-running' according to the state of current thread.

`gdb-frame-number' is set to 0 if current thread is now stopped.

Note that when `gdb-gud-control-all-threads' is t, `gud-running'
cannot be reliably used to determine whether or not execution
control buttons should be shown in menu or toolbar.  Use
`gdb-running-threads-count' and `gdb-stopped-threads-count'
instead.

For all-stop mode, thread information is unavailable while target
is running."
  (let ((old-value gud-running))
    (setq gud-running
          (string= (gdb-mi--field (gdb-current-buffer-thread) 'state)
                   "running"))
    ;; Set frame number to "0" when _current_ threads stops.
    (when (and (gdb-current-buffer-thread)
               (not (eq gud-running old-value)))
      (setq gdb-frame-number "0"))))

(defun gdb-show-run-p ()
  "Return t if \"Run/continue\" should be shown on the toolbar."
  (or (not gdb-active-process)
      (and (or
            (not gdb-gud-control-all-threads)
            (not gdb-non-stop))
           (not gud-running))
      (and gdb-gud-control-all-threads
           (> gdb-stopped-threads-count 0))))

(defun gdb-show-stop-p ()
  "Return t if \"Stop\" should be shown on the toolbar."
  (or (and (or
            (not gdb-gud-control-all-threads)
            (not gdb-non-stop))
           gud-running)
      (and gdb-gud-control-all-threads
           (not (null gdb-running-threads-count))
           (> gdb-running-threads-count 0))))

;; GUD displays the selected GDB frame.  This might might not be the current
;; GDB frame (after up, down etc).  If no GDB frame is visible but the last
;; visited breakpoint is, use that window.
(defun gdb-display-source-buffer (buffer)
  "Find a window to display BUFFER.
Always find a window to display buffer, and return it."
  ;; This function doesn't take care of setting up source window(s) at startup,
  ;; that's handled by `gdb-setup-windows' (if `gdb-many-windows' is non-nil).
  ;; If `buffer' is already shown in a window, use that window.
  (or (get-buffer-window buffer)
      (progn
        ;; First, update the window list.
        (setq gdb-source-window-list
              (cl-remove-duplicates
               (cl-remove-if-not
                (lambda (win)
                  (and (window-live-p win)
                       (eq (window-frame win)
                           (selected-frame))))
                gdb-source-window-list)))
        ;; Should we create a new window or reuse one?
        (if (> gdb-max-source-window-count
               (length gdb-source-window-list))
            ;; Create a new window, push it to window list and return it.
            (car (push (display-buffer buffer gdb-display-source-buffer-action)
                       gdb-source-window-list))
          ;; Reuse a window, we use the oldest window and put that to
          ;; the front of the window list.
          (let ((last-win (car (last gdb-source-window-list)))
                (rest (butlast gdb-source-window-list)))
            (set-window-buffer last-win buffer)
            (setq gdb-source-window-list
                  (cons last-win rest))
            last-win)))))


(defun gdbmi-start-with (str offset match)
  "Return non-nil if string STR starts with MATCH, else returns nil.
OFFSET is the position in STR at which the comparison takes place."
  (let ((match-length (length match))
	(str-length (- (length str) offset)))
    (when (>= str-length match-length)
      (string-equal match (substring str offset (+ offset match-length))))))

(defun gdbmi-same-start (str offset match)
  "Return non-nil if STR and MATCH are equal up to the end of either strings.
OFFSET is the position in STR at which the comparison takes place."
  (let* ((str-length (- (length str) offset))
	 (match-length (length match))
	 (compare-length (min str-length match-length)))
    (when (> compare-length 0)
      (string-equal (substring str offset (+ offset compare-length))
		    (substring match 0 compare-length)))))

(defun gdbmi-is-number (character)
  "Return non-nil if CHARACTER is a numerical character between 0 and 9."
  (and (>= character ?0)
       (<= character ?9)))


(defvar-local gdbmi-bnf-state 'gdbmi-bnf-output
  "Current GDB/MI output parser state.
The parser is placed in a different state when an incomplete data steam is
received from GDB.
This variable will preserve the state required to resume the parsing
when more data arrives.")

(defvar-local gdbmi-bnf-offset 0
  "Offset in `gud-marker-acc' at which the parser is reading.
This offset is used to be able to parse the GDB/MI message
in-place, without the need of copying the string in a temporary buffer
or discarding parsed tokens by substringing the message.")

(defun gdbmi-bnf-init ()
  "Initialize the GDB/MI message parser."
  (setq gdbmi-bnf-state 'gdbmi-bnf-output)
  (setq gdbmi-bnf-offset 0)
  (setq gud-marker-acc ""))


(defun gdbmi-bnf-output ()
  "Implementation of the following GDB/MI output grammar rule:

  output ==>
       ( out-of-band-record )* [ result-record ] gdb-prompt"

  (gdbmi-bnf-skip-unrecognized)
  (while (gdbmi-bnf-out-of-band-record))
  (gdbmi-bnf-result-record)
  (gdbmi-bnf-gdb-prompt))


(defun gdbmi-bnf-skip-unrecognized ()
  "Skip characters until is encounters the beginning of a valid record.
Used as a protection mechanism in case something goes wrong when parsing
a GDB/MI reply message."
  (let ((acc-length (length gud-marker-acc))
	(prefix-offset gdbmi-bnf-offset)
	(prompt "(gdb) \n"))

    (while (and (< prefix-offset acc-length)
                (gdbmi-is-number (aref gud-marker-acc prefix-offset)))
      (setq prefix-offset (1+ prefix-offset)))

    (if (and (< prefix-offset acc-length)
             (not (memq (aref gud-marker-acc prefix-offset)
                        '(?^ ?* ?+ ?= ?~ ?@ ?&)))
             (not (gdbmi-same-start gud-marker-acc gdbmi-bnf-offset prompt))
             (string-match "\\([^^*+=~@&]+\\)" gud-marker-acc
                           gdbmi-bnf-offset))
        (let ((unrecognized-str (match-string 0 gud-marker-acc)))
          (setq gdbmi-bnf-offset (match-end 0))
	  (if gdbmi-debug-mode
              (message "gdbmi-bnf-skip-unrecognized: %s" unrecognized-str))
          (gdb-shell unrecognized-str)
	  t))))


(defun gdbmi-bnf-gdb-prompt ()
  "Implementation of the following GDB/MI output grammar rule:
  gdb-prompt ==>
       `(gdb)' nl

  nl ==>
       CR | CR-LF"

  (let ((prompt "(gdb) \n"))
    (when (gdbmi-start-with gud-marker-acc gdbmi-bnf-offset prompt)
      (if gdbmi-debug-mode (message "gdbmi-bnf-gdb-prompt: %s" prompt))
      (gdb-gdb prompt)
      (setq gdbmi-bnf-offset (+ gdbmi-bnf-offset (length prompt)))

      ;; Returns non-nil to tell gud-gdbmi-marker-filter we've reached
      ;; the end of a GDB reply message.
      t)))


(defun gdbmi-bnf-result-record ()
  "Implementation of the following GDB/MI output grammar rule:

  result-record ==>
       [ token ] `^' result-class ( `,' result )* nl

  token ==>
       any sequence of digits."

  (gdbmi-bnf-result-and-async-record-impl))


(defun gdbmi-bnf-out-of-band-record ()
  "Implementation of the following GDB/MI output grammar rule:

  out-of-band-record ==>
       async-record | stream-record"

  (or (gdbmi-bnf-async-record)
      (gdbmi-bnf-stream-record)))


(defun gdbmi-bnf-async-record ()
  "Implementation of the following GDB/MI output grammar rules:

  async-record ==>
       exec-async-output | status-async-output | notify-async-output

  exec-async-output ==>
       [ token ] `*' async-output

  status-async-output ==>
       [ token ] `+' async-output

  notify-async-output ==>
       [ token ] `=' async-output

  async-output ==>
       async-class ( `,' result )* nl"

  (gdbmi-bnf-result-and-async-record-impl))


(defun gdbmi-bnf-stream-record ()
  "Implement the following GDB/MI output grammar rule:
  stream-record ==>
       console-stream-output | target-stream-output | log-stream-output

  console-stream-output ==>
       `~' c-string

  target-stream-output ==>
       `@' c-string

  log-stream-output ==>
       `&' c-string"
  (when (< gdbmi-bnf-offset (length gud-marker-acc))
    (if (and (member (aref gud-marker-acc gdbmi-bnf-offset) '(?~ ?@ ?&))
             (string-match (concat "\\([~@&]\\)\\(" gdb--string-regexp "\\)\n")
                           gud-marker-acc
                           gdbmi-bnf-offset))
        (let ((prefix (match-string 1 gud-marker-acc))
              (c-string (match-string 2 gud-marker-acc)))

          (setq gdbmi-bnf-offset (match-end 0))
          (if gdbmi-debug-mode (message "gdbmi-bnf-stream-record: %s"
                                        (match-string 0 gud-marker-acc)))

          (cond ((string-equal prefix "~")
                 (gdbmi-bnf-console-stream-output c-string))
                ((string-equal prefix "@")
                 (gdbmi-bnf-target-stream-output c-string))
                ((string-equal prefix "&")
                 (gdbmi-bnf-log-stream-output c-string)))
	  t))))

(defun gdbmi-bnf-console-stream-output (c-string)
  "Handler for the console-stream-output GDB/MI output grammar rule."
  (gdb-console c-string)
  ;; We've written to the GUD console, so we should print the prompt
  ;; after the next result-class or async-class.
  (setq gdb-first-done-or-error t))

(defun gdbmi-bnf-target-stream-output (_c-string)
  "Handler for the target-stream-output GDB/MI output grammar rule."
  ;; Not currently used.
  )

(defun gdbmi-bnf-log-stream-output (c-string)
  "Handler for the log-stream-output GDB/MI output grammar rule."
  ;; Suppress "No registers."  GDB 6.8 and earlier
  ;; duplicates MI error message on internal stream.
  ;; Don't print to GUD buffer.
  (if (not (string-equal (gdb-mi--c-string-from-string c-string)
                         "No registers.\n"))
      (gdb-internals c-string)))


(defconst gdbmi-bnf-result-state-configs
  '(("^" . (("done" . (gdb-done . progressive))
            ("error" . (gdb-error . progressive))
            ("running" . (gdb-starting . atomic))))
    ("*" . (("stopped" . (gdb-stopped . atomic))
            ("running" . (gdb-running . atomic))))
    ("+" . ())
    ("=" . (("thread-created" . (gdb-thread-created . atomic))
            ("thread-selected" . (gdb-thread-selected . atomic))
            ("thread-existed" . (gdb-ignored-notification . atomic))
            ('default . (gdb-ignored-notification . atomic)))))
  "Alist of alists, mapping the type and class of message to a handler function.
Handler functions are all flagged as either `progressive' or `atomic'.
`progressive' handlers are capable of parsing incomplete messages.
They can be called several time with new data chunk as they arrive from GDB.
`progressive' handlers must have an extra argument that is set to a non-nil
value when the message is complete.

Implement the following GDB/MI output grammar rule:
  result-class ==>
       `done' | `running' | `connected' | `error' | `exit'

  async-class ==>
       `stopped' | others (where others will be added depending on the needs
                           --this is still in development).")

(defun gdbmi-bnf-result-and-async-record-impl ()
  "Common implementation of the result-record and async-record rule.
Both rules share the same syntax.  Those records may be very large in size.
For that reason, the \"result\" part of the  record is parsed by
`gdbmi-bnf-incomplete-record-result', which will keep
receiving characters as they arrive from GDB until the record is complete."
  (let ((acc-length (length gud-marker-acc))
	(prefix-offset gdbmi-bnf-offset))

    (while (and (< prefix-offset acc-length)
                (gdbmi-is-number (aref gud-marker-acc prefix-offset)))
      (setq prefix-offset (1+ prefix-offset)))

    (if (and (< prefix-offset acc-length)
             (member (aref gud-marker-acc prefix-offset) '(?* ?+ ?= ?^))
             (string-match "\\([0-9]*\\)\\([*+=^]\\)\\(.+?\\)\\([,\n]\\)"
                           gud-marker-acc gdbmi-bnf-offset))

        (let ((token (match-string 1 gud-marker-acc))
	      (prefix (match-string 2 gud-marker-acc))
	      (class (match-string 3 gud-marker-acc))
	      (complete (string-equal (match-string 4 gud-marker-acc) "\n"))
	      class-alist
	      class-command)

          (setq gdbmi-bnf-offset (match-end 0))
	  (if gdbmi-debug-mode (message "gdbmi-bnf-result-record: %s"
                                        (match-string 0 gud-marker-acc)))

          (setq class-alist
                (cdr (assoc prefix gdbmi-bnf-result-state-configs)))
          (setq class-command (cdr (assoc class class-alist)))
          (if (null class-command)
              (setq class-command (cdr (assoc 'default class-alist))))

          (if complete
              (if class-command
                  (if (equal (cdr class-command) 'progressive)
                      (funcall (car class-command) token "" complete)
                    (funcall (car class-command) token "")))
            (setq gdbmi-bnf-state
                  (lambda ()
                    (gdbmi-bnf-incomplete-record-result token class-command)))
            (funcall gdbmi-bnf-state))
	  t))))

(defun gdbmi-bnf-incomplete-record-result (token class-command)
  "State of the parser used to progressively parse a result-record or async-record
rule from an incomplete data stream.  The parser will stay in this state until
the end of the current result or async record is reached."
  (when (< gdbmi-bnf-offset (length gud-marker-acc))
    ;; Search the data stream for the end of the current record:
    (let* ((newline-pos (string-search "\n" gud-marker-acc gdbmi-bnf-offset))
	   (is-progressive (equal (cdr class-command) 'progressive))
       (is-complete (not (null newline-pos)))
       result-str)

      (when gdbmi-debug-mode
        (message "gdbmi-bnf-incomplete-record-result: %s"
                 (substring gud-marker-acc gdbmi-bnf-offset newline-pos)))

      ;; Update the gdbmi-bnf-offset only if the current chunk of data can
      ;; be processed by the class-command handler:
      (when (or is-complete is-progressive)
        (setq result-str
              (substring gud-marker-acc gdbmi-bnf-offset newline-pos))

        ;; Move gdbmi-bnf-offset past the end of the chunk.
        (setq gdbmi-bnf-offset (+ gdbmi-bnf-offset (length result-str)))
        (when newline-pos
          (setq gdbmi-bnf-offset (1+ gdbmi-bnf-offset))))

      ;; Update the parsing state before invoking the handler in class-command
      ;; to make sure it's not left in an invalid state if the handler was
      ;; to generate an error.
      (if is-complete
	  (setq gdbmi-bnf-state 'gdbmi-bnf-output))

      (if class-command
	  (if is-progressive
	      (funcall (car class-command) token result-str is-complete)
	    (if is-complete
		(funcall (car class-command) token result-str))))

      (unless is-complete
        ;; Incomplete gdb response: abort parsing until we receive more data.
        (if gdbmi-debug-mode (message "gdbmi-bnf-incomplete-record-result, aborting: incomplete stream"))
        (throw 'gdbmi-incomplete-stream nil))

      is-complete)))


; The following grammar rules are not parsed directly by this GDBMI-BNF parser.
; The handling of those rules is currently done by the handlers registered
; in gdbmi-bnf-result-state-configs
;
; result ==>
;      variable "=" value
;
; variable ==>
;      string
;
; value ==>
;      const | tuple | list
;
; const ==>
;      c-string
;
; tuple ==>
;      "{}" | "{" result ( "," result )* "}"
;
; list ==>
;      "[]" | "[" value ( "," value )* "]" | "[" result ( "," result )* "]"

;; FIXME: This is fragile: it relies on the assumption that all the
;; non-ASCII strings output by GDB, including names of the source
;; files, values of string variables in the inferior, etc., are all
;; encoded in the same encoding.

(defcustom gdb-mi-decode-strings t
  "When non-nil, decode octal escapes in GDB output into non-ASCII text.

If the value is a coding-system, use that coding-system to decode
the bytes reconstructed from octal escapes.  Any other non-nil value
means to decode using the coding-system set for the GDB process."
  :type '(choice
          (const :tag "Don't decode" nil)
          (const :tag "Decode using default coding-system" t)
          (coding-system :tag "Decode using this coding-system"))
  :group 'gdb
  :version "25.1")

(defun gud-gdbmi-marker-filter (string)
  "Filter GDB/MI output."

  ;; Record transactions if logging is enabled.
  (when gdb-enable-debug
    (push (cons 'recv string) gdb-debug-log)
    (if (and gdb-debug-log-max
	     (> (length gdb-debug-log) gdb-debug-log-max))
	(setcdr (nthcdr (1- gdb-debug-log-max) gdb-debug-log) nil)))

  ;; Recall the left over gud-marker-acc from last time.
  (setq gud-marker-acc (concat gud-marker-acc string))

  ;; Start accumulating output for the GUD buffer.
  (setq gdb-filter-output "")

  (let ((acc-length (length gud-marker-acc)))
    (catch 'gdbmi-incomplete-stream
      (while (and (< gdbmi-bnf-offset acc-length)
		  (funcall gdbmi-bnf-state)))))

  (when (/= gdbmi-bnf-offset 0)
    (setq gud-marker-acc (substring gud-marker-acc gdbmi-bnf-offset))
    (setq gdbmi-bnf-offset 0))

  (when (and gdbmi-debug-mode (> (length gud-marker-acc) 0))
    (message "gud-gdbmi-marker-filter, unparsed string: %s" gud-marker-acc))

  gdb-filter-output)

(defun gdb-gdb (_output-field)
  ;; This is needed because the "explore" command is not ended by the
  ;; likes of "end" or "quit", but instead by a RET at the appropriate
  ;; place, and we know we have exited "explore" when we get the
  ;; "(gdb)" prompt.
  (and (> gdb-control-level 0)
       (setq gdb-control-level (1- gdb-control-level))))

(defun gdb-shell (output-field)
  (setq gdb-filter-output
        (concat output-field gdb-filter-output)))

(defun gdb-ignored-notification (_token _output-field))

;; gdb-invalidate-threads is defined to accept 'update-threads signal
(defun gdb-thread-created (_token _output-field))
(defun gdb-thread-exited (_token output-field)
  "Handle =thread-exited async record.
Unset `gdb-thread-number' if current thread exited and update threads list."
  (let* ((thread-id (gdb-mi--field (gdb-mi--from-string output-field) 'id)))
    (if (string= gdb-thread-number thread-id)
        (gdb-setq-thread-number nil))
    ;; When we continue current thread and it quickly exits,
    ;; the pending triggers in gdb-handler-list left after gdb-running
    ;; disallow us to properly call -thread-info without --thread option.
    ;; Thus we need to use gdb-wait-for-pending.
    (gdb-wait-for-pending
     (lambda () (gdb-emit-signal gdb-buf-publisher 'update-threads)))))

(defun gdb-thread-selected (_token output-field)
  "Handler for =thread-selected MI output record.

Sets `gdb-thread-number' to new id."
  (let* ((result (gdb-mi--from-string output-field))
         (thread-id (gdb-mi--field result 'id)))
    (gdb-setq-thread-number thread-id)
    ;; Typing `thread N' in GUD buffer makes GDB emit `^done' followed
    ;; by `=thread-selected' notification. `^done' causes `gdb-update'
    ;; as usually. Things happen too fast and second call (from
    ;; gdb-thread-selected handler) gets cut off by our beloved
    ;; pending triggers.
    ;; Solution is `gdb-wait-for-pending': it guarantees that its
    ;; argument will get called when `gdb-handler-list' if free of
    ;; pending triggers.
    (gdb-wait-for-pending #'gdb-update)))

(defun gdb-running (_token output-field)
  (let* ((thread-id
          (gdb-mi--field (gdb-mi--from-string output-field) 'thread-id)))
    ;; We reset gdb-frame-number to nil if current thread has gone
    ;; running. This can't be done in gdb-thread-list-handler-custom
    ;; because we need correct gdb-frame-number by the time
    ;; -thread-info command is sent.
    (when (or (string-equal thread-id "all")
              (string-equal thread-id gdb-thread-number))
      (setq gdb-frame-number nil)))
  (setq gdb-inferior-status "running")
  (gdb-force-mode-line-update
   (propertize gdb-inferior-status 'face font-lock-type-face))
  (when (not gdb-non-stop)
    (setq gud-running t))
  (setq gdb-active-process t))

(defun gdb-starting (_output-field _result)
  ;; CLI commands don't emit ^running at the moment so use gdb-running too.
  (setq gdb-inferior-status "running")
  (gdb-force-mode-line-update
   (propertize gdb-inferior-status 'face font-lock-type-face))
  (setq gdb-active-process t)
  (setq gud-running t))

;; -break-insert -t didn't give a reason before gdb 6.9

(defun gdb-stopped (_token output-field)
  "Given the contents of *stopped MI async record, select new
current thread and update GDB buffers."
  ;; Reason is available with target-async only
  (let* ((result (gdb-mi--from-string output-field))
         (reason (gdb-mi--field result 'reason))
         (thread-id (gdb-mi--field result 'thread-id))
         (retval (gdb-mi--field result 'return-value))
         (varnum (gdb-mi--field result 'gdb-result-var)))

    ;; -data-list-register-names needs to be issued for any stopped
    ;; thread
    (when (not gdb-register-names)
      (gdb-input (concat "-data-list-register-names"
			 (if gdb-supports-non-stop
			     (concat " --thread " thread-id)))
		 'gdb-register-names-handler))

    ;; Don't set gud-last-frame here as it's currently done in
    ;; gdb-frame-handler because synchronous GDB doesn't give these fields
    ;; with CLI.
    ;;(when file
    ;;  (setq
    ;;   ;; Extract the frame position from the marker.
    ;;   gud-last-frame (cons file
    ;;    		    (string-to-number
    ;;    		     (match-string 6 gud-marker-acc)))))

    (setq gdb-inferior-status (or reason "unknown"))
    (gdb-force-mode-line-update
     (propertize gdb-inferior-status 'face font-lock-warning-face))
    (if (string-equal reason "exited-normally")
	(setq gdb-active-process nil))

    (when (and retval varnum
               ;; When the user typed CLI commands, GDB/MI helpfully
               ;; includes the "Value returned" response in the "~"
               ;; record; here we avoid displaying it twice.
               (not (string-match "^Value returned is " gdb-filter-output)))
      (setq gdb-filter-output
            (concat gdb-filter-output
                    (format "Value returned is %s = %s\n" varnum retval))))

    ;; Select new current thread.

    ;; Don't switch if we have no reasons selected
    (when gdb-switch-reasons
      ;; Switch from another stopped thread only if we have
      ;; gdb-switch-when-another-stopped:
      (when (or gdb-switch-when-another-stopped
                (not (string= "stopped"
                              (gdb-mi--field (gdb-current-buffer-thread) 'state))))
        ;; Switch if current reason has been selected or we have no
        ;; reasons
        (if (or (eq gdb-switch-reasons t)
                (member reason gdb-switch-reasons))
            (when (not (string-equal gdb-thread-number thread-id))
              (message "Switched to thread %s" thread-id)
              (gdb-setq-thread-number thread-id))
          (message "Thread %s stopped" thread-id))))

    ;; Print "(gdb)" to GUD console
    (when gdb-first-done-or-error
      (setq gdb-filter-output (concat gdb-filter-output gdb-prompt-name)))

    ;; In non-stop, we update information as soon as another thread gets
    ;; stopped
    (when (or gdb-first-done-or-error
              gdb-non-stop)
      ;; In all-stop this updates gud-running properly as well.
      (gdb-update)
      (setq gdb-first-done-or-error nil))
    (run-hook-with-args 'gdb-stopped-functions result)))

;; Remove the trimmings from log stream containing debugging messages
;; being produced by GDB's internals, use warning face and send to GUD
;; buffer.
(defun gdb-internals (output-field)
  (setq gdb-filter-output
	(gdb-concat-output
	 gdb-filter-output
	 (if (string= output-field "\"\\n\"")
	     ""
	   (let ((error-message
		  (gdb-mi--c-string-from-string output-field)))
	     (put-text-property
	      0 (length error-message)
	      'face font-lock-warning-face
	      error-message)
	     error-message)))))

;; Remove the trimmings from the console stream and send to GUD buffer
;; (frontend MI commands should not print to this stream)
(defun gdb-console (output-field)
  (setq gdb-filter-output
	(gdb-concat-output gdb-filter-output
                           (gdb-mi--c-string-from-string output-field))))

(defun gdb-done (token-number output-field is-complete)
  (gdb-done-or-error token-number 'done output-field is-complete))

(defun gdb-error (token-number output-field is-complete)
  (gdb-done-or-error token-number 'error output-field is-complete))

(defun gdb-done-or-error (token-number type output-field is-complete)
  (if (string-equal token-number "")
      ;; Output from command entered by user
      (progn
	(setq gdb-output-sink 'user)
	(setq token-number nil)
	;; MI error - send to minibuffer
	(when (eq type 'error)
          ;; Skip "msg=" from `output-field'
          (message "%s" (gdb-mi--c-string-from-string
                         (substring output-field 4)))
          ;; Don't send to the console twice.  (If it is a console error
          ;; it is also in the console stream.)
          (setq output-field nil)))
    ;; Output from command from frontend.
    (setq gdb-output-sink 'emacs))

  ;; The process may already be dead (e.g. C-d at the gdb prompt).
  (let* ((proc (get-buffer-process gud-comint-buffer))
	 (no-proc (or (null proc)
		      (memq (process-status proc) '(exit signal)))))

    (when (and is-complete gdb-first-done-or-error)
      (unless (or token-number gud-running no-proc)
	(setq gdb-filter-output (concat gdb-filter-output gdb-prompt-name)))
      (gdb-update no-proc)
      (setq gdb-first-done-or-error nil))

    (setq gdb-filter-output
	  (gdb-concat-output gdb-filter-output output-field))

    ;; We are done concatenating to the output sink.  Restore it to user sink:
    (setq gdb-output-sink 'user)

    (when (and token-number is-complete)
      (with-current-buffer
	  (gdb-get-buffer-create 'gdb-partial-output-buffer)
	(gdb-handle-reply (string-to-number token-number))))

  (when is-complete
    (gdb-clear-partial-output))))

(defun gdb-concat-output (so-far new)
  (cond
   ((eq gdb-output-sink 'user) (concat so-far new))
   ((eq gdb-output-sink 'emacs)
    (gdb-append-to-partial-output new)
    so-far)))

(defun gdb-append-to-partial-output (string)
  (with-current-buffer (gdb-get-buffer-create 'gdb-partial-output-buffer)
    (goto-char (point-max))
    (insert string)))

(defun gdb-clear-partial-output ()
  (with-current-buffer (gdb-get-buffer-create 'gdb-partial-output-buffer)
    (erase-buffer)))

;; Parse GDB/MI result records: this process converts
;;  list      [...]      ->  list
;;  tuple     {...}      ->  list
;;  result    KEY=VALUE  ->  (KEY . VALUE) where KEY is a symbol
;;  c-string  "..."      ->  string

(defun gdb-mi--parse-tuple-or-list (end-char)
  "Parse a tuple or list, either returned as a Lisp list.
END-CHAR is the ending delimiter; will stop at end-of-buffer otherwise."
  (let ((items nil))
    (while (not (or (eobp)
                    (eq (following-char) end-char)))
      (let ((item (gdb-mi--parse-result-or-value)))
        (push item items)
        (when (eq (following-char) ?,)
          (forward-char))))
    (when (eq (following-char) end-char)
      (forward-char))
    (nreverse items)))

(defun gdb-mi--parse-c-string ()
  "Parse a c-string."
  (let ((start (point))
        (pieces nil)
        (octals-used nil))
    (while (and (re-search-forward (rx (or ?\\ ?\")))
                (not (eq (preceding-char) ?\")))
      (push (buffer-substring start (1- (point))) pieces)
      (cond
       ((looking-at (rx (any "0-7") (? (any "0-7") (? (any "0-7")))))
        (push (unibyte-string (string-to-number (match-string 0) 8)) pieces)
        (setq octals-used t)
        (goto-char (match-end 0)))
       ((looking-at (rx (any "ntrvfab\"\\")))
        (push (cdr (assq (following-char)
                         '((?n . "\n")
                           (?t . "\t")
                           (?r . "\r")
                           (?v . "\v")
                           (?f . "\f")
                           (?a . "\a")
                           (?b . "\b")
                           (?\" . "\"")
                           (?\\ . "\\"))))
              pieces)
        (forward-char))
       (t
        (warn "Unrecognised escape char: %c" (following-char))))
      (setq start (point)))
    (push (buffer-substring start (1- (point))) pieces)
    (let ((s (apply #'concat (nreverse pieces))))
      (if (and octals-used gdb-mi-decode-strings)
          (let ((coding
                 (if (coding-system-p gdb-mi-decode-strings)
                     gdb-mi-decode-strings
                   (buffer-local-value
                    'buffer-file-coding-system
                    ;; FIXME: This is somewhat expensive.
                    (gdb-get-buffer-create 'gdb-partial-output-buffer)))))
            (decode-coding-string s coding))
        s))))

;; FIXME: Ideally this function should not be needed.
(defun gdb-mi--c-string-from-string (string)
  "Parse a c-string from (the beginning of) STRING."
  (with-temp-buffer
    (insert string)
    (goto-char (1+ (point-min)))        ; Skip leading double quote.
    (gdb-mi--parse-c-string)))

(defun gdb-mi--parse-value ()
  "Parse a value."
  (cond
   ((eq (following-char) ?\{)
    (forward-char)
    (gdb-mi--parse-tuple-or-list ?\}))
   ((eq (following-char) ?\[)
    (forward-char)
    (gdb-mi--parse-tuple-or-list ?\]))
   ((eq (following-char) ?\")
    (forward-char)
    (gdb-mi--parse-c-string))
   (t (error "Bad start of result or value: %c" (following-char)))))

(defun gdb-mi--parse-result-or-value ()
  "Parse a result (key=value) or value."
  (if (looking-at (rx (group (+ (any "a-zA-Z" ?_ ?-))) "="))
      (progn
        (goto-char (match-end 0))
        (let* ((variable (intern (match-string 1)))
               (value (gdb-mi--parse-value)))
          (cons variable value)))
    (gdb-mi--parse-value)))

(defun gdb-mi--parse-results ()
  "Parse zero or more result productions as a list."
  (gdb-mi--parse-tuple-or-list nil))

(defun gdb-mi--fix-key (key value)
  "Convert any result (key-value pair) in VALUE whose key is KEY to its value."
  (cond
   ((atom value) value)
   ((symbolp (car value))
    (if (eq (car value) key)
        (cdr value)
      (cons (car value) (gdb-mi--fix-key key (cdr value)))))
   (t (mapcar (lambda (x) (gdb-mi--fix-key key x)) value))))

(defun gdb-mi--extend-fullname (remote value)
  "Prepend REMOTE to any result string with `fullname' as the key in VALUE."
  (cond
   ((atom value) value)
   ((symbolp (car value))
    (if (and (eq (car value) 'fullname)
             (stringp (cdr value)))
        (cons 'fullname (concat remote (cdr value)))
      (cons (car value) (gdb-mi--extend-fullname remote (cdr value)))))
   (t (mapcar (lambda (x) (gdb-mi--extend-fullname remote x)) value))))

(defun gdb-mi--read-buffer (fix-key)
  "Parse the current buffer as a list of result productions.
If FIX-KEY is a non-nil symbol, convert all FIX-KEY=VALUE results into VALUE.
This is used to get rid of useless keys in lists in MI messages;
eg, [key=.., key=..].  -stack-list-frames and -break-info are
examples of MI commands which issue such responses."
  (goto-char (point-min))
  (let ((results (gdb-mi--parse-results)))
    (let ((remote (file-remote-p default-directory)))
      (when remote
        (setq results (gdb-mi--extend-fullname remote results))))
    (when fix-key
      (setq results (gdb-mi--fix-key fix-key results)))
    results))

(defun gdb-mi--from-string (string &optional fix-key)
  "Prepare and parse STRING containing GDB/MI output.

FIX-KEY works as in `gdb-mi--read-buffer'."
  (with-temp-buffer
    (insert string)
    (gdb-mi--read-buffer fix-key)))

(defun gdb-mi--partial-output (&optional fix-key)
  "Prepare and parse gdb-partial-output-buffer.

FIX-KEY works as in `gdb-mi--read-buffer'."
  (with-current-buffer (gdb-get-buffer-create 'gdb-partial-output-buffer)
    (gdb-mi--read-buffer fix-key)))

(defun gdb-line-posns (line)
  "Return a pair of LINE beginning and end positions."
  (let ((offset (1+ (- line (line-number-at-pos)))))
    (cons
     (line-beginning-position offset)
     (line-end-position offset))))

(defmacro gdb-mark-line (line variable)
  "Set VARIABLE marker to point at beginning of LINE.

If current window has no fringes, inverse colors on LINE.

Return position where LINE begins."
  `(save-excursion
     (let* ((posns (gdb-line-posns ,line))
            (start-posn (car posns))
            (end-posn (cdr posns)))
       (set-marker ,variable (copy-marker start-posn))
       (when (not (> (car (window-fringes)) 0))
         (put-text-property start-posn end-posn
                            'font-lock-face '(:inverse-video t)))
       start-posn)))

(defun gdb-pad-string (string padding)
  (format (concat "%" (number-to-string padding) "s") string))

;; gdb-table struct is a way to programmatically construct simple
;; tables. It help to reliably align columns of data in GDB buffers
;; and provides
(cl-defstruct gdb-table
  (column-sizes nil)
  (rows nil)
  (row-properties nil)
  (right-align nil))

(defun gdb-table-add-row (table row &optional properties)
  "Add ROW of string to TABLE and recalculate column sizes.

When non-nil, PROPERTIES will be added to the whole row when
calling `gdb-table-string'."
  (let ((rows (gdb-table-rows table))
        (row-properties (gdb-table-row-properties table))
        (column-sizes (gdb-table-column-sizes table))
        (right-align (gdb-table-right-align table)))
    (when (not column-sizes)
      (setf (gdb-table-column-sizes table)
            (make-list (length row) 0)))
    (setf (gdb-table-rows table)
          (append rows (list row)))
    (setf (gdb-table-row-properties table)
          (append row-properties (list properties)))
    (setf (gdb-table-column-sizes table)
          (cl-mapcar (lambda (x s)
                         (let ((new-x
                                (max (abs x) (string-width (or s "")))))
                           (if right-align new-x (- new-x))))
                       (gdb-table-column-sizes table)
                       row))
    ;; Avoid trailing whitespace at eol
    (if (not (gdb-table-right-align table))
        (setcar (last (gdb-table-column-sizes table)) 0))))

(defun gdb-table-string (table &optional sep)
  "Return TABLE as a string with columns separated with SEP."
  (let ((column-sizes (gdb-table-column-sizes table)))
    (mapconcat
     'identity
     (cl-mapcar
      (lambda (row properties)
        (apply 'propertize
               (mapconcat 'identity
                          (cl-mapcar (lambda (s x) (gdb-pad-string s x))
                                       row column-sizes)
                          sep)
               properties))
      (gdb-table-rows table)
      (gdb-table-row-properties table))
     "\n")))

(defmacro def-gdb-auto-update-trigger (trigger-name gdb-command
                                                    handler-name
                                                    &optional signal-list)
  "Define a trigger TRIGGER-NAME which sends GDB-COMMAND and sets
HANDLER-NAME as its handler.  HANDLER-NAME is bound to current
buffer with `gdb-bind-function-to-buffer'.

If SIGNAL-LIST is non-nil, GDB-COMMAND is sent only when the
defined trigger is called with an argument from SIGNAL-LIST.  It's
not recommended to define triggers with empty SIGNAL-LIST.
Normally triggers should respond at least to the `update' signal.

Normally the trigger defined by this command must be called from
the buffer where HANDLER-NAME must work.  This should be done so
that buffer-local thread number may be used in GDB-COMMAND (by
calling `gdb-current-context-command').
`gdb-bind-function-to-buffer' is used to achieve this, see
`gdb-get-buffer-create'.

Triggers defined by this command are meant to be used as a
trigger argument when describing buffer types with
`gdb-set-buffer-rules'."
  `(defun ,trigger-name (&optional signal)
     (when
         (or (not ,signal-list)
             (memq signal ,signal-list))
       (gdb-input ,gdb-command
                  (gdb-bind-function-to-buffer ',handler-name (current-buffer))
                  (cons (current-buffer) ',trigger-name)))))

;; Used by disassembly buffer only, the rest use
;; def-gdb-trigger-and-handler
(defmacro def-gdb-auto-update-handler (handler-name custom-defun
                                                    &optional nopreserve)
  "Define a handler HANDLER-NAME calling CUSTOM-DEFUN.

Handlers are normally called from the buffers they put output in.

Erase current buffer and evaluate CUSTOM-DEFUN.
Then call `gdb-update-buffer-name'.

If NOPRESERVE is non-nil, window point is not restored after CUSTOM-DEFUN."
  `(defun ,handler-name ()
     (let* ((inhibit-read-only t)
            ,@(unless nopreserve
                '((window (get-buffer-window (current-buffer) 0))
                  (start (window-start window))
                  (p (window-point window)))))
       (erase-buffer)
       (,custom-defun)
       (gdb-update-buffer-name)
       ,@(when (not nopreserve)
          '((set-window-start window start t)
            (set-window-point window p))))))

(defmacro def-gdb-trigger-and-handler (trigger-name gdb-command
                                                    handler-name custom-defun
                                                    &optional signal-list)
  "Define trigger and handler.

TRIGGER-NAME trigger is defined to send GDB-COMMAND.
See `def-gdb-auto-update-trigger'.

HANDLER-NAME handler uses customization of CUSTOM-DEFUN.
See `def-gdb-auto-update-handler'."
  `(progn
     (def-gdb-auto-update-trigger ,trigger-name
       ,gdb-command
       ,handler-name ,signal-list)
     (def-gdb-auto-update-handler ,handler-name
       ,custom-defun)))



;; Breakpoint buffer : This displays the output of `-break-list'.
(def-gdb-trigger-and-handler
  gdb-invalidate-breakpoints "-break-list"
  gdb-breakpoints-list-handler gdb-breakpoints-list-handler-custom
  '(start update))

(gdb-set-buffer-rules
 'gdb-breakpoints-buffer
 'gdb-breakpoints-buffer-name
 'gdb-breakpoints-mode
 'gdb-invalidate-breakpoints)

(defun gdb-breakpoints-list-handler-custom ()
  (let ((breakpoints-list (gdb-mi--field
                           (gdb-mi--field (gdb-mi--partial-output 'bkpt)
                                          'BreakpointTable)
                           'body))
        (table (make-gdb-table)))
    (setq gdb-breakpoints-list nil)
    (gdb-table-add-row table '("Num" "Type" "Disp" "Enb" "Addr" "Hits" "What"))
    (dolist (breakpoint breakpoints-list)
      (add-to-list 'gdb-breakpoints-list
                   (cons (gdb-mi--field breakpoint 'number)
                         breakpoint))
      (let ((at (gdb-mi--field breakpoint 'at))
            (pending (gdb-mi--field breakpoint 'pending))
            (func (gdb-mi--field breakpoint 'func))
	    (type (gdb-mi--field breakpoint 'type)))
        (gdb-table-add-row table
                           (list
                            (gdb-mi--field breakpoint 'number)
                            (or type "")
                            (or (gdb-mi--field breakpoint 'disp) "")
                            (let ((flag (gdb-mi--field breakpoint 'enabled)))
                              (if (string-equal flag "y")
                                  (eval-when-compile
                                    (propertize "y" 'font-lock-face
                                                font-lock-warning-face))
                                (eval-when-compile
                                  (propertize "n" 'font-lock-face
                                              font-lock-comment-face))))
                            (gdb-mi--field breakpoint 'addr)
                            (or (gdb-mi--field breakpoint 'times) "")
                            (if (and type (string-match ".*watchpoint" type))
                                (gdb-mi--field breakpoint 'what)
                              (or pending at
                                  (concat "in "
                                          (propertize (or func "unknown")
                                                      'font-lock-face font-lock-function-name-face)
                                          (gdb-frame-location breakpoint)))))
                           ;; Add clickable properties only for breakpoints with file:line
                           ;; information
                           (append (list 'gdb-breakpoint breakpoint)
                                   (when func '(help-echo "mouse-2, RET: visit breakpoint"
                                                mouse-face highlight))))))
    (insert (gdb-table-string table " "))
    (gdb-place-breakpoints)))

;; Put breakpoint icons in relevant margins (even those set in the GUD buffer).
(defun gdb-place-breakpoints ()
  ;; Remove all breakpoint-icons in source buffers but not assembler buffer.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (and (eq gud-minor-mode 'gdbmi)
               (not (string-match "\\` ?\\*.+\\*\\'" (buffer-name))))
          (gdb-remove-breakpoint-icons (point-min) (point-max)))))
  (dolist (breakpoint gdb-breakpoints-list)
    (let* ((breakpoint (cdr breakpoint)) ; gdb-breakpoints-list is
                                        ; an associative list
           (line (gdb-mi--field breakpoint 'line)))
      (when line
        (let ((file (gdb-mi--field breakpoint 'fullname))
              (flag (gdb-mi--field breakpoint 'enabled))
              (bptno (gdb-mi--field breakpoint 'number)))
          (unless (and file (file-exists-p file))
            (setq file (cdr (assoc bptno gdb-location-alist))))
	  (if (or (null file)
		  (string-equal file "File not found"))
	      ;; If the full filename is not recorded in the
	      ;; breakpoint structure or in `gdb-location-alist', use
	      ;; -file-list-exec-source-file to extract it.
	      (when (setq file (gdb-mi--field breakpoint 'file))
		(gdb-input (concat "list " file ":1") 'ignore)
		(gdb-input "-file-list-exec-source-file"
			   (lambda () (gdb-get-location
				       bptno line flag))))
	    (with-current-buffer (find-file-noselect file 'nowarn)
	      (gdb-init-buffer)
	      ;; Only want one breakpoint icon at each location.
	      (gdb-put-breakpoint-icon (string-equal flag "y") bptno
				       (string-to-number line)))))))))

(defconst gdb-source-file-regexp
  (concat "fullname=\\(" gdb--string-regexp "\\)"))

(defun gdb-get-location (bptno line flag)
  "Glean name of source file using `gdb-source-file-regexp', and visit it.
Place breakpoint icon in its buffer."
  (goto-char (point-min))
  (catch 'file-not-found
    (let (source-file)
      (if (re-search-forward gdb-source-file-regexp nil t)
          (progn
            (setq source-file (gdb-mi--c-string-from-string (match-string 1)))
            (delete (cons bptno "File not found") gdb-location-alist)
            (push (cons bptno source-file) gdb-location-alist))
        (gdb-resync)
        (unless (assoc bptno gdb-location-alist)
	  (push (cons bptno "File not found") gdb-location-alist)
	  (message-box "Cannot find source file for breakpoint location.
Add directory to search path for source files using the GDB command, dir."))
        (throw 'file-not-found nil))
      (with-current-buffer (find-file-noselect source-file)
        (gdb-init-buffer)
        ;; Only want one breakpoint icon at each location.
        (gdb-put-breakpoint-icon (string-equal flag "y") bptno
                                 (string-to-number line))))))

(add-hook 'find-file-hook 'gdb-find-file-hook)

(defun gdb-find-file-hook ()
  "Set up buffer for debugging if file is part of the source code
of the current session."
  (if (and (buffer-name gud-comint-buffer)
	   ;; in case gud or gdb-ui is just loaded
	   gud-comint-buffer
	   (eq (buffer-local-value 'gud-minor-mode gud-comint-buffer)
	       'gdbmi))
      (if (member buffer-file-name gdb-source-file-list)
	  (with-current-buffer (find-buffer-visiting buffer-file-name)
	    (gdb-init-buffer)))))

(declare-function gud-remove "gdb-mi" t t) ; gud-def
(declare-function gud-break  "gdb-mi" t t) ; gud-def
(declare-function fringe-bitmaps-at-pos "fringe.c" (&optional pos window))

(defun gdb-mouse-set-clear-breakpoint (event)
  "Set/clear breakpoint in left fringe/margin at mouse click.
If not in a source or disassembly buffer just set point."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((posn (event-end event)))
    (with-selected-window (posn-window posn)
      (if (or (buffer-file-name) (derived-mode-p 'gdb-disassembly-mode))
	  (if (numberp (posn-point posn))
	      (save-excursion
		(goto-char (posn-point posn))
		(if (or (posn-object posn)
			(eq (car (fringe-bitmaps-at-pos (posn-point posn)))
			    'breakpoint))
		    (gud-remove nil)
		  (gud-break nil)))))
      (posn-set-point posn))))

(defun gdb-mouse-toggle-breakpoint-margin (event)
  "Enable/disable breakpoint in left margin with mouse click."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((posn (event-end event)))
    (if (numberp (posn-point posn))
	(with-selected-window (posn-window posn)
	  (save-excursion
	    (goto-char (posn-point posn))
	    (if	(posn-object posn)
		(gud-basic-call
		 (let ((bptno (get-text-property
			       0 'gdb-bptno (car (posn-string posn)))))
		   (concat
		    (if (get-text-property
			 0 'gdb-enabled (car (posn-string posn)))
			"-break-disable "
		      "-break-enable ")
		    bptno)))))))))

(defun gdb-mouse-toggle-breakpoint-fringe (event)
  "Enable/disable breakpoint in left fringe with mouse click."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let* ((posn (event-end event))
	 (pos (posn-point posn))
	 obj)
    (when (numberp pos)
      (with-selected-window (posn-window posn)
	(with-current-buffer (window-buffer)
	  (goto-char pos)
	  (dolist (overlay (overlays-in pos pos))
	    (when (overlay-get overlay 'put-break)
	      (setq obj (overlay-get overlay 'before-string))))
	  (when (stringp obj)
	    (gud-basic-call
	     (concat
	      (if (get-text-property 0 'gdb-enabled obj)
		  "-break-disable "
		"-break-enable ")
              (get-text-property 0 'gdb-bptno obj)))))))))

(defun gdb-breakpoints-buffer-name ()
  (concat "*breakpoints of " (gdb-get-target-string) "*"))

(defun gdb-display-breakpoints-buffer (&optional thread)
  "Display GDB breakpoints."
  (interactive)
  (gdb-display-buffer (gdb-get-buffer-create 'gdb-breakpoints-buffer thread)))

(defun gdb-frame-breakpoints-buffer (&optional thread)
  "Display GDB breakpoints in another frame."
  (interactive)
  (display-buffer (gdb-get-buffer-create 'gdb-breakpoints-buffer thread)
		  gdb-display-buffer-other-frame-action))

(defvar gdb-breakpoints-mode-map
  (let ((map (make-sparse-keymap))
	(menu (make-sparse-keymap "Breakpoints")))
    (define-key menu [quit] '("Quit"   . gdb-delete-frame-or-window))
    (define-key menu [goto] '("Goto"   . gdb-goto-breakpoint))
    (define-key menu [delete] '("Delete" . gdb-delete-breakpoint))
    (define-key menu [toggle] '("Toggle" . gdb-toggle-breakpoint))
    (suppress-keymap map)
    (define-key map [menu-bar breakpoints] (cons "Breakpoints" menu))
    (define-key map " " 'gdb-toggle-breakpoint)
    (define-key map "D" 'gdb-delete-breakpoint)
    ;; Don't bind "q" to kill-this-buffer as we need it for breakpoint icons.
    (define-key map "q" 'gdb-delete-frame-or-window)
    (define-key map "\r" 'gdb-goto-breakpoint)
    (define-key map "\t" (lambda ()
                           (interactive)
                           (gdb-set-window-buffer
                            (gdb-get-buffer-create 'gdb-threads-buffer) t)))
    (define-key map [mouse-2] 'gdb-goto-breakpoint)
    (define-key map [follow-link] 'mouse-face)
    map))

(defun gdb-delete-frame-or-window ()
  "Delete frame if there is only one window.  Otherwise delete the window."
  (interactive)
  (if (one-window-p) (delete-frame)
    (delete-window)))

;;from make-mode-line-mouse-map
(defun gdb-make-header-line-mouse-map (mouse function) "\
Return a keymap with single entry for mouse key MOUSE on the header line.
MOUSE is defined to run function FUNCTION with no args in the buffer
corresponding to the mode line clicked."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'header-line mouse) function)
    (define-key map (vector 'header-line 'down-mouse-1) 'ignore)
    map))

(defmacro gdb-propertize-header (name buffer help-echo mouse-face face)
  `(propertize ,name
	       'help-echo ,help-echo
	       'mouse-face ',mouse-face
	       'face ',face
	       'local-map
	       (gdb-make-header-line-mouse-map
		'mouse-1
		(lambda (event) (interactive "e")
		  (save-selected-window
		    (select-window (posn-window (event-start event)))
                    (gdb-set-window-buffer
                     (gdb-get-buffer-create ',buffer) t) )))))


;; uses "-thread-info". Needs GDB 7.0 onwards.
;;; Threads view

(defun gdb-threads-buffer-name ()
  (concat "*threads of " (gdb-get-target-string) "*"))

(defun gdb-display-threads-buffer (&optional thread)
  "Display GDB threads."
  (interactive)
  (gdb-display-buffer (gdb-get-buffer-create 'gdb-threads-buffer thread)))

(defun gdb-frame-threads-buffer (&optional thread)
  "Display GDB threads in another frame."
  (interactive)
  (display-buffer (gdb-get-buffer-create 'gdb-threads-buffer thread)
		  gdb-display-buffer-other-frame-action))

(def-gdb-trigger-and-handler
  gdb-invalidate-threads (gdb-current-context-command "-thread-info")
  gdb-thread-list-handler gdb-thread-list-handler-custom
  '(start update update-threads))

(gdb-set-buffer-rules
 'gdb-threads-buffer
 'gdb-threads-buffer-name
 'gdb-threads-mode
 'gdb-invalidate-threads)

(defvar gdb-threads-font-lock-keywords
  '(("in \\([^ ]+\\)"  (1 font-lock-function-name-face))
    (" \\(stopped\\)"  (1 font-lock-warning-face))
    (" \\(running\\)"  (1 font-lock-string-face))
    ("\\(\\(\\sw\\|[_.]\\)+\\)="  (1 font-lock-variable-name-face)))
  "Font lock keywords used in `gdb-threads-mode'.")

(defvar gdb-threads-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'gdb-select-thread)
    (define-key map "f" 'gdb-display-stack-for-thread)
    (define-key map "F" 'gdb-frame-stack-for-thread)
    (define-key map "l" 'gdb-display-locals-for-thread)
    (define-key map "L" 'gdb-frame-locals-for-thread)
    (define-key map "r" 'gdb-display-registers-for-thread)
    (define-key map "R" 'gdb-frame-registers-for-thread)
    (define-key map "d" 'gdb-display-disassembly-for-thread)
    (define-key map "D" 'gdb-frame-disassembly-for-thread)
    (define-key map "i" 'gdb-interrupt-thread)
    (define-key map "c" 'gdb-continue-thread)
    (define-key map "s" 'gdb-step-thread)
    (define-key map "\t"
      (lambda ()
        (interactive)
        (gdb-set-window-buffer
         (gdb-get-buffer-create 'gdb-breakpoints-buffer) t)))
    (define-key map [mouse-2] 'gdb-select-thread)
    (define-key map [follow-link] 'mouse-face)
    map))

(defvar gdb-threads-header
  (list
   (gdb-propertize-header
    "Breakpoints" gdb-breakpoints-buffer
    "mouse-1: select" mode-line-highlight mode-line-inactive)
   " "
   (gdb-propertize-header "Threads" gdb-threads-buffer
			  nil nil mode-line)))

(define-derived-mode gdb-threads-mode gdb-parent-mode "Threads"
  "Major mode for GDB threads."
  (setq gdb-thread-position (make-marker))
  (add-to-list 'overlay-arrow-variable-list 'gdb-thread-position)
  (setq header-line-format gdb-threads-header)
  (setq-local font-lock-defaults '(gdb-threads-font-lock-keywords))
  'gdb-invalidate-threads)

(defun gdb-thread-list-handler-custom ()
  (let ((threads-list (gdb-mi--field (gdb-mi--partial-output) 'threads))
        (table (make-gdb-table))
        (marked-line nil))
    (setq gdb-threads-list nil)
    (setq gdb-running-threads-count 0)
    (setq gdb-stopped-threads-count 0)
    (set-marker gdb-thread-position nil)

    (dolist (thread (reverse threads-list))
      (let ((running (equal (gdb-mi--field thread 'state) "running")))
        (add-to-list 'gdb-threads-list
                     (cons (gdb-mi--field thread 'id)
                           thread))
        (cl-incf (if running
                     gdb-running-threads-count
                   gdb-stopped-threads-count))

        (gdb-table-add-row
         table
         (list
          (gdb-mi--field thread 'id)
          (concat
           (if gdb-thread-buffer-verbose-names
               (concat (gdb-mi--field thread 'target-id) " ") "")
           (gdb-mi--field thread 'state)
           ;; Include frame information for stopped threads
           (if (not running)
               (concat
                " in " (gdb-mi--field (gdb-mi--field thread 'frame) 'func)
                (if gdb-thread-buffer-arguments
                    (concat
                     " ("
                     (let ((args (gdb-mi--field (gdb-mi--field thread 'frame)
                                                'args)))
                       (mapconcat
                        (lambda (arg)
                          (format "%s=%s"
                                  (gdb-mi--field arg 'name)
                                  (gdb-mi--field arg 'value)))
                        args ","))
                     ")")
                  "")
                (if gdb-thread-buffer-locations
                    (gdb-frame-location (gdb-mi--field thread 'frame)) "")
                (if gdb-thread-buffer-addresses
                    (concat " at " (gdb-mi--field (gdb-mi--field thread 'frame)
                                                  'addr))
                  ""))
             "")))
         (list
          'gdb-thread thread
          'mouse-face 'highlight
          'help-echo "mouse-2, RET: select thread")))
      (when (string-equal gdb-thread-number
                          (gdb-mi--field thread 'id))
        (setq marked-line (length gdb-threads-list))))
    (insert (gdb-table-string table " "))
    (when marked-line
      (gdb-mark-line marked-line gdb-thread-position)))
  ;; We update gud-running here because we need to make sure that
  ;; gdb-threads-list is up-to-date
  (gdb-update-gud-running)
  (gdb-emit-signal gdb-buf-publisher 'update-disassembly))

(defmacro def-gdb-thread-buffer-command (name custom-defun &optional doc)
  "Define a NAME command which will act upon thread on the current line.

CUSTOM-DEFUN may use locally bound `thread' variable, which will
be the value of `gdb-thread' property of the current line.
If `gdb-thread' is nil, error is signaled."
  `(defun ,name (&optional event)
     ,(when doc doc)
     (interactive (list last-input-event))
     (if event (posn-set-point (event-end event)))
     (save-excursion
       (beginning-of-line)
       (let ((thread (get-text-property (point) 'gdb-thread)))
         (if thread
             ,custom-defun
           (error "Not recognized as thread line"))))))

(defmacro def-gdb-thread-buffer-simple-command (name buffer-command
                                                     &optional doc)
  "Define a NAME which will call BUFFER-COMMAND with id of thread
on the current line."
  `(def-gdb-thread-buffer-command ,name
     (,buffer-command (gdb-mi--field thread 'id))
     ,doc))

(def-gdb-thread-buffer-command gdb-select-thread
  (let ((new-id (gdb-mi--field thread 'id)))
    (gdb-setq-thread-number new-id)
    (gdb-input (concat "-thread-select " new-id) 'ignore)
    (gdb-update))
  "Select the thread at current line of threads buffer.")

(def-gdb-thread-buffer-simple-command
  gdb-display-stack-for-thread
  gdb-preemptively-display-stack-buffer
  "Display stack buffer for the thread at current line.")

(def-gdb-thread-buffer-simple-command
  gdb-display-locals-for-thread
  gdb-preemptively-display-locals-buffer
  "Display locals buffer for the thread at current line.")

(def-gdb-thread-buffer-simple-command
  gdb-display-registers-for-thread
  gdb-preemptively-display-registers-buffer
  "Display registers buffer for the thread at current line.")

(def-gdb-thread-buffer-simple-command
  gdb-display-disassembly-for-thread
  gdb-preemptively-display-disassembly-buffer
  "Display disassembly buffer for the thread at current line.")

(def-gdb-thread-buffer-simple-command
  gdb-frame-stack-for-thread
  gdb-frame-stack-buffer
  "Display another frame with stack buffer for thread at current line.")

(def-gdb-thread-buffer-simple-command
  gdb-frame-locals-for-thread
  gdb-frame-locals-buffer
  "Display another frame with locals buffer for thread at current line.")

(def-gdb-thread-buffer-simple-command
  gdb-frame-registers-for-thread
  gdb-frame-registers-buffer
  "Display another frame with registers buffer for the thread at current line.")

(def-gdb-thread-buffer-simple-command
  gdb-frame-disassembly-for-thread
  gdb-frame-disassembly-buffer
  "Display another frame with disassembly buffer for the thread at current line.")

(defmacro def-gdb-thread-buffer-gud-command (name gud-command &optional doc)
  "Define a NAME which will execute GUD-COMMAND with
`gdb-thread-number' locally bound to id of thread on the current
line."
  `(def-gdb-thread-buffer-command ,name
     (if gdb-non-stop
         (let ((gdb-thread-number (gdb-mi--field thread 'id))
               (gdb-gud-control-all-threads nil))
           (call-interactively #',gud-command))
       (error "Available in non-stop mode only, customize `gdb-non-stop-setting'"))
     ,doc))

(def-gdb-thread-buffer-gud-command
  gdb-interrupt-thread
  gud-stop-subjob
  "Interrupt thread at current line.")

;; Defined opaquely in M-x gdb via gud-def.
(declare-function gud-cont "gdb-mi" (arg) t)

(def-gdb-thread-buffer-gud-command
  gdb-continue-thread
  gud-cont
  "Continue thread at current line.")

(declare-function gud-step "gdb-mi" (arg) t)

(def-gdb-thread-buffer-gud-command
  gdb-step-thread
  gud-step
  "Step thread at current line.")


;;; Memory view

(defcustom gdb-memory-rows 8
  "Number of data rows in memory window."
  :type 'integer
  :group 'gud
  :version "23.2")

(defcustom gdb-memory-columns 4
  "Number of data columns in memory window."
  :type 'integer
  :group 'gud
  :version "23.2")

(defcustom gdb-memory-format "x"
  "Display format of data items in memory window."
  :type '(choice (const :tag "Hexadecimal" "x")
          (const :tag "Signed decimal" "d")
          (const :tag "Unsigned decimal" "u")
          (const :tag "Octal" "o")
          (const :tag "Binary" "t"))
  :group 'gud
  :version "22.1")

(defcustom gdb-memory-unit 4
  "Unit size of data items in memory window."
  :type '(choice (const :tag "Byte" 1)
          (const :tag "Halfword" 2)
          (const :tag "Word" 4)
          (const :tag "Giant word" 8))
  :group 'gud
  :version "23.2")

(def-gdb-trigger-and-handler
  gdb-invalidate-memory
  (format "-data-read-memory %s %s %d %d %d"
          (gdb-mi-quote gdb-memory-address-expression)
          gdb-memory-format
          gdb-memory-unit
          gdb-memory-rows
          gdb-memory-columns)
  gdb-read-memory-handler
  gdb-read-memory-custom
  '(start update))

(gdb-set-buffer-rules
 'gdb-memory-buffer
 'gdb-memory-buffer-name
 'gdb-memory-mode
 'gdb-invalidate-memory)

(defun gdb-memory-column-width (size format)
  "Return length of string with memory unit of SIZE in FORMAT.

SIZE is in bytes, as in `gdb-memory-unit'.  FORMAT is a string as
in `gdb-memory-format'."
  (let ((format-base (cdr (assoc format
                                 '(("x" . 16)
                                   ("d" . 10) ("u" . 10)
                                   ("o" . 8)
                                   ("t" . 2))))))
    (if format-base
        (let ((res (ceiling (log (expt 2.0 (* size 8)) format-base))))
          (cond ((string-equal format "x")
                 (+ 2 res)) ; hexadecimal numbers have 0x in front
                ((or (string-equal format "d")
                     (string-equal format "o"))
                 (1+ res))
                (t res)))
      (error "Unknown format"))))

(defun gdb-read-memory-custom ()
  (let* ((res (gdb-mi--partial-output))
         (err-msg (gdb-mi--field res 'msg)))
    (if (not err-msg)
        (let ((memory (gdb-mi--field res 'memory)))
          (when gdb-memory-last-address
            ;; Nil means last retrieve emits error or just started the session.
            (setq gdb--memory-display-warning nil))
          (setq gdb-memory-address (gdb-mi--field res 'addr))
          (setq gdb-memory-next-page (gdb-mi--field res 'next-page))
          (setq gdb-memory-prev-page (gdb-mi--field res 'prev-page))
          (setq gdb-memory-last-address gdb-memory-address)
          (dolist (row memory)
            (insert (concat (gdb-mi--field row 'addr) ":"))
            (dolist (column (gdb-mi--field row 'data))
              (insert (gdb-pad-string column
                                      (+ 2 (gdb-memory-column-width
                                            gdb-memory-unit
                                            gdb-memory-format)))))
            (newline)))
      ;; Show last page instead of empty buffer when out of bounds
      (when gdb-memory-last-address
        (let ((gdb-memory-address-expression gdb-memory-last-address))
          ;; If we don't set `gdb-memory-last-address' to nil,
          ;; `gdb-invalidate-memory' eventually calls
          ;; `gdb-read-memory-custom', making an infinite loop.
          (setq gdb-memory-last-address nil
                gdb--memory-display-warning t)
          (gdb-invalidate-memory 'update)
          (user-error "Error when retrieving memory: %s Displaying last successful page" err-msg))))))

(defvar gdb-memory-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "q" 'kill-current-buffer)
    (define-key map "n" 'gdb-memory-show-next-page)
    (define-key map "p" 'gdb-memory-show-previous-page)
    (define-key map "a" 'gdb-memory-set-address)
    (define-key map "t" 'gdb-memory-format-binary)
    (define-key map "o" 'gdb-memory-format-octal)
    (define-key map "u" 'gdb-memory-format-unsigned)
    (define-key map "d" 'gdb-memory-format-signed)
    (define-key map "x" 'gdb-memory-format-hexadecimal)
    (define-key map "b" 'gdb-memory-unit-byte)
    (define-key map "h" 'gdb-memory-unit-halfword)
    (define-key map "w" 'gdb-memory-unit-word)
    (define-key map "g" 'gdb-memory-unit-giant)
    (define-key map "R" 'gdb-memory-set-rows)
    (define-key map "C" 'gdb-memory-set-columns)
    map))

(defun gdb-memory-set-address-event (event)
  "Handle a click on address field in memory buffer header."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (gdb-memory-set-address)))

;; Non-event version for use within keymap
(defun gdb-memory-set-address ()
  "Set the start memory address."
  (interactive)
  (let ((arg (read-from-minibuffer "Memory address: ")))
    (setq gdb-memory-address-expression arg))
  (gdb-invalidate-memory 'update))

(defmacro def-gdb-set-positive-number (name variable echo-string &optional doc)
  "Define a function NAME which reads new VAR value from minibuffer."
  `(defun ,name (event)
     ,(when doc doc)
     (interactive "e")
     (save-selected-window
       (select-window (posn-window (event-start event)))
       (let* ((arg (read-from-minibuffer ,echo-string))
              (count (string-to-number arg)))
         (if (<= count 0)
             (error "Positive number only")
           (customize-set-variable ',variable count)
           (gdb-invalidate-memory 'update))))))

(def-gdb-set-positive-number
  gdb-memory-set-rows
  gdb-memory-rows
  "Rows: "
  "Set the number of data rows in memory window.")

(def-gdb-set-positive-number
  gdb-memory-set-columns
  gdb-memory-columns
  "Columns: "
  "Set the number of data columns in memory window.")

(defmacro def-gdb-memory-format (name format doc)
  "Define a function NAME to switch memory buffer to use FORMAT.

DOC is an optional documentation string."
  `(defun ,name () ,(when doc doc)
     (interactive)
     (customize-set-variable 'gdb-memory-format ,format)
     (gdb-invalidate-memory 'update)))

(def-gdb-memory-format
  gdb-memory-format-binary "t"
  "Set the display format to binary.")

(def-gdb-memory-format
  gdb-memory-format-octal "o"
  "Set the display format to octal.")

(def-gdb-memory-format
  gdb-memory-format-unsigned "u"
  "Set the display format to unsigned decimal.")

(def-gdb-memory-format
  gdb-memory-format-signed "d"
  "Set the display format to decimal.")

(def-gdb-memory-format
  gdb-memory-format-hexadecimal "x"
  "Set the display format to hexadecimal.")

(defvar gdb-memory-format-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line down-mouse-3] 'gdb-memory-format-menu-1)
    map)
  "Keymap to select format in the header line.")

(defvar gdb-memory-format-menu
  (let ((map (make-sparse-keymap "Format")))

    (define-key map [binary]
      '(menu-item "Binary" gdb-memory-format-binary
        :button (:radio . (equal gdb-memory-format "t"))))
    (define-key map [octal]
      '(menu-item "Octal" gdb-memory-format-octal
        :button (:radio . (equal gdb-memory-format "o"))))
    (define-key map [unsigned]
      '(menu-item "Unsigned Decimal" gdb-memory-format-unsigned
        :button (:radio . (equal gdb-memory-format "u"))))
    (define-key map [signed]
      '(menu-item "Signed Decimal" gdb-memory-format-signed
        :button (:radio . (equal gdb-memory-format "d"))))
    (define-key map [hexadecimal]
      '(menu-item "Hexadecimal" gdb-memory-format-hexadecimal
        :button (:radio . (equal gdb-memory-format "x"))))
    map)
  "Menu of display formats in the header line.")

(defun gdb-memory-format-menu (event)
  (interactive "@e")
  (x-popup-menu event gdb-memory-format-menu))

(defun gdb-memory-format-menu-1 (event)
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (let* ((selection (gdb-memory-format-menu event))
	   (binding (and selection (lookup-key gdb-memory-format-menu
					       (vector (car selection))))))
      (if binding (call-interactively binding)))))

(defmacro def-gdb-memory-unit (name unit-size doc)
  "Define a function NAME to switch memory unit size to UNIT-SIZE.

DOC is an optional documentation string."
  `(defun ,name () ,(when doc doc)
     (interactive)
     (customize-set-variable 'gdb-memory-unit ,unit-size)
     (gdb-invalidate-memory 'update)))

(def-gdb-memory-unit gdb-memory-unit-giant 8
  "Set the unit size to giant words (eight bytes).")

(def-gdb-memory-unit gdb-memory-unit-word 4
  "Set the unit size to words (four bytes).")

(def-gdb-memory-unit gdb-memory-unit-halfword 2
  "Set the unit size to halfwords (two bytes).")

(def-gdb-memory-unit gdb-memory-unit-byte 1
  "Set the unit size to bytes.")

(defmacro def-gdb-memory-show-page (name address-var &optional doc)
  "Define a function NAME which show new address in memory buffer.

The defined function switches Memory buffer to show address
stored in ADDRESS-VAR variable.

DOC is an optional documentation string."
  `(defun ,name
     ,(when doc doc)
     (interactive)
     (let ((gdb-memory-address ,address-var))
       (gdb-invalidate-memory))))

(def-gdb-memory-show-page gdb-memory-show-previous-page
  gdb-memory-prev-page)

(def-gdb-memory-show-page gdb-memory-show-next-page
  gdb-memory-next-page)

(defvar gdb-memory-unit-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line down-mouse-3] 'gdb-memory-unit-menu-1)
    map)
  "Keymap to select units in the header line.")

(defvar gdb-memory-unit-menu
  (let ((map (make-sparse-keymap "Unit")))
    (define-key map [giantwords]
      '(menu-item "Giant words" gdb-memory-unit-giant
        :button (:radio . (equal gdb-memory-unit 8))))
    (define-key map [words]
      '(menu-item "Words" gdb-memory-unit-word
        :button (:radio . (equal gdb-memory-unit 4))))
    (define-key map [halfwords]
      '(menu-item "Halfwords" gdb-memory-unit-halfword
        :button (:radio . (equal gdb-memory-unit 2))))
    (define-key map [bytes]
      '(menu-item "Bytes" gdb-memory-unit-byte
        :button (:radio . (equal gdb-memory-unit 1))))
    map)
  "Menu of units in the header line.")

(defun gdb-memory-unit-menu (event)
  (interactive "@e")
  (x-popup-menu event gdb-memory-unit-menu))

(defun gdb-memory-unit-menu-1 (event)
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (let* ((selection (gdb-memory-unit-menu event))
	   (binding (and selection (lookup-key gdb-memory-unit-menu
					       (vector (car selection))))))
      (if binding (call-interactively binding)))))

(defvar gdb-memory-font-lock-keywords
  '(;; <__function.name+n>
    ("<\\(\\(\\sw\\|[_.]\\)+\\)\\(\\+[0-9]+\\)?>"
     (1 font-lock-function-name-face)))
  "Font lock keywords used in `gdb-memory-mode'.")

(defvar gdb-memory-header
  '(:eval
    (concat
     "Start address "
     ;; If `gdb-memory-address-expression' is nil, `propertize' would error.
     (propertize (or gdb-memory-address-expression "N/A")
                 'face font-lock-warning-face
                 'help-echo "mouse-1: set start address"
                 'mouse-face 'mode-line-highlight
                 'local-map (gdb-make-header-line-mouse-map
                             'mouse-1
                             #'gdb-memory-set-address-event))
     (if gdb--memory-display-warning
         (propertize " !" 'face '(:inherit error :weight bold))
       "")
     " ["
     (propertize "-"
                 'face font-lock-warning-face
                 'help-echo "mouse-1: decrement address"
                 'mouse-face 'mode-line-highlight
                 'local-map (gdb-make-header-line-mouse-map
                             'mouse-1
                             #'gdb-memory-show-previous-page))
     "|"
     (propertize "+"
                 'face font-lock-warning-face
                 'help-echo "mouse-1: increment address"
                 'mouse-face 'mode-line-highlight
                 'local-map (gdb-make-header-line-mouse-map
                             'mouse-1
                             #'gdb-memory-show-next-page))
     "]: "
     ;; If `gdb-memory-address' is nil, `propertize' would error.
     (propertize (or gdb-memory-address "N/A")
                 'face font-lock-warning-face)
     "  Rows: "
     (propertize (number-to-string gdb-memory-rows)
                 'face font-lock-warning-face
                 'help-echo "mouse-1: set number of columns"
                 'mouse-face 'mode-line-highlight
                 'local-map (gdb-make-header-line-mouse-map
                             'mouse-1
                             #'gdb-memory-set-rows))
     "  Columns: "
     (propertize (number-to-string gdb-memory-columns)
                 'face font-lock-warning-face
                 'help-echo "mouse-1: set number of columns"
                 'mouse-face 'mode-line-highlight
                 'local-map (gdb-make-header-line-mouse-map
                             'mouse-1
                             #'gdb-memory-set-columns))
     "  Display Format: "
     (propertize gdb-memory-format
                 'face font-lock-warning-face
                 'help-echo "mouse-3: select display format"
                 'mouse-face 'mode-line-highlight
                 'local-map gdb-memory-format-map)
     "  Unit Size: "
     (propertize (number-to-string gdb-memory-unit)
                 'face font-lock-warning-face
                 'help-echo "mouse-3: select unit size"
                 'mouse-face 'mode-line-highlight
                 'local-map gdb-memory-unit-map)))
  "Header line used in `gdb-memory-mode'.")

(define-derived-mode gdb-memory-mode gdb-parent-mode "Memory"
  "Major mode for examining memory."
  (setq header-line-format gdb-memory-header)
  (setq-local font-lock-defaults '(gdb-memory-font-lock-keywords))
  'gdb-invalidate-memory)

(defun gdb-memory-buffer-name ()
  (concat "*memory of " (gdb-get-target-string) "*"))

(defun gdb-display-memory-buffer (&optional thread)
  "Display GDB memory contents."
  (interactive)
  (gdb-display-buffer (gdb-get-buffer-create 'gdb-memory-buffer thread)))

(defun gdb-frame-memory-buffer ()
  "Display memory contents in another frame."
  (interactive)
  (display-buffer (gdb-get-buffer-create 'gdb-memory-buffer)
		  gdb-display-buffer-other-frame-action))


;;; Disassembly view

(defun gdb-disassembly-buffer-name ()
  (gdb-current-context-buffer-name
   (concat "disassembly of " (gdb-get-target-string))))

(defun gdb-display-disassembly-buffer (&optional thread)
  "Display GDB disassembly information."
  (interactive)
  (gdb-display-buffer (gdb-get-buffer-create 'gdb-disassembly-buffer thread)))

(def-gdb-preempt-display-buffer
  gdb-preemptively-display-disassembly-buffer
  'gdb-disassembly-buffer)

(defun gdb-frame-disassembly-buffer (&optional thread)
  "Display GDB disassembly information in another frame."
  (interactive)
  (display-buffer (gdb-get-buffer-create 'gdb-disassembly-buffer thread)
		  gdb-display-buffer-other-frame-action))

(def-gdb-auto-update-trigger gdb-invalidate-disassembly
  (let* ((frame (gdb-current-buffer-frame))
         (file (gdb-mi--field frame 'fullname))
         (line (gdb-mi--field frame 'line)))
    (if file
      (format "-data-disassemble -f %s -l %s -n -1 -- 0" file line)
    ;; If we're unable to get a file name / line for $PC, simply
    ;; follow $PC, disassembling the next 10 (x ~15 (on IA) ==
    ;; 150 bytes) instructions.
    "-data-disassemble -s $pc -e \"$pc + 150\" -- 0"))
  gdb-disassembly-handler
  ;; We update disassembly only after we have actual frame information
  ;; about all threads, so no there's `update' signal in this list
  '(start update-disassembly))

(def-gdb-auto-update-handler
  gdb-disassembly-handler
  gdb-disassembly-handler-custom
  t)

(gdb-set-buffer-rules
 'gdb-disassembly-buffer
 'gdb-disassembly-buffer-name
 'gdb-disassembly-mode
 'gdb-invalidate-disassembly)

(defvar gdb-disassembly-font-lock-keywords
  '(;; <__function.name+n>
    ("<\\(\\(\\sw\\|[_.]\\)+\\)\\(\\+[0-9]+\\)?>"
     (1 font-lock-function-name-face))
    ;; 0xNNNNNNNN <__function.name+n>: opcode
    ("^0x[0-9a-f]+ \\(<\\(\\(\\sw\\|[_.]\\)+\\)\\+[0-9]+>\\)?:[ \t]+\\(\\sw+\\)"
     (4 font-lock-keyword-face))
    ;; %register(at least i386)
    ("%\\sw+" . font-lock-variable-name-face)
    ("^\\(Dump of assembler code for function\\) \\(.+\\):"
     (1 font-lock-comment-face)
     (2 font-lock-function-name-face))
    ("^\\(End of assembler dump\\.\\)" . font-lock-comment-face))
  "Font lock keywords used in `gdb-disassembly-mode'.")

(defvar gdb-disassembly-mode-map
  ;; TODO
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-current-buffer)
    map))

(define-derived-mode gdb-disassembly-mode gdb-parent-mode "Disassembly"
  "Major mode for GDB disassembly information."
  ;; TODO Rename overlay variable for disassembly mode
  (add-to-list 'overlay-arrow-variable-list 'gdb-disassembly-position)
  (setq fringes-outside-margins t)
  (setq-local gdb-disassembly-position (make-marker))
  (setq-local font-lock-defaults '(gdb-disassembly-font-lock-keywords))
  'gdb-invalidate-disassembly)

(defun gdb-disassembly-handler-custom ()
  (let* ((instructions (gdb-mi--field (gdb-mi--partial-output) 'asm_insns))
         (address (gdb-mi--field (gdb-current-buffer-frame) 'addr))
         (table (make-gdb-table))
         (marked-line nil))
    (dolist (instr instructions)
      (gdb-table-add-row table
                         (list
                          (gdb-mi--field instr 'address)
                          (let
                              ((func-name (gdb-mi--field instr 'func-name))
                               (offset (gdb-mi--field instr 'offset)))
                            (if func-name
                                (format "<%s+%s>:" func-name offset)
                              ""))
                          (gdb-mi--field instr 'inst)))
      (when (string-equal (gdb-mi--field instr 'address)
                          address)
        (progn
          (setq marked-line (length (gdb-table-rows table)))
          (setq fringe-indicator-alist
                (if (string-equal gdb-frame-number "0")
                    nil
                  '((overlay-arrow . hollow-right-triangle)))))))
    (insert (gdb-table-string table " "))
    (gdb-disassembly-place-breakpoints)
    ;; Mark current position with overlay arrow and scroll window to
    ;; that point
    (when marked-line
      (let ((window (get-buffer-window (current-buffer) 0)))
        (set-window-point window (gdb-mark-line marked-line
                                                gdb-disassembly-position))))
    (setq mode-name
          (gdb-current-context-mode-name
           (concat "Disassembly: "
                   (gdb-mi--field (gdb-current-buffer-frame) 'func))))))

(defun gdb-disassembly-place-breakpoints ()
  (gdb-remove-breakpoint-icons (point-min) (point-max))
  (dolist (breakpoint gdb-breakpoints-list)
    (let* ((breakpoint (cdr breakpoint))
           (bptno (gdb-mi--field breakpoint 'number))
           (flag (gdb-mi--field breakpoint 'enabled))
           (address (gdb-mi--field breakpoint 'addr)))
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward (concat "^" address) nil t)
            (gdb-put-breakpoint-icon (string-equal flag "y") bptno))))))


(defvar gdb-breakpoints-header
  (list
   (gdb-propertize-header "Breakpoints" gdb-breakpoints-buffer
			  nil nil mode-line)
   " "
   (gdb-propertize-header "Threads" gdb-threads-buffer
			  "mouse-1: select" mode-line-highlight
                          mode-line-inactive)))

;;; Breakpoints view
(define-derived-mode gdb-breakpoints-mode gdb-parent-mode "Breakpoints"
  "Major mode for gdb breakpoints."
  (setq header-line-format gdb-breakpoints-header)
  'gdb-invalidate-breakpoints)

(defun gdb-toggle-breakpoint ()
  "Enable/disable breakpoint at current line of breakpoints buffer."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((breakpoint (get-text-property (point) 'gdb-breakpoint)))
      (if breakpoint
          (gud-basic-call
           (concat (if (equal "y" (gdb-mi--field breakpoint 'enabled))
                       "-break-disable "
                     "-break-enable ")
                   (gdb-mi--field breakpoint 'number)))
        (error "Not recognized as break/watchpoint line")))))

(defun gdb-delete-breakpoint ()
  "Delete the breakpoint at current line of breakpoints buffer."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((breakpoint (get-text-property (point) 'gdb-breakpoint)))
      (if breakpoint
          (gud-basic-call (concat "-break-delete "
                                  (gdb-mi--field breakpoint 'number)))
        (error "Not recognized as break/watchpoint line")))))

(defun gdb-goto-breakpoint (&optional event)
  "Go to the location of breakpoint at current line of breakpoints buffer."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  ;; Hack to stop gdb-goto-breakpoint displaying in GUD buffer.
  (let ((window (get-buffer-window gud-comint-buffer)))
    (if window (save-selected-window  (select-window window))))
  (save-excursion
    (beginning-of-line)
    (let ((breakpoint (get-text-property (point) 'gdb-breakpoint)))
      (if breakpoint
          (let ((bptno (gdb-mi--field breakpoint 'number))
                (file  (gdb-mi--field breakpoint 'fullname))
                (line  (gdb-mi--field breakpoint 'line)))
            (save-selected-window
              (let* ((buffer (find-file-noselect
                              (if (file-exists-p file) file
                                (cdr (assoc bptno gdb-location-alist)))))
                     (window (gdb-display-source-buffer buffer)))
                (with-current-buffer buffer
                  (goto-char (point-min))
                  (forward-line (1- (string-to-number line)))
                  (set-window-point window (point))))))
        (error "Not recognized as break/watchpoint line")))))


;; Frames buffer.  This displays a perpetually correct backtrack trace.
;;
(def-gdb-trigger-and-handler
  gdb-invalidate-frames (gdb-current-context-command "-stack-list-frames")
  gdb-stack-list-frames-handler gdb-stack-list-frames-custom
  '(start update))

(gdb-set-buffer-rules
 'gdb-stack-buffer
 'gdb-stack-buffer-name
 'gdb-frames-mode
 'gdb-invalidate-frames)

(defun gdb-frame-location (frame)
  "Return \" of file:line\" or \" of library\" for structure FRAME.

FRAME must have either \"file\" and \"line\" members or \"from\"
member."
  (let ((file (gdb-mi--field frame 'file))
        (line (gdb-mi--field frame 'line))
        (from (gdb-mi--field frame 'from)))
    (let ((res (or (and file line (concat file ":" line))
                   from)))
      (if res (concat " of " res) ""))))

(defun gdb-stack-list-frames-custom ()
  (let ((stack (gdb-mi--field (gdb-mi--partial-output 'frame) 'stack))
        (table (make-gdb-table)))
    (set-marker gdb-stack-position nil)
    (dolist (frame stack)
      (gdb-table-add-row table
                         (list
                          (gdb-mi--field frame 'level)
                          "in"
                          (concat
                           (gdb-mi--field frame 'func)
                           (if gdb-stack-buffer-locations
                               (gdb-frame-location frame) "")
                           (if gdb-stack-buffer-addresses
                               (concat " at " (gdb-mi--field frame 'addr)) "")))
                         `(mouse-face highlight
                                      help-echo "mouse-2, RET: Select frame"
                                      gdb-frame ,frame)))
    (insert (gdb-table-string table " ")))
  (when (and gdb-frame-number
             (gdb-buffer-shows-main-thread-p))
    (gdb-mark-line (1+ (string-to-number gdb-frame-number))
                   gdb-stack-position))
  (setq mode-name
        (gdb-current-context-mode-name "Frames")))

(defun gdb-stack-buffer-name ()
  (gdb-current-context-buffer-name
   (concat "stack frames of " (gdb-get-target-string))))

(defun gdb-display-stack-buffer (&optional thread)
  "Display GDB backtrace for current stack."
  (interactive)
  (gdb-display-buffer (gdb-get-buffer-create 'gdb-stack-buffer thread)))

(def-gdb-preempt-display-buffer
  gdb-preemptively-display-stack-buffer
  'gdb-stack-buffer nil t)

(defun gdb-frame-stack-buffer (&optional thread)
  "Display GDB backtrace for current stack in another frame."
  (interactive)
  (display-buffer (gdb-get-buffer-create 'gdb-stack-buffer thread)
		  gdb-display-buffer-other-frame-action))

(defvar gdb-frames-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-current-buffer)
    (define-key map "\r" 'gdb-select-frame)
    (define-key map [mouse-2] 'gdb-select-frame)
    (define-key map [follow-link] 'mouse-face)
    map))

(defvar gdb-frames-font-lock-keywords
  '(("in \\([^ ]+\\)"  (1 font-lock-function-name-face)))
  "Font lock keywords used in `gdb-frames-mode'.")

(define-derived-mode gdb-frames-mode gdb-parent-mode "Frames"
  "Major mode for gdb call stack."
  (setq gdb-stack-position (make-marker))
  (add-to-list 'overlay-arrow-variable-list 'gdb-stack-position)
  (setq truncate-lines t)  ;; Make it easier to see overlay arrow.
  (setq-local font-lock-defaults '(gdb-frames-font-lock-keywords))
  'gdb-invalidate-frames)

(defun gdb-select-frame (&optional event)
  "Select the frame and display the relevant source."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  (let ((frame (get-text-property (point) 'gdb-frame)))
    (if frame
        (if (gdb-buffer-shows-main-thread-p)
            (let ((new-level (gdb-mi--field frame 'level)))
              (setq gdb-frame-number new-level)
              (gdb-input (concat "-stack-select-frame " new-level)
			 'ignore)
              (gdb-update))
          (error "Could not select frame for non-current thread"))
      (error "Not recognized as frame line"))))


;; Locals buffer.
;; uses "-stack-list-locals --simple-values". Needs GDB 6.1 onwards.
(def-gdb-trigger-and-handler
  gdb-invalidate-locals
  (concat (gdb-current-context-command "-stack-list-locals")
          " --simple-values")
  gdb-locals-handler gdb-locals-handler-custom
  '(start update))

(gdb-set-buffer-rules
 'gdb-locals-buffer
 'gdb-locals-buffer-name
 'gdb-locals-mode
 'gdb-invalidate-locals)

(defvar gdb-locals-watch-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'gud-watch)
    (define-key map [mouse-2] 'gud-watch)
    map)
  "Keymap to create watch expression of a complex data type local variable.")

(defvar gdb-edit-locals-map-1
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'gdb-edit-locals-value)
    (define-key map [mouse-2] 'gdb-edit-locals-value)
    map)
  "Keymap to edit value of a simple data type local variable.")

(defun gdb-edit-locals-value (&optional event)
  "Assign a value to a variable displayed in the locals buffer."
  (interactive (list last-input-event))
  (save-excursion
    (if event (posn-set-point (event-end event)))
    (beginning-of-line)
    (let* ((var (gdb-mi--field
                 (get-text-property (point) 'gdb-local-variable) 'name))
	   (value (read-string (format "New value (%s): " var))))
      (gud-basic-call
       (concat  "-gdb-set variable " var " = " value)))))

;; Don't display values of arrays or structures.
;; These can be expanded using gud-watch.
(defun gdb-locals-handler-custom ()
  (let ((locals-list (gdb-mi--field (gdb-mi--partial-output) 'locals))
        (table (make-gdb-table)))
    (dolist (local locals-list)
      (let ((name (gdb-mi--field local 'name))
            (value (gdb-mi--field local 'value))
            (type (gdb-mi--field local 'type)))
        (when (not value)
          (setq value "<complex data type>"))
        (if (or (not value)
                (string-match "0x" value))
            (add-text-properties 0 (length name)
                                 `(mouse-face highlight
                                              help-echo "mouse-2: create watch expression"
                                              local-map ,gdb-locals-watch-map)
                                 name)
          (add-text-properties 0 (length value)
                               `(mouse-face highlight
                                            help-echo "mouse-2: edit value"
                                            local-map ,gdb-edit-locals-map-1)
                               value))
        (gdb-table-add-row
         table
         (list
          (propertize type 'font-lock-face font-lock-type-face)
          (propertize name 'font-lock-face font-lock-variable-name-face)
          value)
         `(gdb-local-variable ,local))))
    (insert (gdb-table-string table " "))
    (setq mode-name
          (gdb-current-context-mode-name
           (concat "Locals: "
                   (gdb-mi--field (gdb-current-buffer-frame) 'func))))))

(defvar gdb-locals-header
  (list
   (gdb-propertize-header "Locals" gdb-locals-buffer
			  nil nil mode-line)
   " "
   (gdb-propertize-header "Registers" gdb-registers-buffer
			  "mouse-1: select" mode-line-highlight
                          mode-line-inactive)))

(defvar gdb-locals-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-current-buffer)
    (define-key map "\t" (lambda ()
                           (interactive)
                           (gdb-set-window-buffer
                            (gdb-get-buffer-create
                             'gdb-registers-buffer
                             gdb-thread-number) t)))
    map))

(define-derived-mode gdb-locals-mode gdb-parent-mode "Locals"
  "Major mode for gdb locals."
  (setq header-line-format gdb-locals-header)
  'gdb-invalidate-locals)

(defun gdb-locals-buffer-name ()
  (gdb-current-context-buffer-name
   (concat "locals of " (gdb-get-target-string))))

(defun gdb-display-locals-buffer (&optional thread)
  "Display the local variables of current GDB stack."
  (interactive)
  (gdb-display-buffer (gdb-get-buffer-create 'gdb-locals-buffer thread)))

(def-gdb-preempt-display-buffer
  gdb-preemptively-display-locals-buffer
  'gdb-locals-buffer nil t)

(defun gdb-frame-locals-buffer (&optional thread)
  "Display the local variables of the current GDB stack in another frame."
  (interactive)
  (display-buffer (gdb-get-buffer-create 'gdb-locals-buffer thread)
		  gdb-display-buffer-other-frame-action))


;; Registers buffer.

(def-gdb-trigger-and-handler
  gdb-invalidate-registers
  (concat (gdb-current-context-command "-data-list-register-values") " x")
  gdb-registers-handler
  gdb-registers-handler-custom
  '(start update))

(gdb-set-buffer-rules
 'gdb-registers-buffer
 'gdb-registers-buffer-name
 'gdb-registers-mode
 'gdb-invalidate-registers)

(defun gdb-header-click-event-handler (function)
  "Return a function that handles clicking event on gdb header buttons.

This function switches to the window where the header locates and
executes FUNCTION."
  (lambda (event)
    (interactive "e")
    (save-selected-window
      ;; Make sure we are in the right buffer.
      (select-window (posn-window (event-start event)))
      (funcall function))))

(defun gdb-registers-toggle-filter ()
  "Toggle register filter."
  (interactive)
  (setq gdb-registers-enable-filter
        (not gdb-registers-enable-filter))
  ;; Update the register buffer.
  (gdb-invalidate-registers 'update))

(defun gdb-registers-handler-custom ()
  (when gdb-register-names
    (let ((register-values
           (gdb-mi--field (gdb-mi--partial-output) 'register-values))
          (table (make-gdb-table)))
      (dolist (register register-values)
        (let* ((register-number (gdb-mi--field register 'number))
               (value (gdb-mi--field register 'value))
               (register-name (nth (string-to-number register-number)
                                   gdb-register-names)))
          ;; Add register if `gdb-registers-filter-pattern-list' is nil;
          ;; or any pattern that `gdb-registers-filter-pattern-list'
          ;; matches.
          (when (or (null gdb-registers-enable-filter)
                    ;; Return t if any register name matches a pattern.
                    (cl-loop for pattern
                             in gdb-registers-filter-pattern-list
                             if (string-match pattern register-name)
                             return t
                             finally return nil))
            (gdb-table-add-row
             table
             (list
              (propertize register-name
                          'font-lock-face font-lock-variable-name-face)
              (if (member register-number gdb-changed-registers)
                  (propertize value 'font-lock-face font-lock-warning-face)
                value))
             `(mouse-face highlight
                          help-echo "mouse-2: edit value"
                          gdb-register-name ,register-name)))))
      (insert (gdb-table-string table " ")))
    (setq mode-name
          (gdb-current-context-mode-name "Registers"))))

(defun gdb-edit-register-value (&optional event)
  "Assign a value to a register displayed in the registers buffer."
  (interactive (list last-input-event))
  (save-excursion
    (if event (posn-set-point (event-end event)))
    (beginning-of-line)
    (let* ((var (get-text-property (point) 'gdb-register-name))
	   (value (read-string (format "New value (%s): " var))))
      (gud-basic-call
       (concat  "-gdb-set variable $" var " = " value)))))

(defvar gdb-registers-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "\r" 'gdb-edit-register-value)
    (define-key map [mouse-2] 'gdb-edit-register-value)
    (define-key map "q" 'kill-current-buffer)
    (define-key map "\t" (lambda ()
                           (interactive)
                           (gdb-set-window-buffer
                            (gdb-get-buffer-create
                             'gdb-locals-buffer
                             gdb-thread-number) t)))
    (define-key map "f" #'gdb-registers-toggle-filter)
    map))

(defvar gdb-registers-header
  (list
   (gdb-propertize-header "Locals" gdb-locals-buffer
			  "mouse-1: select" mode-line-highlight
                          mode-line-inactive)
   " "
   (gdb-propertize-header "Registers" gdb-registers-buffer
			  nil nil mode-line)
   " "
   '(:eval
     (format
      "[filter %s %s]"
      (propertize
       (if gdb-registers-enable-filter "[on]" "[off]")
       'face (if gdb-registers-enable-filter
                 '(:weight bold :inherit success)
               'shadow)
       'help-echo "mouse-1: toggle filter"
       'mouse-face 'mode-line-highlight
       'local-map (gdb-make-header-line-mouse-map
                   'mouse-1 (gdb-header-click-event-handler
                             #'gdb-registers-toggle-filter)))
      (propertize
       "[set]"
       'face 'mode-line
       'help-echo "mouse-1: Customize filter patterns"
       'mouse-face 'mode-line-highlight
       'local-map (gdb-make-header-line-mouse-map
                   'mouse-1 (lambda ()
                              (interactive)
                              (customize-variable-other-window
                               'gdb-registers-filter-pattern-list))))))))

(define-derived-mode gdb-registers-mode gdb-parent-mode "Registers"
  "Major mode for gdb registers."
  (setq header-line-format gdb-registers-header)
  'gdb-invalidate-registers)

(defun gdb-registers-buffer-name ()
  (gdb-current-context-buffer-name
   (concat "registers of " (gdb-get-target-string))))

(defun gdb-display-registers-buffer (&optional thread)
  "Display GDB register contents."
  (interactive)
  (gdb-display-buffer (gdb-get-buffer-create 'gdb-registers-buffer thread)))

(def-gdb-preempt-display-buffer
  gdb-preemptively-display-registers-buffer
  'gdb-registers-buffer nil t)

(defun gdb-frame-registers-buffer (&optional thread)
  "Display GDB register contents in another frame."
  (interactive)
  (display-buffer (gdb-get-buffer-create 'gdb-registers-buffer thread)
		  gdb-display-buffer-other-frame-action))

;; Needs GDB 6.4 onwards (used to fail with no stack).
(defun gdb-get-changed-registers ()
  (when (gdb-get-buffer 'gdb-registers-buffer)
    (gdb-input "-data-list-changed-registers"
               'gdb-changed-registers-handler
               'gdb-get-changed-registers)))

(defun gdb-changed-registers-handler ()
  (setq gdb-changed-registers nil)
  (dolist (register-number
           (gdb-mi--field (gdb-mi--partial-output) 'changed-registers))
    (push register-number gdb-changed-registers)))

(defun gdb-register-names-handler ()
  ;; Don't use pending triggers because this handler is called
  ;; only once (in gdb-init-1)
  (setq gdb-register-names nil)
  (dolist (register-name
           (gdb-mi--field (gdb-mi--partial-output) 'register-names))
    (push register-name gdb-register-names))
  (setq gdb-register-names (reverse gdb-register-names)))


(defun gdb-get-source-file-list ()
  "Create list of source files for current GDB session.
If buffers already exist for any of these files, `gud-minor-mode'
is set in them."
  (goto-char (point-min))
  (while (re-search-forward gdb-source-file-regexp nil t)
    (push (gdb-mi--c-string-from-string (match-string 1))
          gdb-source-file-list))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (member buffer-file-name gdb-source-file-list)
	(gdb-init-buffer)))))

(defun gdb-get-main-selected-frame ()
  "Trigger for `gdb-frame-handler' which uses main current thread.
Called from `gdb-update'."
  (gdb-input (gdb-current-context-command "-stack-info-frame")
             'gdb-frame-handler
             'gdb-get-main-selected-frame))

(defun gdb-frame-handler ()
  "Set `gdb-selected-frame' and `gdb-selected-file' to show
overlay arrow in source buffer."
  (let ((frame (gdb-mi--field (gdb-mi--partial-output) 'frame)))
    (when frame
      (setq gdb-selected-frame (gdb-mi--field frame 'func))
      (setq gdb-selected-file
            (when-let ((full (gdb-mi--field frame 'fullname)))
              (file-local-name full)))
      (setq gdb-frame-number (gdb-mi--field frame 'level))
      (setq gdb-frame-address (gdb-mi--field frame 'addr))
      (let ((line (gdb-mi--field frame 'line)))
        (setq gdb-selected-line (and line (string-to-number line)))
        (when (and gdb-selected-file gdb-selected-line)
          (setq gud-last-frame (cons gdb-selected-file gdb-selected-line))
          (gud-display-frame)))
      (if gud-overlay-arrow-position
          (let ((buffer (marker-buffer gud-overlay-arrow-position))
                (position (marker-position gud-overlay-arrow-position)))
            (when buffer
              (with-current-buffer buffer
                (setq fringe-indicator-alist
                      (if (string-equal gdb-frame-number "0")
                          nil
                        '((overlay-arrow . hollow-right-triangle))))
                (setq gud-overlay-arrow-position (make-marker))
                (set-marker gud-overlay-arrow-position position))))))))

(defconst gdb-prompt-name-regexp
  (concat "value=\\(" gdb--string-regexp "\\)"))

(defun gdb-get-prompt ()
  "Find prompt for GDB session."
  (goto-char (point-min))
  (setq gdb-prompt-name nil)
  (re-search-forward gdb-prompt-name-regexp nil t)
  (setq gdb-prompt-name (gdb-mi--c-string-from-string (match-string 1)))
  ;; Insert first prompt.
  (setq gdb-filter-output (concat gdb-filter-output gdb-prompt-name)))

;;;; Window management
(defun gdb-display-buffer (buf)
  "Show buffer BUF, and make that window dedicated."
  (let ((window (display-buffer buf)))
    (set-window-dedicated-p window t)
    window))

  ;; (let ((answer (get-buffer-window buf 0)))
  ;;   (if answer
  ;; 	(display-buffer buf nil 0) ;Deiconify frame if necessary.
  ;;     (let ((window (get-lru-window)))
  ;; 	(if (eq (buffer-local-value 'gud-minor-mode (window-buffer window))
  ;;               'gdbmi)
  ;; 	    (let ((largest (get-largest-window)))
  ;; 	      (setq answer (split-window largest))
  ;; 	      (set-window-buffer answer buf)
  ;; 	      (set-window-dedicated-p answer t)
  ;; 	      answer)
  ;; 	  (set-window-buffer window buf)
  ;; 	  window)))))


(defun gdb-preempt-existing-or-display-buffer (buf &optional split-horizontal)
  "Find window displaying a buffer with the same
`gdb-buffer-type' as BUF and show BUF there.  If no such window
exists, just call `gdb-display-buffer' for BUF.  If the window
found is already dedicated, split window according to
SPLIT-HORIZONTAL and show BUF in the new window."
  (if buf
      (when (not (get-buffer-window buf))
        (let* ((buf-type (gdb-buffer-type buf))
               (existing-window
                (get-window-with-predicate
                 (lambda (w)
                   (and (eq buf-type
                            (gdb-buffer-type (window-buffer w)))
                        (not (window-dedicated-p w)))))))
          (if existing-window
              (set-window-buffer existing-window buf)
            (let ((dedicated-window
                   (get-window-with-predicate
                    (lambda (w)
                      (eq buf-type
                          (gdb-buffer-type (window-buffer w)))))))
              (if dedicated-window
                  (set-window-buffer
                   (split-window dedicated-window nil split-horizontal) buf)
                (gdb-display-buffer buf))))))
    (error "Null buffer")))

;;; Shared keymap initialization:

(let ((menu (make-sparse-keymap "GDB-Windows")))
  (define-key gud-menu-map [displays]
    `(menu-item "GDB-Windows" ,menu
		:visible (eq gud-minor-mode 'gdbmi)))
  (define-key menu [gdb-restore-windows]
    '(menu-item "Restore Initial Layout" gdb-restore-windows
      :help "Restore the initial GDB window layout."))
  ;; Window layout vs window configuration: We use "window layout" in
  ;; GDB UI.  Internally we refer to "window configuration" because
  ;; that's the data structure used to store window layouts.  Though
  ;; bare in mind that there is a small difference between what we
  ;; store and what normal window configuration functions
  ;; output. Because GDB buffers (source, local, breakpoint, etc) are
  ;; different between each debugging sessions, simply save/load
  ;; window configurations doesn't
  ;; work. `gdb-save-window-configuration' and
  ;; `gdb-load-window-configuration' do some tricks to store and
  ;; recreate each buffer in the layout.
  (define-key menu [load-layout] '("Load Layout" "Load GDB window configuration (layout) from a file" . gdb-load-window-configuration))
  (define-key menu [save-layout] '("Save Layout" "Save current GDB window configuration (layout) to a file" . gdb-save-window-configuration))
  (define-key menu [restore-layout-after-quit]
    '(menu-item "Restore Layout After Quit" gdb-toggle-restore-window-configuration
       :button (:toggle . gdb-restore-window-configuration-after-quit)
       :help "Toggle between always restore the window configuration (layout) after GDB quits and never restore.\n You can also change this setting in Customize to conditionally restore."))
  (define-key menu [gdb] '("Gdb" . gdb-display-gdb-buffer))
  (define-key menu [threads] '("Threads" . gdb-display-threads-buffer))
  (define-key menu [memory] '("Memory" . gdb-display-memory-buffer))
  (define-key menu [disassembly]
    '("Disassembly" . gdb-display-disassembly-buffer))
  (define-key menu [registers] '("Registers" . gdb-display-registers-buffer))
  (define-key menu [inferior]
    '("IO" . gdb-display-io-buffer))
  (define-key menu [locals] '("Locals" . gdb-display-locals-buffer))
  (define-key menu [frames] '("Stack" . gdb-display-stack-buffer))
  (define-key menu [breakpoints]
    '("Breakpoints" . gdb-display-breakpoints-buffer)))

(let ((menu (make-sparse-keymap "GDB-Frames")))
  (define-key gud-menu-map [frames]
    `(menu-item "GDB-Frames" ,menu
		:visible (eq gud-minor-mode 'gdbmi)))
  (define-key menu [gdb] '("Gdb" . gdb-frame-gdb-buffer))
  (define-key menu [threads] '("Threads" . gdb-frame-threads-buffer))
  (define-key menu [memory] '("Memory" . gdb-frame-memory-buffer))
  (define-key menu [disassembly]
    '("Disassembly" . gdb-frame-disassembly-buffer))
  (define-key menu [registers] '("Registers" . gdb-frame-registers-buffer))
  (define-key menu [inferior]
    '("IO" . gdb-frame-io-buffer))
  (define-key menu [locals] '("Locals" . gdb-frame-locals-buffer))
  (define-key menu [frames] '("Stack" . gdb-frame-stack-buffer))
  (define-key menu [breakpoints]
    '("Breakpoints" . gdb-frame-breakpoints-buffer)))

(let ((menu (make-sparse-keymap "GDB-MI")))
  (define-key menu [gdb-customize]
    `(menu-item "Customize" ,(lambda () (interactive) (customize-group 'gdb))
      :help "Customize Gdb Graphical Mode options."))
  (define-key menu [gdb-many-windows]
    '(menu-item "Display Other Windows" gdb-many-windows
      :help "Toggle display of locals, stack and breakpoint information"
      :button (:toggle . gdb-many-windows)))
  (define-key menu [sep1]
    '(menu-item "--"))
  (define-key menu [all-threads]
    `(menu-item "GUD controls all threads"
      ,(lambda ()
         (interactive)
         (setq gdb-gud-control-all-threads t))
      :help "GUD start/stop commands apply to all threads"
      :button (:radio . gdb-gud-control-all-threads)))
  (define-key menu [current-thread]
    `(menu-item "GUD controls current thread"
      ,(lambda ()
         (interactive)
         (setq gdb-gud-control-all-threads nil))
      :help "GUD start/stop commands apply to current thread only"
      :button (:radio . (not gdb-gud-control-all-threads))))
  (define-key menu [sep2]
    '(menu-item "--"))
  (define-key menu [gdb-customize-reasons]
    `(menu-item "Customize switching..."
      ,(lambda ()
         (interactive)
         (customize-option 'gdb-switch-reasons))))
  (define-key menu [gdb-switch-when-another-stopped]
    (menu-bar-make-toggle-command
     gdb-toggle-switch-when-another-stopped
     gdb-switch-when-another-stopped
     "Automatically switch to stopped thread"
     "GDB thread switching %s" "Switch to stopped thread"))
  (define-key gud-menu-map [mi]
    `(menu-item "GDB-MI" ,menu :visible (eq gud-minor-mode 'gdbmi))))

;; TODO Fit these into tool-bar-local-item-from-menu call in gud.el.
;; GDB-MI menu will need to be moved to gud.el. We can't use
;; tool-bar-local-item-from-menu here because it appends new buttons
;; to toolbar from right to left while we want our A/T throttle to
;; show up right before Run button.
(define-key-after gud-tool-bar-map [all-threads]
  '(menu-item "Switch to non-stop/A mode" gdb-control-all-threads
    :image (find-image '((:type xpm :file "gud/thread.xpm")))
    :visible (and (eq gud-minor-mode 'gdbmi)
                  gdb-non-stop
                  (not gdb-gud-control-all-threads)))
  'run)

(define-key-after gud-tool-bar-map [current-thread]
  '(menu-item "Switch to non-stop/T mode" gdb-control-current-thread
    :image (find-image '((:type xpm :file "gud/all.xpm")))
    :visible (and (eq gud-minor-mode 'gdbmi)
                  gdb-non-stop
                  gdb-gud-control-all-threads))
  'all-threads)

(defun gdb-frame-gdb-buffer ()
  "Display GUD buffer in another frame."
  (interactive)
  (display-buffer-other-frame gud-comint-buffer))

(defun gdb-display-gdb-buffer ()
  "Display GUD buffer."
  (interactive)
  (pop-to-buffer gud-comint-buffer nil 0))

(defun gdb-set-window-buffer (name &optional ignore-dedicated window)
  "Set buffer of selected window to NAME and dedicate window.

When IGNORE-DEDICATED is non-nil, buffer is set even if selected
window is dedicated."
  (unless window (setq window (selected-window)))
  (when ignore-dedicated
    (set-window-dedicated-p window nil))
  (set-window-buffer window (get-buffer name))
  (set-window-dedicated-p window t))

(defun gdb-toggle-restore-window-configuration ()
  "Toggle whether to restore window configuration when GDB quits."
  (interactive)
  (setq gdb-restore-window-configuration-after-quit
        (not gdb-restore-window-configuration-after-quit)))

(defun gdb-get-source-buffer ()
  "Return a buffer displaying source file or nil if we can't find one.
The source file is the file that contains the source location
where GDB stops.  There could be multiple source files during a
debugging session, we get the most recently showed one.  If
program hasn't started running yet, the source file is the \"main
file\" where the GDB session starts (see `gdb-main-file')."
  (if gud-last-last-frame
      (gud-find-file (car gud-last-last-frame))
    (when gdb-main-file
      (gud-find-file gdb-main-file))))

(defun gdb-setup-windows ()
  "Lay out the window pattern for option `gdb-many-windows'."
  (if gdb-default-window-configuration-file
      (gdb-load-window-configuration
       (if (file-name-absolute-p gdb-default-window-configuration-file)
           gdb-default-window-configuration-file
         (expand-file-name gdb-default-window-configuration-file
                           gdb-window-configuration-directory)))
    ;; Create default layout as before.
    (gdb-get-buffer-create 'gdb-locals-buffer)
    (gdb-get-buffer-create 'gdb-stack-buffer)
    (gdb-get-buffer-create 'gdb-breakpoints-buffer)
    (set-window-dedicated-p (selected-window) nil)
    (switch-to-buffer gud-comint-buffer)
    (delete-other-windows)
    (let ((win0 (selected-window))
          (win1 (split-window nil ( / ( * (window-height) 3) 4)))
          (win2 (split-window nil ( / (window-height) 3)))
          (win3 (split-window-right)))
      (gdb-set-window-buffer (gdb-locals-buffer-name) nil win3)
      (select-window win2)
      (set-window-buffer win2 (or (gdb-get-source-buffer)
                                  (list-buffers-noselect)))
      (setq gdb-source-window-list (list (selected-window)))
      (let ((win4 (split-window-right)))
        (gdb-set-window-buffer
         (gdb-get-buffer-create 'gdb-inferior-io) nil win4))
      (select-window win1)
      (gdb-set-window-buffer (gdb-stack-buffer-name))
      (let ((win5 (split-window-right)))
        (gdb-set-window-buffer (if gdb-show-threads-by-default
                                   (gdb-threads-buffer-name)
                                 (gdb-breakpoints-buffer-name))
                               nil win5))
      (select-window win0))))

(defun gdb-buffer-p (buffer)
  "Return t if BUFFER is GDB-related."
  (with-current-buffer buffer
    (eq gud-minor-mode 'gdbmi)))

(defun gdb-function-buffer-p (buffer)
  "Return t if BUFFER is a GDB function buffer.

Function buffers are locals buffer, registers buffer, etc, but
not including main command buffer (the one where you type GDB
commands) or source buffers (that display program source code)."
  (with-current-buffer buffer
    (derived-mode-p 'gdb-parent-mode 'gdb-inferior-io-mode)))

(defun gdb--buffer-type (buffer)
  "Return the type of BUFFER if it is a function buffer.
Buffer type is like `gdb-registers-type', `gdb-stack-buffer'.
These symbols are used by `gdb-get-buffer-create'.

Return nil if BUFFER is not a GDB function buffer."
  (with-current-buffer buffer
    (cl-loop for rule in gdb-buffer-rules
             for mode-name = (gdb-rules-buffer-mode rule)
             for type = (car rule)
             if (eq mode-name major-mode)
             return type
             finally return nil)))

(defun gdb-save-window-configuration (file)
  "Save current window configuration (layout) to FILE.
You can later restore this configuration from that file by
`gdb-load-window-configuration'."
  (interactive (list (read-file-name
                      "Save window configuration to file: "
                      (or gdb-window-configuration-directory
                          default-directory))))
  ;; We replace the buffer in each window with a placeholder, store
  ;; the buffer type (register, breakpoint, etc) in window parameters,
  ;; and write the window configuration to the file.
  (save-window-excursion
    (let ((placeholder (get-buffer-create " *gdb-placeholder*"))
          (window-persistent-parameters
           (cons '(gdb-buffer-type . writable) window-persistent-parameters)))
      (unwind-protect
          (dolist (win (window-list nil 'no-minibuffer))
            (select-window win)
            (when (gdb-buffer-p (current-buffer))
              (set-window-parameter
               nil 'gdb-buffer-type
               (cond ((gdb-function-buffer-p (current-buffer))
                      ;; 1) If a user arranged the window
                      ;; configuration herself and saves it, windows
                      ;; are probably not dedicated.  2) We use the
                      ;; same dedication flag as in
                      ;; `gdb-display-buffer'.
                      (set-window-dedicated-p nil t)
                      ;; We save this gdb-buffer-type symbol so
                      ;; we can later pass it to `gdb-get-buffer-create';
                      ;; one example: `gdb-registers-buffer'.
                      (or (gdb--buffer-type (current-buffer))
                          (error "Unrecognized gdb buffer mode: %s" major-mode)))
                     ;; Command buffer.
                     ((derived-mode-p 'gud-mode) 'command)
                     ;; Consider everything else as source buffer.
                     (t 'source)))
              (with-window-non-dedicated nil
                (set-window-buffer nil placeholder)
                (set-window-prev-buffers (selected-window) nil)
                (set-window-next-buffers (selected-window) nil))))
        ;; Save the window configuration to FILE.
        (let ((window-config (window-state-get nil t)))
          (with-temp-buffer
            (prin1 window-config (current-buffer))
            (write-file file t)))
        (kill-buffer placeholder)))))

(defun gdb-load-window-configuration (file)
  "Restore window configuration (layout) from FILE.
FILE should be a window configuration file saved by
`gdb-save-window-configuration'."
  (interactive (list (read-file-name
                      "Restore window configuration from file: "
                      (or gdb-window-configuration-directory
                          default-directory))))
  ;; Basically, we restore window configuration and go through each
  ;; window and restore the function buffers.
  (let* ((placeholder (get-buffer-create " *gdb-placeholder*")))
    (unwind-protect ; Don't leak buffer.
        (let ((window-config (with-temp-buffer
                               (insert-file-contents file)
                               ;; We need to go to point-min because
                               ;; `read' reads from point
                               (goto-char (point-min))
                               (read (current-buffer))))
              (source-buffer (or (gdb-get-source-buffer)
                                 ;; Do the same thing as in
                                 ;; `gdb-setup-windows' if no source
                                 ;; buffer is found.
                                 (list-buffers-noselect)))
              buffer-type)
          (window-state-put window-config (frame-root-window))
          (dolist (window (window-list nil 'no-minibuffer))
            (with-selected-window window
              (setq buffer-type (window-parameter nil 'gdb-buffer-type))
              (pcase buffer-type
                ('source (when source-buffer
                           (set-window-buffer nil source-buffer)
                           (push (selected-window) gdb-source-window-list)))
                ('command (switch-to-buffer gud-comint-buffer))
                (_ (let ((buffer (gdb-get-buffer-create buffer-type)))
                     (with-window-non-dedicated nil
                       (set-window-buffer nil buffer))))))))
      (kill-buffer placeholder))))

(define-minor-mode gdb-many-windows
  "If nil just pop up the GUD buffer unless `gdb-show-main' is t.
In this case it starts with two windows: one displaying the GUD
buffer and the other with the source file with the main routine
of the debugged program.  Non-nil means display the layout shown for
`gdb'."
  :global t
  :group 'gdb
  :version "22.1"
  (if (and gud-comint-buffer
           (buffer-name gud-comint-buffer))
      (ignore-errors
        (gdb-restore-windows))))

(defun gdb-restore-windows ()
  "Restore the basic arrangement of windows used by gdb.
This arrangement depends on the values of variable
`gdb-many-windows' and `gdb-default-window-configuration-file'."
  ;; This function is used when the user messed up window
  ;; configuration and wants to "reset to default".  The function that
  ;; sets up window configuration on start up is
  ;; `gdb-get-source-file'.
  (interactive)
  (switch-to-buffer gud-comint-buffer) ;Select the right window and frame.
  (delete-other-windows)
  (if gdb-many-windows
      (gdb-setup-windows)
    (when (or gud-last-last-frame gdb-show-main)
      (let ((win (split-window)))
        (set-window-buffer
         win
         (if gud-last-last-frame
             (gud-find-file (car gud-last-last-frame))
           (gud-find-file gdb-main-file)))
        (setq gdb-source-window-list (list win))))))

;; Called from `gud-sentinel' in gud.el:
(defun gdb-reset ()
  "Exit a debugging session cleanly.
Kills the gdb buffers, and resets variables and the source buffers."
  ;; The gdb-inferior buffer has a pty hooked up to the main gdb
  ;; process.  This pty must be deleted explicitly.
  (let ((pty (get-process "gdb-inferior")))
    (if pty (delete-process pty)))
  ;; Find gdb-mi buffers and kill them.
  (dolist (buffer (buffer-list))
    (unless (eq buffer gud-comint-buffer)
      (with-current-buffer buffer
        (if (eq gud-minor-mode 'gdbmi)
            (if (string-match "\\` ?\\*.+\\*\\'" (buffer-name))
                (kill-buffer nil)
              (gdb-remove-breakpoint-icons (point-min) (point-max) t)
              (setq gud-minor-mode nil)
              (kill-local-variable 'tool-bar-map)
              (kill-local-variable 'gdb-define-alist))))))
  (setq gdb-disassembly-position nil)
  (setq overlay-arrow-variable-list
        (delq 'gdb-disassembly-position overlay-arrow-variable-list))
  (setq fringe-indicator-alist '((overlay-arrow . right-triangle)))
  (setq gdb-stack-position nil)
  (setq overlay-arrow-variable-list
        (delq 'gdb-stack-position overlay-arrow-variable-list))
  (setq gdb-thread-position nil)
  (setq overlay-arrow-variable-list
        (delq 'gdb-thread-position overlay-arrow-variable-list))
  (if (boundp 'speedbar-frame) (speedbar-timer-fn))
  (setq gud-running nil)
  (setq gdb-active-process nil)
  (remove-hook 'after-save-hook 'gdb-create-define-alist t)
  ;; Recover window configuration.
  (when (or (eq gdb-restore-window-configuration-after-quit t)
            (and (eq gdb-restore-window-configuration-after-quit
                     'if-gdb-show-main)
                 gdb-show-main)
            (and (eq gdb-restore-window-configuration-after-quit
                     'if-gdb-many-windows)
                 gdb-many-windows))
    (when gdb--window-configuration-before
      (window-state-put gdb--window-configuration-before)
      ;; This way we don't accidentally restore an outdated window
      ;; configuration.
      (setq gdb--window-configuration-before nil))))

(defun gdb-get-source-file ()
  "Find the source file where the program starts and display it with related
buffers, if required."
  ;; This function is called only once on startup.
  (goto-char (point-min))
  (if (re-search-forward gdb-source-file-regexp nil t)
      (setq gdb-main-file (gdb-mi--c-string-from-string (match-string 1))))
  (if gdb-many-windows
      (gdb-setup-windows)
    (gdb-get-buffer-create 'gdb-breakpoints-buffer)
    (and gdb-show-main
	 gdb-main-file
	 (display-buffer (gud-find-file gdb-main-file))))
  (gdb-force-mode-line-update
   (propertize "ready" 'face font-lock-variable-name-face)))

;;from put-image
(defun gdb-put-string (putstring pos &optional dprop &rest sprops)
  "Put string PUTSTRING in front of POS in the current buffer.
PUTSTRING is displayed by putting an overlay into the current buffer with a
`before-string' string that has a `display' property whose value is
PUTSTRING."
  (let ((string (make-string 1 ?x))
        (buffer (current-buffer)))
    (setq putstring (copy-sequence putstring))
    (let ((overlay (make-overlay pos pos buffer))
          (prop (or dprop
                    (list (list 'margin 'left-margin) putstring))))
      (put-text-property 0 1 'display prop string)
      (if sprops
          (add-text-properties 0 1 sprops string))
      (overlay-put overlay 'put-break t)
      (overlay-put overlay 'before-string string))))

;;from remove-images
(defun gdb-remove-strings (start end &optional buffer)
  "Remove strings between START and END in BUFFER.
Remove only strings that were put in BUFFER with calls to `gdb-put-string'.
BUFFER nil or omitted means use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (dolist (overlay (overlays-in start end))
    (when (overlay-get overlay 'put-break)
      (delete-overlay overlay))))

(defun gdb-put-breakpoint-icon (enabled bptno &optional line)
  (let* ((posns (gdb-line-posns (or line (line-number-at-pos))))
         (start (- (car posns) 1))
         (end (+ (cdr posns) 1))
         (putstring (if enabled "B" "b"))
         (source-window (get-buffer-window (current-buffer) 0)))
    (add-text-properties
     0 1 '(help-echo "mouse-1: clear bkpt, mouse-3: enable/disable bkpt")
     putstring)
    (if enabled
        (add-text-properties
         0 1 `(gdb-bptno ,bptno gdb-enabled t) putstring)
      (add-text-properties
       0 1 `(gdb-bptno ,bptno gdb-enabled nil) putstring))
    (gdb-remove-breakpoint-icons start end)
    (if (display-images-p)
        (if (>= (or left-fringe-width
                    (if source-window (car (window-fringes source-window)))
                    gdb-buffer-fringe-width) 8)
            (gdb-put-string
             nil (1+ start)
             `(left-fringe breakpoint
                           ,(if enabled
                                'breakpoint-enabled
                              'breakpoint-disabled))
             'gdb-bptno bptno
             'gdb-enabled enabled)
          (when (< left-margin-width 2)
            (save-current-buffer
              (setq left-margin-width 2)
              (if source-window
                  (set-window-margins
                   source-window
                   left-margin-width right-margin-width))))
          (put-image
           (if enabled
               (or breakpoint-enabled-icon
                   (setq breakpoint-enabled-icon
                         (find-image `((:type xpm :data
                                        ,breakpoint-xpm-data
                                        :ascent 100 :pointer hand)
                                       (:type pbm :data
                                        ,breakpoint-enabled-pbm-data
                                        :ascent 100 :pointer hand)))))
             (or breakpoint-disabled-icon
                 (setq breakpoint-disabled-icon
                       (find-image `((:type xpm :data
                                      ,breakpoint-xpm-data
                                      :conversion disabled
                                      :ascent 100 :pointer hand)
                                     (:type pbm :data
                                      ,breakpoint-disabled-pbm-data
                                      :ascent 100 :pointer hand))))))
           (+ start 1)
           putstring
           'left-margin))
      (when (< left-margin-width 2)
        (save-current-buffer
          (setq left-margin-width 2)
          (let ((window (get-buffer-window (current-buffer) 0)))
            (if window
                (set-window-margins
                 window left-margin-width right-margin-width)))))
      (gdb-put-string
       (propertize putstring
                   'face (if enabled
                             'breakpoint-enabled 'breakpoint-disabled))
       (1+ start)))))

(defun gdb-remove-breakpoint-icons (start end &optional remove-margin)
  (gdb-remove-strings start end)
  (if (display-images-p)
      (remove-images start end))
  (when remove-margin
    (setq left-margin-width 0)
    (let ((window (get-buffer-window (current-buffer) 0)))
      (if window
          (set-window-margins
           window left-margin-width right-margin-width)))))


;;; Functions for inline completion.

(defvar gud-gdb-fetch-lines-in-progress)
(defvar gud-gdb-fetch-lines-string)
(defvar gud-gdb-fetch-lines-break)
(defvar gud-gdb-fetched-lines)

(defun gud-gdbmi-completions (context command)
  "Completion table for GDB/MI commands.
COMMAND is the prefix for which we seek completion.
CONTEXT is the text before COMMAND on the line."
  (let ((gud-gdb-fetch-lines-in-progress t)
	(gud-gdb-fetch-lines-string nil)
	(gud-gdb-fetch-lines-break (length context))
	(gud-gdb-fetched-lines nil)
	;; This filter dumps output lines to `gud-gdb-fetched-lines'.
	(gud-marker-filter #'gud-gdbmi-fetch-lines-filter))
    (with-current-buffer (gdb-get-buffer 'gdb-partial-output-buffer)
      (gdb-input (concat "complete " context command)
		 (lambda () (setq gud-gdb-fetch-lines-in-progress nil)))
      (while gud-gdb-fetch-lines-in-progress
	(accept-process-output (get-buffer-process gud-comint-buffer))))
    (gud-gdb-completions-1 gud-gdb-fetched-lines)))

(defun gud-gdbmi-fetch-lines-filter (string)
  "Custom filter function for `gud-gdbmi-completions'."
  (setq string (concat gud-gdb-fetch-lines-string
		       (gud-gdbmi-marker-filter string)))
  (while (string-match "\n" string)
    (push (substring string gud-gdb-fetch-lines-break (match-beginning 0))
	  gud-gdb-fetched-lines)
    (setq string (substring string (match-end 0))))
  "")

(provide 'gdb-mi)

;;; gdb-mi.el ends here
