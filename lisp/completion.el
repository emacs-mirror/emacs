;;; completion.el --- dynamic word-completion code

;; Maintainer: bug-completion@think.com
;; Keywords: abbrev

;;; Commentary:

;;; This file is very badly designed in that it redefines
;;; standard functions of Emacs.  This is bad design, because
;;; this file cannot be updated to correspond to the latest
;;; versions of those functions.  Therefore, you must expect
;;; it to produce unpredictable and undesirable results.
;;; This file needs to be redesigned to work in a modular fashion.
;;;  -- rms.

;;; This is a Completion system for GNU Emacs
;;;
;;;  E-Mail: 
;;;   Internet: completion@think.com, bug-completion@think.com
;;;   UUCP: {rutgers,harvard,mit-eddie}!think!completion
;;;
;;;    If you are a new user, we'd appreciate knowing your site name and
;;; any comments you have.
;;;
;;;
;;;				NO WARRANTY
;;;
;;; This software is distributed free of charge and is in the public domain.
;;; Anyone may use, duplicate or modify this program.  Thinking Machines
;;; Corporation does not restrict in any way the use of this software by
;;; anyone.
;;; 
;;; Thinking Machines Corporation provides absolutely no warranty of any kind.
;;; The entire risk as to the quality and performance of this program is with
;;; you.  In no event will Thinking Machines Corporation be liable to you for
;;; damages, including any lost profits, lost monies, or other special,
;;; incidental or consequential damages arising out of the use of this program.
;;;
;;; You must not restrict the distribution of this software.
;;;
;;; Please keep this notice and author information in any copies you make.
;;;
;;; 4/90
;;;
;;;
;;; Advertisement
;;;---------------
;;;  Try using this.  If you are like most you will be happy you did.
;;; 
;;; What to put in .emacs
;;;-----------------------
;;; (load "completion") ;; If it's not part of the standard band.
;;; (initialize-completions)
;;; 
;;; For best results, be sure to byte-compile the file first.
;;;

;;; Authors 
;;;---------
;;;     Jim Salem      {salem@think.com}
;;;     Brewster Kahle {brewster@think.com}
;;;  Thinking Machines Corporation
;;;  245 First St., Cambridge MA 02142 (617) 876-1111
;;;
;;; Mailing Lists
;;;---------------
;;;
;;; Bugs to bug-completion@think.com
;;; Comments to completion@think.com
;;; Requests to be added completion-request@think.com
;;;
;;; Availability
;;;--------------
;;; Anonymous FTP from think.com
;;;

;;;---------------------------------------------------------------------------
;;; Documentation [Slightly out of date]
;;;---------------------------------------------------------------------------
;;;  (also check the documentation string of the functions)
;;;
;;; Introduction
;;;---------------
;;;  
;;;     After you type a few characters, pressing the "complete" key inserts
;;; the rest of the word you are likely to type.  
;;;
;;; This watches all the words that you type and remembers them.  When 
;;; typing a new word, pressing "complete" (meta-return) "completes" the
;;; word by inserting the most recently used word that begins with the 
;;; same characters.  If you press meta-return repeatedly, it cycles
;;; through all the words it knows about.
;;;
;;;  If you like the completion then just continue typing, it is as if you
;;; entered the text by hand.  If you want the inserted extra characters 
;;; to go away, type control-w or delete.  More options are described below.
;;;
;;;  The guesses are made in the order of the most recently "used".  Typing
;;; in a word and then typing a separator character (such as a space) "uses" 
;;; the word.  So does moving a cursor over the word.  If no words are found, 
;;; it uses an extended version of the dabbrev style completion.
;;;
;;;   You automatically save the completions you use to a file between 
;;; sessions.  
;;;
;;;   Completion enables programmers to enter longer, more descriptive 
;;; variable names while typing fewer keystrokes than they normally would.
;;;
;;;
;;; Full documentation
;;;---------------------
;;;
;;;   A "word" is any string containing characters with either word or symbol 
;;; syntax.  [E.G. Any alphanumeric string with hypens, underscores, etc.]
;;; Unless you change the constants, you must type at least three characters
;;; for the word to be recognized.  Only words longer than 6 characters are
;;; saved.
;;;
;;;   When you load this file, completion will be on.  I suggest you use the
;;; compiled version (because it is noticibly faster).
;;;
;;;  M-X completion-mode toggles whether or not new words are added to the
;;; database by changing the value of *completep*.
;;;
;;;  SAVING/LOADING COMPLETIONS
;;;   Completions are automatically saved from one session to another
;;; (unless *save-completions-p* or *completep* is nil).
;;; Loading this file (or calling initialize-completions) causes EMACS
;;; to load a completions database for a saved completions file 
;;; (default: ~/.completions).  When you exit, EMACS saves a copy of the
;;; completions that you 
;;; often use.  When you next start, EMACS loads in the saved completion file.
;;;
;;;   The number of completions saved depends loosely on 
;;; *saved-completions-decay-factor*.  Completions that have never been 
;;; inserted via "complete" are not saved.  You are encouraged to experiment
;;; with different functions (see compute-completion-min-num-uses).
;;;
;;;   Some completions are permanent and are always saved out.  These 
;;; completions have their num-uses slot set to T.  Use 
;;; add-permanent-completion to do this
;;;
;;;   Completions are saved only if *completep* is T.  The number of old
;;; versions kept of the saved completions file is controlled by 
;;; *completion-file-versions-kept*.
;;;
;;; COMPLETE KEY OPTIONS
;;;   The complete function takes a numeric arguments.  
;;;  control-u :: leave the point at the beginning of the completion rather 
;;;               than the middle.
;;;  a number  :: rotate through the possible completions by that amount
;;;  `-'       :: same as -1 (insert previous completion)
;;;
;;; HOW THE DATABASE IS MAINTAINED
;;;  <write>
;;;
;;; UPDATING THE DATABASE MANUALLY
;;;   m-x kill-completion 
;;;     kills the completion at point.
;;;   m-x add-completion
;;;   m-x add-permanent-completion
;;;   
;;; UPDATING THE DATABASE FROM A SOURCE CODE FILE
;;;   m-x add-completions-from-buffer
;;;     Parses all the definition names from a C or LISP mode buffer and
;;;     adds them to the completion database.
;;;
;;;   m-x add-completions-from-lisp-file 
;;;     Parses all the definition names from a C or Lisp mode file and
;;;     adds them to the completion database.
;;;
;;; UPDATING THE DATABASE FROM A TAGS TABLE
;;;   m-x add-completions-from-tags-table
;;;     Adds completions from the current tags-table-buffer.
;;;
;;; HOW A COMPLETION IS FOUND
;;;  <write>
;;;
;;; STRING CASING
;;;   Completion is string case independent if case-fold-search has its 
;;;  normal default of T.  Also when the completion is inserted the case of the
;;;  entry is coerced appropriately.  
;;;  [E.G.  APP --> APPROPRIATELY     app --> appropriately  
;;;         App --> Appropriately]
;;;
;;; INITIALIZATION
;;;  The form `(initialize-completions)' initializes the completion system by 
;;; trying to load in the user's completions.  After the first cal, further 
;;; calls have no effect so one should be careful not to put the form in a 
;;; site's standard site-init file.
;;;
;;;---------------------------------------------------------------------------
;;;
;;;

;;;-----------------------------------------------
;;; Porting Notes
;;;-----------------------------------------------
;;;
;;;  Should run on 18.49, 18.52, and 19.0
;;;  Tested on vanilla version.
;;;  This requires the standard cl.el file.  It could easily rewritten to not 
;;; require it.  It defines remove which is not in cl.el.
;;;
;;;  FUNCTIONS BASHED
;;; The following functions are bashed but it is done carefully and should not
;;; cause problems ::
;;;   kill-region, next-line, previous-line, newline, newline-and-indent,
;;;   kill-emacs
;;;
;;;
;;;---------------------------------------------------------------------------
;;; Functions you might like to call
;;;---------------------------------------------------------------------------
;;;
;;;  add-completion  string &optional num-uses
;;;    Adds a new string to the database
;;;
;;;  add-permanent-completion  string
;;;    Adds a new string to the database with num-uses = T
;;;

;;;  kill-completion string
;;;    Kills the completion from the database.
;;;
;;;  clear-all-completions
;;;    Clears the database
;;;
;;;  list-all-completions
;;;    Returns a list of all completions.
;;;
;;;
;;;  next-completion string &optional index
;;;    Returns a completion entry that starts with string.
;;;
;;;  find-exact-completion string
;;;    Returns a completion entry that exactly matches string.
;;;
;;;  complete
;;;    Inserts a completion at point
;;;
;;;  initialize-completions
;;;    Loads the completions file and sets up so that exiting emacs will 
;;;  save them.
;;;
;;;  save-completions-to-file &optional filename  
;;;  load-completions-from-file &optional filename
;;;
;;;-----------------------------------------------
;;; Other functions
;;;-----------------------------------------------
;;;
;;;  get-completion-list string
;;;
;;; These things are for manipulating the structure
;;;  make-completion string num-uses
;;;  completion-num-uses completion 
;;;  completion-string completion
;;;  set-completion-num-uses completion num-uses
;;;  set-completion-string completion string
;;;  
;;;

;;;-----------------------------------------------
;;; To Do :: (anybody ?)
;;;-----------------------------------------------
;;;
;;;   Implement Lookup and keyboard interface in C
;;;   Add package prefix smarts (for Common Lisp)
;;;   Add autoprompting of possible completions after every keystroke (fast
;;;      terminals only !)
;;;   Add doc. to texinfo
;;;
;;;
;;;-----------------------------------------------
;;; Change Log:
;;;-----------------------------------------------
;;;    Sometime in '84 Brewster implemented a somewhat buggy version for 
;;; Symbolics LISPMs.
;;;    Jan. '85 Jim became enamored of the idea and implemented a faster, 
;;; more robust version.
;;;    With input from many users at TMC, (rose, craig, and gls come to mind),
;;; the current style of interface was developed. 
;;;    9/87, Jim and Brewster took terminals home.  Yuck.  After 
;;; complaining for a while Brewester implemented a subset of the current 
;;; LISPM version for GNU Emacs.  
;;;    8/88  After complaining for a while (and with sufficient 
;;; promised rewards), Jim reimplemented a version of GNU completion
;;; superior to that of the LISPM version.
;;;
;;;-----------------------------------------------
;;; Acknowlegements
;;;-----------------------------------------------
;;;  Cliff Lasser (cal@think.com), Kevin Herbert (kph@cisco.com),
;;;  eero@media-lab, kgk@cs.brown.edu, jla@ai.mit.edu,
;;;
;;;-----------------------------------------------
;;; Change Log
;;;-----------------------------------------------
;;; From version 9 to 10
;;;  - Allowance for non-integral *completion-version* nos.
;;;  - Fix cmpl-apply-as-top-level for keyboard macros
;;;  - Fix broken completion merging (in save-completions-to-file)
;;;  - More misc. fixes for version 19.0 of emacs
;;;
;;; From Version 8 to 9
;;;  - Ported to version 19.0 of emacs (backcompatible with version 18)
;;;  - Added add-completions-from-tags-table (with thanks to eero@media-lab)
;;;
;;; From Version 7 to 8
;;;  - Misc. changes to comments
;;;  - new completion key bindings: c-x o, M->, M-<, c-a, c-e
;;;  - cdabbrev now checks all the visible window buffers and the "other buffer"
;;;  - `%' is now a symbol character rather than a separator (except in C mode)
;;;
;;; From Version 6 to 7
;;;  - Fixed bug with saving out .completion file the first time
;;;
;;; From Version 5 to 6
;;;  - removed statistics recording
;;;  - reworked advise to handle autoloads
;;;  - Fixed fortran mode support
;;;  - Added new cursor motion triggers
;;;
;;; From Version 4 to 5
;;;  - doesn't bother saving if nothing has changed
;;;  - auto-save if haven't used for a 1/2 hour
;;;  - save period extended to two weeks
;;;  - minor fix to capitalization code
;;;  - added *completion-auto-save-period* to variables recorded.
;;;  - added reenter protection to cmpl-record-statistics-filter
;;;  - added backup protection to save-completions-to-file (prevents 
;;;    problems with disk full errors)

;;; Code:

;;;-----------------------------------------------
;;; Requires
;;; Version
;;;-----------------------------------------------

;;(require 'cl) ;; DOTIMES, etc. {actually done after variable defs.}

(defconst *completion-version*  10
  "Tested for EMACS versions 18.49, 18.52, 18.55 and beyond and 19.0.")

;;;---------------------------------------------------------------------------
;;; User changeable parameters
;;;---------------------------------------------------------------------------

(defvar *completep* t
  "*Set to nil to turn off the completion hooks.
(No new words added to the database or saved to the init file).")

(defvar *save-completions-p* t
  "*If non-nil, the most useful completions are saved to disk when
exiting EMACS.  See *saved-completions-decay-factor*.")

(defvar *saved-completions-filename* "~/.completions"
  "*The filename to save completions to.")

(defvar *saved-completion-retention-time* 336
  "*The maximum amount of time to save a completion for if it has not been used.
In hours.  (1 day = 24, 1 week = 168).  If this is 0, non-permanent completions
will not be saved unless these are used.  Default is two weeks.")

(defvar *separator-character-uses-completion-p* nil
  "*If non-nil, typing a separator character after a completion symbol that
is not part of the database marks it as used (so it will be saved).")

(defvar *completion-file-versions-kept* kept-new-versions
  "*Set this to the number of versions you want save-completions-to-file
to keep.")

(defvar *print-next-completion-speed-threshold* 4800
  "*The baud rate at or above which to print the next potential completion
after inserting the current one."
  )

(defvar *print-next-completion-does-cdabbrev-search-p* nil
  "*If non-nil, the next completion prompt will also do a cdabbrev search.
This can be time consuming.")

(defvar *cdabbrev-radius* 15000
  "*How far to search for cdabbrevs.  In number of characters.  If nil, the 
whole buffer is searched.")

(defvar *modes-for-completion-find-file-hook* '(lisp c)
  "*A list of modes {either C or Lisp}.  Definitions from visited files
of those types are automatically added to the completion database.")

(defvar *record-cmpl-statistics-p* nil
  "*If non-nil, statistics are automatically recorded.")

(defvar *completion-auto-save-period* 1800
  "*The period in seconds to wait for emacs to be idle before autosaving
the completions.  Default is a 1/2 hour.")

(defconst *completion-min-length* nil ;; defined below in eval-when
  "*The minimum length of a stored completion.
DON'T CHANGE WITHOUT RECOMPILING !  This is used by macros.")

(defconst *completion-max-length* nil ;; defined below in eval-when
  "*The maximum length of a stored completion.
DON'T CHANGE WITHOUT RECOMPILING !  This is used by macros.")

(defconst *completion-prefix-min-length* nil ;; defined below in eval-when
  "The minimum length of a completion search string.
DON'T CHANGE WITHOUT RECOMPILING !  This is used by macros.")

(defmacro eval-when-compile-load-eval (&rest body)
  ;; eval everything before expanding
  (mapcar 'eval body)
  (cons 'progn body))

(defun completion-eval-when ()
  (eval-when-compile-load-eval
   ;; These vars. are defined at both compile and load time.
   (setq *completion-min-length* 6)
   (setq *completion-max-length* 200)
   (setq *completion-prefix-min-length* 3)
   ;; Need this file around too
   (require 'cl)))

(completion-eval-when)
 
;;;---------------------------------------------------------------------------
;;; Internal Variables
;;;---------------------------------------------------------------------------

(defvar cmpl-initialized-p nil
  "Set to t when the completion system is initialized.  Indicates that the
old completion file has been read in.")

(defvar cmpl-completions-accepted-p nil
  "Set to T as soon as the first completion has been accepted.  Used to
decide whether to save completions.")


;;;---------------------------------------------------------------------------
;;; Low level tools
;;;---------------------------------------------------------------------------

;;;-----------------------------------------------
;;; Misc.
;;;-----------------------------------------------

(defun remove (item list)
  (setq list (copy-sequence list))
  (delq item list))

(defun minibuffer-window-selected-p ()
  "True iff the current window is the minibuffer."
  (eq (minibuffer-window) (selected-window)))

(eval-when-compile-load-eval
(defun function-needs-autoloading-p (symbol)
  ;; True iff symbol is represents an autoloaded function and has not yet been
  ;; autoloaded.
  (and (listp (symbol-function symbol))
       (eq 'autoload (car (symbol-function symbol)))
       )))

(defun function-defined-and-loaded (symbol)
  ;; True iff symbol is bound to a loaded function.
  (and (fboundp symbol) (not (function-needs-autoloading-p symbol))))

(defmacro read-time-eval (form)
  ;; Like the #. reader macro
  (eval form))

;;;-----------------------------------------------
;;; Emacs Version 19 compatibility
;;;-----------------------------------------------

(defconst emacs-is-version-19 (string= (substring emacs-version 0 2) "19"))

(defun cmpl19-baud-rate ()
  (if emacs-is-version-19
      baud-rate
      (baud-rate)))

(defun cmpl19-sit-for (amount)
  (if (and emacs-is-version-19 (= amount 0))
      (sit-for 1 t)
      (sit-for amount)))

;;;-----------------------------------------------
;;; Advise
;;;-----------------------------------------------

(defmacro completion-advise (function-name where &rest body)
  "Adds the body code before calling function.  This advise is not compiled.
WHERE is either :BEFORE or :AFTER."
  (completion-advise-1 function-name where body)
  )

(defmacro cmpl-apply-as-top-level (function arglist)
  "Calls function-name interactively if inside a call-interactively."
  (list 'cmpl-apply-as-top-level-1 function arglist
	'(let ((executing-macro nil)) (interactive-p)))
  )

(defun cmpl-apply-as-top-level-1 (function arglist interactive-p)
  (if (and interactive-p (commandp function))
      (call-interactively function)
      (apply function arglist)
      ))

(eval-when-compile-load-eval

(defun cmpl-defun-preamble (function-name)
  (let ((doc-string
	 (condition-case e
	      ;; This condition-case is here to stave
	      ;; off bizarre load time errors 18.52 gets
	      ;; on the function c-mode
	      (documentation function-name)
	    (error nil)))
	(interactivep (commandp function-name))
	)
    (append
     (if doc-string (list doc-string))
     (if interactivep '((interactive)))
     )))

(defun completion-advise-1 (function-name where body &optional new-name)
  (unless new-name (setq new-name function-name))
  (let ((quoted-name (list 'quote function-name))
	(quoted-new-name (list 'quote new-name))
	)

  (cond ((function-needs-autoloading-p function-name)
	 (list* 'defun function-name '(&rest arglist)
		(append
		 (cmpl-defun-preamble function-name)
		 (list (list 'load (second (symbol-function function-name)))
		       (list 'eval
			     (list 'completion-advise-1 quoted-name
				   (list 'quote where) (list 'quote body)
				   quoted-new-name))
		       (list 'cmpl-apply-as-top-level quoted-new-name 'arglist)
		       )))
	 )
	(t
	 (let ((old-def-name
		(intern (concat "$$$cmpl-" (symbol-name function-name))))
	       )
    
	   (list 'progn
		 (list 'defvar old-def-name
		       (list 'symbol-function quoted-name))
		 (list* 'defun new-name '(&rest arglist)
			(append
			 (cmpl-defun-preamble function-name)
			 (ecase where
			   (:before
			    (list (cons 'progn body)
				  (list 'cmpl-apply-as-top-level
					old-def-name 'arglist)))
			   (:after
			    (list* (list 'cmpl-apply-as-top-level
					 old-def-name 'arglist)
				   body)
			    )))
			)))
	 ))))
) ;; eval-when
			 

;;;-----------------------------------------------
;;; String case coercion
;;;-----------------------------------------------

(defun cmpl-string-case-type (string)
  "Returns :capitalized, :up, :down, :mixed, or :neither."
  (let ((case-fold-search nil))
    (cond ((string-match "[a-z]" string)
	   (cond ((string-match "[A-Z]" string)
		  (cond ((and (> (length string) 1)
			      (null (string-match "[A-Z]" string 1)))
			 ':capitalized)
			(t
			 ':mixed)))
		 (t ':down)))
	  (t
	   (cond ((string-match "[A-Z]" string)
		  ':up)
		 (t ':neither))))
    ))

;;; Tests -
;;; (cmpl-string-case-type "123ABCDEF456") --> :up
;;; (cmpl-string-case-type "123abcdef456") --> :down
;;; (cmpl-string-case-type "123aBcDeF456") --> :mixed
;;; (cmpl-string-case-type "123456")       --> :neither
;;; (cmpl-string-case-type "Abcde123")     --> :capitalized

(defun cmpl-coerce-string-case (string case-type)
  (cond ((eq case-type ':down) (downcase string))
	((eq case-type ':up) (upcase string))
	((eq case-type ':capitalized)
	 (setq string (downcase string))
	 (aset string 0 (logand ?\337 (aref string 0)))
	 string)
	(t string)
	))

(defun cmpl-merge-string-cases (string-to-coerce given-string)
  (let ((string-case-type (cmpl-string-case-type string-to-coerce))
	)
    (cond ((memq string-case-type '(:down :up :capitalized))
	   ;; Found string is in a standard case.  Coerce to a type based on
	   ;; the given string
	   (cmpl-coerce-string-case string-to-coerce
			       (cmpl-string-case-type given-string))
	   )
	  (t
	   ;; If the found string is in some unusual case, just insert it
	   ;; as is
	   string-to-coerce)
	  )))

;;; Tests -
;;; (cmpl-merge-string-cases "AbCdEf456" "abc")     --> AbCdEf456
;;; (cmpl-merge-string-cases "abcdef456" "ABC")     --> ABCDEF456
;;; (cmpl-merge-string-cases "ABCDEF456" "Abc")     --> Abcdef456
;;; (cmpl-merge-string-cases "ABCDEF456" "abc")     --> abcdef456


;;;-----------------------------------------------
;;; Emacs Idle Time hooks
;;;-----------------------------------------------

(defvar cmpl-emacs-idle-process nil)

(defvar cmpl-emacs-idle-interval 150
  "Seconds between running the Emacs idle process.")

(defun init-cmpl-emacs-idle-process ()
  "Initialize the emacs idle process."
  (let ((live (and cmpl-emacs-idle-process
		   (eq (process-status cmpl-emacs-idle-process) 'run)))
	;; do not allocate a pty
	(process-connection-type nil))
    (if live
	(kill-process cmpl-emacs-idle-process))
    (if cmpl-emacs-idle-process
	(delete-process cmpl-emacs-idle-process))
    (setq cmpl-emacs-idle-process
	  (start-process "cmpl-emacs-idle" nil
			 "loadst" 
			 "-n" (int-to-string cmpl-emacs-idle-interval)))
    (process-kill-without-query cmpl-emacs-idle-process)
    (set-process-filter cmpl-emacs-idle-process 'cmpl-emacs-idle-filter)
    ))

(defvar cmpl-emacs-buffer nil)
(defvar cmpl-emacs-point 0)
(defvar cmpl-emacs-last-command nil)
(defvar cmpl-emacs-last-command-char nil)
(defun cmpl-emacs-idle-p ()
  ;; returns T if emacs has been idle
  (if (and (eq cmpl-emacs-buffer (current-buffer))
	   (= cmpl-emacs-point (point))
	   (eq cmpl-emacs-last-command last-command)
	   (eq last-command-char last-command-char)
	   )
      t ;; idle
      ;; otherwise, update count
      (setq cmpl-emacs-buffer (current-buffer))
      (setq cmpl-emacs-point (point))
      (setq cmpl-emacs-last-command last-command)
      (setq last-command-char last-command-char)
      nil
      ))
	   
(defvar cmpl-emacs-idle-time 0
  "The idle time of Emacs in seconds.")

(defvar inside-cmpl-emacs-idle-filter nil)
(defvar cmpl-emacs-idle-time-hooks nil)

(defun cmpl-emacs-idle-filter (proc string)
  ;; This gets called every cmpl-emacs-idle-interval seconds
  ;; Update idle time clock
  (if (cmpl-emacs-idle-p)
      (incf cmpl-emacs-idle-time cmpl-emacs-idle-interval)
      (setq cmpl-emacs-idle-time 0))

  (unless inside-cmpl-emacs-idle-filter
    ;; Don't reenter if we are hung

    (setq inside-cmpl-emacs-idle-filter t)
  
    (dolist (function cmpl-emacs-idle-time-hooks)
      (condition-case e
	   (funcall function)
	 (error nil)
	 ))
    (setq inside-cmpl-emacs-idle-filter nil)
    ))


;;;-----------------------------------------------
;;; Time 
;;;-----------------------------------------------
;;; What a backwards way to get the time!  Unfortunately, GNU Emacs
;;; doesn't have an accessible time function.

(defconst cmpl-hours-per-day  24)
(defconst cmpl-hours-per-year  (* 365 cmpl-hours-per-day))
(defconst cmpl-hours-per-4-years  (+ (* 4 cmpl-hours-per-year)
				    cmpl-hours-per-day))
(defconst cmpl-days-since-start-of-year
    '(0 31 59 90 120 151 181 212 243 273 304 334))
(defconst cmpl-days-since-start-of-leap-year
    '(0 31 60 91 121 152 182 213 244 274 305 335))
(defconst cmpl-months
    '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun cmpl-hours-since-1900-internal (month day year hours)
  "Month is an integer from 1 to 12.  Year is a two digit integer (19XX)"
  (+ ;; Year
    (* (/ (1- year) 4) cmpl-hours-per-4-years)
    (* (1+ (mod (1- year) 4)) cmpl-hours-per-year)
    ;; minus two to account for  1968 rather than 1900
    ;; month
    (* cmpl-hours-per-day
       (nth (1- month) (if (zerop (mod year 4))
			   cmpl-days-since-start-of-leap-year
			   cmpl-days-since-start-of-year)))
    (* (1- day) cmpl-hours-per-day)
    hours))

(defun cmpl-month-from-string (month-string)
  "Month string is a three char. month string"
  (let ((count 1))
    (do ((list cmpl-months (cdr list))
	 )
	((or (null list) (string-equal month-string (car list))))
      (setq count (1+ count)))
    (if (> count 12)
	(error "Unknown month - %s" month-string))
    count))

(defun cmpl-hours-since-1900 (&optional time-string)
  "String is a string in the format of current-time-string (the default)."
  (let* ((string (or time-string (current-time-string)))
	 (month (cmpl-month-from-string (substring string 4 7)))
	 (day (string-to-int (substring string 8 10)))
	 (year (string-to-int (substring string 22 24)))
	 (hour (string-to-int (substring string 11 13)))
	 )
    (cmpl-hours-since-1900-internal month day year hour)))

;;; Tests -
;;;(cmpl-hours-since-1900 "Wed Jan  1 00:00:28 1900") -->  35040
;;;(cmpl-hours-since-1900 "Wed Nov  2 23:00:28 1988") --> 778751
;;;(cmpl-hours-since-1900 "Wed Jan 23 14:34:28 1988") --> 771926
;;;(cmpl-hours-since-1900 "Wed Feb 23 14:34:28 1988") --> 772670
;;;(cmpl-hours-since-1900 "Wed Mar 23 14:34:28 1988") --> 773366
;;;(cmpl-hours-since-1900 "Wed Apr 23 14:34:28 1988") --> 774110
;;;(cmpl-hours-since-1900 "Wed May 23 14:34:28 1988") --> 774830
;;;(cmpl-hours-since-1900 "Wed Jun 23 14:34:28 1988") --> 775574
;;;(cmpl-hours-since-1900 "Wed Jul 23 14:34:28 1988") --> 776294
;;;(cmpl-hours-since-1900 "Wed Aug 23 14:34:28 1988") --> 777038
;;;(cmpl-hours-since-1900 "Wed Sep 23 14:34:28 1988") --> 777782
;;;(cmpl-hours-since-1900 "Wed Oct 23 14:34:28 1988") --> 778502
;;;(cmpl-hours-since-1900 "Wed Nov 23 14:34:28 1988") --> 779246
;;;(cmpl-hours-since-1900 "Wed Dec 23 14:34:28 1988") --> 779966
;;;(cmpl-hours-since-1900 "Wed Jan 23 14:34:28 1957") --> 500198
;;;(cmpl-hours-since-1900 "Wed Feb 23 14:34:28 1957") --> 500942
;;;(cmpl-hours-since-1900 "Wed Mar 23 14:34:28 1957") --> 501614
;;;(cmpl-hours-since-1900 "Wed Apr 23 14:34:28 1957") --> 502358
;;;(cmpl-hours-since-1900 "Wed May 23 14:34:28 1957") --> 503078
;;;(cmpl-hours-since-1900 "Wed Jun 23 14:34:28 1957") --> 503822
;;;(cmpl-hours-since-1900 "Wed Jul 23 14:34:28 1957") --> 504542
;;;(cmpl-hours-since-1900 "Wed Aug 23 14:34:28 1957") --> 505286
;;;(cmpl-hours-since-1900 "Wed Sep 23 14:34:28 1957") --> 506030
;;;(cmpl-hours-since-1900 "Wed Oct 23 14:34:28 1957") --> 506750
;;;(cmpl-hours-since-1900 "Wed Nov 23 14:34:28 1957") --> 507494
;;;(cmpl-hours-since-1900 "Wed Dec 23 14:34:28 1957") --> 508214


;;;---------------------------------------------------------------------------
;;; "Symbol" parsing functions
;;;---------------------------------------------------------------------------
;;; The functions symbol-before-point, symbol-under-point, etc. quickly return
;;; an appropriate symbol string.  The strategy is to temporarily change
;;; the syntax table to enable fast symbol searching.  There are three classes
;;; of syntax in these "symbol" syntax tables ::
;;;
;;; syntax (?_) - "symbol" chars (e.g. alphanumerics)
;;; syntax (?w) - symbol chars to ignore at end of words (e.g. period).  
;;; syntax (? ) - everything else
;;;
;;; Thus by judicious use of scan-sexps and forward-word, we can get
;;; the word we want relatively fast and without consing.  
;;;
;;; Why do we need a separate category for "symbol chars to ignore at ends" ?
;;; For example, in LISP we want starting :'s trimmed 
;;; so keyword argument specifiers also define the keyword completion.  And,
;;; for example, in C we want `.' appearing in a structure ref. to
;;; be kept intact in order to store the whole structure ref.; however, if 
;;; it appears at the end of a symbol it should be discarded because it is
;;; probably used as a period.

;;; Here is the default completion syntax ::
;;; Symbol chars :: A-Z a-z 0-9 @ / \ * + ~ $ < > %
;;; Symbol chars to ignore at ends :: _ : . -
;;; Separator chars. :: <tab> <space> ! ^ & ( ) = ` | { } [ ] ; " ' #
;;;                     , ? <Everything else>

;;; Mode specific differences and notes ::
;;;  LISP diffs ->
;;;    Symbol chars :: ! & ? = ^
;;;
;;; C diffs ->
;;;   Separator chars :: + * / : %
;;;  A note on the hypen (`-').  Perhaps, the hypen should also be a separator
;;; char., however, we wanted to have completion symbols include pointer 
;;; references.  For example, "foo->bar" is a symbol as far as completion is
;;; concerned.
;;;
;;; FORTRAN diffs ->
;;;   Separator chars :: + - * / :
;;;
;;; Pathname diffs ->
;;;   Symbol chars :: .
;;;  Of course there is no pathname "mode" and in fact we have not implemented
;;; this table.  However, if there was such a mode, this is what it would look
;;; like.

;;;-----------------------------------------------
;;; Table definitions
;;;-----------------------------------------------

(defun make-standard-completion-syntax-table ()
  (let ((table (make-vector 256 0)) ;; default syntax is whitespace
	)
    ;; alpha chars
    (dotimes (i 26)
      (modify-syntax-entry (+ ?a i) "_" table)
      (modify-syntax-entry (+ ?A i) "_" table))
    ;; digit chars.
    (dotimes (i 10)
      (modify-syntax-entry (+ ?0 i) "_" table))
    ;; Other ones
    (let ((symbol-chars '(?@ ?/ ?\\ ?* ?+ ?~ ?$ ?< ?> ?%))
	  (symbol-chars-ignore '(?_ ?- ?: ?.))
	  )
      (dolist (char symbol-chars)
	(modify-syntax-entry char "_" table))
      (dolist (char symbol-chars-ignore)
	(modify-syntax-entry char "w" table)
	)
      )
    table))

(defconst cmpl-standard-syntax-table (make-standard-completion-syntax-table))

(defun make-lisp-completion-syntax-table ()
  (let ((table (copy-syntax-table cmpl-standard-syntax-table))
	(symbol-chars '(?! ?& ?? ?= ?^))
	)
    (dolist (char symbol-chars)
      (modify-syntax-entry char "_" table))
    table))
	   
(defun make-c-completion-syntax-table ()
  (let ((table (copy-syntax-table cmpl-standard-syntax-table))
	(separator-chars '(?+ ?* ?/ ?: ?%))
	)
    (dolist (char separator-chars)
      (modify-syntax-entry char " " table))
    table))

(defun make-fortran-completion-syntax-table ()
  (let ((table (copy-syntax-table cmpl-standard-syntax-table))
	(separator-chars '(?+ ?- ?* ?/ ?:))
	)
    (dolist (char separator-chars)
      (modify-syntax-entry char " " table))
    table))

(defconst cmpl-lisp-syntax-table       (make-lisp-completion-syntax-table))
(defconst cmpl-c-syntax-table          (make-c-completion-syntax-table))
(defconst cmpl-fortran-syntax-table    (make-fortran-completion-syntax-table))

(defvar cmpl-syntax-table cmpl-standard-syntax-table
  "This variable holds the current completion syntax table.")
(make-variable-buffer-local 'cmpl-syntax-table)

;;;-----------------------------------------------
;;; Installing the appropriate mode tables
;;;-----------------------------------------------

(completion-advise lisp-mode-variables :after
  (setq cmpl-syntax-table cmpl-lisp-syntax-table)
  )

(completion-advise c-mode :after
  (setq cmpl-syntax-table cmpl-c-syntax-table)
  )

(completion-advise fortran-mode :after
  (setq cmpl-syntax-table cmpl-fortran-syntax-table)
  (completion-setup-fortran-mode)
  )

;;;-----------------------------------------------
;;; Symbol functions
;;;-----------------------------------------------
(defvar cmpl-symbol-start nil
  "Set to the first character of the symbol after one of the completion
symbol functions is called.")
(defvar cmpl-symbol-end nil
  "Set to the last character of the symbol after one of the completion
symbol functions is called.")
;;; These are temp. vars. we use to avoid using let.
;;;   Why ?  Small speed improvement.
(defvar cmpl-saved-syntax nil)
(defvar cmpl-saved-point nil)

(defun symbol-under-point ()
  "Returns the symbol that the point is currently on if it is longer
than *completion-min-length*."
  (setq cmpl-saved-syntax (syntax-table))
  (set-syntax-table cmpl-syntax-table)
  (cond 
  ;; Cursor is on following-char and after preceding-char
    ((memq (char-syntax (following-char)) '(?w ?_))     
     (setq cmpl-saved-point (point)
	   cmpl-symbol-start (scan-sexps (1+ cmpl-saved-point) -1)
	   cmpl-symbol-end (scan-sexps cmpl-saved-point 1))
     ;; remove chars to ignore at the start
     (cond ((= (char-syntax (char-after cmpl-symbol-start)) ?w)
	    (goto-char cmpl-symbol-start)
	    (forward-word 1)
	    (setq cmpl-symbol-start (point))
	    (goto-char cmpl-saved-point)
	    ))
     ;; remove chars to ignore at the end
     (cond ((= (char-syntax (char-after (1- cmpl-symbol-end))) ?w)
	    (goto-char cmpl-symbol-end)
	    (forward-word -1)
	    (setq cmpl-symbol-end (point))
	    (goto-char cmpl-saved-point)
	    ))
     ;; restore state
     (set-syntax-table cmpl-saved-syntax)
     ;; Return completion if the length is reasonable
     (if (and (<= (read-time-eval *completion-min-length*)
		  (- cmpl-symbol-end cmpl-symbol-start))
	      (<= (- cmpl-symbol-end cmpl-symbol-start)
		  (read-time-eval *completion-max-length*)))
	 (buffer-substring cmpl-symbol-start cmpl-symbol-end))
     )
    (t 
     ;; restore table if no symbol
     (set-syntax-table cmpl-saved-syntax)
     nil)
    ))

;;; tests for symbol-under-point
;;;  `^' indicates cursor pos. where value is returned
;;;  simple-word-test
;;;  ^^^^^^^^^^^^^^^^  --> simple-word-test
;;;  _harder_word_test_
;;;  ^^^^^^^^^^^^^^^^^^ --> harder_word_test
;;;  .___.______.
;;;  --> nil
;;;  /foo/bar/quux.hello
;;;  ^^^^^^^^^^^^^^^^^^^ --> /foo/bar/quux.hello
;;;

(defun symbol-before-point ()
  "Returns a string of the symbol immediately before point
or nil if there isn't one longer than *completion-min-length*."       
  ;; This is called when a word separator is typed so it must be FAST !
  (setq cmpl-saved-syntax (syntax-table))
  (set-syntax-table cmpl-syntax-table)
  ;; Cursor is on following-char and after preceding-char
  (cond ((= (setq cmpl-preceding-syntax (char-syntax (preceding-char))) ?_)
	 ;; No chars. to ignore at end
	 (setq cmpl-symbol-end (point)
	       cmpl-symbol-start (scan-sexps (1+ cmpl-symbol-end) -1)
	       )
	 ;; remove chars to ignore at the start
	 (cond ((= (char-syntax (char-after cmpl-symbol-start)) ?w)
		(goto-char cmpl-symbol-start)
		(forward-word 1)
		(setq cmpl-symbol-start (point))
		(goto-char cmpl-symbol-end)
		))
	 ;; restore state
	 (set-syntax-table cmpl-saved-syntax)
	 ;; return value if long enough
	 (if (>= cmpl-symbol-end
		 (+ cmpl-symbol-start
		    (read-time-eval *completion-min-length*)))
	     (buffer-substring cmpl-symbol-start cmpl-symbol-end))
	 )
	((= cmpl-preceding-syntax ?w)
	 ;; chars to ignore at end
	 (setq cmpl-saved-point (point)
	       cmpl-symbol-start (scan-sexps (1+ cmpl-saved-point) -1))
	 ;; take off chars. from end
	 (forward-word -1)
	 (setq cmpl-symbol-end (point))
	 ;; remove chars to ignore at the start
	 (cond ((= (char-syntax (char-after cmpl-symbol-start)) ?w)
		(goto-char cmpl-symbol-start)
		(forward-word 1)
		(setq cmpl-symbol-start (point))
		))
	 ;; restore state
	 (goto-char cmpl-saved-point)
	 (set-syntax-table cmpl-saved-syntax)
	 ;; Return completion if the length is reasonable
	 (if (and (<= (read-time-eval *completion-min-length*)
		      (- cmpl-symbol-end cmpl-symbol-start))
		  (<= (- cmpl-symbol-end cmpl-symbol-start)
		      (read-time-eval *completion-max-length*)))
	     (buffer-substring cmpl-symbol-start cmpl-symbol-end))
	 )
	(t 
	 ;; restore table if no symbol
	 (set-syntax-table cmpl-saved-syntax)
	 nil)
	))

;;; tests for symbol-before-point
;;;  `^' indicates cursor pos. where value is returned
;;;  simple-word-test
;;;  ^ --> nil
;;;   ^ --> nil
;;;          ^  --> simple-w
;;;                  ^ --> simple-word-test
;;;  _harder_word_test_
;;;                   ^  --> harder_word_test
;;;                    ^  --> harder_word_test
;;;          ^ --> harder
;;;  .___....
;;;  --> nil

(defun symbol-under-or-before-point ()
  ;;; This could be made slightly faster but it is better to avoid
  ;;; copying all the code.
  ;;; However, it is only used by the completion string prompter.
  ;;; If it comes into common use, it could be rewritten.
  (setq cmpl-saved-syntax (syntax-table))
  (set-syntax-table cmpl-syntax-table)
  (cond ((memq (char-syntax (following-char)) '(?w ?_))
	 (set-syntax-table cmpl-saved-syntax)
	 (symbol-under-point))
	(t
	 (set-syntax-table cmpl-saved-syntax)
	 (symbol-before-point))
	))


(defun symbol-before-point-for-complete ()
  ;; "Returns a string of the symbol immediately before point
  ;; or nil if there isn't one.  Like symbol-before-point but doesn't trim the
  ;; end chars."
  ;; Cursor is on following-char and after preceding-char
  (setq cmpl-saved-syntax (syntax-table))
  (set-syntax-table cmpl-syntax-table)
  (cond ((memq (setq cmpl-preceding-syntax (char-syntax (preceding-char)))
	       '(?_ ?w))
	 (setq cmpl-symbol-end (point)
	       cmpl-symbol-start (scan-sexps (1+ cmpl-symbol-end) -1)
	       )
	 ;; remove chars to ignore at the start
	 (cond ((= (char-syntax (char-after cmpl-symbol-start)) ?w)
		(goto-char cmpl-symbol-start)
		(forward-word 1)
		(setq cmpl-symbol-start (point))
		(goto-char cmpl-symbol-end)
		))
	 ;; restore state
	 (set-syntax-table cmpl-saved-syntax)
	 ;; Return completion if the length is reasonable
	 (if (and (<= (read-time-eval
		       *completion-prefix-min-length*)
		      (- cmpl-symbol-end cmpl-symbol-start))
		  (<= (- cmpl-symbol-end cmpl-symbol-start)
		      (read-time-eval *completion-max-length*)))
	     (buffer-substring cmpl-symbol-start cmpl-symbol-end))
	 )
	(t 
	 ;; restore table if no symbol
	 (set-syntax-table cmpl-saved-syntax)
	 nil)
	))

;;; tests for symbol-before-point-for-complete
;;;  `^' indicates cursor pos. where value is returned
;;;  simple-word-test
;;;  ^ --> nil
;;;   ^ --> nil
;;;          ^  --> simple-w
;;;                  ^ --> simple-word-test
;;;  _harder_word_test_
;;;                   ^  --> harder_word_test
;;;                    ^  --> harder_word_test_
;;;          ^ --> harder_
;;;  .___....
;;;  --> nil



;;;---------------------------------------------------------------------------
;;; Statistics Recording
;;;---------------------------------------------------------------------------

;;; Note that the guts of this has been turned off.  The guts
;;; are in completion-stats.el.

;;;-----------------------------------------------
;;; Conditionalizing code on *record-cmpl-statistics-p*
;;;-----------------------------------------------
;;; All statistics code outside this block should use this
(defmacro cmpl-statistics-block (&rest body)
  "Only executes body if we are recording statistics."
  (list 'cond
	(list* '*record-cmpl-statistics-p* body)
	))		 

;;;-----------------------------------------------
;;; Completion Sources
;;;-----------------------------------------------

;; ID numbers
(defconst cmpl-source-unknown 0)
(defconst cmpl-source-init-file 1)
(defconst cmpl-source-file-parsing 2)
(defconst cmpl-source-separator 3)
(defconst cmpl-source-cursor-moves 4)
(defconst cmpl-source-interactive 5)
(defconst cmpl-source-cdabbrev 6)
(defconst num-cmpl-sources 7)
(defvar current-completion-source cmpl-source-unknown)



;;;---------------------------------------------------------------------------
;;; Completion Method #2: dabbrev-expand style
;;;---------------------------------------------------------------------------
;;;
;;;   This method is used if there are no useful stored completions.  It is 
;;; based on dabbrev-expand with these differences :
;;;   1) Faster (we don't use regexps)
;;;   2) case coercion handled correctly
;;; This is called cdabbrev to differentiate it.
;;;   We simply search backwards through the file looking for words which
;;; start with the same letters we are trying to complete.
;;;

(defvar cdabbrev-completions-tried nil)
;;;  "A list of all the cdabbrev completions since the last reset.")

(defvar cdabbrev-current-point 0)
;;;  "The current point position the cdabbrev search is at.")

(defvar cdabbrev-current-window nil)
;;;  "The current window we are looking for cdabbrevs in.  T if looking in
;;; (other-buffer), NIL if no more  cdabbrevs.")

(defvar cdabbrev-wrapped-p nil)
;;;  "T if the cdabbrev search has wrapped around the file.")

(defvar cdabbrev-abbrev-string "")
(defvar cdabbrev-start-point 0)

;;; Test strings for cdabbrev
;;; cdat-upcase   ;;same namestring
;;; CDAT-UPCASE   ;;ok
;;; cdat2         ;;too short
;;; cdat-1-2-3-4  ;;ok
;;; a-cdat-1      ;;doesn't start correctly
;;; cdat-simple   ;;ok


(defun reset-cdabbrev (abbrev-string &optional initial-completions-tried)
  "Resets the cdabbrev search to search for abbrev-string.
initial-completions-tried is a list of downcased strings to ignore
during the search."
  (setq cdabbrev-abbrev-string abbrev-string
	cdabbrev-completions-tried
	(cons (downcase abbrev-string) initial-completions-tried)
	)
  (reset-cdabbrev-window t)
  )

(defun set-cdabbrev-buffer ()
  ;; cdabbrev-current-window must not be NIL
  (set-buffer (if (eq cdabbrev-current-window t)
		  (other-buffer)
		  (window-buffer cdabbrev-current-window)))
  )


(defun reset-cdabbrev-window (&optional initializep)
  "Resets the cdabbrev search to search for abbrev-string.
initial-completions-tried is a list of downcased strings to ignore
during the search."
  ;; Set the window
  (cond (initializep
	 (setq cdabbrev-current-window (selected-window))
	 )
	((eq cdabbrev-current-window t)
	 ;; Everything has failed
	 (setq cdabbrev-current-window nil))
	(cdabbrev-current-window
	 (setq cdabbrev-current-window (next-window cdabbrev-current-window))
	 (if (eq cdabbrev-current-window (selected-window))
	     ;; No more windows, try other buffer.
	     (setq cdabbrev-current-window t)))
	)
  (when cdabbrev-current-window
    (save-excursion
      (set-cdabbrev-buffer)
      (setq cdabbrev-current-point (point)
	    cdabbrev-start-point cdabbrev-current-point
	    cdabbrev-stop-point
	    (if *cdabbrev-radius*
		(max (point-min)
		     (- cdabbrev-start-point *cdabbrev-radius*))
		(point-min))
	    cdabbrev-wrapped-p nil)
      )))

(defun next-cdabbrev ()
  "Return the next possible cdabbrev expansion or nil if there isn't one.
reset-cdabbrev must've been called.  This is sensitive to case-fold-search."
  ;; note that case-fold-search affects the behavior of this function
  ;; Bug: won't pick up an expansion that starts at the top of buffer
  (when cdabbrev-current-window
    (let (saved-point 
	  saved-syntax
	  (expansion nil)
	  downcase-expansion tried-list syntax saved-point-2)
      (save-excursion
	(unwind-protect
	    (progn
	      ;; Switch to current completion buffer
	      (set-cdabbrev-buffer)
	      ;; Save current buffer state
	      (setq saved-point  (point)
		    saved-syntax (syntax-table))
	      ;; Restore completion state
	      (set-syntax-table cmpl-syntax-table)
	      (goto-char cdabbrev-current-point)
	      ;; Loop looking for completions
	      (while
		  ;; This code returns t if it should loop again
		  (cond
		    (;; search for the string
		     (search-backward cdabbrev-abbrev-string cdabbrev-stop-point t)
		     ;; return nil if the completion is valid
		     (not
		      (and
		       ;; does it start with a separator char ?
		       (or (= (setq syntax (char-syntax (preceding-char))) ? )
			   (and (= syntax ?w)
				;; symbol char to ignore at end.  Are we at end ?
				(progn
				  (setq saved-point-2 (point))
				  (forward-word -1)
				  (prog1
				    (= (char-syntax (preceding-char)) ? )
				    (goto-char saved-point-2)
				    ))))
		       ;; is the symbol long enough ?
		       (setq expansion (symbol-under-point))
		       ;; have we not tried this one before
		       (progn
			 ;; See if we've already used it
			 (setq tried-list cdabbrev-completions-tried
			       downcase-expansion (downcase expansion))
			 (while (and tried-list
				     (not (string-equal downcase-expansion
							(car tried-list))))
			   ;; Already tried, don't choose this one
			   (setq tried-list (cdr tried-list))
			   )
			 ;; at this point tried-list will be nil if this
			 ;; expansion has not yet been tried
			 (if tried-list
			     (setq expansion nil)
			     t)
			 ))))
		    ;; search failed
		    (cdabbrev-wrapped-p
		     ;; If already wrapped, then we've failed completely
		     nil)
		    (t
		     ;; need to wrap
		     (goto-char (setq cdabbrev-current-point
				      (if *cdabbrev-radius*
					  (min (point-max) (+ cdabbrev-start-point *cdabbrev-radius*))
					  (point-max))))
		
		     (setq cdabbrev-wrapped-p t))
		    ))
	      ;; end of while loop
	      (cond (expansion
		     ;; successful
		     (setq cdabbrev-completions-tried
			   (cons downcase-expansion cdabbrev-completions-tried)
			   cdabbrev-current-point (point))))
	      )
	  (set-syntax-table saved-syntax)
	  (goto-char saved-point)
	  ))
      ;; If no expansion, go to next window
      (cond (expansion)
	    (t (reset-cdabbrev-window)
	       (next-cdabbrev)))
      )))

;;; The following must be eval'd in the minibuffer ::
;;; (reset-cdabbrev "cdat")
;;; (next-cdabbrev)  --> "cdat-simple"
;;; (next-cdabbrev)  --> "cdat-1-2-3-4"
;;; (next-cdabbrev)  --> "CDAT-UPCASE"
;;; (next-cdabbrev)  --> "cdat-wrapping"
;;; (next-cdabbrev)  --> "cdat_start_sym"
;;; (next-cdabbrev)  --> nil
;;; (next-cdabbrev)  --> nil
;;; (next-cdabbrev)  --> nil

;;; _cdat_start_sym
;;; cdat-wrapping


;;;---------------------------------------------------------------------------
;;; Completion Database
;;;---------------------------------------------------------------------------

;;; We use two storage modes for the two search types ::
;;;  1) Prefix {cmpl-prefix-obarray} for looking up possible completions
;;;      Used by search-completion-next
;;;      the value of the symbol is nil or a cons of head and tail pointers
;;;  2) Interning {cmpl-obarray} to see if it's in the database
;;;      Used by find-exact-completion, completion-in-database-p
;;;      The value of the symbol is the completion entry

;;; bad things may happen if this length is changed due to the way
;;; GNU implements obarrays
(defconst cmpl-obarray-length 511)

(defvar cmpl-prefix-obarray (make-vector cmpl-obarray-length 0)
  "An obarray used to store the downcased completion prefices.
Each symbol is bound to a list of completion entries.")

(defvar cmpl-obarray (make-vector cmpl-obarray-length 0)
  "An obarray used to store the downcased completions.
Each symbol is bound to a single completion entry.")

;;;-----------------------------------------------
;;; Completion Entry Structure Definition
;;;-----------------------------------------------

;;; A completion entry is a LIST of string, prefix-symbol num-uses, and
;;; last-use-time (the time the completion was last used)
;;; last-use-time is T if the string should be kept permanently
;;; num-uses is incremented everytime the completion is used.

;;; We chose lists because (car foo) is faster than (aref foo 0) and the 
;;; creation time is about the same.

;;; READER MACROS

(defmacro completion-string (completion-entry)
  (list 'car completion-entry))

(defmacro completion-num-uses (completion-entry)
  ;;  "The number of times it has used.  Used to decide whether to save 
  ;; it."
  (list 'car (list 'cdr completion-entry)))

(defmacro completion-last-use-time (completion-entry)
  ;;  "The time it was last used.  In hours since 1900.  Used to decide
  ;; whether to save it.  T if one should always save it."
  (list 'nth 2 completion-entry))

(defmacro completion-source (completion-entry)
  (list 'nth 3 completion-entry))

;;; WRITER MACROS
(defmacro set-completion-string (completion-entry string)
  (list 'setcar completion-entry string))

(defmacro set-completion-num-uses (completion-entry num-uses)
  (list 'setcar (list 'cdr completion-entry) num-uses))

(defmacro set-completion-last-use-time (completion-entry last-use-time)
  (list 'setcar (list 'cdr (list 'cdr completion-entry)) last-use-time))

;;; CONSTRUCTOR
(defun make-completion (string)
  "Returns a list of a completion entry."
  (list (list string 0 nil current-completion-source)))

;; Obsolete
;;(defmacro cmpl-prefix-entry-symbol (completion-entry)
;;  (list 'car (list 'cdr completion-entry)))



;;;-----------------------------------------------
;;; Prefix symbol entry definition
;;;-----------------------------------------------
;;; A cons of (head . tail)

;;; READER Macros

(defmacro cmpl-prefix-entry-head (prefix-entry)
  (list 'car prefix-entry))

(defmacro cmpl-prefix-entry-tail (prefix-entry)
  (list 'cdr prefix-entry))

;;; WRITER Macros

(defmacro set-cmpl-prefix-entry-head (prefix-entry new-head)
  (list 'setcar prefix-entry new-head))

(defmacro set-cmpl-prefix-entry-tail (prefix-entry new-tail)
  (list 'setcdr prefix-entry new-tail))

;;; Contructor

(defun make-cmpl-prefix-entry (completion-entry-list)
  "Makes a new prefix entry containing only completion-entry."
  (cons completion-entry-list completion-entry-list))

;;;-----------------------------------------------
;;; Completion Database - Utilities
;;;-----------------------------------------------

(defun clear-all-completions ()
  "Initializes the completion storage.  All existing completions are lost."
  (interactive)
  (setq cmpl-prefix-obarray (make-vector cmpl-obarray-length 0))
  (setq cmpl-obarray (make-vector cmpl-obarray-length 0))
  (cmpl-statistics-block
    (record-clear-all-completions))
  )

(defun list-all-completions ()
  "Returns a list of all the known completion entries."
  (let ((return-completions nil))
    (mapatoms 'list-all-completions-1 cmpl-prefix-obarray)
    return-completions))

(defun list-all-completions-1 (prefix-symbol)
  (if (boundp prefix-symbol)
      (setq return-completions
	    (append (cmpl-prefix-entry-head (symbol-value prefix-symbol))
		    return-completions))))

(defun list-all-completions-by-hash-bucket ()
  "Returns a list of lists of all the known completion entries organized by 
hash bucket."
  (let ((return-completions nil))
    (mapatoms 'list-all-completions-by-hash-bucket-1 cmpl-prefix-obarray)
    return-completions))

(defun list-all-completions-by-hash-bucket-1 (prefix-symbol)
  (if (boundp prefix-symbol)
      (setq return-completions
	    (cons (cmpl-prefix-entry-head (symbol-value prefix-symbol))
		  return-completions))))


;;;-----------------------------------------------
;;; Updating the database
;;;-----------------------------------------------
;;;
;;;   These are the internal functions used to update the datebase
;;;
;;;
(defvar completion-to-accept nil)
  ;;"Set to a string that is pending its acceptance."
  ;; this checked by the top level reading functions

(defvar cmpl-db-downcase-string nil)
  ;; "Setup by find-exact-completion, etc.  The given string, downcased."
(defvar cmpl-db-symbol nil)
  ;; "The interned symbol corresponding to cmpl-db-downcase-string.
  ;; Set up by cmpl-db-symbol."
(defvar cmpl-db-prefix-symbol nil)
  ;; "The interned prefix symbol corresponding to cmpl-db-downcase-string."
(defvar cmpl-db-entry nil)
(defvar cmpl-db-debug-p nil
  "Set to T if you want to debug the database.")

;;; READS
(defun find-exact-completion (string)
  "Returns the completion entry for string or nil.
Sets up cmpl-db-downcase-string and cmpl-db-symbol."
  (and (boundp (setq cmpl-db-symbol
		     (intern (setq cmpl-db-downcase-string (downcase string))
			     cmpl-obarray)))
       (symbol-value cmpl-db-symbol)
       ))

(defun find-cmpl-prefix-entry (prefix-string)
  "Returns the prefix entry for string.
Sets cmpl-db-prefix-symbol.
Prefix-string must be exactly *completion-prefix-min-length* long
and downcased.  Sets up cmpl-db-prefix-symbol."
  (and (boundp (setq cmpl-db-prefix-symbol
		     (intern prefix-string cmpl-prefix-obarray)))
       (symbol-value cmpl-db-prefix-symbol)))

(defvar inside-locate-completion-entry nil)
;; used to trap lossage in silent error correction

(defun locate-completion-entry (completion-entry prefix-entry)
  "Locates the completion entry.
Returns a pointer to the element before the completion entry or nil if
the completion entry is at the head.
Must be called after find-exact-completion."
  (let ((prefix-list (cmpl-prefix-entry-head prefix-entry))
	 next-prefix-list
	 )
    (cond
      ((not (eq (car prefix-list) completion-entry))
       ;; not already at head
       (while (and prefix-list
		   (not (eq completion-entry
			    (car (setq next-prefix-list (cdr prefix-list)))
			    )))
	 (setq prefix-list next-prefix-list))
       (cond (;; found
	      prefix-list)
	     ;; Didn't find it.  Database is messed up.
	     (cmpl-db-debug-p
	      ;; not found, error if debug mode
	      (error "Completion entry exists but not on prefix list - %s"
		     string))
	     (inside-locate-completion-entry
	      ;; recursive error: really scrod
	      (locate-completion-db-error))
	     (t
	       ;; Patch out
	       (set cmpl-db-symbol nil)
	       ;; Retry
	       (locate-completion-entry-retry completion-entry)
	       ))))))

(defun locate-completion-entry-retry (old-entry)
  (let ((inside-locate-completion-entry t))
    (add-completion (completion-string old-entry)
		    (completion-num-uses old-entry)
		    (completion-last-use-time old-entry))
    (let ((cmpl-entry (find-exact-completion (completion-string old-entry)))
	  (pref-entry
	   (if cmpl-entry
	       (find-cmpl-prefix-entry
		 (substring cmpl-db-downcase-string
			    0 *completion-prefix-min-length*))))
	  )
      (if (and cmpl-entry pref-entry)
	  ;; try again
	  (locate-completion-entry cmpl-entry pref-entry)
	  ;; still losing
	  (locate-completion-db-error))
      )))

(defun locate-completion-db-error ()
  ;; recursive error: really scrod
  (error "Completion database corrupted.  Try M-x clear-all-completions.  Send bug report.")
  )

;;; WRITES
(defun add-completion-to-tail-if-new (string)
  "If STRING is not in the database add it to appropriate prefix list.
STRING is added to the end of the approppriate prefix list with
num-uses = 0.  The database is unchanged if it is there.  STRING must be
longer than *completion-prefix-min-length*.
This must be very fast.
Returns the completion entry."
  (or (find-exact-completion string)
      ;; not there
      (let (;; create an entry
	    (entry (make-completion string))
	    ;; setup the prefix
	    (prefix-entry (find-cmpl-prefix-entry
			    (substring cmpl-db-downcase-string 0
				       (read-time-eval
					*completion-prefix-min-length*))))
	    )
	;; The next two forms should happen as a unit (atomically) but
	;; no fatal errors should result if that is not the case.
	(cond (prefix-entry
	       ;; These two should be atomic, but nothing fatal will happen
	       ;; if they're not.
	       (setcdr (cmpl-prefix-entry-tail prefix-entry) entry)
	       (set-cmpl-prefix-entry-tail prefix-entry entry))
	      (t
	       (set cmpl-db-prefix-symbol (make-cmpl-prefix-entry entry))
	       ))
	;; statistics
	(cmpl-statistics-block
	  (note-added-completion))
	;; set symbol
	(set cmpl-db-symbol (car entry))
	)))

(defun add-completion-to-head (string)
  "If STRING is not in the database, add it to prefix list.
STRING is added to the head of the approppriate prefix list.  Otherwise
it is moved to the head of the list.  STRING must be longer than
*completion-prefix-min-length*.
Updates the saved string with the supplied string.
This must be very fast.
Returns the completion entry."
  ;; Handle pending acceptance
  (if completion-to-accept (accept-completion))
  ;; test if already in database
  (if (setq cmpl-db-entry (find-exact-completion string))
      ;; found
      (let* ((prefix-entry (find-cmpl-prefix-entry
			     (substring cmpl-db-downcase-string 0
					(read-time-eval
					 *completion-prefix-min-length*))))
	     (splice-ptr (locate-completion-entry cmpl-db-entry prefix-entry))
	     (cmpl-ptr (cdr splice-ptr))
	     )
	;; update entry
	(set-completion-string cmpl-db-entry string)
	;; move to head (if necessary)
	(cond (splice-ptr
	       ;; These should all execute atomically but it is not fatal if
	       ;; they don't.
	       ;; splice it out
	       (or (setcdr splice-ptr (cdr cmpl-ptr))
		   ;; fix up tail if necessary
		   (set-cmpl-prefix-entry-tail prefix-entry splice-ptr))
	       ;; splice in at head
	       (setcdr cmpl-ptr (cmpl-prefix-entry-head prefix-entry))
	       (set-cmpl-prefix-entry-head prefix-entry cmpl-ptr)
	       ))
	cmpl-db-entry)
    ;; not there
    (let (;; create an entry
	  (entry (make-completion string))
	  ;; setup the prefix
	  (prefix-entry (find-cmpl-prefix-entry
			  (substring cmpl-db-downcase-string 0
				     (read-time-eval
				      *completion-prefix-min-length*))))
	  )
      (cond (prefix-entry
	     ;; Splice in at head
	     (setcdr entry (cmpl-prefix-entry-head prefix-entry))
	     (set-cmpl-prefix-entry-head prefix-entry entry))
	    (t
	     ;; Start new prefix entry
	     (set cmpl-db-prefix-symbol (make-cmpl-prefix-entry entry))
	     ))
      ;; statistics
      (cmpl-statistics-block
	(note-added-completion))
      ;; Add it to the symbol
      (set cmpl-db-symbol (car entry))
      )))
      
(defun delete-completion (string)
  "Deletes the completion from the database.
String must be longer than *completion-prefix-min-length*."
  ;; Handle pending acceptance
  (if completion-to-accept (accept-completion))
  (if (setq cmpl-db-entry (find-exact-completion string))
      ;; found
      (let* ((prefix-entry (find-cmpl-prefix-entry 
			     (substring cmpl-db-downcase-string 0
					(read-time-eval
					 *completion-prefix-min-length*))))
	     (splice-ptr (locate-completion-entry cmpl-db-entry prefix-entry))
	     )
	 ;; delete symbol reference
	 (set cmpl-db-symbol nil)
	 ;; remove from prefix list
	 (cond (splice-ptr
		;; not at head
		(or (setcdr splice-ptr (cdr (cdr splice-ptr)))
		    ;; fix up tail if necessary
		    (set-cmpl-prefix-entry-tail prefix-entry splice-ptr))
		)
	       (t
		;; at head
		(or (set-cmpl-prefix-entry-head
		      prefix-entry (cdr (cmpl-prefix-entry-head prefix-entry)))
		    ;; List is now empty
		    (set cmpl-db-prefix-symbol nil))
		))
	 (cmpl-statistics-block
	   (note-completion-deleted))
	 )
      (error "Unknown completion: %s.  Couldn't delete it." string)
      ))

;;; Tests --
;;;  - Add and Find -
;;; (add-completion-to-head "banana")     --> ("banana" 0 nil 0)
;;; (find-exact-completion "banana")      --> ("banana" 0 nil 0)
;;; (find-exact-completion "bana")        --> nil
;;; (car (find-cmpl-prefix-entry "ban"))  --> (("banana" ...))
;;; (cdr (find-cmpl-prefix-entry "ban"))  --> (("banana" ...))
;;; (add-completion-to-head "banish")     --> ("banish" 0 nil 0)
;;; (find-exact-completion "banish")      --> ("banish" 0 nil 0)
;;; (car (find-cmpl-prefix-entry "ban"))  --> (("banish" ...) ("banana" ...))
;;; (cdr (find-cmpl-prefix-entry "ban"))  --> (("banana" ...))
;;; (add-completion-to-head "banana")     --> ("banana" 0 nil 0)
;;; (car (find-cmpl-prefix-entry "ban"))  --> (("banana" ...) ("banish" ...))
;;; (cdr (find-cmpl-prefix-entry "ban"))  --> (("banish" ...))
;;;
;;;  - Deleting -
;;; (add-completion-to-head "banner")     --> ("banner" 0 nil 0)
;;; (delete-completion "banner")        
;;; (find-exact-completion "banner")      --> nil
;;; (car (find-cmpl-prefix-entry "ban"))  --> (("banana" ...) ("banish" ...))
;;; (cdr (find-cmpl-prefix-entry "ban"))  --> (("banish" ...))
;;; (add-completion-to-head "banner")     --> ("banner" 0 nil 0) 
;;; (delete-completion "banana")        
;;; (car (find-cmpl-prefix-entry "ban"))  --> (("banner" ...) ("banish" ...))
;;; (cdr (find-cmpl-prefix-entry "ban"))  --> (("banish" ...))
;;; (delete-completion "banner")        
;;; (delete-completion "banish")                 
;;; (find-cmpl-prefix-entry "ban")        --> nil
;;; (delete-completion "banner")          --> error
;;;
;;; - Tail -
;;; (add-completion-to-tail-if-new "banana") --> ("banana" 0 nil 0)
;;; (car (find-cmpl-prefix-entry "ban"))     --> (("banana" ...))
;;; (cdr (find-cmpl-prefix-entry "ban"))     --> (("banana" ...))
;;; (add-completion-to-tail-if-new "banish") --> ("banish" 0 nil 0)
;;; (car (find-cmpl-prefix-entry "ban"))     -->(("banana" ...) ("banish" ...))
;;; (cdr (find-cmpl-prefix-entry "ban"))     -->(("banish" ...))
;;;


;;;---------------------------------------------------------------------------
;;; Database Update :: Interface level routines
;;;---------------------------------------------------------------------------
;;; 
;;; These lie on top of the database ref. functions but below the standard
;;; user interface level


(defun interactive-completion-string-reader (prompt)
  (let* ((default (symbol-under-or-before-point))
	 (new-prompt
	  (if default
	      (format "%s: (default: %s) " prompt default)
	      (format "%s: " prompt))
	   )
	 (read (completing-read new-prompt cmpl-obarray))
	 )
    (if (zerop (length read)) (setq read (or default "")))
    (list read)
    ))

(defun check-completion-length (string)
  (if (< (length string) *completion-min-length*)
      (error "The string \"%s\" is too short to be saved as a completion."
	     string)
      (list string)))

(defun add-completion (string &optional num-uses last-use-time)
  "If the string is not there, it is added to the head of the completion list.
Otherwise, it is moved to the head of the list.
The completion is altered appropriately if num-uses and/or last-use-time is 
specified."
  (interactive (interactive-completion-string-reader "Completion to add"))
  (check-completion-length string)
  (let* ((current-completion-source (if (interactive-p)
					cmpl-source-interactive
					current-completion-source))
	 (entry (add-completion-to-head string)))
    
    (if num-uses (set-completion-num-uses entry num-uses))
    (if last-use-time
	(set-completion-last-use-time entry last-use-time))
    ))

(defun add-permanent-completion (string)
  "Adds string if it isn't already there and and makes it a permanent string."
  (interactive
    (interactive-completion-string-reader "Completion to add permanently"))
  (let ((current-completion-source (if (interactive-p)
				       cmpl-source-interactive
				       current-completion-source))
	)
    (add-completion string nil t)
    ))

(defun kill-completion (string)
  (interactive (interactive-completion-string-reader "Completion to kill"))
  (check-completion-length string)
  (delete-completion string)
  )

(defun accept-completion ()
  "Accepts the pending completion in completion-to-accept.
This bumps num-uses.  Called by add-completion-to-head and 
completion-search-reset."
  (let ((string completion-to-accept)
	;; if this is added afresh here, then it must be a cdabbrev
	(current-completion-source cmpl-source-cdabbrev)
	entry
	)
    (setq completion-to-accept nil)
    (setq entry (add-completion-to-head string))
    (set-completion-num-uses entry (1+ (completion-num-uses entry)))
    (setq cmpl-completions-accepted-p t)
    ))

(defun use-completion-under-point ()
  "Adds the completion symbol underneath the point into the completion buffer."
  (let ((string (and *completep* (symbol-under-point)))
	(current-completion-source cmpl-source-cursor-moves))
    (if string (add-completion-to-head string))))
	
(defun use-completion-before-point ()
  "Adds the completion symbol before point into
the completion buffer."
  (let ((string (and *completep* (symbol-before-point)))
	(current-completion-source cmpl-source-cursor-moves))
    (if string (add-completion-to-head string))))

(defun use-completion-under-or-before-point ()
  "Adds the completion symbol before point into the completion buffer."
  (let ((string (and *completep* (symbol-under-or-before-point)))
	(current-completion-source cmpl-source-cursor-moves))
    (if string (add-completion-to-head string))))

(defun use-completion-before-separator ()
  "Adds the completion symbol before point into the completion buffer.
Completions added this way will automatically be saved if
*separator-character-uses-completion-p* is non-nil."
  (let ((string (and *completep* (symbol-before-point)))
	(current-completion-source cmpl-source-separator)
	entry)
    (cmpl-statistics-block
      (note-separator-character string)
      )
    (cond (string
	   (setq entry (add-completion-to-head string))
	   (when (and *separator-character-uses-completion-p*
		      (zerop (completion-num-uses entry)))
	     (set-completion-num-uses entry 1)
	     (setq cmpl-completions-accepted-p t)
	     )))
    ))

;;; Tests --
;;;  - Add and Find -
;;; (add-completion "banana" 5 10)  
;;; (find-exact-completion "banana")  --> ("banana" 5 10 0)
;;; (add-completion "banana" 6)     
;;; (find-exact-completion "banana")  --> ("banana" 6 10 0)
;;; (add-completion "banish")
;;; (car (find-cmpl-prefix-entry "ban"))  --> (("banish" ...) ("banana" ...))
;;;
;;;  - Accepting -
;;; (setq completion-to-accept "banana")
;;; (accept-completion)                   
;;; (find-exact-completion "banana")      --> ("banana" 7 10)
;;; (car (find-cmpl-prefix-entry "ban"))  --> (("banana" ...) ("banish" ...))
;;; (setq completion-to-accept "banish")
;;; (add-completion "banner")           
;;; (car (find-cmpl-prefix-entry "ban"))
;;;        --> (("banner" ...) ("banish" 1 ...) ("banana" 7 ...))
;;;
;;;  - Deleting -
;;; (kill-completion "banish")          
;;; (car (find-cmpl-prefix-entry "ban"))  --> (("banner" ...) ("banana" ...))


;;;---------------------------------------------------------------------------
;;; Searching the database
;;;---------------------------------------------------------------------------
;;; Functions outside this block must call completion-search-reset followed
;;; by calls to completion-search-next or completion-search-peek
;;;

;;; Status variables
;; Commented out to improve loading speed
(defvar cmpl-test-string "")
;;  "The current string used by completion-search-next."
(defvar cmpl-test-regexp "")
;;  "The current regexp used by completion-search-next. 
;;   (derived from cmpl-test-string)"
(defvar cmpl-last-index 0)
;;  "The last index that completion-search-next was called with."
(defvar cmpl-cdabbrev-reset-p nil)
;;  "Set to t when cdabbrevs have been reset."
(defvar cmpl-next-possibilities nil)
;;   "A pointer to the element BEFORE the next set of possible completions.
;;  cadr of this is the cmpl-next-possibility"
(defvar cmpl-starting-possibilities nil)
;;  "The initial list of starting possibilities."
(defvar cmpl-next-possibility nil)
;;   "The cached next possibility."
(defvar cmpl-tried-list nil)
;;   "A downcased list of all the completions we have tried."


(defun completion-search-reset (string)
  "Given a string, sets up the get-completion and completion-search-next functions.
String must be longer than *completion-prefix-min-length*."
  (if completion-to-accept (accept-completion))
  (setq cmpl-starting-possibilities
	(cmpl-prefix-entry-head
	  (find-cmpl-prefix-entry (downcase (substring string 0 3))))
	cmpl-test-string string
	cmpl-test-regexp (concat (regexp-quote string) "."))
  (completion-search-reset-1)
  )

(defun completion-search-reset-1 ()
  (setq cmpl-next-possibilities cmpl-starting-possibilities
	cmpl-next-possibility   nil
	cmpl-cdabbrev-reset-p nil
	cmpl-last-index -1
	cmpl-tried-list nil
	))

(defun completion-search-next (index)
  "Returns the next completion entry.
If index is out of sequence it resets and starts from the top.
If there are no more entries it tries cdabbrev and returns only a string."
  (cond
    ((= index (setq cmpl-last-index (1+ cmpl-last-index)))
     (completion-search-peek t))
    ((minusp index)
     (completion-search-reset-1)
     (setq cmpl-last-index index)
     ;; reverse the possibilities list
     (setq cmpl-next-possibilities (reverse cmpl-starting-possibilities))
     ;; do a "normal" search
     (while (and (completion-search-peek nil)
		 (minusp (setq index (1+ index))))
       (setq cmpl-next-possibility nil)
       )
     (cond ((not cmpl-next-possibilities))
	    ;; If no more possibilities, leave it that way
	   ((= -1 cmpl-last-index)
	    ;; next completion is at index 0.  reset next-possibility list 
	    ;; to start at beginning
	    (setq cmpl-next-possibilities cmpl-starting-possibilities))
	   (t
	    ;; otherwise point to one before current
	    (setq cmpl-next-possibilities
		  (nthcdr (- (length cmpl-starting-possibilities)
			     (length cmpl-next-possibilities))
			  cmpl-starting-possibilities))
	    )))
    (t
     ;; non-negative index, reset and search
     ;;(prin1 'reset)
     (completion-search-reset-1)
     (setq cmpl-last-index index)
     (while (and (completion-search-peek t)
		 (not (minusp (setq index (1- index)))))
       (setq cmpl-next-possibility nil)
       ))
    )
  (prog1
      cmpl-next-possibility
    (setq cmpl-next-possibility nil)
    ))
      

(defun completion-search-peek (use-cdabbrev)
  "Returns the next completion entry without actually moving the pointers.
Calling this again or calling completion-search-next will result in the same 
string being returned.  Depends on case-fold-search.
If there are no more entries it tries cdabbrev and then returns only a string."
  (cond
    ;; return the cached value if we have it
    (cmpl-next-possibility)
    ((and cmpl-next-possibilities
	  ;; still a few possibilities left
	  (progn
	    (while
		(and (not (eq 0 (string-match cmpl-test-regexp
					      (completion-string (car cmpl-next-possibilities)))))
		     (setq cmpl-next-possibilities (cdr cmpl-next-possibilities))
		     ))
	    cmpl-next-possibilities
	    ))
     ;; successful match
     (setq cmpl-next-possibility (car cmpl-next-possibilities)
	   cmpl-tried-list (cons (downcase (completion-string cmpl-next-possibility))
				 cmpl-tried-list)
	   cmpl-next-possibilities (cdr cmpl-next-possibilities)
	   )
     cmpl-next-possibility)
    (use-cdabbrev
     ;; unsuccessful, use cdabbrev
     (cond ((not cmpl-cdabbrev-reset-p)
	    (reset-cdabbrev cmpl-test-string cmpl-tried-list)
	    (setq cmpl-cdabbrev-reset-p t)
	    ))
     (setq cmpl-next-possibility (next-cdabbrev))
     )
    ;; Completely unsuccessful, return nil
    ))

;;; Tests --
;;;  - Add and Find -
;;; (add-completion "banana")       
;;; (completion-search-reset "ban")  
;;; (completion-search-next 0)        --> "banana"
;;;
;;;  - Discrimination -
;;; (add-completion "cumberland")       
;;; (add-completion "cumberbund")       
;;; cumbering   
;;; (completion-search-reset "cumb")
;;; (completion-search-peek t)        --> "cumberbund"
;;; (completion-search-next 0)        --> "cumberbund"
;;; (completion-search-peek t)        --> "cumberland"
;;; (completion-search-next 1)        --> "cumberland"
;;; (completion-search-peek nil)      --> nil
;;; (completion-search-next 2)        --> "cumbering"  {cdabbrev}
;;; (completion-search-next 3)        -->  nil or "cumming"{depends on context}
;;; (completion-search-next 1)        --> "cumberland"
;;; (completion-search-peek t)        --> "cumbering"  {cdabbrev}
;;;
;;;  - Accepting -
;;; (completion-search-next 1)        --> "cumberland"
;;; (setq completion-to-accept "cumberland")
;;; (completion-search-reset "foo")
;;; (completion-search-reset "cum")
;;; (completion-search-next 0)        --> "cumberland"
;;;
;;;  - Deleting -
;;; (kill-completion "cumberland")
;;; cummings    
;;; (completion-search-reset "cum")
;;; (completion-search-next 0)        --> "cumberbund"
;;; (completion-search-next 1)        --> "cummings"
;;;
;;;  - Ignoring Capitalization -
;;; (completion-search-reset "CuMb")
;;; (completion-search-next 0)            --> "cumberbund"



;;;-----------------------------------------------
;;; COMPLETE
;;;-----------------------------------------------

(defun completion-mode ()
  "Toggles whether or not new words are added to the database."
  (interactive)
  (setq *completep* (not *completep*))
  (message "Completion mode is now %s." (if *completep* "ON" "OFF"))
  )
	
(defvar cmpl-current-index 0)
(defvar cmpl-original-string nil)
(defvar cmpl-last-insert-location -1)
(defvar cmpl-leave-point-at-start nil)

(defun complete (&optional arg)
  "Inserts a completion at point.  
Point is left at end.  Consective calls rotate through all possibilities.
Prefix args ::
  control-u :: leave the point at the beginning of the completion rather 
               than at the end.
  a number  :: rotate through the possible completions by that amount
  `-'       :: same as -1 (insert previous completion)
 {See the comments at the top of completion.el for more info.}
"
  (interactive "*p")
  ;;; Set up variables
  (cond ((eq last-command this-command)
	 ;; Undo last one
	 (delete-region cmpl-last-insert-location (point))
	 ;; get next completion
	 (setq cmpl-current-index (+ cmpl-current-index (or arg 1)))
	 )
	(t
	 (if (not cmpl-initialized-p)
	     (initialize-completions)) ;; make sure everything's loaded
	 (cond ((consp current-prefix-arg) ;; control-u
		(setq arg 0)
		(setq cmpl-leave-point-at-start t)
		)
	       (t
		(setq cmpl-leave-point-at-start nil)
		))
	 ;; get string
	 (setq cmpl-original-string (symbol-before-point-for-complete))
	 (cond ((not cmpl-original-string)
		(setq this-command 'failed-complete)
		(error "To complete, the point must be after a symbol at least %d character long."
		       *completion-prefix-min-length*)))
	 ;; get index	     
	 (setq cmpl-current-index (if current-prefix-arg arg 0))
	 ;; statistics
	 (cmpl-statistics-block
	   (note-complete-entered-afresh cmpl-original-string))
	 ;; reset database
	 (completion-search-reset cmpl-original-string)
	 ;; erase what we've got
	 (delete-region cmpl-symbol-start cmpl-symbol-end)
	 ))

  ;; point is at the point to insert the new symbol
  ;; Get the next completion
  (let* ((print-status-p
	  (and (>= (cmpl19-baud-rate) *print-next-completion-speed-threshold*)
	       (not (minibuffer-window-selected-p))))
	 (insert-point (point))
	 (entry (completion-search-next cmpl-current-index))
	 string
	 )
    ;; entry is either a completion entry or a string (if cdabbrev)

    ;; If found, insert
    (cond (entry
	   ;; Setup for proper case
	   (setq string (if (stringp entry)
			    entry (completion-string entry)))
	   (setq string (cmpl-merge-string-cases
			  string cmpl-original-string))
	   ;; insert
	   (insert string)
	   ;; accept it
	   (setq completion-to-accept string)
	   ;; fixup and cache point
	   (cond (cmpl-leave-point-at-start
		  (setq cmpl-last-insert-location (point))
		  (goto-char insert-point))
		 (t;; point at end,
		  (setq cmpl-last-insert-location insert-point))
		 )
	   ;; statistics
	   (cmpl-statistics-block
	     (note-complete-inserted entry cmpl-current-index))
	   ;; Done ! cmpl-stat-complete-successful
	   ;;display the next completion
	   (cond
	     ((and print-status-p
		   ;; This updates the display and only prints if there
		   ;; is no typeahead
		   (cmpl19-sit-for 0)
		   (setq entry
			 (completion-search-peek
			   *print-next-completion-does-cdabbrev-search-p*)))
	      (setq string (if (stringp entry)
			       entry (completion-string entry)))
	      (setq string (cmpl-merge-string-cases
			     string cmpl-original-string))
	      (message "Next completion: %s" string)
	      ))
	   )
	  (t;; none found, insert old 
	   (insert cmpl-original-string)
	   ;; Don't accept completions
	   (setq completion-to-accept nil)
	   ;; print message
	   (if (and print-status-p (cmpl19-sit-for 0))
	       (message "No %scompletions."
			(if (eq this-command last-command) "more " "")))
	   ;; statistics
	   (cmpl-statistics-block
	     (record-complete-failed cmpl-current-index))
	   ;; Pretend that we were never here
	   (setq this-command 'failed-complete)
	   ))))

;;;-----------------------------------------------
;;; "Complete" Key Keybindings
;;;-----------------------------------------------

;;; Complete key definition
;;;  These define c-return and meta-return
;;; In any case you really want to bind this to a single keystroke
(if (fboundp 'key-for-others-chord)
    (condition-case e
	 ;; this can fail if some of the prefix chars. are already used
	 ;; as commands (this happens on wyses)
	 (global-set-key (key-for-others-chord "return" '(control)) 'complete)
      (error)
      ))
(if (fboundp 'gmacs-keycode)
    (global-set-key (gmacs-keycode "return" '(control)) 'complete)
    )
(global-set-key "\M-\r" 'complete)

;;; Tests -
;;; (add-completion "cumberland")
;;; (add-completion "cumberbund")
;;; cum
;;; Cumber
;;; cumbering
;;; cumb


;;;---------------------------------------------------------------------------
;;; Parsing definitions from files into the database
;;;---------------------------------------------------------------------------

;;;-----------------------------------------------
;;; Top Level functions ::
;;;-----------------------------------------------

;;; User interface
(defun add-completions-from-file (file)
  "Parses all the definition names from a Lisp mode file and adds them to the 
completion database."
  (interactive "fFile: ")
  (setq file (if (fboundp 'expand-file-name-defaulting)
		 (expand-file-name-defaulting file)
		 (expand-file-name file)))
  (let* ((buffer (get-file-buffer file))
	 (buffer-already-there-p buffer)
	 )
    (when (not buffer-already-there-p)
      (let ((*modes-for-completion-find-file-hook* nil))
	(setq buffer (find-file-noselect file))
	))
    (unwind-protect
	 (save-excursion
	   (set-buffer buffer)
	   (add-completions-from-buffer)
	   )
      (when (not buffer-already-there-p)
	(kill-buffer buffer))
      )))

(defun add-completions-from-buffer ()
  (interactive)
  (let ((current-completion-source cmpl-source-file-parsing)
	(start-num
	 (cmpl-statistics-block
	  (aref completion-add-count-vector cmpl-source-file-parsing)))
	mode
	)
    (cond ((memq major-mode '(emacs-lisp-mode lisp-mode))
	   (add-completions-from-lisp-buffer)
	   (setq mode 'lisp)
	   )
	  ((memq major-mode '(c-mode))
	   (add-completions-from-c-buffer)
	   (setq mode 'c)
	   )
	  (t
	   (error "Do not know how to parse completions in %s buffers."
		  major-mode)
	   ))
    (cmpl-statistics-block
      (record-cmpl-parse-file
	mode (point-max)
	(- (aref completion-add-count-vector cmpl-source-file-parsing)
	   start-num)))
    ))

;;; Find file hook
(defun cmpl-find-file-hook ()
  (cond (*completep*
	 (cond ((and (memq major-mode '(emacs-lisp-mode lisp-mode))
		     (memq 'lisp *modes-for-completion-find-file-hook*)
		     )
		(add-completions-from-buffer))
	       ((and (memq major-mode '(c-mode))
		     (memq 'c *modes-for-completion-find-file-hook*)
		     )
		(add-completions-from-buffer)
		)))
	))
    
(pushnew 'cmpl-find-file-hook find-file-hooks)

;;;-----------------------------------------------
;;; Tags Table Completions
;;;-----------------------------------------------

(defun add-completions-from-tags-table ()
  ;; Inspired by eero@media-lab.media.mit.edu
  "Add completions from the current tags-table-buffer."
  (interactive)
  (visit-tags-table-buffer)		;this will prompt if no tags-table
  (save-excursion
    (goto-char (point-min))
    (let (string)
      (condition-case e
	   (while t
	     (search-forward "\177")
	     (backward-char 3)
	     (and (setq string (symbol-under-point))
		  (add-completion-to-tail-if-new string))
	     (forward-char 3)
	     )
	 (search-failed)
	 ))))


;;;-----------------------------------------------
;;; Lisp File completion parsing
;;;-----------------------------------------------
;;;   This merely looks for phrases beginning with (def.... or
;;; (package:def ... and takes the next word.
;;;
;;; We tried using forward-lines and explicit searches but the regexp technique
;;; was faster.  (About 100K characters per second)
;;;
(defconst *lisp-def-regexp*
  "\n(\\(\\w*:\\)?def\\(\\w\\|\\s_\\)*\\s +(*"
  "A regexp that searches for lisp definition form."
  )

;;; Tests -
;;;  (and (string-match *lisp-def-regexp* "\n(defun foo") (match-end 0)) -> 8
;;;  (and (string-match *lisp-def-regexp* "\n(si:def foo") (match-end 0)) -> 9
;;;  (and (string-match *lisp-def-regexp* "\n(def-bar foo")(match-end 0)) -> 10
;;;  (and (string-match *lisp-def-regexp* "\n(defun (foo") (match-end 0)) -> 9

(defun add-completions-from-lisp-buffer ()
  "Parses all the definition names from a Lisp mode buffer and adds them to 
the completion database."
  ;;; Benchmarks
  ;;;  Sun-3/280 - 1500 to 3000 lines of lisp code per second
  (let (string)
    (save-excursion
      (goto-char (point-min))
      (condition-case e
	   (while t
	     (re-search-forward *lisp-def-regexp*)
	     (and (setq string (symbol-under-point))
		  (add-completion-to-tail-if-new string))
	     )
	 (search-failed)
	 ))))


;;;-----------------------------------------------
;;; C file completion parsing
;;;-----------------------------------------------
;;; C :
;;;  Looks for #define or [<storage class>] [<type>] <name>{,<name>}
;;; or structure, array or pointer defs.
;;; It gets most of the definition names.
;;;
;;; As you might suspect by now, we use some symbol table hackery
;;;
;;; Symbol separator chars (have whitespace syntax) --> , ; * = (
;;; Opening char --> [ {
;;; Closing char --> ] }
;;; openning and closing must be skipped over
;;; Whitespace chars (have symbol syntax)
;;; Everything else has word syntax

(defun make-c-def-completion-syntax-table ()
  (let ((table (make-vector 256 0))
	(whitespace-chars '(?  ?\n ?\t ?\f  ?\v ?\r))
	;; unforunately the ?( causes the parens to appear unbalanced
	(separator-chars '(?, ?* ?= ?\( ?\;
			   ))
	)
    ;; default syntax is whitespace
    (dotimes (i 256)
      (modify-syntax-entry i "w" table))
    (dolist (char whitespace-chars)
      (modify-syntax-entry char "_" table))
    (dolist (char separator-chars)
      (modify-syntax-entry char " " table))
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\} "){" table)
    table))

(defconst cmpl-c-def-syntax-table (make-c-def-completion-syntax-table))

;;; Regexps
(defconst *c-def-regexp*
    ;; This stops on lines with possible definitions
    "\n[_a-zA-Z#]"
  ;; This stops after the symbol to add.
  ;;"\n\\(#define\\s +.\\|\\(\\(\\w\\|\\s_\\)+\\b\\s *\\)+[(;,[*{=]\\)"
  ;; This stops before the symbol to add.  {Test cases in parens. below}
  ;;"\n\\(\\(\\w\\|\\s_\\)+\\s *(\\|\\(\\(#define\\|auto\\|extern\\|register\\|static\\|int\\|long\\|short\\|unsigned\\|char\\|void\\|float\\|double\\|enum\\|struct\\|union\\|typedef\\)\\s +\\)+\\)"
  ;; this simple version picks up too much extraneous stuff
  ;; "\n\\(\\w\\|\\s_\\|#\\)\\B"
  "A regexp that searches for a definition form."
  )
;
;(defconst *c-cont-regexp*
;  "\\(\\w\\|\\s_\\)+\\b\\s *\\({\\|\\(\\[[0-9\t ]*\\]\\s *\\)*,\\(*\\|\\s \\)*\\b\\)"
;  "This regexp should be used in a looking-at to parse for lists of variables.")
;
;(defconst *c-struct-regexp*
;  "\\(*\\|\\s \\)*\\b"
;  "This regexp should be used to test whether a symbol follows a structure definition.")

;(defun test-c-def-regexp (regexp string)
;  (and (eq 0 (string-match regexp string)) (match-end 0))
;  )

;;; Tests -
;;;  (test-c-def-regexp *c-def-regexp* "\n#define foo") -> 10 (9)
;;;  (test-c-def-regexp *c-def-regexp* "\nfoo (x, y) {") -> 6 (6)
;;;  (test-c-def-regexp *c-def-regexp* "\nint foo (x, y)") -> 10 (5)
;;;  (test-c-def-regexp *c-def-regexp* "\n int foo (x, y)") -> nil
;;;  (test-c-def-regexp *c-cont-regexp* "oo, bar") -> 4
;;;  (test-c-def-regexp *c-cont-regexp* "oo, *bar") -> 5
;;;  (test-c-def-regexp *c-cont-regexp* "a [5][6], bar") -> 10
;;;  (test-c-def-regexp *c-cont-regexp* "oo(x,y)") -> nil
;;;  (test-c-def-regexp *c-cont-regexp* "a [6] ,\t bar") -> 9
;;;  (test-c-def-regexp *c-cont-regexp* "oo {trout =1} my_carp;") -> 14
;;;  (test-c-def-regexp *c-cont-regexp* "truct_p complex foon") -> nil

(defun add-completions-from-c-buffer ()
  "Parses all the definition names from a C mode buffer and adds them to the 
completion database."
  ;; Benchmark --
  ;;  Sun 3/280-- 1250 lines/sec.

  (let (string next-point char
	(saved-syntax (syntax-table))
	)
    (save-excursion
      (goto-char (point-min))
      (catch 'finish-add-completions
	(unwind-protect
	     (while t
	       ;; we loop here only when scan-sexps fails
	       ;; (i.e. unbalance exps.)
	       (set-syntax-table cmpl-c-def-syntax-table)
	       (condition-case e
		    (while t
		      (re-search-forward *c-def-regexp*)
		      (cond
			((= (preceding-char) ?#)
			 ;; preprocessor macro, see if it's one we handle
			 (setq string (buffer-substring (point) (+ (point) 6)))
			 (cond ((or (string-equal string "define")
				    (string-equal string "ifdef ")
				    )
				;; skip forward over definition symbol
				;; and add it to database
				(and (forward-word 2)
				     (setq string (symbol-before-point))
				     ;;(push string foo)
				     (add-completion-to-tail-if-new string)
				     ))))
			(t
			 ;; C definition
			 (setq next-point (point))
			 (while (and
				  next-point
				  ;; scan to next separator char.
				  (setq next-point (scan-sexps next-point 1))
				  )
			   ;; position the point on the word we want to add
			   (goto-char next-point)
			   (while (= (setq char (following-char)) ?*)
			     ;; handle pointer ref
			     ;; move to next separator char.
			     (goto-char
			       (setq next-point (scan-sexps (point) 1)))
			     )
			   (forward-word -1)
			   ;; add to database
			   (if (setq string (symbol-under-point))
			       ;; (push string foo)
			       (add-completion-to-tail-if-new string)
			       ;; Local TMC hack (useful for parsing paris.h)
			       (if (and (looking-at "_AP") ;; "ansi prototype"
					(progn
					  (forward-word -1)
					  (setq string
						(symbol-under-point))
					  ))
				   (add-completion-to-tail-if-new string)
				   )
			       )
			   ;; go to next
			   (goto-char next-point)
			   ;; (push (format "%c" (following-char)) foo)
			   (if (= (char-syntax char) ?\()
			       ;; if on an opening delimiter, go to end
			       (while (= (char-syntax char) ?\()
				 (setq next-point (scan-sexps next-point 1)
				       char (char-after next-point))
				 )
			       (or (= char ?,)
				   ;; Current char is an end char.
				   (setq next-point nil)
				   ))
			   ))))
		  (search-failed ;;done
		    (throw 'finish-add-completions t)
		    )
		  (error
		    ;; Check for failure in scan-sexps
		    (if (or (string-equal (second e)
					  "Containing expression ends prematurely")
			    (string-equal (second e) "Unbalanced parentheses"))
			;; unbalanced paren., keep going
			;;(ding)
			(forward-line 1)
			(message "Error parsing C buffer for completions.  Please bug report.")
			(throw 'finish-add-completions t)
			))
		  ))
	  (set-syntax-table saved-syntax)
	  )))))


;;;---------------------------------------------------------------------------
;;; Init files
;;;---------------------------------------------------------------------------

(defun kill-emacs-save-completions ()
  "The version of save-completions-to-file called at kill-emacs time."
  (when (and *save-completions-p* *completep* cmpl-initialized-p)
    (cond
      ((not cmpl-completions-accepted-p)
       (message "Completions database has not changed - not writing."))
      (t
       (save-completions-to-file)
       ))
    ))

(defconst saved-cmpl-file-header
    ";;; Completion Initialization file.
;;; Version = %s
;;; Format is (<string> . <last-use-time>)
;;;  <string> is the completion
;;;  <last-use-time> is the time the completion was last used
;;;    If it is t, the completion will never be pruned from the file.
;;;    Otherwise it is in hours since 1900.
\n")

(defun completion-backup-filename (filename)
  (concat filename ".BAK"))

(defun save-completions-to-file (&optional filename)
  "Saves a completion init file.
If file is not specified, then *saved-completions-filename* is used."
  (interactive)
  (setq filename (expand-file-name (or filename *saved-completions-filename*)))
  (when (file-writable-p filename)
    (if (not cmpl-initialized-p)
	(initialize-completions));; make sure everything's loaded
    (message "Saving completions to file %s" filename)

    (let* ((trim-versions-without-asking t)
	   (kept-old-versions 0)
	   (kept-new-versions *completion-file-versions-kept*)
	   last-use-time
	   (current-time (cmpl-hours-since-1900))
	   (total-in-db 0)
	   (total-perm 0)
	   (total-saved 0)
	   (backup-filename (completion-backup-filename filename))
	   )
    
      (save-excursion
	(get-buffer-create " *completion-save-buffer*")
	(set-buffer  " *completion-save-buffer*")
	(setq buffer-file-name filename)

	(when (not (verify-visited-file-modtime (current-buffer)))
	  ;; file has changed on disk.  Bring us up-to-date
	  (message "Completion file has changed.  Merging. . .")
	  (load-completions-from-file filename t)
	  (message "Merging finished.  Saving completions to file %s" filename)
	  )

	;; prepare the buffer to be modified
	(clear-visited-file-modtime)
	(erase-buffer)
	;; (/ 1 0)
	(insert (format saved-cmpl-file-header *completion-version*))
	(dolist (completion (list-all-completions))
	  (setq total-in-db (1+ total-in-db))
	  (setq last-use-time (completion-last-use-time completion))
	  ;; Update num uses and maybe write completion to a file
	  (cond ((or;; Write to file if
		  ;; permanent
		  (and (eq last-use-time t)
		       (setq total-perm (1+ total-perm)))
		  ;; or if
		  (if (plusp (completion-num-uses completion))
		      ;; it's been used
		      (setq last-use-time current-time)
		      ;; or it was saved before and
		      (and last-use-time
			   ;; *saved-completion-retention-time* is nil
			   (or (not *saved-completion-retention-time*)
			       ;; or time since last use is < ...retention-time*
			       (< (- current-time last-use-time)
				  *saved-completion-retention-time*))
			   )))
		 ;; write to file
		 (setq total-saved (1+ total-saved))
		 (insert (prin1-to-string (cons (completion-string completion)
						last-use-time)) "\n")
		 )))
	
	;; write the buffer
	(condition-case e
	     (let ((file-exists-p (file-exists-p filename)))
	       (when file-exists-p
		 ;; If file exists . . .
		 ;; Save a backup(so GNU doesn't screw us when we're out of disk)
		 ;; (GNU leaves a 0 length file if it gets a disk full error!)
	       
		 ;; If backup doesn't exit, Rename current to backup
		 ;;  {If backup exists the primary file is probably messed up}
		 (unless (file-exists-p backup-filename)
		   (rename-file filename backup-filename))
		 ;; Copy the backup back to the current name
		 ;; (so versioning works)
		 (copy-file backup-filename filename t)
		 )
	       ;; Save it
	       (save-buffer)
	       (when file-exists-p
		 ;; If successful, remove backup
		 (delete-file backup-filename)
		 ))
	   (error
	    (set-buffer-modified-p nil)
	    (message "Couldn't save completion file %s." filename)
	    ))
	;; Reset accepted-p flag
	(setq cmpl-completions-accepted-p nil) 
	)
      (cmpl-statistics-block
       (record-save-completions total-in-db total-perm total-saved))
      )))

(defun autosave-completions ()
  (when (and *save-completions-p* *completep* cmpl-initialized-p
	     *completion-auto-save-period*
	     (> cmpl-emacs-idle-time *completion-auto-save-period*)
	     cmpl-completions-accepted-p)
    (save-completions-to-file)
    ))

(pushnew 'autosave-completions cmpl-emacs-idle-time-hooks)

(defun load-completions-from-file (&optional filename no-message-p)
  "Loads a completion init file.
If file is not specified, then *saved-completions-filename* is used."
  (interactive)
  (setq filename (expand-file-name (or filename *saved-completions-filename*)))
  (let* ((backup-filename (completion-backup-filename filename))
	 (backup-readable-p (file-readable-p backup-filename))
	 )
    (when backup-readable-p (setq filename backup-filename))
    (when (file-readable-p filename)
      (if (not no-message-p)
	  (message "Loading completions from %sfile %s . . ."
		   (if backup-readable-p "backup " "") filename))
      (save-excursion
	(get-buffer-create " *completion-save-buffer*")
	(set-buffer  " *completion-save-buffer*")
	(setq buffer-file-name filename)
	;; prepare the buffer to be modified
	(clear-visited-file-modtime)
	(erase-buffer)
  
	(let ((insert-okay-p nil)
	      (buffer (current-buffer))
	      (current-time (cmpl-hours-since-1900))
	      string num-uses entry last-use-time
	      cmpl-entry cmpl-last-use-time
	      (current-completion-source cmpl-source-init-file)
	      (start-num
	       (cmpl-statistics-block
		(aref completion-add-count-vector cmpl-source-file-parsing)))
	      (total-in-file 0) (total-perm 0)
	      )
	  ;; insert the file into a buffer
	  (condition-case e
	       (progn (insert-file-contents filename t)
		      (setq insert-okay-p t))

	     (file-error 
	      (message "File error trying to load completion file %s."
		       filename)))
	  ;; parse it 
	  (when insert-okay-p
	    (goto-char (point-min))

	    (condition-case e
		 (while t
		   (setq entry (read buffer))
		   (setq total-in-file (1+ total-in-file))
		   (cond
		     ((and (consp entry)
			   (stringp (setq string (car entry)))
			   (cond
			     ((eq (setq last-use-time (cdr entry)) 'T)
			      ;; handle case sensitivity
			      (setq total-perm (1+ total-perm))
			      (setq last-use-time t))
			     ((eq last-use-time t)
			      (setq total-perm (1+ total-perm)))
			     ((integerp last-use-time))
			     ))
		      ;; Valid entry
		      ;; add it in
		      (setq cmpl-last-use-time
			    (completion-last-use-time
			     (setq cmpl-entry
				   (add-completion-to-tail-if-new string))
			     ))
		      (if (or (eq last-use-time t) 
			      (and (> last-use-time 1000);;backcompatibility
				   (not (eq cmpl-last-use-time t))
				   (or (not cmpl-last-use-time)
				       ;; more recent
				       (> last-use-time  cmpl-last-use-time))
				   ))
			  ;; update last-use-time
			  (set-completion-last-use-time cmpl-entry last-use-time)
			  ))
		     (t
		      ;; Bad format
		      (message "Error: invalid saved completion - %s"
			       (prin1-to-string entry))
		      ;; try to get back in sync
		      (search-forward "\n(")
		      )))
	       (search-failed
		(message "End of file while reading completions.")
		)
	       (end-of-file
		(if (= (point) (point-max))
		    (if (not no-message-p)
			(message "Loading completions from file %s . . . Done."
				 filename))
		    (message "End of file while reading completions.")
		    ))
	       ))

	  (cmpl-statistics-block
	   (record-load-completions
	    total-in-file total-perm
	    (- (aref completion-add-count-vector cmpl-source-init-file)
	       start-num)))

	  )))))

(defun initialize-completions ()
  "Loads the default completions file.
Also sets up so that exiting emacs will automatically save the file."
  (interactive)
  (cond ((not cmpl-initialized-p)
	 (load-completions-from-file)
	 ))
  (init-cmpl-emacs-idle-process)
  (setq cmpl-initialized-p t)
  )


;;;-----------------------------------------------
;;; Kill EMACS patch
;;;-----------------------------------------------

(completion-advise kill-emacs :before
    ;; | All completion code should go in here
    ;;\ /
    (kill-emacs-save-completions)
    ;;/ \
    ;; | All completion code should go in here
    (cmpl-statistics-block
      (record-cmpl-kill-emacs))
    )


;;;-----------------------------------------------
;;; Kill region patch
;;;-----------------------------------------------

;;; Patched to remove the most recent completion
(defvar $$$cmpl-old-kill-region (symbol-function 'kill-region))

(defun kill-region (&optional beg end)
  "Kill between point and mark.
The text is deleted but saved in the kill ring.
The command \\[yank] can retrieve it from there.
/(If you want to kill and then yank immediately, use \\[copy-region-as-kill].)

This is the primitive for programs to kill text (as opposed to deleting it).
Supply two arguments, character numbers indicating the stretch of text
 to be killed.
Any command that calls this function is a \"kill command\".
If the previous command was also a kill command,
the text killed this time appends to the text killed last time
to make one entry in the kill ring.
Patched to remove the most recent completion."
  (interactive "*")  
  (cond ((and (eq last-command 'complete) (eq last-command-char ?\C-w))
	 (delete-region (point) cmpl-last-insert-location)
	 (insert cmpl-original-string)
	 (setq completion-to-accept nil)
	 (cmpl-statistics-block
	   (record-complete-failed))
	 )
	(t
	  (if (not beg)
	      (setq beg (min (point) (mark))
		    end (max (point) (mark)))
	    )
	 (funcall $$$cmpl-old-kill-region beg end)
	 )))

;;;-----------------------------------------------
;;; Patches to self-insert-command.
;;;-----------------------------------------------

;;; Need 2 versions: generic seperator chars. and space (to get auto fill
;;; to work)

;;; All common separators (eg. space "(" ")" """) characters go through a
;;; function to add new words to the list of words to complete from:
;;;  COMPLETION-SEPARATOR-SELF-INSERT-COMMAND (arg).
;;; If the character before this was an alpha-numeric then this adds the 
;;; symbol befoe point to the completion list (using ADD-COMPLETION).

(defun completion-separator-self-insert-command (arg)
  (interactive "p")
  (use-completion-before-separator)
  (self-insert-command arg)
  )

(defun completion-separator-self-insert-autofilling (arg)
  (interactive "p")
  (use-completion-before-separator)
  (self-insert-command arg)
  (and (> (current-column) fill-column)
       auto-fill-function
       (funcall auto-fill-function))
  )

;;;-----------------------------------------------
;;; Wrapping Macro
;;;-----------------------------------------------

;;; Note that because of the way byte compiling works, none of 
;;; the functions defined with this macro get byte compiled.

(defmacro def-completion-wrapper (function-name type &optional new-name)
  "Add a call to update the completion database before function execution.
TYPE is the type of the wrapper to be added.  Can be :before or :under."
  (completion-advise-1
   function-name ':before
   (ecase type
     (:before '((use-completion-before-point)))
     (:separator '((use-completion-before-separator)))
     (:under '((use-completion-under-point)))
     (:under-or-before
      '((use-completion-under-or-before-point)))
     (:minibuffer-separator
      '((let ((cmpl-syntax-table cmpl-standard-syntax-table))
	  (use-completion-before-separator))))
     )
   new-name
   ))

;;;(defun foo (x y z) (+ x y z))
;;;foo
;;;(macroexpand '(def-completion-wrapper foo :under))
;;;(progn (defvar $$$cmpl-foo (symbol-function (quote foo))) (defun foo (&rest arglist) (progn (use-completion-under-point)) (cmpl-apply-as-top-level $$$cmpl-foo arglist)))
;;;(defun bar (x y z) "Documentation" (+ x y z))
;;;bar
;;;(macroexpand '(def-completion-wrapper bar :under))
;;;(progn (defvar $$$cmpl-bar (symbol-function (quote bar))) (defun bar (&rest arglist) "Documentation" (progn (use-completion-under-point)) (cmpl-apply-as-top-level $$$cmpl-bar arglist)))
;;;(defun quuz (x &optional y z) "Documentation" (interactive "P") (+ x y z))
;;;quuz
;;;(macroexpand '(def-completion-wrapper quuz :before))
;;;(progn (defvar $$$cmpl-quuz (symbol-function (quote quuz))) (defun quuz (&rest arglist) "Documentation" (interactive) (progn (use-completion-before-point)) (cmpl-apply-as-top-level $$$cmpl-quuz arglist)))


;;;---------------------------------------------------------------------------
;;; Patches to standard keymaps insert completions
;;;---------------------------------------------------------------------------

;;;-----------------------------------------------
;;; Separators
;;;-----------------------------------------------
;;; We've used the completion syntax table given  as a guide.
;;;
;;; Global separator chars.
;;;  We left out <tab> because there are too many special cases for it.  Also,
;;; in normal coding it's rarely typed after a word.
(global-set-key " " 'completion-separator-self-insert-autofilling)
(global-set-key "!" 'completion-separator-self-insert-command)
(global-set-key "%" 'completion-separator-self-insert-command)
(global-set-key "^" 'completion-separator-self-insert-command)
(global-set-key "&" 'completion-separator-self-insert-command)
(global-set-key "(" 'completion-separator-self-insert-command)
(global-set-key ")" 'completion-separator-self-insert-command)
(global-set-key "=" 'completion-separator-self-insert-command)
(global-set-key "`" 'completion-separator-self-insert-command)
(global-set-key "|" 'completion-separator-self-insert-command)
(global-set-key "{" 'completion-separator-self-insert-command)
(global-set-key "}" 'completion-separator-self-insert-command)
(global-set-key "[" 'completion-separator-self-insert-command)
(global-set-key "]" 'completion-separator-self-insert-command)
(global-set-key ";" 'completion-separator-self-insert-command)
(global-set-key "\"" 'completion-separator-self-insert-command)
(global-set-key "'" 'completion-separator-self-insert-command)
(global-set-key "#" 'completion-separator-self-insert-command)
(global-set-key "," 'completion-separator-self-insert-command)
(global-set-key "?" 'completion-separator-self-insert-command)

;;; We include period and colon even though they are symbol chars because :
;;;  - in text we want to pick up the last word in a sentence.
;;;  - in C pointer refs. we want to pick up the first symbol
;;;  - it won't make a difference for lisp mode (package names are short)
(global-set-key "." 'completion-separator-self-insert-command)
(global-set-key ":" 'completion-separator-self-insert-command)

;;; Lisp Mode diffs
(define-key lisp-mode-map "!" 'self-insert-command)
(define-key lisp-mode-map "&" 'self-insert-command)
(define-key lisp-mode-map "%" 'self-insert-command)
(define-key lisp-mode-map "?" 'self-insert-command)
(define-key lisp-mode-map "=" 'self-insert-command)
(define-key lisp-mode-map "^" 'self-insert-command)

;;; C mode diffs.
(def-completion-wrapper electric-c-semi :separator)
(define-key c-mode-map "+" 'completion-separator-self-insert-command)
(define-key c-mode-map "*" 'completion-separator-self-insert-command)
(define-key c-mode-map "/" 'completion-separator-self-insert-command)

;;; FORTRAN mode diffs. (these are defined when fortran is called)
(defun completion-setup-fortran-mode ()
  (define-key fortran-mode-map "+" 'completion-separator-self-insert-command)
  (define-key fortran-mode-map "-" 'completion-separator-self-insert-command)
  (define-key fortran-mode-map "*" 'completion-separator-self-insert-command)
  (define-key fortran-mode-map "/" 'completion-separator-self-insert-command)
  )

;;;-----------------------------------------------
;;; End of line chars.
;;;-----------------------------------------------
(def-completion-wrapper newline :separator)
(def-completion-wrapper newline-and-indent :separator)
;;;(if (function-defined-and-loaded 'shell-send-input)
;;;    (def-completion-wrapper shell-send-input :separator))
(def-completion-wrapper exit-minibuffer :minibuffer-separator)
(def-completion-wrapper eval-print-last-sexp :separator)
(def-completion-wrapper eval-last-sexp :separator)
;;(def-completion-wrapper minibuffer-complete-and-exit :minibuffer)

;;;-----------------------------------------------
;;; Cursor movement
;;;-----------------------------------------------

(def-completion-wrapper next-line :under-or-before)
(def-completion-wrapper previous-line :under-or-before)
(def-completion-wrapper beginning-of-buffer :under-or-before)
(def-completion-wrapper end-of-buffer :under-or-before)

;; we patch these explicitly so they byte compile and so we don't have to
;; patch the faster underlying function.

(defun cmpl-beginning-of-line (&optional n)
  "Move point to beginning of current line.\n\
With argument ARG not nil or 1, move forward ARG - 1 lines first.\n\
If scan reaches end of buffer, stop there without error."
  (interactive "p")
  (use-completion-under-or-before-point)
  (beginning-of-line n)
  )

(defun cmpl-end-of-line (&optional n)
  "Move point to end of current line.\n\
With argument ARG not nil or 1, move forward ARG - 1 lines first.\n\
If scan reaches end of buffer, stop there without error."
  (interactive "p")
  (use-completion-under-or-before-point)
  (end-of-line n)
  )

(defun cmpl-forward-char (n)
  "Move point right ARG characters (left if ARG negative).\n\
On reaching end of buffer, stop and signal error."
  (interactive "p")
  (use-completion-under-or-before-point)
  (forward-char n)
  )
(defun cmpl-backward-char (n)
  "Move point left ARG characters (right if ARG negative).\n\
On attempt to pass beginning or end of buffer, stop and signal error."
  (interactive "p")
  (use-completion-under-point)
  (if (eq last-command 'complete)
      ;; probably a failed completion if you have to back up
      (cmpl-statistics-block (record-complete-failed)))
  (backward-char n)
  )

(defun cmpl-forward-word (n)
  "Move point forward ARG words (backward if ARG is negative).\n\
Normally returns t.\n\
If an edge of the buffer is reached, point is left there\n\
and nil is returned."
  (interactive "p")
  (use-completion-under-or-before-point)
  (forward-word n)
  )
(defun cmpl-backward-word (n)
  "Move backward until encountering the end of a word.
With argument, do this that many times.
In programs, it is faster to call forward-word with negative arg."
  (interactive "p")
  (use-completion-under-point)
  (if (eq last-command 'complete)
      ;; probably a failed completion if you have to back up
      (cmpl-statistics-block (record-complete-failed)))
  (forward-word (- n))
  )

(defun cmpl-forward-sexp (n)
  "Move forward across one balanced expression.
With argument, do this that many times."
  (interactive "p")
  (use-completion-under-or-before-point)
  (forward-sexp n)
  )
(defun cmpl-backward-sexp (n)
  "Move backward across one balanced expression.
With argument, do this that many times."
  (interactive "p")
  (use-completion-under-point)
  (if (eq last-command 'complete)
      ;; probably a failed completion if you have to back up
      (cmpl-statistics-block (record-complete-failed)))
  (backward-sexp n)
  )

(defun cmpl-delete-backward-char (n killflag)
  "Delete the previous ARG characters (following, with negative ARG).\n\
Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).\n\
Interactively, ARG is the prefix arg, and KILLFLAG is set if\n\
ARG was explicitly specified."
  (interactive "p\nP")
  (if (eq last-command 'complete)
      ;; probably a failed completion if you have to back up
      (cmpl-statistics-block (record-complete-failed)))
  (delete-backward-char n killflag)
  )

(defvar $$$cmpl-old-backward-delete-char-untabify
  (symbol-function 'backward-delete-char-untabify))

(defun backward-delete-char-untabify (arg &optional killp)
  "Delete characters backward, changing tabs into spaces.
Delete ARG chars, and kill (save in kill ring) if KILLP is non-nil.
Interactively, ARG is the prefix arg (default 1)
and KILLP is t if prefix arg is was specified."
  (interactive "*p\nP")
  (if (eq last-command 'complete)
      ;; probably a failed completion if you have to back up
      (cmpl-statistics-block (record-complete-failed)))
  (funcall $$$cmpl-old-backward-delete-char-untabify arg killp)
  )


(global-set-key "\C-?" 'cmpl-delete-backward-char)
(global-set-key "\M-\C-F" 'cmpl-forward-sexp)
(global-set-key "\M-\C-B" 'cmpl-backward-sexp)
(global-set-key "\M-F" 'cmpl-forward-word)
(global-set-key "\M-B" 'cmpl-backward-word)
(global-set-key "\C-F" 'cmpl-forward-char)
(global-set-key "\C-B" 'cmpl-backward-char)
(global-set-key "\C-A" 'cmpl-beginning-of-line)
(global-set-key "\C-E" 'cmpl-end-of-line)

;;;-----------------------------------------------
;;; Misc.
;;;-----------------------------------------------

(def-completion-wrapper electric-buffer-list :under-or-before)
(def-completion-wrapper list-buffers :under-or-before)
(def-completion-wrapper scroll-up :under-or-before)
(def-completion-wrapper scroll-down :under-or-before)
(def-completion-wrapper execute-extended-command
    :under-or-before)
(def-completion-wrapper other-window :under-or-before)

;;;-----------------------------------------------
;;; Local Thinking Machines stuff
;;;-----------------------------------------------

;;; Tests --
;;; foobarbiz
;;; foobar 
;;; fooquux 
;;; fooper

(cmpl-statistics-block
  (record-completion-file-loaded))

;;; completion.el ends here
