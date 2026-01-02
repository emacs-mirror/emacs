;;; dictionary.el --- Client for rfc2229 dictionary servers  -*- lexical-binding:t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; Author: Torsten Hilbrich <torsten.hilbrich@gmx.net>
;; Keywords: interface, dictionary

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

;; dictionary allows you to interact with dictionary servers.
;;
;; Use `M-x customize-group RET dictionary RET' to modify user settings.
;;
;; Main commands for interaction are:
;; `M-x dictionary'        - open a new dictionary buffer
;; `M-x dictionary-search' - search for the definition of a word
;;
;; You can find more information in the README file of the GitHub
;; repository https://github.com/myrkr/dictionary-el

;;; Code:

(require 'custom)
(require 'dictionary-connection)
(require 'button)
(require 'help-mode)
(require 'external-completion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff for customizing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dictionary-current-server)
(defun dictionary-set-server-var (name value)
  "Customize helper for setting variable NAME to VALUE.
The helper is used by customize to check for an active connection
when setting a variable.  The user has then the choice to close
the existing connection."
  (if (and (boundp 'dictionary-connection)
	   dictionary-connection
	   (eq (dictionary-connection-status dictionary-connection) 'up)
	   (y-or-n-p
	    (concat "Close existing connection to " dictionary-current-server "? ")))
      (dictionary-connection-close dictionary-connection))
  (set-default name value))

(defgroup dictionary nil
  "Client for accessing the dictd server based dictionaries."
  :group 'applications)

(defgroup dictionary-proxy nil
  "Proxy configuration options for the dictionary client."
  :group 'dictionary)

(defcustom dictionary-server
  nil
  "This server is contacted for searching the dictionary.

You can specify here:

- Automatic: First try localhost, then dict.org after confirmation
- localhost: Only use localhost
- dict.org: Only use dict.org
- User-defined: You can specify your own server here"
  :group 'dictionary
  :set #'dictionary-set-server-var
  :type '(choice (const :tag "Automatic" nil)
                 (const :tag "localhost" "localhost")
                 (const :tag "dict.org" "dict.org")
                 (string :tag "User-defined"))
  :version "28.1")

(defcustom dictionary-port
  2628
  "The port of the dictionary server.
This port is probably always 2628 so there should be no need to modify it."
  :group 'dictionary
  :set #'dictionary-set-server-var
  :type 'natnum
  :version "28.1")

(defcustom dictionary-identification
  "dictionary.el emacs lisp dictionary client"
  "This is the identification string that will be sent to the server."
  :group 'dictionary
  :type 'string
  :version "28.1")

(defcustom dictionary-default-dictionary
  "*"
  "The dictionary which is used for searching definitions and matching.
* and ! have a special meaning, * search all dictionaries, ! search until
one dictionary yields matches.
Otherwise, the value should be a string, the name of the dictionary to use.
Dictionary names are generally specific to the servers, and are obtained
via `dictionary-dictionaries'."
  :group 'dictionary
  :type 'string
  :version "28.1")

(defcustom dictionary-default-strategy
  "."
  "The default strategy for listing matching words.
The value should be a string.  The special value \".\" means
the default search strategy for `dictionary-server' in use.
Other values are specific to servers and dictionaries.
In a `dictionary-mode' buffer, you can use
\\[dictionary-select-strategy] to change the buffer-local value;
it will show the available strategies from which you can choose.
To change the value for other buffers, customize this option
using \\[customize-option]."
  :group 'dictionary
  :type 'string
  :version "28.1")

(defcustom dictionary-default-popup-strategy
  "exact"
  "The default strategy for listing matching words within a popup window.

The following algorithms (defined by the dictd server) are supported
by the choice value:

- Exact match

  The found word exactly matches the searched word.

- Similar sounding

  The found word sounds similar to the searched word.  For this match type
  the soundex algorithm defined by Donald E. Knuth is used.  It will only
  work with English words and the algorithm is not very reliable (i.e.,
  the soundex algorithm is quite simple).

- Levenshtein distance one

  The Levenshtein distance is defined as the number of insertions, deletions,
  or replacements needed to get the searched word.  This algorithm searches
  for word where spelling mistakes are allowed.  Levenshtein distance one
  means there is either a deleted character, an inserted character, or a
  modified one.

- User choice

  Here you can enter any matching algorithm supported by your
  dictionary server."
  :group 'dictionary
  :type '(choice (const :tag "Exact match" "exact")
		 (const :tag "Similar sounding" "soundex")
		 (const :tag "Levenshtein distance one" "lev")
		 (string :tag "User choice"))
  :version "28.1")

(defcustom dictionary-create-buttons
  t
  "Create some clickable buttons on top of the window if non-nil."
  :group 'dictionary
  :type 'boolean
  :version "28.1")

(defcustom dictionary-link-dictionary
  "*"
  "The dictionary which is used in links.
* means to create links that search all dictionaries,
nil means to create links that search only in the same dictionary
where the current word was found."
  :group 'dictionary
  :type '(choice (const :tag "Link to all dictionaries" "*")
		 (const :tag "Link only to the same dictionary" nil)
		 (string :tag "User choice"))
  :version "28.1")

(defcustom dictionary-mode-hook
  nil
  "Hook run in dictionary mode buffers."
  :group 'dictionary
  :type 'hook
  :version "28.1")

(defcustom dictionary-post-buffer-hook
  nil
  "Hook run at the end of every update of the dictionary buffer."
  :group 'dictionary
  :type 'hook
  :version "28.1")

(defcustom dictionary-use-http-proxy
  nil
  "Connects via a HTTP proxy using the CONNECT command when not nil."
  :group 'dictionary-proxy
  :set #'dictionary-set-server-var
  :type 'boolean
  :version "28.1")

(defcustom dictionary-proxy-server
  "proxy"
  "The name of the HTTP proxy to use when `dictionary-use-http-proxy' is set."
  :group 'dictionary-proxy
  :set #'dictionary-set-server-var
  :type 'string
  :version "28.1")

(defcustom dictionary-proxy-port
  3128
  "The port of the proxy server, used only when `dictionary-use-http-proxy' is set."
  :group 'dictionary-proxy
  :set #'dictionary-set-server-var
  :type 'natnum
  :version "28.1")

(defcustom dictionary-use-single-buffer
  nil
  "Should the dictionary command reuse previous dictionary buffers?"
  :group 'dictionary
  :type 'boolean
  :version "28.1")

(defcustom dictionary-description-open-delimiter
  ""
  "The delimiter to display in front of the dictionaries description."
  :group 'dictionary
  :type 'string
  :version "28.1")

(defcustom dictionary-description-close-delimiter
  ""
  "The delimiter to display after of the dictionaries description."
  :group 'dictionary
  :type 'string
  :version "28.1")

;; Define only when coding-system-list is available
(defcustom dictionary-coding-systems-for-dictionaries
  '( ("mueller" . koi8-r))
  "Mapping of dictionaries to coding systems.
Each entry in this list defines the coding system to be used for that
dictionary.  The default coding system for all other dictionaries
is utf-8"
  :group 'dictionary
  :type `(repeat (cons :tag "Association"
                       (string :tag "Dictionary name")
                       (choice :tag "Coding system"
                               :value 'utf-8
                               ,@(mapcar (lambda (x) (list 'const x))
                                         (coding-system-list))
                               )))
  :version "28.1")

(defcustom dictionary-read-word-prompt "Search word"
  "Prompt string to use when prompting for a word."
  :type 'string
  :version "30.1")

(defcustom dictionary-display-definition-function nil
  "Function to use for displaying dictionary definitions.
It is called with three string arguments: the word being defined,
the dictionary name, and the full definition."
  :type '(choice (const :tag "Dictionary buffer" nil)
                 (const :tag "Help buffer"
                        dictionary-display-definition-in-help-buffer)
                 (function :tag "Custom function"))
  :version "30.1")

(defcustom dictionary-read-word-function #'dictionary-read-word-default
  "Function to use for prompting for a word.
It is called with one string argument, the name of the dictionary to use, and
must return a string."
  :type '(choice (const :tag "Default" dictionary-read-word-default)
                 (const :tag "Dictionary-based completion"
                        dictionary-completing-read-word)
                 (function :tag "Custom function"))
  :version "30.1")

(defcustom dictionary-read-dictionary-function
  #'dictionary-read-dictionary-default
  "Function to use for prompting for a dictionary.
It is called with no arguments and must return a string."
  :type '(choice (const :tag "Default" dictionary-read-dictionary-default)
                 (const :tag "Choose among server-provided dictionaries"
                        dictionary-completing-read-dictionary)
                 (function :tag "Custom function"))
  :version "30.1")

(defcustom dictionary-search-interface nil
  "Controls how `dictionary-search' prompts for words and displays definitions.

When set to `help', `dictionary-search' displays definitions in a *Help* buffer,
and provides completion for word selection based on dictionary matches.

Otherwise, `dictionary-search' displays definitions in a *Dictionary* buffer."
  :type '(choice (const :tag "Dictionary buffer" nil)
                 (const :tag "Help buffer" help))
  :set (lambda (symbol value)
         (let ((vals (pcase value
                       ('help '(dictionary-display-definition-in-help-buffer
                                dictionary-completing-read-word
                                dictionary-completing-read-dictionary))
                       (_     '(nil
                                dictionary-read-word-default
                                dictionary-read-dictionary-default)))))
           (seq-setq (dictionary-display-definition-function
                      dictionary-read-word-function
                      dictionary-read-dictionary-function)
                     vals))
         (set-default-toplevel-value symbol value))
  :initialize #'custom-initialize-changed
  :version "30.1")

(defface dictionary-word-definition-face
  ;; w32 first, because 'supports' doesn't really tell whether the font
  ;; family exists, and MS-Windows selects an ugly font as result.
  '((((type w32))
     (:font "Sans Serif"))
    (((supports (:family "DejaVu Serif")))
     (:family "DejaVu Serif"))
    (((type x))
     (:font "Sans Serif"))
    (t
     (:font "default")))
"The face that is used for displaying the definition of the word."
:group 'dictionary
:version "31.1")

(defface dictionary-word-entry-face
  '((((type x))
     (:slant italic))
    (((type tty) (class color))
     (:foreground "green"))
    (t
     (:inverse-video t)))
  "The face that is used for displaying the initial word entry line."
  :group 'dictionary
  :version "28.1")

(defface dictionary-button-face
  '((t
     (:weight bold)))
  "The face that is used for displaying buttons."
  :group 'dictionary
  :version "28.1")

(defface dictionary-reference-face
  '((((type x)
      (class color)
      (background dark))
     (:foreground "yellow"))
    (((type tty)
      (class color)
      (background dark))
     (:foreground "cyan"))
    (((class color)
      (background light))
     (:foreground "blue"))
    (t
     (:underline t)))

  "The face that is used for displaying a reference word."
  :group 'dictionary
  :version "28.1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer local variables for storing the current state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dictionary-window-configuration
  nil
  "The window configuration to be restored upon closing the buffer.")

(defvar dictionary-selected-window
  nil
  "The currently selected window.")

(defvar dictionary-position-stack
  nil
  "The history buffer for point and window position.")

(defvar dictionary-data-stack
  nil
  "The history buffer for functions and arguments.")

(defvar dictionary-positions
  nil
  "The current positions.")

(defvar dictionary-current-data
  nil
  "The item that will be placed on stack next time.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar-keymap dictionary-mode-map
  :doc "Keymap for the dictionary mode."
  :suppress t :parent button-buffer-map
  "q"     #'dictionary-close
  "h"     #'describe-mode
  "s"     #'dictionary-search
  "d"     #'dictionary-lookup-definition
  "D"     #'dictionary-select-dictionary
  "M"     #'dictionary-select-strategy
  "m"     #'dictionary-match-words
  "l"     #'dictionary-previous
  "n"     #'forward-button
  "p"     #'backward-button
  "SPC"   #'scroll-up-command
  "S-SPC" #'scroll-down-command
  "M-SPC" #'scroll-down-command
  "DEL"   #'scroll-down-command)

(easy-menu-define dictionary-mode-menu dictionary-mode-map
  "Menu for the Dictionary mode."
  '("Dictionary"
    ["Search Definition" dictionary-search
     :help "Look up a new word"]
    ["List Matching Words" dictionary-match-words
     :help "List all words matching a pattern"]
    ["Lookup Word At Point" dictionary-lookup-definition
     :help "Look up the word at point"]
    ["Select Dictionary" dictionary-select-dictionary
     :help "Select one or more dictionaries to search within"]
    ["Select Match Strategy" dictionary-select-strategy
     :help "Select the algorithm to match queries and entries with"]
    ["Back" dictionary-previous
     :help "Return to the previous match or location"]))

(defvar dictionary-connection
  nil
  "The current network connection.")

(defvar dictionary-marker
  nil
  "Stores the point position while buffer display.")

(defvar dictionary-word-history
  '()
  "History list of searched word.")

(defvar dictionary--last-match nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic function providing startup actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dictionary-tool-bar-map
  (let ((map (make-sparse-keymap)))
    ;; Most of these items are the same as in the default tool bar
    ;; map, but with extraneous items removed, and with extra search
    ;; and navigation items.
    (tool-bar-local-item-from-menu 'find-file "new" map
                                   nil :label "New File"
			           :vert-only t)
    (tool-bar-local-item-from-menu 'menu-find-file-existing "open" map
                                   nil :label "Open" :vert-only t)
    (tool-bar-local-item-from-menu 'dired "diropen" map nil :vert-only t)
    (tool-bar-local-item-from-menu 'kill-this-buffer "close" map nil
                                   :vert-only t)
    (define-key-after map [separator-1] menu-bar-separator)
    (tool-bar-local-item-from-menu 'dictionary-search "search"
			           map dictionary-mode-map :vert-only t
                                   :help "Start a new search query.")
    (tool-bar-local-item-from-menu 'dictionary-previous "left-arrow"
			           map dictionary-mode-map
                                   :vert-only t
                                   :help "Go backwards in history.")
    map)
  "Like the default `tool-bar-map', but with additions for Dictionary mode.")

;;;###autoload
(define-derived-mode dictionary-mode special-mode "Dictionary"
  "Mode for searching a dictionary.

This is a mode for searching a dictionary server implementing the
protocol defined in RFC 2229.

This is a quick reference to this mode describing the default key bindings:
\\<dictionary-mode-map>
 \\[dictionary-close]	close the dictionary buffer
 \\[describe-mode]	display this help
 \\[dictionary-search]	ask for a new word to search
 \\[dictionary-lookup-definition]	search for word at point
 \\[forward-button] or \\`TAB'	move point to the next link
 \\[backward-button] or \\`S-TAB'	move point to the previous link

 \\[dictionary-match-words]	ask for a pattern and list all matching words
 \\[dictionary-select-dictionary]	select the default dictionary
 \\[dictionary-select-strategy]	select the default search strategy

 \\`RET'	visit link at point
 \\`<mouse-2>'	visit clicked link"
  (buffer-disable-undo)
  (setq-local dictionary-data-stack nil)
  (setq-local dictionary-position-stack nil)
  (make-local-variable 'dictionary-current-data)
  (make-local-variable 'dictionary-positions)
  (make-local-variable 'dictionary-default-dictionary)
  (make-local-variable 'dictionary-default-strategy)
  ;; Replace the tool bar map with `dictionary-tool-bar-map'.
  (setq-local tool-bar-map dictionary-tool-bar-map)
  (add-hook 'kill-buffer-hook #'dictionary-close t t))

;;;###autoload
(defun dictionary ()
  "Create a new dictionary buffer and install `dictionary-mode'."
  (interactive)
  (let ((buffer (or (and dictionary-use-single-buffer
                         (get-buffer "*Dictionary*"))
                    (generate-new-buffer "*Dictionary*")))
        (window-configuration (current-window-configuration))
        (selected-window (frame-selected-window)))

    (switch-to-buffer-other-window buffer)
    (dictionary-mode)

    (setq-local dictionary-window-configuration window-configuration)
    (setq-local dictionary-selected-window selected-window)
    (dictionary-check-connection)
    (dictionary-new-buffer)
    (dictionary-store-positions)
    (dictionary-store-state 'dictionary-new-buffer nil)))

(defun dictionary-new-buffer ()
  "Create a new and clean buffer."

  (dictionary-pre-buffer)
  (dictionary-post-buffer))

(defsubst dictionary-reply-code (reply)
  "Return the reply code stored in REPLY."
  (get reply 'reply-code))

(defsubst dictionary-reply (reply)
  "Return the string reply stored in REPLY."
  (get reply 'reply))

(defsubst dictionary-reply-list (reply)
  "Return the reply list stored in REPLY."
  (get reply 'reply-list))

(defun dictionary-open-server (server)
  "Opens a new connection to SERVER.
The connection takes the proxy setting in customization group
`dictionary-proxy' into account."
  (let ((wanted 'raw-text)
        (coding-system nil))
    (if (member wanted (coding-system-list))
        (setq coding-system wanted))
    (let ((coding-system-for-read coding-system)
          (coding-system-for-write coding-system))
      (setq dictionary-current-server server)
      (message "Opening connection to %s:%s" server
               dictionary-port)
      (dictionary-connection-close dictionary-connection)
      (setq dictionary-connection
            (if dictionary-use-http-proxy
                (dictionary-connection-open dictionary-proxy-server
                                            dictionary-proxy-port)
              (dictionary-connection-open server dictionary-port)))
      (set-process-query-on-exit-flag
       (dictionary-connection-process dictionary-connection)
       nil)

      (when dictionary-use-http-proxy
        (message "Proxy CONNECT to %s:%d"
                 dictionary-proxy-server
                 dictionary-proxy-port)
        (dictionary-send-command (format "CONNECT %s:%d HTTP/1.1"
                                         server
                                         dictionary-port))
        ;; just a \r\n combination
        (dictionary-send-command "")

        ;; read first line of reply
        (let* ((reply (dictionary-read-reply))
               (reply-list (dictionary-split-string reply)))
          ;; first item is protocol, second item is code
          (unless (= (string-to-number (cadr reply-list)) 200)
            (error "Bad reply from proxy server %s" reply))

          ;; skip the following header lines until empty found
          (while (not (equal reply ""))
            (setq reply (dictionary-read-reply)))))

      (dictionary-check-initial-reply)
      (dictionary-send-command (concat "client " dictionary-identification))
      (let ((reply (dictionary-read-reply-and-split)))
        (message nil)
        (unless (dictionary-check-reply reply 250)
          (error "Unknown server answer: %s"
                 (dictionary-reply reply)))))))

(defun dictionary-check-connection ()
  "Check if there is already a connection open."
  (if (not (and dictionary-connection
		(eq (dictionary-connection-status dictionary-connection) 'up)))
      (if dictionary-server
          (dictionary-open-server dictionary-server)
        (let ((server "localhost"))
          (condition-case nil
              (dictionary-open-server server)
            (error
             (if (y-or-n-p
                  (format "Failed to open server %s, continue with dict.org? "
                          server))
                 (dictionary-open-server "dict.org")
               (error "Failed automatic server selection, please customize dictionary-server"))))))))

(defun dictionary-mode-p ()
  "Return non-nil if current buffer has `dictionary-mode'."
  (eq major-mode 'dictionary-mode))

(defun dictionary-ensure-buffer ()
  "If current buffer is not a dictionary buffer, create a new one."
  (unless (dictionary-mode-p)
    (dictionary)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dealing with closing the buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictionary-close (&rest _ignored)
  "Close the current dictionary buffer and its connection."
  (interactive)
  (when (derived-mode-p 'dictionary-mode)
    (setq major-mode nil)
    (if (<= (length (match-buffers '(derived-mode . dictionary-mode))) 0)
        (dictionary-connection-close dictionary-connection))
    (let ((configuration dictionary-window-configuration)
          (selected-window dictionary-selected-window))
      (kill-buffer (current-buffer))
      (set-window-configuration configuration)
      (select-window selected-window))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpful functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictionary-send-command (string)
  "Send the command STRING to the network connection."
  (dictionary-check-connection)
  ;;;; #####
  (dictionary-connection-send-crlf dictionary-connection string))

(defun dictionary-read-reply ()
  "Read the reply line from the server."
  (let ((answer (dictionary-connection-read-crlf dictionary-connection)))
    (if (string-match "\r?\n" answer)
	(substring answer 0 (match-beginning 0))
      answer)))

(defun dictionary-split-string (string)
  "Split STRING consisting of space-separated words into elements.
This function knows about the special meaning of quotes (\")"
  (let ((list))
    (while (and string (> (length string) 0))
      (let ((search "\\(\\s-+\\)")
	    (start 0))
	(if (= (aref string 0) ?\")
	    (setq search "\\(\"\\)\\s-*"
		  start 1))
	(if (string-match search string start)
	    (progn
	      (setq list (cons (substring string start (- (match-end 1) 1)) list)
		    string (substring string (match-end 0))))
	  (setq list (cons string list)
		string nil))))
    (nreverse list)))

(defun dictionary-read-reply-and-split ()
  "Read the reply, split it into words and return it."
  (let ((answer (make-symbol "reply-data"))
	(reply (dictionary-read-reply)))
    (let ((reply-list (dictionary-split-string reply)))
      (put answer 'reply reply)
      (put answer 'reply-list reply-list)
      (put answer 'reply-code (string-to-number (car reply-list)))
      answer)))

(defun dictionary-read-answer ()
  "Read the complete answer.
The answer is delimited by a decimal point (.) on a line by itself."
  (let ((answer (dictionary-connection-read-to-point dictionary-connection))
	(start 0))
    (while (string-match "\r\n" answer start)
      (setq answer (replace-match "\n" t t answer))
      (setq start (1- (match-end 0))))
    (setq start 0)
    (if (string-match "\n\\.\n.*" answer start)
	(setq answer (replace-match "" t t answer)))
    answer))

(defun dictionary-check-reply (reply code)
  "Extract the reply code from REPLY and check against CODE."
  (let ((number (dictionary-reply-code reply)))
    (and (numberp number)
	 (= number code))))

(defun dictionary-coding-system (dictionary)
  "Select coding system to use for DICTIONARY."
  (let ((coding-system
         (or (cdr (assoc dictionary
                         dictionary-coding-systems-for-dictionaries))
             'utf-8)))
    (if (member coding-system (coding-system-list))
        coding-system
      nil)))

(defun dictionary-decode-charset (text dictionary)
  "Convert TEXT from the charset configured for DICTIONARY."
  (let ((coding-system (dictionary-coding-system dictionary)))
    (if coding-system
	(decode-coding-string text coding-system)
      text)))

(defun dictionary-encode-charset (text dictionary)
  "Convert TEXT to the charset defined for DICTIONARY."
  (let ((coding-system (dictionary-coding-system dictionary)))
    (if coding-system
	(encode-coding-string text coding-system)
      text)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communication functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictionary-check-initial-reply ()
  "Read the first reply from server and check it."
  (let ((reply (dictionary-read-reply-and-split)))
    (unless (dictionary-check-reply reply 220)
      (dictionary-connection-close dictionary-connection)
      (error "Server returned: %s" (dictionary-reply reply)))))

;; Store the current state
(defun dictionary-store-state (function data)
  "Store the current state of operation for later restore.
The current state consist of a tuple of FUNCTION and DATA.
This is basically an implementation of a history to return to a
previous state."
  (if dictionary-current-data
      (progn
	(push dictionary-current-data dictionary-data-stack)
	(unless dictionary-positions
	  (error "dictionary-store-state called before dictionary-store-positions"))
	(push dictionary-positions dictionary-position-stack)))
  (setq dictionary-current-data
	(cons function data)))

(defun dictionary-store-positions ()
  "Store the current positions for later restore."

  (setq dictionary-positions (cons (point) (window-start))))

;; Restore the previous state
(defun dictionary-restore-state (&rest _ignored)
  "Restore the state just before the last operation."
  (let ((position (pop dictionary-position-stack))
	(data (pop dictionary-data-stack)))
    (unless position
      (error "Already at begin of history"))
    (apply (car data) (cdr data))
    (set-window-start (selected-window) (cdr position))
    (goto-char (car position))
    (setq dictionary-current-data data)))

;; The normal search

(defun dictionary-new-search (args &optional all)
  "Save the current state and start a new search based on ARGS.
The parameter ARGS is a cons cell where car is the word to search
and cdr is the dictionary where to search the word in."
  (dictionary-store-positions)
  (let ((word (car args))
	(dictionary (cdr args)))

    (if all
	(setq dictionary dictionary-default-dictionary))
    (dictionary-ensure-buffer)
    (dictionary-new-search-internal word dictionary 'dictionary-display-search-result)
    (dictionary-store-state 'dictionary-new-search-internal
			    (list word dictionary 'dictionary-display-search-result))))

(defun dictionary-new-search-internal (word dictionary function)
  "Start a new search for WORD in DICTIONARY after preparing the buffer.
FUNCTION is the callback which is called for each search result."
  (dictionary-pre-buffer)
  (dictionary-do-search word dictionary function))

(defun dictionary-do-search (word dictionary function &optional nomatching)
  "Search for WORD in DICTIONARY and call FUNCTION for each result.
Optional argument NOMATCHING, if non-nil, means suppress the display
of the \"Searching\" report and of the matching words."
  (unless nomatching
    (insert (format-message "Searching for `%s' in `%s'\n" word dictionary)))
  (dictionary-send-command (concat "define "
				   (dictionary-encode-charset dictionary "")
				   " \""
				   (dictionary-encode-charset word dictionary)
				   "\""))

  (message nil)
  (let ((reply (dictionary-read-reply-and-split)))
    (if (dictionary-check-reply reply 552)
	(progn
	  (unless nomatching
	    (insert (format-message "Word `%s' not found\n" word))
	    (dictionary-do-matching
             word
	     dictionary
	     "."
	     (lambda (reply)
               (insert "Maybe you are looking for one of these words\n")
               (dictionary-display-only-match-result reply)))
	    (dictionary-post-buffer)))
      (if (dictionary-check-reply reply 550)
          (error "Dictionary \"%s\" is unknown, please select an existing one"
		 dictionary)
	(unless (dictionary-check-reply reply 150)
	  (error "Unknown server answer: %s" (dictionary-reply reply)))
	(funcall function reply)))))

(define-button-type 'dictionary-link
  'face 'dictionary-reference-face
  'action (lambda (button)
            (let ((func (button-get button 'callback))
                  (data (button-get button 'data))
                  (list-data (button-get button 'list-data)))
              (if list-data
                  (apply func list-data)
                (funcall func data)))))

(define-button-type 'dictionary-button
  :supertype 'dictionary-link
  'face 'dictionary-button-face)

(defun dictionary-pre-buffer ()
  "These commands are executed at the begin of a new buffer."
  (setq buffer-read-only nil)
  (erase-buffer)
  (if dictionary-create-buttons
      (progn
        (insert-button "[Back]" :type 'dictionary-button
                       'callback 'dictionary-restore-state
                       'help-echo "Mouse-2 to go backwards in history")
	(insert " ")
        (insert-button "[Search definition]" :type 'dictionary-button
                       'callback 'dictionary-search
                       'help-echo "Mouse-2 to look up a new word")
	(insert "         ")

	(insert-button "[Matching words]" :type 'dictionary-button
                       'callback 'dictionary-match-words
                       'help-echo "Mouse-2 to find matches for a pattern")
	(insert "        ")

	(insert-button "[Quit]" :type 'dictionary-button
                       'callback 'dictionary-close
                       'help-echo "Mouse-2 to close this window")

	(insert "\n       ")

        (insert-button "[Select dictionary]" :type 'dictionary-button
                       'callback 'dictionary-select-dictionary
                       'help-echo "Mouse-2 to select dictionary for future searches")
	(insert "         ")
        (insert-button "[Select match strategy]" :type 'dictionary-button
                       'callback 'dictionary-select-strategy
                       'help-echo "Mouse-2 to select matching algorithm")
	(insert "\n\n")))
  (setq dictionary-marker (point-marker)))

(defun dictionary-post-buffer ()
  "These commands are executed at the end of a new buffer."
  (goto-char dictionary-marker)

  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (run-hooks 'dictionary-post-buffer-hook))

(defun dictionary-display-search-result (reply)
  "Start displaying the result in REPLY."

  (let ((number (nth 1 (dictionary-reply-list reply))))
    (insert number (if (equal number "1")
		       " definition"
		     " definitions")
	    " found\n\n")
    (setq reply (dictionary-read-reply-and-split))
    (while (dictionary-check-reply reply 151)
      (let* ((reply-list (dictionary-reply-list reply))
	     (dictionary (nth 2 reply-list))
	     (description (nth 3 reply-list))
	     (word (nth 1 reply-list)))
	(dictionary-display-word-entry dictionary description)
	(setq reply (dictionary-read-answer))
	(dictionary-display-word-definition reply word dictionary)
	(setq reply (dictionary-read-reply-and-split))))
    (dictionary-post-buffer)))

(defun dictionary-display-word-entry (dictionary description)
  "Insert an explanation for DESCRIPTION from DICTIONARY.
The DICTIONARY is only used for decoding the bytes to display the DESCRIPTION."
  (let ((start (point)))
    (insert "From "
	    dictionary-description-open-delimiter
	    (dictionary-decode-charset description dictionary)
	    dictionary-description-close-delimiter
	    " [" (dictionary-decode-charset dictionary dictionary) "]:")
    (put-text-property start (point) 'face 'dictionary-word-entry-face)
    (insert "\n\n")))

(defun dictionary-display-word-definition (reply word dictionary)
  "Insert the definition in REPLY for the current WORD from DICTIONARY.
It will replace links which are found in the REPLY and replace
them with buttons to perform a new search."
  (let ((start (point)))
    (insert (dictionary-decode-charset reply dictionary))
    (insert "\n\n")
    (put-text-property start (point) 'face 'dictionary-word-definition-face)
    (let ((regexp "\\({+\\)\\([^ '\"][^}]*\\)\\(}+\\)"))
      (goto-char start)
      (while (< (point) (point-max))
	(if (search-forward-regexp regexp nil t)
	    (let ((match-start (match-beginning 2))
		  (match-end (match-end 2)))
              (if (display-color-p)
		  ;; Compensate for the replacement
		  (let ((brace-match-length (- (match-end 1)
					       (match-beginning 1))))
		    (setq match-start (- (match-beginning 2)
					 brace-match-length))
		    (setq match-end (- (match-end 2)
				       brace-match-length))
		    (replace-match "\\2")))
	      (dictionary-mark-reference match-start match-end
					 'dictionary-new-search
					 word dictionary))
	  (goto-char (point-max)))))))

(defun dictionary-mark-reference (start end call displayed-word dictionary)
  "Format the area from START to END as link calling CALL.
The word is taken from the buffer, the DICTIONARY is given as argument."
  (let ((word (buffer-substring-no-properties start end)))
    (while (string-match "\n\\s-*" word)
      (setq word (replace-match " " t t word)))
    (while (string-match "[*\"]" word)
      (setq word (replace-match "" t t word)))
    (when dictionary-link-dictionary
      (setq dictionary dictionary-link-dictionary))

    (unless (equal word displayed-word)
      (make-button start end :type 'dictionary-link
                   'callback call
                   'data (cons word dictionary)
                   'help-echo (concat "Press Mouse-2 to lookup \""
                                      word "\" in \"" dictionary "\"")))))

(defun dictionary-select-dictionary (&rest _ignored)
  "Save the current state and start a dictionary selection."
  (interactive)
  (dictionary-ensure-buffer)
  (dictionary-store-positions)
  (dictionary-do-select-dictionary)
  (dictionary-store-state 'dictionary-do-select-dictionary nil))

(defun dictionary-do-select-dictionary (&rest _ignored)
  "The workhorse for doing the dictionary selection."

  (message "Looking up databases and descriptions")
  (dictionary-send-command "show db")

  (let ((reply (dictionary-read-reply-and-split)))
    (message nil)
    (if (dictionary-check-reply reply 554)
	(error "No dictionary present")
      (unless (dictionary-check-reply reply 110)
	(error "Unknown server answer: %s"
	       (dictionary-reply reply)))
      (dictionary-display-dictionaries))))

(defun dictionary-simple-split-string (string &optional pattern)
  "Return a list of substrings of STRING which are separated by PATTERN.
If PATTERN is omitted, it defaults to \"[ \\f\\t\\n\\r\\v]+\"."
  (or pattern
      (setq pattern "[ \f\t\n\r\v]+"))
  ;; The FSF version of this function takes care not to cons in case
  ;; of infloop.  Maybe we should synch?
  (let (parts (start 0))
    (while (string-match pattern string start)
      (setq parts (cons (substring string start (match-beginning 0)) parts)
	    start (match-end 0)))
    (nreverse (cons (substring string start) parts))))

(defun dictionary-display-dictionaries ()
  "Handle the display of all dictionaries existing on the server."
  (dictionary-pre-buffer)
  (insert "Please select your default dictionary:\n\n")
  (dictionary-display-dictionary-line "* \"All dictionaries\"")
  (dictionary-display-dictionary-line "! \"The first matching dictionary\"")
  (let* ((reply (dictionary-read-answer))
	 (list (dictionary-simple-split-string reply "\n+")))
    (mapc #'dictionary-display-dictionary-line list))
  (dictionary-post-buffer))

(defun dictionary-display-dictionary-line (string)
  "Display a single dictionary and its description read from STRING."
  (let* ((list (dictionary-split-string string))
	 (dictionary (car list))
	 (description (cadr list))
	 (translated (dictionary-decode-charset description dictionary)))
    (if dictionary
	(if (equal dictionary "--exit--")
	    (insert "(end of default search list)\n")
          (insert-button (concat dictionary ": " translated) :type 'dictionary-link
                         'callback 'dictionary-set-dictionary
                         'data (cons dictionary description)
                         'help-echo "Mouse-2 to select this dictionary")
          (unless (dictionary-special-dictionary dictionary)
            (insert " ")
            (insert-button "(Details)" :type 'dictionary-link
                           'callback 'dictionary-set-dictionary
                           'list-data (list (cons dictionary description) t)
                           'help-echo "Mouse-2 to get more information"))
	  (insert "\n")))))

(defun dictionary-set-dictionary (param &optional more)
  "Select the dictionary which is the car of PARAM as new default."
  (if more
      (dictionary-display-more-info param)
    (let ((dictionary (car param)))
      (setq dictionary-default-dictionary dictionary)
      (dictionary-restore-state)
      (message "Dictionary %s has been selected" dictionary))))

(defun dictionary-special-dictionary (name)
  "Check whether the special * or ! dictionary are seen in NAME."
  (or (equal name "*")
      (equal name "!")))

(defun dictionary-display-more-info (param)
  "Display the available information on the dictionary found in PARAM."

  (let ((dictionary (car param))
	(description (cdr param)))
    (unless (dictionary-special-dictionary dictionary)
      (dictionary-store-positions)
      (message "Requesting more information on %s" dictionary)
      (dictionary-send-command
       (concat "show info " (dictionary-encode-charset dictionary "")))
      (let ((reply (dictionary-read-reply-and-split)))
	(message nil)
	(if (dictionary-check-reply reply 550)
	    (error "Dictionary \"%s\" does not exist" dictionary)
	  (unless (dictionary-check-reply reply 112)
	    (error "Unknown server answer: %s" (dictionary-reply reply)))
	  (dictionary-pre-buffer)
	  (insert "Information on dictionary: ")
          (insert-button description :type 'dictionary-link
                         'callback 'dictionary-set-dictionary
                         'data (cons dictionary description)
                         'help-echo "Mouse-2 to select this dictionary")
	  (insert "\n\n")
	  (setq reply (dictionary-read-answer))
	  (insert reply)
	  (dictionary-post-buffer)))

      (dictionary-store-state 'dictionary-display-more-info dictionary))))

(defun dictionary-select-strategy (&rest _ignored)
  "Save the current state and start a strategy selection."
  (interactive)
  (dictionary-ensure-buffer)
  (dictionary-store-positions)
  (dictionary-do-select-strategy)
  (dictionary-store-state 'dictionary-do-select-strategy nil))

(defun dictionary-do-select-strategy ()
  "The workhorse for doing the strategy selection."

  (message "Request existing matching algorithm")
  (dictionary-send-command "show strat")

  (let ((reply (dictionary-read-reply-and-split)))
    (message nil)
    (if (dictionary-check-reply reply 555)
	(error "No strategies available")
      (unless (dictionary-check-reply reply 111)
	(error "Unknown server answer: %s"
	       (dictionary-reply reply)))
      (dictionary-display-strategies))))

(defun dictionary-display-strategies ()
  "Handle the display of all strategies existing on the server."
  (dictionary-pre-buffer)
  (insert "Please select your default search strategy:\n\n")
  (dictionary-display-strategy-line ". \"The servers default\"")
  (let* ((reply (dictionary-read-answer))
	 (list (dictionary-simple-split-string reply "\n+")))
    (mapc #'dictionary-display-strategy-line list))
  (dictionary-post-buffer))

(defun dictionary-display-strategy-line (string)
  "Display a single strategy found in STRING."
  (let* ((list (dictionary-split-string string))
	 (strategy (car list))
	 (description (cadr list)))
    (if strategy
	(progn
          (insert-button description :type 'dictionary-link
                         'callback 'dictionary-set-strategy
                         'data strategy
                         'help-echo "Mouse-2 to select this matching algorithm")
	  (insert "\n")))))

(defun dictionary-set-strategy (strategy &rest _ignored)
  "Select this STRATEGY as new default."
  (setq dictionary-default-strategy strategy)
  (dictionary-restore-state)
  (message "Strategy %s has been selected" strategy))

(defun dictionary-new-matching (word)
  "Run a new matching search on WORD."
  (dictionary-store-positions)
  (dictionary-ensure-buffer)
  (dictionary-new-matching-internal word dictionary-default-dictionary
                                    dictionary-default-strategy
                                    'dictionary-display-match-result)
  (dictionary-store-state 'dictionary-new-matching-internal
			  (list word dictionary-default-dictionary
				dictionary-default-strategy
				'dictionary-display-match-result)))

(defun dictionary-new-matching-internal (word dictionary strategy function)
  "Start a new matching for WORD in DICTIONARY after preparing the buffer.
FUNCTION is the callback which is called for each search result."
  (dictionary-pre-buffer)
  (dictionary-do-matching word dictionary strategy function))

(defun dictionary-do-matching (word dictionary strategy function)
  "Search for WORD with STRATEGY in DICTIONARY and display them with FUNCTION."
  (insert (format-message "Lookup matching words for `%s' in `%s' using `%s'\n"
                          word dictionary strategy))
  (dictionary-send-command
   (concat "match " (dictionary-encode-charset dictionary "") " "
	   (dictionary-encode-charset strategy "") " \""
	   (dictionary-encode-charset word "") "\""))
  (let ((reply (dictionary-read-reply-and-split)))
    (message nil)
    (if (dictionary-check-reply reply 550)
	(error "Dictionary \"%s\" is invalid" dictionary))
    (if (dictionary-check-reply reply 551)
	(error "Strategy \"%s\" is invalid" strategy))
    (if (dictionary-check-reply reply 552)
	(let ((errmsg (format-message
                       (concat
		        "No match for `%s' with strategy `%s' in "
		        "dictionary `%s'.")
	               word strategy dictionary)))
          (insert errmsg "\n")
          (user-error errmsg)))
    (unless (dictionary-check-reply reply 152)
      (error "Unknown server answer: %s" (dictionary-reply reply)))
    (funcall function reply)))

(defun dictionary-display-only-match-result (reply)
  "Display the results from the current matches in REPLY without the headers."
  (let ((number (nth 1 (dictionary-reply-list reply)))
	(list (dictionary-simple-split-string (dictionary-read-answer) "\n+")))
    (insert number " matching word" (if (equal number "1") "" "s")
	    " found\n\n")
    (let ((result nil))
      (mapc (lambda (item)
	      (let* ((list (dictionary-split-string item))
		     (dictionary (car list))
		     (word (cadr list))
		     (hash (assoc dictionary result)))
		(if dictionary
		    (if hash
			(setcdr hash (cons word (cdr hash)))
		      (setq result (cons
				    (cons dictionary (list word))
				    result))))))
	    list)
      (dictionary-display-match-lines (reverse result)))))

(defun dictionary-display-match-result (reply)
  "Display the results in REPLY from a match operation."
  (let ((number (nth 1 (dictionary-reply-list reply)))
	(list (dictionary-simple-split-string (dictionary-read-answer) "\n+")))
    (insert number " matching word" (if (equal number "1") "" "s")
	    " found\n\n")
    (let ((result nil))
      (mapc (lambda (item)
	      (let* ((list (dictionary-split-string item))
		     (dictionary (car list))
		     (word (cadr list))
		     (hash (assoc dictionary result)))
		(if dictionary
		    (if hash
			(setcdr hash (cons word (cdr hash)))
		      (setq result (cons
				    (cons dictionary (list word))
				    result))))))
	    list)
      (dictionary-display-match-lines (reverse result))))
  (dictionary-post-buffer))

(defun dictionary-display-match-lines (list)
  "Display a line for each match found in LIST."
  (mapc (lambda (item)
	  (let ((dictionary (car item))
		(word-list (cdr item)))
	    (insert "Matches from " dictionary ":\n")
	    (mapc (lambda (word)
		    (setq word (dictionary-decode-charset word dictionary))
		    (insert "  ")
                    (insert-button word :type 'dictionary-link
                                   'callback 'dictionary-new-search
                                   'data (cons word dictionary)
                                   'help-echo "Mouse-2 to lookup word")
		    (insert "\n")) (reverse word-list))
	    (insert "\n")))
	list))

;; Returns a sensible default for dictionary-search:
;; - if region is active returns its contents
;; - otherwise return the word near the point
(defun dictionary-search-default ()
  (cond
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ((car (get-char-property (point) 'data)))
   (t (current-word t))))

(defun dictionary-read-dictionary-default ()
  "Prompt for a dictionary name."
  (read-string (if dictionary-default-dictionary
		   (format "Dictionary (%s): "
                           dictionary-default-dictionary)
		 "Dictionary: ")
	       nil nil dictionary-default-dictionary))

(defun dictionary-read-word-default (_dictionary)
  "Prompt for a word to search in the dictionary."
  (let ((default (dictionary-search-default)))
    (read-string (format-prompt dictionary-read-word-prompt default)
                 nil 'dictionary-word-history default)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User callable commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun dictionary-search (word &optional dictionary)
  "Search for WORD in all the known dictionaries.
Interactively, prompt for WORD, and offer the word at point as default.

Optional argument DICTIONARY means restrict the search to only
that one dictionary.  Interactively, with prefix argument,
prompt for DICTIONARY."
  (interactive
   (let ((dict
          (if current-prefix-arg
              (funcall dictionary-read-dictionary-function)
	    dictionary-default-dictionary)))
     (list (funcall dictionary-read-word-function dict) dict)))
  (unless dictionary
    (setq dictionary dictionary-default-dictionary))
  (if dictionary-display-definition-function
      (if-let* ((definition (dictionary-define-word word dictionary)))
          (funcall dictionary-display-definition-function word dictionary definition)
        (user-error "No definition found for \"%s\"" word))
    ;; if called by pressing the button
    (unless word
      (setq word (read-string "Search word: " nil 'dictionary-word-history)))
    ;; just in case non-interactively called
    (dictionary-new-search (cons word dictionary))))

;;;###autoload
(defun dictionary-lookup-definition ()
  "Unconditionally lookup the word at point."
  (interactive)
  (let ((word (current-word)))
    (unless word
      (user-error "No word at point"))
    (dictionary-new-search (cons word dictionary-default-dictionary))))

(defun dictionary-previous ()
  "Go to the previous location in the current buffer."
  (interactive)
  (unless (dictionary-mode-p)
    (error "Current buffer is no dictionary buffer"))
  (dictionary-restore-state))

(defun dictionary-help ()
  "Display a little help."
  (declare (obsolete describe-mode "29.1"))
  (interactive nil dictionary-mode)
  (describe-function 'dictionary-mode))

;;;###autoload
(defun dictionary-match-words (&optional pattern &rest _ignored)
  "Search PATTERN in current default dictionary using default strategy."
  (interactive)
  ;; can't use interactive because of mouse events
  (or pattern
      (setq pattern (read-string "Search pattern: "
                                 nil 'dictionary-word-history)))
  (dictionary-new-matching pattern))

;;;###autoload
(defun dictionary-mouse-popup-matching-words (event)
  "Display entries matching the word at the cursor retrieved using EVENT."
  (interactive "e")
  (let ((word (save-window-excursion
		(save-excursion
		  (mouse-set-point event)
		  (current-word)))))
    (dictionary-popup-matching-words word)))

;;;###autoload
(defun dictionary-popup-matching-words (&optional word)
  "Display entries matching WORD or the current word if not given."
  (interactive)
  (dictionary-do-matching (or word (current-word)
                              (user-error "Nothing to search for"))
			  dictionary-default-dictionary
			  dictionary-default-popup-strategy
			  'dictionary-process-popup-replies))

(defun dictionary-process-popup-replies (_)
  (let ((list (dictionary-simple-split-string (dictionary-read-answer) "\n+")))

    (let ((result (mapcar (lambda (item)
			    (let* ((list (dictionary-split-string item))
				   (dictionary (car list))
				   (word (dictionary-decode-charset
					  (cadr list) dictionary)))
			      (message word)
			      (if (equal word "")
				  [ "-" nil nil]
				(vector (concat "[" dictionary "] " word)
					`(dictionary-new-search
					  '(,word . ,dictionary))
					t ))))

			  list)))
      (easy-menu-define dictionary-mode-map-menu dictionary-mode-map
        "Menu used for displaying dictionary popup"
        (cons "Matching words"
              `(,@result)))
      (popup-menu dictionary-mode-map-menu))))

;;; Tooltip support

;; Add a mode indicator named "Dict"
(defvar dictionary-tooltip-mode
  nil
  "Indicates whether the dictionary tooltip mode is active.")
(nconc minor-mode-alist '((dictionary-tooltip-mode " Dict")))

(defcustom dictionary-tooltip-dictionary
  t
  "The dictionary to lookup words for `dictionary-tooltip-mode'.
If this is nil, `dictionary-tooltip-mode' is effectively disabled: no tooltips
will be shown.
If the value is t, `dictionary-tooltip-mode' will use the same dictionary as
specified by `dictionary-default-dictionary'.
Otherwise, the value should be a string, the name of a dictionary to use, and
can use the same special values * and ! as for `dictionary-default-dictionary',
with the same meanings.
Dictionary names are generally specific to the servers, and are obtained
via `dictionary-dictionaries'."
  :group 'dictionary
  :type '(choice (const :tag "None (disables Dictionary tooltips)" nil)
                 (const :tag "Same as `dictionary-default-dictionary'" t)
                 string)
  :version "30.1")

(defun dictionary-definition (word &optional dictionary)
  (unwind-protect
      (let ((dictionary (or dictionary dictionary-default-dictionary)))
	(dictionary-do-search word dictionary 'dictionary-read-definition t))
    nil))

(defun dictionary-read-definition (_)
  (let ((list (dictionary-simple-split-string (dictionary-read-answer) "\n+")))
    (mapconcat #'identity (cdr list) "\n")))

;;; Tooltip support for GNU Emacs
(defvar global-dictionary-tooltip-mode
  nil)

(defun dictionary-word-at-mouse-event (event)
  (let ((buf (tooltip-event-buffer event)))
    (when (bufferp buf)
      (with-current-buffer buf
        (let ((point (posn-point (event-end event))))
          ;; posn-point can return something other than buffer position when
          ;; the mouse pointer is over the menu bar or tool bar or tab-bar.
          (when (number-or-marker-p point)
            (if (use-region-p)
	        (when (and (<= (region-beginning) point)
                           (<= point (region-end)))
	          (buffer-substring (region-beginning) (region-end)))
              (save-excursion
                (goto-char point)
                (current-word)))))))))

(defvar dictionary-tooltip-mouse-event nil
  "Event that triggered the tooltip mode.")

(defun dictionary-display-tooltip (_)
  "Search the current word in the `dictionary-tooltip-dictionary'."
  (interactive "e")
  (if (and dictionary-tooltip-mode dictionary-tooltip-dictionary)
      ;; This function runs from the tooltip timer.  We don't want to
      ;; signal errors from the timer due to "Unknown server answers",
      ;; we prefer not to show anything in that case.  FIXME: Perhaps
      ;; use with-demoted-errors, to show the unknown answers in the
      ;; echo-area?
      (ignore-errors
        (let* ((word (dictionary-word-at-mouse-event
                      dictionary-tooltip-mouse-event))
               (dict (if (eq dictionary-tooltip-dictionary t)
                         dictionary-default-dictionary
                       dictionary-tooltip-dictionary)))
          (if word
              (let ((definition (dictionary-definition word dict)))
                (if definition
                    (tooltip-show (dictionary-decode-charset
                                   definition dict)))))
          t)
        nil)))

(defun dictionary-tooltip-track-mouse (event)
  "Hide current tooltip and setup next tooltip in response to mouse movement EVENT."
  (interactive "e")
  (tooltip-hide)
  (when dictionary-tooltip-mode
    (setq dictionary-tooltip-mouse-event (copy-sequence event))
    (tooltip-start-delayed-tip)))

(defun dictionary-switch-tooltip-mode (state)
  "Turn dictionary tooltip mode on or off depending on STATE.

It is normally called internally with a non-nil value to enable the
tooltip mode.  The hook function uses the value of the variable
`dictionary-tooltip-mode' to decide if some action must be taken.
When disabling the tooltip mode, that variable will be set to nil."
  (tooltip-mode state)
  (if state
      (add-hook 'tooltip-functions #'dictionary-display-tooltip)
    (remove-hook 'tooltip-functions #'dictionary-display-tooltip)))

;;;###autoload
(defun dictionary-tooltip-mode (&optional arg)
  "Display tooltips for the current word.

This function can be used to enable or disable the tooltip mode
for the current buffer (based on ARG).  If global-tooltip-mode is
active it will overwrite that mode for the current buffer."
  (interactive "P")
  (require 'tooltip)
  (let ((on (if arg
                (> (prefix-numeric-value arg) 0)
              (not dictionary-tooltip-mode))))
    (setq-local dictionary-tooltip-mode on)
    (setq-local track-mouse on)
    (make-local-variable 'dictionary-tooltip-mouse-event)
    (dictionary-switch-tooltip-mode 1)
    (if on
        (local-set-key [mouse-movement] 'dictionary-tooltip-track-mouse)
      (local-set-key [mouse-movement] 'ignore))
    ;; Unconditionally ignore mouse-movement events on the tool bar and
    ;; tab-bar, since these are unrelated to the current buffer.
    ;; FIXME: This disables help-echo for tab-bar and tool-bar buttons.
    (local-set-key [tool-bar mouse-movement] 'ignore)
    (local-set-key [tab-bar mouse-movement] 'ignore)
    on))

;;;###autoload
(defun global-dictionary-tooltip-mode (&optional arg)
  "Enable/disable `dictionary-tooltip-mode' for all buffers.

Internally it provides a default for the `dictionary-tooltip-mode'.
It can be overwritten for each buffer using `dictionary-tooltip-mode'.

Note: (global-dictionary-tooltip-mode 0) will not disable the mode
any buffer where (dictionary-tooltip-mode 1) has been called."
  (interactive "P")
  (require 'tooltip)
  (let ((on (if arg (> (prefix-numeric-value arg) 0)
              (not global-dictionary-tooltip-mode))))
    (setq global-dictionary-tooltip-mode on)
    (setq-default dictionary-tooltip-mode on)
    (make-local-variable 'dictionary-tooltip-mouse-event)
    (setq-default track-mouse on)
    (dictionary-switch-tooltip-mode 1)
    (global-set-key [mouse-movement]
                    (if on #'dictionary-tooltip-track-mouse #'ignore))
    on))

;;; Context menu support

(defun dictionary-search-word-at-mouse (event)
  (interactive "e")
  (let ((word (save-window-excursion
		(save-excursion
		  (mouse-set-point event)
		  (current-word)))))
    (dictionary-search word)))

;;;###autoload
(defun dictionary-context-menu (menu click)
  "Populate MENU with dictionary commands at CLICK.
When you add this function to `context-menu-functions',
the context menu will contain an item that searches
the word at mouse click."
  (when (thing-at-mouse click 'word)
    (define-key-after menu [dictionary-separator] menu-bar-separator
      'middle-separator)
    (define-key-after menu [dictionary-search-word-at-mouse]
      '(menu-item "Dictionary Search" dictionary-search-word-at-mouse
                  :help "Search the word at mouse click in dictionary")
      'dictionary-separator))
  menu)

(defun dictionary-define-word (word dictionary)
  "Return the definition of WORD in DICTIONARY, or nil if not found."
  (dictionary-send-command
   (format "define %s \"%s\"" dictionary word))
  (when (and (= (read (dictionary-read-reply)) 150)
             (= (read (dictionary-read-reply)) 151))
    (dictionary-read-answer)))

(defun dictionary-match-word (word &rest _)
  "Return dictionary matches for WORD as a list of strings.
Further arguments are currently ignored."
  (unless (string-empty-p word)
    (if (string= (car dictionary--last-match) word)
        (cdr dictionary--last-match)
      (dictionary-send-command
       (format "match %s %s \"%s\""
               dictionary-default-dictionary
               dictionary-default-strategy
               word))
      (when (and (= (read (dictionary-read-reply)) 152))
        (with-temp-buffer
          (insert (dictionary-read-answer))
          (goto-char (point-min))
          (let ((result nil))
            (while (not (eobp))
              (search-forward " " nil t)
              (push (read (current-buffer)) result)
              (search-forward "\n" nil t))
            (setq result (reverse result))
            (setq dictionary--last-match (cons word result))
            result))))))

(defun dictionary-completing-read-word (dictionary)
  "Prompt for a word with completion based on matches in DICTIONARY."
  (let* ((completion-ignore-case t)
         (dictionary-default-dictionary dictionary)
         (word-at-point (thing-at-point 'word t))
         (default (dictionary-match-word word-at-point)))
    (completing-read (format-prompt dictionary-read-word-prompt default)
                     (external-completion-table 'dictionary-definition
                                                #'dictionary-match-word)
                     nil t nil 'dictionary-word-history default t)))

(defun dictionary-dictionaries ()
  "Return the list of dictionaries the server supports.
The elements of the list have the form (NAME . DESCRIPTION),
where NAME is the string that identifies the dictionary for
the server, and DESCRIPTION is its more detailed description,
which usually includes the languages it supports."
  (dictionary-send-command "show db")
  (when (and (= (read (dictionary-read-reply)) 110))
    (with-temp-buffer
      (insert (dictionary-read-answer))
      ;; We query the server using 'raw-text', so decode now to present
      ;; human-readable names to the user.
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (goto-char (point-min))
      (let ((result '(("!" . "First matching dictionary")
                      ("*" . "All dictionaries"))))
        (while (not (eobp))
          (push (cons (buffer-substring
                       (search-forward "\n" nil t)
                       (1- (search-forward " " nil t)))
                      (read (current-buffer)))
                result))
        (reverse result)))))

(defun dictionary-completing-read-dictionary ()
  "Prompt for a dictionary the server supports."
  (let* ((dicts (dictionary-dictionaries))
         (len (apply #'max (mapcar #'length (mapcar #'car dicts)))))
    (completing-read
     (format-prompt "Select dictionary"
                    dictionary-default-dictionary)
     (completion-table-with-metadata
      dicts
      `((annotation-function
         . ,(lambda (key)
              (concat (make-string (1+ (- len (length key))) ?\s)
                      (alist-get key dicts nil nil #'string=))))))
     nil t nil nil dictionary-default-dictionary)))

(define-button-type 'help-word
  :supertype 'help-xref
  'help-function 'dictionary-search
  'help-echo "mouse-2, RET: describe this word")

(defun dictionary-display-definition-in-help-buffer (word dictionary definition)
  "Display DEFINITION, the definition of WORD in DICTIONARY."
  (let ((help-buffer-under-preparation t))
    (help-setup-xref (list #'dictionary-search word dictionary)
                     (called-interactively-p 'interactive))
    (with-help-window (help-buffer)
      (with-current-buffer (help-buffer)
        (insert definition)
        ;; Buttonize references to other definitions.  These appear as
        ;; words enclosed with curly braces.
        (goto-char (point-min))
        (while (re-search-forward (rx "{"
                                      (group-n 1 (* (not (any ?}))))
                                      "}")
                                  nil t)
          (help-xref-button 1 'help-word
                            (match-string 1)
                            dictionary))))))

(defvar dictionary-color-support (display-color-p)
  "Determines if the Emacs has support to display color.")
(make-obsolete-variable 'dictionary-color-support 'display-color-p "30.1")

(provide 'dictionary)
;;; dictionary.el ends here
