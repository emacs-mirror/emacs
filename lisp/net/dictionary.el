;;; dictionary.el --- Client for rfc2229 dictionary servers

;; Author: Torsten Hilbrich <torsten.hilbrich@gmx.net>
;; Keywords: interface, dictionary
;; Version: 1.11
;; Package-Requires: ((connection "1.11") (link "1.11"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; dictionary allows you to interact with dictionary servers.
;; Use M-x customize-group dictionary to modify user settings.
;;
;; Main functions for interaction are:
;; dictionary        - opens a new dictionary buffer
;; dictionary-search - search for the definition of a word
;;
;; You can find more information in the README file of the GitHub
;; repository https://github.com/myrkr/dictionary-el

;;; Code:

(require 'cl-lib)
(require 'easymenu)
(require 'custom)
(require 'dictionary-connection)
(require 'button)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff for customizing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dictionary-server)
(defun dictionary-set-server-var (name value)
  (if (and (boundp 'dictionary-connection)
	   dictionary-connection
	   (eq (dictionary-connection-status dictionary-connection) 'up)
	   (y-or-n-p
	    (concat "Close existing connection to " dictionary-server "? ")))
      (dictionary-connection-close dictionary-connection))
  (set-default name value))

(defgroup dictionary nil
  "Client for accessing the dictd server based dictionaries"
  :group 'hypermedia)

(defgroup dictionary-proxy nil
  "Proxy configuration options for the dictionary client"
  :group 'dictionary)

(defcustom dictionary-server
  "dict.org"
  "This server is contacted for searching the dictionary"
  :group 'dictionary
  :set 'dictionary-set-server-var
  :type 'string
  :version "28.1")

(defcustom dictionary-port
  2628
  "The port of the dictionary server.
 This port is propably always 2628 so there should be no need to modify it."
  :group 'dictionary
  :set 'dictionary-set-server-var
  :type 'number
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
 one dictionary yields matches."
  :group 'dictionary
  :type 'string
  :version "28.1")

(defcustom dictionary-default-strategy
  "."
  "The default strategy for listing matching words."
  :group 'dictionary
  :type 'string
  :version "28.1")

(defcustom dictionary-default-popup-strategy
  "exact"
  "The default strategy for listing matching words within a popup window.

The following algorithm (defined by the dictd server) are supported
by the choice value:

- Exact match

  The found word exactly matches the searched word.

- Similiar sounding

  The found word sounds similiar to the searched word.  For this match type
  the soundex algorithm defined by Donald E. Knuth is used.  It will only
  works with english words and the algorithm is not very reliable (i.e.,
  the soundex algorithm is quite simple).

- Levenshtein distance one

  The Levenshtein distance is defined as the number of insertions, deletions,
  or replacements needed to get the searched word.  This algorithm searches
  for word where spelling mistakes are allowed.  Levenshtein distance one
  means there is either a deleted character, an inserted character, or a
  modified one.

- User choice

  Here you can enter any matching algorithm supported by your
  dictionary server.
"
  :group 'dictionary
  :type '(choice (const :tag "Exact match" "exact")
		 (const :tag "Similiar sounding" "soundex")
		 (const :tag "Levenshtein distance one" "lev")
		 (string :tag "User choice"))
  :version "28.1")

(defcustom dictionary-create-buttons
  t
  "Create some clickable buttons on top of the window if non-nil."
  :group 'dictionary
  :type 'boolean
  :version "28.1")

(defcustom dictionary-mode-hook
  nil
  "Hook run in dictionary mode buffers."
  :group 'dictionary
  :type 'hook
  :version "28.1")

(defcustom dictionary-use-http-proxy
  nil
  "Connects via a HTTP proxy using the CONNECT command when not nil."
  :group 'dictionary-proxy
  :set 'dictionary-set-server-var
  :type 'boolean
  :version "28.1")

(defcustom dictionary-proxy-server
  "proxy"
  "The name of the HTTP proxy to use when dictionary-use-http-proxy is set."
  :group 'dictionary-proxy
  :set 'dictionary-set-server-var
  :type 'string
  :version "28.1")

(defcustom dictionary-proxy-port
  3128
  "The port of the proxy server, used only when dictionary-use-http-proxy is set."
  :group 'dictionary-proxy
  :set 'dictionary-set-server-var
  :type 'number
  :version "28.1")

(defcustom dictionary-use-single-buffer
  nil
  "Should the dictionary command reuse previous dictionary buffers?"
  :group 'dictionary
  :type 'boolean
  :version "28.1")

(defcustom dictionary-description-open-delimiter
  ""
  "The delimiter to display in front of the dictionaries description"
  :group 'dictionary
  :type 'string
  :version "28.1")

(defcustom dictionary-description-close-delimiter
  ""
  "The delimiter to display after of the dictionaries description"
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

(defface dictionary-word-definition-face
'((((supports (:family "DejaVu Serif")))
   (:family "DejaVu Serif"))
  (((type x))
   (:font "Sans Serif"))
  (t
   (:font "default")))
"The face that is used for displaying the definition of the word."
:group 'dictionary
:version "28.1")

(defface dictionary-word-entry-face
  '((((type x))
     (:italic t))
    (((type tty) (class color))
     (:foreground "green"))
    (t
     (:inverse t)))
  "The face that is used for displaying the initial word entry line."
  :group 'dictionary
  :version "28.1")

(defface dictionary-button-face
  '((t
     (:bold t)))
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
  "The window configuration to be restored upon closing the buffer")

(defvar dictionary-selected-window
  nil
  "The currently selected window")

(defvar dictionary-position-stack
  nil
  "The history buffer for point and window position")

(defvar dictionary-data-stack
  nil
  "The history buffer for functions and arguments")

(defvar dictionary-positions
  nil
  "The current positions")

(defvar dictionary-current-data
  nil
  "The item that will be placed on stack next time")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar dictionary-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-parent map button-buffer-map)

    (define-key map "q" 'dictionary-close)
    (define-key map "h" 'dictionary-help)
    (define-key map "s" 'dictionary-search)
    (define-key map "d" 'dictionary-lookup-definition)
    (define-key map "D" 'dictionary-select-dictionary)
    (define-key map "M" 'dictionary-select-strategy)
    (define-key map "m" 'dictionary-match-words)
    (define-key map "l" 'dictionary-previous)
    (define-key map "n" 'forward-button)
    (define-key map "p" 'backward-button)
    (define-key map " " 'scroll-up)
    (define-key map (read-kbd-macro "M-SPC") 'scroll-down)
    map)
  "Keymap for the dictionary mode.")

(defvar dictionary-connection
  nil
  "The current network connection")

(defvar dictionary-instances
  0
  "The number of open dictionary buffers")

(defvar dictionary-marker
  nil
  "Stores the point position while buffer display.")

(defvar dictionary-color-support
  (condition-case nil
      (x-display-color-p)
    (error nil))
  "Determines if the Emacs has support to display color")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic function providing startup actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun dictionary-mode ()
  "This is a mode for searching a dictionary server implementing
 the protocol defined in RFC 2229.

 This is a quick reference to this mode describing the default key bindings:

 * q close the dictionary buffer
 * h display this help information
 * s ask for a new word to search
 * d search the word at point
 * n or Tab place point to the next link
 * p or S-Tab place point to the prev link

 * m ask for a pattern and list all matching words.
 * D select the default dictionary
 * M select the default search strategy

 * Return or Button2 visit that link
 "

  (unless (eq major-mode 'dictionary-mode)
    (cl-incf dictionary-instances))

  (kill-all-local-variables)
  (buffer-disable-undo)
  (use-local-map dictionary-mode-map)
  (setq major-mode 'dictionary-mode)
  (setq mode-name "Dictionary")

  (make-local-variable 'dictionary-data-stack)
  (setq dictionary-data-stack nil)
  (make-local-variable 'dictionary-position-stack)
  (setq dictionary-position-stack nil)

  (make-local-variable 'dictionary-current-data)
  (make-local-variable 'dictionary-positions)

  (make-local-variable 'dictionary-default-dictionary)
  (make-local-variable 'dictionary-default-strategy)

  (add-hook 'kill-buffer-hook 'dictionary-close t t)
  (run-hooks 'dictionary-mode-hook))

;;;###autoload
(defun dictionary ()
  "Create a new dictonary buffer and install dictionary-mode"
  (interactive)
  (let ((buffer (or (and dictionary-use-single-buffer
                         (get-buffer "*Dictionary*"))
                    (generate-new-buffer "*Dictionary*")))
        (window-configuration (current-window-configuration))
        (selected-window (frame-selected-window)))

    (switch-to-buffer-other-window buffer)
    (dictionary-mode)

    (make-local-variable 'dictionary-window-configuration)
    (make-local-variable 'dictionary-selected-window)
    (setq dictionary-window-configuration window-configuration)
    (setq dictionary-selected-window selected-window)
    (dictionary-check-connection)
    (dictionary-new-buffer)
    (dictionary-store-positions)
    (dictionary-store-state 'dictionary-new-buffer nil)))

(defun dictionary-new-buffer (&rest ignore)
  "Create a new and clean buffer"

  (dictionary-pre-buffer)
  (dictionary-post-buffer))

(defmacro dictionary-reply-code (reply)
  "Return the reply code stored in `reply'."
  (list 'get reply ''reply-code))

(defmacro dictionary-reply (reply)
  "Return the string reply stored in `reply'."
  (list 'get reply ''reply))

(defmacro dictionary-reply-list (reply)
  "Return the reply list stored in `reply'."
  (list 'get reply ''reply-list))

(defun dictionary-check-connection ()
  "Check if there is already a connection open"
  (if (not (and dictionary-connection
		(eq (dictionary-connection-status dictionary-connection) 'up)))
      (let ((wanted 'raw-text)
	    (coding-system nil))
	(if (member wanted (coding-system-list))
	    (setq coding-system wanted))
	(let ((coding-system-for-read coding-system)
	      (coding-system-for-write coding-system))
	  (message "Opening connection to %s:%s" dictionary-server
		   dictionary-port)
	  (dictionary-connection-close dictionary-connection)
	  (setq dictionary-connection
		(if dictionary-use-http-proxy
		    (dictionary-connection-open dictionary-proxy-server
                                                dictionary-proxy-port)
		  (dictionary-connection-open dictionary-server dictionary-port)))
	  (set-process-query-on-exit-flag
	   (dictionary-connection-process dictionary-connection)
	   nil)

	  (when dictionary-use-http-proxy
	    (message "Proxy CONNECT to %s:%d"
		     dictionary-proxy-server
		     dictionary-proxy-port)
	    (dictionary-send-command (format "CONNECT %s:%d HTTP/1.1"
					     dictionary-server
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
		     (dictionary-reply reply))))))))

(defun dictionary-mode-p ()
  "Return non-nil if current buffer has dictionary-mode"
  (eq major-mode 'dictionary-mode))

(defun dictionary-ensure-buffer ()
  "If current buffer is not a dictionary buffer, create a new one."
  (unless (dictionary-mode-p)
    (dictionary)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dealing with closing the buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictionary-close (&rest ignore)
  "Close the current dictionary buffer and its connection"
  (interactive)
  (if (eq major-mode 'dictionary-mode)
      (progn
	(setq major-mode nil)
	(if (<= (cl-decf dictionary-instances) 0)
	    (dictionary-connection-close dictionary-connection))
	(let ((configuration dictionary-window-configuration)
	      (selected-window dictionary-selected-window))
	  (kill-buffer (current-buffer))
	  (set-window-configuration configuration)
	  (select-window selected-window)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpful functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dictionary-send-command (string)
  "Send the command `string' to the network connection."
  (dictionary-check-connection)
  ;;;; #####
  (dictionary-connection-send-crlf dictionary-connection string))

(defun dictionary-read-reply ()
  "Read the reply line from the server"
  (let ((answer (dictionary-connection-read-crlf dictionary-connection)))
    (if (string-match "\r?\n" answer)
	(substring answer 0 (match-beginning 0))
      answer)))

(defun dictionary-split-string (string)
  "Split the `string' constiting of space separated words into elements.
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
  "Read the reply, split it into words and return it"
  (let ((answer (make-symbol "reply-data"))
	(reply (dictionary-read-reply)))
    (let ((reply-list (dictionary-split-string reply)))
      (put answer 'reply reply)
      (put answer 'reply-list reply-list)
      (put answer 'reply-code (string-to-number (car reply-list)))
      answer)))

(defun dictionary-read-answer ()
  "Read an answer delimited by a . on a single line"
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
  "Check if the reply in `reply' has the `code'."
  (let ((number (dictionary-reply-code reply)))
    (and (numberp number)
	 (= number code))))

(defun dictionary-coding-system (dictionary)
  "Select coding system to use for that dictionary"
  (let ((coding-system
         (or (cdr (assoc dictionary
                         dictionary-coding-systems-for-dictionaries))
             'utf-8)))
    (if (member coding-system (coding-system-list))
        coding-system
      nil)))

(defun dictionary-decode-charset (text dictionary)
  "Convert the text from the charset defined by the dictionary given."
  (let ((coding-system (dictionary-coding-system dictionary)))
    (if coding-system
	(decode-coding-string text coding-system)
      text)))

(defun dictionary-encode-charset (text dictionary)
  "Convert the text to the charset defined by the dictionary given."
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
  "Stores the current state of operation for later restore."

  (if dictionary-current-data
      (progn
	(push dictionary-current-data dictionary-data-stack)
	(unless dictionary-positions
	  (error "dictionary-store-state called before dictionary-store-positions"))
	(push dictionary-positions dictionary-position-stack)))
  (setq dictionary-current-data
	(cons function data)))

(defun dictionary-store-positions ()
  "Stores the current positions for later restore."

  (setq dictionary-positions (cons (point) (window-start))))

;; Restore the previous state
(defun dictionary-restore-state (&rest ignored)
  "Restore the state just before the last operation"
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
  "Save the current state and start a new search"
  (interactive)
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
  "Starts a new search after preparing the buffer"
  (dictionary-pre-buffer)
  (dictionary-do-search word dictionary function))

(defun dictionary-do-search (word dictionary function &optional nomatching)
  "The workhorse for doing the search"

  (message "Searching for %s in %s" word dictionary)
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
	    (beep)
	    (insert "Word not found, maybe you are looking "
		    "for one of these words\n\n")
	    (dictionary-do-matching word
				    dictionary
				    "."
				    'dictionary-display-only-match-result)
	    (dictionary-post-buffer)))
      (if (dictionary-check-reply reply 550)
	  (error "Dictionary \"%s\" is unknown, please select an existing one."
		 dictionary)
	(unless (dictionary-check-reply reply 150)
	  (error "Unknown server answer: %s" (dictionary-reply reply)))
	(funcall function reply)))))

(define-button-type 'dictionary-link
  'face 'dictionary-reference-face
  'action (lambda (button) (funcall (button-get button 'callback)
                                    (button-get button 'data))))

(define-button-type 'dictionary-button
  :supertype 'dictionary-link
  'face 'dictionary-button-face)

(defun dictionary-pre-buffer ()
  "These commands are executed at the begin of a new buffer"
  (setq buffer-read-only nil)
  (erase-buffer)
  (if dictionary-create-buttons
      (progn
        (insert-button "[Back]" :type 'dictionary-button
                       'callback 'dictionary-restore-state
                       'help-echo (purecopy "Mouse-2 to go backwards in history"))
	(insert " ")
        (insert-button "[Search Definition]" :type 'dictionary-button
                       'callback 'dictionary-search
                       'help-echo (purecopy "Mouse-2 to look up a new word"))
	(insert "         ")

	(insert-button "[Matching words]" :type 'dictionary-button
                       'callback 'dictionary-match-words
                       'help-echo (purecopy "Mouse-2 to find matches for a pattern"))
	(insert "        ")

	(insert-button "[Quit]" :type 'dictionary-button
                       'callback 'dictionary-close
                       'help-echo (purecopy "Mouse-2 to close this window"))

	(insert "\n       ")

        (insert-button "[Select Dictionary]" :type 'dictionary-button
                       'callback 'dictionary-select-dictionary
                       'help-echo (purecopy "Mouse-2 to select dictionary for future searches"))
	(insert "         ")
        (insert-button "[Select Match Strategy]" :type 'dictionary-button
                       'callback 'dictionary-select-strategy
                       'help-echo (purecopy "Mouse-2 to select matching algorithm"))
	(insert "\n\n")))
  (setq dictionary-marker (point-marker)))

(defun dictionary-post-buffer ()
  "These commands are executed at the end of a new buffer"
  (goto-char dictionary-marker)

  (set-buffer-modified-p nil)
  (setq buffer-read-only t))

(defun dictionary-display-search-result (reply)
  "This function starts displaying the result starting with the `reply'."

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
	(dictionary-display-word-entry word dictionary description)
	(setq reply (dictionary-read-answer))
	(dictionary-display-word-definition reply word dictionary)
	(setq reply (dictionary-read-reply-and-split))))
    (dictionary-post-buffer)))

(defun dictionary-display-word-entry (word dictionary description)
  "Insert an explanation for the current definition."
  (let ((start (point)))
    (insert "From "
	    dictionary-description-open-delimiter
	    (dictionary-decode-charset description dictionary)
	    dictionary-description-close-delimiter
	    " [" (dictionary-decode-charset dictionary dictionary) "]:"
	    "\n\n")
    (put-text-property start (point) 'face 'dictionary-word-entry-face)))

(defun dictionary-display-word-definition (reply word dictionary)
  "Insert the definition for the current word"
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
	      (if dictionary-color-support
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
  "Format the area from `start' to `end' as link calling `call'.
The word is taken from the buffer, the `dictionary' is given as argument."
  (let ((word (buffer-substring-no-properties start end)))
    (while (string-match "\n\\s-*" word)
      (setq word (replace-match " " t t word)))
    (while (string-match "[*\"]" word)
      (setq word (replace-match "" t t word)))

    (unless (equal word displayed-word)
      (make-button start end :type 'dictionary-link
                   'callback call
                   'data (cons word dictionary)
                   'help-echo (concat "Press Mouse-2 to lookup \""
                                      word "\" in \"" dictionary "\"")))))

(defun dictionary-select-dictionary (&rest ignored)
  "Save the current state and start a dictionary selection"
  (interactive)
  (dictionary-ensure-buffer)
  (dictionary-store-positions)
  (dictionary-do-select-dictionary)
  (dictionary-store-state 'dictionary-do-select-dictionary nil))

(defun dictionary-do-select-dictionary (&rest ignored)
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
      (dictionary-display-dictionarys reply))))

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

(defun dictionary-display-dictionarys (reply)
  "Handle the display of all dictionaries existing on the server"
  (dictionary-pre-buffer)
  (insert "Please select your default dictionary:\n\n")
  (dictionary-display-dictionary-line "* \"All dictionaries\"")
  (dictionary-display-dictionary-line "! \"The first matching dictionary\"")
  (let* ((reply (dictionary-read-answer))
	 (list (dictionary-simple-split-string reply "\n+")))
    (mapc 'dictionary-display-dictionary-line list))
  (dictionary-post-buffer))

(defun dictionary-display-dictionary-line (string)
  "Display a single dictionary"
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
                         'help-echo (purecopy "Mouse-2 to select this dictionary"))
	  (insert "\n")))))

(defun dictionary-set-dictionary (param &optional more)
  "Select this dictionary as new default"

  (if more
      (dictionary-display-more-info param)
    (let ((dictionary (car param)))
      (setq dictionary-default-dictionary dictionary)
      (dictionary-restore-state)
      (message "Dictionary %s has been selected" dictionary))))

(defun dictionary-display-more-info (param)
  "Display the available information on the dictionary"

  (let ((dictionary (car param))
	(description (cdr param)))
    (unless (or (equal dictionary "*")
		(equal dictionary "!"))
      (dictionary-store-positions)
      (message "Requesting more information on %s" dictionary)
      (dictionary-send-command
       (concat "show info " (dictionary-encode-charset dictionary "")))
      (let ((reply (dictionary-read-reply-and-split)))
	(message nil)
	(if (dictionary-check-reply reply 550)
	    (error "Dictionary \"%s\" not existing" dictionary)
	  (unless (dictionary-check-reply reply 112)
	    (error "Unknown server answer: %s" (dictionary-reply reply)))
	  (dictionary-pre-buffer)
	  (insert "Information on dictionary: ")
          (insert-button description :type 'dictionary-link
                         'callback 'dictionary-set-dictionary
                         'data (cons dictionary description)
                         'help-echo (purecopy "Mouse-2 to select this dictionary"))
	  (insert "\n\n")
	  (setq reply (dictionary-read-answer))
	  (insert reply)
	  (dictionary-post-buffer)))

      (dictionary-store-state 'dictionary-display-more-info dictionary))))

(defun dictionary-select-strategy (&rest ignored)
  "Save the current state and start a strategy selection"
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
      (dictionary-display-strategies reply))))

(defun dictionary-display-strategies (reply)
  "Handle the display of all strategies existing on the server"
  (dictionary-pre-buffer)
  (insert "Please select your default search strategy:\n\n")
  (dictionary-display-strategy-line ". \"The servers default\"")
  (let* ((reply (dictionary-read-answer))
	 (list (dictionary-simple-split-string reply "\n+")))
    (mapc 'dictionary-display-strategy-line list))
  (dictionary-post-buffer))

(defun dictionary-display-strategy-line (string)
  "Display a single strategy"
  (let* ((list (dictionary-split-string string))
	 (strategy (car list))
	 (description (cadr list)))
    (if strategy
	(progn
          (insert-button description :type 'dictionary-link
                         'callback 'dictionary-set-strategy
                         'data strategy
                         'help-echo (purecopy "Mouse-2 to select this matching algorithm"))
	  (insert "\n")))))

(defun dictionary-set-strategy (strategy &rest ignored)
  "Select this strategy as new default"
  (setq dictionary-default-strategy strategy)
  (dictionary-restore-state)
  (message "Strategy %s has been selected" strategy))

(defun dictionary-new-matching (word)
  "Run a new matching search on `word'."
  (dictionary-ensure-buffer)
  (dictionary-store-positions)
  (dictionary-do-matching word dictionary-default-dictionary
			  dictionary-default-strategy
			  'dictionary-display-match-result)
  (dictionary-store-state 'dictionary-do-matching
			  (list word dictionary-default-dictionary
				dictionary-default-strategy
				'dictionary-display-match-result)))

(defun dictionary-do-matching (word dictionary strategy function)
  "Ask the server about matches to `word' and display it."

  (message "Lookup matching words for %s in %s using %s"
	   word dictionary strategy)
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
	(error (concat
		"No match for \"%s\" with strategy \"%s\" in "
		"dictionary \"%s\".")
	       word strategy dictionary))
    (unless (dictionary-check-reply reply 152)
      (error "Unknown server answer: %s" (dictionary-reply reply)))
    (funcall function reply)))

(defun dictionary-display-only-match-result (reply)
  "Display the results from the current matches without the headers."

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
  "Display the results from the current matches."
  (dictionary-pre-buffer)

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
  "Display the match lines."
  (mapc (lambda (item)
	  (let ((dictionary (car item))
		(word-list (cdr item)))
	    (insert "Matches from " dictionary ":\n")
	    (mapc (lambda (word)
		    (setq word (dictionary-decode-charset word dictionary))
		    (insert "  ")
                    (insert-button word :type 'dictionary-button
                                   'callback 'dictionary-new-search
                                   'data (cons word dictionary)
                                   'help-echo (purecopy "Mouse-2 to lookup word"))
		    (insert "\n")) (reverse word-list))
	    (insert "\n")))
	list))

;; Returns a sensible default for dictionary-search:
;; - if region is active returns its contents
;; - otherwise return the word near the point
(defun dictionary-search-default ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (current-word t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User callable commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun dictionary-search (word &optional dictionary)
  "Search the `word' in `dictionary' if given or in all if nil.
It presents the word at point as default input and allows editing it."
  (interactive
   (list (let ((default (dictionary-search-default)))
           (read-string (if default
                            (format "Search word (%s): " default)
                          "Search word: ")
                        nil nil default))
	 (if current-prefix-arg
	     (read-string (if dictionary-default-dictionary
			      (format "Dictionary (%s): " dictionary-default-dictionary)
			    "Dictionary: ")
			  nil nil dictionary-default-dictionary)
	   dictionary-default-dictionary)))

  ;; if called by pressing the button
  (unless word
    (setq word (read-string "Search word: ")))
  ;; just in case non-interactivly called
  (unless dictionary
    (setq dictionary dictionary-default-dictionary))
  (dictionary-new-search (cons word dictionary)))

;;;###autoload
(defun dictionary-lookup-definition ()
  "Unconditionally lookup the word at point."
  (interactive)
  (dictionary-new-search (cons (current-word) dictionary-default-dictionary)))

(defun dictionary-previous ()
  "Go to the previous location in the current buffer"
  (interactive)
  (unless (dictionary-mode-p)
    (error "Current buffer is no dictionary buffer"))
  (dictionary-restore-state))

(defun dictionary-help ()
  "Display a little help"
  (interactive)
  (describe-function 'dictionary-mode))

;;;###autoload
(defun dictionary-match-words (&optional pattern &rest ignored)
  "Search `pattern' in current default dictionary using default strategy."
  (interactive)
  ;; can't use interactive because of mouse events
  (or pattern
      (setq pattern (read-string "Search pattern: ")))
  (dictionary-new-matching pattern))

;;;###autoload
(defun dictionary-mouse-popup-matching-words (event)
  "Display entries matching the word at the cursor"
  (interactive "e")
  (let ((word (save-window-excursion
		(save-excursion
		  (mouse-set-point event)
		  (current-word)))))
    (selected-window)
    (dictionary-popup-matching-words word)))

;;;###autoload
(defun dictionary-popup-matching-words (&optional word)
  "Display entries matching the word at the point"
  (interactive)
  (unless (functionp 'popup-menu)
    (error "Sorry, popup menus are not available in this emacs version"))
  (dictionary-do-matching (or word (current-word))
			  dictionary-default-dictionary
			  dictionary-default-popup-strategy
			  'dictionary-process-popup-replies))

(defun dictionary-process-popup-replies (reply)
  (let ((number (nth 1 (dictionary-reply-list reply)))
	(list (dictionary-simple-split-string (dictionary-read-answer) "\n+")))

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
      (let ((menu (make-sparse-keymap 'dictionary-popup)))

	(easy-menu-define dictionary-mode-map-menu dictionary-mode-map
	  "Menu used for displaying dictionary popup"
	  (cons "Matching words"
		`(,@result)))
	(popup-menu dictionary-mode-map-menu)))))

;;; Tooltip support

;; Add a mode indicater named "Dict"
(defvar dictionary-tooltip-mode
  nil
  "Indicates wheather the dictionary tooltip mode is active")
(nconc minor-mode-alist '((dictionary-tooltip-mode " Dict")))

(defcustom dictionary-tooltip-dictionary
  nil
  "This dictionary to lookup words for tooltips"
  :group 'dictionary
  :type '(choice (const :tag "None" nil) string)
  :version "28.1")

(defun dictionary-definition (word &optional dictionary)
  (interactive)
  (unwind-protect
      (let ((dictionary (or dictionary dictionary-default-dictionary)))
	(dictionary-do-search word dictionary 'dictionary-read-definition t))
    nil))

(defun dictionary-read-definition (reply)
  (let ((list (dictionary-simple-split-string (dictionary-read-answer) "\n+")))
    (mapconcat 'identity (cdr list) "\n")))

(defvar global-dictionary-tooltip-mode
  nil)

;;; Tooltip support for GNU Emacs
(defun dictionary-display-tooltip (event)
  "Search the current word in the `dictionary-tooltip-dictionary'."
  (interactive "e")
  (if dictionary-tooltip-dictionary
      (let ((word (save-window-excursion
                    (save-excursion
                      (mouse-set-point event)
                      (current-word)))))
        (let ((definition
                (dictionary-definition word dictionary-tooltip-dictionary)))
          (if definition
              (tooltip-show
               (dictionary-decode-charset definition
                                          dictionary-tooltip-dictionary)))
          t))
    nil))

;;;###autoload
(defun dictionary-tooltip-mode (&optional arg)
  "Display tooltips for the current word"
  (interactive "P")
  (require 'tooltip)
  (let ((on (if arg
                (> (prefix-numeric-value arg) 0)
              (not dictionary-tooltip-mode))))
    (make-local-variable 'dictionary-tooltip-mode)
    (setq dictionary-tooltip-mode on)
    ;; make sure that tooltip is still (global available) even is on
    ;; if nil
    (tooltip-mode 1)
    (add-hook 'tooltip-hook 'dictionary-display-tooltip)
    (make-local-variable 'track-mouse)
    (setq track-mouse on)))

;;;###autoload
(defun global-dictionary-tooltip-mode (&optional arg)
  "Enable/disable dictionary-tooltip-mode for all buffers"
  (interactive "P")
  (require 'tooltip)
  (let* ((on (if arg (> (prefix-numeric-value arg) 0)
               (not global-dictionary-tooltip-mode)))
         (hook-fn (if on 'add-hook 'remove-hook)))
    (setq global-dictionary-tooltip-mode on)
    (tooltip-mode 1)
    (funcall hook-fn 'tooltip-hook 'dictionary-display-tooltip)
    (setq-default dictionary-tooltip-mode on)
    (setq-default track-mouse on)))

(provide 'dictionary)
;;; dictionary.el ends here
