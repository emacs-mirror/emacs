;;; eww.el --- Emacs Web Wowser  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2026 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: html

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

;;; Code:

(require 'cl-lib)
(require 'mm-url)
(require 'puny)
(require 'shr)
(require 'text-property-search)
(require 'thingatpt)
(require 'url)
(require 'url-queue)
(require 'url-file)
(require 'vtable)
(require 'xdg)
(require 'track-changes)
(eval-when-compile (require 'subr-x))

(defgroup eww nil
  "Emacs Web Wowser."
  :version "25.1"
  :link '(custom-manual "(eww) Top")
  :group 'web
  :prefix "eww-")

(defcustom eww-header-line-format "%t: %u"
  "Header line format.
- %t is replaced by the title.
- %u is replaced by the URL."
  :version "24.4"
  :type 'string)

(defcustom eww-search-confirm-send-region t
  "Whether to ask for confirmation before sending the region to a search engine.
Non-nil if EWW should ask for confirmation before sending the
selected region to the configured search engine.  This is the
default to mitigate the risk of accidental data leak.  Set this
variable to nil to send the region to the search engine
straight away."
  :version "31.1"
  :type 'boolean)

(defcustom eww-search-prefix "https://duckduckgo.com/html/?q="
  "Prefix URL to search engine."
  :version "24.4"
  :type 'string)

(defcustom eww-use-browse-url "\\`mailto:"
  "EWW will use `browse-url' when following links that match this regexp.
The action to be taken can be further customized via
`browse-url-handlers'."
  :version "28.1"
  :type 'regexp)

(defcustom eww-default-download-directory "~/Downloads/"
  "Default directory where `eww' saves downloaded files.
Used by `eww--download-directory', which see."
  :version "29.1"
  :type 'directory)

(defun eww--download-directory ()
  "Return the name of the EWW download directory.
The default is specified by `eww-default-download-directory'; however,
if that directory doesn't exist and the DOWNLOAD XDG user directory
is defined, use the latter instead."
  (or (and (file-exists-p eww-default-download-directory)
           eww-default-download-directory)
      (when-let* ((dir (xdg-user-dir "DOWNLOAD")))
        (file-name-as-directory dir))
      eww-default-download-directory))

(defcustom eww-download-directory 'eww--download-directory
  "Directory where files will downloaded.
This should either be a directory name or a function (called with
no parameters) that returns a directory name."
  :version "28.1"
  :type '(choice directory function))

;;;###autoload
(defcustom eww-suggest-uris
  '(eww-links-at-point
    thing-at-point-url-at-point
    eww-current-url
    eww-bookmark-urls)
  "List of functions called to form the list of default URIs for `eww'.
Each of the elements is a function returning either a string or a list
of strings.  The results will be joined into a single list with
duplicate entries (if any) removed."
  :version "30.1"
  :type 'hook
  :options '(eww-links-at-point
             thing-at-point-url-at-point
             eww-current-url
             eww-bookmark-urls))

(defcustom eww-guess-content-type-functions
  '(eww--html-if-doctype)
  "List of functions used by EWW to guess the content-type of Web pages.
These are only used when the page does not have a valid Content-Type
header.  Functions are called in order, until one of them returns a
non-nil value to be used as Content-Type.  The functions receive two
arguments: an alist of page's headers, and the buffer that holds the
complete response of the server from which the page was requested.
If the list of the functions is exhausted without any non-nil value,
EWW assumes content-type is \"application/octet-stream\", per RFC-9110."
  :version "31.1"
  :type '(repeat function))

(defcustom eww-bookmarks-directory user-emacs-directory
  "Directory where bookmark files will be stored."
  :version "25.1"
  :type 'directory)

(defcustom eww-desktop-remove-duplicates t
  "Whether to remove duplicates from the history when saving desktop data.
If non-nil, repetitive EWW history entries (comprising of the URI, the
title, and the point position) will not be saved as part of the Emacs
desktop.  Otherwise, such entries will be retained."
  :version "25.1"
  :type 'boolean)

(defcustom eww-restore-desktop nil
  "How to restore EWW buffers on `desktop-restore'.
If t or `auto', the buffers will be reloaded automatically.
If nil, buffers will require manual reload, and will contain the text
specified in `eww-restore-reload-prompt' instead of the actual Web
page contents."
  :version "25.1"
  :type '(choice (choice :tag "Restore all automatically" :value t
                         (const t)
                         (const auto))
                 (const :tag "Require manual reload" nil)))

(defcustom eww-restore-reload-prompt
  "\n\n *** Use \\[eww-reload] to reload this buffer. ***\n"
  "The string to put in the buffers not reloaded on `desktop-restore'.
This prompt will be used if `eww-restore-desktop' is nil.

The string will be passed through `substitute-command-keys'."
  :version "25.1"
  :type 'string)

(defcustom eww-history-limit 50
  "Maximum number of entries to retain in the history."
  :version "25.1"
  :type '(choice (const :tag "Unlimited" nil)
                 integer))

(defcustom eww-retrieve-command nil
  "Command to retrieve an URL via an external program.
If nil, `url-retrieve' is used to download the data.
If `sync', `url-retrieve-synchronously' is used.
For other non-nil values, this should be a list of strings where
the first item is the program, and the rest are the arguments."
  :version "28.1"
  :type '(choice (const :tag "Use `url-retrieve'" nil)
                 (const :tag "Use `url-retrieve-synchronously'" sync)
                 (repeat :tag "Command/args" string )))

(defcustom eww-use-external-browser-for-content-type
  "\\`\\(video/\\|audio/\\|application/ogg\\)"
  "Always use external browser for specified content-type."
  :version "24.4"
  :type '(choice (const :tag "Never" nil)
                 regexp))

(defcustom eww-browse-url-new-window-is-tab 'tab-bar
  "Whether to open up new windows in a tab or a new buffer.
If t, then open the URL in a new tab rather than a new buffer if
`eww-browse-url' is asked to open it in a new window.
If `tab-bar', then open the URL in a new tab only when
the tab bar is enabled."
  :version "27.1"
  :type '(choice (const :tag "Always open URL in new tab" t)
                 (const :tag "Open new tab when tab bar is enabled" tab-bar)
                 (const :tag "Never open URL in new tab" nil)))

(defcustom eww-before-browse-history-function #'eww-delete-future-history
  "A function to call to update history before browsing to a new page.
EWW provides the following values for this option:

* `eww-delete-future-history': Delete any history entries after the
  currently-shown one.  This is the default behavior, and works the same
  as in most other web browsers.

* `eww-clone-previous-history': Clone and prepend any history entries up
  to the currently-shown one.  This is like `eww-delete-future-history',
  except that it preserves the previous contents of the history list at
  the end.

* `ignore': Preserve the current history unchanged.  This will result in
  the new page simply being prepended to the existing history list.

You can also set this to any other function you wish."
  :version "30.1"
  :type '(choice (function-item :tag "Delete future history"
                                eww-delete-future-history)
                 (function-item :tag "Clone previous history"
                                eww-clone-previous-history)
                 (function-item :tag "Preserve history"
                                ignore)
                 (function :tag "Custom function")))

(defcustom eww-after-render-hook nil
  "A hook called after eww has finished rendering the buffer."
  :version "25.1"
  :type 'hook)

(defcustom eww-auto-rename-buffer nil
  "Automatically rename EWW buffers once the page is rendered.

When nil, do not rename the buffer.  With a non-nil value
determine the renaming scheme, as follows:

- `title': Use the web page's title.
- `url': Use the web page's URL.
- a function's symbol: Run a user-defined function that returns a
  string with which to rename the buffer.  Sample of a
  user-defined function:

  (defun my-eww-rename-buffer ()
    (when (eq major-mode \\='eww-mode)
      (when-let* ((string (or (plist-get eww-data :title)
                              (plist-get eww-data :url))))
        (format \"*%s*\" string))))

The string of `title' and `url' is always truncated to the value
of `eww-buffer-name-length'."
  :version "29.1"
  :type '(choice
          (const :tag "Do not rename buffers (default)" nil)
          (const :tag "Rename buffer to web page title" title)
          (const :tag "Rename buffer to web page URL" url)
          (function :tag "A user-defined function to rename the buffer")))

(defcustom eww-buffer-name-length 40
  "Length of renamed buffer name, per `eww-auto-rename-buffer'."
  :type 'natnum
  :version "29.1")

(defcustom eww-form-checkbox-selected-symbol "[X]"
  "Symbol used to represent a selected checkbox.
See also `eww-form-checkbox-symbol'."
  :version "24.4"
  :type '(choice (const "[X]")
                 (const "☒")            ; Unicode BALLOT BOX WITH X
                 (const "☑")            ; Unicode BALLOT BOX WITH CHECK
                 string))

(defcustom eww-form-checkbox-symbol "[ ]"
  "Symbol used to represent a checkbox.
See also `eww-form-checkbox-selected-symbol'."
  :version "24.4"
  :type '(choice (const "[ ]")
                 (const "☐")            ; Unicode BALLOT BOX
                 string))

(defcustom eww-url-transformers '(eww-remove-tracking)
  "This is a list of transforming functions applied to an URL before usage.
The functions will be called with the URL as the single
parameter, and should return the (possibly) transformed URL."
  :type '(repeat function)
  :version "29.1")

(defcustom eww-readable-urls nil
  "A list of regexps matching URLs to display in readable mode by default.
EWW will display matching URLs using `eww-readable' (which see).

Each element can be one of the following forms: a regular expression in
string form or a cons cell of the form (REGEXP . READABILITY).  If
READABILITY is non-nil, this behaves the same as the string form;
otherwise, URLs matching REGEXP will never be displayed in readable mode
by default."
  :type '(repeat (choice (string :tag "Readable URL")
                         (cons :tag "URL and Readability"
                               (string :tag "URL")
                               (radio (const :tag "Readable" t)
                                      (const :tag "Non-readable" nil)))))
  :version "30.1")

(defcustom eww-readable-adds-to-history t
  "If non-nil, calling `eww-readable' adds a new entry to the history."
  :type 'boolean
  :version "30.1")

(defface eww-form-submit
  '((((type x w32 ns haiku pgtk android) (class color))	; Like default mode line
     :box (:line-width 2 :style released-button)
     :background "#808080" :foreground "black"))
  "Face for eww buffer buttons."
  :version "24.4")

(defface eww-form-file
  '((((type x w32 ns haiku pgtk android) (class color))	; Like default mode line
     :box (:line-width 2 :style released-button)
     :background "#808080" :foreground "black"))
  "Face for eww buffer buttons."
  :version "25.1")

(defface eww-form-checkbox
  '((((type x w32 ns haiku pgtk android) (class color))	; Like default mode line
     :box (:line-width 2 :style released-button)
     :background "lightgrey" :foreground "black"))
  "Face for eww buffer buttons."
  :version "24.4")

(defface eww-form-select
  '((((type x w32 ns haiku pgtk android) (class color))	; Like default mode line
     :box (:line-width 2 :style released-button)
     :background "lightgrey" :foreground "black"))
  "Face for eww buffer buttons."
  :version "24.4")

(defface eww-form-text
  '((t :background "#505050"
       :foreground "white"
       :box (:line-width 1)))
  "Face for eww text inputs."
  :version "24.4")

(defface eww-form-textarea
  '((t :background "#C0C0C0"
       :foreground "black"
       :box (:line-width 1)))
  "Face for eww textarea inputs."
  :version "24.4")

(defface eww-invalid-certificate
  '((default :weight bold)
    (((class color)) :foreground "red"))
  "Face for web pages with invalid certificates."
  :version "25.1")

(defface eww-valid-certificate
  '((default :weight bold)
    (((class color)) :foreground "ForestGreen"))
  "Face for web pages with valid certificates."
  :version "25.1")

(defvar eww-data nil)
(defvar eww-history nil)
(defvar eww-history-position 0
  "The 1-indexed position in `eww-history'.
If zero, EWW is at the newest page, which isn't yet present in
`eww-history'.")
(defvar eww-prompt-history nil)
(defvar-local eww--change-tracker-id nil)

(defvar eww-local-regex "localhost"
  "When this regex is found in the URL, it's not a keyword but an address.")

(defvar eww-accept-content-types
  "text/html, text/plain, text/sgml, text/css, application/xhtml+xml, */*;q=0.01"
  "Value used for the HTTP \"Accept\" header.")

(defvar-keymap eww-link-keymap
  :parent shr-map
  "RET" #'eww-follow-link
  "<mouse-2>" #'eww-follow-link)

(defvar-keymap eww-image-link-keymap
  :parent shr-image-map
  "RET" #'eww-follow-link)

(defvar-keymap eww-minibuffer-url-keymap
  :doc "Keymap used in the minibuffer prompt for URLs or keywords."
  :parent minibuffer-local-completion-map
  "SPC" #'self-insert-command
  "?" #'self-insert-command)

(defun eww-suggested-uris nil
  "Return the list of URIs to suggest at the `eww' prompt.
This list can be customized via `eww-suggest-uris'."
  (let ((obseen (obarray-make 42))
	(uris nil))
    (dolist (fun eww-suggest-uris)
      (let ((ret (funcall fun)))
	(dolist (uri (if (stringp ret) (list ret) ret))
	  (when (and uri (not (intern-soft uri obseen)))
	    (intern uri obseen)
	    (push   uri uris)))))
    (nreverse uris)))

;;;###autoload
(defun eww-browse ()
  "Function to be run to parse command line URLs.
This is meant to be used for MIME handlers or command line use.

Setting the handler for \"text/x-uri;\" to
\"emacs -f eww-browse %u\" will then start up Emacs and call eww
to browse the url.

This can also be used on the command line directly:

 emacs -f eww-browse https://gnu.org

will start Emacs and browse the GNU web site."
  (interactive)
  (unless command-line-args-left
    (user-error "No URL given"))
  (eww (pop command-line-args-left)))


;;;###autoload
(defun eww (url &optional new-buffer buffer)
  "Fetch URL and render the page.
If the input doesn't look like an URL or a domain name, the
word(s) will be searched for via `eww-search-prefix'.

If NEW-BUFFER is non-nil (interactively, the prefix arg), use a
new buffer instead of reusing the default EWW buffer.

If BUFFER, the data to be rendered is in that buffer.  In that
case, this function doesn't actually fetch URL.  BUFFER will be
killed after rendering.

For more information, see Info node `(eww) Top'."
  (interactive
   (let ((uris (eww-suggested-uris))
         (minibuffer-local-completion-map eww-minibuffer-url-keymap))
     (list (completing-read (format-prompt "Enter URL or keywords"
                                           (and uris (car uris)))
                            (seq-uniq (append eww-prompt-history uris))
                            nil nil nil 'eww-prompt-history uris)
           current-prefix-arg)))
  (setq url (eww--dwim-expand-url url))
  (pop-to-buffer-same-window
   (cond
    (new-buffer
     (generate-new-buffer "*eww*"))
    ((eq major-mode 'eww-mode)
     (current-buffer))
    (t
     (get-buffer-create "*eww*"))))
  (eww-setup-buffer)
  (eww--before-browse)
  ;; Check whether the domain only uses "Highly Restricted" Unicode
  ;; IDNA characters.  If not, transform to punycode to indicate that
  ;; there may be funny business going on.
  (let ((parsed (url-generic-parse-url url)))
    (when (url-host parsed)
      (unless (puny-highly-restrictive-domain-p (url-host parsed))
        (setf (url-host parsed) (puny-encode-domain (url-host parsed)))))
    ;; When the URL is on the form "http://a/../../../g", chop off all
    ;; the leading "/.."s.
    (when (url-filename parsed)
      (while (string-match "\\`/[.][.]/" (url-filename parsed))
        (setf (url-filename parsed) (substring (url-filename parsed) 3))))
    (setq url (url-recreate-url parsed)))
  (setq url (eww--transform-url url))
  (plist-put eww-data :url url)
  (plist-put eww-data :title "")
  (eww--after-page-change)
  (let ((inhibit-read-only t))
    (insert (format "Loading %s..." url))
    (goto-char (point-min)))
  (let ((url-mime-accept-string eww-accept-content-types))
    (if buffer
        (let ((eww-buffer (current-buffer)))
          (with-current-buffer buffer
            (eww-render nil url nil eww-buffer)))
      (eww-retrieve url #'eww-render
                    (list url nil (current-buffer))))))

(defun eww-retrieve (url callback cbargs)
  (cond
   ((null eww-retrieve-command)
    (url-retrieve url callback cbargs))
   ((eq eww-retrieve-command 'sync)
    (let ((data-buffer (url-retrieve-synchronously url)))
      (with-current-buffer data-buffer
        (apply callback nil cbargs))))
   (t
    (let ((buffer (generate-new-buffer " *eww retrieve*"))
          (error-buffer (generate-new-buffer " *eww error*")))
      (with-current-buffer buffer
        (set-buffer-multibyte nil)
        (make-process
         :name "*eww fetch*"
         :buffer (current-buffer)
         :stderr error-buffer
         :command (append eww-retrieve-command (list url))
         :sentinel (lambda (process _)
                     (unless (process-live-p process)
                       (when (buffer-live-p error-buffer)
                         (when (get-buffer-process error-buffer)
                           (delete-process (get-buffer-process error-buffer) ))
                         (kill-buffer error-buffer))
                       (when (buffer-live-p buffer)
                         (with-current-buffer buffer
                           (goto-char (point-min))
                           (insert "Content-type: text/html; charset=utf-8\n\n")
                           (apply #'funcall callback nil cbargs)))))))))))

(function-put 'eww 'browse-url-browser-kind 'internal)

(defun eww--dwim-expand-url (url)
  (setq url (string-trim url))
  (cond ((string-match-p "\\`file:/" url))
	;; Don't mangle file: URLs at all.
        ((string-match-p "\\`ftp://" url)
         (user-error "FTP is not supported"))
        (t
	 ;; Anything that starts with something that vaguely looks
	 ;; like a protocol designator is interpreted as a full URL.
         (if (or (string-match "\\`[A-Za-z]+:" url)
		 ;; Also try to match "naked" URLs like
		 ;; en.wikipedia.org/wiki/Free software
		 (string-match "\\`[A-Za-z_]+\\.[A-Za-z._]+/" url)
		 (and (= (length (split-string url)) 1)
		      (or (and (not (string-match-p "\\`[\"'].*[\"']\\'" url))
			       (> (length (split-string url "[.:]")) 1))
			  (string-match eww-local-regex url))))
             (progn
               (unless (string-match-p "\\`[a-zA-Z][-a-zA-Z0-9+.]*://" url)
                 (setq url (concat "http://" url)))
               ;; Some sites do not redirect final /
               (when (string= (url-filename (url-generic-parse-url url)) "")
                 (setq url (concat url "/"))))
           (setq url (concat eww-search-prefix
                             (mapconcat
                              #'url-hexify-string (split-string url) "+"))))))
  url)

(defun eww--preprocess-html (start end)
  "Translate all < characters that do not look like start of tags into &lt;."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let ((case-fold-search t))
        (while (re-search-forward "<[^0-9a-z!?/]" nil t)
          (goto-char (match-beginning 0))
          (delete-region (point) (1+ (point)))
          (insert "&lt;"))))))

;;;###autoload (defalias 'browse-web 'eww)

;;;###autoload
(defun eww-open-file (file &optional new-buffer)
  "Render FILE using EWW.
If NEW-BUFFER is non-nil (interactively, the prefix arg), use a
new buffer instead of reusing the default EWW buffer."
  (interactive "fFile: \nP")
  (let ((url-allow-non-local-files t))
    (eww (concat "file://"
	         (and (memq system-type '(windows-nt ms-dos))
		      "/")
	         (expand-file-name file))
         new-buffer)))

(defun eww--file-buffer (file)
  (with-current-buffer (generate-new-buffer " *eww file*")
    (set-buffer-multibyte nil)
    (insert "Content-type: " (or (mailcap-extension-to-mime
			          (url-file-extension file))
                                 "application/octet-stream")
            "\n\n")
    (insert-file-contents file)
    (current-buffer)))

;;;###autoload
(defun eww-search-words ()
  "Search the web for the text in the region.
If region is active (and not whitespace), search the web for the
text between region beginning and end, subject to user's confirmation
controlled by `eww-search-confirm-send-region'.  Else, prompt the
user for a search string.  See the variable `eww-search-prefix'
for the search engine used."
  (interactive)
  (if (use-region-p)
      (let ((region-string (buffer-substring (region-beginning) (region-end))))
        (if (not (string-match-p "\\`[ \n\t\r\v\f]*\\'" region-string))
            (when
                (or (not eww-search-confirm-send-region)
                    (yes-or-no-p
                     (format-message
                      "Really send the region to the search engine? ")))
              (eww region-string))
          (call-interactively #'eww)))
    (call-interactively #'eww)))

(defun eww--open-url-in-new-buffer (url)
  "Open the URL in a new EWW buffer."
  ;; Clone is useful to keep history, but we
  ;; should not clone from a non-eww buffer.
  (with-current-buffer
      (if (eq major-mode 'eww-mode) (clone-buffer)
        (generate-new-buffer "*eww*"))
    (unless (equal url (eww-current-url))
      (eww-mode)
      (eww (if (consp url) (car url) url)))))

(defun eww-open-in-new-buffer (&optional no-select url)
  "Fetch URL (interactively, the link at point) into a new EWW buffer.

NO-SELECT non-nil means do not make the new buffer the current buffer."
  (interactive "P")
  (if-let* ((url (or url (eww-suggested-uris))))
      (if (or (eq eww-browse-url-new-window-is-tab t)
              (and (eq eww-browse-url-new-window-is-tab 'tab-bar)
                   tab-bar-mode))
          (let ((tab-bar-new-tab-choice t))
            (tab-new)
            (eww--open-url-in-new-buffer url)
            (when no-select
              (tab-bar-switch-to-recent-tab)))
        (if no-select
            (save-window-excursion (eww--open-url-in-new-buffer url))
          (eww--open-url-in-new-buffer url)))
    (user-error "No link at point")))

(defun eww-html-p (content-type)
  "Return non-nil if CONTENT-TYPE designates an HTML content type.
Currently this means either text/html or application/xhtml+xml."
  (member content-type '("text/html"
			 "application/xhtml+xml")))

(defun eww--guess-content-type (headers response-buffer)
  "Use HEADERS and RESPONSE-BUFFER to guess the Content-Type.
Will call each function in `eww-guess-content-type-functions', until one
of them returns a value.  This mechanism is used only if there isn't a
valid Content-Type header.  If none of the functions can guess, return
\"application/octet-stream\"."
  (save-excursion
    (or (run-hook-with-args-until-success
         'eww-guess-content-type-functions headers response-buffer)
        "application/octet-stream")))

(defun eww--html-if-doctype (_headers response-buffer)
  "Return \"text/html\" if RESPONSE-BUFFER has an HTML doctype declaration.
HEADERS is unused."
  ;; https://html.spec.whatwg.org/multipage/syntax.html#the-doctype
  (with-current-buffer response-buffer
    (let ((case-fold-search t))
      (save-excursion
        (goto-char (point-min))
        ;; Match basic "<!doctype html>" and also legacy variants as
        ;; specified in link above -- being purposely lax about it.
        (when (search-forward "<!doctype html" nil t)
          "text/html")))))

(defun eww--rename-buffer ()
  "Rename the current EWW buffer.
The renaming scheme is performed in accordance with
`eww-auto-rename-buffer'."
  (let ((rename-string)
        (formatter
         (lambda (string)
           (format "*%s # eww*" (truncate-string-to-width
                                 string eww-buffer-name-length))))
        (site-title (plist-get eww-data :title))
        (site-url (plist-get eww-data :url)))
    (cond ((null eww-auto-rename-buffer))
          ((eq eww-auto-rename-buffer 'url)
           (setq rename-string (funcall formatter site-url)))
          ((functionp eww-auto-rename-buffer)
           (setq rename-string (funcall eww-auto-rename-buffer)))
          (t (setq rename-string
                   (funcall formatter (if (or (equal site-title "")
                                              (null site-title))
                                          "Untitled"
                                        site-title)))))
    (when rename-string
      (rename-buffer rename-string t))))

(defun eww-render (status url &optional point buffer encode)
  (let* ((headers (eww-parse-headers))
	 (content-type
	  (mail-header-parse-content-type
           (if (zerop (length (cdr (assoc "content-type" headers))))
               (eww--guess-content-type headers (current-buffer))
             (cdr (assoc "content-type" headers)))))
	 (charset (intern
		   (downcase
		    (or (cdr (assq 'charset (cdr content-type)))
			(eww-detect-charset (eww-html-p (car content-type)))
			"utf-8"))))
	 (data-buffer (current-buffer))
	 (shr-target-id (url-target (url-generic-parse-url url)))
	 last-coding-system-used)
    (let ((redirect (plist-get status :redirect)))
      (when redirect
        (setq url redirect)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Save the https peer status.
        (plist-put eww-data :peer (plist-get status :peer))
        ;; Make buffer listings more informative.
        (setq list-buffers-directory url)
        ;; Let the URL library have a handle to the current URL for
        ;; referer purposes.
        (setq url-current-lastloc (url-generic-parse-url url)))
      (unwind-protect
	  (progn
	    (cond
             ((and eww-use-external-browser-for-content-type
                   (string-match-p eww-use-external-browser-for-content-type
                                   (car content-type)))
              (erase-buffer)
              (insert "<title>Unsupported content type</title>")
              (insert (format "<h1>Content-type %s is unsupported</h1>"
                              (car content-type)))
              (insert (format "<a href=%S>Direct link to the document</a>"
                              url))
              (goto-char (point-min))
              (eww-display-html (or encode charset) url nil point buffer))
	     ((eww-html-p (car content-type))
              (eww-display-html (or encode charset) url nil point buffer))
	     ((equal (car content-type) "application/pdf")
	      (eww-display-pdf))
	     ((string-match-p "\\`image/" (car content-type))
	      (eww-display-image buffer))
	     (t
	      (eww-display-raw buffer (or encode charset 'utf-8))))
	    (with-current-buffer buffer
	      (plist-put eww-data :url url)
	      (eww--after-page-change)
	      (and last-coding-system-used
		   (set-buffer-file-coding-system last-coding-system-used))
              (unless shr-fill-text
                (visual-line-mode)
                (visual-wrap-prefix-mode))
	      (run-hooks 'eww-after-render-hook)
              ;; Enable undo again so that undo works in text input
              ;; boxes.
              (setq buffer-undo-list nil)))
        (kill-buffer data-buffer)))
    (unless (buffer-live-p buffer)
      (kill-buffer data-buffer))))

(defun eww-parse-headers ()
  (let ((headers nil))
    (goto-char (point-min))
    (while (and (not (eobp))
		(not (eolp)))
      (when (looking-at "\\([^:]+\\): *\\(.*\\)")
	(push (cons (downcase (match-string 1))
		    (match-string 2))
	      headers))
      (forward-line 1))
    (unless (eobp)
      (forward-line 1))
    headers))

(defun eww-detect-charset (html-p)
  (let ((case-fold-search t)
	(pt (point)))
    (or (and html-p
	     (re-search-forward
	      "<meta[\t\n\r ]+[^>]*charset=\"?\\([^\t\n\r \"/>]+\\)[\\\"'.*]" nil t)
	     (goto-char pt)
	     (match-string 1))
	(and (looking-at
	      "[\t\n\r ]*<\\?xml[\t\n\r ]+[^>]*encoding=\"\\([^\"]+\\)")
	     (match-string 1)))))

(declare-function libxml-parse-html-region "xml.c"
		  (start end &optional base-url discard-comments))

(defun eww--parse-html-region (start end &optional coding-system)
  "Parse the HTML between START and END, returning the DOM as an S-expression.
Use CODING-SYSTEM to decode the region; if nil, decode as UTF-8.

This replaces the region with the preprocessed HTML."
  (setq coding-system (or coding-system 'utf-8))
  (with-restriction start end
    (unless (and (not enable-multibyte-characters)
		 (eq coding-system 'utf-8))
      (condition-case nil
          (decode-coding-region (point-min) (point-max) coding-system)
        (coding-system-error nil)))
    ;; Remove CRLF and replace NUL with &#0; before parsing.
    (while (re-search-forward "\\(\r$\\)\\|\0" nil t)
      (replace-match (if (match-beginning 1) "" "&#0;") t t))
    (eww--preprocess-html (point-min) (point-max))
    (libxml-parse-html-region (point-min) (point-max))))

(defsubst eww-document-base (url dom)
  `(base ((href . ,url)) ,dom))

(defun eww-display-document (document &optional point buffer)
  (unless (fboundp 'libxml-parse-html-region)
    (error "This function requires Emacs to be compiled with libxml2"))
  (setq buffer (or buffer (current-buffer)))
  (unless (buffer-live-p buffer)
    (error "Buffer %s doesn't exist" buffer))
  ;; There should be a better way to abort loading images
  ;; asynchronously.
  (setq url-queue nil)
  (let ((url (when (eq (car document) 'base)
               (alist-get 'href (cadr document)))))
    (unless url
      (error "Document is missing base URL"))
    (with-current-buffer buffer
      (setq bidi-paragraph-direction nil)
      (plist-put eww-data :dom document)
      (let ((inhibit-read-only t)
            ;; Possibly set by the caller, e.g., `eww-render' which
            ;; preserves the old URL #target before chasing redirects.
            (shr-target-id (or shr-target-id
                               (url-target (url-generic-parse-url url))))
	    (shr-external-rendering-functions
             (append
              shr-external-rendering-functions
              '((title . eww-tag-title)
                (form . eww-tag-form)
                (input . eww-tag-input)
                (button . eww-form-submit)
                (textarea . eww-tag-textarea)
                (select . eww-tag-select)
                (link . eww-tag-link)
                (meta . eww-tag-meta)
                (a . eww-tag-a)))))
        ;; Unregister any existing change tracker while we render the
        ;; document.
        (when eww--change-tracker-id
          (track-changes-unregister eww--change-tracker-id)
          (setq eww--change-tracker-id nil))
	(erase-buffer)
        (with-delayed-message (2 "Rendering HTML...")
	  (shr-insert-document document))
	(cond
	 (point
	  (goto-char point))
	 (shr-target-id
	  (goto-char (point-min))
          (let ((match (text-property-search-forward
                        'shr-target-id shr-target-id #'member)))
            (when match
              (goto-char (prop-match-beginning match)))))
	 (t
	  (goto-char (point-min))
	  ;; Don't leave point inside forms, because the normal eww
	  ;; commands aren't available there.
	  (while (and (not (eobp))
		      (get-text-property (point) 'eww-form))
	    (forward-line 1)))))
      ;; We used to enable this in `eww-mode', but it cause tracking of
      ;; changes while we insert the document, whereas we only care
      ;; about changes performed afterwards.
      (setq eww--change-tracker-id (track-changes-register
                                    #'eww--track-changes :nobefore t))
      (eww-size-text-inputs))))

(defun eww-display-html (charset url &optional document point buffer)
  (let ((source (buffer-substring (point) (point-max))))
    (with-current-buffer buffer
      (plist-put eww-data :source source)))
  (unless document
    (let ((dom (eww--parse-html-region (point) (point-max) charset))
          readable)
      (when-let* (((eww-default-readable-p url))
                  (readable-dom (eww-readable-dom dom)))
        (setq dom readable-dom
              readable t))
      (with-current-buffer buffer
        (plist-put eww-data :readable readable))
      (setq document (eww-document-base url dom))))
  (eww-display-document document point buffer))

(defun eww-handle-link (dom)
  (let* ((rel (dom-attr dom 'rel))
	 (href (dom-attr dom 'href))
	 (where (assoc
		 ;; The text associated with :rel is case-insensitive.
		 (if rel (downcase rel))
		 '(("next" . :next)
		   ;; Texinfo uses "previous", but HTML specifies
		   ;; "prev", so recognize both.
		   ("previous" . :previous)
		   ("prev" . :previous)
		   ;; HTML specifies "start" but also "contents",
		   ;; and Gtk seems to use "home".  Recognize
		   ;; them all; but store them in different
		   ;; variables so that we can readily choose the
		   ;; "best" one.
		   ("start" . :start)
		   ("home" . :home)
		   ("contents" . :contents)
		   ("up" . :up)))))
    (when (and href where)
      (when (memq (cdr where) '(:next :previous))
        ;; Multi-page isearch support.
        (setq-local multi-isearch-next-buffer-function
                    #'eww-isearch-next-buffer))
      (plist-put eww-data (cdr where) href))))

(defvar eww-redirect-level 1)

(defun eww-tag-meta (dom)
  (when (and (cl-equalp (dom-attr dom 'http-equiv) "refresh")
             (< eww-redirect-level 5))
    (when-let* ((refresh (dom-attr dom 'content)))
      (when (or (string-match "^\\([0-9]+\\) *;.*url=\"\\([^\"]+\\)\"" refresh)
                (string-match "^\\([0-9]+\\) *;.*url='\\([^']+\\)'" refresh)
                (string-match "^\\([0-9]+\\) *;.*url=\\([^ ]+\\)" refresh))
        (let ((timeout (match-string 1 refresh))
              (url (match-string 2 refresh))
              (eww-redirect-level (1+ eww-redirect-level)))
          (if (equal timeout "0")
              (eww (shr-expand-url url))
            (eww-tag-a
             (dom-node 'a `((href . ,(shr-expand-url url)))
                       (format "Auto refresh in %s second%s disabled"
                               timeout
                               (if (equal timeout "1")
                                   ""
                                 "s"))))))))))

(defun eww-tag-link (dom)
  (eww-handle-link dom)
  (shr-generic dom))

(defun eww-tag-a (dom)
  (eww-handle-link dom)
  (let ((start (point)))
    (shr-tag-a dom)
    (if (dom-attr dom 'href)
        (put-text-property start (point)
                           'keymap
                           (if (mm-images-in-region-p start (point))
                               eww-image-link-keymap
                             eww-link-keymap)))))

(defun eww--limit-string-pixelwise (string pixels)
  (if (not pixels)
      string
    (with-temp-buffer
      (insert string)
      (if (< (eww--pixel-column) pixels)
	  string
	;; Iterate to find appropriate length.
	(while (and (> (eww--pixel-column) pixels)
		    (not (bobp)))
	  (forward-char -1))
	;; Return at least one character.
	(buffer-substring (point-min) (max (point)
					   (1+ (point-min))))))))

(defun eww--pixel-column ()
  (if (not (get-buffer-window (current-buffer)))
      (save-window-excursion
        ;; Avoid errors if the selected window is a dedicated one,
        ;; and they just want to insert a document into it.
        (set-window-dedicated-p nil nil)
	(set-window-buffer nil (current-buffer))
	(car (window-text-pixel-size nil (line-beginning-position) (point))))
    (car (window-text-pixel-size nil (line-beginning-position) (point)))))

(defun eww-update-header-line-format ()
  (setq header-line-format
	(and eww-header-line-format
	     (let ((peer (plist-get eww-data :peer))
                   (url (plist-get eww-data :url))
                   (title (propertize
                           (if (zerop (length (plist-get eww-data :title)))
		               "[untitled]"
                             (plist-get eww-data :title))
                           'face 'variable-pitch)))
	       ;; This connection is https.
	       (when peer
                 (add-face-text-property 0 (length title)
				         (if (plist-get peer :warnings)
				             'eww-invalid-certificate
				           'eww-valid-certificate)
                                         t title))
               ;; Limit the length of the title so that the host name
               ;; of the URL is always visible.
               (when url
                 (setq url (propertize url 'face 'variable-pitch))
                 (let* ((parsed (url-generic-parse-url url))
                        (host-length (string-pixel-width
                                      (propertize
                                       (format "%s://%s" (url-type parsed)
                                               (url-host parsed))
                                       'face 'variable-pitch)))
                        (width (window-width nil t)))
                   (cond
                    ;; The host bit is wider than the window, so nix
                    ;; the title.
                    ((> (+ host-length (string-pixel-width "xxxxx")) width)
                     (setq title ""))
                    ;; Trim the title.
                    ((> (+ (string-pixel-width (concat title "xx"))
                           host-length)
                        width)
                     (setq title
                           (concat
                            (eww--limit-string-pixelwise
                             title (- width host-length
                                      (string-pixel-width
                                       (propertize "...: " 'face
                                                   'variable-pitch))))
                            (propertize "..." 'face 'variable-pitch)))))))
	       (string-replace
		"%" "%%"
		(format-spec
		 eww-header-line-format
		 `((?u . ,(or url ""))
		   (?t . ,title))))))))

(defun eww--before-browse ()
  (funcall eww-before-browse-history-function)
  (setq eww-history-position 0
        eww-data (list :title "")))

(defun eww--after-page-change ()
  (eww-update-header-line-format)
  (eww--rename-buffer))

(defun eww-tag-title (dom)
  (plist-put eww-data :title
	     (replace-regexp-in-string
	      "^ \\| $" ""
	      (replace-regexp-in-string "[ \t\r\n]+" " " (dom-inner-text dom))))
  (eww--after-page-change))

(defun eww-display-raw (buffer &optional encode)
  (let ((data (buffer-substring (point) (point-max))))
    (unless (buffer-live-p buffer)
      (error "Buffer %s doesn't exist" buffer))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert data)
	(condition-case nil
	    (decode-coding-region (point-min) (point) encode)
	  (coding-system-error nil)))
      (goto-char (point-min)))))

(defun eww-display-image (buffer)
  (let ((data (shr-parse-image-data)))
    (unless (buffer-live-p buffer)
      (error "Buffer %s doesn't exist" buffer))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
	(erase-buffer)
	(shr-put-image data nil))
      (goto-char (point-min)))))

(declare-function mailcap-view-mime "mailcap" (type))
(defun eww-display-pdf ()
  (let ((buf (current-buffer))
        (pos (point)))
    (with-current-buffer (get-buffer-create "*eww pdf*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (set-buffer-multibyte nil)
        (insert-buffer-substring buf pos)
        (mailcap-view-mime "application/pdf"))
      (if (zerop (buffer-size))
          ;; Buffer contents passed to shell command via temporary file.
          (kill-buffer)
        (goto-char (point-min))
        (pop-to-buffer-same-window (current-buffer))))))

(defun eww-setup-buffer ()
  (when (or (plist-get eww-data :url)
            (plist-get eww-data :dom))
    (eww-save-history))
  (let ((inhibit-read-only t))
    (remove-overlays)
    (erase-buffer))
  (setq bidi-paragraph-direction nil)
  ;; May be set later if there's a next/prev link.
  (setq-local multi-isearch-next-buffer-function nil)
  (unless (eq major-mode 'eww-mode)
    (eww-mode))
  (buffer-disable-undo))

(defun eww-current-url nil
  "Return URI of the Web page the current EWW buffer is visiting."
  (plist-get eww-data :url))

(defun eww-links-at-point ()
  "Return list of URIs, if any, linked at point."
  (seq-filter #'stringp
	      (list (get-text-property (point) 'shr-url)
	            (get-text-property (point) 'image-url))))

(defun eww-view-source ()
  "View the HTML source code of the current page."
  (interactive nil eww-mode)
  (let ((buf (get-buffer-create "*eww-source*"))
        (source (plist-get eww-data :source)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
	(delete-region (point-min) (point-max))
	(insert (or source "no source"))
	(goto-char (point-min))
        ;; Decode the source and set the buffer's encoding according
        ;; to what the HTML source specifies in its 'charset' header,
        ;; if any.
        (let ((cs (find-auto-coding "" (point-max))))
          (when (consp cs)
            (setq cs (car cs))
            (when (coding-system-p cs)
              (decode-coding-region (point-min) (point-max) cs)
              (setq buffer-file-coding-system last-coding-system-used))))
        (cond
         ((fboundp 'mhtml-mode)
          (mhtml-mode))
         ((fboundp 'html-mode)
	  (html-mode)))))
    (view-buffer buf)))

(defun eww-toggle-paragraph-direction ()
  "Cycle the paragraph direction between left-to-right, right-to-left and auto."
  (interactive nil eww-mode)
  (setq bidi-paragraph-direction
        (cond ((eq bidi-paragraph-direction 'left-to-right)
               nil)
              ((eq bidi-paragraph-direction 'right-to-left)
               'left-to-right)
              (t
               'right-to-left)))
  (message "The paragraph direction is now %s"
           (if (null bidi-paragraph-direction)
               "automatic"
             bidi-paragraph-direction)))

(defun eww-readable (&optional arg)
  "Toggle display of only the main \"readable\" parts of the current web page.
This command uses heuristics to find the parts of the web page that
contain the main textual portion, leaving out navigation menus and the
like.

If called interactively, toggle the display of the readable parts.  If
the prefix argument is positive, display the readable parts, and if it
is zero or negative, display the full page.

If called from Lisp, toggle the display of the readable parts if ARG is
`toggle'.  Display the readable parts if ARG is nil, omitted, or is a
positive number.  Display the full page if ARG is a negative number.

When `eww-readable-adds-to-history' is non-nil, calling this function
adds a new entry to `eww-history'."
  (interactive (list (if current-prefix-arg
                         (prefix-numeric-value current-prefix-arg)
                       'toggle))
               eww-mode)
  (let* ((old-data eww-data)
	 (make-readable (cond
                         ((eq arg 'toggle)
                          (not (plist-get old-data :readable)))
                         ((and (numberp arg) (< arg 1))
                          nil)
                         (t t)))
         (dom (with-temp-buffer
		(insert (plist-get old-data :source))
                (eww--parse-html-region (point-min) (point-max))))
         (base (plist-get eww-data :url)))
    (when make-readable
      (unless (setq dom (eww-readable-dom dom))
        (message "Unable to extract readable text from this page")))
    (when dom
      (when eww-readable-adds-to-history
        (eww-save-history)
        (eww--before-browse)
        (dolist (elem '(:source :url :peer))
          (plist-put eww-data elem (plist-get old-data elem))))
      (eww-display-document (eww-document-base base dom))
      (plist-put eww-data :readable make-readable)
      (eww--after-page-change))))

(defun eww--string-count-words (string)
  "Return the number of words in STRING."
  (let ((start 0)
        (count 0))
    (while (string-match split-string-default-separators string start)
      (when (< start (match-beginning 0))
        (incf count))
      (setq start (match-end 0)))
    (when (length> string (1+ start))
      (incf count))
    count))

(defun eww--dom-count-words (node)
  "Return the number of words in all the textual data under NODE."
  (cond
   ((stringp node)
    (eww--string-count-words node))
   ((memq (dom-tag node) '(script comment))
    0)
   (t
    (let ((total 0))
      (dolist (elem (dom-children node) total)
        (incf total (eww--dom-count-words elem)))))))

(defun eww--walk-readability (node callback &optional noscore)
  "Walk through all children of NODE to score readability.
After scoring, call CALLBACK with the node and score.  If NOSCORE is
non-nil, don't actually compute a score; just call the callback."
  (let ((score nil))
    (unless noscore
      (cond
       ((stringp node)
        (setq score (eww--string-count-words node)
              noscore t))
       ((memq (dom-tag node) '(head comment script style template))
        (setq score -2
              noscore t))
       ((eq (dom-tag node) 'meta)
        (setq score -1
              noscore t))
       ((eq (dom-tag node) 'img)
        (setq score 2
              noscore t))
       ((eq (dom-tag node) 'a)
        (setq score (- (eww--dom-count-words node))
              noscore t))
       (t
        (setq score -1))))
    (when (consp node)
      (dolist (elem (dom-children node))
        (let ((subscore (eww--walk-readability elem callback noscore)))
          (when (and (not noscore) subscore)
            (incf score subscore)))))
    (funcall callback node score)
    score))

(defun eww-readable-dom (dom)
  "Return a readable version of DOM.
If EWW can't create a readable version, return nil instead."
  (let ((head-nodes nil)
        (best-node nil)
        (best-score most-negative-fixnum))
    (eww--walk-readability
     dom
     (lambda (node score)
       (when (consp node)
         (when (and score (> score best-score)
                    ;; We set a lower bound to how long we accept that
                    ;; the readable portion of the page is going to be.
                    (> (eww--dom-count-words node) 100))
           (setq best-score score
                 best-node node))
         ;; Keep track of any <title> and <link> tags we find to include
         ;; in the final document.  EWW uses them for various features,
         ;; like renaming the buffer or navigating to "next" and
         ;; "previous" pages.  NOTE: We could probably filter out
         ;; stylesheet <link> tags here, though it doesn't really matter
         ;; since we don't *do* anything with stylesheets...
         (when (memq (dom-tag node) '(title link base))
           ;; Copy the node, but not any of its (non-text) children.
           ;; This way, we can ensure that we don't include a node
           ;; directly in our list in addition to as a child of some
           ;; other node in the list.  This is ok for <title> and <link>
           ;; tags, but might need changed if supporting other tags.
           (let* ((inner-text (dom-inner-text node))
                  (new-node `(,(dom-tag node)
                              ,(dom-attributes node)
                              ,@(when (length> inner-text 0)
                                  (list inner-text)))))
             (push new-node head-nodes))))))
    (when (and best-node (not (eq best-node dom)))
      `(html nil
             (head nil ,@head-nodes)
             (body nil ,best-node)))))

(defun eww-score-readability (node)
  (declare (obsolete 'eww--walk-readability "31.1"))
  (eww--walk-readability
   node
   (lambda (node score)
     (when (and score (consp node))
       (dom-set-attribute node :eww-readability-score score)))))

(defun eww-highest-readability (node)
  (declare (obsolete 'eww-readable-dom "31.1"))
  (let ((result node)
	highest)
    (dolist (elem (dom-non-text-children node))
      (when (> (or (dom-attr
		    (setq highest (eww-highest-readability elem))
		    :eww-readability-score)
		   most-negative-fixnum)
	       (or (dom-attr result :eww-readability-score)
		   most-negative-fixnum))
        ;; We set a lower bound to how long we accept that the
        ;; readable portion of the page is going to be.
        (when (> (length (split-string (dom-inner-text highest))) 100)
	  (setq result highest))))
    result))

(defun eww-default-readable-p (url)
  "Return non-nil if URL should be displayed in readable mode by default.
This consults the entries in `eww-readable-urls' (which see)."
  (catch 'found
    (let (result)
      (dolist (regexp eww-readable-urls)
        (if (consp regexp)
            (setq result (cdr regexp)
                  regexp (car regexp))
          (setq result t))
        (when (string-match regexp url)
          (throw 'found result))))))

(defvar-keymap eww-mode-map
  "g" #'eww-reload             ;FIXME: revert-buffer-function instead!
  "G" #'eww
  "M-RET" #'eww-open-in-new-buffer
  "TAB" #'shr-next-link
  "C-M-i" #'shr-previous-link
  "<backtab>" #'shr-previous-link
  "<delete>" #'scroll-down-command
  "l" #'eww-back-url
  "r" #'eww-forward-url
  "n" #'eww-next-url
  "p" #'eww-previous-url
  "u" #'eww-up-url
  "t" #'eww-top-url
  "&" #'eww-browse-with-external-browser
  "d" #'eww-download
  "w" #'eww-copy-page-url
  "A" #'eww-copy-alternate-url
  "C" #'url-cookie-list
  "v" #'eww-view-source
  "R" #'eww-readable
  "H" #'eww-list-histories
  "E" #'eww-set-character-encoding
  "s" #'eww-switch-to-buffer
  "S" #'eww-list-buffers
  "F" #'eww-toggle-fonts
  "D" #'eww-toggle-paragraph-direction
  "M-C" #'eww-toggle-colors
  "M-I" #'eww-toggle-images

  "b" #'eww-add-bookmark
  "B" #'eww-list-bookmarks
  "M-n" #'eww-next-bookmark
  "M-p" #'eww-previous-bookmark

  "<mouse-8>" #'eww-back-url
  "<mouse-9>" #'eww-forward-url

  :menu '("Eww"
          ["Close browser" quit-window t]
          ["Reload" eww-reload t]
          ["Follow URL in new buffer" eww-open-in-new-buffer]
          ["Back to previous page" eww-back-url
           :active (< eww-history-position (length eww-history))]
          ["Forward to next page" eww-forward-url
           :active (> eww-history-position 1)]
          ["Browse with external browser" eww-browse-with-external-browser t]
          ["Download" eww-download t]
          ["View page source" eww-view-source]
          ["Copy page URL" eww-copy-page-url t]
          ["List histories" eww-list-histories t]
          ["Switch to buffer" eww-switch-to-buffer t]
          ["List buffers" eww-list-buffers t]
          ["Add bookmark" eww-add-bookmark t]
          ["List bookmarks" eww-list-bookmarks t]
          ["List cookies" url-cookie-list t]
          ["Toggle fonts" eww-toggle-fonts t]
          ["Toggle colors" eww-toggle-colors t]
          ["Toggle images" eww-toggle-images t]
          ["Character Encoding" eww-set-character-encoding]
          ["Toggle Paragraph Direction" eww-toggle-paragraph-direction]))

(defun eww-context-menu (menu click)
  "Populate MENU with eww commands at CLICK."
  (define-key menu [eww-separator] menu-bar-separator)
  (let ((easy-menu (make-sparse-keymap "Eww")))
    (easy-menu-define nil easy-menu nil
      '("Eww"
        ["Back to previous page" eww-back-url
	 :active (< eww-history-position (length eww-history))]
	["Forward to next page" eww-forward-url
	 :active (> eww-history-position 1)]
	["Reload" eww-reload t]))
    (dolist (item (reverse (lookup-key easy-menu [menu-bar eww])))
      (when (consp item)
        (define-key menu (vector (car item)) (cdr item)))))

  (when (or (mouse-posn-property (event-start click) 'shr-url)
            (mouse-posn-property (event-start click) 'image-url))
    (define-key menu [shr-mouse-browse-url-new-window]
      `(menu-item "Follow URL in new window" ,(if browse-url-new-window-flag
                                                  'shr-mouse-browse-url
                                                'shr-mouse-browse-url-new-window)
                  :help "Browse the URL under the mouse cursor in a new window"))
    (define-key menu [shr-mouse-browse-url]
      `(menu-item "Follow URL" ,(if browse-url-new-window-flag
                                    'shr-mouse-browse-url-new-window
                                  'shr-mouse-browse-url)
                  :help "Browse the URL under the mouse cursor")))

  menu)

(defvar eww-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (dolist (tool-bar-item
             '((quit-window . "close")
               (eww-reload . "refresh")
               (eww-back-url . "left-arrow")
               (eww-forward-url . "right-arrow")
               (eww-view-source . "show")
               (eww-copy-page-url . "copy")
               (eww-add-bookmark . "bookmark_add"))) ;; ...
      (tool-bar-local-item-from-menu
       (car tool-bar-item) (cdr tool-bar-item) map eww-mode-map))
    map)
  "Tool bar for `eww-mode'.")

(declare-function set-text-conversion-style "textconv.c")

(defun eww-check-text-conversion ()
  "Check if point is within a field and toggle text conversion.
Set `text-conversion-style' to the value `action' if it isn't
already and point is within the prompt field, or if
`text-conversion-style' is nil, so as to guarantee that
the input method functions properly for the purpose of typing
within text input fields."
  (when (and (eq major-mode 'eww-mode)
             (fboundp 'set-text-conversion-style))
    (if (eq (car-safe (get-text-property (point) 'field))
            :eww-form)
        (unless (eq text-conversion-style 'action)
          (set-text-conversion-style 'action))
      (unless (not text-conversion-style)
        (set-text-conversion-style nil)))))

;; Autoload cookie needed by desktop.el.
;;;###autoload
(define-derived-mode eww-mode special-mode
  `("eww"
    (:eval (when (plist-get eww-data :readable)
             '(:propertize ":readable"
               help-echo "Showing only human-readable text of page"))))
  "Mode for browsing the web."
  :interactive nil
  (setq-local eww-data (list :title ""))
  (setq-local browse-url-browser-function #'eww-browse-url)
  (add-hook 'context-menu-functions #'eww-context-menu 5 t)
  (setq-local eww-history nil)
  (setq-local eww-history-position 0)
  (when (boundp 'tool-bar-map)
    (setq-local tool-bar-map eww-tool-bar-map))
  ;; desktop support
  (setq-local desktop-save-buffer #'eww-desktop-misc-data)
  (setq truncate-lines t)
  ;; visual-wrap-prefix-mode support
  (setq-local adaptive-fill-function #'shr-adaptive-fill-function)
  ;; thingatpt support
  (setq-local thing-at-point-provider-alist
              (cons '(url . eww--url-at-point)
                    thing-at-point-provider-alist))
  (setq-local forward-thing-provider-alist
              (cons '(url . eww--forward-url)
                    forward-thing-provider-alist))
  (setq-local bounds-of-thing-at-point-provider-alist
              (cons '(url . eww--bounds-of-url-at-point)
                    bounds-of-thing-at-point-provider-alist))
  (setq-local bookmark-make-record-function #'eww-bookmark-make-record)
  (buffer-disable-undo)
  (setq-local shr-url-transformer #'eww--transform-url)
  ;; Also rescale images when rescaling the text.
  (add-hook 'text-scale-mode-hook #'eww--rescale-images nil t)
  (setq-local outline-search-function #'shr-outline-search
              outline-level 'shr-outline-level)
  (add-hook 'post-command-hook #'eww-check-text-conversion nil t)
  (setq buffer-read-only t)
  ;; Insertion at the first character of a field should inherit the
  ;; field's face, form and field, not the previous character's.
  (setq text-property-default-nonsticky '((face . t) (eww-form . t)
                                          (field . t))))

(declare-function imagep "image.c")
(defvar text-scale-mode)
(defvar text-scale-mode-amount)
(defvar image-scaling-factor)
(defun eww--rescale-images ()
  (let ((scaling (if text-scale-mode
                     (+ 1 (* text-scale-mode-amount 0.1))
                   1)))
    (save-excursion
      (goto-char (point-min))
      (while-let ((match (text-property-search-forward
                          'display nil
                          (lambda (_ value)
                            (and value (get-display-property
                                        nil 'image nil value))))))
        (let* ((image (cons 'image
                            (get-display-property nil 'image nil
                                                  (prop-match-value match))))
               (original-scale (or (image-property image :original-scale)
                                   (setf (image-property image :original-scale)
                                         (or (image-property image :scale)
                                             'default)))))
          (when (eq original-scale 'default)
            (setq original-scale (image-compute-scaling-factor
                                  image-scaling-factor)))
          (setf (image-property image :scale)
                (* original-scale scaling)))))))

(defun eww--url-at-point ()
  "`thing-at-point' provider function."
  (thing-at-point-for-char-property 'shr-url))

(defun eww--forward-url (backward)
  "`forward-thing' provider function."
  (forward-thing-for-char-property 'shr-url backward))

(defun eww--bounds-of-url-at-point ()
  "`bounds-of-thing-at-point' provider function."
  (bounds-of-thing-at-point-for-char-property 'shr-url))

;;;###autoload
(defun eww-browse-url (url &optional new-window)
  "Ask the EWW browser to load URL.

Interactively, if the variable `browse-url-new-window-flag' is non-nil,
loads the document in a new buffer tab on the window tab-line.  A non-nil
prefix argument reverses the effect of `browse-url-new-window-flag'.

If `tab-bar-mode' is enabled, then whenever a document would
otherwise be loaded in a new buffer, it is loaded in a new tab
in the tab-bar on an existing frame.  See more options in
`eww-browse-url-new-window-is-tab'.

Non-interactively, this uses the optional second argument NEW-WINDOW
instead of `browse-url-new-window-flag'."
  (when new-window
    (when (or (eq eww-browse-url-new-window-is-tab t)
              (and (eq eww-browse-url-new-window-is-tab 'tab-bar)
                   tab-bar-mode))
      (let ((tab-bar-new-tab-choice t))
        (tab-new)))
    (pop-to-buffer-same-window
     (generate-new-buffer
      (format "*eww-%s*" (url-host (url-generic-parse-url
                                    (eww--dwim-expand-url url))))))
    (eww-mode))
  (let ((url-allow-non-local-files t))
    (eww url)))

(function-put 'eww-browse-url 'browse-url-browser-kind 'internal)

(defun eww-back-url ()
  "Go to the previously displayed page."
  (interactive nil eww-mode)
  (when (>= eww-history-position (length eww-history))
    (user-error "No previous page"))
  (if (eww-save-history)
      ;; We were at the latest page (which was just added to the
      ;; history), so go back two entries.
      (setq eww-history-position 2)
    (setq eww-history-position (1+ eww-history-position)))
  (eww-restore-history (elt eww-history (1- eww-history-position))))

(defun eww-forward-url ()
  "Go to the next displayed page."
  (interactive nil eww-mode)
  (when (<= eww-history-position 1)
    (user-error "No next page"))
  (eww-save-history)
  (setq eww-history-position (1- eww-history-position))
  (eww-restore-history (elt eww-history (1- eww-history-position))))

(defun eww-restore-history (elem)
  (let ((inhibit-read-only t)
	(text (plist-get elem :text)))
    (setq eww-data elem)
    (if (null text)
	(eww-reload)			; FIXME: restore :point?
      (erase-buffer)
      (insert text)
      (goto-char (plist-get elem :point))
      ;; Make buffer listings more informative.
      (setq list-buffers-directory (plist-get elem :url))
      (eww--after-page-change))))

(defun eww-next-url ()
  "Go to the page marked `next'.
A page is marked `next' if rel=\"next\" appears in a <link>
or <a> tag."
  (interactive nil eww-mode)
  (if (plist-get eww-data :next)
      (eww-browse-url (shr-expand-url (plist-get eww-data :next)
				      (plist-get eww-data :url)))
    (user-error "No `next' on this page")))

(defun eww-previous-url ()
  "Go to the page marked `previous'.
A page is marked `previous' if rel=\"previous\" appears in a <link>
or <a> tag."
  (interactive nil eww-mode)
  (if (plist-get eww-data :previous)
      (eww-browse-url (shr-expand-url (plist-get eww-data :previous)
				      (plist-get eww-data :url)))
    (user-error "No `previous' on this page")))

(defun eww-up-url ()
  "Go to the page marked `up'.
A page is marked `up' if rel=\"up\" appears in a <link>
or <a> tag."
  (interactive nil eww-mode)
  (if (plist-get eww-data :up)
      (eww-browse-url (shr-expand-url (plist-get eww-data :up)
				      (plist-get eww-data :url)))
    (user-error "No `up' on this page")))

(defun eww-top-url ()
  "Go to the page marked `top'.
A page is marked `top' if rel=\"start\", rel=\"home\", or rel=\"contents\"
appears in a <link> or <a> tag."
  (interactive nil eww-mode)
  (let ((best-url (or (plist-get eww-data :start)
		      (plist-get eww-data :contents)
		      (plist-get eww-data :home))))
    (if best-url
	(eww-browse-url (shr-expand-url best-url (plist-get eww-data :url)))
      (user-error "No `top' for this page"))))

(defun eww-reload (&optional local encode)
  "Reload the current page.
If LOCAL is non-nil (interactively, the command was invoked with
a prefix argument), don't reload the page from the network, but
just re-display the HTML already fetched."
  (interactive "P" eww-mode)
  (let ((url (plist-get eww-data :url)))
    (if local
	(if (null (plist-get eww-data :dom))
	    (error "No current HTML data")
	  (eww-display-document (plist-get eww-data :dom) (point)))
      (let ((parsed (url-generic-parse-url url)))
        (if (equal (url-type parsed) "file")
            ;; Use Tramp instead of url.el for files (since url.el
            ;; doesn't work well with Tramp files).
            (let ((eww-buffer (current-buffer)))
              (with-current-buffer (eww--file-buffer (url-filename parsed))
                (eww-render nil url nil eww-buffer)))
          (let ((url-mime-accept-string eww-accept-content-types))
            (eww-retrieve url #'eww-render
		          (list url (point) (current-buffer) encode))))))))

;; Form support.

(defvar eww-form nil)

(defvar-keymap eww-submit-map
  "RET" #'eww-submit
  "C-c C-c" #'eww-submit
  "<mouse-2>" #'eww-submit)

(defvar-keymap eww-submit-file
  "RET" #'eww-select-file
  "C-c C-c" #'eww-submit
  "<mouse-2>" #'eww-select-file)

(defvar-keymap eww-checkbox-map
  "SPC" #'eww-toggle-checkbox
  "RET" #'eww-toggle-checkbox
  "C-c C-c" #'eww-submit
  "<mouse-2>" #'eww-toggle-checkbox)

(defvar-keymap eww-text-map
  :full t :parent text-mode-map
  "RET" #'eww-submit
  "C-a" #'eww-beginning-of-text
  "C-c C-c" #'eww-submit
  "C-e" #'eww-end-of-text
  "TAB" #'shr-next-link
  "M-TAB" #'shr-previous-link
  "<backtab>" #'shr-previous-link)

(defvar-keymap eww-textarea-map
  :full t :parent text-mode-map
  "RET" #'forward-line
  "C-c C-c" #'eww-submit
  "TAB" #'shr-next-link
  "M-TAB" #'shr-previous-link
  "<backtab>" #'shr-previous-link)

(defvar-keymap eww-select-map
  :doc "Map for select buttons"
  "RET" #'eww-change-select
  "<follow-link>" 'mouse-face
  "<mouse-2>" #'eww-change-select
  "C-c C-c" #'eww-submit)

(defun eww-beginning-of-text ()
  "Move to the start of the input field."
  (interactive nil eww-mode)
  (goto-char (eww-beginning-of-field)))

(defun eww-end-of-text ()
  "Move to the end of the text in the input field."
  (interactive nil eww-mode)
  (goto-char (eww-end-of-field))
  (let ((start (eww-beginning-of-field)))
    (while (and (equal (following-char) ? )
		(> (point) start))
      (forward-char -1))
    (when (> (point) start)
      (forward-char 1))))

(defun eww-beginning-of-field ()
  (cond
   ((bobp)
    (point))
   ((not (eq (get-text-property (point) 'eww-form)
	     (get-text-property (1- (point)) 'eww-form)))
    (point))
   (t
    (previous-single-property-change
     (point) 'eww-form nil (point-min)))))

(defun eww-end-of-field ()
  (1- (next-single-property-change
       (point) 'eww-form nil (point-max))))

(defun eww-tag-form (dom)
  (let ((eww-form (list (cons :method (dom-attr dom 'method))
			(cons :action (dom-attr dom 'action))))
	(start (point)))
    (insert "\n")
    (shr-ensure-paragraph)
    (shr-generic dom)
    (unless (bolp)
      (insert "\n"))
    (insert "\n")
    (when (> (point) start)
      (put-text-property start (1+ start)
			 'eww-form eww-form))))

(defun eww-form-submit (dom)
  (let ((start (point))
	(value (dom-attr dom 'value)))
    (if (null value)
        (shr-generic dom)
      (insert value))
    ;; If the contents of the <button>...</button> turns out to be
    ;; empty, or the value was blank, default to this:
    (when (= (point) start)
      (insert "Submit"))
    (add-face-text-property start (point) 'eww-form-submit)
    (put-text-property start (point) 'eww-form
		       (list :eww-form eww-form
			     :value value
			     :type "submit"
			     :name (dom-attr dom 'name)))
    (put-text-property start (point) 'keymap eww-submit-map)
    ;; Pretend to touch-screen.el that this is a button.
    (put-text-property start (point) 'button t)
    (insert " ")))

(defun eww-form-checkbox (dom)
  (let ((start (point)))
    (if (dom-attr dom 'checked)
	(insert eww-form-checkbox-selected-symbol)
      (insert eww-form-checkbox-symbol))
    (add-face-text-property start (point) 'eww-form-checkbox)
    (put-text-property start (point) 'eww-form
		       (list :eww-form eww-form
			     :value (dom-attr dom 'value)
			     :type (downcase (dom-attr dom 'type))
			     :checked (dom-attr dom 'checked)
			     :name (dom-attr dom 'name)))
    (put-text-property start (point) 'keymap eww-checkbox-map)
    ;; Pretend to touch-screen.el that this is a button.
    (put-text-property start (point) 'button t)
    (insert " ")))

(defun eww-form-file (dom)
  (let ((start (point))
	(value (dom-attr dom 'value)))
    (setq value
	  (if (zerop (length value))
	      " No file selected"
	    value))
    (insert "Browse")
    (add-face-text-property start (point) 'eww-form-file)
    (insert value)
    (put-text-property start (point) 'eww-form
		       (list :eww-form eww-form
			     :value (dom-attr dom 'value)
			     :type (downcase (dom-attr dom 'type))
			     :name (dom-attr dom 'name)))
    (put-text-property start (point) 'keymap eww-submit-file)
    ;; Pretend to touch-screen.el that this is a button.
    (put-text-property start (point) 'button t)
    (insert " ")))

(defun eww-select-file (&optional event)
  "Change the value of the upload file menu under point.
EVENT, if non-nil, is the mouse event that preceded this command.
Interactively, EVENT is the value of `last-nonmenu-event'."
  (interactive (list last-nonmenu-event) eww-mode)
  (when (and event (setq event (event-start event)))
    (goto-char (posn-point event)))
  (let*  ((input (get-text-property (point) 'eww-form)))
    (let ((filename
	   (let ((insert-default-directory t))
	     (read-file-name "filename:  "))))
      (eww-update-field filename (length "Browse"))
              (plist-put input :filename filename))))

(defun eww-form-text (dom)
  (let ((start (point))
	(type (downcase (or (dom-attr dom 'type) "text")))
	(value (or (dom-attr dom 'value) ""))
	(width (string-to-number (or (dom-attr dom 'size) "40")))
        (readonly-property (if (or (dom-attr dom 'disabled)
				   (dom-attr dom 'readonly))
                               'read-only
                             'inhibit-read-only))
        form)
    (setq form (list :eww-form eww-form
                     :value value
                     :type type
                     :name (dom-attr dom 'name)))
    (insert value)
    (when (< (length value) width)
      (insert (make-string (- width (length value)) ? )))
    (put-text-property start (point) 'face 'eww-form-text)
    (put-text-property start (point) 'inhibit-read-only t)
    (put-text-property start (point) 'local-map eww-text-map)
    (put-text-property start (point) readonly-property t)
    (put-text-property start (point) 'eww-form form)
    (put-text-property start (point) 'field form)
    (put-text-property start (point) 'front-sticky t)
    (insert " ")))

(defconst eww-text-input-types '("text" "password" "textarea"
                                 "color" "date" "datetime" "datetime-local"
                                 "email" "month" "number" "search" "tel"
                                 "time" "url" "week")
  "List of input types which represent a text input.
See URL `https://developer.mozilla.org/en-US/docs/Web/HTML/Element/Input'.")

(defun eww--track-changes (tracker-id)
  (track-changes-fetch
   tracker-id
   (lambda (beg end len)
     (eww--process-text-input beg end len)
     ;; Disregard our own changes.
     (track-changes-fetch tracker-id #'ignore))))

(defun eww--process-text-input (beg end replace-length)
  (when-let* ((_ (integerp replace-length))
              (pos end)
              (form (or (get-text-property pos 'eww-form)
                        (progn
                          (setq pos (max (point-min) (1- beg)))
                          (get-text-property pos 'eww-form)))))
    (let* ((properties (text-properties-at pos))
           (buffer-undo-list t)
	   (inhibit-read-only t)
	   (length (- end beg replace-length))
	   (type (plist-get form :type)))
      (when (member type eww-text-input-types)
	;; Make sure the new text has the right properties, which also
	;; integrates the new text into the "current field".
	(set-text-properties beg end properties)
	;; FIXME: This tries to preserve the "length" of the input field,
        ;; but we should try to preserve the *width* instead.
        ;; FIXME: Maybe instead of inserting/deleting spaces, we should
        ;; have a single stretch-space character at the end.
	(cond
	 ((> length 0)
	  ;; Delete some space at the end.
	  (save-excursion
	    (goto-char
	     (if (equal type "textarea")
		 (1- (line-end-position))
	       (eww-end-of-field)))
	    (while (and (> length 0)
			(eq (char-after (1- (point))) ? ))
	      (delete-region (1- (point)) (point))
              (decf length))))
	 ((< length 0)
	  ;; Add padding.
	  (save-excursion
	    (goto-char pos)
	    (let* ((field-length (- (eww-end-of-field)
                                    (eww-beginning-of-field)))
                   (ideal-length (cdr (assq :length form))))
              ;; FIXME: This test isn't right for multiline fields.
              (when (or (null ideal-length) (> ideal-length field-length))
	        (goto-char
		 (if (equal type "textarea")
		     (1- (line-end-position))
		   (1+ (eww-end-of-field))))
		(let ((start (point)))
	          (insert (make-string (min (abs length)
	                                    (- ideal-length field-length))
	                               ? ))
	          (set-text-properties start (point) properties)))))))
	(let ((value (buffer-substring-no-properties
		      (eww-beginning-of-field)
		      (eww-end-of-field))))
	  (when (string-match " +\\'" value)
	    (setq value (substring value 0 (match-beginning 0))))
	  (plist-put form :value value)
          (plist-put form :type type)
	  (when (equal type "password")
	    ;; Display passwords as asterisks.
	    (let ((start (eww-beginning-of-field)))
	      (put-text-property
               start (+ start (length value))
               'display (make-string (length value) ?*)))))))))

(defun eww-tag-textarea (dom)
  (let ((value (or (dom-inner-text dom) ""))
	(lines (string-to-number (or (dom-attr dom 'rows) "10")))
	(width (string-to-number (or (dom-attr dom 'cols) "10")))
	start end form)
    (shr-ensure-newline)
    (setq start (point))
    (insert value)
    (shr-ensure-newline)
    (when (< (count-lines start (point)) lines)
      (dotimes (_ (- lines (count-lines start (point))))
	(insert "\n")))
    (setq end (point-marker))
    (goto-char start)
    (while (< (point) end)
      (end-of-line)
      (let ((pad (- width (- (point) (line-beginning-position)))))
	(when (> pad 0)
	  (insert (make-string pad ? ))))
      (add-face-text-property (line-beginning-position)
			      (point) 'eww-form-textarea)
      (put-text-property (line-beginning-position) (point) 'inhibit-read-only t)
      (put-text-property (line-beginning-position) (point)
			 'local-map eww-textarea-map)
      (forward-line 1))
    (setq form (list :eww-form eww-form
		     :value value
		     :type "textarea"
		     :name (dom-attr dom 'name)))
    (put-text-property start (point) 'eww-form form)
    (put-text-property start (point) 'front-sticky t)
    (put-text-property start (point) 'field form)
    (put-text-property start (1+ start) 'shr-tab-stop t)))

(defun eww-tag-input (dom)
  (let ((type (downcase (or (dom-attr dom 'type) "text")))
	(start (point)))
    (cond
     ((or (equal type "checkbox")
	  (equal type "radio"))
      (eww-form-checkbox dom))
     ((equal type "file")
      (eww-form-file dom))
     ((equal type "submit")
      (eww-form-submit dom))
     ((equal type "hidden")
      (let ((form eww-form)
	    (name (dom-attr dom 'name)))
	;; Don't add <input type=hidden> elements repeatedly.
	(while (and form
		    (or (not (consp (car form)))
			(not (eq (caar form) 'hidden))
			(not (equal (plist-get (cdr (car form)) :name)
				    name))))
	  (setq form (cdr form)))
	(unless form
	  (nconc eww-form (list
			   (list 'hidden
				 :name name
				 :value (or (dom-attr dom 'value) "")))))))
     (t
      (eww-form-text dom)))
    (unless (= start (point))
      (put-text-property start (1+ start) 'help-echo "Input field")
      ;; Mark this as an element we can TAB to.
      (put-text-property start (1+ start) 'shr-tab-stop t))))

(defun eww-tag-select (dom)
  (shr-ensure-paragraph)
  (let ((menu (list :name (dom-attr dom 'name)
		    :eww-form eww-form))
	(options nil)
	(start (point))
	(max 0))
    (dolist (elem (dom-by-tag dom 'option))
      (when (dom-attr elem 'selected)
	(nconc menu (list :value (dom-attr elem 'value))))
      (let ((display (dom-inner-text elem)))
	(setq max (max max (length display)))
	(push (list 'item
		    :value (dom-attr elem 'value)
		    :display display)
	      options)))
    (when options
      (setq options (nreverse options))
      ;; If we have no selected values, default to the first value.
      (unless (plist-get menu :value)
	(nconc menu (list :value (nth 2 (car options)))))
      (nconc menu options)
      (let ((selected (eww-select-display menu)))
	(insert selected
		(make-string (- max (length selected)) ? )))
      (put-text-property start (point) 'eww-form menu)
      (add-face-text-property start (point) 'eww-form-select)
      (put-text-property start (point) 'keymap eww-select-map)
      ;; Pretend to touch-screen.el that this is a button.
      (put-text-property start (point) 'button t)
      (unless (= start (point))
       (put-text-property start (1+ start) 'help-echo "select field")
       (put-text-property start (1+ start) 'shr-tab-stop t))
      (shr-ensure-paragraph))))

(defun eww-select-display (select)
  (let ((value (plist-get select :value))
	display)
    (dolist (elem select)
      (when (and (consp elem)
		 (eq (car elem) 'item)
		 (equal value (plist-get (cdr elem) :value)))
	(setq display (plist-get (cdr elem) :display))))
    display))

(defun eww--form-items (form)
  (cl-loop for elem in form
           when (and (consp elem)
                     (eq (car elem) 'item))
           collect (cdr elem)))

(defun eww-change-select (event)
  "Change the value of the select drop-down menu under point."
  (interactive
   (list last-nonmenu-event)
   eww-mode)
  (mouse-set-point event)
  (let ((input (get-text-property (point) 'eww-form)))
    (popup-menu
     (cons
      "Change Value"
      (mapcar
       (lambda (elem)
         (vector (plist-get elem :display)
                 (lambda ()
                   (interactive)
                   (plist-put input :value (plist-get elem :value))
                   (goto-char (eww-update-field (plist-get elem :display))))
                 t))
       (eww--form-items input)))
     event)))

(defun eww-update-field (string &optional offset)
  (unless offset
    (setq offset 0))
  (let ((properties (text-properties-at (point)))
	(start (+ (eww-beginning-of-field) offset))
	(current-end (1+ (eww-end-of-field)))
	(new-end (+ (eww-beginning-of-field) (length string)))
        (inhibit-read-only t))
    (delete-region start current-end)
    (forward-char offset)
    (insert string
	    (make-string (- (- (+ new-end offset) start) (length string)) ? ))
    (when (= 0 offset)
      (set-text-properties start new-end properties))
    start))

(defun eww-toggle-checkbox (&optional event)
  "Toggle the value of the checkbox under point.
EVENT, if non-nil, is the mouse event that preceded this command.
Interactively, EVENT is the value of `last-nonmenu-event'."
  (interactive (list last-nonmenu-event) eww-mode)
  (when (and event (setq event (event-start event)))
    (goto-char (posn-point event)))
  (let* ((input (get-text-property (point) 'eww-form))
	 (type (plist-get input :type)))
    (if (equal type "checkbox")
	(goto-char
	 (1+
	  (if (plist-get input :checked)
	      (progn
		(plist-put input :checked nil)
		(eww-update-field eww-form-checkbox-symbol))
	    (plist-put input :checked t)
	    (eww-update-field eww-form-checkbox-selected-symbol))))
      ;; Radio button.  Switch all other buttons off.
      (let ((name (plist-get input :name)))
	(save-excursion
	  (dolist (elem (eww-inputs (plist-get input :eww-form)))
	    (when (equal (plist-get (cdr elem) :name) name)
	      (goto-char (car elem))
	      (if (not (eq (cdr elem) input))
		  (progn
		    (plist-put (cdr elem) :checked nil)
		    (eww-update-field eww-form-checkbox-symbol))
		(plist-put (cdr elem) :checked t)
		(eww-update-field eww-form-checkbox-selected-symbol)))))
	(forward-char 1)))))

(defun eww-inputs (form)
  (let ((start (point-min))
	(inputs nil))
    (while (and start
		(< start (point-max)))
      (when (or (get-text-property start 'eww-form)
		(setq start (next-single-property-change start 'eww-form)))
	(when (eq (plist-get (get-text-property start 'eww-form) :eww-form)
		  form)
	  (push (cons start (get-text-property start 'eww-form))
		inputs))
	(setq start (next-single-property-change start 'eww-form))))
    (nreverse inputs)))

(defun eww-size-text-inputs ()
  (let ((start (point-min)))
    (while (and start
		(< start (point-max)))
      (when (or (get-text-property start 'eww-form)
		(setq start (next-single-property-change start 'eww-form)))
	(let ((props (get-text-property start 'eww-form))
              (beg start))
          (setq start (next-single-property-change
                       start 'eww-form nil (point-max)))
          (nconc props (list (cons :length (- start beg)))))))))

(defun eww-input-value (input)
  (let ((type (plist-get input :type))
	(value (plist-get input :value)))
    (cond
     ((equal type "textarea")
      (with-temp-buffer
	(insert value)
	(goto-char (point-min))
	(while (re-search-forward "^ +\n\\| +$" nil t)
	  (replace-match "" t t))
	(buffer-string)))
     (t
      (if (string-match " +\\'" value)
	  (substring value 0 (match-beginning 0))
	value)))))

(defun eww-submit (&optional event)
  "Submit the form under point or EVENT.
EVENT, if non-nil, is the mouse event that preceded this command.
Interactively, EVENT is the value of `last-nonmenu-event'."
  (interactive (list last-nonmenu-event) eww-mode)
  (when (and event (setq event (event-start event)))
    (goto-char (posn-point event)))
  (let* ((this-input (get-text-property (point) 'eww-form))
	 (form (plist-get this-input :eww-form))
	 values next-submit)
    (dolist (elem (sort (eww-inputs form)
			(lambda (o1 o2)
			  (< (car o1) (car o2)))))
      (let* ((input (cdr elem))
	     (input-start (car elem))
	     (name (plist-get input :name)))
	(when name
	  (cond
	   ((member (plist-get input :type) '("checkbox" "radio"))
	    (when (plist-get input :checked)
              (push (cons name (or (plist-get input :value) "on"))
		    values)))
	   ((equal (plist-get input :type) "file")
            (when-let* ((file (plist-get input :filename)))
              (push (list "file"
                          (cons "filedata"
                                (with-temp-buffer
                                  (insert-file-contents file)
                                  (buffer-string)))
                          (cons "name" name)
                          ;; RFC 2183 declares that recipients should
                          ;; only respect the basename of the filename
                          ;; parameter, and the leading directories
                          ;; might divulge private information, so we
                          ;; only send the basename in our request.
                          (cons "filename" (file-name-nondirectory file)))
                    values)))
	   ((equal (plist-get input :type) "submit")
	    ;; We want the values from buttons if we hit a button if
	    ;; we hit enter on it, or if it's the first button after
	    ;; the field we did hit return on.
	    (when (or (eq input this-input)
		      (and (not (eq input this-input))
			   (null next-submit)
			   (> input-start (point))))
	      (setq next-submit t)
	      (push (cons name (plist-get input :value))
		    values)))
	   (t
	    (push (cons name (eww-input-value input))
		  values))))))
    (dolist (elem form)
      (when (and (consp elem)
		 (eq (car elem) 'hidden))
	(push (cons (plist-get (cdr elem) :name)
		    (or (plist-get (cdr elem) :value) ""))
	      values)))
    (if (and (stringp (cdr (assq :method form)))
	     (equal (downcase (cdr (assq :method form))) "post"))
	(let ((mtype))
	  (dolist (x values mtype)
	    (if (equal (car x) "file")
		(progn
		  (setq mtype "multipart/form-data"))))
	  (cond ((equal mtype "multipart/form-data")
		 (let ((boundary (mml-compute-boundary '())))
		   (let ((url-request-method "POST")
			 (url-request-extra-headers
			  (list (cons "Content-Type"
				      (concat "multipart/form-data; boundary="
					      boundary))))
			 (url-request-data
			  (mm-url-encode-multipart-form-data values boundary)))
		     (eww-browse-url (shr-expand-url
				      (cdr (assq :action form))
				      (plist-get eww-data :url))))))
		(t
		 (let ((url-request-method "POST")
		       (url-request-extra-headers
			'(("Content-Type" .
			   "application/x-www-form-urlencoded")))
		       (url-request-data
			(mm-url-encode-www-form-urlencoded values)))
		   (eww-browse-url (shr-expand-url
				    (cdr (assq :action form))
				    (plist-get eww-data :url)))))))
      (eww-browse-url
       (concat
	(if (cdr (assq :action form))
	    (shr-expand-url (cdr (assq :action form)) (plist-get eww-data :url))
	  (plist-get eww-data :url))
	"?"
	(mm-url-encode-www-form-urlencoded values))))))

(defun eww-browse-with-external-browser (&optional url)
  "Browse the current URL with an external browser.
Use `browse-url-secondary-browser-function' if it is an external
browser, otherwise use `browse-url-with-browser-kind' to open an
external browser."
  (interactive nil eww-mode)
  (setq url (or url (plist-get eww-data :url)))
  (if (eq 'external (browse-url--browser-kind
                     browse-url-secondary-browser-function url))
      (let ((browse-url-browser-function browse-url-secondary-browser-function))
        (browse-url url))
    (browse-url-with-browser-kind 'external url)))

(defun eww-remove-tracking (url)
  "Remove the commong utm_ tracking cookies from URLs."
  (replace-regexp-in-string ".utm_.*" "" url))

(defun eww--transform-url (url)
  "Apply `eww-url-transformers'."
  (when url
    (dolist (func eww-url-transformers)
      (setq url (funcall func url)))
    url))

(defun eww-follow-link (&optional external mouse-event)
  "Browse the URL at point, optionally the position of MOUSE-EVENT.

EXTERNAL is the prefix argument.  If called interactively with
\\[universal-argument] pressed once, browse the URL using
`eww-browse-with-external-browser'.  If called interactively, with
\\[universal-argument] pressed twice, browse in new buffer."
  (interactive
   (list current-prefix-arg last-nonmenu-event)
   eww-mode)
  (mouse-set-point mouse-event)
  (let* ((orig-url (get-text-property (point) 'shr-url))
         (url (eww--transform-url orig-url))
         target)
    (cond
     ((not url)
      (message "No link under point"))
     ((string-match-p eww-use-browse-url url)
      ;; This respects the user options `browse-url-handlers'
      ;; and `browse-url-mailto-function'.
      (browse-url url))
     ((and (consp external) (<= (car external) 4))
      (eww-browse-with-external-browser url)
      (shr--blink-link))
     ;; This is a #target url in the same page as the current one.
     ((and (setq target (url-target (url-generic-parse-url url)))
	   (eww-same-page-p url (plist-get eww-data :url)))
      (let ((old-data eww-data)
            (point (point)))
	(eww-save-history)
        (eww--before-browse)
        ;; Copy previous `eww-data', since everything but the URL will
        ;; stay the same, and we don't re-render the document.
        (setq eww-data (copy-sequence old-data))
	(plist-put eww-data :url url)
        (goto-char (point-min))
        (if-let* ((match (text-property-search-forward 'shr-target-id target #'member)))
            (goto-char (prop-match-beginning match))
          (goto-char (if (equal target "top")
                         (point-min)
                       point)))))
     (t
      (eww-browse-url orig-url external)))))

(defun eww-same-page-p (url1 url2)
  "Return non-nil if URL1 and URL2 represent the same page.
Differences in #targets are ignored."
  (let ((obj1 (url-generic-parse-url url1))
	(obj2 (url-generic-parse-url url2)))
    (setf (url-target obj1) nil)
    (setf (url-target obj2) nil)
    (equal (url-recreate-url obj1) (url-recreate-url obj2))))

(defun eww-copy-page-url ()
  "Copy the URL of the current page into the kill ring."
  (interactive nil eww-mode)
  (message "%s" (plist-get eww-data :url))
  (kill-new (plist-get eww-data :url)))

(defun eww-download ()
  "Download a Web page to `eww-download-directory'.
Use link at point if there is one, else the current page's URL.
This command downloads the page to the download directory, under
a file name generated from the last portion of the page's URL,
after the last slash.  (If URL ends in a slash, the page will be
saved under the name \"!\".)
If there's already a file by that name in the download directory,
this command will modify the name to make it unique.
The command shows in the echo-area the actual file name where the
page was saved."
  (interactive nil eww-mode)
  (let ((dir (if (stringp eww-download-directory)
                 eww-download-directory
               (funcall eww-download-directory))))
    (access-file dir "Download failed")
    (let ((url (or (get-text-property (point) 'shr-url)
                   (eww-current-url))))
      (if (not url)
          (message "No URL under point")
        (url-retrieve url #'eww-download-callback (list url dir))))))

(defun eww-download-callback (status url dir)
  (unless (plist-get status :error)
    (let* ((obj (url-generic-parse-url url))
           (path (directory-file-name (car (url-path-and-query obj))))
           (file (eww-make-unique-file-name
                  (eww-decode-url-file-name (file-name-nondirectory path))
                  dir)))
      (goto-char (point-min))
      (re-search-forward "\r?\n\r?\n")
      (let ((coding-system-for-write 'no-conversion))
        (write-region (point) (point-max) file))
      (message "Saved %s" file))))

(defun eww-decode-url-file-name (string)
  (let* ((binary (url-unhex-string string))
         (decoded
          (decode-coding-string
           binary
           ;; Possibly set by `universal-coding-system-argument'.
           (or coding-system-for-read
               ;; RFC 3986 says that %AB stuff is utf-8.
               (if (equal (decode-coding-string binary 'utf-8)
                          '(unicode))
                   'utf-8
                 ;; But perhaps not.
                 (car (detect-coding-string binary))))))
         (encodes (find-coding-systems-string decoded)))
    (if (or (equal encodes '(undecided))
            (memq (coding-system-base (or file-name-coding-system
                                          default-file-name-coding-system))
                  encodes))
        decoded
      ;; If we can't encode the decoded file name (due to language
      ;; environment settings), then we return the original, hexified
      ;; string.
      string)))

(defun eww-make-unique-file-name (file directory)
  (cond
   ((zerop (length file))
    (setq file "!"))
   ((string-match "\\`[.]" file)
    (setq file (concat "!" file))))
  (let ((count 1)
        (stem file)
        (suffix ""))
    (when (string-match "\\`\\(.*\\)\\([.][^.]+\\)" file)
      (setq stem (match-string 1 file)
            suffix (match-string 2 file)))
    (while (file-exists-p (expand-file-name file directory))
      (setq file (format "%s(%d)%s" stem count suffix))
      (setq count (1+ count)))
    (expand-file-name file directory)))

(defun eww-set-character-encoding (charset)
  "Set character encoding to CHARSET.
If CHARSET is nil then use UTF-8."
  (interactive "zUse character set (default `utf-8'): " eww-mode)
  (if (null charset)
      (eww-reload nil 'utf-8)
    (eww-reload nil charset)))

(defun eww--buffer-p (buf)
  (provided-mode-derived-p (buffer-local-value 'major-mode buf)
                           'eww-mode))

(defun eww-switch-to-buffer ()
  "Prompt for an EWW buffer to display in the selected window.
If no such buffer exist, fallback to calling `eww'."
  (interactive nil eww-mode)
  (let ((list (seq-filter
               (lambda (buf)
                 (and (eww--buffer-p buf) (not (eq buf (current-buffer)))))
               (buffer-list))))
    (if list
        (pop-to-buffer-same-window
         (if (length= list 1)
             (car list)
           (completing-read "Switch to EWW buffer: "
                            (completion-table-with-metadata
                             (mapcar #'buffer-name list)
                             `((category . buffer)
                               (annotation-function
                                . ,(lambda (buf)
                                     (with-current-buffer buf
                                       (format " %s" (eww-current-url)))))))
                            nil t nil nil (car-safe list))))
      (call-interactively #'eww))))

(defun eww-toggle-fonts ()
  "Toggle whether to use monospaced or font-enabled layouts."
  (interactive nil eww-mode)
  (setq shr-use-fonts (not shr-use-fonts))
  (eww-reload)
  (message "Proportional fonts are now %s"
           (if shr-use-fonts "on" "off")))

(defun eww-toggle-colors ()
  "Toggle whether to use HTML-specified colors or not."
  (interactive nil eww-mode)
  (message "Colors are now %s"
	   (if (setq shr-use-colors (not shr-use-colors))
	       "on"
	     "off"))
  (eww-reload))

(defun eww-toggle-images ()
  "Toggle whether or not to display images."
  (interactive nil eww-mode)
  (setq shr-inhibit-images (not shr-inhibit-images))
  (eww-reload)
  (message "Images are now %s"
           (if shr-inhibit-images "off" "on")))

;;; Bookmarks code

(defvar eww-bookmarks nil)

(defun eww-add-bookmark ()
  "Bookmark the current page."
  (interactive nil eww-mode)
  (eww-read-bookmarks)
  (dolist (bookmark eww-bookmarks)
    (when (equal (plist-get eww-data :url) (plist-get bookmark :url))
      (user-error "Already bookmarked")))
  (when (y-or-n-p "Bookmark this page?")
    (let ((title (replace-regexp-in-string "[\n\t\r]" " "
					   (plist-get eww-data :title))))
      (setq title (replace-regexp-in-string "\\` +\\| +\\'" "" title))
      (push (list :url (plist-get eww-data :url)
		  :title title
		  :time (current-time-string))
	    eww-bookmarks))
    (eww-write-bookmarks)
    (message "Bookmarked %s (%s)" (plist-get eww-data :url)
	     (plist-get eww-data :title))))

(defun eww-write-bookmarks ()
  "Write the bookmarks in `eww-bookmarks-directory'."
  (with-temp-file (expand-file-name "eww-bookmarks" eww-bookmarks-directory)
    (insert ";; Auto-generated file; don't edit -*- mode: lisp-data -*-\n")
    (let ((print-length nil)
          (print-level nil))
      (pp eww-bookmarks (current-buffer)))))

(defun eww-read-bookmarks (&optional error-out)
  "Read bookmarks from `eww-bookmarks'.
If ERROR-OUT, signal user-error if there are no bookmarks."
  (let ((file (expand-file-name "eww-bookmarks" eww-bookmarks-directory)))
    (setq eww-bookmarks
	  (unless (zerop (or (file-attribute-size (file-attributes file)) 0))
	    (with-temp-buffer
	      (insert-file-contents file)
	      (read (current-buffer)))))
    (when (and error-out (not eww-bookmarks))
      (user-error "No bookmarks are defined"))))

;;;###autoload
(defun eww-list-bookmarks (&optional build-only)
  "Display the eww bookmarks.
Optional argument BUILD-ONLY, when non-nil, means to build the buffer
without popping it."
  (interactive)
  (eww-read-bookmarks t)
  (with-current-buffer (get-buffer-create "*eww bookmarks*")
    (eww-bookmark-mode)
    (eww--bookmark-prepare))
  (unless build-only
    (pop-to-buffer "*eww bookmarks*")))

(defun eww--bookmark-prepare ()
  "Display a table with the list of eww bookmarks.
Will remove all buffer contents first."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (make-vtable
     :columns '((:name "Order" :min-width 6)
                (:name "Title" :min-width "25%" :max-width "50%")
                (:name "URL"))
     :objects-function #'eww--bookmark-format-data
     ;; use fixed-font face
     :face 'default
     :sort-by '((0 . ascend))
     )))

(defun eww--bookmark-format-data ()
  "Format `eww-bookmarks' for use in a vtable.
The data is returned as a list (order title url bookmark), for use
in of `eww-bookmark-mode'.  Order stars counting from 1."
  (seq-map-indexed (lambda (bm index)
                     (list
                      (+ 1 index)
                      (plist-get bm :title)
                      (plist-get bm :url)
                      bm))
                   eww-bookmarks))

(defvar eww-bookmark-kill-ring nil)

(defun eww--bookmark-check-order-sort ()
  "Signal a user error unless the bookmark vtable is sorted by asc order."
  ;; vtables sort respecting the previous sort column. As long as
  ;; "order" was last, the kill/yank operations will make sense, no
  ;; matter what sort was used before.
  (when-let* ((the-table (vtable-current-table))
              (last-sort-pair (car (last (vtable-sort-by the-table)))))
    (unless (and (= 0 (car last-sort-pair))
                 (eq 'ascend (cdr last-sort-pair)))
      (user-error
       "Can't kill/yank bookmarks unless the table is sorted by ascending Order"))))

(defun eww-bookmark-kill ()
  "Kill the current bookmark."
  (interactive nil eww-bookmark-mode)
  (eww--bookmark-check-order-sort)
  (let ((bookmark-at-point (nth 3 (vtable-current-object)))
	(position (point)))
    (unless bookmark-at-point
      (user-error "No bookmark on the current line"))
    (forward-line 1)
    (push bookmark-at-point eww-bookmark-kill-ring)
    (setq eww-bookmarks (delq bookmark-at-point eww-bookmarks))
    (eww-write-bookmarks)
    (when (= (point) (point-max))
      (forward-line -1)) ; don't go outside the vtable, or reverting fails
    (vtable-revert-command)
    (goto-char position)))

(defun eww-bookmark-yank ()
  "Yank a previously killed bookmark to the current line."
  (interactive nil eww-bookmark-mode)
  (eww--bookmark-check-order-sort)
  (unless eww-bookmark-kill-ring
    (user-error "No previously killed bookmark"))
  (let* ((bookmark-at-point (nth 3 (vtable-current-object)))
         (index-bap (seq-position eww-bookmarks bookmark-at-point))
         ;; store in a list, for simpler concat
         (bookmark-to-insert (list (pop eww-bookmark-kill-ring)))
         (position (point)))
    (setq eww-bookmarks
          (if (= (point) (point-max))
              ;; special case: point is in the last line of the buffer,
              ;; so we know to append the bookmark at the end
              (progn
                (goto-char (point-min)) ; move inside the vtable instance
                (seq-concatenate 'list eww-bookmarks bookmark-to-insert))
            ;; TODO: a simpler way of doing this?
            (seq-concatenate 'list
                             (seq-subseq eww-bookmarks 0 index-bap)
                             bookmark-to-insert
                             (seq-subseq eww-bookmarks index-bap))))
    (eww-write-bookmarks)
    (vtable-revert-command)
    (vtable-beginning-of-table)
    (goto-char position)))

(defun eww-bookmark-browse ()
  "Browse the bookmark under point in eww."
  (interactive nil eww-bookmark-mode)
  (let ((bookmark-at-point (nth 3 (vtable-current-object))))
    (unless bookmark-at-point
      (user-error "No bookmark on the current line"))
    (quit-window)
    (eww-browse-url (plist-get bookmark-at-point :url))))

(defun eww-next-bookmark ()
  "Go to the next bookmark in the list."
  (interactive nil eww-bookmark-mode)
  (let (fresh-buffer target-bookmark)
    (unless (get-buffer "*eww bookmarks*")
      (setq fresh-buffer t)
      (eww-list-bookmarks t))
    (with-current-buffer "*eww bookmarks*"
      (unless fresh-buffer
        (forward-line 1))
      (setq target-bookmark (nth 3 (vtable-current-object))))
    (unless target-bookmark
      ;; usually because we moved past end of the table
      (user-error "No next bookmark"))
    (eww-browse-url (plist-get target-bookmark :url))))

(defun eww-previous-bookmark ()
  "Go to the previous bookmark in the list."
  (interactive nil eww-bookmark-mode)
  (let (fresh-buffer target-bookmark)
    (unless (get-buffer "*eww bookmarks*")
      (setq fresh-buffer t)
      (eww-list-bookmarks t))
    (with-current-buffer "*eww bookmarks*"
      (when fresh-buffer
	(vtable-end-of-table))
      ;; didn't move to a previous line, because we
      ;; were already on the first one
      (unless (= -1 (forward-line -1))
        (setq target-bookmark (nth 3 (vtable-current-object)))))
    (unless target-bookmark
      (user-error "No previous bookmark"))
    (eww-browse-url (plist-get target-bookmark :url))))

(defun eww-bookmark-urls ()
  "Get the URLs from the current list of bookmarks."
  (interactive nil eww-boomark-mode)
  (eww-read-bookmarks)
  (mapcar (lambda (x) (plist-get x :url)) eww-bookmarks))

(defvar-keymap eww-bookmark-mode-map
  "C-k" #'eww-bookmark-kill
  "C-y" #'eww-bookmark-yank
  "RET" #'eww-bookmark-browse
  :menu '("Eww Bookmark"
          ["Exit" quit-window t]
          ["Browse" eww-bookmark-browse
           :active (nth 3 (vtable-current-object))]
          ["Kill" eww-bookmark-kill
           :active (nth 3 (vtable-current-object))]
          ["Yank" eww-bookmark-yank
           :active eww-bookmark-kill-ring]))

(define-derived-mode eww-bookmark-mode special-mode "eww bookmarks"
  "Mode for listing bookmarks.

\\{eww-bookmark-mode-map}"
  :interactive nil
  (buffer-disable-undo)
  (setq truncate-lines t))

;;; History code

(defun eww-save-history ()
  "Save the current page's data to the history.
If the current page is a historical one loaded from
`eww-history' (e.g. by calling `eww-back-url'), this will update the
page's entry in `eww-history' and return nil.  Otherwise, add a new
entry to `eww-history' and return t."
  (plist-put eww-data :point (point))
  (plist-put eww-data :text (buffer-string))
  (if (zerop eww-history-position)
      (let ((history-delete-duplicates nil))
        (add-to-history 'eww-history eww-data eww-history-limit t)
        (setq eww-history-position 1)
        t)
    (setf (elt eww-history (1- eww-history-position)) eww-data)
    nil))

(defun eww-delete-future-history ()
  "Remove any entries in `eww-history' after the currently-shown one.
This is useful for `eww-before-browse-history-function' to make EWW's
navigation to a new page from a historical one work like other web
browsers: it will delete any \"future\" history elements before adding
the new page to the end of the history.

For example, if `eww-history' looks like this (going from newest to
oldest, with \"*\" marking the current page):

  E D C* B A

then calling this function updates `eww-history' to:

  C* B A"
  (when (> eww-history-position 1)
    (setq eww-history (nthcdr (1- eww-history-position) eww-history)
          ;; We don't really need to set this since `eww--before-browse'
          ;; sets it too, but this ensures that other callers can use
          ;; this function and get the expected results.
          eww-history-position 1)))

(defun eww-clone-previous-history ()
  "Clone and prepend entries in `eww-history' up to the currently-shown one.
These cloned entries get added to the beginning of `eww-history' so that
it's possible to navigate back to the very first page for this EWW
without deleting any history entries.

For example, if `eww-history' looks like this (going from newest to
oldest, with \"*\" marking the current page):

  E D C* B A

then calling this function updates `eww-history' to:

  C* B A E D C B A

This is useful for setting `eww-before-browse-history-function' (which
see)."
  (when (> eww-history-position 1)
    (setq eww-history (take eww-history-limit
                            (append (nthcdr (1- eww-history-position)
                                            eww-history)
                                    eww-history))
          ;; As with `eww-delete-future-history', we don't really need
          ;; to set this since `eww--before-browse' sets it too, but
          ;; let's be thorough.
          eww-history-position 1)))

(defvar eww-current-buffer)

(defun eww-list-histories ()
  "List the eww-histories."
  (interactive)
  (when (null eww-history)
    (error "No eww-histories are defined"))
  (let ((eww-history-trans eww-history)
	(buffer (current-buffer)))
    (set-buffer (get-buffer-create "*eww history*"))
    (eww-history-mode)
    (setq-local eww-current-buffer buffer)
    (let ((inhibit-read-only t)
	  (domain-length 0)
	  (title-length 0)
	  url title format start)
      (erase-buffer)
      (dolist (history eww-history-trans)
	(setq start (point))
	(setq domain-length (max domain-length (length (plist-get history :url))))
	(setq title-length (max title-length (length (plist-get history :title)))))
      (setq format (format "%%-%ds %%-%ds" title-length domain-length)
	    header-line-format
	    (concat " " (format format "Title" "URL")))
      (dolist (history eww-history-trans)
	(setq start (point))
	(setq url (plist-get history :url))
	(setq title (plist-get history :title))
	(insert (format format title url))
	(insert "\n")
	(put-text-property start (1+ start) 'eww-history history))
      (goto-char (point-min)))
    (pop-to-buffer "*eww history*")))

(defun eww-history-browse ()
  "Browse the history under point in eww."
  (interactive nil eww-history-mode)
  (let ((history (get-text-property (line-beginning-position) 'eww-history)))
    (unless history
      (error "No history on the current line"))
    (let ((buffer eww-current-buffer))
      (quit-window)
      (when buffer
	(pop-to-buffer-same-window buffer)))
    (eww-restore-history history)))

(defvar-keymap eww-history-mode-map
  "RET" #'eww-history-browse
  "n" #'next-line
  "p" #'previous-line
  :menu '("Eww History"
          ["Exit" quit-window t]
          ["Browse" eww-history-browse
           :active (get-text-property (line-beginning-position)
                                      'eww-history)]))

(define-derived-mode eww-history-mode special-mode "eww history"
  "Mode for listing eww-histories.

\\{eww-history-mode-map}"
  :interactive nil
  (buffer-disable-undo)
  (setq truncate-lines t))

;;; eww buffers list

(defun eww-buffer-list ()
  "Return a list of all live eww buffers."
  (match-buffers #'eww--buffer-p))

(defun eww-list-buffers ()
  "Pop a buffer with a list of eww buffers."
  (interactive)
  (if (null (eww-buffer-list))
      (message "No EWW buffers.")
    (with-current-buffer (get-buffer-create "*eww buffers*")
      (eww-buffers-mode)
      (eww--list-buffers-display-table))
    (pop-to-buffer "*eww buffers*")))

(defun eww--list-buffers-display-table (&optional _ignore-auto _noconfirm)
  "Display a table with the list of eww buffers.
Will remove all buffer contents first.  The parameters IGNORE-AUTO and
NOCONFIRM are ignored, they are for compatibility with
`revert-buffer-function'."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (make-vtable
     :columns '((:name "Title" :min-width "25%" :max-width "50%")
                (:name "URL"))
     :objects-function #'eww--list-buffers-get-data
     ;; use fixed-font face
     :face 'default)))

(defun eww--list-buffers-get-data ()
  "Return the eww-data of BUF, assumed to be a eww buffer.
The format of the data is (title url buffer), for use in of
`eww-buffers-mode'."
  (mapcar (lambda (buf)
            (let ((buf-eww-data (buffer-local-value 'eww-data buf)))
              (list (plist-get buf-eww-data :title)
                    (plist-get buf-eww-data :url)
                    buf)))
          (eww-buffer-list)))

(defun eww-buffer-select ()
  "Switch to eww buffer."
  (interactive nil eww-buffers-mode)
  (let ((buffer (nth 2 (vtable-current-object))))
    (unless buffer
      (error "No buffer on current line"))
    (quit-window)
    (pop-to-buffer-same-window buffer)))

(defun eww-buffer-show ()
  "Display buffer under point in eww buffer list."
  (let ((buffer (nth 2 (vtable-current-object))))
    (unless buffer
      (error "No buffer on current line"))
    (other-window -1)
    (pop-to-buffer-same-window buffer)
    (other-window 1)))

(defun eww-buffer-show-next ()
  "Move to next eww buffer in the list and display it."
  (interactive nil eww-buffers-mode)
  (forward-line)
  (when (eobp)
    (goto-char (point-min)))
  (eww-buffer-show))

(defun eww-buffer-show-previous ()
  "Move to previous eww buffer in the list and display it."
  (interactive nil eww-buffers-mode)
  (beginning-of-line)
  (when (bobp)
    (goto-char (point-max)))
  (forward-line -1)
  (eww-buffer-show))

(defun eww-buffer-kill ()
  "Kill buffer from eww list."
  (interactive nil eww-buffers-mode)
  (let* ((start (line-beginning-position))
	 (buffer (nth 2 (vtable-current-object)))
	 (inhibit-read-only t))
    (unless buffer
      (user-error "No buffer on the current line"))
    (kill-buffer buffer)
    (forward-line 1)
    (delete-region start (point)))
  (when (eobp)
    (forward-line -1))
  (eww-buffer-show))

(defvar-keymap eww-buffers-mode-map
  "C-k" #'eww-buffer-kill
  "RET" #'eww-buffer-select
  "n" #'eww-buffer-show-next
  "p" #'eww-buffer-show-previous
  :menu '("Eww Buffers"
          ["Exit" quit-window t]
          ["Select" eww-buffer-select
           :active (nth 2 (vtable-current-object))]
          ["Kill" eww-buffer-kill
           :active (nth 2 (vtable-current-object))]))

(define-derived-mode eww-buffers-mode special-mode "eww buffers"
  "Mode for listing buffers.

\\{eww-buffers-mode-map}"
  :interactive nil
  (buffer-disable-undo)
  (setq truncate-lines t)
  ;; This is set so that pressing "g" with point just below the table
  ;; will still update the listing.
  (setq-local revert-buffer-function #'eww--list-buffers-display-table))

;;; Desktop support

(defvar eww-desktop-data-save
  '(:url :title :point)
  "List of `eww-data' properties to preserve in the desktop file.
Also used when saving `eww-history'.")

(defun eww-desktop-data-1 (alist)
  (let ((acc  nil)
        (tail alist))
    (while tail
      (let ((k (car  tail))
            (v (cadr tail)))
        (when (memq k eww-desktop-data-save)
          (setq acc (cons k (cons v acc)))))
      (setq tail  (cddr tail)))
    acc))

(defun eww-desktop-history-duplicate (a b)
  (let ((tail a) (r t))
    (while tail
      (if (or (memq (car tail) '(:point)) ; ignore :point
	      (equal (cadr tail)
		     (plist-get b (car tail))))
	  (setq tail (cddr tail))
	(setq tail nil
	      r    nil)))
    ;; .
    r))

(defun eww-desktop-misc-data (_directory)
  "Return a property list with data used to restore eww buffers.
This list will contain, as :history, the list, whose first element is
the value of `eww-data', and the tail is `eww-history'.

If `eww-desktop-remove-duplicates' is non-nil, duplicate
entries (if any) will be removed from the list.

Only the properties listed in `eww-desktop-data-save' are included.
Generally, the list should not include the (usually overly large)
:dom, :source and :text properties."
  (let ((history (mapcar #'eww-desktop-data-1
                         (cons eww-data eww-history)))
        (posn eww-history-position) rval)
    (list :history
          (if eww-desktop-remove-duplicates
              (prog1
                  (setq
                   rval (cl-remove-duplicates
                         history :test #'eww-desktop-history-duplicate))
                (setq posn
                      (cl-position
                       (elt history eww-history-position)
                       rval :test #'eq)))
            history)
          :history-position posn)))

(defun eww-restore-desktop (file-name buffer-name misc-data)
  "Restore an eww buffer from its desktop file record.
If `eww-restore-desktop' is t or `auto', this function will also
initiate the retrieval of the respective URI in the background.
Otherwise, the restored buffer will contain a prompt to do so by using
\\[eww-reload]."
  (with-current-buffer (get-buffer-create buffer-name)
    (eww-mode)
    ;; NB: eww-history, eww-data are buffer-local per (eww-mode)
    (setq eww-history       (cdr (plist-get misc-data :history))
	  eww-data      (or (car (plist-get misc-data :history))
			    ;; backwards compatibility
			    (list :url (plist-get misc-data :uri)))
          eww-history-position (plist-get misc-data :history-position))
    (unless file-name
      (when (plist-get eww-data :url)
	(cl-case eww-restore-desktop
	  ((t auto) (eww (plist-get eww-data :url)))
	  ((nil) (when (zerop (buffer-size))
	           (let ((inhibit-read-only t))
	             (insert (substitute-command-keys
		              eww-restore-reload-prompt))))))))
    ;; .
    (current-buffer)))

(add-to-list 'desktop-buffer-mode-handlers
             '(eww-mode . eww-restore-desktop))

;;; Isearch support

(defun eww-isearch-next-buffer (&optional _buffer wrap)
  "Go to the next page to search using `rel' attribute for navigation."
  (let ((eww-retrieve-command 'sync))
    (if wrap
        (condition-case nil
	    (eww-top-url)
	  (error nil))
      (if isearch-forward
	  (eww-next-url)
        (eww-previous-url))))
  (current-buffer))

;;; bookmark.el support

(declare-function bookmark-make-record-default
                  "bookmark" (&optional no-file no-context posn))
(declare-function bookmark-prop-get "bookmark" (bookmark prop))

(defun eww-bookmark-name ()
  "Create a default bookmark name for the current EWW buffer."
  (plist-get eww-data :title))

(defun eww-bookmark-make-record ()
  "Create a bookmark for the current EWW buffer."
  `(,(eww-bookmark-name)
    ,@(bookmark-make-record-default t)
    (location . ,(plist-get eww-data :url))
    (handler . eww-bookmark-jump)))

;;;###autoload
(defun eww-bookmark-jump (bookmark)
  "Default bookmark handler for EWW buffers."
  (eww (bookmark-prop-get bookmark 'location)))

(put 'eww-bookmark-jump 'bookmark-handler-type "EWW")

(provide 'eww)

;;; Alternate links (RSS and Atom feeds, etc.)

(defun eww--alternate-urls (dom &optional base)
  "Return an alist of alternate links in DOM.

Each element is a list of the form (URL TYPE TITLE) where URL is
the href attribute of the link expanded relative to BASE, TYPE is
its type attribute, and TITLE is its title attribute.  If any of
these attributes is absent, the corresponding element is nil."
  (let ((alternates
         (seq-filter
          (lambda (attrs) (string= (alist-get 'rel attrs)
                                   "alternate"))
          (mapcar #'dom-attributes (dom-by-tag dom 'link)))))
    (mapcar (lambda (alternate)
              (list (url-expand-file-name (alist-get 'href alternate)
                                          base)
                    (alist-get 'type  alternate)
                    (alist-get 'title alternate)))
            alternates)))

(defun eww-read-alternate-url ()
  "Get the URL of an alternate link of this page.

If there is just one alternate link, return its URL.  If there
are multiple alternate links, prompt for one in the minibuffer
with completion.  If there are none, return nil."
  (when-let* ((alternates (eww--alternate-urls
                           (plist-get eww-data :dom)
                           (plist-get eww-data :url))))
    (let ((url-max-width
           (seq-max (mapcar #'string-pixel-width
                            (mapcar #'car alternates))))
          (title-max-width
           (seq-max (mapcar #'string-pixel-width
                            (mapcar #'caddr alternates))))
          (sep-width (string-pixel-width " ")))
      (if (cdr alternates)
            (completing-read
             "Alternate URL: "
             (completion-table-with-metadata
              alternates
              `((annotation-function
                 . ,(lambda (feed)
                      (let* ((attrs (alist-get feed
                                               alternates
                                               nil
                                               nil
                                               #'string=))
                             (type (car attrs))
                             (title (cadr attrs)))
                        (concat
                         (propertize " " 'display
                                     `(space :align-to
                                             (,(+ sep-width
                                                  url-max-width))))
                         title
                         (when type
                           (concat
                            (propertize " " 'display
                                        `(space :align-to
                                                (,(+ (* 2 sep-width)
                                                     url-max-width
                                                     title-max-width))))
                            "[" type "]"))))))))
             nil t)
        (caar alternates)))))

(defun eww-copy-alternate-url ()
  "Copy the alternate URL of the current page into the kill ring.
If there are multiple alternate links on the current page, prompt
for one in the minibuffer, with completion.
Alternate links are references that an HTML page may include to
point to its alternative representations, such as a translated
version or an RSS feed."
  (interactive nil eww-mode)
  (if-let* ((url (eww-read-alternate-url)))
      (progn
        (kill-new url)
        (message "Copied %s to kill ring" url))
    (user-error "No alternate links found on this page!")))

;;; eww.el ends here
