;;; xesam.el --- Xesam interface to search engines.

;; Copyright (C) 2008 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: tools, hypermedia

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an interface to the Xesam, a D-Bus based "eXtEnsible
;; Search And Metadata specification".  It has been tested with
;;
;; xesam-glib 0.3.4, xesam-tools 0.6.1
;; beagle 0.3.7, beagle-xesam 0.2
;; strigi 0.5.10

;; The precondition for this package is a D-Bus aware Emacs.  This is
;; configured per default, when Emacs is built on a machine running
;; D-Bus.  Furthermore, there must be at least one search engine
;; running, which support the Xesam interface.  Beagle and strigi have
;; been tested; tracker, pinot and recoll are also said to support
;; Xesam.  You can check the existence of such a search engine by
;;
;;   (dbus-list-queued-owners :session "org.freedesktop.xesam.searcher")

;; In order to start a search, you must load xesam.el:
;;
;;   (require 'xesam)

;; xesam.el supports two types of queries, which are explained *very* short:
;;
;; * Full text queries.  Just search keys shall be given, like
;;
;;     hello world
;;
;;   A full text query in xesam.el is restricted to files.
;;
;; * Xesam End User Search Language queries.  The Xesam query language
;;   is described at <http://xesam.org/main/XesamUserSearchLanguage>,
;;   which must be consulted for the whole features.
;;
;;   A query string consists of search keys, collectors, selectors,
;;   and phrases.  Search keys are words like in a full text query:
;;
;;     hello word
;;
;;   A selector is a tuple <keyword><relation>.  <keyword> can be any
;;   predefined Xesam keyword, the most common keywords are "ext"
;;   (file name extension), "format " (mime type), "tag" (user
;;   keywords) and "type" (types of items, like "audio", "file",
;;   "picture", "attachment").  <relation> is a comparison to a value,
;;   which must be a string (relation ":" or "=") or number (relation
;;   "<=", ">=", "<", ">"):
;;
;;     type:attachment ext=el
;;
;;   A collector is one of the items "AND", "and", "&&", "OR", "or",
;;   "||", or "-".  The default collector on multiple terms is "AND";
;;   "-" means "AND NOT".
;;
;;     albinus -type:file
;;
;;   A phrase is a string enclosed in quotes, with appended modifiers
;;   (single letters).  Examples of modifiers are "c" (case
;;   sensitive), "C" (case insensitive), "e" (exact match), "r"
;;   (regular expression):
;;
;;     "Hello world"c

;; You can customize, whether you want to apply a Xesam user query, or
;; a full text query.  Note, that not every search engine supports
;; both query types.
;;
;;   (setq xesam-query-type 'fulltext-query)
;;
;; Another option to be customised is the number of hits to be
;; presented at once.
;;
;;   (setq xesam-hits-per-page 50)

;; A search can be started by the command
;;
;;   M-x xesam-search
;;
;; When several search engines are registered, the engine to be used
;; can be selected via minibuffer completion.  Afterwards, the query
;; shall be entered in the minibuffer.

;;; Code:

;; D-Bus support in the Emacs core can be disabled with configuration
;; option "--without-dbus".  Declare used subroutines and variables.
(declare-function dbus-call-method "dbusbind.c")
(declare-function dbus-register-signal "dbusbind.c")

(require 'dbus)

;; Pacify byte compiler.
(eval-when-compile
  (require 'cl))

;; Widgets are used to highlight the search results.
(require 'widget)

(eval-when-compile
  (require 'wid-edit))

;; `run-at-time' is used in the signal handler.
(require 'timer)

;; The default search field is "xesam:url".  It must be inspected.
(require 'url)

(defgroup xesam nil
  "Xesam compatible interface to search engines."
  :group 'extensions
  :group 'hypermedia
  :version "23.1")

(defcustom xesam-query-type 'user-query
  "Xesam query language type."
  :group 'xesam
  :type '(choice
	  (const :tag "Xesam user query" user-query)
	  (const :tag "Xesam fulltext query" fulltext-query)))

(defcustom xesam-hits-per-page 20
  "Number of search hits to be displayed in the result buffer"
  :group 'xesam
  :type 'integer)

(defvar xesam-debug nil
  "Insert debug information.")

(defconst xesam-service-search "org.freedesktop.xesam.searcher"
  "The D-Bus name used to talk to Xesam.")

(defconst xesam-path-search "/org/freedesktop/xesam/searcher/main"
  "The D-Bus object path used to talk to Xesam.")

;; Methods: "NewSession", "SetProperty", "GetProperty",
;; "CloseSession", "NewSearch", "StartSearch", "GetHitCount",
;; "GetHits", "GetHitData", "CloseSearch" and "GetState".
;; Signals: "HitsAdded", "HitsRemoved", "HitsModified", "SearchDone"
;; and "StateChanged".
(defconst xesam-interface-search "org.freedesktop.xesam.Search"
  "The D-Bus Xesam search interface.")

(defconst xesam-all-fields
  '("xesam:35mmEquivalent" "xesam:Alarm" "xesam:Archive" "xesam:Audio"
    "xesam:AudioList" "xesam:Content" "xesam:DataObject" "xesam:DeletedFile"
    "xesam:Document" "xesam:Email" "xesam:EmailAttachment" "xesam:Event"
    "xesam:File" "xesam:FileSystem" "xesam:FreeBusy" "xesam:IMMessage"
    "xesam:Image" "xesam:Journal" "xesam:Mailbox" "xesam:Media"
    "xesam:MediaList" "xesam:Message" "xesam:PIM" "xesam:Partition"
    "xesam:Photo" "xesam:Presentation" "xesam:Project" "xesam:RemoteResource"
    "xesam:Software" "xesam:SourceCode" "xesam:Spreadsheet" "xesam:Storage"
    "xesam:Task" "xesam:TextDocument" "xesam:Video" "xesam:Visual"
    "xesam:aimContactMedium" "xesam:aperture" "xesam:aspectRatio"
    "xesam:attachmentEncoding" "xesam:attendee" "xesam:audioBirate"
    "xesam:audioChannels" "xesam:audioCodec" "xesam:audioCodecType"
    "xesam:audioSampleFormat" "xesam:audioSampleRate" "xesam:author"
    "xesam:bcc" "xesam:birthDate" "xesam:blogContactURL"
    "xesam:cameraManufacturer" "xesam:cameraModel" "xesam:cc" "xesam:ccdWidth"
    "xesam:cellPhoneNumber" "xesam:characterCount" "xesam:charset"
    "xesam:colorCount" "xesam:colorSpace" "xesam:columnCount" "xesam:comment"
    "xesam:commentCharacterCount" "xesam:conflicts" "xesam:contactMedium"
    "xesam:contactName" "xesam:contactNick" "xesam:contactPhoto"
    "xesam:contactURL" "xesam:contains" "xesam:contenKeyword"
    "xesam:contentComment" "xesam:contentCreated" "xesam:contentModified"
    "xesam:contentType" "xesam:contributor" "xesam:copyright" "xesam:creator"
    "xesam:definesClass" "xesam:definesFunction" "xesam:definesGlobalVariable"
    "xesam:deletionTime" "xesam:depends" "xesam:description" "xesam:device"
    "xesam:disclaimer" "xesam:documentCategory" "xesam:duration"
    "xesam:emailAddress" "xesam:eventEnd" "xesam:eventLocation"
    "xesam:eventStart" "xesam:exposureBias" "xesam:exposureProgram"
    "xesam:exposureTime" "xesam:faxPhoneNumber" "xesam:fileExtension"
    "xesam:fileSystemType" "xesam:flashUsed" "xesam:focalLength"
    "xesam:focusDistance" "xesam:formatSubtype" "xesam:frameCount"
    "xesam:frameRate" "xesam:freeSpace" "xesam:gender" "xesam:generator"
    "xesam:generatorOptions" "xesam:group" "xesam:hash" "xesam:hash"
    "xesam:height" "xesam:homeEmailAddress" "xesam:homePhoneNumber"
    "xesam:homePostalAddress" "xesam:homepageContactURL"
    "xesam:horizontalResolution" "xesam:icqContactMedium" "xesam:id"
    "xesam:imContactMedium" "xesam:interests" "xesam:interlaceMode"
    "xesam:isEncrypted" "xesam:isImportant" "xesam:isInProgress"
    "xesam:isPasswordProtected" "xesam:isRead" "xesam:isoEquivalent"
    "xesam:jabberContactMedium" "xesam:keyword" "xesam:language" "xesam:legal"
    "xesam:license" "xesam:licenseType" "xesam:lineCount" "xesam:links"
    "xesam:mailingPostalAddress" "xesam:maintainer" "xesam:md5Hash"
    "xesam:mediaCodec" "xesam:mediaCodecBitrate" "xesam:mediaCodecType"
    "xesam:meteringMode" "xesam:mimeType" "xesam:mountPoint"
    "xesam:msnContactMedium" "xesam:name" "xesam:occupiedSpace"
    "xesam:orientation" "xesam:originalLocation" "xesam:owner"
    "xesam:pageCount" "xesam:permissions" "xesam:phoneNumber"
    "xesam:physicalAddress" "xesam:pixelFormat" "xesam:primaryRecipient"
    "xesam:programmingLanguage" "xesam:rating" "xesam:receptionTime"
    "xesam:recipient" "xesam:related" "xesam:remoteUser" "xesam:rowCount"
    "xesam:sampleBitDepth" "xesam:sampleFormat" "xesam:secondaryRecipient"
    "xesam:sha1Hash" "xesam:size" "xesam:skypeContactMedium"
    "xesam:sourceCreated" "xesam:sourceModified" "xesam:storageSize"
    "xesam:subject" "xesam:supercedes" "xesam:title" "xesam:to"
    "xesam:totalSpace" "xesam:totalUncompressedSize" "xesam:url"
    "xesam:usageIntensity" "xesam:userComment" "xesam:userKeyword"
    "xesam:uuid" "xesam:version" "xesam:verticalResolution" "xesam:videoBirate"
    "xesam:videoCodec" "xesam:videoCodecType" "xesam:whiteBalance"
    "xesam:width" "xesam:wordCount" "xesam:workEmailAddress"
    "xesam:workPhoneNumber" "xesam:workPostalAddress"
    "xesam:yahooContactMedium")
  "All fields from the Xesam Core Ontology.
This defconst can be used to check for a new search engine, which
fields are supported.")

(defconst xesam-user-query
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<request xmlns=\"http://freedesktop.org/standards/xesam/1.0/query\">
  <userQuery>
    %s
  </userQuery>
</request>"
  "The Xesam user query XML.")

(defconst xesam-fulltext-query
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<request xmlns=\"http://freedesktop.org/standards/xesam/1.0/query\">
  <query content=\"xesam:Document\" source=\"xesam:File\">
    <fullText>
      <string>%s</string>
    </fullText>
  </query>
</request>"
  "The Xesam fulltext query XML.")

(defun xesam-get-property (engine property)
  "Return the PROPERTY value of ENGINE."
  ;; "GetProperty" returns a variant, so we must use the car.
  (car (dbus-call-method
	:session (car engine) xesam-path-search
	xesam-interface-search  "GetProperty"
	(cdr engine) property)))

(defun xesam-set-property (engine property value)
  "Set the PROPERTY of ENGINE to VALUE.
VALUE can be a string, a non-negative integer, a boolean
value (nil or t), or a list of them.  It returns the new value of
PROPERTY in the search engine.  This new value can be different
from VALUE, depending on what the search engine accepts."
  ;; "SetProperty" returns a variant, so we must use the car.
  (car (dbus-call-method
	:session (car engine) xesam-path-search
	xesam-interface-search  "SetProperty"
	(cdr engine) property
	;; The value must be a variant.  It can be only a string, an
	;; unsigned int, a boolean, or an array of them.  So we need
	;; no type keyword; we let the type check to the search
	;; engine.
	(list :variant value))))

(defvar xesam-minibuffer-vendor-history nil
  "Interactive vendor history.")

(defvar xesam-minibuffer-query-history nil
  "Interactive query history.")

;; Pacify byte compiler.
(defvar xesam-engine nil)
(defvar xesam-search nil)
(defvar xesam-current nil)
(defvar xesam-count nil)
(defvar xesam-from nil)
(defvar xesam-to nil)
(defvar xesam-refreshing nil)


;;; Search engines.

(defvar xesam-search-engines nil
  "List of available Xesam search engines.
Every entry is a triple of the unique D-Bus service name of the
engine, the session identifier, and the display name.  Example:

  \(\(\":1.59\" \"0t1214948020ut358230u0p2698r3912347765k3213849828\" \"Tracker Xesam Service\")
   \(\":1.27\" \"strigisession1369133069\" \"Strigi Desktop Search\"))

A Xesam-compatible search engine is identified as a queued D-Bus
service of `xesam-service-search'.")

(defun xesam-delete-search-engine (&rest args)
  "Removes service from `xesam-search-engines'."
  (when (and (= (length args) 3) (stringp (car args)))
    (setq xesam-search-engines
	  (delete (assoc (car args) xesam-search-engines)
		  xesam-search-engines))))

(defun xesam-search-engines ()
  "Return Xesam search engines, stored in `xesam-search-engines'.
The first search engine is the name owner of `xesam-service-search'.
If there is no registered search engine at all, the function returns `nil'."
  (let ((services (dbus-ignore-errors
		    (dbus-list-queued-owners
		     :session xesam-service-search)))
	engine vendor-id hit-fields)
    (dolist (service services)
      (unless (assoc-string service xesam-search-engines)

	;; Open a new session, and add it to the search engines list.
	(add-to-list
	 'xesam-search-engines
	 (setq engine
	       (cons service
		     (dbus-call-method
		      :session service xesam-path-search
		      xesam-interface-search "NewSession")))
	 'append)

	;; Set the "search.live" property; otherwise the search engine
	;; might refuse to answer.
;	(xesam-set-property engine "search.live" t)

	;; Check the vendor properties.
	(setq vendor-id (xesam-get-property engine "vendor.id")
	      hit-fields (xesam-get-property engine "hit.fields"))

	;; Ususally, `hit.fields' shall describe supported fields.
	;; That is not the case now, so we set it ourselves.
	;; Hopefully, this will change later.
	(setq hit-fields
	      (cond
	       ((string-equal vendor-id "Beagle")
		'("xesam:mimeType" "xesam:url"))
	       ((string-equal vendor-id "Strigi")
		'("xesam:author" "xesam:cc" "xesam:cc" "xesam:charset"
		  "xesam:contentType" "xesam:fileExtension" "xesam:id"
		  "xesam:lineCount" "xesam:links" "xesam:mimeType" "xesam:name"
		  "xesam:size" "xesam:sourceModified" "xesam:subject"
		  "xesam:to" "xesam:url"))
	       ((string-equal vendor-id "TrackerXesamSession")
		'("xesam:relevancyRating" "xesam:url"))
	       ;; xesam-tools yahoo service.
	       (t '("xesam:contentModified" "xesam:mimeType" "xesam:summary"
		    "xesam:title" "xesam:url" "yahoo:displayUrl"))))

	(xesam-set-property engine "hit.fields" hit-fields)
	(xesam-set-property engine "hit.fields.extended" '("xesam:snippet"))

	;; Let us notify, when the search engine disappears.
	(dbus-register-signal
	 :session dbus-service-dbus dbus-path-dbus
	 dbus-interface-dbus "NameOwnerChanged"
	 'xesam-delete-search-engine service))))
  xesam-search-engines)


;;; Search buffers.

(define-derived-mode xesam-mode nil "Xesam"
  "Major mode for presenting search results of a Xesam search.
In this mode, widgets represent the search results.

\\{xesam-mode-map}
Turning on Xesam mode runs the normal hook `xesam-mode-hook'."
  ;; Initialize buffer.
  (setq buffer-read-only t)
  (let ((inhibit-read-only t))
    (erase-buffer))

  ;; Keymap.
  (set-keymap-parent xesam-mode-map widget-keymap)
  (define-key xesam-mode-map "q" 'quit-window)

  ;; Local variables.
  (set (make-local-variable 'mode-line-position)  (list "%p (0/0)"))
  ;; `xesam-engine' and `xesam-search' will be set in `xesam-new-search'.
  (set (make-local-variable 'xesam-engine) nil)
  (set (make-local-variable 'xesam-search) nil)
  (set (make-local-variable 'xesam-current) 1)
  (set (make-local-variable 'xesam-count) 0)
  (set (make-local-variable 'xesam-from) 1)
  (set (make-local-variable 'xesam-to) xesam-hits-per-page)
  ;; `xesam-refreshing' is an indicator, whether the buffer is just
  ;; being updated.  Needed, because `xesam-refresh-search-buffer'
  ;; can be triggered by an event.
  (set (make-local-variable 'xesam-refreshing) nil))

(defun xesam-buffer-name (service search)
  "Return the buffer name where to present search results.
SERVICE is the D-Bus unique service name of the Xesam search engine.
SEARCH is the search identification in that engine.  Both must be strings."
  (format "*%s/%s*" service search))

(defun xesam-refresh-entry (engine search hit-number)
  "Refreshes one entry in the search buffer."
  (let* ((result
	  (car
	   (dbus-call-method
	    :session (car engine) xesam-path-search
	    xesam-interface-search "GetHits" search 1)))
	 (snippet)
	 ;; We must disable this for the time being; the search
	 ;; engines don't return usable values so far.
;	  (caaar
;	   (dbus-ignore-errors
;	     (dbus-call-method
;	      :session  (car engine) xesam-path-search
;	      xesam-interface-search "GetHitData"
;	      search (list hit-number) '("snippet")))))
	 widget)

    ;; Create widget.
    (setq widget (widget-convert 'link))

    ;; Take all results.
    (dolist (field (xesam-get-property engine "hit.fields"))
      (when (not (zerop (length (caar result))))
	(when xesam-debug
	  (widget-insert (format "%s: %s\n" field (caar result))))
	(widget-put widget (intern (concat ":" field)) (caar result)))
      (setq result (cdr result)))

    ;; Strigi doesn't return URLs in xesam:url.  We must fix this.
    (when
	(not (url-type (url-generic-parse-url (widget-get widget :xesam:url))))
      (widget-put
       widget :xesam:url (concat "file://" (widget-get widget :xesam:url))))

    ;; First line: :tag.
    (cond
     ((widget-member widget :xesam:title)
      (widget-put widget :tag (widget-get widget :xesam:title)))
     ((widget-member widget :xesam:subject)
      (widget-put widget :tag (widget-get widget :xesam:subject)))
     ((widget-member widget :xesam:mimeType)
      (widget-put widget :tag (widget-get widget :xesam:mimeType)))
     ((widget-member widget :xesam:name)
      (widget-put widget :tag (widget-get widget :xesam:name))))

    ;; Last Modified.
    (when (widget-member widget :xesam:sourceModified)
      (widget-put
       widget :tag
       (format
	"%s\nLast Modified: %s"
	(or (widget-get widget :tag) "")
	(format-time-string
	 "%d %B %Y, %T"
	 (seconds-to-time
	  (string-to-number (widget-get widget :xesam:sourceModified)))))))

    ;; Second line: :value.
    (widget-put widget :value (widget-get widget :xesam:url))

    (cond
     ;; In case of HTML, we use a URL link.
     ((and (widget-member widget :xesam:mimeType)
	   (string-equal "text/html" (widget-get widget :xesam:mimeType)))
      (setcar widget 'url-link))

     ;; For local files, we will open the file as default action.
     ((string-match "file"
		    (url-type (url-generic-parse-url
			       (widget-get widget :xesam:url))))
      (widget-put
       widget :notify
       '(lambda (widget &rest ignore)
	  (find-file
	   (url-filename (url-generic-parse-url (widget-value widget))))))
      (widget-put
       widget :value
       (url-filename (url-generic-parse-url (widget-get widget :xesam:url))))))

    ;; Third line: :doc.
    (cond
     ((widget-member widget :xesam:summary)
      (widget-put widget :doc (widget-get widget :xesam:summary)))
     ((widget-member widget :xesam:snippet)
      (widget-put widget :doc (widget-get widget :xesam:snippet))))

    (when (widget-member widget :doc)
      (widget-put widget :help-echo (widget-get widget :doc))
      (with-temp-buffer
	(insert (widget-get widget :doc))
	(fill-region-as-paragraph (point-min) (point-max))
	(widget-put widget :doc (buffer-string))))

    ;; Format the widget.
    (widget-put
     widget :format
     (format "%d. %s%%[%%v%%]\n%s\n" hit-number
	     (if (widget-member widget :tag) "%{%t%}\n" "")
	     (if (widget-member widget :doc) "%h" "")))

    ;; Write widget.
    (goto-char (point-max))
    (widget-default-create widget)
    (set-buffer-modified-p nil)
    (setq mode-line-position
	  (list (format "%%p (%d/%d)" xesam-current xesam-count)))
    (redisplay)))

(defun xesam-refresh-search-buffer (engine search)
  "Refreshes the buffer, presenting results of SEARCH."
  (with-current-buffer (xesam-buffer-name (car engine) search)
    ;; Work only if nobody else is here.
    (unless xesam-refreshing
      (setq xesam-refreshing t)
      (unwind-protect
	  ;; `xesam-from' is the first result id to be presented.
	  ;; `xesam-current' is the last result which has been presented.
	  ;; `xesam-to' is the upper result to be presented.
	  ;; All of them are buffer-local variables.
	  (let ((from (max xesam-from xesam-current))
		widget next)
	    ;; Add all result widgets.  The upper boundary is always
	    ;; computed, because new hits might have arrived while
	    ;; running.
	    (while (<= from (min xesam-to xesam-count))
	      (xesam-refresh-entry engine search from)
	      (setq next t
		    from (1+ from)
		    xesam-current from))

	    ;; Add "NEXT" widget.
	    (when (and next (> xesam-count xesam-to))
	      (goto-char (point-max))
	      (widget-create
	       'link
	       :notify
	       '(lambda (widget &rest ignore)
		  (setq xesam-from (+ xesam-from xesam-hits-per-page)
			xesam-to   (+ xesam-to   xesam-hits-per-page))
		  (widget-delete widget)
		  (xesam-refresh-search-buffer xesam-engine xesam-search))
	       "NEXT")
	      (widget-beginning-of-line)))

	;; Return with save settings.
	(setq xesam-refreshing nil)))))


;;; Search functions.

(defun xesam-signal-handler (&rest args)
  "Handles the different D-Bus signals of a Xesam search."
  (let* ((service (dbus-event-service-name last-input-event))
	 (member (dbus-event-member-name last-input-event))
	 (search (nth 0 args))
	 (buffer (xesam-buffer-name service search)))

    (when (get-buffer buffer)
      (with-current-buffer buffer
	(cond

	 ((string-equal member "HitsAdded")
	  (setq xesam-count (+ xesam-count (nth 1 args))
		mode-line-position
		(list (format "%%p (%d/%d)" (1- xesam-current) xesam-count)))
	  ;; We use `run-at-time' in order to not block the event queue.
	  (run-at-time
	   0 nil
	   'xesam-refresh-search-buffer
	   (assoc service xesam-search-engines) search))

	 ((string-equal member "SearchDone")
	  (setq mode-line-process
		(propertize " Done" 'face 'font-lock-type-face))
	  (force-mode-line-update)))))))

(defun xesam-new-search (engine query)
  "Create a new search session.
ENGINE identifies the search engine.  QUERY is a string in the
Xesam user query language.  A string, identifying the search, is
returned."
  (let* ((service (car engine))
	 (session (cdr engine))
	 (search (dbus-call-method
		  :session service xesam-path-search
		  xesam-interface-search "NewSearch" session query)))
    ;; Let us notify for relevant signals.  We ignore "HitsRemoved",
    ;; "HitsModified" and "StateChanged"; there is nothing to do for
    ;; us.
    (dbus-register-signal
     :session service xesam-path-search
     xesam-interface-search "HitsAdded"
     'xesam-signal-handler search)
    (dbus-register-signal
     :session service xesam-path-search
     xesam-interface-search "SearchDone"
     'xesam-signal-handler search)
    (dbus-call-method
     :session (car engine) xesam-path-search
     xesam-interface-search "StartSearch" search)
    ;; Create the search buffer.
    (with-current-buffer
	(generate-new-buffer (xesam-buffer-name service search))
      (switch-to-buffer-other-window (current-buffer))
      (xesam-mode)
      (setq xesam-engine engine
	    xesam-search search
	    mode-line-buffer-identification
	    (xesam-get-property engine "vendor.id"))
      (when xesam-debug
	(widget-insert
	 (format "vendor.id: %s\n"
		 (xesam-get-property engine "vendor.id"))
	 (format "vendor.version: %s\n"
		 (xesam-get-property engine "vendor.version"))
	 (format "vendor.display: %s\n"
		 (xesam-get-property engine "vendor.display"))
	 (format "vendor.xesam: %s\n"
		 (xesam-get-property engine "vendor.xesam"))
	 (format "vendor.ontology.fields: %s\n"
		 (xesam-get-property engine "vendor.ontology.fields"))
	 (format "vendor.ontology.contents: %s\n"
		 (xesam-get-property engine "vendor.ontology.contents"))
	 (format "vendor.ontology.sources: %s\n"
		 (xesam-get-property engine "vendor.ontology.sources"))
	 (format "vendor.extensions: %s\n"
		 (xesam-get-property engine "vendor.extensions"))
	 (format "vendor.ontologies: %s\n"
		 (xesam-get-property engine "vendor.ontologies"))
	 (format "vendor.maxhits: %s\n\n"
		 (xesam-get-property engine "vendor.maxhits")))))

    ;; Return search id.
    search))

(defun xesam-search (engine query)
  "Perform an interactive search.
ENGINE is the Xesam search engine to be applied, it must be one of the
entries of `xesam-search-engines'.  QUERY is the search string in the
Xesam user query language.  If the search engine does not support
the Xesam user query language, a Xesam fulltext search is applied.

The default search engine is the first entry in `xesam-search-engines'.
Example:

  (xesam-search (car (xesam-search-engines)) \"emacs\")"
  (interactive
   (let* ((vendors (mapcar
		    '(lambda (x) (xesam-get-property x "vendor.display"))
		    (xesam-search-engines)))
	  (vendor
	   (if (> (length vendors) 1)
	       (completing-read
		"Enter search engine: " vendors nil t
		(try-completion "" vendors) 'xesam-minibuffer-vendor-history)
	     (car vendors))))
     (list
      ;; ENGINE.
      (when vendor
	(dolist (elt (xesam-search-engines) engine)
	  (when (string-equal (xesam-get-property elt "vendor.display") vendor)
	    (setq engine elt))))
      ;; QUERY.
      (when vendor
	(read-from-minibuffer
	 "Enter search string: " nil nil nil
	 'xesam-minibuffer-query-history)))))

  (if (and engine (stringp query))
      (if (eq xesam-query-type 'user-query)
	  (xesam-new-search engine (format xesam-user-query query))
	(xesam-new-search engine (format xesam-fulltext-query query)))
    ;; There might be no search engine available ATM.
    (message "No query applied")))

(provide 'xesam)

;;; TODO:

;; * Retrieve several results at once.
;; * Improve mode-line handling. Show search string etc.
;; * Minibuffer completion for user queries.

;; * Mid term
;;   - If available, use ontologies for field selection.
;;   - Search engines for Emacs bugs database, wikipedia, google,
;;     yahoo, ebay, ...

;; arch-tag: 7fb9fc6c-c2ff-4bc7-bb42-bacb80cce2b2
;;; xesam.el ends here
