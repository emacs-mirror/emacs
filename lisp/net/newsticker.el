;;; newsticker.el --- A Newsticker for Emacs.

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; Author:      Ulf Jasper <ulf.jasper@web.de>
;; Filename:    newsticker.el
;; URL:         http://www.nongnu.org/newsticker
;; Created:     17. June 2003
;; Keywords:    News, RSS, Atom
;; Time-stamp:  "29. Januar 2007, 21:05:09 (ulf)"

;; ======================================================================

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(defconst newsticker-version "1.10" "Version number of newsticker.el.")

;; ======================================================================
;;; Commentary:

;; Overview
;; --------

;; Newsticker provides a newsticker for Emacs.  A newsticker is a thing
;; that asynchronously retrieves headlines from a list of news sites,
;; prepares these headlines for reading, and allows for loading the
;; corresponding articles in a web browser.

;; Headlines consist of a title and (possibly) a small description.  They
;; are contained in "RSS" (RDF Site Summary) or "Atom" files.  Newsticker
;; should work with the following RSS formats:
;;  * RSS 0.91
;;    (see http://backend.userland.com/rss091 or
;;    http://my.netscape.com/publish/formats/rss-spec-0.91.html)
;;  * RSS 0.92
;;    (see http://backend.userland.com/rss092)
;;  * RSS 1.0
;;    (see http://purl.org/rss/1.0/spec)
;;  * RSS 2.0
;;    (see http://blogs.law.harvard.edu/tech/rss)
;; as well as the following Atom formats:
;;  * Atom 0.3
;;  * Atom 1.0
;;    (see http://www.ietf.org/internet-drafts/draft-ietf-atompub-format-11.txt)
;; That makes Newsticker.el an "Atom aggregator, "RSS reader", "RSS
;; aggregator", and "Feed Reader".

;; Newsticker provides several commands for reading headlines, navigating
;; through them, marking them as read/unread, hiding old headlines
;; etc.  Headlines can be displayed as plain text or as rendered HTML.

;; Headlines can be displayed in the echo area, either scrolling like
;; messages in a stock-quote ticker, or just changing.

;; Newsticker allows for automatic processing of headlines by providing
;; hooks and (sample) functions for automatically downloading images and
;; enclosed files (as delivered by podcasts, e.g.).

;; Requirements
;; ------------
;; Newsticker can be used with GNU Emacs version 21.1 or later as well as
;; XEmacs.  It requires an XML-parser (`xml.el') which is part of GNU
;; Emacs.  If you are using XEmacs you want to get the `net-utils' package
;; which contains `xml.el' for XEmacs.

;; Newsticker requires a program which can retrieve files via http and
;; prints them to stdout.  By default Newsticker will use wget for this
;; task.

;; Installation
;; ------------

;; If you are using Newsticker as part of GNU Emacs there is no need to
;; perform any installation steps in order to use Newsticker.  Otherwise
;; place Newsticker in a directory where Emacs can find it.  Add the
;; following line to your Emacs startup file (`~/.emacs').
;;   (add-to-list 'load-path "/path/to/newsticker/")
;;   (autoload 'newsticker-start "newsticker" "Emacs Newsticker" t)
;;   (autoload 'newsticker-show-news "newsticker" "Emacs Newsticker" t)

;; If you are using `imenu', which allows for navigating with the help of a
;; menu, you should add the following to your Emacs startup file
;; (`~/.emacs').
;;   (add-hook 'newsticker-mode-hook 'imenu-add-menubar-index)

;; That's it.

;; Usage
;; -----
;; The command newsticker-show-news will display all available headlines in
;; a special buffer, called `*newsticker*'.  It will also start the
;; asynchronous download of headlines.  The modeline in the `*newsticker*'
;; buffer informs whenever new headlines have arrived.  Clicking
;; mouse-button 2 or pressing RET in this buffer on a headline will call
;; browse-url to load the corresponding news story in your favourite web
;; browser.

;; The scrolling, or flashing of headlines in the echo area, can be started
;; with the command newsticker-start-ticker.  It can be stopped with
;; newsticker-stop-ticker.

;; If you just want to start the periodic download of headlines use the
;; command newsticker-start.  Calling newsticker-stop will stop the
;; periodic download, but will call newsticker-stop-ticker as well.

;; Configuration
;; -------------
;; All Newsticker options are customizable, i.e. they can be changed with
;; Emacs customization methods: Call the command customize-group and enter
;; `newsticker' for the customization group.

;; All Newsticker options have reasonable default values, so that in most
;; cases it is not necessary to customize settings before starting
;; Newsticker for the first time.

;; Newsticker options are organized in the following groups.

;; * newsticker-feed contains options that define which news
;;   feeds are retrieved and how this is done.
;;   o newsticker-url-list defines the list of headlines which are
;;     retrieved.
;;   o newsticker-retrieval-interval defines how often headlines are
;;     retrieved.
;; * newsticker-headline-processing contains options that define how the
;;   retrieved headlines are processed.
;;   o newsticker-keep-obsolete-items decides whether unread headlines that
;;     have been removed from the feed are kept in the Newsticker cache.
;; * newsticker-layout contains options that define how the buffer for
;;   reading news headlines is formatted.
;;   o newsticker-item-format defines how the title of a headline is
;;     formatted.
;; * newsticker-ticker contains options that define how headlines are shown
;;   in the echo area.
;;   o newsticker-display-interval and newsticker-scroll-smoothly define
;;     how headlines are shown in the echo area.
;; * newsticker-hooks contains options for hooking other Emacs commands to
;;   newsticker functions.
;;   o newsticker-new-item-functions allows for automatic processing of
;;     headlines.  See `newsticker-download-images', and
;;     `newsticker-download-enclosures' for sample functions.
;; * newsticker-miscellaneous contains other Newsticker options.

;; Please have a look at the customization buffers for the complete list of
;; options.

;; Remarks
;; -------
;; This newsticker is designed do its job silently in the background
;; without disturbing you.  However, it is probably impossible to prevent
;; such a tool from slightly attenuating your Editor's responsiveness every
;; once in a while.

;; Byte-compiling newsticker.el is recommended.

;; ======================================================================
;;; History:

;; 1.10 (2007-01-29)
;;     * Bugfixes mostly: `newsticker--decode-iso8601-date',
;;       `newsticker--sentinel', and others. 
;;     * Renamed `newsticker--retrieval-timer-list' to
;;       `newsticker-retrieval-timer-list'.  Removed
;;       `newsticker-running-p' -- check newsticker-retrieval-timer-list
;;       to find out whether newsticker is running.  Removed
;;       `newsticker-ticker-running-p'.
;;     * Try to cache images in w3m-rendered HTML text.
;;     * Other minor changes.

;; 1.9 (2005-11-01)
;;     * Rewrote feed parsing part.  Newsticker now supports RSS 0.91,
;;       0.92, 1.0, 2.0 as well as Atom 0.3 and 1.0 -- thanks to Thien-Thi
;;       Nguyen.
;;     * Changed auto-marking mechanism: Replaced variable
;;       `newsticker-auto-mark-filter' with new variable
;;       `newsticker-auto-mark-filter-list', which allows for looking not
;;       only at the title but also at the description of a headline.
;;     * Call `newsticker--ticker-text-setup' only after all pending
;;       downloads processes have finished.
;;     * Improved handling of coding systems.
;;     * Added magic autoload comments.
;;     * Bugfixes:
;;       - `hide-entry' was hiding too much when called for the last
;;          headline,
;;       - update mode-line and menu-bar when necessary,
;;       - repaired `newsticker--imenu-goto',
;;       - other minor things.

;; 1.8 (2005-08-26)
;;     * Added commands `newsticker-show-extra' and `newsticker-hide-extra'
;;       to show and hide extra RSS elements, bound to "sx" and "hx"
;;       resp. Changed default value of `newsticker-show-all-rss-elements'
;;       to nil.
;;     * mode-line: Introduced special mode-line-format for newsticker.
;;     * Get feed logos only once every 24 h.
;;     * Default faces changed.
;;     * Minor fixes.

;; 1.7 (2005-06-25)
;;     * Tool-bar support: most important commands can be called from
;;       tool-bar buttons.
;;     * Auto-Narrowing introduced: *newsticker* buffer can be narrowed to
;;       a single item (bound to key `xi') or a single feed (bound to
;;       `xf').
;;     * Enclosure support: enclosed items are shown (see
;;       `newsticker-enclosure-face') and can be (automatically) downloaded
;;       (see below). For those of you who read "podcasts".
;;     * Added variable `newsticker-auto-mark-filter' for automatically
;;       marking items as immortal or old.
;;     * Added hook variable `newsticker-new-item-functions' for handling
;;       new items.  Added sample functions `newsticker-download-images',
;;       and `newsticker-download-enclosures'.
;;     * Added hook variable `newsticker-select-item-hook' which is run
;;       after `newsticker-(next|previous)-(new-)?-item'.
;;     * Added hook variable `newsticker-select-feed-hook' which is run
;;       after `newsticker-(next|previous)-feed'.
;;     * Added hook variable `newsticker-buffer-change-hook' which is run
;;       after the contents or visibility of the newsticker buffer has
;;       changed, e.g. after `newsticker-buffer-update' or
;;       `newsticker-show-feed-desc'.
;;     * Added command `newsticker-handle-url' for interactively launching
;;       arbitrary programs for URLs, bound to `C-RET'.
;;     * URLs in extra elements are clickable.
;;     * Better support for w3, added command
;;       `newsticker-w3m-show-inline-images' for displaying all inline
;;       images.
;;     * Insert an artificial headline which notifies about failed
;;       retrievals.
;;     * Use pubDate element (RSS 2.0) instead of retrieval time when
;;       available.
;;     * Customizable options grouped.
;;     * Bugfixes: `newsticker--imenu-create-index'; strip whitespace
;;       from links; apply coding-system to extra-elements; time-comparison
;;       for obsolete items; and others which I have forgotten.
;;     * Workaround for another bug in xml-parse-region -- thanks to
;;       anonymous for sending patch.
;;     * Renamed invisible buffers ` *wget-newsticker-<feed>*' to
;;       ` *newsticker-wget-<feed>*'.
;;     * Tested with GNU Emacs versions 21.3 and 22.0 and XEmacs
;;       21.something.

;; 1.6 * Support for (some) optional RSS elements: guid, dc:date. See
;;       `newsticker-show-all-rss-elements' `newsticker-extra-face'.
;;     * Better support for w3m -- `newsticker-default-face' is obsolete
;;       now, removed `newsticker-w3m-toggle-inline-image'.
;;     * Added `newsticker-desc-comp-max' -- comparison of item
;;       descriptions can take quite some time.
;;     * Added `newsticker--buffer-make-item-completely-visible' to
;;       ensure that the current item is fully visible.
;;     * Allow for non-positive retrieval-interval, which make newsticker
;;       get news only once.
;;     * Use :set for customizable variables.
;;     * Added `newsticker-buffer-force-update', bound to key `U'.
;;     * Added concept of obsolete items, see
;;       `newsticker-keep-obsolete-items', `newsticker-obsolete-item-face',
;;       `newsticker-obsolete-item-max-age'.
;;     * Added `newsticker-add-url'.
;;     * OPML export.
;;     * Save pre-formatted titles => even better performance!!
;;     * `newsticker-*-new-item' wraps at beginning/end of buffer.
;;     * Always sort obsolete items to end of item list.
;;     * Bugfixes:
;;       - newsticker-hide-entry,
;;       - changes of feed-titles led to duplicate feed items,
;;       - faces for rendered HTML texts,
;;       - length of ticker-text (for "exotic"/multibyte texts),
;;         Thanks to Hiroshi Maruyama.
;;       - suppress items with empty title and description
;;       - newsticker-sort-method was ignored!
;;       - prevent call of fill-region on HTML-rendered descriptions.

;; 1.5 * Rewrote the visibility stuff. newsticker does not inherit
;;       outline anymore.  Now you have complete freedom for
;;       `newsticker-*-format'.
;;     * Save pre-formatted descriptions => incredible performance boost!!
;;     * Introduced `newsticker-(start|stop)-ticker'.
;;     * Introduced statistics for heading-format and
;;       `newsticker-statistics-face'.
;;     * Introduced `newsticker-enable-logo-manipulations'.
;;     * Compare link of items (as well as title and desc).
;;     * Added `newsticker-start-hook' and `newsticker-stop-hook', thanks
;;       to mace.
;;     * Bugfixes -- thanks to Ryan Yeske, Jari Aalto, Bruce Ingalls.
;;     * Tested with Emacs 21.3.50, 21.3.1, 21.2, 21.1; XEmacs 21.4.15

;; 1.4 * Enabled HTML rendering, added `newsticker-html-renderer' to
;;       choose a HTML rendering engine, thanks to Greg Scott for testing
;;     * New Outline handling using text properties instead of "**"
;;       prefixes.
;;     * Added possibility to mark single item as old (bound to key
;;       `o' (`newsticker-mark-item-at-point-as-read').
;;     * Added possibility to mark single item as immortal (bound to key
;;       `i' (`newsticker-mark-item-at-point-as-immortal').
;;     * Added possibility to display feed logos.
;;     * Added `newsticker-heading-format', `newsticker-item-format'.
;;     * Added `newsticker-date-format'.
;;     * Added `newsticker-justification'.
;;     * Added `newsticker-automatically-mark-visited-items-as-old'.
;;     * Added `newsticker-w3m-toggle-inline-image' which calls
;;       `w3m-toggle-inline-image' if `newsticker-html-renderer' is
;;       `w3m-region'. Exists for convenience only (bound to key
;;       `RET').

;; 1.3 * Compare title AND desc to check whether item is old, except
;;       for feed desc
;;     * Mark as not-up-to-date only after new items have arrived.
;;     * Added XEmacs compatibility code, tested with XEmacs 21.4.13.
;;     * Tested with Emacs 21.3.50 and Emacs 21.2.something.
;;     * Bugfix: Apply coding-systems to feed title and description,
;;       thanks to OHASHI Akira
;;     * Bugfix: xml-parser-workaround did not work for japanese texts,
;;       thanks to OHASHI Akira
;;     * Kill wget-buffers unless newsticker-debug is not nil.
;;     * Bugfix: xml-parser-workaround for "DOCTYPE rdf:RDF"

;; 1.2 Peter S Galbraith <psg@debian.org>
;;     * Added `newsticker-url-list-defaults', splitting the URLs into
;;       a customizable selection list, and a user add-on list.
;;     * Minor checkdoc fixes.

;; 1.1 * Introduced optional feed-specific wget-arguments.
;;     * Keep order of feeds as given in `newsticker-url-list' in
;;       *newsticker* buffer.
;;     * Ignore unsupported coding systems.

;; 1.0 * Introduced feed-specific retrieval-timers.
;;     * Removed dependency on 'cl (cddddr).
;;     * Thanks to Kevin Rodgers and T.V.  Raman for their help.
;;     * Use utf-8 for reading and writing cache data.
;;     * Reported to work with Emacs 21.3.50.

;; 0.99 * Minor tweaks.
;;      * Tested with Emacs 21.3.2

;; 0.98 * Check exit status of wget processes.  Keep cache data if
;;        something went wrong.  Throw error when old wget-processes
;;        are hanging around.
;;      * Introduced newsticker-specific faces.
;;      * Added `newsticker-show-descriptions-of-new-items'.
;;      * Added `newsticker-hide-old-items-in-newsticker-buffer'.
;;      * Added `newsticker-(hide|show)-old-items'.

;; 0.97 * Minor tweaks.

;; 0.96 * Added caching.
;;      * newsticker-mode inherits outline-mode.
;;      * newsticker-mode supports imenu.
;;      * Easy buffer-navigation with newsticker-mode's keymap.
;;      * Some bugs fixed.
;;      * Thanks to Moritz Epple for documentation tips.

;; 0.95 * Added newsticker-mode -- Thanks to T.V.  Raman.
;;      * Catch xml-parser errors -- Thanks to T.V.  Raman.
;;      * Remove stupid newlines in titles (headlines) -- Thanks to
;;        Jeff Rancier.

;; 0.94 * Added clickerability and description for channel headings.
;;      * Made it work for (at least some) rss 0.9<something> feeds.

;; 0.93 * Added some more sites.
;;      * Do not flood the *Messages* buffer.
;;      * First attempt at handling coding systems.

;; 0.92 * Added `newsticker-wget-name'.
;;      * Try to display message only if minibuffer and echo area are
;;        not in use already.
;;      * Dirty workaround for newer versions of xml.el: Remove
;;        whitespace in rdf.
;;      * Tested with Emacs 21.3.2 and CVS-snapshot of 2003-06-21.

;; 0.91 * First bugfix: *newsticker* is read-only.

;; 0.9  * First release.
;;      * Tested with Emacs 21.3.2 and wget 1.8.2.

;; ======================================================================
;;; To Do:

;; * Image handling for XEmacs (create-image does not exist)

;; ======================================================================
;;; Code:

(require 'derived)
(require 'xml)

;; Silence warnings
(defvar tool-bar-map)
(defvar w3-mode-map)
(defvar w3m-minor-mode-map)

;; ======================================================================
;;; Newsticker status
;; ======================================================================

(defvar newsticker--retrieval-timer-list nil
  "List of timers for news retrieval.
This is an alist, each element consisting of (feed-name . timer).")

(defvar newsticker--display-timer nil
  "Timer for newsticker display.")

;;;###autoload
(defun newsticker-running-p ()
  "Check whether newsticker is running.
Return t if newsticker is running, nil otherwise.  Newsticker is
considered to be running if the newsticker timer list is not empty."
  (> (length newsticker--retrieval-timer-list) 0))

;;;###autoload
(defun newsticker-ticker-running-p ()
  "Check whether newsticker's actual ticker is running.
Return t if ticker is running, nil otherwise.  Newsticker is
considered to be running if the newsticker timer list is not
empty."
  (timerp newsticker--display-timer))

;; ======================================================================
;;; Customizables
;; ======================================================================
(defgroup newsticker nil
  "Aggregator for RSS and Atom feeds."
  :group 'applications)

(defconst newsticker--raw-url-list-defaults
  '(("CNET News.com"
     "http://export.cnet.com/export/feeds/news/rss/1,11176,,00.xml")
    ("Debian Security Advisories"
    "http://www.debian.org/security/dsa.en.rdf")
    ("Debian Security Advisories - Long format"
    "http://www.debian.org/security/dsa-long.en.rdf")
    ("Emacs Wiki"
    "http://www.emacswiki.org/cgi-bin/wiki.pl?action=rss"
    nil
    3600)
    ("Freshmeat.net"
    "http://freshmeat.net/backend/fm.rdf")
    ("Kuro5hin.org"
    "http://www.kuro5hin.org/backend.rdf")
    ("LWN (Linux Weekly News)"
    "http://lwn.net/headlines/rss")
    ("NewsForge"
    "http://newsforge.com/index.rss")
    ("NY Times: Technology"
    "http://partners.userland.com/nytRss/technology.xml")
    ("NY Times"
    "http://partners.userland.com/nytRss/nytHomepage.xml")
    ("Quote of the day"
    "http://www.quotationspage.com/data/qotd.rss"
    "07:00"
    86400)
    ("The Register"
    "http://www.theregister.co.uk/tonys/slashdot.rdf")
    ("slashdot"
    "http://slashdot.org/index.rss"
    nil
    3600)                        ;/. will ban you if under 3600 seconds!
    ("Wired News"
    "http://www.wired.com/news_drop/netcenter/netcenter.rdf")
    ("Heise News (german)"
    "http://www.heise.de/newsticker/heise.rdf")
    ("Tagesschau (german)"
    "http://www.tagesschau.de/newsticker.rdf"
    nil
    1800)
    ("Telepolis (german)"
    "http://www.heise.de/tp/news.rdf"))
  "Default URL list in raw form.
This list is fed into defcustom via `newsticker--splicer'.")

(defun newsticker--splicer (item)
  "Convert ITEM for splicing into `newsticker-url-list-defaults'."
  (let ((result (list 'list :tag (nth 0 item) (list 'const (nth 0 item))))
        (element (cdr item)))
    (while element
      (setq result (append result (list (list 'const (car element)))))
      (setq element (cdr element)))
    result))

;; ======================================================================
;;; Customization
;; ======================================================================
(defun newsticker--set-customvar (symbol value)
  "Set newsticker-variable SYMBOL value to VALUE.

Calls all necessary actions which are necessary in order to make
the new value effective.  Changing `newsticker-url-list', for example,
will re-start the retrieval-timers."
  (unless (condition-case nil
              (eq (symbol-value symbol) value)
            (error nil))
    (set symbol value)
    (cond ((eq symbol 'newsticker-sort-method)
           (when (fboundp 'newsticker--cache-sort)
             (message "Applying new sort method...")
             (newsticker--cache-sort)
             (newsticker--buffer-set-uptodate nil)
             (message "Applying new sort method...done")))
          ((memq symbol '(newsticker-url-list-defaults
                          newsticker-url-list
                          newsticker-retrieval-interval))
           (when (and (fboundp 'newsticker-running-p)
                      (newsticker-running-p))
             (message "Restarting newsticker")
             (newsticker-stop)
             (newsticker-start)))
          ((eq symbol 'newsticker-display-interval)
           (when (and (fboundp 'newsticker-running-p)
                      (newsticker-running-p))
             (message "Restarting ticker")
             (newsticker-stop-ticker)
             (newsticker-start-ticker)
             (message "")))
          ((memq symbol '(newsticker-hide-old-items-in-echo-area
                          newsticker-hide-obsolete-items-in-echo-area
                          newsticker-hide-immortal-items-in-echo-area))
           (when (and (fboundp 'newsticker-running-p)
                      (newsticker-running-p))
             (message "Restarting newsticker")
             (newsticker-stop-ticker)
             (newsticker--ticker-text-setup)
             (newsticker-start-ticker)
             (message "")))
          ((memq symbol '(newsticker-hide-old-items-in-newsticker-buffer
                          newsticker-show-descriptions-of-new-items))
           (when (fboundp 'newsticker--buffer-set-uptodate)
             (newsticker--buffer-set-uptodate nil)))
          ((memq symbol '(newsticker-heading-format
                          newsticker-item-format
                          newsticker-desc-format
                          newsticker-date-format
                          newsticker-statistics-format
                          newsticker-justification
                          newsticker-use-full-width
                          newsticker-html-renderer
                          newsticker-feed-face
                          newsticker-new-item-face
                          newsticker-old-item-face
                          newsticker-immortal-item-face
                          newsticker-obsolete-item-face
                          newsticker-date-face
                          newsticker-statistics-face
                          ;;newsticker-default-face
                          ))
           (when (fboundp 'newsticker--forget-preformatted)
             (newsticker--forget-preformatted)))
          (t
           (error "Ooops %s" symbol)))))

;; customization group feed
(defgroup newsticker-feed nil
  "Settings for news feeds."
  :group 'newsticker)

(defcustom newsticker-url-list-defaults
 '(("Emacs Wiki"
    "http://www.emacswiki.org/cgi-bin/wiki.pl?action=rss"
    nil
    3600))
  "A customizable list of news feeds to select from.
These were mostly extracted from the Radio Community Server at
http://subhonker6.userland.com/rcsPublic/rssHotlist.

You may add other entries in `newsticker-url-list'."
  :type `(set ,@(mapcar `newsticker--splicer
                        newsticker--raw-url-list-defaults))
  :set 'newsticker--set-customvar
  :group 'newsticker-feed)

(defcustom newsticker-url-list nil
  "The news feeds which you like to watch.

This alist will be used in addition to selection made customizing
`newsticker-url-list-defaults'.

This is an alist.  Each element consists of two items: a LABEL and a URL,
optionally followed by a START-TIME, INTERVAL specifier and WGET-ARGUMENTS.

The LABEL gives the name of the news feed.  It can be an arbitrary string.

The URL gives the location of the news feed.  It must point to a valid
RSS or Atom file.  The file is retrieved by calling wget, or whatever you
specify as `newsticker-wget-name'.

The START-TIME can be either a string, or nil.  If it is a string it
specifies a fixed time at which this feed shall be retrieved for the
first time.  (Examples: \"11:00pm\", \"23:00\".)  If it is nil (or
unspecified), this feed will be retrieved immediately after calling
`newsticker-start'.

The INTERVAL specifies the time between retrievals for this feed.  If it
is nil (or unspecified) the default interval value as set in
`newsticker-retrieval-interval' is used.

\(newsticker.el calls `run-at-time'. The newsticker-parameters START-TIME
and INTERVAL correspond to the `run-at-time'-parameters TIME and REPEAT.)

WGET-ARGUMENTS specifies arguments for wget (see `newsticker-wget-name')
which apply for this feed only, overriding the value of
`newsticker-wget-arguments'."
  :type '(repeat (list :tag "News feed"
                       (string :tag "Label")
                       (string :tag "URI")
                       (choice :tag "Start"
                               (const   :tag "Default" nil)
                               (string  :tag "Fixed Time"))
                       (choice :tag "Interval"
                               (const   :tag "Default" nil)
                               (const   :tag "Hourly" 3600)
                               (const   :tag "Daily" 86400)
                               (const   :tag "Weekly" 604800)
                               (integer :tag "Interval"))
                       (choice :tag "Wget Arguments"
                               (const  :tag "Default arguments" nil)
                               (repeat :tag "Special arguments" string))))
  :set 'newsticker--set-customvar
  :group 'newsticker-feed)

(defcustom newsticker-wget-name
  "wget"
  "Name of the program which is called to retrieve news from the web.
The canonical choice is wget but you may take any other program which is
able to return the contents of a news feed file on stdout."
  :type 'string
  :group 'newsticker-feed)

(defcustom newsticker-wget-arguments
  '("-q" "-O" "-")
  "Arguments which are passed to wget.
There is probably no reason to change the default settings, unless you
are living behind a firewall."
  :type '(repeat (string :tag "Argument"))
  :group 'newsticker-feed)

(defcustom newsticker-retrieval-interval
  3600
  "Time interval for retrieving new news items (seconds).
If this value is not positive (i.e. less than or equal to 0)
items are retrieved only once!
Please note that some feeds, e.g. Slashdot, will ban you if you
make it less than 1800 seconds (30 minutes)!"
  :type '(choice :tag "Interval"
                 (const   :tag "No automatic retrieval" 0)
                 (const   :tag "Hourly" 3600)
                 (const   :tag "Daily" 86400)
                 (const   :tag "Weekly" 604800)
                 (integer :tag "Interval"))
  :set 'newsticker--set-customvar
  :group 'newsticker-feed)

(defcustom newsticker-desc-comp-max
  100
  "Relevant length of headline descriptions.
This value gives the maximum number of characters which will be
taken into account when newsticker compares two headline
descriptions."
  :type 'integer
  :group 'newsticker-feed)

;; customization group behaviour
(defgroup newsticker-headline-processing nil
  "Settings for the automatic processing of headlines."
  :group 'newsticker)

(defcustom newsticker-automatically-mark-items-as-old
  t
  "Decides whether to automatically mark items as old.
If t a new item is considered as new only after its first retrieval.  As
soon as it is retrieved a second time, it becomes old.  If not t all
items stay new until you mark them as old.  This is done in the
*newsticker* buffer."
  :type 'boolean
  :group 'newsticker-headline-processing)

(defcustom newsticker-automatically-mark-visited-items-as-old
  t
  "Decides whether to automatically mark visited items as old.
If t an item is marked as old as soon as the associated link is
visited, i.e. after pressing RET or mouse2 on the item's
headline."

  :type 'boolean
  :group 'newsticker-headline-processing)

(defcustom newsticker-keep-obsolete-items
  t
  "Decides whether to keep unread items which have been removed from feed.
If t a new item, which has been removed from the feed, is kept in
the cache until it is marked as read."
  :type 'boolean
  :group 'newsticker-headline-processing)

(defcustom newsticker-obsolete-item-max-age
  (* 60 60 24)
  "Maximal age of obsolete items, in seconds.
Obsolete items which are older than this value will be silently
deleted at the next retrieval."
  :type 'integer
  :group 'newsticker-headline-processing)

(defcustom newsticker-auto-mark-filter-list
  nil
  "A list of filters for automatically marking headlines.

This is an alist of the form (FEED-NAME PATTERN-LIST).  I.e. each
element consists of a FEED-NAME a PATTERN-LIST.  Each element of
the pattern-list has the form (AGE TITLE-OR-DESCRIPTION REGEXP).
AGE must be one of the symbols 'old or 'immortal.
TITLE-OR-DESCRIPTION must be on of the symbols 'title,
'description, or 'all.  REGEXP is a regular expression, i.e. a
string.

This filter is checked after a new headline has been retrieved.
If FEED-NAME matches the name of the corresponding news feed, the
pattern-list is checked: The new headline will be marked as AGE
if REGEXP matches the headline's TITLE-OR-DESCRIPTION.

If, for example, `newsticker-auto-mark-filter-list' looks like
 \((slashdot ('old 'title \"^Forget me!$\") ('immortal 'title \"Read me\")
  \('immortal 'all \"important\"))))

then all articles from slashdot are marked as old if they have
the title \"Forget me!\".  All articles with a title containing
the string \"Read me\" are marked as immortal.  All articles which
contain the string \"important\" in their title or their
description are marked as immortal."
  :type '(repeat (list :tag "Auto mark filter"
                       (string :tag "Feed name")
                       (repeat
                        (list :tag "Filter element"
                              (choice
                               :tag "Auto-assigned age"
                               (const :tag "Old" old)
                               (const :tag "Immortal" immortal))
                              (choice
                               :tag "Title/Description"
                               (const :tag "Title" title)
                               (const :tag "Description" description)
                               (const :tag "All" all))
                              (string :tag "Regexp")))))
  :group 'newsticker-headline-processing)

;; customization group layout
(defgroup newsticker-layout nil
  "Settings for layout of the feed reader."
  :group 'newsticker)

(defcustom newsticker-sort-method
  'sort-by-original-order
  "Sort method for news items.
The following sort methods are available:
* `sort-by-original-order' keeps the order in which the items
  appear in the headline file (please note that for immortal items,
  which have been removed from the news feed, there is no original
  order),
* `sort-by-time' looks at the time at which an item has been seen
  the first time.  The most recent item is put at top,
* `sort-by-title' will put the items in an alphabetical order."
  :type '(choice
          (const :tag "Keep original order" sort-by-original-order)
          (const :tag "Sort by time"        sort-by-time)
          (const :tag "Sort by title"       sort-by-title))
  :set 'newsticker--set-customvar
  :group 'newsticker-layout)

(defcustom newsticker-hide-old-items-in-newsticker-buffer
  nil
  "Decides whether to automatically hide old items in the *newsticker* buffer.
If set to t old items will be completely folded and only new
items will show up in the *newsticker* buffer.  Otherwise old as
well as new items will be visible."
  :type 'boolean
  :set 'newsticker--set-customvar
  :group 'newsticker-layout)

(defcustom newsticker-show-descriptions-of-new-items
  t
  "Whether to automatically show descriptions of new items in *newsticker*.
If set to t old items will be folded and new items will be
unfolded.  Otherwise old as well as new items will be folded."
  :type 'boolean
  :set 'newsticker--set-customvar
  :group 'newsticker-layout)

(defcustom newsticker-heading-format
  "%l
%t %d %s"
  "Format string for feed headings.
The following printf-like specifiers can be used:
%d  The date the feed was retrieved.  See `newsticker-date-format'.
%l  The logo (image) of the feed.  Most news feeds provide a small
    image as logo.  Newsticker can display them, if Emacs can --
    see `image-types' for a list of supported image types.
%L  The logo (image) of the feed.  If the logo is not available
    the title of the feed is used.
%s  The statistical data of the feed.  See `newsticker-statistics-format'.
%t  The title of the feed, i.e. its name."
  :type 'string
  :set 'newsticker--set-customvar
  :group 'newsticker-layout)

(defcustom newsticker-item-format
  "%t %d"
  "Format string for news item headlines.
The following printf-like specifiers can be used:
%d  The date the item was (first) retrieved.  See `newsticker-date-format'.
%l  The logo (image) of the feed.  Most news feeds provide a small
    image as logo.  Newsticker can display them, if Emacs can --
    see `image-types' for a list of supported image types.
%L  The logo (image) of the feed.  If the logo is not available
    the title of the feed is used.
%t  The title of the item."
  :type 'string
  :set 'newsticker--set-customvar
  :group 'newsticker-layout)

(defcustom newsticker-desc-format
  "%d %c"
  "Format string for news descriptions (contents).
The following printf-like specifiers can be used:
%c  The contents (description) of the item.
%d  The date the item was (first) retrieved.  See
    `newsticker-date-format'."
  :type 'string
  :set 'newsticker--set-customvar
  :group 'newsticker-layout)

(defcustom newsticker-date-format
  "(%A, %H:%M)"
  "Format for the date part in item and feed lines.
See `format-time-string' for a list of valid specifiers."
  :type 'string
  :set 'newsticker--set-customvar
  :group 'newsticker-layout)

(defcustom newsticker-statistics-format
  "[%n + %i + %o + %O = %a]"
  "Format for the statistics part in feed lines.
The following printf-like specifiers can be used:
%a  The number of all items in the feed.
%i  The number of immortal items in the feed.
%n  The number of new items in the feed.
%o  The number of old items in the feed.
%O  The number of obsolete items in the feed."
  :type 'string
  :set 'newsticker--set-customvar
  :group 'newsticker-layout)

(defcustom newsticker-show-all-news-elements
  nil
  "Show all news elements."
  :type 'boolean
  ;;:set 'newsticker--set-customvar
  :group 'newsticker-layout)

;; image related things
(defcustom newsticker-enable-logo-manipulations
  t
  "If non-nil newsticker manipulates logo images.
This enables the following image properties: heuristic mask for all
logos, and laplace-conversion for images without new items."
  :type 'boolean
  :group 'newsticker-layout)


;; rendering
(defcustom newsticker-justification
  'left
  "How to fill item descriptions.
If non-nil newsticker calls `fill-region' to wrap long lines in
item descriptions.  However, if an item description contains HTML
text and `newsticker-html-renderer' is non-nil, filling is not
done."
  :type '(choice :tag "Justification"
                 (const :tag "No filling" nil)
                 (const :tag "Left"       left)
                 (const :tag "Right"      right)
                 (const :tag "Center"     center)
                 (const :tag "Full"       full))
  :set 'newsticker--set-customvar
  :group 'newsticker-layout)

(defcustom newsticker-use-full-width
  t
  "Decides whether to use the full window width when filling.
If non-nil newsticker sets `fill-column' so that the whole
window is used when filling.  See also `newsticker-justification'."
  :type 'boolean
  :set 'newsticker--set-customvar
  :group 'newsticker-layout)

(defcustom newsticker-html-renderer
  nil
  "Function for rendering HTML contents.
If non-nil, newsticker.el will call this function whenever it finds
HTML-like tags in item descriptions.  Possible functions are, for
example, `w3m-region', `w3-region', and (if you have htmlr.el installed)
`newsticker-htmlr-render'.

In order to make sure that the HTML renderer is loaded when you
run newsticker, you should add one of the following statements to
your .emacs.  If you use w3m,

  (autoload 'w3m-region \"w3m\"
    \"Render region in current buffer and replace with result.\" t)

or, if you use w3,

  (require 'w3-auto)

or, if you use htmlr

  (require 'htmlr)"
  :type '(choice :tag "Function"
                 (const :tag "None" nil)
                 (const :tag "w3" w3-region)
                 (const :tag "w3m" w3m-region)
                 (const :tag "htmlr" newsticker-htmlr-render))
  :set 'newsticker--set-customvar
  :group 'newsticker-layout)


;; faces
(defgroup newsticker-faces nil
  "Settings for the faces of the feed reader."
  :group 'newsticker-layout)

(defface newsticker-feed-face
  '((((class color) (background dark))
     (:family "helvetica" :bold t :height 1.2 :foreground "misty rose"))
    (((class color) (background light))
     (:family "helvetica" :bold t :height 1.2 :foreground "black")))
  "Face for news feeds."
  :group 'newsticker-faces)

(defface newsticker-new-item-face
  '((((class color) (background dark))
     (:family "helvetica" :bold t))
    (((class color) (background light))
     (:family "helvetica" :bold t)))
  "Face for new news items."
  :group 'newsticker-faces)

(defface newsticker-old-item-face
  '((((class color) (background dark))
     (:family "helvetica" :bold t :foreground "orange3"))
    (((class color) (background light))
     (:family "helvetica" :bold t :foreground "red4")))
  "Face for old news items."
  :group 'newsticker-faces)

(defface newsticker-immortal-item-face
  '((((class color) (background dark))
     (:family "helvetica" :bold t :italic t :foreground "orange"))
    (((class color) (background light))
     (:family "helvetica" :bold t :italic t :foreground "blue")))
  "Face for immortal news items."
  :group 'newsticker-faces)

(defface newsticker-obsolete-item-face
  '((((class color) (background dark))
     (:family "helvetica" :bold t :strike-through t))
    (((class color) (background light))
     (:family "helvetica" :bold t :strike-through t)))
  "Face for old news items."
  :group 'newsticker-faces)

(defface newsticker-date-face
  '((((class color) (background dark))
     (:family "helvetica" :italic t :height 0.8))
    (((class color) (background light))
     (:family "helvetica" :italic t :height 0.8)))
  "Face for newsticker dates."
  :group 'newsticker-faces)

(defface newsticker-statistics-face
  '((((class color) (background dark))
     (:family "helvetica" :italic t :height 0.8))
    (((class color) (background light))
     (:family "helvetica" :italic t :height 0.8)))
  "Face for newsticker dates."
  :group 'newsticker-faces)

(defface newsticker-enclosure-face
  '((((class color) (background dark))
     (:bold t :background "orange"))
    (((class color) (background light))
     (:bold t :background "orange")))
  "Face for enclosed elements."
  :group 'newsticker-faces)

(defface newsticker-extra-face
  '((((class color) (background dark))
     (:italic t :foreground "gray50" :height 0.8))
    (((class color) (background light))
     (:italic t :foreground "gray50" :height 0.8)))
  "Face for newsticker dates."
  :group 'newsticker-faces)

;; (defface newsticker-default-face
;;   '((((class color) (background dark))
;;      (:inherit default))
;;     (((class color) (background light))
;;      (:inherit default)))
;;   "Face for the description of news items."
;;   ;;:set 'newsticker--set-customvar
;;   :group 'newsticker-faces)


;; customization group ticker
(defgroup newsticker-ticker nil
  "Settings for the headline ticker."
  :group 'newsticker)

(defcustom newsticker-display-interval
  0.3
  "Time interval for displaying news items in the echo area (seconds).
If equal or less than 0 no messages are shown in the echo area.  For
smooth display (see `newsticker-scroll-smoothly') a value of 0.3 seems
reasonable.  For non-smooth display a value of 10 is a good starting
point."
  :type 'number
  :set 'newsticker--set-customvar
  :group 'newsticker-ticker)

(defcustom newsticker-scroll-smoothly
  t
  "Decides whether to flash or scroll news items.
If t the news headlines are scrolled (more-or-less) smoothly in the echo
area.  If nil one headline after another is displayed in the echo area.
The variable `newsticker-display-interval' determines how fast this
display moves/changes and whether headlines are shown in the echo area
at all.  If you change `newsticker-scroll-smoothly' you should also change
`newsticker-display-interval'."
  :type 'boolean
  :group 'newsticker-ticker)

(defcustom newsticker-hide-immortal-items-in-echo-area
  t
  "Decides whether to show immortal/non-expiring news items in the ticker.
If t the echo area will not show immortal items.  See also
`newsticker-hide-old-items-in-echo-area'."
  :type 'boolean
  :set 'newsticker--set-customvar
  :group 'newsticker-ticker)

(defcustom newsticker-hide-old-items-in-echo-area
  t
  "Decides whether to show only the newest news items in the ticker.
If t the echo area will show only new items, i.e. only items which have
been added between the last two retrievals."
  :type 'boolean
  :set 'newsticker--set-customvar
  :group 'newsticker-ticker)

(defcustom newsticker-hide-obsolete-items-in-echo-area
  t
  "Decides whether to show obsolete items items in the ticker.
If t the echo area will not show obsolete items.  See also
`newsticker-hide-old-items-in-echo-area'."
  :type 'boolean
  :set 'newsticker--set-customvar
  :group 'newsticker-ticker)

(defgroup newsticker-hooks nil
  "Settings for newsticker hooks."
  :group 'newsticker)

(defcustom newsticker-start-hook
  nil
  "Hook run when starting newsticker.
This hook is run at the very end of `newsticker-start'."
  :options '(newsticker-start-ticker)
  :type 'hook
  :group 'newsticker-hooks)

(defcustom newsticker-stop-hook
  nil
  "Hook run when stopping newsticker.
This hook is run at the very end of `newsticker-stop'."
  :options nil
  :type 'hook
  :group 'newsticker-hooks)

(defcustom newsticker-new-item-functions
  nil
  "List of functions run after a new headline has been retrieved.
Each function is called with the following three arguments:
FEED  the name of the corresponding news feed,
TITLE the title of the headline,
DESC  the decoded description of the headline.

See `newsticker-download-images', and
`newsticker-download-enclosures' for sample functions.

Please note that these functions are called only once for a
headline after it has been retrieved for the first time."
  :type 'hook
  :options '(newsticker-download-images
             newsticker-download-enclosures)
  :group 'newsticker-hooks)

(defcustom newsticker-select-item-hook
  'newsticker--buffer-make-item-completely-visible
  "List of functions run after a headline has been selected.
Each function is called after one of `newsticker-next-item',
`newsticker-next-new-item', `newsticker-previous-item',
`newsticker-previous-new-item' has been called.

The default value 'newsticker--buffer-make-item-completely-visible
assures that the current item is always completely visible."
  :type 'hook
  :options '(newsticker--buffer-make-item-completely-visible)
  :group 'newsticker-hooks)

(defcustom newsticker-select-feed-hook
  'newsticker--buffer-make-item-completely-visible
  "List of functions run after a feed has been selected.
Each function is called after one of `newsticker-next-feed', and
`newsticker-previous-feed' has been called.

The default value 'newsticker--buffer-make-item-completely-visible
assures that the current feed is completely visible."
  :type 'hook
  :options '(newsticker--buffer-make-item-completely-visible)
  :group 'newsticker-hooks)

(defcustom newsticker-buffer-change-hook
  'newsticker-w3m-show-inline-images
  "List of functions run after the newsticker buffer has been updated.
Each function is called after `newsticker-buffer-update' has been called.

The default value '`newsticker-w3m-show-inline-images' loads inline
images."
  :type 'hook
  :group 'newsticker-hooks)

(defcustom newsticker-narrow-hook
  'newsticker-w3m-show-inline-images
  "List of functions run after narrowing in newsticker buffer has changed.
Each function is called after
`newsticker-toggle-auto-narrow-to-feed' or
`newsticker-toggle-auto-narrow-to-item' has been called.

The default value '`newsticker-w3m-show-inline-images' loads inline
images."
  :type 'hook
  :group 'newsticker-hooks)

(defgroup newsticker-miscellaneous nil
  "Miscellaneous newsticker settings."
  :group 'newsticker)

(defcustom newsticker-cache-filename
  "~/.newsticker-cache"
  "Name of the newsticker cache file."
  :type 'string
  :group 'newsticker-miscellaneous)

(defcustom newsticker-imagecache-dirname
  "~/.newsticker-images"
  "Name of the directory where newsticker stores cached images."
  :type 'string
  :group 'newsticker-miscellaneous)

;; debugging
(defcustom newsticker-debug
  nil
  "Enables some features needed for debugging newsticker.el.

If set to t newsticker.el will print lots of debugging messages, and the
buffers *newsticker-wget-<feed>* will not be closed."
  :type 'boolean
  ;;:set 'newsticker--set-customvar
  :group 'newsticker-miscellaneous)

;; ======================================================================
;;; Compatibility section, XEmacs, Emacs
;; ======================================================================
(unless (fboundp 'time-add)
  (require 'time-date);;FIXME
  (defun time-add (t1 t2)
    (seconds-to-time (+ (time-to-seconds t1) (time-to-seconds t2)))))

(unless (fboundp 'match-string-no-properties)
  (defalias 'match-string-no-properties 'match-string))

(when (featurep 'xemacs)
  (unless (fboundp 'replace-regexp-in-string)
    (defun replace-regexp-in-string (re rp st)
      (save-match-data ;; apparently XEmacs needs save-match-data
	(replace-in-string st re rp)))))

;; copied from subr.el
(unless (fboundp 'add-to-invisibility-spec)
  (defun add-to-invisibility-spec (arg)
    "Add elements to `buffer-invisibility-spec'.
See documentation for `buffer-invisibility-spec' for the kind of elements
that can be added."
    (if (eq buffer-invisibility-spec t)
        (setq buffer-invisibility-spec (list t)))
    (setq buffer-invisibility-spec
          (cons arg buffer-invisibility-spec))))

;; copied from subr.el
(unless (fboundp 'remove-from-invisibility-spec)
  (defun remove-from-invisibility-spec (arg)
    "Remove elements from `buffer-invisibility-spec'."
    (if (consp buffer-invisibility-spec)
        (setq buffer-invisibility-spec
              (delete arg buffer-invisibility-spec)))))

;; ======================================================================
;;; Internal variables
;; ======================================================================
(defvar newsticker--item-list nil
  "List of newsticker items.")
(defvar newsticker--item-position 0
  "Actual position in list of newsticker items.")
(defvar newsticker--prev-message "There was no previous message yet!"
  "Last message that the newsticker displayed.")
(defvar newsticker--scrollable-text ""
  "The text which is scrolled smoothly in the echo area.")
(defvar newsticker--buffer-uptodate-p nil
  "Tells whether the newsticker buffer is up to date.")
(defvar newsticker--latest-update-time (current-time)
  "The time at which the latest news arrived.")
(defvar newsticker--process-ids nil
  "List of PIDs of active newsticker processes.")

(defvar newsticker--cache nil "Cached newsticker data.
This is a list of the form

 ((label1
   (title description link time age index preformatted-contents
    preformatted-title)
   ...)
  (label2
   (title description link time age index preformatted-contents
    preformatted-title)
   ...)
  ...)

where LABEL is a symbol.  TITLE, DESCRIPTION, and LINK are
strings.  TIME is a time value as returned by `current-time'.
AGE is a symbol: 'new, 'old, 'immortal, and 'obsolete denote
ordinary news items, whereas 'feed denotes an item which is not a
headline but describes the feed itself.  INDEX denotes the
original position of the item -- used for restoring the original
order.  PREFORMATTED-CONTENTS and PREFORMATTED-TITLE hold the
formatted contents of the item's description and title.  This
speeds things up if HTML rendering is used, which is rather
slow.")

(defvar newsticker--auto-narrow-to-feed nil
  "Automatically narrow to current news feed.
If non-nil only the items of the current news feed are visible.")

(defvar newsticker--auto-narrow-to-item nil
  "Automatically narrow to current news item.
If non-nil only the current headline is visible.")

(defconst newsticker--error-headline
  "[COULD NOT DOWNLOAD HEADLINES!]"
  "Title of error headline which will be inserted if news retrieval fails.")

;; ======================================================================
;;; Toolbar
;; ======================================================================
(defconst newsticker--next-item-image
  (if (fboundp 'create-image)
      (create-image "/* XPM */
static char * next_xpm[] = {
\"24 24 42 1\",
\" 	c None\",
\".	c #000000\",
\"+	c #7EB6DE\",
\"@	c #82BBE2\",
\"#	c #85BEE4\",
\"$	c #88C1E7\",
\"%	c #8AC3E8\",
\"&	c #87C1E6\",
\"*	c #8AC4E9\",
\"=	c #8CC6EA\",
\"-	c #8CC6EB\",
\";	c #88C2E7\",
\">	c #8BC5E9\",
\",	c #8DC7EB\",
\"'	c #87C0E6\",
\")	c #8AC4E8\",
\"!	c #8BC5EA\",
\"~	c #8BC4E9\",
\"{	c #88C1E6\",
\"]	c #89C3E8\",
\"^	c #86BFE5\",
\"/	c #83BBE2\",
\"(	c #82BBE1\",
\"_	c #86C0E5\",
\":	c #87C0E5\",
\"<	c #83BCE2\",
\"[	c #81B9E0\",
\"}	c #81BAE1\",
\"|	c #78B0D9\",
\"1	c #7BB3DB\",
\"2	c #7DB5DD\",
\"3	c #7DB6DD\",
\"4	c #72A9D4\",
\"5	c #75ACD6\",
\"6	c #76AED7\",
\"7	c #77AFD8\",
\"8	c #6BA1CD\",
\"9	c #6EA4CF\",
\"0	c #6FA6D1\",
\"a	c #6298C6\",
\"b	c #659BC8\",
\"c	c #5C91C0\",
\"                        \",
\"                        \",
\"       .                \",
\"       ..               \",
\"       .+.              \",
\"       .@#.             \",
\"       .#$%.            \",
\"       .&*=-.           \",
\"       .;>,,,.          \",
\"       .;>,,,=.         \",
\"       .')!==~;.        \",
\"       .#{]*%;^/.       \",
\"       .(#_':#<.        \",
\"       .+[@</}.         \",
\"       .|1232.          \",
\"       .4567.           \",
\"       .890.            \",
\"       .ab.             \",
\"       .c.              \",
\"       ..               \",
\"       .                \",
\"                        \",
\"                        \",
\"                        \"};
"
                 'xpm t)
   "Image for the next item button."))

(defconst newsticker--previous-item-image
  (if (fboundp 'create-image)
      (create-image "/* XPM */
static char * previous_xpm[] = {
\"24 24 39 1\",
\" 	c None\",
\".	c #000000\",
\"+	c #7BB3DB\",
\"@	c #83BCE2\",
\"#	c #7FB8DF\",
\"$	c #89C2E7\",
\"%	c #86BFE5\",
\"&	c #83BBE2\",
\"*	c #8CC6EA\",
\"=	c #8BC4E9\",
\"-	c #88C2E7\",
\";	c #85BEE4\",
\">	c #8DC7EB\",
\",	c #89C3E8\",
\"'	c #8AC4E8\",
\")	c #8BC5EA\",
\"!	c #88C1E6\",
\"~	c #8AC4E9\",
\"{	c #8AC3E8\",
\"]	c #86C0E5\",
\"^	c #87C0E6\",
\"/	c #87C0E5\",
\"(	c #82BBE2\",
\"_	c #81BAE1\",
\":	c #7FB7DF\",
\"<	c #7DB6DD\",
\"[	c #7DB5DD\",
\"}	c #7CB4DC\",
\"|	c #79B1DA\",
\"1	c #76ADD7\",
\"2	c #77AFD8\",
\"3	c #73AAD4\",
\"4	c #70A7D1\",
\"5	c #6EA5D0\",
\"6	c #6CA2CE\",
\"7	c #689ECB\",
\"8	c #6399C7\",
\"9	c #6095C4\",
\"0	c #5C90C0\",
\"                        \",
\"                        \",
\"                .       \",
\"               ..       \",
\"              .+.       \",
\"             .@#.       \",
\"            .$%&.       \",
\"           .*=-;.       \",
\"          .>>*,%.       \",
\"         .>>>*,%.       \",
\"        .')**=-;.       \",
\"       .;!,~{-%&.       \",
\"        .;]^/;@#.       \",
\"         .(@&_:+.       \",
\"          .<[}|1.       \",
\"           .2134.       \",
\"            .567.       \",
\"             .89.       \",
\"              .0.       \",
\"               ..       \",
\"                .       \",
\"                        \",
\"                        \",
\"                        \"};
"
                 'xpm t)
   "Image for the previous item button."))

(defconst newsticker--previous-feed-image
  (if (fboundp 'create-image)
      (create-image "/* XPM */
static char * prev_feed_xpm[] = {
\"24 24 52 1\",
\" 	c None\",
\".	c #000000\",
\"+	c #70A7D2\",
\"@	c #75ADD6\",
\"#	c #71A8D3\",
\"$	c #79B1DA\",
\"%	c #7BB3DB\",
\"&	c #7DB5DD\",
\"*	c #83BBE2\",
\"=	c #7EB6DE\",
\"-	c #78B0D9\",
\";	c #7FB7DE\",
\">	c #88C2E7\",
\",	c #85BEE4\",
\"'	c #80B9E0\",
\")	c #80B8DF\",
\"!	c #8CC6EA\",
\"~	c #89C3E8\",
\"{	c #86BFE5\",
\"]	c #81BAE1\",
\"^	c #7CB4DC\",
\"/	c #7FB8DF\",
\"(	c #8DC7EB\",
\"_	c #7BB3DC\",
\":	c #7EB7DE\",
\"<	c #8BC4E9\",
\"[	c #8AC4E9\",
\"}	c #8AC3E8\",
\"|	c #87C0E6\",
\"1	c #87C0E5\",
\"2	c #83BCE2\",
\"3	c #75ACD6\",
\"4	c #7FB7DF\",
\"5	c #77AED8\",
\"6	c #71A8D2\",
\"7	c #70A7D1\",
\"8	c #76ADD7\",
\"9	c #6CA2CE\",
\"0	c #699FCC\",
\"a	c #73AAD4\",
\"b	c #6BA1CD\",
\"c	c #669CC9\",
\"d	c #6298C5\",
\"e	c #689ECB\",
\"f	c #6499C7\",
\"g	c #6095C3\",
\"h	c #5C91C0\",
\"i	c #5E93C2\",
\"j	c #5B90C0\",
\"k	c #588CBC\",
\"l	c #578CBC\",
\"m	c #5589BA\",
\"                        \",
\"                        \",
\"     ...          .     \",
\"     .+.         ..     \",
\"     .@.        .#.     \",
\"     .$.       .%@.     \",
\"     .&.      .*=-.     \",
\"     .;.     .>,'%.     \",
\"     .).    .!~{]^.     \",
\"     ./.   .(!~{]_.     \",
\"     .:.  .!!<>,'%.     \",
\"     .&. .~[}>{*=-.     \",
\"     .$.  .|1,2/%@.     \",
\"     .3.   .*]4%56.     \",
\"     .7.    .^$8#9.     \",
\"     .0.     .a7bc.     \",
\"     .d.      .efg.     \",
\"     .h.       .ij.     \",
\"     .k.        .l.     \",
\"     .m.         ..     \",
\"     ...          .     \",
\"                        \",
\"                        \",
\"                        \"};
"
                 'xpm t)
   "Image for the previous feed button."))

(defconst newsticker--next-feed-image
  (if (fboundp 'create-image)
      (create-image "/* XPM */
static char * next_feed_xpm[] = {
\"24 24 57 1\",
\" 	c None\",
\".	c #000000\",
\"+	c #6CA2CE\",
\"@	c #75ADD6\",
\"#	c #71A8D3\",
\"$	c #79B1DA\",
\"%	c #7EB7DE\",
\"&	c #7DB5DD\",
\"*	c #81BAE1\",
\"=	c #85BEE4\",
\"-	c #78B0D9\",
\";	c #7FB7DE\",
\">	c #83BCE3\",
\",	c #87C1E6\",
\"'	c #8AC4E9\",
\")	c #7BB3DB\",
\"!	c #80B8DF\",
\"~	c #88C2E7\",
\"{	c #8BC5E9\",
\"]	c #8DC7EB\",
\"^	c #7CB4DC\",
\"/	c #7FB8DF\",
\"(	c #84BDE3\",
\"_	c #7BB3DC\",
\":	c #83BCE2\",
\"<	c #87C0E6\",
\"[	c #8AC4E8\",
\"}	c #8BC5EA\",
\"|	c #8CC6EA\",
\"1	c #88C1E6\",
\"2	c #89C3E8\",
\"3	c #8AC3E8\",
\"4	c #7EB6DE\",
\"5	c #82BBE1\",
\"6	c #86C0E5\",
\"7	c #87C0E5\",
\"8	c #75ACD6\",
\"9	c #7AB2DA\",
\"0	c #81B9E0\",
\"a	c #82BBE2\",
\"b	c #71A8D2\",
\"c	c #70A7D1\",
\"d	c #74ACD6\",
\"e	c #699FCC\",
\"f	c #6EA5D0\",
\"g	c #72A9D4\",
\"h	c #669CC9\",
\"i	c #6298C5\",
\"j	c #679DCA\",
\"k	c #6BA1CD\",
\"l	c #6095C3\",
\"m	c #5C91C0\",
\"n	c #5F94C2\",
\"o	c #5B90C0\",
\"p	c #588CBC\",
\"q	c #578CBC\",
\"r	c #5589BA\",
\"                        \",
\"                        \",
\"     .          ...     \",
\"     ..         .+.     \",
\"     .@.        .#.     \",
\"     .$%.       .@.     \",
\"     .&*=.      .-.     \",
\"     .;>,'.     .).     \",
\"     .!=~{].    .^.     \",
\"     ./(~{]].   ._.     \",
\"     .%:<[}||.  .).     \",
\"     .&*=12'3~. .-.     \",
\"     .$45=6<7.  .@.     \",
\"     .8940a:.   .b.     \",
\"     .cd-)&.    .+.     \",
\"     .efg8.     .h.     \",
\"     .ijk.      .l.     \",
\"     .mn.       .o.     \",
\"     .p.        .q.     \",
\"     ..         .r.     \",
\"     .          ...     \",
\"                        \",
\"                        \",
\"                        \"};
"
                 'xpm t)
   "Image for the next feed button."))

(defconst newsticker--mark-read-image
  (if (fboundp 'create-image)
      (create-image "/* XPM */
static char * mark_read_xpm[] = {
\"24 24 44 1\",
\" 	c None\",
\".	c #C20000\",
\"+	c #BE0000\",
\"@	c #C70000\",
\"#	c #CE0000\",
\"$	c #C90000\",
\"%	c #BD0000\",
\"&	c #CB0000\",
\"*	c #D10000\",
\"=	c #D70000\",
\"-	c #D30000\",
\";	c #CD0000\",
\">	c #C60000\",
\",	c #D40000\",
\"'	c #DA0000\",
\")	c #DE0000\",
\"!	c #DB0000\",
\"~	c #D60000\",
\"{	c #D00000\",
\"]	c #DC0000\",
\"^	c #E00000\",
\"/	c #E40000\",
\"(	c #E10000\",
\"_	c #DD0000\",
\":	c #D80000\",
\"<	c #E50000\",
\"[	c #E70000\",
\"}	c #E60000\",
\"|	c #E20000\",
\"1	c #E90000\",
\"2	c #E80000\",
\"3	c #E30000\",
\"4	c #DF0000\",
\"5	c #D90000\",
\"6	c #CC0000\",
\"7	c #C10000\",
\"8	c #C30000\",
\"9	c #BF0000\",
\"0	c #B90000\",
\"a	c #BC0000\",
\"b	c #BB0000\",
\"c	c #B80000\",
\"d	c #B50000\",
\"e	c #B70000\",
\"                        \",
\"                        \",
\"                        \",
\"    .              +    \",
\"   +@#            $.%   \",
\"    &*=          -;>    \",
\"     ,')        !~{     \",
\"      ]^/      (_:      \",
\"       (<[    }|)       \",
\"        <[1  2<|        \",
\"         }222[<         \",
\"          }}}<          \",
\"          333|          \",
\"         _4^4)]         \",
\"        ~:'  5=-        \",
\"       6{-    *#$       \",
\"      7>$      @89      \",
\"     0a+        %bc     \",
\"    ddc          edd    \",
\"   ddd            ddd   \",
\"    d              d    \",
\"                        \",
\"                        \",
\"                        \"};
"
                 'xpm t)
   "Image for the next feed button."))

(defconst newsticker--mark-immortal-image
  (if (fboundp 'create-image)
      (create-image "/* XPM */
static char * mark_immortal_xpm[] = {
\"24 24 93 2\",
\"  	c None\",
\". 	c #171717\",
\"+ 	c #030303\",
\"@ 	c #000000\",
\"# 	c #181818\",
\"$ 	c #090909\",
\"% 	c #FFC960\",
\"& 	c #FFCB61\",
\"* 	c #FFCB62\",
\"= 	c #FFC961\",
\"- 	c #FFC75F\",
\"; 	c #FFC65E\",
\"> 	c #FFCA61\",
\", 	c #FFCD63\",
\"' 	c #FFCF65\",
\") 	c #FFD065\",
\"! 	c #FFCE64\",
\"~ 	c #FFC35C\",
\"{ 	c #FFC45D\",
\"] 	c #FFD166\",
\"^ 	c #FFD267\",
\"/ 	c #FFD368\",
\"( 	c #FFD167\",
\"_ 	c #FFC05A\",
\": 	c #010101\",
\"< 	c #040404\",
\"[ 	c #FFCC62\",
\"} 	c #FFD569\",
\"| 	c #FFD56A\",
\"1 	c #FFC860\",
\"2 	c #FFC25B\",
\"3 	c #FFBB56\",
\"4 	c #020202\",
\"5 	c #060606\",
\"6 	c #FFC15B\",
\"7 	c #FFC85F\",
\"8 	c #FFD469\",
\"9 	c #FFD66A\",
\"0 	c #FFBC57\",
\"a 	c #1B1B1B\",
\"b 	c #070707\",
\"c 	c #FFBA55\",
\"d 	c #FFB451\",
\"e 	c #FFB954\",
\"f 	c #FFB350\",
\"g 	c #FFB652\",
\"h 	c #FFBE58\",
\"i 	c #FFCD64\",
\"j 	c #FFD066\",
\"k 	c #FFC059\",
\"l 	c #FFB14E\",
\"m 	c #0B0B0B\",
\"n 	c #FFBB55\",
\"o 	c #FFC15A\",
\"p 	c #FFB552\",
\"q 	c #FFAD4B\",
\"r 	c #080808\",
\"s 	c #FFAF4C\",
\"t 	c #FFB853\",
\"u 	c #FFA948\",
\"v 	c #050505\",
\"w 	c #FFB04E\",
\"x 	c #FFB753\",
\"y 	c #FFBC56\",
\"z 	c #FFC55D\",
\"A 	c #FFC55E\",
\"B 	c #FFC45C\",
\"C 	c #FFBD57\",
\"D 	c #FFB854\",
\"E 	c #FFB34F\",
\"F 	c #FFAB4A\",
\"G 	c #FFA545\",
\"H 	c #FFAA49\",
\"I 	c #FFB04D\",
\"J 	c #FFB551\",
\"K 	c #FFBF58\",
\"L 	c #FFB24F\",
\"M 	c #FFAC4A\",
\"N 	c #FFA646\",
\"O 	c #FFA344\",
\"P 	c #FFA848\",
\"Q 	c #FFB14F\",
\"R 	c #FFAF4D\",
\"S 	c #FFA546\",
\"T 	c #FFA243\",
\"U 	c #FFA445\",
\"V 	c #FFAE4C\",
\"W 	c #FFA444\",
\"X 	c #FFA142\",
\"Y 	c #FF9F41\",
\"Z 	c #0A0A0A\",
\"` 	c #FF9E40\",
\" .	c #FF9F40\",
\"                                                \",
\"                                                \",
\"                                                \",
\"                  . + @ @ + #                   \",
\"              $ @ % & * * = - + +               \",
\"            @ ; > , ' ) ' ! * - ~ @             \",
\"          @ { > ! ] ^ / / ( ' * ; _ :           \",
\"        < _ ; [ ) / } | } / ] , 1 2 3 4         \",
\"        5 6 7 , ] 8 9 9 9 } ^ ! = ~ 0 a         \",
\"      b c 6 - , ] 8 9 9 9 } ^ ! % ~ 0 d 5       \",
\"      : e _ ; * ) / 8 } } / ] , 1 2 3 f 5       \",
\"      : g h { = i j ^ / ^ ] ! * ; k e l m       \",
\"      : f n o ; > , ' ) ' ! * - 2 0 p q r       \",
\"      : s g 0 6 ; % > * * = - ~ h t l u r       \",
\"      v u w x y k ~ z A z B o C D E F G b       \",
\"        5 H I J e 0 h K h C c x L M N .         \",
\"        4 O P q Q d g x g J L R H S T <         \",
\"          @ T U P F q V q M H N W X +           \",
\"            @ Y T O W G G W O X Y @             \",
\"              4 Z ` Y Y Y  .` 4 4               \",
\"                  5 : : @ @ Z                   \",
\"                                                \",
\"                                                \",
\"                                                \"};
"
                 'xpm t)
   "Image for the next feed button."))


(defconst newsticker--narrow-image
  (if (fboundp 'create-image)
      (create-image "/* XPM */
static char * narrow_xpm[] = {
\"24 24 48 1\",
\" 	c None\",
\".	c #000000\",
\"+	c #969696\",
\"@	c #9E9E9E\",
\"#	c #A4A4A4\",
\"$	c #AAAAAA\",
\"%	c #AEAEAE\",
\"&	c #B1B1B1\",
\"*	c #B3B3B3\",
\"=	c #B4B4B4\",
\"-	c #B2B2B2\",
\";	c #AFAFAF\",
\">	c #ABABAB\",
\",	c #A6A6A6\",
\"'	c #A0A0A0\",
\")	c #989898\",
\"!	c #909090\",
\"~	c #73AAD4\",
\"{	c #7AB2DA\",
\"]	c #7FB8DF\",
\"^	c #84BDE3\",
\"/	c #88C2E7\",
\"(	c #8BC5E9\",
\"_	c #8DC7EB\",
\":	c #8CC6EA\",
\"<	c #89C3E8\",
\"[	c #86BFE5\",
\"}	c #81BAE1\",
\"|	c #7BB3DC\",
\"1	c #75ACD6\",
\"2	c #6DA4CF\",
\"3	c #979797\",
\"4	c #A3A3A3\",
\"5	c #A8A8A8\",
\"6	c #ADADAD\",
\"7	c #ACACAC\",
\"8	c #A9A9A9\",
\"9	c #A5A5A5\",
\"0	c #9A9A9A\",
\"a	c #929292\",
\"b	c #8C8C8C\",
\"c	c #808080\",
\"d	c #818181\",
\"e	c #838383\",
\"f	c #848484\",
\"g	c #858585\",
\"h	c #868686\",
\"i	c #828282\",
\"                        \",
\"                        \",
\"                        \",
\"   ..................   \",
\"   .+@#$%&*=*-;>,')!.   \",
\"   ..................   \",
\"                        \",
\"                        \",
\"   ..................   \",
\"   .~{]^/(___:<[}|12.   \",
\"   ..................   \",
\"                        \",
\"                        \",
\"   ..................   \",
\"   .!3@45>666789'0ab.   \",
\"   ..................   \",
\"                        \",
\"                        \",
\"   ..................   \",
\"   .cccdefghhgficccc.   \",
\"   ..................   \",
\"                        \",
\"                        \",
\"                        \"};
"
                 'xpm t)
   "Image for the next feed button."))

(defconst newsticker--get-all-image
  (if (fboundp 'create-image)
      (create-image "/* XPM */
static char * get_all_xpm[] = {
\"24 24 70 1\",
\" 	c None\",
\".	c #000000\",
\"+	c #F3DA00\",
\"@	c #F5DF00\",
\"#	c #F7E300\",
\"$	c #F9E700\",
\"%	c #FAEA00\",
\"&	c #FBEC00\",
\"*	c #FBED00\",
\"=	c #FCEE00\",
\"-	c #FAEB00\",
\";	c #F9E800\",
\">	c #F8E500\",
\",	c #F6E000\",
\"'	c #F4DB00\",
\")	c #F1D500\",
\"!	c #EFD000\",
\"~	c #B7CA00\",
\"{	c #BFD100\",
\"]	c #C5D700\",
\"^	c #CBDB00\",
\"/	c #CFDF00\",
\"(	c #D2E200\",
\"_	c #D4E400\",
\":	c #D3E300\",
\"<	c #D0E000\",
\"[	c #CCDD00\",
\"}	c #C7D800\",
\"|	c #C1D300\",
\"1	c #BACC00\",
\"2	c #B1C500\",
\"3	c #A8BC00\",
\"4	c #20A900\",
\"5	c #22AF00\",
\"6	c #24B500\",
\"7	c #26B900\",
\"8	c #27BC00\",
\"9	c #27BE00\",
\"0	c #28BF00\",
\"a	c #27BD00\",
\"b	c #26BA00\",
\"c	c #25B600\",
\"d	c #23B100\",
\"e	c #21AB00\",
\"f	c #1FA400\",
\"g	c #1C9B00\",
\"h	c #21AA00\",
\"i	c #24B300\",
\"j	c #25B800\",
\"k	c #25B700\",
\"l	c #24B400\",
\"m	c #23B000\",
\"n	c #1FA500\",
\"o	c #1D9E00\",
\"p	c #20A800\",
\"q	c #21AC00\",
\"r	c #23B200\",
\"s	c #22AD00\",
\"t	c #1D9F00\",
\"u	c #20A700\",
\"v	c #1EA100\",
\"w	c #1C9C00\",
\"x	c #1DA000\",
\"y	c #1B9800\",
\"z	c #1A9600\",
\"A	c #1A9700\",
\"B	c #1A9500\",
\"C	c #199200\",
\"D	c #189100\",
\"E	c #178C00\",
\"                        \",
\"                        \",
\"                        \",
\"                        \",
\"   ...................  \",
\"   .+@#$%&*=*&-;>,')!.  \",
\"   ...................  \",
\"                        \",
\"   ...................  \",
\"   .~{]^/(___:<[}|123.  \",
\"   ...................  \",
\"                        \",
\"   ...................  \",
\"    .45678909abcdefg.   \",
\"     .h5icj7jklmeno.    \",
\"      .pq5drrmshft.     \",
\"       .fu4h4pnvw.      \",
\"        .oxvxtwy.       \",
\"         .zAAzB.        \",
\"          .CCD.         \",
\"           .E.          \",
\"            .           \",
\"                        \",
\"                        \"};
"
                 'xpm t)
   "Image for the next feed button."))


(defconst newsticker--update-image
  (if (fboundp 'create-image)
      (create-image "/* XPM */
static char * update_xpm[] = {
\"24 24 37 1\",
\" 	c None\",
\".	c #076D00\",
\"+	c #0A8600\",
\"@	c #0A8800\",
\"#	c #098400\",
\"$	c #087200\",
\"%	c #087900\",
\"&	c #098500\",
\"*	c #098100\",
\"=	c #087600\",
\"-	c #097E00\",
\";	c #097F00\",
\">	c #0A8700\",
\",	c #0A8C00\",
\"'	c #097C00\",
\")	c #098300\",
\"!	c #0A8900\",
\"~	c #0A8E00\",
\"{	c #0B9200\",
\"]	c #087700\",
\"^	c #076E00\",
\"/	c #076C00\",
\"(	c #076B00\",
\"_	c #076A00\",
\":	c #076900\",
\"<	c #076800\",
\"[	c #066700\",
\"}	c #066500\",
\"|	c #066400\",
\"1	c #066300\",
\"2	c #066600\",
\"3	c #066200\",
\"4	c #076700\",
\"5	c #065E00\",
\"6	c #066100\",
\"7	c #065F00\",
\"8	c #066000\",
\"                        \",
\"                        \",
\"                        \",
\"    .    +@@@+#         \",
\"    $% &@      +*       \",
\"    =-#          ;      \",
\"    %*>,          '     \",
\"    ')!~{          =    \",
\"                   ]$   \",
\"   ^                ^   \",
\"   .                .   \",
\"   /                (   \",
\"   _                :   \",
\"   <                [   \",
\"   }                |   \",
\"   [[                   \",
\"    1          $.:23    \",
\"     3          4}35    \",
\"      6          655    \",
\"       76      85 55    \",
\"        5555555    5    \",
\"                        \",
\"                        \",
\"                        \"};
"
                 'xpm t)
   "Image for the update button."))

(defconst newsticker--browse-image
  (if (fboundp 'create-image)
      (create-image "/* XPM */
static char * visit_xpm[] = {
\"24 24 39 1\",
\" 	c None\",
\".	c #000000\",
\"+	c #FFFFFF\",
\"@	c #00E63D\",
\"#	c #00E83E\",
\"$	c #00E73D\",
\"%	c #00E93E\",
\"&	c #00E63C\",
\"*	c #00E53C\",
\"=	c #00E23B\",
\"-	c #00E33B\",
\";	c #00E83D\",
\">	c #00E13A\",
\",	c #00DD38\",
\"'	c #00DE38\",
\")	c #00E23A\",
\"!	c #00E43C\",
\"~	c #00DF39\",
\"{	c #00DB37\",
\"]	c #00D634\",
\"^	c #00D734\",
\"/	c #00E039\",
\"(	c #00DC37\",
\"_	c #00D835\",
\":	c #00D332\",
\"<	c #00CD2F\",
\"[	c #00DB36\",
\"}	c #00D433\",
\"|	c #00CF30\",
\"1	c #00DA36\",
\"2	c #00D936\",
\"3	c #00D533\",
\"4	c #00D131\",
\"5	c #00CE2F\",
\"6	c #00CC2F\",
\"7	c #00CA2D\",
\"8	c #00C62B\",
\"9	c #00C52A\",
\"0	c #00BE27\",
\"                        \",
\"                        \",
\"            .           \",
\"           .+.          \",
\"          .+++.         \",
\"         .++.++.        \",
\"        .++.@.++.       \",
\"       .++.##$.++.      \",
\"      .++.%%%#&.++.     \",
\"     .++.$%%%#*=.++.    \",
\"    .++.-@;##$*>,.++.   \",
\"   .++.')!&@@*=~{].++.  \",
\"  .++.^{~>---)/(_:<.++. \",
\"   .++.^[,~/~'(_}|.++.  \",
\"    .++.]_1[12^:|.++.   \",
\"     .++.:}33:45.++.    \",
\"      .++.<5567.++.     \",
\"       .++.889.++.      \",
\"        .++.0.++.       \",
\"         .++.++.        \",
\"          .+++.         \",
\"           .+.          \",
\"            .           \",
\"                        \"};
"
                 'xpm t)
   "Image for the browse button."))


(defvar newsticker-tool-bar-map
  (if (featurep 'xemacs)
      nil
    (let ((tool-bar-map (make-sparse-keymap)))
      (define-key tool-bar-map [newsticker-sep-1]
        (list 'menu-item "--double-line"))
      (define-key tool-bar-map [newsticker-browse-url]
        (list 'menu-item "newsticker-browse-url" 'newsticker-browse-url
              :visible t
              :help "Browse URL for item at point"
              :image newsticker--browse-image))
      (define-key tool-bar-map [newsticker-buffer-force-update]
        (list 'menu-item "newsticker-buffer-force-update"
              'newsticker-buffer-force-update
              :visible t
              :help "Update newsticker buffer"
              :image newsticker--update-image
              :enable '(not newsticker--buffer-uptodate-p)))
      (define-key tool-bar-map [newsticker-get-all-news]
        (list 'menu-item "newsticker-get-all-news" 'newsticker-get-all-news
              :visible t
              :help "Get news for all feeds"
              :image newsticker--get-all-image))
      (define-key tool-bar-map [newsticker-mark-item-at-point-as-read]
        (list 'menu-item "newsticker-mark-item-at-point-as-read"
              'newsticker-mark-item-at-point-as-read
              :visible t
              :image newsticker--mark-read-image
              :help "Mark current item as read"
              :enable '(newsticker-item-not-old-p)))
      (define-key tool-bar-map [newsticker-mark-item-at-point-as-immortal]
        (list 'menu-item "newsticker-mark-item-at-point-as-immortal"
              'newsticker-mark-item-at-point-as-immortal
              :visible t
              :image newsticker--mark-immortal-image
              :help "Mark current item as immortal"
              :enable '(newsticker-item-not-immortal-p)))
      (define-key tool-bar-map [newsticker-toggle-auto-narrow-to-feed]
        (list 'menu-item "newsticker-toggle-auto-narrow-to-feed"
              'newsticker-toggle-auto-narrow-to-feed
              :visible t
              :help "Toggle visibility of other feeds"
              :image newsticker--narrow-image))
      (define-key tool-bar-map [newsticker-next-feed]
        (list 'menu-item "newsticker-next-feed" 'newsticker-next-feed
              :visible t
              :help "Go to next feed"
              :image newsticker--next-feed-image
              :enable '(newsticker-next-feed-available-p)))
      (define-key tool-bar-map [newsticker-next-item]
        (list 'menu-item "newsticker-next-item" 'newsticker-next-item
              :visible t
              :help "Go to next item"
              :image newsticker--next-item-image
              :enable '(newsticker-next-item-available-p)))
      (define-key tool-bar-map [newsticker-previous-item]
        (list 'menu-item "newsticker-previous-item" 'newsticker-previous-item
              :visible t
              :help "Go to previous item"
              :image newsticker--previous-item-image
              :enable '(newsticker-previous-item-available-p)))
      (define-key tool-bar-map [newsticker-previous-feed]
        (list 'menu-item "newsticker-previous-feed" 'newsticker-previous-feed
              :visible t
              :help "Go to previous feed"
              :image newsticker--previous-feed-image
              :enable '(newsticker-previous-feed-available-p)))
      ;; standard icons / actions
      (tool-bar-add-item "close"
                         'newsticker-close-buffer
                         'newsticker-close-buffer
                         :help "Close newsticker buffer")
      (tool-bar-add-item "preferences"
                         'newsticker-customize
                         'newsticker-customize
                         :help "Customize newsticker")
      tool-bar-map)))

;; ======================================================================
;;; Newsticker mode
;; ======================================================================

(define-derived-mode newsticker-mode fundamental-mode
  "NewsTicker"
  "Viewing news feeds in Emacs."
  (set (make-local-variable 'tool-bar-map) newsticker-tool-bar-map)
  (set (make-local-variable 'imenu-sort-function) nil)
  (set (make-local-variable 'scroll-conservatively) 999)
  (setq imenu-create-index-function 'newsticker--imenu-create-index)
  (setq imenu-default-goto-function 'newsticker--imenu-goto)
  (setq buffer-read-only t)
  (auto-fill-mode -1) ;; turn auto-fill off!
  (font-lock-mode -1) ;; turn off font-lock!!
  (set (make-local-variable 'font-lock-defaults) nil)
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (setq mode-line-format
        (list "-"
              'mode-line-mule-info
              'mode-line-modified
              'mode-line-frame-identification
              " Newsticker ("
              '(newsticker--buffer-uptodate-p
                "up to date"
                "NEED UPDATE")
              ") "
              '(:eval (format "[%d]" (length newsticker--process-ids)))
              " -- "
              '(:eval (newsticker--buffer-get-feed-title-at-point))
              ": "
              '(:eval (newsticker--buffer-get-item-title-at-point))
              " %-"))
  (add-to-invisibility-spec 't)
  (unless newsticker-show-all-news-elements
    (add-to-invisibility-spec 'extra))
  (newsticker--buffer-set-uptodate nil))

;; refine its mode-map
(define-key newsticker-mode-map "sO" 'newsticker-show-old-items)
(define-key newsticker-mode-map "hO" 'newsticker-hide-old-items)
(define-key newsticker-mode-map "sa" 'newsticker-show-all-desc)
(define-key newsticker-mode-map "ha" 'newsticker-hide-all-desc)
(define-key newsticker-mode-map "sf" 'newsticker-show-feed-desc)
(define-key newsticker-mode-map "hf" 'newsticker-hide-feed-desc)
(define-key newsticker-mode-map "so" 'newsticker-show-old-item-desc)
(define-key newsticker-mode-map "ho" 'newsticker-hide-old-item-desc)
(define-key newsticker-mode-map "sn" 'newsticker-show-new-item-desc)
(define-key newsticker-mode-map "hn" 'newsticker-hide-new-item-desc)
(define-key newsticker-mode-map "se" 'newsticker-show-entry)
(define-key newsticker-mode-map "he" 'newsticker-hide-entry)
(define-key newsticker-mode-map "sx" 'newsticker-show-extra)
(define-key newsticker-mode-map "hx" 'newsticker-hide-extra)

(define-key newsticker-mode-map " "  'scroll-up)
(define-key newsticker-mode-map "q"  'newsticker-close-buffer)
(define-key newsticker-mode-map "p"  'newsticker-previous-item)
(define-key newsticker-mode-map "P"  'newsticker-previous-new-item)
(define-key newsticker-mode-map "F"  'newsticker-previous-feed)
(define-key newsticker-mode-map "\t" 'newsticker-next-item)
(define-key newsticker-mode-map "n"  'newsticker-next-item)
(define-key newsticker-mode-map "N"  'newsticker-next-new-item)
(define-key newsticker-mode-map "f"  'newsticker-next-feed)
(define-key newsticker-mode-map "M"  'newsticker-mark-all-items-as-read)
(define-key newsticker-mode-map "m"
  'newsticker-mark-all-items-at-point-as-read-and-redraw)
(define-key newsticker-mode-map "o"
  'newsticker-mark-item-at-point-as-read)
(define-key newsticker-mode-map "O"
  'newsticker-mark-all-items-at-point-as-read)
(define-key newsticker-mode-map "G"  'newsticker-get-all-news)
(define-key newsticker-mode-map "g"  'newsticker-get-news-at-point)
(define-key newsticker-mode-map "u"  'newsticker-buffer-update)
(define-key newsticker-mode-map "U"  'newsticker-buffer-force-update)
(define-key newsticker-mode-map "a"  'newsticker-add-url)

(define-key newsticker-mode-map "i"
  'newsticker-mark-item-at-point-as-immortal)

(define-key newsticker-mode-map "xf"
  'newsticker-toggle-auto-narrow-to-feed)
(define-key newsticker-mode-map "xi"
  'newsticker-toggle-auto-narrow-to-item)

;; maps for the clickable portions
(defvar newsticker--url-keymap (make-sparse-keymap)
  "Key map for click-able headings in the newsticker buffer.")
(define-key newsticker--url-keymap [mouse-2]
  'newsticker-mouse-browse-url)
(define-key newsticker--url-keymap "\n"
  'newsticker-browse-url)
(define-key newsticker--url-keymap "\C-m"
  'newsticker-browse-url)
(define-key newsticker--url-keymap [(control return)]
  'newsticker-handle-url)

;; newsticker menu
(defvar newsticker-menu (make-sparse-keymap "Newsticker"))

(define-key newsticker-menu [newsticker-browse-url]
  '("Browse URL for item at point" . newsticker-browse-url))
(define-key newsticker-menu [newsticker-separator-1]
  '("--"))
(define-key newsticker-menu [newsticker-buffer-update]
  '("Update buffer" . newsticker-buffer-update))
(define-key newsticker-menu [newsticker-separator-2]
  '("--"))
(define-key newsticker-menu [newsticker-get-all-news]
  '("Get news from all feeds" . newsticker-get-all-news))
(define-key newsticker-menu [newsticker-get-news-at-point]
  '("Get news from feed at point" . newsticker-get-news-at-point))
(define-key newsticker-menu [newsticker-separator-3]
  '("--"))
(define-key newsticker-menu [newsticker-mark-all-items-as-read]
  '("Mark all items as read" . newsticker-mark-all-items-as-read))
(define-key newsticker-menu [newsticker-mark-all-items-at-point-as-read]
  '("Mark all items in feed at point as read" .
    newsticker-mark-all-items-at-point-as-read))
(define-key newsticker-menu [newsticker-mark-item-at-point-as-read]
  '("Mark item at point as read" .
    newsticker-mark-item-at-point-as-read))
(define-key newsticker-menu [newsticker-mark-item-at-point-as-immortal]
  '("Toggle immortality for item at point" .
    newsticker-mark-item-at-point-as-immortal))
(define-key newsticker-menu [newsticker-separator-4]
  '("--"))
(define-key newsticker-menu [newsticker-toggle-auto-narrow-to-item]
  '("Narrow to single item" . newsticker-toggle-auto-narrow-to-item))
(define-key newsticker-menu [newsticker-toggle-auto-narrow-to-feed]
  '("Narrow to single news feed" . newsticker-toggle-auto-narrow-to-feed))
(define-key newsticker-menu [newsticker-hide-old-items]
  '("Hide old items" . newsticker-hide-old-items))
(define-key newsticker-menu [newsticker-show-old-items]
  '("Show old items" . newsticker-show-old-items))
(define-key newsticker-menu [newsticker-next-item]
  '("Go to next item" . newsticker-next-item))
(define-key newsticker-menu [newsticker-previous-item]
  '("Go to previous item" . newsticker-previous-item))

;; bind menu to mouse
(define-key newsticker-mode-map [down-mouse-3] newsticker-menu)
;; Put menu in menu-bar
(define-key newsticker-mode-map [menu-bar Newsticker]
  (cons "Newsticker" newsticker-menu))


;; ======================================================================
;;; shortcuts
;; ======================================================================
(defsubst newsticker--title (item)
  "Return title of ITEM."
  (nth 0 item))
(defsubst newsticker--desc (item)
  "Return description of ITEM."
  (nth 1 item))
(defsubst newsticker--link (item)
  "Return link of ITEM."
  (nth 2 item))
(defsubst newsticker--time (item)
  "Return time of ITEM."
  (nth 3 item))
(defsubst newsticker--age (item)
  "Return age of ITEM."
  (nth 4 item))
(defsubst newsticker--pos (item)
  "Return position/index of ITEM."
  (nth 5 item))
(defsubst newsticker--preformatted-contents (item)
  "Return pre-formatted text of ITEM."
  (nth 6 item))
(defsubst newsticker--preformatted-title (item)
  "Return pre-formatted title of ITEM."
  (nth 7 item))
(defsubst newsticker--extra (item)
  "Return extra attributes of ITEM."
  (nth 8 item))
(defsubst newsticker--guid (item)
  "Return guid of ITEM."
  (let ((guid (assoc 'guid (newsticker--extra item))))
    (if (stringp guid)
        guid
      (car (xml-node-children guid)))))
(defsubst newsticker--enclosure (item)
  "Return enclosure element of ITEM in the form \(...FIXME...\) or nil."
  (let ((enclosure (assoc 'enclosure (newsticker--extra item))))
    (if enclosure
        (xml-node-attributes enclosure))))

;; ======================================================================
;;; User fun
;; ======================================================================

;;;###autoload
(defun newsticker-start (&optional do-not-complain-if-running)
  "Start the newsticker.
Start the timers for display and retrieval.  If the newsticker, i.e. the
timers, are running already a warning message is printed unless
DO-NOT-COMPLAIN-IF-RUNNING is not nil.
Run `newsticker-start-hook' if newsticker was not running already."
  (interactive)
  (let ((running (newsticker-running-p)))
    ;; read old cache if it exists and newsticker is not running
    (unless running
      (let* ((coding-system-for-read 'utf-8)
             (buf (find-file-noselect newsticker-cache-filename)))
        (when buf
          (set-buffer buf)
          (goto-char (point-min))
          (condition-case nil
              (setq newsticker--cache (read buf))
            (error
             (message "Error while reading newsticker cache file!")
             (setq newsticker--cache nil))))))
    ;; start retrieval timers -- for sake of simplicity we will start
    ;; one timer for each feed
    (mapc (lambda (item)
            (let* ((feed-name (car item))
                   (start-time (nth 2 item))
                   (interval (or (nth 3 item)
                                 newsticker-retrieval-interval))
                   (timer (assoc (car item)
                                 newsticker--retrieval-timer-list)))
              (if timer
                  (or do-not-complain-if-running
                      (message "Timer for %s is running already!"
                               feed-name))
                (newsticker--debug-msg "Starting timer for %s: %s, %d"
                                       feed-name start-time interval)
                ;; do not repeat retrieval if interval not positive
                (if (<= interval 0)
                    (setq interval nil))
                ;; Suddenly XEmacs doesn't like start-time 0
                (if (or (not start-time)
                        (and (numberp start-time) (= start-time 0)))
                    (setq start-time 1))
                ;; (message "start-time %s" start-time)
                (setq timer (run-at-time start-time interval
                                         'newsticker-get-news feed-name))
                (if interval
                    (add-to-list 'newsticker--retrieval-timer-list
                                 (cons feed-name timer))))))
          (append newsticker-url-list-defaults newsticker-url-list))
    (unless running
      (run-hooks 'newsticker-start-hook)
      (message "Newsticker started!"))))

;;;###autoload
(defun newsticker-start-ticker ()
  "Start newsticker's ticker (but not the news retrieval).
Start display timer for the actual ticker if wanted and not
running already."
  (interactive)
  (if (and (> newsticker-display-interval 0)
           (not newsticker--display-timer))
      (setq newsticker--display-timer
            (run-at-time newsticker-display-interval
                         newsticker-display-interval
                         'newsticker--display-tick))))

(defun newsticker-stop ()
  "Stop the newsticker and the newsticker-ticker.
Cancel the timers for display and retrieval.  Run `newsticker-stop-hook'
if newsticker has been running."
  (interactive)
  (newsticker--cache-update t)
  (newsticker-stop-ticker)
  (when (newsticker-running-p)
    (mapc (lambda (name-and-timer)
            (cancel-timer (cdr name-and-timer)))
          newsticker--retrieval-timer-list)
    (setq newsticker--retrieval-timer-list nil)
    (run-hooks 'newsticker-stop-hook)
    (message "Newsticker stopped!")))

(defun newsticker-stop-ticker ()
  "Stop newsticker's ticker (but not the news retrieval)."
  (interactive)
  (when newsticker--display-timer
    (cancel-timer newsticker--display-timer)
    (setq newsticker--display-timer nil)))

;; the functions we need for retrieval and display
;;;###autoload
(defun newsticker-show-news ()
  "Switch to newsticker buffer.  You may want to bind this to a key."
  (interactive)
  (newsticker-start t) ;; will start only if not running
  (newsticker-buffer-update)
  (switch-to-buffer "*newsticker*"))

(defun newsticker-buffer-force-update ()
  "Update the newsticker buffer, even if not necessary."
  (interactive)
  (newsticker-buffer-update t))

(defun newsticker-buffer-update (&optional force)
  "Update the *newsticker* buffer.
Unless FORCE is t this is done only if necessary, i.e. when the
*newsticker* buffer is not up-to-date."
  (interactive)
  ;; bring cache data into proper order....
  (newsticker--cache-sort)
  ;; fill buffer
  (save-excursion
    (let ((buf (get-buffer "*newsticker*")))
      (if buf
          (switch-to-buffer buf)
        (switch-to-buffer (get-buffer-create "*newsticker*"))
        (newsticker--buffer-set-uptodate nil)))
   (when (or force
             (not newsticker--buffer-uptodate-p))
     (message "Preparing newsticker buffer...")
     (setq buffer-undo-list t)
     (let ((inhibit-read-only t))
       (set-buffer-modified-p nil)
       (erase-buffer)
       (newsticker-mode)
       ;; Emacs 21.3.50 does not care if we turn off auto-fill in the
       ;; definition of newsticker-mode, so we do it here (again)
       (auto-fill-mode -1)

       (set-buffer-file-coding-system 'utf-8)

       (if newsticker-use-full-width
           (set (make-local-variable 'fill-column) (1- (window-width))))
       (newsticker--buffer-insert-all-items)

       ;; FIXME: needed for methods buffer in ecb
       ;; (set-visited-file-name "*newsticker*")

       (set-buffer-modified-p nil)
        (newsticker-hide-all-desc)
        (if newsticker-hide-old-items-in-newsticker-buffer
            (newsticker-hide-old-items))
        (if newsticker-show-descriptions-of-new-items
            (newsticker-show-new-item-desc))
       )
     (message ""))
   (newsticker--buffer-set-uptodate t)
   (run-hooks 'newsticker-buffer-change-hook)))

(defun newsticker-get-all-news ()
  "Launch retrieval of news from all configured newsticker sites.
This does NOT start the retrieval timers."
  (interactive)
  ;; launch retrieval of news
  (mapc (lambda (item)
          (newsticker-get-news (car item)))
        (append newsticker-url-list-defaults newsticker-url-list)))

(defun newsticker-get-news-at-point ()
  "Launch retrieval of news for the feed point is in.
This does NOT start the retrieval timers."
  (interactive)
  ;; launch retrieval of news
  (let ((feed (get-text-property (point) 'feed)))
      (when feed
        (newsticker--debug-msg "Getting news for %s" (symbol-name feed))
        (newsticker-get-news (symbol-name feed)))))

(defun newsticker-add-url (url name)
  "Add given URL under given NAME to `newsticker-url-list'.
If URL is nil it is searched at point."
  (interactive
   (list
    (read-string "URL: "
                 (save-excursion
                   (end-of-line)
                   (and
                    (re-search-backward
                     "http://"
                     (if (> (point) (+ (point-min) 100))
                         (- (point) 100)
                       (point-min))
                     t)
                    (re-search-forward
                     "http://[-a-zA-Z0-9&/_.]*"
                     (if (< (point) (- (point-max) 200))
                         (+ (point) 200)
                       (point-max))
                     t)
                    (buffer-substring-no-properties (match-beginning 0)
                                                    (match-end 0)))))
    (read-string "Name: ")))
  (add-to-list 'newsticker-url-list (list name url nil nil nil) t)
  (customize-variable 'newsticker-url-list))

;; External.
(declare-function w3m-toggle-inline-image "ext:w3m" (&optional force no-cache))

(defun newsticker-w3m-show-inline-images ()
  "Show inline images in visible text ranges.
In-line images in invisible text ranges are hidden.  This function
calls `w3m-toggle-inline-image'.  It works only if
`newsticker-html-renderer' is set to `w3m-region'."
  (interactive)
  (if (eq newsticker-html-renderer 'w3m-region)
      (let ((inhibit-read-only t))
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (let ((pos (point)))
              (while pos
                (setq pos (next-single-property-change pos 'w3m-image))
                (when pos
                  (goto-char pos)
                  (when (get-text-property pos 'w3m-image)
                    (let ((invis (newsticker--lists-intersect-p
                                  (get-text-property (1- (point))
                                                     'invisible)
                                  buffer-invisibility-spec)))
                      (unless  (car (get-text-property (1- (point))
                                                       'display))
                        (unless invis
                          (w3m-toggle-inline-image t)))))))))))))

(defadvice w3m-insert-image (after newsticker activate)
  (newsticker--buffer-after-w3m-insert-image (ad-get-arg 0) (ad-get-arg 1)))

(defun newsticker--buffer-after-w3m-insert-image (beg end)
  "Save preformatted contents after an image has been inserted
between BEG and END."
  (when (string= (buffer-name) "*newsticker*")
    (save-excursion
      (newsticker--buffer-beginning-of-item)
      (let* ((pos     (point))
             (feed    (get-text-property pos 'feed))
             (age     (get-text-property pos 'nt-age))
             (title   (get-text-property pos 'nt-title))
             (guid    (get-text-property pos 'nt-guid))
             (nt-desc (get-text-property pos 'nt-desc))
             (item    (newsticker--cache-contains newsticker--cache
                                                  feed title nt-desc
                                                  nil nil guid))
             (desc-beg (newsticker--buffer-goto '(desc)))
             (desc-end (newsticker--buffer-end-of-item)))
        ;;(add-text-properties beg end (list nt-type desc))
        (add-text-properties beg end (list 'invisible
                                           (get-text-property end 'invisible)))
        ;;(message "newsticker--buffer-after-w3m-insert-image at %s, %s: `%s'" 
        ;;         beg feed title)
        (if item
            (newsticker--cache-set-preformatted-contents
             item (buffer-substring desc-beg desc-end))
          (message "ooops in newsticker--buffer-after-w3m-insert-image at %s, %s: `%s'" 
                   beg feed title))))))

;; ======================================================================
;;; keymap stuff
;; ======================================================================
(defun newsticker-close-buffer ()
  "Close the newsticker buffer."
  (interactive)
  (newsticker--cache-update t)
  (bury-buffer))

(defun newsticker-next-new-item (&optional do-not-wrap-at-eob)
  "Go to next new news item.
If no new item is found behind point, search is continued at
beginning of buffer unless optional argument DO-NOT-WRAP-AT-EOB
is non-nil."
  (interactive)
  (widen)
  (let ((go-ahead t))
    (while go-ahead
      (unless (newsticker--buffer-goto '(item) 'new)
	;; found nothing -- wrap
	(unless do-not-wrap-at-eob
	  (goto-char (point-min))
	  (newsticker-next-new-item t))
	(setq go-ahead nil))
      (unless (newsticker--lists-intersect-p
               (get-text-property (point) 'invisible)
               buffer-invisibility-spec)
	;; this item is invisible -- continue search
        (setq go-ahead nil))))
  (run-hooks 'newsticker-select-item-hook)
  (point))

(defun newsticker-previous-new-item (&optional do-not-wrap-at-bob)
  "Go to previous new news item.
If no new item is found before point, search is continued at
beginning of buffer unless optional argument DO-NOT-WRAP-AT-BOB
is non-nil."
  (interactive)
  (widen)
  (let ((go-ahead t))
    (while go-ahead
      (unless (newsticker--buffer-goto '(item) 'new t)
	(unless do-not-wrap-at-bob
	  (goto-char (point-max))
	  (newsticker--buffer-goto '(item) 'new t)))
      (unless (newsticker--lists-intersect-p
               (get-text-property (point) 'invisible)
		    buffer-invisibility-spec)
	(setq go-ahead nil))))
  (run-hooks 'newsticker-select-item-hook)
  (point))

(defun newsticker-next-item (&optional do-not-wrap-at-eob)
  "Go to next news item.
Return new buffer position.
If no item is found below point, search is continued at beginning
of buffer unless optional argument DO-NOT-WRAP-AT-EOB is
non-nil."
  (interactive)
  (widen)
  (let ((go-ahead t)
        (search-list '(item)))
    (if newsticker--auto-narrow-to-item
        (setq search-list '(item feed)))
    (while go-ahead
      (unless (newsticker--buffer-goto search-list)
	;; found nothing -- wrap
	(unless do-not-wrap-at-eob
	  (goto-char (point-min)))
	(setq go-ahead nil))
      (unless (newsticker--lists-intersect-p
               (get-text-property (point) 'invisible)
		    buffer-invisibility-spec)
	(setq go-ahead nil))))
  (run-hooks 'newsticker-select-item-hook)
  (force-mode-line-update)
  (point))

(defun newsticker-next-item-same-feed ()
  "Go to next news item in the same feed.
Return new buffer position.  If no item is found below point or if
auto-narrow-to-item is enabled, nil is returned."
  (interactive)
  (if newsticker--auto-narrow-to-item
      nil
    (let ((go-ahead t)
          (current-pos (point))
          (end-of-feed (save-excursion (newsticker--buffer-end-of-feed))))
      (while go-ahead
        (unless (newsticker--buffer-goto '(item))
          (setq go-ahead nil))
        (unless (newsticker--lists-intersect-p
                 (get-text-property (point) 'invisible)
                 buffer-invisibility-spec)
          (setq go-ahead nil)))
      (if (and (> (point) current-pos)
               (< (point) end-of-feed))
          (point)
        (goto-char current-pos)
        nil))))

(defun newsticker-previous-item (&optional do-not-wrap-at-bob)
  "Go to previous news item.
Return new buffer position.
If no item is found before point, search is continued at
beginning of buffer unless optional argument DO-NOT-WRAP-AT-BOB
is non-nil."
  (interactive)
  (widen)
  (let ((go-ahead t)
        (search-list '(item)))
    (if newsticker--auto-narrow-to-item
        (setq search-list '(item feed)))
    (when (bobp)
      (unless do-not-wrap-at-bob
	(goto-char (point-max))))
    (while go-ahead
      (if (newsticker--buffer-goto search-list nil t)
          (unless (newsticker--lists-intersect-p
                   (get-text-property (point) 'invisible)
                   buffer-invisibility-spec)
            (setq go-ahead nil))
        (goto-char (point-min))
        (setq go-ahead nil))))
  (run-hooks 'newsticker-select-item-hook)
  (force-mode-line-update)
  (point))

(defun newsticker-next-feed ()
  "Go to next news feed.
Return new buffer position."
  (interactive)
  (widen)
  (newsticker--buffer-goto '(feed))
  (run-hooks 'newsticker-select-feed-hook)
  (force-mode-line-update)
  (point))

(defun newsticker-previous-feed ()
  "Go to previous news feed.
Return new buffer position."
  (interactive)
  (widen)
  (newsticker--buffer-goto '(feed) nil t)
  (run-hooks 'newsticker-select-feed-hook)
  (force-mode-line-update)
  (point))

(defun newsticker-mark-all-items-at-point-as-read-and-redraw ()
  "Mark all items as read and clear ticker contents."
  (interactive)
  (when (or newsticker--buffer-uptodate-p
            (y-or-n-p
             "Buffer is not up to date -- really mark items as read? "))
    (newsticker-mark-all-items-of-feed-as-read
     (get-text-property (point) 'feed))))

(defun newsticker-mark-all-items-of-feed-as-read (feed)
  "Mark all items as read, clear ticker, and redraw *newsticker* buffer."
  (when feed
    (let ((pos (point)))
      (message "Marking all items as read for %s" (symbol-name feed))
      (newsticker--cache-replace-age newsticker--cache feed 'new 'old)
      (newsticker--cache-replace-age newsticker--cache feed 'obsolete
                                     'old)
      (newsticker--cache-update)
      (newsticker--buffer-set-uptodate nil)
      (newsticker--ticker-text-setup)
      (newsticker-buffer-update)
      ;; go back to where we came frome
      (goto-char pos)
      (end-of-line)
      (newsticker--buffer-goto '(feed) nil t))))
  
(defun newsticker-mark-all-items-at-point-as-read ()
  "Mark all items as read and clear ticker contents."
  (interactive)
  (when (or newsticker--buffer-uptodate-p
            (y-or-n-p
             "Buffer is not up to date -- really mark items as read? "))
    (newsticker--do-mark-item-at-point-as-read t)
    (while (newsticker-next-item-same-feed)
      (newsticker--do-mark-item-at-point-as-read t))
    (newsticker-next-item t)))

(defun newsticker-mark-item-at-point-as-read (&optional respect-immortality)
  "Mark item at point as read and move to next item.
If optional argument RESPECT-IMMORTALITY is not nil immortal items do
not get changed."
  (interactive)
  (when (or newsticker--buffer-uptodate-p
            (y-or-n-p
             "Buffer is not up to date -- really mark this item as read? "))
    (newsticker--do-mark-item-at-point-as-read respect-immortality)
    ;; move forward
    (newsticker-next-item t)))

(defun newsticker--do-mark-item-at-point-as-read (&optional respect-immortality)
  "Mark item at point as read.
If optional argument RESPECT-IMMORTALITY is not nil immortal items do
not get changed."
  (let ((feed (get-text-property (point) 'feed)))
    (when feed
      (save-excursion
        (newsticker--buffer-beginning-of-item)
        (let ((inhibit-read-only t)
              (age (get-text-property (point) 'nt-age))
              (title (get-text-property (point) 'nt-title))
              (guid (get-text-property (point) 'nt-guid))
              (nt-desc (get-text-property (point) 'nt-desc))
              (pos (save-excursion (newsticker--buffer-end-of-item)))
              item)
          (when (or (eq age 'new)
                    (eq age 'obsolete)
                    (and (eq age 'immortal)
                         (not respect-immortality)))
            ;; find item
            (setq item (newsticker--cache-contains newsticker--cache
                                                   feed title nt-desc
                                                   nil nil guid))
            ;; mark as old
            (when item
              (setcar (nthcdr 4 item) 'old)
              (newsticker--do-forget-preformatted item))
            ;; clean up ticker
            (if (or (and (eq age 'new)
                         newsticker-hide-immortal-items-in-echo-area)
                    (and (memq age '(old immortal))
                         (not
                          (eq newsticker-hide-old-items-in-newsticker-buffer
                              newsticker-hide-immortal-items-in-echo-area))))
                (newsticker--ticker-text-remove feed title))
            ;; set faces etc.
            (save-excursion
              (save-restriction
                (widen)
                (put-text-property (point) pos 'nt-age 'old)
                (newsticker--buffer-set-faces (point) pos)))
            (set-buffer-modified-p nil)))))))

(defun newsticker-mark-item-at-point-as-immortal ()
  "Mark item at point as read."
  (interactive)
  (when (or newsticker--buffer-uptodate-p
            (y-or-n-p
             "Buffer is not up to date -- really mark this item as read? "))
    (let ((feed (get-text-property (point) 'feed))
          (item nil))
      (when feed
        (save-excursion
          (newsticker--buffer-beginning-of-item)
          (let ((inhibit-read-only t)
                (oldage (get-text-property (point) 'nt-age))
                (title (get-text-property (point) 'nt-title))
                (guid (get-text-property (point) 'nt-guid))
                (pos  (save-excursion (newsticker--buffer-end-of-item))))
            (let ((newage 'immortal))
              (if (eq oldage 'immortal)
                  (setq newage 'old))
              (setq item (newsticker--cache-contains newsticker--cache
                                                     feed title nil nil nil
                                                     guid))
              ;; change age
              (when item
                (setcar (nthcdr 4 item) newage)
                (newsticker--do-forget-preformatted item))
              (if (or (and (eq newage 'immortal)
                           newsticker-hide-immortal-items-in-echo-area)
                      (and (eq newage 'obsolete)
                           newsticker-hide-obsolete-items-in-echo-area)
                      (and (eq oldage 'immortal)
                           (not
                            (eq newsticker-hide-old-items-in-newsticker-buffer
                                newsticker-hide-immortal-items-in-echo-area))))
                  (newsticker--ticker-text-remove feed title)
                (newsticker--ticker-text-setup))
              (save-excursion
                (save-restriction
                  (widen)
                  (put-text-property (point) pos 'nt-age newage)
                  (if (eq newage 'immortal)
                      (put-text-property (point) pos 'nt-age 'immortal)
                    (put-text-property (point) pos 'nt-age 'old))
                  (newsticker--buffer-set-faces (point) pos))))))
          (if item
            (newsticker-next-item t))))))

(defun newsticker-mark-all-items-as-read ()
  "Mark all items as read and clear ticker contents."
  (interactive)
  (when (or newsticker--buffer-uptodate-p
            (y-or-n-p
             "Buffer is not up to date -- really mark items as read? "))
    (newsticker--cache-replace-age newsticker--cache 'any 'new 'old)
    (newsticker--buffer-set-uptodate nil)
    (newsticker--ticker-text-setup)
    (newsticker--cache-update)
    (newsticker-buffer-update)))

(defun newsticker-hide-extra ()
  "Hide the extra elements of items."
  (interactive)
  (newsticker--buffer-hideshow 'extra nil)
  (newsticker--buffer-redraw))

(defun newsticker-show-extra ()
  "Show the extra elements of items."
  (interactive)
  (newsticker--buffer-hideshow 'extra t)
  (newsticker--buffer-redraw))

(defun newsticker-hide-old-item-desc ()
  "Hide the description of old items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-old nil)
  (newsticker--buffer-redraw))

(defun newsticker-show-old-item-desc ()
  "Show the description of old items."
  (interactive)
  (newsticker--buffer-hideshow 'item-old t)
  (newsticker--buffer-hideshow 'desc-old t)
  (newsticker--buffer-redraw))

(defun newsticker-hide-new-item-desc ()
  "Hide the description of new items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-new nil)
  (newsticker--buffer-hideshow 'desc-immortal nil)
  (newsticker--buffer-hideshow 'desc-obsolete nil)
  (newsticker--buffer-redraw))

(defun newsticker-show-new-item-desc ()
  "Show the description of new items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-new t)
  (newsticker--buffer-hideshow 'desc-immortal t)
  (newsticker--buffer-hideshow 'desc-obsolete t)
  (newsticker--buffer-redraw))

(defun newsticker-hide-feed-desc ()
  "Hide the description of feeds."
  (interactive)
  (newsticker--buffer-hideshow 'desc-feed nil)
  (newsticker--buffer-redraw))

(defun newsticker-show-feed-desc ()
  "Show the description of old items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-feed t)
  (newsticker--buffer-redraw))

(defun newsticker-hide-all-desc ()
  "Hide the descriptions of feeds and all items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-feed nil)
  (newsticker--buffer-hideshow 'desc-immortal nil)
  (newsticker--buffer-hideshow 'desc-obsolete nil)
  (newsticker--buffer-hideshow 'desc-new  nil)
  (newsticker--buffer-hideshow 'desc-old  nil)
  (newsticker--buffer-redraw))

(defun newsticker-show-all-desc ()
  "Show the descriptions of feeds and all items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-feed t)
  (newsticker--buffer-hideshow 'desc-immortal  t)
  (newsticker--buffer-hideshow 'desc-obsolete  t)
  (newsticker--buffer-hideshow 'desc-new  t)
  (newsticker--buffer-hideshow 'desc-old  t)
  (newsticker--buffer-redraw))

(defun newsticker-hide-old-items ()
  "Hide old items."
  (interactive)
  (newsticker--buffer-hideshow 'desc-old nil)
  (newsticker--buffer-hideshow 'item-old nil)
  (newsticker--buffer-redraw))

(defun newsticker-show-old-items ()
  "Show old items."
  (interactive)
  (newsticker--buffer-hideshow 'item-old t)
  (newsticker--buffer-redraw))

(defun newsticker-hide-entry ()
  "Hide description of entry at point."
  (interactive)
  (save-excursion
    (let* (pos1 pos2
                (inhibit-read-only t)
                inv-prop org-inv-prop
                is-invisible)
      (newsticker--buffer-beginning-of-item)
      (newsticker--buffer-goto '(desc))
      (setq pos1 (max (point-min) (1- (point))))
      (newsticker--buffer-goto '(extra feed item nil))
      (setq pos2 (max (point-min) (1- (point))))
      (setq inv-prop (get-text-property pos1 'invisible))
      (setq org-inv-prop (get-text-property pos1 'org-invisible))
      (cond ((eq inv-prop t)
             ;; do nothing
             )
            ((eq org-inv-prop nil)
             (add-text-properties pos1 pos2
                                  (list 'invisible (list t)
                                        'org-invisible inv-prop)))
            (t
             ;; toggle
             (add-text-properties pos1 pos2
                                  (list 'invisible org-inv-prop))
             (remove-text-properties pos1 pos2 '(org-invisible))))))
  (newsticker--buffer-redraw))

(defun newsticker-show-entry ()
  "Show description of entry at point."
  (interactive)
  (save-excursion
    (let* (pos1 pos2
                (inhibit-read-only t)
                inv-prop org-inv-prop
                is-invisible)
      (newsticker--buffer-beginning-of-item)
      (newsticker--buffer-goto '(desc))
      (setq pos1 (max (point-min) (1- (point))))
      (newsticker--buffer-goto '(extra feed item))
      (setq pos2 (max (point-min) (1- (point))))
      (setq inv-prop (get-text-property pos1 'invisible))
      (setq org-inv-prop (get-text-property pos1 'org-invisible))
      (cond ((eq org-inv-prop nil)
             (add-text-properties pos1 pos2
                                  (list 'invisible nil
                                        'org-invisible inv-prop)))
            (t
             ;; toggle
             (add-text-properties pos1 pos2
                                  (list 'invisible org-inv-prop))
             (remove-text-properties pos1 pos2 '(org-invisible))))))
  (newsticker--buffer-redraw))

(defun newsticker-toggle-auto-narrow-to-feed ()
  "Toggle narrowing to current news feed.
If auto-narrowing is active, only news item of the current feed
are visible."
  (interactive)
  (newsticker-set-auto-narrow-to-feed
   (not newsticker--auto-narrow-to-feed)))

(defun newsticker-set-auto-narrow-to-feed (value)
  "Turn narrowing to current news feed on or off.
If VALUE is nil, auto-narrowing is turned off, otherwise it is turned on."
  (interactive)
  (setq newsticker--auto-narrow-to-item nil)
  (setq newsticker--auto-narrow-to-feed value)
  (widen)
  (newsticker--buffer-make-item-completely-visible)
  (run-hooks 'newsticker-narrow-hook))

(defun newsticker-toggle-auto-narrow-to-item ()
  "Toggle narrowing to current news item.
If auto-narrowing is active, only one item of the current feed
is visible."
  (interactive)
  (newsticker-set-auto-narrow-to-item
   (not newsticker--auto-narrow-to-item)))

(defun newsticker-set-auto-narrow-to-item (value)
  "Turn narrowing to current news item on or off.
If VALUE is nil, auto-narrowing is turned off, otherwise it is turned on."
  (interactive)
  (setq newsticker--auto-narrow-to-feed nil)
  (setq newsticker--auto-narrow-to-item value)
  (widen)
  (newsticker--buffer-make-item-completely-visible)
  (run-hooks 'newsticker-narrow-hook))

(defun newsticker-customize ()
  "Open the newsticker customization group."
  (interactive)
  (customize-group "newsticker"))

(defun newsticker-next-feed-available-p ()
  "Return t if position is before last feed, nil otherwise."
  (save-excursion
    (let ((p (point)))
      (newsticker--buffer-goto '(feed))
      (not (= p (point))))))

(defun newsticker-previous-feed-available-p ()
  "Return t if position is behind first feed, nil otherwise."
  (save-excursion
    (let ((p (point)))
      (newsticker--buffer-goto '(feed) nil t)
      (not (= p (point))))))

(defun newsticker-next-item-available-p ()
  "Return t if position is before last feed, nil otherwise."
  (save-excursion
    (catch 'result
      (while (< (point) (point-max))
        (unless (newsticker--buffer-goto '(item))
          (throw 'result nil))
        (unless (newsticker--lists-intersect-p
                 (get-text-property (point) 'invisible)
                 buffer-invisibility-spec)
          (throw 'result t))))))

(defun newsticker-previous-item-available-p ()
  "Return t if position is behind first item, nil otherwise."
  (save-excursion
    (catch 'result
      (while (> (point) (point-min))
        (unless (newsticker--buffer-goto '(item) nil t)
          (throw 'result nil))
        (unless (newsticker--lists-intersect-p
                 (get-text-property (point) 'invisible)
                 buffer-invisibility-spec)
          (throw 'result t))))))

(defun newsticker-item-not-old-p ()
  "Return t if there is an item at point which is not old, nil otherwise."
    (when (get-text-property (point) 'feed)
      (save-excursion
        (newsticker--buffer-beginning-of-item)
        (let ((age (get-text-property (point) 'nt-age)))
          (and (memq  age '(new immortal obsolete)) t)))))

(defun newsticker-item-not-immortal-p ()
  "Return t if there is an item at point which is not immortal, nil otherwise."
    (when (get-text-property (point) 'feed)
      (save-excursion
        (newsticker--buffer-beginning-of-item)
        (let ((age (get-text-property (point) 'nt-age)))
          (and (memq  age '(new old obsolete)) t)))))

;; ======================================================================
;;; local stuff
;; ======================================================================
(defun newsticker-get-news (feed-name)
  "Get news from the site FEED-NAME and load feed logo.
FEED-NAME must be a string which occurs as the label (i.e. the first element)
in an element of `newsticker-url-list' or `newsticker-url-list-defaults'."
  (newsticker--debug-msg "%s: Getting news for %s"
                         (format-time-string "%A, %H:%M" (current-time))
                         feed-name)
  (let* ((buffername (concat " *newsticker-wget-" feed-name "*"))
         (item (or (assoc feed-name newsticker-url-list)
                   (assoc feed-name newsticker-url-list-defaults)
                   (error
                    "Cannot get news for %s: Check newsticker-url-list"
                    feed-name)))
         (url (cadr item))
         (wget-arguments (or (car (cdr (cdr (cdr (cdr item)))))
                             newsticker-wget-arguments)))
    (save-excursion
      (set-buffer (get-buffer-create buffername))
      (erase-buffer)
      ;; throw an error if there is an old wget-process around
      (if (get-process feed-name)
          (error "Another wget-process is running for %s" feed-name))
      ;; start wget
      (let* ((args (append wget-arguments (list url)))
             (proc (apply 'start-process feed-name buffername
                          newsticker-wget-name args)))
        (set-process-coding-system proc 'no-conversion 'no-conversion)
        (set-process-sentinel proc 'newsticker--sentinel)
        (setq newsticker--process-ids (cons (process-id proc)
                                            newsticker--process-ids))
        (force-mode-line-update)))))

(defun newsticker-mouse-browse-url (event)
  "Call `browse-url' for the link of the item at which the EVENT occurred."
  (interactive "e")
  (save-excursion
    (switch-to-buffer (window-buffer (posn-window (event-end event))))
    (let ((url (get-text-property (posn-point (event-end event))
                                  'nt-link)))
      (when url
        (browse-url url)
        (save-excursion
          (goto-char (posn-point (event-end event)))
          (if newsticker-automatically-mark-visited-items-as-old
              (newsticker-mark-item-at-point-as-read t)))))))

(defun newsticker-browse-url ()
  "Call `browse-url' for the link of the item at point."
  (interactive)
  (let ((url (get-text-property (point) 'nt-link)))
    (when url
      (browse-url url)
      (if newsticker-automatically-mark-visited-items-as-old
          (newsticker-mark-item-at-point-as-read t)))))

(defvar newsticker-open-url-history
  '("wget" "xmms" "realplay")
  "...")

(defun newsticker-handle-url ()
  "Ask for a program to open the link of the item at point."
  (interactive)
  (let ((url (get-text-property (point) 'nt-link)))
    (when url
      (let ((prog (read-string "Open url with: " nil
                               'newsticker-open-url-history)))
        (when prog
          (message "%s %s" prog url)
          (start-process prog prog prog url)
      (if newsticker-automatically-mark-visited-items-as-old
          (newsticker-mark-item-at-point-as-read t)))))))

(defun newsticker--sentinel (process event)
  "Sentinel for extracting news titles from an RDF buffer.
Argument PROCESS is the process which has just changed its state.
Argument EVENT tells what has happened to the process."
  (let* ((p-status (process-status process))
         (exit-status (process-exit-status process))
         (time (current-time))
         (name (process-name process))
         (name-symbol (intern name))
         (something-was-added nil))
    ;; catch known errors (zombie processes, rubbish-xml etc.
    ;; if an error occurs the news feed is not updated!
    (catch 'oops
      (unless (and (eq p-status 'exit)
                   (= exit-status 0))
        (setq newsticker--cache
              (newsticker--cache-add
               newsticker--cache
               name-symbol
               newsticker--error-headline
               (format
                (concat "%s: Newsticker could not retrieve news from %s.\n"
                        "Return status: `%s'\n"
                        "Command was `%s'")
                (format-time-string "%A, %H:%M" (current-time))
                name event (process-command process))
               ""
               (current-time)
               'new
               0 nil))
        (message "%s: Error while retrieving news from %s"
                 (format-time-string "%A, %H:%M" (current-time))
                 (process-name process))
        (throw 'oops nil))
      (let* ((coding-system 'utf-8)
             (node-list
              (save-current-buffer
                (set-buffer (process-buffer process))
                ;; a very very dirty workaround to overcome the
                ;; problems with the newest (20030621) xml.el:
                ;; remove all unnecessary whitespace
                (goto-char (point-min))
                (while (re-search-forward ">[ \t\r\n]+<" nil t)
                  (replace-match "><" nil t))
                ;; and another brutal workaround (20031105)!  For some
                ;; reason the xml parser does not like the colon in the
                ;; doctype name "rdf:RDF"
                (goto-char (point-min))
                (if (re-search-forward "<!DOCTYPE[ \t\n]+rdf:RDF" nil t)
                  (replace-match "<!DOCTYPE rdfColonRDF" nil t))
                ;; finally.... ~##^°!!!!!
                (goto-char (point-min))
                (while (search-forward "\r\n" nil t)
                  (replace-match "\n" nil t))
                ;; still more brutal workarounds (20040309)!  The xml
                ;; parser does not like doctype rss
                (goto-char (point-min))
                (if (re-search-forward "<!DOCTYPE[ \t\n]+rss[ \t\n]*>" nil t)
                  (replace-match "" nil t))
                ;; And another one (20050618)! (Fixed in GNU Emacs 22.0.50.18)
                ;; Remove comments to avoid this xml-parsing bug:
                ;; "XML files can have only one toplevel tag"
                (goto-char (point-min))
                (while (search-forward "<!--" nil t)
                  (let ((start (match-beginning 0)))
                    (unless (search-forward "-->" nil t)
                      (error "Can't find end of comment"))
                    (delete-region start (point))))
                ;; And another one (20050702)! If description is HTML
                ;; encoded and starts with a `<', wrap the whole
                ;; description in a CDATA expression.  This happened for
                ;; http://www.thefreedictionary.com/_/WoD/rss.aspx?type=quote
                (goto-char (point-min))
                (while (re-search-forward
                        "<description>\\(<img.*?\\)</description>" nil t)
                  (replace-match
                   "<description><![CDATA[ \\1 ]]></description>"))
                ;; And another one (20051123)! XML parser does not like this:
                ;; <yweather:location city="Frankfurt/Main" region="" country="GM" />
                ;; try to "fix" empty attributes
                ;; This happened for
                ;; http://xml.weather.yahoo.com/forecastrss?p=GMXX0040&u=f
                (goto-char (point-min))
                (while (re-search-forward "\\(<[^>]*\\)=\"\"" nil t)
                  (replace-match "\\1=\" \""))
                ;;
                (set-buffer-modified-p nil)
                ;; check coding system
                (goto-char (point-min))
                (if (re-search-forward "encoding=\"\\([^\"]+\\)\""
                                       nil t)
                    (setq coding-system (intern (downcase (match-string 1))))
                  (setq coding-system
                        (condition-case nil
                              (check-coding-system coding-system)
                          (coding-system-error
                           (message
                            "newsticker.el: ignoring coding system %s for %s"
                            coding-system name)
                           nil))))
                ;; Decode if possible
                (when coding-system
                  (decode-coding-region (point-min) (point-max)
                                        coding-system))
                (condition-case errordata
                    ;; The xml parser might fail
                    ;; or the xml might be bugged
                    (xml-parse-region (point-min) (point-max))
                  (error (message "Could not parse %s: %s"
                                  (buffer-name) (cadr errordata))
                         (throw 'oops nil)))))
             (topnode (car node-list))
             (channelnode (car (xml-get-children topnode 'channel)))
             (imageurl nil))
        ;; mark all items as obsolete
        (newsticker--cache-replace-age newsticker--cache
                                       name-symbol
                                       'new 'obsolete-new)
        (newsticker--cache-replace-age newsticker--cache
                                       name-symbol
                                       'old 'obsolete-old)
        (newsticker--cache-replace-age newsticker--cache
                                       name-symbol
                                       'feed 'obsolete-old)

        ;; check Atom/RSS version and call corresponding parser
        (condition-case error-data
            (if (cond
                 ;; RSS 0.91
                 ((and (eq 'rss (xml-node-name topnode))
                       (string= "0.91" (xml-get-attribute topnode 'version)))
                  (setq imageurl (newsticker--get-logo-url-rss-0.91 topnode))
                  (newsticker--parse-rss-0.91 name time topnode))
                 ;; RSS 0.92
                 ((and (eq 'rss (xml-node-name topnode))
                       (string= "0.92" (xml-get-attribute topnode 'version)))
                  (setq imageurl (newsticker--get-logo-url-rss-0.92 topnode))
                  (newsticker--parse-rss-0.92 name time topnode))
                 ;; RSS 1.0
                 ((eq 'rdf:RDF (xml-node-name topnode))
                  (setq imageurl (newsticker--get-logo-url-rss-1.0 topnode))
                  (newsticker--parse-rss-1.0 name time topnode))
                 ;; RSS 2.0
                 ((and (eq 'rss (xml-node-name topnode))
                       (string= "2.0" (xml-get-attribute topnode 'version)))
                  (setq imageurl (newsticker--get-logo-url-rss-2.0 topnode))
                  (newsticker--parse-rss-2.0 name time topnode))
                 ;; Atom 0.3
                 ((and (eq 'feed (xml-node-name topnode))
                       (string= "http://purl.org/atom/ns#"
                                (xml-get-attribute topnode 'xmlns)))
                  (setq imageurl (newsticker--get-logo-url-atom-0.3 topnode))
                  (newsticker--parse-atom-0.3 name time topnode))
                 ;; Atom 1.0
                 ((and (eq 'feed (xml-node-name topnode))
                       (string= "http://www.w3.org/2005/Atom"
                                (xml-get-attribute topnode 'xmlns)))
                  (setq imageurl (newsticker--get-logo-url-atom-1.0 topnode))
                  (newsticker--parse-atom-1.0 name time topnode))
                 ;; unknown feed type
                 (t
                  (newsticker--debug-msg "Feed type unknown: %s: %s"
                                         (xml-node-name topnode) name)
                  nil))
                (setq something-was-added t))
          (xerror (message "sentinelerror in %s: %s" name error-data)))

        ;; Remove those old items from cache which have been removed from
        ;; the feed
        (newsticker--cache-replace-age newsticker--cache
                                       name-symbol 'obsolete-old 'deleteme)
        (newsticker--cache-remove newsticker--cache name-symbol
                                  'deleteme)
        ;; Remove those new items from cache which have been removed from
        ;; the feed.  Or keep them as `obsolete'
        (if (not newsticker-keep-obsolete-items)
            (newsticker--cache-remove newsticker--cache
                                      name-symbol 'obsolete-new)
          (setq newsticker--cache
                (newsticker--cache-mark-expired
                 newsticker--cache name-symbol 'obsolete 'obsolete-expired
                 newsticker-obsolete-item-max-age))
          (newsticker--cache-remove newsticker--cache
                                    name-symbol 'obsolete-expired)
          (newsticker--cache-replace-age newsticker--cache
                                         name-symbol 'obsolete-new
                                         'obsolete))
        (newsticker--update-process-ids)
        ;; setup scrollable text
        (when (= 0 (length newsticker--process-ids))
          (newsticker--ticker-text-setup))
        (setq newsticker--latest-update-time (current-time))
        (when something-was-added
          ;; FIXME: should we care about removed items as well?
          (newsticker--cache-update)
          (newsticker--buffer-set-uptodate nil))
        ;; kill the process buffer if wanted
        (unless newsticker-debug
          (kill-buffer (process-buffer process)))
        ;; launch retrieval of image
        (when (and imageurl
                   (string-match "%l" newsticker-heading-format))
          (newsticker--image-get name imageurl))))))

(defun newsticker--get-logo-url-atom-1.0 (node)
  "Return logo URL from atom 1.0 data in NODE."
  (car (xml-node-children
        (car (xml-get-children node 'logo)))))

(defun newsticker--get-logo-url-atom-0.3 (node)
  "Return logo URL from atom 0.3 data in NODE."
  (car (xml-node-children
        (car (xml-get-children (car (xml-get-children node 'image)) 'url)))))

(defun newsticker--get-logo-url-rss-2.0 (node)
  "Return logo URL from RSS 2.0 data in NODE."
  (car (xml-node-children
        (car (xml-get-children
              (car (xml-get-children
                    (car (xml-get-children node 'channel)) 'image)) 'url)))))

(defun newsticker--get-logo-url-rss-1.0 (node)
  "Return logo URL from RSS 1.0 data in NODE."
  (car (xml-node-children
        (car (xml-get-children (car (xml-get-children node 'image)) 'url)))))

(defun newsticker--get-logo-url-rss-0.92 (node)
  "Return logo URL from RSS 0.92 data in NODE."
  (car (xml-node-children
        (car (xml-get-children (car (xml-get-children node 'image)) 'url)))))

(defun newsticker--get-logo-url-rss-0.91 (node)
  "Return logo URL from RSS 0.91 data in NODE."
  (car (xml-node-children
        (car (xml-get-children (car (xml-get-children node 'image)) 'url)))))

(defun newsticker--parse-atom-0.3 (name time topnode)
  "Parse Atom 0.3 data.
Return value as well as arguments NAME, TIME, and TOPNODE are the
same as in `newsticker--parse-atom-1.0'."
  (newsticker--debug-msg "Parsing Atom 0.3 feed %s" name)
  (let (new-feed new-item)
    (setq new-feed (newsticker--parse-generic-feed
                    name time
                    ;; title
                    (car (xml-node-children
                          (car (xml-get-children topnode 'title))))
                    ;; desc
                    (car (xml-node-children
                          (car (xml-get-children topnode 'content))))
                    ;; link
                    (xml-get-attribute
                     (car (xml-get-children topnode 'link)) 'href)
                    ;; extra-elements
                    (xml-node-children topnode)))
    (setq new-item (newsticker--parse-generic-items
                    name time (xml-get-children topnode 'entry)
                    ;; title-fn
                    (lambda (node)
                      (car (xml-node-children
                            (car (xml-get-children node 'title)))))
                    ;; desc-fn
                    (lambda (node)
                      (or (car (xml-node-children
                                (car (xml-get-children node 'content))))
                          (car (xml-node-children
                                (car (xml-get-children node 'summary))))))
                    ;; link-fn
                    (lambda (node)
                      (xml-get-attribute
                       (car (xml-get-children node 'link)) 'href))
                    ;; time-fn
                    (lambda (node)
                      (newsticker--decode-rfc822-date
                            (car (xml-node-children
                                  (car (xml-get-children node 'modified))))))
                    ;; guid-fn
                    (lambda (node)
                      (let ((tguid (assoc 'guid (xml-node-children node))))
                        (if (stringp tguid)
                            tguid
                          (car (xml-node-children tguid)))))
                    ;; extra-fn
                    (lambda (node)
                      (xml-node-children node))))
    (or new-item new-feed)))

(defun newsticker--parse-atom-1.0 (name time topnode)
  "Parse Atom 1.0 data.
Argument NAME gives the name of a news feed.  TIME gives the
system time at which the data have been retrieved.  TOPNODE
contains the feed data as returned by the xml parser.

For the Atom 1.0 specification see
http://www.atompub.org/2005/08/17/draft-ietf-atompub-format-11.html"
  (newsticker--debug-msg "Parsing Atom 1.0 feed %s" name)
  (let (new-feed new-item)
    (setq new-feed (newsticker--parse-generic-feed
                    name time
                    ;; title
                    (car (xml-node-children
                          (car (xml-get-children topnode 'title))))
                    ;; desc
                    (car (xml-node-children
                          (car (xml-get-children topnode 'subtitle))))
                    ;; link
                    (car (xml-node-children
                          (car (xml-get-children topnode 'link))))
                    ;; extra-elements
                    (xml-node-children topnode)))
    (setq new-item (newsticker--parse-generic-items
                    name time (xml-get-children topnode 'entry)
                    ;; title-fn
                    (lambda (node)
                      (car (xml-node-children
                            (car (xml-get-children node 'title)))))
                    ;; desc-fn
                    (lambda (node)
                      (or (car (xml-node-children
                                (car (xml-get-children node 'content))))
                          (car (xml-node-children
                                (car (xml-get-children node 'summary))))))
                    ;; link-fn
                    (lambda (node)
                      (car (xml-node-children
                            (car (xml-get-children node 'link)))))
                    ;; time-fn
                    (lambda (node)
                      (newsticker--decode-iso8601-date
                       (or (car (xml-node-children
                                 (car (xml-get-children node 'updated))))
                           (car (xml-node-children
                                 (car (xml-get-children node 'published)))))))
                    ;; guid-fn
                    (lambda (node)
                      (car (xml-node-children
                            (car (xml-get-children node 'id)))))
                    ;; extra-fn
                    (lambda (node)
                      (xml-node-children node))))
    (or new-item new-feed)))

(defun newsticker--parse-rss-0.91 (name time topnode)
  "Parse RSS 0.91 data.
Return value as well as arguments NAME, TIME, and TOPNODE are the
same as in `newsticker--parse-atom-1.0'.

For the RSS 0.91 specification see http://backend.userland.com/rss091 or
http://my.netscape.com/publish/formats/rss-spec-0.91.html."
  (newsticker--debug-msg "Parsing RSS 0.91 feed %s" name)
  (let* ((channelnode (car (xml-get-children topnode 'channel)))
         (pub-date (newsticker--decode-rfc822-date
                    (car (xml-node-children
                          (car (xml-get-children channelnode 'pubDate))))))
         is-new-feed has-new-items)
    (setq is-new-feed (newsticker--parse-generic-feed
                       name time
                       ;; title
                       (car (xml-node-children
                             (car (xml-get-children channelnode 'title))))
                       ;; desc
                       (car (xml-node-children
                             (car (xml-get-children channelnode
                                                    'description))))
                       ;; link
                       (car (xml-node-children
                             (car (xml-get-children channelnode 'link))))
                       ;; extra-elements
                       (xml-node-children channelnode)))
    (setq has-new-items (newsticker--parse-generic-items
                         name time (xml-get-children channelnode 'item)
                         ;; title-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'title)))))
                         ;; desc-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'description)))))
                         ;; link-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'link)))))
                         ;; time-fn
                         (lambda (node)
                           pub-date)
                         ;; guid-fn
                         (lambda (node)
                           nil)
                         ;; extra-fn
                         (lambda (node)
                           (xml-node-children node))))
    (or has-new-items is-new-feed)))

(defun newsticker--parse-rss-0.92 (name time topnode)
  "Parse RSS 0.92 data.
Return value as well as arguments NAME, TIME, and TOPNODE are the
same as in `newsticker--parse-atom-1.0'.

For the RSS 0.92 specification see http://backend.userland.com/rss092."
  (newsticker--debug-msg "Parsing RSS 0.92 feed %s" name)
  (let* ((channelnode (car (xml-get-children topnode 'channel)))
         (pub-date (newsticker--decode-rfc822-date
                    (car (xml-node-children
                          (car (xml-get-children channelnode 'pubDate))))))
         is-new-feed has-new-items)
    (setq is-new-feed (newsticker--parse-generic-feed
                       name time
                       ;; title
                       (car (xml-node-children
                             (car (xml-get-children channelnode 'title))))
                       ;; desc
                       (car (xml-node-children
                             (car (xml-get-children channelnode
                                                    'description))))
                       ;; link
                       (car (xml-node-children
                             (car (xml-get-children channelnode 'link))))
                       ;; extra-elements
                       (xml-node-children channelnode)))
    (setq has-new-items (newsticker--parse-generic-items
                         name time (xml-get-children channelnode 'item)
                         ;; title-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'title)))))
                         ;; desc-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'description)))))
                         ;; link-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'link)))))
                         ;; time-fn
                         (lambda (node)
                           pub-date)
                         ;; guid-fn
                         (lambda (node)
                           nil)
                         ;; extra-fn
                         (lambda (node)
                           (xml-node-children node))))
    (or has-new-items is-new-feed)))

(defun newsticker--parse-rss-1.0 (name time topnode)
  "Parse RSS 1.0 data.
Return value as well as arguments NAME, TIME, and TOPNODE are the
same as in `newsticker--parse-atom-1.0'.

For the RSS 1.0 specification see http://web.resource.org/rss/1.0/spec."
  (newsticker--debug-msg "Parsing RSS 1.0 feed %s" name)
  (let* ((channelnode (car (xml-get-children topnode 'channel)))
         is-new-feed has-new-items)
    (setq is-new-feed (newsticker--parse-generic-feed
                       name time
                       ;; title
                       (car (xml-node-children
                             (car (xml-get-children channelnode 'title))))
                       ;; desc
                       (car (xml-node-children
                             (car (xml-get-children channelnode
                                                    'description))))
                       ;; link
                       (car (xml-node-children
                             (car (xml-get-children channelnode 'link))))
                       ;; extra-elements
                       (xml-node-children channelnode)))
    (setq has-new-items (newsticker--parse-generic-items
                         name time (xml-get-children topnode 'item)
                         ;; title-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'title)))))
                         ;; desc-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node
                                                        'description)))))
                         ;; link-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'link)))))
                         ;; time-fn
                         (lambda (node)
                           (newsticker--decode-iso8601-date
                            (car (xml-node-children
                                  (car (xml-get-children node 'dc:date))))))
                         ;; guid-fn
                         (lambda (node)
                           nil)
                         ;; extra-fn
                         (lambda (node)
                           (xml-node-children node))))
    (or has-new-items is-new-feed)))

(defun newsticker--parse-rss-2.0 (name time topnode)
  "Parse RSS 2.0 data.
Return value as well as arguments NAME, TIME, and TOPNODE are the
same as in `newsticker--parse-atom-1.0'.

For the RSS 2.0 specification see http://blogs.law.harvard.edu/tech/rss."
  (newsticker--debug-msg "Parsing RSS 2.0 feed %s" name)
  (let* ((channelnode (car (xml-get-children topnode 'channel)))
         is-new-feed has-new-items)
    (setq is-new-feed (newsticker--parse-generic-feed
                       name time
                       ;; title
                       (car (xml-node-children
                             (car (xml-get-children channelnode 'title))))
                       ;; desc
                       (car (xml-node-children
                             (car (xml-get-children channelnode
                                                    'description))))
                       ;; link
                       (car (xml-node-children
                             (car (xml-get-children channelnode 'link))))
                       ;; extra-elements
                       (xml-node-children channelnode)))
    (setq has-new-items (newsticker--parse-generic-items
                         name time (xml-get-children channelnode 'item)
                         ;; title-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'title)))))
                         ;; desc-fn
                         (lambda (node)
                           (or (car (xml-node-children
                                     (car (xml-get-children node
                                                            'content:encoded))))
                               (car (xml-node-children
                                     (car (xml-get-children node
                                                            'description))))))
                         ;; link-fn
                         (lambda (node)
                           (car (xml-node-children
                                 (car (xml-get-children node 'link)))))
                         ;; time-fn
                         (lambda (node)
                           (newsticker--decode-rfc822-date
                            (car (xml-node-children
                                  (car (xml-get-children node 'pubDate))))))
                         ;; guid-fn
                         (lambda (node)
                           (let* ((tguid (assoc 'guid
                                                (xml-node-children node))))
                             (if (stringp tguid)
                                 tguid
                               (car (xml-node-children tguid)))))
                         ;; extra-fn
                         (lambda (node)
                           (xml-node-children node))))
    (or has-new-items is-new-feed)))

(defun newsticker--parse-generic-feed (name time title desc link
                                            extra-elements)
  "Parse generic news feed data.
Argument NAME gives the name of a news feed.  TIME gives the
system time at which the data have been retrieved.

The arguments TITLE, DESC, LINK, and EXTRA-ELEMENTS give the feed's title,
description, link, and extra elements resp."
  (let ((title (or title "[untitled]"))
        (link (or link ""))
        (old-item nil)
        (position 0)
        (something-was-added nil))
    ;; decode numeric entities
    (setq title (newsticker--decode-numeric-entities title))
    (setq desc  (newsticker--decode-numeric-entities desc))
    (setq link  (newsticker--decode-numeric-entities link))
    ;; remove whitespace from title, desc, and link
    (setq title (newsticker--remove-whitespace title))
    (setq desc (newsticker--remove-whitespace desc))
    (setq link (newsticker--remove-whitespace link))

    ;; handle the feed itself
    (unless (newsticker--cache-contains newsticker--cache
                                        (intern name) title
                                        desc link 'feed)
      (setq something-was-added t))
    (setq newsticker--cache
          (newsticker--cache-add newsticker--cache (intern name)
                                 title desc link time 'feed position
                                 extra-elements 'feed time))
    something-was-added))

(defun newsticker--parse-generic-items (name time itemlist
                                             title-fn desc-fn
                                             link-fn time-fn
                                             guid-fn extra-fn)
  "Parse generic news feed data.
Argument NAME gives the name of a news feed.  TIME gives the
system time at which the data have been retrieved.  ITEMLIST
contains the news items returned by the xml parser.

The arguments TITLE-FN, DESC-FN, LINK-FN, TIME-FN, GUID-FN, and
EXTRA-FN give functions for extracting title, description, link,
time, guid, and extra-elements resp.  They are called with one
argument, which is one of the items in ITEMLIST."
  (let (title desc link
        (old-item nil)
        (position 0)
        (something-was-added nil))
    ;; gather all items for this feed
    (mapc (lambda (node)
            (setq position (1+ position))
            (setq title (or (funcall title-fn node) "[untitled]"))
            (setq desc (funcall desc-fn node))
            (setq link (or (funcall link-fn node) ""))
            (setq time (or (funcall time-fn node) time))
            ;; It happened that the title or description
            ;; contained evil HTML code that confused the
            ;; xml parser.  Therefore:
            (unless (stringp title)
              (setq title (prin1-to-string title)))
            (unless (or (stringp desc) (not desc))
              (setq desc (prin1-to-string desc)))
            ;; ignore items with empty title AND empty desc
            (when (or (> (length title) 0)
                      (> (length desc) 0))
              ;; decode numeric entities
              (setq title (newsticker--decode-numeric-entities title))
              (when desc
                (setq desc  (newsticker--decode-numeric-entities desc)))
              (setq link (newsticker--decode-numeric-entities link))
              ;; remove whitespace from title, desc, and link
              (setq title (newsticker--remove-whitespace title))
              (setq desc (newsticker--remove-whitespace desc))
              (setq link (newsticker--remove-whitespace link))
              ;; add data to cache
              ;; do we have this item already?
              (let* ((guid (funcall guid-fn node)))
                ;;(message "guid=%s" guid)
                (setq old-item
                      (newsticker--cache-contains newsticker--cache
                                                  (intern name) title
                                                  desc link nil guid)))
              ;; add this item, or mark it as old, or do nothing
              (let ((age1 'new)
                    (age2 'old)
                    (item-new-p nil))
                (if old-item
                    (let ((prev-age (newsticker--age old-item)))
                      (unless
                          newsticker-automatically-mark-items-as-old
                        (if (eq prev-age 'obsolete-old)
                            (setq age2 'old)
                          (setq age2 'new)))
                      (if (eq prev-age 'immortal)
                          (setq age2 'immortal)))
                  ;; item was not there
                  (setq item-new-p t)
                  (setq something-was-added t))
                (setq newsticker--cache
                      (newsticker--cache-add
                       newsticker--cache (intern name) title desc link
                       time age1 position (funcall extra-fn node)
                       age2))
                (when item-new-p
                  (let ((item (newsticker--cache-contains
                               newsticker--cache (intern name) title
                               desc link nil)))
                    (if newsticker-auto-mark-filter-list
                        (newsticker--run-auto-mark-filter name item))
                    (run-hook-with-args
                     'newsticker-new-item-functions name item))))))
          itemlist)
    something-was-added))

(defun newsticker--display-tick ()
  "Called from the display timer.
This function calls a display function, according to the variable
`newsticker-scroll-smoothly'."
  (if newsticker-scroll-smoothly
      (newsticker--display-scroll)
    (newsticker--display-jump)))

(defsubst newsticker--echo-area-clean-p ()
  "Check whether somebody is using the echo area / minibuffer.
Return t if echo area and minibuffer are unused."
  (not (or (active-minibuffer-window)
           (and (current-message)
                (not (string= (current-message)
                              newsticker--prev-message))))))

(defun newsticker--display-jump ()
  "Called from the display timer.
This function displays the next ticker item in the echo area, unless
there is another message displayed or the minibuffer is active."
  (let ((message-log-max nil));; prevents message text from being logged
    (when (newsticker--echo-area-clean-p)
      (setq newsticker--item-position (1+ newsticker--item-position))
      (when (>= newsticker--item-position (length newsticker--item-list))
        (setq newsticker--item-position 0))
      (setq newsticker--prev-message
            (nth newsticker--item-position newsticker--item-list))
      (message "%s" newsticker--prev-message))))

(defun newsticker--display-scroll ()
  "Called from the display timer.
This function scrolls the ticker items in the echo area, unless
there is another message displayed or the minibuffer is active."
  (when (newsticker--echo-area-clean-p)
    (let* ((width (- (frame-width) 1))
           (message-log-max nil);; prevents message text from being logged
           (i newsticker--item-position)
           subtext
           (s-text newsticker--scrollable-text)
           (l (length s-text)))
      ;; don't show anything if there is nothing to show
      (unless (< (length s-text) 1)
        ;; repeat the ticker string if it is shorter than frame width
        (while (< (length s-text) width)
          (setq s-text (concat s-text s-text)))
        ;; get the width of the printed string
        (setq l (length s-text))
        (cond ((< i (- l width))
               (setq subtext (substring s-text i (+ i width))))
              (t
               (setq subtext (concat
                              (substring s-text i l)
                              (substring s-text 0 (- width (- l i)))))))
        ;; Take care of multibyte strings, for which (string-width) is
        ;; larger than (length).
        ;; Actually, such strings may be smaller than (frame-width)
        ;; because return values of (string-width) are too large:
        ;; (string-width "<japanese character>") => 2
        (let ((t-width (1- (length subtext))))
          (while (> (string-width subtext) width)
            (setq subtext (substring subtext 0 t-width))
            (setq t-width (1- t-width))))
        ;; show the ticker text and save current position
        (message "%s" subtext)
        (setq newsticker--prev-message subtext)
        (setq newsticker--item-position (1+ i))
        (when (>= newsticker--item-position l)
          (setq newsticker--item-position 0))))))

;; ======================================================================
;;; misc
;; ======================================================================
(defun newsticker--decode-numeric-entities (string)
  "Decode SGML numeric entities by their respective utf characters.
This function replaces numeric entities in the input STRING and
returns the modified string.  For example \"&#42;\" gets replaced
by \"*\"."
  (if (and string (stringp string))
      (let ((start 0))
        (while (string-match "&#\\([0-9]+\\);" string start)
          (condition-case nil
              (setq string (replace-match
                            (string (read (substring string
                                                     (match-beginning 1)
                                                     (match-end 1))))
                            nil nil string))
            (error nil))
          (setq start (1+ (match-beginning 0))))
        string)
    nil))

(defun newsticker--remove-whitespace (string)
  "Remove leading and trailing whitespace from STRING."
  ;; we must have ...+ but not ...* in the regexps otherwise xemacs loops
  ;; endlessly...
  (when (and string (stringp string))
    (replace-regexp-in-string
     "[ \t\r\n]+$" ""
     (replace-regexp-in-string "^[ \t\r\n]+" "" string))))

(defun newsticker--do-forget-preformatted (item)
  "Forget pre-formatted data for ITEM.
Remove the pre-formatted from `newsticker--cache'."
  (if (nthcdr 7 item)
      (setcar (nthcdr 7 item) nil))
  (if (nthcdr 6 item)
      (setcar (nthcdr 6 item) nil)))

(defun newsticker--forget-preformatted ()
  "Forget all cached pre-formatted data.
Remove the pre-formatted from `newsticker--cache'."
  (mapc (lambda (feed)
          (mapc 'newsticker--do-forget-preformatted
                (cdr feed)))
        newsticker--cache)
  (newsticker--buffer-set-uptodate nil))

(defun newsticker--debug-msg (string &rest args)
  "Print newsticker debug messages.
This function calls `message' with arguments STRING and ARGS, if
`newsticker-debug' is non-nil."
  (and newsticker-debug
       ;;(not (active-minibuffer-window))
       ;;(not (current-message))
       (apply 'message string args)))

(defun newsticker--decode-iso8601-date (iso8601-string)
  "Return ISO8601-STRING in format like `decode-time'.
Converts from ISO-8601 to Emacs representation.
Examples:
2004-09-17T05:09:49+00:00
2004-09-17T05:09+00:00
2004-09-17T05:09:49
2004-09-17T05:09
2004-09-17
2004-09
2004"
  (if iso8601-string
      (when (string-match
             (concat
              "^ *\\([0-9]\\{4\\}\\)"
              "\\(-\\([0-9]\\{2\\}\\)"
              "\\(-\\([0-9]\\{2\\}\\)"
              "\\(T"
              "\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
              "\\(:\\([0-9]\\{2\\}\\)\\)?"
              "\\(\\([-+Z]\\)\\(\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)\\)?\\)?"
              "\\)?\\)?\\)? *$")
             iso8601-string)
        (let ((year (read (match-string 1 iso8601-string)))
              (month (read (or (match-string 3 iso8601-string)
                               "1")))
              (day (read (or (match-string 5 iso8601-string)
                             "1")))
              (hour (read (or (match-string 7 iso8601-string)
                              "0")))
              (minute (read (or (match-string 8 iso8601-string)
                                "0")))
              ;;(second (read (or (match-string 10 iso8601-string)
              ;; "0")))
              (sign (match-string 12 iso8601-string))
              (offset-hour (read (or (match-string 14 iso8601-string)
                                     "0")))
              (offset-minute (read (or (match-string 15 iso8601-string)
                                       "0")))
              (second 0))
          (cond ((string= sign "+")
                 (setq hour (- hour offset-hour))
                 (setq minute (- minute offset-minute)))
                ((string= sign "-")
                 (setq hour (+ hour offset-hour))
                 (setq minute (+ minute offset-minute))))
          ;; if UTC subtract current-time-zone offset
          ;;(setq second (+ (car (current-time-zone)) second)))

          (condition-case nil
              (encode-time second minute hour day month year t)
            (error
             (message "Cannot decode \"%s\"" iso8601-string)
             nil))))
    nil))

(defun newsticker--decode-rfc822-date (rfc822-string)
  "Return RFC822-STRING in format like `decode-time'.
Converts from RFC822 to Emacs representation.
Examples:
Sat, 07 Sep 2002 00:00:01 GMT
07 Sep 2002 00:00:01 GMT
07 Sep 2002"
  (if (and rfc822-string (stringp rfc822-string))
      (when (string-match
             (concat
              "\\s-*"
              ;; week day
              "\\(\\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\)\\s-*,?\\)?\\s-*"
              ;; day
              "\\([0-9]\\{1,2\\}\\)\\s-+"
              ;; month
              "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|"
              "Sep\\|Oct\\|Nov\\|Dec\\)\\s-+"
              ;; year
              "\\([0-9]\\{2,4\\}\\)"
              ;; time may be missing
              "\\(\\s-+"
              ;; hour
              "\\([0-9]\\{2\\}\\)"
              ;; minute
              ":\\([0-9]\\{2\\}\\)"
              ;; second
              "\\(:\\([0-9]\\{2\\}\\)\\)?"
              ;; zone -- fixme
              "\\(\\s-+.*\\)?"
              "\\)?")
             rfc822-string)
        (let ((day (read (match-string 3 rfc822-string)))
              (month-name (match-string 4 rfc822-string))
              (month 0)
              (year (read (match-string 5 rfc822-string)))
              (hour (read (or (match-string 7 rfc822-string) "0")))
              (minute (read (or (match-string 8 rfc822-string) "0")))
              (second (read (or (match-string 10 rfc822-string) "0")))
              ;;(zone (match-string 11 rfc822-string))
              )
          (condition-case error-data
              (let ((i 1))
                (mapc (lambda (m)
                        (if (string= month-name m)
                            (setq month i))
                        (setq i (1+ i)))
                      '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
                        "Sep" "Oct" "Nov" "Dec"))
                (encode-time second minute hour day month year t))
            (error
             (message "Cannot decode \"%s\": %s %s" rfc822-string
                      (car error-data) (cdr error-data))
             nil))))
      nil))

(defun newsticker--lists-intersect-p (list1 list2)
  "Return t if LIST1 and LIST2 share elements."
  (let ((result nil))
    (mapc (lambda (elt)
            (if (memq elt list2)
                (setq result t)))
          list1)
    result))

(defun newsticker--update-process-ids ()
  "Update list of ids of active newsticker processes.
Checks list of active processes against list of newsticker processes."
  (let ((active-procs (process-list))
        (new-list nil))
    (mapc (lambda (proc)
            (let ((id (process-id proc)))
              (if (memq id newsticker--process-ids)
                  (setq new-list (cons id new-list)))))
          active-procs)
    (setq newsticker--process-ids new-list))
  (force-mode-line-update))

;; ======================================================================
;;; images
;; ======================================================================
(defun newsticker--image-get (feed-name url)
  "Get image of the news site FEED-NAME from URL.
If the image has been downloaded in the last 24h do nothing."
  (let ((image-name (concat newsticker-imagecache-dirname "/"
                            feed-name)))
    (if (and (file-exists-p image-name)
             (time-less-p (current-time)
                          (time-add (nth 5 (file-attributes image-name))
                                    (seconds-to-time 86400))))
        (newsticker--debug-msg "%s: Getting image for %s skipped"
                               (format-time-string "%A, %H:%M" (current-time))
                               feed-name)
      ;; download
      (newsticker--debug-msg "%s: Getting image for %s"
                             (format-time-string "%A, %H:%M" (current-time))
                             feed-name)
      (let* ((buffername (concat " *newsticker-wget-image-" feed-name "*"))
             (item (or (assoc feed-name newsticker-url-list)
                       (assoc feed-name newsticker-url-list-defaults)
                       (error
                        "Cannot get news for %s: Check newsticker-url-list"
                        feed-name)))
             (wget-arguments (or (car (cdr (cdr (cdr (cdr item)))))
                                 newsticker-wget-arguments)))
        (save-excursion
          (set-buffer (get-buffer-create buffername))
          (erase-buffer)
          ;; throw an error if there is an old wget-process around
          (if (get-process feed-name)
              (error "Another wget-process is running for image %s"
                     feed-name))
          ;; start wget
          (let* ((args (append wget-arguments (list url)))
                 (proc (apply 'start-process feed-name buffername
                              newsticker-wget-name args)))
            (set-process-coding-system proc 'no-conversion 'no-conversion)
            (set-process-sentinel proc 'newsticker--image-sentinel)))))))

(defun newsticker--image-sentinel (process event)
  "Sentinel for image-retrieving PROCESS caused by EVENT."
  (let* ((p-status (process-status process))
         (exit-status (process-exit-status process))
         (feed-name (process-name process)))
    ;; catch known errors (zombie processes, rubbish-xml, etc.)
    ;; if an error occurs the news feed is not updated!
    (catch 'oops
      (unless (and (eq p-status 'exit)
                   (= exit-status 0))
        (message "%s: Error while retrieving image from %s"
                 (format-time-string "%A, %H:%M" (current-time))
                 feed-name)
        (throw 'oops nil))
      (let (image-name)
        (save-excursion
          (set-buffer (process-buffer process))
          (setq image-name (concat newsticker-imagecache-dirname "/"
                                   feed-name))
          (set-buffer-file-coding-system 'no-conversion)
          ;; make sure the cache dir exists
          (unless (file-directory-p newsticker-imagecache-dirname)
            (make-directory newsticker-imagecache-dirname))
          ;; write and close buffer
          (let ((require-final-newline nil)
                (backup-inhibited t)
                (coding-system-for-write 'no-conversion))
            (write-region nil nil image-name nil 'quiet))
          (set-buffer-modified-p nil)
          (kill-buffer (current-buffer)))))))

(defun newsticker--image-read (feed-name-symbol disabled)
  "Read the cached image for FEED-NAME-SYMBOL from disk.
If DISABLED is non-nil the image will be converted to a disabled look
\(unless `newsticker-enable-logo-manipulations' is not t\).
Return the image."
  (let ((image-name (concat newsticker-imagecache-dirname "/"
                            (symbol-name feed-name-symbol)))
        (img nil))
    (when (file-exists-p image-name)
      (condition-case error-data
          (setq img (create-image
                     image-name nil nil
                     :conversion (and newsticker-enable-logo-manipulations
                                      disabled
                                      'disabled)
                     :mask (and newsticker-enable-logo-manipulations
                                'heuristic)
                     :ascent 70))
        (error
         (message "Error: cannot create image for %s: %s"
                  feed-name-symbol error-data))))
    img))

;; ======================================================================
;;; imenu stuff
;; ======================================================================
(defun newsticker--imenu-create-index ()
  "Scan newsticker buffer and return an index for imenu."
  (save-excursion
    (goto-char (point-min))
    (let ((index-alist nil)
          (feed-list nil)
          (go-ahead t))
      (while go-ahead
        (let ((type  (get-text-property (point) 'nt-type))
              (title (get-text-property (point) 'nt-title)))
          (cond ((eq type 'feed)
                 ;; we're on a feed heading
                 (when feed-list
                   (if index-alist
                       (nconc index-alist (list feed-list))
                     (setq index-alist (list feed-list))))
                 (setq feed-list (list title)))
                (t
                 (nconc feed-list
                        (list (cons title (point)))))))
	(setq go-ahead (newsticker--buffer-goto '(item feed))))
      (if index-alist
	  (nconc index-alist (list feed-list))
	(setq index-alist (list feed-list)))
      index-alist)))

(defun newsticker--imenu-goto (name pos &rest args)
  "Go to item NAME at position POS and show item.
ARGS are ignored."
  (goto-char pos)
  ;; show headline
  (newsticker--buffer-goto '(desc extra feed item))
  (let* ((inhibit-read-only t)
         (pos1 (max (point-min) (1- pos)))
         (pos2 (max pos1 (1- (point))))
         (inv-prop (get-text-property pos 'invisible))
         (org-inv-prop (get-text-property pos 'org-invisible)))
    (when (eq org-inv-prop nil)
      (add-text-properties pos1 pos2 (list 'invisible nil
                                          'org-invisible inv-prop))))
  ;; show desc
  (newsticker-show-entry))

;; ======================================================================
;;; buffer stuff
;; ======================================================================
(defun newsticker--buffer-set-uptodate (value)
  "Set the uptodate-status of the newsticker buffer to VALUE.
The mode-line is changed accordingly."
  (setq newsticker--buffer-uptodate-p value)
  (let ((b (get-buffer "*newsticker*")))
    (when b
      (save-excursion
       (set-buffer b)
       (if value
           (setq mode-name "Newsticker -- up to date -- ")
         (setq mode-name "Newsticker -- NEED UPDATE -- ")))
      (force-mode-line-update 0))))

(defun newsticker--buffer-redraw ()
  "Redraw the newsticker window."
  (if (fboundp 'force-window-update)
      (force-window-update (current-buffer))
    (redraw-frame (selected-frame)))
  (run-hooks 'newsticker-buffer-change-hook)
  (sit-for 0))

(defun newsticker--buffer-insert-all-items ()
  "Insert all cached newsticker items into the current buffer.
Keeps order of feeds as given in `newsticker-url-list' and
`newsticker-url-list-defaults'."
  (goto-char (point-min))
  (mapc (lambda (url-item)
          (let* ((feed-name (car url-item))
                 (feed-name-symbol (intern feed-name))
                 (feed (assoc feed-name-symbol newsticker--cache))
                 (items (cdr feed))
                 (pos (point)))
            (when feed
              ;; insert the feed description
              (mapc (lambda (item)
                      (when (eq (newsticker--age item) 'feed)
                        (newsticker--buffer-insert-item item
                                                        feed-name-symbol)))
                    items)
              ;;insert the items
              (mapc (lambda (item)
                      (if (memq (newsticker--age item) '(new immortal old
                                                             obsolete))
                          (newsticker--buffer-insert-item item
                                                          feed-name-symbol)))
                    items)
              (put-text-property pos (point) 'feed (car feed))

              ;; insert empty line between feeds
              (let ((p (point)))
                (insert "\n")
                (put-text-property p (point) 'hard t)))))
        (append newsticker-url-list newsticker-url-list-defaults))

  (newsticker--buffer-set-faces (point-min) (point-max))
  (newsticker--buffer-set-invisibility (point-min) (point-max))
  (goto-char (point-min)))

(defun newsticker--buffer-insert-item (item &optional feed-name-symbol)
  "Insert a news item in the current buffer.
Insert a formatted representation of the ITEM.  The optional parameter
FEED-NAME-SYMBOL determines how the item is formatted and whether the
item-retrieval time is added as well."
  ;; insert headline
  (if (eq (newsticker--age item) 'feed)
      (newsticker--buffer-do-insert-text item 'feed feed-name-symbol)
    (newsticker--buffer-do-insert-text item 'item feed-name-symbol))
  ;; insert the description
  (newsticker--buffer-do-insert-text item 'desc feed-name-symbol))

(defun newsticker--buffer-do-insert-text (item type feed-name-symbol)
  "Actually insert contents of news item, format it, render it and all that.
ITEM is a news item, TYPE tells which part of the item shall be inserted,
FEED-NAME-SYMBOL tells to which feed this item belongs."
  (let* ((pos (point))
         (format newsticker-desc-format)
         (pos-date-start nil)
         (pos-date-end nil)
         (pos-stat-start nil)
         (pos-stat-end nil)
         (pos-text-start nil)
         (pos-text-end nil)
         (pos-extra-start nil)
         (pos-extra-end nil)
         (pos-enclosure-start nil)
         (pos-enclosure-end nil)
         (age (newsticker--age item))
         (preformatted-contents (newsticker--preformatted-contents item))
         (preformatted-title (newsticker--preformatted-title item)))
    (cond ((and preformatted-contents
                (not (eq (aref preformatted-contents 0) ?\n));; we must
                                                       ;; NOT have a line
                                                       ;; break!
                (eq type 'desc))
           (insert preformatted-contents))
          ((and preformatted-title
                (not (eq (aref preformatted-title 0) ?\n));; we must NOT have a
                                                    ;; line break!
                (eq type 'item))
           (insert preformatted-title))
          (t
           ;; item was not formatted before.
           ;; Let's go.
           (if (eq type 'item)
               (setq format newsticker-item-format)
             (if (eq type 'feed)
                 (setq format newsticker-heading-format)))

           (while (> (length format) 0)
             (let ((prefix (if (> (length format) 1)
                               (substring format 0 2)
                             "")))
               (cond ((string= "%c" prefix)
                      ;; contents
                      (when (newsticker--desc item)
                        (setq pos-text-start (point-marker))
                        (insert (newsticker--desc item))
                        (setq pos-text-end (point-marker)))
                      (setq format (substring format 2)))
                     ((string= "%d" prefix)
                      ;; date
                      (setq pos-date-start (point-marker))
                      (if (newsticker--time item)
                          (insert (format-time-string newsticker-date-format
                                                      (newsticker--time item))))
                      (setq pos-date-end (point-marker))
                      (setq format (substring format 2)))
                     ((string= "%l" prefix)
                      ;; logo
                      (let ((disabled (cond ((eq (newsticker--age item) 'feed)
                                             (= (newsticker--stat-num-items
                                                 feed-name-symbol 'new) 0))
                                            (t
                                             (not (eq (newsticker--age item)
                                                      'new))))))
                        (let ((img (newsticker--image-read feed-name-symbol
                                                           disabled)))
                          (when img
                            (newsticker--insert-image img (car item)))))
                      (setq format (substring format 2)))
                     ((string= "%L" prefix)
                      ;; logo or title
                      (let ((disabled (cond ((eq (newsticker--age item) 'feed)
                                             (= (newsticker--stat-num-items
                                                 feed-name-symbol 'new) 0))
                                            (t
                                             (not (eq (newsticker--age item)
                                                      'new))))))
                        (let ((img (newsticker--image-read feed-name-symbol
                                                           disabled)))
                          (if img
                              (newsticker--insert-image img (car item))
                            (when (car item)
                              (setq pos-text-start (point-marker))
			      (if (eq (newsticker--age item) 'feed)
				  (insert (newsticker--title item))
				;; FIXME: This is not the "real" title!
				(insert (format "%s"
						(car (newsticker--cache-get-feed
						      feed-name-symbol)))))
                              (setq pos-text-end (point-marker))))))
                      (setq format (substring format 2)))
                     ((string= "%s" prefix)
                      ;; statistics
                      (setq pos-stat-start (point-marker))
                      (if (eq (newsticker--age item) 'feed)
                          (insert (newsticker--buffer-statistics
                                   feed-name-symbol)))
                      (setq pos-stat-end (point-marker))
                      (setq format (substring format 2)))
                     ((string= "%t" prefix)
                      ;; title
                      (when (car item)
                        (setq pos-text-start (point-marker))
                        (insert (car item))
                        (setq pos-text-end (point-marker)))
                      (setq format (substring format 2)))
                     ((string-match "%." prefix)
                      ;; unknown specifier!
                      (insert prefix)
                      (setq format (substring format 2)))
                     ((string-match "^\\([^%]*\\)\\(.*\\)" format) ;; FIXME!
                      ;; everything else
                      (let ((p (point)))
                        (insert (substring format
                                           (match-beginning 1) (match-end 1)))
                        ;; in case that the format string contained newlines
                        (put-text-property p (point) 'hard t))
                      (setq format (substring format (match-beginning 2)))))))

           ;; decode HTML if possible...
           (let ((is-rendered-HTML nil))
             (when (and newsticker-html-renderer pos-text-start pos-text-end)
               (condition-case error-data
                   (save-excursion
                     ;; check whether it is necessary to call html renderer
                     ;; (regexp inspired by htmlr.el)
                     (goto-char pos-text-start)
                     (when (re-search-forward
                            "</?[A-Za-z1-6]*\\|&#?[A-Za-z0-9]+;" pos-text-end t)
                       ;; (message "%s" (newsticker--title item))
                       (let ((w3m-fill-column (if newsticker-use-full-width
                                                  -1 fill-column))
                             (w3-maximum-line-length
                              (if newsticker-use-full-width nil fill-column)))
                         (save-excursion
                           (funcall newsticker-html-renderer pos-text-start
                                    pos-text-end)))
                       (cond ((eq newsticker-html-renderer 'w3m-region)
                              (add-text-properties pos (point-max)
                                                   (list 'keymap
                                                         w3m-minor-mode-map)))
                             ((eq newsticker-html-renderer 'w3-region)
                              (add-text-properties pos (point-max)
                                                   (list 'keymap w3-mode-map))))
                       (setq is-rendered-HTML t)))
                 (error
                  (message "Error: HTML rendering failed: %s, %s"
                           (car error-data) (cdr error-data)))))
             ;; After html rendering there might be chunks of blank
             ;; characters between rendered text and date, statistics or
             ;; whatever.  Remove it
             (when (and (eq type 'item) is-rendered-HTML)
               (goto-char pos)
               (while (re-search-forward "[ \t]*\n[ \t]*" nil t)
                 (replace-match " " nil nil))
               (goto-char (point-max)))
             (when (and newsticker-justification
                        (memq type '(item desc))
                        (not is-rendered-HTML))
               (condition-case nil
                   (let ((use-hard-newlines t))
                     (fill-region pos (point-max) newsticker-justification))
                 (error nil))))

           ;; remove leading and trailing newlines
           (goto-char pos)
           (unless (= 0 (skip-chars-forward " \t\r\n"))
             (delete-region pos (point)))
           (goto-char (point-max))
           (let ((end (point)))
             (unless (= 0 (skip-chars-backward " \t\r\n" (1+ pos)))
               (delete-region (point) end)))
           (goto-char (point-max))
           ;; closing newline
           (unless nil ;;(eq pos (point))
             (insert "\n")
             (put-text-property (1- (point)) (point) 'hard t))

           ;; insert enclosure element
           (when (eq type 'desc)
             (setq pos-enclosure-start (point))
             (newsticker--buffer-insert-enclosure item)
             (setq pos-enclosure-end (point)))

           ;; show extra elements
           (when (eq type 'desc)
             (goto-char (point-max))
             (setq pos-extra-start (point))
             (newsticker--buffer-print-extra-elements item)
             (setq pos-extra-end (point)))

           ;; text properties
           (when (memq type '(feed item))
             (add-text-properties pos (1- (point))
                                  (list 'mouse-face 'highlight
                                        'nt-link (newsticker--link item)
                                        'help-echo
                                        (format "mouse-2: visit item (%s)"
                                                (newsticker--link item))
                                        'keymap newsticker--url-keymap))
             (add-text-properties pos (point)
                                  (list 'nt-title (newsticker--title item)
                                        'nt-desc (newsticker--desc item))))

           (add-text-properties pos (point)
                                (list 'nt-type type
                                      'nt-face type
                                      'nt-age  age
                                      'nt-guid (newsticker--guid item)))
           (when (and pos-date-start pos-date-end)
             (put-text-property pos-date-start pos-date-end 'nt-face 'date))
           (when (and pos-stat-start pos-stat-end)
             (put-text-property pos-stat-start pos-stat-end 'nt-face 'stat))
           (when (and pos-extra-start pos-extra-end)
             (put-text-property pos-extra-start pos-extra-end
                                'nt-face 'extra)
             (put-text-property pos-extra-start pos-extra-end
                                'nt-type 'extra))
           (when (and pos-enclosure-start pos-enclosure-end
                      (> pos-enclosure-end pos-enclosure-start))
             (put-text-property pos-enclosure-start (1- pos-enclosure-end)
                                'nt-face 'enclosure))

           ;; left margin
           ;;(unless (memq type '(feed item))
           ;;(set-left-margin pos (1- (point)) 1))

           ;; save rendered stuff
           (cond ((eq type 'desc)
		  ;; preformatted contents
		  (newsticker--cache-set-preformatted-contents
		   item (buffer-substring pos (point))))
		  ((eq type 'item)
		   ;; preformatted title
		   (newsticker--cache-set-preformatted-title
		    item (buffer-substring pos (point)))))))))

(defun newsticker--buffer-print-extra-elements (item)
  "Insert extra-elements of ITEM in a pretty form into the current buffer."
  (let ((ignored-elements '(items link title description
                                  content:encoded
                                  dc:subject dc:date item guid
                                  pubDate enclosure))
        (left-column-width 1))
    (mapc (lambda (extra-element)
            (unless (memq (car extra-element) ignored-elements)
              (setq left-column-width (max left-column-width
                                           (length (symbol-name
                                                    (car extra-element)))))))
          (newsticker--extra item))
    (mapc (lambda (extra-element)
            (unless (memq (car extra-element) ignored-elements)
              (newsticker--buffer-do-print-extra-element extra-element
                                                         left-column-width)))
          (newsticker--extra item))))

(defun newsticker--buffer-do-print-extra-element (extra-element width)
  "Actually print an EXTRA-ELEMENT using the given WIDTH."
  (let ((name (symbol-name (car extra-element))))
    (insert (format "%s: " name))
    (insert (make-string (- width (length name)) ? )))
  (let (;;(attributes (cadr extra-element)) ;FIXME!!!!
        (contents (cddr extra-element)))
    (cond ((listp contents)
           (mapc (lambda (i)
                   (if (and (stringp i)
                            (string-match "^http://.*" i))
                       (let ((pos (point)))
                         (insert i " ") ; avoid self-reference from the
                                        ; nt-link thing
                         (add-text-properties
                          pos (point)
                          (list 'mouse-face 'highlight
                                'nt-link i
                                'help-echo
                                (format "mouse-2: visit (%s)" i)
                                'keymap newsticker--url-keymap)))
                         (insert (format "%s" i))))
                 contents))
          (t
           (insert (format "%s" contents))))
    (insert "\n")))

(defun newsticker--buffer-insert-enclosure (item)
  "Insert enclosure element of a news ITEM into the current buffer."
  (let ((enclosure (newsticker--enclosure item))
        (beg (point)))
    (when enclosure
      (let ((url (cdr (assoc 'url enclosure)))
            (length (string-to-number (or (cdr (assoc 'length enclosure))
                                          "0")))
            (type (cdr (assoc 'type enclosure))))
        (cond ((> length 1048576)
               (insert (format "Enclosed file (%s, %1.2f MBytes)" type
                               (/ length 1048576))))
              ((> length 1024)
               (insert (format "Enclosed file (%s, %1.2f KBytes)" type
                               (/ length 1024)))))
        (add-text-properties beg (point)
                             (list 'mouse-face 'highlight
                                   'nt-link url
                                   'help-echo (format
                                               "mouse-2: visit (%s)" url)
                                   'keymap newsticker--url-keymap
                                   'nt-face 'enclosure
                                   'nt-type 'desc))
        (insert "\n")))))

(defun newsticker--buffer-statistics (feed-name-symbol)
  "Return a statistic string for the feed given by FEED-NAME-SYMBOL.
See `newsticker-statistics-format'."
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     "%a"
     (format "%d" (newsticker--stat-num-items feed-name-symbol))
     (replace-regexp-in-string
      "%i"
      (format "%d" (newsticker--stat-num-items feed-name-symbol 'immortal))
      (replace-regexp-in-string
       "%n"
       (format "%d" (newsticker--stat-num-items feed-name-symbol 'new))
       (replace-regexp-in-string
        "%o"
        (format "%d" (newsticker--stat-num-items feed-name-symbol 'old))
        (replace-regexp-in-string
         "%O"
         (format "%d" (newsticker--stat-num-items feed-name-symbol 'obsolete))
         newsticker-statistics-format)))))))

(defun newsticker--buffer-set-faces (start end)
  "Add face properties according to mark property.
Scans the buffer between START and END."
  (save-excursion
    ;;(put-text-property start end 'face 'newsticker-default-face)
    (goto-char start)
    (let ((pos1 start)
          (pos2 1)
          (nt-face (get-text-property start 'nt-face))
          (nt-age (get-text-property start 'nt-age)))
      (when nt-face
        (setq pos2 (next-single-property-change (point) 'nt-face))
        (newsticker--set-face-properties pos1 pos2 nt-face nt-age)
        (setq nt-face (get-text-property pos2 'nt-face))
        (setq pos1 pos2))
      (while (and (setq pos2 (next-single-property-change pos1 'nt-face))
                  (<= pos2 end)
                  (> pos2 pos1))
        (newsticker--set-face-properties pos1 pos2 nt-face nt-age)
        (setq nt-face (get-text-property pos2 'nt-face))
        (setq nt-age (get-text-property pos2 'nt-age))
        (setq pos1 pos2)))))

(defun newsticker--buffer-set-invisibility (start end)
  "Add invisibility properties according to nt-type property.
Scans the buffer between START and END.  Sets the 'invisible
property to '(<nt-type>-<nt-age> <nt-type> <nt-age>)."
  (save-excursion
    ;; reset invisibility settings
    (put-text-property start end 'invisible nil)
    ;; let's go
    (goto-char start)
    (let ((pos1 start)
          (pos2 1)
          (nt-type (get-text-property start 'nt-type))
          (nt-age (get-text-property start 'nt-age)))
      (when nt-type
        (setq pos2 (next-single-property-change (point) 'nt-type))
        (put-text-property (max (point-min) pos1) (1- pos2)
                           'invisible
                           (list (intern
                                  (concat
                                   (symbol-name
                                    (if (eq nt-type 'extra) 'desc nt-type))
                                   "-"
                                   (symbol-name nt-age)))
                                 nt-type
                                 nt-age))
        (setq nt-type (get-text-property pos2 'nt-type))
        (setq pos1 pos2))
      (while (and (setq pos2 (next-single-property-change pos1 'nt-type))
                  (<= pos2 end)
                  (> pos2 pos1))
        ;; must shift one char to the left in order to handle inivisible
        ;; newlines, motion in invisible text areas and all that correctly
        (put-text-property (1- pos1) (1- pos2)
                           'invisible
                           (list (intern
                                  (concat
                                   (symbol-name
                                    (if (eq nt-type 'extra) 'desc nt-type))
                                   "-"
                                   (symbol-name nt-age)))
                                 nt-type
                                 nt-age))
        (setq nt-type (get-text-property pos2 'nt-type))
        (setq nt-age (get-text-property pos2 'nt-age))
        (setq pos1 pos2)))))

(defun newsticker--set-face-properties (pos1 pos2 nt-face age)
  "Set the face for the text between the positions POS1 and POS2.
The face is chosen according the values of NT-FACE and AGE."
  (let ((face (cond ((eq nt-face 'feed)
                     'newsticker-feed-face)
                    ((eq nt-face 'item)
                     (cond ((eq age 'new)
                            'newsticker-new-item-face)
                           ((eq age 'old)
                            'newsticker-old-item-face)
                           ((eq age 'immortal)
                            'newsticker-immortal-item-face)
                           ((eq age 'obsolete)
                            'newsticker-obsolete-item-face)))
                    ((eq nt-face 'date)
                     'newsticker-date-face)
                    ((eq nt-face 'stat)
                     'newsticker-statistics-face)
                    ((eq nt-face 'extra)
                     'newsticker-extra-face)
                    ((eq nt-face 'enclosure)
                     'newsticker-enclosure-face))))
    (when face
      (put-text-property pos1 (max pos1 pos2) 'face face))))

(defun newsticker--insert-image (img string)
  "Insert IMG with STRING at point."
  (insert-image img string))

;; ======================================================================
;;; HTML rendering
;; ======================================================================

;; External.
(declare-function htmlr-reset "ext:htmlr" ())
(declare-function htmlr-step  "ext:htmlr" ())

(defun newsticker-htmlr-render (pos1 pos2) ;
  "Replacement for `htmlr-render'.
Renders the HTML code in the region POS1 to POS2 using htmlr."
  (let ((str (buffer-substring-no-properties pos1 pos2)))
    (delete-region pos1 pos2)
    (insert
     (with-temp-buffer
       (insert str)
       (goto-char (point-min))
       ;; begin original htmlr-render
       (htmlr-reset)
       ;; something omitted here...
       (while (< (point) (point-max))
         (htmlr-step))
       ;; end original htmlr-render
       (newsticker--remove-whitespace (buffer-string))))))

;; ======================================================================
;;; Functions working on the *newsticker* buffer
;; ======================================================================
(defun newsticker--buffer-make-item-completely-visible ()
  "Scroll buffer until current item is completely visible."
  (when newsticker--auto-narrow-to-feed
    (let* ((min (or (save-excursion (newsticker--buffer-beginning-of-feed))
                    (point-min)))
           (max (or (save-excursion (newsticker--buffer-end-of-feed))
                    (point-max))))
      (narrow-to-region min max)))
  (when newsticker--auto-narrow-to-item
    (let* ((min (or (save-excursion (newsticker--buffer-beginning-of-item))
                    (point-min)))
           (max (or (save-excursion (newsticker--buffer-end-of-item))
                    (point-max))))
      (narrow-to-region min max)))
  (sit-for 0)
  ;; do not count lines and stuff because that does not work when images
  ;; are displayed. Do it the simple way:
  (save-excursion
    (newsticker--buffer-end-of-item)
    (unless (pos-visible-in-window-p)
      (recenter -1)))
  (unless (pos-visible-in-window-p)
    (recenter 0)))

(defun newsticker--buffer-get-feed-title-at-point ()
  "Return feed symbol of headline at point."
  (format "%s" (or (get-text-property (point) 'feed) " ")))

(defun newsticker--buffer-get-item-title-at-point ()
  "Return feed symbol of headline at point."
  (format "%s" (or (get-text-property (point) 'nt-title) " ")))

(defun newsticker--buffer-goto (types &optional age backwards)
  "Search next occurrence of TYPES in current buffer.
TYPES is a list of symbols.  If TYPES is found point is moved, if
not point is left unchanged.  If optional parameter AGE is not
nil, the type AND the age must match.  If BACKWARDS is t, search
backwards."
  (let ((pos (save-excursion
	       (save-restriction
		 (widen)
		 (catch 'found
		   (let ((tpos (point)))
		     (while (setq tpos
				  (if backwards
				      (if (eq tpos (point-min))
					  nil
					(or (previous-single-property-change
					     tpos 'nt-type)
					    (point-min)))
				    (next-single-property-change
				     tpos 'nt-type)))
		       (and (memq (get-text-property tpos 'nt-type) types)
			    (or (not age)
				(eq (get-text-property tpos 'nt-age) age))
			    (throw 'found tpos)))))))))
    (when pos
      (goto-char pos))
    pos))

(defun newsticker--buffer-hideshow (mark-age onoff)
  "Hide or show items of type MARK-AGE.
If ONOFF is nil the item is hidden, otherwise it is shown."
  (if onoff
      (remove-from-invisibility-spec mark-age)
    (add-to-invisibility-spec mark-age)))

(defun newsticker--buffer-beginning-of-item ()
  "Move point to the beginning of the item at point.
Return new position."
  (if (bobp)
      (point)
    (let ((type (get-text-property (point) 'nt-type))
          (typebefore (get-text-property (1- (point)) 'nt-type)))
      (if (and (memq type '(item feed))
                   (not (eq type typebefore)))
          (point)
        (newsticker--buffer-goto '(item feed) nil t)
        (point)))))

(defun newsticker--buffer-beginning-of-feed ()
  "Move point to the beginning of the feed at point.
Return new position."
  (if (bobp)
      (point)
    (let ((type (get-text-property (point) 'nt-type))
          (typebefore (get-text-property (1- (point)) 'nt-type)))
      (if (and (memq type '(feed))
                   (not (eq type typebefore)))
          (point)
        (newsticker--buffer-goto '(feed) nil t)
        (point)))))

(defun newsticker--buffer-end-of-item ()
  "Move point to the end of the item at point.
Take care: end of item is at the end of its last line!"
  (when (newsticker--buffer-goto '(item feed nil))
    (point)))

(defun newsticker--buffer-end-of-feed ()
  "Move point to the end of the last item of the feed at point.
Take care: end of item is at the end of its last line!"
  (when (newsticker--buffer-goto '(feed nil))
    (backward-char 1)
    (point)))

;; ======================================================================
;;; manipulation of ticker text
;; ======================================================================
(defun newsticker--ticker-text-setup ()
  "Build the ticker text which is scrolled or flashed in the echo area."
  ;; reset scrollable text
  (setq newsticker--scrollable-text "")
  (setq newsticker--item-list nil)
  (setq newsticker--item-position 0)
  ;; build scrollable text from cache data
  (let ((have-something nil))
    (mapc
     (lambda (feed)
       (let ((feed-name (symbol-name (car feed))))
         (let ((num-new (newsticker--stat-num-items (car feed) 'new))
               (num-old (newsticker--stat-num-items (car feed) 'old))
               (num-imm (newsticker--stat-num-items (car feed) 'immortal))
               (num-obs (newsticker--stat-num-items (car feed) 'obsolete)))
           (when (or (> num-new 0)
                     (and (> num-old 0)
                          (not newsticker-hide-old-items-in-echo-area))
                     (and (> num-imm 0)
                          (not newsticker-hide-immortal-items-in-echo-area))
                     (and (> num-obs 0)
                          (not newsticker-hide-obsolete-items-in-echo-area)))
             (setq have-something t)
             (mapc
              (lambda (item)
                (let ((title (replace-regexp-in-string
                              "[\r\n]+" " "
                              (newsticker--title item)))
                      (age (newsticker--age item)))
                  (unless (string= title newsticker--error-headline)
                    (when
                        (or (eq age 'new)
                            (and (eq age 'old)
                                 (not newsticker-hide-old-items-in-echo-area))
                            (and (eq age 'obsolete)
                                 (not
                                  newsticker-hide-obsolete-items-in-echo-area))
                            (and (eq age 'immortal)
                                 (not
                                  newsticker-hide-immortal-items-in-echo-area)))
                      (setq title (newsticker--remove-whitespace title))
                      ;; add to flash list
                      (add-to-list 'newsticker--item-list
                                   (concat feed-name ": " title) t)
                      ;; and to the scrollable text
                      (setq newsticker--scrollable-text
                            (concat newsticker--scrollable-text
                                    " " feed-name ": " title " +++"))))))
                (cdr feed))))))
     newsticker--cache)
    (when have-something
      (setq newsticker--scrollable-text
            (concat "+++ "
                    (format-time-string "%A, %H:%M"
                                        newsticker--latest-update-time)
                    " ++++++" newsticker--scrollable-text)))))

(defun newsticker--ticker-text-remove (feed title)
  "Remove the item of FEED with TITLE from the ticker text."
  ;; reset scrollable text
  (setq newsticker--item-position 0)
  (let ((feed-name (symbol-name feed))
        (t-title (replace-regexp-in-string "[\r\n]+" " " title)))
    ;; remove from flash list
    (setq newsticker--item-list (remove (concat feed-name ": " t-title)
                                        newsticker--item-list))
    ;; and from the scrollable text
    (setq newsticker--scrollable-text
          (replace-regexp-in-string
           (regexp-quote (concat " " feed-name ": " t-title " +++"))
           ""
           newsticker--scrollable-text))
    (if (string-match (concat "^\\+\\+\\+ [A-Z][a-z]+, "
                              "[012]?[0-9]:[0-9][0-9] \\+\\+\\+\\+\\+\\+$")
                              newsticker--scrollable-text)
        (setq newsticker--scrollable-text ""))))

;; ======================================================================
;;; manipulation of cached data
;; ======================================================================
(defun newsticker--cache-set-preformatted-contents (item contents)
  "Set preformatted contents of ITEM to CONTENTS."
  (if (nthcdr 6 item)
      (setcar (nthcdr 6 item) contents)
    (setcdr (nthcdr 5 item) (list contents))))

(defun newsticker--cache-set-preformatted-title (item title)
  "Set preformatted title of ITEM to TITLE."
  (if (nthcdr 7 item)
      (setcar (nthcdr 7 item) title)
    (setcdr (nthcdr 6 item) title)))

(defun newsticker--cache-replace-age (data feed old-age new-age)
  "Mark all items in DATA in FEED which carry age OLD-AGE with NEW-AGE.
If FEED is 'any it applies to all feeds.  If OLD-AGE is 'any,
all marks are replaced by NEW-AGE.  Removes all pre-formatted contents."
  (mapc (lambda (a-feed)
          (when (or (eq feed 'any)
                    (eq (car a-feed) feed))
            (let ((items (cdr a-feed)))
              (mapc (lambda (item)
                      (when (or (eq old-age 'any)
                                (eq (newsticker--age item) old-age))
                        (setcar (nthcdr 4 item) new-age)
                        (newsticker--do-forget-preformatted item)))
                    items))))
        data)
  data)

(defun newsticker--cache-mark-expired (data feed old-age new-age time)
  "Mark all expired entries.
This function sets the age entries in DATA in the feed FEED.  If
an item's age is OLD-AGE it is set to NEW-AGE if the item is
older than TIME."
  (mapc
   (lambda (a-feed)
     (when (or (eq feed 'any)
               (eq (car a-feed) feed))
       (let ((items (cdr a-feed)))
         (mapc
          (lambda (item)
            (when (eq (newsticker--age item) old-age)
              (let ((exp-time (time-add (newsticker--time item)
                                        (seconds-to-time time))))
                (when (time-less-p exp-time (current-time))
                  (newsticker--debug-msg
                   "Item `%s' from %s has expired on %s"
                   (newsticker--title item)
                   (format-time-string "%Y-%02m-%d, %H:%M"
                                       (newsticker--time item))
                   (format-time-string "%Y-%02m-%d, %H:%M" exp-time))
                  (setcar (nthcdr 4 item) new-age)))))
          items))))
   data)
  data)

(defun newsticker--cache-contains (data feed title desc link age
                                        &optional guid)
  "Check DATA whether FEED contains an item with the given properties.
This function returns the contained item or nil if it is not
contained.
The properties which are checked are TITLE, DESC, LINK, AGE, and
GUID.  In general all properties must match in order to return a
certain item, except for the following cases.

If AGE equals 'feed the TITLE, DESCription and LINK do not
matter.  If DESC is nil it is ignored as well.  If
`newsticker-desc-comp-max' is non-nil, only the first
`newsticker-desc-comp-max' characters of DESC are taken into
account.

If GUID is non-nil it is sufficient to match this value, and the
other properties are ignored."
  (condition-case nil
      (catch 'found
        (when (and desc newsticker-desc-comp-max
                   (> (length desc) newsticker-desc-comp-max))
          (setq desc (substring desc 0 newsticker-desc-comp-max)))
        (mapc
         (lambda (this-feed)
           (when (eq (car this-feed) feed)
             (mapc (lambda (anitem)
                     (when (or
                            ;; global unique id can match
                            (and guid
                                 (string= guid (newsticker--guid anitem)))
                            ;; or title, desc, etc.
                            (and
                             ;;(or (not (eq age 'feed))
			     ;;  (eq (newsticker--age anitem) 'feed))
                             (string= (newsticker--title anitem)
                                      title)
                             (or (not link)
                                 (string= (newsticker--link anitem)
                                          link))
                             (or (not desc)
                                 (if (and desc newsticker-desc-comp-max
                                          (> (length (newsticker--desc anitem))
                                             newsticker-desc-comp-max))
                                     (string= (substring
                                               (newsticker--desc anitem)
                                               0 newsticker-desc-comp-max)
                                              desc)
                                   (string= (newsticker--desc anitem)
                                            desc)))))
                       (throw 'found anitem)))
                   (cdr this-feed))))
         data)
        nil)
    (error nil)))

(defun newsticker--cache-add (data feed-name-symbol title desc link time age
                                   position extra-elements
                                   &optional updated-age updated-time
                                   preformatted-contents
                                   preformatted-title)
  "Add another item to cache data.
Add to DATA in the FEED-NAME-SYMBOL an item with TITLE, DESC,
LINK, TIME, AGE, POSITION, and EXTRA-ELEMENTS.  If this item is
contained already, its mark is set to UPDATED-AGE, its time is
set to UPDATED-TIME, and its pre-formatted contents is set to
PREFORMATTED-CONTENTS and PREFORMATTED-TITLE.  Returns the age
which the item got."
  (let ((item (newsticker--cache-contains data feed-name-symbol title
                                          desc link age)))
    (if item
      ;; does exist already -- change age, update time and position
        (progn
          (if (nthcdr 5 item)
              (setcar (nthcdr 5 item) position)
            (setcdr (nthcdr 4 item) (list position)))
          (setcar (nthcdr 4 item) updated-age)
          (if updated-time
              (setcar (nthcdr 3 item) updated-time))
          ;; replace cached pre-formatted contents
	  (newsticker--cache-set-preformatted-contents
	   item preformatted-contents)
	  (newsticker--cache-set-preformatted-title
	   item preformatted-title))
      ;; did not exist or age equals 'feed-name-symbol
      (catch 'found
        (mapc (lambda (this-feed)
                (when (eq (car this-feed) feed-name-symbol)
                  (setcdr this-feed (nconc (cdr this-feed)
                                           (list (list title desc link
                                                       time age position
                                                       preformatted-contents
                                                       preformatted-title
                                                       extra-elements))))
                  (throw 'found this-feed)))
              data)
        ;; the feed is not contained
        (add-to-list 'data (list feed-name-symbol
                                 (list title desc link time age position
                                       preformatted-contents
                                       preformatted-title
                                       extra-elements))
                     t))))
  data)

(defun newsticker--cache-remove (data feed-symbol age)
  "Remove all entries from DATA in the feed FEED-SYMBOL with AGE.
FEED-SYMBOL may be 'any.  Entries from old feeds, which are no longer in
`newsticker-url-list' or `newsticker-url-list-defaults', are removed as
well."
  (let* ((pos data)
         (feed (car pos))
         (last-pos nil))
    (while feed
      (if (or (assoc (symbol-name (car feed)) newsticker-url-list)
              (assoc (symbol-name (car feed)) newsticker-url-list-defaults))
          ;; feed is still valid=active
          ;; (message "Keeping feed %s" (car feed))
          (if  (or (eq feed-symbol 'any)
                   (eq feed-symbol (car feed)))
              (let* ((item-pos (cdr feed))
                     (item (car item-pos))
                     (prev-pos nil))
                (while item
                  ;;(message "%s" (car item))
                  (if (eq age (newsticker--age item))
                      ;; remove this item
                      (progn
                        ;;(message "Removing item %s" (car item))
                        (if prev-pos
                            (setcdr prev-pos (cdr item-pos))
                          (setcdr feed (cdr item-pos))))
                    ;;(message "Keeping item %s" (car item))
                    (setq prev-pos item-pos))
                  (setq item-pos (cdr item-pos))
                  (setq item (car item-pos)))))
        ;; feed is not active anymore
        ;; (message "Removing feed %s" (car feed))
        (if last-pos
            (setcdr last-pos (cdr pos))
          (setq data (cdr pos))))
      (setq last-pos pos)
      (setq pos (cdr pos))
      (setq feed (car pos)))))

;; ======================================================================
;;; Sorting
;; ======================================================================
(defun newsticker--cache-item-compare-by-time (item1 item2)
  "Compare two news items ITEM1 and ITEM2 by comparing their time values."
  (catch 'result
    (let ((age1 (newsticker--age item1))
          (age2 (newsticker--age item2)))
      (if (not (eq age1 age2))
          (cond ((eq age1 'obsolete)
                 (throw 'result nil))
                ((eq age2 'obsolete)
                 (throw 'result t)))))
    (let* ((time1 (newsticker--time item1))
           (time2 (newsticker--time item2)))
      (cond ((< (nth 0 time1) (nth 0 time2))
             nil)
            ((> (nth 0 time1) (nth 0 time2))
             t)
            ((< (nth 1 time1) (nth 1 time2))
             nil)
            ((> (nth 1 time1) (nth 1 time2))
             t)
            ((< (or (nth 2 time1) 0) (or (nth 2 time2) 0))
             nil)
            ((> (or (nth 2 time1) 0) (or (nth 2 time2) 0))
             t)
            (t
             nil)))))

(defun newsticker--cache-item-compare-by-title (item1 item2)
  "Compare ITEM1 and ITEM2 by comparing their titles."
  (catch 'result
    (let ((age1 (newsticker--age item1))
          (age2 (newsticker--age item2)))
      (if (not (eq age1 age2))
          (cond ((eq age1 'obsolete)
                 (throw 'result nil))
                ((eq age2 'obsolete)
                 (throw 'result t)))))
    (string< (newsticker--title item1) (newsticker--title item2))))

(defun newsticker--cache-item-compare-by-position (item1 item2)
  "Compare ITEM1 and ITEM2 by comparing their original positions."
  (catch 'result
    (let ((age1 (newsticker--age item1))
          (age2 (newsticker--age item2)))
      (if (not (eq age1 age2))
          (cond ((eq age1 'obsolete)
                 (throw 'result nil))
                ((eq age2 'obsolete)
                 (throw 'result t)))))
    (< (or (newsticker--pos item1) 0) (or (newsticker--pos item2) 0))))

(defun newsticker--cache-sort ()
  "Sort the newsticker cache data."
  (let ((sort-fun (cond ((eq newsticker-sort-method 'sort-by-time)
                         'newsticker--cache-item-compare-by-time)
                        ((eq newsticker-sort-method 'sort-by-title)
                         'newsticker--cache-item-compare-by-title)
                        ((eq newsticker-sort-method 'sort-by-original-order)
                         'newsticker--cache-item-compare-by-position))))
    (mapc (lambda (feed-list)
            (setcdr feed-list (sort (cdr feed-list)
                                    sort-fun)))
          newsticker--cache)))

(defun newsticker--cache-update (&optional save)
  "Update newsticker cache file.
If optional argument SAVE is not nil the cache file is saved to disk."
  (save-excursion
    (let ((coding-system-for-write 'utf-8)
          (buf (find-file-noselect newsticker-cache-filename)))
      (when buf
        (set-buffer buf)
        (setq buffer-undo-list t)
        (erase-buffer)
        (insert ";; -*- coding: utf-8 -*-\n")
        (insert (prin1-to-string newsticker--cache))
        (when save
          (save-buffer))))))

(defun newsticker--cache-get-feed (feed)
  "Return the cached data for the feed FEED.
FEED is a symbol!"
  (assoc feed newsticker--cache))

;; ======================================================================
;;; Statistics
;; ======================================================================
(defun newsticker--stat-num-items (feed &optional age)
  "Return number of items in the given FEED which have the given AGE.
If AGE is nil, the total number of items is returned."
  (let ((items (cdr (newsticker--cache-get-feed feed)))
        (num 0))
    (while items
      (if age
          (if (eq (newsticker--age (car items)) age)
              (setq num (1+ num)))
        (if (memq (newsticker--age (car items)) '(new old immortal obsolete))
              (setq num (1+ num))))
      (setq items (cdr items)))
    num))

;; ======================================================================
;;; OPML
;; ======================================================================
(defun newsticker-opml-export ()
  "OPML subscription export.
Export subscriptions to a buffer in OPML Format."
  (interactive)
  (with-current-buffer (get-buffer-create "*OPML Export*")
    (set-buffer-file-coding-system 'utf-8)
    (insert (concat
             "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
             "<!-- OPML generated by Emacs newsticker.el -->\n"
             "<opml version=\"1.0\">\n"
             "  <head>\n"
             "    <title>mySubscriptions</title>\n"
             "    <dateCreated>" (format-time-string "%a, %d %b %Y %T %z")
             "</dateCreated>\n"
             "    <ownerEmail>" user-mail-address "</ownerEmail>\n"
             "    <ownerName>" (user-full-name) "</ownerName>\n"
             "  </head>\n"
             "  <body>\n"))
    (mapc (lambda (sub)
            (insert "    <outline text=\"")
            (insert (newsticker--title sub))
            (insert "\" xmlUrl=\"")
            (insert (cadr sub))
            (insert "\"/>\n"))
          (append newsticker-url-list newsticker-url-list-defaults))
    (insert "  </body>\n</opml>\n"))
  (pop-to-buffer "*OPML Export*")
  (when (fboundp 'sgml-mode)
    (sgml-mode)))

(defun newsticker-opml-import (filename)
  "Import OPML data from FILENAME."
  (interactive "fOPML file: ")
  (set-buffer (find-file-noselect filename))
  (goto-char (point-min))
  (let* ((node-list (xml-parse-region (point-min) (point-max)))
         (body (car (xml-get-children (car node-list) 'body)))
         (outlines (xml-get-children body 'outline)))
    (mapc (lambda (outline)
            (let ((name (xml-get-attribute outline 'text))
                  (url (xml-get-attribute outline 'xmlUrl)))
              (add-to-list 'newsticker-url-list
                           (list name url nil nil nil) t)))
          outlines))
  (customize-variable 'newsticker-url-list))

;; ======================================================================
;;; Auto marking
;; ======================================================================
(defun newsticker--run-auto-mark-filter (feed item)
  "Automatically mark an item as old or immortal.
This function checks the variable `newsticker-auto-mark-filter-list'
for an entry that matches FEED and ITEM."
  (let ((case-fold-search t))
    (mapc (lambda (filter)
            (let ((filter-feed (car filter))
                  (pattern-list (cadr filter)))
            (when (string-match filter-feed feed)
              (newsticker--do-run-auto-mark-filter item pattern-list))))
          newsticker-auto-mark-filter-list)))

(defun newsticker--do-run-auto-mark-filter (item list)
  "Actually compare ITEM against the pattern-LIST
\(from `newsticker-auto-mark-filter-list')."
  (mapc (lambda (pattern)
          (let ((age    (nth 0 pattern))
                (place  (nth 1 pattern))
                (regexp (nth 2 pattern))
                (title (newsticker--title item))
                (desc  (newsticker--desc item)))
            (when (or (eq place 'title) (eq place 'all))
              (when (and title (string-match regexp title))
                (newsticker--debug-msg "Auto-marking as %s: `%s'"
                                       age (newsticker--title item))
                (setcar (nthcdr 4 item) age)))
            (when (or (eq place 'description) (eq place 'all))
              (when (and desc (string-match regexp desc))
                (newsticker--debug-msg "Auto-marking as %s: `%s'"
                                       age (newsticker--title item))
                (setcar (nthcdr 4 item) age)))))
        list))


;; ======================================================================
;;; hook samples
;; ======================================================================
(defun newsticker-new-item-functions-sample (feed item)
  "Demonstrate the use of the `newsticker-new-item-functions' hook.
This function just prints out the values of the FEED and title of the ITEM."
  (message (concat "newsticker-new-item-functions-sample: feed=`%s', "
                   "title=`%s'")
           feed (newsticker--title item)))

(defun newsticker-download-images (feed item)
  "Download the first image.
If FEED equals \"imagefeed\" download the first image URL found
in the description=contents of ITEM to the directory
\"~/tmp/newsticker/FEED/TITLE\" where TITLE is the title of the item."
  (when (string= feed "imagefeed")
    (let ((title (newsticker--title item))
          (desc (newsticker--desc item)))
      (when (string-match "<img src=\"\\(http://[^ \"]+\\)\"" desc)
        (let ((url (substring desc (match-beginning 1) (match-end 1)))
              (temp-dir (concat "~/tmp/newsticker/" feed "/" title))
              (org-dir default-directory))
          (unless (file-directory-p temp-dir)
            (make-directory temp-dir t))
          (cd temp-dir)
          (message "Getting image %s" url)
          (apply 'start-process "wget-image"
                 " *newsticker-wget-download-images*"
                 newsticker-wget-name
                 (list url))
          (cd org-dir))))))

(defun newsticker-download-enclosures (feed item)
  "In all FEEDs download the enclosed object of the news ITEM.
The object is saved to the directory \"~/tmp/newsticker/FEED/TITLE\", which
is created if it does not exist.  TITLE is the title of the news
item.  Argument FEED is ignored.
This function is suited for adding it to `newsticker-new-item-functions'."
  (let ((title (newsticker--title item))
        (enclosure (newsticker--enclosure item)))
    (when enclosure
      (let ((url (cdr (assoc 'url enclosure)))
            (temp-dir (concat "~/tmp/newsticker/" feed "/" title))
            (org-dir default-directory))
        (unless (file-directory-p temp-dir)
          (make-directory temp-dir t))
        (cd temp-dir)
        (message "Getting enclosure %s" url)
        (apply 'start-process "wget-enclosure"
               " *newsticker-wget-download-enclosures*"
               newsticker-wget-name
               (list url))
        (cd org-dir)))))


(provide 'newsticker)

;; arch-tag: ab761dfa-67bc-4207-bc64-4307271dc381
;;; newsticker.el ends here
