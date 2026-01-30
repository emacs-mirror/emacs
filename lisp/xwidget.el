;;; xwidget.el --- api functions for xwidgets  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2026 Free Software Foundation, Inc.

;; Author: Joakim Verona <joakim@verona.se>

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

;; See the node "(emacs)Embedded WebKit Widgets" in the Emacs manual for
;; help on user-facing features, and "(elisp)Embedded Native Widgets" in
;; the Emacs Lisp reference manual for help on more API functions.

;;; Code:

;; This breaks compilation when we don't have xwidgets.
;; And is pointless when we do, since it's in C and so preloaded.
;;(require 'xwidget-internal)

(require 'bookmark)
(require 'format-spec)

(declare-function make-xwidget "xwidget.c"
                  (type title width height &optional arguments buffer related))
(declare-function xwidget-buffer "xwidget.c" (xwidget))
(declare-function set-xwidget-buffer "xwidget.c" (xwidget buffer))
(declare-function xwidget-size-request "xwidget.c" (xwidget))
(declare-function xwidget-resize "xwidget.c" (xwidget new-width new-height))
(declare-function xwidget-webkit-execute-script "xwidget.c"
                  (xwidget script &optional callback))
(declare-function xwidget-webkit-uri "xwidget.c" (xwidget))
(declare-function xwidget-webkit-title "xwidget.c" (xwidget))
(declare-function xwidget-webkit-goto-uri "xwidget.c" (xwidget uri))
(declare-function xwidget-webkit-goto-history "xwidget.c" (xwidget rel-pos))
(declare-function xwidget-webkit-zoom "xwidget.c" (xwidget factor))
(declare-function xwidget-plist "xwidget.c" (xwidget))
(declare-function set-xwidget-plist "xwidget.c" (xwidget plist))
(declare-function xwidget-view-window "xwidget.c" (xwidget-view))
(declare-function xwidget-view-model "xwidget.c" (xwidget-view))
(declare-function delete-xwidget-view "xwidget.c" (xwidget-view))
(declare-function get-buffer-xwidgets "xwidget.c" (buffer))
(declare-function xwidget-query-on-exit-flag "xwidget.c" (xwidget))
(declare-function xwidget-webkit-back-forward-list "xwidget.c" (xwidget &optional limit))
(declare-function xwidget-webkit-estimated-load-progress "xwidget.c" (xwidget))
(declare-function xwidget-webkit-set-cookie-storage-file "xwidget.c" (xwidget file))
(declare-function xwidget-live-p "xwidget.c" (xwidget))
(declare-function xwidget-webkit-stop-loading "xwidget.c" (xwidget))
(declare-function xwidget-info "xwidget.c" (xwidget))

(defgroup xwidget nil
  "Displaying native widgets in Emacs buffers."
  :group 'widgets)

(defun xwidget-insert (pos type title width height &optional args related)
  "Insert an xwidget at position POS.
Supply the xwidget's TYPE, TITLE, WIDTH, HEIGHT, and RELATED.
See `make-xwidget' for the possible TYPE values.
The usage of optional argument ARGS depends on the xwidget.
This returns the result of `make-xwidget'."
  (goto-char pos)
  (let ((id (make-xwidget type title width height args nil related)))
    (put-text-property (point) (+ 1 (point))
                       'display (list 'xwidget ':xwidget id))
    id))

(defun xwidget-at (pos)
  "Return xwidget at POS."
  (let* ((disp (get-text-property pos 'display))
         (xw (ignore-errors (car (cdr (cdr disp))))))
    (when (xwidget-live-p xw) xw)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; webkit support
(require 'browse-url)
(require 'image-mode);;for some image-mode alike functionality
(require 'seq)
(require 'url-handlers)

(defgroup xwidget-webkit nil
  "Displaying webkit xwidgets in Emacs buffers."
  :version "29.1"
  :group 'web
  :prefix "xwidget-webkit-")

(defcustom xwidget-webkit-buffer-name-format "*xwidget-webkit: %T*"
  "Template for naming `xwidget-webkit' buffers.
It can use the following special constructs:

  %T -- the title of the Web page loaded by the xwidget.
  %U -- the URI of the Web page loaded by the xwidget."
  :type 'string
  :version "29.1")

(defcustom xwidget-webkit-cookie-file nil
  "The name of the file where `xwidget-webkit-browse-url' will store cookies.
They will be stored as plain text in Mozilla \"cookies.txt\"
format.  If nil, do not store cookies.  You must kill all xwidget-webkit
buffers for this setting to take effect after setting it to nil."
  :type '(choice (const :tag "Do not store cookies" nil) file)
  :version "29.1")

;;;###autoload
(defun xwidget-webkit-browse-url (url &optional new-session)
  "Ask xwidget-webkit to browse URL.
NEW-SESSION specifies whether to create a new xwidget-webkit session.
Interactively, URL defaults to the string looking like a url around point."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: ")))
  (or (featurep 'xwidget-internal)
      (user-error "Your Emacs was not compiled with xwidgets support"))
  (when (stringp url)
    ;; If it's a "naked url", just try adding https: to it.
    (unless (string-match "\\`[A-Za-z]+:" url)
      (setq url (concat "https://" url)))
    (if new-session
        (xwidget-webkit-new-session url)
      (xwidget-webkit-goto-url url))))

(function-put 'xwidget-webkit-browse-url 'browse-url-browser-kind 'internal)

(defun xwidget-webkit-clone-and-split-below ()
  "Clone current URL into a new widget place in new window below.
Get the URL of current session, then browse to the URL
in `split-window-below' with a new xwidget webkit session."
  (interactive nil xwidget-webkit-mode)
  (let ((url (xwidget-webkit-uri (xwidget-webkit-current-session))))
    (with-selected-window (split-window-below)
      (xwidget-webkit-new-session url))))

(defun xwidget-webkit-clone-and-split-right ()
  "Clone current URL into a new widget place in new window right.
Get the URL of current session, then browse to the URL
in `split-window-right' with a new xwidget webkit session."
  (interactive nil xwidget-webkit-mode)
  (let ((url (xwidget-webkit-uri (xwidget-webkit-current-session))))
    (with-selected-window (split-window-right)
      (xwidget-webkit-new-session url))))

(declare-function xwidget-perform-lispy-event "xwidget.c")

(defvar xwidget-webkit--input-method-events nil
  "Internal variable used to store input method events.")

(defvar-local xwidget-webkit--loading-p nil
  "Whether or not a page is being loaded.")

(defvar-local xwidget-webkit--progress-update-timer nil
  "Timer that updates the display of page load progress in the header line.")

(defun xwidget-webkit-pass-command-event-with-input-method ()
  "Handle a `with-input-method' event."
  (interactive)
  (let ((key (pop unread-command-events)))
    (setq xwidget-webkit--input-method-events
          (funcall input-method-function key))
    (exit-minibuffer)))

(defun xwidget-webkit-pass-command-event ()
  "Pass `last-command-event' to the current buffer's WebKit widget.
If `current-input-method' is non-nil, consult `input-method-function'
for the actual events that will be sent."
  (interactive)
  (if (and current-input-method
           (characterp last-command-event))
      (let ((xwidget-webkit--input-method-events nil)
            (minibuffer-local-map (make-keymap)))
        (define-key minibuffer-local-map [with-input-method]
          'xwidget-webkit-pass-command-event-with-input-method)
        (push last-command-event unread-command-events)
        (push 'with-input-method unread-command-events)
        (read-from-minibuffer "" nil nil nil nil nil t)
        (dolist (event xwidget-webkit--input-method-events)
          (xwidget-perform-lispy-event (xwidget-webkit-current-session)
                                       event)))
    (xwidget-perform-lispy-event (xwidget-webkit-current-session)
                                 last-command-event)))

;;todo.
;; - check that the webkit support is compiled in
(defvar xwidget-webkit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'xwidget-webkit-browse-url)
    (define-key map "a" 'xwidget-webkit-adjust-size-dispatch)
    (define-key map "b" 'xwidget-webkit-back)
    (define-key map "f" 'xwidget-webkit-forward)
    (define-key map "r" 'xwidget-webkit-reload)
    (define-key map "\C-m" 'xwidget-webkit-insert-string)
    (define-key map "w" 'xwidget-webkit-current-url)
    (define-key map "+" 'xwidget-webkit-zoom-in)
    (define-key map "-" 'xwidget-webkit-zoom-out)
    (define-key map "e" 'xwidget-webkit-edit-mode)
    (define-key map "\C-r" 'xwidget-webkit-isearch-mode)
    (define-key map "\C-s" 'xwidget-webkit-isearch-mode)
    (define-key map "H" 'xwidget-webkit-browse-history)

    ;;similar to image mode bindings
    (define-key map (kbd "SPC")                 'xwidget-webkit-scroll-up)
    (define-key map (kbd "S-SPC")               'xwidget-webkit-scroll-down)
    (define-key map (kbd "DEL")                 'xwidget-webkit-scroll-down)

    (define-key map [remap scroll-up]           'xwidget-webkit-scroll-up-line)
    (define-key map [remap scroll-up-command]   'xwidget-webkit-scroll-up)

    (define-key map [remap scroll-down]         'xwidget-webkit-scroll-down-line)
    (define-key map [remap scroll-down-command] 'xwidget-webkit-scroll-down)

    (define-key map [remap forward-char]        'xwidget-webkit-scroll-forward)
    (define-key map [remap backward-char]       'xwidget-webkit-scroll-backward)
    (define-key map [remap right-char]          'xwidget-webkit-scroll-forward)
    (define-key map [remap left-char]           'xwidget-webkit-scroll-backward)
    (define-key map [remap previous-line]       'xwidget-webkit-scroll-down-line)
    (define-key map [remap next-line]           'xwidget-webkit-scroll-up-line)

    ;; (define-key map [remap move-beginning-of-line] 'image-bol)
    ;; (define-key map [remap move-end-of-line]       'image-eol)
    (define-key map [remap beginning-of-buffer] 'xwidget-webkit-scroll-top)
    (define-key map [remap end-of-buffer]       'xwidget-webkit-scroll-bottom)
    map)
  "Keymap for `xwidget-webkit-mode'.")

(easy-menu-define nil xwidget-webkit-mode-map "Xwidget WebKit menu."
  (list "Xwidget WebKit"
        ["Browse URL" xwidget-webkit-browse-url
         :active t
         :help "Prompt for a URL, then instruct WebKit to browse it"]
        ["Back" xwidget-webkit-back t]
        ["Forward" xwidget-webkit-forward t]
        ["Reload" xwidget-webkit-reload t]
        ["History" xwidget-webkit-browse-history t]
        ["Insert String" xwidget-webkit-insert-string
         :active t
         :help "Insert a string into the currently active field"]
        ["Zoom In" xwidget-webkit-zoom-in t]
        ["Zoom Out" xwidget-webkit-zoom-out t]
        ["Edit Mode" xwidget-webkit-edit-mode
         :active t
         :style toggle
         :selected xwidget-webkit-edit-mode
         :help "Send self inserting characters to the WebKit widget"]
        ["Save Selection" xwidget-webkit-copy-selection-as-kill
         :active t
         :help "Save the browser's selection in the kill ring"]
        ["Incremental Search" xwidget-webkit-isearch-mode
         :active (not xwidget-webkit-isearch-mode)
         :help "Perform incremental search inside the WebKit widget"]
        ["Stop Loading" xwidget-webkit-stop
         :active xwidget-webkit--loading-p]))

(defvar xwidget-webkit-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (tool-bar-local-item-from-menu 'xwidget-webkit-stop
                                     "cancel"
                                     map
                                     xwidget-webkit-mode-map)
      (tool-bar-local-item-from-menu 'xwidget-webkit-back
                                     "left-arrow"
                                     map
                                     xwidget-webkit-mode-map)
      (tool-bar-local-item-from-menu 'xwidget-webkit-forward
                                     "right-arrow"
                                     map
                                     xwidget-webkit-mode-map)
      (tool-bar-local-item-from-menu 'xwidget-webkit-reload
                                     "refresh"
                                     map
                                     xwidget-webkit-mode-map)
      (tool-bar-local-item-from-menu 'xwidget-webkit-zoom-in
                                     "zoom-in"
                                     map
                                     xwidget-webkit-mode-map)
      (tool-bar-local-item-from-menu 'xwidget-webkit-zoom-out
                                     "zoom-out"
                                     map
                                     xwidget-webkit-mode-map)
      (tool-bar-local-item-from-menu 'xwidget-webkit-browse-url
                                     "connect-to-url"
                                     map
                                     xwidget-webkit-mode-map)
      (tool-bar-local-item-from-menu 'xwidget-webkit-isearch-mode
                                     "search"
                                     map
                                     xwidget-webkit-mode-map))))

(defun xwidget-webkit-zoom-in ()
  "Increase webkit view zoom factor."
  (interactive nil xwidget-webkit-mode)
  (xwidget-webkit-zoom (xwidget-webkit-current-session) 0.1))

(defun xwidget-webkit-zoom-out ()
  "Decrease webkit view zoom factor."
  (interactive nil xwidget-webkit-mode)
  (xwidget-webkit-zoom (xwidget-webkit-current-session) -0.1))

(defun xwidget-webkit-scroll-up (&optional arg)
  "Scroll webkit up by ARG pixels; or full window height if no ARG.
Stop if bottom of page is reached.
Interactively, ARG is the prefix numeric argument.
Negative ARG scrolls down."
  (interactive "P" xwidget-webkit-mode)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   (format "window.scrollBy(0, %d);"
           (or arg (xwidget-window-inside-pixel-height (selected-window))))))

(defun xwidget-webkit-scroll-down (&optional arg)
  "Scroll webkit down by ARG pixels; or full window height if no ARG.
Stop if top of page is reached.
Interactively, ARG is the prefix numeric argument.
Negative ARG scrolls up."
  (interactive "P" xwidget-webkit-mode)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   (format "window.scrollBy(0, -%d);"
           (or arg (xwidget-window-inside-pixel-height (selected-window))))))

(defun xwidget-webkit-scroll-up-line (&optional n)
  "Scroll webkit up by N lines.
The height of line is calculated with `window-font-height'.
Stop if the bottom edge of the page is reached.
If N is omitted or nil, scroll up by one line."
  (interactive "p" xwidget-webkit-mode)
  (xwidget-webkit-scroll-up (* n (window-font-height))))

(defun xwidget-webkit-scroll-down-line (&optional n)
  "Scroll webkit down by N lines.
The height of line is calculated with `window-font-height'.
Stop if the top edge of the page is reached.
If N is omitted or nil, scroll down by one line."
  (interactive "p" xwidget-webkit-mode)
  (xwidget-webkit-scroll-down (* n (window-font-height))))

(defun xwidget-webkit-scroll-forward (&optional n)
  "Scroll webkit horizontally by N chars.
If the widget is larger than the window, hscroll by N columns
instead.  The width of char is calculated with
`window-font-width'.  If N is omitted or nil, scroll forwards by
one char."
  (interactive "p" xwidget-webkit-mode)
  (let ((session (xwidget-webkit-current-session)))
    (if (> (- (aref (xwidget-info session) 2)
              (window-text-width nil t))
           (window-font-width))
        (set-window-hscroll nil (+ (window-hscroll) n))
      (xwidget-webkit-execute-script session
                                     (format "window.scrollBy(%d, 0);"
                                             (* n (window-font-width)))))))

(defun xwidget-webkit-scroll-backward (&optional n)
  "Scroll webkit back by N chars.
If the widget is larger than the window, hscroll backwards by N
columns instead.  The width of char is calculated with
`window-font-width'.  If N is omitted or nil, scroll backwards by
one char."
  (interactive "p" xwidget-webkit-mode)
  (let ((session (xwidget-webkit-current-session)))
    (if (and (> (- (aref (xwidget-info session) 2)
                   (window-text-width nil t))
                (window-font-width))
             (> (window-hscroll) 0))
        (set-window-hscroll nil (- (window-hscroll) n))
      (xwidget-webkit-execute-script session
                                     (format "window.scrollBy(-%d, 0);"
                                             (* n (window-font-width)))))))

(defun xwidget-webkit-scroll-top ()
  "Scroll webkit to the very top."
  (interactive nil xwidget-webkit-mode)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   "window.scrollTo(pageXOffset, 0);"))

(defun xwidget-webkit-scroll-bottom ()
  "Scroll webkit to the very bottom."
  (interactive nil xwidget-webkit-mode)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   "window.scrollTo(pageXOffset, window.document.body.scrollHeight);"))

;; The xwidget event needs to go in the special map.  To receive
;; xwidget events, you should place a callback in the property list of
;; the xwidget, instead of handling these events manually.
;;
;; See `xwidget-webkit-new-session' for an example of how to do this.
(define-key special-event-map [xwidget-event] #'xwidget-event-handler)

(defun xwidget-log (&rest msg)
  "Log MSG to a buffer."
  (let ((buf (get-buffer-create " *xwidget-log*")))
    (with-current-buffer buf
      (insert (apply #'format msg))
      (insert "\n"))))

(defun xwidget-event-handler ()
  "Receive xwidget event."
  (interactive nil xwidget-webkit-mode)
  (xwidget-log "stuff happened to xwidget %S" last-input-event)
  (let*
      ((xwidget-event-type (nth 1 last-input-event))
       (xwidget (nth 2 last-input-event))
       (xwidget-callback (xwidget-get xwidget 'callback)))
    (when xwidget-callback
      (funcall xwidget-callback xwidget xwidget-event-type))))

(defun xwidget-webkit--update-progress-timer-function (xwidget)
  "Force an update of the header line of XWIDGET's buffer."
  (with-current-buffer (xwidget-buffer xwidget)
    (force-mode-line-update)))

(defun xwidget-webkit-buffer-kill ()
  "Clean up an xwidget-webkit buffer before it is killed."
  (when (timerp xwidget-webkit--progress-update-timer)
    (cancel-timer xwidget-webkit--progress-update-timer)))

(defun xwidget-webkit-callback (xwidget xwidget-event-type)
  "Callback for xwidgets.
XWIDGET instance, XWIDGET-EVENT-TYPE depends on the originating xwidget."
  (if (not (buffer-live-p (xwidget-buffer xwidget)))
      (xwidget-log
       "error: callback called for xwidget with dead buffer")
    (cond ((eq xwidget-event-type 'load-changed)
           (let ((title (xwidget-webkit-title xwidget))
                 (uri (xwidget-webkit-uri xwidget)))
             (when-let* ((buffer (get-buffer "*Xwidget WebKit History*")))
               (with-current-buffer buffer
                 (revert-buffer)))
             (with-current-buffer (xwidget-buffer xwidget)
               (if (string-equal (nth 3 last-input-event)
                                 "load-finished")
                   (progn
                     (setq xwidget-webkit--loading-p nil)
                     (cancel-timer xwidget-webkit--progress-update-timer))
                 (unless xwidget-webkit--loading-p
                   (setq xwidget-webkit--loading-p t
                         xwidget-webkit--progress-update-timer
                         (run-at-time 0.5 0.5 #'xwidget-webkit--update-progress-timer-function
                                      xwidget)))))
             ;; This function will be called multi times, so only
             ;; change buffer name when the load actually completes
             ;; this can limit buffer-name flicker in mode-line.
             (when (or (string-equal (nth 3 last-input-event)
                                     "load-finished")
                       (> (length title) 0))
               (with-current-buffer (xwidget-buffer xwidget)
                 (force-mode-line-update)
                 (xwidget-log "webkit finished loading: %s" title)
                 ;; Do not adjust webkit size to window here, the
                 ;; selected window can be the mini-buffer window
                 ;; unwantedly.
                 (rename-buffer
                  (format-spec
                   xwidget-webkit-buffer-name-format
                   `((?T . ,title)
                     (?U . ,uri)))
                  t)))))
          ((eq xwidget-event-type 'decide-policy)
           (let ((strarg  (nth 3 last-input-event)))
             (if (string-match ".*#\\(.*\\)" strarg)
                 (xwidget-webkit-show-id-or-named-element
                  xwidget
                  (match-string 1 strarg)))))
          ;; TODO: Response handling other than download.
          ((eq xwidget-event-type 'download-callback)
           (let ((url  (nth 3 last-input-event))
                 (mime-type (nth 4 last-input-event))
                 (file-name (nth 5 last-input-event)))
             (xwidget-webkit-save-as-file url mime-type file-name)))
          ((eq xwidget-event-type 'javascript-callback)
           (let ((proc (nth 3 last-input-event))
                 (arg  (nth 4 last-input-event)))
             (funcall proc arg)))
          (t (xwidget-log "unhandled event:%s" xwidget-event-type)))))

(defvar bookmark-make-record-function)
(when (memq window-system '(mac ns))
  (defcustom xwidget-webkit-enable-plugins nil
    "Enable plugins for xwidget webkit.
If non-nil, plugins are enabled.  Otherwise, disabled."
    :type 'boolean
    :version "28.1"))

(define-derived-mode xwidget-webkit-mode special-mode "xwidget-webkit"
  "Xwidget webkit view mode."
  (setq buffer-read-only t)
  (add-hook 'kill-buffer-hook #'xwidget-webkit-buffer-kill)
  (setq-local tool-bar-map xwidget-webkit-tool-bar-map)
  (setq-local bookmark-make-record-function
              #'xwidget-webkit-bookmark-make-record)
  (setq-local header-line-format
              (list "WebKit: "
                    '(:eval
                      (xwidget-webkit-title (xwidget-webkit-current-session)))
                    '(:eval
                      (when xwidget-webkit--loading-p
                        (let ((session (xwidget-webkit-current-session)))
                          (format " [%d%%%%]"
                                  (* 100
                                     (xwidget-webkit-estimated-load-progress
                                      session))))))))
  ;; Keep track of [vh]scroll when switching buffers
  (image-mode-setup-winprops))

;;; Download, save as file.

(defcustom xwidget-webkit-download-dir "~/Downloads/"
  "Directory where download file saved."
  :version "28.1"
  :type 'file)

(defun xwidget-webkit-save-as-file (url mime-type file-name)
  "For XWIDGET webkit, save URL of MIME-TYPE to location specified by user.
FILE-NAME combined with `xwidget-webkit-download-dir' is the default file name
of the prompt when reading.  When the file name the user specified is a
directory, URL is saved at the specified directory as FILE-NAME."
  (let ((save-name (read-file-name
                    (format "Save URL `%s' of type `%s' in file/directory: "
                            url mime-type)
                    xwidget-webkit-download-dir
                    (when file-name
                      (expand-file-name
                       file-name
                       xwidget-webkit-download-dir)))))
    (if (file-directory-p save-name)
        (setq save-name
              (expand-file-name (file-name-nondirectory file-name) save-name)))
    (setq xwidget-webkit-download-dir (file-name-directory save-name))
    (url-copy-file url save-name t)))

;;; Bookmarks integration

(defcustom xwidget-webkit-bookmark-jump-new-session nil
  "Whether to jump to a bookmarked URL in a new xwidget webkit session.
If non-nil, create a new xwidget webkit session, otherwise use
the value of `xwidget-webkit-last-session'."
  :version "28.1"
  :type 'boolean)

(defun xwidget-webkit-bookmark-make-record ()
  "Create a bookmark record for a webkit xwidget."
  (nconc (bookmark-make-record-default t t)
         `((page . ,(xwidget-webkit-uri (xwidget-webkit-current-session)))
           (handler . xwidget-webkit-bookmark-jump-handler))))

;;;###autoload
(defun xwidget-webkit-bookmark-jump-handler (bookmark)
  "Jump to the web page bookmarked by the bookmark record BOOKMARK.
If `xwidget-webkit-bookmark-jump-new-session' is non-nil, create
a new xwidget-webkit session, otherwise use an existing session."
  (let* ((url (bookmark-prop-get bookmark 'page))
	 (xwbuf (if (or xwidget-webkit-bookmark-jump-new-session
                        (not (xwidget-webkit-current-session)))
	            (xwidget-webkit--create-new-session-buffer url)
                  (xwidget-buffer (xwidget-webkit-current-session)))))
    (with-current-buffer xwbuf
      (xwidget-webkit-goto-uri (xwidget-webkit-current-session) url))
    (set-buffer xwbuf)))

;;; xwidget webkit session

(defvar xwidget-webkit-last-session-buffer nil)

(defun xwidget-webkit-last-session ()
  "Last active webkit, or nil."
  (if (buffer-live-p xwidget-webkit-last-session-buffer)
      (with-current-buffer xwidget-webkit-last-session-buffer
        (xwidget-at (point-min)))
    nil))

(defun xwidget-webkit-current-session ()
  "Either the webkit in the current buffer, or the last one used.
The latter might be nil."
  (or (xwidget-at (point-min)) (xwidget-webkit-last-session)))

(defun xwidget-adjust-size-to-content (xw)
  "Resize XW to content."
  ;; xwidgets doesn't support widgets that have their own opinions about
  ;; size well, yet this reads the desired size and resizes the Emacs
  ;; allocated area accordingly.
  (let ((size (xwidget-size-request xw)))
    (xwidget-resize xw (car size) (cadr size))))

(defun xwidget-webkit-stop ()
  "Stop trying to load the current page."
  (interactive)
  (xwidget-webkit-stop-loading (xwidget-webkit-current-session)))

(defvar xwidget-webkit-activeelement-js"
function findactiveelement(doc){
//alert(doc.activeElement.value);
   if(doc.activeElement.value != undefined){
      return doc.activeElement;
   }else{
        // recurse over the child documents:
        var frames = doc.getElementsByTagName('frame');
        for (var i = 0; i < frames.length; i++)
        {
                var d = frames[i].contentDocument;
                 var rv = findactiveelement(d);
                 if(rv != undefined){
                    return rv;
                 }
        }
    }
    return undefined;
};


"

  "Javascript that finds the active element."
  ;; Yes it's ugly, because:
  ;; - there is apparently no way to find the active frame other than recursion
  ;; - the js "for each" construct misbehaved on the "frames" collection
  ;; - a window with no frameset still has frames.length == 1, but
  ;; frames[0].document.activeElement != document.activeElement
  ;;TODO the activeelement type needs to be examined, for iframe, etc.
  )

(defun xwidget-webkit-insert-string ()
  "Insert string into the active field in the current webkit widget."
  ;; Read out the string in the field first and provide for edit.
  (interactive nil xwidget-webkit-mode)
  ;; As the prompt differs on JavaScript execution results,
  ;; the function must handle the prompt itself.
  (let ((xww (xwidget-webkit-current-session)))
    (xwidget-webkit-execute-script
     xww
     (concat xwidget-webkit-activeelement-js "
(function () {
  var res = findactiveelement(document);
  if (res)
    return [res.value, res.type];
})();")
     (lambda (field)
       "Prompt a string for the FIELD and insert in the active input."
       (let ((str (pcase field
                    (`[,val "text"]
                     (read-string "Text: " val))
                    (`[,val "password"]
                     (read-passwd "Password: " nil val))
                    (`[,val "textarea"]
                     (xwidget-webkit-begin-edit-textarea xww val)))))
         (xwidget-webkit-execute-script
          xww
          (format "findactiveelement(document).value='%s'" str)))))))

(defvar xwidget-xwbl)
(defun xwidget-webkit-begin-edit-textarea (xw text)
  "Start editing of a webkit text area.
XW is the xwidget identifier, TEXT is retrieved from the webkit."
  (switch-to-buffer
   (generate-new-buffer "textarea"))
  (setq-local xwidget-xwbl xw)
  (insert text))

(defun xwidget-webkit-end-edit-textarea ()
  "End editing of a webkit text area."
  (interactive nil xwidget-webkit-mode)
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\\n" nil t))
  (xwidget-webkit-execute-script
   xwidget-xwbl
   (format "findactiveelement(document).value='%s'"
           (buffer-substring (point-min) (point-max))))
  ;;TODO convert linefeed to \n
  )

(defun xwidget-webkit-show-element (xw element-selector)
  "Make webkit xwidget XW show a named element ELEMENT-SELECTOR.
The ELEMENT-SELECTOR must be a valid CSS selector.  For example,
use this to display an anchor."
  (interactive (list (xwidget-webkit-current-session)
                     (read-string "Element selector: "))
               xwidget-webkit-mode)
  (xwidget-webkit-execute-script
   xw
   (format "
(function (query) {
  var el = document.querySelector(query);
  if (el !== null) {
    window.scrollTo(0, el.offsetTop);
  }
})('%s');"
    element-selector)))

(defun xwidget-webkit-show-named-element (xw element-name)
  "Make webkit xwidget XW show a named element ELEMENT-NAME.
For example, use this to display an anchor."
  (interactive (list (xwidget-webkit-current-session)
                     (read-string "Element name: "))
               xwidget-webkit-mode)
  ;; TODO: This needs to be interfaced into browse-url somehow.  The
  ;; tricky part is that we need to do this in two steps: A: load the
  ;; base url, wait for load signal to arrive B: navigate to the
  ;; anchor when the base url is finished rendering
  (xwidget-webkit-execute-script
   xw
   (format "
(function (query) {
  var el = document.getElementsByName(query)[0];
  if (el !== undefined) {
    window.scrollTo(0, el.offsetTop);
  }
})('%s');"
    element-name)))

(defun xwidget-webkit-show-id-element (xw element-id)
  "Make webkit xwidget XW show an id-element ELEMENT-ID.
For example, use this to display an anchor."
  (interactive (list (xwidget-webkit-current-session)
                     (read-string "Element id: "))
               xwidget-webkit-mode)
  (xwidget-webkit-execute-script
   xw
   (format "
(function (query) {
  var el = document.getElementById(query);
  if (el !== null) {
    window.scrollTo(0, el.offsetTop);
  }
})('%s');"
    element-id)))

(defun xwidget-webkit-show-id-or-named-element (xw element-id)
   "Make webkit xwidget XW show a name or element id ELEMENT-ID.
For example, use this to display an anchor."
  (interactive (list (xwidget-webkit-current-session)
                     (read-string "Name or element id: "))
               xwidget-webkit-mode)
  (xwidget-webkit-execute-script
   xw
   (format "
(function (query) {
  var el = document.getElementById(query) ||
           document.getElementsByName(query)[0];
  if (el !== undefined) {
    window.scrollTo(0, el.offsetTop);
  }
})('%s');"
    element-id)))

(defun xwidget-webkit-adjust-size-to-content ()
  "Adjust webkit to content size."
  (interactive nil xwidget-webkit-mode)
  (xwidget-adjust-size-to-content (xwidget-webkit-current-session)))

(defun xwidget-webkit-adjust-size-dispatch ()
  "Adjust size according to mode."
  (interactive nil xwidget-webkit-mode)
  (xwidget-webkit-adjust-size-to-window (xwidget-webkit-current-session))
  ;; The recenter is intended to correct a visual glitch.
  ;; It errors out if the buffer isn't visible, but then we don't get
  ;; the glitch, so silence errors.
  (ignore-errors
    (recenter-top-bottom)))

;; Utility functions

(defun xwidget-window-inside-pixel-width (window)
  "Return Emacs WINDOW body width in pixel."
  (let ((edges (window-inside-pixel-edges window)))
    (- (nth 2 edges) (nth 0 edges))))

(defun xwidget-window-inside-pixel-height (window)
  "Return Emacs WINDOW body height in pixel."
  (let ((edges (window-inside-pixel-edges window)))
    (- (nth 3 edges) (nth 1 edges))))

(defun xwidget-webkit-adjust-size-to-window (xwidget &optional window)
  "Adjust the size of the webkit XWIDGET to fit the WINDOW."
  (xwidget-resize xwidget
                  (xwidget-window-inside-pixel-width window)
                  (xwidget-window-inside-pixel-height window)))

(defun xwidget-webkit-adjust-size (w h)
  "Manually set webkit size to width W, height H."
  ;; TODO shouldn't be tied to the webkit xwidget
  (interactive "nWidth:\nnHeight:\n" xwidget-webkit-mode)
  (xwidget-resize (xwidget-webkit-current-session) w h))

(defun xwidget-webkit-fit-width ()
  "Adjust width of webkit to window width."
  (interactive nil xwidget-webkit-mode)
  (xwidget-webkit-adjust-size (- (nth 2 (window-inside-pixel-edges))
                                 (car (window-inside-pixel-edges)))
                              1000))

(defun xwidget-webkit-auto-adjust-size (window)
  "Adjust the size of the webkit widget in the given WINDOW."
  (with-current-buffer (window-buffer window)
    (when (eq major-mode 'xwidget-webkit-mode)
      (let ((xwidget (xwidget-webkit-current-session)))
        (xwidget-webkit-adjust-size-to-window xwidget window)))))

(defun xwidget-webkit-adjust-size-in-frame (frame)
  "Dynamically adjust webkit widget for all windows of the FRAME."
  (walk-windows 'xwidget-webkit-auto-adjust-size 'no-minibuf frame))

(eval-after-load 'xwidget-webkit-mode
  (add-to-list 'window-size-change-functions
               'xwidget-webkit-adjust-size-in-frame))

(defun xwidget-webkit--create-new-session-buffer (url &optional callback)
  "Create a new webkit session buffer to display URL in an xwidget.
Optional function CALLBACK specifies the callback for webkit xwidgets;
see `xwidget-webkit-callback'."
  (let* ((bufname
          ;; Generate a temp-name based on current buffer name.  The
          ;; buffer will subsequently be renamed by
          ;; `xwidget-webkit-callback'.  This approach can avoid
          ;; flicker of buffer-name in mode-line.
          (generate-new-buffer-name (buffer-name)))
         (callback (or callback #'xwidget-webkit-callback))
         (current-session (xwidget-webkit-current-session))
         xw)
    (setq xwidget-webkit-last-session-buffer (get-buffer-create bufname))
    ;; The xwidget id is stored in a text property, so we need to have
    ;; at least character in this buffer.
    ;; Insert invisible url, good default for next `g' to browse url.
    (with-current-buffer xwidget-webkit-last-session-buffer
      (let ((start (point)))
        (insert url)
        (put-text-property start (+ start (length url)) 'invisible t)
        (setq xw (xwidget-insert
                  start 'webkit bufname
                  (xwidget-window-inside-pixel-width (selected-window))
                  (xwidget-window-inside-pixel-height (selected-window))
                  nil current-session)))
      (when xwidget-webkit-cookie-file
        (xwidget-webkit-set-cookie-storage-file
         xw (expand-file-name xwidget-webkit-cookie-file)))
      (xwidget-put xw 'callback callback)
      (xwidget-put xw 'display-callback #'xwidget-webkit-display-callback)
      (xwidget-webkit-mode))
    xwidget-webkit-last-session-buffer))

(defun xwidget-webkit-new-session (url)
  "Display URL in a new webkit xwidget."
  (switch-to-buffer (xwidget-webkit--create-new-session-buffer url))
  (xwidget-webkit-goto-uri (xwidget-webkit-last-session) url))

(defun xwidget-webkit-import-widget (xwidget)
  "Create a new webkit session buffer from XWIDGET, an existing xwidget.
Return the buffer."
  (let* ((bufname
          ;; Generate a temp-name based on current buffer name. it
          ;; will be renamed by `xwidget-webkit-callback' in the
          ;; future. This approach can limit flicker of buffer-name in
          ;; mode-line.
          (generate-new-buffer-name (buffer-name)))
         (callback #'xwidget-webkit-callback)
         (buffer (get-buffer-create bufname)))
    (with-current-buffer buffer
      (setq xwidget-webkit-last-session-buffer buffer)
      (save-excursion
        (erase-buffer)
        (insert ".")
        (put-text-property (point-min) (point-max)
                           'display (list 'xwidget :xwidget xwidget)))
      (xwidget-put xwidget 'callback callback)
      (xwidget-put xwidget 'display-callback
                   #'xwidget-webkit-display-callback)
      (set-xwidget-buffer xwidget buffer)
      (xwidget-webkit-mode))
    buffer))

(defun xwidget-webkit-display-event (event)
  "Trigger display callback for EVENT."
  (interactive "e")
  (let ((xwidget (cadr event))
        (source (caddr event)))
    (when (xwidget-get source 'display-callback)
      (funcall (xwidget-get source 'display-callback)
               xwidget source))))

(defun xwidget-webkit-display-callback (xwidget _source)
  "Import XWIDGET and display it."
  (display-buffer (xwidget-webkit-import-widget xwidget)))

(define-key special-event-map [xwidget-display-event] 'xwidget-webkit-display-event)

(defun xwidget-webkit-goto-url (url)
  "Goto URL with xwidget webkit."
  (if (xwidget-webkit-current-session)
      (progn
        (xwidget-webkit-goto-uri (xwidget-webkit-current-session) url)
        (switch-to-buffer (xwidget-buffer (xwidget-webkit-current-session))))
    (xwidget-webkit-new-session url)))

(defun xwidget-webkit-back ()
  "Go back to previous URL in xwidget webkit buffer."
  (interactive nil xwidget-webkit-mode)
  (xwidget-webkit-goto-history (xwidget-webkit-current-session) -1))

(defun xwidget-webkit-forward ()
  "Go forward in history."
  (interactive nil xwidget-webkit-mode)
  (xwidget-webkit-goto-history (xwidget-webkit-current-session) 1))

(defun xwidget-webkit-reload ()
  "Reload current URL."
  (interactive nil xwidget-webkit-mode)
  (xwidget-webkit-goto-history (xwidget-webkit-current-session) 0))

(defun xwidget-webkit-current-url ()
  "Display the current xwidget webkit URL and place it on the `kill-ring'."
  (interactive nil xwidget-webkit-mode)
  (let ((url (xwidget-webkit-uri (xwidget-webkit-current-session))))
    (when url (kill-new url))
    (message "URL: %s" url)))

(defun xwidget-webkit-browse-history ()
  "Display a buffer containing the history of page loads."
  (interactive)
  (setq xwidget-webkit-last-session-buffer (current-buffer))
  (let ((buffer (get-buffer-create "*Xwidget WebKit History*")))
    (with-current-buffer buffer
      (xwidget-webkit-history-mode))
    (display-buffer buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xwidget-webkit-get-selection (proc)
  "Get the webkit selection and pass it to PROC."
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   "window.getSelection().toString();"
   proc))

(defun xwidget-webkit-copy-selection-as-kill ()
  "Get the webkit selection and put it on the `kill-ring'."
  (interactive nil xwidget-webkit-mode)
  (xwidget-webkit-get-selection #'kill-new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Xwidget plist management (similar to the process plist functions)

(defun xwidget-get (xwidget propname)
  "Get an xwidget's property value.
XWIDGET is an xwidget, PROPNAME a property.
Returns the last value stored with `xwidget-put'."
  (plist-get (xwidget-plist xwidget) propname))

(defun xwidget-put (xwidget propname value)
  "Set an xwidget's property value.
XWIDGET is an xwidget, PROPNAME a property to be set to specified VALUE.
You can retrieve the value with `xwidget-get'."
  (set-xwidget-plist xwidget
                     (plist-put (xwidget-plist xwidget) propname value)))

(defvar-keymap xwidget-webkit-edit-mode-map :full t)

(define-key xwidget-webkit-edit-mode-map [backspace] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [tab] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [left] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [right] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [up] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [down] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [return] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [C-left] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [C-right] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [C-up] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [C-down] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [C-return] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [S-left] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [S-right] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [S-up] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [S-down] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [S-return] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [M-left] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [M-right] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [M-up] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [M-down] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [M-return] 'xwidget-webkit-pass-command-event)
(define-key xwidget-webkit-edit-mode-map [C-backspace] 'xwidget-webkit-pass-command-event)

(define-minor-mode xwidget-webkit-edit-mode
  "Minor mode for editing the content of WebKit buffers.

This defines most self-inserting characters and some common
keyboard shortcuts to `xwidget-webkit-pass-command-event', which
will pass the key events corresponding to these characters to the
WebKit widget."
  :keymap xwidget-webkit-edit-mode-map)

(substitute-key-definition 'self-insert-command
                           'xwidget-webkit-pass-command-event
                           xwidget-webkit-edit-mode-map
                           global-map)

(declare-function xwidget-webkit-search "xwidget.c")
(declare-function xwidget-webkit-next-result "xwidget.c")
(declare-function xwidget-webkit-previous-result "xwidget.c")
(declare-function xwidget-webkit-finish-search "xwidget.c")

(defvar-local xwidget-webkit-isearch--string ""
  "The current search query.")
(defvar-local xwidget-webkit-isearch--is-reverse nil
  "Whether or not the current isearch should be reverse.")
(defvar xwidget-webkit-isearch--read-string-buffer nil
  "The buffer we are reading input method text for, if any.")

(defun xwidget-webkit-isearch--update (&optional only-message)
  "Update the current buffer's WebKit widget's search query.
If ONLY-MESSAGE is non-nil, the query will not be sent to the
WebKit widget.  The query will be set to the contents of
`xwidget-webkit-isearch--string'."
  (unless only-message
    (xwidget-webkit-search xwidget-webkit-isearch--string
                           (xwidget-webkit-current-session)
                           t xwidget-webkit-isearch--is-reverse t))
  (let ((message-log-max nil))
    (message "%s" (concat (propertize "Search contents: " 'face 'minibuffer-prompt)
                          xwidget-webkit-isearch--string))))

(defun xwidget-webkit-isearch-erasing-char (count)
  "Erase the last COUNT characters of the current query."
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (when (> (length xwidget-webkit-isearch--string) 0)
    (setq xwidget-webkit-isearch--string
          (substring xwidget-webkit-isearch--string 0
                     (- (length xwidget-webkit-isearch--string) count))))
  (xwidget-webkit-isearch--update))

(defun xwidget-webkit-isearch-with-input-method ()
  "Handle a request to use the input method to modify the search query."
  (interactive)
  (let ((key (car unread-command-events))
	events)
    (setq unread-command-events (cdr unread-command-events)
	  events (funcall input-method-function key))
    (dolist (k events)
      (with-current-buffer xwidget-webkit-isearch--read-string-buffer
        (setq xwidget-webkit-isearch--string
              (concat xwidget-webkit-isearch--string
                      (char-to-string k)))))
    (exit-minibuffer)))

(defun xwidget-webkit-isearch-printing-char-with-input-method (char)
  "Handle printing char CHAR with the current input method."
  (let ((minibuffer-local-map (make-keymap))
        (xwidget-webkit-isearch--read-string-buffer (current-buffer)))
    (define-key minibuffer-local-map [with-input-method]
      'xwidget-webkit-isearch-with-input-method)
    (setq unread-command-events
          (cons 'with-input-method
                (cons char unread-command-events)))
    (read-string "Search contents: "
                 xwidget-webkit-isearch--string
                 'junk-hist nil t)
    (xwidget-webkit-isearch--update)))

(defun xwidget-webkit-isearch-printing-char (char &optional count)
  "Add ordinary character CHAR to the search string and search.
With argument, add COUNT copies of CHAR."
  (interactive (list last-command-event
                     (prefix-numeric-value current-prefix-arg)))
  (if current-input-method
      (xwidget-webkit-isearch-printing-char-with-input-method char)
    (setq xwidget-webkit-isearch--string (concat xwidget-webkit-isearch--string
                                                 (make-string (or count 1) char))))
  (xwidget-webkit-isearch--update))

(defun xwidget-webkit-isearch-forward (count)
  "Move to the next search result COUNT times."
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (let ((was-reverse xwidget-webkit-isearch--is-reverse))
    (setq xwidget-webkit-isearch--is-reverse nil)
    (when was-reverse
      (xwidget-webkit-isearch--update)
      (setq count (1- count))))
  (let ((i 0))
    (while (< i count)
      (xwidget-webkit-next-result (xwidget-webkit-current-session))
      (incf i)))
  (xwidget-webkit-isearch--update t))

(defun xwidget-webkit-isearch-backward (count)
  "Move to the previous search result COUNT times."
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (let ((was-reverse xwidget-webkit-isearch--is-reverse))
    (setq xwidget-webkit-isearch--is-reverse t)
    (unless was-reverse
      (xwidget-webkit-isearch--update)
      (setq count (1- count))))
  (let ((i 0))
    (while (< i count)
      (xwidget-webkit-previous-result (xwidget-webkit-current-session))
      (incf i)))
  (xwidget-webkit-isearch--update t))

(defun xwidget-webkit-isearch-exit ()
  "Exit incremental search of a WebKit buffer."
  (interactive)
  (xwidget-webkit-isearch-mode 0))

(defvar-keymap xwidget-webkit-isearch-mode-map
  :doc "The keymap used inside `xwidget-webkit-isearch-mode'."
  :full t)

(set-char-table-range (nth 1 xwidget-webkit-isearch-mode-map)
                      (cons 0 (max-char))
                      'xwidget-webkit-isearch-exit)

(substitute-key-definition 'self-insert-command
                           'xwidget-webkit-isearch-printing-char
                           xwidget-webkit-isearch-mode-map
                           global-map)

(define-key xwidget-webkit-isearch-mode-map (kbd "DEL")
  'xwidget-webkit-isearch-erasing-char)
(define-key xwidget-webkit-isearch-mode-map [backspace] 'xwidget-webkit-isearch-erasing-char)
(define-key xwidget-webkit-isearch-mode-map [return] 'xwidget-webkit-isearch-exit)
(define-key xwidget-webkit-isearch-mode-map "\r" 'xwidget-webkit-isearch-exit)
(define-key xwidget-webkit-isearch-mode-map "\C-g" 'xwidget-webkit-isearch-exit)
(define-key xwidget-webkit-isearch-mode-map "\C-r" 'xwidget-webkit-isearch-backward)
(define-key xwidget-webkit-isearch-mode-map "\C-s" 'xwidget-webkit-isearch-forward)
(define-key xwidget-webkit-isearch-mode-map "\C-y" 'xwidget-webkit-isearch-yank-kill)
(define-key xwidget-webkit-isearch-mode-map "\C-\\" 'toggle-input-method)
(define-key xwidget-webkit-isearch-mode-map "\t" 'xwidget-webkit-isearch-printing-char)

(let ((meta-map (make-keymap)))
  (set-char-table-range (nth 1 meta-map)
                        (cons 0 (max-char))
                        'xwidget-webkit-isearch-exit)
  (define-key xwidget-webkit-isearch-mode-map (char-to-string meta-prefix-char) meta-map))

(define-minor-mode xwidget-webkit-isearch-mode
  "Minor mode for performing incremental search inside WebKit buffers.

This resembles the regular incremental search, but it does not
support recursive edits.

If this mode is activated with `\\<xwidget-webkit-isearch-mode-map>\\[xwidget-webkit-isearch-backward]', then the search will by default
start in the reverse direction.

To navigate around the search results, type
\\<xwidget-webkit-isearch-mode-map>\\[xwidget-webkit-isearch-forward] to move forward, and
\\<xwidget-webkit-isearch-mode-map>\\[xwidget-webkit-isearch-backward] to move backward.

To insert the string at the front of the kill ring into the
search query, type \\<xwidget-webkit-isearch-mode-map>\\[xwidget-webkit-isearch-yank-kill].

Press \\<xwidget-webkit-isearch-mode-map>\\[xwidget-webkit-isearch-exit] to exit incremental search."
  :keymap xwidget-webkit-isearch-mode-map
  (if xwidget-webkit-isearch-mode
      (progn
        (setq xwidget-webkit-isearch--string "")
        (setq xwidget-webkit-isearch--is-reverse (eq last-command-event ?\C-r))
        (xwidget-webkit-isearch--update))
    (xwidget-webkit-finish-search (xwidget-webkit-current-session))))

(defun xwidget-webkit-isearch-yank-kill ()
  "Append the most recent kill from `kill-ring' to the current query."
  (interactive)
  (unless xwidget-webkit-isearch-mode
    (xwidget-webkit-isearch-mode t))
  (setq xwidget-webkit-isearch--string
        (concat xwidget-webkit-isearch--string
                (current-kill 0)))
  (xwidget-webkit-isearch--update))

(defvar-local xwidget-webkit-history--session nil
  "The xwidget this history buffer controls.")

(define-button-type 'xwidget-webkit-history 'action #'xwidget-webkit-history-select-item)

(defun xwidget-webkit-history--insert-item (item)
  "Insert specified ITEM into the current buffer."
  (let ((idx (car item))
        (title (cadr item))
        (uri (caddr item)))
    (push (list idx (vector (list (number-to-string idx)
                                  :type 'xwidget-webkit-history)
                            (list title :type 'xwidget-webkit-history)
                            (list uri :type 'xwidget-webkit-history)))
          tabulated-list-entries)))

(defun xwidget-webkit-history-select-item (pos)
  "Navigate to the history item underneath POS."
  (interactive "P")
  (let ((id (tabulated-list-get-id pos)))
    (xwidget-webkit-goto-history xwidget-webkit-history--session id))
  (xwidget-webkit-history-reload))

(defun xwidget-webkit-history-reload (&rest _ignored)
  "Reload the current history buffer."
  (interactive)
  (setq tabulated-list-entries nil)
  (let* ((back-forward-list
          (xwidget-webkit-back-forward-list xwidget-webkit-history--session))
         (back-list (car back-forward-list))
         (here (cadr back-forward-list))
         (forward-list (caddr back-forward-list)))
    (mapc #'xwidget-webkit-history--insert-item (nreverse forward-list))
    (xwidget-webkit-history--insert-item here)
    (mapc #'xwidget-webkit-history--insert-item back-list)
    (tabulated-list-print t nil)
    (goto-char (point-min))
    (let ((position (line-beginning-position (1+ (length back-list)))))
      (goto-char position)
      (setq-local overlay-arrow-position (make-marker))
      (set-marker overlay-arrow-position position))))

(define-derived-mode xwidget-webkit-history-mode tabulated-list-mode
  "Xwidget Webkit History"
  "Major mode for browsing the history of an Xwidget Webkit buffer.
Each line describes an entry in history."
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq tabulated-list-format [("Index" 10 nil)
                               ("Title" 50 nil)
                               ("URL" 100 nil)])
  (setq tabulated-list-entries nil)
  (setq xwidget-webkit-history--session (xwidget-webkit-current-session))
  (xwidget-webkit-history-reload)
  (setq-local revert-buffer-function #'xwidget-webkit-history-reload)
  (tabulated-list-init-header))

(define-key xwidget-webkit-history-mode-map (kbd "RET")
  #'xwidget-webkit-history-select-item)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xwidget-view-list)              ; xwidget.c
(defvar xwidget-list)                   ; xwidget.c

(defun xwidget-delete-zombies ()
  "Helper for `xwidget-cleanup'."
  (dolist (xwidget-view xwidget-view-list)
    (when (or (not (window-live-p (xwidget-view-window xwidget-view)))
              (not (memq (xwidget-view-model xwidget-view)
                         xwidget-list)))
      (delete-xwidget-view xwidget-view))))

(defun xwidget-cleanup ()
  "Delete zombie xwidgets."
  ;; During development it was sometimes easy to wind up with zombie
  ;; xwidget instances.
  ;; This function tries to implement a workaround should it occur again.
  (interactive)
  ;; Kill xviews that should have been deleted but still linger.
  (xwidget-delete-zombies)
  ;; Redraw display otherwise ghost of zombies will remain to haunt the screen
  (redraw-display))

(defun xwidget-kill-buffer-query-function ()
  "Ask before killing a buffer that has xwidgets."
  (let ((xwidgets (get-buffer-xwidgets (current-buffer))))
    (or (not xwidgets)
        (not (memq t (mapcar #'xwidget-query-on-exit-flag xwidgets)))
        (yes-or-no-p
         (format "Buffer %S has xwidgets; kill it? " (buffer-name))))))

(when (featurep 'xwidget-internal)
  (add-hook 'kill-buffer-query-functions #'xwidget-kill-buffer-query-function)
  ;; This would have felt better in C, but this seems to work well in
  ;; practice though.
  (add-hook 'window-configuration-change-hook #'xwidget-delete-zombies))

(provide 'xwidget)
;;; xwidget.el ends here
