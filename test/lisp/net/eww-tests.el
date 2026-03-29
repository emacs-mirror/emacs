;;; eww-tests.el --- tests for eww.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Free Software Foundation, Inc.

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

(require 'ert)
(require 'eww)

(defvar eww-test--response-function (lambda (url) (concat "\n" url))
  "A function for returning a mock response for URL.
The default just returns an empty list of headers and the URL as the
body.")

(defvar eww-test--lots-of-words
  (string-join (make-list 20 "All work and no play makes Jack a dull boy.")
               " ")
  "A long enough run of words to satisfy EWW's readable mode cutoff.")

(defvar eww-test--wordy-page
  (concat "<html>"
          "<head>"
          "<title>Welcome to my home page</title>"
          "<link rel=\"home\" href=\"somewhere.invalid\">"
          "<base href=\"/foo/\">"
          "</head><body>"
          "<a>This is an uninteresting sentence.</a>"
          "<div>" eww-test--lots-of-words "</div>"
          "</body></html>"))

(defmacro eww-test--with-mock-retrieve (&rest body)
  "Evaluate BODY with a mock implementation of `eww-retrieve'.
This avoids network requests during our tests.  Additionally, prepare a
temporary EWW buffer for our tests."
  (declare (indent 0))
    `(cl-letf (((symbol-function 'eww-retrieve)
                (lambda (url callback args)
                  (with-temp-buffer
                    (insert (funcall eww-test--response-function url))
                    (apply callback nil args)))))
       (with-temp-buffer
         (eww-mode)
         ,@body)))

(defun eww-test--history-urls ()
  (mapcar (lambda (elem) (plist-get elem :url)) eww-history))

;;; Tests:

(ert-deftest eww-test/display/html ()
  "Test displaying a simple HTML page."
  (skip-unless (libxml-available-p))
  (eww-test--with-mock-retrieve
    (let ((eww-test--response-function
           (lambda (url)
             (concat "Content-Type: text/html\n\n"
                     (format "<html><body><h1>Hello</h1>%s</body></html>"
                             url)))))
      (eww "example.invalid")
      ;; Check that the buffer contains the rendered HTML.
      (should (equal (buffer-string) "Hello\n\n\nhttp://example.invalid/\n"))
      (should (equal (get-text-property (point-min) 'face)
                     '(shr-text shr-h1)))
      ;; Check that the DOM includes the `base'.
      (should (equal (pcase (plist-get eww-data :dom)
                       (`(base ((href . ,url)) ,_) url))
                     "http://example.invalid/")))))

(ert-deftest eww-test/tag/textarea/starts-on-new-line ()
  "Test that inserting a newline before a <textarea> works correctly."
  (skip-unless (libxml-available-p))
  (eww-test--with-mock-retrieve
    (let ((shr-fill-text nil)
          (eww-test--response-function
           (lambda (_url)
             (concat "Content-Type: text/html\n\n"
                     "<html><body>"
                     "field: <textarea>text</textarea>"
                     "</body></html>"))))
      (eww "example.invalid")
      ;; Make sure the text before the <textarea> doesn't have any of
      ;; the <textarea> properties.
      (goto-char (point-min))
      (should-not (get-text-property (point) 'eww-form))
      (should (eq (get-text-property (point) 'face) 'shr-text))
      ;; Make sure the <textarea> has the correct properties.
      (forward-line 1)
      (should (get-text-property (point) 'eww-form))
      (should (eq (get-text-property (point) 'face)
                  'eww-form-textarea)))))

(ert-deftest eww-test/tag/textarea/reload ()
  "Ensure that reloading a document with a <textarea> works correctly."
  (skip-unless (libxml-available-p))
  (eww-test--with-mock-retrieve
    (let ((shr-fill-text nil)
          (eww-test--response-function
           (lambda (_url)
             (concat "Content-Type: text/html\n\n"
                     "<html><body>"
                     "<textarea>text</textarea>"
                     "<div>this is the end</div>"
                     "</body></html>"))))
      (eww "example.invalid")
      ;; Make sure that text after the <textarea> is regular text.
      (goto-char (point-max))
      (forward-line -1)
      (should (eq (get-text-property (point) 'face) 'shr-text))
      ;; Now reload, and make sure the above is still true.
      (eww-reload)
      (accept-process-output)           ; Let track-changes run.
      (goto-char (point-max))
      (forward-line -1)
      (should (eq (get-text-property (point) 'face) 'shr-text)))))

(ert-deftest eww-test/history/new-page ()
  "Test that when visiting a new page, the previous one goes into the history."
  (eww-test--with-mock-retrieve
    (eww "one.invalid")
    (eww "two.invalid")
    (should (equal (eww-test--history-urls)
                   '("http://one.invalid/")))
    (eww "three.invalid")
    (should (equal (eww-test--history-urls)
                   '("http://two.invalid/"
                     "http://one.invalid/")))))

(ert-deftest eww-test/history/back-forward ()
  "Test that navigating through history just changes our history position.
See bug#69232."
  (eww-test--with-mock-retrieve
    (eww "one.invalid")
    (eww "two.invalid")
    (eww "three.invalid")
    (let ((url-history '("http://three.invalid/"
                         "http://two.invalid/"
                         "http://one.invalid/")))
      ;; Go back one page.  This should add "three.invalid" to the
      ;; history, making our position in the list 2.
      (eww-back-url)
      (should (equal (eww-test--history-urls) url-history))
      (should (= eww-history-position 2))
      ;; Go back again.
      (eww-back-url)
      (should (equal (eww-test--history-urls) url-history))
      (should (= eww-history-position 3))
      ;; At the beginning of the history, so trying to go back should
      ;; signal an error.
      (should-error (eww-back-url))
      ;; Go forward once.
      (eww-forward-url)
      (should (equal (eww-test--history-urls) url-history))
      (should (= eww-history-position 2))
      ;; Go forward again.
      (eww-forward-url)
      (should (equal (eww-test--history-urls) url-history))
      (should (= eww-history-position 1))
      ;; At the end of the history, so trying to go forward should
      ;; signal an error.
      (should-error (eww-forward-url)))))

(ert-deftest eww-test/history/reload-in-place ()
  "Test that reloading historical pages updates their history entry in-place.
See bug#69232."
  (eww-test--with-mock-retrieve
    (eww "one.invalid")
    (eww "two.invalid")
    (eww "three.invalid")
    (eww-back-url)
    ;; Make sure our history has the original page text.
    (should (equal (plist-get (nth 1 eww-history) :text)
                   "http://two.invalid/"))
    (should (= eww-history-position 2))
    ;; Reload the page.
    (let ((eww-test--response-function
           (lambda (url) (concat "\nreloaded " url))))
      (eww-reload)
      (should (= eww-history-position 2)))
    ;; Go to another page, and make sure the history is correct,
    ;; including the reloaded page text.
    (eww "four.invalid")
    (should (equal (eww-test--history-urls) '("http://two.invalid/"
                                              "http://one.invalid/")))
    (should (equal (plist-get (nth 0 eww-history) :text)
                   "reloaded http://two.invalid/"))
    (should (= eww-history-position 0))))

(ert-deftest eww-test/history/before-navigate/delete-future-history ()
  "Test that going to a new page from a historical one deletes future history.
See bug#69232."
  (eww-test--with-mock-retrieve
    (eww "one.invalid")
    (eww "two.invalid")
    (eww "three.invalid")
    (eww-back-url)
    (eww "four.invalid")
    (eww "five.invalid")
    (should (equal (eww-test--history-urls) '("http://four.invalid/"
                                              "http://two.invalid/"
                                              "http://one.invalid/")))
    (should (= eww-history-position 0))))

(ert-deftest eww-test/history/before-navigate/ignore-history ()
  "Test that going to a new page from a historical one preserves history.
This sets `eww-before-browse-history-function' to `ignore' to preserve
history.  See bug#69232."
  (let ((eww-before-browse-history-function #'ignore))
    (eww-test--with-mock-retrieve
      (eww "one.invalid")
      (eww "two.invalid")
      (eww "three.invalid")
      (eww-back-url)
      (eww "four.invalid")
      (eww "five.invalid")
      (should (equal (eww-test--history-urls) '("http://four.invalid/"
                                                "http://three.invalid/"
                                                "http://two.invalid/"
                                                "http://one.invalid/")))
      (should (= eww-history-position 0)))))

(ert-deftest eww-test/history/before-navigate/clone-previous ()
  "Test that going to a new page from a historical one clones prior history.
This sets `eww-before-browse-history-function' to
`eww-clone-previous-history' to clone the history.  See bug#69232."
  (let ((eww-before-browse-history-function #'eww-clone-previous-history))
    (eww-test--with-mock-retrieve
      (eww "one.invalid")
      (eww "two.invalid")
      (eww "three.invalid")
      (eww-back-url)
      (eww "four.invalid")
      (eww "five.invalid")
      (should (equal (eww-test--history-urls)
                     '(;; New page and cloned history.
                       "http://four.invalid/"
                       "http://two.invalid/"
                       "http://one.invalid/"
                       ;; Original history.
                       "http://three.invalid/"
                       "http://two.invalid/"
                       "http://one.invalid/")))
      (should (= eww-history-position 0)))))

(ert-deftest eww-test/readable/toggle-display ()
  "Test toggling the display of the \"readable\" parts of a web page."
  (skip-unless (libxml-available-p))
  (eww-test--with-mock-retrieve
    (let* ((shr-width most-positive-fixnum)
           (shr-use-fonts nil)
           (eww-test--response-function
            (lambda (_url)
              (concat "Content-Type: text/html\n\n"
                      eww-test--wordy-page))))
      (eww "example.invalid")
      ;; Make sure EWW renders the whole document.
      (should-not (plist-get eww-data :readable))
      (should (string-prefix-p
               "This is an uninteresting sentence."
               (buffer-substring-no-properties (point-min) (point-max))))
      (eww-readable 'toggle)
      ;; Now, EWW should render just the "readable" parts.
      (should (plist-get eww-data :readable))
      (should (string-match-p
               (concat "\\`" (regexp-quote eww-test--lots-of-words) "\n*\\'")
               (buffer-substring-no-properties (point-min) (point-max))))
      (eww-readable 'toggle)
      ;; Finally, EWW should render the whole document again.
      (should-not (plist-get eww-data :readable))
      (should (string-prefix-p
               "This is an uninteresting sentence."
               (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest eww-test/readable/default-readable ()
  "Test that EWW displays readable parts of pages by default when applicable."
  (skip-unless (libxml-available-p))
  (eww-test--with-mock-retrieve
    (let* ((eww-test--response-function
            (lambda (_url)
              (concat "Content-Type: text/html\n\n"
                      eww-test--wordy-page)))
           (eww-readable-urls '("://example\\.invalid/")))
      (eww "example.invalid")
      ;; Make sure EWW uses "readable" mode.
      (should (plist-get eww-data :readable))
      ;; Make sure the page include the <title>, <link>, and <base> nodes.
      (should (equal (plist-get eww-data :title) "Welcome to my home page"))
      (should (equal (plist-get eww-data :home) "somewhere.invalid"))
      (let* ((html (dom-child-by-tag (plist-get eww-data :dom) 'html))
             (base-tags (dom-by-tag html 'base)))
        (should (length= base-tags 1))
        (should (equal (dom-attr (car base-tags) 'href) "/foo/"))))))

(ert-deftest eww-test/readable/default-readable/non-readable-page ()
  "Test that EWW handles readable-by-default correctly for non-readable pages."
  (skip-unless (libxml-available-p))
  (eww-test--with-mock-retrieve
    (let* ((eww-test--response-function
            (lambda (_url)
              (concat "Content-Type: text/html\n\n"
                      "<html><body><h1>Hello</h1></body></html>")))
           (eww-readable-urls '("://example\\.invalid/")))
      (eww "example.invalid")
      ;; Make sure EWW doesn't use "readable" mode here.
      (should-not (plist-get eww-data :readable)))))

(ert-deftest eww-test/readable/reload-resets-readable ()
  "Test that reloading a page resets the \"readable\" state."
  (skip-unless (libxml-available-p))
  (eww-test--with-mock-retrieve
    (let* ((shr-width most-positive-fixnum)
           (shr-use-fonts nil)
           (eww-test--response-function
            (lambda (_url)
              (concat "Content-Type: text/html\n\n"
                      eww-test--wordy-page))))
      (eww "example.invalid")
      (eww-readable 'toggle)
      (should (plist-get eww-data :readable))
      ;; Reload the page, and check if the result uses readable view.
      (eww-reload)
      (should-not (plist-get eww-data :readable)))))

(provide 'eww-tests)
;; eww-tests.el ends here
