;;; dnd-tests.el --- Tests for window system independent DND support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

;; Tests for stuff in dnd.el that doesn't require a window system.

;; The drag API tests only check the behavior of the simplified drag
;; APIs in dnd.el.  Actual drags are not performed during the
;; automated testing process (make check), but some of the tests can
;; also be run under X.

;;; Code:

(require 'dnd)
(require 'cl-lib)
(require 'tramp)
(require 'select)
(require 'ert-x)
(require 'browse-url)

(defvar dnd-tests-selection-table nil
  "Alist of selection names to their values.")

(defvar x-treat-local-requests-remotely)
(defvar x-dnd-preserve-selection-data)

;; Define some replacements for functions used by the drag-and-drop
;; code on X when running under something else.
(unless (eq window-system 'x)
  ;; Substitute for x-begin-drag, which isn't present on all systems.
  (defalias 'x-begin-drag
    (lambda (_targets &optional action frame &rest _)
      ;; Verify that frame is either nil or a valid frame.
      (when (and frame (not (frame-live-p frame)))
        (signal 'wrong-type-argument frame))
      ;; Verify that the action is valid and pretend the drag succeeded
      ;; (by returning the action).
      (cl-ecase action
        (XdndActionCopy action)
        (XdndActionMove action)
        (XdndActionLink action)
        ;; These two are not technically valid, but x-begin-drag accepts
        ;; them anyway.
        (XdndActionPrivate action)
        (XdndActionAsk 'XdndActionPrivate))))

  ;; This doesn't work during tests.
  (defalias 'gui-set-selection
    (lambda (type data)
      (or (gui--valid-simple-selection-p data)
          (and (vectorp data)
	       (let ((valid t))
	         (dotimes (i (length data))
	           (or (gui--valid-simple-selection-p (aref data i))
		       (setq valid nil)))
	         valid))
          (signal 'error (list "invalid selection" data)))
      (setf (alist-get type dnd-tests-selection-table) data))))

(declare-function x-get-selection-internal "xselect.c")

(defun dnd-tests-verify-selection-data (type)
  "Return the data of the drag-and-drop selection converted to TYPE."
  (if (eq window-system 'x)
      (let ((x-treat-local-requests-remotely t))
        (x-get-selection-internal 'XdndSelection type))
    (let* ((basic-value (cdr (assq 'XdndSelection
                                   dnd-tests-selection-table)))
           (local-value (if (stringp basic-value)
                            (or (get-text-property 0 type basic-value)
                                basic-value)
                          basic-value))
           (converter-list (cdr (assq type selection-converter-alist)))
           (converter (if (consp converter-list)
                          (cdr converter-list)
                        converter-list)))
      (if (and local-value converter)
          (funcall converter 'XdndSelection type local-value)
        (error "No selection converter or local value: %s" type)))))

(defun dnd-tests-remote-accessible-p ()
  "Return if a test involving remote files can proceed."
  (ignore-errors
    (and
     (file-remote-p ert-remote-temporary-file-directory)
     (file-directory-p ert-remote-temporary-file-directory)
     (file-writable-p ert-remote-temporary-file-directory))))

(defun dnd-tests-make-temp-name ()
  "Return a temporary remote file name for test.
The temporary file is not created."
  (expand-file-name (make-temp-name "dnd-test-remote")
                    ert-remote-temporary-file-directory))

(defun dnd-tests-parse-tt-netfile (netfile)
  "Parse NETFILE and return its components.
NETFILE should be a canonicalized ToolTalk file name.
Return a list of its hostname, real path, and local path."
  (save-match-data
    (when (string-match (concat "HOST=0-\\([[:digit:]]+\\),RPATH=\\([[:digit:]]+\\)-"
                                "\\([[:digit:]]+\\),LPATH=\\([[:digit:]]+\\)-"
                                "\\([[:digit:]]+\\)\\(:\\)")
                        netfile)
      (let ((beg (match-end 6)))
        (list (substring netfile beg
                         (+ beg 1
                            (string-to-number (match-string 1 netfile))))
              (substring netfile
                         (+ beg
                            (string-to-number (match-string 2 netfile)))
                         (+ beg 1
                            (string-to-number (match-string 3 netfile))))
              (substring netfile
                         (+ beg
                            (string-to-number (match-string 4 netfile)))
                         (+ beg 1
                            (string-to-number (match-string 5 netfile)))))))))

(defun dnd-tests-extract-selection-data (selection expect-cons)
  "Return the selection data in SELECTION.
SELECTION can either be the value of `gui-get-selection', or the
return value of a selection converter.

If EXPECT-CONS, then expect SELECTION to be a cons (when not
running under X).

This function only tries to handle strings."
  (when (and expect-cons (not (eq window-system 'x)))
    (should (and (consp selection)
                 (stringp (cdr selection)))))
  (if (stringp selection)
      selection
    (cdr selection)))

(ert-deftest dnd-tests-begin-text-drag ()
  ;; When running this test under X, please make sure to drop onto a
  ;; program with reasonably correct behavior, such as dtpad, gedit,
  ;; or Mozilla.
  ;;                ASCII            Latin-1       UTF-8
  (let ((test-text "hello, everyone! sæl öllsömul! всем привет")
        (x-dnd-preserve-selection-data t))
    ;; Verify that dragging works.
    (should (eq (dnd-begin-text-drag test-text) 'copy))
    (should (eq (dnd-begin-text-drag test-text nil 'move) 'move))
    ;; Verify that the important data types are converted correctly.
    (let ((string-data (dnd-tests-verify-selection-data 'STRING)))
      ;; Check that the Latin-1 target is converted correctly.
      (should (equal (dnd-tests-extract-selection-data string-data t)
                     (encode-coding-string test-text
                                           'iso-8859-1))))
    ;; And that UTF8_STRING and the Xdnd UTF8 string are as well.
    (let* ((string-data (dnd-tests-verify-selection-data
                         'UTF8_STRING))
           (string-data-1 (dnd-tests-verify-selection-data
                           'text/plain\;charset=utf-8))
           (extracted-1 (dnd-tests-extract-selection-data string-data-1 t))
           (extracted (dnd-tests-extract-selection-data string-data t)))
      (should (and (stringp extracted) (stringp extracted-1)))
      (should (equal extracted extracted-1)))
    ;; Now check text/plain.
    (let ((string-data (dnd-tests-verify-selection-data
                        'text/plain)))
      (should (equal (dnd-tests-extract-selection-data string-data t)
                     (encode-coding-string test-text 'ascii))))))

(ert-deftest dnd-tests-begin-file-drag ()
  ;; These tests also involve handling remote file names.
  (skip-unless (and (dnd-tests-remote-accessible-p)
                    ;; TODO: make these tests work under X.
                    (not (eq window-system 'x))))
  (let ((normal-temp-file (expand-file-name (make-temp-name "dnd-test")
                                            temporary-file-directory))
        (normal-multibyte-file (expand-file-name
                                (make-temp-name "тест-на-перетаскивание")
                                temporary-file-directory))
        (remote-temp-file (dnd-tests-make-temp-name))
        (x-dnd-preserve-selection-data t))
    ;; Touch those files if they don't exist.
    (unless (file-exists-p normal-temp-file)
      (write-region "" 0 normal-temp-file))
    (unless (file-exists-p normal-multibyte-file)
      (write-region "" 0 normal-multibyte-file))
    (unless (file-exists-p remote-temp-file)
      (write-region "" 0 remote-temp-file))
    (unwind-protect
        (progn
          ;; Now test dragging a normal file.
          (should (eq (dnd-begin-file-drag normal-temp-file) 'copy))
          ;; Test that the selection data is correct.
          (let ((uri-list-data (cdr (dnd-tests-verify-selection-data 'text/uri-list)))
                (username-data (dnd-tests-verify-selection-data 'text/x-xdnd-username))
                (file-name-data (cdr (dnd-tests-verify-selection-data 'FILE_NAME)))
                (host-name-data (cdr (dnd-tests-verify-selection-data 'HOST_NAME)))
                (netfile-data (cdr (dnd-tests-verify-selection-data '_DT_NETFILE))))
            ;; Check if the URI list is formatted correctly.
            (let* ((split-uri-list (split-string uri-list-data "[\0\r\n]" t))
                   (decoded (dnd-get-local-file-name (car split-uri-list))))
              (should (equal decoded normal-temp-file)))
            ;; Test that the username reported is correct.
            (should (equal username-data (user-real-login-name)))
            ;; Test that the file name data is correct.
            (let* ((split-file-names (split-string file-name-data "\0"))
                   (file-name (car split-file-names)))
              ;; Make sure there are no extra leading or trailing NULL bytes.
              (should (and split-file-names (null (cdr split-file-names))))
              ;; Make sure the file name is encoded correctly;
              (should-not (multibyte-string-p file-name))
              ;; Make sure decoding the file name results in the
              ;; originals.
              (should (equal (decode-coding-string file-name
                                                   (or file-name-coding-system
                                                       default-file-name-coding-system))
                             normal-temp-file))
              ;; Also make sure the hostname is correct.
              (should (equal host-name-data (system-name))))
            ;; Check that the netfile hostname, rpath and lpath are correct.
            (let ((parsed (dnd-tests-parse-tt-netfile netfile-data))
                  (filename (encode-coding-string normal-temp-file
                                                  (or file-name-coding-system
                                                      default-file-name-coding-system))))
              (should (equal (nth 0 parsed) (system-name)))
              (should (equal (nth 1 parsed) filename))
              (should (equal (nth 2 parsed) filename))))
          ;; And the remote file.
          (should (eq (dnd-begin-file-drag remote-temp-file) 'copy))
          ;; Test that the remote file was added to the list of files
          ;; to remove later.
          (should dnd-last-dragged-remote-file)
          ;; Make sure the appropriate hook is added so the remote
          ;; files are removed when Emacs exits.
          (should (memq #'dnd-remove-last-dragged-remote-file
                        kill-emacs-hook))
          ;; Test that the remote file was removed.
          (should (progn
                    (dnd-begin-file-drag normal-temp-file)
                    (not dnd-last-dragged-remote-file)))
          ;; Make sure the remote file removal hook was deleted.
          (should-not (memq #'dnd-remove-last-dragged-remote-file
                            kill-emacs-hook))
          ;; Test that links to remote files can't be created.
          (should-error (dnd-begin-file-drag remote-temp-file nil 'link))
          ;; Test dragging a file with a multibyte filename.
          (should (eq (dnd-begin-file-drag normal-multibyte-file) 'copy))
          ;; Test that the ToolTalk filename is encodes and decodes correctly.
          (let* ((netfile-data (cdr (dnd-tests-verify-selection-data '_DT_NETFILE)))
                 (parsed (dnd-tests-parse-tt-netfile netfile-data))
                 (filename (encode-coding-string normal-multibyte-file
                                                 (or file-name-coding-system
                                                     default-file-name-coding-system))))
            (should (equal (nth 0 parsed) (system-name)))
            (should (equal (nth 1 parsed) filename))
            (should (equal (nth 2 parsed) filename))))
      (delete-file normal-temp-file)
      (delete-file normal-multibyte-file)
      (delete-file remote-temp-file))))

(ert-deftest dnd-tests-begin-drag-files ()
  (skip-unless (and (dnd-tests-remote-accessible-p)
                    ;; TODO: make these tests work under X.
                    (not (eq window-system 'x))))
  (let ((normal-temp-file (expand-file-name (make-temp-name "dnd-test")
                                            temporary-file-directory))
        (normal-temp-file-1 (expand-file-name (make-temp-name "dnd-test")
                                              temporary-file-directory))
        (remote-temp-file (dnd-tests-make-temp-name))
        (nonexistent-local-file
         (expand-file-name (make-temp-name "dnd-test")
                           temporary-file-directory))
        (nonexistent-remote-file (dnd-tests-make-temp-name))
        (nonexistent-remote-file-1 (dnd-tests-make-temp-name))
        (x-dnd-preserve-selection-data t))
    ;; Touch those files if they don't exist.
    (unless (file-exists-p normal-temp-file)
      (write-region "" 0 normal-temp-file))
    (unless (file-exists-p normal-temp-file-1)
      (write-region "" 0 normal-temp-file))
    (unless (file-exists-p remote-temp-file)
      (write-region "" 0 remote-temp-file))
    (ignore-errors
      (delete-file nonexistent-local-file)
      (delete-file nonexistent-remote-file)
      (delete-file nonexistent-remote-file-1))
    (unwind-protect
        (progn
          ;; Now test dragging a normal file and a remote file.
          (should (eq (dnd-begin-drag-files (list normal-temp-file
                                                  remote-temp-file))
                      'copy))
          ;; Test that the remote file produced was added to the list
          ;; of files to remove upon the next call.
          (should dnd-last-dragged-remote-file)
          ;; Make sure the appropriate hook is added so the remote
          ;; files are removed when Emacs exits.
          (should (memq #'dnd-remove-last-dragged-remote-file
                        kill-emacs-hook))
          ;; Two local files at the same time.
          (should (eq (dnd-begin-drag-files (list normal-temp-file
                                                  normal-temp-file-1))
                      'copy))
          ;; Test that the remote files were removed.
          (should-not dnd-last-dragged-remote-file)
          ;; And so was the hook.
          (should-not (memq #'dnd-remove-last-dragged-remote-file
                            kill-emacs-hook))
          ;; Test the selection data is correct.
          (let ((uri-list-data (cdr (dnd-tests-verify-selection-data 'text/uri-list)))
                (username-data (dnd-tests-verify-selection-data 'text/x-xdnd-username))
                (file-name-data (cdr (dnd-tests-verify-selection-data 'FILE_NAME)))
                (host-name-data (cdr (dnd-tests-verify-selection-data 'HOST_NAME))))
            ;; Check if the URI list is formatted correctly.
            (let* ((split-uri-list (split-string uri-list-data "[\0\r\n]" t))
                   (decoded (mapcar #'dnd-get-local-file-name split-uri-list)))
              (should (equal (car decoded) normal-temp-file))
              (should (equal (cadr decoded) normal-temp-file-1)))
            ;; Test that the username reported is correct.
            (should (equal username-data (user-real-login-name)))
            ;; Test that the file name data is correct.
            (let ((split-file-names (split-string file-name-data "\0")))
              ;; Make sure there are no extra leading or trailing NULL bytes.
              (should (equal (length split-file-names) 2))
              ;; Make sure all file names are encoded correctly;
              (dolist (name split-file-names)
                (should-not (multibyte-string-p name)))
              ;; Make sure decoding the file names result in the
              ;; originals.
              (should (equal (decode-coding-string (car split-file-names)
                                                   (or file-name-coding-system
                                                       default-file-name-coding-system))
                             normal-temp-file))
              (should (equal (decode-coding-string (cadr split-file-names)
                                                   (or file-name-coding-system
                                                       default-file-name-coding-system))
                             normal-temp-file-1))
              ;; Also make sure the hostname is correct.
              (should (equal host-name-data (system-name)))))
          ;; Multiple local files with some remote files that will
          ;; fail, and some that won't.
          (should (and (eq (dnd-begin-drag-files (list normal-temp-file
                                                       remote-temp-file
                                                       remote-temp-file
                                                       nonexistent-remote-file
                                                       normal-temp-file-1
                                                       nonexistent-remote-file-1))
                           'copy)
                       ;; Make sure exactly two valid remote files
                       ;; were downloaded.
                       (eq (length dnd-last-dragged-remote-file) 2)))
          ;; Make sure the appropriate hook is added so the remote
          ;; files are removed when Emacs exits.
          (should (memq #'dnd-remove-last-dragged-remote-file
                        kill-emacs-hook))
          ;; Make sure links can't be created to remote files.
          (should-error (dnd-begin-drag-files (list normal-temp-file
                                                    remote-temp-file
                                                    normal-temp-file-1)
                                              nil 'link))
          ;; And that they can to normal files.
          (should (eq (dnd-begin-drag-files (list normal-temp-file
                                                  normal-temp-file-1)
                                            nil 'link)
                      'link))
          ;; Make sure the remote file removal hook was deleted.
          (should-not (memq #'dnd-remove-last-dragged-remote-file
                            kill-emacs-hook))
          ;; Make sure you can't drag an empty list of files.
          (should-error (dnd-begin-drag-files nil))
          ;; And when all remote files are inaccessible.
          (should-error (dnd-begin-drag-files (list nonexistent-remote-file
                                                    nonexistent-remote-file-1))))
      (delete-file normal-temp-file)
      (delete-file normal-temp-file-1)
      (delete-file remote-temp-file))))

(ert-deftest dnd-tests-get-local-file-uri ()
  ;; 'dnd-get-local-file-uri' always returns nil on MS-Windows
  (unless (eq system-type 'windows-nt)
    (should (equal (dnd-get-local-file-uri "file://localhost/path/to/foo")
                   "file:///path/to/foo"))
    (should (equal (dnd-get-local-file-uri
                    (format "file://%s/path/to/" (system-name)))
                   "file:///path/to/")))
  (should-not (dnd-get-local-file-uri "file://some-remote-host/path/to/foo"))
  (should-not (dnd-get-local-file-uri "file:///path/to/foo")))

(ert-deftest dnd-tests-open-remote-url ()
  ;; Expensive test to make sure opening an FTP URL during
  ;; drag-and-drop works.
  :tags '(:expensive-test)
  ;; Don't run if there is no ftp client.
  (skip-unless (executable-find "ftp"))
  ;; Don't run this test if the FTP server isn't reachable.
  (skip-unless (and (fboundp 'network-lookup-address-info)
                    (network-lookup-address-info "ftp.gnu.org")))
  ;; Make sure bug#56078 doesn't happen again.
  (let ((url "ftp://anonymous@ftp.gnu.org/")
        ;; This prints a bunch of annoying spaces to stdout.
        (inhibit-message t))
    (should (prog1 t (dnd-open-remote-url url 'private)))))

(ert-deftest dnd-tests-direct-save ()
  ;; This test just verifies that a direct save works; the window
  ;; system specific test is in x-dnd-tests.el.  When running this
  ;; interactively, keep in mind that there are only two file managers
  ;; which are known to implement XDS correctly: System G (see
  ;; https://nps-systemg.sourceforge.net), and Emacs itself.  GTK file
  ;; managers such as Nautilus will not work, since they prefer the
  ;; `text/uri-list' selection target to `XdndDirectSave0', contrary
  ;; to the XDS specification.
  (let ((window-system window-system)
        (normal-temp-file (expand-file-name (make-temp-name "dnd-test")
                                            temporary-file-directory)))
    (unwind-protect
        (progn
          (unless (file-exists-p normal-temp-file)
            (write-region "" 0 normal-temp-file))
          (unless (eq window-system 'x)
            ;; Use a window system that isn't X, since we only want to test
            ;; the fallback code when run non-interactively.
            (setq window-system 'haiku))
          (should (eq (dnd-direct-save normal-temp-file
                                       (make-temp-name "target-file-name"))
                      'copy)))
      (ignore-errors
        (delete-file normal-temp-file)))))



(defvar dnd-tests-list-1 '("file:///usr/openwin/include/pixrect/pr_impl.h"
                           "file:///usr/openwin/include/pixrect/pr_io.h")
  "Sample data for tests concerning the treatment of drag-and-drop URLs.")

(defvar dnd-tests-list-2 '("file:///usr/openwin/include/pixrect/pr_impl.h"
                           "file://remote/usr/openwin/include/pixrect/pr_io.h")
  "Sample data for tests concerning the treatment of drag-and-drop URLs.")

(defvar dnd-tests-list-3 (append dnd-tests-list-2 '("http://example.com"))
  "Sample data for tests concerning the treatment of drag-and-drop URLs.")

(defvar dnd-tests-list-4 (append dnd-tests-list-3 '("scheme1://foo.bar"
                                                    "scheme2://foo.bar"))
  "Sample data for tests concerning the treatment of drag-and-drop URLs.")

(defun dnd-tests-local-file-function (urls _action)
  "Signal an error if URLS doesn't match `dnd-tests-list-1'.
ACTION is ignored.  Return the symbol `copy' otherwise."
  (should (equal urls dnd-tests-list-1))
  'copy)

(put 'dnd-tests-local-file-function 'dnd-multiple-handler t)

(defun dnd-tests-remote-file-function (urls _action)
  "Signal an error if URLS doesn't match `dnd-tests-list-2'.
ACTION is ignored.  Return the symbol `copy' otherwise."
  (should (equal urls dnd-tests-list-2))
  'copy)

(put 'dnd-tests-remote-file-function 'dnd-multiple-handler t)

(defun dnd-tests-http-scheme-function (url _action)
  "Signal an error if URLS doesn't match `dnd-tests-list-3''s third element.
ACTION is ignored.  Return the symbol `private' otherwise."
  (should (equal url (car (last dnd-tests-list-3))))
  'private)

(defun dnd-tests-browse-url-handler (url &rest _ignored)
  "Verify URL is `dnd-tests-list-4''s fourth element."
  (should (equal url (nth 3 dnd-tests-list-4))))

(put 'dnd-tests-browse-url-handler 'browse-url-browser-kind 'internal)

(ert-deftest dnd-tests-receive-multiple-urls ()
  (let ((dnd-protocol-alist '(("^file:///" . dnd-tests-local-file-function)
                              ("^file:" . error)
                              ("^unrelated-scheme:" . error)))
        (browse-url-handlers nil))
    ;; Check that the order of the alist is respected when the
    ;; precedences of two handlers are equal.
    (should (equal (dnd-handle-multiple-urls (selected-window)
                                             (copy-sequence
                                              dnd-tests-list-1)
                                             'copy)
                   'copy))
    ;; Check that sorting handlers by precedence functions correctly.
    (setq dnd-protocol-alist '(("^file:///" . error)
                               ("^file:" . dnd-tests-remote-file-function)
                               ("^unrelated-scheme:" . error)))
    (should (equal (dnd-handle-multiple-urls (selected-window)
                                             (copy-sequence
                                              dnd-tests-list-2)
                                             'copy)
                   'copy))
    ;; Check that multiple handlers can be called at once, and actions
    ;; are properly "downgraded" to private when multiple handlers
    ;; return inconsistent values.
    (setq dnd-protocol-alist '(("^file:" . dnd-tests-remote-file-function)
                               ("^file:///" . error)
                               ("^http://" . dnd-tests-http-scheme-function)))
    (should (equal (dnd-handle-multiple-urls (selected-window)
                                             (copy-sequence
                                              dnd-tests-list-3)
                                             'copy)
                   'private))
    ;; Now verify that the function's documented fallback behavior
    ;; functions correctly.  Set browse-url-handlers to an association
    ;; list incorporating a test function, then guarantee that is
    ;; called.
    (setq browse-url-handlers '(("^scheme1://" . dnd-tests-browse-url-handler)))
    ;; Furthermore, guarantee the fifth argument of the test data is
    ;; inserted, for no apposite handler exists.
    (save-window-excursion
      (set-window-buffer nil (get-buffer-create " *dnd-tests*"))
      (set-buffer (get-buffer-create " *dnd-tests*"))
      (erase-buffer)
      (should (equal (dnd-handle-multiple-urls (selected-window)
                                               (copy-sequence
                                                dnd-tests-list-4)
                                               'copy)
                     'private))
      (should (equal (buffer-string) (nth 4 dnd-tests-list-4))))
    ;; Check that a handler enumerated twice in the handler list
    ;; receives URIs assigned to it only once.
    (let* ((received-p nil)
           (lambda (lambda (uri _action)
                     (should (equal uri "scheme1://test"))
                     (should (null received-p))
                     (setq received-p 'copy))))
      (setq dnd-protocol-alist (list (cons "scheme1://" lambda)
                                     (cons "scheme1://" lambda)))
      (should (equal (dnd-handle-multiple-urls (selected-window)
                                               (list "scheme1://test")
                                               'copy)
                     'copy)))))

(ert-deftest dnd-tests-default-file-name-handlers ()
  (let* ((local-files-opened nil)
         (remote-files-opened nil)
         (function-1 (lambda (file _uri)
                       (push file local-files-opened)
                       'copy))
         (function-2 (lambda (file _uri)
                       (push file remote-files-opened)
                       'copy)))
    (unwind-protect
        (progn
          (advice-add #'dnd-open-local-file :override
                      function-1)
          (advice-add #'dnd-open-file :override
                      function-2)
          ;; Guarantee that file names are properly categorized as either
          ;; local or remote by the default dnd-protocol-alist.
          (dnd-handle-multiple-urls
           (selected-window)
           (list
            ;; These are run-of-the-mill local file URIs.
            "file:///usr/include/sys/acct.h"
            "file:///usr/include/sys/acctctl.h"
            ;; These URIs incorporate a host; they should match
            ;; function-2 but never function-1.
            "file://remotehost/usr/src/emacs/configure.ac"
            "file://remotehost/usr/src/emacs/configure"
            ;; These URIs are generated by drag-and-drop event
            ;; handlers from local file names alone; they are not
            ;; echt URIs in and of themselves, but a product of our
            ;; drag and drop code.
            "file:/etc/vfstab"
            "file:/etc/dfs/sharetab"
            ;; These URIs are generated under MS-Windows.
            "file:c:/path/to/file/name"
            "file:d:/path/to/file/name")
           'copy)
          (should (equal (sort local-files-opened #'string<)
                         '("file:///usr/include/sys/acct.h"
                           "file:///usr/include/sys/acctctl.h"
                           "file:/etc/dfs/sharetab"
                           "file:/etc/vfstab"
                           "file:c:/path/to/file/name"
                           "file:d:/path/to/file/name")))
          (should (equal (sort remote-files-opened #'string<)
                         '("file://remotehost/usr/src/emacs/configure"
                           "file://remotehost/usr/src/emacs/configure.ac"))))
      (advice-remove #'dnd-open-local-file function-2))))

(provide 'dnd-tests)
;;; dnd-tests.el ends here
