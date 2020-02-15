;;; secrets-tests.el --- Tests of Secret Service API -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `https://www.gnu.org/licenses/'.

;;; Code:

(require 'ert)
(require 'secrets)
(require 'seq)
(require 'notifications)

;; We do not want chatty messages.
(setq secrets-debug nil)

(ert-deftest secrets-test00-availability ()
  "Test availability of Secret Service API."
  :expected-result (if secrets-enabled :passed :failed)
  (should secrets-enabled)
  (should (dbus-ping :session secrets-service))

  ;; Exit.
  (secrets--test-close-all-sessions))

(defun secrets--test-get-all-sessions ()
  "Return all object paths for existing secrets sessions."
  (let ((session-path (concat secrets-path "/session")))
    (delete
     session-path
     (dbus-introspect-get-all-nodes :session secrets-service session-path))))

(defun secrets--test-close-all-sessions ()
  "Close all secrets sessions which are bound to this Emacs."
  (secrets-close-session)
  ;; We loop over all other sessions.  If a session does not belong to
  ;; us, a `dbus-error' is fired, which we ignore.
  (dolist (path (secrets--test-get-all-sessions))
    (dbus-ignore-errors
      (dbus-call-method
       :session secrets-service path secrets-interface-session "Close"))))

(defun secrets--test-delete-all-session-items ()
  "Delete all items of collection \"session\" bound to this Emacs."
  (dolist (item (secrets-list-items "session"))
    (secrets-delete-item "session" item)))

(ert-deftest secrets-test01-sessions ()
  "Test opening / closing a secrets session."
  (skip-unless secrets-enabled)
  (skip-unless (secrets-empty-path secrets-session-path))

  (unwind-protect
      (progn
	;; Simple opening / closing of a session.
	(should (secrets-open-session))
	(should-not (secrets-empty-path secrets-session-path))
	(should (secrets-close-session))
	(should (secrets-empty-path secrets-session-path))

	;; Reopening a new session.
	(should (string-equal (secrets-open-session) (secrets-open-session)))
	(should (string-equal secrets-session-path (secrets-open-session)))
	(should-not
	 (string-equal (secrets-open-session) (secrets-open-session 'reopen)))
	(should-not
	 (string-equal secrets-session-path (secrets-open-session 'reopen))))

    ;; Exit.
    (should (secrets-close-session))
    (secrets--test-close-all-sessions)))

(ert-deftest secrets-test02-collections ()
  "Test creation / deletion a secrets collections."
  (skip-unless secrets-enabled)
  (skip-unless (secrets-empty-path secrets-session-path))

  (unwind-protect
      (progn
	(should (secrets-open-session))
	(should (member "session" (secrets-list-collections)))

	;; Create a random collection.  This asks for a password
	;; outside our control, so we make it in the interactive case
	;; only.
	(unless noninteractive
	  (let ((collection (md5 (concat (prin1-to-string process-environment)
					 (current-time-string))))
		(alias (secrets-get-alias "default")))
	    (notifications-notify
	     :title (symbol-name (ert-test-name (ert-running-test)))
	     :body "Please enter the password \"secret\" twice")
	    ;; The optional argument ALIAS does not seem to work.
	    (should (secrets-create-collection collection))
	    (should (member collection (secrets-list-collections)))

	    ;; We reset the alias.  The temporary collection "session"
	    ;; is not accepted.
	    (secrets-set-alias collection "default")
	    (should (string-equal (secrets-get-alias "default") collection))

	    ;; Delete alias.
	    (secrets-delete-alias "default")
	    (should-not (secrets-get-alias "default"))

	    ;; Lock / unlock the collection.
	    (secrets-lock-collection collection)
	    (should
	     (secrets-get-collection-property
	      (secrets-collection-path collection) "Locked"))
	    (notifications-notify
	     :title (symbol-name (ert-test-name (ert-running-test)))
	     :body "Please enter the password \"secret\"")
	    (secrets-unlock-collection collection)
	    (should-not
	     (secrets-get-collection-property
	      (secrets-collection-path collection) "Locked"))

	    ;; Delete the collection.  The alias disappears as well.
	    (secrets-set-alias collection "default")
	    (secrets-delete-collection collection)
	    (should-not (secrets-get-alias "default"))

	    ;; Reset alias.
	    (when alias
	      (secrets-set-alias alias "default")
	      (should (string-equal (secrets-get-alias "default") alias))))))

    ;; Exit.
    (should (secrets-close-session))
    (secrets--test-close-all-sessions)))

(ert-deftest secrets-test03-items ()
  "Test creation / deletion a secret item."
  (skip-unless secrets-enabled)
  (skip-unless (secrets-empty-path secrets-session-path))

  (unwind-protect
      (let (item-path)
	(should (secrets-open-session))

        ;; Cleanup.  There could be items in the "session" collection.
        (secrets--test-delete-all-session-items)

	;; There shall be no items in the "session" collection.
	(should-not (secrets-list-items "session"))

	;; Create a new item.
	(should (setq item-path (secrets-create-item "session" "foo" "secret")))
        (dolist (item `("foo" ,item-path))
	  (should (string-equal (secrets-get-secret "session" item) "secret")))

	;; Create another item with same label.
	(should (secrets-create-item "session" "foo" "geheim"))
	(should (equal (secrets-list-items "session") '("foo" "foo")))

	;; Create an item with attributes.
	(should
         (setq item-path
               (secrets-create-item
	        "session" "bar" "secret"
	        :method "sudo" :user "joe" :host "remote-host")))
        (dolist (item `("bar" ,item-path))
	  (should
	   (string-equal (secrets-get-attribute "session" item :method) "sudo"))
          ;; The attribute :xdg:schema is added silently.
	  (should
           (seq-set-equal-p
	    (secrets-get-attributes "session" item)
	    '((:xdg:schema . "org.freedesktop.Secret.Generic")
              (:host . "remote-host") (:user . "joe") (:method . "sudo")))))

	;; Create an item with another schema.
	(should
         (setq item-path
               (secrets-create-item
                "session" "baz" "secret" :xdg:schema "org.gnu.Emacs.foo")))
        (dolist (item `("baz" ,item-path))
	  (should
	   (equal
	    (secrets-get-attributes "session" item)
	    '((:xdg:schema . "org.gnu.Emacs.foo")))))

	;; Delete them.
	(dolist (item (secrets-list-items "session"))
	  (secrets-delete-item "session" item))
	(should-not (secrets-list-items "session")))

    ;; Exit.
    (secrets--test-delete-all-session-items)
    (should (secrets-close-session))
    (secrets--test-close-all-sessions)))

(ert-deftest secrets-test04-search ()
  "Test searching of secret items."
  (skip-unless secrets-enabled)
  (skip-unless (secrets-empty-path secrets-session-path))

  (unwind-protect
      (progn
	(should (secrets-open-session))

        ;; Cleanup.  There could be items in the "session" collection.
        (secrets--test-delete-all-session-items)

	;; There shall be no items in the "session" collection.
	(should-not (secrets-list-items "session"))

	;; Create some items.
	(should
         (secrets-create-item
	  "session" "foo" "secret"
	  :method "sudo" :user "joe" :host "remote-host"))
	(should
         (secrets-create-item
	  "session" "bar" "secret"
	  :method "sudo" :user "smith" :host "remote-host"))
	(should
         (secrets-create-item
	  "session" "baz" "secret"
	  :method "ssh" :user "joe" :host "other-host"))

	;; Search the items.  `secrets-search-items' uses
	;; `secrets-search-item-paths' internally, it is sufficient to
	;; test only one of them.
	(should-not (secrets-search-item-paths "session" :user "john"))
	(should-not (secrets-search-items "session" :user "john"))
	(should-not
         (secrets-search-items "session" :xdg:schema "org.gnu.Emacs.foo"))
	(should
	 (equal
          (sort (secrets-search-items "session" :user "joe") #'string-lessp)
	  '("baz" "foo")))
	(should
	 (equal
	  (secrets-search-items "session":method "sudo" :user "joe") '("foo")))
	(should
	 (equal
          (sort (secrets-search-items "session") #'string-lessp)
	  '("bar" "baz" "foo"))))

    ;; Exit.
    (secrets--test-delete-all-session-items)
    (should (secrets-close-session))
    (secrets--test-close-all-sessions)))

(defun secrets-test-all (&optional interactive)
  "Run all tests for \\[secrets]."
  (interactive "p")
  (funcall
   (if interactive #'ert-run-tests-interactively #'ert-run-tests-batch)
   "^secrets"))

(provide 'secrets-tests)
;;; secrets-tests.el ends here
