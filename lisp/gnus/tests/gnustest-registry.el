;;; gnustest-registry.el --- Registry and Gnus registry testing for Gnus
;; Copyright (C) 2011-2012 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(eval-when-compile
  (when (null (ignore-errors (require 'ert)))
    (defmacro* ert-deftest (name () &body docstring-keys-and-body))))

(ignore-errors
  (require 'ert))

(require 'registry)
(require 'gnus-registry)

(ert-deftest gnustest-registry-instantiation-test ()
  (should (registry-db "Testing")))

(ert-deftest gnustest-registry-match-test ()
  (let ((entry '((hello "goodbye" "bye") (blank))))

    (message "Testing :regex matching")
    (should (registry--match :regex entry '((hello "nye" "bye"))))
    (should (registry--match :regex entry '((hello "good"))))
    (should-not (registry--match :regex entry '((hello "nye"))))
    (should-not (registry--match :regex entry '((hello))))

    (message "Testing :member matching")
    (should (registry--match :member entry '((hello "bye"))))
    (should (registry--match :member entry '((hello "goodbye"))))
    (should-not (registry--match :member entry '((hello "good"))))
    (should-not (registry--match :member entry '((hello "nye"))))
    (should-not (registry--match :member entry '((hello)))))
  (message "Done with matching testing."))

(defun gnustest-registry-make-testable-db (n &optional name file)
  (let* ((db (registry-db
              (or name "Testing")
              :file (or file "unused")
              :max-hard n
              :max-soft 0               ; keep nothing not precious
              :precious '(extra more-extra)
              :tracked '(sender subject groups))))
    (dotimes (i n)
      (registry-insert db i `((sender "me")
                              (subject "about you")
                              (more-extra) ; empty data key should be pruned
                              ;; first 5 entries will NOT have this extra data
                              ,@(when (< 5 i) (list (list 'extra "more data")))
                              (groups ,(number-to-string i)))))
    db))

(ert-deftest gnustest-registry-usage-test ()
  (let* ((n 100)
         (db (gnustest-registry-make-testable-db n)))
    (message "size %d" n)
    (should (= n (registry-size db)))
    (message "max-hard test")
    (should-error (registry-insert db "new" '()))
    (message "Individual lookup")
    (should (= 58 (caadr (registry-lookup db '(1 58 99)))))
    (message "Grouped individual lookup")
    (should (= 3 (length (registry-lookup db '(1 58 99)))))
    (when (boundp 'lexical-binding)
      (message "Individual lookup (breaks before lexbind)")
      (should (= 58
                 (caadr (registry-lookup-breaks-before-lexbind db '(1 58 99)))))
      (message "Grouped individual lookup (breaks before lexbind)")
      (should (= 3
                 (length (registry-lookup-breaks-before-lexbind db
                                                                '(1 58 99))))))
    (message "Search")
    (should (= n (length (registry-search db :all t))))
    (should (= n (length (registry-search db :member '((sender "me"))))))
    (message "Secondary index search")
    (should (= n (length (registry-lookup-secondary-value db 'sender "me"))))
    (should (equal '(74) (registry-lookup-secondary-value db 'groups "74")))
    (message "Delete")
    (should (registry-delete db '(1) t))
    (decf n)
    (message "Search after delete")
    (should (= n (length (registry-search db :all t))))
    (message "Secondary search after delete")
    (should (= n (length (registry-lookup-secondary-value db 'sender "me"))))
    ;; (message "Pruning")
    ;; (let* ((tokeep (registry-search db :member '((extra "more data"))))
    ;;        (count (- n (length tokeep)))
    ;;        (pruned (registry-prune db))
    ;;        (prune-count (length pruned)))
    ;;   (message "Expecting to prune %d entries and pruned %d"
    ;;            count prune-count)
    ;;   (should (and (= count 5)
    ;;                (= count prune-count))))
    (message "Done with usage testing.")))

(ert-deftest gnustest-registry-persistence-test ()
  (let* ((n 100)
         (tempfile (make-temp-file "registry-persistence-"))
         (name "persistence tester")
         (db (gnustest-registry-make-testable-db n name tempfile))
         size back)
    (message "Saving to %s" tempfile)
    (eieio-persistent-save db)
    (setq size (nth 7 (file-attributes tempfile)))
    (message "Saved to %s: size %d" tempfile size)
    (should (< 0 size))
    (with-temp-buffer
      (insert-file-contents-literally tempfile)
      (should (looking-at (concat ";; Object "
                                  name
                                  "\n;; EIEIO PERSISTENT OBJECT"))))
    (message "Reading object back")
    (setq back (eieio-persistent-read tempfile))
    (should back)
    (message "Read object back: %d keys, expected %d==%d"
             (registry-size back) n (registry-size db))
    (should (= (registry-size back) n))
    (should (= (registry-size back) (registry-size db)))
    (delete-file tempfile))
  (message "Done with persistence testing."))

(ert-deftest gnustest-gnus-registry-misc-test ()
  (should-error (gnus-registry-extract-addresses '("" "")))

  (should (equal '("Ted Zlatanov <tzz@lifelogs.com>"
                   "noname <ed@you.me>"
                   "noname <cyd@stupidchicken.com>"
                   "noname <tzz@lifelogs.com>")
                 (gnus-registry-extract-addresses
                  (concat "Ted Zlatanov <tzz@lifelogs.com>, "
                          "ed <ed@you.me>, " ; "ed" is not a valid name here
                          "cyd@stupidchicken.com, "
                          "tzz@lifelogs.com")))))

(ert-deftest gnustest-gnus-registry-usage-test ()
  (let* ((n 100)
         (tempfile (make-temp-file "gnus-registry-persist"))
         (db (gnus-registry-make-db tempfile))
         (gnus-registry-db db)
         back size)
    (message "Adding %d keys to the test Gnus registry" n)
    (dotimes (i n)
      (let ((id (number-to-string i)))
        (gnus-registry-handle-action id
                                     (if (>= 50 i) "fromgroup" nil)
                                     "togroup"
                                     (when (>= 70 i)
                                       (format "subject %d" (mod i 10)))
                                     (when (>= 80 i)
                                       (format "sender %d" (mod i 10))))))
    (message "Testing Gnus registry size is %d" n)
    (should (= n (registry-size db)))
    (message "Looking up individual keys (registry-lookup)")
    (should (equal (loop for e
                         in (mapcar 'cadr
                                    (registry-lookup db '("20" "83" "72")))
                         collect (assq 'subject e)
                         collect (assq 'sender e)
                         collect (assq 'group e))
                   '((subject "subject 0") (sender "sender 0") (group "togroup")
                     (subject) (sender) (group "togroup")
                     (subject) (sender "sender 2") (group "togroup"))))

    (message "Looking up individual keys (gnus-registry-id-key)")
    (should (equal (gnus-registry-get-id-key "34" 'group) '("togroup")))
    (should (equal (gnus-registry-get-id-key "34" 'subject) '("subject 4")))
    (message "Trying to insert a duplicate key")
    (should-error (gnus-registry-insert db "55" '()))
    (message "Looking up individual keys (gnus-registry-get-or-make-entry)")
    (should (gnus-registry-get-or-make-entry "22"))
    (message "Saving the Gnus registry to %s" tempfile)
    (should (gnus-registry-save tempfile db))
    (setq size (nth 7 (file-attributes tempfile)))
    (message "Saving the Gnus registry to %s: size %d" tempfile size)
    (should (< 0 size))
    (with-temp-buffer
      (insert-file-contents-literally tempfile)
      (should (looking-at (concat ";; Object "
                                  "Gnus Registry"
                                  "\n;; EIEIO PERSISTENT OBJECT"))))
    (message "Reading Gnus registry back")
    (setq back (eieio-persistent-read tempfile))
    (should back)
    (message "Read Gnus registry back: %d keys, expected %d==%d"
             (registry-size back) n (registry-size db))
    (should (= (registry-size back) n))
    (should (= (registry-size back) (registry-size db)))
    (delete-file tempfile)
    (message "Pruning Gnus registry to 0 by setting :max-soft")
    (oset db :max-soft 0)
    (registry-prune db)
    (should (= (registry-size db) 0)))
  (message "Done with Gnus registry usage testing."))

(provide 'gnustest-registry)
