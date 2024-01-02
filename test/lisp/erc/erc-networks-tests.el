;;; erc-networks-tests.el --- Tests for erc-networks.  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert-x) ; cl-lib
(require 'erc)

(defun erc-networks-tests--create-dead-proc (&optional buf)
  (let ((p (start-process "true" (or buf (current-buffer)) "true")))
    (while (process-live-p p) (sit-for 0.1))
    p))

(defun erc-networks-tests--create-live-proc (&optional buf)
  (let ((proc (start-process "sleep" (or buf (current-buffer)) "sleep" "1")))
    (set-process-query-on-exit-flag proc nil)
    proc))

;; When we drop 27, call `get-buffer-create with INHIBIT-BUFFER-HOOKS.
(defun erc-networks-tests--clean-bufs ()
  (let (erc-kill-channel-hook
        erc-kill-server-hook
        erc-kill-buffer-hook)
    (dolist (buf (erc-buffer-list))
      (kill-buffer buf))))

(defun erc-networks-tests--bufnames (prefix)
  (let* ((case-fold-search)
         (pred (lambda (b) (string-prefix-p prefix (buffer-name b))))
         (prefixed (seq-filter pred (buffer-list))))
    (sort (mapcar #'buffer-name prefixed) #'string<)))

(ert-deftest erc-networks--id ()
  (cl-letf (((symbol-function 'float-time)
             (lambda (&optional _) 0.0)))

    ;; Fixed
    (should (equal (erc-networks--id-fixed-create 'foo)
                   (make-erc-networks--id-fixed :ts (float-time)
                                                :symbol 'foo)))

    ;; Eliding
    (let* ((erc-network 'FooNet)
           (erc-server-current-nick "Joe")
           (identity (erc-networks--id-create nil)))

      (should (equal identity #s(erc-networks--id-qualifying
                                 0.0 FooNet [FooNet "joe"] 1)))
      (should (equal (erc-networks--id-qualifying-grow-id identity)
                     'FooNet/joe))
      (should (equal identity #s(erc-networks--id-qualifying
                                 0.0 FooNet/joe [FooNet "joe"] 2)))
      (should-not (erc-networks--id-qualifying-grow-id identity))
      (should (equal identity #s(erc-networks--id-qualifying
                                 0.0 FooNet/joe [FooNet "joe"] 2))))

    ;; Compat
    (with-current-buffer (get-buffer-create "fake.chat")
      (with-suppressed-warnings ((obsolete erc-rename-buffers))
        (let (erc-rename-buffers)
          (should (equal (erc-networks--id-create nil)
                         (make-erc-networks--id-fixed :ts (float-time)
                                                      :symbol 'fake.chat)))))
      (kill-buffer))))

(ert-deftest erc-networks--id-create ()
  (cl-letf (((symbol-function 'float-time)
             (lambda (&optional _) 0.0)))

    (should (equal (erc-networks--id-create 'foo)
                   (make-erc-networks--id-fixed :ts (float-time)
                                                :symbol 'foo)))
    (should (equal (erc-networks--id-create "foo")
                   (make-erc-networks--id-fixed :ts (float-time)
                                                :symbol 'foo)))
    (should (equal (erc-networks--id-create [h i])
                   (make-erc-networks--id-fixed :ts (float-time)
                                                :symbol (quote \[h\ \i\]))))

    (with-current-buffer (get-buffer-create "foo")
      (let ((expected (make-erc-networks--id-fixed :ts (float-time)
                                                   :symbol 'foo)))
        (with-suppressed-warnings ((obsolete erc-rename-buffers))
          (let (erc-rename-buffers)
            (should (equal (erc-networks--id-create nil) expected))))
        (with-suppressed-warnings ((obsolete erc-reuse-buffers))
          (let (erc-reuse-buffers)
            (should (equal (erc-networks--id-create nil) expected))
            (should (equal (erc-networks--id-create 'bar) expected)))))
      (kill-buffer))))

(ert-deftest erc-networks--id-qualifying-prefix-length ()
  (should-not (erc-networks--id-qualifying-prefix-length
               (make-erc-networks--id-qualifying)
               (make-erc-networks--id-qualifying)))

  (should-not (erc-networks--id-qualifying-prefix-length
               (make-erc-networks--id-qualifying :parts [1 2])
               (make-erc-networks--id-qualifying :parts [2 3])))

  (should (= 1 (erc-networks--id-qualifying-prefix-length
                (make-erc-networks--id-qualifying :parts [1])
                (make-erc-networks--id-qualifying :parts [1 2]))))

  (should (= 1 (erc-networks--id-qualifying-prefix-length
                (make-erc-networks--id-qualifying :parts [1 2])
                (make-erc-networks--id-qualifying :parts [1 3]))))

  (should (= 2 (erc-networks--id-qualifying-prefix-length
                (make-erc-networks--id-qualifying :parts [1 2])
                (make-erc-networks--id-qualifying :parts [1 2]))))

  (should (= 1 (erc-networks--id-qualifying-prefix-length
                (make-erc-networks--id-qualifying :parts ["1"])
                (make-erc-networks--id-qualifying :parts ["1"])))))

(ert-deftest erc-networks--id-sort-buffers ()
  (let (oldest middle newest)

    (with-temp-buffer
      (setq erc-networks--id (erc-networks--id-fixed-create 'oldest)
            oldest (current-buffer))

      (with-temp-buffer
        (setq erc-networks--id (erc-networks--id-fixed-create 'middle)
              middle (current-buffer))

        (with-temp-buffer
          (setq erc-networks--id (erc-networks--id-fixed-create 'newest)
                newest (current-buffer))

          (should (equal (erc-networks--id-sort-buffers
                          (list oldest newest middle))
                         (list newest middle oldest))))))))

(ert-deftest erc-networks-rename-surviving-target-buffer--channel ()
  (should (memq #'erc-networks-rename-surviving-target-buffer
                erc-kill-channel-hook))

  (let ((chan-foonet-buffer (get-buffer-create "#chan@foonet")))

    (with-current-buffer chan-foonet-buffer
      (erc-mode)
      (setq erc-networks--id (make-erc-networks--id-qualifying
                              :parts [foonet "bob"] :len 1)
            erc--target (erc--target-from-string "#chan")))

    (with-current-buffer (get-buffer-create "#chan@barnet")
      (erc-mode)
      (setq erc-networks--id (make-erc-networks--id-qualifying
                              :parts [barnet "bob"] :len 1)
            erc--target (erc--target-from-string "#chan")))

    (kill-buffer "#chan@barnet")
    (should (equal (erc-networks-tests--bufnames "#chan") '("#chan")))
    (should (eq chan-foonet-buffer (get-buffer "#chan"))))

  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks-rename-surviving-target-buffer--query ()
  (should (memq #'erc-networks-rename-surviving-target-buffer
                erc-kill-buffer-hook))

  (let ((bob-foonet (get-buffer-create "bob@foonet")))

    (with-current-buffer bob-foonet
      (erc-mode)
      (setq erc-networks--id (make-erc-networks--id-qualifying
                              :parts [foonet "bob"] :len 1)
            erc--target (erc--target-from-string "bob")))

    (with-current-buffer (get-buffer-create "bob@barnet")
      (erc-mode)
      (setq erc-networks--id (make-erc-networks--id-qualifying
                              :parts [barnet "bob"] :len 1)
            erc--target (erc--target-from-string "bob")))

    (kill-buffer "bob@barnet")
    (should (equal (erc-networks-tests--bufnames "bob") '("bob")))
    (should (eq bob-foonet (get-buffer "bob"))))

  (erc-networks-tests--clean-bufs))

;; A non-ERC buffer exists named "bob", and we're killing one of two
;; ERC target buffers named "bob@<netid>".  The surviving buffer
;; retains its suffix.

(ert-deftest erc-networks-rename-surviving-target-buffer--query-non-target ()
  (should (memq #'erc-networks-rename-surviving-target-buffer
                erc-kill-buffer-hook))

  (let ((existing (get-buffer-create "bob"))
        (bob-foonet (get-buffer-create "bob@foonet")))

    (with-current-buffer bob-foonet
      (erc-mode)
      (setq erc-networks--id (make-erc-networks--id-qualifying
                              :parts [foonet "bob"] :len 1)
            erc--target (erc--target-from-string "bob")))

    (with-current-buffer (get-buffer-create "bob@barnet")
      (erc-mode)
      (setq erc-networks--id (make-erc-networks--id-qualifying
                              :parts [barnet "bob"] :len 1)
            erc--target (erc--target-from-string "bob")))

    (kill-buffer "bob@barnet")
    (should (buffer-live-p existing))
    (should (buffer-live-p bob-foonet))
    (kill-buffer existing))

  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks-rename-surviving-target-buffer--multi ()

  (ert-info ("Multiple leftover channels untouched")
    (with-current-buffer (get-buffer-create "#chan@foonet")
      (erc-mode)
      (setq erc--target (erc--target-from-string "#chan")))
    (with-current-buffer (get-buffer-create "#chan@barnet")
      (erc-mode)
      (setq erc--target (erc--target-from-string "#chan")))
    (with-current-buffer (get-buffer-create "#chan@baznet")
      (erc-mode)
      (setq erc--target (erc--target-from-string "#chan")))
    (kill-buffer "#chan@baznet")
    (should (equal (erc-networks-tests--bufnames "#chan")
                   '("#chan@barnet" "#chan@foonet")))
    (erc-networks-tests--clean-bufs))

  (ert-info ("Multiple leftover queries untouched")
    (with-current-buffer (get-buffer-create "bob@foonet")
      (erc-mode)
      (setq erc--target (erc--target-from-string "bob")))
    (with-current-buffer (get-buffer-create "bob@barnet")
      (erc-mode)
      (setq erc--target (erc--target-from-string "bob")))
    (with-current-buffer (get-buffer-create "bob@baznet")
      (erc-mode)
      (setq erc--target (erc--target-from-string "bob")))
    (kill-buffer "bob@baznet")
    (should (equal (erc-networks-tests--bufnames "bob")
                   '("bob@barnet" "bob@foonet")))
    (erc-networks-tests--clean-bufs)))

;; As of May 2022, this "shrink" stuff runs whenever an ERC buffer is
;; killed because `erc-networks-shrink-ids-and-buffer-names' is a
;; default member of all three erc-kill-* functions.

;; Note: this overlaps a fair bit with the "hook" variants, i.e.,
;; `erc-networks--shrink-ids-and-buffer-names--hook-outstanding-*' If
;; this ever fails, just delete this and fix those.  But please copy
;; over and adapt the comments first.

(ert-deftest erc-networks--shrink-ids-and-buffer-names--perform-outstanding ()
  ;; While some buffer #a@barnet/dummy is being killed, its display ID
  ;; is not collapsed because collisions still exist.
  ;;
  ;; Note that we don't have to set `erc-server-connected' because
  ;; this function is intentionally connectivity agnostic.
  (with-current-buffer (get-buffer-create "foonet/tester")
    (erc-mode)
    (setq erc-server-current-nick "tester" ; Always set (`erc-open')
          ;; Set when transport connected
          erc-server-process (erc-networks-tests--create-live-proc)
          ;; Both set just before IRC (logically) connected (post MOTD)
          erc-network 'foonet
          erc-networks--id (make-erc-networks--id-qualifying
                            :symbol 'foonet/tester
                            :parts [foonet "tester"]
                            :len 2))) ; is/was a plain foonet collision

  ;; Presumably, some server buffer named foonet/dummy was just
  ;; killed, hence the length 2 display ID.

  ;; A target buffer for chan #a exists for foonet/tester.  The
  ;; precise form of its name should not affect shrinking.
  (with-current-buffer (get-buffer-create
                        (elt ["#a" "#a@foonet" "#a@foonet/tester"] (random 3)))
    (erc-mode)
    (setq erc-server-process (buffer-local-value 'erc-server-process
                                                 (get-buffer "foonet/tester"))
          erc-network 'foonet
          erc-server-current-nick "tester"
          erc-networks--id (buffer-local-value 'erc-networks--id
                                               (get-buffer "foonet/tester"))
          erc--target (erc--target-from-string "#a")))

  ;; Another network context exists (so we have buffers to iterate
  ;; over), and it's also part of a collision group.
  (with-current-buffer (get-buffer-create "barnet/tester")
    (erc-mode)
    (setq erc-network 'barnet
          erc-server-current-nick "tester"
          erc-networks--id (make-erc-networks--id-qualifying
                            :symbol 'barnet/tester
                            :parts [barnet "tester"]
                            :len 2)
          erc-server-process (erc-networks-tests--create-live-proc)))

  (with-current-buffer (get-buffer-create "barnet/dummy")
    (erc-mode)
    (setq erc-network 'barnet
          erc-server-current-nick "dummy"
          erc-networks--id (make-erc-networks--id-qualifying
                            :symbol 'barnet/dummy
                            :parts [barnet "dummy"]
                            :len 2)
          erc-server-process (erc-networks-tests--create-live-proc)))

  ;; The buffer being killed is not part of the foonet collision
  ;; group, which contains one display ID eligible for shrinkage.
  (with-current-buffer (get-buffer-create
                        (elt ["#a@barnet" "#a@barnet/tester"] (random 2)))
    (erc-mode)
    (setq erc-network 'barnet
          erc-server-current-nick "tester"
          erc-server-process (buffer-local-value 'erc-server-process
                                                 (get-buffer "barnet/tester"))
          erc-networks--id (buffer-local-value 'erc-networks--id
                                               (get-buffer "barnet/tester"))
          erc--target (erc--target-from-string "#a")))

  (with-temp-buffer ; doesn't matter what the current buffer is
    (setq erc-networks--id (make-erc-networks--id-qualifying)) ; mock
    (erc-networks--shrink-ids-and-buffer-names))

  (should (equal (mapcar #'buffer-name (erc-buffer-list))
                 '("foonet" ; shrunk
                   "#a@foonet" ; shrunk
                   "barnet/tester"
                   "barnet/dummy"
                   "#a@barnet/tester")))

  (erc-networks-tests--clean-bufs))

;; This likewise overlaps with the "hook" variants below.  If this
;; should ever fail, just delete it and optionally fix those.

(ert-deftest erc-networks--shrink-ids-and-buffer-names--perform-collapse ()
  ;; This is similar to the "outstanding" variant above, but both
  ;; groups are eligible for renaming, which is abnormal but possible
  ;; when recovering from some mishap.
  (with-current-buffer (get-buffer-create "foonet/tester")
    (erc-mode)
    (setq erc-network 'foonet
          erc-server-current-nick "tester"
          erc-networks--id (make-erc-networks--id-qualifying
                            :symbol 'foonet/tester
                            :parts [foonet "tester"]
                            :len 2)
          erc-server-process (erc-networks-tests--create-live-proc)))

  (with-current-buffer
      (get-buffer-create (elt ["#a" "#a@foonet/tester"] (random 2)))
    (erc-mode)
    (setq erc-server-process (buffer-local-value 'erc-server-process
                                                 (get-buffer "foonet/tester"))
          erc-network 'foonet
          erc-server-current-nick "tester"
          erc-networks--id (buffer-local-value 'erc-networks--id
                                               (get-buffer "foonet/tester"))
          erc--target (erc--target-from-string "#a")))

  (with-current-buffer (get-buffer-create "barnet/tester")
    (erc-mode)
    (setq erc-network 'barnet
          erc-server-current-nick "tester"
          erc-networks--id (make-erc-networks--id-qualifying
                            :symbol 'barnet/tester
                            :parts [barnet "tester"]
                            :len 2)
          erc-server-process (erc-networks-tests--create-live-proc)))

  (with-current-buffer
      (get-buffer-create (elt ["#b" "#b@foonet/tester"] (random 2)))
    (erc-mode)
    (setq erc-network 'barnet
          erc-server-current-nick "tester"
          erc-server-process (buffer-local-value 'erc-server-process
                                                 (get-buffer "barnet/tester"))
          erc-networks--id (buffer-local-value 'erc-networks--id
                                               (get-buffer "barnet/tester"))
          erc--target (erc--target-from-string "#b")))

  (with-temp-buffer
    (setq erc-networks--id (make-erc-networks--id-qualifying))
    (erc-networks--shrink-ids-and-buffer-names))

  (should (equal (mapcar #'buffer-name (erc-buffer-list))
                 '("foonet" "#a" "barnet" "#b")))

  (erc-networks-tests--clean-bufs))

(defun erc-networks--shrink-ids-and-buffer-names--hook-outstanding-common ()

  (with-current-buffer (get-buffer-create "foonet/tester")
    (erc-mode)
    (setq erc-network 'foonet
          erc-server-current-nick "tester"
          erc-networks--id (make-erc-networks--id-qualifying
                            :symbol 'foonet/tester
                            :parts [foonet "tester"]
                            :len 2)
          erc-server-process (erc-networks-tests--create-live-proc)))

  (with-current-buffer (get-buffer-create "#a@foonet/tester")
    (erc-mode)
    (setq erc-server-process (buffer-local-value 'erc-server-process
                                                 (get-buffer "foonet/tester"))
          erc-network 'foonet
          erc-server-current-nick "tester"
          erc-networks--id (buffer-local-value 'erc-networks--id
                                               (get-buffer "foonet/tester"))
          erc--target (erc--target-from-string "#a")))

  (with-current-buffer (get-buffer-create "barnet/tester")
    (erc-mode)
    (setq erc-network 'barnet
          erc-server-current-nick "tester"
          erc-networks--id (make-erc-networks--id-qualifying
                            :symbol 'barnet/tester
                            :parts [barnet "tester"]
                            :len 2)
          erc-server-process (erc-networks-tests--create-live-proc)))

  (with-current-buffer (get-buffer-create "barnet/dummy")
    (erc-mode)
    (setq erc-network 'barnet
          erc-server-current-nick "dummy"
          erc-networks--id (make-erc-networks--id-qualifying
                            :symbol 'barnet/dummy
                            :parts [barnet "dummy"]
                            :len 2)
          erc-server-process (erc-networks-tests--create-live-proc)))

  (with-current-buffer (get-buffer-create "#a@barnet/tester")
    (erc-mode)
    (setq erc-network 'barnet
          erc-server-current-nick "tester"
          erc-server-process (buffer-local-value 'erc-server-process
                                                 (get-buffer "barnet/tester"))
          erc-networks--id (buffer-local-value 'erc-networks--id
                                               (get-buffer "barnet/tester"))
          erc--target (erc--target-from-string "#a"))))

(ert-deftest erc-networks--shrink-ids-and-buffer-names--hook-outstanding-srv ()
  (erc-networks--shrink-ids-and-buffer-names--hook-outstanding-common)
  (with-current-buffer (get-buffer-create "foonet/dummy")
    (erc-mode)
    (setq erc-network 'foonet
          erc-server-current-nick "dummy"
          erc-networks--id (make-erc-networks--id-qualifying
                            :symbol 'foonet/dummy
                            :parts [foonet "dummy"]
                            :len 2)
          erc-server-process (erc-networks-tests--create-live-proc))
    (kill-buffer))

  (should (equal (mapcar #'buffer-name (erc-buffer-list))
                 '("foonet"
                   "#a@foonet"
                   "barnet/tester"
                   "barnet/dummy"
                   "#a@barnet/tester")))
  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--shrink-ids-and-buffer-names--hook-outstanding-tgt ()
  (erc-networks--shrink-ids-and-buffer-names--hook-outstanding-common)
  (with-current-buffer (get-buffer-create "#a@foonet/dummy")
    (erc-mode)
    (setq erc-network 'foonet
          erc-server-current-nick "dummy"
          erc-networks--id (make-erc-networks--id-qualifying
                            :symbol 'foonet/dummy
                            :parts [foonet "dummy"]
                            :len 2)
          erc--target (erc--target-from-string "#a")
          erc-server-process (with-temp-buffer
                               (erc-networks-tests--create-dead-proc))))

  (with-current-buffer "#a@foonet/dummy" (kill-buffer))

  ;; Identical to *-server variant above
  (should (equal (mapcar #'buffer-name (erc-buffer-list))
                 '("foonet"
                   "#a@foonet"
                   "barnet/tester"
                   "barnet/dummy"
                   "#a@barnet/tester")))
  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks-rename-surviving-target-buffer--shrink ()
  (erc-networks--shrink-ids-and-buffer-names--hook-outstanding-common)

  ;; This buffer isn't "#a@foonet" (yet) because the shrink-ids hook
  ;; hasn't run.  However, when it's the rename hook runs, its network
  ;; id *is* "foonet", not "foonet/tester".
  (with-current-buffer "#a@foonet/tester" (kill-buffer))

  (should (equal (mapcar #'buffer-name (erc-buffer-list))
                 '("foonet"
                   "barnet/tester"
                   "barnet/dummy"
                   "#a")))

  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--shrink-ids-and-buffer-names--server ()

  (with-current-buffer (get-buffer-create "foonet/tester")
    (erc-mode)
    (setq erc-network 'foonet
          erc-server-current-nick "tester"
          erc-networks--id (make-erc-networks--id-qualifying
                            :symbol 'foonet/tester
                            :parts [foonet "tester"]
                            :len 2)
          erc-server-process (erc-networks-tests--create-live-proc)))

  (with-current-buffer (get-buffer-create "foonet/dummy")
    (erc-mode)
    (setq erc-network 'foonet
          erc-server-current-nick "dummy"
          erc-networks--id (make-erc-networks--id-qualifying
                            :symbol 'foonet/dummy
                            :parts [foonet "dummy"]
                            :len 2)
          erc-server-process (erc-networks-tests--create-dead-proc))
    (kill-buffer))

  (should (equal (mapcar #'buffer-name (erc-buffer-list)) '("foonet")))

  (erc-networks-tests--clean-bufs))

(defun erc-networks--shrink-ids-and-buffer-names--hook-collapse (check)

  (with-current-buffer (get-buffer-create "foonet/tester")
    (erc-mode)
    (setq erc-network 'foonet
          erc-server-current-nick "tester"
          erc-networks--id (make-erc-networks--id-qualifying
                            :symbol 'foonet/tester
                            :parts [foonet "tester"]
                            :len 2)
          erc-server-process (erc-networks-tests--create-live-proc)))

  (with-current-buffer (get-buffer-create "#a@foonet/tester")
    (erc-mode)
    (setq erc-server-process (buffer-local-value 'erc-server-process
                                                 (get-buffer "foonet/tester"))
          erc-network 'foonet
          erc-server-current-nick "tester"
          erc-networks--id (buffer-local-value 'erc-networks--id
                                               (get-buffer "foonet/tester"))
          erc--target (erc--target-from-string "#a")))

  (with-current-buffer (get-buffer-create "barnet/tester")
    (erc-mode)
    (setq erc-network 'barnet
          erc-server-current-nick "tester"
          erc-networks--id (make-erc-networks--id-qualifying
                            :symbol 'barnet/tester
                            :parts [barnet "tester"]
                            :len 2)
          erc-server-process (erc-networks-tests--create-live-proc)))

  (with-current-buffer (get-buffer-create  "#b@foonet/tester")
    (erc-mode)
    (setq erc-network 'barnet
          erc-server-current-nick "tester"
          erc-server-process (buffer-local-value 'erc-server-process
                                                 (get-buffer "barnet/tester"))
          erc-networks--id (buffer-local-value 'erc-networks--id
                                               (get-buffer "barnet/tester"))
          erc--target (erc--target-from-string "#b")))

  (funcall check)

  (should (equal (mapcar #'buffer-name (erc-buffer-list))
                 '("foonet" "#a" "barnet" "#b")))

  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--shrink-ids-and-buffer-names--hook-collapse-server ()
  (erc-networks--shrink-ids-and-buffer-names--hook-collapse
   (lambda ()
     (with-current-buffer (get-buffer-create "foonet/dummy")
       (erc-mode)
       (setq erc-network 'foonet
             erc-server-current-nick "dummy"
             erc-networks--id (make-erc-networks--id-qualifying
                               :symbol 'foonet/dummy
                               :parts [foonet "dummy"]
                               :len 2)
             erc-server-process (erc-networks-tests--create-live-proc))
       (kill-buffer)))))

(ert-deftest erc-networks--shrink-ids-and-buffer-names--hook-collapse-target ()
  (erc-networks--shrink-ids-and-buffer-names--hook-collapse
   (lambda ()
     (with-current-buffer (get-buffer-create "#a@foonet/dummy")
       (erc-mode)
       (setq erc-network 'foonet
             erc-server-current-nick "dummy"
             erc-networks--id (make-erc-networks--id-qualifying
                               :symbol 'foonet/dummy
                               :parts [foonet "dummy"]
                               :len 2)
             ;; `erc-kill-buffer-function' uses legacy target detection
             ;; but falls back on buffer name, so no need for:
             ;;
             ;;   erc-default-recipients '("#a")
             ;;
             erc--target (erc--target-from-string "#a")
             erc-server-process (with-temp-buffer
                                  (erc-networks-tests--create-dead-proc)))
       (kill-buffer)))))

;; FIXME this test is old and may describe impossible states:
;; leftover identities being qual-equal but not eq (implies
;; `erc-networks--reclaim-orphaned-target-buffers' is somehow broken).
;;
;; Otherwise, the point of this test is to show that server process
;; identity does not impact the hunt for duplicates.

(defun erc-tests--prep-erc-networks--reconcile-buffer-names--duplicates (start)

  (with-current-buffer (get-buffer-create "foonet")
    (erc-mode)
    (setq erc-network 'foonet
          erc-server-current-nick "tester"
          erc-networks--id (erc-networks--id-create nil)
          erc-server-process (funcall start)))

  (with-current-buffer (get-buffer-create "#chan") ; prior session
    (erc-mode)
    (setq erc-server-process (buffer-local-value 'erc-server-process
                                                 (get-buffer "foonet"))
          erc--target (erc--target-from-string "#chan")
          erc-networks--id (erc-networks--id-create nil)))

  (ert-info ("Conflicts not recognized as ERC buffers and not renamed")
    (get-buffer-create "#chan@foonet")
    (should (equal (erc-networks-tests--bufnames "#chan")
                   '("#chan" "#chan@foonet"))))

  ;; These are dupes (not "collisions")

  (with-current-buffer "#chan@foonet" ; same proc
    (erc-mode)
    (setq erc--target (erc--target-from-string "#chan")
          erc-network 'foonet
          erc-server-current-nick "tester"
          erc-server-process (buffer-local-value 'erc-server-process
                                                 (get-buffer "foonet"))
          erc-networks--id (erc-networks--id-create nil)))

  (with-current-buffer (get-buffer-create "#chan@foonet<dead>")
    (erc-mode)
    (setq erc--target (erc--target-from-string "#chan")
          erc-server-process (erc-networks-tests--create-dead-proc)
          erc-network 'foonet
          erc-server-current-nick "tester"
          erc-networks--id (erc-networks--id-create nil)))

  (with-current-buffer (get-buffer-create "#chan@foonet<live>")
    (erc-mode)
    (setq erc--target (erc--target-from-string "#chan")
          erc-server-process (erc-networks-tests--create-live-proc)
          erc-network 'foonet
          erc-server-current-nick "tester"
          erc-networks--id (erc-networks--id-create nil)))

  (let ((created (list (get-buffer "#chan@foonet<live>")
                       (get-buffer "#chan@foonet<dead>")
                       (get-buffer "#chan@foonet"))))

    (with-current-buffer "foonet"
      (should (string= (erc-networks--reconcile-buffer-names
                        (erc--target-from-string "#chan") erc-networks--id)
                       "#chan")))

    (ert-info ("All buffers considered dupes renamed")
      (should (equal (erc-networks-tests--bufnames "#chan")
                     '("#chan" "#chan<2>" "#chan<3>" "#chan<4>"))))

    (ert-info ("All buffers renamed from newest to oldest")
      (should (equal created (list (get-buffer "#chan<2>")
                                   (get-buffer "#chan<3>")
                                   (get-buffer "#chan<4>"))))))

  (erc-networks-tests--clean-bufs))

(defun erc-tests--prep-erc-networks--reconcile-buffer-names--dupes-given (go)

  ;; The connection's network is discovered before target buffers are
  ;; created.  This shows that the network doesn't matter when only
  ;; "given" IDs are present.
  (with-current-buffer (get-buffer-create "oofnet")
    (erc-mode)
    (setq erc-networks--id (erc-networks--id-create 'oofnet)
          erc-network 'foonet
          erc-server-current-nick "tester"
          erc-server-process (funcall go)))

  (with-current-buffer (get-buffer-create "#chan") ; prior session
    (erc-mode)
    (setq erc-networks--id (erc-networks--id-create 'oofnet)
          erc-server-process (buffer-local-value 'erc-server-process
                                                 (get-buffer "oofnet"))
          erc--target (erc--target-from-string "#chan")))

  (with-current-buffer (get-buffer-create "#chan@oofnet") ;dupe/not collision
    (erc-mode)
    (setq erc-networks--id (erc-networks--id-create 'oofnet)
          erc-server-process (buffer-local-value 'erc-server-process
                                                 (get-buffer "oofnet"))
          erc--target (erc--target-from-string "#chan")))

  (with-current-buffer "oofnet"
    (should (string= (erc-networks--reconcile-buffer-names
                      (erc--target-from-string "#chan") erc-networks--id)
                     "#chan")))

  (ert-info ("All buffers matching target and network renamed")
    (should (equal (erc-networks-tests--bufnames "#chan")
                   '("#chan" "#chan<2>"))))

  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--reconcile-buffer-names--duplicates ()
  (ert-info ("Process live, no error")
    (erc-tests--prep-erc-networks--reconcile-buffer-names--duplicates
     #'erc-networks-tests--create-live-proc))

  (ert-info ("Process live, no error, given ID")
    (erc-tests--prep-erc-networks--reconcile-buffer-names--dupes-given
     #'erc-networks-tests--create-live-proc))

  (ert-info ("Process dead")
    (erc-tests--prep-erc-networks--reconcile-buffer-names--duplicates
     #'erc-networks-tests--create-dead-proc))

  (ert-info ("Process dead, given ID")
    (erc-tests--prep-erc-networks--reconcile-buffer-names--dupes-given
     #'erc-networks-tests--create-dead-proc)))

(defun erc-tests--prep-erc-networks--reconcile-buffer-names--no-srv-buf (check)
  (let ((foonet-proc (with-temp-buffer
                       (erc-networks-tests--create-dead-proc))))
    (with-current-buffer (get-buffer-create "barnet")
      (erc-mode)
      (setq erc-network 'barnet
            erc-server-current-nick "tester"
            erc-networks--id (erc-networks--id-create nil)
            erc-server-process (erc-networks-tests--create-dead-proc)))

    ;; Different proc and not "qual-equal" (different elts)
    (with-current-buffer (get-buffer-create "#chan")
      (erc-mode)
      (setq erc-network 'foonet
            erc-server-current-nick "tester"
            erc-networks--id (erc-networks--id-create nil)
            erc--target (erc--target-from-string "#chan")
            erc-server-process foonet-proc))
    (funcall check)
    (erc-networks-tests--clean-bufs)))

(ert-deftest erc-networks--reconcile-buffer-names--no-server-buf ()
  (ert-info ("Existing #chan buffer respected")
    (erc-tests--prep-erc-networks--reconcile-buffer-names--no-srv-buf
     (lambda ()
       (with-current-buffer "barnet"
         (should (string= (erc-networks--reconcile-buffer-names
                           (erc--target-from-string "#chan") erc-networks--id)
                          "#chan@barnet")))
       (ert-info ("Existing #chan buffer found and renamed")
         (should (equal (erc-networks-tests--bufnames "#chan")
                        '("#chan@foonet")))))))

  (ert-info ("Existing #chan buffer")
    (erc-tests--prep-erc-networks--reconcile-buffer-names--no-srv-buf
     (lambda ()
       (with-current-buffer (get-buffer-create "foonet")
         (erc-mode)
         (setq erc-network 'foonet
               erc-server-current-nick "tester"
               erc-networks--id (erc-networks--id-create nil)
               erc-server-process (erc-networks-tests--create-dead-proc))
         (should (string= (erc-networks--reconcile-buffer-names
                           (erc--target-from-string "#chan") erc-networks--id)
                          "#chan")))
       (ert-info ("Nothing renamed")
         (should (equal (erc-networks-tests--bufnames "#chan") '("#chan")))))))

  (ert-info ("Existing #chan@foonet and #chan@barnet buffers")
    (erc-tests--prep-erc-networks--reconcile-buffer-names--no-srv-buf
     (lambda ()
       (with-current-buffer "#chan"
         (rename-buffer "#chan@foonet"))
       (should-not (get-buffer "#chan@barnet"))
       (with-current-buffer (get-buffer-create "#chan@barnet")
         (erc-mode)
         (setq erc--target (erc--target-from-string "#chan")
               erc-server-process (buffer-local-value 'erc-server-process
                                                      (get-buffer "barnet"))
               erc-networks--id (erc-networks--id-create nil)))
       (with-current-buffer (get-buffer-create "foonet")
         (erc-mode)
         (setq erc-network 'foonet
               erc-server-current-nick "tester"
               erc-server-process (erc-networks-tests--create-live-proc)
               erc-networks--id (erc-networks--id-create nil))
         (set-process-query-on-exit-flag erc-server-process nil)
         (should (string= (erc-networks--reconcile-buffer-names
                           (erc--target-from-string "#chan") erc-networks--id)
                          "#chan@foonet")))
       (ert-info ("Nothing renamed")
         (should (equal (erc-networks-tests--bufnames "#chan")
                        '("#chan@barnet" "#chan@foonet"))))))))

(defun erc-tests--prep-erc-networks--reconcile-buffer-names--no-srv-buf-given
    (check)
  (let ((oofnet-proc (with-temp-buffer
                       (erc-networks-tests--create-dead-proc))))

    (with-current-buffer (get-buffer-create "rabnet")
      (erc-mode)
      ;; Again, given name preempts network lookup (unrealistic but
      ;; highlights priorities)
      (setq erc-networks--id (erc-networks--id-create 'rabnet)
            erc-network 'barnet
            erc-server-current-nick "tester"
            erc-server-process (erc-networks-tests--create-dead-proc)))

    ;; Identity is not "qual-equal" to above
    (with-current-buffer (get-buffer-create "#chan")
      (erc-mode)
      (setq erc-networks--id (erc-networks--id-create 'oofnet)
            erc-network 'foonet
            erc--target (erc--target-from-string "#chan")
            erc-server-process oofnet-proc))
    (funcall check)
    (erc-networks-tests--clean-bufs)))

(ert-deftest erc-networks--reconcile-buffer-names--no-server-buf-given ()

  (ert-info ("Existing #chan buffer respected")
    (erc-tests--prep-erc-networks--reconcile-buffer-names--no-srv-buf-given
     (lambda ()
       (with-current-buffer "rabnet"
         (should (string= (erc-networks--reconcile-buffer-names
                           (erc--target-from-string "#chan") erc-networks--id)
                          "#chan@rabnet")))

       (ert-info ("Existing #chan buffer found and renamed")
         (should (equal (erc-networks-tests--bufnames "#chan")
                        '("#chan@oofnet")))))))

  (ert-info ("Existing #chan@oofnet and #chan@rabnet buffers")
    (erc-tests--prep-erc-networks--reconcile-buffer-names--no-srv-buf-given
     (lambda ()
       ;; #chan has already been uniquified (but not grown)
       (with-current-buffer "#chan" (rename-buffer "#chan@oofnet"))
       (should-not (get-buffer "#chan@rabnet"))

       (with-current-buffer (get-buffer-create "#chan@rabnet")
         (erc-mode)
         (setq erc--target (erc--target-from-string "#chan")
               erc-server-process (buffer-local-value 'erc-server-process
                                                      (get-buffer "rabnet"))
               erc-networks--id (buffer-local-value 'erc-networks--id
                                                    (get-buffer "rabnet"))))

       (with-current-buffer (get-buffer-create "oofnet")
         (erc-mode)
         (setq erc-network 'oofnet
               erc-server-current-nick "tester"
               erc-server-process (erc-networks-tests--create-live-proc)
               erc-networks--id (erc-networks--id-create 'oofnet)) ; given
         (set-process-query-on-exit-flag erc-server-process nil)
         (should (string= (erc-networks--reconcile-buffer-names
                           (erc--target-from-string "#chan") erc-networks--id)
                          "#chan@oofnet")))

       (ert-info ("Nothing renamed")
         (should (equal (erc-networks-tests--bufnames "#chan")
                        '("#chan@oofnet" "#chan@rabnet"))))))))

;; This shows a corner case where a user explicitly assigns a "given"
;; ID via `erc-tls' but later connects again without one.  It would
;; actually probably be better if the given identity were to win and
;; the derived one got an <n>-suffix.
;;
;; If we just compared net identities, the two would match, but they
;; don't here because one has a given name and the other a
;; discovered/assembled one; so they are *not* qual-equal.
(ert-deftest erc-networks--reconcile-buffer-names--no-srv-buf-given-mismatch ()
  ;; Existing #chan buffer *not* respected
  (erc-tests--prep-erc-networks--reconcile-buffer-names--no-srv-buf-given
   (lambda ()
     (with-current-buffer (get-buffer-create "oofnet")
       (erc-mode)
       (setq erc-network 'oofnet
             erc-server-current-nick "tester"
             erc-server-process (erc-networks-tests--create-dead-proc)
             erc-networks--id (erc-networks--id-create nil)) ; derived
       (should (string= (erc-networks--reconcile-buffer-names
                         (erc--target-from-string "#chan") erc-networks--id)
                        "#chan@oofnet")))

     (ert-info ("Collision renamed but not grown (because it's a given)")
       ;; Original chan uniquified and moved out of the way
       (should (equal (erc-networks-tests--bufnames "#chan")
                      '("#chan@oofnet<2>")))))))

(defun erc-tests--prep-erc-networks--reconcile-buffer-names--multi-net (check)

  (with-current-buffer (get-buffer-create "foonet")
    (erc-mode)
    (setq erc-network 'foonet
          erc-server-current-nick "tester"
          erc-server-process (erc-networks-tests--create-dead-proc)
          erc-networks--id (erc-networks--id-create nil))) ; derived

  (with-current-buffer (get-buffer-create "barnet")
    (erc-mode)
    (setq erc-network 'barnet
          erc-server-current-nick "tester"
          erc-server-process (erc-networks-tests--create-dead-proc)
          erc-networks--id (erc-networks--id-create nil))) ; derived

  (with-current-buffer
      (get-buffer-create (elt ["#chan" "#chan@foonet"] (random 2)))
    (erc-mode)
    (setq erc--target (erc--target-from-string "#chan"))
    (cl-multiple-value-setq (erc-server-process erc-networks--id)
      (with-current-buffer "foonet"
        (list erc-server-process erc-networks--id))))

  (with-current-buffer (get-buffer-create "#chan@barnet")
    (erc-mode)
    (setq erc--target (erc--target-from-string "#chan"))
    (cl-multiple-value-setq (erc-server-process erc-networks--id)
      (with-current-buffer "barnet"
        (list erc-server-process erc-networks--id))))

  (funcall check)
  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--reconcile-buffer-names--multi-net ()
  (ert-info ("Same network rename")
    (erc-tests--prep-erc-networks--reconcile-buffer-names--multi-net
     (lambda ()
       (with-current-buffer "foonet"
         (let ((result (erc-networks--reconcile-buffer-names
                        (erc--target-from-string "#chan") erc-networks--id)))
           (should (string= result "#chan@foonet"))))

       (should (equal (erc-networks-tests--bufnames "#chan")
                      '("#chan@barnet" "#chan@foonet"))))))

  (ert-info ("Same network keep name")
    (erc-tests--prep-erc-networks--reconcile-buffer-names--multi-net
     (lambda ()
       (with-current-buffer "barnet"
         (let ((result (erc-networks--reconcile-buffer-names
                        (erc--target-from-string "#chan") erc-networks--id)))
           (should (string= result "#chan@barnet"))))

       (should (equal (erc-networks-tests--bufnames "#chan")
                      '("#chan@barnet" "#chan@foonet")))))))

(defun erc-tests--prep-erc-networks--reconcile-buffer-names--multi-net-given
    (check)

  (with-current-buffer (get-buffer-create "oofnet")
    (erc-mode)
    (setq erc-network 'foonet
          erc-server-current-nick "tester"
          erc-networks--id (erc-networks--id-create 'oofnet) ; one given
          erc-server-process (erc-networks-tests--create-dead-proc)))

  (with-current-buffer (get-buffer-create "rabnet")
    (erc-mode)
    (setq erc-network 'barnet
          erc-server-current-nick "tester"
          erc-networks--id (erc-networks--id-create 'rabnet) ; another given
          erc-server-process (erc-networks-tests--create-dead-proc)))

  (with-current-buffer (get-buffer-create (elt ["chan" "#chan@oofnet"]
                                               (random 2)))
    (erc-mode)
    (setq erc--target (erc--target-from-string "#chan"))
    (cl-multiple-value-setq (erc-server-process erc-networks--id)
      (with-current-buffer "oofnet"
        (list erc-server-process erc-networks--id))))

  (with-current-buffer (get-buffer-create "#chan@barnet")
    (erc-mode)
    (setq erc--target (erc--target-from-string "#chan"))
    (cl-multiple-value-setq (erc-server-process erc-networks--id)
      (with-current-buffer "rabnet"
        (list erc-server-process erc-networks--id))))

  (funcall check)
  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--reconcile-buffer-names--multi-net-given ()
  (ert-info ("Same network rename")
    (erc-tests--prep-erc-networks--reconcile-buffer-names--multi-net-given
     (lambda ()
       (with-current-buffer "oofnet"
         (let ((result (erc-networks--reconcile-buffer-names
                        (erc--target-from-string "#chan") erc-networks--id)))
           (should (string= result "#chan@oofnet"))))

       (should (equal (erc-networks-tests--bufnames "#chan")
                      '("#chan@oofnet" "#chan@rabnet"))))))

  (ert-info ("Same network keep name")
    (erc-tests--prep-erc-networks--reconcile-buffer-names--multi-net-given
     (lambda ()
       (with-current-buffer "rabnet"
         (let ((result (erc-networks--reconcile-buffer-names
                        (erc--target-from-string "#chan") erc-networks--id)))
           (should (string= result "#chan@rabnet"))))

       (should (equal (erc-networks-tests--bufnames "#chan")
                      '("#chan@oofnet" "#chan@rabnet")))))))

(defun erc-tests--prep-erc-networks--reconcile-buffer-names--multi-net-mixed
    (check)

  (with-current-buffer (get-buffer-create "foonet")
    (erc-mode)
    (setq erc-network 'foonet
          erc-server-current-nick "tester"
          erc-networks--id (erc-networks--id-create nil) ; one derived
          erc-server-process (erc-networks-tests--create-dead-proc)))

  (with-current-buffer (get-buffer-create "my-conn")
    (erc-mode)
    (setq erc-network 'barnet
          erc-server-current-nick "tester"
          erc-networks--id (erc-networks--id-create 'my-conn) ; one given
          erc-server-process (erc-networks-tests--create-dead-proc)))

  (with-current-buffer (get-buffer-create (elt ["#chan" "#chan@foonet"]
                                               (random 2)))
    (erc-mode)
    (setq erc--target (erc--target-from-string "#chan"))
    (cl-multiple-value-setq (erc-server-process erc-networks--id)
      (with-current-buffer "foonet"
        (list erc-server-process erc-networks--id))))

  (with-current-buffer (get-buffer-create "#chan@my-conn")
    (erc-mode)
    (setq erc--target (erc--target-from-string "#chan"))
    (cl-multiple-value-setq (erc-server-process erc-networks--id)
      (with-current-buffer "my-conn"
        (list erc-server-process erc-networks--id))))

  (funcall check)
  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--reconcile-buffer-names--multi-net-existing ()

  (ert-info ("Buf name derived from network")
    (erc-tests--prep-erc-networks--reconcile-buffer-names--multi-net-mixed
     (lambda ()
       (with-current-buffer "foonet"
         (let ((result (erc-networks--reconcile-buffer-names
                        (erc--target-from-string "#chan") erc-networks--id)))
           (should (string= result "#chan@foonet"))))

       (should (equal (erc-networks-tests--bufnames "#chan")
                      '("#chan@foonet" "#chan@my-conn"))))))

  (ert-info ("Buf name given")
    (erc-tests--prep-erc-networks--reconcile-buffer-names--multi-net-mixed
     (lambda ()
       (with-current-buffer "my-conn"
         (let ((result (erc-networks--reconcile-buffer-names
                        (erc--target-from-string "#chan") erc-networks--id)))
           (should (string= result "#chan@my-conn"))))

       (should (equal (erc-networks-tests--bufnames "#chan")
                      '("#chan@foonet" "#chan@my-conn")))))))

(ert-deftest erc-networks--reconcile-buffer-names--multi-net-suffixed ()
  ;; Two networks, same channel.  One network has two connections.
  ;; When the same channel is joined on the latter under a different
  ;; nick, all buffer names involving that network are suffixed with
  ;; the network identity.

  (with-current-buffer (get-buffer-create "foonet/bob")
    (erc-mode)
    (setq erc-network 'foonet
          erc-server-current-nick "bob"
          erc-networks--id (make-erc-networks--id-qualifying
                            :symbol 'foonet/bob
                            :parts [foonet "bob"]
                            :len 2)
          erc-server-process (erc-networks-tests--create-live-proc)))

  (with-current-buffer (get-buffer-create
                        (elt ["#chan@foonet" "#chan@foonet/bob"] (random 2)))
    (erc-mode)
    (setq erc--target (erc--target-from-string "#chan")
          erc-server-process (buffer-local-value 'erc-server-process
                                                 (get-buffer "foonet/bob"))
          erc-networks--id (buffer-local-value 'erc-networks--id
                                               (get-buffer "foonet/bob"))))

  (with-current-buffer (get-buffer-create "barnet")
    (erc-mode)
    (setq erc-network 'barnet
          erc-server-current-nick (elt ["alice" "bob"] (random 2))
          erc-networks--id (erc-networks--id-create 'barnet)
          erc-server-process (erc-networks-tests--create-live-proc)))

  (with-current-buffer (get-buffer-create "#chan@barnet")
    (erc-mode)
    (setq erc--target (erc--target-from-string "#chan")
          erc-server-process (buffer-local-value 'erc-server-process
                                                 (get-buffer "barnet"))
          erc-networks--id (buffer-local-value 'erc-networks--id
                                               (get-buffer "barnet"))))

  (with-current-buffer (get-buffer-create "foonet/alice")
    (erc-mode)
    (setq erc-network 'foonet
          erc-server-current-nick "alice"
          erc-networks--id (make-erc-networks--id-qualifying
                            :symbol 'foonet/alice
                            :parts [foonet "alice"]
                            :len 2)
          erc-server-process (erc-networks-tests--create-live-proc)))

  (with-current-buffer "foonet/alice"
    (let ((result (erc-networks--reconcile-buffer-names
                   (erc--target-from-string "#chan") erc-networks--id)))
      (should (string= result "#chan@foonet/alice"))))

  (should (equal (erc-networks-tests--bufnames "#chan")
                 '("#chan@barnet" "#chan@foonet/bob")))

  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--reconcile-buffer-names--local ()
  (with-current-buffer (get-buffer-create "DALnet")
    (erc-mode)
    (setq erc-network 'DALnet
          erc-server-announced-name "elysium.ga.us.dal.net"
          erc-server-process (erc-networks-tests--create-dead-proc)
          erc--isupport-params (make-hash-table)
          erc-networks--id (erc-networks--id-create nil))
    (puthash 'CHANTYPES '("&#") erc--isupport-params))

  (ert-info ("Local chan buffer from older, disconnected identity")
    (with-current-buffer (get-buffer-create "&chan")
      (erc-mode)
      ;; Cheat here because localp is determined on identity init
      (setq erc--target (with-current-buffer "DALnet"
                          (erc--target-from-string "&chan"))
            erc-network 'DALnet
            erc-server-announced-name "twisted.ma.us.dal.net"
            erc-server-process (erc-networks-tests--create-dead-proc)
            erc-networks--id (erc-networks--id-create nil))))

  (ert-info ("Local channels renamed using network server names")
    (with-current-buffer "DALnet"
      (let ((result (erc-networks--reconcile-buffer-names
                     (erc--target-from-string "&chan") erc-networks--id)))
        (should (string= result "&chan@elysium.ga.us.dal.net")))))

  (should (get-buffer "&chan@twisted.ma.us.dal.net"))
  (should-not (get-buffer "&chan"))
  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--set-name ()
  (with-current-buffer (get-buffer-create "localhost:6667")
    (let (erc-server-announced-name
          (erc--isupport-params (make-hash-table))
          erc-network
          erc-quit-hook
          (erc-server-process (erc-networks-tests--create-live-proc))
          calls)
      (erc-mode)

      (cl-letf (((symbol-function 'erc-display-line)
                 (lambda (&rest r) (push r calls))))

        (ert-info ("Signals when `erc-server-announced-name' unset")
          (should-error (erc-networks--set-name nil (make-erc-response)))
          (should-not calls))

        (ert-info ("Signals when table empty and NETWORK param unset")
          (setq erc-server-announced-name "irc.fake.gnu.org")
          (should (eq 'error (erc-networks--set-name nil (make-erc-response))))
          (should (string-match-p (rx "*** Failed") (car (pop calls)))))))

    (erc-networks-tests--clean-bufs)))

(ert-deftest erc-networks--ensure-announced ()
  (with-current-buffer (get-buffer-create "localhost:6667")
    (should (local-variable-if-set-p 'erc-server-announced-name))
    (let (erc-insert-modify-hook
          (erc-server-process (erc-networks-tests--create-live-proc))
          (parsed (make-erc-response
                   :unparsed ":irc.barnet.org 422 tester :MOTD File is missing"
                   :sender "irc.barnet.org"
                   :command "422"
                   :command-args '("tester" "MOTD File is missing")
                   :contents "MOTD File is missing")))

      (erc-mode) ; boilerplate displayable start (needs `erc-server-process')
      (insert "\n\n")
      (setq erc-input-marker (make-marker) erc-insert-marker (make-marker))
      (set-marker erc-insert-marker (point-max))
      (erc-display-prompt) ; boilerplate displayable end

      (erc-networks--ensure-announced erc-server-process parsed)
      (goto-char (point-min))
      (search-forward "Failed")
      (should (string= erc-server-announced-name "irc.barnet.org")))
    (when noninteractive (kill-buffer))))

(ert-deftest erc-networks--rename-server-buffer--no-existing--orphan ()
  (with-current-buffer (get-buffer-create "#chan")
    (erc-mode)
    (setq erc-network 'FooNet
          erc-server-current-nick "tester"
          erc--target (erc--target-from-string "#chan")
          erc-networks--id (erc-networks--id-create nil)))

  (with-current-buffer (get-buffer-create "irc.foonet.org")
    (erc-mode)
    (setq erc-network 'FooNet
          erc-server-current-nick "tester"
          erc-server-process (erc-networks-tests--create-live-proc)
          erc-networks--id (erc-networks--id-create nil))
    (should-not (erc-networks--rename-server-buffer erc-server-process))
    (should (string= (buffer-name) "FooNet")))

  (ert-info ("Channel buffer reassociated")
    (erc-server-process-alive "#chan")
    (with-current-buffer "#chan"
      (should erc-server-connected)
      (erc-with-server-buffer
        (should (string= (buffer-name) "FooNet")))))

  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--rename-server-buffer--existing--reuse ()
  (let* ((old-buf (get-buffer-create "FooNet"))
         (old-proc (erc-networks-tests--create-dead-proc old-buf)))

    (with-current-buffer old-buf
      (erc-mode)
      (insert "*** Old buf")
      (setq erc-network 'FooNet
            erc-server-current-nick "tester"
            erc-insert-marker (set-marker (make-marker) (point-max))
            erc-server-process old-proc
            erc-networks--id (erc-networks--id-create nil)))

    (with-current-buffer (get-buffer-create "#chan")
      (erc-mode)
      (setq erc-network 'FooNet
            erc-server-process old-proc
            erc-networks--id (erc-networks--id-create nil)
            erc--target (erc--target-from-string "#chan")))

    (ert-info ("New buffer steals name, content")
      (with-current-buffer (get-buffer-create "irc.foonet.org")
        (erc-mode)
        (setq erc-network 'FooNet
              erc-server-current-nick "tester"
              erc-server-process (erc-networks-tests--create-live-proc)
              erc-networks--id (erc-networks--id-create nil))
        (should-not (erc-networks--rename-server-buffer erc-server-process))
        (should (string= (buffer-name) "FooNet"))
        (goto-char (point-min))
        (should (search-forward "Old buf"))))

    (ert-info ("Channel buffer reassociated")
      (erc-server-process-alive "#chan")
      (with-current-buffer "#chan"
        (should erc-server-connected)
        (should-not (eq erc-server-process old-proc))
        (erc-with-server-buffer
          (should (string= (buffer-name) "FooNet")))))

    (ert-info ("Original buffer killed off")
      (should-not (buffer-live-p old-buf))))

  (erc-networks-tests--clean-bufs))

;; This is for compatibility with pre-28.1 behavior.  Basically, we're
;; trying to match the behavior bug for bug.  All buffers were always
;; suffixed and never reassociated.  28.1 introduced a regression that
;; reversed the latter, but we've reverted that.

(ert-deftest erc-networks--rename-server-buffer--existing--noreuse ()
  (with-suppressed-warnings ((obsolete erc-reuse-buffers))
    (should erc-reuse-buffers) ; default
    (let* ((old-buf (get-buffer-create "irc.foonet.org:6697/irc.foonet.org"))
           (old-proc (erc-networks-tests--create-dead-proc old-buf))
           erc-reuse-buffers)
      (with-current-buffer old-buf
        (erc-mode)
        (insert "*** Old buf")
        (setq erc-network 'FooNet
              erc-server-current-nick "tester"
              erc-insert-marker (set-marker (make-marker) (point-max))
              erc-server-process old-proc
              erc-networks--id (erc-networks--id-create nil)))
      (with-current-buffer (get-buffer-create "#chan")
        (erc-mode)
        (setq erc-network 'FooNet
              erc-server-process old-proc
              erc-networks--id (buffer-local-value 'erc-networks--id old-buf)
              erc--target (erc--target-from-string "#chan"))
        (rename-buffer (erc-networks--construct-target-buffer-name erc--target)))

      (ert-info ("Server buffer uniquely renamed")
        (with-current-buffer
            (get-buffer-create "irc.foonet.org:6697/irc.foonet.org<2>")
          (erc-mode)
          (setq erc-network 'FooNet
                erc-server-current-nick "tester"
                erc-server-process (erc-networks-tests--create-live-proc)
                erc-networks--id (erc-networks--id-create nil))
          (should-not (erc-networks--rename-server-buffer erc-server-process))
          (should (string= (buffer-name)
                           "irc.foonet.org:6697/irc.foonet.org<2>"))
          (goto-char (point-min))
          (should-not (search-forward "Old buf" nil t))))

      (ert-info ("Channel buffer not reassociated")
        (should-not
         (erc-server-process-alive
          (should (get-buffer "#chan/irc.foonet.org"))))
        (with-current-buffer (get-buffer "#chan/irc.foonet.org")
          (should-not erc-server-connected)
          (should (eq erc-server-process old-proc))
          (erc-with-server-buffer
            (should (string= (buffer-name)
                             "irc.foonet.org:6697/irc.foonet.org")))))

      (ert-info ("Old buffer still around")
        (should (buffer-live-p old-buf)))))
  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--rename-server-buffer--reconnecting ()
  (let* ((old-buf (get-buffer-create "FooNet"))
         (old-proc (erc-networks-tests--create-dead-proc old-buf)))

    (with-current-buffer old-buf
      (erc-mode)
      (insert "*** Old buf")
      (setq erc-network 'FooNet
            erc-server-current-nick "tester"
            erc-insert-marker (set-marker (make-marker) (point-max))
            erc-server-process old-proc
            erc-networks--id (erc-networks--id-create nil)))

    (with-current-buffer (get-buffer-create "#chan")
      (erc-mode)
      (setq erc-network 'FooNet
            erc-server-process old-proc
            erc--target (erc--target-from-string "#chan")
            erc-networks--id (erc-networks--id-create nil)))

    (ert-info ("No new buffer")
      (with-current-buffer old-buf
        (setq erc-server-process (erc-networks-tests--create-live-proc))
        (should-not (erc-networks--rename-server-buffer erc-server-process))
        (should (string= (buffer-name) "FooNet"))
        (goto-char (point-min))
        (should (search-forward "Old buf"))))

    (ert-info ("Channel buffer updated with live proc")
      (erc-server-process-alive "#chan")
      (with-current-buffer "#chan"
        (should erc-server-connected)
        (should-not (eq erc-server-process old-proc))
        (erc-with-server-buffer
          (should (string= (buffer-name) "FooNet"))))))

  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--rename-server-buffer--id ()
  (let* ((old-buf (get-buffer-create "MySession"))
         (old-proc (erc-networks-tests--create-dead-proc old-buf)))

    (with-current-buffer old-buf
      (erc-mode)
      (insert "*** Old buf")
      (setq erc-network 'FooNet
            erc-networks--id (erc-networks--id-create 'MySession)
            erc-insert-marker (set-marker (make-marker) (point-max))
            erc-server-process old-proc))

    (with-current-buffer (get-buffer-create "#chan")
      (erc-mode)
      (setq erc-network 'FooNet
            erc-networks--id (erc-networks--id-create 'MySession)
            erc-server-process old-proc
            erc--target (erc--target-from-string "#chan")))

    (ert-info ("No new buffer")
      (with-current-buffer old-buf
        (setq erc-server-process (erc-networks-tests--create-live-proc))
        (should-not (erc-networks--rename-server-buffer erc-server-process))
        (should (string= (buffer-name) "MySession"))
        (goto-char (point-min))
        (should (search-forward "Old buf"))))

    (ert-info ("Channel buffer updated with live proc")
      (erc-server-process-alive "#chan")
      (with-current-buffer "#chan"
        (should erc-server-connected)
        (should-not (eq erc-server-process old-proc))
        (erc-with-server-buffer
          (should (string= (buffer-name) "MySession"))))))

  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--rename-server-buffer--existing--live ()
  (let* (erc-kill-server-hook
         erc-insert-modify-hook
         (old-buf (get-buffer-create "FooNet"))
         (old-proc (erc-networks-tests--create-live-proc old-buf))) ; live

    (with-current-buffer old-buf
      (erc-mode)
      (insert "*** Old buf")
      (setq erc-network 'FooNet
            erc-server-current-nick "tester"
            erc-insert-marker (set-marker (make-marker) (point-max))
            erc-server-process old-proc
            erc-networks--id (erc-networks--id-create nil))
      (should (erc-server-process-alive)))

    (with-current-buffer (get-buffer-create "#chan")
      (erc-mode)
      (setq erc-network 'FooNet
            erc-server-process old-proc
            erc-networks--id (erc-networks--id-create nil)
            erc-server-connected t
            erc--target (erc--target-from-string "#chan")))

    (ert-info ("New buffer rejected, abandoned, not killed")
      (with-current-buffer (get-buffer-create "irc.foonet.org")
        (erc-mode)
        (setq erc-network 'FooNet
              erc-server-current-nick "tester"
              erc-insert-marker (set-marker (make-marker) (point-max))
              erc-server-process (erc-networks-tests--create-live-proc)
              erc-networks--id (erc-networks--id-create nil))
        (should-not (erc-networks--rename-server-buffer erc-server-process))
        (should (eq erc-active-buffer old-buf))
        (should-not (erc-server-process-alive))
        (should (string= (buffer-name) "irc.foonet.org"))
        (goto-char (point-min))
        (search-forward "still connected")))

    (ert-info ("Channel buffer updated with live proc")
      (should (erc-server-process-alive "#chan"))
      (with-current-buffer "#chan"
        (should erc-server-connected)
        (should (erc-server-buffer-live-p))
        (should (eq erc-server-process old-proc))
        (should (buffer-live-p (process-buffer erc-server-process)))
        (with-current-buffer (process-buffer erc-server-process)
          (should (eq (current-buffer) (get-buffer "FooNet")))
          (should (eq (current-buffer) old-buf))))))

  (should (get-buffer "FooNet"))
  (should (get-buffer "irc.foonet.org"))

  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--rename-server-buffer--local-match ()
  (let* ((old-buf (get-buffer-create "FooNet"))
         (old-proc (erc-networks-tests--create-dead-proc old-buf)))

    (with-current-buffer old-buf
      (erc-mode)
      (insert "*** Old buf")
      (setq erc-network 'FooNet
            erc-server-current-nick "tester"
            erc-server-announced-name "us-east.foonet.org"
            erc-insert-marker (set-marker (make-marker) (point-max))
            erc-server-process old-proc
            erc--isupport-params (make-hash-table)
            erc-networks--id (erc-networks--id-create nil))
      (puthash 'CHANTYPES '("&#") erc--isupport-params))

    (with-current-buffer (get-buffer-create "&chan")
      (erc-mode)
      (setq erc-network 'FooNet
            erc-server-process old-proc
            erc-server-announced-name "us-east.foonet.org"
            erc--target (erc--target-from-string "&chan")
            erc-networks--id (erc-networks--id-create nil)))

    (ert-info ("New server buffer steals name, content")
      (with-current-buffer (get-buffer-create "irc.foonet.org")
        (erc-mode)
        (setq erc-network 'FooNet
              erc-server-current-nick "tester"
              erc-server-announced-name "us-east.foonet.org"
              erc-server-process (erc-networks-tests--create-live-proc)
              erc--isupport-params (make-hash-table)
              erc-networks--id (erc-networks--id-create nil))
        (puthash 'CHANTYPES '("&#") erc--isupport-params)
        (should-not (erc-networks--rename-server-buffer erc-server-process))
        (should (string= (buffer-name) "FooNet"))
        (goto-char (point-min))
        (should (search-forward "Old buf"))))

    (ert-info ("Channel buffer reassociated when &local server matches")
      (should (erc-server-process-alive "&chan"))
      (with-current-buffer "&chan"
        (should erc-server-connected)
        (should-not (eq erc-server-process old-proc))
        (erc-with-server-buffer
          (should (string= (buffer-name) "FooNet")))))

    (ert-info ("Original buffer killed off")
      (should-not (buffer-live-p old-buf)))

    (erc-networks-tests--clean-bufs)))

(ert-deftest erc-networks--rename-server-buffer--local-nomatch ()
  (let* ((old-buf (get-buffer-create "FooNet"))
         (old-proc (erc-networks-tests--create-dead-proc old-buf)))

    (with-current-buffer old-buf
      (erc-mode)
      (insert "*** Old buf")
      (setq erc-network 'FooNet
            erc-server-current-nick "tester"
            erc-server-announced-name "us-west.foonet.org"
            erc-insert-marker (set-marker (make-marker) (point-max))
            erc-server-process old-proc
            erc--isupport-params (make-hash-table)
            erc-networks--id (erc-networks--id-create nil))
      (puthash 'CHANTYPES '("&#") erc--isupport-params))

    (with-current-buffer (get-buffer-create "&chan")
      (erc-mode)
      (setq erc-network 'FooNet
            erc-server-process old-proc
            erc-server-announced-name "us-west.foonet.org" ; west
            erc--target (erc--target-from-string "&chan")
            erc-networks--id (erc-networks--id-create nil)))

    (ert-info ("New server buffer steals name, content")
      (with-current-buffer (get-buffer-create "irc.foonet.org")
        (erc-mode)
        (setq erc-network 'FooNet
              erc-server-current-nick "tester"
              erc-server-announced-name "us-east.foonet.org" ; east
              erc-server-process (erc-networks-tests--create-live-proc)
              erc--isupport-params (make-hash-table)
              erc-networks--id (erc-networks--id-create nil))

        (puthash 'CHANTYPES '("&#") erc--isupport-params)
        (should-not (erc-networks--rename-server-buffer erc-server-process))
        (should (string= (buffer-name) "FooNet"))
        (goto-char (point-min))
        (should (search-forward "Old buf"))))

    (ert-info ("Channel buffer now orphaned even though network matches")
      (should-not (erc-server-process-alive "&chan"))
      (with-current-buffer "&chan"
        (should-not erc-server-connected)
        (should (eq erc-server-process old-proc))
        (erc-with-server-buffer
          (should (string= (buffer-name) "FooNet")))))

    (ert-info ("Original buffer killed off")
      (should-not (buffer-live-p old-buf)))

    (erc-networks-tests--clean-bufs)))

(ert-deftest erc-networks--update-server-identity--double-existing ()
  (with-temp-buffer
    (erc-mode)
    (setq erc-networks--id (make-erc-networks--id-qualifying
                            :parts [foonet "bob"] :len 1))

    (with-current-buffer (get-buffer-create "#chan@foonet/bob")
      (erc-mode)
      (setq erc-networks--id (make-erc-networks--id-qualifying
                              :parts [foonet "bob"] :len 2)))
    (with-current-buffer (get-buffer-create "foonet/alice")
      (erc-mode)
      (setq erc-networks--id
            (make-erc-networks--id-qualifying :parts [foonet "alice"] :len 2)))

    (ert-info ("Adopt equivalent identity")
      (should (eq (erc-networks--update-server-identity)
                  (buffer-local-value 'erc-networks--id
                                      (get-buffer "#chan@foonet/bob")))))

    (ert-info ("Ignore non-matches")
      (should-not (erc-networks--update-server-identity))
      (should (eq erc-networks--id
                  (buffer-local-value 'erc-networks--id
                                      (get-buffer "#chan@foonet/bob"))))))

  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--update-server-identity--double-new ()
  (with-temp-buffer
    (erc-mode)
    (setq erc-networks--id (make-erc-networks--id-qualifying
                            :parts [foonet "bob"] :len 1))

    (with-current-buffer (get-buffer-create "foonet/alice")
      (erc-mode)
      (setq erc-networks--id
            (make-erc-networks--id-qualifying :parts [foonet "alice"] :len 2)))
    (with-current-buffer (get-buffer-create "#chan@foonet/alice")
      (erc-mode)
      (setq erc-networks--id (buffer-local-value 'erc-networks--id
                                                 (get-buffer "foonet/alice"))))

    (ert-info ("Evolve identity to prevent ambiguity")
      (should-not (erc-networks--update-server-identity))
      (should (= (erc-networks--id-qualifying-len erc-networks--id) 2))
      (should (eq (erc-networks--id-symbol erc-networks--id) 'foonet/bob))))

  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--update-server-identity--double-bounded ()
  (with-temp-buffer
    (erc-mode)
    (setq erc-networks--id (make-erc-networks--id-qualifying
                            :parts [foonet "bob"] :len 1))

    (with-current-buffer (get-buffer-create "foonet/alice/home")
      (erc-mode)
      (setq erc-networks--id (make-erc-networks--id-qualifying
                              :parts [foonet "alice" home] :len 3)))
    (with-current-buffer (get-buffer-create "#chan@foonet/alice/home")
      (erc-mode)
      (setq erc-networks--id
            (buffer-local-value 'erc-networks--id
                                (get-buffer "foonet/alice/home"))))

    (ert-info ("Evolve identity to prevent ambiguity")
      (should-not (erc-networks--update-server-identity))
      (should (= (erc-networks--id-qualifying-len erc-networks--id) 2))
      (should (eq (erc-networks--id-symbol erc-networks--id) 'foonet/bob))))

  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--update-server-identity--double-even ()
  (with-temp-buffer
    (erc-mode)
    (setq erc-networks--id
          (make-erc-networks--id-qualifying :parts [foonet "bob"] :len 1))

    (with-current-buffer (get-buffer-create "foonet")
      (erc-mode)
      (setq erc-networks--id
            (make-erc-networks--id-qualifying :parts [foonet "alice"] :len 1)))
    (with-current-buffer (get-buffer-create "#chan")
      (erc-mode)
      (setq erc--target (erc--target-from-string "#chan")
            erc-networks--id (buffer-local-value 'erc-networks--id
                                                 (get-buffer "foonet"))))

    (ert-info ("Evolve identity to prevent ambiguity")
      (should-not (erc-networks--update-server-identity))
      (should (= (erc-networks--id-qualifying-len erc-networks--id) 2))
      (should (eq (erc-networks--id-symbol erc-networks--id) 'foonet/bob)))

    (ert-info ("Collision renamed")
      (with-current-buffer "foonet/alice"
        (should (eq (erc-networks--id-symbol erc-networks--id) 'foonet/alice)))

      (with-current-buffer "#chan@foonet/alice"
        (should (eq (erc-networks--id-symbol erc-networks--id)
                    'foonet/alice)))))

  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--update-server-identity--triple-new ()
  (with-temp-buffer
    (erc-mode)
    (setq erc-networks--id
          (make-erc-networks--id-qualifying :parts [foonet "bob" home] :len 1))

    (with-current-buffer (get-buffer-create "foonet/bob/office")
      (erc-mode)
      (setq erc-networks--id
            (make-erc-networks--id-qualifying :parts [foonet "bob" office]
                                              :len 3)))
    (with-current-buffer (get-buffer-create "#chan@foonet/bob/office")
      (erc-mode)
      (setq erc-networks--id
            (buffer-local-value 'erc-networks--id
                                (get-buffer "foonet/bob/office"))))

    (ert-info ("Extend our identity's canonical ID so that it's unique")
      (should-not (erc-networks--update-server-identity))
      (should (= (erc-networks--id-qualifying-len erc-networks--id) 3))))

  (erc-networks-tests--clean-bufs))

(ert-deftest erc-networks--determine ()
  (should (eq (erc-networks--determine "irc.libera.chat") 'Libera.Chat))
  (should (eq (erc-networks--determine "irc.oftc.net") 'OFTC))
  (should (eq (erc-networks--determine "irc.dal.net") 'DALnet))

  (let ((erc-server-announced-name "zirconium.libera.chat"))
    (should (eq (erc-networks--determine) 'Libera.Chat)))
  (let ((erc-server-announced-name "weber.oftc.net"))
    (should (eq (erc-networks--determine) 'OFTC)))
  (let ((erc-server-announced-name "redemption.ix.us.dal.net"))
    (should (eq (erc-networks--determine) 'DALnet)))

  ;; Failure
  (let ((erc-server-announced-name "irc-us2.alphachat.net"))
    (should (eq (erc-networks--determine)
                erc-networks--name-missing-sentinel))))

;;; erc-networks-tests.el ends here
