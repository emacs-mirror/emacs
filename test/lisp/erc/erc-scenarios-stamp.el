;;; erc-scenarios-stamp.el --- Misc `erc-stamp' scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert-x)
(eval-and-compile
  (let ((load-path (cons (ert-resource-directory) load-path)))
    (require 'erc-scenarios-common)))

(require 'erc-stamp)

(defvar erc-scenarios-stamp--user-marker nil)

(defun erc-scenarios-stamp--on-post-modify ()
  (when-let* (((erc--check-msg-prop 'erc--cmd 4)))
    (set-marker erc-scenarios-stamp--user-marker (point-max))
    (ert-info ("User marker correctly placed at `erc-insert-marker'")
      (should (= ?\n (char-before erc-scenarios-stamp--user-marker)))
      (should (= erc-scenarios-stamp--user-marker erc-insert-marker))
      (save-excursion
        (goto-char erc-scenarios-stamp--user-marker)
        ;; The raw message ends in " Iabefhkloqv".  However,
        ;; `erc-server-004' only prints up to the 5th parameter.
        (should (looking-back "CEIMRUabefhiklmnoqstuv\n"))))))

(ert-deftest erc-scenarios-stamp--left/display-margin-mode ()

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/reconnect")
       (dumb-server (erc-d-run "localhost" t 'unexpected-disconnect))
       (port (process-contact dumb-server :service))
       (erc-scenarios-stamp--user-marker (make-marker))
       (erc-stamp--current-time 704591940)
       (erc-stamp--tz t)
       (erc-server-flood-penalty 0.1)
       (erc-insert-timestamp-function #'erc-insert-timestamp-left)
       (erc-modules (cons 'fill-wrap erc-modules))
       (erc-timestamp-only-if-changed-flag nil)
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :full-name "tester"
                                :nick "tester")

        (add-hook 'erc-insert-post-hook #'erc-scenarios-stamp--on-post-modify
                  nil t)
        (funcall expect 5 "This server is in debug mode")

        (ert-info ("Stamps appear in left margin and are invisible")
          (should (eq 'erc-timestamp (field-at-pos (pos-bol))))
          (should (= (pos-bol) (field-beginning (pos-bol))))
          (should (eq 'query-notice (get-text-property (pos-bol) 'erc--msg)))
          (should (eq 'NOTICE (get-text-property (pos-bol) 'erc--cmd)))
          (should (= ?- (char-after (field-end (pos-bol)))))
          (should (equal (get-text-property (1+ (field-end (pos-bol)))
                                            'erc--speaker)
                         "irc.foonet.org"))
          (should (pcase (get-text-property (pos-bol) 'display)
                    (`((margin left-margin) ,s)
                     (eq 'timestamp (get-text-property 0 'invisible s))))))

        ;; We set a third-party marker at the end of 004's message (on
        ;; then "\n"), post-insertion.
        (ert-info ("User markers untouched by subsequent message left stamp")
          (save-excursion
            (goto-char erc-scenarios-stamp--user-marker)
            (should (looking-back "CEIMRUabefhiklmnoqstuv\n"))
            (should (looking-at (rx "[")))))))))

(ert-deftest erc-scenarios-stamp--legacy-date-stamps ()
  (with-suppressed-warnings ((obsolete erc-stamp-prepend-date-stamps-p))
    (erc-scenarios-common-with-cleanup
        ((erc-scenarios-common-dialog "base/reconnect")
         (erc-stamp-prepend-date-stamps-p t)
         (dumb-server (erc-d-run "localhost" t 'unexpected-disconnect))
         (port (process-contact dumb-server :service))
         (erc-server-flood-penalty 0.1)
         (expect (erc-d-t-make-expecter)))

      (ert-info ("Connect")
        (with-current-buffer (erc :server "127.0.0.1"
                                  :port port
                                  :full-name "tester"
                                  :nick "tester")
          (funcall expect 5 "*** Welcome")
          (goto-char (1- (match-beginning 0)))
          (should (eq 'erc-timestamp (field-at-pos (point))))
          (should (eq 'notice (erc--get-inserted-msg-prop 'erc--msg)))
          ;; Force redraw of date stamp.
          (setq erc-timestamp-last-inserted-left nil)

          (funcall expect 5 "This server is in debug mode")
          (while (and (zerop (forward-line -1))
                      (not (eq 'erc-timestamp (field-at-pos (point))))))
          (should (erc--get-inserted-msg-prop 'erc--cmd))
          (should-not erc-stamp--date-mode)
          (should-not erc-stamp--date-stamps))))))

;; This user-owned hook member places a marker on the first message in
;; a buffer.  Inserting a date stamp in front of it shouldn't move the
;; marker.
(defun erc-scenarios-stamp--on-insert-modify ()
  (unless (marker-position erc-scenarios-stamp--user-marker)
    (set-marker erc-scenarios-stamp--user-marker (point-min))
    (save-excursion
      (goto-char erc-scenarios-stamp--user-marker)
      (should (looking-at "Opening"))))

  ;; Sometime after the first message ("Opening connection.."), assert
  ;; that the marker we just placed hasn't moved.
  (when (erc--check-msg-prop 'erc--cmd 2)
    (save-restriction
      (widen)
      (ert-info ("Date stamp preserves opening user marker")
        (goto-char erc-scenarios-stamp--user-marker)
        (should-not (eq 'erc-timestamp (field-at-pos (point))))
        (should (looking-at "Opening"))
        (should (eq 'unknown (get-text-property (point) 'erc--msg))))))

  ;; On 003 ("*** This server was created on"), clear state to force a
  ;; new date stamp on the next message.
  (when (erc--check-msg-prop 'erc--cmd 3)
    (setq erc-timestamp-last-inserted-left nil)
    (set-marker erc-scenarios-stamp--user-marker erc-insert-marker)))

(ert-deftest erc-scenarios-stamp--date-mode/left-and-right ()

  (should (eq erc-insert-timestamp-function
              #'erc-insert-timestamp-left-and-right))

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/reconnect")
       (dumb-server (erc-d-run "localhost" t 'unexpected-disconnect))
       (port (process-contact dumb-server :service))
       (erc-scenarios-stamp--user-marker (make-marker))
       (erc-server-flood-penalty 0.1)
       (erc-modules (if (zerop (random 2))
                        (cons 'fill-wrap erc-modules)
                      erc-modules))
       (expect (erc-d-t-make-expecter))
       (erc-mode-hook
        (cons (lambda ()
                (add-hook 'erc-insert-modify-hook
                          #'erc-scenarios-stamp--on-insert-modify -99 t))
              erc-mode-hook)))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :full-name "tester"
                                :nick "tester")

        (funcall expect 5 "Welcome to the foonet")
        (funcall expect 5 "*** AWAYLEN=390")

        (ert-info ("Date stamp preserves other user marker")
          (goto-char erc-scenarios-stamp--user-marker)
          (should-not (eq 'erc-timestamp (field-at-pos (point))))
          (should (looking-at (rx "*** irc.foonet.org oragono")))
          (should (eq 's004 (get-text-property (point) 'erc--msg))))

        (funcall expect 5 "This server is in debug mode")))))

;; Assert that only one date stamp per day appears in the server
;; buffer when reconnecting.
(ert-deftest erc-scenarios-stamp--date-mode/reconnect ()
  :tags '(:expensive-test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/reconnect")
       (erc-server-flood-penalty 0.1)
       (erc-stamp--tz t)
       (erc-server-reconnect-function #'erc-server-delayed-reconnect)
       (erc-server-auto-reconnect t)
       ;; Start close to midnight: 2024-06-02T23:58:11.055Z
       (erc-stamp--current-time (if (< emacs-major-version 29)
                                    '(26205 1811 55000 0)
                                  '(1717372691055 . 1000)))
       (erc-insert-post-hook (cons (lambda ()
                                     (setq erc-stamp--current-time
                                           (time-add erc-stamp--current-time 0.1)))
                                   erc-insert-post-hook))
       (dumb-server (erc-d-run "localhost" t
                               'unexpected-disconnect 'unexpected-disconnect))
       ;; Define overriding formatting function for catalog entry
       ;; `disconnected' to spoof time progressing past midnight.
       (erc-message-english-disconnected
        (let ((orig erc-message-english-disconnected))
          (lambda (&rest _)
            (setq erc-stamp--current-time
                  (time-add erc-stamp--current-time 120))
            orig)))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (funcall expect 10 "debug mode")))

    ;; Ensure date stamps are unique per server buffer.
    (with-current-buffer "FooNet"
      (funcall expect 10 "[Mon Jun  3 2024]")
      (funcall expect -0.1 "[Mon Jun  3 2024]") ; no duplicates
      (funcall expect 10 "[00:00]")
      (funcall expect -0.1 "[00:00]")
      (funcall expect 10 "Welcome to the foonet")
      (delete-process erc-server-process))))

;;; erc-scenarios-stamp.el ends here
