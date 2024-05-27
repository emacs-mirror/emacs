;;; track-changes.el --- API to react to buffer modifications  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Version: 1.2
;; Package-Requires: ((emacs "24"))

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

;; This library is a layer of abstraction above `before-change-functions'
;; and `after-change-functions' which takes care of accumulating changes
;; until a time when its client finds it convenient to react to them.
;;
;; It provides an API that is easier to use correctly than our
;; `*-change-functions' hooks.  Problems that it claims to solve:
;;
;; - Before and after calls are not necessarily paired.
;; - The beg/end values don't always match.
;; - There's usually only one call to the hooks per command but
;;   there can be thousands of calls from within a single command,
;;   so naive users will tend to write code that performs poorly
;;   in those rare cases.
;; - The hooks are run at a fairly low-level so there are things they
;;   really shouldn't do, such as modify the buffer or wait.
;; - The after call doesn't get enough info to rebuild the before-change state,
;;   so some callers need to use both before-c-f and after-c-f (and then
;;   deal with the first two points above).
;;
;; The new API is almost like `after-change-functions' except that:
;; - It provides the "before string" (i.e. the previous content of
;;   the changed area) rather than only its length.
;; - It can combine several changes into larger ones.
;; - Clients do not have to process changes right away, instead they
;;   can let changes accumulate (by combining them into a larger change)
;;   until it is convenient for them to process them.
;; - By default, changes are signaled at most once per command.

;; The API consists in the following functions:
;;
;;     (track-changes-register SIGNAL &key NOBEFORE DISJOINT IMMEDIATE)
;;     (track-changes-fetch ID FUNC)
;;     (track-changes-unregister ID)
;;
;; A typical use case might look like:
;;
;;     (defvar my-foo--change-tracker nil)
;;     (define-minor-mode my-foo-mode
;;       "Fooing like there's no tomorrow."
;;       (if (null my-foo-mode)
;;           (when my-foo--change-tracker
;;             (track-changes-unregister my-foo--change-tracker)
;;             (setq my-foo--change-tracker nil))
;;         (unless my-foo--change-tracker
;;           (setq my-foo--change-tracker
;;                 (track-changes-register
;;                  (lambda (id)
;;                    (track-changes-fetch
;;                     id (lambda (beg end before)
;;                          ..DO THE THING..))))))))

;;; News:

;; Since v1.1:
;;
;; - New function `track-changes-inconsistent-state-p'.

;;; Code:

;; Random ideas:
;; - We could let trackers specify a function to record auxiliary info
;;   about a state.  This would be called from the first before-c-f
;;   and then provided to FUNC.  TeXpresso could use it to avoid needing
;;   the BEFORE string: it could record the total number of bytes
;;   in the "before" state so that from `track-changes-fetch' it could
;;   compute the number of bytes that used to be in BEG/END.
;; - We could also let them provide another function to run in
;;   before-c-f to signal errors if the change is not acceptable,
;;   but contrary to before-c-f it would be called only when we
;;   move t-c--before-beg/end so it scales better when there are
;;   many small changes.

(require 'cl-lib)

;;;; Internal types and variables.

(cl-defstruct (track-changes--tracker
               ;; (:noinline t) ;Requires Emacs≥27
               (:constructor nil)
               (:constructor track-changes--tracker ( signal state
                                                      &optional
                                                      nobefore immediate)))
  signal state nobefore immediate)

(cl-defstruct (track-changes--state
               ;; (:noinline t) ;Requires Emacs≥27
               (:constructor nil)
               (:constructor track-changes--state ()))
  "Object holding a description of a buffer state.
A buffer state is described by a BEG/END/BEFORE triplet which say how to
recover that state from the next state.  I.e. if the buffer's contents
reflects the next state, you can recover the previous state by replacing
the BEG..END region with the BEFORE string.

NEXT is the next state object (i.e. a more recent state).
If NEXT is nil it means it's the most recent state and it may be incomplete
\(BEG/END/BEFORE may be nil), in which case those fields will take their
values from `track-changes--before-(beg|end|before)' when the next
state is created."
  (beg (point-max))
  (end (point-min))
  (before nil)
  (next nil))

(defvar-local track-changes--trackers ()
  "List of trackers currently registered in the buffer.")
(defvar-local track-changes--clean-trackers ()
  "List of trackers that are clean.
Those are the trackers that get signaled when a change is made.")

(defvar-local track-changes--disjoint-trackers ()
 "List of trackers that want to react to disjoint changes.
These trackers are signaled every time track-changes notices
that some upcoming changes touch another \"distant\" part of the buffer.")

(defvar-local track-changes--state nil)

;; `track-changes--before-*' keep track of the content of the
;; buffer when `track-changes--state' was cleaned.
(defvar-local track-changes--before-beg 0
  "Beginning position of the remembered \"before string\".")
(defvar-local track-changes--before-end 0
  "End position of the text replacing the \"before string\".")
(defvar-local track-changes--before-string ""
  "String holding some contents of the buffer before the current change.
This string is supposed to cover all the already modified areas plus
the upcoming modifications announced via `before-change-functions'.
If all trackers are `nobefore', then this holds the `buffer-size' before
the current change.")
(defvar-local track-changes--before-no t
  "If non-nil, all the trackers are `nobefore'.
Should be equal to (memq #\\='track-changes--before before-change-functions).")

(defvar-local track-changes--before-clean 'unset
  "Status of `track-changes--before-*' vars.
More specifically it indicates which \"before\" they hold.
- nil: The vars hold the \"before\" info of the current state.
- `unset': The vars hold the \"before\" info of some older state.
  This is what it is set to right after creating a fresh new state.
- `set': Like nil but the state is still clean because the buffer has not
  been modified yet.  This is what it is set to after the first
  `before-change-functions'  but before an `after-change-functions'.")

(defvar-local track-changes--buffer-size nil
  "Current size of the buffer, as far as this library knows.
This is used to try and detect cases where buffer modifications are \"lost\".")

;;;; Exposed API.

(defvar track-changes-record-errors
  ;; By default, record errors only for non-release versions, because we
  ;; presume that these might be too old to receive fixes, so better not
  ;; annoy the user too much about errors.
  (string-match "\\..*\\." emacs-version)
  "If non-nil, keep track of errors in `before/after-chage-functions' calls.
The errors are kept in `track-changes--error-log'.")

(cl-defun track-changes-register ( signal &key nobefore disjoint immediate)
  "Register a new tracker whose change-tracking function is SIGNAL.
Return the ID of the new tracker.

SIGNAL is a function that will be called with one argument (the tracker ID)
after the current buffer is modified, so that it can react to the change.
Once called, SIGNAL is not called again until `track-changes-fetch'
is called with the corresponding tracker ID.

If optional argument NOBEFORE is non-nil, it means that this tracker does
not need the BEFORE strings (it will receive their size instead).

If optional argument DISJOINT is non-nil, SIGNAL is called every time just
before combining changes from \"distant\" parts of the buffer.
This is needed when combining disjoint changes into one bigger change
is unacceptable, typically for performance reasons.
These calls are distinguished from normal calls by calling SIGNAL with
a second argument which is the distance between the upcoming change and
the previous changes.
BEWARE: In that case SIGNAL is called directly from `before-change-functions'
and should thus be extra careful: don't modify the buffer, don't call a function
that may block, ...
In order to prevent the upcoming change from being combined with the previous
changes, SIGNAL needs to call `track-changes-fetch' before it returns.

By default SIGNAL is called after a change via a 0 seconds timer.
If optional argument IMMEDIATE is non-nil it means SIGNAL should be called
as soon as a change is detected,
BEWARE: In that case SIGNAL is called directly from `after-change-functions'
and should thus be extra careful: don't modify the buffer, don't call a function
that may block, do as little work as possible, ...
When IMMEDIATE is non-nil, the SIGNAL should probably not always call
`track-changes-fetch', since that would defeat the purpose of this library."
  (when (and nobefore disjoint)
    ;; FIXME: Without `before-change-functions', we can discover
    ;; a disjoint change only after the fact, which is not good enough.
    ;; But we could use a stripped down before-change-function,
    (error "`disjoint' not supported for `nobefore' trackers"))
  (track-changes--clean-state)
  (unless nobefore
    (setq track-changes--before-no nil)
    (add-hook 'before-change-functions #'track-changes--before nil t))
  (add-hook 'after-change-functions  #'track-changes--after  nil t)
  (let ((tracker (track-changes--tracker signal track-changes--state
                                         nobefore immediate)))
    (push tracker track-changes--trackers)
    (push tracker track-changes--clean-trackers)
    (when disjoint
      (push tracker track-changes--disjoint-trackers))
    tracker))

(defun track-changes-unregister (id)
  "Remove the tracker denoted by ID.
Trackers can consume resources (especially if `track-changes-fetch' is
not called), so it is good practice to unregister them when you don't
need them any more."
  (unless (memq id track-changes--trackers)
    (error "Unregistering a non-registered tracker: %S" id))
  (setq track-changes--trackers (delq id track-changes--trackers))
  (setq track-changes--clean-trackers (delq id track-changes--clean-trackers))
  (setq track-changes--disjoint-trackers
        (delq id track-changes--disjoint-trackers))
  (when (cl-every #'track-changes--tracker-nobefore track-changes--trackers)
    (setq track-changes--before-no t)
    (remove-hook 'before-change-functions #'track-changes--before t))
  (when (null track-changes--trackers)
    (mapc #'kill-local-variable
          '(track-changes--before-beg
            track-changes--before-end
            track-changes--before-string
            track-changes--buffer-size
            track-changes--before-clean
            track-changes--state))
    (remove-hook 'after-change-functions  #'track-changes--after  t)))

(defun track-changes-fetch (id func)
  "Fetch the pending changes for tracker ID pass them to FUNC.
ID is the tracker ID returned by a previous `track-changes-register'.
FUNC is a function.  It is called with 3 arguments (BEGIN END BEFORE)
where BEGIN..END delimit the region that was changed since the last
time `track-changes-fetch' was called and BEFORE is a string containing
the previous content of that region (or just its length as an integer
if the tracker ID was registered with the `nobefore' option).
If track-changes detected that some changes were missed, then BEFORE will
be the symbol `error' to indicate that the buffer got out of sync.
This reflects a bug somewhere, so please report it when it happens.

If no changes occurred since the last time, it doesn't call FUNC and
returns nil, otherwise it returns the value returned by FUNC
and re-enable the TRACKER corresponding to ID."
  (cl-assert (memq id track-changes--trackers))
  (unless (equal track-changes--buffer-size (buffer-size))
    (track-changes--recover-from-error))
  (let ((beg nil)
        (end nil)
        (before t)
        (lenbefore 0)
        (states ()))
    ;; Transfer the data from `track-changes--before-string'
    ;; to the tracker's state object, if needed.
    (track-changes--clean-state)
    ;; We want to combine the states from most recent to oldest,
    ;; so reverse them.
    (let ((state (track-changes--tracker-state id)))
      (while state
        (push state states)
        (setq state (track-changes--state-next state))))

    (cond
     ((eq (car states) track-changes--state)
      (cl-assert (null (track-changes--state-before (car states))))
      (setq states (cdr states)))
     (t
      ;; The states are disconnected from the latest state because
      ;; we got out of sync!
      (cl-assert (eq (track-changes--state-before (car states)) 'error))
      (setq beg (point-min))
      (setq end (point-max))
      (setq before 'error)
      (setq states nil)))

    (dolist (state states)
      (let ((prevbeg (track-changes--state-beg state))
            (prevend (track-changes--state-end state))
            (prevbefore (track-changes--state-before state)))
        (if (eq before t)
            (progn
              ;; This is the most recent change.  Just initialize the vars.
              (setq beg prevbeg)
              (setq end prevend)
              (setq lenbefore
                    (if (stringp prevbefore) (length prevbefore) prevbefore))
              (setq before
                    (unless (track-changes--tracker-nobefore id) prevbefore)))
          (let ((endb (+ beg lenbefore)))
            (when (< prevbeg beg)
              (if (not before)
                  (setq lenbefore (+ (- beg prevbeg) lenbefore))
                (setq before
                      (concat (buffer-substring-no-properties
                               prevbeg beg)
                              before))
                (setq lenbefore (length before)))
              (setq beg prevbeg)
              (cl-assert (= endb (+ beg lenbefore))))
            (when (< endb prevend)
              (let ((new-end (+ end (- prevend endb))))
                (if (not before)
                    (setq lenbefore (+ lenbefore (- new-end end)))
                  (setq before
                        (concat before
                                (buffer-substring-no-properties
                                 end new-end)))
                  (setq lenbefore (length before)))
                (setq end new-end)
                (cl-assert (= prevend (+ beg lenbefore)))
                (setq endb (+ beg lenbefore))))
            (cl-assert (<= beg prevbeg prevend endb))
            ;; The `prevbefore' is covered by the new one.
            (if (not before)
                (setq lenbefore
                      (+ (- prevbeg beg)
                         (if (stringp prevbefore)
                             (length prevbefore) prevbefore)
                         (- endb prevend)))
              (setq before
                    (concat (substring before 0 (- prevbeg beg))
                            prevbefore
                            (substring before (- (length before)
                                                 (- endb prevend)))))
              (setq lenbefore (length before)))))))
    (unwind-protect
        (if (null beg)
            (progn
              (cl-assert (null states))
              ;; We may have been called in the middle of another
              ;; `track-changes-fetch', in which case we may be in a clean
              ;; state but not yet on `track-changes--clean-trackers'
              ;;(cl-assert (memq id track-changes--clean-trackers))
              (cl-assert (eq (track-changes--tracker-state id)
                             track-changes--state))
              ;; Nothing to do.
              nil)
          (cl-assert (not (memq id track-changes--clean-trackers)))
          (cl-assert (<= (point-min) beg end (point-max)))
          ;; Update the tracker's state *before* running `func' so we don't risk
          ;; mistakenly replaying the changes in case `func' exits non-locally.
          (setf (track-changes--tracker-state id) track-changes--state)
          (funcall func beg end (or before lenbefore)))
      ;; Re-enable the tracker's signal only after running `func', so
      ;; as to avoid nested invocations.
      (cl-pushnew id track-changes--clean-trackers))))

(defun track-changes-inconsistent-state-p ()
  "Return whether the current buffer is in an inconsistent state.
Ideally `before/after-change-functions' should be called for each and every
buffer change, but some packages make transient changes without
running those hooks.
This function tries to detect those situations so clients can decide
to postpone their work to a later time when the buffer is hopefully
returned to a consistent state."
  (or (not (equal track-changes--buffer-size (buffer-size)))
      inhibit-modification-hooks))

;;;; Auxiliary functions.

(defun track-changes--clean-state ()
  (cond
   ((null track-changes--state)
    (cl-assert track-changes--before-clean)
    (cl-assert (null track-changes--buffer-size))
    ;; No state has been created yet.  Do it now.
    (setq track-changes--buffer-size (buffer-size))
    (when track-changes--before-no
      (setq track-changes--before-string (buffer-size)))
    (setq track-changes--state (track-changes--state)))
   (track-changes--before-clean
    ;; If the state is already clean, there's nothing to do.
    nil)
   (t
    (cl-assert (<= (track-changes--state-beg track-changes--state)
                   (track-changes--state-end track-changes--state)))
    (let ((actual-beg (track-changes--state-beg track-changes--state))
          (actual-end (track-changes--state-end track-changes--state)))
      (if track-changes--before-no
          (progn
            (cl-assert (integerp track-changes--before-string))
            (setf (track-changes--state-before track-changes--state)
                  (- track-changes--before-string
                     (- (buffer-size) (- actual-end actual-beg))))
            (setq track-changes--before-string (buffer-size)))
        (cl-assert (<= track-changes--before-beg
                       actual-beg actual-end
                       track-changes--before-end))
        (cl-assert (null (track-changes--state-before track-changes--state)))
        ;; The `track-changes--before-*' vars can cover more text than the
        ;; actually modified area, so trim it down now to the relevant part.
        (unless (= (- track-changes--before-end track-changes--before-beg)
                   (- actual-end actual-beg))
          (setq track-changes--before-string
                (substring track-changes--before-string
                           (- actual-beg track-changes--before-beg)
                           (- (length track-changes--before-string)
                              (- track-changes--before-end actual-end))))
          (setq track-changes--before-beg actual-beg)
          (setq track-changes--before-end actual-end))
        (setf (track-changes--state-before track-changes--state)
              track-changes--before-string)))
    ;; Note: We preserve `track-changes--before-*' because they may still
    ;; be needed, in case `after-change-functions' are run before the next
    ;; `before-change-functions'.
    ;; Instead, we set `track-changes--before-clean' to `unset' to mean that
    ;; `track-changes--before-*' can be reset at the next
    ;; `before-change-functions'.
    (setq track-changes--before-clean 'unset)
    (let ((new (track-changes--state)))
      (setf (track-changes--state-next track-changes--state) new)
      (setq track-changes--state new)))))

(defvar track-changes--error-log ()
  "List of errors encountered.
Each element is a triplet (BUFFER-NAME BACKTRACE RECENT-KEYS).")

(defun track-changes--recover-from-error ()
  ;; We somehow got out of sync.  This is usually the result of a bug
  ;; elsewhere that causes the before-c-f and after-c-f to be improperly
  ;; paired, or to be skipped altogether.
  ;; Not much we can do, other than force a full re-synchronization.
  (if (not track-changes-record-errors)
      (message "Recovering from confusing calls to `before/after-change-functions'!")
    (warn "Missing/incorrect calls to `before/after-change-functions'!!
Details logged to `track-changes--error-log'")
    (push (list (buffer-name)
                (let* ((bf (backtrace-frames
                            #'track-changes--recover-from-error))
                       (tail (nthcdr 50 bf)))
                  (when tail (setcdr tail '...))
                  bf)
                (let ((rk (recent-keys 'include-cmds)))
                  (if (< (length rk) 20) rk (substring rk -20))))
          track-changes--error-log))
  (setq track-changes--before-clean 'unset)
  (setq track-changes--buffer-size (buffer-size))
  ;; Create a new state disconnected from the previous ones!
  ;; Mark the previous one as junk, just to be clear.
  (setf (track-changes--state-before track-changes--state) 'error)
  (setq track-changes--state (track-changes--state)))

(defun track-changes--before (beg end)
  (cl-assert track-changes--state)
  (cl-assert (<= beg end))
  (let* ((size (- end beg))
         (reset (lambda ()
                  (cl-assert track-changes--before-clean)
                  (setq track-changes--before-clean 'set)
                  (setf track-changes--before-string
                        (buffer-substring-no-properties beg end))
                  (setf track-changes--before-beg beg)
                  (setf track-changes--before-end end)))

         (signal-if-disjoint
          (lambda (pos1 pos2)
            (let ((distance (- pos2 pos1)))
              (when (> distance
                       ;; If the distance is smaller than the size of the
                       ;; current change, then we may as well consider it
                       ;; as "near".
                       (max (length track-changes--before-string)
                            size
                            (- track-changes--before-end
                               track-changes--before-beg)))
                (dolist (tracker track-changes--disjoint-trackers)
                  (funcall (track-changes--tracker-signal tracker)
                           tracker distance))
                ;; Return non-nil if the state was cleaned along the way.
                track-changes--before-clean)))))

    (if track-changes--before-clean
        (progn
          ;; Detect disjointness with previous changes here as well,
          ;; so that if a client calls `track-changes-fetch' all the time,
          ;; it doesn't prevent others from getting a disjointness signal.
          (when (and track-changes--before-beg
                     (let ((found nil))
                       (dolist (tracker track-changes--disjoint-trackers)
                         (unless (memq tracker track-changes--clean-trackers)
                           (setq found t)))
                       found))
            ;; There's at least one `tracker' that wants to know about disjoint
            ;; changes *and* it has unseen pending changes.
            ;; FIXME: This can occasionally signal a tracker that's clean.
            (if (< beg track-changes--before-beg)
                (funcall signal-if-disjoint end track-changes--before-beg)
              (funcall signal-if-disjoint track-changes--before-end beg)))
          (funcall reset))
      (save-restriction
        (widen)
        (cl-assert (<= (point-min)
                       track-changes--before-beg
                       track-changes--before-end
                       (point-max)))
        (when (< beg track-changes--before-beg)
          (if (and track-changes--disjoint-trackers
                   (funcall signal-if-disjoint end track-changes--before-beg))
              (funcall reset)
            (let* ((old-bbeg track-changes--before-beg)
                   ;; To avoid O(N²) behavior when faced with many small
                   ;; changes, we copy more than needed.
                   (new-bbeg
                    (min beg (max (point-min)
                                  (- old-bbeg
                                     (length track-changes--before-string))))))
              (setf track-changes--before-beg new-bbeg)
              (cl-callf (lambda (old new) (concat new old))
                  track-changes--before-string
                (buffer-substring-no-properties new-bbeg old-bbeg)))))

        (when (< track-changes--before-end end)
          (if (and track-changes--disjoint-trackers
                   (funcall signal-if-disjoint track-changes--before-end beg))
              (funcall reset)
            (let* ((old-bend track-changes--before-end)
                   ;; To avoid O(N²) behavior when faced with many small
                   ;; changes, we copy more than needed.
                   (new-bend
                    (max end (min (point-max)
                                  (+ old-bend
                                     (length track-changes--before-string))))))
              (setf track-changes--before-end new-bend)
              (cl-callf concat track-changes--before-string
                (buffer-substring-no-properties old-bend new-bend)))))))))

(defun track-changes--after (beg end len)
  (cl-assert track-changes--state)
  (and (eq track-changes--before-clean 'unset)
       (not track-changes--before-no)
       ;; This can be a sign that a `before-change-functions' went missing,
       ;; or that we called `track-changes--clean-state' between
       ;; a `before-change-functions' and `after-change-functions'.
       (track-changes--before beg end))
  (setq track-changes--before-clean nil)
  (let ((offset (- (- end beg) len)))
    (cl-incf track-changes--before-end offset)
    (cl-incf track-changes--buffer-size offset)
    (if (not (or track-changes--before-no
                 (save-restriction
                   (widen)
                   (<= (point-min)
                       track-changes--before-beg
                       beg end
                       track-changes--before-end
                       (point-max)))))
        ;; BEG..END is not covered by previous `before-change-functions'!!
        (track-changes--recover-from-error)
      ;; Note the new changes.
      (when (< beg (track-changes--state-beg track-changes--state))
        (setf (track-changes--state-beg track-changes--state) beg))
      (cl-callf (lambda (old-end) (max end (+ old-end offset)))
          (track-changes--state-end track-changes--state))
      (cl-assert (or track-changes--before-no
                     (<= track-changes--before-beg
                         (track-changes--state-beg track-changes--state)
                         beg end
                         (track-changes--state-end track-changes--state)
                         track-changes--before-end)))))
  (while track-changes--clean-trackers
    (let ((tracker (pop track-changes--clean-trackers)))
      (if (track-changes--tracker-immediate tracker)
          (funcall (track-changes--tracker-signal tracker) tracker)
        (run-with-timer 0 nil #'track-changes--call-signal
                        (current-buffer) tracker)))))

(defun track-changes--call-signal (buf tracker)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      ;; Silence ourselves if `track-changes-fetch' was called
      ;; or the tracker was unregistered in the mean time.
      (when (and (not (memq tracker track-changes--clean-trackers))
                 (memq tracker track-changes--trackers))
        (funcall (track-changes--tracker-signal tracker) tracker)))))

;;;; Extra candidates for the API.

;; The functions below came up during the design of this library, but
;; I'm not sure if they're worth the trouble or not, so for now I keep
;; them here (with a "--" in the name) for documentation.  --Stef

;; This could be a good alternative to using a temp-buffer like in
;; `eglot--virtual-pos-to-lsp-position': since presumably we've just
;; been changing this very area of the buffer, the gap should be
;; ready nearby, so the operation should be fairly cheap, while
;; giving you the comfort of having access to the *full* buffer text.
;;
;; It may seem silly to go back to the previous state, since we could have
;; used `before-change-functions' to run FUNC right then when we were in
;; that state.  The advantage is that with track-changes we get to decide
;; retroactively which state is the one for which we want to call FUNC and
;; which BEG..END to use: when that state was current we may have known
;; then that it would be "the one" but we didn't know what BEG and END
;; should be because those depend on the changes that came afterwards.
(defun track-changes--in-revert (beg end before func)
  "Call FUNC with the buffer contents temporarily reverted to BEFORE.
FUNC is called with no arguments and with point right after BEFORE.
FUNC is not allowed to modify the buffer and it should refrain from using
operations that use a cache populated from the buffer's content,
such as `syntax-ppss'."
  (catch 'track-changes--exit
    (with-silent-modifications ;; This has to be outside `atomic-change-group'.
      (atomic-change-group
        (goto-char end)
        (insert-before-markers before)
        (delete-region beg end)
        (throw 'track-changes--exit
               (let ((inhibit-read-only nil)
                     (buffer-read-only t))
                 (funcall func)))))))

;; This one is a cheaper version of (track-changes-fetch id #'ignore),
;; e.g. for clients that don't want to see their own changes.
(defun track-changes--reset (id)
  "Mark all past changes as handled for tracker ID.
Re-arms ID's signal."
  (track-changes--clean-state)
  (setf (track-changes--tracker-state id) track-changes--state)
  (cl-pushnew id track-changes--clean-trackers)
  (cl-assert (not (track-changes--pending-p id))))

(defun track-changes--pending-p (id)
  "Return non-nil if there are pending changes for tracker ID."
  (or (not track-changes--before-clean)
      (track-changes--state-next id)))

(defmacro with--track-changes (id vars &rest body)
  (declare (indent 2) (debug (form sexp body)))
  `(track-changes-fetch ,id (lambda ,vars ,@body)))

(provide 'track-changes)
;;; track-changes.el ends here
