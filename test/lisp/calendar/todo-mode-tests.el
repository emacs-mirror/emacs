;;; todo-mode-tests.el --- tests for todo-mode.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

;; Author: Stephen Berman <stephen.berman@gmx.net>
;; Keywords: calendar

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
(require 'ert-x)
(require 'todo-mode)

(defvar todo-test-data-dir
  (file-truename
   (expand-file-name "todo-mode-resources/"
                     (file-name-directory (or load-file-name
                                              buffer-file-name))))
  "Base directory of todo-mode.el test data files.")

(defvar todo-test-file-1 (expand-file-name "todo-test-1.todo"
                                           todo-test-data-dir)
  "Todo mode test file.")

(defvar todo-test-archive-1 (expand-file-name "todo-test-1.toda"
                                              todo-test-data-dir)
  "Todo Archive mode test file.")

(defmacro with-todo-test (&rest body)
  "Set up an isolated todo-mode test environment."
  (declare (debug (body)))
  `(let* ((todo-test-home (make-temp-file "todo-test-home-" t))
          ;; Since we change HOME, clear this to avoid a conflict
          ;; e.g. if Emacs runs within the user's home directory.
          (abbreviated-home-dir nil)
          (process-environment (cons (format "HOME=%s" todo-test-home)
                                     process-environment))
          (todo-directory todo-test-data-dir)
          (todo-default-todo-file (todo-short-file-name
				   (car (funcall todo-files-function)))))
     (unwind-protect
         (progn ,@body)
       ;; Restore pre-test-run state of test files.
       (dolist (f (directory-files todo-directory))
         (let ((buf (get-file-buffer f)))
           (when buf
             (with-current-buffer buf
               (restore-buffer-modified-p nil)
               (kill-buffer)))))
       (delete-directory todo-test-home t))))

(defun todo-test--show (num &optional archive)
  "Display category NUM of test todo file.
With non-nil ARCHIVE argument, display test archive file category."
  (let* ((file (if archive todo-test-archive-1 todo-test-file-1))
         (buf (find-file-noselect file)))
    (set-buffer buf)
    (if archive (todo-archive-mode) (todo-mode))
    (setq todo-category-number num)
    (todo-category-select)
    (goto-char (point-min))))

;; (defun todo-test-get-archive (num)
;;   "Display category NUM of todo archive test file."
;;   (let ((archive-buf (find-file-noselect todo-test-archive-1)))
;;     (set-buffer archive-buf)
;;     (todo-archive-mode)
;;     (setq todo-category-number num)
;;     (todo-category-select)))

(defun todo-test--is-current-buffer (filename)
  "Return non-nil if FILENAME's buffer is current."
  (let ((bufname (buffer-file-name (current-buffer))))
    (and bufname (equal (file-truename bufname) filename))))

(ert-deftest todo-test-todo-quit01 ()
  "Test the behavior of todo-quit with archive and todo files.
Invoking todo-quit in todo-archive-mode should make the
corresponding todo-mode category current, if it exits, otherwise
the current todo-mode category.  Quitting todo-mode without an
intermediate buffer switch should not make the archive buffer
current again."
  (with-todo-test
   (todo-test--show 2 'archive)
   (let ((cat-name (todo-current-category)))
     (todo-quit)
     (should (todo-test--is-current-buffer todo-test-file-1))
     (should (equal (todo-current-category) cat-name))
     (todo-test--show 1 'archive)
     (setq cat-name (todo-current-category))
     (todo-quit)
     (should (todo-test--is-current-buffer todo-test-file-1))
     (should (equal todo-category-number 1))
     (todo-forward-category)         ; Category 2 in todo file now current.
     (todo-test--show 3 'archive)    ; No corresponding category in todo file.
     (setq cat-name (todo-current-category))
     (todo-quit)
     (should (todo-test--is-current-buffer todo-test-file-1))
     (should (equal todo-category-number 2))
     (todo-quit)
     (should-not (todo-test--is-current-buffer todo-test-archive-1)))))

(ert-deftest todo-test-todo-quit02 () ; bug#27121
  "Test the behavior of todo-quit with todo and non-todo buffers.
If the buffer made current by invoking todo-quit in a todo-mode
buffer is buried by quit-window, the todo-mode buffer should not
become current."
  (with-todo-test
   (todo-show)
   (should (todo-test--is-current-buffer todo-test-file-1))
   (let ((dir (dired default-directory)))
     (todo-show)
     (todo-quit)
     (should (equal (current-buffer) dir))
     (quit-window)
     (should-not (todo-test--is-current-buffer todo-test-file-1)))))

(ert-deftest todo-test-item-highlighting () ; bug#27133
  "Test whether `todo-toggle-item-highlighting' highlights whole item.
In particular, all lines of a multiline item should be highlighted."
  (with-todo-test
   (todo-test--show 1)
   (todo-toggle-item-highlighting)
   (let ((end (1- (todo-item-end)))
         (beg (todo-item-start)))
     (should (eq (get-char-property beg 'face) 'hl-line))
     (should (eq (get-char-property end 'face) 'hl-line))
     (should (> (count-lines beg end) 1))
     (should (eq (next-single-char-property-change beg 'face) (1+ end))))
   (todo-toggle-item-highlighting)))   ; Turn off highlighting (for test rerun).

(ert-deftest todo-test-revert-buffer01 ()   ; bug#27609
  "Test whether todo-mode buffer remains read-only after reverting."
  (with-todo-test
   (todo-show)
   (let ((opoint (point)))
     (should (equal buffer-read-only t))
     (todo-revert-buffer nil t)
     (should (equal buffer-read-only t))
     (should (eq (point) opoint)))))

(ert-deftest todo-test-revert-buffer02 ()   ; bug#27609
  "Test whether todo-archive-mode buffer remains read-only after reverting."
  (with-todo-test
   (todo-test--show 1 'archive)
   (let ((opoint (point)))
     (should (equal buffer-read-only t))
     (todo-revert-buffer nil t)
     (should (equal buffer-read-only t))
     (should (eq (point) opoint)))))

(ert-deftest todo-test-raise-lower-priority ()
  "Test the behavior of todo-{raise,lower}-item-priority."
  (with-todo-test
   ;; (todo-show)
   (todo-test--show 1)
   (goto-char (point-min))
   (let ((p1 (point))
	 (s1 (todo-item-string))
	 p2 s2 p3 p4)
     ;; First item in category.
     (should (equal p1 (todo-item-start)))
     (todo-next-item)
     (setq p2 (point))
     ;; Second item in category.
     (setq s2 (todo-item-string))
     ;; Second item is lower.
     (should (> p2 p1))
     ;; Case 1: lowering priority.
     (todo-previous-item)
     (todo-lower-item-priority)
     ;; Now what was the first item is the second and vice versa.
     (setq p1 (point))
     (should (equal s1 (todo-item-string)))
     (todo-previous-item)
     (setq p2 (point))
     (should (equal s2 (todo-item-string)))
     (should (> p1 p2))
     ;; Case 2: raising priority.
     (todo-next-item)
     (todo-raise-item-priority)
     ;; Now what had become the second item is again the first and
     ;; vice versa.
     (setq p1 (point))
     (should (equal s1 (todo-item-string)))
     (todo-next-item)
     (setq p2 (point))
     (should (equal s2 (todo-item-string)))
     (should (> p2 p1))
     ;; Case 3: empty line (bug#27609).
     (goto-char (point-max))
     ;; The last line in the category is always empty.
     (should-not (todo-item-string))
     (todo-raise-item-priority)
     ;; Raising item priority on the empty string is a noop.
     (should (equal (point) (point-max)))
     (todo-lower-item-priority)
     ;; Lowering item priority on the empty string is a noop.
     (should (equal (point) (point-max)))
     ;; Case 4: done item (bug#27609).
     ;; todo-toggle-view-done-items recenters the window if point is
     ;; not visible, so we have to make sure the todo-mode buffer is
     ;; in a live window in the test run to avoid failing with (error
     ;; "`recenter'ing a window that does not display ;; current-buffer.").
     ;; (But this is not necessary in todo-test-toggle-item-header01
     ;; below -- why not, or why is it here?  Note that without
     ;; setting window buffer, the test only fails on the first run --
     ;; on rerunning it passes.)
     (set-window-buffer nil (current-buffer))
     (todo-toggle-view-done-items)
     (todo-next-item)
     ;; Now the current item is the first done item.
     (should (todo-done-item-p))
     (setq p3 (point))
     (todo-raise-item-priority)
     ;; Raising item priority on a done item is a noop.
     (should (eq (point) p3))
     (todo-lower-item-priority)
     ;; Lowering item priority on a done item is a noop.
     (should (eq (point) p3))
     ;; Case 5: raising first item and lowering last item.
     (goto-char (point-min))            ; Now on first item.
     ;; Changing item priority moves point to todo-item-start, so move
     ;; it away from there for the test.
     (end-of-line)
     (setq p4 (point))
     (todo-raise-item-priority)
     ;; Raising priority of first item is a noop.
     (should (equal (point) p4))
     (goto-char (point-max))
     (todo-previous-item)               ; Now on last item.
     (end-of-line)
     (setq p4 (point))
     (todo-lower-item-priority)
     (should (equal (point) p4)))))

(ert-deftest todo-test-todo-mark-unmark-category () ; bug#27609
  "Test behavior of todo-mark-category and todo-unmark-category."
  (with-todo-test
   (todo-show)
   (let ((cat (todo-current-category)))
     (todo-mark-category)
     (should (equal (todo-get-count 'todo cat)
		    (cdr (assoc cat todo-categories-with-marks))))
     (todo-unmark-category)
     (should-not (assoc cat todo-categories-with-marks)))))

(defun todo-test--move-item (cat &optional priority file)
  "Move item(s) to category CAT with priority PRIORITY (for todo item).
This provides a noninteractive API for todo-move-item for use in
automatic testing."
  (let ((cat0 (car (nth (1- cat) todo-categories)))
        (file0 (or file todo-current-todo-file)))
    (cl-letf (((symbol-function 'todo-read-category)
               (lambda (_prompt &optional _match-type _file) (cons cat0 file0)))
              ((symbol-function 'read-number) ; For todo-set-item-priority
               (lambda (_prompt &optional _default) (or priority 1))))
      (todo-move-item))))

(ert-deftest todo-test-move-item01 ()
  "Test moving a todo item to another category with a given priority."
  (with-todo-test
   (todo-test--show 1)
   (let* ((cat1 (todo-current-category))
	  (cat2 (car (nth 1 todo-categories)))
	  (cat1-todo (todo-get-count 'todo cat1))
	  (cat2-todo (todo-get-count 'todo cat2))
	  (item (todo-item-string)))
     (todo-test--move-item 2 3)
     (should (equal (todo-current-category) cat2))
     (should (equal (todo-item-string) item))
     (should (equal (overlay-get (todo-get-overlay 'prefix) 'before-string)
		     "3 "))
     (todo-backward-category)           ; Go to first category again.
     (should-error (search-forward item))
     (should (= (todo-get-count 'todo cat1) (1- cat1-todo)))
     (should (= (todo-get-count 'todo cat2) (1+ cat2-todo))))))

(ert-deftest todo-test-move-item02 ()   ; bug#27609
  "Test moving a marked todo item to previous category."
  (with-todo-test
   (todo-test--show 2)
   (let* ((cat2 (todo-current-category))
	  (cat1 (car (nth 0 todo-categories)))
	  (cat2-todo (todo-get-count 'todo cat2))
	  (cat1-todo (todo-get-count 'todo cat1))
	  (item (todo-item-string)))
     ;; If todo-toggle-mark-item is not called interactively, its
     ;; optional prefix argument evaluates to nil and this raises a
     ;; wrong-type-argument error.
     (call-interactively 'todo-toggle-mark-item)
     (todo-test--move-item 1)
     (should (equal (todo-current-category) cat1))
     (should (equal (todo-item-string) item))
     (should (equal (overlay-get (todo-get-overlay 'prefix) 'before-string)
		     "1 "))
     (todo-forward-category)           ; Go to second category again.
     (should-error (search-forward item))
     (should (= (todo-get-count 'todo cat1) (1+ cat1-todo)))
     (should (= (todo-get-count 'todo cat2) (1- cat2-todo))))))

(ert-deftest todo-test-move-item03 ()   ; bug#27609
  "Test moving a done item to another category.
In the new category it should be the first done item."
  (with-todo-test
   (todo-test--show 1)
   (let* ((cat1 (todo-current-category))
	  (cat2 (car (nth 1 todo-categories)))
	  (cat1-done (todo-get-count 'done cat1))
	  (cat2-done (todo-get-count 'done cat2)))
     (goto-char (point-max))
     (set-window-buffer nil (current-buffer)) ; Why is this necessary?
     (todo-toggle-view-done-items)
     (todo-next-item)
     (let ((item (todo-item-string)))
       (todo-test--move-item 2)
       (should (equal (todo-current-category) cat2))
       (should (equal (todo-item-string) item))
       (should (todo-done-item-p))
       (forward-line -1)
       (should (looking-at todo-category-done))
       (todo-backward-category)
       (should-error (search-forward item))
       (should (= (todo-get-count 'done cat1) (1- cat1-done)))
       (should (= (todo-get-count 'done cat2) (1+ cat2-done)))))))

(ert-deftest todo-test-move-item04 ()   ; bug#27609
  "Test moving both a todo and a done item to another category.
In the new category the todo item should have the provided
priority and the done item should be the first done item."
  (with-todo-test
   (todo-test--show 1)
   (let* ((cat1 (todo-current-category))
	  (cat2 (car (nth 1 todo-categories)))
	  (cat1-todo (todo-get-count 'todo cat1))
	  (cat2-todo (todo-get-count 'todo cat2))
	  (cat1-done (todo-get-count 'done cat1))
	  (cat2-done (todo-get-count 'done cat2))
	  (todo-item (todo-item-string)))
     (call-interactively 'todo-toggle-mark-item)
     (goto-char (point-max))
     ;; Why is this necessary here but not below?
     (set-window-buffer nil (current-buffer))
     (todo-toggle-view-done-items)
     (todo-next-item)
     (let ((done-item (todo-item-string)))
       (call-interactively 'todo-toggle-mark-item)
       (todo-test--move-item 2 3)
       (should (equal (todo-current-category) cat2))
       ;; Point should be on the moved todo item.
       (should (equal (todo-item-string) todo-item))
       ;; Done items section should be visible and the move done item
       ;; should be at the top of it.
       (should (search-forward done-item))
       (should (todo-done-item-p))
       (forward-line -1)
       (should (looking-at todo-category-done))
       ;; Make sure marked items are no longer in first category.
       (todo-backward-category)
       (should-error (search-forward todo-item))
       (todo-toggle-view-done-items)
       (should-error (search-forward done-item))
       (should (= (todo-get-count 'todo cat1) (1- cat1-todo)))
       (should (= (todo-get-count 'todo cat2) (1+ cat2-todo)))
       (should (= (todo-get-count 'done cat1) (1- cat1-done)))
       (should (= (todo-get-count 'done cat2) (1+ cat2-done)))))))

(ert-deftest todo-test-move-item05 ()   ; bug#27609
  "Test moving multiple todo and done items to another category.
Both types of item should be moved en bloc to the new category,
and the top todo item should have the provided priority and
the top done item should be the first done item."
  (with-todo-test
   (todo-test--show 1)
   (let* ((cat1 (todo-current-category))
	  (cat2 (car (nth 1 todo-categories)))
	  (cat1-todo (todo-get-count 'todo cat1))
	  (cat2-todo (todo-get-count 'todo cat2))
	  (cat1-done (todo-get-count 'done cat1))
	  (cat2-done (todo-get-count 'done cat2))
	  (todo-items (buffer-string))
          (done-items (prog2 (todo-toggle-view-done-only)
                          (buffer-string)
                        (todo-toggle-view-done-only))))
     ;; Why is this necessary here but not below?
     (set-window-buffer nil (current-buffer))
     (todo-toggle-view-done-items)
     (todo-mark-category)
     (todo-test--move-item 2 3)
     (should (equal (todo-current-category) cat2))
     ;; Point should be at the start of the first moved todo item.
     (should (looking-at (regexp-quote todo-items)))
     ;; Done items section should be visible and the move done item
     ;; should be at the top of it.
     (should (search-forward done-items))
     (goto-char (match-beginning 0))
     (should (todo-done-item-p))
     (forward-line -1)
     (should (looking-at todo-category-done))
     ;; Make sure marked items are no longer in first category.
     (todo-backward-category)
     (should (eq (point-min) (point-max))) ; All todo items were moved.
     ;; This passes when run interactively but fails in a batch run:
     ;; the message is displayed but (current-message) evaluates to
     ;; nil.
     ;; (todo-toggle-view-done-items)         ; All done items were moved.
     ;; (let ((msg (current-message)))
     ;;   (should (equal msg "There are no done items in this category.")))
     (todo-toggle-view-done-only)
     (should (eq (point-min) (point-max))) ; All done items were moved.
     (should (= (todo-get-count 'todo cat1) 0))
     (should (= (todo-get-count 'todo cat2) (+ cat1-todo cat2-todo)))
     (should (= (todo-get-count 'done cat1) 0))
     (should (= (todo-get-count 'done cat2) (+ cat1-done cat2-done))))))

(ert-deftest todo-test-toggle-item-header01 () ; bug#27609
  "Test toggling item header from an empty category."
  (with-todo-test
   (todo-test--show 3)
   (should (eq (point-min) (point-max))) ; Category is empty.
   (todo-toggle-item-header)
   (todo-backward-category)
   ;; Header is hidden.
   (should (equal (overlay-get (todo-get-overlay 'header) 'display) ""))
   (todo-forward-category)
   (todo-toggle-item-header)
   (todo-backward-category)
   ;; Header is shown.
   (should-not (todo-get-overlay 'header))))

;; FIXME: This test doesn't show the effect of the display overlay on
;; calling todo-next-item in todo-mode: When using Todo mode, the
;; display engine moves point out of the overlay, but here point does
;; not get moved, even when display-graphic-p.
(ert-deftest todo-test-toggle-item-header02 () ; bug#27609
  "Test navigating between items with hidden header."
  ;; This makes no difference for testing todo-next-item.
  ;; (skip-unless (display-graphic-p))
  (with-todo-test
   (todo-test--show 2)
   (let* ((start0 (point))
          (find-start (lambda ()
                        (re-search-forward
                         (concat todo-date-string-start
                                 todo-date-pattern
			         "\\( " diary-time-regexp "\\)?"
			         (regexp-quote todo-nondiary-end) "?")
		         (line-end-position) t)
                        (forward-char)
                        (point)))
          (start1 (save-excursion (funcall find-start)))
          (start2 (save-excursion (todo-next-item) (funcall find-start))))
     (should (looking-at todo-item-start))
     (todo-toggle-item-header)
     ;; Point hasn't changed...
     (should (eq (point) start0))
     (should (looking-at todo-item-start))
     (todo-next-item)
     ;; FIXME: This should (and when using todo-mode does) put point
     ;; at the start of the item's test, not at todo-item-start, like
     ;; todo-previous-item below.  But the following tests fail; why?
     ;; (N.B.: todo-backward-item, called by todo-previous-item,
     ;; explicitly moves point forward to where it needs to be because
     ;; otherwise the display engine moves it backward.)
     ;; (should (eq (point) start2))
     ;; (should-not (looking-at todo-item-start))
     ;; And these pass, though they shouldn't:
     (should-not (eq (point) start2))
     (should (looking-at todo-item-start))
     (todo-previous-item)
     ;; ...but now it has.
     (should (eq (point) start1))
     (should-not (looking-at todo-item-start))
     ;; This fails just like the above.
     ;; (todo-next-item)
     ;; (should (eq (point) start2))
     ;; (should-not (looking-at todo-item-start))
     ;; This is the status quo but is it desirable?
     (todo-toggle-item-header)
     (should (eq (point) start1))
     (should-not (looking-at todo-item-start)))))

(ert-deftest todo-test-toggle-item-header03 () ; bug#27609
  "Test display of hidden item header when changing item's priority."
  (with-todo-test
   (todo-test--show 2)
   (todo-toggle-item-header)
   (should (equal (overlay-get (todo-get-overlay 'header) 'display) ""))
   (todo-lower-item-priority)
   (should (equal (overlay-get (todo-get-overlay 'header) 'display) ""))
   (todo-raise-item-priority)
   (should (equal (overlay-get (todo-get-overlay 'header) 'display) ""))
   ;; Set priority noninteractively.
   (cl-letf (((symbol-function 'read-number)
              (lambda (_prompt &optional _default) 3)))
     (todo-item-undone))
   (should (equal (overlay-get (todo-get-overlay 'header) 'display) ""))))

(ert-deftest todo-test-toggle-item-header04 () ; bug#27609
  "Test display of hidden item header under todo-item-(un)done."
  (with-todo-test
   (todo-test--show 1)
   (let ((item (todo-item-string)))
     (todo-toggle-item-header)
     (should (equal (overlay-get (todo-get-overlay 'header) 'display) ""))
     (todo-item-done)
     ;; Without set-window-buffer here this test passes when run
     ;; interactively but fails in a batch run.
     (set-window-buffer nil (current-buffer))
     (todo-toggle-view-done-items)
     (should (search-forward item))
     (todo-item-start)
     (should (equal (overlay-get (todo-get-overlay 'header) 'display) ""))
     ;; Set priority for todo-item-undone noninteractively.
     (cl-letf (((symbol-function 'read-number)
		(lambda (_prompt &optional _default) 1)))
       (todo-item-undone))
     (should (equal (overlay-get (todo-get-overlay 'header) 'display) "")))))

(ert-deftest todo-test-toggle-item-header05 () ; bug#27609
  "Test display of hidden item header under todo-move-item."
  (with-todo-test
   (todo-test--show 1)
   (todo-toggle-item-header)
   (todo-test--move-item 2 3)
   (should (equal (overlay-get (todo-get-overlay 'header) 'display) ""))))

(ert-deftest todo-test-toggle-item-header06 () ; bug#27609
  "Test display of hidden item header under (un)archiving.
The relocated item's header should take on the display status of
headers in the goal file, even when the display status in the
source file is different."
  (with-todo-test
   (todo-test--show 1)
   (todo-toggle-item-header)
   (todo-toggle-view-done-only)         ; Go to first (i.e. top) done item.
   (let ((item (todo-item-string)))
     (todo-archive-done-item)
     (todo-toggle-view-done-only)       ; To display all items on unarchiving.
     (todo-find-archive)
     (should (equal (todo-item-string) item)) ; The just archived item.
     ;; The archive file headers are displayed by default.
     (should-not (todo-get-overlay 'header))
     (todo-unarchive-items)
     ;; Headers in the todo file are still hidden.
     (should (equal (overlay-get (todo-get-overlay 'header) 'display) "")))))

(defun todo-test--insert-item (item &optional priority
                                    _arg diary-type date-type time where)
  "Insert string ITEM into current category with priority PRIORITY.
The remaining arguments (except _ARG, which is ignored) specify
item insertion parameters.  This provides a noninteractive API
for todo-insert-item for use in automatic testing."
  (cl-letf (((symbol-function 'read-from-minibuffer)
             (lambda (_prompt) item))
            ((symbol-function 'read-number) ; For todo-set-item-priority
             (lambda (_prompt &optional _default) (or priority 1))))
    (todo-insert-item--basic nil diary-type date-type time where)))

(ert-deftest todo-test-toggle-item-header07 () ; bug#27609
  "Test display of hidden item header under todo-insert-item."
  (with-todo-test
   (todo-test--show 1)
   (todo-toggle-item-header)
   (let ((item "Test display of hidden item header under todo-insert-item."))
     (todo-test--insert-item item 1)
     (should (equal (overlay-get (todo-get-overlay 'header) 'display) "")))))

(defun todo-test--done-items-separator (&optional eol)
  "Set up test of command interaction with done items separator.
With non-nil argument EOL, return the position at the end of the
separator, otherwise, return the position at the beginning."
  (todo-test--show 1)
  (goto-char (point-max))
  ;; See comment about recentering in todo-test-raise-lower-priority.
  (set-window-buffer nil (current-buffer))
  (todo-toggle-view-done-items)
  ;; FIXME: Point should now be on the first done item, and in batch
  ;; testing it is, so we have to move back one line to the done items
  ;; separator; but for some reason, in the graphical test
  ;; environment, it stays on the last empty line of the todo items
  ;; section, so there we have to advance one character to the done
  ;; items separator.
  (if (display-graphic-p)
      (forward-char)
    (forward-line -1))
  (if eol (forward-char)))

(ert-deftest todo-test-done-items-separator01-bol () ; bug#32343
  "Test item copying and here insertion at BOL of separator.
Both should be user errors."
  (with-todo-test
   (todo-test--done-items-separator)
   (let* ((copy-err "Item copying is not valid here")
          (here-err "Item insertion is not valid here")
          (insert-item-test (lambda (where)
                              (should-error (todo-insert-item--basic
                                             nil nil nil nil where)))))
     (should (string= copy-err (cadr (funcall insert-item-test 'copy))))
     (should (string= here-err (cadr (funcall insert-item-test 'here)))))))

(ert-deftest todo-test-done-items-separator01-eol () ; bug#32343
  "Test item copying and here insertion at EOL of separator.
Both should be user errors."
  (with-todo-test
   (todo-test--done-items-separator 'eol)
   (let* ((copy-err "Item copying is not valid here")
          (here-err "Item insertion is not valid here")
          (insert-item-test (lambda (where)
                              (should-error (todo-insert-item--basic
                                             nil nil nil nil where)))))
     (should (string= copy-err (cadr (funcall insert-item-test 'copy))))
     (should (string= here-err (cadr (funcall insert-item-test 'here)))))))

(ert-deftest todo-test-done-items-separator02-bol () ; bug#32343
  "Test item editing commands at BOL of done items separator.
They should all be noops."
  (with-todo-test
   (todo-test--done-items-separator)
   (should-not (todo-item-done))
   (should-not (todo-raise-item-priority))
   (should-not (todo-lower-item-priority))
   (should-not (called-interactively-p #'todo-set-item-priority))
   (should-not (called-interactively-p #'todo-move-item))
   (should-not (called-interactively-p #'todo-delete-item))
   (should-not (called-interactively-p #'todo-edit-item))))

(ert-deftest todo-test-done-items-separator02-eol () ; bug#32343
  "Test item editing command at EOL of done items separator.
They should all be noops."
  (with-todo-test
   (todo-test--done-items-separator 'eol)
   (should-not (todo-item-done))
   (should-not (todo-raise-item-priority))
   (should-not (todo-lower-item-priority))
   (should-not (called-interactively-p #'todo-set-item-priority))
   (should-not (called-interactively-p #'todo-move-item))
   (should-not (called-interactively-p #'todo-delete-item))
   (should-not (called-interactively-p #'todo-edit-item))))

(ert-deftest todo-test-done-items-separator03-bol () ; bug#32343
  "Test item marking at BOL of done items separator.
This should be a noop, adding no marks to the category."
  (with-todo-test
   (todo-test--done-items-separator)
   (call-interactively #'todo-toggle-mark-item)
   (should-not (assoc (todo-current-category) todo-categories-with-marks))))

(ert-deftest todo-test-done-items-separator03-eol () ; bug#32343
  "Test item marking at EOL of done items separator.
This should be a noop, adding no marks to the category."
  (with-todo-test
   (todo-test--done-items-separator 'eol)
   (call-interactively #'todo-toggle-mark-item)
   (should-not (assoc (todo-current-category) todo-categories-with-marks))))

(ert-deftest todo-test-done-items-separator04-bol () ; bug#32343
  "Test moving to previous item from BOL of done items separator.
This should move point to the last not done todo item."
  (with-todo-test
   (todo-test--done-items-separator)
   (let ((last-item (save-excursion
                      ;; Move to empty line after last todo item.
                      (forward-line -1)
                      (todo-previous-item)
                      (todo-item-string))))
     (should (string= last-item (save-excursion
                                  (todo-previous-item)
                                  (todo-item-string)))))))

(ert-deftest todo-test-done-items-separator04-eol () ; bug#32343
  "Test moving to previous item from EOL of done items separator.
This should move point to the last not done todo item."
  (with-todo-test
   (todo-test--done-items-separator 'eol)
   (let ((last-item (save-excursion
                      ;; Move to empty line after last todo item.
                      (forward-line -1)
                      (todo-previous-item)
                      (todo-item-string))))
     (should (string= last-item (save-excursion
                                  (todo-previous-item)
                                  (todo-item-string)))))))

(ert-deftest todo-test-done-items-separator05-bol () ; bug#32343
  "Test moving to next item from BOL of done items separator.
This should move point to the first done todo item."
  (with-todo-test
   (todo-test--done-items-separator)
   (let ((first-done (save-excursion
                      ;; Move to empty line after last todo item.
                      (forward-line -1)
                      (todo-next-item)
                      (todo-item-string))))
     (should (string= first-done (save-excursion
                                  (todo-next-item)
                                  (todo-item-string)))))))

(ert-deftest todo-test-done-items-separator05-eol () ; bug#32343
  "Test moving to next item from EOL of done items separator.
This should move point to the first done todo item."
  (with-todo-test
   (todo-test--done-items-separator 'eol)
   (let ((first-done (save-excursion
                      ;; Move to empty line after last todo item.
                      (forward-line -1)
                      (todo-next-item)
                      (todo-item-string))))
     (should (string= first-done (save-excursion
                                  (todo-next-item)
                                  (todo-item-string)))))))

;; Item highlighting uses hl-line-mode, which enables highlighting in
;; post-command-hook.  For some reason, in the test environment, the
;; hook function is not automatically run, so after enabling item
;; highlighting, use ert-simulate-command around the next command,
;; which explicitly runs the hook function.
(ert-deftest todo-test-done-items-separator06-bol () ; bug#32343
  "Test enabling item highlighting at BOL of done items separator.
Subsequently moving to an item should show it highlighted."
  (with-todo-test
   (todo-test--done-items-separator)
   (call-interactively #'todo-toggle-item-highlighting)
   (ert-simulate-command '(todo-previous-item))
   (should (eq 'hl-line (get-char-property (point) 'face)))))

(ert-deftest todo-test-done-items-separator06-eol () ; bug#32343
  "Test enabling item highlighting at EOL of done items separator.
Subsequently moving to an item should show it highlighted."
  (with-todo-test
   (todo-test--done-items-separator 'eol)
   (todo-toggle-item-highlighting)
   (forward-line -1)
   (ert-simulate-command '(todo-previous-item))
   (should (eq 'hl-line (get-char-property (point) 'face)))))

(ert-deftest todo-test-done-items-separator07 () ; bug#32343
  "Test item highlighting when crossing done items separator.
The highlighting should remain enabled."
  (with-todo-test
   (todo-test--done-items-separator)
   (todo-previous-item)
   (todo-toggle-item-highlighting)
   (todo-next-item)               ; Now on empty line above separator.
   (forward-line)                 ; Now on separator.
   (ert-simulate-command '(forward-line)) ; Now on first done item.
   (should (eq 'hl-line (get-char-property (point) 'face)))))

(ert-deftest todo-test-current-file-in-edit-mode () ; bug#32437
  "Test the value of todo-current-todo-file in todo-edit-mode."
  (with-todo-test
   (todo-test--show 1)
   ;; The preceding calls todo-mode but does not run pre-command-hook
   ;; in the test environment, thus failing to set
   ;; todo-global-current-todo-file, which is needed for the test
   ;; after todo-edit-item--text.  So force the hook function to run.
   (ert-simulate-command '(todo-mode))
   (let ((curfile todo-current-todo-file))
     (should (equal curfile todo-test-file-1))
     (todo-edit-item--text 'multiline)
     (should (equal todo-current-todo-file curfile))
     (todo-edit-quit)
     (todo-edit-file)
     (should (equal todo-current-todo-file curfile))
     (todo-edit-quit))
   (todo-find-archive)
   (let ((curfile todo-current-todo-file))
     (should (equal curfile todo-test-archive-1))
     (todo-edit-file)
     (should (equal todo-current-todo-file curfile)))))

(ert-deftest todo-test-edit-quit () ; bug#32437
  "Test result of exiting todo-edit-mode on a whole file.
Exiting should return to the same todo-mode or todo-archive-mode
buffer from which the editing command was invoked."
  (with-todo-test
   (todo-test--show 1)
   (let ((buf (current-buffer)))
     (todo-edit-file)
     (todo-edit-quit)
     (should (eq (current-buffer) buf))
     (should (eq major-mode 'todo-mode))
   (todo-find-archive)
   (let ((buf (current-buffer)))
     (todo-edit-file)
     (todo-edit-quit)
     (should (eq (current-buffer) buf))
     (should (eq major-mode 'todo-archive-mode))))))

(defun todo-test--add-file (file cat)
  "Add file FILE with category CAT to todo-files and show it.
This provides a noninteractive API for todo-add-file for use in
automatic testing."
  (let ((file0 (file-truename (concat todo-test-data-dir file ".todo")))
        todo-add-item-if-new-category)  ; Don't need an item in cat.
    (cl-letf (((symbol-function 'todo-read-file-name)
               (lambda (_prompt) file0))
              ((symbol-function 'todo-read-category)
               (lambda (_prompt &optional _match-type _file) (cons cat file0))))
      (call-interactively 'todo-add-file) ; Interactive to call todo-show.
      (todo-add-category file0 cat))))

(defun todo-test--delete-file ()
  "Delete current todo file without prompting."
  (cl-letf (((symbol-function 'yes-or-no-p)
             (lambda (_prompt) t)))
    (todo-delete-file)))

(ert-deftest todo-test-add-and-delete-file () ; bug#32627
  "Test adding a new todo file and then deleting it.
Calling todo-show should display the last current todo file, not
necessarily the new file.  After deleting the new file, todo-show
should display the previously current (or default) todo file."
  (with-todo-test
   (todo-show)
   (should (equal todo-current-todo-file todo-test-file-1))
   (let* ((file (concat todo-directory "todo-test-2.todo"))
          (file-nb (file-name-base file))
          (cat "cat21"))
     (todo-test--add-file file-nb cat)  ; Add new file and show it.
     (should (equal todo-current-todo-file file))
     (todo-quit)   ; Quitting todo-mode displays previous buffer.
     (should (equal todo-current-todo-file todo-test-file-1))
     (switch-to-buffer "*scratch*")
     (todo-show)   ; Show the last current todo-file (not the new one).
     (should (equal todo-current-todo-file todo-test-file-1))
     (switch-to-buffer (get-file-buffer file)) ; Back to new file.
     (should (equal todo-current-todo-file file))
     (todo-test--delete-file)
     (todo-show)                        ; Back to old file.
     (should (equal todo-current-todo-file todo-test-file-1))
     (delete-file (concat file "~")))))


(provide 'todo-mode-tests)
;;; todo-mode-tests.el ends here
