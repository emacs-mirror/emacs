;;; backtrace-tests.el --- Tests for backtraces -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

;; Author: Gemini Lasswell

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

(require 'backtrace)
(require 'ert)
(require 'ert-x)
(require 'seq)

;; Delay evaluation of the backtrace-creating functions until
;; load so that the backtraces are the same whether this file
;; is compiled or not.

(eval-and-compile
  (defconst backtrace-tests--uncompiled-functions
    '(progn
       (defun backtrace-tests--make-backtrace (arg)
         (backtrace-tests--setup-buffer))

       (defun backtrace-tests--setup-buffer ()
         "Set up the current buffer in backtrace mode."
         (backtrace-mode)
         (setq backtrace-frames (backtrace-get-frames))
         (let ((this-index))
           ;; Discard all past `backtrace-tests-make-backtrace'.
           (dotimes (index (length backtrace-frames))
             (when (eq (backtrace-frame-fun (nth index backtrace-frames))
                       'backtrace-tests--make-backtrace)
               (setq this-index index)))
           (setq backtrace-frames (seq-subseq backtrace-frames 0 (1+ this-index))))
         (backtrace-print))))

  (eval backtrace-tests--uncompiled-functions))

(defun backtrace-tests--backtrace-lines ()
  (if debugger-stack-frame-as-list
      '("  (backtrace-get-frames)\n"
        "  (setq backtrace-frames (backtrace-get-frames))\n"
        "  (backtrace-tests--setup-buffer)\n"
        "  (backtrace-tests--make-backtrace %s)\n")
    '("  backtrace-get-frames()\n"
      "  (setq backtrace-frames (backtrace-get-frames))\n"
      "  backtrace-tests--setup-buffer()\n"
      "  backtrace-tests--make-backtrace(%s)\n")))

(defconst backtrace-tests--line-count (length (backtrace-tests--backtrace-lines)))

(defun backtrace-tests--backtrace-lines-with-locals ()
  (let ((lines (backtrace-tests--backtrace-lines))
        (locals '("    [no locals]\n"
                  "    [no locals]\n"
                  "    [no locals]\n"
                  "    arg = %s\n")))
    (apply #'append (cl-mapcar #'list lines locals))))

(defun backtrace-tests--result (value)
  (format (apply #'concat (backtrace-tests--backtrace-lines))
          (cl-prin1-to-string value)))

(defun backtrace-tests--result-with-locals (value)
  (let ((str (cl-prin1-to-string value)))
    (format (apply #'concat (backtrace-tests--backtrace-lines-with-locals))
            str str)))

;; TODO check that debugger-batch-max-lines still works

(defconst backtrace-tests--header "Test header\n")
(defun backtrace-tests--insert-header ()
  (insert backtrace-tests--header))

;;; Tests

(ert-deftest backtrace-tests--variables ()
  "Backtrace buffers can show and hide local variables."
  (ert-with-test-buffer (:name "variables")
    (let ((results (concat backtrace-tests--header
                           (backtrace-tests--result 'value)))
          (last-frame (format (nth (1- backtrace-tests--line-count)
                                   (backtrace-tests--backtrace-lines)) 'value))
          (last-frame-with-locals
           (format (apply #'concat (nthcdr (* 2 (1- backtrace-tests--line-count))
                                           (backtrace-tests--backtrace-lines-with-locals)))
                   'value 'value)))
      (backtrace-tests--make-backtrace 'value)
      (setq backtrace-insert-header-function #'backtrace-tests--insert-header)
      (backtrace-print)
      (should (string= (backtrace-tests--get-substring (point-min) (point-max))
                       results))
      ;; Go to the last frame.
      (goto-char (point-max))
      (forward-line -1)
      ;; Turn on locals for that frame.
      (backtrace-toggle-locals)
      (should (string= (backtrace-tests--get-substring (point) (point-max))
                       last-frame-with-locals))
      (should (string= (backtrace-tests--get-substring (point-min) (point-max))
                       (concat results
                               (format (car (last (backtrace-tests--backtrace-lines-with-locals)))
                                       'value))))
      ;; Turn off locals for that frame.
      (backtrace-toggle-locals)
      (should (string= (backtrace-tests--get-substring (point) (point-max))
                       last-frame))
      (should (string= (backtrace-tests--get-substring (point-min) (point-max))
                       results))
      ;; Turn all locals on.
      (backtrace-toggle-locals '(4))
      (should (string= (backtrace-tests--get-substring (point) (point-max))
                       last-frame-with-locals))
      (should (string= (backtrace-tests--get-substring (point-min) (point-max))
                       (concat backtrace-tests--header
                               (backtrace-tests--result-with-locals 'value))))
      ;; Turn all locals off.
      (backtrace-toggle-locals '(4))
      (should (string= (backtrace-tests--get-substring
                        (point) (+ (point) (length last-frame)))
                       last-frame))
      (should (string= (backtrace-tests--get-substring (point-min) (point-max))
                       results)))))

(ert-deftest backtrace-tests--backward-frame ()
  "`backtrace-backward-frame' moves backward to the start of a frame."
  (ert-with-test-buffer (:name "backward")
    (let ((results (concat backtrace-tests--header
                           (backtrace-tests--result nil))))
      (backtrace-tests--make-backtrace nil)
      (setq backtrace-insert-header-function #'backtrace-tests--insert-header)
      (backtrace-print)
      (should (string= (backtrace-tests--get-substring (point-min) (point-max))
                       results))

      ;; Try to move backward from header.
      (goto-char (+ (point-min) (/ (length backtrace-tests--header) 2)))
      (let ((pos (point)))
        (should-error (backtrace-backward-frame))
        (should (= pos (point))))

      ;; Try to move backward from start of first line.
      (forward-line)
      (let ((pos (point)))
        (should-error (backtrace-backward-frame))
        (should (= pos (point))))

      ;; Move backward from middle of line.
      (let ((start (point)))
        (forward-char (/ (length (nth 0 (backtrace-tests--backtrace-lines))) 2))
        (backtrace-backward-frame)
        (should (= start (point))))

      ;; Move backward from end of buffer.
      (goto-char (point-max))
      (backtrace-backward-frame)
      (let* ((last (format (car (last (backtrace-tests--backtrace-lines))) nil))
             (len (length last)))
        (should (string= (buffer-substring-no-properties (point) (+ (point) len))
                         last)))

      ;; Move backward from start of line.
      (backtrace-backward-frame)
      (let* ((line (car (last (backtrace-tests--backtrace-lines) 2)))
             (len (length line)))
        (should (string= (buffer-substring-no-properties (point) (+ (point) len))
                         line))))))

(ert-deftest backtrace-tests--forward-frame ()
  "`backtrace-forward-frame' moves forward to the start of a frame."
  (ert-with-test-buffer (:name "forward")
    (let* ((arg '(1 2 3))
           (results (concat backtrace-tests--header
                            (backtrace-tests--result arg)))
           (first-line (nth 0 (backtrace-tests--backtrace-lines))))
      (backtrace-tests--make-backtrace arg)
      (setq backtrace-insert-header-function #'backtrace-tests--insert-header)
      (backtrace-print)
      (should (string= (backtrace-tests--get-substring (point-min) (point-max))
                       results))
      ;; Move forward from header.
      (goto-char (+ (point-min) (/ (length backtrace-tests--header) 2)))
      (backtrace-forward-frame)
      (should (string= (backtrace-tests--get-substring
                        (point) (+ (point) (length first-line)))
                       first-line))

      (let ((start (point))
            (offset (/ (length first-line) 2))
            (second-line (nth 1 (backtrace-tests--backtrace-lines))))
        ;; Move forward from start of first frame.
        (backtrace-forward-frame)
        (should (string= (backtrace-tests--get-substring
                          (point) (+ (point) (length second-line)))
                         second-line))
        ;; Move forward from middle of first frame.
        (goto-char (+ start offset))
        (backtrace-forward-frame)
        (should (string= (backtrace-tests--get-substring
                          (point) (+ (point) (length second-line)))
                         second-line)))
      ;; Try to move forward from middle of last frame.
      (goto-char (- (point-max)
                    (/ 2 (length (car (last (backtrace-tests--backtrace-lines)))))))
      (should-error (backtrace-forward-frame))
      ;; Try to move forward from end of buffer.
      (goto-char (point-max))
      (should-error (backtrace-forward-frame)))))

(ert-deftest backtrace-tests--single-and-multi-line ()
  "Forms in backtrace frames can be on a single line or on multiple lines."
  (ert-with-test-buffer (:name "single-multi-line")
    (let* ((arg '(lambda (x)  ; Quote this so it isn't made into a closure.
                   (let ((number (1+ x)))
                     (+ x number))))
           (header-string "Test header: ")
           (header (format "%s%s\n" header-string arg))
           (insert-header-function (lambda ()
                                     (insert header-string)
                                     (insert (backtrace-print-to-string arg))
                                     (insert "\n")))
           (results (concat header (backtrace-tests--result arg)))
           (last-line (format (nth (1- backtrace-tests--line-count)
                                   (backtrace-tests--backtrace-lines))
                              arg))
           (last-line-locals (format (nth (1- (* 2 backtrace-tests--line-count))
                                          (backtrace-tests--backtrace-lines-with-locals))
                                     arg)))

      (backtrace-tests--make-backtrace arg)
      (setq backtrace-insert-header-function insert-header-function)
      (backtrace-print)
      (should (string= (backtrace-tests--get-substring (point-min) (point-max))
                       results))
      ;; Check pp and collapse for the form in the header.
      (goto-char (point-min))
      (backtrace-tests--verify-single-and-multi-line header)
      ;; Check pp and collapse for the last frame.
      (goto-char (point-max))
      (backtrace-backward-frame)
      (backtrace-tests--verify-single-and-multi-line last-line)
      ;; Check pp and collapse for local variables in the last line.
      (goto-char (point-max))
      (backtrace-backward-frame)
      (backtrace-toggle-locals)
      (forward-line)
      (backtrace-tests--verify-single-and-multi-line last-line-locals))))

(defun backtrace-tests--verify-single-and-multi-line (line)
  "Verify that `backtrace-single-line' and `backtrace-multi-line' work at point.
Point should be at the beginning of a line, and LINE should be a
string containing the text of the line at point.  Assume that the
line contains the strings \"lambda\" and \"number\"."
  (let ((pos (point)))
    (backtrace-multi-line)
    ;; Verify point is still at the start of the line.
    (should (= pos (point))))

  ;; Verify the form now spans multiple lines.
  (let ((pos (point)))
    (search-forward "number")
    (should-not (= pos (point-at-bol))))
  ;; Collapse the form.
  (backtrace-single-line)
  ;; Verify that the form is now back on one line,
  ;; and that point is at the same place.
  (should (string= (backtrace-tests--get-substring
                    (- (point) 6) (point)) "number"))
  (should-not (= (point) (point-at-bol)))
  (should (string= (backtrace-tests--get-substring
                    (point-at-bol) (1+ (point-at-eol)))
                   line)))

(ert-deftest backtrace-tests--print-circle ()
  "Backtrace buffers can toggle `print-circle' syntax."
  (ert-with-test-buffer (:name "print-circle")
    (let* ((print-circle nil)
           (arg (let ((val (make-list 5 'a))) (nconc val val) val))
           (results (backtrace-tests--make-regexp
                     (backtrace-tests--result arg)))
           (results-circle (regexp-quote (let ((print-circle t))
                                           (backtrace-tests--result arg))))
           (last-frame (backtrace-tests--make-regexp
                        (format (nth (1- backtrace-tests--line-count)
                                     (backtrace-tests--backtrace-lines))
                                arg)))
           (last-frame-circle (regexp-quote
                               (let ((print-circle t))
                                 (format (nth (1- backtrace-tests--line-count)
                                              (backtrace-tests--backtrace-lines))
                                         arg)))))
      (backtrace-tests--make-backtrace arg)
      (backtrace-print)
      (should (string-match-p results
                              (backtrace-tests--get-substring (point-min) (point-max))))
      ;; Go to the last frame.
      (goto-char (point-max))
      (forward-line -1)
      ;; Turn on print-circle for that frame.
      (backtrace-toggle-print-circle)
      (should (string-match-p last-frame-circle
                              (backtrace-tests--get-substring (point) (point-max))))
      ;; Turn off print-circle for the frame.
      (backtrace-toggle-print-circle)
      (should (string-match-p last-frame
                              (backtrace-tests--get-substring (point) (point-max))))
      (should (string-match-p results
                              (backtrace-tests--get-substring (point-min) (point-max))))
      ;; Turn print-circle on for the buffer.
      (backtrace-toggle-print-circle '(4))
      (should (string-match-p last-frame-circle
                              (backtrace-tests--get-substring (point) (point-max))))
      (should (string-match-p results-circle
                              (backtrace-tests--get-substring (point-min) (point-max))))
      ;; Turn print-circle off.
      (backtrace-toggle-print-circle '(4))
      (should (string-match-p last-frame
                              (backtrace-tests--get-substring
                               (point) (+ (point) (length last-frame)))))
      (should (string-match-p results
                              (backtrace-tests--get-substring (point-min) (point-max)))))))

(ert-deftest backtrace-tests--print-gensym ()
  "Backtrace buffers can toggle `print-gensym' syntax."
  (ert-with-test-buffer (:name "print-gensym")
    (let* ((print-gensym nil)
           (arg (list (gensym "first") (gensym) (gensym "last")))
           (results (backtrace-tests--make-regexp
                     (backtrace-tests--result arg)))
           (results-gensym (regexp-quote (let ((print-gensym t))
                                           (backtrace-tests--result arg))))
           (last-frame (backtrace-tests--make-regexp
                        (format (nth (1- backtrace-tests--line-count)
                                     (backtrace-tests--backtrace-lines))
                                arg)))
           (last-frame-gensym (regexp-quote
                               (let ((print-gensym t))
                                 (format (nth (1- backtrace-tests--line-count)
                                              (backtrace-tests--backtrace-lines))
                                         arg)))))
      (backtrace-tests--make-backtrace arg)
      (backtrace-print)
      (should (string-match-p results
                              (backtrace-tests--get-substring (point-min) (point-max))))
      ;; Go to the last frame.
      (goto-char (point-max))
      (forward-line -1)
      ;; Turn on print-gensym for that frame.
      (backtrace-toggle-print-gensym)
      (should (string-match-p last-frame-gensym
                              (backtrace-tests--get-substring (point) (point-max))))
      ;; Turn off print-gensym for the frame.
      (backtrace-toggle-print-gensym)
      (should (string-match-p last-frame
                              (backtrace-tests--get-substring (point) (point-max))))
      (should (string-match-p results
                              (backtrace-tests--get-substring (point-min) (point-max))))
      ;; Turn print-gensym on for the buffer.
      (backtrace-toggle-print-gensym '(4))
      (should (string-match-p last-frame-gensym
                              (backtrace-tests--get-substring (point) (point-max))))
      (should (string-match-p results-gensym
                              (backtrace-tests--get-substring (point-min) (point-max))))
      ;; Turn print-gensym off.
      (backtrace-toggle-print-gensym '(4))
      (should (string-match-p last-frame
                              (backtrace-tests--get-substring
                               (point) (+ (point) (length last-frame)))))
      (should (string-match-p results
                              (backtrace-tests--get-substring (point-min) (point-max)))))))

(defun backtrace-tests--make-regexp (str)
  "Make regexp from STR for `backtrace-tests--print-circle'.
Used for results of printing circular objects without
`print-circle' on.  Look for #n in string STR where n is any
digit and replace with #[0-9]."
  (let ((regexp (regexp-quote str)))
    (with-temp-buffer
      (insert regexp)
      (goto-char (point-min))
      (while (re-search-forward "#[0-9]" nil t)
        (replace-match "#[0-9]")))
    (buffer-string)))

(ert-deftest backtrace-tests--expand-ellipsis ()
  "Backtrace buffers ellipsify large forms as buttons which expand the ellipses."
  ;; make a backtrace with an ellipsis
  ;; expand the ellipsis
  (ert-with-test-buffer (:name "variables")
    (let* ((print-level nil)
           (print-length nil)
           (backtrace-line-length 300)
           (arg (make-list 40 (make-string 10 ?a)))
           (results (backtrace-tests--result arg)))
      (backtrace-tests--make-backtrace arg)
      (backtrace-print)

      ;; There should be an ellipsis. Find and expand it.
      (goto-char (point-min))
      (search-forward "...")
      (backward-char)
      (push-button)

      (should (string= (backtrace-tests--get-substring (point-min) (point-max))
                       results)))))

(ert-deftest backtrace-tests--expand-ellipses ()
  "Backtrace buffers ellipsify large forms and can expand the ellipses."
  (ert-with-test-buffer (:name "variables")
    (let* ((print-level nil)
           (print-length nil)
           (backtrace-line-length 300)
           (arg (let ((outer (make-list 40 (make-string 10 ?a)))
                      (nested (make-list 40 (make-string 10 ?b))))
                  (setf (nth 39 nested) (make-list 40 (make-string 10 ?c)))
                  (setf (nth 39 outer) nested)
                  outer))
           (results (backtrace-tests--result-with-locals arg)))

      ;; Make a backtrace with local variables visible.
      (backtrace-tests--make-backtrace arg)
      (backtrace-print)
      (backtrace-toggle-locals '(4))

      ;; There should be two ellipses.
      (goto-char (point-min))
      (should (search-forward "..."))
      (should (search-forward "..."))
      (should-error (search-forward "..."))

      ;; Expanding the last frame without argument should expand both
      ;; ellipses, but the expansions will contain one ellipsis each.
      (let ((buffer-len (- (point-max) (point-min))))
        (goto-char (point-max))
        (backtrace-backward-frame)
        (backtrace-expand-ellipses)
        (should (> (- (point-max) (point-min)) buffer-len))
        (goto-char (point-min))
        (should (search-forward "..."))
        (should (search-forward "..."))
        (should-error (search-forward "...")))

      ;; Expanding with argument should remove all ellipses.
      (goto-char (point-max))
      (backtrace-backward-frame)
      (backtrace-expand-ellipses '(4))
      (goto-char (point-min))

      (should-error (search-forward "..."))
      (should (string= (backtrace-tests--get-substring (point-min) (point-max))
                       results)))))


(ert-deftest backtrace-tests--to-string ()
  "Backtraces can be produced as strings."
  (let ((frames (ert-with-test-buffer (:name nil)
                  (backtrace-tests--make-backtrace "string")
                  backtrace-frames)))
    (should (string= (backtrace-to-string frames)
                     (backtrace-tests--result "string")))))

(defun backtrace-tests--get-substring (beg end)
  "Return the visible text between BEG and END.
Strip the string properties because it makes failed test results
easier to read."
  (substring-no-properties (filter-buffer-substring beg end)))

(provide 'backtrace-tests)

;;; backtrace-tests.el ends here
