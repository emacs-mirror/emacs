;; -*- lexical-binding:t -*-

;; Copyright (C) 2015-2026 Free Software Foundation, Inc.

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

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'hi-lock)


;; +===================================================================================+
;; | Framework
;; +===================================================================================+

(defmacro perf-define-constant-test (name &optional doc &rest body)
  (declare (indent 1) (debug (symbol &optional string &rest form)))
  `(progn
     (put ',name 'perf-constant-test t)
     (defun ,name nil ,doc ,@body)))

(defmacro perf-define-variable-test (name args &optional doc &rest body)
  (declare (indent 2) (debug defun))
  (unless (and (consp args)
               (= (length args) 1))
    (error "Function %s should accept exactly one argument." name))
  `(progn
     (put ',name 'perf-variable-test t)
     (defun ,name ,args ,doc ,@body)))

(defmacro perf-define-test-suite (name &rest tests)
  (declare (indent 1))
  `(put ',name 'perf-test-suite
        ,(cons 'list tests)))

(defun perf-constant-test-p (test)
  (get test 'perf-constant-test))

(defun perf-variable-test-p (test)
  (get test 'perf-variable-test))

(defun perf-test-suite-p (suite)
  (not (null (perf-test-suite-elements suite))))

(defun perf-test-suite-elements (suite)
  (get suite 'perf-test-suite))

(defun perf-expand-suites (test-and-suites)
  (apply #' append (mapcar (lambda (elt)
                             (if (perf-test-suite-p elt)
                                 (perf-test-suite-elements elt)
                               (list elt)))
                           test-and-suites)))
(defun perf-test-p (symbol)
  (or (perf-variable-test-p symbol)
      (perf-constant-test-p symbol)))

(defun perf-all-tests ()
  (let (result)
    (mapatoms (lambda (symbol)
                (when (and (fboundp symbol)
                           (perf-test-p symbol))
                  (push symbol result))))
    (sort result #'string-lessp)))

(defvar perf-default-test-argument 4096)

(defun perf-run-1 (&optional k n &rest tests)
  "Run TESTS K times using N as argument for non-constant ones.

Return test-total elapsed time."
  (random "")
  (when (and n (not (numberp n)))
    (push k tests)
    (push n tests)
    (setq n nil k nil))
  (when (and k (not (numberp k)))
    (push k tests)
    (setq k nil))
  (let* ((k (or k 1))
         (n (or n perf-default-test-argument))
         (tests (perf-expand-suites (or tests
                                        (perf-all-tests))))
         (variable-tests (seq-filter #'perf-variable-test-p tests))
         (constant-tests (seq-filter #'perf-constant-test-p tests))
         (max-test-string-width (perf-max-symbol-length tests)))
    (unless (seq-every-p #'perf-test-p tests)
      (error "Some of these are not tests: %s" tests))
    (cl-labels ((format-result (result)
                  (cond
                   ((numberp result) (format "%.2f" result))
                   ((stringp result) result)
                   ((null result) "N/A")))
                (format-test (fn)
                  (concat (symbol-name fn)
                          (make-string
                           (+ (- max-test-string-width
                                 (length (symbol-name fn)))
                              1)
                           ?\s)))
                (format-summary (results _total)
                  (let ((min (apply #'min results))
                        (max (apply #'max results))
                        (avg (/ (apply #'+ results) (float (length results)))))
                    (format "n=%d min=%.2f avg=%.2f max=%.2f" (length results) min avg max)))
                (run-test (fn)
                  (let ((total 0) results)
                    (dotimes (_ (max 0 k))
                      (garbage-collect)
                      (princ (concat " " (format-test fn)))
                      (let ((result  (condition-case-unless-debug err
                                         (cond
                                          ((perf-variable-test-p fn)
                                           (random "") (car (funcall fn n)))
                                          ((perf-constant-test-p fn)
                                           (random "") (car (funcall fn)))
                                          (t "skip"))
                                       (error (error-message-string err)))))
                        (when (numberp result)
                          (cl-incf total result)
                          (push result results))
                        (princ (format-result result))
                        (terpri)))
                    (when (> (length results) 1)
                      (princ (concat "#" (format-test fn)
                                     (format-summary results total)))
                      (terpri)))))
      (when variable-tests
        (terpri)
        (dolist (fn variable-tests)
          (run-test fn)
          (terpri)))
      (when constant-tests
        (dolist (fn constant-tests)
          (run-test fn)
          (terpri))))))

(defun perf-run (&optional k n &rest tests)
  (interactive
   (let* ((n (if current-prefix-arg
                 (prefix-numeric-value current-prefix-arg)
               perf-default-test-argument))
          (tests (mapcar #'intern
                         (completing-read-multiple
                          (format "Run tests (n=%d): " n)
                          (perf-all-tests) nil t nil 'perf-test-history))))
     (cons 1 (cons n tests))))
  (with-current-buffer (get-buffer-create "*perf-results*")
    (let ((inhibit-read-only t)
          (standard-output (current-buffer)))
      (erase-buffer)
      (apply #'perf-run-1 k n tests)
      (display-buffer (current-buffer)))))


(defun perf-batch-parse-command-line (args)
  (let ((k 1)
        (n perf-default-test-argument)
        tests)
    (while args
      (cond ((string-match-p "\\`-[cn]\\'" (car args))
             (unless (and (cdr args)
                          (string-match-p "\\`[0-9]+\\'" (cadr args)))
               (error "%s expects a natnum argument" (car args)))
             (if (equal (car args) "-c")
                 (setq k (string-to-number (cadr args)))
               (setq n (string-to-number (cadr args))))
             (setq args (cddr args)))
            (t (push (intern (pop args)) tests))))
    (list k n tests)))


(defun perf-run-batch ()
  "Runs tests from `command-line-args-left' and kill emacs."
  (let ((standard-output #'external-debugging-output))
    (condition-case err
        (cl-destructuring-bind (k n tests)
            (perf-batch-parse-command-line command-line-args-left)
          (apply #'perf-run-1 k n tests)
          (save-buffers-kill-emacs))
      (error
       (princ (error-message-string err))
       (save-buffers-kill-emacs)))))

(defconst perf-number-of-columns 70)

(defun perf-insert-lines (n)
  "Insert N lines into the current buffer."
  (dotimes (i n)
    (insert (make-string 70 (if (= (% i 2) 0)
                                ?.
                              ?O))
            ?\n)))

(defun perf-switch-to-buffer-scroll-random (n &optional buffer)
  (interactive)
  (set-window-buffer nil (or buffer (current-buffer)))
  (goto-char (point-min))
  (redisplay t)
  (dotimes (_ n)
    (goto-char (random (point-max)))
    (recenter)
    (redisplay t)))

(defun perf-insert-overlays (n &optional create-callback random-p)
  (if random-p
      (perf-insert-overlays-random n create-callback)
    (perf-insert-overlays-sequential n create-callback)))

(defun perf-insert-overlays-sequential (n &optional create-callback)
  "Insert an overlay every Nth line."
  (declare (indent 1))
  (let ((i 0)
        (create-callback (or create-callback #'ignore)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (= 0 (% i n))
          (let ((ov (make-overlay (point-at-bol) (point-at-eol))))
            (funcall create-callback ov)
            (overlay-put ov 'priority (random (buffer-size)))))
        (cl-incf i)
        (forward-line)))))

(defun perf-insert-overlays-random (n &optional create-callback)
  "Insert an overlay every Nth line."
  (declare (indent 1))
  (let ((create-callback (or create-callback #'ignore)))
    (save-excursion
      (while (>= (cl-decf n) 0)
        (let* ((beg (1+ (random (point-max))))
               (ov (make-overlay beg (+ beg (random 70)))))
          (funcall create-callback ov)
          (overlay-put ov 'priority (random (buffer-size))))))))

(defun perf-insert-overlays-hierarchical (n &optional create-callback)
  (let ((create-callback (or create-callback #'ignore)))
    (save-excursion
      (goto-char (point-min))
      (let ((spacing (floor (/ (/ (count-lines (point-min) (point-max))
                                  (float 3))
                               n))))
        (when (< spacing 1)
          (error "Hierarchical overlay overflow !!"))
        (dotimes (i n)
          (funcall create-callback
                   (make-overlay (point)
                                 (save-excursion
                                   (goto-char (point-max))
                                   (forward-line (- (* spacing i)))
                                   (point))))

          (when (eobp)
            (error "End of buffer in hierarchical overlays"))
          (forward-line spacing))))))

(defun perf-overlay-ascii-chart (&optional buffer width)
  (interactive)
  (save-current-buffer
    (when buffer (set-buffer buffer))
    (unless width (setq width 100))
    (let* ((ovl (sort (overlays-in (point-min) (point-max))
                      (lambda (ov1 ov2)
                        (or (<= (overlay-start ov1)
                                (overlay-start ov2))
                            (and
                             (= (overlay-start ov1)
                                (overlay-start ov2))
                             (< (overlay-end ov1)
                                (overlay-end ov2)))))))
           (ov-width (apply #'max (mapcar (lambda (ov)
                                            (- (overlay-end ov)
                                               (overlay-start ov)))
                                          ovl)))
           (ov-min (apply #'min (mapcar #'overlay-start ovl)))
           (ov-max (apply #'max (mapcar #'overlay-end ovl)))
           (scale (/ (float width) (+ ov-min ov-width))))
      (with-current-buffer (get-buffer-create "*overlay-ascii-chart*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (buffer-disable-undo)
          (insert (format "%06d%s%06d\n" ov-min (make-string (- width 12) ?\s) ov-max))
          (dolist (ov ovl)
            (let ((length (round (* scale (- (overlay-end ov)
                                             (overlay-start ov))))))
              (insert (make-string (round (* scale (overlay-start ov))) ?\s))
              (cl-case length
                (0 (insert "O"))
                (1 (insert "|"))
                (t (insert (format "|%s|" (make-string (- length 2) ?-)))))
              (insert "\n")))
          (goto-char (point-min)))
        (read-only-mode 1)
        (pop-to-buffer (current-buffer))))))

(defconst perf-overlay-faces (mapcar #'intern (seq-take hi-lock-face-defaults 3)))

(defun perf-overlay-face-callback (ov)
  (overlay-put ov 'face (nth (random (length perf-overlay-faces))
                             perf-overlay-faces)))

(defun perf-overlay-invisible-callback (ov)
  (overlay-put ov 'invisble (= 1 (random 2))))

(defun perf-overlay-display-callback (ov)
  (overlay-put ov 'display (make-string 70 ?*)))

(defmacro perf-define-display-test (overlay-type property-type scroll-type)
  (let ((name (intern (format "perf-display-%s/%s/%s"
                              overlay-type property-type scroll-type)))
        (arg (make-symbol "n")))

    `(perf-define-variable-test ,name (,arg)
       (with-temp-buffer
         (perf-insert-lines ,arg)
         (overlay-recenter (point-max))
         ,@(perf-define-display-test-1 arg overlay-type property-type scroll-type)))))

(defun perf-define-display-test-1 (arg overlay-type property-type scroll-type)
  (list (append (cl-case overlay-type
                  (sequential
                   (list 'perf-insert-overlays-sequential 2))
                  (hierarchical
                   `(perf-insert-overlays-hierarchical (/ ,arg 10)))
                  (random
                   `(perf-insert-overlays-random (/ ,arg 2)))
                  (t (error "Invalid insert type: %s" overlay-type)))
                (list
                 (cl-case property-type
                   (display '#'perf-overlay-display-callback)
                   (face '#'perf-overlay-face-callback)
                   (invisible '#'perf-overlay-invisible-callback)
                   (t (error "Invalid overlay type: %s" overlay-type)))))
        (list 'benchmark-run 1
              (cl-case scroll-type
                (scroll '(perf-switch-to-buffer-scroll-up-and-down))
                (random `(perf-switch-to-buffer-scroll-random (/ ,arg 50)))
                (t (error "Invalid scroll type: %s" overlay-type))))))

(defun perf-max-symbol-length (symbols)
  "Return the longest symbol in SYMBOLS, or -1 if symbols is nil."
  (if (null symbols)
      -1
    (apply #'max (mapcar
                  (lambda (elt)
                    (length (symbol-name elt)))
                  symbols))))

(defun perf-insert-text (n)
  "Insert N character into the current buffer."
  (let ((ncols 68)
        (char ?.))
    (dotimes (_  (/ n ncols))
      (insert (make-string (1- ncols) char) ?\n))
    (when (> (% n ncols) 0)
      (insert (make-string (1- (% n ncols)) char) ?\n))))

(defconst perf-insert-overlays-default-length 24)

(defun perf-insert-overlays-scattered (n &optional length)
  "Insert N overlays of max length 24 randomly."
  (dotimes (_ n)
    (let ((begin (random (1+ (point-max)))))
      (make-overlay
       begin (+ begin (random (1+ (or length perf-insert-overlays-default-length 0))))))))

(defvar perf-marker-gc-protection nil)

(defun perf-insert-marker-scattered (n)
  "Insert N marker randomly."
  (setq perf-marker-gc-protection nil)
  (dotimes (_ n)
    (push (copy-marker (random (1+ (point-max))))
          perf-marker-gc-protection)))

(defun perf-switch-to-buffer-scroll-up-and-down (&optional buffer)
  (interactive)
  (set-window-buffer nil (or buffer (current-buffer)))
  (goto-char (point-min))
  (redisplay t)
  (while (condition-case nil
             (progn (scroll-up) t)
           (end-of-buffer nil))
    (redisplay t))
  (while (condition-case nil
             (progn (scroll-down) t)
           (beginning-of-buffer nil))
    (redisplay t)))

(defun perf-emacs-lisp-setup ()
  (add-to-list 'imenu-generic-expression
               '(nil "^\\s-*(perf-define\\(?:\\w\\|\\s_\\)*\\s-*\\(\\(?:\\w\\|\\s_\\)+\\)" 1)))

(add-hook 'emacs-lisp-mode 'perf-emacs-lisp-setup)


;; +===================================================================================+
;; | Basic performance tests
;; +===================================================================================+

(perf-define-variable-test perf-make-overlay (n)
  (with-temp-buffer
    (overlay-recenter (point-min))
    (benchmark-run 1
      (dotimes (_ n)
        (make-overlay 1 1)))))

(perf-define-variable-test perf-make-overlay-continuous (n)
  (with-temp-buffer
    (perf-insert-text n)
    (overlay-recenter (point-max))
    (benchmark-run 1
      (dotimes (i n)
        (make-overlay i (1+ i))))))

(perf-define-variable-test perf-make-overlay-scatter (n)
  (with-temp-buffer
    (perf-insert-text n)
    (benchmark-run 1
      (perf-insert-overlays-scattered n))))

(perf-define-variable-test perf-delete-overlay (n)
  (with-temp-buffer
    (let ((ovls (cl-loop for i from 1 to n
                        collect (make-overlay 1 1))))
      (overlay-recenter (point-min))
      (benchmark-run 1
        (mapc #'delete-overlay ovls)))))

(perf-define-variable-test perf-delete-overlay-continuous (n)
  (with-temp-buffer
    (perf-insert-text n)
    (let ((ovls (cl-loop for i from 1 to n
                         collect (make-overlay i (1+ i)))))
      (overlay-recenter (point-min))
      (benchmark-run 1
        (mapc #'delete-overlay ovls)))))

(perf-define-variable-test perf-delete-overlay-scatter (n)
  (with-temp-buffer
    (perf-insert-text n)
    (let ((ovls (progn (perf-insert-overlays-scattered n)
                       (overlays-in (point-min) (point-max)))))
      (benchmark-run 1
        (mapc #'delete-overlay ovls)))))

(perf-define-variable-test perf-overlays-at (n)
  (with-temp-buffer
    (perf-insert-text n)
    (perf-insert-overlays-scattered n)
    (benchmark-run 1
      (dotimes (i (point-max))
        (overlays-at i)))))

(perf-define-variable-test perf-overlays-in (n)
  (with-temp-buffer
    (perf-insert-text n)
    (perf-insert-overlays-scattered n)
    (let ((len perf-insert-overlays-default-length))
      (benchmark-run 1
        (dotimes (i (- (point-max) len))
          (overlays-in i (+ i len)))))))

(perf-define-variable-test perf-insert-before (n)
  (with-temp-buffer
    (perf-insert-text n)
    (perf-insert-overlays-scattered n)
    (goto-char 1)
    (overlay-recenter (point-min))
    (benchmark-run 1
      (dotimes (_ (/ n 2))
        (insert ?X)))))

(perf-define-variable-test perf-insert-before-empty (n)
  (let ((perf-insert-overlays-default-length 0))
    (perf-insert-before n)))
(perf-define-variable-test perf-insert-after-empty (n)
  (let ((perf-insert-overlays-default-length 0))
    (perf-insert-after n)))
(perf-define-variable-test perf-insert-scatter-empty (n)
  (let ((perf-insert-overlays-default-length 0))
    (perf-insert-scatter n)))
(perf-define-variable-test perf-delete-before-empty (n)
  (let ((perf-insert-overlays-default-length 0))
    (perf-delete-before n)))
(perf-define-variable-test perf-delete-after-empty (n)
  (let ((perf-insert-overlays-default-length 0))
    (perf-delete-after n)))
(perf-define-variable-test perf-delete-scatter-empty (n)
  (let ((perf-insert-overlays-default-length 0))
    (perf-delete-scatter n)))

(defmacro perf-define-marker-test (type where)
  (let ((name (intern (format "perf-%s-%s-marker" type where))))
    `(perf-define-variable-test ,name (n)
       (with-temp-buffer
         (perf-insert-text n)
         (perf-insert-marker-scattered n)
         (goto-char ,(cl-case where
                       (after (list 'point-max))
                       (t (list 'point-min))))
         (benchmark-run 1
           (dotimes (_ (/ n 2))
             ,@(when (eq where 'scatter)
                 (list '(goto-char (max 1 (random (point-max))))))
             ,(cl-case type
                (insert (list 'insert ?X))
                (delete (list 'delete-char (if (eq where 'after) -1 1))))))))))

(perf-define-test-suite perf-marker-suite
  (perf-define-marker-test insert before)
  (perf-define-marker-test insert after)
  (perf-define-marker-test insert scatter)
  (perf-define-marker-test delete before)
  (perf-define-marker-test delete after)
  (perf-define-marker-test delete scatter))

(perf-define-variable-test perf-insert-after (n)
  (with-temp-buffer
    (perf-insert-text n)
    (perf-insert-overlays-scattered n)
    (goto-char (point-max))
    (overlay-recenter (point-max))
    (benchmark-run 1
      (dotimes (_ (/ n 2))
        (insert ?X)))))

(perf-define-variable-test perf-insert-scatter (n)
  (with-temp-buffer
    (perf-insert-text n)
    (perf-insert-overlays-scattered n)
    (goto-char (point-max))
    (benchmark-run 1
      (dotimes (_ (/ n 2))
        (goto-char (1+ (random (point-max))))
        (insert ?X)))))

(perf-define-variable-test perf-delete-before (n)
  (with-temp-buffer
    (perf-insert-text n)
    (perf-insert-overlays-scattered n)
    (goto-char 1)
    (overlay-recenter (point-min))
    (benchmark-run 1
      (dotimes (_ (/ n 2))
        (delete-char 1)))))

(perf-define-variable-test perf-delete-after (n)
  (with-temp-buffer
    (perf-insert-text n)
    (perf-insert-overlays-scattered n)
    (goto-char (point-max))
    (overlay-recenter (point-max))
    (benchmark-run 1
      (dotimes (_ (/ n 2))
        (delete-char -1)))))

(perf-define-variable-test perf-delete-scatter (n)
  (with-temp-buffer
    (perf-insert-text n)
    (perf-insert-overlays-scattered n)
    (goto-char (point-max))
    (benchmark-run 1
      (dotimes (_ (/ n 2))
        (goto-char (max 1 (random (point-max))))
        (delete-char 1)))))

(perf-define-test-suite perf-insert-delete-suite
  'perf-insert-before
  'perf-insert-after
  'perf-insert-scatter
  'perf-delete-before
  'perf-delete-after
  'perf-delete-scatter
  )


;; +===================================================================================+
;; | Redisplay (new)
;; +===================================================================================+

;; 5000
;; 25000
;; 75000

;; Number of Overlays =  N / 2
;;
;; (except for the hierarchical case, where it is divided by 10.)

  ;; . scrolling through a buffer with lots of overlays that affect faces
  ;;   of characters in the buffer text
  ;; . scrolling through a buffer with lots of overlays that define
  ;;   'display' properties which are strings
  ;; . scrolling through a buffer with lots of overlays that define
  ;;   'invisible' properties

(perf-define-test-suite perf-display-suite
  (perf-define-display-test sequential display scroll)
  (perf-define-display-test sequential display random)
  (perf-define-display-test sequential face scroll)
  (perf-define-display-test sequential face random)
  (perf-define-display-test sequential invisible scroll)
  (perf-define-display-test sequential invisible random)
  (perf-define-display-test random display scroll)
  (perf-define-display-test random display random)
  (perf-define-display-test random face scroll)
  (perf-define-display-test random face random)
  (perf-define-display-test random invisible scroll)
  (perf-define-display-test random invisible random))

;; |------------|
;;   |--------|
;;     |----|
(perf-define-display-test hierarchical face scroll)




;; +===================================================================================+
;; | Real World
;; +===================================================================================+

(require 'python)

(defconst perf-many-errors-file
  (expand-file-name "many-errors.py"
                    (and load-file-name (file-name-directory load-file-name))))

(perf-define-constant-test perf-realworld-flycheck
  (interactive)
  (package-initialize)
  (when (and (require 'flycheck nil t)
             (file-exists-p perf-many-errors-file)
             (or (executable-find "pylint")
                 (executable-find "flake8")))
    (setq flycheck-python-pylint-executable
          (executable-find "pylint"))
    (setq flycheck-python-flake8-executable
          (executable-find "flake8"))
    (setq python-indent-guess-indent-offset-verbose nil)
    (setq flycheck-check-syntax-automatically nil)
    (setq flycheck-checker-error-threshold nil)
    (setq flycheck-display-errors-function nil)
    (with-current-buffer (find-file-noselect perf-many-errors-file)
      (let* ((done)
             (flycheck-after-syntax-check-hook
              (list (lambda () (setq done t)))))
        (flycheck-mode 1)
        (flycheck-buffer)
        (benchmark-run 1
          (while (not done)
            (accept-process-output))
          (perf-switch-to-buffer-scroll-up-and-down)
          (flycheck-mode -1))))))

;; https://lists.gnu.org/archive/html/emacs-devel/2009-04/msg00242.html
(defun make-lines-invisible (regexp &optional arg)
  "Make all lines matching a regexp invisible and intangible.
With a prefix arg, make it visible again.  It is not necessary
that REGEXP matches the whole line; if a hit is found, the
affected line gets automatically selected.

This command affects the whole buffer."
  (interactive "MRegexp: \nP")
  (let (ov
        ovs
        count)
    (cond
     ((equal arg '(4))
      (setq ovs (overlays-in (point-min) (point-max)))
      (mapc (lambda (o)
              (if (overlay-get o 'make-lines-invisible)
                  (delete-overlay o)))
            ovs))
     (t
      (save-excursion
        (goto-char (point-min))
        (setq count 0)
        (while (re-search-forward regexp nil t)
          (setq count (1+ count))
          (if (= (% count 100) 0)
              (message "%d" count))
          (setq ov (make-overlay (line-beginning-position)
                                 (1+ (line-end-position))))
          (overlay-put ov 'make-lines-invisible t)
          (overlay-put ov 'invisible t)
          (overlay-put ov 'intangible t)
          (goto-char (line-end-position))))))))

(perf-define-constant-test perf-realworld-make-lines-invisible
  (with-temp-buffer
    (insert-file-contents "/usr/share/dict/words")
    (set-window-buffer nil (current-buffer))
    (redisplay t)
    (overlay-recenter (point-max))
    (benchmark-run 1
      (make-lines-invisible "a"))))

(perf-define-constant-test perf-realworld-line-numbering
  (interactive)
  (with-temp-buffer
    (insert-file-contents "/usr/share/dict/words")
    (overlay-recenter (point-max))
    (goto-char (point-min))
    (let* ((nlines (count-lines (point-min) (point-max)))
           (line 1)
           (width 0))
      (dotimes (i nlines) ;;-with-progress-reporter "Creating overlays"
        (let ((ov (make-overlay (point) (point)))
              (str (propertize (format "%04d" line) 'face 'shadow)))
          (overlay-put ov 'before-string
                       (propertize " " 'display `((margin left-margin) ,str)))
          (setq width (max width (length str)))
          (cl-incf line)
          (forward-line)))
      (benchmark-run 1
        (let ((left-margin-width width))
          (perf-switch-to-buffer-scroll-up-and-down))))))

(perf-define-test-suite perf-realworld-suite
  'perf-realworld-flycheck
  'perf-realworld-make-lines-invisible
  'perf-realworld-line-numbering)


;; +===================================================================================+
;; | next-overlay-change
;; +===================================================================================+

(perf-define-variable-test perf-noc-hierarchical/forward/linear (n)
  "Search linear for the next change on every line."
  (with-temp-buffer
    (perf-insert-lines (* 3 n))
    (perf-insert-overlays-hierarchical n)
    (goto-char (point-min))
    (benchmark-run 1
      (while (not (eobp))
        (next-overlay-change (point))
        (forward-line)))))

(perf-define-variable-test perf-noc-sequential/forward/linear (n)
  "Search linear for the next change on every line."
  (with-temp-buffer
    (perf-insert-lines (* 3 n))
    (perf-insert-overlays-sequential n)
    (goto-char (point-min))
    (benchmark-run 1
      (while (not (eobp))
        (next-overlay-change (point))
        (forward-line)))))

(perf-define-variable-test perf-noc-hierarchical/forward/backnforth (n)
  "Search back and forth for the next change from `point-min' to `point-max'."
  (with-temp-buffer
    (perf-insert-lines (* 3 n))
    (overlay-recenter (point-max))
    (perf-insert-overlays-hierarchical n)
    (goto-char (point-min))
    (benchmark-run 1
      (while (not (eobp))
        (next-overlay-change (point))
        (next-overlay-change (+ (point) 2))
        (forward-char)))))

(perf-define-test-suite perf-noc-suite
  'perf-noc-hierarchical/forward/linear
  'perf-noc-hierarchical/forward/backnforth
  'perf-noc-hierarchical/forward/backnforth)
