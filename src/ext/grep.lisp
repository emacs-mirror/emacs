(defpackage :lem/grep
  (:use :cl
        :lem)
  (:export
   :grep
   :*grep-command*
   :*grep-args*)
  #+sbcl
  (:lock t))
(in-package :lem/grep)

(defun run-grep (string directory)
  (multiple-value-bind (output error-output status-code)
      (uiop:run-program string
                        :directory directory
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (cond ((eql status-code 0)
           output)
          ((eql status-code 1)
           "")
          (t
           (editor-error "~A"
                         (string-right-trim '(#\newline #\space)
                                            error-output))))))

(defun parse-grep-result (text)
  (let* ((text (string-right-trim '(#\newline) text))
         (lines (uiop:split-string text :separator '(#\newline)))
         (file-line-content-tuples
           (mapcar (lambda (line)
                     (destructuring-bind (file line-number content)
                         (ppcre:split ":" line :limit 3)
                       (list file
                             (parse-integer line-number)
                             content)))
                   lines)))
    file-line-content-tuples))

(defun move (directory file line-number)
  (let ((buffer (find-file-buffer (merge-pathnames file directory))))
    (move-to-line (buffer-point buffer) line-number)))

(defun make-move-function (directory file line-number)
  (lambda ()
    (move directory file line-number)))

(defun get-content-string (start)
  (with-point ((start start)
               (end start))
    (line-start start)
    (next-single-property-change start :content-start)
    (character-offset start 1)
    (line-end end)
    (points-to-string start end)))

(defun change-grep-buffer (start end old-len)
  (declare (ignore end old-len))
  (let ((string (get-content-string start))
        (move (lem/peek-source:get-move-function start)))
    (when move
      (with-point ((point (funcall move)))
        (with-point ((start point)
                     (end point))
          (line-start start)
          (line-end end)
          (buffer-undo-boundary (point-buffer start))
          (delete-between-points start end)
          (insert-string start string)
          (buffer-undo-boundary (point-buffer start))))))
  (lem/peek-source:show-matched-line))

(defvar *grep-command* "git grep")
(defvar *grep-args* "-nHI")
(defvar *last-query* (str:concat *grep-command* " " *grep-args* " "))
(defvar *last-directory* nil)

(define-command grep (query &optional (directory (buffer-directory)))
    ((prompt-for-string "" :initial-value *last-query* :history-symbol 'grep)
     (princ-to-string (prompt-for-directory "Directory: "
                                            :directory (if *last-directory*
                                                           (princ-to-string *last-directory*)
                                                           (buffer-directory)))))
  "Run an interactive grep."
  (let* ((directory (uiop:ensure-directory-pathname directory))
         (result (parse-grep-result (run-grep query directory))))
    (if (null result)
        (editor-error "No match")
        (lem/peek-source:with-collecting-sources (collector :read-only nil)
          (loop :for (file line-number content) :in result
                :do (lem/peek-source:with-appending-source
                        (point :move-function (make-move-function directory file line-number))
                      (insert-string point file :attribute 'lem/peek-source:filename-attribute
                                     :mode 'peek-grep-mode
                                     :read-only t)
                      (insert-string point ":" :read-only t)
                      (insert-string point (princ-to-string line-number)
                                     :attribute 'lem/peek-source:position-attribute
                                     :read-only t)
                      (insert-string point ":" :read-only t :content-start t)
                      (insert-string point content)))
          (add-hook (variable-value 'after-change-functions :buffer (lem/peek-source:collector-buffer collector))
                    'change-grep-buffer)))
    (setf *last-query* query
          *last-directory* directory)))

(define-command project-grep () ()
  "Run grep at the project root directory."
  (let* ((cwd (buffer-directory))
         (project-root (lem-core/commands/project:find-root cwd))
         (root (or project-root cwd))
         (query (prompt-for-string "" :initial-value *last-query* :history-symbol 'grep)))
    (grep query root)))

(define-command grep-move-to-content-start () ()
  "Move to the first non-whitespace content character in the current line."
  (with-point ((p (current-point)))
    (line-start p)
    (loop while (and (not (end-line-p p))
                     (not (text-property-at p :content-start)))
          do (character-offset p 1))
    (move-point (current-point) p)
    ;; Skip trailing ":" and whitespace
    (forward-char)
    (skip-whitespace-forward (current-point) t)))

(define-command grep-help () ()
  "Show grep help."
  (with-pop-up-typeout-window (s (make-buffer "*Help*") :erase t)
    (format s "grep command (M-x grep)~&")
    (format s "~%")
    (format s "The left window shows grep results, the right window shows a result in its source file.~&")
    (format s "~%")
    (format s "Available keybindings:~&")
    (format s "- up/down arrows, n/p, or C-p/C-n: go to the previous/next line~&")
    (format s "- a: move to the content in the current line~&")
    (format s "- C-x o or M-o: go to the other window~&")
    (format s "- Enter: visit the file of the result at point~&")
    (format s "- Escape or C-x 0: quit~&")
    (format s "- C-x ?: bring this help~&")
    (format s "~%")
    (format s "The results buffer on the left is editable. Any change is written to file and is reflected immediately on the right.~&")
    (format s "You can use editing tools such as M-x query-replace in the results buffer.~&")
    (format s "~%")))

(defvar *peek-grep-mode-keymap* (make-keymap :name '*peek-grep-mode-keymap* 
                                             :parent lem/peek-source:*peek-source-keymap*))
(define-minor-mode peek-grep-mode
    (:name "Peek"
     :keymap *peek-grep-mode-keymap*))

(define-key *global-keymap* "C-x p g" 'project-grep)
(define-key *peek-grep-mode-keymap* "C-x ?" 'grep-help)  ;; originally bound to describe-key.
(define-key *peek-grep-mode-keymap* "n" 'lem/peek-source:peek-source-next)
(define-key *peek-grep-mode-keymap* "p" 'lem/peek-source:peek-source-previous)
(define-key *peek-grep-mode-keymap* "a" 'grep-move-to-content-start)
