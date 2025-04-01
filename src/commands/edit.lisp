(defpackage :lem-core/commands/edit
  (:use :cl
        :lem/common/killring
        :lem-core
        :lem-core/commands/move)
  (:export :process-input-character
           :get-self-insert-char
           :self-insert-before-hook
           :self-insert-after-hook
           :self-insert
           :quoted-insert
           :newline
           :open-line
           :delete-next-char
           :delete-previous-char
           :copy-region
           :copy-region-to-clipboard
           :kill-region
           :kill-region-to-clipboard
           :kill-line
           :kill-whole-line
           :yank
           :yank-pop
           :yank-pop-next
           :yank-to-clipboard
           :paste-from-clipboard
           :entab-line
           :detab-line
           :delete-blank-lines
           :just-one-space
           :delete-indentation
           :transpose-characters
           :undo
           :redo
           :delete-trailing-whitespace)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/edit)

(setf (keymap-undef-hook *global-keymap*) 'self-insert)

(define-key *global-keymap* "C-q" 'quoted-insert)
(define-key *global-keymap* "Return" 'newline)
(define-key *global-keymap* "C-o" 'open-line)
(define-key *global-keymap* "C-d" 'delete-next-char)
(define-key *global-keymap* "Delete" 'delete-next-char)
(define-key *global-keymap* "C-h" 'delete-previous-char)
(define-key *global-keymap* "Backspace" 'delete-previous-char)
(define-key *global-keymap* "M-w" 'copy-region)
(define-key *global-keymap* "C-w" 'kill-region)
(define-key *global-keymap* "C-k" 'kill-line)
(define-key *global-keymap* "C-Shift-Backspace" 'kill-whole-line)
(define-key *global-keymap* "C-y" 'yank)
(define-key *global-keymap* "M-y" 'yank-pop)
(define-key *global-keymap* "C-x C-o" 'delete-blank-lines)
(define-key *global-keymap* "M-Space" 'just-one-space)
(define-key *global-keymap* "M-^" 'delete-indentation)
(define-key *global-keymap* "C-t" 'transpose-characters)
(define-key *global-keymap* "C-\\" 'undo)
(define-key *global-keymap* "C-_" 'redo)
(define-key *global-keymap* "C-/" 'redo)

(define-editor-variable self-insert-before-hook '())
(define-editor-variable self-insert-after-hook '())

(defun get-self-insert-char ()
  (insertion-key-p (last-read-key-sequence)))

(defclass self-insert-advice () ())

(defgeneric process-input-character (char n))

(define-command (self-insert (:advice-classes self-insert-advice editable-advice))
    (&optional (n 1) (char (get-self-insert-char)))
    (:universal (get-self-insert-char))
  "Processes the key entered."
  (process-input-character char n))

(defmethod process-input-character (char n)
  (unless (get-self-insert-char)
    (error 'undefined-key-error))
  (run-hooks (variable-value 'self-insert-before-hook) char)
  (self-insert-aux char n)
  (run-hooks (variable-value 'self-insert-after-hook) char))

(defun self-insert-aux (char n &optional sticky)
  (insert-character (current-point) char n)
  (when sticky
    (character-offset (current-point) (- n))))

(define-command (newline (:advice-classes editable-advice)) (&optional (n 1)) (:universal)
  "Insert a new line."
  (self-insert-aux #\newline n))

(define-command (open-line (:advice-classes editable-advice)) (n) (:universal)
  "Insert a new line without moving the cursor position."
  (self-insert-aux #\newline n t))

(define-command quoted-insert (&optional (n 1)) (:universal)
  "Insert the next entered key (including control characters)."
  (let* ((key (read-key))
         (char (or (key-to-char key) (code-char 0))))
    (self-insert-aux char n)))

(defmethod execute :around (mode
                            (command quoted-insert)
                            argument)
  (let* ((key (read-key))
         (char (or (key-to-char key) (code-char 0))))
    (do-each-cursors ()
      (self-insert-aux char (or argument 1)))))


(define-command (delete-next-char (:advice-classes editable-advice)) (&optional n) (:universal-nil)
  "Delete the next character."
  (unless (end-buffer-p (current-point))
    (let ((repeat-command (continue-flag :kill))
          (killp (not (null n)))
          (killed-string (delete-character (current-point) (or n 1))))
      (when killp
        (with-killring-context (:appending repeat-command)
          (copy-to-clipboard-with-killring killed-string))))))

(defun delete-cursor-region (point)
  (let ((start (cursor-region-beginning point))
        (end (cursor-region-end point)))
    (delete-character start (count-characters start end))))

(defun delete-previous-char-1 (n)
  (backward-char (or n 1))
  (handler-case (with-killring-context (:before-inserting t)
                  (delete-next-char n))
    (read-only-error (e)
      (forward-char (or n 1))
      (error e))))

(define-command (delete-previous-char (:advice-classes editable-advice)) (&optional n) (:universal-nil)
  "Delete the previous character."
  (cond ((mark-active-p (cursor-mark (current-point)))
         (delete-cursor-region (current-point)))
        (t
         (delete-previous-char-1 n))))

(defun copy-cursor-region (point)
  (with-killring-context (:appending (continue-flag :kill))
    (let ((start (cursor-region-beginning point))
          (end (cursor-region-end point)))
      (copy-to-clipboard-with-killring (points-to-string start end)))
    (mark-cancel (cursor-mark point))))

(define-command copy-region (start end) (:region)
  "Copy the text of region."
  (with-killring-context (:appending (continue-flag :kill))
    (copy-to-clipboard-with-killring (points-to-string start end)))
  (buffer-mark-cancel (current-buffer)))

(define-command copy-region-to-clipboard (start end) (:region)
  "Copy the selected text to the clipboard."
  (copy-to-clipboard (points-to-string start end)))

(defun kill-cursor-region (point)
  (let* ((start (cursor-region-beginning point))
         (end (cursor-region-end point))
         (killed-string (delete-character start (count-characters start end))))
    (with-killring-context (:appending (continue-flag :kill))
      (copy-to-clipboard-with-killring killed-string))
    (mark-cancel (cursor-mark point))))

(define-command kill-region (start end) (:region)
  "Kill the text of region."
  (when (point< end start)
    (rotatef start end))
  (let ((repeat-command (continue-flag :kill))
        (killed-string (delete-character start (count-characters start end))))
    (with-killring-context (:appending repeat-command)
      (when (and (not repeat-command)
                 (enable-clipboard-p))
        (let ((clipboard-string (get-clipboard-data)))
          (unless (string= clipboard-string (peek-killring-item (current-killring) 0))
            (push-killring-item (current-killring) clipboard-string))))
      (copy-to-clipboard-with-killring killed-string))))

(define-command kill-region-to-clipboard (start end) (:region)
  "Kill the text of region and copy to the clipboard."
  (copy-region-to-clipboard start end)
  (delete-character start (count-characters start end)))

(define-command (kill-line (:advice-classes editable-advice)) (&optional arg) (:universal-nil)
  "Kill from the current cursor position to the end of the line."
  (flet ((end-line-p* (p)
           (with-point ((p p))
             (skip-whitespace-forward p t)
             (end-line-p p)))
         (start-line-p* (p)
           (when (start-line-p p)
             (with-point ((p p))
               (skip-whitespace-forward p t)
               (end-line-p p)))))
    (save-excursion
      (with-point ((start (current-point) :right-inserting))
        (cond
          ((null arg)
           (let ((p (current-point)))
             (cond ((end-buffer-p p)
                    (error 'end-of-buffer :point p))
                   ((start-line-p* p)
                    (line-offset p 1))
                   ((end-line-p* p)
                    (if (line-offset p 1)
                        (skip-whitespace-forward p t)
                        (line-end p)))
                   (t (line-end p)))
             (kill-region start p)))
          (t
           (or (line-offset (current-point) arg)
               (buffer-end (current-point)))
           (let ((end (current-point)))
             (kill-region start end))))))))

(define-command kill-whole-line (&optional (n 1)) (:universal)
  "If n is positive, kill n whole lines forward starting at the
beginning of the current line.  If n is 0, do nothing.  And if n
is negative, kill n lines above without deleting anything on the
current line."
  (cond ((zerop n) nil)
        ((minusp n) (save-excursion
                      (move-to-beginning-of-logical-line)
                      (kill-line n)))
        (t (progn (move-to-beginning-of-logical-line)
                  (kill-line n)))))

(defun yank-string (point string)
  (change-yank-start point
                     (copy-point point :right-inserting))
  (if (in-string-or-comment-p point)
      (insert-string point string)
      (insert-string-and-indent point string))
  (change-yank-end point
                   (copy-point point :left-inserting))
  (continue-flag :yank))

(defun yank-1 (arg)
  (let ((string (if (null arg)
                    (yank-from-clipboard-or-killring)
                    (peek-killring-item (current-killring) (1- arg)))))
    (when string
      (yank-string (current-point) string))))

(define-command yank (&optional arg) (:universal-nil)
  "Paste the copied text."
  (yank-1 arg))

(define-command (yank-pop (:advice-classes editable-advice)) (&optional n) (:universal)
  "Replaces the immediately pasted text with the next text in the killring."
  (let ((start (cursor-yank-start (current-point)))
        (end (cursor-yank-end (current-point)))
        (prev-yank-p (continue-flag :yank)))
    (cond ((and start end prev-yank-p)
           (delete-between-points start end)
           (rotate-killring (current-killring))
           (yank-1 n))
          (t
           (message "Previous command was not a yank")
           nil))))

(define-command (yank-pop-next (:advice-classes editable-advice)) (&optional n) (:universal)
  "Replaces the immediately preceding yank-pop text with the text before the kill ring."
  (let ((start (cursor-yank-start (current-point)))
        (end (cursor-yank-end (current-point)))
        (prev-yank-p (continue-flag :yank)))
    (cond ((and start end prev-yank-p)
           (delete-between-points start end)
           (rotate-killring-undo (current-killring))
           (yank-1 n))
          (t
           (message "Previous command was not a yank")
           nil))))

(define-command yank-to-clipboard (&optional arg) (:universal)
  "Copy the text of the killring to the clipboard."
  (let ((string
          (peek-killring-item (current-killring)
                              (if (null arg) 0 (1- arg)))))
    (copy-to-clipboard string)))

(define-command (paste-from-clipboard (:advice-classes editable-advice)) () ()
  "Inserts text from the clipboard."
  (insert-string (current-point) (get-clipboard-data)))

(defun tab-line-aux (n make-space-str)
  (let ((p (current-point)))
    (dotimes (_ n t)
      (with-point ((p2 (back-to-indentation p)))
        (let ((count (point-column p2)))
          (multiple-value-bind (div mod)
              (floor count (variable-value 'tab-width))
            (line-start p)
            (delete-between-points p p2)
            (insert-string p (funcall make-space-str div))
            (insert-character p #\space mod)))
        (unless (line-offset p 1)
          (return))))))

(define-command (entab-line (:advice-classes editable-advice)) (n) (:universal)
  "Replaces the indent of the current line from space to tab."
  (tab-line-aux n
                #'(lambda (n)
                    (make-string n :initial-element #\tab))))

(define-command (detab-line (:advice-classes editable-advice)) (n) (:universal)
  "Replaces the indent of the current line from tab to space."
  (tab-line-aux n
                (lambda (n)
                  (make-string (* n (variable-value 'tab-width))
                               :initial-element #\space))))

(define-command (delete-blank-lines (:advice-classes editable-advice)) () ()
  "Delete blank lines before and after the cursor."
  (let ((point (current-point)))
    (loop
      (unless (blank-line-p point)
        (line-offset point 1)
        (return))
      (unless (line-offset point -1)
        (return)))
    (loop
      (when (end-buffer-p point)
        (return))
      (let ((nblanks (blank-line-p point)))
        (if nblanks
            (delete-character point nblanks)
            (return))))))

(defun delete-while-whitespaces (ignore-newline-p)
  (let ((n (skip-chars-forward (current-point)
                               (if ignore-newline-p
                                   '(#\space #\tab)
                                   '(#\space #\tab #\newline)))))
    (delete-character (current-point) (- n))))

(define-command (just-one-space (:advice-classes editable-advice)) () ()
  "Combines consecutive whitespace before and after the cursor into one."
  (skip-whitespace-backward (current-point) t)
  (delete-while-whitespaces t)
  (insert-character (current-point) #\space 1))

(define-command (delete-indentation (:advice-classes editable-advice)) () ()
  "Merge the current line with the previous line."
  (with-point ((p (current-point)))
    (line-start p)
    (unless (start-buffer-p p)
      (delete-character p -1)
      (skip-whitespace-backward p t)
      (loop :while (and (syntax-space-char-p (character-at p))
                        (not (end-buffer-p p)))
            :do (delete-character p))
      (unless (or (start-line-p p)
                  (syntax-closed-paren-char-p (character-at p))
                  (with-point ((p p))
                    (and (character-offset p -1)
                         (let ((c (character-at p)))
                           (or (end-line-p p)
                               (syntax-open-paren-char-p c)
                               (syntax-expr-prefix-char-p c))))))
        (insert-character p #\space)))))

(define-command (transpose-characters (:advice-classes editable-advice)) () ()
  "Swaps the characters before and after the cursor."
  (let ((point (current-point)))
    (cond ((start-line-p point))
          ((end-line-p point)
           (let ((c1 (character-at point -1))
                 (c2 (character-at point -2)))
             (unless (eql c2 #\newline)
               (delete-character point -2)
               (insert-string point (format nil "~C~C" c1 c2)))))
          (t
           (let ((c1 (character-at point 0))
                 (c2 (character-at point -1)))
             (delete-character point 1)
             (delete-character point -1)
             (insert-string point (format nil "~C~C" c1 c2)))))))

(define-command undo (n) (:universal)
  "Undo."
  ;; TODO: multiple cursors
  (dotimes (_ n t)
    (unless (buffer-undo (current-point))
      (editor-error "Undo Error"))))

(define-command redo (n) (:universal)
  "Redo."
  ;; TODO: multiple cursors
  (dotimes (_ n t)
    (unless (buffer-redo (current-point))
      (editor-error "Redo Error"))))

(defun *crement-aux (fn)
  (let ((point (current-point)))
    (skip-symbol-backward point)
    (with-point ((start point))
      (skip-symbol-forward point)
      (let ((word (points-to-string start point)))
        (let ((n (handler-case (parse-integer word)
                   (error ()
                     (editor-error "not integer")))))
          (delete-between-points start point)
          (insert-string point (princ-to-string (funcall fn n))))))))

(define-command (increment (:advice-classes editable-advice)) () ()
  "Increments the number before the cursor."
  (*crement-aux #'1+))

(define-command (decrement (:advice-classes editable-advice)) () ()
  "Decrements the number before the cursor."
  (*crement-aux #'1-))

(define-command delete-trailing-whitespace (&optional (buffer (current-buffer))) ()
  "Removes all end-of-line and end-of-buffer whitespace from the current buffer."
  (with-point ((point (buffer-point buffer) :left-inserting))
    (buffer-start point)
    (loop
      (line-end point)
      (let ((n (skip-chars-backward point
                                    (lambda (c)
                                      (member c '(#\space #\tab))))))
        (unless (zerop n)
          (delete-character point n)))
      (unless (line-offset point 1)
        (return)))
    (let ((n (skip-whitespace-backward (buffer-end point))))
      (unless (string= (uiop:strcat #\newline)
                       (points-to-string (buffer-end-point buffer) point))
        (delete-character point n)
        (buffer-end point)
        (unless (start-line-p point)
          (insert-character point #\newline))))))

(defmethod execute :around (mode
                            (command delete-previous-char)
                            argument)
  (cond ((mark-active-p (cursor-mark (current-point)))
         (do-each-cursors ()
           (delete-cursor-region (current-point))))
        (t
         (do-each-cursors ()
           (delete-previous-char-1 argument)))))

(defmethod execute :around (mode
                            (command copy-region)
                            argument)
  (check-marked)
  (do-each-cursors ()
    (copy-cursor-region (current-point))))

(defmethod execute :around (mode
                            (command kill-region)
                            argument)
  (check-marked)
  (do-each-cursors ()
    (kill-cursor-region (current-point))))

(defmethod execute :around (mode
                            (command yank)
                            argument)
  (with-enable-clipboard (and (enable-clipboard-p)
                              (null (buffer-fake-cursors (current-buffer))))
    (process-each-cursors #'call-next-method)))

(defmethod lem-core:paste-using-mode (mode text)
  (yank-string (current-point) text))
