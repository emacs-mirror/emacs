(defpackage :lem-core/commands/word
  (:use :cl
        :lem-core
        :lem-core/commands/edit)
  (:export :forward-word
           :previous-word
           :delete-word
           :backward-delete-word
           :downcase-region
           :uppercase-region
           :capitalize-word
           :lowercase-word
           :uppercase-word
           :forward-paragraph
           :backward-paragraph
           :kill-paragraph
           :count-words)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/word)

(define-key *global-keymap* "M-f" 'forward-word)
(define-key *global-keymap* "C-Right" 'forward-word)
(define-key *global-keymap* "M-b" 'previous-word)
(define-key *global-keymap* "C-Left" 'previous-word)
(define-key *global-keymap* "M-d" 'delete-word)
(define-key *global-keymap* "C-Delete" 'delete-word)
(define-key *global-keymap* "C-M-h" 'backward-delete-word)
(define-key *global-keymap* "M-Backspace" 'backward-delete-word)
(define-key *global-keymap* "C-Backspace" 'backward-delete-word)
(define-key *global-keymap* "C-x C-l" 'downcase-region)
(define-key *global-keymap* "C-x C-u" 'uppercase-region)
(define-key *global-keymap* "M-c" 'capitalize-word)
(define-key *global-keymap* "M-l" 'lowercase-word)
(define-key *global-keymap* "M-u" 'uppercase-word)
(define-key *global-keymap* "M-}" 'forward-paragraph)
(define-key *global-keymap* "M-{" 'backward-paragraph)
(define-key *global-keymap* "M-k" 'kill-paragraph)
(define-key *global-keymap* "M-=" 'count-words)

(defun word-type (char)
  (when (characterp char)
    (cond ((char<= (code-char 12354) ;#\HIRAGANA_LETTER_A
                   char
                   (code-char 12435) ;#\HIRAGANA_LETTER_N
                   )
           :hiragana)
          ((char<= (code-char 12450) ;#\KATAKANA_LETTER_A
                   char
                   (code-char 12531) ;#\KATAKANA_LETTER_N
                   )
           :katakana)
          ((or (<= #x4E00
                   (char-code char)
                   #x9FFF)
               (find char "仝々〆〇ヶ"))
           :kanji)
          ((alphanumericp char)
           :alphanumeric))))

(defun word-offset (point n)
  (multiple-value-bind (skip-chars-forward
                        char-offset
                        end-buffer-p)
      (if (plusp n)
          (values #'skip-chars-forward
                  0
                  #'end-buffer-p)
          (values #'skip-chars-backward
                  -1
                  #'start-buffer-p))
    (loop :repeat (abs n)
          :do (funcall skip-chars-forward point (complement #'word-type))
              (when (funcall end-buffer-p point)
                (return))
              (let ((type (word-type (character-at point char-offset))))
                (if (null type)
                    (return nil)
                    (funcall skip-chars-forward
                             point
                             (lambda (c) (eql type (word-type c))))))
          :finally (return point))))

(define-command (forward-word (:advice-classes movable-advice)) (n) (:universal)
  "Move to cursor to next word."
  (word-offset (current-point) n))

(define-command (previous-word (:advice-classes movable-advice)) (n) (:universal)
  "Move to cursor to previous word"
  (word-offset (current-point) (- n)))

(define-command (delete-word (:advice-classes editable-advice)) (n) (:universal)
  "Delete the next word."
  (with-point ((point (current-point) :right-inserting))
    (let ((start (current-point))
          (end (or (word-offset point n)
                   (if (plusp n)
                       (buffer-end point)
                       (buffer-start point)))))
      (cond ((point= start end))
            ((point< start end)
             (kill-region start end))
            (t
             (kill-region end start))))))

(define-command (backward-delete-word (:advice-classes editable-advice)) (n) (:universal)
  "Delete the previous word."
  (with-killring-context (:before-inserting t)
    (delete-word (- n))))

(defun case-region-aux (start end case-fun replace-char-p)
  (save-excursion
    (with-point ((point start :left-inserting))
      (loop :while (and (point< point end)
                        (not (end-buffer-p point)))
            :do (let ((c (character-at point 0)))
                  (cond ((char= c #\newline)
                         (character-offset point 1))
                        ((funcall replace-char-p c)
                         (delete-character point)
                         (insert-character point (funcall case-fun c)))
                        (t
                         (character-offset point 1))))))))

(defun downcase-cursor-region (point)
  (case-region-aux (cursor-region-beginning point)
                   (cursor-region-end point)
                   #'char-downcase
                   #'identity))

(defun uppercase-cursor-region (point)
  (case-region-aux (cursor-region-beginning point)
                   (cursor-region-end point)
                   #'char-upcase
                   #'identity))

(define-command downcase-region (start end) (:region)
  "Replaces the selected region with a downcase."
  (case-region-aux start end #'char-downcase #'identity))

(define-command uppercase-region (start end) (:region)
  "Replaces the selected region with a uppercase."
  (case-region-aux start end #'char-upcase #'identity))

(defun case-word-aux (point n replace-char-p first-case rest-case)
  (dotimes (_ n)
    (skip-chars-forward point (complement #'word-type))
    (when (end-buffer-p point)
      (return))
    (let ((c (character-at point)))
      (delete-character point)
      (insert-character point (funcall first-case c))
      (with-point ((end (or (word-offset (copy-point point :temporary) 1)
                            (buffer-end point))
                        :left-inserting))
        (case-region-aux point
                         end
                         rest-case
                         replace-char-p)
        (move-point point end)))))

(define-command (capitalize-word (:advice-classes editable-advice)) (&optional (n 1)) (:universal)
  "Replace the following word with capital-case."
  (case-word-aux (current-point) n #'alphanumericp #'char-upcase #'char-downcase))

(define-command (lowercase-word (:advice-classes editable-advice)) (&optional (n 1)) (:universal)
  "Replace the following word with lowercase."
  (case-word-aux (current-point) n #'alphanumericp #'char-downcase #'char-downcase))

(define-command (uppercase-word (:advice-classes editable-advice)) (&optional (n 1)) (:universal)
  "Replace the following word with uppercase."
  (case-word-aux (current-point) n #'alphanumericp #'char-upcase #'char-upcase))

(define-command (forward-paragraph (:advice-classes movable-advice)) (&optional (n 1)) (:universal)
  "Move cursor to forward paragraph."
  (let ((point (current-point))
        (dir (if (plusp n) 1 -1)))
    (dotimes (_ (abs n))
      (loop :while (blank-line-p point)
            :do (unless (line-offset point dir)
                  (return-from forward-paragraph)))
      (loop :until (blank-line-p point)
            :do (unless (line-offset point dir)
                  (when (plusp dir) (buffer-end point))
                  (return-from forward-paragraph))))))

(define-command (backward-paragraph (:advice-classes movable-advice)) (&optional (n 1)) (:universal)
  "Move cursor to backward paragraph."
  (forward-paragraph (- n)))

(define-command (kill-paragraph (:advice-classes editable-advice)) (&optional (n 1)) (:universal)
  "Kill the forward paragraph."
  (dotimes (_ n t)
    (with-point ((start (current-point) :right-inserting))
      (forward-paragraph)
      (kill-region start
                   (current-point)))))

(defun %count-words (start end)
  (save-excursion
    (let ((wnum 0))
      (loop :for point := (copy-point start :temporary) :then (word-offset point 1)
            :while (and point (point< point end))
            :do (incf wnum))
      wnum)))

(define-command count-words () ()
  "Count the number of lines/words/characters in the buffer."
  (let ((buffer (current-buffer)))
    (multiple-value-bind (start end)
        (if (buffer-mark-p buffer)
            (values (region-beginning buffer)
                    (region-end buffer))
            (values (buffer-start-point buffer)
                    (buffer-end-point buffer)))
      (let ((chnum (count-characters start end))
            (wnum (%count-words start end))
            (linum (count-lines start end)))
        (show-message (format nil "~a has ~d lines, ~d words and ~d characters."
                              (if (buffer-mark-p buffer)
                                  "Region"
                                  "Buffer")
                              linum wnum chnum))))))

(defmethod execute :around (mode
                            (command downcase-region)
                            argument)
  (check-marked)
  (do-each-cursors ()
    (downcase-cursor-region (current-point))))

(defmethod execute :around (mode
                            (command uppercase-region)
                            argument)
  (check-marked)
  (do-each-cursors ()
    (uppercase-cursor-region (current-point))))
