(defpackage :lem-go-mode
  (:use :cl
        :lem
        :lem/completion-mode
        :lem/language-mode)
  (:import-from
   :lem/tmlanguage
   :load-tmlanguage)
  (:export :gofmt
           :*go-mode-hook*
           :go-mode))
(in-package :lem-go-mode)

(defvar *go-syntax-table*
  (let ((table (make-syntax-table
                :space-chars '(#\space #\tab #\newline)
                :symbol-chars '(#\_)
                :paren-pairs '((#\( . #\))
                               (#\{ . #\})
                               (#\[ . #\]))
                :string-quote-chars '(#\" #\' #\`)
                :line-comment-string "//"
                :block-comment-pairs '(("/*" . "*/"))))
        (tmlanguage (load-tmlanguage
                     (merge-pathnames "go.json"
                                      (asdf:system-source-directory :lem-go-mode)))))
    (set-syntax-parser table tmlanguage)
    table))

(define-major-mode go-mode language-mode
    (:name "Go"
     :keymap *go-mode-keymap*
     :syntax-table *go-syntax-table*
     :mode-hook *go-mode-hook*
     :formatter 'gofmt)
  (setf (variable-value 'enable-syntax-highlight) t)
  (setf (variable-value 'calc-indent-function) 'go-calc-indent)
  (setf (variable-value 'indent-tabs-mode) t)
  (setf (variable-value 'tab-width) 8)
  (setf (variable-value 'beginning-of-defun-function) 'go-beginning-of-defun)
  (setf (variable-value 'end-of-defun-function) 'go-end-of-defun)
  (setf (variable-value 'line-comment) "//")
  (setf (variable-value 'insertion-line-comment) "// ")
  (setf (variable-value 'find-definitions-function) 'go-find-definitions)
  (setf (variable-value 'completion-spec) 'go-completion)
  (setf (variable-value 'idle-function) 'go-idle-function)
  (add-hook (variable-value 'after-save-hook :buffer (current-buffer)) 'goflymake))

(define-file-type ("go") go-mode)

(define-key *go-mode-keymap* "}" 'go-electric-close)
(define-key *go-mode-keymap* "C-c C-d" 'godef-describe)
(define-key *go-mode-keymap* "C-c M-c" 'go-remove-notes)

(defun go-beginning-of-defun (point n)
  (loop :repeat n :do (search-backward-regexp point "^\\w[^=(]*")))

(defun go-end-of-defun (point n)
  (if (minusp n)
      (go-beginning-of-defun point (- n))
      (search-forward-regexp point "^(?:\\}|\\))")))

(defun following-word (point)
  (points-to-string point
		    (form-offset (copy-point point :temporary) 1)))

(defun semicolon-p (point)
  (let ((c (character-at point -1)))
    (case c
      ((#\;) t)
      ((#\' #\" #\`) t)
      ((#\+ #\-)
       (character-offset point -1)
       (eql c (character-at point -1)))
      ((#\) #\] #\}) t)
      (t
       (and (/= 0 (skip-chars-backward point #'syntax-word-char-p))
            #-(and)(not (member (following-word point)
                                '("break" "continue" "fallthrough" "return")
                                :test #'string=)))))))

(defun go-calc-indent (point)
  (let ((tab-width (variable-value 'tab-width :default point)))
    (back-to-indentation point)
    (cond ((maybe-beginning-of-comment point)
           (1+ (point-column point)))
          ((in-string-p point)
           nil)
          (t
           (let ((inside-indenting-paren nil)
                 (indent))
             (setf indent
                   (with-point ((point point))
                     (when (scan-lists point -1 1 t)
                       (case (character-at point)
                         (#\{
                          (back-to-indentation point)
                          (+ tab-width (point-column point)))
                         (#\(
                          (let ((n 0))
                            (when (with-point ((point point))
                                    (and (form-offset point -1)
                                         (member (following-word point)
                                                 '("import" "const" "var" "type" "package")
                                                 :test #'string=)))
                              (setf inside-indenting-paren t)
                              (incf n tab-width))
                            (back-to-indentation point)
                            (+ (point-column point) n)))))))
             (when (null indent)
               (return-from go-calc-indent 0))
             (cond ((looking-at point "case\\W|default\\W")
                    (decf indent tab-width))
                   ((with-point ((point point))
                      (when (looking-at (line-start point) "\\s*}")
                        (line-end point)
                        (skip-chars-backward point '(#\)))
                        (form-offset point -1)
                        (back-to-indentation point)
                        (setf indent (point-column point))))))
             (with-point ((point point))
               (line-start point)
               (skip-space-and-comment-backward point)
               (when (case (character-at point -1)
                       ((nil #\{ #\:)
                        nil)
                       (#\(
                        (not inside-indenting-paren))
                       (#\,
                        (and (scan-lists point -1 1 t)
                             (not (eql #\{ (character-at point)))))
                       (t
                        (not (semicolon-p point))))
                 (incf indent tab-width)))
             (with-point ((point point))
               (when (and (looking-at (line-start point) "^\\s*\\)\\s*$")
                          inside-indenting-paren)
                 (decf indent tab-width)))
             indent)))))

(define-command go-electric-close (n) (:universal)
  (self-insert n)
  (indent))

(define-command godoc (command)
    ((prompt-for-string "godoc "))
  (let ((text
          (with-output-to-string (out)
            (uiop:run-program (list "godoc" command)
                              :output out
                              :error-output out
                              :ignore-error-status t)))
        (buffer (make-buffer "*godoc*" :read-only-p t :enable-undo-p nil)))
    (change-buffer-mode buffer 'go-mode)
    (with-pop-up-typeout-window (out buffer :erase t)
      (write-string text out))))

(defun buffer-text-using-cache (buffer)
  (cond
    ((eql (buffer-value buffer 'prev-tick)
          (buffer-modified-tick buffer))
     (buffer-value buffer 'text))
    (t
     (setf (buffer-value buffer 'prev-tick)
           (buffer-modified-tick buffer))
     (setf (buffer-value buffer 'text)
           (points-to-string (buffer-start-point buffer)
                             (buffer-end-point buffer))))))

(defun call-godef (point)
  (let ((buffer (point-buffer point)))
    (let ((text
            (with-output-to-string (out)
              (with-input-from-string (in (buffer-text-using-cache buffer))
                (uiop:run-program (list "godef" "-i" "-t" "-f"
                                        (namestring (probe-file (buffer-filename buffer)))
                                        "-o" (princ-to-string (point-bytes point)))
                                  :input in
                                  :output out
                                  :ignore-error-status t)))))
      (with-input-from-string (in text)
        (values (read-line in nil)
                (loop :for line := (read-line in nil)
                      :while line
                      :collect line))))))

(defun godef-successful-p (output)
  (not (or (string= "-" output)
           (string= "godef: no identifier found" output)
           (ppcre:scan '(:sequence :start-anchor "godef: no declaration found for ") output)
           (ppcre:scan '(:sequence :start-anchor "error finding import path for ") output))))

(defun godef-error (output)
  (cond ((godef-successful-p output)
         nil)
        ((string= "." output)
         "godef: expression is not defined anywhere")
        (t
         output)))

(defun godef-parse (output)
  (ppcre:register-groups-bind (filename line-number charpos)
      ("(.+):(\\d+):(\\d+)" output)
    (when (and filename line-number charpos)
      (make-xref-location :filespec filename
                          :position (lem/language-mode::make-position
                                     (parse-integer line-number)
                                     (1- (parse-integer charpos)))))))

(defun go-find-definitions (point)
  (unless (buffer-filename (point-buffer point))
    (editor-error "Cannot use godef on a buffer without a file name"))
  (let ((file (call-godef point)))
    (cond
      ((not (godef-successful-p file))
       (editor-error "~A" (godef-error file)))
      (t
       (display-xref-locations (godef-parse file))))))

(define-command godef-describe () ()
  (let ((description (nth-value 1 (call-godef (current-point)))))
    (when description
      (display-popup-message (format nil "~{~A~^~%~}" description)))))

(defun parse-gocode (text)
  (let ((json (yason:parse text)))
    (let ((len (first json))
          (candidates (second json)))
      (declare (ignore len))
      (loop :for ht :in candidates
            :for class := (gethash "class" ht)
            :for name := (gethash "name" ht)
            :for type := (gethash "type" ht)
            :collect (make-completion-item :label name
                                           :detail (format nil "~40A ~A " type class))))))

(defun gocode (point)
  (let ((buffer (point-buffer point)))
    (let ((text
            (with-output-to-string (out)
              (with-input-from-string (in (buffer-text-using-cache buffer))
                (uiop:run-program (list "gocode" "-f=json" "autocomplete"
                                        (or (buffer-filename buffer) "")
                                        (format nil "c~D" (1- (position-at-point point))))
                                  :input in
                                  :output out)))))
      (parse-gocode text))))

(defun go-completion (point)
  (gocode point))

(defvar *goflymake-overlays* '())

(defun goflymake-note (point line-number error-message)
  (move-to-line point line-number)
  (let ((ov (make-overlay (back-to-indentation point)
                          (line-end (copy-point point :temporary))
                          'compiler-note-attribute)))
    (push ov *goflymake-overlays*)
    (overlay-put ov 'message error-message)))

(defun fly-send-result (text buffer-name flymake-file)
  (send-event
   (lambda ()
     (delete-file flymake-file)
     (alexandria:when-let ((buffer (get-buffer buffer-name)))
       (with-input-from-string (in text)
         (with-point ((p (buffer-point buffer)))
           (loop :for line := (read-line in nil)
                 :while line
                 :do (ppcre:register-groups-bind (line-number error-message)
                         (":(\\d+):\\s*(.*)" line)
                       (when (and line-number error-message)
                         (goflymake-note p (parse-integer line-number)
                                         error-message))))))))))

(defvar *fly-thread* nil)

(defun run-flymake (fn)
  (when (and *fly-thread* (bt2:thread-alive-p *fly-thread*))
    (bt2:destroy-thread *fly-thread*))
  (setf *fly-thread*
        (bt2:make-thread fn :name "go-flymake")))

(define-command goflymake (buffer)
    ((current-buffer))
  (when (eq 'go-mode (buffer-major-mode buffer))
    (mapc #'delete-overlay *goflymake-overlays*)
    (setf *goflymake-overlays* '())
    (let* ((buffer-name (buffer-name buffer))
           (directory (buffer-directory buffer))
           (filename (buffer-filename buffer))
           (flymake-file (make-pathname :name (concatenate 'string
                                                           "flymake_"
                                                           (pathname-name filename))
                                        :type "go"
                                        :directory (pathname-directory filename))))
      (uiop:copy-file filename flymake-file)
      (run-flymake
       (lambda ()
         (let ((text
                 (with-output-to-string (out)
                   (uiop:run-program
                    (format nil "cd '~A'; goflymake -debug=false '~A'"
                            directory
                            flymake-file)
                    :output out
                    :error-output out
                    :ignore-error-status t))))
           (fly-send-result text buffer-name flymake-file)))))))

(define-command goflymake-message () ()
  (dolist (ov *goflymake-overlays*)
    (when (and (eq (current-buffer) (overlay-buffer ov))
               (point<= (overlay-start ov) (current-point))
               (point<= (current-point) (overlay-end ov)))
      (display-popup-message (overlay-get ov 'message))
      (return t))))

(define-command go-remove-notes () ()
  (mapc #'delete-overlay *goflymake-overlays*)
  (setf *goflymake-overlays* nil))

(defun go-idle-function ()
  (goflymake-message))

(defun gofmt (buf)
  "Format a Go buffer with gofmt."
  (let ((file (buffer-filename buf)))
    (uiop:run-program
     (format nil "gofmt -w ~a" file)
     :ignore-error-status t))
  (revert-buffer t))
