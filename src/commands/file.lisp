(defpackage :lem-core/commands/file
  (:use :cl :lem-core)
  (:export :*find-file-executor*
           :find-file-executor
           :execute-find-file
           :find-file
           :find-file-recursively
           :read-file
           :add-newline-at-eof-on-writing-file
           :save-buffer
           :save-current-buffer
           :change-file-name
           :write-file
           :write-region-file
           :insert-file
           :save-some-buffers
           :sync-buffer-with-file-content
           :revert-buffer
           :revert-buffer-function
           :change-directory
           :current-directory
           :prompt-for-files-recursively
           :format-current-buffer
           :file-history
           :find-history-file
           :*file-history-limit*
           :get-file-mode
           :format-current-buffer)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/file)

(define-key *global-keymap* "C-x C-f" 'find-file)
(define-key *global-keymap* "C-x C-r" 'read-file)
(define-key *global-keymap* "C-x C-s" 'save-current-buffer)
(define-key *global-keymap* "C-x C-w" 'write-file)
(define-key *global-keymap* "C-x C-h" 'find-history-file)
(define-key *global-keymap* "C-x Tab" 'insert-file)
(define-key *global-keymap* "C-x s" 'save-some-buffers)

;; Programs to find files recursively:
;; We want symbols and not strings, for CLOS dispatch.
(defvar *find-programs* (list :fdfind :fd :find :lisp)
  "List of program candidates to be used to find files recursively.
  Must be symbols of program names, for example \":find\" for the unix \"find\" program.")

(defvar *find-program*)                 ;; unbound: search and set at first use.

(defvar *find-program-timeout* 5
  "Timeout (in seconds) for listing files recursively.")

(define-condition fallback-to-find-file (simple-condition)
  ())

(defun expand-files* (filename)
  (directory-files (expand-file-name filename (buffer-directory))))

(defun maybe-create-directory (directory)
  (when (prompt-for-y-or-n-p
         (format nil "Directory does not exist: ~A. Create" directory))
    (ensure-directories-exist directory)))

(defun directory-for-file-or-lose (filename)
  (let ((directory (directory-namestring filename)))
    (unless (or (uiop:directory-exists-p directory)
                (maybe-create-directory directory))
      (error 'editor-abort))
    directory))

(defgeneric execute-find-file (executor mode pathname))
(defclass find-file-executor () ())

(defvar *find-file-executor* (make-instance 'find-file-executor))

(define-command find-file (arg) (:universal)
  "Open the file."
  (let ((*default-external-format* *default-external-format*))
    (let ((filename
            (cond ((and (numberp arg) (= 1 arg))
                   (prompt-for-file
                    "Find File: "
                    :directory (buffer-directory)
                    :default nil
                    :existing nil))
                  ((numberp arg)
                   (setf *default-external-format*
                         (prompt-for-encodings
                          "Encodings: "
                          :history-symbol 'mh-read-file-encodings))
                   (prompt-for-file
                    "Find File: "
                    :directory (buffer-directory)
                    :default nil
                    :existing nil))
                  ((or (pathnamep arg)
                       (uiop:absolute-pathname-p arg))
                   (namestring arg)))))
      (let (buffer)
        (dolist (pathname (expand-files* filename))
          (setf buffer (execute-find-file *find-file-executor*
                                          (get-file-mode pathname)
                                          pathname)))
        (when (bufferp buffer)
          (switch-to-buffer buffer t nil))))))

(defmethod execute-find-file :before (executor mode pathname)
  (directory-for-file-or-lose pathname))

(defmethod execute-find-file (executor mode pathname)
  (handler-case
      (multiple-value-bind (buffer new-file-p)
          (find-file-buffer pathname)
        (values buffer new-file-p))
    (encoding-read-error ()
      (open-external-file pathname))))

(defun find-program ()
  "Return the first program of *find-programs* that exists on this system.
  Cache the result on *find-program*.
  On non-Unix platforms, fallback to the :lisp method."
  #-unix
  (progn
    (print "lem-core/commands/file: WHICH is not defined for your OS. We fallback *find-program* to the :lisp method.")
    :lisp)
  #+unix
  (if (boundp '*find-program*)
      *find-program*
      (loop for key in *find-programs*
            for name = (str:downcase (string key))
            if (eql :lisp key)
              do (setf *find-program* :lisp)
            else do
              (when (exist-program-p name)
                (setf *find-program* key)
                (return key)))))

(defgeneric get-files-recursively (program)
  (:documentation "Find files recursively on the current working
  directory with the program set in `*find-program*'.
  Use uiop:with-current-directory in the caller.")
  (:method (finder)
    (error "No file finder was found for ~a.~&Use any of *find-programs*: ~S" finder *find-programs*)))

(defmethod get-files-recursively ((finder (eql :fdfind)))
  ;; fdfind excludes .git, node_modules and such by default.
  (str:lines
   (uiop:run-program (list "fdfind") :output :string)))

(defmethod get-files-recursively ((finder (eql :fd)))
  (str:lines
   (uiop:run-program (list "fd") :output :string)))

(defmethod get-files-recursively ((finder (eql :find)))
  (str:lines
   (uiop:run-program (list "find" ".") :output :string)))

(defun %shorten-path (cwd path)
  (str:replace-all cwd "" path))

(defmethod get-files-recursively ((finder (eql :lisp)))
  "Find all files recursively, without external tools."
  ;; XXX: this method returns full paths, instead of paths starting at the current directory.
  (let ((results)
        (cwd-string (namestring (uiop:getcwd))))
    (uiop:collect-sub*directories
     (uiop:getcwd)
     (constantly t)
     (constantly t)
     (lambda (subdir)
       (setf results
             (nconc results
                    ;; For the file select, we want strings, not pathnames.
                    (loop for path in (append (uiop:subdirectories subdir)
                                              (uiop:directory-files subdir))
                          for path-string = (namestring path)
                          ;; Return file names relative to the current directory,
                          ;; not absolute paths.
                          ;; Ex: hello.lisp instead of /home/user/lem/hello.lisp
                          collect (%shorten-path cwd-string path-string))))))
    results))

(defun get-files-recursively-with-timeout (find-program &key (timeout *find-program-timeout*))
  "Find files recursively, with timeout.
  If finding files times out, such as in a HOME directory, stop the operation.

  Return a list of files or signal a FALLBACK-TO-FIND-FILE simple condition."
  (let ((thread (bt2:make-thread
                 (lambda ()
                   (get-files-recursively find-program))
                 :name "Lem get-files-recursively")))
    (handler-case
        (bt2:with-timeout (timeout)
          (bt2:join-thread thread))
      (bt2:timeout ()
        (bt2:destroy-thread thread)
        (signal 'fallback-to-find-file)))))

(defun prompt-for-files-recursively ()
  "Prompt for a file, listing all files under the buffer's directory recursively.

  If listing all files times out, abort the process and fallback to the simple find-file."
  (handler-bind ((fallback-to-find-file
                   ;; Fallback to simple find-file.
                   (lambda (c)
                     (declare (ignore c))
                     (message "Time out! Finding files recursively under ~A was aborted." (buffer-directory))
                     (prompt-for-file
                      "Find File: "
                      :directory (buffer-directory)
                      :default nil
                      :existing nil))))
    (let ((candidates (get-files-recursively-with-timeout (find-program))))
      (prompt-for-string
       "File: "
       :completion-function (lambda (x) (completion-files x candidates))
       :test-function (lambda (name) (member name candidates :test #'string=))))))

(define-command find-file-recursively (arg) (:universal)
  "Open a file, from the list of all files present under the buffer's directory, recursively."
  ;; ARG is currently not used, use it when needed.
  (declare (ignorable arg))
  (let ((cwd (buffer-directory)))
    (uiop:with-current-directory (cwd)
      (let ((filename (prompt-for-files-recursively))
            buffer)
        (when filename
          (setf buffer (execute-find-file *find-file-executor*
                                          (get-file-mode filename)
                                          filename))
          (when buffer
            (switch-to-buffer buffer t nil)))))))


(define-command read-file (filename) ((:new-file "Read File: "))
  "Open the file as a read-only."
  (when (pathnamep filename)
    (setf filename (namestring filename)))
  (dolist (pathname (expand-files* filename))
    (let ((buffer (find-file-buffer (namestring pathname))))
      (setf (buffer-read-only-p buffer) t)
      (switch-to-buffer buffer t nil)))
  t)

(define-editor-variable Add-Newline-at-EOF-on-Writing-File nil)

(defun add-newline-at-eof (buffer)
  (when (variable-value 'Add-Newline-at-EOF-on-Writing-File :default buffer)
    (unless (start-line-p (buffer-end-point buffer))
      (with-point ((p (buffer-point buffer) :left-inserting))
        (save-excursion
          (insert-character p #\newline))))))

(define-editor-variable delete-trailing-whitespace-on-writing-file nil)

(defun clear-trailing-whitespace-on-write (&optional buffer)
  (when (variable-value 'delete-trailing-whitespace-on-writing-file :default buffer)
    (lem-core/commands/edit:delete-trailing-whitespace buffer)))

(defun save-buffer (buffer &optional force-p)
  (cond
    ((and (or force-p (buffer-modified-p buffer))
          (buffer-filename buffer))
     (add-newline-at-eof buffer)
     (clear-trailing-whitespace-on-write buffer)
     (write-to-file buffer (buffer-filename buffer))
     (buffer-unmark buffer)
     (buffer-filename buffer))
    ((null (buffer-filename buffer))
     (editor-error "No file name"))
    (t nil)))

(define-command save-current-buffer (&optional force-p) (:universal-nil)
  "Saves the current buffer text to a file"
  (let ((buffer (current-buffer)))
    (alexandria:when-let (filename (save-buffer buffer force-p))
      (message "Wrote ~A" filename))))

(define-command write-file (filename) ((:new-file "Write File: "))
  "Saves the text in the current buffer to the specified file"
  (let* ((old (buffer-name))
         (new (file-namestring filename))
         (expand-file-name (expand-file-name filename)))
    (unless (and (find expand-file-name (mapcar #'buffer-filename
                                                (buffer-list))
                       :test #'equal)
                 (not (prompt-for-y-or-n-p (format nil
                                                   "~a is opened, overwrite it?"
                                                   expand-file-name))))
      (directory-for-file-or-lose filename)
      (unless (string= old new)
        (buffer-rename (current-buffer)
                       (if (get-buffer new)
                           (unique-buffer-name new)
                           new)))
      (setf (buffer-filename) expand-file-name)
      (add-newline-at-eof (current-buffer))
      (save-current-buffer t))))

(define-command write-region-file (start end filename)
    (:region (:new-file "Write Region To File: "))
  "Saves the region of text to the specified file"
  (setf filename (expand-file-name filename))
  (add-newline-at-eof (point-buffer start))
  (write-region-to-file start end filename)
  (message "Wrote ~A" filename))

(define-command insert-file (filename) ((:file "Insert file: "))
  "Inserts the contents of the file into the current buffer."
  (insert-file-contents (current-point)
                        (expand-file-name filename))
  t)

(define-command save-some-buffers (&optional save-silently-p) (:universal-nil)
  "Save some files in the open buffer."
  (let ((prev-buffer (current-buffer)))
    (dolist (buffer (buffer-list))
      (when (and (buffer-modified-p buffer)
                 (buffer-filename buffer))
        (switch-to-buffer buffer nil)
        (when (or save-silently-p
                  (prompt-for-y-or-n-p (format nil "Save file ~A" (buffer-filename buffer))))
          (save-current-buffer))))
    (switch-to-buffer prev-buffer nil)))

(defun revert-buffer-function (buffer)
  (buffer-value buffer 'revert-buffer-function))

(defun (setf revert-buffer-function) (function buffer)
  (setf (buffer-value buffer 'revert-buffer-function)
        function))

(defun sync-buffer-with-file-content (buffer)
  (with-buffer-read-only buffer nil
    (let* ((point (buffer-point buffer))
           (line-number (line-number-at-point point))
           (column (point-column point)))
      (erase-buffer buffer)
      (insert-file-contents point (buffer-filename buffer))
      (buffer-unmark buffer)
      (update-changed-disk-date buffer)
      (move-to-line point line-number)
      (move-to-column point column)
      t)))

(define-command revert-buffer (does-not-ask-p) (:universal-nil)
  "Restores the buffer. Normally this command will cause the contents of the file to be reflected in the buffer."
  (let ((ask (not does-not-ask-p))
        (buffer (current-buffer)))
    (alexandria:if-let (fn (revert-buffer-function buffer))
      (funcall fn buffer)
      (when (and (or (buffer-modified-p buffer)
                     (changed-disk-p buffer))
                 (if ask
                     (prompt-for-y-or-n-p (format nil "Revert buffer from file ~A" (buffer-filename)))
                     t))
        (sync-buffer-with-file-content buffer)))))

(defvar *last-revert-time* nil)

(defun ask-revert-buffer ()
  (when (or (null *last-revert-time*)
            (< (* 2 (/ internal-time-units-per-second 10))
               (- (get-internal-real-time) *last-revert-time*)))
    (setf *last-revert-time* (get-internal-real-time))
    (when (changed-disk-p (current-buffer))
      (revert-buffer t)
      #+(or)
      (cond ((eql (buffer-value (current-buffer) 'no-revert-buffer)
                  (file-write-date (buffer-filename))))
            ((prompt-for-y-or-n-p (format nil "Revert buffer from file ~A" (buffer-filename)))
             (revert-buffer t))
            (t
             (setf (buffer-value (current-buffer) 'no-revert-buffer)
                   (file-write-date (buffer-filename))))))))

(add-hook *pre-command-hook* 'ask-revert-buffer)

(define-command change-directory (directory)
    ((prompt-for-directory "change directory: " :directory (buffer-directory)))
  "Change directories associated with the buffer."
  (let ((directory (expand-file-name directory (buffer-directory))))
    (setf (buffer-directory) directory)
    (uiop:chdir directory)
    (setf *default-pathname-defaults* (uiop:getcwd)))
  t)

(define-command current-directory (&optional insert) (:universal-nil)
  "Display the directory of the active buffer.
With prefix argument INSERT, insert the directory of the active buffer at point."
  (let ((dir (buffer-directory)))
    (if insert
        (insert-string (current-point) dir)
        (message "Directory ~a" dir))))

(define-command format-current-buffer () ()
  "Save changes and try to format the current buffer.

Supported modes include: c-mode with clang-format, go-mode with gofmt, js-mode and json-mode with prettier, and lisp-mode. Additionally rust-mode uses rustfmt."
  (format-buffer))

(defvar *files-history*)
(defvar *file-history-limit* 10
  "The maximum number of files to keep in the file history.")
  
(defun file-history ()
  "Return or create the files' history struct.
  The history file is saved on (lem-home)/history/files"
  (unless (boundp '*files-history*)
    (let* ((pathname (merge-pathnames "history/files" (lem-home)))
           (history (lem/common/history:make-history :pathname pathname :limit *file-history-limit*)))
      (setf *files-history* history)))
  *files-history*)

(defun add-to-file-history (buffer)
  "Add the buffer's filename to the file history."
  (let ((filename (buffer-filename buffer)))
    (when filename
      (lem/common/history:add-history (file-history) 
                                      (namestring filename)
                                      :allow-duplicates nil
                                      :move-to-top t)
      (lem/common/history:save-file (file-history)))))

(add-hook *find-file-hook* 'add-to-file-history)

(define-command find-history-file () ()
  "Prompt for a file from the file history and open it."
  (let* ((history (file-history))
         (candidates (lem/common/history:history-data-list history)))
    (if candidates
        (let ((filename (prompt-for-string
                         "File: "
                         :completion-function (lambda (x) (completion-strings x (reverse candidates)))
                         :test-function (lambda (name) (member name candidates :test #'string=)))))
          (when filename
            (find-file filename)))
        (editor-error "No file history."))))
