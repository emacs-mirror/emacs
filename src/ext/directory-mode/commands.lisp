(uiop:define-package :lem/directory-mode/commands
  (:use :cl
        :lem
        :lem/directory-mode/internal
        :lem/directory-mode/attributes)
  (:import-from :lem/directory-mode/mode
                :directory-mode)
  (:import-from :lem/directory-mode/file
                :delete-file*
                :rename-file*
                :copy-files
                :rename-files)
  (:export :directory-mode-update-buffer
           :directory-mode-up-directory
           :directory-mode-find-file
           :directory-mode-read-file
           :directory-mode-find-file-next-window
           :directory-mode-next-line
           :directory-mode-previous-line
           :directory-mode-mark-and-next-line
           :directory-mode-unmark-and-next-line
           :directory-mode-unmark-and-previous-line
           :directory-mode-toggle-marks
           :directory-mode-unmark-all
           :directory-mode-mark-regexp
           :directory-mode-mark-directories
           :directory-mode-mark-links
           :directory-mode-mark-suffix
           :directory-mode-mark-extension
           :directory-mode-next-mark
           :directory-mode-previous-mark
           :directory-mode-query-replace
           :directory-mode-query-replace-regexp
           :directory-mode-query-replace-symbol
           :directory-mode-delete-files
           :directory-mode-copy-files
           :directory-mode-rename-files
           :directory-mode-rename-file
           :directory-mode-sort-files
           :make-directory
           :find-file-directory
           :directory-mode-kill-lines))
(in-package :lem/directory-mode/commands)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:lock-package :lem/directory-mode/commands))

(defun search-filename-and-recenter (filename)
  "Search `filename` in this files listing, recenter the window on it"
  (move-to-beginning-of-buffer)
  (search-forward (current-point) filename)
  (window-recenter (current-window))
  (character-offset (current-point) (* -1 (length filename))))

(define-command directory-mode-update-buffer () ()
  (update-buffer (current-buffer)))

(define-command directory-mode-up-directory () ()
  (let ((dir (buffer-directory)))
    (switch-to-buffer
     (directory-buffer (uiop:pathname-parent-directory-pathname (buffer-directory))))
    (search-filename-and-recenter
     (concatenate
      'string
      (car
       (reverse
        (split-sequence:split-sequence
         (uiop:directory-separator-for-host)
         dir
         :remove-empty-subseqs t)))
      (string (uiop:directory-separator-for-host))))))

(define-command directory-mode-find-file () ()
  (open-selected-file :read-only nil :next-window nil))

(define-command directory-mode-read-file () ()
  (open-selected-file :read-only t :next-window nil))

(define-command directory-mode-find-file-next-window () ()
  (open-selected-file :read-only nil :next-window t))

(define-command directory-mode-next-line (p) (:universal)
  (line-offset (current-point) p))

(define-command directory-mode-previous-line (p) (:universal)
  (line-offset (current-point) (- p)))

(define-command directory-mode-mark-and-next-line () ()
  (set-mark (current-point) t)
  (directory-mode-next-line 1))

(define-command directory-mode-unmark-and-next-line () ()
  (set-mark (current-point) nil)
  (directory-mode-next-line 1))

(define-command directory-mode-unmark-and-previous-line () ()
  (directory-mode-previous-line 1)
  (set-mark (current-point) nil))

(define-command directory-mode-toggle-marks () ()
  (filter-marks (current-point)
                (lambda (p) (not (get-mark p)))))

(define-command directory-mode-unmark-all () ()
  (filter-marks (current-point) (constantly nil)))

(define-command directory-mode-mark-regexp (regex &optional arg) ((:string "Regex: ") :universal-nil)
  "Mark all files matching the regular expression REGEX.
With prefix argument ARG, unmark all those files."
  (let ((scanner (ppcre:create-scanner regex)))
    (filter-marks (current-point)
                  (lambda (p)
                    (if (ppcre:scan scanner (get-name p))
                        (not arg)
                        (get-mark p))))))

(define-command directory-mode-mark-directories (&optional arg) (:universal-nil)
  "Mark all directories in the current buffer except '..'.
With prefix argument ARG, unmark all those directories."
  (filter-marks (current-point)
                (lambda (p)
                  (line-start p)
                  (move-to-file-position p)
                  (if (eq 'directory-attribute (text-property-at p :attribute))
                      (not arg)
                      (get-mark p)))))

(define-command directory-mode-mark-links (&optional arg) (:universal-nil)
  "Mark all symbolic links in the current buffer.
With prefix argument ARG, unmark all those links."
  (filter-marks (current-point)
                (lambda (p)
                  (line-start p)
                  (move-to-file-position p)
                  (if (eq 'link-attribute (text-property-at p :attribute))
                      (not arg)
                      (get-mark p)))))

(define-command directory-mode-mark-suffix (suffix &optional arg) ((:string "Suffix: ") :universal-nil)
  "Mark all files with the given SUFFIX.
The provided SUFFIX is a string, and not a file extenion, meaning every file with
a name ending in SUFFIX will be marked.
With prefix argument ARG, unmark all those files."
  (filter-marks (current-point)
                (lambda (p)
                  (let ((name (get-name p)))
                    ;; Use < so exact matches are not marked
                    (if (and (< (length suffix) (length name))
                             (string= name suffix :start1 (- (length name) (length suffix))))
                        (not arg)
                        (get-mark p))))))

(define-command directory-mode-mark-extension (extension &optional arg) ((:string "Extension: ") :universal-nil)
  "Mark all files with the given EXTENSION.
A '.' is prepended to the EXTENSION if not present.
With prefix argument ARG, unmark all those files."
  ;; Support empty extension, which will mark all files ending with a '.'.
  (when (or (= 0 (length extension))
            (char/= (aref extension 0) #\.))
    (setf extension (concatenate 'string "." extension)))
  (directory-mode-mark-suffix extension arg))

(define-command directory-mode-next-mark (n) (:universal)
  "Move to the next Nth marked entry."
  (cond ((= 0 n)
         nil)
        ((< n 0)
         (directory-mode-previous-mark (- n)))
        (t (let* ((all-marks (delete-if (lambda (p)
                                          (point< p (current-point)))
                                        (marked-lines (current-point))))
                  (result (nth (- n 1) all-marks)))
             (if result
                 (progn
                   (move-point (current-point) result)
                   (move-to-file-position (current-point)))
                 (editor-error "No next mark"))))))

(define-command directory-mode-previous-mark (n) (:universal)
  "Move to the previous Nth marked entry."
  (cond ((= 0 n) nil)
        ((< n 0) (directory-mode-next-mark (- n)))
        (t (with-point ((point (current-point)))
             (line-start point)
             (let* ((all-marks (delete-if (lambda (p)
                                            (point>= p point))
                                          (marked-lines point)))
                    (result (last all-marks n)))
               (if (and result
                        (= n (length result)))
                   (progn
                     (move-point (current-point) (car result))
                     (move-to-file-position (current-point)))
                   (editor-error "No previous mark")))))))

(defun query-replace-marked-files (query-function)
  (destructuring-bind (before after)
      (lem/isearch:read-query-replace-args)
    (dolist (file (marked-files (current-point)))
      (find-file file)
      (buffer-start (current-point))
      (funcall query-function before after))))

(define-command directory-mode-query-replace () ()
  (query-replace-marked-files 'lem/isearch:query-replace))

(define-command directory-mode-query-replace-regexp () ()
  (query-replace-marked-files 'lem/isearch:query-replace-regexp))

(define-command directory-mode-query-replace-symbol () ()
  (query-replace-marked-files 'lem/isearch:query-replace-symbol))

(define-command directory-mode-delete-files () ()
  (let ((files (selected-files (current-point))))
    (when (prompt-for-y-or-n-p (format nil "Really delete files~%~{- ~A~%~}" files))
      (dolist (file files)
        (delete-file* file))
      (update-all-buffers))))

(defun get-dest-directory ()
  (dolist (window (window-list) (buffer-directory))
    (when (and (not (eq window (current-window)))
               (eq 'directory-mode (buffer-major-mode (window-buffer window))))
      (return (buffer-directory (window-buffer window))))))

(define-command directory-mode-copy-files () ()
  (let ((dst-file (prompt-for-file "Destination Filename: " :directory (get-dest-directory)))
        (files (selected-files (current-point))))
    (copy-files files dst-file))
  (update-all-buffers))

(define-command directory-mode-rename-files () ()
  (let ((dst-file (prompt-for-file "Destination Filename: " :directory (get-dest-directory))))
    (rename-files (selected-files (current-point)) dst-file))
  (update-all-buffers))

(defun move-to-file-position (point)
  (with-point ((limit point))
    (line-end limit)
    (next-single-property-change point :file limit)))

(defun replace-file-name (point string)
  (when (alexandria:emptyp string) (setf string " "))
  (line-start point)
  (move-to-file-position point)
  (character-at point 1)
  (let ((file (text-property-at point :file))
        (*inhibit-read-only* t))
    (with-point ((end point))
      (line-end end)
      (delete-between-points point end)
      (insert-string point string :file file))))

(defun prompt-for-rename-file (point)
  (let ((file (current-file point)))
    (save-excursion
      (move-point (current-point) point)
      (prompt-for-string
       ""
       :initial-value (if file (file-namestring file) "")
       :test-function (lambda (string)
                        (not (alexandria:emptyp string)))
       :gravity :cursor
       :use-border nil))))

(define-command directory-mode-rename-file () ()
  (with-point ((point (current-point) :right-inserting))
    (move-to-file-position point)
    (alexandria:when-let (source-file (text-property-at point :file))
      (replace-file-name point "")
      (unwind-protect
           (let* ((new-file (merge-pathnames (prompt-for-rename-file point)
                                             (buffer-directory (current-buffer)))))
             (when (probe-file new-file)
               (editor-error "The filename already exists."))
             (rename-file* source-file new-file))
        (directory-mode-update-buffer)))))

(define-command directory-mode-sort-files () ()
  "Sort files: by name, by last modification time, then by size.

  Each new directory buffer first uses the default sort method (`lem/directory-mode:*default-sort-method*')"
  (let ((path (get-pathname (current-point))))
    (cond
      ;; mtime -> size
      ((eql (buffer-value (current-buffer) :sort-method) :mtime)
       (message "Sorting by size")
       (setf (buffer-value (current-buffer) :sort-method) :size)
       (update-buffer (current-buffer) :sort-method :size))
      ;; size -> pathname
      ((eql (buffer-value (current-buffer) :sort-method) :size)
       (message "Sorting by name")
       (setf (buffer-value (current-buffer) :sort-method) :pathname)
       (update-buffer (current-buffer) :sort-method :pathname))
      (t
       ;; At first call, the buffer's sort-method is not set.
       (message "Sorting by last modification time")
       (setf (buffer-value (current-buffer) :sort-method) :mtime)
       (update-buffer (current-buffer) :sort-method :mtime)))

    ;; Follow file name.
    (when (and path (str:non-blank-string-p (file-namestring path)))
      (search-filename-and-recenter (file-namestring path)))))

(define-command make-directory (filename) ((:new-file "Make directory: "))
  (setf filename (uiop:ensure-directory-pathname filename))
  (ensure-directories-exist filename)
  (update-all-buffers))

(define-command find-file-directory () ()
  "Open this file's directory and place point on the filename."
  (let ((fullpath (buffer-filename)))
    (cond
      ((mode-active-p (current-buffer) 'directory-mode)
       (directory-mode-up-directory))
      ((null fullpath)
       (message "No file at point"))
      (t
       (switch-to-buffer
        (find-file-buffer (lem-core/commands/file::directory-for-file-or-lose (buffer-directory))))
       (let ((filename (file-namestring fullpath)))
         (search-filename-and-recenter (file-namestring filename)))))))

(define-command directory-mode-kill-lines () ()
  "Delete the marked lines from the directory-mode buffer.
This does not delete the marked entries, but only remove them from the buffer."
  (with-buffer-read-only (current-buffer) nil
    (let ((*inhibit-read-only* t)
          (marked-lines (marked-lines (current-point))))
      (save-excursion
        ;; Reverse the lines so killing is done from the end of the buffer
        (loop :for marked-line :in (nreverse marked-lines)
              :do (move-point (current-point) marked-line)
                  (kill-whole-line))))))

(defmethod execute :after ((mode directory-mode) command argument)
  (when (mode-active-p (current-buffer) 'directory-mode)
    (update-highlight-line-overlay (current-point))))
