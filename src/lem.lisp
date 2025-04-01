(in-package :lem-core)

(defvar *set-location-hook* '())
(defvar *before-init-hook* '())
(defvar *after-init-hook* '())
(defvar *splash-function* nil)

(defvar *in-the-editor* nil)

(defvar *syntax-scan-window-recursive-p* nil)
(defvar *help* "Usage: lem [ OPTION-OR-FILENAME ] ...
Options:
        -q, --no-init-file        do not load ~/.lem/init.lisp
        --slime PORT              start slime on PORT
        --eval EXPR               evaluate lisp expression EXPR
        --debug                   enable debugger
        --log-filename FILENAME   file name of the log file
        --kill                    immediately exit
        -i, --interface INTERFACE interface to use, either sdl2 or ncurses
        -v, --version             print the version number and exit
        -h, --help                display this help and exit"
"Help output for cli")

(defun syntax-scan-window (window)
  (check-type window window)
  (when (and (enable-syntax-highlight-p (window-buffer window))
             (null *syntax-scan-window-recursive-p*))
    (let ((*syntax-scan-window-recursive-p* t))
      (syntax-scan-region
       (line-start (copy-point (window-view-point window) :temporary))
       (or (line-offset (copy-point (window-view-point window) :temporary)
                        (window-height window))
           (buffer-end-point (window-buffer window)))))))

(defun syntax-scan-buffer (buffer)
  (check-type buffer buffer)
  (syntax-scan-region
   (buffer-start-point buffer)
   (buffer-end-point buffer)))

(defun setup-first-frame ()
  (let ((frame (make-frame nil)))
    (map-frame (implementation) frame)
    (setup-frame frame (primordial-buffer))))

(defclass lem-timer-manager (timer-manager) ())
(defmethod send-timer-notification ((lem-timer-manager timer-manager) continue)
  (send-event (lambda ()
                (funcall continue)
                (redraw-display))))

(let ((once nil))
  (defun setup ()
    (setup-first-frame)
    (unless once
      (setf once t)
      (start-timer (make-idle-timer (lambda ()
                                      (syntax-scan-window (current-window)))
                                    :name "syntax-scan")
                   100 :repeat t)
      (add-hook *window-scroll-functions*
                (lambda (window)
                  (syntax-scan-window window)))
      (add-hook *window-size-change-functions*
                (lambda (window)
                  (syntax-scan-window window)))
      (add-hook *window-show-buffer-functions*
                (lambda (window)
                  (syntax-scan-window window)))
      (add-hook (variable-value 'after-change-functions :global)
                (lambda (start end old-len)
                  (declare (ignore old-len))
                  (syntax-scan-region start end)))
      (add-hook *find-file-hook*
                (lambda (buffer)
                  (process-file buffer)
                  (syntax-scan-buffer buffer))
                5000)
      (add-hook (variable-value 'before-save-hook :global)
                (lambda (buffer)
                  (process-file buffer)))
      (add-hook *input-hook*
                (lambda (event)
                  (push event *this-command-keys*))))))

(defun teardown ()
  (teardown-frames))

(defstruct command-line-arguments
  args
  (debug nil)
  (log-filename nil)
  (no-init-file nil)
  (interface nil))

(defun parse-args (args)
  (let ((parsed-args
          (make-command-line-arguments))
        (file-count 0))
    (setf (command-line-arguments-args parsed-args)
          `(,@(loop :while args
                    :for arg := (pop args)
                    :when (cond ((member arg '("-h" "--help") :test #'equal)
				 (format t "~a~%" *help*)
				 (uiop:quit))
				((member arg '("-q" "--no-init-file") :test #'equal)
                                 (setf (command-line-arguments-no-init-file parsed-args)
                                       t)
                                 nil)
                                ((equal arg "--slime")
                                 (let ((host-port (pop args)))
                                   ;; TODO: handle incorrect input
                                   (destructuring-bind (host port)
                                       (uiop:split-string (string-trim " " host-port) :separator ":")
                                     `(uiop:symbol-call :lem-lisp-mode :slime-connect ,host ,(parse-integer port) nil))))
                                ((equal arg "--eval")
                                 `(eval ,(read-from-string (pop args))))
                                ((equal arg "--debug")
                                 (setf (command-line-arguments-debug parsed-args)
                                       t))
                                ((equal arg "--log-filename")
                                 (let ((filename (pop args)))
                                   (unless filename
                                     (error "Please, specify a filename to log to."))
                                   (setf (command-line-arguments-log-filename parsed-args)
                                         filename)))
                                ((member arg '("-i" "--interface") :test #'equal)
                                 (let ((interface (pop args)))
                                   (if (and interface (plusp (length interface)))
                                       (setf (command-line-arguments-interface parsed-args)
                                             (alexandria:make-keyword (string-upcase interface)))
                                       (write-line "Please specify an interface to use."
                                                   *error-output*))))
                                ((equal arg "--kill")
                                 `(uiop:quit))
                                ((member arg '("-v" "--version") :test #'equal)
                                 (format t "~A~%" (get-version-string))
                                 (uiop:quit)
                                 nil)
                                ((or (stringp arg) (pathnamep arg))
                                 (incf file-count)
                                 `(uiop:symbol-call :lem :find-file ,(merge-pathnames arg (uiop:getcwd))))
                                (t
                                 arg))
                    :collect it)
            ,@(and (zerop file-count)
                   *splash-function*
                   `((funcall ,*splash-function*)))))
    parsed-args))

(defun apply-args (args)
  (mapc #'eval (command-line-arguments-args args)))

(defvar *original-home* (user-homedir-pathname))

(defun init-quicklisp (path)
  (macrolet ((ql-symbol-value (symbol)
               `(symbol-value (uiop:find-symbol* ,symbol :quicklisp))))
    (symbol-macrolet
        ((ql-home (ql-symbol-value :*quicklisp-home*))
         (ql-local-dir (ql-symbol-value :*local-project-directories*))
         (ql-dist-url (ql-symbol-value :*initial-dist-url*)))
      (flet ((replace-homedir (x)
               (let ((subpath (uiop:subpathp x *original-home*)))
                 (if subpath
                     (merge-pathnames subpath (user-homedir-pathname))
                     x)))
             (qmerge (pathname)
               (uiop:symbol-call :quicklisp :qmerge pathname))
             (ql-install-dist (url &key (prompt t))
               (uiop:symbol-call :quicklisp :install-dist url :prompt prompt)))
        (setf ql-home (ensure-directories-exist path)
              ql-local-dir (mapcar #'replace-homedir ql-local-dir)
              asdf:*central-registry* (mapcar #'replace-homedir asdf:*central-registry*)
              asdf:*user-cache* (replace-homedir asdf:*user-cache*))
        (asdf:initialize-source-registry)
        (asdf:clear-output-translations)
        (unless (uiop:directory-exists-p (qmerge (format nil "dists/~A/" (pathname-name ql-dist-url))))
          (ql-install-dist ql-dist-url :prompt nil))))))

(defun load-init-file ()
  (flet ((maybe-load (path)
           (when (probe-file path)
             (load path)
             (message "Load file: ~a" path)
             t)))
    (let ((home (user-homedir-pathname))
          (current-dir (probe-file "."))
          (*package* (find-package :lem-user)))
      (or (maybe-load (merge-pathnames "init.lisp" (lem-home)))
          (maybe-load (merge-pathnames ".lemrc" home)))
      (unless (uiop:pathname-equal current-dir (user-homedir-pathname))
        (maybe-load (merge-pathnames ".lemrc" current-dir))))))

(defun initialize-source-registry ()
  (asdf:initialize-source-registry
   `(:source-registry
     :inherit-configuration
     (:also-exclude ".qlot")
     (:tree ,(asdf:system-source-directory :lem)))))

(defun init-at-build-time ()
  "This function is called when an lem executable file is built.
If a file named $HOME/.lem/build-init.lisp exists, it is loaded.
The difference is that init.lisp loading is called when the editor is started,
while build-init.lisp is called when the binary file is created.
See scripts/build-ncurses.lisp or scripts/build-sdl2.lisp"
  (initialize-source-registry)
  (let ((file (merge-pathnames "build-init.lisp" (lem-home))))
    (when (uiop:file-exists-p file)
      (load file))))

(defun init (args)
  (unless (equal (funcall 'user-homedir-pathname) ;; funcall for sbcl optimization
                 *original-home*)
    (when (uiop:featurep :quicklisp)
      (init-quicklisp (merge-pathnames "quicklisp/" (lem-home)))))
  (run-hooks *before-init-hook*)
  (unless (command-line-arguments-no-init-file args)
    (load-init-file))
  (run-hooks *after-init-hook*)
  (apply-args args))

(defun run-editor-thread (initialize args finalize)
  (bt2:make-thread
   (lambda ()
     (when initialize (funcall initialize))
     (unwind-protect
          (let (#+lispworks (lw:*default-character-element-type* 'character))
            (with-editor-stream ()
              (with-timer-manager (make-instance 'lem-timer-manager)
                (setf *in-the-editor* t)
                (setup)
                (let ((report (toplevel-command-loop (lambda () (init args)))))
                  (when finalize (funcall finalize report))
                  (teardown)))))
       (setf *in-the-editor* nil)))
   :name "editor"))

(defun find-editor-thread ()
  (find "editor" (bt2:all-threads)
        :test #'equal
        :key #'bt2:thread-name))

(defun lem (&rest args)

  ;; for sbcl, set the default file encoding to utf-8
  ;; (on windows, the default is determined by code page (e.g. :cp932))
  #+sbcl
  (setf sb-impl::*default-external-format* :utf-8)

  (setf args (parse-args args))

  (cond
    ((command-line-arguments-log-filename args)
     (apply #'log:config
            :sane
            :daily (command-line-arguments-log-filename args)
            (list
             (if (command-line-arguments-debug args)
                 :debug
                 :info))))
    (t (log:config :off)))

  (log:info "Starting Lem")

  (cond (*in-the-editor*
         (apply-args args))
        (t
         (let ((implementation
                (get-default-implementation
                 :errorp nil
                 :implementation
                 (or (command-line-arguments-interface args)
                     (if (interactive-stream-p *standard-input*)
                         :ncurses
                         :sdl2)))))
           (unless implementation
             (maybe-load-systems :lem-ncurses)
             (setf implementation (get-default-implementation)))
           (invoke-frontend
            (lambda (&optional initialize finalize)
              (run-editor-thread initialize args finalize))
            :implementation implementation)))))

(defun main (&optional (args (uiop:command-line-arguments)))
  (apply #'lem args))

#+sbcl
(push #'(lambda (x)
          (if x
              (lem x)
              (lem))
          t)
      sb-ext:*ed-functions*)


(add-hook *after-init-hook*
          (lambda ()
            (when *editor-warnings*
              (let ((buffer (make-buffer "*EDITOR WARNINGS*")))
                (dolist (warning *editor-warnings*)
                  (insert-string (buffer-point buffer) warning)
                  (insert-character (buffer-point buffer) #\newline))
                (pop-to-buffer buffer)))))
