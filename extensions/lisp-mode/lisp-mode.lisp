(in-package :lem-lisp-mode/internal)

(define-editor-variable load-file-functions '())
(define-editor-variable before-compile-functions '())
(define-editor-variable before-eval-functions '())

(define-attribute compilation-region-highlight
  (t :background :base09))

(define-attribute evaluation-region-highlight
  (t :background :base0B))

(defparameter *default-port* 4005)
(defparameter *localhost* "127.0.0.1")

(set-syntax-parser lem-lisp-syntax:*syntax-table*
                   (make-tmlanguage-lisp))

(define-major-mode lisp-mode language-mode
    (:name "Lisp"
     :description "Contains necessary functions to handle lisp code."
     :keymap *lisp-mode-keymap*
     :syntax-table lem-lisp-syntax:*syntax-table*
     :mode-hook *lisp-mode-hook*
     :formatter #'indent-buffer)
  (modeline-add-status-list 'lisp-mode (current-buffer))
  (setf (variable-value 'beginning-of-defun-function) 'lisp-beginning-of-defun)
  (setf (variable-value 'end-of-defun-function) 'lisp-end-of-defun)
  (setf (variable-value 'indent-tabs-mode) nil)
  (setf (variable-value 'enable-syntax-highlight) t)
  (setf (variable-value 'calc-indent-function) 'calc-indent)
  (setf (variable-value 'indent-when-yank) t)
  (setf (variable-value 'line-comment) ";")
  (setf (variable-value 'insertion-line-comment) ";; ")
  (setf (variable-value 'language-mode-tag) 'lisp-mode)
  (setf (variable-value 'find-definitions-function) 'lisp-find-definitions)
  (setf (variable-value 'find-references-function) 'lisp-find-references)
  (setf (variable-value 'completion-spec)
        (make-completion-spec 'completion-symbol-async :async t))
  (setf (variable-value 'idle-function) 'lisp-idle-function)
  (setf (variable-value 'root-uri-patterns) '(".asd"))
  (setf (variable-value 'detective-search)
        (make-instance 'lem/detective:search-regex
                       :function-regex
                       (lem/detective:make-capture-regex
                        :regex "^\\(defun "
                        :function #'lem-lisp-mode/detective:capture-reference)
                       :class-regex
                       (lem/detective:make-capture-regex
                        :regex "^\\(defclass "
                        :function #'lem-lisp-mode/detective:capture-reference)
                       :package-regex
                       (lem/detective:make-capture-regex
                        :regex "^\\(in-package "
                        :function #'lem-lisp-mode/detective:capture-reference)
                       :variable-regex
                       (lem/detective:make-capture-regex
                        :regex "^(?:\\(defvar |\\(defparameter )"
                        :function #'lem-lisp-mode/detective:capture-reference)
                       :macro-regex
                       (lem/detective:make-capture-regex
                        :regex "^\\(defmacro "
                        :function #'lem-lisp-mode/detective:capture-reference)
                       :misc-regex
                       (lem/detective:make-capture-regex
                        :regex "^\\(deftest "
                        :function #'lem-lisp-mode/detective:capture-reference)))
  (unless (connected-p) (self-connect))

  (setf (buffer-context-menu (current-buffer))
        (make-instance 'lem/context-menu:context-menu
                       :compute-items-function 'compute-context-menu-items))

  (lem/link:link-mode t))

(define-key *lisp-mode-keymap* "C-M-q" 'lisp-indent-sexp)
(define-key *lisp-mode-keymap* "C-c M-p" 'lisp-set-package)
(define-key *global-keymap* "M-:" 'lisp-eval-string)
(define-key *lisp-mode-keymap* "C-c M-:" 'lisp-eval-string)
(define-key *lisp-mode-keymap* "C-M-x" 'lisp-eval-defun)
(define-key *lisp-mode-keymap* "C-c C-n" 'lisp-next-compilation-notes)
(define-key *lisp-mode-keymap* "C-c C-p" 'lisp-previous-compilation-notes)
(define-key *lisp-mode-keymap* "C-c C-l" 'lisp-load-file)
(define-key *lisp-mode-keymap* "C-c M-c" 'lisp-remove-notes)
(define-key *lisp-mode-keymap* "C-c C-k" 'lisp-compile-and-load-file)
(define-key *lisp-mode-keymap* "C-c C-c" 'lisp-compile-defun)
(define-key *lisp-mode-keymap* "C-c C-d d" 'lisp-describe-symbol)
(define-key *lisp-mode-keymap* "C-c C-z" 'lisp-switch-to-repl-buffer)
(define-key *lisp-mode-keymap* "C-c z" 'lisp-switch-to-repl-buffer)
(define-key *lisp-mode-keymap* "C-c g" 'lisp-interrupt)
(define-key *lisp-mode-keymap* "C-c C-q" 'lisp-quickload)
(define-key *lisp-mode-keymap* "Return" 'newline-and-indent)
(define-key *lisp-mode-keymap* "C-c C-j" 'lisp-eval-expression-in-repl)
(define-key *lisp-mode-keymap* "C-c ~" 'lisp-listen-in-current-package)
(define-key *lisp-mode-keymap* "C-c m s" 'slime)
(define-key *lisp-mode-keymap* "C-c m r" 'slime-restart)
(define-key *lisp-mode-keymap* "C-c m q" 'slime-quit)
(define-key *lisp-mode-keymap* "C-c m c" 'slime-connect)

(defmethod convert-modeline-element ((element (eql 'lisp-mode)) window)
  (format nil "  ~A~A" (buffer-package (window-buffer window) "CL-USER")
          (if (current-connection)
              (format nil " ~A:~A"
                      (connection-implementation-name (current-connection))
                      (if (self-connection-p (current-connection))
                          "SELF"
                          (connection-pid (current-connection))))
              "")))

(defun point-over-symbol-with-menu-opened-p ()
  (let ((point (get-point-on-context-menu-open)))
    (when (and point
               (symbol-string-at-point point))
      point)))

(defun context-menu-describe-symbol ()
  (let ((point (point-over-symbol-with-menu-opened-p)))
    (when point
      (lem/context-menu:make-item
       :label "Describe symbol"
       :callback 'lisp-describe-symbol-at-point))))

(defun context-menu-find-definition ()
  (let ((point (point-over-symbol-with-menu-opened-p)))
    (when point
      (lem/context-menu:make-item
       :label "Find definition"
       :callback (lambda (&rest args)
                   (declare (ignore args))
                   (lisp-find-definitions point))))))

(defun context-menu-find-references ()
  (let ((point (point-over-symbol-with-menu-opened-p)))
    (when point
      (lem/context-menu:make-item
       :label "Find references"
       :callback (lambda (&rest args)
                   (declare (ignore args))
                   (lisp-find-references point))))))

(defun context-menu-hyperspec ()
  (let ((point (point-over-symbol-with-menu-opened-p)))
    (when point
      (lem/context-menu:make-item
       :label "Lookup symbol in Hyperspec"
       :callback (lambda (&rest args)
                   (declare (ignore args))
                   ;; TODO: resolve forward references
                   (uiop:symbol-call :lem-lisp-mode/hyperspec
                                     :hyperspec-at-point
                                     point))))))

(defun context-menu-export-symbol ()
  (let ((point (point-over-symbol-with-menu-opened-p)))
    (when (and point
               (buffer-filename (current-buffer))
               (not (equal "asd" (pathname-type (buffer-filename (current-buffer))))))
      (lem/context-menu:make-item
       :label "Export symbol"
       :callback (lambda (&rest args)
                   (declare (ignore args))
                   (uiop:symbol-call :lem-lisp-mode/exporter
                                     :lisp-add-export
                                     (symbol-string-at-point point)))))))

(defun context-menu-browse-class-as-tree ()
  (let ((point (point-over-symbol-with-menu-opened-p)))
    (when point
      (lem/context-menu:make-item
       :label "Browse class as tree"
       :callback (lambda (&rest args)
                   (declare (ignore args))
                   ;; TODO: resolve forward references
                   (uiop:symbol-call :lem-lisp-mode/class-browser
                                     :lisp-browse-class-as-tree))))))

(defun compute-context-menu-items ()
  (remove
   nil
   (append
    ;; TODO: resolve forward references
    (uiop:symbol-call :lem-lisp-mode/eval :compute-context-menu-items)
    (list (context-menu-describe-symbol)
          (context-menu-find-definition)
          (context-menu-find-references)
          (context-menu-hyperspec)
          (context-menu-export-symbol)
          (context-menu-browse-class-as-tree)))))

(defun change-current-connection (connection)
  (when (current-connection)
    (abort-all (current-connection) "change connection")
    (notify-change-connection-to-wait-message-thread))
  (setf (current-connection) connection)
  (when (repl-buffer)
    (write-string-to-repl
     (if (self-connection-p connection)
         (format nil
                 "~%; changed connection (self connection)")
         (format nil
                 "~%; changed connection (~A ~A)"
                 (connection-implementation-name connection)
                 (connection-implementation-version connection)))
     :attribute 'syntax-comment-attribute)))

(defmethod switch-connection ((connection connection))
  (change-current-connection connection))

(defun connected-p ()
  (not (null (current-connection))))

(defun add-and-change-connection (connection)
  (add-connection connection)
  (change-current-connection connection))

(defun remove-and-change-connection (connection)
  (remove-connection connection)
  (when (eq connection (current-connection))
    (change-current-connection (first (connection-list))))
  (values))

(defvar *self-connected-port* nil)

(defun self-connected-p ()
  (not (null *self-connected-port*)))

(defun self-connected-port ()
  *self-connected-port*)

(defun self-connect ()
  (unless lem-lisp-mode/test-api:*disable-self-connect*
    (let ((port (lem/common/socket:random-available-port)))
      (log:debug "Starting internal SWANK and connecting to it" micros:*communication-style*)
      (let ((micros::*swank-debug-p* nil))
        (micros:create-server :port port :style :spawn))
      (connect-to-micros *localhost* port)
      (update-buffer-package)

      ;; XXX:
      ;; Systems added after lem initialization are not visible from within this process and must
      ;; be re-initialized.
      (asdf:clear-source-registry)

      (setf *self-connected-port* port))))

(defun self-connection ()
  (find-if #'self-connection-p (connection-list)))

(defun check-connection ()
  (unless (connected-p)
    (self-connect)))

(defun buffer-package (buffer &optional default)
  (let ((package-name (buffer-value buffer "package" default)))
    (typecase package-name
      (null (alexandria:if-let (package-name (guess-current-position-package (buffer-point buffer)))
              (string-upcase package-name)
              default))
      ((or symbol string)
       (string-upcase package-name))
      ((cons (or symbol string))
       (string-upcase (car package-name))))))

(defun (setf buffer-package) (package buffer)
  (setf (buffer-value buffer "package") package))

(defvar *current-package* nil)

(defun current-package ()
  (or *current-package*
      (buffer-package (current-buffer))
      (connection-package (current-connection))))

(defun buffer-thread-id (buffer)
  (buffer-value buffer 'thread))

(defun (setf buffer-thread-id) (value buffer)
  (setf (buffer-value buffer 'thread) value))

(defun current-micros-thread ()
  (or (buffer-thread-id (current-buffer))
      t))

(defmethod get-features ()
  (when (connected-p)
    (connection-features (current-connection))))

(defun indentation-update (info)
  (loop :for (name indent packages) :in info
        :do (lem-lisp-syntax:update-system-indentation name indent packages)))

(defun calc-indent (point)
  (lem-lisp-syntax:calc-indent point))

(defun call-with-remote-eval (form continuation
                              &key (connection (current-connection))
                                   (thread (current-micros-thread))
                                   (package (current-package))
                                   request-id)
  (remote-eval connection
               form
               :continuation continuation
               :thread thread
               :package package
               :request-id request-id))

(defmacro with-remote-eval ((form &rest args
                                  &key connection
                                       thread
                                       package
                                       request-id)
                            continuation)
  (declare (ignore connection thread package request-id))
  `(call-with-remote-eval ,form ,continuation ,@args))

(defun lisp-eval-internal (emacs-rex-fun rex-arg package)
  (let ((tag (gensym))
        (thread-id (current-micros-thread)))
    (catch tag
      (funcall emacs-rex-fun
               (current-connection)
               rex-arg
               :continuation (lambda (result)
                               (alexandria:destructuring-ecase result
                                 ((:ok value)
                                  (throw tag value))
                                 ((:abort condition)
                                  (declare (ignore condition))
                                  (editor-error "Synchronous Lisp Evaluation aborted"))))
               :package package
               :thread thread-id)
      (handler-case (loop (sit-for 10 nil))
        (editor-abort ()
          (send-message-string (current-connection) (format nil "(:emacs-interrupt ~D)" thread-id))
          (keyboard-quit))))))

(defun lisp-eval-from-string (string &optional (package (current-package)))
  (lisp-eval-internal 'remote-eval-from-string string package))

(defun lisp-eval (sexp &optional (package (current-package)))
  (lisp-eval-internal 'remote-eval sexp package))

(defun lisp-eval-async (form &optional cont (package (current-package)))
  (let ((buffer (current-buffer)))
    (with-broadcast-connections (connection)
      (with-remote-eval (form :package package :connection connection)
        (lambda (value)
          (alexandria:destructuring-ecase value
            ((:ok result)
             (when cont
               (let ((prev (current-buffer)))
                 (setf (current-buffer) buffer)
                 (funcall cont result)
                 (unless (eq (current-buffer)
                             (window-buffer (current-window)))
                   (setf (current-buffer) prev)))))
            ((:abort condition)
             (display-message "Evaluation aborted on ~A." condition))))))))

(defun eval-with-transcript (form &key (package (current-package)))
  (with-broadcast-connections (connection)
    (with-remote-eval (form :package package :connection connection)
      (lambda (value)
        (alexandria:destructuring-ecase value
          ((:ok x)
           (display-message "~A" x))
          ((:abort condition)
           (display-message "Evaluation aborted on ~A." condition)))))))

(defun re-eval-defvar (string)
  (eval-with-transcript `(micros:re-evaluate-defvar ,string)))

(defun interactive-eval (string &key (package (current-package)))
  (eval-with-transcript `(micros:interactive-eval ,string) :package package))

(defun %lisp-disassemble (symbol &key (package (current-package)))
  (car (lisp-eval
        `(micros:eval-and-grab-output
          ,(format nil "(disassemble '~a)" symbol))
        package)))

(define-command lisp-disassemble () ()
  (check-connection)
  (let* ((name (or (symbol-string-at-point (current-point))
                   (prompt-for-symbol-name "Disassemble: ")))
         (buffer (make-buffer "*lisp-dissasemble*")))

    (with-pop-up-typeout-window (s buffer :erase t)
      (format s "~a" (%lisp-disassemble name)))))

(defun new-package (name prompt-string)
  (setf (connection-package (current-connection)) name)
  (setf (connection-prompt-string (current-connection)) prompt-string)
  t)

(defun read-package-name ()
  (check-connection)
  (let ((package-names (mapcar #'string-downcase
                               (lisp-eval
                                '(micros:list-all-package-names t)))))
    (string-upcase (prompt-for-string
                    "Package: "
                    :completion-function (lambda (string)
                                           (completion string package-names))
                    :test-function (lambda (string)
                                     (find string package-names :test #'string=))
                    :history-symbol 'mh-lisp-package))))

(defun read-asdf-system-name ()
  (check-connection)
  (let ((system-names (lisp-eval '(micros:list-systems))))
    (prompt-for-string
     "System: "
     :completion-function (lambda (string)
                            (completion string system-names))
     :test-function (lambda (string)
                      (find string system-names :test #'string=))
     :history-symbol 'mh-lisp-system)))

(defun lisp-beginning-of-defun (point n)
  (lem-lisp-syntax:beginning-of-defun point (- n)))

(defun lisp-end-of-defun (point n)
  (if (minusp n)
      (lisp-beginning-of-defun point (- n))
      (dotimes (_ n)
        (with-point ((p point))
          (cond ((and (lem-lisp-syntax:beginning-of-defun p -1)
                      (point<= p point)
                      (or (form-offset p 1)
                          (progn
                            (move-point point p)
                            (return)))
                      (point< point p))
                 (move-point point p)
                 (skip-whitespace-forward point t)
                 (when (end-line-p point)
                   (character-offset point 1)))
                (t
                 (form-offset point 1)
                 (skip-whitespace-forward point t)
                 (when (end-line-p point)
                   (character-offset point 1))))))))

(define-command lisp-indent-sexp () ()
  (with-point ((end (current-point) :right-inserting))
    (when (form-offset end 1)
      (indent-points (current-point) end))))

(defmethod execute ((mode lisp-mode) (command open-line) argument)
  (cond ((not (null argument))
         (call-next-method))
        ((and (eql #\( (character-at (current-point)))
              (start-line-p (current-point)))
         (call-next-method))
        (t
         (with-point ((saved-point (current-point)))
           (insert-character (current-point) #\newline)
           (indent-line (current-point))
           (when (blank-line-p (current-point))
             (with-point ((start (current-point))
                          (end (current-point)))
               (line-start start)
               (line-end end)
               (delete-between-points start end)))
           (move-point (current-point) saved-point)))))

(define-command lisp-set-package (package-name) ((read-package-name))
  (check-connection)
  (cond ((string= package-name ""))
        ((eq (current-buffer) (repl-buffer))
         (destructuring-bind (name prompt-string)
             (lisp-eval `(micros:set-package ,package-name))
           (new-package name prompt-string)
           (lem/listener-mode:refresh-prompt (repl-buffer))))
        (t
         (setf (buffer-package (current-buffer)) package-name))))

(define-command lisp-listen-in-current-package () ()
  (check-connection)
  (alexandria:when-let ((repl-buffer (repl-buffer))
                        (package (buffer-package (current-buffer)))
                        (original-buffer (current-buffer)))
    (save-excursion
      (cond ((lisp-eval `(not (null (cl:find-package ,package))))
             (save-excursion
               (setf (current-buffer) repl-buffer)
               (destructuring-bind (name prompt-string)
                   (lisp-eval `(micros:set-package ,package))
                 (new-package name prompt-string)))
             (start-lisp-repl)
             (buffer-end (buffer-point repl-buffer)))
            (t
             (message "Package ~A not found" package)
             (setf (current-buffer) original-buffer))))))

(define-command lisp-current-directory () ()
  (message "Current directory: ~a"
           (lisp-eval `(micros:default-directory))))

(define-command lisp-set-directory (&key directory) ()
  (unless directory
    (setf directory
          (prompt-for-directory "New directory: " :directory (buffer-directory))))
  (lisp-eval
   `(micros:set-default-directory
     (micros/backend:filename-to-pathname ,directory))))

(define-command lisp-interrupt () ()
  (with-broadcast-connections (connection)
    (send-message-string
     connection
     (format nil "(:emacs-interrupt ~A)" (current-micros-thread)))))

(defun prompt-for-sexp (string &optional initial)
  (prompt-for-string string
                     :initial-value initial
                     :completion-function (lambda (string)
                                            (declare (ignore string))
                                            (completion-symbol (current-point)))
                     :history-symbol 'mh-sexp))

(define-command lisp-eval-string (string)
    ((prompt-for-sexp "Lisp Eval: "))
  (check-connection)
  (interactive-eval string))

(defun self-current-package ()
  (or (find (or *current-package*
                (buffer-package (current-buffer))
                (guess-current-position-package (current-point)))
            (list-all-packages)
            :test 'equalp
            :key 'package-name)
      *package*))

(defun cover-annotation (point)
  (with-point ((p point))
    (when (and (line-offset p -1)
               (ppcre:scan "^\\s*@[\\w-]+\\s*$" (line-string p)))
      (move-point point p))))

(defun top-of-defun-with-annotation (point)
  (lem-lisp-syntax:top-of-defun point)
  (cover-annotation point))

(define-command lisp-eval-defun () ()
  "Evaluate top-level form around point and instrument."
  (check-connection)
  (with-point ((point (current-point)))
    (top-of-defun-with-annotation point)
    (with-point ((start point)
                 (end point))
      (scan-lists end 1 0)
      (run-hooks (variable-value 'before-eval-functions) start end)
      (let ((string (points-to-string start end)))
        (if (ppcre:scan "^\\(defvar(?:\\s|$)" string)
            (re-eval-defvar string)
            (interactive-eval string))))))

(define-command lisp-load-file (filename)
    ((prompt-for-file "Load File: "
                      :directory (or (buffer-filename) (buffer-directory))
                      :default nil
                      :existing t))
  "Load the Lisp file named FILENAME."
  (check-connection)
  (when (uiop:file-exists-p filename)
    (let ((filename (convert-local-to-remote-file filename)))
      (run-hooks (variable-value 'load-file-functions) filename)
      (interactive-eval
       (prin1-to-string
        `(if (and (cl:find-package :roswell)
                  (cl:find-symbol (cl:string :load) :roswell))
             (uiop:symbol-call :roswell :load ,filename)
             (micros:load-file ,filename)))
       :package "CL-USER"))))

(defun compilation-finished (result)
  (destructuring-bind (notes successp duration loadp fastfile)
      (rest result)
    (show-compile-result notes duration
                         (if (not loadp)
                             successp
                             (and fastfile successp)))
    (highlight-notes notes)
    (cond ((and loadp fastfile successp)
           (lisp-eval-async `(micros:load-file ,(convert-local-to-remote-file fastfile))
                            (lambda (result)
                              (declare (ignore result))
                              (uiop:delete-file-if-exists
                               (convert-remote-to-local-file fastfile)))))
          (fastfile
           (uiop:delete-file-if-exists
            (convert-remote-to-local-file fastfile))))))

(defun show-compile-result (notes secs successp)
  (display-message (format nil "~{~A~^ ~}"
                           (remove-if #'null
                                      (list (if successp
                                                "Compilation finished"
                                                "Compilation failed")
                                            (unless notes
                                              "(No warnings)")
                                            (when secs
                                              (format nil "[~,2f secs]" secs)))))))

(defun make-highlight-overlay (pos buffer message source-context)
  (with-point ((point (buffer-point buffer)))
    (move-to-position point pos)
    (skip-chars-backward point #'syntax-symbol-char-p)
    (let ((overlay (make-overlay point
                                 (or (form-offset (copy-point point :temporary) 1)
                                     (buffer-end-point buffer))
                                 'compiler-note-attribute))
          (message (with-output-to-string (out)
                     (write-string message out)
                     (when source-context
                       (terpri out)
                       (write-string source-context out)))))
      (set-hover-message overlay
                         message
                         :style '(:gravity :mouse-cursor :offset-y 1))
      (overlay-put overlay 'message message)
      overlay)))

(defvar *note-overlays* nil)

(defun buffer-compilation-notes-overlays (buffer)
  (buffer-value buffer 'compilation-notes-overlays))

(defun (setf buffer-compilation-notes-overlays) (overlays buffer)
  (setf (buffer-value buffer 'compilation-notes-overlays)
        (sort overlays #'point< :key #'overlay-start)))

(defun buffer-compilation-notes-timer (buffer)
  (buffer-value buffer 'compilation-notes-timer))

(defun (setf buffer-compilation-notes-timer) (value buffer)
  (setf (buffer-value buffer 'compilation-notes-timer) value))

(defun convert-notes (notes)
  (loop :for note :in notes
        :when (destructuring-bind (&key location message source-context &allow-other-keys) note
                (when location
                  (alexandria:when-let ((xref-location
                                         (source-location-to-xref-location location nil t)))
                    (list xref-location
                          message
                          source-context))))
        :collect :it))

(defun delete-compilations-buffer ()
  (let ((buffer (get-buffer "*lisp-compilations*")))
    (when buffer
      (kill-buffer buffer))))

(defun show-compilation-notes ()
  (dolist (overlay (buffer-compilation-notes-overlays (current-buffer)))
    (when (point<= (overlay-start overlay) (current-point) (overlay-end overlay))
      (display-message "~A" (overlay-get overlay 'message))
      (return))))

(defun move-to-next-compilation-notes (point)
  (alexandria:when-let ((overlay (loop :for overlay :in (buffer-compilation-notes-overlays
                                                         (point-buffer point))
                                       :when (point< point (overlay-start overlay))
                                       :return overlay)))
    (move-point point (overlay-start overlay))))

(defun move-to-previous-compilation-notes (point)
  (alexandria:when-let ((overlay (loop :for last-overlay := nil :then overlay
                                       :for overlay :in (buffer-compilation-notes-overlays
                                                         (point-buffer point))
                                       :when (point<= point (overlay-start overlay))
                                       :return last-overlay
                                       :finally (return last-overlay))))
    (move-point point (overlay-start overlay))))

(defun remove-compilation-notes-overlay-in-the-changed-point (point arg)
  (declare (ignore arg))
  (dolist (overlay (buffer-compilation-notes-overlays (point-buffer point)))
    (when (point<= (overlay-start overlay) point (overlay-end overlay))
      (delete-overlay overlay)
      (alexandria:deletef (buffer-compilation-notes-overlays (point-buffer point)) overlay))))

(defun highlight-notes (notes)
  (lisp-remove-notes)

  (loop :for (xref-location message source-context) :in (convert-notes notes)
        :do (let* ((pos (xref-location-position xref-location))
                   (buffer (xref-filespec-to-buffer (xref-location-filespec xref-location))))
              (push (make-highlight-overlay pos buffer message source-context)
                    (buffer-compilation-notes-overlays buffer))))

  (setf (buffer-compilation-notes-timer (current-buffer))
        (start-timer (make-idle-timer 'show-compilation-notes :name "lisp-show-compilation-notes")
                     200
                     :repeat t))

  (add-hook (variable-value 'before-change-functions :buffer (current-buffer))
            'remove-compilation-notes-overlay-in-the-changed-point))

(define-command lisp-remove-notes () ()
  (alexandria:when-let (timer (buffer-compilation-notes-timer (current-buffer)))
    (stop-timer timer))
  (dolist (overlay (buffer-compilation-notes-overlays (current-buffer)))
    (delete-overlay overlay))
  (setf (buffer-compilation-notes-overlays (current-buffer)) '()))

(define-command lisp-next-compilation-notes () ()
  (move-to-next-compilation-notes (current-point)))

(define-command lisp-previous-compilation-notes () ()
  (move-to-previous-compilation-notes (current-point)))

(define-command lisp-compile-and-load-file () ()
  (check-connection)
  (when (buffer-modified-p (current-buffer))
    (save-current-buffer))
  (let* ((buffer (current-buffer))
         (file (buffer-filename buffer)))
    (run-hooks (variable-value 'load-file-functions) file)
    (if (str:starts-with-p "#!" (buffer-text buffer))
        (let ((real-start (copy-point (buffer-start-point buffer) :temporary)))
          (move-to-line real-start 2)
          (lisp-compile-region real-start (buffer-end-point buffer)))
        (lisp-eval-async `(micros:compile-file-for-emacs ,(convert-local-to-remote-file file) t)
                         #'compilation-finished))))

(define-command lisp-compile-region (start end) (:region)
  (check-connection)
  (let ((string (points-to-string start end))
        (position `((:position ,(position-at-point start))
                    (:line
                     ,(line-number-at-point (current-point))
                     ,(point-charpos (current-point))))))
    (run-hooks (variable-value 'before-compile-functions) start end)
    (lisp-eval-async `(micros:compile-string-for-emacs ,string
                                                       ,(buffer-name (current-buffer))
                                                       ',position
                                                       ,(buffer-filename (current-buffer))
                                                       nil)
                     #'compilation-finished)))

(define-command lisp-compile-defun () ()
  (check-connection)
  (with-point ((point (current-point)))
    (top-of-defun-with-annotation point)
    (with-point ((start point)
                 (end point))
      (scan-lists end 1 0)
      (lisp-compile-region start end))))

(define-command lisp-eval-expression-in-repl () ()
  (check-connection)
  (with-point ((point (current-point)))
    (top-of-defun-with-annotation point)
    (with-point ((start point)
                 (end point))
      (scan-lists end 1 0)
      (send-string-to-listener (points-to-string start end)
                               (buffer-package (current-buffer))))))

(defun form-string-at-point ()
  (with-point ((point (current-point)))
    (skip-chars-backward point #'syntax-symbol-char-p)
    (with-point ((start point)
                 (end point))
      (form-offset end 1)
      (points-to-string start end))))

(define-command lisp-quickload (system-name)
    ((prompt-for-symbol-name "System: " (buffer-package (current-buffer))))
  (check-connection)
  (eval-with-transcript `(,(uiop:find-symbol* :quickload :quicklisp) ,(string system-name))))

(defun prompt-for-symbol-name (prompt &optional (initial ""))
  (let ((package (current-package)))
    (prompt-for-string prompt
                       :initial-value initial
                       :test-function (lambda (str) (< 0 (length str)))
                       :completion-function (lambda (string)
                                              (lem-lisp-mode/completion:symbol-completion
                                               string package))
                       :history-symbol 'mh-read-symbol)))

(defun definition-to-location (definition)
  (destructuring-bind (title location) definition
    (source-location-to-xref-location location title t)))

(defun definitions-to-locations (definitions)
  (loop :for def :in definitions
        :for xref := (definition-to-location def)
        :when xref
        :collect xref))

(defun find-local-definition (point name)
  (alexandria:when-let (point (lem-lisp-syntax:search-local-definition point name))
    (list (make-xref-location :filespec (point-buffer point)
                              :position (position-at-point point)))))

(defun find-definitions-by-name (name)
  (let ((definitions (lisp-eval `(micros:find-definitions-for-emacs ,name))))
    (definitions-to-locations definitions)))

(defun find-definitions-default (point)
  (let ((name (or (symbol-string-at-point point)
                  (prompt-for-symbol-name "Edit Definition of: "))))
    (alexandria:when-let (result (find-local-definition point name))
      (return-from find-definitions-default result))
    (find-definitions-by-name name)))

(defparameter *find-definitions* '(find-definitions-default
                                   lem/link:find-definition))

(defun lisp-find-definitions (point)
  (check-connection)
  (display-xref-locations (some (alexandria:rcurry #'funcall point) *find-definitions*)))

(defun lisp-find-references (point)
  (check-connection)
  (let* ((name (or (symbol-string-at-point point)
                   (prompt-for-symbol-name "Edit uses of: ")))
         (data (lisp-eval `(micros:xrefs '(:calls :macroexpands :binds
                                           :references :sets :specializes)
                                         ,name))))
    (display-xref-references
     (loop
       :for (type . definitions) :in data
       :for defs := (definitions-to-locations definitions)
       :collect (make-xref-references :type type
                                      :locations defs)))))

(defun completion-symbol (point)
  (check-connection)
  (with-point ((start point)
               (end point))
    (skip-chars-backward start #'syntax-symbol-char-p)
    (skip-chars-forward end #'syntax-symbol-char-p)
    (when (point< start end)
      (lem-lisp-mode/completion:region-completion start end (current-package)))))

(defun completion-symbol-async (point then)
  (check-connection)
  (let ((string (symbol-string-at-point point)))
    (when string
      (remote-eval-from-string
       (current-connection)
       (lem-lisp-mode/completion:make-completions-form-string string (current-package))
       :continuation (lambda (result)
                       (alexandria:destructuring-ecase result
                         ((:ok completions)
                          (with-point ((start (current-point))
                                       (end (current-point)))
                            (skip-symbol-backward start)
                            (skip-symbol-forward end)
                            (funcall then
                                     (lem-lisp-mode/completion:make-completion-items
                                      completions
                                      start
                                      end))))
                         ((:abort condition)
                          (editor-error "abort ~A" condition))))
       :thread (current-micros-thread)
       :package (current-package)))))

(defun describe-symbol (symbol-name)
  (when symbol-name
    (when (string= "" symbol-name)
      (editor-error "No symbol given"))
    (let ((markdown (lisp-eval
                     `(micros/lsp-api:hover-symbol ,symbol-name))))
      (if (and markdown (not (alexandria:emptyp markdown)))
          (lem/hover:show-hover (lem/markdown-buffer:markdown-buffer markdown))
          (message "No documentation")))))

(defun lisp-describe-symbol-at-point (window)
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (dest-point (get-point-on-context-menu-open)))
    (when (eq (point-buffer point)
              (point-buffer dest-point))
      (move-point point dest-point)
      (describe-symbol (symbol-string-at-point point)))))

(define-command lisp-describe-symbol () ()
  (check-connection)
  (let ((symbol-name
          (prompt-for-symbol-name "Describe symbol: "
                                  (or (symbol-string-at-point (current-point)) ""))))
    (describe-symbol symbol-name)))

(defvar *wait-message-thread* nil)

(defun notify-change-connection-to-wait-message-thread ()
  (bt2:interrupt-thread *wait-message-thread*
                        (lambda () (error 'change-connection))))

(defun message-waiting-some-connections-p (&key (timeout 0))
  (with-broadcast-connections (connection)
    (when (message-waiting-p connection :timeout timeout)
      (return-from message-waiting-some-connections-p t))))

(defun start-thread ()
  (unless *wait-message-thread*
    (setf *wait-message-thread*
          (bt2:make-thread
           (lambda () (loop
                        :named exit
                        :do
                           (handler-case
                               (loop

                                 ;; workaround for windows
                                 ;;  (sleep seems to be necessary to receive
                                 ;;   change-connection event immediately)
                                    #+(and sbcl win32)
                                    (sleep 0.001)

                                    (unless (connected-p)
                                      (setf *wait-message-thread* nil)
                                      (return-from exit))
                                    (when (message-waiting-some-connections-p :timeout 1)
                                      (let ((barrior t))
                                        (send-event (lambda ()
                                                      (unwind-protect (progn (pull-events)
                                                                             (redraw-display))
                                                        (setq barrior nil))))
                                        (loop
                                          (unless (connected-p)
                                            (return))
                                          (unless barrior
                                            (return))
                                          (sleep 0.1)))))
                             (change-connection ()))))
           :name "lisp-wait-message"))))

(defun connected-slime-message (connection)
  (display-popup-message
   (format nil "Swank server running on ~A ~A"
           (connection-implementation-name connection)
           (connection-implementation-version connection))
   :timeout 1
   :style '(:gravity :center)))

(defun connect-to-micros (hostname port)
  (let ((connection
          (handler-case (if (eq hostname *localhost*)
                            (or (ignore-errors (new-connection "127.0.0.1" port))
                                (new-connection "localhost" port))
                            (new-connection hostname port))
            (error (c)
              (editor-error "~A" c)))))
    (add-and-change-connection connection)
    (start-thread)
    connection))

(define-command slime-connect (hostname port &optional (start-repl t))
    ((:splice
      (list (prompt-for-string "Hostname: " :initial-value *localhost*)
            (parse-integer
             (prompt-for-string "Port: "
                                :initial-value (princ-to-string *default-port*))))))
  (let ((connection (connect-to-micros hostname port)))
    (when start-repl (start-lisp-repl))
    (connected-slime-message connection)))

(defun connect-to-multiple-servers (host-and-ports)
  (dolist (host-and-port host-and-ports)
    (destructuring-bind (&key host port) host-and-port
      (connect-to-micros host port))))

(defun pull-events ()
  (when (connected-p)
    (with-broadcast-connections (connection)
      (handler-case (loop :while (message-waiting-p connection)
                          :do (dispatch-message (read-message connection)))
        (disconnected ()
          (remove-and-change-connection connection))))))

(defun read-from-minibuffer (thread tag prompt initial-value)
  (let ((input (prompt-for-sexp prompt initial-value)))
    (dispatch-message `(:emacs-return ,thread ,tag ,input))))

(defun show-source-location (source-location)
  (alexandria:destructuring-case source-location
    ((:error message)
     (display-message "~A" message))
    ((t &rest _)
     (declare (ignore _))
     (let ((xref-location (source-location-to-xref-location source-location)))
       (go-to-location xref-location
                       (lambda (buffer)
                         (switch-to-window
                          (pop-to-buffer buffer))))))))

(defun source-location-to-xref-location (location &optional content no-errors)
  (alexandria:destructuring-ecase location
    ((:location location-buffer position _hints)
     (declare (ignore _hints))
     (let ((buffer (location-buffer-to-buffer location-buffer)))
       (with-point ((point (buffer-point buffer)))
         (move-to-location-position point position)
         (make-xref-location :content (or content "")
                             :filespec buffer
                             :position (position-at-point point)))))
    ((:error message)
     (unless no-errors
       (editor-error "~A" message)))))

(defun location-buffer-to-buffer (location-buffer)
  (alexandria:destructuring-ecase location-buffer
    ((:file filename)
     (find-file-buffer (convert-remote-to-local-file filename)))
    ((:buffer buffer-name)
     (let ((buffer (get-buffer buffer-name)))
       (unless buffer (editor-error "~A is already deleted buffer" buffer-name))
       buffer))
    ((:buffer-and-file buffer filename)
     (or (get-buffer buffer)
         (find-file-buffer (convert-remote-to-local-file filename))))
    ((:source-form string)
     (let ((buffer (make-buffer "*lisp-source*")))
       (erase-buffer buffer)
       (change-buffer-mode buffer 'lisp-mode)
       (insert-string (buffer-point buffer) string)
       (buffer-start (buffer-point buffer))
       buffer))
    #+(or)((:zip file entry))
    ))

(defun move-to-location-position (point location-position)
  (alexandria:destructuring-ecase location-position
    ((:position pos)
     (move-to-bytes point (1+ pos)))
    ((:offset start offset)
     (move-to-position point (1+ start))
     (character-offset point offset))
    ((:line line-number &optional column)
     (move-to-line point line-number)
     (if column
         (line-offset point 0 column)
         (back-to-indentation point)))
    ((:function-name name)
     (buffer-start point)
     (search-forward-regexp point (ppcre:create-scanner
                                   `(:sequence
                                     "(def"
                                     (:greedy-repetition 1 nil (:char-class :word-char-class #\-))
                                     (:greedy-repetition 1 nil :whitespace-char-class)
                                     (:greedy-repetition 0 nil #\()
                                     ,name
                                     (:char-class :whitespace-char-class #\( #\)))
                                   :case-insensitive-mode t))
     (line-start point))
    ;; ((:method name specializers &rest qualifiers)
    ;;  )
    ;; ((:source-path source-path start-position)
    ;;  )
    ((:eof)
     (buffer-end point))))


(defun prompt-for-lisp-command ()
  (let* ((commands (lem-lisp-mode/implementation:get-usable-commands))
         (default-command (lem-lisp-mode/implementation:default-command))
         (command (prompt-for-string (format nil "lisp command (~A): " default-command)
                                     :completion-function (lambda (str) (completion str commands))
                                     :history-symbol 'read-lisp-command))
         (command (if (string= command "")
                      default-command
                      command)))
    (setf (config :slime-lisp-command) command)
    command))

(defun lisp-process-buffer-name (port)
  (format nil "*Run Lisp/~D*" port))

(defun get-lisp-process-buffer (port)
  (get-buffer (lisp-process-buffer-name port)))

(defun make-lisp-process-buffer (port)
  (make-buffer (lisp-process-buffer-name port)))

(defun run-lisp (&key command port directory)
  (labels ((output-callback (string)
             (let* ((buffer (make-lisp-process-buffer port))
                    (point (buffer-point buffer)))
               (buffer-end point)
               (insert-escape-sequence-string point string))))
    (let ((process
            (lem-process:run-process (uiop:split-string command)
                                     :directory directory
                                     :output-callback #'output-callback)))
      (lem-process:process-send-input process (format nil "(require :asdf)~%"))
      process)))

(defun send-micros-create-server (process port)
  (let ((file (asdf:system-source-file (asdf:find-system :micros))))
    (lem-process:process-send-input
     process
     (format nil "(asdf:load-asd ~S)" file)))
  ;; Try to quickload micros, but fallback to asdf:load-system if ql not installed
  (lem-process:process-send-input
   process
   "(handler-case (eval (read-from-string \"(ql:quickload :micros)\"))
      (error (c) (asdf:load-system :micros)))")
  (lem-process:process-send-input
   process
   (format nil "(micros:create-server :port ~D :dont-close t)~%" port)))

(defun run-slime (command &key (directory (buffer-directory)))
  (let* ((port (lem/common/socket:random-available-port))
         (process (run-lisp :command command :directory directory :port port)))
    (send-micros-create-server process port)
    (start-lisp-repl-internal :new-process t)
    (let ((spinner
            (start-loading-spinner :modeline
                                   :buffer (repl-buffer)
                                   :loading-message "Slime is starting up")))
      (let (timer
            (retry-count 0))
        (labels ((interval ()
                   (handler-case
                       (let ((conn (connect-to-micros *localhost* port)))
                         (setf (connection-command conn) command)
                         (setf (connection-process conn) process)
                         (setf (connection-process-directory conn) directory)
                         conn)
                     (editor-error (c)
                       (cond ((or (not (lem-process:process-alive-p process))
                                  (< 30 retry-count))
                              (failure c))
                             (t
                              (incf retry-count))))
                     (:no-error (conn)
                       (connected-slime-message conn)
                       ;; replのプロンプトの表示とカーソル位置の変更をしたいが
                       ;; 他のファイルの作業中にバッファ/ウィンドウが切り替わると作業の邪魔なので
                       ;; with-current-windowで元に戻す
                       (unless (repl-buffer)
                         (with-current-window (current-window) (start-lisp-repl)))
                       (success))))
                 (success ()
                   (finalize)
                   #-win32
                   (add-hook *exit-editor-hook* 'slime-quit-all))
                 (failure (c)
                   (finalize)
                   (pop-up-typeout-window (make-lisp-process-buffer port))
                   (error c))
                 (finalize ()
                   (stop-timer timer)
                   (stop-loading-spinner spinner)))
          (setf timer (start-timer (make-timer #'interval) 500 :repeat t)))))))

(define-command slime (&optional ask-command) (:universal-nil)
  (let ((command (if ask-command
                     (prompt-for-lisp-command)
                     (lem-lisp-mode/implementation:default-command))))
    (run-slime command)))

(defun delete-lisp-connection (connection)
  (prog1 (when (connection-process connection)
           (alexandria:when-let (buffer (get-lisp-process-buffer (connection-port connection)))
             (kill-buffer buffer))
           (lem-process:delete-process (connection-process connection))
           t)
    (remove-and-change-connection connection)
    (usocket:socket-close (lem-lisp-mode/connection::connection-socket connection))))

(define-command slime-quit () ()
  (when (self-connection-p (current-connection))
    (editor-error "The current connection is myself"))
  (when (current-connection)
    (delete-lisp-connection (current-connection))))

(defun slime-quit* ()
  (ignore-errors (slime-quit)))

(defun slime-quit-all ()
  (flet ((find-connection ()
           (dolist (c (connection-list))
             (when (connection-process c)
               (return c)))))
    (loop
      (let ((connection (find-connection)))
        (unless connection (return))
        (delete-lisp-connection connection)))))

(defun sit-for* (second)
  (loop :with end-time := (+ (get-internal-real-time)
                             (* second internal-time-units-per-second))
        :for e := (receive-event (float
                                  (/ (- end-time (get-internal-real-time))
                                     internal-time-units-per-second)))
        :while (key-p e)))

(define-command slime-restart () ()
  (when (current-connection)
    (alexandria:when-let ((last-command (connection-command (current-connection)))
                          (directory (connection-process-directory (current-connection))))
      (when (slime-quit)
        (sit-for* 3)
        (run-slime last-command :directory directory)))))

(define-command slime-self-connect (&optional (start-repl t)) ()
  (unless (self-connected-p)
    (self-connect))
  (when start-repl (start-lisp-repl)))


(defun buffer-pathname-type (buffer)
  (alexandria:when-let (pathname (buffer-filename buffer))
    (pathname-type pathname)))

(defun guess-current-position-package (point)
  (with-point ((p point))
    (loop
      (ppcre:register-groups-bind (package-name)
          ("^\\s*\\(\\s*(?:cl:|common-lisp:)?in-package (?:#?:|')?([^\)\\s]*)\\s*\\)"
           (string-downcase (line-string p)))
        (return package-name))
      (unless (line-offset p -1)
        (if (equal "asd" (buffer-pathname-type (point-buffer point)))
            (return "ASDF-USER")
            (return))))))

(defun update-buffer-package ()
  (let ((package (guess-current-position-package (current-point))))
    (when package
      (lisp-set-package package))))

(defun lisp-idle-function ()
  (when (connected-p)
    (let ((major-mode (buffer-major-mode (current-buffer))))
      (when (eq major-mode 'lisp-mode)
        (update-buffer-package)))))

(define-command lisp-scratch () ()
  (let ((buffer (primordial-buffer)))
    (change-buffer-mode buffer 'lisp-mode)
    (switch-to-buffer buffer)))

(defun highlight-region (start end attribute name)
  (let ((overlay (make-overlay start end attribute)))
    (start-timer (make-timer (lambda ()
                               (delete-overlay overlay))
                             :name name
                             :handle-function (lambda (err)
                                                (declare (ignore err))
                                                (ignore-errors
                                                  (delete-overlay overlay))))
                 100)))

(defun highlight-compilation-region (start end)
  (highlight-region start
                    end
                    'compilation-region-highlight
                    "delete-compilation-region-overlay"))

(defun highlight-evaluation-region (start end)
  (highlight-region start
                    end
                    'evaluation-region-highlight
                    "delete-evaluation-region-overlay"))

(add-hook (variable-value 'before-compile-functions :global)
          'highlight-compilation-region)

(add-hook (variable-value 'before-eval-functions :global)
          'highlight-evaluation-region)

;; workaround for windows
#+win32
(progn
  (defun slime-quit-all-for-win32 ()
    "quit slime and remove connection to exit lem normally on windows (incomplete)"
    (let ((conn-list (connection-list)))
      (slime-quit-all)
      (loop :while (current-connection)
            :do (remove-and-change-connection (current-connection)))
      #+sbcl
      (progn
        (sleep 0.5)
        (dolist (c conn-list)
          (let* ((s  (lem-lisp-mode/connection::connection-socket c))
                 (fd (sb-bsd-sockets::socket-file-descriptor (usocket:socket s))))
            (ignore-errors
              ;;(usocket:socket-shutdown s :IO)
              ;;(usocket:socket-close s)
              (sockint::shutdown fd sockint::SHUT_RDWR)
              (sockint::close fd)))))))
  (add-hook *exit-editor-hook* 'slime-quit-all-for-win32))

(define-file-type ("lisp" "asd" "cl" "lsp" "ros") lisp-mode)
