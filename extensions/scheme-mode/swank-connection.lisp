(in-package :lem-scheme-mode)

(define-editor-variable load-file-functions '())
(define-editor-variable before-compile-functions '())
(define-editor-variable before-eval-functions '())

(define-attribute compilation-region-highlight
  (t :background "orange"))

(define-attribute evaluation-region-highlight
  (t :background "green"))

;(defparameter *default-port* 4005)
(defparameter *default-port* 4006)
(defparameter *localhost* "127.0.0.1")

(defvar *connection-list* '())
(defvar *connection* nil)
(defvar *event-hooks* '())
(defvar *write-string-function* 'write-string-to-repl)
(defvar *last-compilation-result* nil)
;(defvar *indent-table* (make-hash-table :test 'equal))

;; for r7rs-swank (suppress error display for autodoc)
(defvar *suppress-error-disp* nil)

;; debug log
(defun dbg-log-format (fmt &rest args)
  (with-open-file (out "lemlog_swankconn0001.txt"
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :append)
    (fresh-line out)
    (apply #'format out fmt args)
    (terpri out)))

(defun change-current-connection (conn)
  (when *connection*
    (abort-all *connection* "change connection")
    (notify-change-connection-to-wait-message-thread))
  (setf *connection* conn))

(defun connected-p ()
  (not (null *connection*)))

(defun add-connection (conn)
  (push conn *connection-list*)
  (change-current-connection conn))

(defun remove-connection (conn)
  (setf *connection-list* (delete conn *connection-list*))
  ;(change-current-connection (car *connection-list*))
  (setf *connection* (car *connection-list*))
  *connection*)

(defun connection-mode-line (window)
  (format nil " [~A~A]"
          ;(buffer-package (window-buffer window) "(user)")
          (buffer-package (window-buffer window) "-")
          (if *connection*
              (format nil " ~A:~A"
                      (connection-implementation-name *connection*)
                      (connection-pid *connection*))
              "")))

(define-command scheme-connection-list () ()
  (lem/multi-column-list:display
   (make-instance 'lem/multi-column-list:multi-column-list
                  :columns '(" " "hostname" "port" "pid" "name" "version" "command")
                  :items *connection-list*
                  :column-function (lambda (c)
                                     (list (if (eq c *connection*) "*" "")
                                           (connection-hostname c)
                                           (connection-port c)
                                           (connection-pid c)
                                           (connection-implementation-name c)
                                           (connection-implementation-version c)
                                           (connection-command c)))
                  :select-callback (lambda (menu c)
                                     (declare (ignore menu))
                                     (change-current-connection c)))))

(defun check-connection ()
  (unless (connected-p)
    (editor-error "No connection for repl")))

;; for r7rs-swank (check unsupported function)
(defun check-aborted (message)
  (alexandria:destructuring-case message
    ((:return value id)
     (declare (ignore id))
     (alexandria:destructuring-case value
       ((:abort string)
        (declare (ignore string))
        (editor-error "Not supported"))))))

(defun check-scheme-mode ()
  (unless (eq (buffer-major-mode (current-buffer)) 'scheme-mode)
    (editor-error "You are not in scheme-mode")))

(defun buffer-package (buffer &optional default)
  (let ((package-name (buffer-value buffer "package" default)))
    (typecase package-name
      (null default)
      ((or symbol string)
       ;(string-upcase package-name)
       package-name
       )
      ((cons (or symbol string))
       ;(string-upcase (car package-name))
       (car package-name)
       ))))

(defun (setf buffer-package) (package buffer)
  (setf (buffer-value buffer "package") package))

(defvar *current-package* nil)

(defun current-package ()
  (or *current-package*
      (buffer-package (current-buffer))
      (connection-package *connection*)))

(defun current-micros-thread ()
  (or (buffer-value (current-buffer) 'thread)
      t))

(defun features ()
  (when (connected-p)
    (connection-features *connection*)))

(defun indentation-update (info)
  (declare (ignore info))
  ;(push (list :indentation-update info) lem-lisp-syntax.indent::*indent-log*)
  ;(loop :for (name indent packages) :in info
  ;      :do (lem-lisp-syntax:update-system-indentation name indent packages))
  ;#+(or)
  ;(loop :for (name indent packages) :in info
  ;      :do (dolist (package packages)
  ;            (unless (gethash package *indent-table*)
  ;              (setf (gethash package *indent-table*)
  ;                    (make-hash-table :test 'equal)))
  ;            (setf (gethash name (gethash package *indent-table*)) indent)))
  )

(defun scheme-rex (form &key
                        continuation
                        (thread (current-micros-thread))
                        (package (current-package)))
  (emacs-rex *connection*
             form
             :continuation continuation
             :thread thread
             :package package))

(defun scheme-eval-internal (emacs-rex-fun rex-arg package)
  (let ((tag (gensym))
        (thread-id (current-micros-thread)))
    (catch tag
      (funcall emacs-rex-fun
               *connection*
               rex-arg
               :continuation (lambda (result)
                               (alexandria:destructuring-ecase result
                                 ((:ok value)
                                  (throw tag value))
                                 ((:abort condition)
                                  (declare (ignore condition))
                                  (editor-error "Synchronous Scheme Evaluation aborted"))))
               :package package
               :thread thread-id)
      (handler-case (loop (sit-for 10 nil))
        (editor-abort ()
          (send-message-string *connection* (format nil "(:emacs-interrupt ~D)" thread-id))
          (keyboard-quit))))))

(defun scheme-eval-from-string (string &optional (package (current-package)))
  (scheme-eval-internal 'emacs-rex-string string package))

(defun scheme-eval (sexp &optional (package (current-package)))
  (scheme-eval-internal 'emacs-rex sexp package))

(defun scheme-eval-async (form &optional cont (package (current-package)))
  (let ((buffer (current-buffer)))
    (scheme-rex form
                :continuation (lambda (value)
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
                                   (unless *suppress-error-disp*
                                     (message "Evaluation aborted on ~A." condition))))
                                (setf *suppress-error-disp* nil))
                :thread (current-micros-thread)
                :package package)))

(defun eval-with-transcript (form)
  (scheme-rex form
              :continuation (lambda (value)
                              (alexandria:destructuring-ecase value
                                ((:ok x)
                                 (message "~A" x))
                                ((:abort condition)
                                 (message "Evaluation aborted on ~A." condition))))
              :package (current-package)))

(defun re-eval-defvar (string)
  (eval-with-transcript `(swank:re-evaluate-defvar ,string)))

(defun interactive-eval (string)
  (eval-with-transcript `(swank:interactive-eval ,string)))

(defun eval-print (string &optional print-right-margin)
  (let ((value (scheme-eval (if print-right-margin
                                `(let ((*print-right-margin* ,print-right-margin))
                                   (swank:eval-and-grab-output ,string))
                                `(swank:eval-and-grab-output ,string)))))
    (insert-string (current-point) (first value))
    (insert-character (current-point) #\newline)
    (insert-string (current-point) (second value))))

(defun new-package (name prompt-string)
  (setf (connection-package *connection*) name)
  (setf (connection-prompt-string *connection*) prompt-string)
  t)

;; for r7rs-swank (add parentheses)
(defun convert-package-name (string)
  (setf string (string-trim '(#\space #\tab) string))
  (when (or (= (length string) 0)
            (eql (char string 0) #\())
    (return-from convert-package-name string))

  ;; for r7rs-swank-Gauche-custom
  ;;  (convert Gauche's module name to R7RS's library name
  ;;   e.g. scheme.base ==> (scheme base) )
  (when (and (not (ppcre:scan "\\s" string))
             (ppcre:scan "\\." string))
    (loop :with ret := '()
          :with lis := (uiop:split-string string :separator ".")
          :while lis
          :do (if (string= (car lis) "")
                  (if (null (cdr lis))
                      (setf lis (cdr lis))
                      (progn
                        ;; convert '..' to '.'
                        (setf (car ret) (concatenate 'string (car ret) "." (cadr lis)))
                        (setf lis (cddr lis))))
                  (progn
                    (push (car lis) ret)
                    (setf lis (cdr lis))))
          :finally (setf string (format nil "~{~a~^ ~}" (reverse ret)))))

  (concatenate 'string "(" string ")"))

(defun read-package-name ()
  (check-connection)
  ;(let ((package-names (mapcar #'string-downcase
  ;                             (scheme-eval
  ;                              '(swank:list-all-package-names t)))))
  (let ((package-names (scheme-eval
                        '(swank:list-all-package-names t))))

    ;(dbg-log-format "package-names=~S" package-names)
    (convert-package-name
     (prompt-for-string "Library: "
                        :completion-function (lambda (str)
                                               (setf str (convert-package-name str))
                                               (completion str package-names))
                        :test-function (lambda (str)
                                         (setf str (convert-package-name str))
                                         (find str package-names :test #'string=))
                        :history-symbol 'mh-scheme-package))))

;(define-command scheme-set-package (package-name) ((list (read-package-name)))
(define-command scheme-set-library (package-name) ((read-package-name))
  (check-connection)
  (cond ((not *use-scheme-set-library*)
         (editor-error "Not supported"))
        ((string= package-name ""))
        ((or (eq *use-scheme-set-library* :repl)
             (eq (current-buffer) (repl-buffer)))
         (destructuring-bind (name prompt-string)
             (scheme-eval `(swank:set-package ,package-name))
           (new-package name prompt-string)
           (when (repl-buffer)
             (lem/listener-mode:refresh-prompt (repl-buffer)))))
        (t
         (check-scheme-mode)
         (setf (buffer-value (current-buffer) "package") package-name))))

(define-command scheme-interrupt () ()
  (check-connection)
  (send-message-string
   *connection*
   (format nil "(:emacs-interrupt ~A)" (current-micros-thread))))

(defun prompt-for-sexp (string &optional initial)
  (prompt-for-string string
                     :initial-value initial
                     :completion-function (lambda (str)
                                            (declare (ignore str))
                                            (completion-symbol (current-point)))
                     :history-symbol 'mh-scheme-sexp))

(define-command scheme-eval-string (string)
    ((prompt-for-sexp "Scheme Eval: "))
  (check-connection)
  (interactive-eval string))

;;(define-command scheme-eval-defun () ()
(define-command scheme-eval-define () ()
  (check-connection)
  (with-point ((point (current-point)))
    (lem-scheme-syntax:top-of-defun point)
    (with-point ((start point)
                 (end point))
      (scan-lists end 1 0)
      (run-hooks (variable-value 'before-eval-functions) start end)
      (let ((string (points-to-string start end)))
        ;(if (ppcre:scan "^\\(defvar(?:\\s|$)" string)
        ;    (re-eval-defvar string)
        ;    (interactive-eval string))
        (interactive-eval string)
        ))))

(define-command scheme-eval-buffer (&optional (buffer (current-buffer))) ()
  (check-connection)
  (interactive-eval (format nil "(begin ~a)" (buffer-text buffer))))

(defun get-operator-name ()
  (with-point ((point (current-point)))
    (scan-lists point -1 1)
    (character-offset point 1)
    (symbol-string-at-point point)))

(define-command scheme-echo-arglist () ()
  (check-connection)
  (let ((name (get-operator-name))
        (package (current-package)))
    (when name
      (scheme-eval-async `(swank:operator-arglist ,name ,package)
                         (lambda (arglist)
                           (when arglist
                             (message "~A" (ppcre:regex-replace-all "\\s+" arglist " "))))))))

(let (autodoc-symbol)
  (defun autodoc (function)
    (let ((context (lem-scheme-syntax:parse-for-swank-autodoc (current-point))))

      ;; for r7rs-swank (error check)
      (unless context
        (return-from autodoc))

      (unless autodoc-symbol
        (setf autodoc-symbol (intern "AUTODOC" :swank))

        ;; for r7rs-swank (only single colon name 'swank:autodoc' is accepted)
        (in-package :swank)
        (export 'swank::autodoc)
        (in-package :lem-scheme-mode))

      (setf *suppress-error-disp* t)
      (scheme-eval-async
       `(,autodoc-symbol ',context)
       (lambda (doc)
         (trivia:match doc
           ((list doc _)
            (unless (eq doc :not-available)
              (let* ((buffer (make-buffer "*swank:autodoc-fontity*"
                                          :temporary t :enable-undo-p nil))
                     (point (buffer-point buffer)))
                (erase-buffer buffer)
                (change-buffer-mode buffer 'scheme-mode)
                (insert-string point (ppcre:regex-replace-all "\\s*\\n\\s*" doc " "))
                (buffer-start point)
                (multiple-value-bind (result string)
                    (search-forward-regexp point "(?====> (.*) <===)")
                  (when result
                    (with-point ((start point))
                      (character-offset point 5)
                      (search-forward point "<===")
                      (delete-between-points start point)
                      (insert-string point string :attribute 'region))))
                (buffer-start (buffer-point buffer))
                (setf (variable-value 'line-wrap :buffer buffer) nil)
                (funcall function buffer))))))))))

(define-command scheme-autodoc-with-typeout () ()
  (check-connection)
  (autodoc (lambda (temp-buffer)
             (let ((buffer (make-buffer (buffer-name temp-buffer))))
               (with-buffer-read-only buffer nil
                 (erase-buffer buffer)
                 (insert-buffer (buffer-point buffer) temp-buffer))
               (with-pop-up-typeout-window (stream buffer)
                 (declare (ignore stream)))))))

(define-command scheme-autodoc () ()
  (check-connection)
  (autodoc (lambda (buffer) (message-buffer buffer))))

(define-command scheme-insert-space-and-autodoc (n) (:universal)
  (loop :repeat n :do (insert-character (current-point) #\space))
  (when (and (eq *use-scheme-autodoc* t)
             (connected-p))
    (scheme-autodoc)))

(defun check-parens ()
  (with-point ((point (current-point)))
    (buffer-start point)
    (loop :while (form-offset point 1))
    (skip-space-and-comment-forward point)
    (end-buffer-p point)))

(defun compilation-finished (result)

  ;; for r7rs-swank (check unsupported function)
  (check-aborted result)

  (setf *last-compilation-result* result)
  (destructuring-bind (notes successp duration loadp fastfile)
      (rest result)
    (show-compile-result notes duration
                         (if (not loadp)
                             successp
                             (and fastfile successp)))
    (highlight-notes notes)
    (when (and loadp fastfile successp)
      (scheme-eval-async `(swank:load-file ,fastfile)))))

(defun show-compile-result (notes secs successp)
  (message (format nil "~{~A~^ ~}"
                   (remove-if #'null
                              (list (if successp
                                        "Compilation finished"
                                        "Compilation failed")
                                    (unless notes
                                      "(No warnings)")
                                    (when secs
                                      (format nil "[~,2f secs]" secs)))))))

(defun make-highlight-overlay (pos buffer)
  (with-point ((point (buffer-point buffer)))
    (move-to-position point pos)
    (skip-chars-backward point #'syntax-symbol-char-p)
    (make-overlay point
                  (or (form-offset (copy-point point :temporary) 1)
                      (buffer-end-point buffer))
                  'compiler-note-attribute)))

(defvar *note-overlays* nil)

(defun highlight-notes (notes)
  (scheme-remove-notes)
  (when (and (null notes)
             (null (get-buffer-windows (get-buffer "*scheme-compilations*"))))
    (return-from highlight-notes))
  (when (dolist (note notes nil)
          (trivia:match note
            ((trivia:property :location location)
             (when (source-location-to-xref-location location nil t)
               (return t)))))
    (lem/peek-source:with-collecting-sources (collector)
      (dolist (note notes)
        (trivia:match note
          ((and (trivia:property :location location)
                (or (trivia:property :message message) (and))
                (or (trivia:property :source-context source-context) (and)))
           (alexandria:when-let ((xref-location (source-location-to-xref-location location nil t)))
             (let* ((name (xref-filespec-to-filename (xref-location-filespec xref-location)))
                    (pos (xref-location-position xref-location))
                    (buffer (xref-filespec-to-buffer (xref-location-filespec xref-location))))
               (lem/peek-source:with-appending-source
                   (cur-point :move-function (alexandria:curry #'go-to-location xref-location))
                 (insert-string cur-point name :attribute 'lem/peek-source:filename-attribute)
                 (insert-string cur-point ":")
                 (insert-string cur-point (princ-to-string pos)
                                :attribute 'lem/peek-source:position-attribute)
                 (insert-string cur-point ":")
                 (insert-character cur-point #\newline 1)
                 (insert-string cur-point message)
                 (insert-character cur-point #\newline)
                 (insert-string cur-point source-context))
               (push (make-highlight-overlay pos buffer)
                     *note-overlays*)))))))))

(define-command scheme-remove-notes () ()
  (mapc #'delete-overlay *note-overlays*)
  (setf *note-overlays* '()))

(define-command scheme-compile-and-load-file () ()
  (check-connection)

  (unless (buffer-filename (current-buffer))
    (editor-error "No file to compile"))

  (when (buffer-modified-p (current-buffer))
    (when (prompt-for-y-or-n-p "Save file")
      (save-current-buffer)))
  (let ((file (buffer-filename (current-buffer))))
    (run-hooks (variable-value 'load-file-functions) file)
    (scheme-eval-async `(swank:compile-file-for-emacs ,(write-string file) t)
                       #'compilation-finished)))

(define-command scheme-compile-region (start end) (:region)
  (check-connection)
  (let ((string (points-to-string start end))
        (position `((:position ,(position-at-point start))
                    (:line
                     ,(line-number-at-point (current-point))
                     ,(point-charpos (current-point))))))
    (run-hooks (variable-value 'before-compile-functions) start end)
    (scheme-eval-async `(swank:compile-string-for-emacs ,string
                                                        ,(buffer-name (current-buffer))
                                                        ',position
                                                        ,(buffer-filename (current-buffer))
                                                        nil)
                       #'compilation-finished)))

;;(define-command scheme-compile-defun () ()
(define-command scheme-compile-define () ()
  (check-connection)
  (with-point ((point (current-point)))
    (lem-scheme-syntax:top-of-defun point)
    (with-point ((start point)
                 (end point))
      (scan-lists end 1 0)
      (scheme-compile-region start end))))

;(defun form-string-at-point ()
;  (with-point ((point (current-point)))
(defun form-string-at-point (point)
  (skip-chars-backward point #'syntax-symbol-char-p)
  (with-point ((start point)
               (end point))
    (form-offset end 1)
    (points-to-string start end)))

(defun macroexpand-internal (expander)
  (let* ((self (eq (current-buffer) (get-buffer "*scheme-macroexpand*")))
         ;(orig-package-name (buffer-package (current-buffer) "(user)"))
         (orig-package-name (buffer-package (current-buffer)))
         ;(p (and self (copy-point (current-point) :temporary)))
         )

    ;; for r7rs-swank (move point to the outside of parentheses)
    (with-point ((p (current-point)))
      ;(maybe-beginning-of-string p)
      (unless (eql (character-at p) #\()
        (scan-lists p -1 1 t))

      (scheme-eval-async `(,expander ,(form-string-at-point p))
                         (lambda (string)

                           ;; for r7rs-swank-Gauche-custom
                           ;;  (if macro expanded result is string literal,
                           ;;   we decode escape sequence characters (\n \t).)
                           (with-input-from-string (in string)
                             (if (eql (peek-char t in nil) #\")
                                 (setf string (lem-scheme-mode.swank-protocol::read-string in))))

                           (let ((buffer (make-buffer "*scheme-macroexpand*")))
                             (with-buffer-read-only buffer nil
                               (unless self (erase-buffer buffer))
                               (change-buffer-mode buffer 'scheme-mode)
                               ;(setf (buffer-package buffer) orig-package-name)
                               (when orig-package-name
                                 (setf (buffer-package buffer) orig-package-name))
                               (when self
                                 (move-point (current-point) p)
                                 (kill-sexp))
                               (insert-string (buffer-point buffer)
                                              string)
                               (indent-points (buffer-start-point buffer)
                                              (buffer-end-point buffer))
                               (with-pop-up-typeout-window (s buffer)
                                 (declare (ignore s)))
                               (when self
                                 (move-point (buffer-point buffer) p)))))))))

(define-command scheme-macroexpand () ()
  (check-connection)
  ;; for r7rs-swank (swank-macroexpand-1 is not supported)
  ;(macroexpand-internal 'swank:swank-macroexpand-1)
  (macroexpand-internal 'swank:swank-expand-1)
  )

(define-command scheme-macroexpand-all () ()
  (check-connection)
  (macroexpand-internal 'swank:swank-macroexpand-all))

;; for r7rs-swank (fuzzy-completions is not supported)
;(defvar *completion-symbol-with-fuzzy* t)
(defvar *completion-symbol-with-fuzzy* nil)

(defun symbol-completion (str &optional (package (current-package)))
  (let* ((fuzzy *completion-symbol-with-fuzzy*)
         (result (scheme-eval-from-string
                  (format nil "(~A ~S ~S)"
                          (if fuzzy
                              "swank:fuzzy-completions"
                              "swank:completions")
                          str
                          package)
                  ;"COMMON-LISP"
                  )))
    (when result
      (destructuring-bind (completions timeout-p) result
        (declare (ignore timeout-p))
        (completion-hyphen str (mapcar (if fuzzy #'first #'identity) completions))))))

(defun prompt-for-symbol-name (prompt &optional (initial ""))
  (let ((package (current-package)))
    (prompt-for-string prompt
                       :initial-value initial
                       :completion-function (lambda (str)
                                              (symbol-completion str package))
                       :history-symbol 'mh-scheme-read-symbol)))

(defun definition-to-location (definition)
  (destructuring-bind (title location) definition
    (source-location-to-xref-location location title t)))

(defun definitions-to-locations (definitions)
  (loop :for def :in definitions
        :for xref := (definition-to-location def)
        :when xref
        :collect xref))

;(defun find-local-definition (point name)
;  (let ((point (lem-lisp-syntax:search-local-definition point name)))
;    (when point
;      (list (make-xref-location :filespec (point-buffer point)
;                                :position (position-at-point point))))))

(defun find-definitions-default (point)
  (let ((name (or (symbol-string-at-point point)
                  (prompt-for-symbol-name "Edit Definition of: "))))
    ;(let ((result (find-local-definition point name)))
    ;  (when result
    ;    (return-from find-definitions-default result)))
    (let ((definitions (scheme-eval `(swank:find-definitions-for-emacs ,name))))

      ;; for r7rs-swank (check unsupported function)
      (check-aborted definitions)

      (definitions-to-locations definitions))))

(defparameter *find-definitions* '(find-definitions-default))

(defun scheme-find-definitions (point)
  (check-connection)
  (display-xref-locations (some (alexandria:rcurry #'funcall point) *find-definitions*)))

(defun scheme-find-references (point)
  (check-connection)
  (let* ((name (or (symbol-string-at-point point)
                   (prompt-for-symbol-name "Edit uses of: ")))
         (data (scheme-eval `(swank:xrefs '(:calls :macroexpands :binds
                                            :references :sets :specializes)
                                          ,name))))

    ;; for r7rs-swank (check unsupported function)
    (check-aborted data)

    (display-xref-references
     (loop
       :for (type . definitions) :in data
       :for defs := (definitions-to-locations definitions)
       :collect (make-xref-references :type type
                                      :locations defs)))))

(defun completion-symbol (point)
  ;(check-connection)
  (with-point ((start point)
               (end point))
    (skip-chars-backward start #'syntax-symbol-char-p)
    (skip-chars-forward end #'syntax-symbol-char-p)
    (when (point< start end)
      (cond
        ((connected-p)
         (let* ((fuzzy *completion-symbol-with-fuzzy*)
                (result
                  (scheme-eval-from-string (format nil "(~A ~S ~S)"
                                                   (if fuzzy
                                                       "swank:fuzzy-completions"
                                                       "swank:completions")
                                                   (points-to-string start end)
                                                   (current-package)))))
           (when result
             (destructuring-bind (completions timeout-p) result
               (declare (ignore timeout-p))
               (mapcar (lambda (completion)
                         (make-completion-item
                          :label (if fuzzy
                                     (first completion)
                                     completion)
                          :detail (if fuzzy
                                      (fourth completion)
                                      "")
                          :start start
                          :end end))
                       completions)))))
        (t
         (mapcar (lambda (name)
                   (make-completion-item :label name
                                         :start start
                                         :end end))
                 (completion (points-to-string start end)
                             *scheme-completion-names*
                             :test #'alexandria:starts-with-subseq)))))))

(defun show-description (string)
  (let ((buffer (make-buffer "*scheme-description*")))
    (change-buffer-mode buffer 'scheme-mode)
    (with-pop-up-typeout-window (stream buffer :erase t)
      (princ string stream))))

(defun scheme-eval-describe (form)
  (scheme-eval-async form #'show-description))

(define-command scheme-describe-symbol () ()
  (check-connection)
  (let ((symbol-name
          (prompt-for-symbol-name "Describe symbol: "
                                  (or (symbol-string-at-point (current-point)) ""))))
    (when (string= "" symbol-name)
      (editor-error "No symbol given"))
    (scheme-eval-describe `(swank:describe-symbol ,symbol-name))))

(defvar *wait-message-thread* nil)

(defun notify-change-connection-to-wait-message-thread ()
  (bt2:interrupt-thread *wait-message-thread*
                       (lambda () (error 'change-connection))))

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
                                    (when (message-waiting-p *connection* :timeout 1)
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
           :name "scheme-wait-message"))))

(define-command scheme-slime-connect (hostname port &optional (start-repl t))
    ((:splice
      (list (prompt-for-string "Hostname: " :initial-value *localhost*)
            (parse-integer
             (prompt-for-string "Port: "
                                :initial-value (princ-to-string *default-port*))))))
  (enable-scheme-slime-commands)
  (message "Connecting...")
  (let (connection)
    (handler-case (setf connection
                        (if (eq hostname *localhost*)
                            (or (ignore-errors (new-connection "127.0.0.1" port))
                                (new-connection "localhost" port))
                            (new-connection hostname port)))
      (error (c)
        (editor-error "~A" c)))
    (message "Swank server running on ~A ~A"
             (connection-implementation-name connection)
             (connection-implementation-version connection))
    (add-connection connection)
    (when start-repl (start-scheme-repl))
    (start-thread)
    connection))

(defvar *unknown-keywords* nil)
(defun pull-events ()
  (when (and (boundp '*connection*)
             (not (null *connection*)))
    (handler-case (loop :while (message-waiting-p *connection*)
                        :do (dispatch-message (read-message *connection*)))
      (disconnected ()
        (remove-connection *connection*)))))

(defun dispatch-message (message)
  (log-message (prin1-to-string message))
  (dolist (e *event-hooks*)
    (when (funcall e message)
      (return-from dispatch-message)))
  (alexandria:destructuring-case message
    ((:write-string string &rest rest)
     (declare (ignore rest))
     ;(dbg-log-format "write-string=~S" string)
     (funcall *write-string-function* string))
    ((:read-string thread tag)
     (repl-read-string thread tag))
    ((:read-aborted thread tag)
     (repl-abort-read thread tag))
    ;; ((:open-dedicated-output-stream port coding-system)
    ;;  )
    ((:new-package name prompt-string)
     (new-package name prompt-string))
    ((:return value id)

     ;; for r7rs-swank (display error message)
     (unless *suppress-error-disp*
       (alexandria:destructuring-case value
         ((:abort string)
          (funcall *write-string-function*
                   (format nil "; Evaluation aborted: ~A~%" string)))))

     (finish-evaluated *connection* value id))
    ;; ((:channel-send id msg)
    ;;  )
    ;; ((:emacs-channel-send id msg)
    ;;  )
    ((:read-from-minibuffer thread tag prompt initial-value)
     (read-from-minibuffer thread tag prompt initial-value))
    ((:y-or-n-p thread tag question)
     (dispatch-message `(:emacs-return ,thread ,tag ,(prompt-for-y-or-n-p question))))
    ((:emacs-return-string thread tag string)
     (send-message-string
      *connection*
      (format nil "(:emacs-return-string ~A ~A ~S)"
              thread
              tag
              string)))
    ((:new-features features)
     (setf (connection-features *connection*)
           features))
    ((:indentation-update info)
     (indentation-update info))
    ((:eval-no-wait form)
     (eval (read-from-string form)))
    ;; ((:eval thread tag form-string)
    ;;  )
    ((:emacs-return thread tag value)
     (send-message-string
      *connection*
      (format nil "(:emacs-return ~A ~A ~S)" thread tag value)))
    ;; ((:ed what)
    ;;  )
    ;; ((:inspect what thread tag)
    ;;  )
    ;; ((:background-message message)
    ;;  )
    ((:debug-condition thread message)
     (assert thread)
     (message "~A" message))
    ((:ping thread tag)
     (send-message-string
      *connection*
      (format nil "(:emacs-pong ~A ~A)" thread tag)))
    ;; ((:reader-error packet condition)
    ;;  )
    ;; ((:invalid-rpc id message)
    ;;  )
    ;; ((:emacs-skipped-packet _pkg))
    ;; ((:test-delay seconds)
    ;;  )
    ((t &rest args)
     (declare (ignore args))
     (pushnew (car message) *unknown-keywords*))))

(defun read-from-minibuffer (thread tag prompt initial-value)
  (let ((input (prompt-for-sexp prompt initial-value)))
    (dispatch-message `(:emacs-return ,thread ,tag ,input))))

(defun show-source-location (source-location)
  (alexandria:destructuring-case source-location
    ((:error message)
     (message "~A" message))
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
     (find-file-buffer filename))
    ((:buffer buffer-name)
     (let ((buffer (get-buffer buffer-name)))
       (unless buffer (editor-error "~A is already deleted buffer" buffer-name))
       buffer))
    ((:buffer-and-file buffer filename)
     (or (get-buffer buffer)
         (find-file-buffer filename)))
    ((:source-form string)
     (let ((buffer (make-buffer "*scheme-source*")))
       (erase-buffer buffer)
       (change-buffer-mode buffer 'scheme-mode)
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

(defun initialize-forms-string (port)
  (with-output-to-string (out)
    (format out "(swank:create-server :port ~D :dont-close t)~%" port)
    (write-line "(loop (sleep most-positive-fixnum))" out)))

(defun run-swank-server (command port &key (directory (buffer-directory)))
  (bt2:make-thread
   (lambda ()
     (with-input-from-string
         (input (initialize-forms-string port))
       (multiple-value-bind (output error-output status)

           ;; for r7rs-swank (error check)
           (handler-case
               (uiop:run-program command
                                 ;:input input
                                 :output :string
                                 :error-output :string
                                 :directory directory
                                 :ignore-error-status t)
             (error (c) (values "" (format nil "~A" c) -1)))

         (unless (zerop status)
           (send-event (lambda ()
                         (let ((buffer (make-buffer "*Run Scheme Output*")))
                           (with-pop-up-typeout-window (stream buffer
                                                               :erase t
                                                               :read-only t)
                             (format stream "command: ~A~%" command)
                             (format stream "status: ~A~%" status)
                             (format stream "port: ~A~%" port)
                             (format stream "directory: ~A~%" directory)
                             (write-string output stream)
                             (write-string error-output stream)))))))))
   :name (format nil "run-scheme-swank-server-thread '~A'" command)))

(defun run-slime (command &key (directory (buffer-directory)))
  ;;(unless command
  ;;  (setf command (get-lisp-command :impl *impl-name*)))
  (let ((port (or (lem/common/socket:port-available-p *default-port*)
                  (lem/common/socket:random-available-port))))

    ;; for r7rs-swank (make command)
    (unless command
      (setf command
            (mapcar (lambda (str)
                      (ppcre:regex-replace-all ",port"
                                               str
                                               (write-to-string port)))
                    (uiop:ensure-list *scheme-swank-server-run-command*))))
    ;;(dbg-log-format "command=~S" command)

    (let ((thread (run-swank-server command port :directory directory)))
      (sleep 0.5)
      (unless (bt2:thread-alive-p thread)
        (editor-error "Scheme swank server start error")))

    (let ((successp)
          (condition))
      (loop :repeat 10
            :do (handler-case
                    (let ((conn (scheme-slime-connect *localhost* port t)))
                      (setf (connection-command conn) command)
                      (setf (connection-process-directory conn) directory)
                      (setf successp t)
                      (return))
                  (editor-error (c)
                    (setf condition c)
                    (sleep 0.5))))
      (unless successp
        (error condition)))
    #-win32
    (add-hook *exit-editor-hook* 'slime-quit-all)))

;(define-command scheme-slime (&optional ask-impl) ("P")
;  (let ((command (if ask-impl (prompt-for-impl))))
;    (run-slime command)))
(define-command scheme-slime () ()
  (enable-scheme-slime-commands)
  (run-slime nil))

(define-command scheme-slime-quit () ()
  (when *connection*
    (prog1 (when (connection-command *connection*)
             ;(scheme-rex '(uiop:quit))
             ;(scheme-rex '(swank:quit-lisp))
             (ignore-errors (interactive-eval "(exit)"))
             t)
      (remove-connection *connection*))))

(defun slime-quit-all ()
  (flet ((find-connection ()
           (dolist (c *connection-list*)
             (when (connection-command c)
               (return c)))))
    (loop
      (let ((*connection* (find-connection)))
        (unless *connection* (return))
        (scheme-slime-quit)))))

(defun sit-for* (second)
  (loop :with end-time := (+ (get-internal-real-time)
                             (* second internal-time-units-per-second))
        :for e := (receive-event (float
                                  (/ (- end-time (get-internal-real-time))
                                     internal-time-units-per-second)))
        :while (key-p e)))

(define-command scheme-slime-restart () ()
  (when *connection*
    (alexandria:when-let ((last-command (connection-command *connection*))
                          (directory (connection-process-directory *connection*)))
      (when (scheme-slime-quit)
        (sit-for* 3)
        (run-slime last-command :directory directory)))))

(defun scan-current-package (point)
  (with-point ((p point))
    (loop
      (multiple-value-bind (result groups)
          (looking-at (line-start p)
                      ;"^\\s*\\((?:cl:)?in-package (?:#?:|')?([^\)]*)\\)")
                      "^\\s*\\(select-module\\s*([^\)]*)\\)")
        (when result
          (let ((package (aref groups 0)))
            (when package
              (return package))))
        (unless (line-offset p -1)
          (return))))))

(defun update-buffer-package ()
  (let ((package (scan-current-package (current-point))))
    (when package
      (scheme-set-library package))))

(defun scheme-idle-function ()
  (when (connected-p)

    ;; for r7rs-swank (error check)
    (handler-case
        (let ((major-mode (buffer-major-mode (current-buffer))))
          ;(when (and (eq major-mode 'scheme-mode)
          ;           *use-scheme-set-library*
          ;           (not (eq *use-scheme-set-library* :repl)))
          ;  (update-buffer-package))
          (when (and (eq *use-scheme-autodoc* :auto)
                     (member major-mode '(scheme-mode scheme-repl-mode)))
            (scheme-autodoc)))
      (error () (scheme-slime-quit)))))

(defun highlight-region (start end attribute name)
  (let ((overlay (make-overlay start end attribute)))
    (start-timer (make-timer (lambda ()
                               (delete-overlay overlay))
                             :handle-function (lambda (err)
                                                (declare (ignore err))
                                                (ignore-errors
                                                  (delete-overlay overlay)))
                             :name name)
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
    (let ((conn-list (copy-list *connection-list*)))
      (slime-quit-all)
      (loop :while *connection*
            :do (remove-connection *connection*))
      #+sbcl
      (progn
        (sleep 0.5)
        (dolist (c conn-list)
          (let* ((s  (lem-scheme-mode.swank-protocol::connection-socket c))
                 (fd (sb-bsd-sockets::socket-file-descriptor (usocket:socket s))))
            (ignore-errors
              ;;(usocket:socket-shutdown s :IO)
              ;;(usocket:socket-close s)
              (sockint::shutdown fd sockint::SHUT_RDWR)
              (sockint::close fd)))))))
  (add-hook *exit-editor-hook* 'slime-quit-all-for-win32))
