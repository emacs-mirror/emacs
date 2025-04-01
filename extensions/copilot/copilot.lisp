(defpackage :lem-copilot
  (:use :cl
        :lem
        :lem-copilot/utils)
  (:local-nicknames (:client :lem-copilot/client)
                    (:logger :lem-copilot/logger)))
(in-package :lem-copilot)

(define-attribute suggestion-attribute
  (t :foreground "dark gray"))

(define-attribute cycling-attribute
  (t :foreground "green"))

(defvar *client* nil)

(defun client ()
  *client*)

(defun copilot-root ()
  (merge-pathnames "copilot/" (lem-home)))

(defun copilot-path ()
  (merge-pathnames "lib/node_modules/copilot-node-server/copilot/dist/language-server.js"
                   (copilot-root)))

(defun installed-copilot-server-p ()
  (uiop:file-exists-p (copilot-path)))

(defun run-process ()
  (async-process:create-process (list "node"
                                      (namestring (copilot-path))
                                      "--stdio")))

(defun kill-process ()
  (when (client)
    (async-process:delete-process (client:client-process (client)))))

(add-hook *exit-editor-hook* 'kill-process)

(defun enable-copilot-p ()
  (config :copilot))

(defun enable-copilot ()
  (setf (config :copilot) t))


;;; utils
(defvar *language-id-map* (make-hash-table :test 'eq))

(defun register-language-id (mode language-id)
  (setf (gethash mode *language-id-map*) language-id))

(defun buffer-language-id (buffer)
  (gethash (buffer-major-mode buffer) *language-id-map* "text"))

(defun buffer-uri (buffer)
  (lem-lsp-mode::buffer-uri buffer))

(defun buffer-version (buffer)
  (buffer-value buffer 'version 0))

(defun (setf buffer-version) (version buffer)
  (setf (buffer-value buffer 'version) version))

(defun buffer-last-version (buffer)
  (buffer-value buffer 'last-version))

(defun (setf buffer-last-version) (last-version buffer)
  (setf (buffer-value buffer 'last-version) last-version))

(defun buffer-update-version-p (buffer)
  (not (equal (buffer-version buffer)
              (buffer-last-version buffer))))

(defun buffer-completions-cache (buffer)
  (buffer-value buffer 'completions-cache))

(defun (setf buffer-completions-cache) (completions-cache buffer)
  (setf (buffer-value buffer 'completions-cache) completions-cache))

(defun buffer-showing-suggestions-p (buffer)
  (buffer-value buffer 'showing-suggestions-p))

(defun (setf buffer-showing-suggestions-p) (showing-suggestions-p buffer)
  (setf (buffer-value buffer 'showing-suggestions-p) showing-suggestions-p))

(defun point-to-lsp-position (point)
  (hash "line" (1- (line-number-at-point point))
        "character" (point-charpos point)))

(defun move-to-lsp-position (point position)
  (move-to-line point (1+ (gethash "line" position)))
  (line-offset point 0 (gethash "character" position)))

(defun text-document-params (buffer)
  (list :uri (buffer-uri buffer)
        :language-id (buffer-language-id buffer)
        :version (buffer-version buffer)
        :text (buffer-text buffer)))

(defun notify-text-document/did-open (buffer)
  (apply #'client:text-document/did-open
         (client)
         (text-document-params buffer)))

(defun notify-text-document/did-close (buffer)
  (client:text-document/did-close (client) :uri (buffer-uri buffer)))

(defun notify-text-document/did-focus (buffer)
  (client:text-document/did-focus (client) :uri (buffer-uri buffer)))

(defun notify-text-document/did-change (buffer content-changes)
  (let ((version (incf (buffer-version buffer))))
    (client:text-document/did-change (client)
                                      :uri (buffer-uri buffer)
                                      :version version
                                      :content-changes content-changes)))

(defun initialize (client then)
  (client:initialize client
                     :callback (lambda (response)
                                 (send-event (lambda ()
                                               (funcall then response))))))

(defun set-editor-info (client then)
  (client:set-editor-info client
                          :callback (lambda (response)
                                      (send-event (lambda ()
                                                    (funcall then response))))))


;;; copilot-mode
(define-minor-mode copilot-mode
    (:name "Copilot"
     :keymap *copilot-mode-keymap*
     :enable-hook 'copilot-mode-on
     :disable-hook 'copilot-mode-off))

(define-key *copilot-mode-keymap* "M-n" 'copilot-next-suggestion)
(define-key *copilot-mode-keymap* "M-p" 'copilot-previous-suggestion)

(defun setup-client-async (then)
  (let ((client (client:run-client :process (run-process))))
    (client:connect client)
    (initialize client
                (lambda (response)
                  (declare (ignore response))
                  (client:initialized client)
                  (set-editor-info client
                                   (lambda (response)
                                     (declare (ignore response))
                                     (setf *client* client)
                                     (funcall then)))))))

(defun copilot-mode-on ()
  (unless (installed-copilot-server-p)
    (copilot-install-server)
    (reset-buffers))
  (flet ((fn ()
           (add-hook (variable-value 'kill-buffer-hook :buffer (current-buffer)) 'on-kill-buffer)
           (add-hook (variable-value 'before-change-functions :buffer (current-buffer)) 'on-before-change)
           (add-hook *window-show-buffer-functions* 'on-window-show-buffer)
           (add-hook *switch-to-window-hook* 'on-switch-to-window)
           (add-hook *post-command-hook* 'on-post-command)
           (notify-text-document/did-open (current-buffer))))
    (if (client)
        (fn)
        (setup-client-async #'fn))))

(defun copilot-mode-off ()
  (remove-hook (variable-value 'kill-buffer-hook :buffer (current-buffer)) 'on-kill-buffer)
  (remove-hook (variable-value 'before-change-functions :buffer (current-buffer)) 'on-before-change)
  (remove-hook *window-show-buffer-functions* 'on-window-show-buffer)
  (remove-hook *switch-to-window-hook* 'on-switch-to-window))

(defun copilot-mode-p (buffer)
  (mode-active-p buffer 'copilot-mode))

(defun on-kill-buffer (buffer)
  (when (copilot-mode-p buffer)
    (notify-text-document/did-close buffer)))

(defun on-post-command ()
  (cancel-inline-completion))

(defun before-change-arg-to-content-change (point arg)
  (etypecase arg
    (string
     (let ((position (point-to-lsp-position point)))
       (hash "range" (hash "start" position
                           "end" position)
             "text" arg)))
    (integer
     (with-point ((end point))
       (character-offset end arg)
       (hash "range" (hash "start" (point-to-lsp-position point)
                           "end" (point-to-lsp-position end))
             "text" "")))))

(defvar *inhibit-did-change-notification* nil)

(defun on-before-change (point arg)
  (let ((buffer (point-buffer point)))
    (when (and (copilot-mode-p buffer)
               (not *inhibit-did-change-notification*))
      (notify-text-document/did-change
       buffer
       (vector (before-change-arg-to-content-change point arg))))))

(defun on-window-show-buffer (window)
  (let ((buffer (window-buffer window)))
    (when (copilot-mode-p buffer)
      (notify-text-document/did-focus buffer))))

(defun on-switch-to-window (previous-window current-window)
  (declare (ignore previous-window))
  (let ((buffer (window-buffer current-window)))
    (when (copilot-mode-p buffer)
      (notify-text-document/did-focus buffer))))

(defun reset-buffers ()
  (dolist (buffer (remove-if-not #'copilot-mode-p (buffer-list)))
    (setf (buffer-version buffer) 0)
    (notify-text-document/did-open buffer)))

(define-command copilot-restart () ()
  (async-process:delete-process (client:client-process (client)))
  (setup-client-async (lambda ()
                        (reset-buffers)
                        (show-message "copilot restarted"
                                      :style '(:gravity :center)
                                      :timeout 3)
                        (redraw-display))))


;;; complete
(defvar *inline-completion-request* nil)
(defvar *completion-canceled* nil)

(defvar *copilot-completion-keymap* (make-keymap :name "Copilot Completion"))

(define-key *copilot-completion-keymap* "Tab" 'copilot-accept-suggestion)
(define-key *copilot-completion-keymap* 'copilot-next-suggestion 'copilot-next-suggestion)
(define-key *copilot-completion-keymap* 'copilot-previous-suggestion 'copilot-previous-suggestion)

(defun find-copilot-completion-command (key)
  (lookup-keybind key
                  :keymaps (append (lem-core::all-keymaps)
                                   (list *copilot-completion-keymap*))))

(defun search-preffix (str1 str2)
  (loop :for i :from 0
        :for c1 :across str1
        :for c2 :across str2
        :while (char= c1 c2)
        :finally (return i)))

(defun replace-with-inline-completion (point item)
  (let ((range (gethash "range" item)))
    (with-point ((start point :left-inserting)
                 (end point :right-inserting))
      (move-to-lsp-position start (gethash "start" range))
      (move-to-lsp-position end (gethash "end" range))
      (let* ((insert-text (gethash "insertText" item))
             (text (points-to-string start end))
             (pos (search-preffix text insert-text)))
        (character-offset start pos)
        (delete-between-points start end)
        (insert-string end
                       (subseq insert-text pos))))))

(defun preview-inline-completion-item (point item &key additional-text)
  (let ((range (gethash "range" item)))
    (with-point ((start point :left-inserting)
                 (end point :left-inserting))
      (move-to-lsp-position start (gethash "start" range))
      (move-to-lsp-position end (gethash "end" range))
      (let* ((insert-text (gethash "insertText" item))
             (text (points-to-string start end))
             (pos (search-preffix text insert-text)))
        (character-offset start pos)
        (delete-between-points start end)
        (setf (buffer-showing-suggestions-p (point-buffer point)) t)
        (insert-string end
                       (subseq insert-text pos)
                       :attribute 'suggestion-attribute)
        (when additional-text
          (insert-string end " ")
          (insert-string end additional-text
                         :attribute 'cycling-attribute))))))

(defun unshow-inline-completion (point)
  (when (buffer-showing-suggestions-p (point-buffer point))
    (setf (buffer-showing-suggestions-p (point-buffer point)) nil)
    (let ((*inhibit-did-change-notification* t))
      (buffer-undo point))))

(defun elt-safety (items index)
  (if (<= (length items) index)
      (elt items (1- (length items)))
      (elt items index)))

(defun show-inline-completion (point items &key (index 0) cycling)
  (let ((item (elt-safety items index))
        (buffer (point-buffer point))
        (*inhibit-did-change-notification* t))
    (lem-lsp-mode::reset-buffer-diagnostic buffer)
    (buffer-undo-boundary buffer)
    (save-excursion
      (preview-inline-completion-item point
                                      item
                                      :additional-text (when cycling
                                                         (format nil
                                                                 "[~D/~D]"
                                                                 (1+ index)
                                                                 (length items)))))
    (loop :for v := (sit-for 10)
          :while (eq v :timeout)
          :finally (return-from show-inline-completion v))))

(defun prompt-inline-completion (point items &key (index 0) cycling)
  (when items
    (let ((buffer (point-buffer point))
          (key (show-inline-completion point items :index index :cycling cycling)))
      (case (find-copilot-completion-command key)
        (copilot-accept-suggestion
         (read-key)
         (unshow-inline-completion point)
         (buffer-undo-boundary buffer)
         (replace-with-inline-completion point (elt-safety items index))
         (redraw-display))
        (copilot-next-suggestion
         (read-key)
         (inline-completion point
                            :trigger-kind client:+trigger-kind.invoked+
                            :index (mod (1+ index) (length items))
                            :cycling t
                            :show-loading-spinner t))
        (copilot-previous-suggestion
         (read-key)
         (inline-completion point
                            :trigger-kind client:+trigger-kind.invoked+
                            :index (mod (1- index) (length items))
                            :cycling t
                            :show-loading-spinner t))
        (self-insert
         (unshow-inline-completion point)
         (buffer-undo-boundary buffer)
         (self-insert 1 (insertion-key-p (read-key)))
         (inline-completion point))
        (otherwise
         (unshow-inline-completion point)
         (error 'editor-abort :message nil))))))

(defun inline-completion (point &key (trigger-kind 2) (index 0) cycling show-loading-spinner)
  (setf *completion-canceled* nil)
  (let* ((buffer (point-buffer point))
         (spinner (when show-loading-spinner
                    (lem/loading-spinner:start-loading-spinner :line :point point)))
         (request
           (client:text-document/inline-completion
            (client)
            :callback (lambda (response)
                        (send-event (lambda ()
                                      (when spinner
                                        (lem/loading-spinner:stop-loading-spinner spinner))
                                      (unshow-inline-completion point)
                                      (unless *completion-canceled*
                                        (prompt-inline-completion (buffer-point buffer)
                                                                  (gethash "items" response)
                                                                  :index index
                                                                  :cycling cycling)))))
            :error-callback (lambda (&rest args)
                              (declare (ignore args))
                              (unshow-inline-completion point)
                              (send-event (lambda ()
                                            (when spinner
                                              (lem/loading-spinner:stop-loading-spinner spinner)))))
            :uri (buffer-uri buffer)
            :position (point-to-lsp-position point)
            :insert-spaces (if (variable-value 'indent-tabs-mode
                                               :default buffer)
                               'yason:true
                               'yason:false)
            :tab-size (variable-value 'tab-width :default buffer)
            :trigger-kind trigger-kind)))
    (setf *inline-completion-request* request)))

(defun cancel-inline-completion ()
  (unshow-inline-completion (current-point))
  (when *inline-completion-request*
    (client:$/cancel-request (client) (jsonrpc:request-id *inline-completion-request*))
    (setf *inline-completion-request* nil
          *completion-canceled* t)))

(define-command copilot-complete () ()
  (inline-completion (current-point)))

(define-command copilot-accept-suggestion () ()
  ;; dummy command
  )

(define-command copilot-next-suggestion () ()
  ;; dummy command
  )

(define-command copilot-previous-suggestion () ()
  ;; dummy command
  )

(defparameter *delay-complete* 1)
(defvar *complete-timer* nil)

(defmethod execute :after ((mode copilot-mode) (command self-insert) argument)
  (cond (*delay-complete*
         (if *complete-timer*
             (stop-timer *complete-timer*)
             (setf *complete-timer* (make-idle-timer 'copilot-complete :name "Copilot Complete")))
         (start-timer *complete-timer* *delay-complete* :repeat nil))
        (t
         (copilot-complete))))
