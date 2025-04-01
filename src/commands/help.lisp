(defpackage :lem-core/commands/help
  (:use :cl :lem-core)
  (:export :describe-key
           :describe-bindings
           :apropos-command
           :lem-version
           :list-modes)
  #+sbcl
  (:lock t))
(in-package :lem-core/commands/help)

(define-key *global-keymap* "C-x ?" 'describe-key)

(define-command describe-key () ()
  "Tell what is the command associated to a keybinding."
  (show-message "describe-key: ")
  (redraw-display)
  (let* ((kseq (read-key-sequence))
         (cmd (find-keybind kseq)))
    (show-message (format nil "describe-key: ~a ~(~a~)"
                          (keyseq-to-string kseq)
                          cmd))))

(defun describe-bindings-internal (s name keymap &optional first-p)
  (unless first-p
    (princ #\page s)
    (terpri s))
  (let ((column-width 16))
    (loop :while keymap
          :do (format s "~A (~(~A~))~%" name (keymap-name keymap))
              (format s "~va~a~%" column-width "key" "binding")
              (format s "~va~a~%" column-width "---" "-------")
              (traverse-keymap keymap
                               (lambda (kseq command)
                                 (unless (equal "UNDEFINED-KEY" (symbol-name command))
                                   (format s "~va~(~a~)~%"
                                           column-width
                                           (keyseq-to-string kseq)
                                           (symbol-name command)))))
              (setf keymap (keymap-parent keymap))
              (terpri s))))

(define-command describe-bindings () ()
  "Describe the bindings of the buffer's current major mode."
  (let ((buffer (current-buffer))
        (firstp t))
    (with-pop-up-typeout-window (s (make-buffer "*bindings*") :erase t)
      (describe-bindings-internal s
                                  (mode-name (buffer-major-mode buffer))
                                  (setf firstp (mode-keymap (buffer-major-mode buffer)))
                                  t)
      (setf firstp (not firstp))
      (dolist (mode (buffer-minor-modes buffer))
        (describe-bindings-internal s
                                    (mode-name mode)
                                    (mode-keymap mode)
                                    firstp)
        (when (mode-keymap mode)
          (setf firstp t)))
      (describe-bindings-internal s
                                  "Global"
                                  *global-keymap*
                                  firstp))))

(define-command list-modes () ()
  "Output all available major and minor modes."
  (with-pop-up-typeout-window (s (make-buffer "*all-modes*") :erase t)
    (let ((major-modes (major-modes))
          (minor-modes (minor-modes)))
      (labels ((is-technical-mode (mode)
                 (eql (elt (symbol-name mode) 0)
                      #\%))
               (print-modes (title modes)
                 (format s "~A:~2%" title)
                 ;; Remove technical modes where name starts with %
                 (let* ((modes (remove-if #'is-technical-mode modes))
                        (sorted-modes (sort modes #'string< :key #'string-downcase)))
                   (dolist (mode sorted-modes)
                     (format s "* ~A~@[ – ~A~]~%"
                             (mode-name mode)
                             (mode-description mode))))
                 (format s "~2%")))
        (print-modes "Major modes" major-modes)
        (print-modes "Minor modes" minor-modes)))))

(define-command describe-mode () ()
  "Show information about current major mode and enabled minor modes."
  (let* ((buffer (current-buffer))
         (major-mode (buffer-major-mode buffer))
         (minor-modes (buffer-minor-modes buffer))
         (global-minor-modes (loop :for mode :in (minor-modes)
                                   :when (and (mode-active-p buffer mode)
                                              (not (find mode minor-modes)))
                                   :collect mode)))
    (with-pop-up-typeout-window (s (make-buffer "*modes*") :erase t)
      (format s "Major mode is: ~A~@[ – ~A~]~%"
              (mode-name major-mode)
              (mode-description major-mode))
      (when minor-modes
        (format s "~2&Minor modes enabled in this buffer:~2%")
        (dolist (mode minor-modes)
          (format s "* ~A~@[ – ~A~]~%"
                  (mode-name mode)
                  (mode-description mode))))
      (when global-minor-modes
        (format s "~2&Global minor modes:~2%")
        (dolist (mode global-minor-modes)
          (format s "* ~A~@[ – ~A~]~%"
                  (mode-name mode)
                  (mode-description mode)))))))

(define-command apropos-command () ()
  "Find all symbols in the running Lisp image whose names match a given string."
  (let ((str (prompt-for-string
              "Apropos: "
              :completion-function
              (lambda (str) (completion str (all-command-names))))))
    (with-pop-up-typeout-window (out (make-buffer "*Apropos*") :erase t)
      (dolist (name (all-command-names))
        (when (search str name)
          (describe (command-name (find-command name)) out))))))

(define-command lem-version () ()
  "Display Lem's version."
  (let ((version (get-version-string)))
    (show-message (princ-to-string version))))

(define-command help () ()
  "Show some help."
  (with-pop-up-typeout-window (s (make-buffer "*Help*") :erase t)
    (format s "Welcome to Lem.~&")
    (format s "You are running ~a.~&" (get-version-string))
    (format s "~%")
    (format s "To open a file in Lem, use C-x C-f. Close it with C-x k.~&")
    (format s "Switch buffers with C-x b.~&")
    (format s "Quit Lem with C-x C-c.~&")
    (format s "You can use Vi keys: activate the mode with Alt-x vi-mode.~&")
    (format s "~%")
    (format s "To discover all available keybindings, type Alt-x documentation-describe-bindings.~&")
    (format s "You can check what command a given keybinding is bound to with Alt-x describe-key.~&")
    (format s "~%")
    (format s "Lem works out of the box for Common Lisp development. ~%")
    (format s "You can compile a function right away with C-c C-c,~%")
    (format s "and open a REPL with M-x start-lisp-repl.~%")
    (format s "(the notation M-x means Alt-x.)~%")
    (format s "~%")
    (format s "But Lem supports other programming languages thanks to its built-in LSP client.~%")
    (format s "Please consult our online documentation.~%")))
