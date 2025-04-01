(defpackage :lem-vi-mode/ex
  (:use :cl
        :lem
        :lem-vi-mode/core
        :lem-vi-mode/ex-parser)
  (:import-from :lem-vi-mode/visual
                :visual-p
                :vi-visual-end)
  (:import-from :lem-vi-mode/registers
                :*last-ex-command*)
  (:import-from :lem-vi-mode/utils
                :expand-filename-modifiers)
  (:export :vi-ex
           :*ex-keymap*))
(in-package :lem-vi-mode/ex)

(defvar *ex-keymap* (make-keymap :name '*ex-keymap*))

(define-state ex () ()
  (:default-initargs
   :keymaps (list *ex-keymap*)))

(define-command vi-ex () ()
  (let* ((directory (uiop:getcwd))
         (buffer-filename (lem:buffer-filename))
         (in-visual (visual-p)))
    (with-temporary-state 'ex
      (with-main-window (lem:current-window)
        (execute-ex
         (prompt-for-string
          ":"
          :initial-value (if in-visual "'<,'>" "")
          :completion-function
          (lambda (str)
            (cond
              ((ppcre:scan "^(?:(?:e|vs|sp)[ \\.]|cd )" str)
               (let* ((comp-str (ppcre:regex-replace "^(e|vs|sp|cd)\\s*" str ""))
                      (expanded-comp-str (expand-filename-modifiers comp-str (or buffer-filename directory)))
                      (prefix-len (- (length str) (length comp-str))))
                 (cond
                   ((string= comp-str ".")
                    (list (format nil "~A/" str)))
                   ((equal comp-str expanded-comp-str)
                    ;; Almost same as prompt-file-complete in lem-core/completion-file.lisp
                    ;; except item's :start offsets which will be used when selecting a completion item.
                    (mapcar (lambda (filename)
                              (let ((label (enough-namestring filename directory)))
                                (with-point ((s (lem/prompt-window::current-prompt-start-point))
                                             (e (lem/prompt-window::current-prompt-start-point)))
                                  (lem/completion-mode:make-completion-item
                                   :label label
                                   :start (character-offset s prefix-len)
                                   :end (line-end e)))))
                            (lem/completion-mode::completion-file
                             expanded-comp-str
                             directory
                             :directory-only (and (ppcre:scan "^cd " str) t))))
                   (t
                    (list (with-point ((s (lem/prompt-window::current-prompt-start-point))
                                       (e (lem/prompt-window::current-prompt-start-point)))
                            (lem/completion-mode:make-completion-item
                             :label expanded-comp-str
                             :start (character-offset s prefix-len)
                             :end (line-end e))))))))
              ((ppcre:scan "^(?:b(?:uffer)?|bd(?:elete)?) " str)
               (let ((comp-str (ppcre:regex-replace "^(?:b(?:uffer)?|bd(?:elete)?)\\s+" str "")))
                 (mapcar (lambda (buffer)
                           (let ((prefix-len (- (length str) (length comp-str))))
                             (with-point ((s (lem/prompt-window::current-prompt-start-point))
                                          (e (lem/prompt-window::current-prompt-start-point)))
                               (lem/completion-mode:make-completion-item
                                :label (buffer-name buffer)
                                :detail (let ((filename (buffer-filename buffer)))
                                          (when filename
                                            (enough-namestring filename (probe-file "./"))))
                                :start (character-offset s prefix-len)
                                :end (line-end e)))))
                         (lem/completion-mode::completion-buffer
                          comp-str))))))
          :history-symbol 'vi-ex
          :special-keymap *ex-keymap*))))
    (vi-visual-end)))

(defun execute-ex (string)
  (let ((lem-vi-mode/ex-core:*point* (current-point)))
    (prog1 (eval (parse-ex string))
      (setf *last-ex-command* string))))
