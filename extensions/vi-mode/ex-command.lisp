(defpackage :lem-vi-mode/ex-command
  (:use :cl
        :lem-vi-mode/ex-core)
  (:import-from :lem-vi-mode/jumplist
                :with-jumplist
                :window-jumplist
                :current-jumplist
                :copy-jumplist)
  (:import-from :lem-vi-mode/options
                :execute-set-command)
  (:import-from :lem-vi-mode/utils
                :change-directory*
                :expand-filename-modifiers)
  (:export :*edit-buffer-directory*))
(in-package :lem-vi-mode/ex-command)

(defvar *edit-buffer-directory* nil)

(defun ex-edit (filename force)
  (if (string= filename "")
      (lem:revert-buffer force)
      (with-jumplist
        (lem:find-file (merge-pathnames (expand-filename-modifiers filename)
                                        (if *edit-buffer-directory*
                                            (lem:buffer-directory)
                                            (uiop:getcwd)))))))

(defun ex-write (range filename touch)
  (case (length range)
    (0 (if (string= filename "")
           (lem:save-current-buffer touch)
           (lem:write-file filename)))
    (2 (lem:write-region-file (first range) (second range)
                              (if (string= filename "")
                                  (lem:buffer-filename (lem:current-buffer))
                                  filename)))
    (otherwise (syntax-error))))

(defun ex-write-all (force)
  (lem:save-some-buffers force))

(defun ex-write-quit (range filename force touch)
  (ex-write range filename touch)
  (lem-vi-mode/commands:vi-quit force))

(define-ex-command "^e(?:dit)?$" (range filename)
  (declare (ignore range))
  (ex-edit filename nil))

(define-ex-command "^e(?:dit)?!$" (range filename)
  (declare (ignore range))
  (ex-edit filename t))

(define-ex-command "^(w|write)$" (range filename)
  (ex-write range filename t))

(define-ex-command "^wa(?:ll)?$" (range argument)
  (declare (ignore range argument))
  (ex-write-all nil))

(define-ex-command "^wa(?:ll)?!$" (range argument)
  (declare (ignore range argument))
  (ex-write-all t))

(define-ex-command "^new$" (range filename)
  (declare (ignore range))
  (lem-vi-mode/commands:vi-window-split-vertically-new 1 filename))

(define-ex-command "^vne(?:w)?$" (range filename)
  (declare (ignore range))
  (lem-vi-mode/commands:vi-window-split-horizontally-new 1 filename))

(define-ex-command "^ene(?:w)?$" (range filename)
  (declare (ignore range))
    (lem-vi-mode/commands:vi-switch-to-buffer filename))

(define-ex-command "^update$" (range filename)
  (when (lem:buffer-modified-p (lem:current-buffer))
    (ex-write range filename t)))

(define-ex-command "^bn$" (range argument)
  (declare (ignore range argument))
  (lem:next-buffer))

(define-ex-command "^bp$" (range argument)
  (declare (ignore range argument))
  (lem:previous-buffer))

(define-ex-command "^wq$" (range filename)
  (ex-write-quit range filename nil t))

(define-ex-command "^wq!$" (range filename)
  (ex-write-quit range filename t t))

(define-ex-command "^q$" (range argument)
  (declare (ignore range argument))
  (lem-vi-mode/commands:vi-quit t))

(define-ex-command "^qa$" (range argument)
  (declare (ignore range argument))
  (lem:exit-lem t))

(define-ex-command "^q!$" (range argument)
  (declare (ignore range argument))
  (lem-vi-mode/commands:vi-quit nil))

(define-ex-command "^qa!$" (range argument)
  (declare (ignore range argument))
  (lem:exit-lem nil))

(define-ex-command "^(?:wqa|xa)(?:ll)?$" (range argument)
  (declare (ignore range argument))
  (ex-write-all nil)
  (lem:exit-lem t))

(define-ex-command "^(?:wqa|xa)(?:ll)?!$" (range argument)
  (declare (ignore range argument))
  (ex-write-all t)
  (lem:exit-lem nil))

(define-ex-command "^clo(?:se)?$" (range filename)
  (declare (ignore range filename))
  (lem-vi-mode/commands:vi-close 1))

(define-ex-command "^(x|xit)$" (range filename)
  (ex-write-quit range filename nil nil))

(define-ex-command "^(x|xit)!$" (range filename)
  (ex-write-quit range filename t nil))

(define-ex-command "^on(?:ly)?$" (range filename)
  (declare (ignore range filename))
  (lem:delete-other-windows))

(defun copy-current-jumplist-to-next-window ()
  (let* ((window-list
           (lem:compute-window-list (lem:current-window)))
         (new-window (lem:get-next-window (lem:current-window) window-list)))
    (setf (window-jumplist new-window)
          (copy-jumplist (current-jumplist)))))

(define-ex-command "^(sp|split)$" (range filename)
  (declare (ignore range))
  (lem:split-active-window-vertically)
  (copy-current-jumplist-to-next-window)
  (unless (string= filename "")
    (lem:find-file (merge-pathnames (expand-filename-modifiers filename) (uiop:getcwd)))))

(define-ex-command "^(vs|vsplit)$" (range filename)
  (declare (ignore range))
  (lem:split-active-window-horizontally)
  (copy-current-jumplist-to-next-window)
  (lem:next-window)
  (unless (string= filename "")
    (lem:find-file (merge-pathnames (expand-filename-modifiers filename) (uiop:getcwd)))))

(define-ex-command "^(s|substitute)$" (range argument)
  (with-jumplist
    (let (start end)
      (case (length range)
        ((0)
         (setf start (lem:line-start (lem:copy-point *point* :temporary))
               end (lem:line-end (lem:copy-point *point* :temporary))))
        ((2)
         (setf start (first range)
               end (second range))))
      (destructuring-bind (before after flags)
          (lem-vi-mode/ex-parser:parse-subst-argument argument)
        (if (not (lem:with-point ((s start)
                                  (e end))
                   (lem:search-forward-regexp s before e)))
            (lem:message "Pattern not found")
            (lem:with-point ((last-match (lem:with-point ((s start)
                                                          (e end))
                                           (lem:search-backward-regexp e before s))))
              (let ((query (find "c" flags :test 'string=))
                    (replace-all-in-line (find "g" flags :test 'string=)))
                (flet ((rep (start end count)
                         (lem:with-point ((s start)
                                          (e end))
                           (lem/isearch::query-replace-internal before
                                                                after
                                                                #'lem:search-forward-regexp
                                                                #'lem:search-backward-regexp
                                                                :query query
                                                                :start s
                                                                :end e
                                                                :count count))))
                  (cond
                    (replace-all-in-line
                     (lem:line-start start)
                     (unless (lem:start-line-p end)
                       (lem:line-end end))
                     (rep start end nil))
                    (t
                     (lem:move-point (lem:current-point) start)
                     (loop until (lem:point<= end (lem:current-point))
                           do (lem:with-point ((replace-start (lem:current-point))
                                               (replace-end (lem:current-point)))
                                (lem:line-start replace-start)
                                (lem:line-end replace-end)
                                (rep replace-start replace-end 1))
                              (lem:next-logical-line 1)
                              (lem:line-start (lem:current-point)))))))
              (let ((p (lem:current-point)))
                (lem:move-point p last-match)
                (lem:line-start p))))))))

(define-ex-command "^!" (range command)
  (declare (ignore range))
  (lem:pipe-command
   (format nil "~A ~A"
           (subseq lem-vi-mode/ex-core:*command* 1)
           command)))

(define-ex-command "^(buffers|ls|files)$" (range argument)
  (declare (ignore range argument))
  (lem/list-buffers:list-buffers))

(define-ex-command "^(b|buffer)$" (range buffer-name)
  (declare (ignore range))
  (with-jumplist
    (lem:select-buffer buffer-name)))

(define-ex-command "^bd(?:elete)?$" (range buffer-name)
  (declare (ignore range))
  (lem:kill-buffer (if (string= buffer-name "")
                       (lem:current-buffer)
                       buffer-name)))

(define-ex-command "^set?$" (range option-string)
  (declare (ignore range))
  (flet ((encode-value (value)
           (typecase value
             (cons (format nil "~{~A~^,~}" value))
             (otherwise value))))
    (multiple-value-bind (option-value option-name old-value isset)
        (execute-set-command option-string)
      (let ((*print-case* :downcase))
        (if (and isset
                 (not (equal option-value old-value)))
            (lem:show-message (format nil "~A: ~S => ~S"
                                      option-name
                                      (encode-value old-value)
                                      (encode-value option-value))
                              :timeout 10)
            (lem:show-message (format nil "~A: ~S" option-name (encode-value option-value))
                              :timeout 10))))))

(define-ex-command "^r(?:e|ead)?$" (range filename)
  ;; TODO: range currently does not distinguish between :0 and :1.
  ;; This makes it impossible to insert at the beginning of the file
  (cond
    ((string= "" filename)
     (lem:editor-error "No file name"))
    ((char= #\! (char filename 0))
     (lem:editor-error "Command execution not supported"))
    (t
     (let ((insert-point (case (length range)
                           (0 (lem:current-point))
                           (1 (first range))
                           (2 (lem:point-max (first range) (second range))))))
       (lem:move-point (lem:current-point) insert-point)
       (lem:line-start (lem:current-point))
       (unless (lem:line-offset (lem:current-point) 1)
         (lem:line-end (lem:current-point))
         (lem:insert-character (lem:current-point) #\newline))
       (lem:insert-file-contents (lem:current-point) filename)))))

(define-ex-command "^cd$" (range new-directory)
  (declare (ignore range))
  (let ((new-directory (change-directory* (expand-filename-modifiers new-directory))))
    (lem:message "~A" new-directory)))

(define-ex-command "^noh(?:lsearch)?$" (range argument)
  (declare (ignore range argument))
  (lem/isearch:isearch-end))

(define-ex-command "^pwd?$" (range argument)
  (declare (ignore range argument))
  (lem:current-directory))

(define-ex-command "^jumps?$" (range argument)
  (declare (ignore range argument))
  (lem-vi-mode/commands:vi-jumps))
