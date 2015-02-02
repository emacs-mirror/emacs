;; -*- lexical-binding: t; -*-

(require 'cl)
(require 'xwidget)
(require 'xwidget-test)
(require 'parallel)

(defvar xwidget-parallel-config (list :emacs-path (expand-file-name
                                                   "~/packages/xwidget-build/src/emacs")))

(defmacro xwidget-deftest (name types &rest body)
  (declare (indent defun))
  (if (null types)
      `(ert-deftest ,(intern (format "%s" name)) ()
         (let ((parallel-config xwidget-parallel-config))
           ,@body))
    `(progn
       ,@(loop for type in types
               collect
               `(ert-deftest ,(intern (format "%s-%s" name type)) ()
                  (let ((parallel-config xwidget-parallel-config)
                        (type ',type)
                        (title ,(symbol-name type)))
                    ,@body))))))

(xwidget-deftest xwidget-make-xwidget (Button ToggleButton slider socket cairo)
  (let* ((beg 1)
         (end 1)
         (width 100)
         (height 100)
         (data nil)
         (proc (parallel-start
                (lambda (beg end type title width height data)
                  (require 'xwidget)
                  (require 'cl)
                  (with-temp-buffer
                    (insert ?\0)
                    (let* ((buffer (current-buffer))
                           (xwidget (make-xwidget beg end type title width height data buffer)))
                      (set-xwidget-query-on-exit-flag xwidget nil)
                      (parallel-remote-send (coerce (xwidget-info xwidget) 'list))
                      (parallel-remote-send (buffer-name buffer))
                      (buffer-name (xwidget-buffer xwidget)))))
                :env (list beg end type title width height data)))
         (results (parallel-get-results proc)))
    (should (parallel-success-p proc))
    (when (parallel-success-p proc)
      (destructuring-bind (xwidget-buffer temp-buffer xwidget-info)
          results
        (should (equal (list type title width height)
                       xwidget-info))
        (should (equal temp-buffer xwidget-buffer))))))

(xwidget-deftest xwidget-query-on-exit-flag ()
  (should (equal '(nil t)
                 (parallel-get-results
                  (parallel-start (lambda ()
                                    (require 'xwidget)
                                    (let ((xwidget (make-xwidget 1 1 'Button "Button" 100 100 nil)))
                                      (parallel-remote-send (xwidget-query-on-exit-flag xwidget))
                                      (set-xwidget-query-on-exit-flag xwidget nil)
                                      (xwidget-query-on-exit-flag xwidget))))))))

(xwidget-deftest xwidget-query-on-exit-flag (Button ToggleButton slider socket cairo)
  (should (parallel-get-result
           (parallel-start (lambda (type title)
                             (require 'xwidget)
                             (with-temp-buffer
                               (let ((xwidget (make-xwidget 1 1 type title 10 10 nil)))
                                 (set-xwidget-query-on-exit-flag xwidget nil)
                                 (xwidgetp xwidget))))
                           :env (list type title)))))

(xwidget-deftest xwidget-CHECK_XWIDGET ()
  (should (equal (parallel-get-result
                  (parallel-start (lambda ()
                                    (require 'xwidget)
                                    (xwidget-info nil))))
                 '(wrong-type-argument xwidgetp nil)))
  (should (equal (parallel-get-result
                  (parallel-start (lambda ()
                                    (require 'xwidget)
                                    (xwidget-view-info nil))))
                 '(wrong-type-argument xwidget-view-p nil))))

(xwidget-deftest xwidget-view-p (Button ToggleButton slider socket cairo)
  (should (parallel-get-result
           (parallel-start (lambda (type title)
                             (require 'xwidget)
                             (with-temp-buffer
                               (insert ?\0)
                               (let* ((xwidget (xwidget-insert 1 type title 100 100))
                                      (window (xwidget-display xwidget)))
                                 (set-xwidget-query-on-exit-flag xwidget nil)
                                 (xwidget-view-p
                                  (xwidget-view-lookup xwidget window)))))
                           :env (list type title)
                           :graphical t
                           :emacs-args '("-T" "emacs-debug")))))

(defun xwidget-interactive-tests ()
  "Interactively test Button ToggleButton and slider.

Start Emacs instances and try to insert the xwidget."
  (interactive)
  (flet ((test-xwidget (type)
                       (parallel-get-result
                        (parallel-start (lambda ()
                                          (require 'xwidget)
                                          (with-temp-buffer
                                            (insert ?\0)
                                            (set-xwidget-query-on-exit-flag
                                             (xwidget-insert 1 type (format "%s" type) 100 100) nil)
                                            (display-buffer (current-buffer))
                                            (cons type (or (y-or-n-p (format "Do you see a %s?" type)) 'failed))))
                                        :graphical t
                                        :debug t
                                        :config xwidget-parallel-config))))
    (message "%S" (mapcar #'test-xwidget '(Button ToggleButton slider)))))

(provide 'xwidget-tests)
