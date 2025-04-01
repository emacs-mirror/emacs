(defpackage :lem-lisp-mode/test-runner
  (:use :cl
        :lem
        :lem-lisp-mode/internal))
(in-package :lem-lisp-mode/test-runner)

(define-attribute success-test-attribute
  (t :foreground "green"))

(define-attribute failure-test-attribute
  (t :foreground "red"))

(define-key *lisp-mode-keymap* "C-c C-r" 'lisp-test-runner-run-current)
(define-key *lisp-mode-keymap* "C-c R" 'lisp-test-runner-run-buffer)

(defstruct definition
  package-name
  name
  point)

(defun deftest-reference-p (reference)
  ;;TODO: Make a regex for the test posiblities
  (and (typep reference 'lem/detective:misc-reference)
       (string-equal (lem/detective:misc-custom-type reference)
                     "deftest")))

(defun get-package-from-current-reference (buffer)
  (buffer-package buffer))

(defun make-definition-from-reference (reference buffer)
  (make-definition :package-name (get-package-from-current-reference buffer)
                   :name (lem/detective:reference-name reference)
                   :point (lem/detective:reference-point reference)))

(defun get-test-definition-at (point)
  (lem/detective::check-change :force t :buffer (point-buffer point))
  (let ((reference (lem/detective::current-reference :point point)))
    (when (deftest-reference-p reference)
      (make-definition-from-reference reference (point-buffer point)))))

(defun get-buffer-test-definitions (buffer)
  (lem/detective::check-change :force t :buffer buffer)
  (loop :for reference :in (lem/detective:references-misc (lem/detective:buffer-references buffer))
        :when (deftest-reference-p reference)
        :collect (make-definition-from-reference reference buffer)))

(defun result-text (successp)
  (if successp "Success" "Failure"))

(defun display-test-result (definition &key successp (text (result-text successp)))
  (with-point ((start (definition-point definition))
               (end (definition-point definition)))
    (line-start start)
    (line-end end)
    (lem-lisp-mode/eval:redisplay-evaluated-message
     start
     end
     text
     :attribute (if successp
                    'success-test-attribute
                    'failure-test-attribute))))

(defun start-running-spinner (definition)
  (with-point ((point (definition-point definition)))
    (lem/loading-spinner:start-loading-spinner :line
                                               :point point
                                               :loading-message "Testing")))

(defun run-test (definitions)
  (destructuring-bind (definition &rest rest-definitions)
      (uiop:ensure-list definitions)
    (let ((spinner (start-running-spinner definition)))
      (with-remote-eval (`(micros/test-runner:run-test
                           ,(definition-name definition)
                           ,(definition-package-name definition)))
        (lambda (value)
          (alexandria:destructuring-ecase value
            ((:ok successp)
             (display-test-result definition :successp successp))
            ((:abort value)
             (show-message value)
             (display-test-result definition :successp nil :text value)))
          (lem/loading-spinner:stop-loading-spinner spinner)
          (when rest-definitions
            (run-test rest-definitions)))))))

(define-command lisp-test-runner-run-current () ()
  (let ((test-definition (get-test-definition-at (current-point))))
    (unless test-definition
      (editor-error "Current reference is not a test."))
    (run-test test-definition)))

(define-command lisp-test-runner-run-buffer () ()
  (let ((test-definitions (get-buffer-test-definitions (current-buffer))))
    (unless test-definitions
      (editor-error "No test found in this buffer."))
    (run-test test-definitions)))

;; TODO:
;; - interrupt tests
