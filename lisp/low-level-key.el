;;; -*- lexical-binding: t -*-
(require 'cl-lib)

;; User options
(defvar llk-tap-timeout 1000)
(defvar llk-tap-count 2)
(defvar llk-tap-keys
  '(lshift rshift lctrl rctrl lalt ralt shift ctrl alt))
(defvar llk-bindings nil)
(defvar llm-bindings nil)

(defun llk-init ()
  (interactive)
  (define-key special-event-map [low-level-key] 'llk-handle)
  (define-key special-event-map [low-level-modifier] 'llm-handle)

  (setq llk-bindings nil)
  (setq llm-bindings nil)

  ;; (llm-bind 'tap 'shift 'delete-other-windows)
  ;; (llk-bind 'tap 'lctrl 'hyper)
  (setq enable-low-level-key-events t))

;; For example:
;; (llk-add-binding 'tap 'lshift 'delete-other-windows)
;; Can bind to a command, a function or the symbol 'hyper.
(defun llk-bind (action key function)
  (push (list action key function) llk-bindings))

(defun llm-bind (action key function)
  (push (list action key function) llm-bindings))

;; We store the last events here to test for multitap.
(defvar llk-events nil)
(defvar llm-events nil)

;; If positive, return key ('lshift, etc) else return nil.
(defun llk-detect-n-tap (n timeout)
  ;; The physical-key event is like this:
  ;; (physical-key t lshift 90196265 #<frame>)
  ;; The second element is t for a key press, nil for a key release
  ;; The fourth element is the time in milliseconds
  ;; The fifth is the frame, we don't use it yet.

  (let ((key (cl-third last-input-event)))
    (if (not (member key llk-tap-keys))
        ;; Key not in tap list, clear history
        (setq llk-events nil)
      ;; Clear it also if the first element is from a different key
      (and llk-events
           (not (equal (cl-third (car llk-events)) key))
           (setq llk-events nil))
      (push last-input-event llk-events)
      ;; Only care about last 2xN events
      (ntake (* 2 n) llk-events)
      ;; If we have:
      ;; - Exactly 2 * n events.
      ;; - down, up, down, up, ...
      ;; - not two much time between first and last
      (and (eq (* 2 n) (length llk-events))
           (cl-every 'eq
                     (ntake (* 2 n)
                            (list nil t nil t nil t nil t
                                  nil t nil t nil t nil t))
                     (mapcar 'cl-second llk-events))
           (< (- (cl-fourth (cl-first llk-events))
                 (cl-fourth (car (last llk-events))))
              timeout)
           (progn
             (setq llk-events nil)
             key)))))


;; this function is a copy of llk-detect-n-tap, but for llm-events
(defun llm-detect-n-tap (n timeout)
  (let ((key (cl-third last-input-event)))
    (if (not (member key llk-tap-keys))
        (setq llm-events nil)
      (and llm-events
           (not (equal (cl-third (car llm-events)) key))
           (setq llm-events nil))
      (push last-input-event llm-events)
      (ntake (* 2 n) llm-events)
      (and (eq (* 2 n) (length llm-events))
           (cl-every 'eq
                     (ntake (* 2 n)
                            (list nil t nil t nil t nil t
                                  nil t nil t nil t nil t))
                     (mapcar 'cl-second llm-events))
           (< (- (cl-fourth (cl-first llm-events))
                 (cl-fourth (car (last llm-events))))
              timeout)
           (progn
             (setq llm-events nil)
             key)))))

(defun llk-handle ()
  (interactive)

  (let ((tap-key (llk-detect-n-tap
                  llk-tap-count
                  llk-tap-timeout)))
    (when tap-key
      (let ((func (cl-third
                   (seq-find
                    (lambda (b)
                      (and (eq (cl-first b) 'tap)
                           (eq (cl-second b) tap-key)))
                    llk-bindings))))
        (cond
         ((commandp func) (call-interactively func))
         ((functionp func) (funcall func))
         ((eq 'hyper func)
          (message "H-...")
          (let ((r (read-event)))
            (setq unread-command-events
                  (list (event-apply-modifier
                         r 'hyper 24 "H-"))))))))))

(defun llm-handle()
  (interactive)

  (let ((tap-key (llm-detect-n-tap
                  llk-tap-count
                  llk-tap-timeout)))
    (when tap-key
      (let ((func (cl-third
                   (seq-find
                    (lambda (b)
                      (and (eq (cl-first b) 'tap)
                           (eq (cl-second b) tap-key)))
                    llm-bindings))))
        (cond
         ((commandp func) (call-interactively func))
         ((functionp func) (funcall func))
         ((eq 'hyper func)
          (message "H-...")
          (let ((r (read-event)))
            (setq unread-command-events
                  (list (event-apply-modifier
                         r 'hyper 24 "H-"))))))))))
