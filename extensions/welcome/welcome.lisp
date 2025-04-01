(defpackage #:lem-welcome 
  (:use :cl :lem)
  (:export #:*enable-welcome* #:*message-width* #:*message-content*))
(in-package :lem-welcome)

;; opt out of welcome screen if you want to start into an empty tmp buffer
(defvar *enable-welcome* nil)

;; This parameter is to make sure the content is properly centered in the window
(defparameter *message-width* 45)

;; Modify the welcome content (make sure to also change the width if needed)
(defparameter *message-content* 
"
               Welcome to Lem!
                
                ,:coodddddoc.             
           ',;cldddddddddddddolc.         
        .,';oddddddddddddddddddddo:       
      ''.,loollooodddddddddddddddddo:     
    .'.............';:lddddddddddddddo'   
   '.................   ,ddddddddddddddc  
  '..................    .Oxddddddddddddc 
 ....................''''oK0xdddddddddddd,
................,ldOKKKKKKKK0xdddxx:,,''',
..............ckKKKKKKKKKKKKK0kO0KKo.     
............'kKKKKKKKKKKKKKKKKKKKKKKKKo   
...........'xdl:;;:O000:                  
.................'k0000:                  
 ...............'k000000                  
 ...............xKKKKKKKk                 
  .............'KKKKKKKKKO'               
   ............,KKKKKKKKKKKko.     .      
    ............xKKKKKKKKKKKKK0OkO;       
      ...........dKKKKKKKKKKKKK;          
         .........,lkKKKKKKK0.            
           ...........;xKKKKK0            
                ...';ckKKKKKK0            
                    .lOKx'                ")

(defun display-welcome ()
  (when *enable-welcome*
    ;; print the welcome message to the start buffer
    (with-open-stream (stream (make-buffer-output-stream (buffer-start-point (current-buffer))))
      (loop :with prefix := (/ (- (window-width (current-window)) *message-width*) 2)
            :for line :in (str:lines *message-content*)
            :do (format stream "~v@{~a~:*~}" prefix " ")
            :do (format stream "~a~%" line)))
    ;; hack to move cursor back to the top of the window
    (lem-vi-mode/commands:vi-goto-first-line)))

(add-hook *after-init-hook* #'display-welcome)
