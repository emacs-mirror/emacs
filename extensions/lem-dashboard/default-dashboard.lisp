(in-package :lem-dashboard)

(defvar *default-footer-messages* '("Happy Coding!"
                                    "ほげほげ"
                                    "In Lisp we trust, for bugs we adjust"
                                    "Read, Evaluate, Print, Love"
                                    "May your parentheses always be balanced"
                                    "(setf productivity 'high)"
                                    "<M-x> load-library <RET> tetris"
                                    "Lem Editor Modules? Lisp EMacs? Lem's Not Emacs?"
                                    "(cons 'fun 'programming)"))

(defvar *default-splash* '("
 ----------------------- 
 [   Welcome to Lem!   ] 
 ----------------------- 

       #+====-===        
    ##+-----------=      
  ########**==------=    
 ##########  %-------=   
%#########+==-:---=====  
########:::::::::::+  =  
######*=++-::::::::::    
#########=::::           
%#######*:::::           
 #######+:::::::         
  %######=:::::::::      
    #######*-::::        
       %##*-:::::        
"))

(define-command open-lem-docs () ()
  (open-external-file "https://lem-project.github.io/usage/usage/"))

(define-command open-lem-github () ()
  (open-external-file "https://github.com/lem-project/lem"))

(defun set-default-dashboard (&key 
                              (project-count 5)
                              (file-count 5)
                              (splash *default-splash*)
                              (footer-messages *default-footer-messages*)
                              hide-links)
  (let ((dashboard-items 
          (list (make-instance 'dashboard-splash
                               :item-attribute 'document-metadata-attribute
                               :splash-texts splash)
                (make-instance 'dashboard-working-dir)
                (make-instance 'dashboard-recent-projects 
                               :project-count project-count
                               :bottom-margin 1)
                (make-instance 'dashboard-recent-files 
                               :file-count file-count
                               :bottom-margin 1)
                (make-instance 'dashboard-command
                               :display-text " New Lisp Scratch Buffer (l)"
                               :action-command 'lem-lisp-mode:lisp-scratch
                               :item-attribute 'document-header2-attribute
                               :bottom-margin 2))))
    (unless hide-links
      (setf dashboard-items
            (append dashboard-items
                    (list (make-instance 'dashboard-url 
                                         :display-text " Getting Started (s)"
                                         :url "https://lem-project.github.io/usage/usage/"
                                         :item-attribute 'document-header3-attribute)
                          (make-instance 'dashboard-url
                                         :display-text " GitHub (g)"
                                         :url "https://github.com/lem-project/lem"
                                         :item-attribute 'document-header3-attribute
                                         :bottom-margin 2)))))
    (setf dashboard-items
          (append dashboard-items
                  (list (make-instance 'dashboard-footer-message 
                                       :item-attribute 'document-blockquote-attribute
                                       :messages footer-messages))))
    (set-dashboard dashboard-items)))

(define-key *dashboard-mode-keymap* "s" 'open-lem-docs)
(define-key *dashboard-mode-keymap* "g" 'open-lem-github)
(define-key *dashboard-mode-keymap* "r" 'dashboard-move-to-recent-projects)
(define-key *dashboard-mode-keymap* "f" 'dashboard-move-to-recent-files)
(define-key *dashboard-mode-keymap* "l" 'lem-lisp-mode/internal:lisp-scratch)

(set-default-dashboard)
