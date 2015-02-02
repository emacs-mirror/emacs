;;test like:
;; cd /path/to/xwidgets-emacs-dir
;; make   all&&  src/emacs -q --eval "(progn (load \"`pwd`/lisp/xwidget-test.el\") (xwidget-demo-basic))"


;; you should see:
;; - a gtk button
;; - a gtk toggle button
;; - a gtk slider button
;; - an xembed window(using gtk_socket) showing another emacs instance
;; - an xembed window(using gtk_socket) showing an uzbl web browser if its installed

;;the widgets will move when you type in the buffer. good!

;;there will be redrawing issues when widgets change rows, etc. bad!

;;its currently difficult to give kbd focus to the xembedded emacs,
;;but try evaling the following:

;; (xwidget-set-keyboard-grab 3 1)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; demo/test functions
(require 'xwidget)

(defmacro xwidget-demo (name &rest body)
  `(defun ,(intern (concat "xwidget-demo-" name)) ()
     (interactive)
     (switch-to-buffer ,(format "*xwidget-demo-%s*" name))
     (text-mode);;otherwise no local keymap
     (insert "Some random text for xwidgets to be inserted in for demo purposes.\n")
     ,@body))

(xwidget-demo "a-button"
              (xwidget-insert (point-min) 'Button  "button" 60  50)
              (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic))

(xwidget-demo "a-button-bidi"
              (xwidget-insert (+ 5 (point-min)) 'Button  "button" 60  50)
              (set (make-local-variable 'bidi-paragraph-direction) 'right-to-left)
              (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic))


(xwidget-demo "a-toggle-button"
              (xwidget-insert (point-min) 'ToggleButton  "toggle" 60  50)
              (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic))

(xwidget-demo "a-big-button"
              (xwidget-insert (point-min)  'Button "button" 400  500)
              (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic))

(xwidget-demo "a-socket"
              (xwidget-insert (point-min)  'socket "socket" 500  500)
              (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic))

(xwidget-demo "a-socket-osr-broken"
              (xwidget-insert (point-min)  'socket-osr "socket-osr" 500  500)
              (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic))


(xwidget-demo "a-slider"
              (xwidget-insert (point-min)  'slider "slider" 500  100)
              (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic))

(xwidget-demo "a-canvas"
              (xwidget-insert (point-min)  'cairo "canvas" 1000  1000)
              (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic))

(xwidget-demo "a-webkit-broken"
              (xwidget-insert (point-min)  'webkit "webkit" 1000  1000)
              (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic))

(xwidget-demo "a-webkit-osr"
              (xwidget-insert (point-min)  'webkit-osr "webkit-osr" 1000  1000)
              (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic)
              (xwidget-webkit-goto-uri (xwidget-at 1) "http://www.fsf.org"))

(xwidget-demo "a-xwgir"
              (xwidget-insert (point-min)  'xwgir "xwgir" 1000  1000)
              (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic))

(xwidget-demo "a-xwgir-color-button"
              (xwgir-require-namespace "Gtk" "3.0")
              (put 'ColorButton :xwgir-class '("Gtk" "ColorSelection"))
              (xwidget-insert (point-min)  'ColorButton "xwgir-color-button" 1000  1000)
              (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic))

(xwidget-demo "a-xwgir-button"
              (xwgir-require-namespace "Gtk" "3.0")
              (put 'xwgirButton :xwgir-class '("Gtk" "Button"))

              (xwidget-insert (point-min)  'xwgirButton "xwgir label didnt work..." 700  700)
              (xwgir-xwidget-call-method (xwidget-at 1) "set_label" '( "xwgir label worked!"))
              (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic))

(xwidget-demo "a-xwgir-check-button"
              (xwgir-require-namespace "Gtk" "3.0")
              (put 'xwgirCheckButton :xwgir-class '("Gtk" "CheckButton"))

              (xwidget-insert (point-min)  'xwgirCheckButton "xwgir label didnt work..." 700  700)
              (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic))

(xwidget-demo "a-xwgir-hscale"
              (xwgir-require-namespace "Gtk" "3.0")
              (put 'xwgirHScale :xwgir-class '("Gtk" "HScale"))

              (xwidget-insert (point-min)  'xwgirHScale "xwgir label didnt work..." 700  700)
              (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic))

(xwidget-demo "a-xwgir-webkit"
              (xwgir-require-namespace "WebKit" "3.0")
              (put 'xwgirWebkit :xwgir-class '("WebKit" "WebView"))

              (xwidget-insert (point-min)  'xwgirWebkit "xwgir webkit..." 700  700)
              (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic))



;; tentative testcase:
;; (xwgir-require-namespace "WebKit" "3.0")

;; (put 'webkit-osr :xwgir-class '("WebKit" "WebView"))
;; (xwgir-call-method (xwidget-at 1) "set_zoom_level" '(3.0))

;; (xwgir-require-namespace "Gtk" "3.0")
;; (put 'color-selection :xwgir-class '("Gtk" "ColorSelection"))


(xwidget-demo "basic"
  (xwidget-insert (point-min)  'button "button" 40  50 )
  (xwidget-insert          15  'toggle "toggle" 60  30  )
  (xwidget-insert          30  'socket "emacs"  400 200 )
  (xwidget-insert          20  'slider "slider" 100 50  )
  (xwidget-insert          40  'socket "uzbl-core"   400 400 )
  (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-basic)
)


;it doesnt seem gtk_socket_steal works very well. its deprecated.
; xwininfo -int
; then (xwidget-embed-steal 3 <winid>)
(defun xwidget-demo-grab ()
  (interactive)
  (insert "0 <<< grabbed appp will appear here\n")
  (xwidget-insert          1 1 3 "1" 1000 )
  (define-key (current-local-map) [xwidget-event] 'xwidget-handler-demo-grab)
  )

;ive basically found these xembeddable things:
;openvrml
;emacs
;mplayer
;surf
;uzbl

;try the openvrml:
;/usr/libexec/openvrml-xembed 0 ~/Desktop/HelloWorld.wrl

(defun xwidget-handler-demo-basic ()
  (interactive)
  (message "stuff happened to xwidget %S" last-input-event)
  (let*
      ((xwidget-event-type (nth 1 last-input-event))
       (xwidget (nth 2 last-input-event)))
    (cond ( (eq xwidget-event-type 'xembed-ready)
            (let*
                ((xembed-id (nth 3 last-input-event)))
              (message "xembed ready  event: %S xw-id:%s" xembed-id xwidget)
              ;;will start emacs/uzbl in a xembed socket when its ready
              (cond
               (t;;(eq 3 xwidget)
                (start-process "xembed" "*xembed*" "/var/lib/jenkins/jobs/emacs-xwidgets-automerge/workspace/src/emacs" "-q" "--parent-id" (number-to-string xembed-id) ) )
;;               ((eq 5 xwidget-id)
;;                (start-process "xembed2" "*xembed2*" "uzbl-core"  "-s" (number-to-string xembed-id)  "http://www.fsf.org" )
               )

               )
            ))))



(defun xwidget-handler-demo-grab ()
  (interactive)
  (message "stuff happened to xwidget %S" last-input-event)
  (let*
      ((xwidget-event-type (nth 2 last-input-event)))
    (cond ( (eq xwidget-event-type 'xembed-ready)
            (let*
                ((xembed-id (nth 3 last-input-event)))
              (message "xembed ready  %S" xembed-id)
              )
            ))))
(defun xwidget-dummy-hook ()
  (message "xwidget dummy hook called"))

;  (xwidget-resize-hack 1 200 200)

;(xwidget-demo-basic)

(provide 'xwidget-test-manual)
