;; demo for forms-mode
;;
;; This demo visits /etc/passwd.

(setq forms-file "/etc/passwd")
(setq forms-read-only t)		; to make sure
(setq forms-field-sep ":")
(setq forms-number-of-fields 7)
(setq forms-format-list
     '("====== Visiting /etc/passwd ======\n\n"
       "User : "	1
       "Password : "	2
       "   Uid: "	3
       "   Gid: "	4
       "\n\n"
       "Name : "	5
       "\n\n"
       "Home : "	6
       "\n\n"
       "Shell: "	7
       "\n"))
