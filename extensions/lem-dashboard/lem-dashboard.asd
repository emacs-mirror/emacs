(defsystem "lem-dashboard"
  :depends-on (:lem)
  :serial t
  :components ((:file "lem-dashboard")
               (:file "dashboard-items")
               (:file "default-dashboard")))
