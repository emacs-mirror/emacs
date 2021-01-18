(defvar w32-msys-site-start-install-dir
  (expand-file-name (concat data-directory "../../../../")))

(defvar w32-msys-top-level-dir (concat w32-msys-site-start-install-dir "msys64"))

(when (file-exists-p w32-msys-top-level-dir)
  (setenv "PATH" (concat (getenv "PATH")
                         (replace-regexp-in-string "/" "\\\\"
                                                   w32-msys-top-level-dir)
                         "\\usr\\bin;"))
  (add-to-list 'exec-path (concat w32-msys-top-level-dir "/usr/bin")))
