;; Object Configuration
;; EDE Generic Project Configuration
(ede-generic-config "Configuration"
  :file "EDEConfig.el"
  :build-command "cmake"
  :c-preprocessor-table '(("TEST" . "1")))
