(defsystem "lem-sdl2"
  :depends-on ("sdl2"
               "sdl2-ttf"
               "sdl2-image"
               "lem"
               "lem/extensions"
               "trivial-main-thread")
  :serial t
  :components ((:file "wm")
               (:file "resource")
               (:file "platform")
               (:file "keyboard")
               (:file "font")
               (:file "icon")
               (:file "text-surface-cache")
               (:file "log")
               (:file "sdl2")
               (:file "icon-font")
               (:file "mouse")
               (:file "utils")
               (:file "display")
               (:file "view")
               (:file "main")
               (:file "drawing")
               (:file "graphics")
               (:file "image-buffer")
               (:file "tree")
               (:file "color-picker")))

(defsystem "lem-sdl2/executable"
  :build-operation program-op
  :build-pathname "../../lem"
  :entry-point "lem:main"
  :depends-on ("lem-sdl2"))
