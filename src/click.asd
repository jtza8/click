(defsystem "click"
  :depends-on ("interact" "meta-package")
  :author "Jens Thiede"
  :license "BSD-style"
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "window-container")
               (:file "widget-container")
               (:file "window")
               (:file "button")
               (:file "click")
               (:file "export")))