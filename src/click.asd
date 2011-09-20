(defsystem "click"
  :depends-on ("interact" "meta-package" "split-sequence")
  :author "Jens Thiede"
  :license "BSD-style"
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "conditions")
               (:file "window-container")
               (:file "widget-container")
               (:file "gui-widget")
               (:file "window")
               (:file "label")
               (:file "button")
               (:file "text-box")
               (:file "click")
               (:file "export")))