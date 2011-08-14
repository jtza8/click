(defsystem "click-tests"
  :author "Jens Thiede"
  :license "BSD-style"
  :serial t
  :depends-on ("click" "xlunit")
  :components ((:file "package")
               (:file "window-container-test")
               (:file "widget-test")))