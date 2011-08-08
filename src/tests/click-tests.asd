(defsystem "click-tests"
  :author "Jens Thiede"
  :license "BSD-style"
  :serial t
  :depends-on ("click")
  :components ((:file "package")
               (:file "widget-test")))