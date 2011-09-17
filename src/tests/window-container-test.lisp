; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass window-container-test (test-case)
  ())

(def-test-method test-focus-widget ((test window-container-test))
  (with-display-system ()
    (init-default-theme)
    (let ((container (make-instance 'window-container))
          (window-1 (make-instance 'window))
          (window-2 (make-instance 'window))
          (window-3 (make-instance 'window)))
      (assert-false (focus-window container))
      (add-widget container window-1)
      (add-widget container window-2)
      (add-widget container window-3)
      (assert-eql window-3 (focus-window container) "A"))))