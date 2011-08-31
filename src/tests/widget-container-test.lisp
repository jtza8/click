; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass widget-container-test (test-case)
  ())

(def-test-method test-focus ((test widget-container-test))
  (with-display-system ()
    (init-default-theme)
    (let ((one (make-instance 'button :text "One"))
          (two (make-instance 'button :text "Two"))
          (three (make-instance 'button :text "Three"))
          (container (make-instance 'widget-container)))
      (assert-false (offset-widget-focus container 1))
      (assert-false (offset-widget-focus container -1))
      (assert-false (focused-widget container))
      (add-widget container one)
      (assert-eql one (focused-widget container))
      (add-widget container two)
      (assert-eql one (focused-widget container))
      (add-widget container three)
      (assert-eql one (focused-widget container))
      (assert-eql two (focused-widget container 1))
      (assert-eql three (focused-widget container 2))
      (assert-eql one (focused-widget container 3))
      (assert-eql three (focused-widget container -1))
      (assert-eql one (focused-widget container -3))
      (assert-eql three (focused-widget container -4))
      (offset-widget-focus container 1)
      (assert-eql two (focused-widget container))
      (offset-widget-focus container 1)
      (assert-eql three (focused-widget container))
      (offset-widget-focus container -5)
      (assert-eql one (focused-widget container))
      (remove-widget container one)
      (assert-eql two (focused-widget container))
      (assert-condition 'invalid-widget (focus-widget container one)))))