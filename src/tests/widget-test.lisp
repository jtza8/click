; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun test-widgets-manually ()
  (with-display-system (:clear-colour '(0.5 0.5 0.5 1.0)
                        :width 1024
                        :height 768)
    (init-default-theme)
    (let ((window-container (make-instance 'window-container))
          (window-1 
           (make-instance 'window :x 100 :y 100 :width 150 :height 200))
          (window-2
           (make-instance 'window :x 300 :y 120 :width 300 :height 200))
          (window-3
           (make-instance 'window :x 200 :y 220 :width 250 :height 150)))
      (add-widget window-container window-1)
      (add-widget window-container window-2)
      (add-widget window-container window-3)
      (add-to-root window-container))
    (with-event-loop
        (event
         quit
         (case (event-type event)
           (:key-down
            (with-event-keys (key) event
              (when (eq key :escape)
                (setf quit t))))))
      (update-display-system))))