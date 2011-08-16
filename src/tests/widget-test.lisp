; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun test-widgets-manually ()
  (with-display-system (:clear-colour '(0.5 0.5 0.5 1.0)
                        :width 1024
                        :height 768)
    (init-default-theme)
    (add-to-root
     (window-container
       :widgets (list (window :x 100 :y 100 :width 150 :height 200)
                      (window :x 300 :y 120 :width 300 :height 200)
                      (window :x 200 :y 220 :width 250 :height 150))))
    (with-event-loop
        (event
         quit
         (case (event-type event)
           (:key-down
            (with-event-keys (key) event
              (when (eq key :escape)
                (setf quit t))))))
      (update-display-system))))