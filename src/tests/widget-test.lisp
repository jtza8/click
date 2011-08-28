; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun test-widgets-manually ()
  (with-display-system (:clear-colour '(0.1 0.1 0.1 1.0)
                        :width 1024
                        :height 768
                        :full-screen nil)
    (init-default-theme)
    (add-to-root
     (make-windows
       (window :x 100 :y 100 :width 150 :height 200)
       (window :x 300 :y 120 :width 300 :height 200
               :widgets ((:hello-button
                          (button :x 10 :y 30 :text "One"))
                         (button :x 10 :y 60 :text "Two")
                         (button :x 10 :y 90 :text "Three")))
       (window :x 200 :y 220 :width 250 :height 150)))
    (with-event-loop #'simple-top-level-event-handler
      (update-display-system))))