; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun test-widgets-manually ()
  (with-display-system (screen-colour '(1 1 1 0))
    (init-default-theme)
    (let ((window-1 
           (make-instance 'window :x 100 :y 100 :width 150 :height 200))
          (window-2
           (make-instance 'window :x 300 :y 120 :width 200 :height 150)))
      (add-to-root window-1)
      (add-to-root window-2))))