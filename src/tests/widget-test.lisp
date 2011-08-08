; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun test-widgets-manually ()
  (with-display-system (screen-colour '(1 1 1 0))
    (load-sprite-path (asdf:system-relative-pathname :click "../gui/")
                      :parent-node-path '(:gui))
    (let ((window (make-instance 'window :x 100 :y 100 :width 100 :height 200)))
      (add-to-root window :window))))