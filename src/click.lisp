; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun init-default-theme ()
  (load-sprite-path *base-sprite-path* :parent-node-path *base-node-path*)
  (dolist (panel (list (apply #'sprite-node
                              (append *base-node-path*
                                      '(:window :inactive :panel)))
                       (apply #'sprite-node
                              (append *base-node-path*
                                      '(:window :active :panel)))))
    (dolist (node-name '(:centre :left :right :bottom))
      (setf (node-of panel (list node-name))
            (make-colour-sprite :colour '(0.1 0.1 0.1 1.0))))))
