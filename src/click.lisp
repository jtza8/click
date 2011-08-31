; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun init-default-theme ()
  (setf *base-node*
        (load-sprite-path *base-sprite-path*
                          :parent-node-path *base-node-path*))
  (loop for mode in '(:inactive :active)
        for window = (node-of *base-node* `(,mode :window))
        for button = (node-of *base-node* `(,mode :button))
        do (progn
             (setf (node-of window :title-font)
                   (clone (node-of *base-node* :font :verabd)
                            :colour (if (eq mode :active)
                                        '(0.7 0.7 0.7 1.0)
                                        '(0.3 0.3 0.3 1.0))
                            :size 11)
                   (node-of button :font)
                   (clone (node-of *base-node* :font :vera)
                            :colour '(0.7 0.7 0.7 1.0)
                            :size 10))
             (dolist (node-name '(:centre :left :right :bottom))
               (setf (node-of window (list :panel node-name))
                     (make-colour-sprite :colour '(0.1 0.1 0.1 1.0)))))))
