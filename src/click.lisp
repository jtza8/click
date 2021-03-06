; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defun init-default-theme ()
  (setf *base-node*
        (load-sprite-path *base-sprite-path*
                          :parent-node-path *base-node-path*)
        (node-of *base-node* :label) (make-branch)
        (node-of *base-node* :label :font)
        (clone (node-of *base-node* :font :vera)
               :colour '(0.7 0.7 0.7 1.0)
               :size 10)
        (node-of *base-node* :text-box :font)
        (clone (node-of *base-node* :font :vera)
               :colour '(0.7 0.7 0.7 1.0)
               :size 10)
        (node-of *base-node* :text-box :cursor)
        (make-instance 'colour-sprite
                       :colour '(0.4 0.4 0.4 1.0)
                       :width 1
                       :height 12))
  (loop for mode in '(:inactive :active)
        for window = (node-of *base-node* `(:window ,mode))
        for button = (node-of *base-node* `(:button ,mode))
        for text-box = (node-of *base-node* `(:text-box ,mode))
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
                          :size 10)
                   (node-of button :font)
                   (clone (node-of *base-node* :font :vera)
                          :colour '(0.7 0.7 0.7 1.0)
                          :size 10))
             (dolist (node-name '(:centre :left :right :bottom))
               (setf (node-of window `(:panel ,node-name))
                     (make-instance 'colour-sprite
                                    :colour '(0.1 0.1 0.1 1.0))))
             (dolist (node-name '(:centre :left :right :top :bottom))
               (setf (node-of text-box `(:box ,node-name))
                     (make-instance 'colour-sprite
                                    :colour '(0.2 0.2 0.2 1.0)))))))
