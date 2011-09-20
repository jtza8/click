; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass label (gui-widget)
  ((ignore-focus :initform t)
   (text-sprite :initform (clone (node-of *base-node* :label :font)))))

(defmethod text ((label label))
  (text (slot-value label 'text-sprite)))

(defmethod (setf text) (value (label label))
  (setf (text (slot-value label 'text-sprite)) value))

(defmethod initialize-instance :after ((label label) &key (text ""))
  (setf (text label) text))

(defmethod draw ((label label))
  (with-slots (text-sprite) label
    (draw-sprite text-sprite)))