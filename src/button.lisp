; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass button (widget)
  ((focus :initform nil
           :accessor focus)
   (down :initform nil
         :accessor down)
   (height :initform 20)
   (width :initform 50)
   (infocus-button-sprites :allocation :class)
   (infocus-border-sprites :allocation :class)
   (focus-button-sprites :allocation :class)
   (focus-button-down-sprites :allocation :class)
   (focus-border-sprites :allocation :class)
   text-sprite))

(defmethod initialize-instance :after ((button button) &key (text ""))
  (with-slots (text-sprite) button
    (setf (text button) text)))

(defmethod text ((button button))
  (with-slots (text-sprite) button
    (text text-sprite)))

(defmethod (setf text) (value (button button))
  (with-slots (text-sprite infocus-button-sprites width) button
    (with-sprite-snippets (infocus-button-sprites *button-sprite-names*
                           "button-")
      (setf (text text-sprite) value
            width (max width (+ (width text-sprite)
                                (width button-left)
                                (width button-right)))))))

(defmethod init-sprites :after ((button button))
  (with-slots (focus infocus-button-sprites focus-button-sprites
               focus-button-down-sprites infocus-border-sprites
               focus-border-sprites text-sprite) button
    (let ((base-node (sprite-node (append *base-node-path* '(:button)))))
      (setf text-sprite
            (diverge (sprite-node (append *base-node-path* '(:font :vera)))
                     :colour '(0.7 0.7 0.7 1.0)
                     :size 10))
      (init-class-snippets button
        (infocus-button-sprites *button-sprite-names*
                                 (node-of base-node :inactive :button :up))
        (focus-button-sprites *button-sprite-names*
                               (node-of base-node :active :button :up))
        (focus-button-down-sprites *button-sprite-names*
                                    (node-of base-node :active :button :down))
        (infocus-border-sprites *button-border-sprite-names*
                                 (node-of base-node :inactive :border))
        (focus-border-sprites *button-border-sprite-names*
                               (node-of base-node :active :border))))))

(defmethod draw-button ((button button))
  (with-slots (focus down focus-button-sprites focus-button-down-sprites
               (button-width width) infocus-button-sprites) button
    (with-sprite-snippets ((if focus
                               (if down
                                   focus-button-down-sprites
                                   focus-button-sprites)
                               infocus-button-sprites)
                           *button-sprite-names*)
      ; Left:
      (draw-snippet left)
      ; Centre:
      (incf x (width left))
      (setf width (- button-width (width left) (width right)))
      (draw-snippet centre)
      ; Right:
      (incf x width)
      (clear-width)
      (draw-snippet right))))

(defmethod draw-border ((button button))
  (with-slots (focus infocus-border-sprites focus-border-sprites
               (button-width width) (button-height height)) button
    (with-sprite-snippets ((if focus
                               focus-border-sprites
                               infocus-border-sprites)
                           *button-border-sprite-names*)
      ; Left:
      (decf x (width left))
      (draw-snippet left)
      ; Corner top left:
      (move-to (- (width corner-top-left)) (- (height corner-top-left)))
      (draw-snippet corner-top-left)
      ; Top left:
      (move-to 0 (- (height top-left)))
      (draw-snippet top-left)
      ; Top:
      (incf x (width top-left))
      (setf y (- (height top))
            width (- button-width
                     (width top-left)
                     (width top-right)))
      (draw-snippet top)
      ; Top right:
      (incf x width)
      (clear-width)
      (setf y (- (height top-right)))
      (draw-snippet top-right)
      ; Corner top right:
      (move-to button-width (- (height corner-top-right)))
      (draw-snippet corner-top-right)
      ; Right:
      (setf y 0)
      (draw-snippet right)
      ; Corner bottom right:
      (setf y (height right))
      (draw-snippet corner-bottom-right)
      ; Bottom right:
      (move-to (- button-width (width bottom-right)) button-height)
      (draw-snippet bottom-right)
      ; Bottom:
      (move-to (width bottom-left) button-height)
      (setf width (- button-width
                     (width bottom-left)
                     (width bottom-right)))
      (draw-snippet bottom)
      ; Bottom left:
      (clear-width)
      (move-to 0 button-height)
      (draw-snippet bottom-left)
      ; Corner bottom left:
      (decf x (width corner-bottom-left))
      (draw-snippet corner-bottom-left))))

(defmethod draw-text ((button button))
  (with-slots (text-sprite (button-width width) (button-height height)) button
    (draw-sprite text-sprite
                 :x (/ (- button-width (width text-sprite)) 2)
                 :y (/ (- button-height (height text-sprite)) 2))))

(defmethod draw ((button button))
  (draw-border button)
  (draw-button button)
  (draw-text button))

(defmethod handle-mouse-button-down ((button button) event)
  ())