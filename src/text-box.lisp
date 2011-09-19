; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass text-box (gui-widget)
  ((inactive-box-sprites :allocation :class)
   (inactive-border-sprites :allocation :class)
   (active-box-sprites :allocation :class)
   (active-border-sprites :allocation :class)))

(defmethod init-sprites ((text-box text-box))
  (with-slots (inactive-border-sprites inactive-box-sprites active-box-sprites
               active-border-sprites) text-box
   (init-class-snippets text-box
     (inactive-box-sprites *text-box-box-names*
                           (node-of *base-node* :inactive :text-box :box))
     (active-box-sprites *text-box-box-names*
                         (node-of *base-node* :active :text-box :box))
     (inactive-border-sprites *text-box-border-names*
                           (node-of *base-node* :inactive :text-box :border))
     (active-border-sprites *text-box-border-names*
                            (node-of *base-node* :active :text-box :border)))))

(defmethod draw-box ((text-box text-box))
  (with-slots (focus inactive-box-sprites active-box-sprites
               (text-box-width width) (text-box-height height)) text-box
    (with-sprite-snippets ((if focus
                               active-box-sprites
                               inactive-box-sprites)
                           *text-box-box-names*)
      ;; Corner top left:
      (draw-snippet corner-top-left)
      ;; Top:
      (incf x (width corner-top-left))
      (setf width (- text-box-width
                     (width corner-top-left)
                     (width corner-top-right))
            height (height corner-top-left))
      (draw-snippet top)
      ;; Corner top right:
      (incf x width)
      (clear-width)
      (clear-height)
      (draw-snippet corner-top-right)
      ;; Left:
      (move-to 0 (height corner-top-left))
      (setf width (width corner-top-left)
            height (- text-box-height
                      (height corner-top-left)
                      (height corner-top-right)))
      (draw-snippet left)
      ;; Centre:
      (incf x width)
      (setf width (- text-box-width
                     (width corner-top-left)
                     (width corner-top-right)))
      (draw-snippet centre)
      ;; Right:
      (incf x width)
      (setf width (width corner-top-right))
      (draw-snippet right)
      ;; Corner bottom left:
      (incf y height)
      (setf x 0)
      (clear-width)
      (clear-height)
      (draw-snippet corner-bottom-left)
      ;; Bottom:
      (incf x (width corner-bottom-left))
      (setf height (height corner-bottom-left)
            width (- text-box-width
                     (width corner-top-left)
                     (width corner-top-right)))
      (draw-snippet bottom)
      ;; Corner bottom right:
      (incf x width)
      (clear-width)
      (clear-height)
      (draw-snippet corner-bottom-right))))

(defmethod draw-border ((text-box text-box))
  (with-slots (focus inactive-border-sprites active-border-sprites
               (text-box-width width) (text-box-height height)) text-box
    (with-sprite-snippets ((if focus
                               active-border-sprites
                               inactive-border-sprites)
                           *text-box-border-names*)
      ;; Corner top left:
      (move-to (- (width corner-top-left)) (- (height corner-top-left)))
      (draw-snippet corner-top-left)
      ;; Top left:
      (setf x 0)
      (draw-snippet top-left)
      ;; Top:
      (incf x (width top-left))
      (setf width (- text-box-width
                     (width top-left)
                     (width top-right)))
      (draw-snippet top)
      ;; Top right:
      (incf x width)
      (clear-width)
      (draw-snippet top-right)
      ;; Corner top right:
      (incf x (width top-right))
      (draw-snippet corner-top-right)
      ;; Left Top:
      (move-to (- (width left-top)) 0)
      (draw-snippet left-top)
      ;; Right Top:
      (setf x text-box-width)
      (draw-snippet right-top)
      ;; Left:
      (move-to (- (width corner-top-left)) (height left-top))
      (setf width (width corner-top-left)
            height (- text-box-height
                      (height left-top)
                      (height left-bottom)))
      (draw-snippet left)
      ;; Right:
      (move-to text-box-width (height right-top))
      (setf width (width corner-top-right)
            height (- text-box-height
                      (height right-top)
                      (height right-bottom)))
      (draw-snippet right)
      ;; Left bottom:
      (setf x (- (width left-bottom)))
      (incf y height)
      (clear-width)
      (clear-height)
      (draw-snippet left-bottom)
      ;; Right bottom:
      (move-to text-box-width (- text-box-height
                                 (height right-bottom)))
      (draw-snippet right-bottom)
      ;; Corner bottom left:
      (move-to (- (width corner-bottom-left)) text-box-height)
      (draw-snippet corner-bottom-left)
      ;; Bottom left:
      (setf x 0)
      (draw-snippet bottom-left)
      ;; Bottom
      (incf x (width bottom-left))
      (setf width (- text-box-width
                     (width bottom-left)
                     (width bottom-right))
            height (height corner-bottom-left))
      (draw-snippet bottom)
      ;; Bottom right:
      (incf x width)
      (clear-width)
      (clear-height)
      (draw-snippet bottom-right)
      ;; Corner bottom right:
      (setf x text-box-width)
      (draw-snippet corner-bottom-right))))

(defmethod draw ((text-box text-box))
  (draw-border text-box)
  (draw-box text-box))