; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass window (container)
  ((active :initform nil)
   (inactive-shadows :allocation :class)
   (active-shadows :allocation :class)))

(defmethod init-sprites :after ((window window))
  (with-slots (inactive-shadows active-shadows) window
    (unless (slot-boundp window 'inactive-shadows)
      (setf inactive-shadows
            (make-sprite-snippets 
               *shadow-names*
               (sprite-node :gui :window :inactive :shadows))))
    (unless (slot-boundp window 'active-shadows)
      (setf active-shadows
            (make-sprite-snippets
               *shadow-names*
               (sprite-node :gui :window :active :shadows))))))

(defmethod draw-shadows ((window window))
  (with-slots (active inactive-shadows active-shadows (window-width width)
               (window-height height)) window
    (with-sprite-snippets ((if active active-shadows inactive-shadows)
                           *shadow-names*)
          ; Top left corner:
          (move-to (- (width corner-top-left)) (- (height corner-top-left)))
          (draw-snippet corner-top-left)
          ; Top left:
          (move-to 0 (- (height top-left)))
          (draw-snippet top-left)
          ; Top:
          (incf x (width top-left))
          (setf y (- (height top)))
          (setf width (- window-width (width top-left) (width top-right)))
          (draw-snippet top)
          ; Top right:
          (incf x width)
          (clear-width)
          (setf y (- (height top-right)))
          (draw-snippet top-right)
          ; Top corner right:
          (move-to window-width (- (height corner-top-right)))
          (draw-snippet corner-top-right)
          ; Right top:
          (setf y 0)
          (draw-snippet right-top)
          ; Right:
          (incf y (height right-top))
          (setf height (- window-height
                          (height right-top)
                          (height right-bottom)))
          (draw-snippet right)
          ; Right bottom:
          (incf y height)
          (clear-height)
          (draw-snippet right-bottom)
          ; Corner right bottom:
          (setf y window-height)
          (draw-snippet corner-bottom-right)
          ; Bottom right:
          (decf x (width bottom-right))
          (draw-snippet bottom-right)
          ; Bottom:
          (setf width (- window-width
                         (width bottom-left)
                         (width bottom-right)))
          (decf x width)
          (draw-snippet bottom)
          (clear-width)
          ; Bottom left:
          (setf x 0)
          (draw-snippet bottom-left)
          ; Corner bottom left:
          (decf x (width corner-bottom-left))
          (draw-snippet corner-bottom-left)
          ; Left bottom:
          (decf y (height left-bottom))
          (setf x (- (width left-bottom)))
          (draw-snippet left-bottom)
          ; Left:
          (setf height (- window-height
                          (height left-top)
                          (height left-bottom)))
          (decf y height)
          (draw-snippet left)
          (clear-height)
          ; Left top:
          (setf x (- (width left-top))
                y 0)
          (draw-snippet left-top))))

(defmethod draw ((window window))
  (draw-shadows window))