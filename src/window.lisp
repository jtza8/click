; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass window (container)
  ((active :initform nil)
   (inactive-shadows :initform (make-array (length *shadow-symbols*)))
   (active-shadows :initform (make-array (length *shadow-symbols*)))))

(defmethod update-margins ((window window))
  (flet ((get-max (func &rest sequences)
           (loop for sequence in sequences
                 for seq-max = (apply #'max (map 'list func sequence))
                 for max = seq-max then (max max seq-max)
                 finally (return max))))
    (with-slots (inactive-shadows top-margin right-margin bottom-margin
                 left-margin) window
      (setf top-margin (get-max #'height (subseq inactive-shadows 0 5))
            right-margin (get-max #'width (subseq inactive-shadows 4 9))
            bottom-margin (get-max #'height (subseq inactive-shadows 8 13))
            left-margin (get-max #'width (subseq inactive-shadows 12 16)
                                 (subseq inactive-shadows 0 1))))))

(defmethod init-shadow-sprites ((window window))
  (macrolet ((set-for (shadow-var state)
               `(with-slots (,shadow-var) window
                  (with-nodes ,*shadow-symbols*
                      (sprite-node :gui :window ,state :shadows)
                    (setf ,@(loop for shadow in *shadow-symbols*
                                  for i upfrom 0
                                  collect `(aref ,shadow-var ,i)
                                  collect shadow))))))
    (set-for inactive-shadows :inactive)
    (set-for active-shadows :active)))

(defmethod init-sprites ((window window))
  (init-shadow-sprites window)
  (update-margins window))

(defmacro with-shadows ((shadow-vector &optional (prefix "")) &body body)
  (let ((array (gensym "SHADOW-ARRAY-"))
        (symbols (loop for shadow in *shadow-symbols*
                       collect (intern (format nil "~:@(~a~)~:@(~a~)"
                                               prefix shadow)))))
   `(let ((,array ,shadow-vector))
      (let ,(loop for i upfrom 0
               for symbol in symbols
               collect `(,symbol (aref ,array ,i)))
        (declare (ignorable ,@symbols))
        ,@body))))

(defmethod draw-shadows ((window window))
  (with-slots (active inactive-shadows active-shadows (window-width width)
               (window-height height)) window
    (let ((width nil)
          (height nil)
          x y)
      (with-shadows ((if active active-shadows inactive-shadows))
        (flet ((draw-shadow (shadow &rest rest)
                 (apply #'draw-sprite shadow :x x :y y :width width
                        :height height rest)))
          ; Top left corner:
          (setf x (- (width corner-top-left)) y (- (height corner-top-left)))
          (draw-shadow corner-top-left)
          ; Top left:
          (setf x 0 y (- (height top-left)))
          (draw-shadow top-left)
          ; Top:
          (incf x (width top-left))
          (setf y (- (height top))
                width (- window-width
                         (width top-left)
                         (width top-right)))
          (draw-shadow top)
          ; Top right:
          (incf x width)
          (setf y (- (height top-right)) width nil)
          (draw-shadow top-right)
          ; Top corner right:
          (setf x window-width y (- (height corner-top-right)))
          (draw-shadow corner-top-right)
          ; Right top:
          (setf y 0)
          (draw-shadow right-top)
          ; Right:
          (incf y (height right-top))
          (setf height (- window-height
                          (height right-top)
                          (height right-bottom)))
          (draw-shadow right)
          ; Right bottom:
          (incf y height)
          (setf height nil)
          (draw-shadow right-bottom)
          ; Corner right bottom:
          (setf y window-height)
          (draw-shadow corner-bottom-right)
          ; Bottom right:
          (decf x (width bottom-right))
          (draw-shadow bottom-right)
          ; Bottom:
          (setf width (- window-width
                         (width bottom-left)
                         (width bottom-right)))
          (decf x width)
          (draw-shadow bottom)
          (setf width nil)
          ; Bottom left:
          (setf x 0)
          (draw-shadow bottom-left)
          ; Corner bottom left:
          (decf x (width corner-bottom-left))
          (draw-shadow corner-bottom-left)
          ; Left bottom:
          (decf y (height left-bottom))
          (setf x (- (width left-bottom)))
          (draw-shadow left-bottom)
          ; Left:
          (setf height (- window-height
                          (height left-top)
                          (height left-bottom)))
          (decf y height)
          (draw-shadow left)
          (setf height nil)
          ; Left top:
          (setf x (- (width left-top))
                y 0)
          (draw-shadow left-top))))))

(defmethod draw ((window window))
  (draw-shadows window))