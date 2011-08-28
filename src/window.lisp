; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass window (widget-container)
  ((focus :initform nil
           :accessor focus)
   (dragging :initform nil
             :reader dragging)
   (drag-x-offset :initform 0)
   (drag-y-offset :initform 0)
   (infocus-shadows :allocation :class)
   (focus-shadows :allocation :class)
   (infocus-panel :allocation :class)
   (focus-panel :allocation :class)))

(defmethod initialize-instance :after ((window window) &key)
  (desire-events window :mouse-button-down #'handle-mouse-button-down
                 :mouse-button-up #'handle-mouse-button-up
                 :mouse-motion #'handle-mouse-motion)
  (provide-events window :click-window-focus))

(defmethod init-sprites :after ((window window))
  (with-slots (infocus-shadows focus-shadows
               infocus-panel focus-panel) window
    (let ((base-node (sprite-node (append *base-node-path* '(:window)))))
      (init-class-snippets window
        (infocus-shadows *shadow-names* (node-of base-node :inactive :shadows))
        (focus-shadows *shadow-names* (node-of base-node :active :shadows))
        (infocus-panel *window-panel-names*
                        (node-of base-node :inactive :panel))
        (focus-panel *window-panel-names*
                      (node-of base-node :active :panel))))))

(defmethod handle-mouse-button-down ((window window) event)
  (with-slots (dragging drag-x-offset drag-y-offset) window
    (with-event-keys (x y button) event
      (when (and (within window x y) (= button 1))
        (setf dragging t
              drag-x-offset (- (absolute-x window) x)
              drag-y-offset (- (absolute-y window) y)))))
  (send-event window event))

(defmethod handle-mouse-button-up ((window window) event)
  (with-slots (dragging) window
    (with-event-keys (button) event
      (when (= button 1)
        (setf dragging nil))))
  (send-event window event))

(defmethod handle-mouse-motion ((window window) event)
  (with-slots ((window-x x) (window-y y) dragging drag-x-offset
               drag-y-offset parent) window
    (with-event-keys (x y) event
      (when dragging
        (let ((dx x)
              (dy y))
          (when parent
            (destructuring-bind (ax ay) (absolute-pos parent)
              (setf dx (- x ax) dy (- y ay))))
          (setf window-x (+ dx drag-x-offset)
                window-y (+ dy drag-y-offset))))))
  (send-event window event))

(defmethod draw-shadows ((window window))
  (with-slots (focus infocus-shadows focus-shadows (window-width width)
               (window-height height)) window
    (with-sprite-snippets ((if focus focus-shadows infocus-shadows)
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

(defmethod draw-panel ((window window))
  (with-slots (focus infocus-panel focus-panel (window-width width)
                      (window-height height)) window
    (with-sprite-snippets ((if focus focus-panel infocus-panel)
                           *window-panel-names*)
      (let ((centre-width (- window-width
                             (width corner-top-left)
                             (width corner-top-right)))
            (centre-height (- window-height
                              (height corner-top-right)
                              (height corner-bottom-right))))
        ; Top left corner:
        (draw-snippet corner-top-left)
        ; Top:
        (incf x (width corner-top-left))
        (setf width centre-width)
        (draw-snippet top)
        ; Top right corner:
        (incf x width)
        (clear-width)
        (draw-snippet corner-top-right)
        ; Right:
        (setf height centre-height)
        (setf y (height corner-top-right)
              width (width corner-top-right))
        (draw-snippet right)
        ; Bottom right corner:
        (incf y height)
        (clear-height)
        (clear-width)
        (draw-snippet corner-bottom-right)
        ; Bottom:
        (setf x (width corner-bottom-right)
              width centre-width
              height (height corner-bottom-right))
        (draw-snippet bottom)
        (clear-width)
        (clear-height)
        ; Bottom left corner:
        (setf x 0)
        (draw-snippet corner-bottom-left)
        ; Left:
        (setf y (height corner-top-left)
              height centre-height
              width (width corner-top-left))
        (draw-snippet left)
        ; Centre:
        (setf x width
              width centre-width
              height centre-height)
        (draw-snippet centre)))))

(defmethod draw :before ((window window))
  (draw-shadows window)
  (draw-panel window))