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
   (inactive-shadows :allocation :class)
   (active-shadows :allocation :class)
   (inactive-panel :allocation :class)
   (active-panel :allocation :class)
   active-title-font
   inactive-title-font))

(defmethod initialize-instance :after ((window window) &key (title "Untitled"))
  (desire-events window :mouse-down #'handle-mouse-down
                 :mouse-up #'handle-mouse-up
                 :mouse-pos #'handle-mouse-pos)
  (provide-events window :window-focus)
  (setf (title window) title))

(defmethod init-sprites :after ((window window))
  (with-slots (inactive-shadows active-shadows inactive-panel active-panel
               inactive-title-font active-title-font) window
    (setf inactive-title-font
          (clone (node-of *base-node* :window :inactive :title-font))
          active-title-font
          (clone (node-of *base-node* :window :active :title-font)))
    (init-class-snippets window
      (inactive-shadows *shadow-names*
                        (node-of *base-node* :window :inactive :shadows))
      (active-shadows *shadow-names*
                      (node-of *base-node* :window :active :shadows))
      (inactive-panel *window-panel-names*
                      (node-of *base-node* :window :inactive :panel))
      (active-panel *window-panel-names*
                    (node-of *base-node* :window :active :panel)))))

(defmethod handle-mouse-down ((window window) event)
  (with-slots (dragging drag-x-offset drag-y-offset) window
    (with-event-keys (button) event
      (multiple-value-bind (x y) (mouse-pos)
        (when (and (within window x y) (eq button :left))
          (setf dragging t
                drag-x-offset (- (absolute-x window) x)
                drag-y-offset (- (absolute-y window) y)))))))

(defmethod handle-mouse-up ((window window) event)
  (with-slots (dragging) window
    (with-event-keys (button) event
      (when (eq button :left)
        (setf dragging nil))))
  (send-event window event))

(defmethod handle-mouse-pos ((window window) event)
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

(defmethod title ((window window))
  (with-slots (focus active-title-font inactive-title-font) window
    (text (if focus active-title-font inactive-title-font))))

(defmethod (setf title) (value (window window))
  (with-slots (active-title-font inactive-title-font) window
    (setf (text active-title-font) value
          (text inactive-title-font) value)))

(defmethod draw-shadows ((window window))
  (with-slots (focus inactive-shadows active-shadows (window-width width)
               (window-height height)) window
    (with-sprite-snippets ((if focus active-shadows inactive-shadows)
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
  (with-slots (focus inactive-panel active-panel (window-width width)
                      (window-height height)) window
    (with-sprite-snippets ((if focus active-panel inactive-panel)
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

(defmethod draw-title ((window window))
  (with-slots (focus active-title-font inactive-title-font width) window
    (let ((title-font (if focus active-title-font inactive-title-font)))
     (draw-sprite title-font :x (/ (- width (width title-font)) 2)
                  :y *window-title-y*))))

(defmethod draw :before ((window window))
  (draw-shadows window)
  (draw-panel window)
  (draw-title window))