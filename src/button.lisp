; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass button (gui-widget)
  ((down :initform nil
         :accessor down)
   (height :initform 20)
   (width :initform 50)
   (inactive-button-sprites :allocation :class)
   (inactive-border-sprites :allocation :class)
   (active-button-sprites :allocation :class)
   (active-button-down-sprites :allocation :class)
   (active-border-sprites :allocation :class)
   (ideal-width :initform 0)
   inactive-text-sprite
   active-text-sprite))

(defmethod initialize-instance :after ((button button)
                                       &key (text "") (width 50))
  (with-slots (text-sprite ideal-width) button
    (setf (text button) text
          ideal-width width)
    (update-width button))
  (desire-events button :mouse-down #'handle-mouse-down
                 :mouse-up #'handle-mouse-up
                 :mouse-pos #'handle-mouse-pos
                 :key-down #'handle-key-down
                 :key-up #'handle-key-up)
  (provide-events button :button-click))

(defmethod init-sprites :after ((button button))
  (with-slots (focus inactive-button-sprites active-button-sprites
               active-button-down-sprites inactive-border-sprites
               active-border-sprites inactive-text-sprite
               active-text-sprite) button
    (setf inactive-text-sprite
          (clone (node-of *base-node* :inactive :button :font))
          active-text-sprite
          (clone (node-of *base-node* :active :button :font)))
    (init-class-snippets button
      (inactive-button-sprites *button-sprite-names*
                               (node-of *base-node* :inactive :button :up))
      (active-button-sprites *button-sprite-names*
                             (node-of *base-node* :active :button :up))
      (active-button-down-sprites *button-sprite-names*
                                  (node-of *base-node* :active :button :down))
      (inactive-border-sprites *button-border-sprite-names*
                               (node-of *base-node* :inactive :button :border))
      (active-border-sprites *button-border-sprite-names*
                             (node-of *base-node* :active :button :border)))))

(internal update-width)
(defmethod update-width ((button button))
  (with-slots (focus active-text-sprite inactive-text-sprite
               active-button-sprites inactive-button-sprites width
               ideal-width) button
    (with-sprite-snippets ((if focus
                               active-button-sprites
                               inactive-button-sprites)
                           *button-sprite-names*
                           "button-")
      (setf width (max ideal-width (+ (width (if focus
                                                 active-text-sprite
                                                 inactive-text-sprite))
                                      (width button-left)
                                      (width button-right)))))))

(defmethod (setf width) (value (button button))
  (with-slots (ideal-width) button
    (setf ideal-width value)
    (update-width button)))

(defmethod text ((button button))
  (with-slots (focus inactive-text-sprite active-text-sprite) button
    (text (if focus active-text-sprite inactive-text-sprite))))

(defmethod (setf text) (value (button button))
  (with-slots (inactive-text-sprite active-text-sprite width) button
    (setf (text inactive-text-sprite) value
          (text active-text-sprite) value)
    (update-width button)))

(defmethod (setf focus) (value (button button))
  (with-slots (focus) button
    (setf focus value)
    (update-width button)))

(internal update-button-state)
(defmethod update-button-state ((button button))
  (with-slots (down) button
    (multiple-value-bind (x y) (mouse-pos)
      (setf down (and (mouse-down-p :left) (within button x y))))))

(defmethod handle-mouse-down ((button button) event)
  (with-event-keys ((mouse-button button)) event
    (when (eq mouse-button :left)
      (update-button-state button))))

(defmethod handle-mouse-up ((button button) event)
  (with-slots (down) button
    (with-event-keys ((mouse-button button)) event
      (multiple-value-bind (x y) (mouse-pos)
        (when (and (within button x y) (eq mouse-button :left))
          (send-event button `(:button-click :source ,button))
          (setf down nil))))))

(defmethod handle-mouse-pos ((button button) event)
  (update-button-state button))

(defmethod handle-key-down ((button button) event)
  (with-slots (down) button
    (with-event-keys (key) event
      (when (eq key #\Space)
        (setf down t)))))

(defmethod handle-key-up ((button button) event)
  (with-slots (down) button
    (with-event-keys (key) event
      (when (and down (eq key #\Space))
        (send-event button `(:button-click :source ,button))
        (setf down nil)))))

(defmethod draw-button ((button button))
  (with-slots (focus down active-button-sprites active-button-down-sprites
               (button-width width) inactive-button-sprites) button
    (with-sprite-snippets ((if focus
                               (if down
                                   active-button-down-sprites
                                   active-button-sprites)
                               inactive-button-sprites)
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
  (with-slots (focus inactive-border-sprites active-border-sprites
               (button-width width) (button-height height)) button
    (with-sprite-snippets ((if focus
                               active-border-sprites
                               inactive-border-sprites)
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
  (with-slots (focus inactive-text-sprite active-text-sprite
               (button-width width) (button-height height)) button
    (let ((text-sprite (if focus active-text-sprite inactive-text-sprite)))
      (draw-sprite text-sprite
                   :x (/ (- button-width (width text-sprite)) 2)
                   :y (/ (- button-height (size text-sprite)) 2)))))

(defmethod draw ((button button))
  (draw-border button)
  (draw-button button)
  (draw-text button))