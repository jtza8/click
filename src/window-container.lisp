; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass window-container (container)
  ())

(defmethod initialize-instance :after ((container window-container) &key)
  (desire-events container :mouse-down #'handle-mouse-down
                 :key-down #'handle-key-down))

(defmethod focused-window ((container window-container) &optional (offset 0))
  (with-slots (widgets) container
    (let ((length (length widgets)))
      (unless (zerop length)
        (aref widgets (rem (+ (1- length) offset) length))))))

(defmethod focus-window ((container window-container) target-window)
  (with-slots (widgets) container
    (let ((focused-window (focused-window container)))
      (setf (focus focused-window) nil
            (focus target-window) t)
      (send-event focused-window '(:window-focus
                                   :state nil))
      (send-event target-window '(:window-focus
                                  :state t))
      (order-top container target-window))))

(defmethod add-widget ((container window-container) (window widget)
                       &optional tag)
  (declare (ignore tag))
  (check-type window window)
  (let ((focused-window (focused-window container)))
    (unless (null focused-window)
      (setf (focus focused-window) nil))
    (setf (focus window) t))
  (call-next-method))

(defmethod handle-mouse-down ((container window-container) event)
  (with-slots (widgets) container
    (when (> (length widgets) 1)
      (with-event-keys (button) event
        (multiple-value-bind (x y) (mouse-pos)
          (unless (and (within (focused-window container) x y)
                       (eq button :left))
            (loop for i from (- (length widgets) 1) downto 0
                  for new-window = (aref widgets i)
                  when (within new-window x y)
                  do (progn
                       (focus-window container new-window)
                       (return)
                       ;; Click to focus?
                       ;; (return-from handle-mouse-down)
                       )))))))
  (send-event container event))

(defmethod handle-key-down ((container window-container) event)
  (with-event-keys (key) event
    (if (and (key-down-p :lalt :ralt)
             (eq key :pageup))
        (focus-window container (focused-window container 1))
        (send-event container event))))

(defmethod send-event ((container window-container) event &rest targets)
  (declare (ignore targets))
  (call-next-method container event (focused-window container)))