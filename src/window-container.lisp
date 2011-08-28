; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass window-container (container)
  ())

(defmethod initialize-instance :after ((container window-container) &key)
  (desire-events container :mouse-button-down #'handle-mouse-button-down))

(defmethod focus-window ((container window-container))
  (with-slots (widgets) container
    (let ((length (length widgets)))
      (unless (zerop length)
       (elt widgets (1- length))))))

(defmethod add-widget ((container window-container) (window widget)
                       &optional tag)
  (declare (ignore tag))
  (check-type window window)
  (let ((focus-window (focus-window container)))
    (unless (null focus-window)
      (setf (focus focus-window) nil))
    (setf (focus window) t))
  (call-next-method))

(defmethod handle-mouse-button-down ((container window-container) event)
  (with-slots (widgets) container
    (when (> (length widgets) 1)
      (with-event-keys (x y button) event
        (unless (and (within (focus-window container) x y)
                     (= button 1))
          (loop for i from (- (length widgets) 1) downto 0
                for new-window = (aref widgets i)
                when (within new-window x y)
                  do (let ((focus-window (focus-window container)))
                       (setf (focus focus-window) nil
                             (focus new-window) t)
                       (send-event focus-window '(:click-window-focus
                                                   :state nil))
                       (send-event new-window '(:click-window-focus
                                                :state t))
                       (order-top container new-window)
                       (return)
                       ;; Click to focus?
                       ;; (return-from handle-mouse-button-down)
                       ))))))
  (send-event container event))

(defmethod send-event ((container window-container) event &rest targets)
  (declare (ignore targets))
  (call-next-method container event (focus-window container)))