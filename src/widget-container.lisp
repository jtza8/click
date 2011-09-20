; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass widget-container (container)
  ((focused-widget :initform nil)))

(defmethod initialize-instance :after ((container widget-container) &key)
  (desire-events container :key-down #'handle-key-down
                 :mouse-down #'handle-mouse-down))

(declaim (inline focusable-widgets))
(defmethod focusable-widgets ((container widget-container))
  (with-slots (widgets focused-widget) container
    (remove-if #'ignore-focus widgets)))

(defmethod focused-widget ((container widget-container) &optional (offset 0))
  (with-slots (focused-widget) container
    (let ((widgets (focusable-widgets container)))
      (if (or (zerop offset) (null focused-widget))
          focused-widget
          (let ((index (rem (+ (position focused-widget widgets) offset)
                            (length widgets))))
            (aref widgets (if (minusp index)
                              (+ (length widgets) index)
                              index)))))))

(defmethod focus-widget ((container widget-container) widget)
  (with-slots (widgets focused-widget) container
    (let ((widgets (focusable-widgets container)))
      (unless (or (null widget) (find widget widgets))
        (error 'invalid-widget :widget widget))
      (when focused-widget (setf (focus focused-widget) nil))
      (when widget (setf (focus widget) t))
      (setf focused-widget widget))))

(defmethod offset-widget-focus ((container widget-container)
                                &optional (offset 0))
  (with-slots (focused-widget widgets) container
    (let ((widgets (remove-if #'ignore-focus widgets)))
      (when (or (zerop offset) (zerop (length widgets)))
        (return-from offset-widget-focus))
      (let ((widget (focused-widget container offset)))
        (setf (focus focused-widget) nil
              (focus widget) t
              focused-widget widget)))))

(defmethod (setf focus) :after (value (container widget-container))
  (with-slots (focused-widget) container
    (when focused-widget
      (setf (focus focused-widget) value))))

(defmethod add-widget :after ((container widget-container) (widget widget)
                              &optional tag)
  (declare (ignore tag))
  (with-slots (focused-widget) container
    (let ((widgets (focusable-widgets container)))
      (when (and (null focused-widget)
                 (> (length widgets) 0))
        (let ((widget (aref widgets 0)))
          (setf (focus widget) t
                focused-widget widget))))))

(defmethod remove-widget :around ((container widget-container) (widget widget)
                                  &key (unsubscribes t))
  (declare (ignore unsubscribes))
  (let ((next-widget (focused-widget container 1)))
    (call-next-method)
    (focus-widget container next-widget)))

(defmethod send-event ((container widget-container) event &rest targets)
  (declare (ignore targets))
  (let ((target (focused-widget container)))
    (if (and target (desires-event-p target (event-type event)))
        (call-next-method container event target)
        (call-next-method))))

(defmethod handle-key-down ((container widget-container) event)
  (with-event-keys (key) event
    (if (eq key :tab)
        (offset-widget-focus container
                             (if (or (key-down-p :lshift)
                                     (key-down-p :rshift))
                                 -1 1))
        (send-event container event))))

(defmethod handle-mouse-down ((container widget-container) event)
  (send-event container event))

(defmethod handle-mouse-down :around ((container widget-container) event)
  (with-slots (widgets) container
    (multiple-value-bind (x y) (mouse-pos)
      (loop with found-focus-target
            for widget across widgets
            when (within widget x y)
              do (progn (focus-widget container widget)
                        (setf found-focus-target t)
                        (send-event container event)
                        (return))
            finally (unless found-focus-target (call-next-method))))))