; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass widget-container (container)
  ((focused-widget :initform nil)))

(defmethod initialize-instance :after ((container widget-container) &key)
  (desire-events container :key-down #'handle-key-down
                 :mouse-button-down #'handle-mouse-button-down))

(defmethod focused-widget ((container widget-container) &optional (offset 0))
  (with-slots (widgets focused-widget) container
    (if (or (zerop offset) (null focused-widget))
        focused-widget
        (let ((index (rem (+ (position focused-widget widgets) offset)
                          (length widgets))))
          (aref widgets (if (minusp index)
                            (+ (length widgets) index)
                            index))))))

(defmethod focus-widget ((container widget-container) (widget widget))
  (with-slots (widgets focused-widget) container
    (unless (find widget widgets)
      (error 'invalid-widget :widget widget))
    (setf (focus focused-widget) nil
          (focus widget) t
          focused-widget widget)))

(defmethod offset-widget-focus ((container widget-container)
                                &optional (offset 0))
  (with-slots (focused-widget widgets) container
    (when (or (zerop offset) (zerop (length widgets)))
      (return-from offset-widget-focus))
    (let ((widget (focused-widget container offset)))
      (setf (focus focused-widget) nil
            (focus widget) t
            focused-widget widget))))

(defmethod add-widget :after ((container widget-container) (widget widget)
                              &optional tag)
  (declare (ignore tag))
  (with-slots (widgets focused-widget) container
    (when (= (length widgets) 1)
      (let ((widget (aref widgets 0)))
        (setf (focus widget) t
              focused-widget widget)))))

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
  (with-event-keys (key mod-key) event
    (if (eq key :tab)
        (offset-widget-focus container
                             (if (find :lshift mod-key) -1 1))
        (send-event container event))))

(defmethod handle-mouse-button-down ((container widget-container) event)
  (send-event container event))

(defmethod handle-mouse-button-down :around ((container widget-container) event)
  (with-slots (widgets) container
    (with-event-keys (x y) event
      (loop with found-focus-target
            for widget across widgets
            when (within widget x y)
              do (progn (focus-widget container widget)
                        (setf found-focus-target t)
                        (send-event container event)
                        (return))
            finally (unless found-focus-target (call-next-method))))))