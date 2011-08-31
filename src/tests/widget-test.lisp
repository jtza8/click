; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass widget-test-button-controller (listener)
  ((view :initarg :view
         :initform (error "Must specify view."))
   buttons
   subject))

(defmethod initialize-instance
    :after ((controller widget-test-button-controller) &key)
  (desire-events controller :button-click #'handle-button-click)
  (with-slots (view buttons subject) controller
    (setf buttons (list (widget-of view :one)
                        (widget-of view :two)
                        (widget-of view :three))
          subject (widget-of view :subject))
    (dolist (button buttons)
     (subscribe button controller))))

(defmethod handle-button-click
    ((controller widget-test-button-controller) event)
  (with-slots (view buttons subject) controller
    (with-event-keys (source) event
      (cond ((eq source (car buttons))
             (setf (text subject) "Short"))
            ((eq source (cadr buttons))
             (setf (text subject) "Bit Longer"))
            ((eq source (caddr buttons))
             (setf (text subject) "Even a Bit Longer"))))))

(defun test-widgets-manually ()
  (with-display-system (:clear-colour '(0.1 0.1 0.1 1.0)
                        :width 1280
                        :height 800
                        :full-screen nil
                        :title "Widgets Test")
    (init-default-theme)
    (add-to-root
     (make-windows
       (window :x 100 :y 100 :width 150 :height 200)
       (:buttons
        (window :x 300 :y 120 :width 300 :height 200 :title "Buttons"
                :widgets ((:one (button :x 10 :y 30 :text "One"))
                          (:two (button :x 10 :y 60 :text "Two"))
                          (:three (button :x 10 :y 90 :text "Three"))
                          (:subject
                           (button :x 70 :y 30 :text "Rat")))))
       (window :x 200 :y 220 :width 250 :height 150))
     :windows)
    (make-instance 'widget-test-button-controller
      :view (widget-of-root :windows :buttons))
    (with-event-loop #'simple-top-level-event-handler
      (update-display-system))))