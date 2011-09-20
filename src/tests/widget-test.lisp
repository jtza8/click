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
  (with-display-system (:clear-colour '(0.5 0.5 0.5 1.0)
                        :width 1280
                        :height 800
                        :full-screen nil
                        :title "Widgets Test")
    (init-default-theme)
    (add-to-root
     (make-container (window-container)
       (window :x 100 :y 100 :width 300 :height 200 :title "Text Boxes"
               :widgets ((label :x 10 :y 36 :text "Single-line Text-box:")
                         (:one-line
                          (text-box :x 120 :y 30 :width 170 :height 20))
                         (label :x 100 :y 67 :text "Multi-line Text-box:")
                         (:multi-line
                          (text-box :x 10 :y 85 :width 280 :height 105))))
       (:buttons
        (window :x 300 :y 120 :width 300 :height 200 :title "Buttons"
                :widgets ((:one (button :x 10 :y 40 :text "One"))
                          (:two (button :x 10 :y 70 :text "Two"))
                          (:three (button :x 10 :y 100 :text "Three"))
                          (label :text "Changable button:" :x 70 :y 25)
                          (:subject
                           (button :x 70 :y 40 :text "Rat")))))
       (window :x 200 :y 220 :width 250 :height 150 :title "Labels"
               :widgets ((label :text "Unfocus-able label." :x 80 :y 70))))
     :windows)
    (make-instance 'widget-test-button-controller
      :view (widget-of-root :windows :buttons))
    (with-event-loop
      (basic-event-handler)
      (update-display-system))))