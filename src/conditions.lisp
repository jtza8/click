; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(define-condition invalid-widget (error)
  ((widget :initarg widget
           ;; :initform (error "Must specify widget.")
           ))
  (:report (lambda (condition stream)
             (with-slots (widget) condition
               (format stream "Invalid widget: ~s." widget)))))

(define-condition state-conflict (error)
  ((a :initarg :a
      :initform "chipmunks")
   (b :initarg :b
      :initform "meercats"))
  (:report (lambda (condition stream)
             (with-slots (a b) condition
               (format stream
                       "States A and B are mutually exclusive:~%~
                        A: ~a~%~
                        B: ~a"
                       (string-capitalize a :end 1)
                       (string-capitalize b :end 1))))))