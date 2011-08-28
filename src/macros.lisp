; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defmacro make-widget (widget-s-exp)
  (labels ((parse-s-exp (s-exp)
             (if (keywordp (car s-exp))
                 `(list ,(car s-exp) ,(parse-s-exp (cadr s-exp)))
                 (let ((widget-symbol (intern (symbol-name (car s-exp)))))
                   `(make-instance ',widget-symbol
                      ,@(loop for (key value) on (cdr s-exp) by #'cddr
                              collect key
                              if (eq key :widgets)
                              collect `(list ,@(mapcar #'parse-s-exp value))
                              else collect value))))))
    (parse-s-exp widget-s-exp)))

(defmacro make-windows (&body s-exps)
  `(make-instance 'window-container
     :widgets (list ,@(loop for s-exp in s-exps
                            collect `(make-widget ,s-exp)))))
