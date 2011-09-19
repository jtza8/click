; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(in-package :click)

(defclass gui-widget (widget)
  ((focus :initform nil
          :accessor focus)
   (ignore-focus :initform nil
                 :accessor ignore-focus)))

(defmethod (setf focus) (value (widget gui-widget))
  (with-slots (ignore-focus focus) widget
    (when ignore-focus
      (error 'state-conflict
             :a "IGNORE-FOCUS set to T"
             :b "changing FOCUS"))
    (setf focus value)))