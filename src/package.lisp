; Use of this source code is governed by a BSD-style
; license that can be found in the license.txt file
; in the root directory of this project.

(defpackage :click
  (:use :cl :cl-user :interact :meta-package :split-sequence))
(in-package :click)

(defparameter *shadow-names*
  '(corner-top-left top-left top top-right
    corner-top-right right-top right right-bottom
    corner-bottom-right bottom-right bottom bottom-left
    corner-bottom-left left-bottom left left-top))

(defparameter *window-panel-names*
  '(centre corner-top-left top corner-top-right
    right corner-bottom-right bottom corner-bottom-left left))

(defparameter *button-sprite-names*
  '(left centre right))

(defparameter *button-border-sprite-names*
  '(left corner-top-left top-left top top-right corner-top-right
    right corner-bottom-right bottom-right bottom bottom-left
    corner-bottom-left))

(defparameter *text-box-border-names*
  '(corner-top-left top-left top top-right
    corner-top-right right-top right right-bottom
    corner-bottom-right bottom-right bottom bottom-left
    corner-bottom-left left-bottom left left-top))

(defparameter *text-box-box-names*
  '(centre corner-top-left top corner-top-right
    right corner-bottom-right bottom corner-bottom-left left))

(defvar *base-node*)
(defparameter *base-node-path* '(:gui))
(defparameter *base-sprite-path*
  (asdf:system-relative-pathname :click "../gui/"))

(defparameter *window-title-y* 3)
