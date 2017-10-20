


(defpackage aspectm.example
  (:use :cl :aspectm))

(in-package :aspectm.example)


(enable-macroexpand-hooks)

(set-standard-hook 'defun 'store-name :before)

(eval-when (:compile-toplevel)
  (defvar *names* nil))

(defmacro store-name (&whole form name args &body body)
  (declare (ignorable args body))
  (assert (eq (first form) 'defun))
  `(push ',name *names*))

(defun add (x y)
  (+ x y))

(print *names*)

(disable-macroexpand-hooks)

;; (aspectm:enable)
;; 
;; #+aspectm
;; (print 1)

