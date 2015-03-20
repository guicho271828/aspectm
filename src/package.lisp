#|
  This file is a part of aspectm project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage aspectm
  (:use :cl :alexandria :lisp-namespace)
  (:export
   #:define-macroexpand-hook))
(in-package :aspectm)

;; blah blah blah.

(lispn:define-namespace macroexpand-before-hooks)
(lispn:define-namespace macroexpand-around-hooks)
(lispn:define-namespace macroexpand-after-hooks)

(let (prev)
  (defun add-macroexpand-hooks ()
    (psetf *macroexpand-hook* 'macroexpand-hooks-hook
           prev *macroexpand-hook*))
  (defun disable-macroexpand-hooks ()
    (assert (eq *macroexpand-hook* prev) nil
            "*macroexpand-hook* is overwritten as ~a by some other program. stay alert!"
            *macroexpand-hook*)
    (setf *macroexpand-hook* prev))
  (defun macroexpand-hooks-hook (macrofn form env)
    (when-let ((hook (ignore-errors
                       (symbol-macroexpand-before-hooks
                        (car form)))))
      (funcall hook form env))
    (funcall prev macrofn form env)
    (when-let ((hook (ignore-errors
                       (symbol-macroexpand-after-hooks
                        (car form)))))
      (funcall hook form env))))

(defmacro define-macroexpand-hook (name when args &body body)
  (assert (member when '(:before :after)))
  `(setf (,(if (eq :before when)
               'symbol-macroexpand-before-hooks
               'symbol-macroexpand-after-hooks) ',name)
         (lambda ,args ,@body)))


