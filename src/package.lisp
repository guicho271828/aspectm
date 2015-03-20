#|
  This file is a part of aspectm project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage aspectm
  (:use :cl :alexandria :lisp-namespace)
  (:export
   #:enable-macroexpand-hooks
   #:disable-macroexpand-hooks
   ;; around hooks
   #:call-next-hook
   #:add-macroexpand-hook
   ;; standard hooks
   #:define-macroexpand-hook
   #:set-macroexpand-hook-for
   #:remove-macroexpand-hook))
(in-package :aspectm)

;;; enabling hooks

(defvar *old-hook*)
(defun enable-macroexpand-hooks ()
  (psetf *macroexpand-hook* 'call-next-hook
         *old-hook* *macroexpand-hook*))
(defun disable-macroexpand-hooks ()
  (assert (eq *macroexpand-hook* *old-hook*) nil
          "*macroexpand-hook* is overwritten as ~a by some other program. stay alert!"
          *macroexpand-hook*)
  (setf *macroexpand-hook* *old-hook*))


;;; around-hooks

(defvar *macroexpand-around-hooks* '(funcall))
(defun call-next-hook (macrofn form env)
  (destructuring-bind (first-hook *macroexpand-around-hooks*)
      *macroexpand-around-hooks*
    (funcall first-hook macrofn form env)))



;;; standard hooks

(lispn:define-namespace macroexpand-before-hooks)
(lispn:define-namespace macroexpand-after-hooks)


(defun macroexpand-standard-hook (macrofn form env)
  (when-let ((hooks (ignore-errors
                      (symbol-macroexpand-before-hooks
                       (car form)))))
    (map nil (lambda (%) (funcall % form env)) hooks))
  (call-next-hook macrofn form env)
  (when-let ((hooks (ignore-errors
                      (symbol-macroexpand-after-hooks
                       (car form)))))
    (map nil (lambda (%) (funcall % form env)) hooks)))

(defun set-macroexpand-hook-for (name fn &key (when :before) (tag (gensym)))
  (assert (member when '(:before :after)))
  (if (eq :before when)
      (progn
        (unless (macroexpand-before-hooks-boundp name)
          (setf (macroexpand-before-hooks-boundp name) nil))
        (pushnew (cons tag fn)
                 (macroexpand-before-hooks-boundp name)
                 :key #'car))
      (progn
        (unless (macroexpand-after-hooks-boundp name)
          (setf (macroexpand-after-hooks-boundp name) nil))
        (pushnew (cons tag fn)
                 (symbol-macroexpand-after-hooks name)
                 :key #'car)))
  tag)

(defmacro define-macroexpand-hook (name (&key (when :before) (tag (gensym))) args &body body)
  `(set-macroexpand-hook-for
    ',name
    (lambda ,args ,@body)
    :when ,when :tag ',tag))

(defun remove-macroexpand-hook (name when tag)
  (assert (member when '(:before :after)))
  (if (eq :before when)
      (removef (symbol-macroexpand-before-hooks name) tag :key #'car)
      (removef (symbol-macroexpand-after-hooks name) tag :key #'car)))


