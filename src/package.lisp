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
  (psetf *macroexpand-hook* 'macroexpand-hooks-hook
         *old-hook* *macroexpand-hook*))
(defun disable-macroexpand-hooks ()
  (assert (eq *macroexpand-hook* *old-hook*) nil
          "*macroexpand-hook* is overwritten as ~a by some other program. stay alert!"
          *macroexpand-hook*)
  (setf *macroexpand-hook* *old-hook*))


;;; around-hooks


(define-condition in-next-hook () ())

;; none of these variables should be external

(let ((around-hooks '(funcall-as-hook)))
  (defun add-around-hook (fname)
    (assert (symbolp fname))
    (push fname around-hooks))
  (defun remove-around-hook (fname)
    (removef fname around-hooks)
    (unless around-hooks
      (warn "Popping the hooks resulted in NIL. Restoring '(funcall-as-hook)")
      (setf around-hooks '(funcall-as-hook))))
  (defun macroexpand-hooks-hook (macrofn form env)
    (declare (special macrofn form env))
    (call-next-hook))
  (defun call-next-hook ()
    (declare (special macrofn form env))
    (restart-case
        (signal 'in-next-hook)
      (continue ()))
    (destructuring-bind (first-hook around-hooks) around-hooks
      (declare (special around-hooks))
      (let (next-hook-called)
        (handler-bind ((in-next-hook (lambda (c)
                                       (setf next-hook-called t)
                                       (continue c))))
          (funcall first-hook macrofn form env))
        (unless next-hook-called
          (error "~a is not calling the next hook through call-next-hook!" first-hook))))))

(defun funcall-as-hook (macrofn form env)
  (restart-case
      (signal 'in-next-hook)
    (continue ()))
  (funcall macrofn form env))


;;; standard hooks

(lispn:define-namespace before-hooks (cons symbol function))
(lispn:define-namespace after-hooks  (cons symbol function))

(defun standard-hook (macrofn form env)
  `(progn
     ,@(when-let ((hooks (ignore-errors
                           (symbol-before-hooks
                            (car form)))))
         (mapcar (lambda (%) (funcall % form env)) hooks))
     ,(call-next-hook)
     ,@(when-let ((hooks (ignore-errors
                           (symbol-after-hooks
                            (car form)))))
         (mapcar (lambda (%) (funcall % form env)) hooks))))

(defun set-standard-hook (name hook &optional (method :before))
  (assert (member method '(:before :after)))
  (if (eq :before method)
      (progn
        (unless (before-hooks-boundp name)
          (setf (before-hooks-boundp name) nil))
        (pushnew hook (symbol-before-hooks name)))
      (progn
        (unless (after-hooks-boundp name)
          (setf (after-hooks-boundp name) nil))
        (pushnew hook (symbol-after-hooks name))))
  tag)

(defmacro define-standard-hook ((name &optional hook-name (method :before)) args &body body)
  (let ((hook-name (or hook-name (gensym "HOOK"))))
    `(progn
       (defmacro ,hook-name ,args ,@body)
       (set-hook-for ',name ',hook-name ,method))))

(defun remove-standard-hook (name hook method)
  (assert (member method '(:before :after)))
  (if (eq :before method)
      (removef hook (symbol-before-hooks name))
      (removef hook (symbol-after-hooks name))))


