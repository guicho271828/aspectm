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
   #:macroexpand-hooks-hook
   ;; around hooks
   #:call-next-hook
   #:add-around-hook
   #:remove-around-hook
   ;; standard hooks
   #:standard-hook
   #:set-standard-hook
   #:define-standard-hook
   #:remove-standard-hook
   #:around-hooks))
(in-package :aspectm)

;;; enabling hooks

(defvar *old-hook*)
(defun enable-macroexpand-hooks ()
  (psetf *macroexpand-hook* 'macroexpand-hooks-hook
         *old-hook* *macroexpand-hook*))
(defun disable-macroexpand-hooks ()
  (assert (eq *macroexpand-hook* 'macroexpand-hooks-hook) nil
          "*macroexpand-hook* is overwritten from ~a to ~a by some other program. stay alert!"
          'macroexpand-hooks-hook
          *macroexpand-hook*)
  (setf *macroexpand-hook* *old-hook*)
  (makunbound '*old-hook*))


;;; around-hooks


(define-condition in-next-hook () ())

;; none of these variables should be external

(let (ahooks)
  (defun add-around-hook (fname)
    (assert (symbolp fname))
    (push fname ahooks))
  (defun remove-around-hook (fname)
    (removef ahooks fname))
  (defun around-hooks ()
    (copy-list ahooks))
  (defun macroexpand-hooks-hook (macrofn form env)
    (declare (special macrofn form env))
    (let ((ahooks ahooks))
      (declare (special ahooks))
      (call-next-hook)))
  (defun call-next-hook ()
    (declare (special macrofn form env ahooks))
    (restart-case
        ;; in the toplevel, the condition is simply ignored
        (signal 'in-next-hook)
      (continue ()))
    (if ahooks
        (destructuring-bind (first-hook . ahooks) ahooks
          (declare (special ahooks))
          (let (next-hook-called)
            (prog1
              (handler-bind ((in-next-hook (lambda (c)
                                             (setf next-hook-called t)
                                             (continue c))))
                (funcall first-hook macrofn form env))
              (unless next-hook-called
                (error "~a is not calling the next hook through call-next-hook!" first-hook)))))
        (funcall-as-hook macrofn form env))))

(defun funcall-as-hook (macrofn form env)
  (restart-case
      (signal 'in-next-hook)
    (continue ()))
  (funcall macrofn form env))


;;; standard hooks

(lispn:define-namespace before-hooks list)
(lispn:define-namespace after-hooks  list)

(defun standard-hook (macrofn form env)
  (declare (ignorable macrofn))
  (flet ((call-hook (%) (funcall (macro-function % env) form env)))
    (let ((befores (when-let ((hooks (ignore-errors
                                       (symbol-before-hooks
                                        (car form)))))
                     (mapcar #'call-hook hooks)))
          (main (call-next-hook))
          (afters (when-let ((hooks (ignore-errors
                                      (symbol-after-hooks
                                       (car form)))))
                    (mapcar #'call-hook hooks))))
      (if (or (some #'identity befores)
              (some #'identity afters))
          `(progn ,@befores ,main ,@afters)
          main))))

(defun set-standard-hook (name hook &optional (method :before))
  (assert (member method '(:before :after)))
  (if (eq :before method)
      (progn
        (unless (before-hooks-boundp name)
          (setf (symbol-before-hooks name) nil))
        (pushnew hook (symbol-before-hooks name)))
      (progn
        (unless (after-hooks-boundp name)
          (setf (symbol-after-hooks name) nil))
        (pushnew hook (symbol-after-hooks name))))
  hook)

(defmacro define-standard-hook ((name &optional hook-name (method :before)) args &body body)
  (let ((hook-name (or hook-name (gensym "HOOK"))))
    `(progn
       (defmacro ,hook-name ,args ,@body)
       (set-standard-hook ',name ',hook-name ,method))))

(defun remove-standard-hook (name hook method)
  (assert (member method '(:before :after)))
  (if (eq :before method)
      (removef (symbol-before-hooks name) hook)
      (removef (symbol-after-hooks name) hook)))


