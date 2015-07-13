#|
  This file is a part of aspectm project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage aspectm
  (:use :cl :alexandria)
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
   #:around-hooks
   #:with-standard-hook))
(in-package :aspectm)

;;; enabling hooks

(defvar *aspectm-lock* (bt:make-lock "Aspectm Lock"))
(defvar *old-hook*)
(defvar *pathnames* (make-hash-table :test 'equal)
  "hash-table of pathnames as keys and booleans as values. hmmmmm")
(defmacro enable-macroexpand-hooks ()
  `(progn
     (eval-when (:compile-toplevel)
       (when (null *compile-file-pathname*)
         (warn "Why *compile-file-pathname* is nil in compilation environment?"))
         (%enable-macroexpand-hooks))
     (eval-when (:load-toplevel :execute)
       (warn "ENABLE-MACROEXPAND-HOOKS does not take effect outside COMPILATION-ENVIRONMENT."))))

(defmacro disable-macroexpand-hooks ()
  `(progn
     (eval-when (:compile-toplevel)
       (when (null *compile-file-pathname*)
         (warn "Why *compile-file-pathname* is nil in compilation environment?"))
       (%disable-macroexpand-hooks))
     (eval-when (:load-toplevel :execute)
       (warn "DISABLE-MACROEXPAND-HOOKS does not take effect outside COMPILATION-ENVIRONMENT."))))

(defun %enable-macroexpand-hooks ()
  (bt:with-lock-held (*aspectm-lock*)
    (psetf *macroexpand-hook* 'macroexpand-hooks-hook
           *old-hook* *macroexpand-hook*
           (gethash *compile-file-pathname* *pathnames*) t)))

(defun %disable-macroexpand-hooks ()
       (bt:with-lock-held (*aspectm-lock*)
         (assert (eq *macroexpand-hook* 'macroexpand-hooks-hook) nil
                 "*macroexpand-hook* is overwritten from ~a to ~a by some other program.
 Compilation result of this file is INVALID. Stay alert!"
                 'macroexpand-hooks-hook
                 *macroexpand-hook*)
         (setf *macroexpand-hook* *old-hook*
               (gethash *compile-file-pathname* *pathnames*) nil)
         (makunbound '*old-hook*)))

;;; around-hooks


(define-condition in-next-hook () ())

;; none of these variables should be external

(let (ahooks)
  (defun add-around-hook (fname)
    (assert (symbolp fname))
    (with-lock-held (*aspectm-lock*)
      (push fname ahooks)))
  (defun remove-around-hook (fname)
    (with-lock-held (*aspectm-lock*)
      (removef ahooks fname)))
  (defun around-hooks ()
    "Returns a copy of around-hooks as a flesh list. It is safe to modify this value."
    (copy-list ahooks))
  (defun macroexpand-hooks-hook (macrofn form env)
    (declare (special macrofn form env))
    (if (and *compile-file-pathname*
             (gethash *compile-file-pathname* *pathnames*))
        (let ((ahooks ahooks))
          (declare (special ahooks))
          (call-next-hook))
        (funcall-as-hook macrofn form env)))
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
            (unwind-protect
                (handler-bind ((in-next-hook (lambda (c)
                                               (setf next-hook-called t)
                                               (continue c))))
                  ;; first-hook should also call call-next-hook
                  (funcall first-hook macrofn form env))
              (unless next-hook-called
                (error "~a is not calling the next hook through call-next-hook!" first-hook)))))
        (funcall-as-hook macrofn form env))))

(defun funcall-as-hook (macrofn form env)
  (restart-case
      (signal 'in-next-hook)
    (continue ()))
  (funcall *old-hook* macrofn form env))


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
          ;; to stop messing up the compilation result of no hooks are present
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

(defmacro with-standard-hook (name hook method &body body)
  (unwind-protect
      (progn
        (set-standard-hook name hook method)
        (macroexpand `(progn ,@body)))
    (remove-standard-hook name hook method)))

