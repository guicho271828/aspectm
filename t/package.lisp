#|
  This file is a part of aspectm project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :aspectm.test
  (:use :cl
        :aspectm
        :fiveam
        :alexandria  :lisp-namespace))
(in-package :aspectm.test)



(def-suite :aspectm)
(in-suite :aspectm)

;; run test with (run! test-name) 
;;   test as you like ...

(def-fixture mhook ()
  (enable-macroexpand-hooks)
  (unwind-protect
      (&body)
    (disable-macroexpand-hooks)))

(defun hellwhat (&rest args)
  (apply #'funcall args))

(test enable-disable
  (let (tmp)
    (setf tmp *macroexpand-hook*)
    (unwind-protect
        (progn
          (finishes
            (enable-macroexpand-hooks)
            (setf *macroexpand-hook* 'hellwhat))
          (signals error
            (disable-macroexpand-hooks)))
      (setf *macroexpand-hook* tmp)
      (makunbound 'aspectm::*old-hook*))))


(test (stdhook-setup :depends-on enable-disable)
  (with-fixture mhook ()
    (finishes
      (add-around-hook 'standard-hook)
      (remove-around-hook 'standard-hook))))

(def-fixture std ()
  (unwind-protect
      (progn
        (add-around-hook 'standard-hook)
        (&body))
    (remove-around-hook 'standard-hook)))


(defvar *names* nil)
(defmacro store-name (&whole form name args &body body)
  (declare (ignorable args body))
  (assert (eq (first form) 'defun))
  (push name *names*)
  nil)

(test (stdhook :depends-on stdhook-setup)
  (with-fixture mhook ()
    (with-fixture std ()
      (set-standard-hook 'defun 'store-name :before)
      (let (*names*)
        (finishes (macroexpand '(defun myfunc ())))
        (is (member 'myfunc *names*)))
      (remove-standard-hook 'defun 'store-name :before)
      (is-false (member 'store-name (aspectm::symbol-before-hooks 'defun))))))



(test (define-stdhook :depends-on stdhook-setup)
  (with-fixture mhook ()
    (with-fixture std ()
      (unwind-protect 
          (finishes
            (define-standard-hook (defun store-name2) (&whole form name args &body body)
              (declare (ignorable args body))
              (assert (eq (first form) 'defun))
              (push name *names*)
              nil)
            (let (*names*)
              (eval '(defun myfunc ()))
              (is (member 'myfunc *names*))))
        (remove-standard-hook 'defun 'store-name2 :before)
        (is-false (member 'store-name2 (aspectm::symbol-before-hooks 'defun)))))))


