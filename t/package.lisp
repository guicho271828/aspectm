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

(defun test-compile (relative-name)
  (compile-file
   (asdf:system-relative-pathname
    :aspectm.test relative-name)))

(defun hellwhat (&rest args)
  (apply #'funcall args))

(test enable-disable
  (let ((*macroexpand-hook* *macroexpand-hook*)
        (aspectm::*old-hook* aspectm::*old-hook*)
        (*compile-file-pathname* #p"dummy"))
    (do-enable-macroexpand-hooks)
    (setf *macroexpand-hook* 'hellwhat)
    (signals error
      (do-disable-macroexpand-hooks))))

(test (stdhook-setup :depends-on enable-disable)
  (let ((*macroexpand-hook* *macroexpand-hook*)
        (aspectm::*old-hook* aspectm::*old-hook*)
        (*compile-file-pathname* #p"dummy"))
    (finishes
      (do-enable-macroexpand-hooks)
      (add-around-hook 'standard-hook)
      (remove-around-hook 'standard-hook)
      (do-disable-macroexpand-hooks))))

(def-fixture std ()
  (let ((*macroexpand-hook* *macroexpand-hook*)
        (aspectm::*old-hook* aspectm::*old-hook*)
        (*compile-file-pathname* #p"dummy"))
    (finishes
      (do-enable-macroexpand-hooks)
      (add-around-hook 'standard-hook)
      (&body)
      (remove-around-hook 'standard-hook)
      (do-disable-macroexpand-hooks))))


(defvar *names* nil)
(defmacro store-name (&whole form name args &body body)
  (declare (ignorable args body))
  (assert (eq (first form) 'defun))
  `(push ',name *names*))

(test (stdhook :depends-on stdhook-setup)
  (with-fixture std ()
    (set-standard-hook 'defun 'store-name :before)
    (let (*names*)
      (finishes (macroexpand '(defun myfunc ())))
      (is-false (member 'myfunc *names*))
      (finishes (eval '(defun myfunc ())))
      (is-true (member 'myfunc *names*)))
    (remove-standard-hook 'defun 'store-name :before)
    (is-false (member 'store-name (aspectm::symbol-before-hooks 'defun)))))



(test (define-stdhook :depends-on stdhook-setup)
  (with-fixture std ()
    (unwind-protect 
        (finishes
          (define-standard-hook (defun store-name2) (&whole form name args &body body)
            (declare (ignorable args body))
            (assert (eq (first form) 'defun))
            `(push ',name *names*))
          (let (*names*)
            (finishes (macroexpand '(defun myfunc2 ())))
            (is-false (member 'myfunc2 *names*))
            (finishes (eval '(defun myfunc2 ())))
            (is-true (member 'myfunc2 *names*))))
      (remove-standard-hook 'defun 'store-name2 :before)
      (is-false (member 'store-name2 (aspectm::symbol-before-hooks 'defun))))))


