#|
  This file is a part of aspectm project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  aspect-oriented macroexpand hooks for common lisp

  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage aspectm-asd
  (:use :cl :asdf))
(in-package :aspectm-asd)


(defsystem aspectm
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:alexandria  :lisp-namespace)
  :components ((:module "src"
                :components
                ((:file "package"))))
  :description "aspect-oriented macroexpand hooks for common lisp"
  :in-order-to ((test-op (load-op :aspectm.test))))
