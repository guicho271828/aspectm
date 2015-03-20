#|
  This file is a part of aspectm project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
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
  :depends-on (:alexandria :trivia :lisp-namespace)
  :components ((:module "src"
                :components
                ((:file "package"))))
  :description ""
  :in-order-to ((test-op (load-op aspectm.test))))
