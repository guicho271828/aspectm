#|
  This file is a part of aspectm project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage aspectm.test-asd
  (:use :cl :asdf))
(in-package :aspectm.test-asd)


(defsystem aspectm.test
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:aspectm
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (load-op :after (op c) ))
