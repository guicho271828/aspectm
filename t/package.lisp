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

(test aspectm

  )


