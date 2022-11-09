(in-package :cl-user)

(defpackage :bikes.test-asd
  (:use :cl :asdf))
(in-package :bikes.test-asd)

(asdf:defsystem #:bikes.test
  :description "Test bikes"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on (:bikes
               :fiveam)

  :components ((:module "test"
                :components
                ((:file "package")
                 (:file "gear-ratio-tests"))))

  :perform (test-op :after (op c)
                    (eval
                     (read-from-string
                      "(every #'fiveam::TEST-PASSED-P (5am:run :bikes))"))))
