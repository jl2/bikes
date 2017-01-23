;;;; bikes.asd
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:bikes
  :description "A simple framework for computing gear ratios on different bike configurations."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC"
  :serial t
  :components ((:file "package")
               (:file "bikes")))

