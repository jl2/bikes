;; bikes.asd
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:bikes
  :description "A small library for computing gear ratios on different bike drivetrains."
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license "ISC"
  :serial t
  :components ((:file "package")
               (:file "bikes")))

