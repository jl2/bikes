;; bikes.asd
;;
;; Copyright (c) 2023 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(asdf:defsystem #:bikes
  :description "A library for modeling bicycles, computing gear ratios, etc."
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license "ISC"
  :source-control "https://github.com/jl2/bikes.git"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "gear-ratios")
               (:file "bikes")))
