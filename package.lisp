;; package.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:bikes
  (:use #:cl)
  (:export
   #:gear-count
   #:gear-ratio
   #:gear-range
   #:first-gear
   #:last-gear
   #:show-gear-ratios
   #:front
   #:rear
   #:sp-front
   #:sp-rear
   #:next
   #:prev
   #:one-by
   #:two-by
   #:three-by
   #:rohloff
   #:*all-city*
   #:*carver*
   #:*carver-original*
   #:*fargo-original*
   #:*fargo-co-trail*
   #:*fargo-rohloff*
   #:*fargo-rohloff-next*
   #:*fargo-3*
))

