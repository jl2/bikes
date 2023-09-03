;; package.lisp
;;
;; Copyright (c) 2023 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(defpackage #:bikes
  (:use #:cl #:alexandria)
  (:export
   #:gear-count
   #:gear-ratio
   #:gear-range
   #:gain-ratio
   #:first-gear
   #:last-gear
   #:gear-ratios
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
   #:*carver-new*
   #:*carver-three*
   #:*fargo-original*
   #:*fargo-co-trail*
   #:*fargo-rohloff-original*
   #:*fargo-rohloff*
   #:*fargo-rohloff-38*
   #:*fargo-rohloff-two-by*
))
