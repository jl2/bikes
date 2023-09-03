;; bikes.lisp
;; Copyright (c) 2023 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; A small library for computing gear ratios on different bike configurations.

(in-package #:bikes)

;; TODO: Put these in a database or a file.
(defparameter *all-city* (single-speed 42 18))

(defparameter *carver-three* (one-by 36
                               :cassette '(36 32 28 25 22 19 17 15 13 11)))

(defparameter *carver* (one-by 50
                               :cassette '(36 32 28 25 22 19 17 15 13 11)))


(defparameter *carver-original* (two-by 50 34
                                        :cassette '( 36 32 28 25 22 19 17 15 13 11)))

(defparameter *carver-new* (two-by 46 30
                                   :cassette '(34 30 27 25 23 21 19 17 15 13 11)))

(defparameter *fargo-original* (one-by 36
                                       :cassette '( 42 36 32 28 25 22 19 17 15 13 11)
                                       :crank-length 170))

(defparameter *fargo-co-trail* (one-by 34
                                       :cassette '( 42 36 32 28 25 22 19 17 15 13 11)
                                       :crank-length 170))

(defparameter *fargo-rohloff-original* (rohloff 32 16
                                                :crank-length 170))

(defparameter *fargo-rohloff* (rohloff 36 16
                                       :crank-length 170))

(defparameter *fargo-rohloff-38* (rohloff 38 16
                                          :crank-length 170))

(defparameter *fargo-rohloff-two-by* (two-by-rohloff 34 44 16
                                                     :crank-length 170))
