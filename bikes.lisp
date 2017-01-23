;;;; bikes.lisp
;;;;
;;;; Copyright (c) 2016 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

;; A simple framework for computing gear ratios on different bike configurations.

(in-package #:bikes)

(defstruct bike-config 
  (chain-rings '(42) :type list)
  (cassette '(20) :type list)
  (crank-length 175.0 :type float))

(defun make-one-by (cr &rest cassette)
  (make-bike-config :chain-rings (list cr) :cassette cassette))

(defun make-ss (cr cog)
  (make-bike-config :chain-rings (list cr) :cassette (list cog)))

(defun compute-ratios (config)
  (let ((results nil))
    (loop for cr in (bike-config-chain-rings config)
       do (loop for cog in (bike-config-cassette config)
             do
               (push (list (/ cr cog 1.0) cr cog) results)))
    results))

(defun show-gear-ratios (config)
  (let* ((ratios (compute-ratios config))
         (previous (car (car ratios))))
    (format t   "~9a x ~3a = ~5a | ~5a~%" "Chainring" "Cog" "Ratio" "Gap")
    (dolist (gr ratios)
      (format t "~9d x ~3d = ~5f | ~5f~%" (cadr gr) (caddr gr) (car gr) (- previous (car gr)))
      (setf previous (car gr)))))


(defun compare-gear-ratios (config1 config2)
  (let ((c1-ratios (compute-ratios config1))
        (c2-ratios (compute-ratios config2)))
    (format t "~a~%~a~%" c1-ratios c2-ratios)))

(defparameter *all-city* (make-bike-config :chain-rings '(42) :cassette '(18)))

(defparameter *carver* (make-bike-config :chain-rings '(50) :cassette '(36 32 28 25 22 19 17 15 13 11)))

(defparameter *carver-original* (make-bike-config :chain-rings '(50 34) :cassette '(36 32 28 25 22 19 17 15 13 11)))
