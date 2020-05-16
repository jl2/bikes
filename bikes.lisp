;; bikes.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

;; A small library for computing gear ratios on different bike configurations.

(in-package #:bikes)

(defclass group ()
  ((cogs :initform #() :initarg :cogs)))

(defclass chain-rings (group)
  ())

(defclass cassette (group)
  ())

(defclass rohloff ()
  ((cog :initform 16 :initarg :cog)))

(defclass drivetrain ()
  ((front :initarg :front :accessor front)
   (rear :initarg :rear :accessor rear)))

(defclass shift-position ()
  ((front :initarg :front :accessor sp-front)
   (rear :initarg :rear :accessor sp-rear)))

(defun next (drivetrain position)
  (with-slots (front rear) drivetrain
    (next-shift-position front rear position)))

(defun prev (drivetrain position)
  (with-slots (front rear) drivetrain
    (prev-shift-position front rear position)))

(defgeneric next-shift-position (front rear position)
  (:documentation "Return the next shift position."))

(defmethod next-shift-position (front rear position)
  (with-slots ((front-pos front) (rear-pos rear)) position
    (cond
      ;; Move to next cog on rear
      ((< (1+ rear-pos) (gear-count rear))
       (make-instance 'shift-position :front front-pos :rear (1+ rear-pos)))

      ;; Move to next cog on front, first gear on rear
      ((and (= (gear-count rear) (1+ rear-pos))
            (< (1+ front-pos) (gear-count front)))
       (make-instance 'shift-position :front (1+ front-pos) :rear 0))

      ;; No more gears
      (t
       nil))))

(defmethod prev-shift-position (front rear position)
  (with-slots ((front-pos front) (rear-pos rear)) position
    (cond
      ;; Move to previous cog on rear
      ((>= (1- rear-pos) 0)
       (make-instance 'shift-position :front front-pos :rear (1- rear-pos)))

      ;; Move to next cog on front, first gear on rear
      ((and (= rear-pos 0)
            (> (gear-count front) 0))
       (make-instance 'shift-position :front (1- front-pos) :rear (1- (gear-count rear))))

      ;; No more gears
      (t
       nil))))

(defgeneric gear-count (cassette)
  (:documentation "Return the number of cogs."))

(defmethod gear-count ((drivetrain drivetrain))
  (with-slots (front rear) drivetrain
    (* (gear-count front)
       (gear-count rear))))

(defmethod gear-count ((group group))
  (with-slots (cogs) group
    (length cogs)))

(defmethod gear-count ((cassette rohloff))
  14)

(defgeneric first-gear (drivetrain))
(defgeneric last-gear (drivetrain))

(defmethod first-gear (drivetrain)
  (make-instance 'shift-position :front 0 :rear 0))

(defmethod last-gear (drivetrain)
  (with-slots (front rear) drivetrain
    (make-instance 'shift-position :front (1- (gear-count front)) :rear (1- (gear-count rear)))))

(defgeneric tooth-count (group idx)
  (:documentation "Return the tooth count of the cog at idx."))

(defmethod tooth-count ((group group) idx)
  (aref (slot-value group 'cogs) idx))

(defmethod tooth-count ((rohloff rohloff) idx)
  (declare (ignorable idx))
  (slot-value rohloff 'cog))


(defgeneric compute-gear-ratio (front rear shift-position)
  (:documentation "Compute the gear ratio of front and rear shift position."))

(defmethod compute-gear-ratio ((chain-ring group) (cassette group) shift-position)
  (with-slots (front rear) shift-position
    (/ (tooth-count chain-ring front)
       (tooth-count cassette rear)
       1.0)))

(defparameter *rohloff-inner-ratios*
  #(0.279 0.316 0.360 0.409 0.464 0.528 0.600 0.682 0.774 0.881 1.000 1.135 1.292 1.467))

(defmethod compute-gear-ratio ((chain-ring group) (rohloff rohloff) shift-position)
  (with-slots (front rear) shift-position
    (* (/ (tooth-count chain-ring front)
          (tooth-count rohloff rear)
          1.0)
       (aref *rohloff-inner-ratios* rear))))

(defun gear-ratio (drivetrain shift-position)
  (with-slots (front rear) drivetrain
    (compute-gear-ratio front rear shift-position)))

(defun gear-range (drivetrain)
  (/ (gear-ratio drivetrain (last-gear drivetrain))
     (gear-ratio drivetrain (first-gear drivetrain))))

(defun show-gear-ratios (drivetrain)
  (loop
     for sp = (first-gear drivetrain) then (next drivetrain sp)
     while sp
     do
       (format t "Gear ratio: ~a~%" (gear-ratio drivetrain sp))))

(defun single-speed (front rear)
  (make-instance 'drivetrain
                 :front (make-instance 'chain-rings :cogs (make-array 1 :initial-element front))
                 :rear (make-instance 'cassette :cogs (make-array 1 :initial-element rear))))

(defun one-by (front &rest cassette)
  (make-instance 'drivetrain
                 :front (make-instance 'chain-rings :cogs (make-array 1 :initial-element front))
                 :rear (make-instance 'cassette :cogs (make-array (length cassette) :initial-contents cassette))))

(defun two-by (big small &rest cassette)
  (make-instance 'drivetrain
                 :front (make-instance 'chain-rings :cogs (make-array 2 :initial-contents (list big small)))
                 :rear (make-instance 'cassette :cogs (make-array (length cassette) :initial-contents cassette))))

(defun three-by (big middle small &rest cassette)
  (make-instance 'drivetrain
                 :front (make-instance 'chain-rings :cogs (make-array 3 :initial-contents (list big middle small)))
                 :rear (make-instance 'cassette :cogs (make-array (length cassette) :initial-contents cassette))))

(defun rohloff (chainring cog)
  (make-instance 'drivetrain
                 :front (make-instance 'chain-rings :cogs (make-array 1 :initial-element chainring))
                 :rear (make-instance 'rohloff :cog cog)))

(defun two-by-rohloff (ring-1 ring-2 cog)
  (make-instance 'drivetrain
                 :front (make-instance 'chain-rings :cogs (make-array 2 :initial-contents (list ring-1 ring-2)))
                 :rear (make-instance 'rohloff :cog cog)))
;; (defun compare-gear-ratios (config1 config2)
;;   (let ((c1-ratios (compute-ratios config1))
;;         (c2-ratios (compute-ratios config2)))
;;     (format t "~a~%~a~%" c1-ratios c2-ratios)))

(defparameter *all-city* (single-speed 42 18))

(defparameter *carver* (one-by 50 36 32 28 25 22 19 17 15 13 11))

(defparameter *carver-original* (two-by 50 34 36 32 28 25 22 19 17 15 13 11))

(defparameter *fargo-original* (one-by 36 42 36 32 28 25 22 19 17 15 13 11))
(defparameter *fargo-co-trail* (one-by 34 42 36 32 28 25 22 19 17 15 13 11))
(defparameter *fargo-rohloff* (rohloff 34 16))
(defparameter *fargo-rohloff-next* (rohloff 38 16))
(defparameter *fargo-3* (two-by-rohloff 38 44 16))
