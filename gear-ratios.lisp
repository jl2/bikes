;; bikes.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:bikes)

(defclass group ()
  ((cogs :initform #() :initarg :cogs)))

(defclass chain-rings (group)
  ())

(defclass cassette (group)
  ())

(defclass rohloff ()
  ((cog :initform 16 :initarg :cog)))

(defparameter *wheel-radii*(list (cons :20-in 406)
                                 (cons :24-in 507)
                                 (cons :26-in 559)
                                 (cons :275-in 584)
                                 (cons :29-in 622)
                                 (cons :700c 622)))
(defun wheel-radius (size)
  (assoc-value *wheel-radii* size))

(defclass drivetrain ()
  ((front :initarg :front :accessor front)
   (rear :initarg :rear :accessor rear)
   (crank-length :initarg :crank-length :accessor crank-length)
   (wheel-size :initarg :wheel-size :accessor wheel-size :type (or :20-in :24-in :26-in :275-in :29-in :700c))
   (tire-size :initarg :tire-size :accessor tire-size)))

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
  (declare (ignorable drivetrain))
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

(defun gear-ratios (drivetrain)
  (loop
     for sp = (first-gear drivetrain)
     then (next drivetrain sp)
     while sp
     collect (gear-ratio drivetrain sp)))

(defun gain-ratio (drivetrain shift-position)
  "Return the ratio between the distance travelled by the bike and the distance travelled by a pedal."
  (with-slots (wheel-size tire-size crank-length) drivetrain
    (let* ((total-wheel-radius (+ (wheel-radius wheel-size) tire-size))
           (wheel-crank-ratio (/ total-wheel-radius crank-length))
           (gear-ratio (gear-ratio drivetrain shift-position)))
      (* wheel-crank-ratio gear-ratio))))

(defun single-speed (front rear &key (wheel-size :700c) (crank-length 170) (tire-size 25))
  (make-instance 'drivetrain
                 :front (make-instance 'chain-rings :cogs (make-array 1 :initial-element front))
                 :rear (make-instance 'cassette :cogs (make-array 1 :initial-element rear))
                 :wheel-size wheel-size
                 :tire-size tire-size
                 :crank-length crank-length))

(defun one-by (front  &key (wheel-size :700c) (crank-length 170) (tire-size 50) cassette)
  (make-instance 'drivetrain
                 :front (make-instance 'chain-rings :cogs (make-array 1 :initial-element front))
                 :rear (make-instance 'cassette :cogs (make-array (length cassette) :initial-contents cassette))
                 :wheel-size wheel-size
                 :tire-size tire-size
                 :crank-length crank-length))

(defun two-by (big small &key (wheel-size :700c) (crank-length 170) (tire-size 50) cassette)
  (make-instance 'drivetrain
                 :front (make-instance 'chain-rings :cogs (make-array 2 :initial-contents (list big small)))
                 :rear (make-instance 'cassette :cogs (make-array (length cassette) :initial-contents cassette))
                 :wheel-size wheel-size
                 :tire-size tire-size
                 :crank-length crank-length))

(defun three-by (big middle small &key (wheel-size :700c) (crank-length 170) (tire-size 50) cassette)
  (make-instance 'drivetrain
                 :front (make-instance 'chain-rings :cogs (make-array 3 :initial-contents (list big middle small)))
                 :rear (make-instance 'cassette :cogs (make-array (length cassette) :initial-contents cassette))
                 :wheel-size wheel-size
                 :tire-size tire-size
                 :crank-length crank-length))

(defun rohloff (chainring cog &key (wheel-size :700c) (crank-length 170) (tire-size 50))
  (make-instance 'drivetrain
                 :front (make-instance 'chain-rings :cogs (make-array 1 :initial-element chainring))
                 :rear (make-instance 'rohloff :cog cog)
                 :wheel-size wheel-size
                 :tire-size tire-size
                 :crank-length crank-length))

(defun two-by-rohloff (ring-1 ring-2 cog &key (wheel-size :700c) (crank-length 170) (tire-size 50))
  (make-instance 'drivetrain
                 :front (make-instance 'chain-rings :cogs (make-array 2 :initial-contents (list ring-1 ring-2)))
                 :rear (make-instance 'rohloff :cog cog)
                 :wheel-size wheel-size
                 :tire-size tire-size
                 :crank-length crank-length))

(defparameter *all-city* (single-speed 42 18))

(defparameter *carver* (one-by 50 :cassette '(36 32 28 25 22 19 17 15 13 11)))

(defparameter *carver-original* (two-by 50 34 :cassette '( 36 32 28 25 22 19 17 15 13 11)))

(defparameter *fargo-original* (one-by 36 :cassette '( 42 36 32 28 25 22 19 17 15 13 11) :crank-length 170))
(defparameter *fargo-co-trail* (one-by 34 :cassette '( 42 36 32 28 25 22 19 17 15 13 11) :crank-length 170))
(defparameter *fargo-rohloff-original* (rohloff 32 16 :crank-length 170))
(defparameter *fargo-rohloff* (rohloff 36 16 :crank-length 170))
(defparameter *fargo-rohloff-38* (rohloff 38 16 :crank-length 170))
(defparameter *fargo-rohloff-two-by* (two-by-rohloff 34 44 16 :crank-length 170))
