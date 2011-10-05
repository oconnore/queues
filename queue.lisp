
;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *         A simple queue implementation
;;;; *         by Eric O'Connor
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************

(in-package :queues)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(simple-queue
            qsize
            qpush
            qpop
            qtop)))

;;;
;;; Parameters
;;;

(defparameter *minimum-size* 25)

;;;
;;; Data structures
;;;

(defclass simple-queue ()
  ((elements :initform (make-array *minimum-size*
				   :adjustable t
				   :initial-element nil)
	     :accessor elements-of)
   (start :initform 0 :accessor start-of)
   (size :initform 0 :accessor size-of)
   (min-size :initform *minimum-size*
	     :accessor min-size-of)))

;;; ------------------------------------------------------------------

(defmethod print-object ((q simple-queue) stream)
  (print-unreadable-object (q stream :type t)
    (format stream "~A [~A]"
	    (when (plusp (size-of q))
	      (aref (elements-of q) (start-of q)))
            (size-of q))))

;;;
;;; Functions
;;;

(defun make-simple-queue (&key minimum-size)
  (let ((*minimum-size* (or minimum-size
			    *minimum-size*)))
    (make-instance 'simple-queue)))

;;; ------------------------------------------------------------------

(defmethod qsize ((q simple-queue))
  (size-of q))

;;; ------------------------------------------------------------------

(defmethod qclear ((queue simple-queue))
  (loop for x from 0 below (length (elements-of queue))
     do (setf (aref (elements-of queue) x) nil))
  (setf (size-of queue) 0
	(start-of queue) 0))

;;; ------------------------------------------------------------------

(defun %queue-last (queue)
  (mod (+ (start-of queue)
	  (size-of queue))
       (length (elements-of queue))))

;;; ------------------------------------------------------------------

(defmethod qpush ((queue simple-queue) element)
  (let* ((vector (elements-of queue))
	 (vec-size (length vector))
	 (start (start-of queue)))
    (cond ((< (size-of queue) vec-size) t)
	  (t
	   (let ((new-size (* 2 vec-size)))
	     ;; Setf not really necessary, but this protects
	     ;; if someone manually creates an elements vector
	     ;; that was not declared as adjustable
	     (setf (elements-of queue)
		   (adjust-array vector new-size :initial-element nil))
	     (cond ((< start (floor vec-size 2))
		    ;; The head of the queue is within the first half
		    ;; of the vector, concatenate the tail
		    (loop
		       for x from 0 below start
		       for y from vec-size
		       do (setf (aref vector y) (aref vector x)
				(aref vector x) nil)))
		   (t
		    ;; The head of the queue is in the second half
		    ;; of the vector, shift the head to the end
		    ;; of the new vector
		    (loop
		       for x from start below vec-size
		       for y from (- new-size (- vec-size start))
		       do (setf (aref vector y) (aref vector x)
				(aref vector x) nil)))))))
    (setf (aref vector (%queue-last queue))
	  element)
    (incf (size-of queue))
    element))

;;; ------------------------------------------------------------------

(defmethod qpop ((queue simple-queue) &optional empty-value)
  (let ((ret (qtop queue))
	(vector (elements-of queue)))
    (unless ret
      (return-from qpop empty-value))
    (setf (aref vector (start-of queue)) nil)
    (decf (size-of queue))
    (setf (start-of queue) (mod (1+ (start-of queue))
				(length vector)))
    ;; Reset?
    (let ((len (length vector)))
      (cond ((zerop (size-of queue))
	     (setf (start-of queue) 0))
	    ((and (> len (queue-min-size queue))
		  (< (size-of queue) (floor len 3)))
	     ;; Queue should be downsized
	     (let* ((min (queue-min-size queue))
		    (new-size
		     (* min (expt 2 (ceiling
				     (log (ceiling (size-of queue) min) 2))))))
	       (cond ((> (+ (start-of queue) (size-of queue)) len)
		      ;; Queue wraps around the end of the vector,
		      ;; move the last half into the resized vector
		      (loop
			 with move-start = (- new-size (- len (start-of queue)))
			 for x from (start-of queue) below len
			 for y from move-start
			 do (setf (aref vector y) (aref vector x)
				  (aref vector x) nil)
			 finally (setf (start-of queue) move-start)))
		     (t
		      ;; Queue is contained in the vector, but overflows
		      ;; the downsized vector, so shift the overflow section into
		      ;; the new vector (possibly the entire queue)
		      (loop
			 for x from (max (start-of queue) new-size)
			 below (+ (start-of queue)
				  (size-of queue))
			 for z from 0
			 do (setf (aref vector z) (aref vector x)))
		      (unless (< (start-of queue) new-size)
			(setf (start-of queue) 0))))
	       (adjust-array (elements-of queue) new-size
			     :initial-element nil)))))
    (values ret t)))

;;; ------------------------------------------------------------------

(defmethod qtop ((queue simple-queue))
  (when (plusp (size-of queue))
    (values (aref (elements-of queue) (start-of queue))
	    t)))

;;; ------------------------------------------------------------------

(defmethod map-queue (fn (queue simple-queue))
  (let* ((vec (elements-of queue))
	 (len (length vec)))
    (flet ((pos (x)
	     (mod x len)))
      (loop for i from (start-of queue)
	 below (+ (start-of queue)
		  (size-of queue))
	 do (funcall fn (aref vec (pos i)))))))

;;; ------------------------------------------------------------------

(defmethod print-queue ((queue simple-queue)
			&optional (stream *standard-output*))
  (format stream "=== ~A ===~%" queue)
  (map-queue (lambda (x)
	       (format stream "~4T~A~%" x))
	     queue))

;;; ==================================================================
;;; EOF
;;; ==================================================================