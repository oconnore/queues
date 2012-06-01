
;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *         A simple, thread safe queue implementation
;;;; *         by Eric O'Connor
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************

(in-package :queues)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :bordeaux-threads))

;;;
;;; Class
;;;

(defclass simple-cqueue (simple-queue)
  ((lock :initform (make-recursive-lock "queue-lock")
	 :accessor lock-of)))

;;;
;;; Methods
;;;

(defmethod make-queue ((type (eql :simple-cqueue)) &key minimum-size copy)
  (make-queue :simple-queue
	      :minimum-size minimum-size
	      :copy copy
	      :class 'simple-cqueue))

(defmethod qpush ((q simple-cqueue) el)
  (with-lock-held ((lock-of q))
    (call-next-method)))

(defmethod qpop ((q simple-cqueue) &optional empty)
  (declare (ignore empty))
  (with-lock-held ((lock-of q))
    (call-next-method)))

(defmethod qtop ((q simple-cqueue) &optional empty)
  (declare (ignore empty))
  (with-lock-held ((lock-of q))
    (call-next-method)))

(defmethod qsize ((q simple-cqueue))
  (with-lock-held ((lock-of q))
    (call-next-method)))

(defmethod qclear ((q simple-cqueue))
  (with-lock-held ((lock-of q))
    (call-next-method)))

(defmethod map-queue (fn (q simple-cqueue))
  (with-lock-held ((lock-of q))
    (call-next-method)))

(defmethod print-queue ((q simple-cqueue)
			&optional (stream *standard-output*))
  (declare (ignore stream))
  (with-lock-held ((lock-of q))
    (call-next-method)))

;;; ==================================================================
;;; EOF
;;; ==================================================================