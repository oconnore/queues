
;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *         A thread safe fibonacci queue implementation
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

(defclass priority-cqueue (priority-queue)
  ((lock :initform (make-recursive-lock "queue-lock")
	 :accessor lock-of)))

;;;
;;; Methods
;;;

(defun make-priority-cqueue (&key compare copy)
  (make-priority-queue :compare compare
		       :copy copy
		       :class 'priority-cqueue))

(defmethod qpush ((q priority-cqueue) el)
  (with-lock-held ((lock-of q))
    (call-next-method)))

(defmethod qpop ((q priority-cqueue) &optional empty)
  (declare (ignore empty))
  (with-lock-held ((lock-of q))
    (call-next-method)))

(defmethod qtop ((q priority-cqueue) &optional empty)
  (declare (ignore empty))
  (with-lock-held ((lock-of q))
    (call-next-method)))

(defmethod qsize ((q priority-cqueue))
  (with-lock-held ((lock-of q))
    (call-next-method)))

(defmethod qclear ((q priority-cqueue))
  (with-lock-held ((lock-of q))
    (call-next-method)))

(defmethod map-queue (fn (q priority-cqueue))
  (with-lock-held ((lock-of q))
    (call-next-method)))

(defmethod print-queue ((q priority-cqueue)
			&optional (stream *standard-output*))
  (declare (ignore stream))
  (with-lock-held ((lock-of q))
    (call-next-method)))

;;;
;;; Priority Queue-only methods
;;;

(defmethod queue-merge ((q1 priority-cqueue)
			(q2 priority-cqueue))
  (with-lock-held ((lock-of q1))
    (with-lock-held ((lock-of q2))
      (call-next-method))))

(defmethod queue-merge-safe ((q1 priority-cqueue)
			     (q2 priority-cqueue))
  (with-lock-held ((lock-of q1))
    (with-lock-held ((lock-of q2))
      (call-next-method))))

(defmethod queue-find ((q priority-cqueue) predicate-or-key)
  (with-lock-held ((lock-of q))
    (call-next-method)))

(defmethod queue-change ((q priority-cqueue) node new-value)
  (with-lock-held ((lock-of q))
    (call-next-method)))

(defmethod queue-delete ((q priority-cqueue) node)
  (with-lock-held ((lock-of q))
    (call-next-method)))

(defmethod node-active-p ((q priority-cqueue) node)
  (with-lock-held ((lock-of q))
    (call-next-method)))

(defmethod queue-comparison ((q priority-cqueue))
  "No need for synchronization, since the comparison is constant"
  (call-next-method))

;;; ==================================================================
;;; EOF
;;; ==================================================================