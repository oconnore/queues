;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *         A simple queue implementation -- Interface definition
;;;; *         by Eric O'Connor
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************

(in-package :cl-user)

(defpackage queues
  (:use cl)
  (:nicknames q)
  (:export simple-queue
	   simple-cqueue
	   priority-queue
	   priority-cqueue
	   ;; general interface
	   make-queue
	   qsize
	   qclear
	   qpush
	   qpop
	   qtop
	   map-queue
	   print-queue
	   ;; priority queues
	   queue-comparison
	   queue-merge
	   queue-merge-safe
	   queue-change
	   queue-delete
	   queue-find
	   ;; miscellaneous
	   *current-queue-node*
	   queue-node-p
	   node-active-p))

(in-package :queues)

;;;
;;; Interface generics
;;;

;; General

(defgeneric make-queue (type &key &allow-other-keys)
  (:documentation "
Creates a queue of type (:simple-queue :simple-cqueue
:priority-queue :priority-cqueue). *-cqueue denotes
the threadsafe version of the datastructure.

The allowed arguments are :minimum-size and :copy for
simple-queues, and :compare and :copy for priority
queues. Minimum size denotes the smallest size of the
underlying vector. Copy will accept elements from
another queue of the same type. Compare accepts a
binary relation such as #'<"))

(defgeneric qpush (queue element))
(defgeneric qtop (queue &optional empty))
(defgeneric qpop (queue &optional empty))
(defgeneric qsize (queue))
(defgeneric qclear (queue))
(defgeneric map-queue (fn queue))
(defgeneric print-queue (queue &optional stream))
;; Priority queue
(defgeneric node-active-p (queue node))
(defgeneric queue-merge (queue-1 queue-2))
(defgeneric queue-merge-safe (queue-1 queue-2))
(defgeneric queue-find (queue predicate-or-key))
(defgeneric queue-change (queue node new-value))
(defgeneric queue-delete (queue node))
(defgeneric queue-comparison (queue))
;; Autoload
(defmethod make-queue (type &rest args)
  (let ((valid '((simple-queue :queues.simple-queue)
		 (simple-cqueue :queues.simple-cqueue)
		 (priority-queue :queues.priority-queue)
		 (priority-cqueue :queues.priority-cqueue))))
    (let ((value (assoc type valid :test #'string-equal)))
      (when (and value (not (find-class (car value) nil)))
	(restart-case 
	    (error (format nil "~A is not loaded" (car value)))
	  (attempt-require (&optional v)
	    :test (lambda (x) (declare (ignore x)) t)
	    :report (lambda (s)
		      (format s "Attempt to load ~A" (cadr value)))
	    (declare (ignore v))
	    (require (cadr value))
	    (return-from make-queue
	      (apply #'make-queue (cons type args))))))
      (call-next-method))))

;;; ==================================================================
;;; EOF
;;; ==================================================================