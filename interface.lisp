
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
  (:nicknames q))

(in-package :queues)

;;;
;;; Interface generics
;;;

;; General
(defgeneric qpush (queue element))
(defgeneric qtop (queue))
(defgeneric qpop (queue))
(defgeneric qsize (queue))
(defgeneric qclear (queue))
(defgeneric map-queue (fn queue))
(defgeneric print-queue (queue))
;; Priority queue
(defgeneric queue-merge (queue-1 queue-2))
(defgeneric queue-merge-safe (queue-1 queue-2))
(defgeneric queue-find (queue predicate-or-key))
(defgeneric queue-change (queue node new-value))
(defgeneric queue-delete (queue node))
(defgeneric queue-comparison (queue))

;;;
;;; Make queue implementation
;;;

(defun make-queue (&key (type 'simple-queue) compare
		   minimum-size copy)
  (case type
    (simple-queue
     (make-simple-queue :minimum-size minimum-size))
    (priority-queue
     (make-priority-queue :copy copy :compare compare))))