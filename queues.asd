
;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *         A package of queue data structures
;;;; *         by Eric O'Connor
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************

(in-package :cl-user)

(defpackage queues-system
    (:use :cl :asdf))

(in-package :queues-system)

;;;
;;; Variables
;;;

(defparameter *author* "Eric O'Connor")
(defparameter *version* "1")
(defparameter *license* "MIT")

;;;
;;; Definition macro
;;;

(defmacro quick-defsystem (&key name deps (version '*version*) desc files)
  (flet ((str (x)
	   (string-downcase (symbol-name x))))
    `(defsystem ,name
       :name ,(str name)
       :author *author*
       :maintainer *author*
       :description ,desc
       :version ,version
       :license *license*
       ,@(when deps
	       `(:depends-on ,deps))
       :serial t
       :components
       ,(loop for file in files collect
	     `(:file ,(str file))))))

;;;
;;; Queue systems - queue, priority-queue, cqueue, priority-cqueue
;;;

(quick-defsystem :name queues
		 :desc "A queue interface -- used to create and
manipulate queue structures from simple-queue, priority-queue,
or their concurrent versions (cqueue)."
		 :files (package queues-interface))

;;; ------------------------------------------------------------------

(quick-defsystem :name queues.simple-queue
		 :desc "A simple queue implementation"
		 :files (q-package queue))

;;; ------------------------------------------------------------------

(quick-defsystem :name queues.priority-queue
		 :desc "A priority queue (fibonacci) implementation"
		 :files (pq-package priority-queue))

;;; ------------------------------------------------------------------
#+not-yet
(quick-defsystem :name queues.cqueue
		 :desc "A thread safe queue -- based on simple-queue"
		 :deps (bordeaux-threads)
		 :files (cq-package cqueue))

;;; ------------------------------------------------------------------

#+not-yet
(quick-defsystem :name queues.priority-cqueue
		 :desc "A thread safe priority queue -- based on priority-queue"
		 :deps (bordeaux-threads)
		 :files (cpq-package))

;;; ==================================================================
;;; EOF
;;; ==================================================================