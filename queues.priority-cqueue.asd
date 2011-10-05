
;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *         A package of queue data structures
;;;; *         by Eric O'Connor
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************

(in-package :cl-user)
(load (merge-pathnames 
       (make-pathname :name "asdf-header"
		      :type "lisp")
       *load-truename*))
(in-package :queues-system)

;;; ------------------------------------------------------------------

(quick-defsystem :name queues.priority-cqueue
		 :desc "A thread safe fibonacci priority queue"
		 :deps (queues queues.priority-queue bordeaux-threads)
		 :files (priority-cqueue))

;;; ==================================================================
;;; EOF
;;; ==================================================================