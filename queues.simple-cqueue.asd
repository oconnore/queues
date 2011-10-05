
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

(quick-defsystem :name queues.simple-cqueue
		 :desc "A thread safe queue"
		 :deps (queues queues.simple-queue bordeaux-threads)
		 :files (cqueue))

;;; ==================================================================
;;; EOF
;;; ==================================================================