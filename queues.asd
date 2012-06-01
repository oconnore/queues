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

(quick-defsystem :name queues
		 :desc "A queue interface -- used to create and
manipulate queue structures from simple-queue, priority-queue,
or their concurrent versions (cqueue)."
		 :files (interface))

;;; ==================================================================
;;; EOF
;;; ==================================================================