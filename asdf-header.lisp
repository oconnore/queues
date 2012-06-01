
;;;
;;; A package definition and a really quick macro for asdf files
;;;

(in-package :cl-user)

(defpackage queues-system
    (:use :cl :asdf))

(in-package :queues-system)

;;;
;;; Variables
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *author* "Eric O'Connor")
  (defparameter *version* "1.0.0")
  (defparameter *license* "MIT"))

;;;
;;; Definition macro
;;;

(defmacro quick-defsystem (&key name deps (version *version*) desc files)
  (flet ((str (x)
	   (string-downcase (symbol-name x))))
    `(defsystem ,name
       :name ,(str name)
       :author ,*author*
       :maintainer ,*author*
       :description ,desc
       :version ,version
       :license ,*license*
       ,@(when deps
	       `(:depends-on ,deps))
       :serial t
       :components
       ,(loop for file in files collect
	     `(:file ,(str file))))))

;;; ==================================================================
;;; EOF
;;; ==================================================================