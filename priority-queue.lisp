
;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *         A fibonacci heap/queue implementation
;;;; *         by Eric O'Connor
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************

(in-package :queues)

;;; ---------------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(priority-queue
            make-priority-queue
            qsize
            queue-comparison
            print-queue
            *current-queue-node*
            map-queue
            queue-node-p
            qpush
            qtop
            qpop
	    qclear
            queue-merge
            queue-merge-safe
	    queue-change
            queue-delete
            queue-find)))

;;;
;;; Data structures
;;;

(defstruct (node
             (:predicate queue-node-p)
             :named)
  left right value parent child degree mark)

(defmethod print-object ((n node) s)
  (print-unreadable-object (n s :type t)
    (format s "~A" (node-value n))))

;;; ---------------------------------------------------------------------------

(defclass priority-queue ()
  ((min-node :type (or node null)
             :initform nil
             :accessor min-node-of)
   (size :type fixnum
         :initform 0
         :accessor queue-size)
   (compare :type function
            :initform #'<
            :initarg :compare
            :accessor queue-comparison)))

(defmethod print-object ((x queue) s)
  (print-unreadable-object (x s :type t :identity t)
    (format s "~A [~A]"
            (when (min-node-of x)
              (node-value (min-node-of x)))
            (queue-size x))))

;;;
;;; Queue construction
;;;

(defun make-priority-queue (&key (compare #'<) copy)
  (if copy
      (let* ((queue (make-instance
                    'queue
                    :compare (queue-comparison copy))))
        (setf (min-node-of queue) (deep-copy (min-node-of copy))
              (queue-size queue) (queue-size copy))
        queue)
      (make-instance 'queue :compare compare)))

;;;
;;; Queue operations
;;;

(defmethod qclear ((queue priority-queue))
  (setf (min-node-of queue) nil
        (queue-size queue) 0))

;;; ---------------------------------------------------------------------------

(defun %insert (node insert)
  (if node
      (let ((right (node-right node)))
        (setf (node-parent insert) (node-parent node)
              (node-right node) insert
              (node-left insert) node
              (node-right insert) right
              (node-left right) insert))
      (setf (node-parent insert) nil
            (node-right insert) insert
            (node-left insert) insert))
  insert)

;;; ---------------------------------------------------------------------------

(defun %remove (node)
  (let ((right (node-right node))
        (left (node-left node)))
    (if (not (eq node right))
        (setf (node-right left) right
              (node-left right) left)
        nil)))

;;; ---------------------------------------------------------------------------

(defun %concat (node inserted-nodes)
  (let ((last (car inserted-nodes)))
    (mapc (lambda (cur)
            (setf (node-right last) cur
                  (node-left cur) last
                  last cur))
          (cdr inserted-nodes))
    (if node
        (let ((left (node-left node)))
          (setf (node-left node) last
                (node-right last) node
                (node-left (car inserted-nodes)) left
                (node-right left) (car inserted-nodes)))
        (setf (node-right last) (car inserted-nodes)
              (node-left (car inserted-nodes)) last))))

;;; ---------------------------------------------------------------------------

(defun %queue-push (queue element node)
  (let ((min (min-node-of queue)))
    (incf (queue-size queue))
    (flet ((get-node (node element)
	     (cond ((node
		     (setf (node-value node) element
			   (node-degree node) 0)
		     node))
		   (t (make-node :value element
				 :degree 0)))))
      (if min
	  (let* ((new-node (get-node node element))
		 (new (%insert (min-node-of queue) new-node)))
	    (when (funcall (queue-comparison queue)
			   element (node-value min))
	      (setf (min-node-of queue) new)))
	  (setf (min-node-of queue)
		(let ((node (get-node node element)))
		  (setf (node-left node) node
			(node-right node) node))))))
  element)
  

;;; ---------------------------------------------------------------------------

(defmethod qpush ((queue priority-queue) element)
  (%queue-push queue element nil))

;;; ---------------------------------------------------------------------------

(defmethod qtop ((queue priority-queue) &optional empty-value)
  (if (min-node-of queue)
      (values (node-value (min-node-of queue)) t)
      (values empty-value nil)))

;;; ---------------------------------------------------------------------------

(defun %merge (root1 root2)
  (let ((x (node-left root1))
        (y (node-left root2)))
    (setf (node-left root1) y
          (node-left root2) x
          (node-right x) root2
          (node-right y) root1)))

;;; ---------------------------------------------------------------------------

(defun %queue-compatible-p (queue1 queue2)
  (and queue1 queue2
       (eq (queue-comparison queue1)
           (queue-comparison queue2))))

;;; ---------------------------------------------------------------------------

(defun %queue-merge-metadata (queue1 queue2)
  (let ((min1 (min-node-of queue1))
        (min2 (min-node-of queue2))
        (size1 (queue-size queue1))
        (size2 (queue-size queue2)))
    (values
     (if (funcall (queue-comparison queue1)
                  (node-value min2)
                  (node-value min1))
         min2 min1)
     (+ size1 size2))))

;;; ---------------------------------------------------------------------------

(defmethod queue-merge ((queue1 priority-queue) (queue2 priority-queue))
  "Destructively merges queue2 into queue1"
  (when (%queue-compatible-p queue1 queue2)
    (cond ((not queue1) queue2)
          ((not queue2) queue1)
          (t (%merge (min-node-of queue1)
                     (min-node-of queue2))
             (multiple-value-bind (min size)
                 (%queue-merge-metadata queue1 queue2)
               (setf (min-node-of queue1) min
                     (queue-size queue1) size
                     (min-node-of queue2) nil
                     (queue-size queue2) 0))))
    queue1))

;;; ---------------------------------------------------------------------------

(defun deep-copy (node &optional parent)
  (when (queue-node-p node)
    (flet ((copy (old-node
                  &aux (copied-node (copy-node old-node)))
             (setf (node-parent copied-node) parent
                   (node-child copied-node)
                   (deep-copy (node-child old-node) copied-node))
             copied-node))
      (let* ((first (copy node))
             (current first))
        (loop with x = (node-right node)
           until (eq x node)
           do
             (let ((y (copy x)))
               (setf (node-right current) y
                     (node-left y) current
                     current y
                     x (node-right x))))
        (setf (node-right current) first
              (node-left first) current)
        first))))

;;; ---------------------------------------------------------------------------

(defmethod queue-merge-safe ((queue1 priority-queue) (queue2 priority-queue)))
  "Nondestructive merge"
  (when (%queue-compatible-p queue1 queue2)
    (let* ((new-queue (make-priority-queue :compare (queue-comparison queue1)))
           (min1 (deep-copy (min-node-of queue1)))
           (min2 (deep-copy (min-node-of queue2))))
      (%merge min1 min2)
      (multiple-value-bind (min size)
          (%queue-merge-metadata queue1 queue2)
        (setf (min-node-of new-queue) min
              (queue-size new-queue) size)
        new-queue))))

;;; ---------------------------------------------------------------------------

(defun ilog  (base j &aux (n 1))
  (labels ((f (m b k)
             (cond ((> b k) k)
                   (t (incf n m)
                      (let ((h (f (* 2 m)
                                  (* b b)
                                  (floor k b))))
                        (cond ((> b h) h)
                              (t (incf n m) (floor h b))))))))
    (cond ((> base j) 0)
          (t (f 1 base (floor j base))
             n))))

;;; ---------------------------------------------------------------------------

(defun %fib-queue-link (big little)
  (%remove big)
  (setf (node-child little)
        (%concat (node-child little) (list big))
        (node-parent big) little
        (node-degree little) (1+ (node-degree little))
        (node-mark big) nil)
  little)

;;; ---------------------------------------------------------------------------

(defun queue-consolidate (queue)
  (let ((unique (make-array (ceiling (log (queue-size queue) 1.618))
                            :initial-element nil)))
    (map-siblings
     (lambda (node &aux (degree (node-degree node)))
       (unless (node-parent node)
         (loop while (aref unique degree) do
              (let ((other (aref unique degree)))
                (when (eq node other) (return nil))
                (when (funcall (queue-comparison queue)
                               (node-value other)
                               (node-value node))
                  (rotatef other node))
                (%fib-queue-link other node)
                (setf (aref unique degree) nil)
                (incf degree)))
         (setf (aref unique degree) node)))
     (min-node-of queue))
    (setf (min-node-of queue) nil)
    (loop for x across unique
       with node = nil
       with min = nil do
         (when x
           (setf node (%insert node x))
           (when (or (not min)
                     (funcall (queue-comparison queue)
                              (node-value x)
                              (node-value min)))
             (setf min x)))
       finally (setf (min-node-of queue) min))))

;;; ---------------------------------------------------------------------------

(defmethod qpop ((queue priority-queue) &optional empty-value)
  (let ((min (min-node-of queue)))
    (unless min
      (return-from queue-pop
        (values empty-value nil)))
    (labels ((add-children (node)
               (map-children (lambda (x)
                               (%insert min x))
                             node)))
      (add-children min)
      (setf (min-node-of queue) (%remove min))
      (queue-consolidate queue)
      (decf (queue-size queue)))
    (values (node-value min) t)))

;;; ---------------------------------------------------------------------------

(defun %node-cut (queue node parent)
  (setf (node-child parent) (%remove node))
  (decf (node-degree parent))
  (%insert (min-node-of queue) node)
  (setf (node-mark node) nil))

;;; ---------------------------------------------------------------------------

(defun %node-cascading-cut (queue node)
  (let ((parent (node-parent node)))
    (when parent
      (cond ((node-mark node)
             (%node-cut queue node parent)
             (%node-cascading-cut queue parent))
            (t (setf (node-mark node) t))))))

;;; ---------------------------------------------------------------------------

(defun %queue-decrease (queue node value)
  (let ((parent (node-parent node))
        (comp (queue-comparison queue)))
    (setf (node-value node) value)
    (when parent
      (%node-cut queue node parent)
      (%node-cascading-cut queue parent))
    (when (funcall comp value (node-value (min-node-of queue)))
      (setf (min-node-of queue) node))))

;;; ---------------------------------------------------------------------------

(defmethod queue-find ((queue priority-queue) predicate-or-key)
  (let* ((comp (queue-comparison queue))
         (pred (if (functionp predicate-or-key)
                   predicate-or-key
                   (lambda (x)
                     (not (or (funcall comp predicate-or-key x)
                              (funcall comp x predicate-or-key)))))))
    (dfs-forest (lambda (x)
                  (when (funcall pred (node-value x))
                    (return-from queue-find x)))
                (min-node-of queue))))

;;; ---------------------------------------------------------------------------

(defun queue-decrease (queue node newkey)
  (when (queue-node-p node)
    (unless (funcall (queue-comparison queue) newkey (node-value node))
      (error "new key is greater than old key"))
    (%queue-decrease queue node newkey)
    node))

;;; ---------------------------------------------------------------------------

(defmethod queue-change (queue node newkey)
  (cond ((funcall (queue-comparison queue) newkey (node-value node))
	 (queue-decrease queue node newkey))
	(t
	 (queue-delete queue node)
	 (%queue-push queue newkey node))))

;;; ---------------------------------------------------------------------------

(defmethod queue-delete ((queue priority-queue) node)
  (unless (queue-node-p node)
    (setf node (queue-find queue node)))
  (when (queue-node-p node)
    (let ((parent (node-parent node)))
      (when parent
        (%node-cut queue node parent)
        (%node-cascading-cut queue parent))
      (setf (min-node-of queue) node)
      (queue-pop queue)
      (node-value node))))

;;;
;;; Printing and mapping
;;;

(defun map-siblings (fn node)
  (when (queue-node-p node)
    (let ((col (do ((cur (node-right node)
                         (node-right cur))
                    (col nil
                         (cons cur col)))
                   ((eq cur node)
                    (cons node col)))))
      (mapc fn col))
    nil))

;;; ---------------------------------------------------------------------------

(defun map-children (fn node)
  (when (queue-node-p node)
    (map-siblings fn (node-child node))))

;;; ---------------------------------------------------------------------------

(defun dfs-forest (fn node)
  (map-siblings
   (lambda (x)
     (when (node-child x)
       (dfs-forest fn (node-child x)))
     (funcall fn x))
   node))

;;; ---------------------------------------------------------------------------

(defvar *current-queue-node* nil)

(defmethod map-queue (fn (queue priority-queue))
  (dfs-forest (lambda (*current-queue-node*)
                (funcall fn (node-value *current-queue-node*)))
              (min-node-of queue)))

;;; ---------------------------------------------------------------------------

(defun print-node (node stream ind)
  (flet ((pr (x i)
           (write-string
            (make-string (* 4 i) :initial-element #\Space)
            stream)
           (prin1 x stream)
           (format stream " [d:~A, m:~A]" (node-degree x) (node-mark x))
           (write-char #\Newline stream)))
    (pr node ind)
    (let (col)
      (map-children
       (lambda (x) (push x col))
       node)
      (setf col (sort col #'> :key #'node-degree))
      (mapc (lambda (x)
              (print-node x stream (1+ ind)))
            col))
    nil))

;;; ---------------------------------------------------------------------------

(defmethod print-queue ((queue priority-queue)
			&optional (stream *standard-output*)
			&aux (c 0))
  (format stream "=== ~A ===~%" queue)
  (map-siblings
   (lambda (x &aux (y (incf c)))
     (format stream "~A::~A~%" y
             (if (eq x (min-node-of queue)) " min" ""))
     (print-node x stream 1))
   (min-node-of queue)))

;;; ===========================================================================
;;; End of file
;;; ===========================================================================