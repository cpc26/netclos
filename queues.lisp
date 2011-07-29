;;; ;;;  _*_ Mode: lisp ; Syntax: Common-Lisp
;; File queue.cl
;; Creator Bernhard Lahres 
;; changed to make them useful by Michael Trowe
;; Date 11/02/93
;;
;;
;; Classes queue and queue-list
;;
;; queue manages a queue and offers the functions enter-value
;; and get-value 
;;
;; queue-list manages a list of named queues and offers the methods
;; add-named-queue, delete-named-queue and get-named-queue
;;


(in-package nc)

;; class queue

(defclass queue()
  ((head :accessor head :initform ())
   (tail :accessor tail :initform ())))

(defun mq (&rest inits)
  (let ((q (make-instance 'lockable-queue)))
    (setf (head q) inits)
    (setf (tail q) (last inits))
    q))

(defmethod empty-queue-p ((queue queue))
  (null (head queue)))

(defmethod enqueue ((queue queue) value)
  (let ((new-pair (cons value nil)))
    (if (empty-queue-p queue)
	(setf (head queue) new-pair)
      (setf (cdr (tail queue)) new-pair))
    (setf (tail queue) new-pair)
    queue))


(defmethod dequeue ((queue queue))
  (prog1 (car (head queue))
    (setf (head queue) (cdr (head queue)))))



(defmethod describe-queue ((queue queue))
  (dolist (element (head queue))
    (describe element)))

(defmethod all-elements ((queue queue))
  (head queue))

  
(defmethod find-and-delete ((queue queue) element &key (test #'eql))
  (loop for pair on (head queue)
      and previous = () then pair
      for test-result = (funcall test element (car pair))   
      until test-result
      finally (when test-result
		(if previous
		    (setf (cdr previous) (cdr pair))
		  (setf (head queue) (cdr pair)))
		(when (endp (cdr pair))
		  (setf (tail queue) previous))
		(return (car pair)))))

;;; class lockable-queue
;;; queue with a lock for use with multiple processes and
;;; methods for locking and unlocking the queue

(defclass lockable-queue (queue)
	((queue-lock
	:accessor queue-lock 
	:initform (acl-compat-mp:make-process-lock))))

(defmethod enqueue  ((queue lockable-queue) value)
  (declare (ignore value))
  (acl-compat-mp:with-process-lock ((queue-lock queue)) (call-next-method)))

(defmethod dequeue  ((queue lockable-queue))
  (acl-compat-mp:with-process-lock ((queue-lock queue)) (call-next-method)))

(defmethod all-elements ((queue lockable-queue))
  (acl-compat-mp:with-process-lock ((queue-lock queue)) (call-next-method)))


;; class named-queue
;; this class is simply a queue with a name, but no additional methods

(defclass named-queue (queue)
	 ((name :accessor name :initarg :name)))

(defclass lockable-named-queue (named-queue lockable-queue) ())
  

;; class attributed-queue
;; this class is simply a queue with attributes, but no additional methods

(defclass attributed-queue (lockable-queue)
  ((attribute :accessor attribute :initarg :attribute)))





;; class queue-list
;; this class manages a set of named queues

(defclass queue-list ()
	  ((name :accessor name :initarg :name)
	   (queues :accessor queues :initform nil)))



;; enter new named queue
;; return value is the list of named queues

(defmethod add-named-queue ((queue-list queue-list) name)
  (setf (queues queue-list)  (cons (make-instance 'named-queue :name name) (queues queue-list))))



;; get named queue
;; return value is a object of class named-queue
;; if no object with name 'name' is contained in the list,
;; the return value is nil

(defmethod get-named-queue ((queue-list queue-list) name)
  (let ((result nil))
    (dolist (element (queues queue-list) result)
      (if (eq (name element) name)
	  (return element)
	  nil))))


;; delete a named queue, which was inserted with add-named-queue
;; the return value is the list of named queues
;; Should no named queue with name 'name' be contained,
;; no action is taken.

(defmethod delete-named-queue ((queue-list queue-list) name)
  (defun name-equal (list-element)
    (eq (name list-element) name))
  (setf (queues queue-list)
	(remove-if #'name-equal (queues queue-list))))
