;;;-----------------------------------------------------------------------------------
;;; name       : active.lisp
;;; description: The following code is about active-objects. Active Objects are diffent from
;;;              normal Lisp Objets - if you send a message to them they work on it in their
;;;              own (conceptual) process, but always only one message at a time.
;;;              Depending on the mode used for sending, they may
;;;              do it concurrently with the calling process:
;;;              past: asynchronous message passing
;;;              future: asynchronous, but synchronisation via future objects
;;;              now: synchronous message passing.
;;;              To realize active objects, some other classes are necessary:
;;;              scheduler: The scheduler provides processor-time for the active objects
;;;              future: future objects can be returned by an asynchronous functioncall
;;;                      instead of the result. When the result is needed, the calling process
;;;                      does a touch on the future. This touch suspends the process until the
;;;                      result is computed, and returns the result.
;;;              sending-gf: Sending-gf's are generic-functions which transform a function call
;;;                          on an active object directly into a message. Example:
;;;                          Be foo a sending-gf with send-func send-future. What happens when
;;;                          foo is called on an active object? Well, first a future object
;;;                          and then a message (something like '(foo args future-obj)) is
;;;                          created. The message is send to the active object, and foo
;;;                          returns the future object.
;;;              I also realized protected objects, which accpet only now-messages and can be
;;;              locked.
;;; notes      :
;;; contact    : me (Michael Trowe)
;;; copyright  :
;;; history    :
;;; contents   :
;;;-----------------------------------------------------------------------------------
(in-package nc)
(defvar *caller* nil)
(defvar *current-actor* nil)
(defvar *get-scheduler* nil)

(shadow 'stream)
;;; I have to define the standard scheduler class here, because I use it in an initform
;;; of the active-object class.
(defclass standard-scheduler ()
  ((obj-proc-pairs :initform () :accessor obj-proc-pairs)))

(let ((standard-scheduler (make-instance 'standard-scheduler)))
  (setq *get-scheduler* (lambda ()  standard-scheduler)))

;;; the classes

;;; concurency-obj is useful when I later define other classes, wich are not active objects,
;;; but also have some concurrency features.
(defclass concurrency-obj () ())

(defclass active-object (concurrency-obj)
  ((queue :accessor mail-queue
	  :initform (make-instance 'lockable-queue))
   (active-p :initform ()
	     :accessor active-p)
   (lock :accessor obj-lock
	 :initform (acl-compat-mp:make-process-lock))
   (scheduler :accessor scheduler :initform (funcall *get-scheduler*))
   (waiting-pattern :initform () :accessor waiting-pattern)
   (wait-lock :initform (acl-compat-mp:make-process-lock) :accessor wait-lock)
   (waiting-function :accessor waiting-function)))

;;; the following classes are useful mainly for low-level stuff, e.g. to implement
;;; the infrastructure for the distributed object service.
(defclass delayed-reply-object (active-object)
   ((pending-requests :accessor pending-requests
		     :initform ())
   (last-id :accessor last-id
	    :initform 0)
   (current-future :accessor current-future
		   :initform ()))
   (:documentation "These objects can store futures, do some other stuff, e.g. delegate
a call, and later restore the future and set it to some value"))


(defclass autonomous-object ()
  ((process  :accessor process))
  (:documentation "these objects don't need a scheduler. This is useful to implement a
never-ending process"))


;;; auxilliary classes
(defclass future ()
  ((result :accessor result)
   (computed-p :accessor computed-p :initform ())
   (waiting-processes :initform () :accessor waiting-processes)))

(defclass multi-future (future)
  ((computed-nr :initform 0 :accessor computed-nr)
   (nr-of-sends :initarg :nr-of-sends :accessor nr-of-sends)
   (future-list :initarg :future-list :accessor future-list)))

(defclass sending-gf (standard-generic-function)
  ((send-func :accessor send-func :initarg :send-func :initform #'send-now))
  (:metaclass funcallable-standard-class))
;;(:default-initargs :send-func #'send-now))

;;; Normally, the object a message is send to, is the first of the arguments of a sending-gf.
;;  But when fou define setf forms, the manipulated object comes in the secons position
;;; of the lambda list. When (setf (huhu active-obj) new-value) is evaluated, it expands
;;; to something-like (funcall #'(setf huhu) new-value active-obj) (well, at least
;;; conceptually). This means you have to do the processing of the lambda list for setf-gf a
;;; bit different.
(defclass setf-gf  (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class))

(defclass sending-setf (sending-gf setf-gf)
  ()
  (:metaclass funcallable-standard-class))

;;; And now comes the implementation stuff,

;;;------------------------------------------------
;;; here is the main part: operations on active objects
;;;-------------------------------------------------

;;; The three forms od sending a message to an actor.

(defmethod send-future ((obj active-object) caller gf-name &rest args)
  (send obj caller gf-name args (make-future)))

(defmethod send-past ((obj active-object) caller gf-name &rest args)
  (send obj caller gf-name args nil))

(defmethod send-now ((obj active-object) caller gf-name &rest args)
  (touch (send obj caller gf-name args (make-future))))

(defmethod multicast ((objs list) gf-name &rest args)
  (let ((future (make-multi-future (length objs))))
    (loop for obj in objs
	do (send obj *current-actor* gf-name args future))
    future))

(defun send (obj caller gf-name args future)
    (acl-compat-mp:with-process-lock ((obj-lock obj))
      ;;Enqueueing (scheduling) and testing for activation must be serialized with active-test. Otherwise,
      ;;the following scenario might occur:
      ;;1. empty-queue-p in active-test yields true,
      ;;2. then the new messsage is enqueued,
      ;;3. then the test (active-p obj) yields true, so the object won't be reactivated,
      ;;4. and then the object deactivates, though there is still a message to process.
      (if (and (waiting-pattern obj)
	       (message-match (waiting-pattern obj)
			      (list caller gf-name args)))
	  (run-message (scheduler obj)
		       obj caller gf-name args future)
	(schedule  (scheduler obj) obj (list caller gf-name args future))))
    future)

;;; What follows is used to interprete the messages send to an actor
;;; and to controll this process.



(defmethod active-test ((obj active-object))
  (acl-compat-mp:with-process-lock ((obj-lock obj))
    (if (and (empty-queue-p (mail-queue obj))
	    (not (waiting-pattern obj)))
	(unschedule (scheduler obj) obj)
      t)))

(defmethod active-body :around ((obj active-object))
  (with-simple-restart (abort "Let active object ~a resume." obj)
    (call-next-method)))

(defmethod active-body ((obj active-object))
  #|
  (when (equal *local-host* "ki18")
  (format t "Active-body von ~S mit stream ~S" obj (stream obj)))
  |#
  (let ((message (dequeue (mail-queue obj))))
    (when message
      (apply-message obj
		     (first message)
		     (second message)
		     (third message)
		     (fourth message)))))

(defmethod apply-message ((*current-actor* active-object) *caller* message-name args future)
  (call-gf *current-actor*
	   (fdefinition message-name)
	   (if (listp  message-name)
	       (cons (car args) (cons *current-actor* (cdr args)))
	     (cons *current-actor* args))
	   future))

(defmethod call-gf ((obj active-object) (method function) args future)
  (cond (future
	 (write-to-future future (restart-case (apply method args)
				   (nil (result)
				       :report
					 (lambda (stream)
					   (format stream
						   "Provide a value for the future."))
				       :interactive
					 (lambda ()
					   (format t "Future-value: ")
					   (list (eval (read))))
				     result))))
	(t (apply method args))))


(defmethod call-gf ((obj delayed-reply-object) (method function) args future)
  (cond (future
	 (setf (current-future obj) future)
	 (let ((result (apply method args)))
	    (when (current-future obj)
	      (write-to-future future result))))
	(t (apply method args)))
  (setf (current-future obj) nil))



(defmethod store-future ((obj delayed-reply-object))
  (let ((id (next-message-id obj)))
    (push (cons id (current-future obj))
	  (pending-requests obj))
    (setf (current-future obj) nil)
    id))

(defmethod get-future ((obj delayed-reply-object) id)
  (prog1 (cdr (assoc id  (pending-requests obj)))
    (setf (pending-requests obj)
      (delete id (pending-requests obj) :key #'car))))

(defmethod next-message-id ((obj active-object))
  (setf (last-id obj) (mod (1+ (last-id obj)) 65536)))




(defmethod initialize-instance :after ((inst autonomous-object) &rest initargs)
  (declare (ignore initargs))
  (setf (process inst) (acl-compat-mp:make-process :name (string (class-name (class-of inst)))
					:quantum 0.2))
  (acl-compat-mp:process-preset (process inst)  #'active-loop inst)
  (acl-compat-mp:process-enable (process inst)))

(defmacro wait-for ((message-name &rest args) &optional from (caller '?))
  (declare (ignore from))
  `(active-object-wait (scheduler *current-actor*)
		       *current-actor*
		       ,(if (eq caller '?)
			    ''?
			  caller)
		       ',message-name
		       (list ,@(loop for arg in args
				   collect (if (eq arg '?)
					       ''?
					     arg)))))

(defun message-match (pattern message)
  (and (or (eq (first pattern) '?)
	   (eq (first pattern) (first message)))
       (or (eq (second pattern) '?)
	   (eq (second pattern) (second message)))
       (loop for x in (third pattern)
	   for y in (third message)
	   always (or (eq x '?)
		      (equal x y)))))


(defmethod kill ((obj active-object))
  (unschedule (scheduler obj) obj))

(defmethod kill ((obj autonomous-object))
  (acl-compat-mp:process-kill (process obj)))

(defmethod reset :before ((obj autonomous-object))
  (acl-compat-mp:process-reset (process obj)))

(defmethod reset :after ((obj autonomous-object))
  (acl-compat-mp:process-preset (process obj) #'active-loop obj))

(defmethod reset :before ((obj active-object))
  (unschedule (scheduler obj) obj))

(defmethod reset :after ((obj active-object))
  (schedule (scheduler obj) obj ()))

;;;---------------------------------------------------------------
;;; Now the auxilliary stuff: 1. futures
;;;--------------------------------------------------------------
(defun make-future ()
  (make-instance 'future))

(defun make-multi-future (length)
  (make-instance 'multi-future
    :nr-of-sends length
    :future-list (loop repeat length collect (make-future))))

(defmethod touch ((obj t))
  obj)

(defmethod computed-p ((obj t))
  t)


(defmethod touch ((fut future))
  (acl-compat-mp:without-interrupts	; FIXME was excl:without-interrupts
    (unless (computed-p fut)
      (push acl-compat-mp:*current-process* (waiting-processes fut)) ; FIXME was sys:*current-process*
      (acl-compat-mp:process-disable acl-compat-mp:*current-process*)))
  (result fut))

(defmethod maptouch (func (mfut multi-future))
  (loop for fut in (future-list mfut)
      collect (funcall func (touch fut))))

(defmethod write-to-future ((fut future) result)
  (acl-compat-mp:without-interrupts
    (setf (computed-p fut) t)
    (setf (result fut) result))
  (loop for proc in (waiting-processes fut)
      do (acl-compat-mp:process-enable proc)))

(defmethod write-to-future ((mfut multi-future) result)
  (acl-compat-mp:without-interrupts
    (write-to-future (nth (computed-nr mfut) (future-list mfut)) result)
    (incf (computed-nr mfut))
    (when (= (computed-nr mfut) (nr-of-sends mfut))
      (setf (computed-p mfut) t)
      (setf (result mfut) (mapcar #'result (future-list mfut)))
      (loop for proc in (waiting-processes mfut)
	  do (acl-compat-mp:process-enable proc)))))


;;;; an alternative implementation.
#|
(defun make-future ()
  (let ((fut (make-instance 'future)))
    (mp:process-lock (future-lock future) 0)
    fut))

(defmethod touch ((fut future))
  (mp:process-lock (future-lock fut) 1)
  (mp:process-unlock (future-lock fut) 1)
  (result future))

(defmethod write-to-future ((fut future) result)
  (setf (result future) result)
  (mp:process-unlock (future-lock fut) 0))
|#

;;;-----------------------------------------------
;;; 2. schedulers
;;;-----------------------------------------------
(defmethod schedule ((sched standard-scheduler) obj message)
  (enqueue (mail-queue obj) message)
  (unless (active-p obj)
    (setf (active-p obj) t)
    (acl-compat-mp:process-run-function (list :name (symbol-name (class-name (class-of obj)))
					      :quantum 0.2)
					#'active-loop obj)))

(defmethod unschedule ((sched standard-scheduler) obj)
  (setf (active-p obj) nil))

#|
(defmethod unschedule ((sched standard-scheduler) obj)
  (setf (obj-proc-pairs sched)
    (loop for obj-procs on (obj-proc-pairs sched)
	until (eq (caar obj-procs) obj)
	collect (car obj-procs) into new-obj-proc-list
	finally (progn (when obj-procs
			 (mp:process-kill (cdar obj-procs)))
		       (return (append new-obj-proc-list
				       (cdr obj-procs))))))
  (setf (active-p obj) nil))
|#
(defmethod active-loop ((obj active-object))
  (loop while (active-test obj)
      do (active-body obj)))

(defmethod active-object-wait ((sched standard-scheduler) obj caller message-name args)
  (let ((message (find-and-delete (mail-queue obj) (list caller message-name args)
				  :test #'message-match)))
    (if message
	(apply #'apply-message obj message)
	(progn (acl-compat-mp:with-process-lock ((obj-lock obj))
		 (setf (waiting-pattern obj) (list caller message-name args)))
	       (acl-compat-mp:process-lock (wait-lock obj) 0)
	       (acl-compat-mp:process-lock (wait-lock obj) 1)
	       (acl-compat-mp:process-unlock (wait-lock obj) 1)
	       (funcall (waiting-function obj))))))


(defmethod run-message ((sched standard-scheduler) obj caller message-name args future)
  (setf (waiting-pattern obj) ())
  (setf (waiting-function obj) #'(lambda ()
				   (apply-message obj
						  caller
						  message-name
						  args
						  future)))
  (acl-compat-mp:process-unlock (wait-lock obj) 0))

;;;-----------------------------------------
;;; 3. sending-gf
;;;-----------------------------------------

(define-method-combination sending ()
  ((test (:test))
   (send (:send))
   (around (:around))
   (before (:before))
   (primary ())
   (after (:after)))
  (unless (or test primary)
    (method-combination-error "A primary method is required"))
  (flet ((call-methods (methods)
	   (mapcar #'(lambda (method) `(call-method ,method ())) methods)))
    (let* ((prime-form (if (or before after (rest primary))
			   `(multiple-value-prog1
				(progn ,@(call-methods before)
				       (call-method ,(first primary)
						    ,(rest primary))
				       ,@(call-methods (reverse after))))
			 `(call-method ,(first primary) ())))
	      (form (cond ((not primary) '(error "A primary method is required"))
			  (around `(call-method ,(first around)
						(,@(rest around)
						   (make-method ,prime-form))))
			  (t prime-form))))

      (if (and test send)
	  `(if (call-method ,(first test) ())
	       (call-method ,(first send) ())
	     ,form)
	form))))

;;; this is what setf-gfs are good for: To provide a generic interface to the
;;; concurrency-obj in an lambda-list, argument-list etc.
(defmethod get-concurrency-obj ((gf standard-generic-function) args)
  (car args))

(defmethod without-concurrency-obj  ((gf standard-generic-function) args)
  (cdr args))

(defmethod unite-concurrency-obj-and-others  ((gf standard-generic-function) obj args)
  (cons obj args))


(defmethod get-concurrency-obj ((gf setf-gf) args)
  (cadr args))

(defmethod without-concurrency-obj ((gf setf-gf) args)
  (cons (car args) (cddr args)))

(defmethod unite-concurrency-obj-and-others ((gf setf-gf) obj args)
  (cons (car args) (cons obj (cdr args))))


;;; sending-gfs have always the message-combination of type sending
;;; and have always a test and a send method.
(defmethod initialize-instance :after ((gf sending-gf) &rest initargs)
  (declare (ignore initargs))
  (setf (generic-function-method-combination gf) (find-method-combination gf 'sending ()))
  (add-method gf
	      (make-instance 'standard-method
		:function (compile nil (test-lambda gf))
		:specializers (compute-specializers gf)
		:qualifiers '(:test)
		:lambda-list (generic-function-lambda-list gf)))
  (add-method gf
	      (make-instance 'standard-method
		:function (compile nil (send-lambda gf))
		:specializers (compute-specializers gf)
		:qualifiers '(:send)
		:lambda-list (generic-function-lambda-list gf))))

(defmethod reinitialize-instance :after ((gf sending-gf) &rest initargs)
  (declare (ignore initargs))
  (setf (generic-function-method-combination gf) (find-method-combination gf 'sending ()))
  (unless (slot-boundp gf 'send-func)
    (setf (send-func gf) #'send-now))
  (add-method gf
	      (make-instance 'standard-method
		:function (compile nil (test-lambda gf))
		:specializers (compute-specializers gf)
		:qualifiers '(:test)
		:lambda-list (generic-function-lambda-list gf)))
  (add-method gf
	      (make-instance 'standard-method
		:function (compile nil (send-lambda gf))
		:specializers (compute-specializers gf)
		:qualifiers '(:send)
		:lambda-list (generic-function-lambda-list gf))))

(defun test-lambda (gf)
  (if (listp (generic-function-name gf))
      '(lambda (new obj &rest rest)
	 (declare (ignore new rest))
	 (not (eq *current-actor* obj)))
    '(lambda (obj &rest rest)
	 (declare (ignore rest))
      (not (eq *current-actor* obj)))))

(defun proxy-test-lambda (gf)
  (declare (ignore gf))
  '(lambda (&rest rest)
	 (declare (ignore rest))
	 t))


(defun send-lambda (gf)
  (if (listp (generic-function-name gf))
      `(lambda (new obj &rest rest)
	 (apply ,(send-func gf)
		obj
		*current-actor*
		',(generic-function-name gf)
		new
		rest))
    `(lambda (obj &rest rest)
	 (apply ,(send-func gf)
		obj
		*current-actor*
		',(generic-function-name gf)
		rest))))

(defun compute-specializers (gf &optional (specializer-name 'concurrency-obj))
  (unite-concurrency-obj-and-others gf
				     (find-class specializer-name)
				     (make-list (- (length (required-arguments gf)) 1)
						:initial-element (find-class t))))


(defun required-arguments (gf)
  (loop for req in (generic-function-lambda-list gf)
      while (not (member req lambda-list-keywords))
      collect req))


;;; protected objects are useful to enhance performance. You don't need to use an active object
;;; to implement a shared resource. They accept now-messages one-at-a-time (as it should be)
;;; and can be locked.

(defclass protected-obj (concurrency-obj)
  ((obj-lock :accessor obj-lock
	     :initform (acl-compat-mp:make-process-lock))
   (locker :accessor locker :initform :none)))

(defmethod send-now ((*current-actor* protected-obj) *caller* message-name &rest args)
  (let ((f (fdefinition message-name)))
    (if (eq *caller* (locker *current-actor*))
	(apply f (if (listp  message-name)
		     (cons (car args) (cons *current-actor* (cdr args)))
		   (cons *current-actor* args)))
      (unwind-protect (progn
			(acl-compat-mp:process-lock (obj-lock *current-actor*) *caller*)
			(apply f (if (listp  message-name)
				     (cons (car args) (cons *current-actor* (cdr args)))
				     (cons *current-actor* args))))
	(acl-compat-mp:process-unlock (obj-lock *current-actor*) *caller*)))))



(defmethod lock ((obj protected-obj))
  (acl-compat-mp:process-lock (obj-lock obj) *current-actor*)
  (setf (locker obj) *current-actor*))


(defmethod unlock ((obj protected-obj))
  (setf (locker obj) :none)
  (acl-compat-mp:process-unlock (obj-lock obj) *current-actor*))


#| here are some examples to test active objects.

a 1 item buffer:

(defclass buffer (ncl-object)
  ((item :accessor buffer-item :initform ()
	 :moving-behavior :follow)))

(defpargeneric put-item :past (buffer item))

(defpargeneric get-item :future (buffer))



(defmethod put-item ((buf buffer) item)
  (when (buffer-item buf)
    (wait-for (get-item)))
  (print   (setf (buffer-item buf) item)))

(defmethod get-item ((buf buffer))
  (unless (buffer-item buf)
    (wait-for (put-item ?)))
  (prog1 (print (buffer-item buf))
    (setf (buffer-item buf) ())))


an example for protected objects. Be x = y then change doubles both.
but the unlocked change used by concurrent actors may lead to a situation like:
2 actors, x = y = 1.
after change by both actors: x = 3 , y = 4

(defvar *time* 10)
(defclass changer (active-object)
  ())

(defclass protected-var (protected-obj)
  ((x :accessor get-x :initarg :x)
   (y :accessor get-y :initarg :y)))

(defpargeneric get-x :now (var))
(defpargeneric get-y :now (var))
(defpargeneric (setf get-x) :now (new var))
(defpargeneric (setf get-y) :now (new var))

(defpargeneric change :past (changer var))
(defpargeneric locked-change :past (changer var))

(defmethod change ((changer changer) var)
  (let ((x (get-x var))
	(y (get-y var))
	(time *time*))
    (if (= time 0)
	(setf *time* 10)
      (setf *time* 0))
    (setf (get-x var) (+ x y))
    (sleep time)
    (setf (get-y var) (+ x Y))))

(defmethod locked-change ((changer changer) var)
  (lock var)
  (let ((x (get-x var))
	(y (get-y var))
	(time *time*))
    (if (= time 0)
	(setf *time* 10)
      (setf *time* 0))
    (setf (get-x var) (+ x y))
    (sleep time)
    (setf (get-y var) (+ x y)))
  (unlock var))



(setf c1 (make-instance 'changer))
(setf c2 (make-instance 'changer))
(setf v (make-instance 'protected-var :x 1 :y 1))
(change c1 v)
(change c2 v)
(locked-change c1 v)
(locked-change c2 v)


An example for a multicast
(defclass multi-tester (active-object) ((sleep-time :initarg :sleep-time :accessor sleep-time)))

(defpargeneric get-st :future (mt))

(defmethod get-st ((mt multi-tester)) (sleep (sleep-time mt)) (sleep-time mt))

(defun m-test (nr) (maptouch #'print (multicast (loop for i from nr downto 1
						    collect (make-instance 'multi-tester
							      :sleep-time i))
						'get-st)))
(m-test 5)

|#
;;; the end.
;;;------------------------------------------------------
