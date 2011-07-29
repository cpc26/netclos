;;;-----------------------------------------------------------------------------------
;;; name       : message-handler
;;; description: 
;;; notes      : why do I use explicit messages instead of just sending list to be evaluated
;;;              in the remote objectspace? Well,
;;;              - this allows for special handling of requests
;;;              - message-specific packing is possible
;;;              - its easier to change the low-level communication mechanism, e.g. to pvm
;;;              - packing is done in the objspace-process, so the overhead for the calling
;;;                process when passing a message to a proxy is not to big.
;;; contact    : me (Michael Trowe)
;;; copyright  :
;;; history    : 
;;; contents   :
;;;-----------------------------------------------------------------------------------
(in-package nc)

(shadow 'stream)

(defvar *calling-os* ())

(defclass objspace (delayed-reply-object)
  ((host :initarg :host :accessor host)
   (port :initarg :port :accessor port)
   (stream :initarg :objstream :accessor stream))) ; FIXME was stream enstead of objstream

(defpargeneric send-request :future (objspace address caller method args))

(defpargeneric send-operation :past  (objspace address caller method args))

(defpargeneric kernel-send :future (objspace message))

(defpargeneric remote-eval :now (objspace form))

(defclass receiver  (autonomous-object)
  ((objspace :accessor objspace :initarg :objspace)
   (stream :initarg :stream :accessor stream)))

(defmethod initialize-instance :after ((inst receiver) &rest initargs)
  (declare (ignore initargs))
  nil)
#| wird in autonomousobject init-instan. erledigt
  (setf (process inst) (mp:make-process :name (string (class-name (class-of inst)))
					:quantum 0.5
					:priority 1))
  (mp:process-preset (process inst)  #'active-loop inst)
  (mp:process-enable (process inst)))
  |#
 

(defmethod active-loop  ((receiver receiver))
  (handler-case (loop while t
		    do (active-body receiver))
    (end-of-file () (connection-lost *manager* receiver))))

(defmethod active-body ((receiver receiver))
  (let ((*calling-os* (objspace receiver))
	(*package* (find-package "NETCLOS")))
    (acl-compat-mp:wait-for-input-available (stream receiver))
    ;;;(print (stream receiver) excl::*initial-terminal-io*)
    ;;The process stops until input through the stream is available.
   #|
    (when (or (equal *local-host* "ki18"))

      (trace       
       excl::stream-read-char
              excl::stream-write-char
	      excl::stream-read-byte excl::stream-write-byte read))

    (let ((read-thing 
	   (read (stream receiver))))
      (when (or (equal *local-host* "ki18")
		(equal *local-host* "ki3"))
	(format t "B Rec: ~S B: Tran: ~S" (excl::bytes-received (stream receiver))
		(excl::bytes-transmitted (stream receiver))))
      (if (evaluable read-thing)
	  (eval read-thing)
	(progn
	  (break) 
	  )))
	  |#
       (eval (read (stream receiver)))
	  ))				;Read a message and evaluate it.

(defun evaluable (thing)
  (cond ((symbolp thing)
	 nil)
	(t t)))

(defmethod schedule ((sched standard-scheduler) (obj objspace) message)
  (enqueue (mail-queue obj) message)
  (unless (active-p obj)
    (setf (active-p obj) t)
    (acl-compat-mp:process-run-function (list :name (symbol-name (class-name (class-of obj)))
				   :quantum 0.5
				   :priority 1)
			     #'active-loop obj)))    

(defmethod kill :after ((obj objspace))
  (close (stream obj)))

(defmethod reset ((obj objspace))
  (clear-output (stream obj)))

(defmethod kill :after ((obj receiver))
  (close (stream obj)))

(defmethod reset ((rec receiver))
  (clear-input (stream rec)))

(defmethod send-request ((space objspace) address caller method args)
  (kernel-send space
	       (request-call-message :obj-address address
				     :caller caller
				     :method method
				     :arguments args)))

(defmethod send-operation ((space objspace) address caller method args)
 (kernel-send space
	      (operation-call-message :obj-address address
				      :caller caller
				      :method method
				      :arguments args)))

(defmethod remote-eval  ((space objspace) form)
   (kernel-send space (remote-evaluation-message :form form)))

(defmethod kernel-send :before ((space objspace) (message request))
  (setf (request-message-id message)  (store-future space)))
 

(defmethod kernel-send ((space objspace) message)

  (let ((*package* (find-package "NETCLOS")))
    #|(when (and (equal (host space) "ki18")
	       (equal (rmi-class-name message) 'nc::function-node))
      (trace  excl::stream-read-byte excl::stream-write-byte )
      (print (list "TOki18" message))
      (pack message excl::*initial-terminal-io*))
      |#

    (pack message (stream space))

    (finish-output (stream space))

    (values)))
    
(defmethod move ((obj mobile-object) 
		 destination-os)
  (let* ((address (touch (kernel-send destination-os
				      (moving-message :moved-obj obj)))) 
	 (obj-class-name (class-name (class-of obj)))
	 (proxy (change-class obj (ensure-proxy-class obj-class-name))))
    ;;; initialisierung analog zu initialize-instance in distribute.lisp
    
    (setf (masterclass-name proxy) obj-class-name)
    (setf (remote-os proxy) destination-os)
    (setf (remote-address proxy) address)
    (setf (other-pack-string proxy) 
      (format nil "(np ~a '~a (fos \"~a\"))"
	      (remote-address proxy)
	      (format nil "~S" (masterclass-name proxy))
	      (host (remote-os proxy))))
    (setf (master-pack-string proxy) (format nil "(fetch-obj  ~a)"
					     (remote-address proxy)))
    (notify-proxy proxy)))

(defmethod receive-reply ((space objspace) message-id content)
  (write-to-future (get-future space message-id) content))

(defmethod receive-call ((method symbol) *current-actor* obj args)
  (apply (fdefinition method) obj args))

(defmethod receive-call ((method cons) *current-actor* obj args)
  (apply (fdefinition method) (first args) obj (rest args)))



(defmessage request-call (obj-address caller method arguments) 
  :request-p t
  :process-p t
  :receive-action (touch (receive-call method 
				caller
				(fetch-obj obj-address)
				arguments)))

(defmessage operation-call (obj-address caller method arguments) 
  :process-p t
  :receive-action  (receive-call method 
				 caller
				 (fetch-obj obj-address) 
				 arguments))
			  
(defmessage reply (message-id content)
  :receive-action (receive-reply *calling-os* message-id content))

(defmessage moving ((moved-obj :packing move-pack))
  :request-p t 
  :receive-action (ensure-exported moved-obj))

(defmessage rmi (class-name initargs-b)	;remote-make-instance
  :request-p t
  :process-p t
  :receive-action (apply #'make-instance class-name initargs-b))

(defmessage remote-evaluation (form)
  :request-p t
  :process-p t
  :receive-action (eval form))

				  
(defmethod pack ((obj t) &optional stream)
  (print obj stream)
  (values))

(defmethod pack ((obj symbol) &optional stream)
  (if (keywordp obj)
      (print obj stream)
    (print (list 'intern (list 'quote (symbol-name obj)) (intern 
					    (package-name (symbol-package obj)) :keyword))
	   stream))      
  (values))

(defmethod pack ((obj null) &optional stream)
  (write-string "()" stream)
  (values))


(defmethod pack ((obj cons) &optional stream)
  (write-string "(cons " stream)
  (pack (car obj) stream) 
  (write-char #\space stream)
  (pack (cdr obj) stream)
  (write-char #\) stream)
      (values))

(defmethod pack ((obj string) &optional stream)
  (print obj stream)
      (values))

(defmethod pack ((array array) &optional stream)
  (write-string "(unpack-array " stream)
  (pack (array-dimensions array) stream)
  (write-char #\space stream)
  (pack (array-element-type array) stream)
  (write-char #\space stream)
  (loop for i from 0 upto (1- (array-total-size array))
      do (progn (pack (row-major-aref array i) stream)
		(write-char #\space stream)))
  (write-char #\) stream)
      (values))

(defun unpack-array (dims type &rest elements)
  (let ((ar (make-array dims :element-type type)))
    (loop for i from 0 upto (1- (array-total-size ar))
	for el in elements
	do (progn (setf (row-major-aref ar i) el)))
    ar))

(defmethod pack ((func function) &optional stream)
  (let ((denom (get-denom func)))
    (if denom 
	(progn (write-string "(function " stream)
	       (write-string (symbol-name denom) stream)
	       (write-char #\) stream))
      (progn (write-string "(fnp " stream)
	     (write (ensure-exported func) :stream stream)
	     (write (class-name (class-of func)) :stream stream)
	     (write-char #\) stream))))
      (values))
 
(defun fnp (address class-name) 
  (let ((p (notify-proxy (make-proxy class-name
				     address 
				     *calling-os*))))
	 #'(lambda (&rest rest)
	     (send-now p 'apply rest))))

;;; ohne *full-packing* wirds dem Benutzer ueberlassen, wie 
;;; eine klasse verpackt wird.

(defvar *full-packing* nil)
(defmethod pack :around ((obj standard-object) &optional stream)
  (if *full-packing*
      (pack-standard-object obj stream)
    (call-next-method)))

(defun pack-standard-object (obj stream)
  (let ((reference (gentemp "I")))
    (setq *pack-forms* (acons obj reference
			      *pack-forms*))
    (format stream "(progn (proclaim '(special ~a))
		                 (prog1 (setq ~a (make-instance '~S))
		                    (unpack-slots ~a (list"
	    reference reference (class-name (class-of obj)) reference)
    (pack-slots-default (class-of obj) obj stream)
    (write-string "))))" stream))
  (values))


(defun pack-slots-default (class obj stream)
  (loop for slot in (class-slots class)
      do
	(move-pack (slot-value obj (slot-definition-name slot)) stream)))


(defmethod pack ((obj standard-object) &optional stream)
  (multiple-value-bind (address obj-string)
      (ensure-exported obj)
    (declare (ignore address))
    (write-string obj-string stream))
      (values))

(defun np (address class-name &optional (os *calling-os*))
  (notify-proxy (make-proxy class-name
			    address
			    os)))
		   

(defmethod pack ((proxy proxy) &optional stream)
  (if (eq (remote-os proxy) *current-actor*)
      (write-string (master-pack-string proxy) stream)
    (progn (notify-sending proxy)
	   (write-string (other-pack-string proxy) stream)))
      (values))

(defmethod pack ((space objspace) &optional stream)
  (write-string (concatenate 'string "(fos \"" (host space) "\")") stream)
  (values))

(defun fos (host)
  (find host (spaces *manager*) :key #'host :test #'equal))

(defmethod print-object ((n objspace) stream)
 (format stream "#<~S on host: ~S>" (type-of n) (slot-value n 'host)))


(defvar *pack* ())

(defmethod pack ((struc structure-object) &optional stream)
  (let* ((class-name (class-name (class-of struc)))
	 (class-package (symbol-package class-name)))
    (format stream "(~A::MAKE-" (package-name class-package))    
    (write-string (string class-name) stream)
    (write-char #\space stream)
    (loop for slot in (class-slots (class-of struc))
	for name = (slot-definition-name slot)
	do (progn (write-char #\: stream)
		  (write-string (string name) stream)
		  (write-char #\space  stream)
		  (pack (funcall (fdefinition (intern (format nil "~a-~a"
							      class-name
							      name)
						      class-package))
				 struc)
			stream)))
    (write-char #\) stream)
    (values)))

(defun objectspace-active-p (objspace)
  (slot-boundp objspace 'stream))
						 









