;;;-----------------------------------------------------------------------------------
;;; name       : distribute
;;; description: Metaclasses and classes necessary to distribute objects across several
;;;              object spaces. Most important are:
;;;              Metaclass mobile-object-class. Instances of Classes of this Metaclass 
;;;              can be moved from one object space to another. At class definition you 
;;;              specify, which content of the object stays when the object is moved, and 
;;;              which content follows.
;;;              Class Proxy. Proxies work as remote references to objects in other object
;;;              spaces. When a pargeneric or a slot access is applied to a proxy, it gets 
;;;              forwarded to the object the proxy points to. 
;;; notes      : I have to define proxies for future to be able to use futures as arguments
;;;              for remote calls. But using futures as arguments to methods is problematic, 
;;;              because of correct method-dispatch (how can you use the result-type for 
;;;              dispatch?). So I will tackle this problem later (hehe, *much* later in fact).
;;; contact    : me (Michael Trowe)
;;; copyright  :
;;; history    : 
;;; contents   : 
;;;-----------------------------------------------------------------------------------
(in-package nc)

;;;;-------------------------------------------------
;;;; classes
;;;;-------------------------------------------------

;;; metaclasses for mobile-objects
;;;-------------------------------
(defclass mobile-object-class (standard-class) 
  ()
  (:documentation "Instances of this class are classes which instances can be moved
                   and accessed remotely."))

(defclass mobile-direct-slot-definition (standard-direct-slot-definition)
  ((moving-behavior :initarg :moving-behavior :initform ()
	 :accessor slot-definition-moving-behavior)))

(defclass mobile-effective-slot-definition (standard-effective-slot-definition)
  ((moving-behavior :accessor slot-definition-moving-behavior)))


;;; metaclasses for proxies
;;;-------------------------------

(defclass proxy-class (standard-class)
  ())

(defclass effective-local-slot-definition (standard-effective-slot-definition)
  ())

(defclass effective-remote-slot-definition (standard-effective-slot-definition)
  ())  

;; need to validate superclass proxy-class for proxy
(defmethod validate-superclass ((class proxy-class) (superclass standard-class))
  t)

;;; the superclasses for proxies and mobile-objects
;;;------------------------------------------------
(defclass proxy (concurrency-obj) 
  ((remote-address :accessor remote-address
		   :initarg :remote-address)
   (remote-os :accessor remote-os
	      :initarg :remote-os) 
   (masterclass-name :accessor masterclass-name
		     :initarg :masterclass-name)
   (master-pack-string :accessor master-pack-string)
   (other-pack-string :accessor other-pack-string))
  (:metaclass proxy-class))

(defclass mobile-object () 
  ((object-lock :reader object-lock
		:initform (acl-compat-mp:make-process-lock))))


;;; generic-functions that aslo work on proxies
;;;--------------------------------------------
(defclass ncl-gf (sending-gf)
  ()
  (:metaclass funcallable-standard-class))

(defclass ncl-setf (ncl-gf setf-gf)
  ()
  (:metaclass funcallable-standard-class))

;;;--------------------------------------------------------
;;; PROXIES
;;;--------------------------------------------------------



;;; creation and initialization of proxies.
;;;-----------------------------------------

(defun make-proxy (masterclass-name address objspace)
  (make-instance (ensure-proxy-class masterclass-name) 
    :remote-address address
    :remote-os objspace
    :masterclass-name masterclass-name))

(let ((proxy-classes (make-hash-table)))
  (setf (gethash 'function proxy-classes) (find-class 'proxy))
  (defun ensure-proxy-class (masterclass-name)
    (cond ((gethash masterclass-name proxy-classes))
	  ((subtypep masterclass-name 'function) (find-class 'proxy))
	  (t (setf (gethash masterclass-name proxy-classes)
	       (make-instance (ensure-proxy-class-class (class-of (find-class masterclass-name)))
		 :name (intern (format nil "~a~a" 
				       "PROXY-"
				       (symbol-name masterclass-name)))
		 :direct-superclasses (list (find-class 'proxy)
					    (find-class masterclass-name))))
	     (setf (find-class (class-name (gethash masterclass-name proxy-classes)))
	       (gethash masterclass-name proxy-classes))
	     ))))

(defun ensure-proxy-class-class (masterclass-metaclass-name)
  (class-name
   (ensure-class 
    (intern (format nil "~a~a" 
		    "PROXY-METACLASS-"
		    (symbol-name (class-name masterclass-metaclass-name))))
    :direct-superclasses (list (find-class 'proxy-class)
			       (find-class (class-name masterclass-metaclass-name))))))

;;; Der initialisierungs-prozess wird unterbunden, da proxies nur stellvertreter sind
(defmethod initialize-instance :around ((proxy proxy) 
				      &rest initargs)
  (setf (remote-address proxy) (second (member :remote-address initargs)))
  (setf (remote-os proxy) (second (member :remote-os initargs)))
  (setf (masterclass-name proxy) (second (member :masterclass-name initargs)))
  (setf (other-pack-string proxy) (format nil "(np ~a '~a (fos \"~a\"))"
					   (remote-address proxy)
					   (format nil "~S" (masterclass-name proxy))
					   (host (remote-os proxy))))
  (setf (master-pack-string proxy) (format nil "(fetch-obj  ~a)"
					   (remote-address proxy)))
  proxy)

;;; erst nach erzeugung des proxies in objectstore
(defmethod initialize-local-slots ((proxy proxy))
  (let ((local-slot-names (local-slot-names (class-of proxy))))
    (loop for local-slot-name in local-slot-names do
	  (setf (slot-value proxy local-slot-name)
	    (get-slot-value-from-original-object proxy local-slot-name)))))


;;; analog zu slot-value-using-class (s.u.) nur hier genau einmal beim Erzeugen
;;; des proxies.
(defun get-slot-value-from-original-object (proxy local-slot-name)
  ;;; klappt noch nicht, da verklemmung entsteht, wie?
  ;;;(send-now proxy *current-actor* 'slot-value local-slot-name)
  ;;; daher nehme initform
  (initial-slot-value proxy local-slot-name))

	

(defmethod initial-slot-value (class slot)
  nil)



;;; kann ueberdefiniert werden, um spezielle Slots eines Proxies lokal auf jedem client 
;;; zu halten (makro/Kennzeichnung noch notwendig ums deklarativ zu machen)

(defmethod local-slot-names ((class t))
  nil)

(defmethod local-slot-names ((class proxy-class))
  (if (next-method-p) 
      (call-next-method)
    nil))

(defmethod effective-slot-definition-class ((class proxy-class) &rest initargs)
  (let ((existing-definition-class (call-next-method))
	(new-class
	 (find-class (let ((name (second (member :name initargs))))
		       (if (or (eq name 'remote-address)
			       (eq name 'remote-os)
			       (eq name 'masterclass-name)
			       (eq name 'master-pack-string)
			       (eq name 'other-pack-string)
			       (find name (local-slot-names class))
			       )
			   'effective-local-slot-definition
			 'effective-remote-slot-definition)))))	 
    (if (not (eq existing-definition-class (find-class 'standard-effective-slot-definition)))
	(ensure-new-slot-definition-class existing-definition-class 
					      new-class)
      new-class)))



(defmethod initialize-instance :around ((slot effective-remote-slot-definition)
					&rest initargs)
  (apply #'call-next-method 
	 slot 
	 :initfunction ()
					;:allocation :ignore
					;This is as bug according to the MOP-Definition, 
					;You should be able to use allocations other then 
					; :instance or :class. But it conforms with CLtL2. 
	 :initargs ()
	 initargs))
					


;;; the three modes of sending a message to a remote object
;;; -------------------------------------------------------

(defmethod send-future ((proxy proxy) caller method &rest args)
  (send-request (remote-os proxy) 
		(remote-address proxy)
		caller
		method 
		args))

(defmethod send-past ((proxy proxy) caller method &rest args)
  (send-operation  (remote-os proxy) 
		   (remote-address proxy)
		   caller
		   method 
		   args))

(defmethod send-now ((proxy proxy) caller method &rest args)
  (touch (send-request (remote-os proxy) 
		       (remote-address proxy)
		       caller
		       method 
		       args)))


;;; locking can't be handled as a normal method whena pplied to a proxy.
;;; That's because protected objects (the objects that can be locked) specialize
;;; send-now, so this special method would be used for sending, and wouldn't work.
;;;-------------------------------------------------------------------------------
(defmethod lock ((proxy proxy))
  (touch (send-request (remote-os proxy) 
			(remote-address proxy)
			*current-actor*
			'lock
			())))

(defmethod unlock ((proxy proxy)) 
  (touch (send-request (remote-os proxy) 
			(remote-address proxy)
			*current-actor*
			'unlock
			())))
	  

;;; These slot accessor methods may not work with code also using the MOP. The alternative is to
;;; work with reader and writer gf's, but that is pretty inefficient, because it is
;;; difficult to constrain the effect just to proxies.
;;; -------------------------------------------------------------------------------------------
(defmethod slot-definition-initfunction ((slot effective-remote-slot-definition))
  ())

(defmethod slot-value-using-class ((class proxy-class) (proxy proxy)
				   (slot effective-remote-slot-definition))
  (send-now proxy *current-actor* 'slot-value (slot-definition-name slot)))

(defmethod (setf slot-value-using-class) (new-value (class proxy-class) (proxy proxy)
					  (slot effective-remote-slot-definition))
  (send-now proxy *current-actor* '(setf slot-value) new-value (slot-definition-name slot)))

(defmethod slot-boundp-using-class ((class proxy-class) (proxy proxy)
				   (slot effective-remote-slot-definition))
  (send-now proxy *current-actor* 'slot-boundp (slot-definition-name slot)))

(defmethod slot-makunbound-using-class ((class proxy-class) (proxy proxy)
				   (slot effective-remote-slot-definition))
  (send-now proxy *current-actor* 'slot-makunbound (slot-definition-name slot)))


(defmethod slot-missing ((class proxy-class) (proxy proxy)
			 slot  (setslot (eql 'slot-value)) &optional new-value)
  (send-now proxy *current-actor* 'slot-missing slot setslot new-value))


;;; Klassen sind (noch) keine activen objecte, daher geht nicht send-now
;;; ==> active-object auf klassen erweitern
(defmethod slot-missing ((class standard-class) (proxy proxy-class)
			 slot  (setslot (eql 'slot-value)) &optional new-value)
  ;;;(send-now proxy *current-actor* 'slot-missing slot setslot new-value)
  ;;; bis dahin hack:
  ;;; aus concept-id-set-desc.lisp
    (let ((found-slot (find slot (class-slots proxy) :key #'slot-definition-name)))
    (when  found-slot
      (eval (slot-definition-initform  found-slot))))
  )





;;;;--------------------------------------------------------------------
;;;; Initializing of mobile-object classes
;;;;--------------------------------------------------------------------

;;; I don't like all this (ensure-new-slot-definition-class etc.), but it's necessary 
;;; to merge my stuff with  alien MOP stuff.

(defmethod direct-slot-definition-class ((class mobile-object-class) 
					 &rest initargs)
  (declare (ignore initargs))
  (let ((existing-definition-class (call-next-method)))
    (if (not (eq existing-definition-class (find-class 'standard-direct-slot-definition)))
	(ensure-new-slot-definition-class existing-definition-class 
					  (find-class 'mobile-direct-slot-definition))
      (find-class 'mobile-direct-slot-definition))))

(defmethod effective-slot-definition-class ((class mobile-object-class) 
					    &rest initargs)
  (declare (ignore initargs))
  (let ((existing-definition-class (call-next-method)))
    (if (not (eq existing-definition-class (find-class 'standard-effective-slot-definition)))
	(ensure-new-slot-definition-class existing-definition-class 
					  (find-class 'mobile-effective-slot-definition))
      (find-class 'mobile-effective-slot-definition))))


(defun ensure-new-slot-definition-class (existing-class new-class)
  (or (find-class (intern (concatenate 'string 
			    (symbol-name (class-name existing-class))
			    (symbol-name (class-name new-class)))) nil)
            (find-class (intern (concatenate 'string 
			    (symbol-name (class-name new-class))
			    (symbol-name (class-name existing-class)))) nil)
      (let ((new-slot-definition-class
	     (ensure-class (intern (concatenate 'string 
				     (symbol-name (class-name existing-class))
				     (symbol-name (class-name new-class))))
			   :direct-superclasses (list new-class existing-class))))
	(add-method (ensure-generic-function 'moving-behavior)
		    (make-instance 'standard-method
		      :function (compile nil '(lambda (x) (declare (ignore x)) 
					       :follow))
		      :lambda-list (list 'x)
		      :qualifiers ()
		      :specializers (list new-slot-definition-class)))
	new-slot-definition-class)))


(defmethod compute-effective-slot-definition ((class mobile-object-class) name direct-slots)
  (declare (ignore name))
  (let ((effective-slot (call-next-method)))
    (set-moving-behavior class effective-slot (first direct-slots) (rest direct-slots))
    effective-slot))


(defmethod set-moving-behavior ((class mobile-object-class)
				(effective-slot mobile-effective-slot-definition)
				(direct-slot mobile-direct-slot-definition)
				direct-slots)
  (if (slot-definition-moving-behavior direct-slot)
      (setf (slot-definition-moving-behavior effective-slot) 
	(slot-definition-moving-behavior direct-slot))
    (set-moving-behavior class effective-slot (first direct-slots) (rest direct-slots))))



(defmethod set-moving-behavior ((class mobile-object-class)
				(effective-slot mobile-effective-slot-definition)
				(direct-slot direct-slot-definition) 
				direct-slots)
  (declare (ignore direct-slots))
  (if (next-method-p)
      (call-next-method)
    (setf (slot-definition-moving-behavior effective-slot) :ignore)))

(defmethod set-moving-behavior ((class mobile-object-class)
				(effective-slot mobile-effective-slot-definition)
				(direct-slot null)
				direct-slots)
  (declare (ignore direct-slots))
  (setf (slot-definition-moving-behavior effective-slot) :ignore))


#| this code makes all accesors of mobile-objects pargenerics, thus remotely callable.
But that's pretty inefficient, so I don't use it.
(defmethod initialize-instance :before ((slot-definition mobile-direct-slot-definition) 
					  &rest initargs &key readers writers)
  (declare (ignore initargs))
  (loop for reader in readers
      do (ensure-generic-function reader 
				  :generic-function-class (find-class 'ncl-gf)
				  :lambda-list '(obj)))
  (loop for writer in writers
      do (ensure-generic-function writer
				  :generic-function-class (find-class 'ncl-setf)
				  :lambda-list '(new-value obj))))
				  

(defmethod initialize-instance :after ((slot-definition mobile-direct-slot-definition) 
				       &rest initargs)
  (let ((behavior (member :moving-behavior initargs)))
    (when (and behavior
	       (not (or (member (second behavior) '(:stay :follow :ignore))
			(functionp (second behavior)))))
					;does not work, because a functionquoted symbol in the
					;defclass macro
					;is passed as list to the inititialize-instance form.
      (error ":moving-behavior option for slot ~a unknown" 
	     (slot-definition-name slot-definition)))))
|#



;;; supply mobile-object as default superclass for mobile objects
(defmethod initialize-instance :around ((class mobile-object-class) &rest initargs)
  (apply #'call-next-method 
	   class 
	   (if (second (member :direct-superclasses initargs))
	       initargs
	     (cons :direct-superclasses 
		   (cons (list (find-class 'mobile-object))
			 initargs)))))

(defmethod reinitialize-instance :around ((class mobile-object-class) &rest initargs)
  (apply #'call-next-method 
	   class 
	   (if (second (member :direct-superclasses initargs))
	       initargs
	     (cons :direct-superclasses 
		   (cons (list (find-class 'mobile-object))
			 initargs)))))




;;; generate the methods for packing mobile objects
;;; -----------------------------------------------

(defmethod finalize-inheritance :after ((class mobile-object-class)) 
  (pack-method class)
  (unpack-method class))

(defgeneric pack-slots (obj &optional stream))

(defgeneric unpack-slots (obj initargs))




(defun pack-method (class)
  (let ((gf  (ensure-generic-function 'pack-slots)))
    (add-method gf
		(make-instance 'standard-method
		       :function (compile nil (pack-lambda class))
		       :specializers (list class)
		       :qualifiers ()
			 :lambda-list '(obj &optional stream)))))

(defun pack-lambda (class)
  `(lambda (obj &optional stream)
     ,@(loop for slot in (class-slots class)
	   for form = (packform slot)
	   when form 
	   collect (make-unbound-secure slot form))))


(defun make-unbound-secure (slot form)
  (if (slot-definition-initfunction slot)
      form
  `(if (slot-boundp obj ',(slot-definition-name 
			   slot))
       ,form
     (print :unbound stream))))
  
(defun packform (slot)
  (let ((moving-behavior (slot-definition-moving-behavior slot)))
    (cond ((eq moving-behavior :ignore) ())
	  ((eq moving-behavior :stay)
	   `(pack (slot-value obj ',(slot-definition-name slot)) stream))
	  ((eq moving-behavior :follow)
	   `(move-pack (slot-value obj ',(slot-definition-name slot)) stream))
	  (t				; moving-behavior should be a function
					; but it's just (list 'function function-name). 
					;This confuses me a bit, but it still works fine.
	   `(funcall ,moving-behavior 
		     (slot-value obj ',(slot-definition-name slot)) stream)))))


(defun unpack-method (class)
  (let ((gf  (ensure-generic-function 'unpack-slots)))
    (add-method gf
		(make-instance 'standard-method
		       :function (compile nil (unpack-lambda class))
		       :specializers (list class (find-class t))
		       :qualifiers ()
		       :lambda-list '(obj initargs)))))

(defun unpack-lambda (class)
  `(lambda (obj initargs)
     ,@(loop for slot in (class-slots class)
	   for behavior = (slot-definition-moving-behavior slot)
	   unless (eq behavior :ignore)
	   collect (unbound-secure-write slot))))

(defun unbound-secure-write (slot)
  (if (slot-definition-initfunction slot)
      `(progn 
	 (setf (slot-value obj ',(slot-definition-name slot)) (first initargs))
	 (setq initargs (rest initargs)))
    `(let ((value (prog1 (first initargs)
		    (setq initargs (rest initargs)))))
       (unless (eq value :unbound)
	 (setf (slot-value obj ',(slot-definition-name slot))
	   value)))))


;;; ---------------------------------------------------------------------------------------
;;; move-pack packs a form on a stream that, when evaluated, yields an object wtih the same 
;;; structure and content as the original object.
;;; --------------------------------------------------------------------------------------

(defmethod move-pack :around ((obj t) &optional stream)
  (let (temp)
    (if (setq temp (assoc obj *pack-forms*))
	(print (cdr temp) stream)
      (call-next-method))))

(defmethod move-pack ((obj mobile-object) &optional stream)
  (let ((reference (gentemp "I")))
    (setq *pack-forms* (acons obj reference
			      *pack-forms*))
    (format stream "(progn (proclaim '(special ~a))
		                 (prog1 (setq ~a (make-instance '~S))
		                    (unpack-slots ~a (list"
	    reference reference (class-name (class-of obj)) reference)
    (pack-slots obj stream)
    (write-string "))))" stream)))

(defmethod move-pack ((proxy proxy) &optional stream)
  (pack proxy stream))

(defmethod move-pack ((obj t) &optional stream)
  (pack obj stream))

(defmethod move-pack ((obj cons) &optional stream)
  (write-string "(cons " stream)
  (write-char #\space stream)
  (move-pack (car obj) stream)
  (write-char #\space stream)
  (move-pack (cdr obj) stream)
  (write-char #\) stream))

(defmethod move-pack ((struc structure-object) &optional stream)
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
		  (move-pack (funcall (fdefinition (intern (format nil "~a-~a"
								   class-name
								   name)
							   class-package))
				 struc)
			stream)))
    (write-char #\) stream)))

(defmethod move-pack ((array array) &optional stream)
  (write-string "(unpack-array " stream)
  (pack (array-dimensions array) stream)
  (write-char #\space stream)
  (pack (array-element-type array) stream)
  (write-char #\space stream)
  (loop for i from 0 upto (1- (array-total-size array))
      do (progn (move-pack (row-major-aref array i) stream)
		(write-char #\space stream)))
  (write-char #\) stream))

;;;mu = move-unpack
(defun mu (type &rest initargs)
  (let ((obj (make-instance type)))
    (unpack-slots obj initargs)
    obj))



;;;----------------------------------------
;;; initialization of ncl-gf's
;;;----------------------------------------

(defmethod initialize-instance :after ((gf ncl-gf) &rest initargs)
  (declare (ignore initargs))
  (add-method gf
	      (make-instance 'standard-method
		:function (compile nil '(lambda (&rest rest)
					 (declare (ignore rest))
					 t))
		:specializers (compute-specializers gf 'proxy)
		:qualifiers '(:test)
		:lambda-list (generic-function-lambda-list gf))))

(defmethod reinitialize-instance :after ((gf ncl-gf) &rest initargs)
  (declare (ignore initargs))
  (add-method gf
	      (make-instance 'standard-method
		:function (compile nil '(lambda (&rest rest)
					 (declare (ignore rest))
					 t))
		:specializers (compute-specializers gf 'proxy)
		:qualifiers '(:test)
		:lambda-list (generic-function-lambda-list gf)))) 

 

;;;-----------------------------------------
;;; other stuff
;;;-----------------------------------------

;;; touch must be a pargeneric to work across objspace-boundaries,
;;; only it doesn't even work then. 
;;(defpargeneric touch :now (future))

;;; create objects in other objectspaces
(defmethod make-instance :around ((class mobile-object-class) &rest initargs &key location)
  (if location  
      (touch (kernel-send location  
			  (rmi-message :initargs-b (copy-list initargs)
					; lists introduces through the rest argument
					; may have dynamic extent, so they must be copied. 
				       :class-name (class-name class))))
    (call-next-method)))

;;; determine the location of an object
(defmethod location ((obj t))
  *localspace*)

(defmethod location ((obj proxy))
  (remote-os obj))






#| old stuff, but I hate throwing it away.

(defun get-optionals (lambda-list)
  (loop for non-req in (rest (member '&optional 
				     lambda-list))
      while (not (member non-req lambda-list-keywords))
      collect non-req))

(defun keys-or-restp (lambda-list)
  (member '(&key &rest)
	  lambda-list :test #'(lambda (x y)
				(member y x))))



(defmethod compute-proxy-lambda ((gf remote-generic-function) lambda-list)
  (let ((required (required-arguments gf))
	(optionals (mapcar #'first (get-optionals lambda-list)))
	(suppliedp-list (mapcar #'third (get-optionals lambda-list)))
	(rest-arg (when (keys-or-restp lambda-list)
		    (first (last lambda-list)))))
    `(lambda ,lambda-list
       (apply #'send-now ,(first required)
	      ',(generic-function-name gf)
	      ,@(rest required)
	      ,(non-required-arguments optionals suppliedp-list rest-arg)))))


(defun non-required-arguments (optionals suppliedp-list rest-arg)
  (if (endp optionals) 
      rest-arg
    `(if ,(first suppliedp-list)
	 (cons ,(first optionals) ,(non-required-arguments (rest optionals)
							   (rest suppliedp-list)
							   rest-arg))
       ,rest-arg)))

(defun compute-lambda-list (gf)
  (let* ((optionals (get-optionals (generic-function-lambda-list gf)))
	 (suppliedp-list (loop for arg in optionals
			     collect (gensym (symbol-name arg)))))
    (append (required-arguments gf)
	    (when optionals
	      '(&optional))
	    (loop for opt in optionals 
		for sup in suppliedp-list
		collect (list opt nil sup))
	    (when (keys-or-restp  (generic-function-lambda-list gf))
	      (list '&rest (gensym "REST-"))))))




(defclass proxy (concurrency-obj)
  ((remote-address :accessor remote-address
		   :initarg :remote-address)
   (remote-os :accessor remote-os
	      :initarg :remote-os)
   (master-class-name :accessor master-class-name
		      :initarg :master-class-name)))



(defmethod compute-applicable-methods-using-classes ((gf proxy-dispatching-gf) classes)
  (if (loop for class in (without-concurrency-obj gf classes)
	  thereis (eq (class-name class) 'proxy))
      (values nil nil)
    (call-next-method)))

(defmethod compute-applicable-methods ((gf proxy-dispatching-gf) arguments)
  (let* ((proxyp)
	 (classes (unite-concurrency-obj-and-others 
		   gf
		   (class-of (get-concurrency-obj gf arguments)) 
		   (loop with proxy-class = (find-class 'proxy)
		       for arg in (without-concurrency-obj gf arguments)
		       collect (if (eq (class-of arg)
				       proxy-class)
				   (progn (setf proxyp t)
					  (find-class (master-class-name arg)))
				 (class-of arg))))))
    (if proxyp
	(multiple-value-bind (method-list success)
	    (compute-applicable-methods-using-classes gf classes)
	  (if success
	      method-list
	    (call-next-method)))
      (call-next-method))))

(defmethod compute-effective-slot-definition ((class proxy-class) name direct-slots)
  (declare (ignore name))
  (let ((effsd (call-next-method)))
    effsd))


(defmethod  set-correct-behavior ((effsd  effective-remote-slot-definition))
  (setf (slot-definition-allocation effsd) :ignore)
  (setf (slot-definition-initfunction effsd) ())
  (setf (slot-definition-initargs effsd) ()))

(defmethod slot-definition-allocation ((slot effective-remote-slot-definition))
  :ignore)				;This doesn't work either.
  
|#




