;;-----------------------------------------------------------------------------------
;;; name       : ncl-macros
;;; description: all the macros needed for netclos
;;; notes      :
;;; contact    : me (Michael Trowe)
;;; copyright  :
;;; history    : 
;;; contents   :
;;;-----------------------------------------------------------------------------------

(in-package nc)

(defstruct request message-id)

(defun fetch-names (args)
  (loop for arg in args
      collect (if (listp arg)
		  (first arg)
		arg)))

(defmacro defmessage (name args &key receive-action request-p process-p)
  (if request-p
      (create-request name (fetch-names args) args receive-action process-p)
    (create-operation name (fetch-names args) args  receive-action process-p)))

(defun create-request (name arg-names args body process-p)
  (let* ((sym (gensym "message-id"))
	 (body `(kernel-send *calling-os* 
			     (reply-message :content ,body
					    :message-id ,sym))))
    `(progn 
       ,(create-struct name arg-names :super 'request)
       ,(create-defun name (cons sym arg-names) body process-p) 
       ,(create-pack name (cons 'message-id arg-names) (cons 'message-id args)))))

(defun create-operation  (name arg-names args body process-p)
  `(progn 
     ,(create-struct name arg-names)
     ,(create-defun name arg-names body process-p)
     ,(create-pack name arg-names args)))


(defun create-struct (name args &key super)
  (if super
       `(defstruct (,name 
		    (:constructor ,(intern (format nil "~a-MESSAGE" name)))
		    (:include ,super)) 
	  ,@args)
     `(defstruct (,name 
		  (:constructor ,(intern (format nil "~a-MESSAGE" name)))) 
	,@args)))

(defun create-defun (name arg-names body process-p)
  `(defun ,name ,arg-names ,(if process-p
				`(acl-compat-mp:process-run-function ,(format nil "receiving ~a" name)
							  #'(lambda (*calling-os*)
							      ,body)
							  *calling-os*)
			      body)))

(defvar *pack-forms* ())

(defun create-pack (name arg-names args)
  `(defmethod pack ((message ,name) &optional stream)
     (let ((*pack-forms*))
       (write-string ,(concatenate 'string "("  (write-to-string name) " ") stream)
       ,@(loop for arg in args
	     for arg-name in arg-names
	     for packfunc = (if (and (listp arg) 
				     (eq (second arg) :packing))
				(third arg)
			      'pack) 
	     collect `(,packfunc (,(intern 
				    (format nil "~a-~a" name arg-name)) 
				  message)
				 stream))
       (write-char #\) stream))))

;;; mit *thee-agg zum testen von env
#|
(defun create-pack (name arg-names args)
  `(defmethod pack ((message ,name) &optional stream)
     (let ((*pack-forms*))
       (format stream
		"(let ((mks::*the-aggregate* 'pol) (declare (special mks::*the-aggregate*))")
       (write-string ,(concatenate 'string "("  (write-to-string name) " ") stream)
       ,@(loop for arg in args
	     for arg-name in arg-names
	     for packfunc = (if (and (listp arg) 
				     (eq (second arg) :packing))
				(third arg)
			      'pack) 
	     collect `(,packfunc (,(intern 
				    (format nil "~a-~a" name arg-name)) 
				  message)
				 stream))
       (write-char #\) stream)
       (write-char #\) stream)
       )))
|#
#|
(shadow 'defgeneric)

(defmacro defgeneric (name second &rest rest)
  (if (member second '(:past :now :future))
      `(defpargeneric ,name ,second ,@rest)
    `(common-lisp:defgeneric ,name ,second ,@rest)))
|#


(defmacro defpargeneric (name send-func lambda-list &rest options)
  ;; I wanted to use defgeneric, but you can't add your own keyword arguments then.
  ;; So now there is no source-file-recording and fewer compile-time error-checking.
  `(progn (when (and *manager* *defremote-p*)
	    (loop for space in (spaces *manager*)
		do (remote-eval space 
				'(let ((*defremote-p* nil)) 
				  (defpargeneric ,name ,send-func ,lambda-list ,@options)))))
	  (ensure-generic-function 
	   ',name 
	   :lambda-list ',lambda-list
	   :generic-function-class (find-class ',(compute-gf-class 
						  name 
						  (second (assoc :generic-function-class 
								 options))))
	   :send-func ,(compute-send-func send-func)
	   ,@(compute-options options))))

(defun make-args (lambda-list)
  (loop for arg in lambda-list
      unless (member arg lambda-list-keywords)
      collect arg))

(defun compute-send-func (name)
  (if (member name '(:past :now :future))
      (list 'function (intern (format nil "SEND-~a" name) :nc))))

(defun compute-options (optionlist)
  (loop for opt in optionlist
      unless (eq (first opt) :generic-function-class)
      append opt))

(defun compute-gf-class (name class)
  (if (and (listp name) (eq (first name) 'setf))
      (cond ((not class) 'ncl-setf)
	    ((and (subtypep class 'sending-gf) (subtypep class 'setf-gf)) class)
	    (t (error "a generic-function-class used in defpargeneric for a setf form must be a subclass of sending-gf and setf-gf")))
     (cond ((not class) 'ncl-gf)
	    ((subtypep class 'sending-gf) class)
	    (t (error "a generic-function-class used in defpargeneric must be a subclass of sending-gf")))))
    
#|
I want to supply automatic creation of packing methods for structures here

(shadow 'defstruct)

(defmacro defstruct (name &rest rest)
  (let ((class-name (cond ((listp name) (first name))
			(t name))))))
	
|#
