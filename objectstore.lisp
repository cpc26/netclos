;;;-----------------------------------------------------------------------------------
;;; name       : objectstore
;;; description: 
;;; notes      :
;;; contact    : me (Michael Trowe)
;;; copyright  :
;;; history    : 
;;; contents   :
;;;-----------------------------------------------------------------------------------
(in-package nc)

(defclass objstore ()
  ((storage :reader storage
	    :initform (make-hash-table)
	    :initarg :storage)
   (back-map :reader back-map
	     :initform (make-hash-table)
	     :initarg :back-map)
   (next-address :accessor next-address
		 :initform 0
		 :initarg :next-address)
   (proxies :accessor proxies 
	    :initform (make-hash-table :test #'equal)
	    :initarg :proxies)))
   



(defmessage send-notifier (address destination)
  :receive-action (store-destination (objstore *manager*)
				     address
				     destination))

(defmessage reference-revoked (address)
  :receive-action (let* ((store (objstore *manager*))
			 (exports (when(cdr (gethash address (storage store)))
				    (setf (cdr (gethash address (storage store)))
				    (delete *calling-os*
					    (cdr (gethash address (storage store))))))))
		    (unless exports 
		      (remhash (fetch-obj address) (back-map store))
		      (remhash address (storage store)))))



(defun ensure-exported (obj &optional (destination *current-actor*))
  (with-slots (back-map storage next-address) (objstore *manager*)
    (let ((address-and-packform (gethash obj back-map)))
      (if address-and-packform
	  (store-destination  (objstore *manager*) (car address-and-packform) destination)
	(let ((address (setf next-address (1+ next-address))))
	  (setf (gethash address storage) (list obj destination))
	  (setf address-and-packform 
	    (setf (gethash obj back-map) 
	      (cons  address
		     (format nil "(np  ~a '~a)"
			     address
			     (format nil "~S"
				     (class-name (class-of obj)))))))))
      (values (car address-and-packform)
	      (cdr address-and-packform)))))
    
  
(defun fetch-obj (address)
  (car (gethash address (storage (objstore *manager*)))))

(defun print-storage ()
  (print-hash (storage (objstore *manager*))))

(defun print-hash (hashtable)
  (maphash #'(lambda (k v) (print (list k v)))
	   hashtable))

(defun notify-sending (proxy)
  (kernel-send (remote-os proxy)
	    (send-notifier-message 
	      :address (remote-address proxy)
	      :destination *current-actor*)))
  
(defmethod notify-proxy ((proxy proxy))
  (or (get-proxy (objstore *manager*) proxy)
      (store-proxy (objstore *manager*) proxy)))

(defmethod notify-proxy :after ((proxy proxy))
  (initialize-local-slots proxy)
  )


(defun notify-gc (proxy)
  (let ((address (remote-address proxy))
	(os (remote-os proxy)))
    (when address
      (remhash (cons address os)
	       (proxies (objstore *manager*)))
      (let ((*current-actor* ()))
	(kernel-send os
		     (reference-revoked-message
		      :address address))))))
	    


    
(defmethod get-proxy ((store objstore) proxy)
  (let ((pointer (gethash (cons (remote-address proxy)
				(remote-os proxy))
			  (proxies store))))
    (when pointer
	  (aref pointer 0))))
 
		
	    
(defmethod store-proxy ((store objstore) proxy)
  (setf (gethash (cons (remote-address proxy) (remote-os proxy))
		 (proxies store))
    (make-array 1 :weak t :initial-element proxy))
  #+allegro
  (excl:schedule-finalization proxy #'notify-gc)
  #+sbcl
  (sb-ext:finalize proxy #'notify-gc)
  proxy)


(defmethod store-destination ((store objstore) address 
			      destination)
  (pushnew destination (cdr (gethash address (storage store)))))


(defmethod reset ((store objstore))
  (maphash #'(lambda (key proxy-pointer)
	       (declare (ignore key))
	       (kill (aref proxy-pointer 0)))
	   (proxies store))
  (reinitialize-instance store
			 :storage (make-hash-table)
			 :back-map (make-hash-table)
			 :next-address 0
			 :proxies (make-hash-table :test #'equal)))

(defmethod kill ((proxy proxy))
  (setf (remote-address proxy) ()))






