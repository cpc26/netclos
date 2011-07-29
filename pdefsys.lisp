;;;-----------------------------------------------------------------------------------
;;; name       : pdefsys
;;; description: For netclos to work all object spaces got to have consistent global 
;;;              environments. All definitions in effect in any space have to be also in effect
;;;              in all others. (You may ommit some defintions in the remote object spaces, 
;;;              but you have then to be sure that they won't be used in those spaces.)
;;;              The main tool to achieve this is defpsystem, which works like defsystem,
;;;              but when such a system is loaded, it is loaded into all spaces. 
;;;              The other possibillity is set *defremote-p* to t and use defun etc.. But be 
;;;              careful: This only works in the netclos  package, and it only works 
;;;              immediatly. Definitons aren't automatically send to objects spaces created 
;;;              later. And it won't work with closures (you'll get an error.)
;;; notes      :
;;; contact    : me (Michael Trowe)
;;; copyright  :
;;; history    : 
;;; contents   : defpsystem and special versions of defun, defclass, defgeneric and defmethod.
;;;-----------------------------------------------------------------------------------

(in-package nc)

(defvar *defremote-p* nil)

;; (defmacro defpsystem (name options &rest specifications)
;;   `(progn (add-system *manager*
;; 		      ',name ',options ',specifications)
;; 	  (excl:defsystem ,name ,options ,@specifications)))

;; (defun load-psystem (name &rest args)
;;   (let ((system (assoc name (systems *manager*)))
;; 	(*defremote-p* nil))
;;     (apply #'excl:load-system name args)
;;     (when (and *master-p* system) 
;;       (loop for space in (spaces *manager*)
;; 	  do (kernel-send space
;; 			  (system-loaded-message :system-name name
;; 						  :load-args args)))
;;       (setf (cadddr system) :loaded))))

;; (defun compile-psystem (name)
;;   (let ((*defremote-p* nil))
;;     (excl:compile-system name)))
	    
(defmacro defpsystem (name options &rest specifications)
  `(progn (add-system *manager*
		      ',name ',options ',specifications)
	  (asdf:defsystem ,name ,options ,@specifications)))

(defun load-psystem (name &rest args)
  (let ((system (assoc name (systems *manager*)))
	(*defremote-p* nil))
    (apply #'asdf:operate 'asdf:load-op name args)
    (when (and *master-p* system) 
      (loop for space in (spaces *manager*)
	  do (kernel-send space
			  (system-loaded-message :system-name name
						  :load-args args)))
      (setf (cadddr system) :loaded))))

(defun compile-psystem (name)
  (let ((*defremote-p* nil))
    (asdf:operate 'asdf:compile-op name)))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'defun))

(cl:defmacro defun (&rest rest)
  `(progn (eval-when (:execute)
	    (when (and *manager* *defremote-p*)
	      (loop for space in (spaces *manager*)
		 do (remote-eval space '(cl:defun ,@rest)))))
	  (cl:defun ,@rest)))

(define-compiler-macro defun (&rest rest)
  `(cl:defun ,@rest))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'defclass))

(cl:defmacro defclass (&rest rest)
  `(progn (eval-when (:execute)
	    (when (and *manager* *defremote-p*)
	      (loop for space in (spaces *manager*)
		  do (remote-eval space '(cl:defclass ,@rest)))))
	  (cl:defclass ,@rest)))	

(define-compiler-macro defclass (&rest rest)
  `(cl:defclass ,@rest))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'defmethod))

(cl:defmacro defmethod (&rest rest)
  `(progn (eval-when (:execute)
	    (when (and *manager* *defremote-p*)
	      (loop for space in (spaces *manager*)
		  do (remote-eval space '(cl:defmethod ,@rest)))))
	  (cl:defmethod ,@rest)))	

(define-compiler-macro defmethod (&rest rest)
  `(cl:defmethod ,@rest))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'defgeneric))

(cl:defmacro defgeneric (&rest rest)
  `(progn (eval-when (:execute)
	    (when (and *manager* *defremote-p*)
	      (loop for space in (spaces *manager*)
		  do (remote-eval space '(cl:defgeneric ,@rest)))))
	  (cl:defgeneric ,@rest)))	

(define-compiler-macro defgeneric (&rest rest)
  `(cl:defgeneric ,@rest))





