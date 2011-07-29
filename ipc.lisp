;(require 'netclos)
(in-package nc)
;(use-package '(sb-simple-streams))
;;; Client
;;;
(defclass meta-tcp-client (standard-class) ())

(defmethod validate-superclass ((class meta-tcp-client) (superclass standard-class))
  t)

;;; Now this class can be confusing,
;;; it seems to be a character stream, however it supports
;;; multivalent read-byte and write-byte as well for HTTP convenience
;;;
(defclass tcp-client-stream (sb-simple-streams:socket-simple-stream)
  (
   (remote-host :initform nil)
   (remote-port :initform nil)
;   (listen-sockaddr :initform nil :initarg :listen-sockaddr)
   (process :initform nil :accessor tcp-stream-process)
   (alive :initform t :accessor tcp-client-alive))
  (:metaclass meta-tcp-client))

;; (defmethod make-instance ((class meta-tcp-client) &rest args &key host port &allow-other-keys)
;;   )

(defmethod tcp-client-alive ((stream t))
  nil)

(defmethod close :around ((stream tcp-client-stream) &key (abort t))
  ;; When remote end aborts stream while we are transfering
  ;; closing the stream can cause a Signal 13 on our end.
  (setf (tcp-client-alive stream) nil)
  (call-next-method))

(defun tcp-destroy-process (stream)
  (let ((process (tcp-stream-process stream)))
    ;; If closing multiple times
    (if process
        (acl-compat-mp:process-kill process)))
  (setf (tcp-stream-process stream) nil))


;; (setq a (make-instance 'tcp-client-stream :remote-host #(127 0 0 1) :remote-port 6600))


;; (setq b (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))

;; (sb-bsd-sockets:socket-connect b #(127 0 0 1) 6600)

;; (setq a (make-instance 'tcp-client-stream :remote-socket b :direction :io))
;; (close a)

;; (read-line a)

;; (progn
;;   (format a "hello~%")
;;   (force-output a))

;; (tcp-client-alive a)


;;; Server
;;;
(defclass meta-tcp-server (standard-class) ())

(defmethod validate-superclass ((class meta-tcp-server) (superclass standard-class))
  t)

(defclass tcp-server-stream ();socket-simple-stream)
  ((host :initarg :host :accessor host) ;This is the local host
   (port :initarg :port :accessor port)
;   (listen-socket-fd :initarg :fn-in)
;   (listen-sockaddr :initarg :listen-sockaddr)
   (socket :initform nil :accessor tcp-server-socket)
   (client-streams :initform nil)
   (process :initform :initializing :accessor tcp-stream-process))
  (:metaclass meta-tcp-server))

;(defvar *tcp-server-terminate* nil)

;; (defmethod make-instance :around ((class tcp-server-stream) &key host port);&rest initargs &key host port &allow-other-keys)
;;   (format t "make instance 'tcp-server-stream ~A :host ~A :port ~A~%" class host port)
;;   (setf (slot-value class 'host) host
;; 	(slot-value class 'port) port)
;;   (initialize-tcp-server-process class)
;; ;  (call-next-method)
;;   class
;; )

;(setq a (make-instance 'tcp-server-stream :host "localhost" :port 8223))

(defmethod close #|:around|# ((stream tcp-server-stream) &key (abort t))
  (with-slots (process) stream
    (setq process nil))
  (acl-compat-mp:process-allow-schedule)
  (with-slots (client-streams) stream
    (mapc #'(lambda (client)
	      (close (cdr client)))
	  client-streams)))

(defmethod initialize-tcp-server-process ((stream tcp-server-stream))
  (format t "initialize-tcp-server-process ~A~%" stream)
  (let ((server-name (format nil "TCP Server [~a]" (slot-value stream 'port)))
        proc)
    (setq proc (acl-compat-mp:process-run-function server-name
						   #'tcp-server-daemon-function
						   stream))
    (setf (tcp-stream-process stream) proc)
    stream))

;;; Define useful errors
;;;
(define-condition protocol-error (error)
  ((stream :initform nil :initarg :stream)))

(defun nslookup (hostname)
   "Performs a DNS look up for HOSTNAME and returns the address as a
   four element array, suitable for socket-connect.  If HOSTNAME is
   not found, a host-not-found-error condition is thrown."
   (if hostname
       (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name hostname))
       nil))

(defmethod tcp-server-daemon-function ((stream tcp-server-stream))
  (handler-case
      (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
	    (host (host stream))
	    (port (port stream))
	    (client nil))
	(setf (tcp-server-socket stream) socket)
	(setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
	(setf (sb-bsd-sockets:non-blocking-mode socket) t)
	(format t "bind~%") 
	(sb-bsd-sockets:socket-bind socket (nslookup host) port)
	(format t "listen~%") (force-output)
	(sb-bsd-sockets:socket-listen socket 5)
;	(loop until *tcp-server-terminate* do
	(unwind-protect
	     (block nil
	       (handler-case
		   (loop
		      (cond ((tcp-stream-process stream)
			     (setq client (sb-bsd-sockets:socket-accept socket))
;			     (format t "accepted ~A~%" client)
;			     (force-output)
			     (cond ((null client)
				    (sleep 0.1))
				   
				   ((< (sb-bsd-sockets:socket-file-descriptor client) 0)
				    (format t "Accept failure~%")
				    (force-output)
				    (acl-compat-mp:without-scheduling
				      (push (cons 'protocol-error nil)
					    (slot-value stream 'client-streams))))
				   (t
				    (format t "accepted ~A~%" client)
				    (force-output)
				    (acl-compat-mp:without-scheduling
				      (push (cons client t);(sb-bsd-sockets:socket-make-stream client :input t :output t :buffering :none))
					    (slot-value stream 'client-streams))))))
			    (t
			     (return))
			    ))
		 (sb-bsd-sockets:interrupted-error ()
		   (sleep 0.1)))))
	(format t "exit") (force-output)
	(sb-bsd-sockets:socket-close socket))
	
    (sb-bsd-sockets:address-in-use-error ()
      (format t "address ~A : ~A is already in use"  
	      (host stream) (port stream))
      (force-output)
      nil)))


(defmethod stream-read ((stream tcp-server-stream))
  (with-slots (client-streams) stream
    (destructuring-bind (client . client-stream)
	(acl-compat-mp:without-scheduling (pop client-streams))
      (when client
	(if (symbolp client) ;Handle future protocol errors
	    (error client :stream stream))
;; 	(let ((host-address (sockaddr-in-addr listen-sockaddr))
;; 	      host
;; 	      (port (sockaddr-in-port listen-sockaddr)))
;; 	  (setq host (handler-case (get-host-name-by-address host-address)
;; 		       (unknown-host-name () host-address)))
;; 	  ;; Open "" can fail here?
;(format t "tcp-client ~A~%"	
	(make-instance 'tcp-client-stream :remote-socket client :direction :io)))))
;)

;; (setq a (make-instance 'tcp-server-stream :host "localhost" :port 8223))

;; (setq b (tcp-server-daemon-function a))

;; (close a)

;; (setq x1 (stream-read a))

;; (progn
;;   (format x1 "hello, this is ~A ~A~%" (lisp-implementation-type) (lisp-implementation-version)) 
;;   (force-output x1))

;; (read-line x1)

;; (defvar *term* nil)

;; (loop unless *term* do
;;      (format x1 "> ") (force-output x1)
;;      (handler-case
;; 	 (format x1 "~A~%" (eval (read x1)))
;;        (error (err)
;; 	 (format x1 "~A~%" err)))
;;      (force-output x1))

;; (setq *term* t)

;; (close x1)
;; ;(setq *tcp-server-terminate* t)
;; ;(sb-bsd-sockets:socket-close (tcp-server-socket a))


;; (sb-bsd-sockets:non-blocking-mode (tcp-server-socket a))

;; (read-line d)

;; (format d "hello too~%")

;; (type-of d)
