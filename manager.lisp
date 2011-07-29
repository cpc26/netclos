;;--------------------------------------------------------------------------------
;;; name       :
;;; description:
;;; notes      :
;;; contact    : me (Michael Trowe)
;;; copyright  :
;;; history    :
;;; contents   :
;;;--------------------------------------------------------------------------------
(in-package nc)

(shadow 'stream)

(defvar *image* "/usr/bin/sbcl");"/opt/cl/acl-5-01/climcomposer")
(defvar *tmp-dir* "/tmp");;(namestring (translate-logical-pathname "nclos:;tmp")))
(defvar *starter* "ncl-starter");(namestring (translate-logical-pathname "nclos:;ncl-starter")))
(defvar *port* 8122)
(defvar *master-port* ())
(defvar *master-host* ())
;(defvar *master-host* #(127 0 0 1))
;(defvar *master-port* 8122)
(defvar *master-p* t)
(defvar *local-host*)
(defvar *manager* ())
(defvar *init-file* "ld-actors.sys");(namestring (translate-logical-pathname "nclos:;ld-actors.sys")))
(defvar *debug* t)
(defvar *localspace* ())
;; #+ALLEGRO-V5.0.1
;; (let ((EXCL:*ENABLE-PACKAGE-LOCKED-ERRORS* nil))
;; (defclass manager ()
;;   ((stream :accessor stream
;; 	   :initform (make-server-stream))
;;    (spaces :accessor spaces
;; 	   :initform ())
;;    (systems :accessor systems
;; 	    :initform ())
;;    (receivers :accessor receivers
;; 	      :initform ())
;;    (objstore :accessor objstore
;; 	     :initform (make-instance 'objstore)))))

;#-ALLEGRO-V5.0.1
(defclass manager ()
  ((stream :accessor stream
	   :initform (make-server-stream))
   (spaces :accessor spaces
	   :initform ())
   (systems :accessor systems
	    :initform ())
   (receivers :accessor receivers
	      :initform ())
   (objstore :accessor objstore
	     :initform (make-instance 'objstore))))

(defmessage space-added (new-space)
  :receive-action (setf (spaces *manager*)
		    (cons new-space (spaces *manager*))))

(defmessage system-added (name options specifications)
  :receive-action (eval `(defpsystem ,name ,options ,@specifications)))

;;; Initialize the new objspaces just started:
;;; - Establish a connection to those spaces which won't do it themselves.
;;; - Accept the connections of those spaces which try to establish one.
;;; - Receive the systems: define them and load them, if necessary.
(defmessage init (passive-hosts number-of-hosts systems)
  :receive-action (progn
		    (loop for host-and-port in passive-hosts
		        do (establish-connection (car host-and-port) (cdr host-and-port)))
		    (connect-to-spaces (- number-of-hosts
					  (length passive-hosts)
					  1))
					; minus 1, because you have to subtract the
					; local space.
		    (setf (systems *manager*) systems)
		    (loop for system in systems
;			do (eval `(excl:defsystem ,(first system)
		       do (eval `(asdf:defsystem ,(first system)
				   ,(second system) ,@(third system))))
		    (loop for system in systems
			when (eq (fourth system) :loaded)
;			do (excl:load-system (first system)))))
		       do (asdf:operate 'asdf:load-op (first system)))))

(defmessage system-loaded (system-name load-args)
;  :receive-action (apply #'excl:load-system system-name load-args))
  :receive-action (apply #'asdf:operate 'asdf:load-op system-name load-args))


(defun start-virtual-machine (hostlist &key (image *image*)
					    form)
  (start-spaces image form hostlist)
  (connect-to-spaces (length hostlist))
  (initialize-spaces (spaces *manager*)))

;;; (*) Sonst wird (load "ld-actors") geschickt, was nicht evaluiiert
;;; werden kann, wenn rsh in fi-subproc.el (emacs) gewaehlt wird
;;; wenn dort local erkannt wird, sind die #\\ falsch.
(defvar *string-char-handling* nil)

(defun start-spaces (image form hostlist)
;  (let ((unique-lisp-file-ext (gentemp 'F)))
  (let ((unique-lisp-file-ext (gentemp "F")))
    (when (not (probe-file *tmp-dir*))
      (asdf:run-shell-command (format nil "mkdir ~S" *tmp-dir*)))
    (with-open-file (stream (concatenate 'string *tmp-dir* "/tmp"
					 (string unique-lisp-file-ext)
					 ".lisp")
		     :if-exists :supersede
		     :direction :output
		     :if-does-not-exist :create)
      (setq form
;; 	`(let ((*terminal-io* excl::*initial-terminal-io*)
;; 	       (*query-io* excl::*initial-terminal-io*))
	`(let ((*terminal-io* cl::*terminal-io*)
	       (*query-io* cl::*terminal-io*))
	   (load  ,*init-file*)
	   (eval (list 'progn
		       (list 'setf (intern "*MASTER-HOST*" "NC")
			     ,*local-host*)
		       (list 'setf (intern "*MASTER-P*" "NC") nil)
		       (list 'setf (intern "*PORT*" "NC") 8130)
		       (list 'setf (intern "*MASTER-PORT*" "NC") ,*port*)

		       (list (intern "ENSURE-OS" "NC"))))
	   ,form))
      (print form)
      (let ((*package* (find-package "COMMON-LISP-USER")))	;"USER"))
      ;;; (*)

	(loop for char across

	      (format nil "~S~% ~%" form)
	    do
	      (when (and
		     *string-char-handling*
		     (char= char #\"))
		(write-char #\\ stream))
	      (write-char  char stream))))

    (loop for host in hostlist
	do
	  (start-image image host unique-lisp-file-ext))))

;; #+ALLEGRO-V5.0
;; (defun connect-to-spaces (number-of-spaces)
;;   (loop for x from 1 to number-of-spaces
;;       do (mp:wait-for-input-available (stream *manager*))
;;       do (connect-to-space (stream *manager*))))

(defun connect-to-spaces (number-of-spaces)
  (loop for x from 1 to number-of-spaces
      do (acl-compat-mp:wait-for-input-available (stream *manager*))
      do (connect-to-space (stream-read (stream *manager*)))))

(defun connect-to-space (stream &optional (host (read stream)) (port (read stream)))
  (let ((space  (make-instance 'objspace
		  :host host
		  :port port
		  :stream stream)))
    (push space (spaces *manager*))
    (push (make-instance 'receiver :objspace space :stream stream)
	  (receivers *manager*))))

(defun initialize-spaces (spacelist)
  (loop with number-of-hosts =(length spacelist)
      for spaces on spacelist
      while spaces
      do (kernel-send (first spaces)
		      (init-message :passive-hosts (mapcar #'(lambda (space)
							       (cons (host space)
								     (port space)))
							   (rest spaces))
				    :number-of-hosts number-of-hosts
				    :systems (systems *manager*)))))

(defun start-image (image host &optional (unique-lisp-file-ext (gentemp)))
  (format t "sh ~a '~a ~a ~a ~a ~a ~a ~a' &"
				  host
				  *starter*
				  (if *debug*
				      "emacs"
				    "raw")
				  image
				  #+allegro-cl(sys:getenv "DISPLAY")
				  #+sbcl(sb-ext:posix-getenv "DISPLAY")
				  *tmp-dir*
				  (gentemp "E");'E)
				  unique-lisp-file-ext
				  )
  (asdf:run-shell-command (format nil "sh ~a '~a ~a ~a ~a ~a ~a ~a' &"
				  host
				  *starter*
				  (if *debug*
				      "emacs"
				    "raw")
				  image
				  #+allegro-cl(sys:getenv "DISPLAY")
				  #+sbcl(sb-ext:posix-getenv "DISPLAY")
				  *tmp-dir*
				  (gentemp "E");'E)
				  unique-lisp-file-ext
				  ))) ;;; auch an xon

(defmethod add-system ((manager manager) name options specifications)
  (let ((system (assoc name (systems manager))))
    (if system
	(setf (cdr system) (list options specifications :unloaded))
      (setf (systems manager)
	(cons (list name options specifications :unloaded)
	      (systems manager))))
    (when *master-p*
      (loop for space in (spaces manager)
	  do (kernel-send space
			  (system-added-message :name name
						:options options
						:specifications specifications))))))

(defun ensure-os ()
  (declare (optimize (speed 0) (debug 3)))
  (unless *manager*
    (setf *local-host* ;(sys:getenv "HOST"))
	  #+allegro-cl(sys:getenv "HOST")
	  #+sbcl(sb-ext:posix-getenv "HOST"))
    (setf *manager* (make-instance 'manager))
    (unless *master-p*
      (establish-connection *master-host* *master-port*)))
  *manager*)

;#-ALLEGRO-V5.0.1
(defun establish-connection (host port)
  (let ((stream
	 (make-instance 'tcp-client-stream
;					   #+ALLEGRO-V5.0.1
;					   :remote-host
;					   #-ALLEGRO-V5.0.1
					   :remote-host host
;					   #+ALLEGRO-V5.0.1
;					   :local-port
;					   #-ALLEGRO-V5.0.1
					   :remote-port port)))
    (connect-to-space stream host port)
    (print *local-host* stream)
    (print *port* stream)
    (finish-output stream)))


;; #+ALLEGRO-V5.0.1
;; (defun establish-connection (host port)
;;   (let ((stream
;; 	 (change-class
;; 	  (socket:make-socket
;; ;;;	   :connect :active
;; 	   :remote-port port
;; 	   :remote-host host
;; 	   :format :bivalent)
;; 	  'ipc::tcp-client-stream)))
;;     (connect-to-space stream host port)
;;     (print *local-host* stream)
;;     (print *port* stream)
;;     (finish-output stream)))

;;; geht nicht fuer andere
;#-ALLEGRO-V5.0.1
(defun make-server-stream ()
  (let ((stream)
	(init-port *port*))
;    (loop
;	until 
    (progn (format t "binding host ~A port ~A~%" *local-host* *port*) 
	   (handler-case (progn
			   (setf stream
				 (make-instance 'tcp-server-stream
						:host *local-host*
						:port *port*))
			   (initialize-tcp-server-process stream))
	     (error (cond) ())
	     (:no-error (cond) t)))
;	do (incf *port*))
    stream))

;; #+ALLEGRO-V5.0.1
;; (defun make-server-stream ()
;;   (let ((stream))
;;     (loop
;; 	until (handler-case (setf stream
;; ;;; vgl. cl-http/acl501/server/tcp.lisp
;; 			      ;;; kommt so auch nicht wieder
;; 			      (change-class
;; 			       (socket:make-socket
;; 			       :connect :passive ;;;:active kommt nicht wieder
;; 			       :local-port *port* :reuse-address t
;; 			       :local-host *local-host*
;; 			       :format :bivalent)
;; 			       'ipc::tcp-server-stream))
;; 		(error (cond) ())
;; 		(:no-error (cond) t))
;; 	do (incf *port*))
;;     stream))



;; #+ALLEGRO-V5.0
;; (defun establish-connection (host port)
;;   (let ((stream (make-socket-stream host port)))

;;     (connect-to-space stream host port)
;;     (print *local-host* stream)
;;     (print *port* stream)
;;     (finish-output stream)))

;; #+ALLEGRO-V5.0
;; (defun make-socket-stream (host port)
;;   (let ((s (acl-socket:make-socket :remote-host host :remote-port port)))
;;     (accept-connection s)))

;; #+ALLEGRO-V5.0
;; (defun make-server-stream ()
;;   (let ((stream))
;;     (loop
;;       until (handler-case (setf stream (make-socket-stream *local-host*
;; 							    *port*))
;; 	      (error (cond) ())
;; 	      (:no-error (cond) t))
;; 	do (incf *port*))
;;     stream))

(defun kill-machine (&key hard)
  (loop for receiver in (receivers *manager*)
      do (kill receiver))
  (loop for space in (spaces *manager*)
      do (kill space))
  (if hard
      (progn (close (stream *manager*))
	     (setf *manager* ())
	     (ensure-os))
    (progn (reset (objstore *manager*))
	   (setf (receivers *manager*) ())
	   (setf (spaces *manager*) ()))))

(defun reset-machine ()
  (loop for receiver in (receivers *manager*)
      do (reset receiver))
  (loop for space in (spaces *manager*)
      do (reset space))
  (reset (objstore *manager*))
  *manager*)

(defun kill-space (hostname)
  (let ((space (find hostname (spaces *manager*) :key #'host)))
    (kernel-send space
		 (ncl-exit-message))))

;; From clfswm
(defun uquit ()
  #+(or clisp cmu) (ext:quit)
  #+sbcl (sb-ext:quit)
  #+ecl (si:quit)
  #+gcl (lisp:quit)
  #+lispworks (lw:quit)
  #+(or allegro-cl allegro-cl-trial) (excl:exit))

(defmessage ncl-exit ()
;  :receive-action (excl:exit nil :quiet t))
  :receive-action (uquit))


(defun connection-lost (manager receiver)
  (let ((space (objspace receiver)))
    (if (string= *master-host* (host space))
;	(excl:exit nil :quiet t)
	(uquit)
      (progn (setf (spaces manager) (remove space (spaces manager)))
	     (setf (receivers manager) (remove receiver (receivers manager)))
	     (kill space)))))

;(setf excl:*restart-init-function* #'ensure-os)
