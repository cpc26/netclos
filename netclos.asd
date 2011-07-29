;;;-*- Package: NETCLOS; Syntax: Common-Lisp; Mode: Lisp -*-

(defpackage :netclos
  (:nicknames :nc)
  (:use :cl :asdf)
  #+sbcl(:use sb-mop)
  (:export start-virtual-machine send proxy active-object))

(defsystem :netclos
  :name "NetCLOS"
  :author "Michael Trowe, Lothar Hotz"
  :version "git"
  :maintainer "Vitaly Mayatskikh <v.mayatskih@gmail.com>"
  :description "Distributed objects layer for CLOS"
  :serial t
  :depends-on (:acl-compat :sb-simple-streams)
  :components ((:file "ncl-macros")
	       (:file "send-funcs")
	       (:file "queues")
	       (:file "active")
	       (:file "ipc")
	       (:file "manager")
	       (:file "distribute")
	       (:file "pdefsys")
	       (:file "message-handler")
	       (:file "objectstore")
	       (:file "ncl-object")))
