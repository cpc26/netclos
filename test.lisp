(require 'netclos)

(in-package nc)

(trace ensure-os make-server-stream initialize-tcp-server-process)

(ensure-os)


(defclass move-tester (ncl-object)
  ((s1 :accessor s1
       :initarg :s1
       :moving-behavior :stay)
   (s2 :accessor s2
       :initarg :s2
       :moving-behavior :stay)
   (s3 :accessor s3
       :initarg :s3
       :moving-behavior :follow))
  (:metaclass mobile-object-class))

(defpargeneric pg1 :future (tester))

(defmethod pg1 ((tester move-tester)) (s1 tester))

(defpargeneric pg2 :past (tester val))

(defmethod pg2 ((tester move-tester) val) (setf (s1 tester) val))

(defclass move-part ()
  ((s1 :accessor s1
       :initarg :s1
       :moving-behavior :stay))
  (:metaclass  mobile-object-class))

(defun testos (&rest machines)
  (ensure-os)
  (when machines
    (start-virtual-machine  machines))
  (let ((proxy1 (move (make-instance 'move-tester
                        :s1 1
                        :s2 (make-instance 'move-tester)
                        :s3 (make-instance 'move-part))
                      (first (spaces *manager*)))))
    (setf *test* proxy1)))

(defgeneric remfoo (tester1 tester2)
  (:generic-function-class ncl-gf))

(defmethod remfoo ((t1 move-tester) (t2 move-tester))
  (let ((help (s1 t1)))
    (setf (s1 t1) (s1 t2))
    (setf (s1 t2) help)))


(trace ensure-os make-server-stream)

(ensure-os)

;; (defpsystem :test (:default-pathname "nclos:")
;;   (:definitions :actors
;;       (:serial "test")))

(defpsystem :test
    :serial t)

(compile-psystem :test)

(load-psystem :test)
(trace add-method proxy-test-lambda)
