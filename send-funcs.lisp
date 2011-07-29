;;;-----------------------------------------------------------------------------------
;;; name       : send-func.lisp
;;; description: the compiler-macro 'sendable-function' stores a sendable 
;;;              representation of a function (it's interpretable code).
;;; notes      : At the moment this works just in the empty environment. 
;;;              It can't cope with closures or static variables.
;;; contact    : me (Michael Trowe)
;;; copyright  : You wanna copy this code? You fool!!!
;;; history    : 
;;; contents   :
;;;-----------------------------------------------------------------------------------
(in-package nc)
(defvar *ncl-readtable* ())
(define-compiler-macro sendable-function (denom)
  `(load-time-value 
    (let ((func (compile nil ',denom)))
      (store-denom ',denom func)
      func)))

(defmacro sendable-function (denom)
  `(function ,denom))

(defun |#>-reader| (stream subchar arg)
  (declare (ignore subchar arg))
  (list 'sendable-function (read stream t nil t)))

(set-dispatch-macro-character #\# #\> #'|#>-reader|)

(setq *ncl-readtable* *readtable*)

(let ((denoms (make-hash-table)))
  (defun store-denom (denom func)
    (setf (gethash func denoms) denom)
    func)
  (defun get-denom (func)
    (or (gethash func denoms)
	(f-denominator func))))

(defun f-denominator (func)
  (multiple-value-bind (expr x name)
      (function-lambda-expression func)
    (declare (ignore x))
    (cond ((and name (symbolp name))
	   name)
	  (t expr))))
			 

