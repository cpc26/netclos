(in-package nc)

;; need to validate superclass mobile-object-class for ncl-object
(defmethod validate-superclass ((class mobile-object-class) (superclass standard-class))
  t)

(defclass ncl-object (mobile-object active-object)
  ((queue :moving-behavior #'(lambda (queue stream) (print `(mq ,@(all-elements queue)) stream))))
  (:metaclass mobile-object-class))
