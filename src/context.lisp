(in-package :myriam)

(defparameter *context* (pzmq:ctx-new))

(defmacro with-new-context (&body body)
  `(let ((*context* (pzmq:ctx-new)))
     (let ((bt:*default-special-bindings*
             (append
              `((*context* . ,*context*))
              bt:*default-special-bindings*)))
       ,@body)))
