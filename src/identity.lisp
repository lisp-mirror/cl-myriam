(in-package :myriam)

(defparameter *trust-store* (make-hash-table :test #'equal))
(defparameter *trust-store-lock* (bt:make-lock "trust-store-lock"))
(defparameter *target-public-identity* nil)
(defparameter *current-self-identity* nil)

(defclass public-identity ()
  ((public-key
    :initarg :public-key
    :type (simple-array (unsigned-byte 8) (32))
    :accessor public-identity-key)
   (hash
    :initarg :hash
    :type string
    :accessor public-identity-hash)))

(defclass self-identity ()
  ((public-key
    :initarg :public-key
    :type (simple-array (unsigned-byte 8) (32))
    :accessor self-identity-public-key)
   (secret-key
    :initarg :secret-key
    :type (simple-array (unsigned-byte 8) (32))
    :accessor self-identity-secret-key)
   (hash
    :initarg :hash
    :type string
    :accessor self-identity-hash)))

(-> hash-blob ((simple-array (unsigned-byte 8))) (values string &optional))
(defun hash-blob (blob)
  (reduce (lambda (x y)
            (concatenate 'string x y))
          (map 'vector (lambda (x)
                         (write-to-string x :base 16))
               (sha3-digest-vector blob))))

(-> self->public-identity (self-identity) (values public-identity &optional))
(defun self->public-identity (self-id)
  (make-public-identity (self-identity-public-key self-id)))

(-> make-public-identity ((simple-array (unsigned-byte 8))) (values public-identity &optional))
(defun make-public-identity (pkey)
  (make-instance 'public-identity
                 :public-key pkey
                 :hash (hash-blob pkey)))

(-> make-self-identity () (values self-identity &optional))
(defun make-self-identity ()
  (multiple-value-bind (pkey skey) (pzmq:curve-keypair)
    (make-instance 'self-identity
                   :public-key pkey
                   :secret-key skey
                   :hash (hash-blob pkey))))

(-> store-public-identity (public-identity) *)
(defun store-public-identity (id)
  (bt:with-lock-held (*trust-store-lock*)
    (setf (gethash (public-identity-hash id) *trust-store*)
          id)))

(defmacro with-target-identity (id &rest body)
  `(let ((*target-public-identity* ,id))
     ,@body))

(defmacro with-self-identity (self-id &rest body)
  `(let ((*current-self-identity* ,self-id))
     ,@body))
