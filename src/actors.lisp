(in-package #:myriam/actors)

(defparameter *self* nil)
(defparameter *actions* nil)
(defparameter *storage* nil)
(defparameter *self-identity* nil)
(defparameter *trust-store* (make-hash-table :test #'equal))
(defparameter *trust-store-lock* (bt:make-lock "trust-store-lock"))
(defparameter *target-public-identity* nil)
(defparameter *current-self-identity* nil)

;;;
;;; Types/Classes
;;;
(defun message-context-p (x)
  (or (eq x :async)
      (eq x :sync)))

(defun action-context-p (x)
  (or (eq x :async)
      (eq x :sync)
      (eq x :any)))

(deftype message-context-t ()
  '(satisfies message-context-p))

(deftype action-context-t ()
  '(satisfies action-context-p))

(defclass action ()
  ((name
    :initarg :name
    :type keyword
    :accessor action-name)
   (task
    :initarg :task
    :type function
    :accessor action-task)
   (context
    :initarg :context
    :type action-context-t
    :accessor action-context
    :initform :async)))

(defclass message ()
  ((head
    :initarg :head
    :type keyword
    :accessor message-head)
   (body
    :initarg :body
    :type list
    :accessor message-body)
   (context
    :initarg :context
    :type message-context-t
    :accessor message-context
    :initform :async)))

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

(conspack:defencoding message
  head body context)

(-> action (keyword function &optional action-context-t) (values action &optional))
(defun action (name task &optional (context :async))
  "helper to make an instance of an action"
  (make-instance 'action :name name :task task :context context))

;;;
;;; Conditions
;;;
(define-condition invalid-message (error) ()
  (:documentation "An invalid message received/attemped to be sent."))

(define-condition encryption-failed (error) ()
  (:documentation "Could not encrypt message with keys from our trust store."))

(define-condition target-identity-not-set (error) ()
  (:documentation "Target identity has not been set."))

(define-condition self-identity-not-set (error) ()
  (:documentation "Self identity has not been set."))

;;;
;;; Actor spawning
;;;
(defmacro with-actor-parameters (&rest body)
  `(let ((bt:*default-special-bindings*
           `((*self* . ,*self*)
             (*storage* . ,*storage*)
             (*actions* . ,*actions*)
             (*current-self-identity* . *current-self-identity*))))
     ,@body))

(-> spawn (&rest action) (values string bt:thread))
(defun spawn (&rest actions)
  "spawn an actor, return its address and running thread"
  (unless (typep *current-self-identity* 'self-identity)
    (signal (make-condition 'self-identity-not-set)))
  (let ((*actions* (make-hash-table :test #'eq))
        (*self* (make-address))
        (*storage* (make-hash-table :test #'eq)))
    (loop for action in actions
          do (setf (gethash (action-name action) *actions*) action))
    (with-actor-parameters
        (let ((thread (bt:make-thread
                       (lambda ()
                         (pzmq:with-context nil
                           (pzmq:with-socket socket
                               (:rep
                                :curve-server 1
                                :curve-publickey (self-identity-public-key *current-self-identity*)
                                :curve-secretkey (self-identity-secret-key *current-self-identity*))
                             (pzmq:bind socket (address->binding *self*))
                             (loop for raw-msg = (pzmq:recv-octets socket)
                                   do (let ((msg (conspack:decode raw-msg)))
                                        (multiple-value-bind (proc continue?) (message-handle msg)
                                          (pzmq:send socket (conspack:encode (funcall proc)))
                                          (unless continue?
                                            (return))))))))
                       :name (concatenate 'string "actor-" *self*))))
          (values *self* thread)))))

(-> post (string message &key (:context message-context-t)) *)
(defun post (actor msg &key (context :async))
  "send a message to an actor"
  (unless (typep *target-public-identity* 'public-identity)
    (signal (make-condition 'target-identity-not-set)))
  (setf (message-context msg) context)
  (pzmq:with-context nil
    (pzmq:with-socket socket
        (:req
         :curve-publickey (self-identity-public-key *current-self-identity*)
         :curve-secretkey (self-identity-secret-key *current-self-identity*)
         :curve-serverkey (public-identity-key *target-public-identity*))
      (pzmq:connect socket actor)
      (pzmq:send socket (conspack:encode msg))
      (let ((response (conspack:decode (pzmq:recv-octets socket))))
        (if (typep response 'condition)
            (signal response)
            response)))))

(-> send (string message) *)
(defun send (actor msg)
  "send a message to an actor to perform an asychronous task"
  (post actor msg :context :async))

(-> send* (string message) *)
(defun send* (actor msg)
  "send a message to an actor to perform a synchronous task and get a result"
  (post actor msg :context :sync))

;;;
;;; Storage
;;;
(-> fetch-value* (keyword) *)
(defun fetch-value* (key)
  (gethash key *storage*))

(-> store-value* (keyword *) *)
(defun store-value* (key value)
  (setf (gethash key *storage*) value))

;;;
;;; Address helpers
;;;
(-> make-random-port () integer)
(defun make-random-port ()
  (let ((socket (usocket:socket-listen "127.0.0.1" 0)))
    (prog1
        (usocket:get-local-port socket)
      (usocket:socket-close socket))))

(-> make-address (&optional integer) string)
(defun make-address (&optional (port (make-random-port)))
  (format nil "tcp://127.0.0.1:~a" port))

(-> address->binding (string) string)
(defun address->binding (address)
  (let ((port (nth-value 1 (address-tokens address))))
    (format nil "tcp://*:~a" port)))

(-> address-tokens (string) (values string string))
(defun address-tokens (address)
  (let* ((tokens (str:split ":" (cadr (str:split "://" address)))))
    (values (car tokens) (cadr tokens))))

;;;
;;; Messaging
;;;
(-> msg (keyword &rest *) (values message &optional))
(defun msg (head &rest body)
  "Build a message. head should be a keyword symbol"
  (make-instance 'message :head head :body body))

(-> message-handle (message) (values function boolean))
(defun message-handle (obj)
  (let ((default (lambda () 'OK)))
    (if (valid-message-p obj)
        (case (message-head obj)
          (:ping (values (lambda () 'pong) t))
          (:fetch (values (wrap-task #'fetch-value* obj) t))
          (:store (values (wrap-task #'store-value* obj) t))
          (:stop (values default nil))
          (otherwise (let ((action (gethash (message-head obj) *actions*)))
                       (if action
                           (values (wrap-task (action-task action) obj) t)
                           (values (lambda () 'not-found) t)))))
        (values (lambda () 'invalid-message) t))))

(-> wrap-task (function message) function)
(defun wrap-task (task message)
  "wrap task according to its context"
  (case (message-context message)
    (:async (lambda ()
              (with-actor-parameters
                  ;; we don't care about errors inside the task execution thread
                  (bt:make-thread
                   (ignore-errors
                    (lambda ()
                      (apply task (message-body message))))))
              'ok))
    (:sync (lambda ()
             (handler-case (apply task (message-body message))
               ;; simply return the condition to the caller actor so it can be thrown there
               (error (e)
                 e))))
    (t (lambda () 'invalid-message)))) ; should never happen, but...

(-> valid-message-p (message) boolean)
(defun valid-message-p (msg)
  "check if this is a valid message in the correct context"
  (let ((message-context (message-context msg))
        (action (gethash (message-head msg) *actions*)))
    (cond ((valid-predefined-message-p msg) t)
          ((eq (action-context action) :any) t)
          ((eql message-context (action-context action)) t)
          (t nil))))

(-> valid-predefined-message-p (message) boolean)
(defun valid-predefined-message-p (msg)
  "check if this is a predefined message with the correct context - we conly care about :fetch for now"
  (case (message-head msg)
    (:ping t)
    (:store t)
    (:fetch (eq :sync (message-context msg)))
    (:stop t)
    (otherwise nil)))

;;;
;;; Identity helpers
;;;
(-> hash-blob ((simple-array (unsigned-byte 8))) (values string &optional))
(defun hash-blob (blob)
  (reduce (lambda (x y)
            (concatenate 'string x y))
          (map 'vector (lambda (x)
                         (write-to-string x :base 16))
               (sha3-digest-vector blob))))

(-> self->public-identity (self-identity) public-identity)
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
