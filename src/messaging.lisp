(in-package #:myriam)

(defun message-context-p (x)
  (or (eq x :async)
      (eq x :sync)))

(deftype message-context-t ()
  '(satisfies message-context-p))

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

(conspack:defencoding message
  head body context)

(-> msg (keyword &rest *) (values message &optional))
(defun msg (head &rest body)
  "Build a message. head should be a keyword symbol"
  (make-instance 'message :head head :body body))


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
