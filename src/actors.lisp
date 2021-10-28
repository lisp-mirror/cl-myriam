(in-package :myriam)

(defparameter *self* nil)
(defparameter *actions* nil)
(defparameter *storage* nil)

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
                       #'actor-loop
                       :name (concatenate 'string "actor-" *self*))))
          (values *self* thread)))))

(defun actor-loop ()
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

;;;
;;; Message handling
;;;
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
