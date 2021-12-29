(in-package :myriam)

(defun receive-and-process-auth-request (socket accept-p)
  (pzmq:with-messages (ign id)
    (let ((address nil)
          (key nil))
      (pzmq:msg-recv ign socket)
      (pzmq:msg-recv id socket)
      (pzmq:msg-recv ign socket)
      (setf address (babel:octets-to-string (pzmq:recv-octets socket)))
      (pzmq:msg-recv ign socket)
      (pzmq:msg-recv ign socket)
      (setf key (pzmq:recv-octets socket))
      (let ((result (funcall accept-p address key)))
        (if result
            (progn
              (pzmq:send socket (babel:string-to-octets "1.0") :sndmore t)
              (pzmq:msg-send id socket :sndmore t)
              (pzmq:send socket (babel:string-to-octets "200") :sndmore t)
              (pzmq:send socket (babel:string-to-octets "OK") :sndmore t)
              (pzmq:send socket (babel:string-to-octets "actor") :sndmore t)
              (pzmq:send socket nil))
            (progn
              (pzmq:send socket (babel:string-to-octets "1.0") :sndmore t)
              (pzmq:msg-send id socket :sndmore t)
              (pzmq:send socket (babel:string-to-octets "400") :sndmore t)
              (pzmq:send socket (babel:string-to-octets "DENIED") :sndmore t)
              (pzmq:send socket (babel:string-to-octets "none") :sndmore t)
              (pzmq:send socket nil)))))))

(-> spawn-authenticator (function &optional string) bt:thread)
(defun spawn-authenticator (accept-p &optional (name (string-uuid)))
  "Spawn an authenticator that will use accept-p (which should be a predicate that takes an IP address
   and a public key as a byte vector) to either accept or reject incoming connections. Name, if given,
   should be a unique string."
  ;; TODO: figure out a better way to shut down an authenticator
  (pzmq:with-socket (listener *context*) :pair
    ;; we want to be absolutely sure that the authenticator is ready before returning
    (pzmq:bind listener "inproc://auth-sync")
    (let ((thread (bt:make-thread
                   (lambda ()
                     (pzmq:with-socket (authenticator *context*) :rep
                       (pzmq:bind authenticator "inproc://zeromq.zap.01")
                       (pzmq:with-socket (heartbeat *context*) :pair
                         (pzmq:connect heartbeat "inproc://auth-sync")
                         (pzmq:send heartbeat "READY"))
                       (loop do (receive-and-process-auth-request authenticator accept-p))))
                   :name (concatenate 'string "#myriam.auth-thread-" name))))
      (pzmq:recv-string listener)
      (values thread name))))

(-> authenticator-thread (string) (or null bt:thread))
(defun authenticator-thread (name)
  (car (remove-if-not (lambda (thread)
                        (let ((thread-name (bt:thread-name thread)))
                          (string= thread-name
                                   (concatenate 'string "#myriam.auth-thread-" name))))
                      (bt:all-threads))))

(-> kill-authenticator (string) *)
(defun kill-authenticator (name)
  (let ((auth-thread (authenticator-thread name)))
    (when (and auth-thread (bt:thread-alive-p auth-thread))
      (bt:destroy-thread auth-thread))))


(-> authenticator-alive-p (string) (values boolean &optional))
(defun authenticator-alive-p (name)
  (let ((auth-thread (authenticator-thread name)))
    (if auth-thread
        (bt:thread-alive-p auth-thread)
        nil)))
