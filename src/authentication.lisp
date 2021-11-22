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

(-> spawn-authenticator (function) bt:thread)
(defun spawn-authenticator (accept-p)
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
                   :name "#myriam.auth-thread")))
      (pzmq:recv-string listener)
      thread)))

(defun authenticator-thread ()
  (car (remove-if-not
        (lambda (thread)
          (string= "#myriam.auth-thread" (bt:thread-name thread)))
        (bt:all-threads))))

(defun kill-authenticator ()
  (let ((auth-thread (authenticator-thread)))
    (when (and auth-thread (bt:thread-alive-p auth-thread))
      (bt:destroy-thread auth-thread))))

(defun authenticator-alive-p ()
  (let ((auth-thread (authenticator-thread)))
    (if auth-thread
        (bt:thread-alive-p auth-thread)
        nil)))
