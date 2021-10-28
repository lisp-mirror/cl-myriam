(in-package :myriam)

(defmacro with-server-socket (name &body body)
  (let ((socket name))
    `(pzmq:with-context nil
       (pzmq:with-socket ,socket
           (:rep
            :curve-server 1
            :curve-publickey (self-identity-public-key *current-self-identity*)
            :curve-secretkey (self-identity-secret-key *current-self-identity*))
         ,@body))))

(defmacro with-client-socket (name &body body)
  (let ((socket name))
    `(pzmq:with-context nil
       (pzmq:with-socket ,socket
           (:req
            :curve-publickey (self-identity-public-key *current-self-identity*)
            :curve-secretkey (self-identity-secret-key *current-self-identity*)
            :curve-serverkey (public-identity-key *target-public-identity*))
         ,@body))))
