(defpackage :myriam/actors
  (:nicknames #:myriam #:myr)
  (:import-from #:pzmq #:with-context #:with-socket #:bind #:connect #:curve-keypair)
  (:import-from #:usocket #:socket-listen #:get-local-port)
  (:import-from #:str #:split)
  (:import-from #:conspack #:encode #:decode)
  ;; am i importing serapeum just to use its pretty -> type declaration syntax? yes. yes i am.
  (:import-from #:serapeum #:->)
  (:import-from #:qbase64 #:encode-bytes)
  (:import-from #:sha3 #:sha3-digest-vector)
  (:export #:action #:msg #:spawn #:send #:send* #:*self*
           #:with-self-identity #:with-target-identity
           #:*target-public-identity* #:*current-self-identity*
           #:make-self-identity #:self->public-identity)
  (:use #:cl #:bordeaux-threads))

(defpackage :myriam/test
  (:use #:cl #:5am #:myriam/actors))
