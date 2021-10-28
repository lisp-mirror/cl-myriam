(defpackage :myriam
  (:nicknames :myr)
  (:import-from :bordeaux-threads :make-thread)
  (:import-from :pzmq :with-context :with-socket :bind :connect :curve-keypair)
  (:import-from :usocket :socket-listen :get-local-port)
  (:import-from :str :split)
  (:import-from :conspack :encode :decode)
  ;; am i importing serapeum just to use its pretty -> type declaration syntax? yes. yes i am.
  (:import-from :serapeum :->)
  (:import-from :sha3 :sha3-digest-vector)
  (:export :action :msg :spawn :send :send* :*self*
           :with-self-identity :with-target-identity
           :*target-public-identity* :*current-self-identity*
           :make-self-identity :self->public-identity)
  (:use :cl))

(defpackage :myriam/test
  (:import-from :5am :def-suite :in-suite :test :is)
  (:use :cl :myriam))
