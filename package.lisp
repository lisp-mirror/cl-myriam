(defpackage :myriam
  (:nicknames :myr)
  (:import-from :bordeaux-threads :make-thread)
  (:import-from :pzmq :with-context :with-socket :bind :connect :curve-keypair)
  (:import-from :usocket :socket-listen :get-local-port)
  (:import-from :str :split)
  (:import-from :conspack :encode :decode)
  ;; am i importing serapeum just to use its pretty -> type declaration syntax? yes. yes i am.
  (:import-from :serapeum :-> :count-cpus)
  (:import-from :sha3 :sha3-digest-vector)
  (:import-from :lparallel :*kernel* :make-kernel :make-channel :submit-task :try-receive-result
                :*task-category* :kill-tasks)
  (:import-from :ppcre :create-scanner :scan-to-strings)
  (:import-from :uuid :make-v4-uuid)
  (:export :action :msg :spawn :send :send* :*self*
           :with-self-identity :with-target-identity
           :*target-public-identity* :*current-self-identity*
           :make-self-identity :self->public-identity
           :*print-internal-actor-error-messages*
           :change-host :all-actors :stop-all-actors)
  (:use :cl))
