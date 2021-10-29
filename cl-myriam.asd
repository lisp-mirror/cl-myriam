(in-package :cl-user)
(asdf:defsystem cl-myriam
  :description "Simple actor model implementation for local and non-local actors"
  :licence "lgpl3"
  :version "0.0.1"
  :author "Ariela Wenner"
  :depends-on (:bordeaux-threads
               :cl-conspack
               :lparallel
               :pzmq
               :serapeum
               :str
               :usocket
               :sha3)
  :encoding :utf-8
  :serial t
  :components
  ((:file "package")
   (:module "src"
    :components
    ((:file "utils")
     (:file "conditions")
     (:file "address")
     (:file "identity")
     (:file "messaging")
     (:file "action")
     (:file "actors"))))
  :in-order-to ((asdf:test-op (asdf:test-op :cl-myriam/test))))

(asdf:defsystem cl-myriam/test
  :description "Tests for cl-myriam"
  :depends-on (:cl-myriam
               :fiveam)
  :encoding :utf-8
  :serial t
  :components
  ((:module "tests"
    :components
    ((:file "run"))))
  :perform (asdf:test-op (o c) (uiop:symbol-call :5am :run! :myriam)))
