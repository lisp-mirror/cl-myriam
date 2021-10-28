(in-package #:cl-user)
(asdf:defsystem cl-myriam
  :description "Simple actor model implementation for local and non-local actors"
  :licence "lgpl3"
  :version "0.0.1"
  :author "Ariela Wenner"
  :depends-on (:bordeaux-threads
               :cl-conspack
               :pzmq
               :serapeum
               :str
               :usocket
               :fiveam
               :qbase64
               :sha3)
  :encoding :utf-8
  :components
  ((:file "packages")
   (:module "src"
    :components
    ((:file "actors")
     (:file "messaging")
     (:file "address")
     (:file "conditions")
     (:file "identity")
     (:file "action")))))

(asdf:defsystem cl-myriam/test
  :description "Tests for cl-myriam"
  :depends-on (:cl-myriam
               :fiveam
               :bordeaux-threads)
  :encoding :utf-8
  :components
  ((:file "packages")
   (:module "tests"
    :components
    ((:file "run")))))
