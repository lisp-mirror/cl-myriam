(defpackage :myriam/test
  (:import-from :5am :def-suite :in-suite :test :is)
  (:export :run-tests)
  (:use :cl :myriam))

(in-package :myriam/test)

(def-suite :myriam)
(in-suite :myriam)

(defparameter auth-thread nil)
(defparameter auth-name nil)

(multiple-value-bind (thread name)
    (spawn-authenticator
     (lambda (ip pk)
       (declare (ignore pk))
       (if (string= ip "127.0.0.1")
           t
           nil)))
  (setf auth-thread thread)
  (setf auth-name name))

(test auth-thread
  (is (myr:authenticator-alive-p auth-name)))

(setf myr:*current-self-identity* (myr:make-self-identity))
(setf myr:*target-public-identity* (myr:self->public-identity myr:*current-self-identity*))

(test actor-spawn
  (let ((actor (spawn)))
    (is (eq :pong (send* actor (msg :ping))))
    (is (eq :ok (send actor (msg :stop))))))

(test async-message
  (let ((actor (spawn (action :something (lambda () 42) :async))))
    (is (eq :ok (send actor (msg :something))))
    (send actor (msg :stop))))

(test sync-message
  (let ((actor (spawn (action :sum (lambda (x y) (+ x y)) :sync))))
    (is (= 5 (send* actor (msg :sum 2 3))))
    (send actor (msg :stop))))

(test invalid-message
  (let ((actor (spawn (action :self-address (lambda () *self*) :sync))))
    (is (eq :invalid-message (send actor (msg :self-address))))
    (send actor (msg :stop))))

(test non-existent-action
  (let ((actor (spawn)))
    (is (eq :invalid-message (send actor (msg :foo))))
    (send actor (msg :stop))))

(test self-address
  (let ((actor (spawn (action :get-self (lambda () *self*) :sync))))
    (is (string-equal actor (send* actor (msg :get-self))))
    (send actor (msg :stop))))

(test store-and-fetch
  (let ((actor (spawn)))
    (send actor (msg :store :something "banana"))
    (is (string-equal "banana" (send* actor (msg :fetch :something))))
    (send actor (msg :stop))))

(test spawn-many-actors
  (let ((a (spawn))
        (b (spawn))
        (c (spawn))
        (d (spawn))
        (e (spawn))
        (stuff (lambda (x m)
                 (send x (msg :store :stuff m))))
        (get-stuff (lambda (x)
                     (send* x (msg :fetch :stuff))))
        (stop (lambda (x)
                (send x (msg :stop)))))
    (mapcar stuff (list a b c d e) (list "a" "b" "c" "d" "e"))
    (is (equalp (list "a" "b" "c" "d" "e")
                (mapcar get-stuff (list a b c d e))))
    (mapcar stop (list a b c d e))))

(test kill-authenticator
  (kill-authenticator auth-name)
  (sleep 0.25)
  (is (not (myr:authenticator-alive-p auth-name))))
