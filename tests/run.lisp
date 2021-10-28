(defpackage :myriam/test
  (:import-from :5am :def-suite :in-suite :test :is)
  (:export :run-tests)
  (:use :cl :myriam))

(in-package :myriam/test)

(def-suite :myriam)
(in-suite :myriam)

(setf myr:*current-self-identity* (myr:make-self-identity))
(setf myr:*target-public-identity* (myr:self->public-identity myr:*current-self-identity*))

(defun run-tests ()
  (5am:run! :myriam))

(test actor-spawn
  (let ((actor (spawn)))
    (is (eq 'myriam::pong (send* actor (msg :ping))))
    (is (eq 'myriam::ok (send actor (msg :stop))))))

(test async-message
  (let ((actor (spawn (action :something (lambda () 42) :async))))
    (is (eq 'myriam::ok (send actor (msg :something))))
    (send actor (msg :stop))))

(test sync-message
  (let ((actor (spawn (action :sum (lambda (x y) (+ x y)) :sync))))
    (is (= 5 (send* actor (msg :sum 2 3))))
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
    (mapc stuff (list a b c d e) (list "a" "b" "c" "d" "e"))
    (is (equalp (list "a" "b" "c" "d" "e")
                (mapcar get-stuff (list a b c d e))))
    (mapc stop (list a b c d e))))
