(in-package #:myriam)

(-> make-random-port () integer)
(defun make-random-port ()
  (let ((socket (usocket:socket-listen "127.0.0.1" 0)))
    (prog1
        (usocket:get-local-port socket)
      (usocket:socket-close socket))))

(-> make-address (&optional integer) string)
(defun make-address (&optional (port (make-random-port)))
  (format nil "tcp://127.0.0.1:~a" port))

(-> address->binding (string) string)
(defun address->binding (address)
  (let ((port (nth-value 1 (address-tokens address))))
    (format nil "tcp://*:~a" port)))

(-> address-tokens (string) (values string string))
(defun address-tokens (address)
  (let* ((tokens (str:split ":" (cadr (str:split "://" address)))))
    (values (car tokens) (cadr tokens))))
