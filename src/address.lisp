(in-package :myriam)

;;; Just a lazy approximation, not the most strict check in the world...
(defparameter *address-scanner*
  (ppcre:create-scanner
   '(:sequence
     "tcp://"
     (:greedy-repetition 1 nil (:alternation "." :digit-class :word-char-class))
     ":"
     (:greedy-repetition 1 5 :digit-class))))

(defun address-p (obj)
  (if (ppcre:scan-to-strings *address-scanner* obj) t nil))

(deftype valid-address ()
  '(satisfies address-p))

(-> make-random-port () integer)
(defun make-random-port ()
  (let ((socket (usocket:socket-listen "127.0.0.1" 0)))
    (prog1
        (usocket:get-local-port socket)
      (usocket:socket-close socket))))

(-> make-address (&optional integer) valid-address)
(defun make-address (&optional (port (make-random-port)))
  (format nil "tcp://127.0.0.1:~a" port))

(-> address->binding (valid-address) string)
(defun address->binding (address)
  (let ((port (nth-value 1 (address-tokens address))))
    (format nil "tcp://*:~a" port)))

(-> address-tokens (valid-address) (values string string))
(defun address-tokens (address)
  (let* ((tokens (str:split ":" (cadr (str:split "://" address)))))
    (values (car tokens) (cadr tokens))))

(-> change-host (valid-address string) valid-address)
(defun change-host (address new-host)
  (let ((port (nth-value 1 (address-tokens address))))
    (format nil "tcp://~a:~a" new-host port)))
