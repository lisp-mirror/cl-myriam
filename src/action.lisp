(in-package :myriam)

(defun action-context-p (x)
  (or (eq x :async)
      (eq x :sync)
      (eq x :any)))

(deftype action-context-t ()
  '(satisfies action-context-p))

(defclass action ()
  ((name
    :initarg :name
    :type keyword
    :accessor action-name)
   (task
    :initarg :task
    :type function
    :accessor action-task)
   (context
    :initarg :context
    :type action-context-t
    :accessor action-context
    :initform :async)))

(-> action (keyword function &optional action-context-t) (values action &optional))
(defun action (name task &optional (context :async))
  "helper to make an instance of an action"
  (make-instance 'action :name name :task task :context context))
