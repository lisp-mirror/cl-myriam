(in-package :myriam)

(defun kernel-setup ()
  (unless lparallel:*kernel*
    (setf lparallel:*kernel*
          (lparallel:make-kernel (serapeum:count-cpus)))))

(defmacro with-timeout (timeout &body body)
  `(progn
     (kernel-setup)
     (let ((lparallel:*task-category* (format nil "~a" (uuid:make-v4-uuid))))
       (let ((channel (lparallel:make-channel)))
         (lparallel:submit-task channel (lambda () ,@body))
         (multiple-value-bind (result received-p)
             (lparallel:try-receive-result channel :timeout ,timeout)
           (unless received-p
             (lparallel:kill-tasks lparallel:*task-category*))
           result)))))

(defun all-actors ()
  (let ((thread-names (mapcar #'bt:thread-name (bt:all-threads))))
    (mapcar (lambda (name)
              (cadr (str:split "-" name)))
            (remove-if-not (lambda (name)
                             (str:containsp "#myriam.actor"
                                            name))
                           thread-names))))

(defun stop-all-actors ()
  (mapcar (lambda (actor)
            (send actor (msg :stop)))
          (all-actors)))
