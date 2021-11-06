(in-package :myriam)

(defun kernel-setup ()
  (unless lparallel:*kernel*
    (setf lparallel:*kernel*
          (lparallel:make-kernel (serapeum:count-cpus)))))

(defmacro with-timeout (timeout &body body)
  (if (and (integerp timeout) (> timeout 0))
      `(progn
         (kernel-setup)
         (let ((channel (lparallel:make-channel)))
           (lparallel:submit-task channel (lambda () (progn ,@body)))
           (lparallel:try-receive-result channel :timeout ,timeout)))
      (error 'syntax-error)))


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
