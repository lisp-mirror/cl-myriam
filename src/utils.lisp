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
