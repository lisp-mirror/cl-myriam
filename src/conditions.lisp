(in-package :myriam)

(define-condition invalid-message (error) ()
  (:documentation "An invalid message received/attemped to be sent."))

(define-condition encryption-failed (error) ()
  (:documentation "Could not encrypt message with keys from our trust store."))

(define-condition target-identity-not-set (error) ()
  (:documentation "Target identity has not been set."))

(define-condition self-identity-not-set (error) ()
  (:documentation "Self identity has not been set."))
