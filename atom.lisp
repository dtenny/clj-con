(in-package :clj-con)

;;;; Clojure-styled 'atoms' and related functions.
;;;; Metadata and validator-fn is not supported.

;;; SVREF is the only CAS place supported by all Atomics-supported lisps
;;; as of Dec-2023. So we use that.

#-:atomics-cas-svref
(defstruct (atom (:predicate atom?))
  lock                                  ;using locks, not compare-and-swap
  value)

#+:atomics-cas-svref
(defstruct (atom (:predicate atom?))
  (cas-vector #(nil) :type (array t 1))) ;initval just for type conformance, unused

(defun atom (&optional x)
  "Creates and returns an Atom with an initial value of x.
Does not support validator and metadata arguments of the Clojure equivalent."
  #+:atomics-cas-svref
  (make-atom :cas-vector (make-array 1 :initial-element x))
  #-:atomics-cas-svref
  (make-atom :lock (bt2:make-lock) :value x))

(defmethod deref ((atom atom) &optional timeout-ms timeout-val)
  (declare (ignore timeout-ms timeout-val))
  #+:atomics-cas-svref
  (svref (atom-cas-vector atom) 0)
  #-:atomics-cas-svref
  (atom-value atom))

#-:atomics-cas-svref
(defun reset! (atom newval)
  "Sets the value of atom to newval without regard for the
current value. Returns newval."
  (declare (type atom atom))
  (bt2:with-lock-held ((atom-lock atom))
    (setf (atom-value atom) newval)))

#+:atomics-cas-svref
(defun reset! (atom newval)
  "Sets the value of atom to newval without regard for the
current value. Returns newval."
  (declare (type atom atom))
  (let ((v (atom-cas-vector atom)))
    (atomics:atomic-update (svref v 0) (constantly newval)))
  newval)

(defun reset-vals! (atom newval)
  "Sets the value of atom to newval.
Returns the value of the atom before and after the reset as multiple values.
(Note difference from Clojure which does not have multiple value returns)."
  (declare (type atom atom))
  #-:atomics-cas-svref
  (bt2:with-lock-held ((atom-lock atom))
    (let ((oldval (atom-value atom)))
      (setf (atom-value atom) newval)
      (values oldval newval)))
  #+:atomics-cas-svref
  (let ((v (atom-cas-vector atom)))
    (loop with swapped-p = nil
          until swapped-p
          as oldval = (svref v 0)
          do (setf swapped-p (atomics:cas (svref v 0) oldval newval))
          finally (return (values oldval newval)))))

(defun swap! (atom f &rest args)
  "Atomically swaps the value of atom to be:
(apply f current-value-of-atom args). Note that f may be called
multiple times, and thus should be free of side effects.  Returns
the value that was swapped in."
  (declare (type atom atom))
  #-:atomics-cas-svref
  (bt2:with-lock-held ((atom-lock atom))
    (setf (atom-value atom)
          (apply f (atom-value atom) args)))
  #+:atomics-cas-svref
  (let ((v (atom-cas-vector atom))
        (new nil))
    (flet ((updater (old-val)
             (setf new (apply f old-val args))))
      (atomics:atomic-update (svref v 0) #'updater)
      new)))

(defun swap-vals! (atom f &rest args)
  "Atomically swaps the value of atom to be:
(apply f current-value-of-atom args). Note that f may be called
multiple times, and thus should be free of side effects.  Returns un-Clojure-y
multiple values `(old new)`, the value of the atom before and after the swap."
  (declare (type atom atom))
  #-:atomics-cas-svref
  (bt2:with-lock-held ((atom-lock atom))
    (let* ((oldval (atom-value atom))
           (newval (apply f oldval args)))
      (setf (atom-value atom) newval)
      (values oldval newval)))
  #+:atomics-cas-svref
  (let ((v (atom-cas-vector atom))
        (old nil)
        (new nil))
    (flet ((updater (old-val)
             (setf old old-val)
             (setf new (apply f old-val args))))
      (atomics:atomic-update (svref v 0) #'updater)
      (values old new))))

(defun compare-and-set! (atom oldval newval)
  "Atomically sets the value of atom to newval if and only if the
current value of the atom is identical (EQ) to oldval.
Returns non-NIL if the set happened, NIL if it did not."
  (declare (type atom atom))
  #-:atomics-cas-svref
  (bt2:with-lock-held ((atom-lock atom))
    (when (eq (atom-value atom) oldval)
      (setf (atom-value atom) newval)
      t))
  #+:atomics-cas-svref
  (let ((v (atom-cas-vector atom)))
    (atomics:cas (svref v 0) oldval newval)))
      
