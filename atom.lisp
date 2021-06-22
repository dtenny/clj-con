(in-package :clj-con)

;;;; Clojure-styled 'atoms' and related functions.
;;;; Metadata and validator-fn is not supported.
;;;;
;;;; Bordeaux threads (BT) offers little to help with this, it would be much more efficient
;;;; to package up sb-ext: functions such as `compare-and-swap`.
;;;; Perhaps at some point we'll do optimized #+sbcl type stuff, but I wanted BT generic
;;;; implementations to start.

(defstruct (atom (:predicate atom?))
  lock
  value)

(defun atom (&optional x)
  "Creates and returns an Atom with an initial value of x.
Does not support validator and metadata arguments of the Clojure equivalent."
  (make-atom :lock (bt:make-lock) :value x))

(defmethod deref ((atom atom) &optional timeout-ms timeout-val)
  (declare (ignore timeout-ms timeout-val))
  ;; Do I need the lock to reliable examine a structure slot?
  (atom-value atom))

(defun reset! (atom newval)
  "Sets the value of atom to newval without regard for the
current value. Returns newval."
  (declare (type atom atom))
  (bt:with-lock-held ((atom-lock atom))
    (setf (atom-value atom) newval)))

(defun reset-vals! (atom newval)
  "Sets the value of atom to newval. Returns multiple values '(old new), the value of the
atom before and after the reset."
  ;; See `swap-vals!` for return value notes.
  (declare (type atom atom))
  (bt:with-lock-held ((atom-lock atom))
    (let ((oldval (atom-value atom)))
      (setf (atom-value atom) newval)
      (values oldval newval))))

;;; In our BT/mutex constrained implementation of swap-like calls, the function is called
;;; only once, but we reserve the right to call it multiple times in case we have an optimized
;;; implementation later (and of course it matches Clojure in this regard).

(defun swap! (atom f &rest args)
  "Atomically swaps the value of atom to be:
(apply f current-value-of-atom args). Note that f may be called
multiple times, and thus should be free of side effects.  Returns
the value that was swapped in."
  (declare (type atom atom))
  (bt:with-lock-held ((atom-lock atom))
    (setf (atom-value atom)
          (apply f (atom-value atom) args))))

(defun swap-vals! (atom f &rest args)
  "Atomically swaps the value of atom to be:
(apply f current-value-of-atom args). Note that f may be called
multiple times, and thus should be free of side effects.  Returns un-Clojure-y
multiple values `(old new)`, the value of the atom before and after the swap."
  ;; *TBD*: return a simple vector for closer clojure semantics?
  ;; It would make more sense if we had a clojure-compatible destructuring bind.
  ;; But CL's destructuring bind can't be applied to vectors.
  (declare (type atom atom))
  (bt:with-lock-held ((atom-lock atom))
    (let* ((oldval (atom-value atom))
           (newval (apply f oldval args)))
      (setf (atom-value atom) newval)
      (values oldval newval))))

(defun compare-and-set! (atom oldval newval)
  "Atomically sets the value of atom to newval if and only if the
current value of the atom is identical (EQ) to oldval.
Returns T if set happened, else NIL."
  (declare (type atom atom))
  (bt:with-lock-held ((atom-lock atom))
    (when (eq (atom-value atom) oldval)
      (setf (atom-value atom) newval)
      t)))
      
