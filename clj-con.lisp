(in-package :clj-con)

;;; Assumption: threads that have exited are reclaimed by GC. (Correct?)

;;; Behavior of futures with respect to the following functions and future states.
;;;
;;; Operation          | Active | Success | Cancelled (by future-cancel) | Throws
;;; ------------------------------------------------------------------------------------------
;;; deref                blocks   value     Throws CancellationException   Throws ExecutionException   
;;; realized?            false    true      true (*)                       true (*)
;;; future-done?         false    true      true                           true
;;; future-cancel        true     false     false                          false
;;; future-cancelled?    false    false     true                           false
;;;
;;; (*) The behavior for realized? seems off because realized? being true suggest you can
;;; get a value, which you cannot. The doc string for realized? "a value has been produced".
;;; Maybe it should instead say "the future will not block".
;;;
;;; Behavior implemented matches the above clojure behaviors 
;;; (with conditions rather than exceptions), and T/NIL instead of true/false.

;;; CAUTION: Note that the bordeaux-threads `condition-wait` semantics differ
;;; from SBCL's `sb-thread:condition-wait` semantics.
;;; - SBCL's will not re-aquire the lock when it returns nil.
;;; - BT _will_ re-aquire the lock when it return nil.  

(defstruct promise
  "A promise object as created by the `promise` function."
  condition-variable                    ;nil when realized
  lock
  value)                                ;== cv until realized

(defmethod print-object ((promise promise) stream)
  (print-unreadable-object (promise stream :type t :identity t)
    ;; Using `promise-realized?` here without a lock, contrary to other code.
    (format stream "~S" (promise-realized? promise))))

(defstruct (future (:predicate future?))
  "A future object returned by the `future` macro."
  thread                                ;thread executing the function
  lock                                  ;synchronized future state mgmt

  ;; :ready if not started or executing
  ;; :success if the future will have meaningful value
  ;; :unwound if the future had an unhandled condition.
  ;; :cancelled if the future was successfully cancelled with `future-cancel`.
  status

  ;; If :status isn't :success, then the promise may be delivered, but with the condition
  ;; that caused future failure.
  promise                               ;promise notified with function result
  )

(defmethod print-object ((future future) stream)
  (print-unreadable-object (future stream :type t :identity t)
    (format stream "~S" (future-status future))))

(define-condition thread-interrupted () ()
  (:documentation
   "The thread-interrupted condition is signalled in `future` threads when `future-cancel` is called."))

(define-condition cancellation-exception (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Invalid attempt to obtain a future value, via `deref`, on a cancelled future.")))
  (:documentation
   "Signalled by deref (in the calling thread) when a a `future` thread was cancelled.
Named for similarity to clojure/java behavior on deref."))

(define-condition execution-exception (error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Invalid attempt to obtain a future value, via `deref`, on an abnormally unwound future.")))
  (:documentation
   "Signalled by deref (in the calling thread) when a `future` thread unwound its stack with an unhandled signal.
Named for similarity to clojure/java behavior on deref."))

(defmacro with-future-lock (future &body body)
  "Execute body with the future locked."
  `(bt2:with-lock-held ((future-lock ,future))
     ,@body))

(defun promise-realized? (p)
  "True if value has been supplied, caller must lock before calling."
  (not (eql (promise-value p) (promise-condition-variable p))))

(defun promise ()
  "Returns a promise object that can be read with `deref` and set,
once only, with `deliver`. Calls to deref prior to delivery will
block unless the variant of deref with timeout is used. All
subsequent derefs will return the same delivered value without
blocking. See also - `realized?`."
  (let ((cv (bt2:make-condition-variable)))
    (make-promise 
     :lock (bt2:make-lock)
     :value cv                           ; 
     :condition-variable cv)))

(defun deliver (promise val)
  "Delivers the supplied value to the promise, allowing the return of any blocked derefs.
A subsequent call to deliver on a promise will have no effect.

The first deliver call will return the promise.  
Subsequent calls to deliver return NIL.
This is compatible with Clojure, though note that 
`(deliver p 123) (deliver p true)` in clojure causes a ClassCastException,
whereas here the second call returns false."
  (declare (promise promise))
  (bt2:with-lock-held ((promise-lock promise))
    (if (eql (promise-value promise) (promise-condition-variable promise))
        (progn
          (setf (promise-value promise) val)
          (let ((cvar (promise-condition-variable promise)))
            (setf (promise-condition-variable promise) nil)
            (bt2:condition-broadcast cvar))
          promise)
        nil)))

(defgeneric realized? (x)
  (:documentation "Returns true if a value has been produced for a promise or future, nil otherwise.")
  (:method ((f future))
    (realized? (future-promise f)))
  (:method ((p promise))
    (bt2:with-lock-held ((promise-lock p))
      (promise-realized? p))))

(defgeneric deref (thing &optional timeout-ms timeout-val)
  ;;([ref] [ref timeout-ms timeout-val])
  ;; Note that our call signature permits timeout-ms without timeout-val, unlike clojure
  (:documentation
   "Used on various objects to obtain a value from an asynchronous construct.

When applied to an atom, yields the current value of the atom.

When applied to a future, will block if computation not complete.
If the future completed unsuccessfully, deref will signal either cancellation-exception
or execution-exception depending on whether it was cancelled or unwound due to unhandled conditions.

When applied to a promise, will block until a value is delivered.

When called with timeout options (valid only for promises and futures),
can be used for blocking and will return
timeout-val if the timeout (in milliseconds) is reached before a
value is available. See also - realized?.

Note that a call to `deref` with a timeout the returns the timeout value
does not force the promise/future to be `realized?`, it may remain unrealized.

Note that if timeout-ms is supplied, timeout-val is also required, to maintain
parity with Clojure's arity-1 and arity-3 (but no arity-2) calls.")

  (:method ((f future) &optional (timeout-ms nil timeout-supplied-p)
                         (timeout-val nil timeout-val-supplied-p))
    (when (and timeout-supplied-p (not timeout-val-supplied-p))
      (error "TIMEOUT-VAL is required if TIMEOUT-MS is supplied"))
    (with-future-lock f
      (let ((s (future-status f)))
        (case s
          (:success   (deref (future-promise f))) ; timeout data not needed
          ;; TBD: I have seen weasel words about lock release being unpredictable
          ;; when conditions are signalled.  Not sure what to do yet.
          (:cancelled (error (make-condition 'cancellation-exception)))
          (:unwound   (error (make-condition 'execution-exception)))
          ;; Still executing, release the future lock so it can post success
          ;; to the future.
          (t (unless (eq s :ready)
               (error "Unexpected future-status ~s in future ~s" s f))))))
    ;; Future was still executing (in :ready state), wait on the promise
    (let ((v (if timeout-supplied-p
                 (deref (future-promise f) timeout-ms timeout-val)
                 (deref (future-promise f)))))
      ;; If we didn't timeout, the future _must_ have completed, because
      ;; deref on the promise without a timeout should not be spurious
      ;; (unlike the condition variable it uses under the hood).
      ;; However the future could still have encountered a condition
      ;; and so whatever the above deref gave us takes a back seat
      ;; if :cancelled or :unwound apply. This is consistent with clojure, see chart
      ;; at top of module.

      ;; Not locking here.  If we get "ready" but it transitioned to success
      ;; while we're looking, we don't care. If we got the timeout value
      ;; before thread cancellation or unwinding, we return the timeout value
      (if (eql v timeout-val)
          v
          (let ((s (future-status f)))
            (case s
              (:ready (error "Unexpected status ~s in future ~s after promise deref." s f))
              (:cancelled (error (make-condition 'cancellation-exception)))
              (:unwound   (error (make-condition 'execution-exception)))
              (:success v)
              (t (error "Unexpected future-status ~s in future ~s" s f)))))))

  ;; Note that CL expects timeouts in terms of seconds, which may be real values
  ;; expressing fractions of seconds.  That's true of `sleep` and also the condition
  ;; variable timeout specifications.
  (:method ((p promise) &optional (timeout-ms nil timeout-supplied-p) timeout-val)
    (let ((timeout-secs (and timeout-supplied-p (/ timeout-ms 1000)))
          (cv (promise-condition-variable p))
          (lock (promise-lock p)))
      (when timeout-secs
        (assert (> timeout-secs 0)))
      (bt2:with-lock-held (lock)
        (loop until (promise-realized? p)
              do (unless (bt2:condition-wait cv lock :timeout timeout-secs)
                   (return timeout-val)) ;NIL waitval == timeout
                 ;; ABCL _always_ returns true on CONDITION-WAIT
                 ;; Avoid infinite loop looking for NIL timeout value from wait
                 ;; and try to provide deref timeout semantics.
                 ;; Unfortunately, this breaks timeout tests on CCL and perhaps
                 ;; others, so we are picky about when we enable it.
                 #+ABCL
                 (when timeout-secs
                   (if (promise-realized? p)
                       (return (promise-value p))
                       (return timeout-val)))
              finally (return (promise-value p)))))))

(defun future-call (thunk)
  "Takes a function of no args and yields a future object that will
invoke the function in another thread, and will cache the result and
return it on all subsequent calls to deref. If the computation has
not yet finished, calls to deref will block, unless the variant
of deref with timeout is used. See also - realized?."
  (let* ((result-promise (promise))
         (future (make-future :status :ready 
                              :promise result-promise
                              :lock (bt2:make-lock)))
         (thread (bt2:make-thread 
                  (lambda () 
                    (handler-case (let ((result (funcall thunk)))
                                    ;; Want future lock to span future and
                                    ;; promize updates, so no `update-future-status`
                                    (with-future-lock future
                                      (setf (future-status future) :success)
                                      (deliver result-promise result)))
                      (thread-interrupted (c)
                        (declare (ignore c))
                        ;; Assuming the thread interrupt came only from
                        ;; `future-cancel`, which may not be a good idea.  TBD.

                        ;; The cancelling thread already set the future status to
                        ;; :cancelled.  And delivered to promise since it was already
                        ;; holding the lock.  So the important thing is that we've
                        ;; interrupted the thunk(?)  and are about to return from the
                        ;; thread. If assertion below fails, we may need to do this:
                        ;;    (deliver result-promise c)

                        ;; If the future isn't in cancelled state, then
                        ;; this interrupt was received from some source
                        ;; other than future-cancel and we want to know about it.
                        (assert (eql :cancelled (future-status future))))
                      (t (condition) 
                        (assert condition)
                        ;; future lock to span future _and_ promise updates
                        (with-future-lock future
                          (setf (future-status future) :unwound)
                          (deliver result-promise condition))))))))
    ;; Retaining this thread for debugging.  Hopefully not a GC issue
    ;; We shouldn't actually *need* the thread attached to the future object
    ;; for anything other than debugging
    (setf (future-thread future) thread)
    future))

(defmacro future (&body body)
  "Takes a body of expressions and yields a future object that will
invoke the body in another thread, and will cache the result and
return it on all subsequent calls to deref. If the computation has
not yet finished, calls to deref will block, unless the variant of
deref with timeout is used. See also - realized?.

Note that multiple-value returns are lost, only the first (assumed) value is returned."
  `(future-call (lambda () ,@body)))

(defun future-cancel (future)
  "Cancels the future, if possible. 
Returns T if the cancellation request is successful, NIL if it is not.
Note that interrupting threads in CL is not as tidy as clojure See SB-THREAD::INTERRUPT-THREAD.
Unless threads are carefully using sb-sys:without-interrupts, 
their unwind handlers may not work right.  Don't expect something as robust as the JVM's
InterruptedException."
  (declare (future future))
  (let ((old-status))
    (with-future-lock future
      (setf old-status (future-status future))
      ;; TODO? get our return code based on what the thread signal handler at top level
      (when (eql :ready old-status)
        (setf (future-status future) :cancelled)
        (deliver (future-promise future) (make-condition 'thread-interrupted))
        (bt2:interrupt-thread (future-thread future)
                             ;; sb-thread:return-from-thread?
                             (lambda () (signal 'thread-interrupted)))))
    ;; `future-cancel` is successful only if we successfully cancelled the future.
    ;; If it was already cancelled or otherwise problematic, we didn't do that.
    (eql old-status :ready)))

(defun future-cancelled? (future)
  "Return T if the future was explicitly (and successfully) cancelled, NIL otherwise."
  (declare (future future))
  (eql :cancelled (with-future-lock future (future-status future))))

(defun future-done? (future)
  "Return T if future is done, NIL otherwise.
It is 'done' if it is in any state other than :ready 
(thus hasn't started, or is executing the supplied forms)."
  (declare (future future))
  ;; If I know more about what constituted safe/atomic/volatile operations in CL
  ;; I'd skip the lock. Right new CL & BT tool nuances are new to me.
  ;; I'm guessing an SBCL barrier would be enough, BT doesn't supply barriers.
  (not (eql :ready (with-future-lock future (future-status future)))))

;; *TBD*: what happens if we unwind when waiting on a condition varaible?
;; i.e. (future (deref p)) (future-cancel *)

;;; *TBD*: can we safely eliminate some locking used just for reads?
