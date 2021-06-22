(in-package :clj-con)

;;; TBD, whether to support a reader macro for `@` on promises, futures, and so on, 
;;; that expands to `deref`.

;;; TBD, if we were to implement `locking`, it'd be nice to implment wait() and notify()
;;; and notifyAll() with some kind of monitor implementation.  You could also do a poor man's
;;; CLOS inheritance of some kind of 'monitored' class, which added the nessary support.
;;; Meanwhile, you'll notice there's no `locking` exported by this module.

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


;;; On blocking processes: promises can block multiple threads. SBCL thread
;;; support has a 'broadcast' operation to wake them all, but Bordeaux threads
;;; does not (and I'm trying to stick with Bordeaux threads for portability).
;;; So I'm counting the waiting threads myself and looping on the notify calls. Seems to
;;; work, at least for SBCL.  Let me know if there's a cleaner way, nothing
;;; leaped out at me looking at Bordeaux threads.

(defstruct promise
  "A promise object as created by the `promise` function."
  condition-variable                    ;nil when realized
  lock
  waiter-count                          ;n-threads to notify
  value)                                ;== cv until realized

(defstruct (future (:predicate future?))
  "A future object returned by the `future` macro."
  thread                                ;thread executing the function

  ;; :ready if not started or executing
  ;; :success if the future will have meaningful value
  ;; :unwound if the future had an unhandled condition.
  ;; :cancelled if the future was successfully cancelled with `future-cancel`.
  status

  ;; If :status isn't :success, then the promise may be delivered, but with the condition
  ;; that caused future failure.
  promise                               ;promise notified with function result
  )

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
  `(bt:with-lock-held ((promise-lock (future-promise (the future ,future))))
     ,@body))

(defun update-future-status (future new-status)
  "Acquire a lock on the future and update its status to `new-status`.
  Return the old status."
  (declare (future future))
  (with-future-lock future
    (let ((old-status (future-status future)))
      (setf (future-status future) new-status)
      old-status)))

(defun promise-realized? (p)
  "True if value has been supplied, caller must lock before calling."
  (not (eql (promise-value p) (promise-condition-variable p))))

(defun promise ()
  "Returns a promise object that can be read with `deref` and set,
once only, with `deliver`. Calls to deref prior to delivery will
block unless the variant of deref with timeout is used. All
subsequent derefs will return the same delivered value without
blocking. See also - `realized?`."
  (let ((cv (bt:make-condition-variable)))
    (make-promise 
     :lock (bt:make-recursive-lock)
     :waiter-count 0
     :value cv                           ; 
     :condition-variable cv)))

(defun deliver (promise val)
  "Delivers the supplied value to the promise, allowing the return of any blocked derefs.
A subsequent call to deliver on a promise will have no effect.
Returns val."
  (declare (promise promise))
  (bt:with-recursive-lock-held ((promise-lock promise))
    (when (eql (promise-value promise) (promise-condition-variable promise))
      (setf (promise-value promise) val)
      (let ((cvar (promise-condition-variable promise)))
        (setf (promise-condition-variable promise) nil)
        (loop repeat (promise-waiter-count promise)
           do (bt:condition-notify cvar)))))
  val)

(defgeneric realized? (x)
  (:documentation "Returns true if a value has been produced for a promise or future, nil otherwise.")
  (:method ((f future))
    (realized? (future-promise f)))
  (:method ((p promise))
    (bt:with-lock-held ((promise-lock p))
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
value is available. See also - realized?.")

  (:method ((f future) &optional (timeout-ms nil timeout-supplied-p) timeout-val)
    (let ((s (with-future-lock f (future-status f))))
      (case s
        (:success   (deref (future-promise f))) ; timeout shouldn't be necessary, if supplied
        (:cancelled (error (make-condition 'cancellation-exception)))
        (:unwound   (error (make-condition 'execution-exception)))
        ;; Block on promise (possibly with timeout), check future status again after that
        ;; I'm not quite sure this is tight if it comes back ready after derefing the promise
        (:ready     (let ((v (if timeout-supplied-p
                                 (deref (future-promise f) timeout-ms timeout-val)
                                 (deref (future-promise f)))))
                      (if (eql v timeout-val)
                          v
                          (let ((s (with-future-lock f (future-status f))))
                            (case s
                              (:success v)
                              ((:cancelled :unwound) (deref f)) ; recurse to handle negative case
                              (:ready ; why are we still ready?
                               (error "Why is this future still in a ready state? ~s" f))
                              (t (error "Unexpected future-status ~s in future ~s" s f)))))))
        (t          (error "Unexpected future-status ~s" s)))))

  ;; Note that CL expects timeouts in terms of seconds, which may be real values expressing
  ;; fractions of seconds.  That's true of `sleep`, and also the condition variable timeouts.
  (:method ((p promise) &optional (timeout-ms nil timeout-supplied-p) timeout-val)
    (let ((timeout-secs (and timeout-supplied-p (/ timeout-ms 1000))))
      (bt:with-lock-held ((promise-lock p))
        (if (promise-realized? p)
            (promise-value p)
            (progn
              (incf (promise-waiter-count p))
              (if timeout-secs
                  ;; Timeout, may return nil meaning lock not held and timeout expired.
                  ;; Return T means lock held, timeout may or may not have expired.
                  (if (not (bt:condition-wait (promise-condition-variable p) (promise-lock p) 
                                              :timeout timeout-secs))
                      ;; Timeout, no lock held.
                      timeout-val
                      ;; Possible timeout, lock held
                      (if (promise-realized? p)
                          (promise-value p)
                          timeout-val))
                  ;; No timeout, lock always reacquired before returning T
                  (progn (bt:condition-wait (promise-condition-variable p) (promise-lock p))
                         (if (promise-realized? p)
                             (promise-value p)
                             (progn
                               (assert (> (promise-waiter-count p) 0))
                               (decf (promise-waiter-count p)) ;to correct for the incf coming up on recursion
                               (deref p))))))))))) ;recurse and reenter wait after spurious wakeup

(defun future-call (thunk)
  "Takes a function of no args and yields a future object that will
invoke the function in another thread, and will cache the result and
return it on all subsequent calls to deref. If the computation has
not yet finished, calls to deref will block, unless the variant
of deref with timeout is used. See also - realized?."
  (let* ((result-promise (promise))
         (future (make-future :status :ready :promise result-promise))
         (thread (bt:make-thread 
                  (lambda () 
                    (handler-case (let ((result (funcall thunk)))
                                    (with-future-lock future
                                      (setf (future-status future) :success)
                                      (deliver result-promise result)))
                      (thread-interrupted (c)
                        (declare (ignore c))
                        ;; The cancelling thread already set the future status to :cancelled.
                        ;; And delivered to promise since it was already holding the lock.
                        ;; So the important thing is that we've interrupted the thunk(?)
                        ;; and are about to return from the thread.
                        ;;(deliver result-promise c)
                        ) ; return-from-thread?
                      (t (condition) 
                        (update-future-status future :unwound)
                        (deliver result-promise condition)))))))
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
        (bt:interrupt-thread (future-thread future)
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
  ;; If I know more about what constituted safe/atomic/volatile operations in CL
  ;; I'd skip the lock. Right new CL & BT tool nuances are new to me.
  ;; I'm guessing an SBCL barrier would be enough, BT doesn't supply barriers.
  (not (eql :ready (with-future-lock (future-status future)))))

;; *TBD*: what happens if we unwind when waiting on a condition varaible?
;; i.e. (future (deref p)) (future-cancel *)

;;; *TBD*: can we safely eliminate some locking used just for reads?
