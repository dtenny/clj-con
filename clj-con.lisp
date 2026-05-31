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

  ;; :ready if not started
  ;; :running if started still running
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
             (format stream "Invalid attempt to obtain a future value, via `deref`, on an abnormally unwound future. FUTURE-UNWIND-CONDITION may be of use.")))
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
          ;; when conditions are signalled. Do we need to signal out of the lock? Hopefully not.
          (:cancelled (error (make-condition 'cancellation-exception)))
          (:unwound   (error (make-condition 'execution-exception)))
          ((:ready :running) nil) ;continue below
          (t (error "Unexpected future-status ~s in future ~s" s f)))))
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
      (if (eql v timeout-val)
          v
          ;; Future must be complete in one fashion or another
          ;; No lock should be required here.
          (let ((s (future-status f)))
            (case s
              (:cancelled (error (make-condition 'cancellation-exception)))
              (:unwound   (error (make-condition 'execution-exception)))
              (:success v)
              (t (error "Unexpected future-status ~s in future ~s after future-promise deref"
                        s f)))))))

  ;; Note that CL expects timeouts in terms of seconds, which may be real values
  ;; expressing fractions of seconds.  That's true of `sleep` and also the condition
  ;; variable timeout specifications.
  (:method ((p promise) &optional (timeout-ms nil timeout-supplied-p) timeout-val)
    (let ((timeout-secs (and timeout-supplied-p (/ timeout-ms 1000)))
          (cv (promise-condition-variable p))
          (lock (promise-lock p)))
      (when timeout-secs
        (assert (>= timeout-secs 0)))
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

(defun future-unwind-condition (future)
  "In Clojure, if your future unwinds with an exception and you haven't established
some try/catch handling of your own in the future, attempting to DEREF the future
throws a java ExecutionException, and you can potentially pick out details
of the original exception if you catch the ExecutionException.

CLJ-CON mirrors this, if an uncaught _condition_ is signalled such that it would
unwind the stack, then an attempt to DEREF a CLJ-CON FUTURE will signal an error
of type CLJ-CON:EXECUTION-EXCEPTION.

FUTURE-UNWIND-CONDITION tries to make access to the underlying unwinding condition a
bit easier because it's never as straightforward as you expect it to be, and (FIXME)
because it presently needs access to unexported data to be done cleanly.

If the future is _realized_ and experienced an unwind associated with a condition
(e.g. ERROR, CERROR, FUTURE-CANCEL, or other implementation-specific thread events
that may have unwound the stack) return two values:

1. T if the stack was abnormally unwound for any reason, NIL otherwise.
   Note that FUTURE-CANCEL may or may not trigger an unwind.
2. The condition associated with the unwind, if there is one, NIL otherwise.

If the future has not been realized, returns no values."
  (handler-case
      (if (realized? future)
          (progn
            (deref future)            ;may signal condition
            (values nil nil))         ;no condition signalled, no unwind or condition
          (values))
    (cancellation-exception (c)
      (values t c))
    (execution-exception () 
      (if (eq :success (future-status future)) ;lock no longer necessary, we deref'd
          (values nil nil)
          ;; The internal future promise result is the condition.
          ;; *TODO* add the unhandled condition to the signalled execution-exception
          ;; slots instead of peeking at the future promise here.
          (values t (deref (future-promise future)))))))

(defparameter *future-thread-name* nil
  "When bound non-nil this should be a string that will be used as the name of the
thread created in any scoped call to FUTURE.  When NIL, a distinct thread name will
automatically be generated.  This thread-creation-parameter-by-binding technique was
used in order to avoid a FUTURE api incompatible with that provided by Clojure.")

(defun future-call (thunk)
  "Takes a function of no args and yields a future object that will
invoke the function in another thread, and will cache the result and
return it on all subsequent calls to deref. If the computation has
not yet finished, calls to deref will block, unless the variant
of deref with timeout is used. See also - realized?."
  ;; Our goal in the code wrapping the threaded computation is NOT to log errors
  ;; or stop the system from interacting with the debugger when there are unhandled conditions, 
  ;; that is the domain of the thread thunk.
  ;; 
  ;; We have two goals:
  ;; 1. intercept unwinding from the thread to set the future state accordingly
  ;;    and deliver a value.
  ;; 2. 'Deliver' the future value as follows:
  ;;    a. If thunk completes normally and returns a value, return its (primary) value.
  ;;    b. If thunk exits due to an unhandled condition (error or otherwise)
  ;;       return the condition as the value (clojure does something similar)
  ;;    c. If we're unwinding for some other reason unrelated to conditions, 
  ;;       e.g. THROW/RETURN-FROM etc, return NIL.
  (let* ((result-promise (promise))
         (future (make-future :status :ready 
                              :promise result-promise
                              :lock (bt2:make-lock)))
         (thread (bt2:make-thread
                  (lambda () 
                    (let (status result)
                      (with-future-lock future
                        (setf status (future-status future))
                        (when (eq status :ready)
                          (setf status :running (future-status future) :running)))
                      ;; If the future is already in a completion state because of 
                      ;; FUTURE-CANCEL, there's nothing to do. 
                      (when (eq status :running)
                        ;; Execute user FUTURE content
                        (unwind-protect
                             (catch 'unwind ;or use a restart?
                               (let ((*debugger-hook*
                                       ;; If this is invoked, we had an uncaught condition unwinding to the debugger
                                       ;; (vs uncaught conditions via SIGNAL which don't go to the debugger / unwind-the-stack)
                                       ;; For now, we're avoiding locking the future in the handler
                                       (lambda (condition old-hook-value)
                                         (declare (ignore old-hook-value))
                                         (setf result condition status :unwound)
                                         (throw 'unwind nil))))
                                 (handler-case
                                     (setf result (funcall thunk))
                                   (thread-interrupted ()))))
                          ;; FUTURE-CANCEL will already have delivered to the future-promise, if called
                          ;; Otherwise FUTURE-STATUS MUST be :RUNNING if we  are here,
                          ;; and local STATUS is either :RUNNING or :UNWOUND
                          (let (s oops)
                            (with-future-lock future
                              (case (setf s (future-status future))
                                (:cancelled) ;result-promise and status already set
                                (:running 
                                 (setf (future-status future) (if (eq status :unwound) :unwound :success))
                                 (deliver result-promise result))
                                (t      ;unexpected, signal outside lock
                                 (setf oops s))))
                            (when oops
                              (error "Unexpected future state ~s in future ~s" oops future)))))))
                  :name *future-thread-name*)))
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

#|
java.util.concurrent.Future/cancel() says:

boolean cancel(boolean mayInterruptIfRunning)

Attempts to cancel execution of this task. This attempt will fail if the task has
already completed, has already been cancelled, or could not be cancelled for some
other reason. If successful, and this task has not started when cancel is called,
this task should never run. If the task has already started, then the
mayInterruptIfRunning parameter determines whether the thread executing this task
should be interrupted in an attempt to stop the task.

After this method returns, subsequent calls to isDone() will always return
true. Subsequent calls to isCancelled() will always return true if this method
returned true.
---
Clojure's FUTURE-CANCEL is implemented as (.cancel f true)
|#


(defun future-cancel (future)
  "Cancels the future, if possible. 
Returns T if the cancellation request is successful, NIL if it is not.
Note that interrupting threads in CL is not as tidy as clojure See SB-THREAD::INTERRUPT-THREAD.
Unless threads are carefully using sb-sys:without-interrupts, 
their unwind handlers may not work right.  Don't expect something as robust as the JVM's
InterruptedException."
  (check-type future future)
  (let ((old-status))
    (with-future-lock future
      (setf old-status (future-status future))
      ;; TODO? get our return code based on what the thread signal handler at top level
      ;; In :READY state the thread may or may not have started execution.
      (case old-status
        (:ready
         ;; Future thread may or may not have begun execution, but it isn't far enough
         ;; along to have set state to :RUNNING. Pretend, for user purposes, the thread
         ;; hasn't started at all.  The thread prolog will act accordingly when it tries
         ;; to initiate the future's computation. We are avoiding INTERRUPT-THREAD here.
         (setf (future-status future) :cancelled)
         (deliver (future-promise future) (make-condition 'thread-interrupted))
         t)                             ;successful, user task portion of future will never run
        (:running
         ;; The problematic case, we're going to use INTERRUPT-THREAD
         (setf (future-status future) :cancelled)
         (deliver (future-promise future) (make-condition 'thread-interrupted))
         (bt2:interrupt-thread (future-thread future) (lambda () (signal 'thread-interrupted)))
         t)
        (t                              ;:UNWOUND, :CANCELLED, :SUCCESS
         ;; Thread is already done, one way or another.  FUTURE-CANCEL call fails.
         nil)))))

(defun future-cancelled? (future)
  "Return T if the future was explicitly (and successfully) cancelled, NIL otherwise."
  (check-type future future)
  (eql :cancelled (with-future-lock future (future-status future))))

(defun future-done? (future)
  "Return T if future is done, NIL otherwise.
It is 'done' if it is in any state other than :ready 
(thus hasn't started, or is executing the supplied forms)."
  (check-type future future)
  (case (with-future-lock future (future-status future))
    ((:ready :running) nil)
    (t t)))
