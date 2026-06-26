(in-package :cl-user)

(defpackage :clj-con-test
  (:use :cl :bt2 :clj-con :fiveam)
  (:shadowing-import-from #:clj-con #:atom)
  (:export #:run-tests)
  (:documentation "Tests for the :clj-con package."))

(in-package :clj-con-test)

(def-suite test-suite :description ":clj-con tests")
(in-suite test-suite)

(test promise-delivery
  "Test promise delivery"
  (let ((p (promise))
        (result "Hello World"))
    (is (eq p (deliver p result)))
    (is (realized? p))
    (is (string= result (deref p)))
    (is (null (deliver p result)))))

(test no-timeout-waits
  (let* ((p (promise))
         (f1 (future (deref p)))
         (f2 (future (deref p))))
    (deliver p 123)
    (is (= 123 (deref f1)))
    (is (= 123 (deref f2)))))
          
(test timeouts
  (let* ((p (promise))
         (f1 (future (deref p))))
    (is (= 666 (deref p 10 666)))       ;V1.0.0 hangs in ABCL 1.9.0
    (is (not (realized? p)))
    (is (not (realized? f1)))
    (is (= 667 (deref f1 10 667)))
    (is (not (realized? f1)))
    (deliver p t)
    (is (eq t (deref p 10 nil)))
    ;; Just because we've delivered the value does NOT mean the future
    ;; completed and delivered to its own result promise.
    ;; We could easily get the timeout value here. Allegro was pretty 
    ;; consistent about revealing my previously flawed assumption here.
    ;; Hopefully 1000 msecs is enough for it to complete.  Allegro has a funny
    ;; sense of sleep too, it may be a NO-OP for <= 75 msecs, I'm not sure.
    ;; https://franz.com/support/documentation/11.0/multiprocessing.html#90-clsleep-and-minimum-sleeping-time
    ;; 1000 ms should not timeout given that we've already delivered to the promise
    ;; blocking the future.  If we must we could have the future set
    ;; another variable synchronization+test purposes.
    (is (eq t (deref f1 1000 nil)))))

(defun running-future ()
  "Return a future that is running long enough to do tests (or cancellations) on it."
  (let* ((p (promise))
         (f (future (deliver p :started) (sleep 123))))
    (is (eql :started (deref p)))
    f))

(defun completed-future ()
  "Return a future that has completed."
  (let ((f (future t)))
    (deref f)
    f))

(defun cancelled-future ()
  (let* ((p (promise))
         (f (future (deliver p :started) (sleep 123))))
    (is (eql :started (deref p)))
    (is (eql t (future-cancel f)))
    f))

(defun thrown-future ()
  "Return a future that unwound abnormally."
  (let ((f (future (error "unwinding"))))
    (signals clj-con::execution-exception  ; requires 5am test context
      (eql :x (deref f 10000 :x))) ; deref on unwound future will signal condition
    f))

;;;
;;; Here we test the matrix in clj-con.lisp for behavior of futures in various states.
;;;

(test future-cancel
  (is (eql t   (future-cancel (running-future))))
  (is (eql nil (future-cancel (completed-future))))
  (is (eql nil (future-cancel (cancelled-future))))
  (is (eql nil (future-cancel (thrown-future)))))

(test future-cancelled?
  (is (eql nil (future-cancelled? (running-future))))
  (is (eql nil (future-cancelled? (completed-future))))
  (is (eql t   (future-cancelled? (cancelled-future))))
  (is (eql nil (future-cancelled? (thrown-future)))))

(test realized?
  (is (eql nil (realized? (running-future))))
  (is (eql t   (realized? (completed-future))))
  (is (eql t   (realized? (cancelled-future))))
  (is (eql t   (realized? (thrown-future)))))

(test future-done?
  (is (eql nil (realized? (running-future))))
  (is (eql t   (realized? (completed-future))))
  (is (eql t   (realized? (cancelled-future))))
  (is (eql t   (realized? (thrown-future)))))

(test future-deref
  (is (eql :x  (deref (running-future) 10 :x)))
  (is (eql t   (deref (completed-future))))
  (signals clj-con::cancellation-exception (deref (cancelled-future)))
  (signals clj-con::execution-exception (deref (thrown-future)))

  ;; Test for race conditions in promises and interaction with deref.
  (let* ((x 0)
         (a (clj-con:atom x))
         (f (future (dotimes (j 10) (swap! a #'1+)))))
    (is (null (deref f)))               ;wait for future to complete
    (is (realized? f))
    (is ( = 10 (deref a))))

  ;; Longer race condition test on promise behavior (internal to future)
  (let* ((x 0)
         (a (clj-con:atom x))
         (futures (make-array 10 :fill-pointer 0)))
    (dotimes (i 10) 
      (vector-push (future (dotimes (j 100) (swap! a #'1+))) futures))
    (dotimes (i 10)
      (deref (aref futures i)))         ;wait for threads to complete
    (assert (= 1000 (deref a)))
    ;; Note that we cannot reliably assert the value of X here, it will depend on
    ;; how the CL implementation wants to implement access to a closed over X in a thread.
    ;; Or such is my guess.  On sbcl 2.6.1 X will be zero, even though the deref of the atom
    ;; is fine (because the atom ls dealing with the data in a context that works across threads).
    #+(OR)(assert (= 1000 x))))

(define-condition a-plain-condition () ())
(define-condition an-error-condition (error) ())

(test future-unwinds
  ;; Generally testing that futures unwind with expected values
  (let ((vanilla-condition (make-condition 'a-plain-condition))
        (error-condition (make-condition 'an-error-condition)))

    ;; Test that the signal generated by WARN doesn't mess things up.
    (let* (a b s
             (f (future 
                  (setf a 1) 
                  (setf s (with-output-to-string (*error-output*) (warn "2")))
                  (setf b 3)
                  4)))
      (is (= 4 (deref f)))
      (is (= 1 a))
      (is (and (stringp s) (plusp (length s))))
      (is (= 3 b)))

    ;; Test for handled non-error SIGNALled condition.
    (let* (a b c 
             (f (future 
                  (setf a 1) 
                  (handler-case 
                      (signal vanilla-condition)
                    (condition (cond)
                      (setf c cond)))
                  (setf b 3)
                  4)))
      (is (= 4 (deref f)))
      (is (= 1 a))
      (is (= 3 b))
      (is (eq vanilla-condition c)))

    ;; Test for unhandled non-error SIGNALled condition.
    (let* (a b
             (f (future 
                  (setf a 1) 
                  (signal vanilla-condition)
                  (setf b 3)
                  4)))
      (is (= 4 (deref f)))
      (is (= 1 a))
      (is (= 3 b)))

    ;; Test for handled error-typed SIGNALled condition.
    (let* (a b c 
             (f (future 
                  (setf a 1) 
                  (handler-case 
                      (signal error-condition)
                    (error (cond)
                      (setf c cond)))
                  (setf b 3)
                  4)))
      (is (= 4 (deref f)))
      (is (= 1 a))
      (is (= 3 b))
      (is (eq error-condition c)))

    ;; Test for unhandled error-typed SIGNALled condition.
    (let* (a b
             (f (future 
                  (setf a 1) 
                  (signal error-condition)
                  (setf b 3)
                  4)))
      (is (= 4 (deref f)))
      (is (= 1 a))
      (is (= 3 b)))

    ;; Test for handled error-typed ERRORed condition.
    (let* (a b c 
             (f (future 
                  (setf a 1) 
                  (handler-case 
                      (error error-condition)
                    (error (cond)
                      (setf c cond)))
                  (setf b 3)
                  4)))
      (is (= 4 (deref f)))
      (is (= 1 a))
      (is (= 3 b))
      (is (eq error-condition c)))

    ;; Test for unhandled error-typed ERRORed condition.
    (let* (a b
             (f (future 
                  (setf a 1) 
                  (error error-condition)
                  (setf b 3)
                  4)))
      ;; Invalid attempt to obtain a future value, via `deref`, on an abnormally unwound future.
      ;; [Condition of type CLJ-CON::EXECUTION-EXCEPTION]
      (signals error (deref f))
      (is (= 1 a))
      (is (null b)))
    ))

(test future-unwind-condition
  (flet ((wait-on (future)
           (handler-case (deref future) (t ()))))
    (let ((f (future (error "hey!") 1)))
      (wait-on f)
      (multiple-value-bind (unwound-p c)
          (future-unwind-condition f)
        (is-true unwound-p)
        (is (typep c 'simple-error))))
    ;; SBCL 2.6.1: Failed 1 in 1000
    (let ((f (future (sleep 60) :done)))
      (future-cancel f)
      (wait-on f)
      (multiple-value-bind (unwound-p c)
          (future-unwind-condition f)
        (is-true unwound-p)
        (is (typep c 'clj-con:cancellation-exception))))
    (let ((f (future (sleep 0.01) :done)))
      (wait-on f)
      (is (null (future-cancel f)))
      (multiple-value-bind (unwound-p c)
          (future-unwind-condition f)
        (is-false unwound-p)
        (is-false c)))
    (let ((f (future :done)))
      (wait-on f)
      (multiple-value-bind (unwound-p c)
          (future-unwind-condition f)
        (is-false unwound-p)
        (is-false c)))
    (let ((f (future (sleep 10) :done)))
      ;; Isn't realized yet
      (is (null (multiple-value-list (future-unwind-condition f)))))
    ))

(test atoms
  (let ((a (atom 0)))
    (is (eql nil (reset! a nil)))
    (is (eql nil (deref a)))
    (is (equal '(nil 1) (multiple-value-list (reset-vals! a 1))))
    (is (equal '(1 2) (multiple-value-list (swap-vals! a #'1+)))))

  (let ((a (atom 0))
        (b (atom ())))
    (is (atom? a))
    (is (= 0 (deref a)))
    (let ((f1 (future 
                (loop repeat 20
                      as x = (swap! a #'1+)
                      do (swap! b (lambda (old) (cons x old))))
                123))
          (f2 (future
                (loop repeat 20
                      as x = (swap! a #'1+)
                      do (swap! b (lambda (old) (cons x old))))
                456)))
      ;; Some extra diagnostics for sporadic failures, now fixed.
      (let ((result (deref f1 10000 nil)))
        (unless (eql 123 result)
          (format t "~%Oops: ~s != 123: ~s~%" result f1))
        (is (eql 123 result)))
      (let ((result (deref f2 10000 nil)))
        (unless (eql 456 result)
          (format t "~%Oops: ~s != 456: ~s~%" result f2))
        (is (eql 456 result)))
      (is (= 40 (deref a)))
      (let ((expected (loop for x from 40 downto 1 collecting x))
            (actual (deref b)))
        (is (null (set-exclusive-or expected actual)))))))

(test future-chain
  ;; Each future waiting on promises bound by another thread.
  ;; Delivering to one promise creates a domino effect.
  (let* ((n 20)
         (promises (loop repeat (+ n 1) collect (promise)))
         (futures  (make-array n :initial-element nil)))
    ;; CAUTION closing over bindings for mutable data (i.e. 'i'),
    (dotimes (i2 n)
      (let ((i i2))                     ;bind i2 to a non-mutating value
        (setf (aref futures i)
              (future 
                ;; future[i] => (range i n) inclusive
                (let ((result (cons i (deref (elt promises (+ i 1))))))
                  (deliver (elt promises i) result)
                  result)))))
    (is (every (complement #'future-done?) futures))
    (is (every (complement #'realized?) futures))
    (is (every (complement #'realized?) promises))
    (deliver (elt promises n) (list n))
    (let ((expected (loop for i upto n collect i)) ;(0 1 2 3 ... 20)
          (future-vals (map 'list #'deref futures)))
      (is (equalp expected (deref (elt promises 0))))
      (is (every #'realized? promises))
      (is (every #'future-done? futures))
      (is (every #'realized? futures))
      (loop for val in future-vals
            for i from 0
            do (is (equalp val (nthcdr i expected))
                   "Future ~d value ~s was not equalp ~s"
                   i val (nthcdr i expected))))))

(defun run-tests (&optional (n 50))
  "Run all :clj-con tests, N times (default 50).

  (dotimes (i 30) (debug! 'test-suite)) (or larger iteration values)
  can also be useful when debugging sporadic bugs."
  (when (explain! (run 'test-suite))
    (format t "First run of tests passed, running ~d more in silent mode.~%" n)
    (let ((failures 0)
          (*print-names* nil))
      (dotimes (i n) 
        (unless (run-all-tests :summary nil)
          (incf failures)))
      (format t "~%Test suite run ~d times, ~d test suite failure~:P.~%" 
              n failures))))
      

;; Test what happens if we call future-cancel on a pending wait/lock?
;; i.e. (future (deref p)) (future-cancel *)
