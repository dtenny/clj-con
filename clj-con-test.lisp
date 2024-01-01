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
  (signals clj-con::execution-exception (deref (thrown-future))))

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

(defun run-tests ()
  "Run all :clj-con tests.
  (dotimes (i 30) (debug! 'test-suite)) (or larger iteration values)
  can also be useful when debugging sporadic bugs."
  (let ((n 50))
    (when (explain! (run 'test-suite))
      (format t "First run of tests passed, running ~d more in silent mode.~%" n)
      (let ((failures 0)
            (*print-names* nil))
        (dotimes (i n) 
          (unless (run-all-tests :summary nil)
            (incf failures)))
        (format t "~%Test suite run ~d times, ~d test suite failure~:P.~%" 
                n failures)))))
      

;; Test what happens if we call future-cancel on a pending wait/lock?
