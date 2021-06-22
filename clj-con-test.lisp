(in-package :cl-user)

(defpackage :clj-con-test
  (:use :cl :bt :clj-con :fiveam)
  (:shadowing-import-from #:clj-con #:atom)
  (:documentation "Tests for the :clj-con package."))

(in-package :clj-con-test)

(def-suite test-suite :description ":clj-con tests")
(in-suite test-suite)

(test promise-delivery
  "Test promise delivery"
  (let ((p (promise))
        (result "Hello World"))
    (is (string= result (deliver p result)))
    (is (realized? p))
    (is (string= result (deref p)))
    (is (string= result (deliver p result)))))

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
    (is (= 666 (deref p 10 666)))
    (is (not (realized? p)))
    (is (not (realized? f1)))
    (is (= 667 (deref f1 10 667)))
    (is (not (realized? f1)))
    (deliver p t)
    (is (eq t (deref p 10 nil)))
    (is (eq t (deref f1 10 nil)))))

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
      (is (eql 123 (deref f1 10000 nil)))
      (is (eql 456 (deref f2 10000 nil)))
      ;; b will have perfectly descending sequence from 40 to 1.
      (is (= 40 (deref a)))
      (is (equalp (loop for x from 40 downto 1 collecting x) (deref b))))))

;;(explain! (run 'test-suite))

;; Test what happens if we call future-cancel on a pending wait/lock?
