(in-package :cl-user)

(defpackage :clj-con-test-asd
  (:use :cl :asdf))

(in-package :clj-con-test-asd)

(defsystem :clj-con-test
  :version "0.1.0"
  :license "MIT"
  :author "Dave Tenny"
  :description "Tests for the :clj-con package."
  :depends-on (:clj-con :fiveam)
  :components ((:file "clj-con-test")))
