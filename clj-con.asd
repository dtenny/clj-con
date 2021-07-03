(in-package :cl-user)

(defpackage :clj-con-asd
  (:use :cl :asdf))

(in-package :clj-con-asd)

(defsystem :clj-con
  :version "0.1.0"
  :license "MIT"
  :author "Dave Tenny"
  :description "Implements Clojure-styled concurrency operations such as `future` and `promise`."
  :depends-on (:bordeaux-threads)
  :components ((:file "package")
               (:file "clj-con")
               (:file "atom")))
