(in-package :cl-user)

(defpackage :clj-con-asd
  (:use :cl :asdf))

(in-package :clj-con-asd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; *TBD* Is there a way to do this without hard coding assumptions
  ;; about the atomics package support? Unfortunately, listing it
  ;; as an unconditional :depends-on signals an error on unsupported platforms.
  #+(or allegro ccl clasp ecl lispworks mezzano sbcl cmucl)
  (pushnew :clj-con-use-atomics *features*))

(defsystem :clj-con
  :version "1.0.0"
  :license "MIT"
  :author "Dave Tenny"
  :description "Clojure-style concurrency operations like `future`, `promise`, and `atom`."
  :bug-tracker "https://github.com/dtenny/clj-con/issues"
  :source-control (:git "https://github.com/dtenny/clj-con")
  :depends-on (:bordeaux-threads
               #+clj-con-use-atomics :atomics)
  :serial t
  :components ((:file "package")
               (:file "clj-con")
               (:file "atom")))
