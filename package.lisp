(in-package :cl-user)

(defpackage :clj-con
  (:use :cl)
  (:shadow atom) 

  (:export

   ;; From clj-con.lisp, Clojure namesakes
   #:deliver
   #:deref
   #:future
   #:future-call
   #:future-cancel
   #:future-cancelled?
   #:future-done?
   #:future?
   #:promise
   #:realized?
   #:execution-exception                ;java namesake for semantic similarity on deref conditions
   #:cancellation-exception             ;ditto

   ;; From clj-con.lisp, extensions to what Clojure offers
   #:future-unwind-condition            ;not part of Clojure's API
   #:*future-thread-name*               ;to specify the name threads created by FUTURE

   ;; From atom.lisp
   #:atom
   #:atom?
   #:reset!
   #:reset-vals!
   #:swap!
   #:swap-vals!
   #:compare-and-set!
   )

  (:documentation
   "Functions and macros that implement concurrency operations styled after
Clojure operators such as `future` and `promise`. Note that timeouts in this
module are also similar to those found in Clojure and/or the JVM and are
expressed as millisecond values, even though Common Lisp normally specifies
timeouts as seconds (or fractions thereof).
"))

