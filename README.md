# About

`clj-con` defines a set of concurrency operations modeled after their Clojure
counterparts. Sample operators include `future`, `promise`, `deref`,
`deliver`, and `atom`. See the exported symbols in package.lisp for the full list.

Or, if you're familiar with the Clojure Cheatsheet, the project implements the following:

![Cheatsheet Screenshot](https://github.com/dtenny/clj-con/blob/main/Screenshot_20210621_205249.png?raw=true)

along with `promise` and `deliver`.

# Usage

I'm hoping to get this into quicklisp at some point, for now you can quickload
it if you link to it in your quicklisp `local-projects` subdirectory and
update/wipe your system-index accordingly.

    ;; See 'local-projects' note in preceding paragraph            
    (ql:quickload :clj-con) ; to use the code

or

    ;; To run the test (again, see 'local-projects' note)
    (ql:quickload :clj-con-test)
    (in-package :clj-con-test)
    (explain! (run 'test-suite))

# Implementation Status

This was just a whim to bring Clojure's concurrency operators, which I like to
use in unit testing threaded services, to Common Lisp, mostly because I like to use
them in unit testing threaded services. This initial implementation hasn't been tested beyond the
unit tests at all (and only on SBCL), so set your expectations accordingly.

The definitions are meant to be portable and are implemented entirely on
Bordeaux Threads, but for those same reason they aren't terribly efficient (as
BT has no condition variable broadcast, no compare-and-set, and so on). At some point
I could see adding some optimized conditional compilations.


# Differences from Clojure

`reset-vals!` and `swap-vals!` return vectors in Clojure but return
multiple values here. I couldn't see the point of returning vectors when CL
has no destructuring bind (outside of LOOP?) that works on vectors. At least you can use
multiple-value-bind if you want.

# `atom` package conflict

If you're going to `(use :clj-con)`, note that `atom` requires a
`(:shadowing-import-from #:clj-con #:atom)`.  I'm open to better ways to do
this, I'm not much practiced in the arts of Common Lisp packages.

# Use of `interrupt-thread` by `future-cancel`

The Java Virtual Machine's threading tools are really a marvelous thing.  If
you've been in that ecosystem a long time, going back to pthreads with some of
its limitations (or lisp oddities built on them), will feel fragile, and
reading the various SBCL source comments on `interrupt-thread` doesn't do much
to prevent that feeling.

As this implementation is more a trial balloon than anything else, have a care
about repeatedly interrupting threads or using complicated mission critical
handlers in the threads unless you have taken to heart the use of
WITHOUT-INTERRUPTS and other implementation dependent things. I didn't hit any
problems with my simple tests but that isn't saying much.

# Non-Goals

There is no attempt here to bring clojure syntax or persistent data structures to
Common Lisp.  Fortunately neither of those things is particularly prevalent in
Clojure's concurrency operator model, at least not in the clojure.core
namespace. 

Some enterprising person might want to make a readtable that maps `@` to
`deref`, assuming it doesn't conflict with `,@`, but that hasn't been done
here so you'll just have to call `deref`.

# Blocking Queues?

If you're missing clojure.core.async and want some blocking queues for producer/consumer
situations, take a look at the `lparallel.queue` package `(ql:quickload
:lparallel)`. Unlike clojure.core.async it has a `peek` operator which I find useful
when I need to speculatively try something on a queue element without losing FIFO ordering.

# Feedback welcome

`(reverse "moc.liamg@ynnet.evad")`
