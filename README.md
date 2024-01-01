# About

`clj-con` defines a set of concurrency operations modeled after their Clojure
counterparts. Sample operators include `future`, `promise`, `deref`,
`deliver`, and `atom`. See the exported symbols in package.lisp for the full list.

Or, if you're familiar with the Clojure Cheatsheet, the project implements the following:

![Cheatsheet Screenshot](https://github.com/dtenny/clj-con/blob/main/Screenshot_20210621_205249.png?raw=true)

along with `promise` and `deliver`.

## Usage

If you didn't get this via quickload from the quickload repo (because it isn't
there yet), add it to your `~/quicklisp/localprojects/` directory, update/wipe
the `system-index.txt` file accordingly, and then you can quickload it.

    ;; See 'local-projects' note in preceding paragraph            
    (ql:quickload :clj-con) ; to use the code

or

    ;; To run the test (again, see 'local-projects' note)
    (ql:quickload :clj-con-test)
    (clj-con-test:run-tests)

## Supported Lisps

This package will wishfully run on any lisp supporting `bordeaux-threads`,
which is most of them. 

Lisps supporting the `atomics` package will use `compare-and-swap` behavior
via `atomics:cas` for the `atom` implementatio. At the time
of this writing (DEC 2023), that includes:

- Allegro
- CCL
- ECL
- LispWorks
- Mezzano
- SBCL
- CMUCL
- CLASP (*)

*CLASP isn't on the atomics README but does seem to be supported in the code.

If the atomics-provided `:atomics-cas-svref` isn't in `*features*` then the
implementation defaults to a locking behavior to emulate compare-and-swap
in the various `atom` functions that require it.

## Tested Lisps

Here are my experiences so far with Fedora 38 running on an Intel machine
and tests run on some of the lisps.  Note that I am primarily an SBCL user
and am not particularly familiar with the other lisps, their heap
configurations, or even how to debug them since I didn't bother to enable
them for SLIME, I just ran them from the command line.

All tests were run with default memory configurations. I have no explanation
for why some of them seem to be running out of memory, though some may be
running with overly conservative heap sizes by default.
The test suite allocates fewer than 50 threads, but the CLJ-CON package
does expect threads to be reclaimed when the lisp code run on them returns.

Given that the test suite does deliberately signal conditions in the bodies
of many thread tests I suppose it's possible threads are hung and locks
are not being released.  There are caveats about unpredicable unwind
behavior w.r.t. locks in some of the tools used.

All locking is done via `bt2:with-lock-held`.  If you want lisps that seem
to keep chugging along even with many allocations, look for the ones I've
labelled "GOOD".

- SBCL                                                    GOOD

  RUN-TESTS passes. 
  `(dotimes (i 1000) (debug! 'test-suite))` passes.

- CCL                                                     GOOD

  RUN-TESTS passes. 
  `(dotimes (i 1000) (debug! 'test-suite))` passes.

  The test seemed to slow a bit toward the end of the 1000 iterations, as
  if perhaps a lot of gc activity were happening, but it did finish.

- ABCL 1.9.0, OpenJDK 17.0.9                              GOOD (No CAS)

  RUN-TESTS passes. 
  `(dotimes (i 1000) (debug! 'test-suite))` passes.

  This now works but required special timeout logic because
  BT2:CONDITION-WAIT _always_ returns T on ABCL.  Unfortunatey the logic
  which fixes ABCL breaks tests on CCL, and perhaps others.

- LispWorks 8.0.1 Personal Edition.                      UNRELIABLE

  RUN-TESTS passes. 
  `(dotimes (i 1000) (debug! 'test-suite))` runs out of memory.
  Lots of messages on the console "Hanging Unknown thread 5612"

  On a personal note. No init file with personal edition. Really?

- Allegro CL Express 11.0 (`alisp` executable)           UNRELIABLE

  RUN-TESTS passes. 
  `(dotimes (i 1000) (debug! 'test-suite))` gets the following 
  error after a number of iterations:

    Running test suite TEST-SUITE
     Running test PROMISE-DELIVERY ....
     Running test NO-TIMEOUT-WAITS Allegro CL(pid 1082089): System Error (gsgc) Object already pointing to target newspace half: 0x1000c8a1a68
    The internal data structures in the running Lisp image have been
    corrupted and execution cannot continue.  Check all foreign functions
    and any Lisp code that was compiled with high speed and/or low safety,
    as these are two common sources of this failure.  If you cannot find
    anything incorrect in your code you should contact technical support
    for Allegro Common Lisp, and we will try to help determine whether
    this is a coding error or an internal bug.

  The message suggests a gc bug, but maybe that's just a symptom of running
  out of memory.

- ECL 21.2.1                                            UNRELIABLE

  Works for the minimal (run once) `clj-con-test:run-tests` case, but
  runs out of memory if the test suite is run repeatedly.


## V1.0.0, possible breaking changes

1. `compare-and-set!` now returns NIL and non-NIL, instead of
   strict NIL and T values. 
2. `deliver` no longer returns the value delivered, it returns the input promise
   or nil according to clojure semantics, see the doc string for `deliver`.

## Changelog

### v1.0.0 

#### Tested and fixed for multiple platforms.

See "Tested Lisps" above.

#### Add support for compare-and-swap

Added conditional use of the `atomics` package for a real compare-and-swap
behavior in the `atom` implementation.

#### Eliminate use of recursive locks used with condition-variables (ECL fix)

Recent testing with ECL found that ECL doesn't like condition broadcasts with
recursive locks. The recursive locks were changed to non-recursive locks,
hopefully without loss of functionality or introduction of bugs.

#### Migration to Bordeaux-Threads APIV2

The motiviation was to use `CONDITION-BROADCAST` which is not in APIV1
and was forcing CLJ-CON code to loop on `CONDITION-NOTIFY`.

### v0.1.0 - initial bordeaux-threads implementation

Only tested with SBCL and ABCL, known to be broken on ECL.

## Differences from Clojure

### Uses of multiple value return

`reset-vals!` and `swap-vals!` return vectors in Clojure but return
multiple values here. I couldn't see the point of returning vectors when CL
has no destructuring bind that works on vectors. At least you can use
multiple-value-bind if you want, though it doesn't destructure either.

Tip: [metabang-bind](https://github.com/hraban/metabang-bind) (available in
quicklisp) provides a nice destructuring tool that also handles multiple
values.

### Character/number EQ is not identical to Java's `==` used by Clojure

YMMV if you use the atomics-enabled `compare-and-swap` behavior on Common
Lisp characters and numbers, because it uses `EQ` semantics, not `EQL`, and 
EQ is not necessarily true for numbers and characters.

In SBCL, fixnums are usually EQ, and `(eq #\a #\a)` will likely return
true, but have a care. 

## `atom` package conflict

If you're going to `(use :clj-con)` note that `atom` requires a
`(:shadowing-import-from #:clj-con #:atom)`.

## Use of `interrupt-thread` by `future-cancel`

The Java Virtual Machine's threading tools are really a marvelous thing.  If
you've been in that ecosystem a long time, going back to pthreads with some of
its limitations (or lisp oddities built on them), will feel fragile, and
reading the various SBCL source comments on `interrupt-thread` doesn't do much
to prevent that feeling.

The test suite does test `future-cancel` and other ways of unwinding the
thread stack, and seems to work on all tested platforms.  But it may still be a
source of bugs, such as the memory problems noted on some lisps.

Have a care if you are repeatedly interrupting threads or using complicated mission
critical handlers in the threads unless you have taken to heart the use of
SBCL's WITHOUT-INTERRUPTS and other appropriate implementation dependent
tools. I didn't hit any problems with my simple tests but that isn't saying
much.

## Non-Goals

There is no attempt here to bring clojure syntax or persistent data structures to
Common Lisp.  Fortunately neither of those things is particularly prevalent in
Clojure's concurrency operator model, at least not in the clojure.core
namespace. 

Some enterprising person might want to make a readtable that maps `@` to
`deref`, assuming it doesn't conflict with `,@`, but that hasn't been done
here so you'll just have to call `deref`.

## Blocking Queues?

If you're missing clojure.core.async and want some blocking queues for producer/consumer
situations, take a look at the `lparallel.queue` package `(ql:quickload
:lparallel)`. Unlike clojure.core.async it has a `peek` operator which I find useful
when I need to speculatively try something on a queue element without losing FIFO ordering.

The Atomics maintainers were considering adding some queue capabilities in
2023, so you may wish to check there as well. It isn't in the quicklisp
distribution as of June 2023 though.

## Cautionary note for Clojure devs new to Common Lisp

I recommend reading documentation on the bordeaux-threads 
[make-thread](https://sionescu.github.io/bordeaux-threads/threads/make-thread/) 
function for cautions about interactions between threads and dynamic variables.

You also need to mentally prepare yourself for how values you've closed
over in the body of your `future` can mutate.  Consider this example:

    (dotimes (i 20)
      (future ... (print i) ...))

You may be expecting the first value of `i` printed by the first future created
would be zero because dotimes starts at zero.  However depending on your lisp
implementation it may actually print one, or some other value, depending on
time of evaluation and whether the reference to the location/register
holding 'i' has been incremented or not by the time the future body is
executed on the new thread.

The `clj-con-test` package has a test case where this exact issue was encountered on
SBCL, and the workaround was to use something like this:

    (dotimes (i 20)
      (let ((i2 i))          ;of course I could have rebound 'i' as well
        (future ... (print i2) ...)))

This way the future is referencing a binding that won't change.

Binding semantics such as the above may vary by lisp implementation and has
nothing to do with parallelism.  E.g., you _might_ get this:

    (let ((funs nil)) 
      (dotimes (i 3) (push (lambda () i) funs))
      (dotimes (j 3) (print (funcall (elt funs j)))))
    3
    3
    3

The behavior is related to closing over bindings for mutable data.
The CL spec for [dotimes](http://clhs.lisp.se/Body/m_dotime.htm) says this:

    "It is implementation-dependent whether dotimes establishes a new binding
    of var on each iteration or whether it establishes a binding for var once
    at the beginning and then assigns it on any subsequent iterations."

When in doubt, add a binding that won't change for use in your closed over
`future` (or other) bodies.

## Feedback welcome

`(reverse "moc.liamg@ynnet.evad")`

This is a secondary address that isn't monitored every day.
Feel free to submit Github issues if appropriate.
