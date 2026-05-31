# About

`clj-con` defines a set of concurrency operations modeled after their Clojure
counterparts. Sample operators include `future`, `promise`, `deref`,
`deliver`, and `atom`. See the exported symbols in package.lisp for the full list.

Or, if you're familiar with the Clojure Cheatsheet, the project implements the following:

![Cheatsheet Screenshot](https://github.com/dtenny/clj-con/blob/main/Screenshot_20210621_205249.png?raw=true)

along with `promise` and `deliver`.

## Usage

Mar-03-2024: Added to Ultralisp because quicklisp hasn't been updated in 5
months and there are updates I really wanted to get out.

If you didn't get this via quickload using a quicklisp/ultralisp repo, add it to
your `~/quicklisp/localprojects/` directory and update/wipe the
`system-index.txt` file accordingly, and then you can quickload it.

    ;; See 'local-projects' note in preceding paragraph
    (ql:quickload :clj-con) ; to use the code

or

    ;; To run the tests
    (ql:quickload :clj-con-test)
    (clj-con-test:run-tests)

## Example

A lousy example of CLJ-CON, but some people will relate. Here's
a quick and dirty background alarm in your REPL:

    (defun alarm (seconds &optional description)
      "Create an asynchronous task which will try to deliver an alarm notification via *standard-output* and
    the linux `notify-send` tool if it is available.  The optional description will appear in the message
    if provided."
      (clj-con:future 
        (sleep seconds)
        (loop repeat 3 do (write-char #\bell) (finish-output))
        (if description
            (setf description (format nil " for ~a" description))
            (setf description ""))
        (format t "ALARM! ~d seconds is up~a!~%" seconds description)
        (handler-case 
            (uiop:launch-program
             (list "notify-send" "ALARM" (format nil "~d seconds is up~a!" seconds description)))
          (cl:error () (format *error-output* "Attempt to invoke `notify-send` failed.")))))

    CL-USER> (alarm 10 "Hey!")
    #<CLJ-CON:FUTURE :RUNNING {123E491C53}>
    CL-USER> (print "hello world") ;do something while we wait for alarm to go off

    "hello world" 
    "hello world"
    🔔🔔🔔ALARM! 10 seconds is up for Hey!!


## Supported Lisps

CLJ-CON _should_ run on lisps that support `bordeaux-threads` and
`atomics` packages, but once you start pushing the various lisps hard
your mileage may vary. See TESTED LISPS below.

Lisps supporting the `atomics` package will use `compare-and-swap` behavior
via `atomics:cas` for the `atom` implementation. At the time
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

TL;DR: SBCL/ABCL/CCL behave well. Others (as tested), not so much.

Test results were using Fedora (V38 and V41) on a Intel X86_64 machine using
default memory configurations of the respective implementations. 
The test suite allocates about 50 threads, largely serially, but some lisps
are better at thread and related resource reclamation than others.

By default, CLJ-CON-TEST:RUN-TESTS will run the test suite 50 times
if the first pass succeeds. If this passes we generally say the
implementation support is "GOOD". 

We also run the tests more extensively, either (or both) with
`(run-tests 1000)` or `(dotimes (i 1000) (debug! 'test-suite))`
(from the CLJ-CON-TEST package - both doing essentially the same thing with
different levels of debuggability).

If more extensive runs fail, then the GOOD rating is going to be flagged
as in some way, and possibly downgraded.  We know that FUTURE-CANCEL's use
of BT:INTERRUPT-THREAD is _potentially_ problematic, depending on timing
and lisp implementation.  Whether the failure is serious enough for you not
to use FUTURE-CANCEL it is up to you. That a "GOOD" ranked implementation has
tested hundreds of cancellations without issue bodes well, but it's really
going to depend on what you do in your futures, and the unit tests are as
near to do-nothing futures as you'll see (though there's a lot of cancelled
`sleep` calls).

It's my observation that Clojure programmers rarely use FUTURE-CANCEL.
Diligently written CLojure production threads are generally created with
java interop to set thread names, "daemon" status, and so on, and service
threads watch for a stop condition rather than waiting for an
InterruptedException (though they watch for that too).  Still, the JVM
let's you deliver interupts (benefitting FUTURE-CANCEL and ABCL), 
whereas Lisp implementations not on the JVM have more challenges, though
V1.0.1 changes seem to make FUTURE-CANCEL relatively robust.

### SBCL (many versions up to 2.6.1)          GOOD

RUN-TESTS passes. 

`(run-tests 1000)` and `(dotimes (i 1000) (debug! 'test-suite))`
both pass.

### ABCL 1.9.2, OpenJDK 21.0.10               GOOD (No CAS)

RUN-TESTS passes. 

`(run-tests 1000)` and `(dotimes (i 1000) (debug! 'test-suite))`
both pass.

This now works but required special timeout logic because
BT2:CONDITION-WAIT _always_ returns T on ABCL.  Unfortunatey the logic
which fixes ABCL breaks tests on CCL, and perhaps others.

OpenJDK 17.0.9 was tested with ABCL 1.9.0 on CLJ-CON V1.0.0 some years ago and
was not retested for V1.0.1. It worked fine when tested.

V1.0.1 was tested on OpenJDK 21.0.10 (which is not officially supported
by ABCL) with ABCL 1.9.2 and V1.0.1, and also worked fine.

### CCL 1.13 (lx86cl64 executable)          GOOD

RUN-TESTS passes. 

`(run-tests 1000)` and `(dotimes (i 1000) (debug! 'test-suite))`
both pass as well.

### LispWorks 8.0.1 Personal Edition.                      UNRELIABLE

RUN-TESTS passes in V1.0.0.  V1.0.1 was not tested.

`(dotimes (i 1000) (debug! 'test-suite))` runs out of memory.
Lots of messages on the console "Hanging Unknown thread 5612"

I no longer test LispWorks, its intentional
cripplings are such that I often can't even load common/basic lisp tools
due to lack of memory, much less test my code.

### Allegro CL Express 11.0 (`alisp` executable)           UNRELIABLE

RUN-TESTS passes in V1.0.0.  V1.0.1 was not tested.

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
out of memory. I filed a bug with Franz, but repeated attempts to test
Allegro CL have been full of ugly process deaths and I no longer test with it.

### ECL 21.2.1                                            UNRELIABLE

V1.0.0 works for the minimal `clj-con-test:run-tests` case, but
runs out of memory if the test suite is run repeatedly.

### ECL 26.5.5                                            UNRELIABLE

V1.0.1 passed the basic tests for one iterations but did not successfully
reach 50 iterations. It eventually put out the the following errors:

    ;; one of these
    Condition of type: SIMPLE-ERROR
    Attempted to recursively lock #<lock (nonrecursive) 0x7fac03a324b0> which is already owned by #<process "Unknown thread 767" 0x7fac02657240>

    ;; a bunch of these
    Condition of type: UNBOUND-VARIABLE
    The variable SI:*BREAK-LOCALS* is unbound.

    ;; And finally a few of these before dying or being killed by me
    Excessive debugger depth! Probable infinite recursion!
    Quitting process: #<process Unknown thread 767 0x7fac02657240>.

## Changelog

### V1.0.1

#### Handling of abnormal stack unwinds

A bug was fixed whereby the signalling of conditions that weren't subtypes
of `ERROR`, such as occurs by calls to `WARN`, caused abnormal unwinding of the
future/thread.

Future threads now unwind only when the future exits normally
or some semantically valid unwind occurs (such as unhandled conditions
signalled with `ERROR`, or unwinds that occur without conditions such as via `THROW`.

Note that unwound futures, at this time, _suppress_ invocation of the
debugger if unhandled conditions arise. This is more for compatibility with
Clojure than because it is or isn't "a good thing".  For now, if you really
want to invoke the debugger, you need to set up a handler (in your FUTURE)
for the desired conditions and call `CL:INVOKE-DEBUGGER`.

#### FUTURE-CANCEL improvements

The V1.0.0 behavior wasn't super tidy, was redundantly setting
some internal state, and was causing multiple lisps to fail once I started
using it more heavily in new V1.0.1 testing. 

The V1.0.1 implementation is cleaner and also strives to avoid the use
of BT:INTERRUPT-THREAD if it can.

#### Added the FUTURE-UNWIND-CONDITION function

This is a convenience when you're trying to reason about what went wrong
with unwound futures, such as when you're diagnosing the failure of futures
operating as service threads.

#### Exported CANCELLATION-EXCEPTION, EXECUTION-EXCEPTION conditions

These clojure/java namesakes are signalled by DEREF, now exported
for your handler declaration needs.

#### Added `*FUTURE-THREAD-NAME*` to enable naming of threads

You can now bind `CLJ-CON:*FUTURE-THREAD-NAME*` to a string that will be
used to name the thread created by any call to `FUTURE` in scope.
Done this way (using special variables) in order to keep the API consistent
with Clojure's (which does not allow for naming of future threads).

### V1.0.0 

#### POSSIBLE BREAKING CHANGES IN V1.0.0

1. `compare-and-set!` now returns NIL and non-NIL, instead of
   strict NIL and T values. 
2. `deliver` no longer returns the value delivered, it returns the input promise
   or nil according to clojure semantics, see the doc string for `deliver`.

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

### V0.1.0 - initial bordeaux-threads implementation

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
namespace. If you're looking for Clojure-style persistence, collections, sequences, 
and data structure syntax, check out [clj-coll](https://github.com/dtenny/clj-coll).

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

`gitrepo-feedback@protonmail.com`

Feel free to submit Github issues if appropriate. 
LLM-generated issues are likely to be ignored.
