# SigSlot for Common Lisp

SigSlot is a simple Common Lisp library that implements the [Signals &
Slots](https://en.wikipedia.org/wiki/Signals_and_slots) pattern.
It is thread-safe and the performance should be enough
for common uses. The typical use case is for UI interactions and decoupling
of objects.

Backing the sigslot implementation is the observer
pattern, which you can also use for simpler needs.


# Dependencies

SigSlot depends on the following libraries:

* alexandria
* bordeaux-threads
* trivial-garbage
* trivial-main-thread

They are all available on [Quicklisp](https://www.quicklisp.org).


# Installation

As SigSlot is not yet available on Quicklisp, you'll have to download
the source code and put it in `~/quicklisp/local-projects/` for
Quicklisp to find it.

Another option is to use [Ultralisp](https://ultralisp.org/).

Then it's as simple as loading the library with `(ql:quickload :sigslot)`.


# Usage

## Signals and Slots

The general usage of signals and slots is by connecting a slot
function from a target object to a signal. First we'll have to add a signal
to our class, by calling `sigslot:make-signal`:

    (defclass dog ()
      ((barked :accessor dog-barked
               :initform (sigslot:make-signal))))
           
    (setq *dog* (make-instance 'dog))

Then we'll add a function where we `EMIT` our signal. We can pass any
parameters we want and the receiving slot we'll be able to pick them
up as &REST parameters.

    (defun bark (dog &key loud)
      (sigslot:emit (slot-value dog 'barked) :loud loud))

Now comes the time to receive those barkings, so we'll `CONNECT` a slot
function of some object to the `barked` signal of our dog object:

    ;; Our slot function.
    (defun on-bark (dog &rest rest)
      (let ((loud (getf rest :loud)))
        (format t "Dog ~a barked~@[ loudly~]" dog loud)))

    ;; We'll just make a dummy object to connect the signal.
    (setq *dummy* (make-instance 'standard-object))

    ;; And we connect our dummy object to the barked signal.
    (sigslot:connect (dog-barked *dog*) #'on-bark *dummy*)


Finally, we can make our dog bark:

    CL-USER> (bark *dog* :loudly t)
    Dog #<STANDARD-OBJECT {10015015D3}> barked loudly

### Dispatching

By default, all signals dispatch on the thread that called `EMIT` in
the order in which they were connected.

On connection you can specify the dispatching method through the `:DISPATCH`
keyword. The options are:

* `:DIRECT`
* `:QUEUED`
* `:MAIN-THREAD`

The default is `:DIRECT` and works as explained above.

If you instead prefer the dispatch to occur on the scheduler thread,
use `:QUEUED`. The slot function will be queued and dispatched later on a
different thread.

Another option is `:MAIN-THREAD`, which causes the slot to be dispatched on
the main thread, useful for UI apps.


### Automatic disconnection

As connections from objects to signals are stored with a weak reference to
the object, if it becomes garbage collected, then it is automatically
disconnected. Because queued slots carry a strong reference to their object,
the object won't be garbage collected until all queued slots referencing it
have run.


### Limitations

Due to the way slot connections are stored, there can only be one slot-object
connected to a single signal. That is, you can have many objects connected
to a signal through a single slot, but you cannot have the same object
connect through multiple slots to the same signal. You can, though, connect
a single object to multiple signals through multiple slots.

This in practice might not be such a problem, as it is not common for
a single object to connect to the same signal more than once. In fact,
the connection is rejected in these cases.

If you still need this behaviour, you'll have to dispatch from the
slot function to the other functions.

## Observers

The underlying implementation of signals is through the `OBSERVABLE`
class, which hosts a collection of observers. To receive a
notification from an observable object, first we need to subclass from
the `OBSERVABLE` class and register our observer object:

    ;; Subclass from the observable mixin.
    (defclass person (sigslot:observable)
      ((age :type fixnum
            :accessor person-age
            :initform 0)))

    ;; Create an instance of our observable person.
    (setq *person* (make-instance 'person))
    ;; For simplicity, we'll reuse the PERSON class for our observer.
    (setq *boss* (make-instance 'person))

    ;; Register our observer object.
    (sigslot:register-observer *person* *boss*)

Then to notify our observers we'll define an `UPDATE` method that will be
called by `NOTIFY` and then call `NOTIFY` on the observable:

    ;; First we'll define our UPDATE method for the observer.
    (defmethod sigslot:update ((self person) (object person) &rest rest)
      (format t "Boss: Happy birthday. Now get back to work!"))

    ;; Then we'll define an :after method for setf of age, where we can
    ;; add our call to NOTIFY. This is just to show how NOTIFY could be
    ;; called when setting a property.
    (defmethod (setf person-age) :after (new-age (object person))
      (sigslot:notify object new-age))

And to test it:

    CL-USER> (setf (person-age *person*) 46)
    Boss: Happy birthday. Now get back to work!
    46

Finally, we can deregister from the `OBSERVABLE` object by calling:

    (sigslot:deregister-observer *person* *boss*)

### What about much simpler needs?

There's always the standard method combinations using :before, :around
and :after, which you can use to patch-in your observer code. It is
less granular than an observer and more limited, but this might be all
you need without bringing in another dependency:

    (defmethod (setf person-age) :after (new-age (object person))
      (format t "Common Lisp rocks!"))


# Running tests

The library is tested primarily under SBCL on macOS, with some minor
testing on CCL and LispWorks. If you use it with success in your projects with
other compilers/architectures/OSes, please let me know so I can update this document.

To run the test suite:

    CL-USER> (ql:quickload :sigslot/tests)
    CL-USER> (in-package #:sigslot/tests)
    CL-USER> (5am:run! 'sigslot)

    Running test suite SIGSLOT
     Running test REGISTER-OBSERVER ....
     Running test DEREGISTER-OBSERVER ...
     Running test NOTIFY-OBSERVER .
     Running test DO-NOT-NOTIFY-NIL-OBSERVER .
     Running test MANY-OBSERVERS .....
     Running test DIRECT-CONNECT ..
     Running test QUEUED-CONNECT ...
     Running test DISCONNECT ..
     Running test DIRECT-EMIT ..
     Running test NO-EMIT-AFTER-DISCONNECT .
     Did 24 checks.
        Pass: 24 (100%)
        Skip: 0 ( 0%)
        Fail: 0 ( 0%)


# License (MIT)

Copyright (c) 2022 Sebastián Benítez <sebastian@ds9soft.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
