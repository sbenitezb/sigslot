(in-package #:sigslot/tests)

(def-suite sigslot)
(in-suite sigslot)

(defvar *observable*)
(defvar *observer*)
(defvar *update*)
(defvar *val*)

(defparameter *default-slot* #'(lambda (o &rest rest) (setf *val* (list o rest))))

(defclass my-observable (observable)
  ((test-signal :accessor my-observable-test-signal :initform (make-signal))))

(defclass my-observer () ())

(defmethod update ((observable my-observable) (self my-observer) &rest rest)
  (declare (ignorable rest))
  ;; Setting *update* to something will flag this method was called.
  ;; We'll increment it to test multiple update calls.
  (incf *update*))

(def-fixture in-observable ()
  "Configure an observable environment"
  (let ((*observable* (make-instance 'my-observable))
        (*observer* (make-instance 'my-observer)))
    (&body)))

(test register-observer
  "Test registering an observer"
  (with-fixture in-observable ()
    (is (zerop (observer-count *observable*)))
    (is-true (register-observer *observable* *observer*))
    (is (= (observer-count *observable*) 1))
    ;; Check double registration is disallowed.
    (let ((condition nil))
      ;; Catch that warning message.
      (handler-bind ((simple-warning #'(lambda (c)
                                         (setf condition c)
                                         (muffle-warning c))))
        (register-observer *observable* *observer*)
        (is-true condition)))))

(test deregister-observer
  "Test deregistering an observer"
  (with-fixture in-observable ()
    (register-observer *observable* *observer*)
    (is-true (deregister-observer *observable* *observer*))
    (is (zerop (observer-count *observable*)))
    ;; Check deregistering a non existing observer returns NIL.
    (is-false (deregister-observer *observable* *observer*))))

(test notify-observer
  "Test notifying an observer"
  (with-fixture in-observable ()
    (register-observer *observable* *observer*)
    (let ((*update* 0))
      (notify *observable*)
      ;; Check the update method is actually called.
      (is (= *update* 1)))))

(test do-not-notify-nil-observer
  "Test not notifying a nil observer"
  ;; We are not using the fixture here as closing over the observer prevents GC
  ;; it in CCL and LispWorks.
  (setq *observable1* (make-instance 'my-observable))
  (setq *observer1* (make-instance 'my-observer))
  (let ((*update* 0))
    (register-observer *observable1* *observer1*)
    ;; We nullify the observer and force a full GC to cause the weak pointers
    ;; to become invalid to test that the object is not notified.
    (setf *observer1* nil)
    (tg:gc :full t)
    (notify *observable1*)
    ;; Check the update method is not called.
    (is (zerop *update*))))

(test many-observers
  "Test with many observers"
  (let ((*observable* (make-instance 'my-observable))
        (observers (make-array 100))
        (*update* 0))
    ;; Register 100 observers.
    (dotimes (i 100)
      (setf (aref observers i) (make-instance 'my-observer))
      (register-observer *observable* (aref observers i)))
    ;; Check that we have all 100 observers.
    (is (= (observer-count *observable*) 100))
    (notify *observable*)
    ;; Check that all 100 observers were notified.
    (is (= *update* 100))

    ;; Deregister 99 observers.
    (dotimes (i 99)
      (deregister-observer *observable* (aref observers i)))
    ;; Check that there's only 1 left.
    (is (= (observer-count *observable*) 1))
    (notify *observable*)
    ;; Check that the last one has been notified.
    (is (= *update* 101))
    
    ;; Remove the last one and check there's no notification.
    (deregister-observer *observable* (aref observers 99))
    (notify *observable*)
    (is (= *update* 101))))

(test direct-connect
  "Test direct connection to a signal"
  (with-fixture in-observable ()
    (with-slots (test-signal) *observable*
      ;; Check connection.
      (is-true (connect test-signal *default-slot* *observer*))
      ;; Check double connection is rejected.
      (let ((condition nil))
        ;; Catch that warning message.
        (handler-bind ((simple-warning #'(lambda (c)
                                           (setf condition c)
                                           (muffle-warning c))))
          (is-false (connect test-signal *default-slot* *observer*)))))))

(test queued-connect
  "Test queued connection to a signal"
  (with-fixture in-observable ()
    (with-slots (test-signal) *observable*
      ;; Check connection.
      (is-true (connect test-signal *default-slot* *observer*
                        :dispatch :queued))
      (is-true sigslot::*scheduler-thread*)
      (is-true sigslot::*task-queue*))))

(test disconnect
  "Test disconnect from signal"
   (with-fixture in-observable ()
    (with-slots (test-signal) *observable*
      (connect test-signal *default-slot* *observer*)
      ;; Check disconnection.
      (is-true (disconnect test-signal *observer*))
      ;; Check double disconnection.
      (is-false (disconnect test-signal *observer*)))))

(test direct-emit
  "Test direct emit"
   (with-fixture in-observable ()
    (with-slots (test-signal) *observable*
      (connect test-signal *default-slot* *observer*)
      (let ((*val* nil))
        (emit test-signal "TEST")
        ;; Check the slot was called.
        (destructuring-bind (object val) *val*
          (is (string= (first val) "TEST"))
          (is (eq object *observer*)))))))

(test no-emit-after-disconnect
  "Test emit doesn't do anything after disconnect"
   (with-fixture in-observable ()
    (with-slots (test-signal) *observable*
      (connect test-signal *default-slot* *observer*)
      (disconnect test-signal *observer*)
      (let ((*val* nil))
        (emit test-signal "TEST")
        ;; Check the slot was not called.
        (is (null *val*))))))
