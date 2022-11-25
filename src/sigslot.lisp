(in-package #:sigslot)

;;;; Signal/slot specialization of the observer pattern.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :sigslot *features*))

(deftype dispatch-type ()
  "The method by which slots are called.

When using :DIRECT, targets are notified on the thread that calls NOTIFY.
If :QUEUED is specified, notifications are queued for later dispatching on
a different thread, and the NOTIFY method returns immediately.
At last, if :MAIN-THREAD is used, notifications are queued on the main thread."
  '(member :direct :queued :main-thread))

(defvar *scheduler-thread* nil
  "The thread the scheduler function uses for dispatching slots to target
objects.")

(defvar *task-queue* nil
  "A task queue where new notifications are pushed for dispatching.")

(defvar *queue-size* 20
  "The capacity of the task queue.

This is by default set to 20. When the queue is full, the thread calling
EMIT will block until there's room for one more.")

(defvar *unhandled-condition-handler*
  #'(lambda (condition)
      (warn "Condition signaled while executing queued slot: ~a"
            condition))
  "The condition handler to call when an a condition is signaled and
not handled in a queued slot.

The default handler will issue a WARN with the condition.")


(defclass %signal (observable) ())

;;; A wrapper for a target object in a signal connection spec.
;;; This will hold the actual target of the connection and will act as a
;;; receiver of UPDATE notifications from the signal (an OBSERVABLE kind).
;;; Upon receiving a notification in the UPDATE method, the SLOT function
;;; will be called using the specified DISPATCH method.
(defclass %wrapper ()
  ((slot :initarg :slot
         :type function)
   (weak-object :initarg :weak-object)
   (dispatch :initarg :dispatch
             :type dispatch-type)))

;;; This specialization of the OBSERVABLE's UPDATE method will dispatch to the
;;; slot of the wrapped object.
(defmethod update ((self %signal) (wrapper %wrapper) &rest rest)
  (with-slots (slot weak-object dispatch) wrapper
    (when-let* ((object (tg:weak-pointer-value weak-object))
                (thunk #'(lambda ()
                           (apply slot object rest))))
      (ecase dispatch
        (:direct (funcall thunk))
        (:queued (jpl-queues:enqueue thunk *task-queue*))
        (:main-thread (call-in-main-thread thunk))))))

;;; Specialized method for searching an object in the observers array.
(defmethod object-index ((self %signal) (object standard-object) &key)
  (let ((test))
    (etypecase object
      (%wrapper (setf test #'compare-object-with-wrapper))
      (standard-object (setf test #'compare-object-with-object)))
    (call-next-method self object :test test)))

;;;; Top level API functions and methods.

(defun make-signal ()
  "Creates a SIGNAL ready to be connected to.

Ideally, you would call this function from the :INITFORM of a class slot."
  (make-instance '%signal))

(defgeneric connect (signal slot object &key dispatch)
  (:documentation "Connects a SIGNAL to a SLOT function for a given OBJECT.
DISPATCH can be one of :DIRECT (default), :QUEUED or :MAIN-THREAD.

Returns T if successfully connected to SIGNAL, otherwise NIL.")
  (:method ((self %signal) (slot function) (object standard-object)
            &key (dispatch :direct))
    ;; Create the scheduler thread and queue if the dispatch method
    ;; is :QUEUED and it's the first time it's used.
    (check-type dispatch dispatch-type)
    (when (and (eq dispatch :queued)
               (null *scheduler-thread*))
      (make-scheduler))

    ;; We'll make a wrapper object which will contain the slot function and
    ;; a weak ref to the target object (we don't want to store a strong ref to
    ;; the target). We'll also add a finalizer to the target object, so our
    ;; function is called upon GC of that object and we can disconnect the
    ;; signal.
    (let* ((weak-object (tg:make-weak-pointer object))
           (wrapped-slot (make-instance '%wrapper :slot slot
                                                  :weak-object weak-object
                                                  :dispatch dispatch)))
      ;; Register wrapper as observer of the signal.
      (when (register-observer self wrapped-slot)
        ;; Add finalizer to OBJECT to disconnect from the signal on GC.
        (tg:finalize object #'(lambda ()
                                (ignore-errors
                                 (disconnect self wrapped-slot)
                                 (setf wrapped-slot nil))))))))

(defgeneric disconnect (signal object)
  (:documentation "Disconnects a SIGNAL from a slot of the given OBJECT.

Returns T if successfully disconnected from SIGNAL, otherwise NIL.")
  (:method ((self %signal) (object standard-object))
    (deregister-observer self object)))

(defgeneric emit (signal &rest rest)
  (:documentation "Emits a SIGNAL to all connected slots.
Additional arguments can be added to the method call and are used when calling
the slot.")
  (:method ((self %signal) &rest rest)
    (apply #'notify self rest)))


;;;; Private functions.

(defun make-scheduler ()
  (assert (null *scheduler-thread*))
  (assert (null *task-queue*))
  (let ((queue (make-instance 'jpl-queues:bounded-fifo-queue
                              :capacity *queue-size*)))
    (setf *task-queue* (make-instance 'jpl-queues:synchronized-queue
                                      :queue queue))
    (setf *scheduler-thread* (bt:make-thread #'update-scheduler
                                             :name "sigslot scheduler"))))

;;; Loops through all tasks in the global queue, dequeues one and executes
;;; it. This will cause all tasks to execute serially in order, so it's not
;;; really for long running tasks.
;;; All unhandled exceptions from the queued tasks are handled here to prevent
;;; the thread from terminating.
(defun update-scheduler ()
  (assert *task-queue*)
  (loop for task = (jpl-queues:dequeue *task-queue*)
        do (handler-case (funcall task)
             (t (c) (funcall *unhandled-condition-handler* c)))))


(defun compare-object-with-wrapper (x y)
  (when (and x y)
    (let* ((xweak-object (slot-value x 'weak-object))
           (xobject (tg:weak-pointer-value xweak-object))
           (yweak-object (slot-value y 'weak-object))
           (yobject (tg:weak-pointer-value yweak-object)))
      (eq xobject yobject))))

(defun compare-object-with-object (x y)
  (when (and x y)
    (let* ((xweak-object (slot-value x 'weak-object))
           (xobject (tg:weak-pointer-value xweak-object)))
      (eq xobject y))))
