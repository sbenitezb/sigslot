(in-package #:sigslot)

;;;; Generic observable/observer pattern. This observable will notify on the
;;;; current thread where the NOTIFY function is called from. For a different
;;;; behaviour, in which you can choose the threading model, see the signal &
;;;; slots implementation in sigslot.lisp.

(defconstant +max-nil-weaks-before-packing+ 10)

(defclass observable ()
  ((observers :initarg :observers
              :type (array standard-object)
              :documentation "An array of observers.")
   (pack-counter :initform +max-nil-weaks-before-packing+
                 :type fixnum
                 :documentation "A countdown for packing the observers array
when reaching zero.")
   (lock :initform (bt:make-lock)))
  (:documentation "An OBSERVABLE object can register multiple observers that
get notified when the function NOTIFY is called.

All methods are thread safe and the UPDATE method is dispatched on the same
thread where the NOTIFY method is called."))

;;; Initialize observable slots, particularly the observer array,
;;; an array of weak values to store observer objects. If :OBSERVERS key is
;;; passed, it is expected to contain a list of observer objects.
(defmethod initialize-instance :after ((self observable) &rest initargs &key)
  (with-slots (observers) self
    (setf observers (make-array 0 :element-type 'standard-object
                                  :adjustable t :fill-pointer t))
    (when-let (observer-list (getf initargs :observers))
      (check-type observer-list list)
      (mapc #'(lambda (o) (register-observer self o))
            observer-list))))

(defmethod print-object ((self observable) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~d observer~:p" (%count-observers self))))

(defgeneric observer-count (observable)
  (:documentation "Returns the number of valid observers for OBSERVABLE.")
  (:method ((self observable))
    (bt:with-lock-held ((slot-value self 'lock))
      (%count-observers self))))

(defgeneric register-observer (observable object)
  (:documentation "Register an observer OBJECT to receive notifications.")
  (:method ((self observable) (object standard-object))
    (let ((weak-object (tg:make-weak-pointer object)))
      (with-slots (observers lock) self
        (bt:with-lock-held (lock)
          ;; Check whether the OBJECT already exists in the OBSERVERS array.
          (if (object-index self object)
              (warn "Object has already been registered with observable.")
              (progn
                (vector-push-extend weak-object observers)
                (update-pack-count self))))))))

(defgeneric deregister-observer (observable object)
  (:documentation "Deregister an observer OBJECT from OBSERVABLE so it no longer
receives notifications.")
  (:method ((self observable) (object standard-object))
    (with-slots (observers lock) self
      (bt:with-lock-held (lock)
        ;; Find reference to OBJECT in the array and nullify it if found.
        (when-let ((index (object-index self object)))
          (setf (aref observers index) nil)
          (update-pack-count self))))))

(defgeneric notify (observable &rest rest)
  (:documentation "Notify an OBSERVABLE object's observers by calling UPDATE
across the entire observer collection. All extra arguments specified in REST
are applied to UPDATE.")
  (:method ((self observable) &rest rest)
    (with-slots (observers lock) self
      (bt:with-lock-held (lock)
        ;; Go through each observer in the observers array and call the update
        ;; method on them.
        (map nil #'(lambda (weak-pointer)
                     (when-let ((o (tg:weak-pointer-value weak-pointer)))
                       (apply #'update self o rest)))
             observers)))))

(defgeneric update (observable observer &rest rest)
  (:documentation "Called by an OBSERVABLE object to notify an OBSERVER when
an event occurred.

You MUST implement a specialization of this method for the intended OBSERVER
in order to receive updates."))

;;; Decreases the pack counter and executes PACK-OBSERVERS-ARRAY when it reaches
;;; 0.
(defun update-pack-count (observable)
  (with-slots (pack-counter) observable
    (declare (fixnum pack-counter))
    (decf pack-counter)
    (when (<= pack-counter 0)
      (pack-observers-array observable)
      t)))

;;; Remove all null or invalid weak pointers from the observers array.
(defun pack-observers-array (observable)
  (with-slots (pack-counter observers) observable
    (setf pack-counter +max-nil-weaks-before-packing+)
    (setf observers (delete-if #'invalid-weak-pointer-p
                               observers))))

(defun valid-weak-pointer-p (pointer)
  (and pointer
       (tg:weak-pointer-p pointer)
       (tg:weak-pointer-value pointer)))

(defun invalid-weak-pointer-p (pointer)
  (not (valid-weak-pointer-p pointer)))

(defun %count-observers (observable)
  (with-slots (observers lock) observable
    (count-if #'valid-weak-pointer-p observers)))

(defgeneric object-index (observable object &key)
  (:documentation "Looks for the position of OBJECT in the observers array of
OBSERVABLE. If specified, :TEST is a designator for a comparison function that
is applied to each member of the array. The default is the function EQ.

Returns an array index or NIL.")
  (:method ((self observable) (object standard-object) &key (test #'eq))
    (flet ((object-is-weak-p (weak-pointer)
             (funcall test (tg:weak-pointer-value weak-pointer) object)))
      (let ((array (slot-value self 'observers)))
        (declare (array array))
        (position-if #'(lambda (element)
                         (object-is-weak-p element))
                     array)))))
