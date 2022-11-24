;;;; package.lisp

(defpackage #:sigslot
  (:use #:cl)
  (:import-from #:alexandria
                #:when-let
                #:when-let*)
  (:import-from #:trivial-main-thread
                #:call-in-main-thread)
  (:export #:observable
           #:observer-count
           #:register-observer
           #:deregister-observer
           #:notify
           #:update
           #:make-signal
           #:connect
           #:disconnect
           #:emit
           #:*queue-size*
           #:*unhandled-condition-handler*))
