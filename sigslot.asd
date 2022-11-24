;;;; sigslot.asd

(asdf:defsystem #:sigslot
  :description "Signals & Slots library"
  :author "Sebastián Benítez <sebastian@ds9soft.com>"
  :homepage "https://github.com/sbenitezb/sigslot"
  :license  "MIT"
  :version "1.0.0"
  :serial t
  :depends-on (#:alexandria
               #:trivial-garbage
               #:trivial-main-thread
               #:bordeaux-threads
               #:jpl-queues)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "observable")
                             (:file "sigslot"))))
  :long-description
  #.(uiop:read-file-string (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "sigslot/tests"))))

(asdf:defsystem #:sigslot/tests
  :description "Signals & Slots library tests"
  :serial t
  :depends-on (#:fiveam
               #:cl-mock
               #:sigslot)
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "suite"))))
  :perform (test-op (o c)
             (uiop:symbol-call :fiveam '#:run! 'sigslot)))
