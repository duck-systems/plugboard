;;;;
;;;; System definition for the Plug Plugin Utility
;;;;
(asdf:defsystem "systems.duck.plug"
  :description "Plugin utility"
  :version "0.0.1"
  :author "Keith Johnson <quack@duck.systems>"
  :license "MIT"
  :in-order-to ((test-op (test-op "systems.duck.plug/test")))
  :depends-on ("closer-mop")
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "plug")))

(asdf:defsystem "systems.duck.plug/test"
  :description "Tests for SYSTEMS.DUCK.PLUG"
  :author "Keith Johnson <quack@duck.systems>"
  :license "MIT"
  :perform (test-op (op c)
                    (symbol-call :fiveam :run! :plug-tests))
  :depends-on ("fiveam" "systems.duck.plug")
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "tests")))
