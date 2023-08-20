;;;;
;;;; System definition for the Plugboard Plugin Utility
;;;;
(asdf:defsystem "systems.duck.plugboard"
  :description "Plugin utility"
  :version "0.0.1"
  :author "Keith Johnson <quack@duck.systems>"
  :license "MIT"
  :in-order-to ((test-op (test-op "systems.duck.plugboard/test")))
  :depends-on ("closer-mop")
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "plugboard")))

(asdf:defsystem "systems.duck.plugboard/test"
  :description "Tests for plugboard"
  :author "Keith Johnson <quack@duck.systems>"
  :license "MIT"
  :perform (test-op (op c)
                    (symbol-call :fiveam :run! :plugboard-tests))
  :depends-on ("fiveam" "systems.duck.plugboard")
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "tests")))
