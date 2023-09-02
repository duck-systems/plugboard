;;;;
;;;; Test system
;;;;
(asdf:defsystem "systems.duck.plugboard.asdf-test"
  :defsystem-depends-on ("systems.duck.plugboard/asdf")
  :components ((:plugboard "test.txt")))
