;;;;
;;;; Plugboard package definition
;;;;
(defpackage #:systems.duck.plugboard
  (:use #:cl)
  (:local-nicknames (#:mop #:closer-mop))
  (:export #:plugin-class #:plugin-class-p #:defplugin
           #:extendable-class #:extendable-class-p #:defextendable
           #:extendable-object #:extendablep
           #:enable #:disable
           #:on-enabled #:on-disabled
           #:enabled-plugins #:disable-all-plugins))
