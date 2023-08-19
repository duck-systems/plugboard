;;;;
;;;; Plug package definition
;;;;
(defpackage #:systems.duck.plug
  (:use #:cl)
  (:local-nicknames (#:mop #:closer-mop))
  (:export #:plugin-class #:plugin-class-p #:defplugin
           #:enable #:disable
           #:enabled-plugins #:disable-all-plugins))
