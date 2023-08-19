;;;;
;;;; Plug tests
;;;;
(in-package #:systems.duck.plug/tests)

(5am:def-suite* :plug-tests)

;;;
;;; Test classes, plugins, and methods
;;;
(defclass foo () ())
(defclass bar () ())
(defclass baz () ())

(defplugin plug1 () ())
(defplugin plug2 () () (:extends bar))
(defplugin plug3 () () (:extends (bar baz)))

;;;
;;; For testing precedence
;;;
(defgeneric coll (obj) (:method-combination list))
(defmethod coll list (obj) "BASE")
(defmethod coll list ((obj foo)) "FOO")
(defmethod coll list ((obj bar)) "BAR")
(defmethod coll list ((obj baz)) "BAZ")
(defmethod coll list ((obj plug1)) "PLUG1")
(defmethod coll list ((obj plug2)) "PLUG2")
(defmethod coll list ((obj plug3)) "PLUG3")

;;;
;;; For testing auto-enabling
;;;
(defclass quux () ())
(defplugin plug4 () () (:extends quux) (:enable t))
(defplugin plug5 () () (:extends quux) (:enable nil))
(defplugin plug6 () () (:extends quux))

(defun reset ()
  "Resets all plugins to disabled"
  (disable-all-plugins 'foo)
  (disable-all-plugins 'bar)
  (disable-all-plugins 'baz))

(5am:test enables/disables
  (reset)
  (5am:is (null (enabled-plugins 'foo)))
  (5am:is-true (enable 'plug1 'foo))
  (5am:is (find (find-class 'plug1) (enabled-plugins 'foo)))
  (5am:is-true (enable 'plug2 'foo))
  (5am:is (find (find-class 'plug1) (enabled-plugins 'foo)))
  (5am:is (find (find-class 'plug2) (enabled-plugins 'foo)))
  (5am:is-true (enable 'plug3 'foo))
  (5am:is (find (find-class 'plug1) (enabled-plugins 'foo)))
  (5am:is (find (find-class 'plug2) (enabled-plugins 'foo)))
  (5am:is (find (find-class 'plug3) (enabled-plugins 'foo)))
  (5am:is-true (disable 'plug2 'foo))
  (5am:is (find (find-class 'plug1) (enabled-plugins 'foo)))
  (5am:is (find (find-class 'plug3) (enabled-plugins 'foo)))
  (5am:is-true (disable 'plug3 'foo))
  (5am:is (find (find-class 'plug1) (enabled-plugins 'foo)))
  (5am:is-true (disable 'plug1 'foo))
  (5am:is (null (enabled-plugins 'foo))))

(5am:test default-extends
  (reset)
  (5am:is (null (enabled-plugins 'foo)))
  (5am:is (null (enabled-plugins 'bar)))
  (5am:is (null (enabled-plugins 'baz)))
  (5am:is-false (enable 'plug1)) ; No default extends
  (5am:is-true (enable 'plug2))
  (5am:is-true (enable 'plug3))
  (5am:is (null (enabled-plugins 'foo)))
  (5am:is (= 2 (length (enabled-plugins 'bar))))
  (5am:is (find (find-class 'plug2) (enabled-plugins 'bar)))
  (5am:is (find (find-class 'plug3) (enabled-plugins 'bar)))
  (5am:is (= 1 (length (enabled-plugins 'baz))))
  (5am:is (find (find-class 'plug3) (enabled-plugins 'baz))))

(5am:test plugin-precedence
  (let ((foo (make-instance 'foo))
        (bar (make-instance 'bar))
        (baz (make-instance 'baz)))
    (reset)
    (enable 'plug2)
    (enable 'plug3)
    (5am:is (equal '("FOO" "BASE") (coll foo)))
    (5am:is (equal '("BAR" "PLUG3" "PLUG2" "BASE") (coll bar)))
    (5am:is (equal '("BAZ" "PLUG3" "BASE") (coll baz)))
    (disable 'plug2 'bar)
    (5am:is (equal '("BAR" "PLUG3" "BASE") (coll bar)))
    (enable 'plug2 'bar)
    (5am:is (equal '("BAR" "PLUG2" "PLUG3" "BASE") (coll bar)))))

(5am:test auto-enable
  (5am:is (equal (list (find-class 'plug4)) (enabled-plugins 'quux))))
