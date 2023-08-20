;;;;
;;;; Plug tests
;;;;
(in-package #:systems.duck.plugboard/tests)

(5am:def-suite* :plugboard-tests)

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

;;;
;;; For testing notification
;;;
(defextendable ext1 () ())
(defvar *on-enabled-msg* nil)
(defvar *on-disabled-msg* nil)
(defmethod on-enabled ((ext ext1) plugin)
  (push (cons ext plugin) *on-enabled-msg*))
(defmethod on-disabled ((ext ext1) plugin plist)
  (push (cons ext plugin) *on-disabled-msg*))

(defun reset ()
  "Resets all test data and sets all plugins to disabled"
  (disable-all-plugins 'foo)
  (disable-all-plugins 'bar)
  (disable-all-plugins 'baz)
  (disable-all-plugins 'ext1)
  (setf *on-enabled-msg* nil)
  (setf *on-disabled-msg* nil))

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
  (5am:is (equal (list (find-class 'plug6) (find-class 'plug4))
                 (enabled-plugins 'quux))))

(5am:test messages
  (reset)
  (enable 'plug1 'ext1)
  (let ((ext1 (make-instance 'ext1)))
    (5am:is (equal (list (cons ext1 (find-class 'plug1))) *on-enabled-msg*))
    (enable 'plug2 'ext1)
    (format nil "~a" ext1) ; Force U-I-F-R-C to be called on EXT1
    (5am:is (equal (list (cons ext1 (find-class 'plug2))
                         (cons ext1 (find-class 'plug1)))
                   *on-enabled-msg*))
    (let ((ext2 (make-instance 'ext1)))
      (5am:is (equal (list (cons ext2 (find-class 'plug1))
                           (cons ext2 (find-class 'plug2))
                           (cons ext1 (find-class 'plug2))
                           (cons ext1 (find-class 'plug1)))
                     *on-enabled-msg*))
      (disable 'plug1 'ext1)
      (format nil "~a~a" ext1 ext2) ; Force U-I-F-R-C to be called
      (5am:is-true (find (cons ext1 (find-class 'plug1))
                         *on-disabled-msg*
                         :test #'equal))
      (5am:is-true (find (cons ext2 (find-class 'plug1))
                         *on-disabled-msg*
                         :test #'equal)))))
