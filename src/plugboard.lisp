;;;;
;;;; The plugboard plugin utility
;;;;
(in-package #:systems.duck.plugboard)

(defclass plugin-class (standard-class)
  ((extends :reader extends
            :type list ; of symbols
            :documentation "List of classes that this plugin extends"))
  (:documentation "Metaclass for plugins"))

(defun plugin-class-p (class)
  "Checks if CLASS is a plugin class"
  (typep class 'plugin-class))

(defmethod shared-initialize :before ((class plugin-class) slot-names &key extends)
  (declare (ignore slot-names))
  (unless (listp extends)
    (setf extends (list extends)))
  (setf (slot-value class 'extends) extends))

(defmacro defplugin (name direct-superclasses direct-slots &rest options)
  (push `(,(intern (concatenate 'string (symbol-name name) "$sentinel"))) direct-slots)
  `(progn
     (defclass ,name ,direct-superclasses ,direct-slots
       (:metaclass plugin-class)
       ,@(remove :enable options :key #'first))
     ,@(let ((enable-opt (find :enable options :key #'first)))
         (when (or (null enable-opt) (second enable-opt))
           `((enable ',name))))))

(defclass extendable-class (standard-class)
  ()
  (:documentation "Metaclass for extendable classes"))

(defclass extendable-object ()
  ((active-plugins :accessor active-plugins
                   :initarg :active-plugins
                   :type list
                   :documentation "List of plugins known to be active"))
  (:default-initargs :active-plugins nil)
  (:documentation "An extendable object"))

(defgeneric on-enabled (instance plugin-class)
  (:documentation "Called when PLUGIN-CLASS is enabled on INSTANCE")
  (:method (instance plugin-class) nil))

(defgeneric on-disabled (instance plugin-class property-list)
  (:documentation "Called when PLUGIN-CLASS is disabled on INSTANCE")
  (:method (instance plugin-class property-list) nil))

(defun sync-plugins (instance prop-list)
  "Synchronizes the list of active plugins and notifies on change"
  (let* ((previous (active-plugins instance))
         (actual (enabled-plugins instance))
         (removed (set-difference previous actual))
         (new (set-difference actual previous)))
    (loop for plugin in removed do (on-disabled instance plugin prop-list))
    (loop for plugin in new do (on-enabled instance plugin))
    (setf (active-plugins instance) actual)))

(defmethod initialize-instance :after ((instance extendable-object) &key)
  "Syncs up the list of active plugins"
  (sync-plugins instance nil))

(defmacro defextendable (name direct-superclasses direct-slots &rest options)
  `(defclass ,name (,@direct-superclasses extendable-object) ,direct-slots
     ,@options
     (:metaclass extendable-class)))

;;;
;;; Enabling and disabling
;;;
(defun dispatch-plugin-call (plugin extendable fn)
  "Calls FN, possibly multiple times, with every specified combination of PLUGIN and EXTENDABLE.
PLUGIN may be a symbol or a PLUGIN-CLASS instance, while EXTENDABLE can be a symbol, CLASS instance,
list of symbols or classes, or null to use the default extends list from PLUGIN. When calling FN,
PLUGIN and EXTENDABLE are resolved to the underlying class instances."
  (check-type plugin (or symbol plugin-class))
  (check-type extendable (or symbol class list null))
  (cond
    ;; Resolve PLUGIN to a real class
    ((symbolp plugin)
     (dispatch-plugin-call (find-class plugin) extendable fn))
    ;; Default to the list of extends from PLUGIN if null
    ((null extendable)
     (let ((extends-list (extends plugin)))
       (unless (null extends-list)
         (dispatch-plugin-call plugin extends-list fn))))
    ;; Iterate over all EXTENDABLE options
    ((listp extendable)
     (some #'identity ; Return T if any subcalls are T
           (map 'list #'(lambda (x)
                          (dispatch-plugin-call plugin x fn))
                extendable)))
    ;; Resolve EXTENDABLE to a particular class
    ((symbolp extendable)
     (dispatch-plugin-call plugin (find-class extendable) fn))
    ;; Only option left: real classes for both PLUGIN and EXTENDABLE
    (t
     (funcall fn plugin extendable))))

(defmacro with-plugin-call ((plugin extendable) &body body)
  "Executes BODY some number of times with PLUGIN and EXTENDABLE resolved to class instances.
See DISPATCH-PLUGIN-CALL for details of the PLUGIN and EXTENDABLE arguments."
  (declare (type symbol plugin extendable))
  `(dispatch-plugin-call ,plugin ,extendable
                         #'(lambda (,plugin ,extendable) ,@body)))

(defun enable (plugin &optional extendable)
  "Enables PLUGIN on EXTENDABLE. Returns T if PLUGIN enabled on any EXTENDABLE."
  (declare (type (or symbol plugin-class) plugin)
           (type (or symbol class list null) extendable))
  (with-plugin-call (plugin extendable)
    (let ((ds (mop:class-direct-superclasses extendable)))
      (unless (find plugin ds)
        (reinitialize-instance extendable
                               :direct-superclasses
                               (cons plugin ds))
        t))))

(defun disable (plugin &optional extendable)
  "Disables PLUGIN on EXTENDABLE. Returns T if PLUGIN disabled on any EXTENDABLE."
  (declare (type (or symbol plugin-class) plugin)
           (type (or symbol class list null) extendable))
  (with-plugin-call (plugin extendable)
    (let ((ds (mop:class-direct-superclasses extendable)))
      (when (find plugin ds)
        (reinitialize-instance extendable
                               :direct-superclasses
                               (remove plugin ds))
        t))))

;;;
;;; Extendable functionality
;;;
(defun dispatch-extendable-call (extendable fn)
  "Calls FN with EXTENDABLE resolved to a class instance"
  (typecase extendable
    (symbol (dispatch-extendable-call (find-class extendable) fn))
    (class (funcall fn extendable))
    (t (dispatch-extendable-call (class-of extendable) fn))))

(defmacro with-extendable-call ((extendable) &body body)
  "Evaluates BODY with EXTENDABLE resolved to a class instance"
  (declare (type symbol extendable))
  `(dispatch-extendable-call ,extendable
                             #'(lambda (,extendable) ,@body)))

(defun enabled-plugins (extendable)
  "Returns a list of enabled plugin classes for EXTENDABLE (symbol, class, or instance)"
  (with-extendable-call (extendable)
    (remove-if-not #'plugin-class-p (mop:class-direct-superclasses extendable))))

(defun disable-all-plugins (extendable)
  "Disables all plugins applied to EXTENDABLE (symbol, class, or instance)"
  (with-extendable-call (extendable)
    (let ((enabled (enabled-plugins extendable)))
      (loop for plugin in enabled do (disable plugin extendable)))))

;;;
;;; Metaclass machinery.
;;;
(defmethod mop:validate-superclass ((class plugin-class) (superclass standard-class)) t)
(defmethod mop:validate-superclass ((class extendable-class) (superclass standard-class)) t)
(defmethod mop:validate-superclass ((class standard-class) (superclass plugin-class)) t)
(defmethod mop:validate-superclass ((class extendable-class) (superclass plugin-class)) t)

(defmethod update-instance-for-redefined-class :after ((instance extendable-object) added discarded plist &key)
  (sync-plugins instance plist))
