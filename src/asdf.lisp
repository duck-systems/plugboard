;;;;
;;;; ASDF integration to load plugin files
;;;;
(defpackage #:systems.duck.plugboard.asdf
  (:use #:cl)
  (:export #:plugboard-file))

(in-package #:systems.duck.plugboard.asdf)

;;;
;;; The plugboard file class
;;;
(defclass plugboard-file (asdf:source-file)
  ()
  (:documentation "A plugboard specification file"))

;;;
;;; Read the plugboard file and add dependencies on each listed system
;;;
(defmethod asdf:component-depends-on ((op asdf:prepare-op) (comp plugboard-file))
  (let ((plugins
          (with-open-file (fs (asdf:component-pathname comp) :direction :input)
            (loop for line = (read-line fs nil :eof)
                  until (eql line :eof)
                  collecting (string-trim '(#\Space #\Newline #\Tab #\Return) line)))))
    (append
     (map 'list #'(lambda (c)
                    (format t "PLUGBOARD: Got plugin: ~a~%" c)
                    (list 'asdf:load-op (asdf:find-component c nil)))
          plugins)
     (call-next-method))))

;;;
;;; Define empty perform methods
;;;
(defmethod asdf:perform ((op asdf:compile-op) (comp plugboard-file)) nil)
(defmethod asdf:perform ((op asdf:load-op) (comp plugboard-file)) nil)

;;;
;;; Add :PLUGBOARD as a component name, but only on ASDF >= 3.3.5
;;;
(defmacro class-for-type-form ()
  (when (uiop:version<= "3.3.5" (asdf:asdf-version))
    `(defmethod ,(uiop:find-symbol* :class-for-type :asdf/parse-defsystem)
         ((parent asdf/system:system) (type (eql :plugboard)))
       (find-class 'plugboard-file))))

(class-for-type-form)
