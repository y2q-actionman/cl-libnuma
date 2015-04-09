(in-package :cl-libnuma.ext-error.test)

(defparameter *numa-error-called* nil)
(defparameter *numa-warn-called* nil)

(defun numa-error-test-callback (where)
  (setf *numa-error-called* (or where t)))

(defun numa-warn-test-callback (number where)
  (declare (ignore number))
  (setf *numa-warn-called* (or where t)))

(defun install-test-callback ()
  (setf *numa-error* #'numa-error-test-callback
	*numa-warn* #'numa-warn-test-callback))

(defun uninstall-test-callback ()
  (setf *numa-error* nil
	*numa-warn* nil))

(defmacro with-test-callback (() &body body)
  `(let ((*numa-error-called* nil)
	 (*numa-warn-called* nil))
     (install-test-callback)
     (unwind-protect
	  ,@body
       (uninstall-test-callback))))

(defun test-callback-available? ()
  (with-test-callback ()
    (numa-error "test")
    (assert *numa-error-called*)
    (numa-warn 0 "test")
    (assert *numa-warn-called*))
  t)
