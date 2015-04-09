(in-package :cl-libnuma.ext-error.test)

(defparameter *numa-error-called* nil)
(defparameter *numa-warn-called* nil)

(defcallback numa-error-test-callback :void ((where :string))
  (setf *numa-error-called* (or where t)))

(defcallback numa-warn-test-callback :void ((number :int) (where :string))
  (declare (ignore number))
  (setf *numa-warn-called* (or where t)))

(defun install-test-callback ()
  (setf *numa-error-callback* (callback numa-error-test-callback)
	*numa-warn-callback* (callback numa-warn-test-callback)))

(defun uninstall-test-callback ()
  (setf *numa-error-callback* (null-pointer)
	*numa-warn-callback* (null-pointer)))

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
