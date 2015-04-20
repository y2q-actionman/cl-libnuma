(in-package :cl-libnuma.error-handler)

(define-condition numa-error-condition (error)
  ((where :initarg :where :initform "" :type string :reader numa-error-condition-where))
  (:report (lambda (condition stream)
	     (format stream "Error on libnuma: ~A"
		     (numa-error-condition-where condition)))))

(define-condition numa-warn-condition (warning)
  ((number :initarg :number :initform 0 :type integer :reader numa-warn-condition-number)
   (where :initarg :where :initform "" :type string :reader numa-warn-condition-where))
  (:report (lambda (condition stream)
	     (format stream "Warning on libnuma: ~A"
		     (numa-warn-condition-where condition)))))

(defun numa-error-condition-callback (where)
  (error 'numa-error-condition :where where))

(defun numa-warn-condition-callback (number where)
  (warn 'numa-warn-condition :where where :number number))

(let ((old-numa-error-callbacks nil)
      (old-numa-warn-callbacks nil))
  (defun install-condition-callback ()
    (push *numa-error-callback* old-numa-error-callbacks)
    (push *numa-warn-callback* old-numa-warn-callbacks)
    (setf *numa-error-callback* #'numa-error-condition-callback
	  *numa-warn-callback* #'numa-warn-condition-callback))
  (defun uninstall-condition-callback ()
    (setf *numa-error-callback* (pop old-numa-error-callbacks)
	  *numa-warn-callback* (pop old-numa-warn-callbacks))))
