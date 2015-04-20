(in-package :cl-user)

(defpackage :cl-libnuma.error-handler.avail-check
  (:use :cl :cl-libnuma :cl-libnuma.error-handler)
  (:export
   #:error-handler-available?))

(in-package :cl-libnuma.error-handler.avail-check)

(defun error-handler-available? ()
  (unwind-protect
       (progn
	 (install-condition-callback)
	 (handler-bind
	     ((numa-warn-condition
	       #'(lambda (c)
		   (declare (ignore c))
		   (return-from error-handler-available? t))))
	   (numa-parse-nodestring "???"))
	 nil)
    (uninstall-condition-callback)))
