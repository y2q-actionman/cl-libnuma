(in-package :cl-user)

(defpackage :cl-libnuma.ext-error.avail-check
  (:use :cl :cl-libnuma :cl-libnuma.ext-error)
  (:export
   #:ext-error-available?))

(in-package :cl-libnuma.ext-error.avail-check)

(defun ext-error-available? ()
  (unwind-protect
       (progn
	 (install-condition-callback)
	 (handler-bind
	     ((numa-warn-condition
	       #'(lambda (c)
		   (declare (ignore c))
		   (return-from ext-error-available? t))))
	   (numa-parse-nodestring "???"))
	 nil)
    (uninstall-condition-callback)))
