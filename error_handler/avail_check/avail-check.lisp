;; Copyright (c) 2015 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This software is released under the MIT License.
;; See the LICENSE file.

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
	     ((numa-error-condition
	       #'(lambda (c)
		   (declare (ignore c))
		   (return-from error-handler-available? t))))
	   (numa-set-membind *numa-no-nodes-bitmask*))
	 nil)
    (uninstall-condition-callback)))
