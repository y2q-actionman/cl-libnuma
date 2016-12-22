;; Copyright (c) 2015 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This software is released under the MIT License.
;; See the LICENSE file.

(in-package :cl-user)

(defpackage :cl-libnuma.wrapper-syntax
  (:use :cl :cffi :cffi-grovel)
  (:export))

(defpackage :cl-libnuma.error-handler.wrapper
  (:use)
  (:import-from :cl
		#:eval-when #:defparameter #:nil #:setf #:progn #:funcall #:when)
  (:export
   #:*numa-error-callback*
   #:*numa-warn-callback*))

(defpackage :cl-libnuma.error-handler
  (:use :cl
	:cl-libnuma.error-handler.wrapper)
  (:export
   #:*numa-error-callback*
   #:*numa-warn-callback*

   #:numa-error-condition
   #:numa-warn-condition
   #:install-condition-callback
   #:uninstall-condition-callback
   ))
