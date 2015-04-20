(in-package :cl-user)

(defpackage :cl-libnuma.wrapper-syntax
  (:use :cl :cffi :cffi-grovel)
  (:export))

(defpackage :cl-libnuma.error-handler.wrapper
  (:use)
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
