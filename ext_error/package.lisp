(in-package :cl-user)

(defpackage :cl-libnuma.wrapper-syntax
  (:use :cl :cffi :cffi-grovel)
  (:export))

(defpackage :cl-libnuma.ext-error.wrapper
  (:use)
  (:export
   #:*numa-error-callback*
   #:*numa-warn-callback*))

(defpackage :cl-libnuma.ext-error
  (:use :cl
	:cl-libnuma.ext-error.wrapper)
  (:export
   #:*numa-error-callback*
   #:*numa-warn-callback*

   #:numa-error-condition
   #:numa-warn-condition
   #:install-condition-callback
   #:uninstall-condition-callback
   ))
