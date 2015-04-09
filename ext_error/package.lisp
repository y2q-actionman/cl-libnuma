(in-package :cl-user)

(defpackage :cl-libnuma.wrapper-syntax
  (:use :cl :cffi :cffi-grovel)
  (:export
   #:*next-library-default*
   #:*print-error-message*))

(defpackage :cl-libnuma.ext-error.wrapper
  (:use)
  (:export
   #:*numa-error-callback*
   #:*numa-warn-callback*))

(defpackage :cl-libnuma.ext-error
  (:use :cl :cffi :cl-libnuma
	:cl-libnuma.ext-error.wrapper)
  (:export
   #:numa-error
   #:numa-warn
   #:*numa-error-callback*
   #:*numa-warn-callback*))
