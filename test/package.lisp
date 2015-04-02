(in-package :cl-user)

(defpackage :cl-libnuma.test-util
  (:use :cl :cffi)
  (:export
   #:and-assert
   #:assume-condition
   #:cffi-type-exists
   #:cffi-enum-exists))

(defpackage :cl-libnuma.test-grovel
  (:use :cl :cffi :cl-libnuma.test-util
	:cl-libnuma.grovel))

(defpackage :cl-libnuma.test
  (:use :cl :cffi :cl-libnuma.test-util
	:cl-libnuma))
