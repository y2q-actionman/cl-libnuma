(in-package :cl-user)

(defpackage :cl-libnuma.wrapper-syntax.test
  (:use :cl :cffi :cl-libnuma.test-util
	:cl-libnuma.wrapper-syntax)
  (:export
   #:test-wrapper-syntax))

(defpackage :cl-libnuma.ext-error.test
  (:use :cl :cffi :cl-libnuma.test-util
	:cl-libnuma
	:cl-libnuma.ext-error))
