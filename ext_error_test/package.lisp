(in-package :cl-user)

(defpackage :cl-libnuma.wrapper-syntax.test
  (:use :cl :cl-libnuma.test-util
	:cl-libnuma.wrapper-syntax)
  (:export
   #:test-wrapper-syntax))

(defpackage :cl-libnuma.ext-error.test
  (:use :cl :cl-libnuma.test-util
	:cl-libnuma
	:cl-libnuma.ext-error)
  (:import-from :cl-libnuma.test
		#:if-numa-function-exists)
  (:import-from :cl-libnuma.wrapper-syntax.test
		#:test-wrapper-syntax)
  (:export
   #:main))
