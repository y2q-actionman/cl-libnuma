(in-package :cl-user)

(defpackage :cl-libnuma.test-util
  (:use :cl :cffi)
  (:export
   #:assert-progn
   #:assume-condition
   #:always-success
   #:assert-when
   #:cffi-type-exists
   #:cffi-enum-exists
   #:grovel-constant-exists))

(defpackage :cl-libnuma.test-grovel
  (:use :cl :cl-libnuma.test-util
	:cl-libnuma.grovel)
  (:export
   #:test-grovel))

(defpackage :cl-libnuma.test
  (:use :cl :cffi :cl-libnuma.test-util
	:cl-libnuma)
  (:import-from :cl-libnuma.test-grovel
		#:test-grovel)
  (:export
   #:if-numa-function-exists
   #:main))
