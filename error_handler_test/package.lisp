;; Copyright (c) 2015 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This software is released under the MIT License.
;; See the LICENSE file.

(in-package :cl-user)

(defpackage :cl-libnuma.wrapper-syntax.test
  (:use :cl :cl-libnuma.test-util
	:cl-libnuma.wrapper-syntax)
  (:export
   #:test-wrapper-syntax))

(defpackage :cl-libnuma.error-handler.test
  (:use :cl :cl-libnuma.test-util
	:cl-libnuma
	:cl-libnuma.error-handler)
  (:import-from :cl-libnuma.test
		#:if-numa-function-exists)
  (:import-from :cl-libnuma.wrapper-syntax.test
		#:test-wrapper-syntax)
  (:export
   #:main))
