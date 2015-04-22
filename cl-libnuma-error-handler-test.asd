;; Copyright (c) 2015 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This software is released under the MIT License.
;; See the LICENSE file.

(in-package :cl-user)

(asdf:defsystem :cl-libnuma.error-handler.test
  :description "Tests for cl-libnuma.error-handler."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:cl-libnuma.error-handler :cl-libnuma.error-handler.avail-check
					 :cl-libnuma.test)
  :components
  ((:module "error_handler_test"
    :serial t
    :components
    ((:file "package")
     (:file "wrapper-syntax-test")
     (:file "binding")
     (:file "main"))))
  :perform (asdf:test-op (o s)
			 (uiop:symbol-call '#:cl-libnuma.error-handler.test '#:main)))
