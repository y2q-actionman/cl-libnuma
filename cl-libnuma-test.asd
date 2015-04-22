;; Copyright (c) 2015 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This software is released under the MIT License.
;; See the LICENSE file.

(in-package :cl-user)

(asdf:defsystem :cl-libnuma.test
  :description "Tests for cl-libnuma."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:cl-libnuma)
  :components
  ((:module "test"
    :components
    ((:file "package")
     (:file "util" :depends-on ("package"))
     (:file "grovel" :depends-on ("util"))
     (:file "binding-type" :depends-on ("util"))
     (:file "binding-func" :depends-on ("util"))
     (:file "main" :depends-on ("grovel" "binding-type" "binding-func")))))
  :perform (asdf:test-op (o s)
			 (uiop:symbol-call '#:cl-libnuma.test '#:main)))
