;; Copyright (c) 2015 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This software is released under the MIT License.
;; See the LICENSE file.

(in-package :cl-user)

;; needs: apt-get install libnuma-dev

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cffi-grovel))

(asdf:defsystem :cl-libnuma
  :description "A set of CFFI bindings for numa(3)."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:cffi)
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "library")
     (cffi-grovel:grovel-file "grovelling")
     (cffi-grovel:wrapper-file "wrapping" :soname "cl-libnuma-wrapping")
     (:file "binding"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-libnuma.test)))) 

(defmethod asdf:operate :after ((operate (eql 'asdf:load-op)) (component (eql :cl-libnuma))
				&rest args &key &allow-other-keys)
  (declare (ignore args))
  (when (/= (symbol-value (find-symbol "+LIBNUMA-API-VERSION+" :cl-libnuma.grovel))
	    (symbol-value (find-symbol "+CL-LIBNUMA-TARGET-API-VERSION+" :cl-libnuma)))
    (warn "libnuma is newer than cl-libnuma implemented"))
  (unless (uiop:symbol-call :cl-libnuma '#:numa-available)
    (warn "libnuma is unavailable in this system. ~
All functions without numa-avaliable are undefined.")))

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


(asdf:defsystem :cl-libnuma.error-handler
  :description "cl-libnuma, a set of CFFI bindings for numa(3), with lispy error handlings. (experimental)"
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :components
  ((:module "error_handler"
    :serial t
    :components
    ((:file "package")
     (:file "wrapper-syntax")
     (cffi-grovel:wrapper-file "wrapping" :soname "cl-libnuma-error-handler-wrapping")
     (:file "condition")
     )))
  :in-order-to ((asdf:load-op (asdf:load-op #:cl-libnuma) ; This must be loaded after!
			      (asdf:load-op #:cl-libnuma.error-handler.avail-check))
		(asdf:test-op (asdf:test-op #:cl-libnuma.error-handler.test))))

(asdf:defsystem :cl-libnuma.error-handler.avail-check
  :description "An availability check for cl-libnuma.error-handler."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :pathname "error_handler/avail_check"
  :components
  ((:file "avail-check")))

(defmethod asdf:operate :after ((operate (eql 'asdf:load-op))
				(component (eql :cl-libnuma.error-handler.avail-check))
				&rest args &key &allow-other-keys)
  (declare (ignore args))
  (unless (uiop:symbol-call :cl-libnuma.error-handler.avail-check '#:error-handler-available?) ; TODO: naming
    (warn "cl-libnuma's error handlers are unavailable. Please load cl-libnuma.error-handler at first.")))

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
