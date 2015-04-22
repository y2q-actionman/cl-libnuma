;; Copyright (c) 2015 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This software is released under the MIT License.
;; See the LICENSE file.

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cffi-grovel))

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
				(component (eql :cl-libnuma.error-handler))
				&rest args &key &allow-other-keys)
  (declare (ignore args))
  (unless (uiop:symbol-call :cl-libnuma.error-handler.avail-check '#:error-handler-available?)
    (warn "cl-libnuma-error-handler is unavailable. Please load cl-libnuma.error-handler before cl-libnuma.")))
