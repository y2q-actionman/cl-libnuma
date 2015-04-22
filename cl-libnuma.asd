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
