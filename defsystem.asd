(in-package :cl-user)

;; needs: apt-get install libnuma-dev

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cffi-grovel))

(asdf:defsystem :cl-libnuma
  ;; :description ""
  ;; :license "GNU Lesser GPL v 2.1"  ; == same as libnuma ???
  ;; :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:cffi)
  :serial t
  :components ((:file "package")
	       (cffi-grovel:grovel-file "libnuma-grovelling")
	       (:file "libnuma")))

;; This packaging is suspicious!!!!
(defmethod asdf:operate :after ((operate (eql 'asdf:load-op)) (component (eql :cl-libnuma)) &rest args &key allow-other-keys)
  (declare (ignore args))
  (cffi:use-foreign-library cl-libnuma::libnuma))