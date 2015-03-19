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
	       (:file "library")
	       (cffi-grovel:grovel-file "grovelling")
	       (cffi-grovel:wrapper-file "wrapping")
	       (:file "binding")
	       (:file "api")))

(defmethod asdf:operate :after ((operate (eql 'asdf:load-op)) (component (eql :cl-libnuma)) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (unless (zerop (uiop:symbol-call :cl-libnuma '#:numa-available))
    (warn "libnuma is unavailable in this system. ~
All functions without numa-avaliable are undefined.")))
