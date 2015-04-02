(in-package :cl-user)

;; needs: apt-get install libnuma-dev

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cffi-grovel))

(asdf:defsystem :cl-libnuma
  ;; :description ""
  ;; :license "GNU Lesser GPL v 2.1"  ; == same as libnuma ???
  ;; :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:cffi)
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "library")
     (cffi-grovel:grovel-file "grovelling")
     (cffi-grovel:wrapper-file "wrapping")
     (:file "binding")))))

(defmethod asdf:operate :after ((operate (eql 'asdf:load-op)) (component (eql :cl-libnuma)) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (when (/= (symbol-value (find-symbol "+LIBNUMA-API-VERSION+" :cl-libnuma.grovel))
	    (symbol-value (find-symbol "+CL-LIBNUMA-TARGET-API-VERSION+" :cl-libnuma)))
    (warn "libnuma is newer than cl-libnuma implemented"))
  (unless (uiop:symbol-call :cl-libnuma '#:numa-available)
    (warn "libnuma is unavailable in this system. ~
All functions without numa-avaliable are undefined.")))

(asdf:defsystem :cl-libnuma/ext-error
  ;; :description ""
  ;; :license "GNU Lesser GPL v 2.1"  ; == same as libnuma ???
  ;; :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:cl-libnuma)
  :components
  ((:module "ext_error"
    :serial t
    :components
    ((:file "package")
     (:file "wrapper-syntax")
     (cffi-grovel:wrapper-file "wrapping")))))
