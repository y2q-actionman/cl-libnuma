(in-package :cl-user)

;; needs: apt-get install libnuma-dev

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cffi-grovel))

(asdf:defsystem :cl-libnuma
  ;; :description ""
  :license "LLGPL"
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
  ;; :description ""
  :license "LLGPL"
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

(asdf:defsystem :cl-libnuma.ext-error
  ;; :description ""
  :license "LLGPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :components
  ((:module "ext_error"
    :serial t
    :components
    ((:file "package")
     (:file "wrapper-syntax")
     (cffi-grovel:wrapper-file "wrapping" :soname "cl-libnuma-ext-error-wrapping")
     (:file "condition")
     )))
  :in-order-to ((asdf:load-op (asdf:load-op #:cl-libnuma) ; This must be loaded after!
			      (asdf:load-op #:cl-libnuma.ext-error.avail-check))
		(asdf:test-op (asdf:test-op #:cl-libnuma.ext-error.test))))

(asdf:defsystem :cl-libnuma.ext-error.avail-check
  ;; :description ""
  :license "LLGPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :pathname "ext_error/avail_check"
  :components
  ((:file "avail-check")))

(defmethod asdf:operate :after ((operate (eql 'asdf:load-op))
				(component (eql :cl-libnuma.ext-error.avail-check))
				&rest args &key &allow-other-keys)
  (declare (ignore args))
  (unless (uiop:symbol-call :cl-libnuma.ext-error.avail-check '#:ext-error-available?)
    (warn "cl-libnuma's error handlers are unavailable.")))

(asdf:defsystem :cl-libnuma.ext-error.test
  ;; :description ""
  :license "LLGPL"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:cl-libnuma.ext-error :cl-libnuma.test)
  :components
  ((:module "ext_error_test"
    :serial t
    :components
    ((:file "package")
     (:file "wrapper-syntax-test")
     (:file "binding")
     (:file "main"))))
  :perform (asdf:test-op (o s)
			 (uiop:symbol-call '#:cl-libnuma.ext-error.test '#:main)))
