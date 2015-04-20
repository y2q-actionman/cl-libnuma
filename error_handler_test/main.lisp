(in-package :cl-libnuma.error-handler.test)

(defun main ()
  (unless (cl-libnuma.error-handler.avail-check:error-handler-available?)
    (error "cl-libnuma.error-handler is unavailable.
Please restart the Lisp system, load :cl-libnuma.error-handler before :cl-libnuma, and re-run this test."))
  (and (test-wrapper-syntax)
       (test-binding)
       t))
