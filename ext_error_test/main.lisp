(in-package :cl-libnuma.ext-error.test)

(defun main ()
  (and (test-wrapper-syntax)
       (test-binding)
       t))
