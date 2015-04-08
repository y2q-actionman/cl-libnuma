(in-package :cl-libnuma.ext-error.test)

(defun main ()
  (and (cl-libnuma.wrapper-syntax.test::test-wrapper-syntax)
       (test-callback-available?)
       t))
