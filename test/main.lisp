(in-package :cl-libnuma.test)

(defun main ()
  (and (cl-libnuma.test-grovel::test-grovel)
       (test-binding-type)
       t))
