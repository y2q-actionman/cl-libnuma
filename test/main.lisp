(in-package :cl-libnuma.test)

(defun main ()
  (and (test-grovel)
       (test-binding-type)
       (test-binding-func)
       t))
