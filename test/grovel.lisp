(in-package :cl-libnuma.test-grovel)

(defun grovel-constant-exists (sym)
  (and (symbol-value sym)
       (constantp sym)))

(defun test-grovel ()
  (and-assert
   (grovel-constant-exists '+CHAR-BIT+)
   (grovel-constant-exists '+LIBNUMA-API-VERSION+)
   (cffi-type-exists 'size_t)
   (cffi-type-exists 'pid_t)
   (cffi-type-exists '(:struct nodemask_t))
   (cffi-type-exists '(:struct struct-bitmask))
   (cffi-type-exists 'mbind-flag)
   (cffi-enum-exists 'mbind-flag :mpol-mf-move)
   (cffi-enum-exists 'mbind-flag :mpol-mf-move-all)))
