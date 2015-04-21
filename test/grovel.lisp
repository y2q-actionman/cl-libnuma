;; Copyright (c) 2015 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This software is released under the MIT License.
;; See the LICENSE file.

(in-package :cl-libnuma.test-grovel)

(defun test-grovel ()
  (assert-progn
    (grovel-constant-exists '+CHAR-BIT+)
    (grovel-constant-exists '+LIBNUMA-API-VERSION+)
    (cffi-type-exists 'size_t)
    (cffi-type-exists 'pid_t)
    (cffi-type-exists '(:struct nodemask_t))
    (cffi-type-exists '(:struct struct-bitmask))
    (cffi-type-exists 'mbind-flag)
    (cffi-enum-exists 'mbind-flag :mpol-mf-move)
    (cffi-enum-exists 'mbind-flag :mpol-mf-move-all)))
