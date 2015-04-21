;; Copyright (c) 2015 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This software is released under the MIT License.
;; See the LICENSE file.

(in-package :cl-libnuma.test)

(defun main ()
  (and (test-grovel)
       (test-binding-type)
       (test-binding-func)
       t))
