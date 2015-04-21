;; Copyright (c) 2015 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This software is released under the MIT License.
;; See the LICENSE file.

(in-package :cl-libnuma)

(define-foreign-library libnuma
  (:unix (:or "libnuma.so.1" "libnuma.so"))
  (t (:default "libnuma")))

(use-foreign-library libnuma)
