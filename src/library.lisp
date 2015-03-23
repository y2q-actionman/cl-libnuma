(in-package :cl-libnuma)

(define-foreign-library libnuma
  (:unix (:or "libnuma.so.1" "libnuma.so"))
  (t (:default "libnuma")))

(use-foreign-library libnuma)
